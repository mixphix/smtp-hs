{-# LANGUAGE UndecidableInstances #-}

module Codec.MIME
  ( module MIME
  , Part (..)
  , Mult (..)
  , related
  , PartContent (..)
  , PartBuilder (..)
  , SomePart (..)
  , somePart
  , encodeEscapedUtf8
  , buildHeaders
  , partBuilder
  , mixedParts
  , ToSinglePart (..)
  , toSinglePart
  , filePart
  , imagePart
  )
where

import Codec.MIME.Boundary as MIME (Boundary (..))
import Codec.MIME.ContentTransferEncoding as MIME (ContentTransferEncoding (..))
import Codec.MIME.ContentTypes as MIME
  ( ContentType (..)
  , MediaType (..)
  , Multipart (..)
  , contentTypeP
  , mediaTypeP
  , multipartP
  , parseContentType
  , parseMediaType
  , parseMultipart
  , pattern ApplicationPdf
  , pattern MultipartRelated
  , pattern TextHtml
  , pattern TextPlain
  )
import Codec.MIME.Disposition as MIME
  ( DispParam (..)
  , DispType (..)
  , Disposition (..)
  )
import Codec.MIME.Header (ToHeader (toHeader))
import Codec.MIME.QuotedPrintable as MIME (toQP)
import Codec.MIME.TextEncoding as MIME (encodedWord, utf8)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random (MonadRandom (getRandom))
import Data.ByteString.Base64.Lazy qualified as B64L
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (chr, isAscii)
import Data.Foldable (fold)
import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (encodeUtf8)
import Data.Traversable (for)
import GHC.Records (HasField (getField))
import System.FilePath (takeBaseName, takeExtension)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- | The multiplicity of a 'Part'.
data Mult
  = One
  | Many

-- |
-- A 'Part' is an abstract representation of a part of an email;
-- it could represent an attachment, a message body, or even contain several,
-- related sub-parts, such as different translations of the same content.
--
-- Aside from its content, a 'Part' has a 'ContentType',
-- possibly a 'Disposition', possibly a 'ContentTransferEncoding',
-- possibly a "partLocation" (or URI) where it
-- can be found, and perhaps additional headers.
type Part :: Mult -> Type
data Part mult = Part
  { contentType :: ContentType
  , disposition :: Maybe Disposition
  , encoding :: Maybe ContentTransferEncoding
  , location :: Maybe Text
  , headers :: [(Text, Text)]
  , content :: PartContent mult
  }

-- |
-- The content of a 'Part' is either standalone, or it can be a single nested
-- collection of standalone parts.
type PartContent :: Mult -> Type
data PartContent mult where
  Single :: BSL.ByteString -> PartContent One
  Multiple :: NonEmpty (Part One) -> PartContent Many

instance HasField "single" (PartContent One) BSL.ByteString where
  getField :: PartContent One -> BSL.ByteString
  getField (Single p) = p

instance HasField "multiple" (PartContent Many) (NonEmpty (Part One)) where
  getField :: PartContent Many -> NonEmpty (Part One)
  getField (Multiple ps) = ps

-- |
-- An intermediate step between the datatype representation of a 'Part' and the
-- raw 'ByteString' representing its section of the email message.
data PartBuilder = PartBuilder
  { headers :: [(Text, Text)]
  , builder :: Builder
  }

-- | Connect multiple standalone 'Part's into a @multipart/related@ 'Part'.
related :: NonEmpty (Part One) -> Part Many
related parts =
  Part
    (ContentType MultipartRelated [])
    Nothing
    Nothing
    Nothing
    []
    (Multiple parts)

encodeEscapedUtf8 :: Text -> Builder
encodeEscapedUtf8 t = fold ["=?utf-8?Q?", byteString $ encodedWord t, "?="]

-- |
-- Sanitize the header name and value before
-- preparing for 'ByteString' conversion.
buildHeaders :: (Text, Text) -> Builder
buildHeaders (hname, hval) =
  fold
    [ byteString . Text.encodeUtf8 $
        Text.filter (\w -> w /= ':' && '!' <= w && w <= '~') hname
    , ": "
    , if Text.all isAscii hval
        then byteString $ Text.encodeUtf8 hval
        else encodeEscapedUtf8 hval
    , "\n"
    ]

-- | Prepare a standalone 'Part' for 'ByteString' conversion.
singleBuilder :: Part One -> PartBuilder
singleBuilder p = PartBuilder{..}
 where
  Single bs = p.content
  encodingHeader = case p.encoding of
    Nothing -> id
    Just e -> (("Content-Transfer-Encoding", toHeader e) :)
  dispositionHeader = case p.disposition of
    Nothing -> id
    Just d -> (("Content-Disposition", toHeader d) :)
  locationHeader = case p.location of
    Nothing -> id
    Just loc -> (("Content-Location", loc) :)
  headers =
    (("Content-Type", toHeader p.contentType) :)
      . encodingHeader
      . dispositionHeader
      . locationHeader
      $ p.headers
  builder = case p.encoding of
    Just Base64 ->
      foldMap ((<> "\r\n") . lazyByteString . B64L.encode) $
        flip unfoldr bs \b ->
          if BSL.null b then Nothing else pure (BSL.splitAt 57 b)
    Just QuotedPrintable -> toQP True (chr . fromIntegral <$> BSL.unpack bs)
    Just Binary -> toQP False (chr . fromIntegral <$> BSL.unpack bs)
    _ -> lazyByteString bs

builderOf ::
  (HasField "headers" x [(Text, Text)]) =>
  Boundary ->
  NonEmpty x ->
  (x -> Builder) ->
  Builder
builderOf (Boundary b) xs f =
  fold
    [ flip foldMap xs \x ->
        fold
          [ foldMap byteString ["--", Text.encodeUtf8 b, "\n"]
          , foldMap buildHeaders x.headers
          , "\n"
          , f x
          , "\n"
          ]
    , "--"
    , byteString (Text.encodeUtf8 b)
    , "--"
    ]

-- |
-- Prepare a nested 'Part' for 'ByteString' conversion.
-- A 'Boundary' delineator is required.
multipleBuilder :: Boundary -> Part Many -> PartBuilder
multipleBuilder b p = PartBuilder{..}
 where
  headers =
    [
      ( "Content-Type"
      , toHeader $ ContentType MultipartRelated [("boundary", toHeader b)]
      )
    ]
  builder = builderOf b p.content.multiple \x -> lazyByteString x.content.single

-- |
-- Delay the handling of the multiplicity of a part for the purposes
-- of 'partEncoding' the entire message.
data SomePart
  = SPMultOne (Part One)
  | SPMany (Part Many)

instance Eq SomePart where _ == _ = False

instance Ord SomePart where
  SPMultOne part1 `compare` SPMultOne part2 =
    comparing (fmap dispType) part1.disposition part2.disposition
  SPMultOne part1 `compare` SPMany part2 =
    comparing (fmap dispType) part1.disposition part2.disposition
  SPMany part1 `compare` SPMultOne part2 =
    comparing (fmap dispType) part1.disposition part2.disposition
  SPMany part1 `compare` SPMany part2 =
    comparing (fmap dispType) part1.disposition part2.disposition

-- | Wrap the multiplicity of a 'Part'.
somePart :: Part mult -> SomePart
somePart p = case p.content of
  Single _ -> SPMultOne p
  Multiple _ -> SPMany p

-- |
-- Build a message from a collection of parts of arbitrary multiplicity.
--
-- The 'Multipart' value is ignored in the case of a standalone 'Part',
-- but describes the MIME type in the nested case. A good default is 'Related'.
partBuilder ::
  (MonadRandom m) =>
  Multipart ->
  NonEmpty SomePart ->
  m PartBuilder
partBuilder _ (a :| []) = case a of
  SPMultOne p -> pure $ singleBuilder p
  SPMany p -> (`multipleBuilder` p) <$> getRandom
partBuilder m arbs = do
  ps <- for arbs $ partBuilder m . pure
  b <- getRandom
  let headers =
        [
          ( "Content-Type"
          , toHeader $ ContentType (Multipart m) [("boundary", toHeader b)]
          )
        ]
      builder = builderOf b ps (.builder)
  pure PartBuilder{..}

-- |
-- Create a 'PartBuilder' for the entire message given the 'PartBuilder's
-- for each of its parts. The result will have a MIME type of @multipart/mixed@.
mixedParts :: (MonadRandom m) => NonEmpty PartBuilder -> m PartBuilder
mixedParts ps = do
  b <- getRandom
  let headers =
        [
          ( "Content-Type"
          , toHeader $ ContentType (Multipart Mixed) [("boundary", toHeader b)]
          )
        ]
      builder = builderOf b ps (.builder)
  pure PartBuilder{..}

-- |
-- 'ToSinglePart' captures the data-specific method to attach itself
-- to an email message.
--
-- For example, @Html@ values are encoded as @quoted-printable@ text,
-- whereas a 'Text' value is simply converted to UTF-8. Files may have
-- their own appropriate MIME types, so be sure to declare this instance
-- for its representation if you plan to send it via email.
class ToSinglePart a where
  contentTypeFor :: Const ContentType a
  default contentTypeFor :: (Show a) => Const ContentType a
  contentTypeFor = Const $ ContentType TextPlain [("charset", "utf-8")]

  dispositionFor :: Const (Maybe Disposition) a
  default dispositionFor :: (Show a) => Const (Maybe Disposition) a
  dispositionFor = Const Nothing

  encodingFor :: Const (Maybe ContentTransferEncoding) a
  default encodingFor :: (Show a) => Const (Maybe ContentTransferEncoding) a
  encodingFor = Const (Just QuotedPrintable)

  makePartContent :: a -> BSL.ByteString

-- |
-- Convert a value to a 'Part' with the default
-- 'disposition and 'partEncoding' for its type.
toSinglePart :: forall a. (ToSinglePart a) => a -> Part One
toSinglePart a = Part{..}
 where
  Const contentType = contentTypeFor @a
  Const disposition = dispositionFor @a
  Const encoding = encodingFor @a
  location = Nothing
  headers = []
  content = Single (makePartContent a)

-- |
-- Convert a file to a 'Part', with the specified file path,
-- file name and media type.
filePart :: (MonadIO m) => FilePath -> Text -> MediaType -> m (Part One)
filePart fp name media = do
  let contentType = ContentType media [("charset", "utf-8")]
      disposition = Just (Disposition Attachment [FilenameStar name])
      encoding = Just Base64
      location = Just name
      headers = []
  content <- Single <$> liftIO (BSL.readFile fp)
  pure Part{..}

-- | Add the image at the specified file path to an email message.
imagePart :: (MonadIO m) => FilePath -> m (Part One)
imagePart fp = do
  let name = Text.pack $ takeBaseName fp
      ext = Text.pack $ takeExtension fp
      contentType = ContentType (Image [ext]) []
      disposition = Just (Disposition Inline [FilenameStar name])
      encoding = Just Base64
      location = Just name
      headers = []
  content <- Single <$> liftIO (BSL.readFile fp)
  pure Part{..}

instance ToSinglePart Text where
  makePartContent :: Text -> BSL.ByteString
  makePartContent = BSL.fromStrict . Text.encodeUtf8

instance ToSinglePart Html where
  contentTypeFor :: Const ContentType Html
  contentTypeFor = Const (ContentType TextHtml [("charset", "utf-8")])

  dispositionFor :: Const (Maybe Disposition) Html
  dispositionFor = Const Nothing

  encodingFor :: Const (Maybe ContentTransferEncoding) Html
  encodingFor = Const (Just QuotedPrintable)

  makePartContent :: Html -> BSL.ByteString
  makePartContent = renderHtml
