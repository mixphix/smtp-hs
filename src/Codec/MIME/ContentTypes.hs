module Codec.MIME.ContentTypes
  ( ContentType (..)
  , parseContentType
  , contentTypeP
  , MediaType (..)
  , parseMediaType
  , mediaTypeP
  , Multipart (..)
  , parseMultipart
  , multipartP
  , pattern ApplicationPdf
  , pattern TextPlain
  , pattern TextHtml
  , pattern MultipartRelated
  )
where

import Codec.MIME.Header (ToHeader (toHeader))
import Control.Applicative (Alternative (..))
import Data.Foldable (fold)
import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec (ParseError)
import Text.Parsec qualified as Parse
import Text.Parsec.Text (Parser)

-- | The value of the "Content-Type" header along with associated parameters.
data ContentType = ContentType
  { mediaType :: MediaType
  , contentParams :: [(Text, Text)]
  }
  deriving (Eq, Show)

instance ToHeader ContentType where
  toHeader :: ContentType -> Text
  toHeader ContentType{..} =
    toHeader mediaType <> flip foldMap contentParams \(name, val) ->
      fold ["; ", name, "=\"", val, "\""]

parseContentType :: Text -> Either ParseError ContentType
parseContentType = Parse.parse contentTypeP ""

contentTypeP :: Parser ContentType
contentTypeP = do
  mediaType <- mediaTypeP <* Parse.optional (Parse.char ';')
  contentParams <-
    ( on (,) Text.pack
        <$> Parse.manyTill accepted (Parse.char '=')
        <*> Parse.many accepted
      )
      `Parse.sepBy` Parse.char ';'
  pure ContentType{..}
 where
  accepted = Parse.try Parse.alphaNum <|> Parse.oneOf "-_.'"

-- |
-- The media type for the content beneath the header.
--
-- A media type may have multiple related subtypes separated by @+@ characters.
--
-- See [the IANA website](https://www.iana.org/assignments/media-types/media-types.xhtml)
-- to decide which content type is right for your data.
data MediaType
  = Application [Text]
  | Audio [Text]
  | Font [Text]
  | Image [Text]
  | Message [Text]
  | Model [Text]
  | Multipart Multipart
  | Text [Text]
  | Video [Text]
  deriving (Eq, Ord, Show)

instance ToHeader MediaType where
  toHeader :: MediaType -> Text
  toHeader = \case
    Application ts -> "application/" <> Text.intercalate "+" ts
    Audio ts -> "audio/" <> Text.intercalate "+" ts
    Font ts -> "font/" <> Text.intercalate "+" ts
    Image ts -> "image/" <> Text.intercalate "+" ts
    Message ts -> "message/" <> Text.intercalate "+" ts
    Model ts -> "model/" <> Text.intercalate "+" ts
    Multipart m -> "multipart/" <> toHeader m
    Text ts -> "text/" <> Text.intercalate "+" ts
    Video ts -> "video/" <> Text.intercalate "+" ts

parseMediaType :: Text -> Either ParseError MediaType
parseMediaType = Parse.parse mediaTypeP ""

mediaTypeP :: Parser MediaType
mediaTypeP =
  Parse.choice
    . map Parse.try
    $ [ applicationP
      , audioP
      , fontP
      , imageP
      , messageP
      , modelP
      , multipartP'
      , textP
      , videoP
      ]
 where
  applicationP =
    Application
      . map Text.pack
      <$ Parse.string "application/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))
  audioP =
    Audio
      . map Text.pack
      <$ Parse.string "audio/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))
  fontP =
    Font
      . map Text.pack
      <$ Parse.string "font/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))
  imageP =
    Image
      . map Text.pack
      <$ Parse.string "image/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))
  messageP =
    Message
      . map Text.pack
      <$ Parse.string "message/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))
  modelP =
    Model
      . map Text.pack
      <$ Parse.string "model/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))
  multipartP' =
    Multipart <$> (Parse.string "multipart/" *> multipartP)
  textP =
    Text
      . map Text.pack
      <$ Parse.string "text/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))
  videoP =
    Video
      . map Text.pack
      <$ Parse.string "video/"
      <*> (many accepted `Parse.sepBy` Parse.try (Parse.char '+'))

  accepted = Parse.try Parse.alphaNum <|> Parse.oneOf "-_.'"

-- |
-- Typical values of multipart media types.
-- See [the IANA website](https://www.iana.org/assignments/media-types/media-types.xhtml#multipart)
-- for a complete list.
data Multipart
  = Alternative
  | Byteranges
  | Digest
  | Encrypted
  | Example
  | FormData
  | Mixed
  | Multilingual
  | Parallel
  | Related
  | Report
  | Signed
  | VoiceMessage
  deriving (Eq, Ord, Show)

instance ToHeader Multipart where
  toHeader :: Multipart -> Text
  toHeader = \case
    Alternative -> "alternative"
    Byteranges -> "byteranges"
    Digest -> "digest"
    Encrypted -> "encrypted"
    Example -> "example"
    FormData -> "form-data"
    Mixed -> "mixed"
    Multilingual -> "multilingual"
    Parallel -> "parallel"
    Related -> "related"
    Report -> "report"
    Signed -> "signed"
    VoiceMessage -> "voice-message"

parseMultipart :: Text -> Either ParseError Multipart
parseMultipart = Parse.parse multipartP ""

multipartP :: Parser Multipart
multipartP =
  Parse.choice
    . map Parse.try
    $ [ Alternative <$ Parse.string "alternative"
      , Byteranges <$ Parse.string "byteranges"
      , Digest <$ Parse.string "digest"
      , Encrypted <$ Parse.string "encrypted"
      , Example <$ Parse.string "example"
      , FormData <$ Parse.string "form-data"
      , Mixed <$ Parse.string "mixed"
      , Multilingual <$ Parse.string "multilingual"
      , Parallel <$ Parse.string "parallel"
      , Related <$ Parse.string "related"
      , Report <$ Parse.string "report"
      , Signed <$ Parse.string "signed"
      , VoiceMessage <$ Parse.string "voice-message"
      ]

pattern ApplicationPdf :: MediaType
pattern ApplicationPdf = Application ["pdf"]

pattern TextPlain :: MediaType
pattern TextPlain = Text ["plain"]

pattern TextHtml :: MediaType
pattern TextHtml = Text ["html"]

pattern MultipartRelated :: MediaType
pattern MultipartRelated = Multipart Related
