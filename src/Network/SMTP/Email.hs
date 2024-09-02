module Network.SMTP.Email
  ( module Network.SMTP.Email.Parse
  , Mail (..)
  , blankMail
  , renderMail
  , subject
  , body
  , emailBodyImages
  , to
  , cc
  , bcc
  , attachPart
  , attach
  , attachFile
  , attachImage
  )
where

import Codec.MIME
  ( MediaType
  , Multipart (..)
  , Part
  , PartBuilder (..)
  , SomePart
  , ToSinglePart
  , buildHeaders
  , encodeEscapedUtf8
  , filePart
  , imagePart
  , mixedParts
  , partBuilder
  , related
  , somePart
  , toSinglePart
  )
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Network.SMTP.Email.Parse
import Text.Blaze.Html (Html)

buildMailbox :: Mailbox -> Builder
buildMailbox Mailbox{..} =
  fold
    [ foldMap ((<> " ") . encodeEscapedUtf8) mailboxName
    , "<"
    , byteString $ emailByteString mailboxEmail
    , ">"
    ]

data Mail = Mail
  { mailFrom :: Mailbox
  , mailTo :: [Mailbox]
  , mailCc :: [Mailbox]
  , mailBcc :: [Mailbox]
  , mailHeaders :: [(Text, Text)]
  , mailParts :: [NonEmpty SomePart]
  }
  deriving (Generic)

-- | Intended usage:
--
-- @
-- let mail =
--       blankMail [mailbox|from\@example.com|]
--         & to [mailbox|to\@example.com|]
--         & subject \"Example\"
--         & body -- same as: attach @Html
--           ...
-- @
blankMail :: Mailbox -> Mail
blankMail from = Mail from [] [] [] [] []

mailboxHeaders :: Mail -> Builder
mailboxHeaders Mail{..} =
  foldMap
    (foldMap (\(str, m) -> byteString (encodeUtf8 str) <> buildMailbox m))
    [ [("From" :: Text, mailFrom)]
    , map ("To",) mailTo
    , map ("Cc",) mailCc
    , map ("Bcc",) mailBcc
    ]
    <> foldMap buildHeaders (mailHeaders <> [("MIME-Version", "1.0")])

data MailRenderError
  = UnspecifiedTarget
  | UnspecifiedContent
  deriving (Eq, Show)

renderMail ::
  (MonadIO m) =>
  Mail ->
  m (Either MailRenderError BSL.ByteString)
renderMail m = case nonEmpty (sort m.mailParts) of
  _ | null m.mailTo -> pure (Left UnspecifiedTarget)
  Nothing -> pure (Left UnspecifiedContent)
  Just parts -> fmap Right do
    pb <- liftIO do
      for parts (partBuilder Related) >>= \case
        pb :| [] -> pure pb
        pbs -> mixedParts pbs
    pure
      . toLazyByteString
      . fold
      $ [ mailboxHeaders m
        , foldMap buildHeaders pb.headers
        , "\n"
        , pb.builder
        ]

subject :: Text -> Mail -> Mail
subject subj m = m{mailHeaders = ("Subject", subj) : mailHeaders m}

body :: Html -> Mail -> Mail
body = attach

emailBodyImages :: (MonadIO m) => Html -> [FilePath] -> Mail -> m Mail
emailBodyImages html ipaths m = do
  images <- mapM imagePart ipaths
  pure $ attachPart (related $ toSinglePart html :| images) m

to :: [Mailbox] -> Mail -> Mail
to mbxs m = m{mailTo = mbxs <> mailTo m}

cc :: [Mailbox] -> Mail -> Mail
cc mbxs m = m{mailCc = mbxs <> mailCc m}

bcc :: [Mailbox] -> Mail -> Mail
bcc mbxs m = m{mailCc = mbxs <> mailBcc m}

attachPart :: Part mult -> Mail -> Mail
attachPart p m = m{mailParts = mailParts m <> [pure (somePart p)]}

attach :: (ToSinglePart part) => part -> Mail -> Mail
attach p = attachPart (toSinglePart p)

attachFile :: (MonadIO m) => FilePath -> Text -> MediaType -> Mail -> m Mail
attachFile file name media m = filePart file name media <&> (`attachPart` m)

attachImage :: (MonadIO m) => FilePath -> Mail -> m Mail
attachImage file m = imagePart file <&> (`attachPart` m)
