{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.SMTP.Email (
  module Network.SMTP.Email.Parse,
  Mail (..),
  renderMail,
  newmessage,
  subject,
  body,
  emailBodyImages,
  to,
  cc,
  bcc,
  attachPart,
  attach,
  attachFile,
  attachImage,
) where

import Codec.MIME
import Control.Monad.Random
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Network.SMTP.Email.Parse
import Text.Blaze.Html (Html)

buildMailbox :: Mailbox -> Builder
buildMailbox Mailbox{..} =
  fold [maybe mempty ((<> " ") . encodeEscapedUtf8) mailboxName, "<", byteString $ emailByteString mailboxEmail, ">"]

-- |
-- > message :: Mail
-- > message =
-- >   newmessage [mailbox|masterword@masterword.com|]
-- >     & to [mailboxes|vverdi@masterword.com|]
-- >     & subject "New email code!"
-- >     & body
-- >       [hamlet|
-- >         <html>
-- >           <body>
-- >             <p>Right on!!
-- >       |]
data Mail = Mail
  { mailFrom :: Mailbox
  , mailTo :: [Mailbox]
  , mailCc :: [Mailbox]
  , mailBcc :: [Mailbox]
  , mailHeaders :: [(Text, Text)]
  , mailParts :: [NonEmpty SomePart]
  }
  deriving (Generic)

newmessage :: Mailbox -> Mail
newmessage from = Mail from [] [] [] [] []

mailboxHeaders :: Mail -> Builder
mailboxHeaders Mail{..} =
  foldMap
    (foldMap (\(str, m) -> byteString (encodeUtf8 str) <> buildMailbox m))
    [ [("From" :: Text, mailFrom)]
    , map ("To",) mailTo
    , map ("Cc",) mailCc
    , map ("Bcc",) mailBcc
    ]

data MailRenderError
  = UnspecifiedTarget
  | UnspecifiedContent
  deriving (Eq, Show)

renderMail :: (MonadRandom m) => Mail -> m (Either MailRenderError BSL.ByteString)
renderMail m@Mail{..} =
  if null mailTo
    then pure $ Left UnspecifiedTarget
    else case nonEmpty (sort mailParts) of
      Nothing -> pure $ Left UnspecifiedContent
      Just parts -> fmap Right $ do
        PartBuilder{..} <- mixedParts =<< forM parts (partBuilder Alternative)
        pure . toLazyByteString $
          fold
            [ mailboxHeaders m
            , foldMap buildHeaders $ mailHeaders <> [("MIME-Version", "1.0")]
            , foldMap buildHeaders headers
            , "\n"
            , bsbuilder
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
attachPart p m = m{mailParts = mailParts m ++ [pure $ somePart p]}

attach :: (ToSinglePart part) => part -> Mail -> Mail
attach p m = m{mailParts = mailParts m ++ [pure . somePart $ toSinglePart p]}

attachFile :: (MonadIO m) => FilePath -> Text -> MediaType -> Mail -> m Mail
attachFile file name media m = filePart file name media <&> (`attachPart` m)

attachImage :: (MonadIO m) => FilePath -> Mail -> m Mail
attachImage file m = imagePart file <&> (`attachPart` m)
