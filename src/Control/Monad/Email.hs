{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Email
  ( SMTPSettings (..)
  , MonadSMTP (..)
  , HostName
  , PortNumber
  , ConnectionMethod (..)
  , AuthType (LOGIN, LOGIN_OAUTH)
  , Username (..)
  , Password
  , mkPassword
  , module Network.SMTP.Email
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS (toStrict)
import Data.Foldable (for_)
import Network.Connection (TLSSettings)
import Network.SMTP
  ( closeSMTP
  , commandOrQuit
  , connectSMTP'
  , connectSMTPSTARTTLS'
  )
import Network.SMTP.Auth (AuthType (..), Password, Username (..), mkPassword)
import Network.SMTP.Command (Command (..))
import Network.SMTP.Email
import Network.Socket (HostName, PortNumber)

data ConnectionMethod = SMTP | SMTPS | SMTPSTARTTLS deriving (Eq, Read, Show)

data SMTPSettings = SMTPSettings
  { hostname :: HostName
  , port :: Maybe PortNumber
  , cxmethod :: ConnectionMethod
  , authType :: AuthType
  , tlsSettings :: Maybe TLSSettings
  , username :: Maybe Username
  , password :: Maybe Password
  }
  deriving (Show)

class (MonadIO m) => MonadSMTP m where
  {-# MINIMAL smtpSettings #-}
  smtpSettings :: m SMTPSettings

  -- |
  -- Login to the SMTP server using the default 'SMTPSettings',
  -- then render and send the email.
  sendMail :: Mail -> m ()
  sendMail mail = smtpSettings >>= (`sendMailWith` mail)

  -- |
  -- Login to the SMTP server using the provided 'SMTPSettings',
  -- then render and send the email.
  sendMailWith :: SMTPSettings -> Mail -> m ()
  sendMailWith SMTPSettings{..} m@Mail{..} = liftIO do
    let connect = case cxmethod of
          SMTP -> connectSMTP'
          SMTPS -> connectSMTP'
          SMTPSTARTTLS -> connectSMTPSTARTTLS'
    (cx, _response) <- connect hostname port Nothing tlsSettings
    case liftA2 (,) username password of
      Nothing -> pure ()
      Just (u, p) -> void $ commandOrQuit cx 1 (AUTH authType u p) 235
    let box = emailByteString . mailboxEmail
        from = box mailFrom
        tos = box <$> mailTo <> mailCc <> mailBcc
    renderMail m >>= \case
      Left err -> fail (show err)
      Right mail -> do
        void $ commandOrQuit cx 1 (MAIL from) 250
        for_ tos \r -> commandOrQuit cx 1 (RCPT r) 250
        void $ commandOrQuit cx 1 (DATA $ BS.toStrict mail) 250
    closeSMTP cx
