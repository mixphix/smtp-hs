{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Email
  ( ConnectionMethod (..)
  , SMTPSettings (..)
  , MonadSMTP (..)
  , HostName
  , PortNumber
  , module Network.SMTP.Email
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (MonadIO, MonadRandom)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString qualified as BS (toStrict)
import Data.Foldable (for_)
import Network.Connection (TLSSettings)
import Network.SMTP
  ( closeSMTP
  , commandOrQuit
  , connectSMTP'
  , connectSMTPSTARTTLS'
  )
import Network.SMTP.Auth (AuthType (..), Password, Username)
import Network.SMTP.Command (Command (..))
import Network.SMTP.Email
import Network.Socket (HostName, PortNumber)

data ConnectionMethod = SMTP | SMTPS | SMTPSTARTTLS deriving (Eq, Show)

data SMTPSettings = SMTPSettings
  { hostname :: HostName
  , port :: Maybe PortNumber
  , cxmethod :: ConnectionMethod
  , tlsSettings :: Maybe TLSSettings
  , username :: Maybe Username
  , password :: Maybe Password
  }
  deriving (Show)

class (MonadRandom m, MonadIO m) => MonadSMTP m where
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
  sendMailWith SMTPSettings{..} m@Mail{..} = do
    let connect = case cxmethod of
          SMTP -> connectSMTP'
          SMTPS -> connectSMTP'
          SMTPSTARTTLS -> connectSMTPSTARTTLS'
    (connection, _response) <- connect hostname port Nothing tlsSettings
    flip runReaderT connection do
      case liftA2 (,) username password of
        Nothing -> pure ()
        Just (u, p) -> void $ commandOrQuit 1 (AUTH LOGIN u p) 235
      let box = emailByteString . mailboxEmail
          from = box mailFrom
          tos = box <$> mailTo <> mailCc <> mailBcc
      renderMail m >>= \case
        Left er -> liftIO . fail $ show er
        Right mail -> do
          void $ commandOrQuit 1 (MAIL from) 250
          for_ tos \r -> commandOrQuit 1 (RCPT r) 250
          void $ commandOrQuit 1 (DATA $ BS.toStrict mail) 250
      closeSMTP
