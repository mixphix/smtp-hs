module Network.SMTP where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Char (isDigit, ord)
import Data.Foldable (fold, traverse_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (decodeUtf8, encodeUtf8)
import Data.Word (Word8)
import Network.BSD (getHostName)
import Network.Connection
  ( Connection
  , ConnectionContext
  , ConnectionParams (ConnectionParams)
  , TLSSettings (TLSSettingsSimple)
  , connectTo
  , connectionClose
  , connectionGetLine
  , connectionPut
  , connectionSetSecure
  , initConnectionContext
  )
import Network.SMTP.Auth as Network.SMTP (AuthType (..), auth, encodeLogin)
import Network.SMTP.Command as Network.SMTP (Command (..))
import Network.Socket (HostName, PortNumber)

defaulttls :: TLSSettings
defaulttls = TLSSettingsSimple False False False

response :: (MonadReader Connection m, MonadIO m) => m (Int, ByteString)
response = do
  l <- liftIO . connectionGetLine 1000 =<< ask
  let (digits, body) = B8.span isDigit l
  case B8.uncons body of
    Just ('-', bs) -> second ((bs <> "\n") <>) <$> response
    Just (_, bs) -> pure (read (B8.unpack digits), bs)
    Nothing -> pure (read (B8.unpack digits), mempty)

replyCode :: (MonadReader Connection m, MonadIO m) => m Int
replyCode = fst <$> response

cputLine :: (MonadReader Connection m, MonadIO m) => ByteString -> m ()
cputLine bs = liftIO . (`connectionPut` (bs <> "\r\n")) =<< ask

sendCommand ::
  (MonadReader Connection m, MonadIO m) =>
  Command ->
  m (Int, ByteString)
sendCommand = \case
  DATA bs -> do
    cputLine "DATA"
    code <- replyCode
    unless (code == 354)
      . liftIO
      $ fail "This server is not configured to receive data."
    traverse_ (cputLine . padDot . stripCR) $ BS.split lf bs
    cputLine "."
    response
   where
    padDot, stripCR :: ByteString -> ByteString
    stripCR s = fromMaybe s (BS.stripSuffix "\r" s)
    padDot s = "." <> fromMaybe s (BS.stripPrefix "." s)
    lf :: Word8
    lf = fromIntegral $ ord '\n'
  AUTH LOGIN user pw -> do
    let (u, p) = encodeLogin user pw
    cputLine "AUTH LOGIN"
    void response
    cputLine u
    void response
    cputLine p
    rsp@(code, _) <- response
    rsp <$ unless (code == 235) (liftIO $ fail "Authentication failed.")
  AUTH authtype user pw -> do
    cputLine $ "AUTH " <> B8.pack (show authtype)
    (code, msg) <- response
    unless (code == 334) (liftIO $ fail "Authentication failed.")
    cputLine $ auth authtype (Text.decodeUtf8 msg) user pw
    response
  cmd -> do
    cputLine $ case cmd of
      HELO bs -> "HELO " <> bs
      EHLO bs -> "EHLO " <> bs
      MAIL bs -> "MAIL FROM:<" <> bs <> ">"
      RCPT bs -> "RCPT TO:<" <> bs <> ">"
      EXPN bs -> "EXPN " <> bs
      VRFY bs -> "VRFY " <> bs
      HELP "" -> "HELP\r\n"
      HELP bs -> "HELP " <> bs
      x -> B8.pack (show x)
    response

closeSMTP :: (MonadReader Connection m, MonadIO m) => m ()
closeSMTP = do
  void $ sendCommand QUIT
  liftIO . connectionClose =<< ask

command ::
  (MonadReader Connection m, MonadIO m) =>
  Int ->
  Command ->
  Int ->
  m (Maybe ByteString)
command times cmd expect = do
  (code, msg) <- sendCommand cmd
  if
    | code == expect -> pure (Just msg)
    | times <= 0 -> pure Nothing
    | otherwise -> command (pred times) cmd expect

commandOrQuit ::
  (MonadReader Connection m, MonadIO m) =>
  Int ->
  Command ->
  Int ->
  m ByteString
commandOrQuit times cmd expect = do
  (code, msg) <- sendCommand cmd
  if
    | code == expect -> pure msg
    | times > 1 -> commandOrQuit (pred times) cmd expect
    | otherwise -> do
        closeSMTP
        liftIO
          . fail
          . fold
          $ [ "Unexpected reply to \""
            , show cmd
            , "\": Expected "
            , show expect
            , " but got \""
            , show code
            , ": "
            , Text.unpack (Text.decodeUtf8 msg)
            , "\""
            ]

smtpconnect ::
  (MonadReader Connection m, MonadIO m) =>
  IO HostName ->
  m [ByteString]
smtpconnect gethostname = do
  code <- replyCode
  unless (code == 220) do
    connection <- ask
    liftIO do
      connectionClose connection
      fail "Could not connect to server"
  sender <- Text.encodeUtf8 . Text.pack <$> liftIO gethostname
  command 3 (EHLO sender) 250 >>= \case
    Just ehlo -> pure $ drop 1 (B8.lines ehlo)
    Nothing -> do
      mhelo <- command 3 (HELO sender) 250
      pure $ maybe [] (drop 1 . B8.lines) mhelo

smtpconnectSTARTTLS ::
  (MonadReader Connection m, MonadIO m) =>
  IO HostName ->
  ConnectionContext ->
  TLSSettings ->
  m [ByteString]
smtpconnectSTARTTLS gethostname context tls = do
  code <- replyCode
  unless (code == 220) do
    connection <- ask
    liftIO do
      connectionClose connection
      fail "Could not connect to server"
  sender <- Text.encodeUtf8 . Text.pack <$> liftIO gethostname
  void $ commandOrQuit 3 (EHLO sender) 250
  void $ commandOrQuit 1 STARTTLS 220
  void $ liftIO . flip (connectionSetSecure context) tls =<< ask
  drop 1 . B8.lines <$> commandOrQuit 1 (EHLO sender) 250

connectSMTP' ::
  (MonadIO m) =>
  HostName ->
  Maybe PortNumber ->
  Maybe (IO HostName) ->
  Maybe TLSSettings ->
  m (Connection, [ByteString])
connectSMTP' hostname mport mgethost mtls = do
  let port = fromMaybe 25 mport
      gethostname = fromMaybe getHostName mgethost
  connection <-
    liftIO
      $ initConnectionContext
      >>= (`connectTo` ConnectionParams hostname port mtls Nothing)
  (connection,) <$> runReaderT (smtpconnect gethostname) connection

connectSMTPSTARTTLS' ::
  (MonadIO m) =>
  HostName ->
  Maybe PortNumber ->
  Maybe (IO HostName) ->
  Maybe TLSSettings ->
  m (Connection, [ByteString])
connectSMTPSTARTTLS' hostname mport mgethost mtls = do
  let port = fromMaybe 25 mport
      gethostname = fromMaybe getHostName mgethost
  context <- liftIO initConnectionContext
  connection <-
    liftIO
      . connectTo context
      $ ConnectionParams hostname port Nothing Nothing
  fmap (connection,)
    . flip runReaderT connection
    $ smtpconnectSTARTTLS gethostname context (fromMaybe defaulttls mtls)

connectSMTP :: (MonadIO m) => HostName -> m (Connection, [ByteString])
connectSMTP hostname =
  connectSMTP'
    hostname
    Nothing
    Nothing
    Nothing

connectSMTPS :: (MonadIO m) => HostName -> m (Connection, [ByteString])
connectSMTPS hostname =
  connectSMTP'
    hostname
    (Just 465)
    Nothing
    (Just defaulttls)

connectSMTPSTARTTLS :: (MonadIO m) => HostName -> m (Connection, [ByteString])
connectSMTPSTARTTLS hostname =
  connectSMTPSTARTTLS'
    hostname
    (Just 587)
    Nothing
    Nothing
