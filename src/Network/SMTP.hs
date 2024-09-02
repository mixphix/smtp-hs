module Network.SMTP where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
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

response :: (MonadIO m) => Connection -> m (Int, ByteString)
response cx = do
  l <- liftIO $ connectionGetLine 1000 cx
  let (digits, body) = B8.span isDigit l
  case B8.uncons body of
    Just ('-', bs) -> second ((bs <> "\n") <>) <$> response cx
    Just (_, bs) -> pure (read (B8.unpack digits), bs)
    Nothing -> pure (read (B8.unpack digits), mempty)

replyCode :: (MonadIO m) => Connection -> m Int
replyCode = fmap fst . response

cputLine :: (MonadIO m) => Connection -> ByteString -> m ()
cputLine cx bs = liftIO do
  connectionPut cx (bs <> "\r\n")

sendCommand ::
  (MonadIO m) =>
  Connection ->
  Command ->
  m (Int, ByteString)
sendCommand cx = \case
  DATA bs -> do
    cputLine cx "DATA"
    code <- replyCode cx
    unless (code == 354)
      . liftIO
      $ fail "This server is not configured to receive data."
    traverse_ (cputLine cx . padDot . stripCR) $ BS.split lf bs
    cputLine cx "."
    response cx
   where
    padDot, stripCR :: ByteString -> ByteString
    stripCR s = fromMaybe s (BS.stripSuffix "\r" s)
    padDot s = "." <> fromMaybe s (BS.stripPrefix "." s)
    lf :: Word8
    lf = fromIntegral $ ord '\n'
  AUTH LOGIN user pw -> do
    let (u, p) = encodeLogin user pw
    cputLine cx "AUTH LOGIN"
    void $ response cx
    cputLine cx u
    void $ response cx
    cputLine cx p
    rsp@(code, _) <- response cx
    rsp <$ unless (code == 235) (liftIO $ fail "Authentication failed.")
  AUTH authtype user pw -> do
    cputLine cx $ "AUTH " <> B8.pack (show authtype)
    (code, msg) <- response cx
    unless (code == 334) (liftIO $ fail "Authentication failed.")
    cputLine cx $ auth authtype (Text.decodeUtf8 msg) user pw
    response cx
  cmd -> do
    cputLine cx $ case cmd of
      HELO bs -> "HELO " <> bs
      EHLO bs -> "EHLO " <> bs
      MAIL bs -> "MAIL FROM:<" <> bs <> ">"
      RCPT bs -> "RCPT TO:<" <> bs <> ">"
      EXPN bs -> "EXPN " <> bs
      VRFY bs -> "VRFY " <> bs
      HELP "" -> "HELP\r\n"
      HELP bs -> "HELP " <> bs
      x -> B8.pack (show x)
    response cx

closeSMTP :: (MonadIO m) => Connection -> m ()
closeSMTP cx = do
  void $ sendCommand cx QUIT
  liftIO $ connectionClose cx

command ::
  (MonadIO m) =>
  Connection ->
  Int ->
  Command ->
  Int ->
  m (Maybe ByteString)
command cx times cmd expect = do
  (code, msg) <- sendCommand cx cmd
  if
    | code == expect -> pure (Just msg)
    | times <= 0 -> pure Nothing
    | otherwise -> command cx (pred times) cmd expect

commandOrQuit ::
  (MonadIO m) =>
  Connection ->
  Int ->
  Command ->
  Int ->
  m ByteString
commandOrQuit cx times cmd expect = do
  (code, msg) <- sendCommand cx cmd
  if
    | code == expect -> pure msg
    | times > 1 -> commandOrQuit cx (pred times) cmd expect
    | otherwise -> do
        closeSMTP cx
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
  (MonadIO m) =>
  Connection ->
  IO HostName ->
  m [ByteString]
smtpconnect cx gethostname = do
  code <- replyCode cx
  unless (code == 220) $ liftIO do
    connectionClose cx
    fail "Could not connect to server"
  sender <- Text.encodeUtf8 . Text.pack <$> liftIO gethostname
  command cx 3 (EHLO sender) 250 >>= \case
    Just ehlo -> pure $ drop 1 (B8.lines ehlo)
    Nothing -> do
      mhelo <- command cx 3 (HELO sender) 250
      pure $ maybe [] (drop 1 . B8.lines) mhelo

smtpconnectSTARTTLS ::
  (MonadIO m) =>
  Connection ->
  IO HostName ->
  ConnectionContext ->
  TLSSettings ->
  m [ByteString]
smtpconnectSTARTTLS cx gethostname context tls = do
  code <- replyCode cx
  unless (code == 220) do
    liftIO do
      connectionClose cx
      fail "Could not connect to server"
  sender <- Text.encodeUtf8 . Text.pack <$> liftIO gethostname
  void $ commandOrQuit cx 3 (EHLO sender) 250
  void $ commandOrQuit cx 1 STARTTLS 220
  void $ liftIO $ connectionSetSecure context cx tls
  drop 1 . B8.lines <$> commandOrQuit cx 1 (EHLO sender) 250

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
  cx <-
    liftIO $
      initConnectionContext
        >>= (`connectTo` ConnectionParams hostname port mtls Nothing)
  (cx,) <$> smtpconnect cx gethostname

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
  cx <-
    liftIO
      . connectTo context
      $ ConnectionParams hostname port Nothing Nothing
  (cx,)
    <$> smtpconnectSTARTTLS
      cx
      gethostname
      context
      (fromMaybe defaulttls mtls)

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
