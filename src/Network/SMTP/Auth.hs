module Network.SMTP.Auth
  ( Username (..)
  , Password
  , AuthType (..)
  , encodeLogin
  , encodeLoginOAuth
  , auth
  )
where

import Crypto.Hash.Algorithms (MD5)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (copyAndFreeze)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B8
import Data.Char (isAscii)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (encodeUtf8)

data AuthType
  = PLAIN
  | LOGIN
  | LOGIN_OAUTH
  | CRAM_MD5
  deriving (Eq)

instance Show AuthType where
  show :: AuthType -> String
  show = \case
    PLAIN -> "PLAIN"
    LOGIN -> "LOGIN"
    LOGIN_OAUTH -> "XOAUTH2"
    CRAM_MD5 -> "CRAM-MD5"

newtype Username = Username Text
  deriving (Eq)
  deriving newtype (Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show :: Password -> String
  show (Password u) = replicate (Text.length u) '*'

ascii :: Text -> ByteString
ascii t = case Text.partition isAscii t of
  (yes, no)
    | Text.null no -> Text.encodeUtf8 yes
    | otherwise -> error $ "expected ASCII but got: " <> Text.unpack no

encodeLogin :: Username -> Password -> (ByteString, ByteString)
encodeLogin (Username u) (Password p) =
  (B64.encode $ ascii u, B64.encode $ ascii p)

encodeLoginOAuth :: Username -> Password -> ByteString
encodeLoginOAuth (Username u) (Password p) =
  B64.encode . ascii $ fold ["user=", u, "\x01", "auth=Bearer ", p, "\x01\x01"]

auth :: AuthType -> Text -> Username -> Password -> ByteString
auth at c user@(Username u) pass@(Password p) = case at of
  PLAIN -> B64.encode . ascii $ Text.intercalate "\0" [u, u, p]
  LOGIN -> let (u', p') = encodeLogin user pass in B8.unwords [u', p']
  LOGIN_OAUTH -> encodeLoginOAuth user pass
  CRAM_MD5 ->
    B64.encode $ B8.unwords [ascii u, B16.encode $ hmacMD5 (ascii c) (ascii p)]
 where
  hmacMD5 :: ByteString -> ByteString -> ByteString
  hmacMD5 chlg pwd = copyAndFreeze (hmac pwd chlg :: HMAC MD5) (const (pure ()))
