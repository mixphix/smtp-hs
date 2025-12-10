module Codec.MIME.TextEncoding (utf8, percentEncoding, encodedWord) where

import Control.Monad (liftM3)
import Data.Bits (Bits (shiftR, (.&.)))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isAscii, isControl, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (encodeUtf8)
import Data.Word (Word8)
import Numeric (showHex)

-- | Linked list of UTF-8 bytes
utf8 :: Char -> [Word8]
utf8 = BS.unpack . Text.encodeUtf8 . Text.singleton

-- | Like%20this
percentEncoding :: Text -> Text
percentEncoding =
  Text.concatMap (liftM3 bool escape Text.singleton attrchar)
    . Text.filter (not . isControl)
 where
  attrchar :: Char -> Bool
  attrchar c =
    isAlpha c || isAsciiDigit c || c `Text.elem` "!#$&+-.^_`|~"

  isAsciiDigit :: Char -> Bool
  isAsciiDigit c = isAscii c && isDigit c

  escape :: Char -> Text
  escape = foldMap (Text.toUpper . Text.pack . ('%' :) . (`showHex` "")) . utf8

encodedWord :: Text -> ByteString
encodedWord = BS.foldl' (\acc c -> acc <> enc c) mempty . Text.encodeUtf8
 where
  enc :: Word8 -> ByteString
  enc c
    | c `elem` specials = esc c
    | c == 32 = BS.singleton 95
    | 33 <= c && c <= 126 = BS.singleton c
    | otherwise = esc c
   where
    esc :: Word8 -> ByteString
    esc w = foldMap BS.singleton [61, hex (shiftR w 4), hex (w .&. 15)]

    hex :: Word8 -> Word8
    hex w = if w < 10 then w + 48 else w + 55

  specials :: [Word8]
  specials = BS.unpack (Text.encodeUtf8 "\"()<>[]:;@\\,.?_=")
