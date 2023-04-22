module Codec.MIME.TextEncoding (utf8, rfc5987, rfc2822) where

import Control.Monad (liftM3)
import Data.Bits (Bits (shiftR, (.&.)))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isAlpha, isAscii, isControl, isDigit, ord)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (encodeUtf8)
import Data.Word (Word8)
import Numeric (showHex)

-- | Linked list of UTF-8 bytes
utf8 :: Char -> NonEmpty Word8
utf8 (ord -> n) =
  fromIntegral
    <$> if
        | n <= 0x7f -> pure n
        | n <= 0x7ff ->
            0xc0 + shiftR n 6
              :| [0x80 + n .&. 0x3f]
        | n <= 0xffff ->
            0xe0 + shiftR n 12
              :| [ 0x80 + shiftR n 6 .&. 0x3f
                 , 0x80 + n .&. 0x3f
                 ]
        | otherwise ->
            0xf0 + shiftR n 18
              :| [ 0x80 + shiftR n 12 .&. 0x3f
                 , 0x80 + shiftR n 6 .&. 0x3f
                 , 0x80 + n .&. 0x3f
                 ]

-- | Like%20this
rfc5987 :: Text -> Text
rfc5987 =
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

-- | Header name format
rfc2822 :: Text -> ByteString
rfc2822 = Text.foldl' (\acc c -> acc <> enc c) mempty
 where
  enc :: Char -> ByteString
  enc (fromIntegral . ord -> c)
    | c `elem` specials = esc (hex c)
    | 33 <= c && c <= 126 = ByteString.singleton c
    | otherwise = esc (hex c)
   where
    esc :: Word8 -> ByteString
    esc w = foldMap ByteString.singleton [61, shiftR w 4, w .&. 15]

    hex :: Word8 -> Word8
    hex w = if w < 10 then w + 48 else w + 55

  specials :: [Word8]
  specials = ByteString.unpack $ Text.encodeUtf8 "\"()<>[]:;@\\,.?_="
