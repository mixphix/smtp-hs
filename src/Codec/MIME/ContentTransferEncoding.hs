module Codec.MIME.ContentTransferEncoding (ContentTransferEncoding (..)) where

import Codec.MIME.Header (ToHeader (toHeader))

-- | The value of the "Content-Transfer-Encoding" header.
data ContentTransferEncoding
  = SevenBit
  | EightBit
  | Base64
  | QuotedPrintable
  | Binary
  deriving (Eq, Show)

-- | Get the proper 'Text' value for a 'ContentTransferEncoding'.
instance ToHeader ContentTransferEncoding where
  toHeader = \case
    SevenBit -> "7bit"
    EightBit -> "8bit"
    Base64 -> "base64"
    QuotedPrintable -> "quoted-printable"
    Binary -> "binary"
