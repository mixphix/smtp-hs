module Codec.MIME.ContentTransferEncoding
  ( ContentTransferEncoding (..)
  , contenttransferencoding
  )
where

import Data.Text (Text)

-- |
-- The value of the "Content-Transfer-Encoding" header.
data ContentTransferEncoding
  = SevenBit
  | EightBit
  | Base64
  | QuotedPrintable
  | Binary
  deriving (Eq, Show)

-- | Get the proper 'Text' value for a 'ContentTransferEncoding'.
contenttransferencoding :: ContentTransferEncoding -> Text
contenttransferencoding = \case
  SevenBit -> "7bit"
  EightBit -> "8bit"
  Base64 -> "base64"
  QuotedPrintable -> "quoted-printable"
  Binary -> "binary"
