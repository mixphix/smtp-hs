module Codec.MIME.Disposition (Disposition (..), DispType (..), DispParam (..))
where

import Codec.MIME.Header (ToHeader (toHeader))
import Codec.MIME.TextEncoding (percentEncoding)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Compat (LocalTime)
import Data.Time.Format.ISO8601.Compat (iso8601Show)

-- | The value of the "Content-Disposition" header and associated parameters.
data Disposition = Disposition
  { dispType :: DispType
  , dispParams :: [DispParam]
  }
  deriving (Eq)

instance ToHeader Disposition where
  toHeader :: Disposition -> Text
  toHeader Disposition{..} =
    toHeader dispType <> foldMap (("; " <>) . toHeader) dispParams

-- |
-- The disposition type for the content beneath the header.
-- Typically "inline" or "attachment".
data DispType
  = Inline
  | Attachment
  | DispOther Text
  deriving (Eq, Ord, Show)

-- | Get the proper 'Text' value for a 'DispType'.
instance ToHeader DispType where
  toHeader :: DispType -> Text
  toHeader = \case
    Inline -> "inline"
    Attachment -> "attachment"
    DispOther t -> t

-- |
-- Parameters to the content disposition of a section.
-- One should prefer @FilenameStar@ over @Filename@.
data DispParam
  = Name Text
  | FilenameStar Text
  | Filename Text
  | Created LocalTime
  | Modified LocalTime
  | Read LocalTime
  | Size Text
  | Other Text Text
  deriving (Eq, Show)

-- | Get the proper 'Text' value from a 'DispParam'.
instance ToHeader DispParam where
  toHeader :: DispParam -> Text
  toHeader = \case
    Name t -> "name=\"" <> t <> "\""
    FilenameStar t -> "filename*=UTF-8''" <> percentEncoding t
    Filename t -> "filename=" <> percentEncoding t
    Created t -> "creation-date=\"" <> Text.pack (iso8601Show t) <> "\""
    Modified t -> "modification-date=\"" <> Text.pack (iso8601Show t) <> "\""
    Read t -> "read-date=\"" <> Text.pack (iso8601Show t) <> "\""
    Size t -> "size=" <> Text.pack (show t)
    Other t t' -> t <> "=" <> t'
