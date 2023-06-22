module Codec.MIME.Disposition
  ( Disposition (..)
  , disposition
  , DispType (..)
  , disptype
  , DispParam (..)
  , dispparam
  )
where

import Codec.MIME.TextEncoding (rfc5987)
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

-- | Get the proper 'Text' value for a 'Disposition'.
disposition :: Disposition -> Text
disposition Disposition{..} =
  disptype dispType <> foldMap (("; " <>) . dispparam) dispParams

-- |
-- The disposition type for the content beneath the header.
-- Typically "inline" or "attachment".
data DispType
  = Inline
  | Attachment
  | DispOther Text
  deriving (Eq, Ord, Show)

-- | Get the proper 'Text' value for a 'DispType'.
disptype :: DispType -> Text
disptype = \case
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
dispparam :: DispParam -> Text
dispparam = \case
  Name t -> "name=\"" <> t <> "\""
  FilenameStar t -> "filename*=utf-8''" <> rfc5987 t
  Filename t -> "filename=\"" <> t <> "\""
  Created t -> "creation-date=\"" <> Text.pack (iso8601Show t) <> "\""
  Modified t -> "modification-date=\"" <> Text.pack (iso8601Show t) <> "\""
  Read t -> "read-date=\"" <> Text.pack (iso8601Show t) <> "\""
  Size t -> "size=" <> Text.pack (show t)
  Other t t' -> t <> "=" <> t'
