module Codec.MIME.Header (ToHeader (toHeader)) where

import Data.Text (Text)

class ToHeader x where
  toHeader :: x -> Text
