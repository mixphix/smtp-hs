module Codec.MIME.QuotedPrintable (toQP) where

import Codec.MIME.TextEncoding (utf8)
import Control.Block (reduceL)
import Data.ByteString.Builder (Builder)
import Data.Char (toUpper)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Numeric (showHex)

data QPPart
  = Printable Text
  | Quoted Text
  | Tab
  | CarriageReturn
  | LineFeed
  | Space
  deriving (Show)

qpPart :: Char -> QPPart
qpPart char@(utf8 -> bits)
  | char == '=' = Quoted escaped
  | char == '\t' = Tab
  | char == '\r' = CarriageReturn
  | char == '\n' = LineFeed
  | char == ' ' = Space
  | pure 33 <= bits && bits <= pure 126 = Printable (Text.singleton char)
  | otherwise = Quoted escaped
 where
  -- Lowercase hexadecimals are explicitly illegal
  -- https://www.rfc-editor.org/rfc/rfc2045#section-6.7
  escaped = Text.pack $ toUpper <$> foldMap (`showHex` "") bits

squashParts :: Bool -> [QPPart] -> [QPPart]
squashParts isTextual = \case
  Printable t1 : Printable t2 : rest ->
    squashParts isTextual (Printable (t1 <> t2) : rest)
  Quoted t1 : Quoted t2 : rest ->
    squashParts isTextual (Quoted (t1 <> t2) : rest)
  (x : xs) -> x : squashParts isTextual xs
  [] -> []

qpPartToBuilder :: Bool -> Int -> QPPart -> (Int, Builder)
qpPartToBuilder isTextual column = \case
  Printable text
    | column < (74 - Text.length text) ->
        (column + Text.length text, Text.encodeUtf8Builder text)
    | Text.length text > 74 ->
        let (pref, suff) = Text.splitAt (74 - column) text
         in ((Text.encodeUtf8Builder pref <> "=\r\n") <>)
              <$> qpPartToBuilder isTextual 0 (Printable suff)
    | otherwise -> (Text.length text, "=\r\n" <> Text.encodeUtf8Builder text)
  Quoted bits
    | column < (75 - 3 * length codes) -> (column + 3 * length codes, encoded)
    | otherwise -> (3 * length codes, "=\r\n" <> encoded)
   where
    encoded = foldMap (("=" <>) . Text.encodeUtf8Builder) codes
    codes = Text.chunksOf 2 bits
  Tab
    | column < 73 -> (column + 3, "=09")
    | otherwise -> (3, "=\r\n=09")
  CarriageReturn
    | isTextual && column < 75 -> (column + 1, "\r")
    | isTextual -> (0, "\r")
    | not isTextual && column < 72 -> (column + 3, "=0D")
    | otherwise -> (3, "=\r\n=0D")
  LineFeed
    | isTextual && column < 75 -> (0, "\n")
    | isTextual -> (0, "\n")
    | not isTextual && column < 72 -> (column + 3, "=0A")
    | otherwise -> (3, "=\r\n=0A")
  Space
    | column < 75 -> (column + 1, " ")
    | otherwise -> (1, "=\r\n ")

toQP :: Bool -> String -> Builder
toQP isTextual message =
  let qpparts = squashParts isTextual $ qpPart <$> message
   in snd $ reduceL (0, "") qpparts \(column, built) qp ->
        (built <>) <$> qpPartToBuilder isTextual column qp
