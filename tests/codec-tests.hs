module Main where

import Codec.MIME
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy.Char8 qualified as BSL
import Test.Hspec
import Test.QuickCheck

keepRight :: Either a b -> Maybe b
keepRight = \case
  Left _ -> Nothing
  Right x -> Just x

main :: IO ()
main = hspec do
  describe "parseMediaType" do
    it "should parse correctly" do
      parseMediaType "application/epub+zip" `shouldBe` Right (Application ["epub", "zip"])
      parseMediaType "application/xhtml+xml" `shouldBe` Right (Application ["xhtml", "xml"])
      parseMediaType "text/vnd.abc" `shouldBe` Right (Text ["vnd.abc"])
      parseMediaType "multipart/form-data" `shouldBe` Right (Multipart FormData)
      parseMediaType "application/x-www-form-urlencoded" `shouldBe` Right (Application ["x-www-form-urlencoded"])

    it "shouldn't recognize invalid media types" do
      keepRight (parseMediaType "chemical/X") `shouldBe` Nothing
      keepRight (parseMediaType "example/garbage") `shouldBe` Nothing
      keepRight (parseMediaType "xyz/aa+bb+cc") `shouldBe` Nothing

  describe "parseContentType" do
    it "should accept zero parameters" do
      parseContentType "application/pdf" `shouldBe` Right (ContentType ApplicationPdf [])

    it "should accept one parameter" do
      parseContentType "text/html;charset=UTF-8"
        `shouldBe` Right (ContentType TextHtml [("charset", "UTF-8")])

    it "should accept multiple parameters" do
      parseContentType "text/html;charset=UTF-8;me=you.-_"
        `shouldBe` Right (ContentType TextHtml [("charset", "UTF-8"), ("me", "you.-_")])

  describe "quoted printable" do
    it "should wrap lines properly" do
      property @(String -> _) \string -> do
        let qp = Builder.toLazyByteString (toQP True string)
        BSL.splitWith (`elem` ['\r', '\n']) qp
          `shouldSatisfy` all \line -> BSL.length line <= 78
