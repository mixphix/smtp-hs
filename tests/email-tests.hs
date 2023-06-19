module Main (main) where

import Control.Monad.Email
import Data.Function ((&))
import Test.Hspec
import Text.Hamlet (shamlet)

main :: IO ()
main = putStrLn "test suite not implemented yet"

dummyEmail :: Mailbox
dummyEmail = [mailbox|Dummy Email <dummy@email.ftw>|]

testmsg :: Mail
testmsg =
  newmessage dummyEmail
    & to [dummyEmail]
    & subject "Testing out package smtp-hs"
    & body
      [shamlet|
        <html>
          <body>
            <p>Test content.
            <p style="background-color: #009fff;">Please ignore.
      |]
