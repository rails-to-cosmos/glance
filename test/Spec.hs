module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified TestParser as Parser
import qualified TestRoundtrip as Roundtrip
import qualified TestTimestamp as Timestamp
import qualified TestContext as Context
import qualified TestNegative as Negative
import qualified TestTextShow as TextShow
import qualified TestSpans as Spans
import qualified TestQuery as Query
import qualified TestServe as Serve

tests :: TestTree
tests = testGroup "Tests"
  [ Parser.spec
  , Roundtrip.spec
  , Timestamp.spec
  , Context.spec
  , Negative.spec
  , TextShow.spec
  , Spans.spec
  , Query.spec
  , Serve.spec
  ]

main :: IO ()
main = defaultMain tests
