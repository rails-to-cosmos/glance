module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified TestLexer as Lexer
import qualified TestParser as Parser
import qualified TestRoundtrip as Roundtrip
import qualified TestTimestamp as Timestamp
import qualified TestContext as Context
import qualified TestNegative as Negative
import qualified TestTextShow as TextShow
import qualified TestSpans as Spans

tests :: TestTree
tests = testGroup "Tests"
  [ Lexer.spec
  , Parser.spec
  , Roundtrip.spec
  , Timestamp.spec
  , Context.spec
  , Negative.spec
  , TextShow.spec
  , Spans.spec
  ]

main :: IO ()
main = defaultMain tests
