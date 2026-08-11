module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified TestConfig as Config
import qualified TestEdit as Edit
import qualified TestExternal as External
import qualified TestFilter as Filter
import qualified TestIndex as Index
import qualified TestParser as Parser
import qualified TestProperties as Properties
import qualified TestRoundtrip as Roundtrip
import qualified TestTimestamp as Timestamp
import qualified TestContext as Context
import qualified TestDesktop as Desktop
import qualified TestNegative as Negative
import qualified TestTextShow as TextShow
import qualified TestSpans as Spans
import qualified TestQuery as Query
import qualified TestSelfContained as SelfContained
import qualified TestServe as Serve
import qualified TestStore as Store
import qualified TestSubtree as Subtree

tests :: TestTree
tests = testGroup "Tests"
  [ Parser.spec
  , Roundtrip.spec
  , Timestamp.spec
  , Context.spec
  , Negative.spec
  , TextShow.spec
  , Spans.spec
  , Edit.spec
  , Query.spec
  , Index.spec
  , External.spec
  , Filter.spec
  , Subtree.spec
  , Serve.spec
  , SelfContained.spec
  , Store.spec
  , Config.spec
  , Desktop.spec
  , Properties.spec
  ]

main :: IO ()
main = defaultMain tests
