module TestRoundtrip (spec) where

import Data.Maybe (isNothing)
import Data.Org
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults (bare)
import qualified TextShow as TS

-- | How faithfully 'TS.showt' re-serializes an input.  'Stable' pins only that
-- re-parsing the render yields the same elements; 'Exact' also pins the bytes.
-- The split IS the documented render-lossiness budget: a 'Stable' input asserts
-- fidelity the renderer lacks once promoted.
data Fidelity = Stable | Exact

cases :: [(String, Text, Fidelity)]
cases =
  [ ("Single token",            "hello",                                          Exact)
  , ("Multiple tokens",         "hello world",                                    Stable)
  , ("Headline",                "* Hello",                                        Exact)
  , ("Headline with TODO",      "* TODO Hello",                                   Exact)
  , ("Headline with priority",  "** TODO [#A] Hello",                             Exact)
  , ("Headline with tags",      "* Hello :tag1:tag2:",                            Exact)
  , ("Full headline",           "** TODO [#B] My task :work:urgent:",             Exact)
  , ("Deep indent",             "**** Deep headline",                             Stable)
  , ("Pragma category",         "#+CATEGORY: mycat",                              Stable)
  , ("Pragma TODO",             "#+TODO: TODO STARTED | DONE CANCELLED",          Stable)
  , ("Generic pragma",          "#+TITLE: My Document",                           Stable)
  , ("Active timestamp",        "<2024-01-15 Mon 10:30>",                         Exact)
  , ("Inactive timestamp",      "[2024-06-01 Sat 09:00]",                         Stable)
  , ("Active midnight",         "<2024-01-01 Mon 00:00>",                         Stable)
  , ("Inactive midnight",       "[2024-01-01 Mon 00:00]",                         Exact)
  , ("Date-only timestamp",     "<2026-07-08 Wed>",                               Exact)
  , ("Clock range",             "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]", Exact)
  , ("Date range",              "<2024-01-15 Mon>--<2024-01-19 Fri>",             Stable)
  , ("Date-only in a title",    "* Due <2026-07-08 Wed>",                         Exact)
  ]

spec :: TestTree
spec = testGroup "Roundtrip" (map check cases)

-- | Render what INPUT parses to and re-parse it: the elements must survive
-- unchanged.  Both parses must succeed and yield something, or the comparison
-- is vacuous.  'Exact' additionally pins the render to INPUT byte for byte.
check :: (String, Text, Fidelity) -> TestTree
check (desc, input, fidelity) = testCase desc $ do
  let (elems, _ctx, err) = orgParse defaultContext input
      rendered = T.intercalate " " (map TS.showt elems)
      (elems2, _ctx2, err2) = orgParse defaultContext rendered
  assertBool ("parse error on " <> show input) (isNothing err)
  assertBool ("no elements parsed from " <> show input) (not (null elems))
  assertBool ("parse error re-parsing " <> show rendered) (isNothing err2)
  assertEqual ("unstable: " <> show input <> " -> " <> show rendered)
              (bare elems) (bare elems2)
  case fidelity of
    Stable -> pure ()
    Exact  -> assertEqual ("render of " <> show input) input rendered
