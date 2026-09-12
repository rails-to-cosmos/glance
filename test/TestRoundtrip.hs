module TestRoundtrip (spec) where

import Data.Maybe (isNothing)
import Data.Org
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults (bare)
import qualified TextShow as TS

-- | THE RENDER-LOSSINESS BUDGET IS EMPTY: every source here renders back to
-- its own bytes, so the case states the bytes rather than a fidelity tier.
cases :: [(String, Text)]
cases =
  [ ("Single token",            "hello")
  , ("Multiple tokens",         "hello world")
  , ("Headline",                "* Hello")
  , ("Headline with TODO",      "* TODO Hello")
  , ("Headline with priority",  "** TODO [#A] Hello")
  , ("Headline with tags",      "* Hello :tag1:tag2:")
  , ("Full headline",           "** TODO [#B] My task :work:urgent:")
  , ("Deep indent",             "**** Deep headline")
  , ("Pragma category",         "#+CATEGORY: mycat")
  , ("Pragma TODO",             "#+TODO: TODO STARTED | DONE CANCELLED")
  , ("Generic pragma",          "#+TITLE: My Document")
  , ("Active timestamp",        "<2024-01-15 Mon 10:30>")
  , ("Inactive timestamp",      "[2024-06-01 Sat 09:00]")
  , ("Active midnight",         "<2024-01-01 Mon 00:00>")
  , ("Inactive midnight",       "[2024-01-01 Mon 00:00]")
  , ("Date-only timestamp",     "<2026-07-08 Wed>")
    -- Pins that an explicit "--" source never re-renders as the compact form.
  , ("Clock range",             "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]")
  , ("Date range",              "<2024-01-15 Mon>--<2024-01-19 Fri>")
  , ("Compact range",           "<2024-01-15 Mon 10:30-11:30>")
  , ("Compact range inactive",  "[2021-11-09 Tue 17:30-18:30]")
  , ("Compact range repeating", "<2024-01-15 Mon 10:30-11:30 +1w>")
  , ("Compact range seconds",   "<2024-01-15 Mon 10:30:15-11:45:30>")
  , ("Repeater and warning",    "<2024-01-15 Mon +1m -3d>")
  , ("First-only delay",        "[2024-01-15 Mon .+2d --7d]")
  , ("Warning cookie alone",    "<2024-01-15 Mon -3d>")
  , ("Date-only in a title",    "* Due <2026-07-08 Wed>")
  ]

spec :: TestTree
spec = testGroup "Roundtrip" (map check cases)

-- | Both parses must succeed and yield elements, or the comparison is vacuous.
check :: (String, Text) -> TestTree
check (desc, input) = testCase desc $ do
  let (elems, _ctx, err) = orgParse defaultContext input
      rendered = T.intercalate " " (map TS.showt elems)
      (elems2, _ctx2, err2) = orgParse defaultContext rendered
  assertBool ("parse error on " <> show input) (isNothing err)
  assertBool ("no elements parsed from " <> show input) (not (null elems))
  assertBool ("parse error re-parsing " <> show rendered) (isNothing err2)
  assertEqual ("unstable: " <> show input <> " -> " <> show rendered)
              (bare elems) (bare elems2)
  assertEqual ("render of " <> show input) input rendered
