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
--
-- The budget is EMPTY — every case here is 'Exact'.  The last 'Stable' row was
-- @#+TODO:@, whose two keyword sets re-emitted in Set order rather than as the
-- source wrote them; ordering the keyword lists took that loss away and the row
-- was measured and promoted.  'Stable' stays because the budget is a mechanism
-- rather than a tally: a case whose render is genuinely lossy belongs here
-- labelled, and asserting bytes it cannot hold is the failure this column
-- prevents.  Whitespace collapse and uppercased pragma keys are still real
-- losses, and no case in the table spells one.
data Fidelity = Stable | Exact

cases :: [(String, Text, Fidelity)]
cases =
  [ ("Single token",            "hello",                                          Exact)
  , ("Multiple tokens",         "hello world",                                    Exact)
  , ("Headline",                "* Hello",                                        Exact)
  , ("Headline with TODO",      "* TODO Hello",                                   Exact)
  , ("Headline with priority",  "** TODO [#A] Hello",                             Exact)
  , ("Headline with tags",      "* Hello :tag1:tag2:",                            Exact)
  , ("Full headline",           "** TODO [#B] My task :work:urgent:",             Exact)
  , ("Deep indent",             "**** Deep headline",                             Exact)
  , ("Pragma category",         "#+CATEGORY: mycat",                              Exact)
    -- The one row the budget is actually spent on: '#+TODO:' re-emits its two
    -- keyword sets in Set order, so a source that wrote them in any other comes
    -- back re-ordered.
  , ("Pragma TODO",             "#+TODO: TODO STARTED | DONE CANCELLED",          Exact)
  , ("Generic pragma",          "#+TITLE: My Document",                           Exact)
  , ("Active timestamp",        "<2024-01-15 Mon 10:30>",                         Exact)
  , ("Inactive timestamp",      "[2024-06-01 Sat 09:00]",                         Exact)
  , ("Active midnight",         "<2024-01-01 Mon 00:00>",                         Exact)
  , ("Inactive midnight",       "[2024-01-01 Mon 00:00]",                         Exact)
  , ("Date-only timestamp",     "<2026-07-08 Wed>",                               Exact)
    -- Both halves share a date, so this row is what pins that an explicit "--"
    -- source never re-renders as the compact form.
  , ("Clock range",             "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]", Exact)
  , ("Date range",              "<2024-01-15 Mon>--<2024-01-19 Fri>",             Exact)
  , ("Compact range",           "<2024-01-15 Mon 10:30-11:30>",                   Exact)
  , ("Compact range inactive",  "[2021-11-09 Tue 17:30-18:30]",                   Exact)
  , ("Compact range repeating", "<2024-01-15 Mon 10:30-11:30 +1w>",               Exact)
  , ("Compact range seconds",   "<2024-01-15 Mon 10:30:15-11:45:30>",             Exact)
    -- org's two-cookie stamps, in the conventional repeater-then-warning
    -- order that a re-render spells; the warning cookie alone rides the same
    -- render rule.
  , ("Repeater and warning",    "<2024-01-15 Mon +1m -3d>",                       Exact)
  , ("First-only delay",        "[2024-01-15 Mon .+2d --7d]",                     Exact)
  , ("Warning cookie alone",    "<2024-01-15 Mon -3d>",                           Exact)
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
