module TestRoundtrip (spec) where

import Data.Org
import qualified Data.Org as Org
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import qualified TextShow as TS

roundtrip :: Text -> (Text, Bool)
roundtrip input =
  let (elems, _ctx, _err) = orgParse defaultContext input
      rendered = T.intercalate " " (map TS.showt elems)
      (elems2, _ctx2, _err2) = orgParse defaultContext rendered
      bare = map (stripSpans . valueOf)
  in (rendered, bare elems == bare elems2)

roundtripExact :: Text -> (Text, Bool)
roundtripExact input =
  let (elems, _ctx, _err) = orgParse defaultContext input
      rendered = T.intercalate " " (map TS.showt elems)
  in (rendered, rendered == input)

spec :: TestTree
spec = testGroup "Roundtrip"
  [ testGroup "parse . showt . parse == parse (stability)"
    [ stable "Single token"            "hello"
    , stable "Multiple tokens"         "hello world"
    , stable "Headline"                "* Hello"
    , stable "Headline with TODO"      "* TODO Hello"
    , stable "Headline with priority"  "** TODO [#A] Hello"
    , stable "Headline with tags"      "* Hello :tag1:tag2:"
    , stable "Full headline"           "** TODO [#B] My task :work:urgent:"
    , stable "Pragma category"         "#+CATEGORY: mycat"
    , stable "Pragma TODO"             "#+TODO: TODO STARTED | DONE CANCELLED"
    , stable "Generic pragma"          "#+TITLE: My Document"
    , stable "Active timestamp"        "<2024-01-15 Mon 10:30>"
    , stable "Inactive timestamp"      "[2024-06-01 Sat 09:00]"
    , stable "Timestamp no time"       "<2024-01-01 Mon 00:00>"
    , stable "Date-only timestamp"     "<2026-07-08 Wed>"
    , stable "Clock range"             "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"
    , stable "Date range"              "<2024-01-15 Mon>--<2024-01-19 Fri>"
    , stable "Deep indent"             "**** Deep headline"
    ]
  , testGroup "showt . parse == identity (exact roundtrip)"
    [ exact "Single token"             "hello"
    , exact "Headline simple"          "* Hello"
    , exact "Headline TODO"            "* TODO Hello"
    , exact "Headline priority"        "** TODO [#A] Hello"
    , exact "Headline tags"            "* Hello :tag1:tag2:"
    , exact "Full headline"            "** TODO [#B] My task :work:urgent:"
    , exact "Timestamp with time"      "<2024-01-15 Mon 10:30>"
    , exact "Date-only timestamp"      "<2026-07-08 Wed>"
    , exact "Clock range"              "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"
    , exact "Date-only in a title"     "* Due <2026-07-08 Wed>"
    ]
  ]

stable :: String -> Text -> TestTree
stable desc input = testCase desc $
  let (rendered, ok) = roundtrip input
  in assertBool ("Roundtrip unstable: " <> show input <> " -> " <> show rendered) ok

exact :: String -> Text -> TestTree
exact desc input = testCase desc $
  let (rendered, ok) = roundtripExact input
  in assertBool ("Expected: " <> show input <> "\n     Got: " <> show rendered) ok
