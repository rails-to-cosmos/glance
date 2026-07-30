module TestSpans (spec) where

import Data.Maybe (isJust)
import Data.Org
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (bareParse, headlinesOf, titled)
import qualified TextShow as TS

-- Test inputs

-- | A document and the label its assertions carry.
data Case = Case { caseName :: !String, caseInput :: !Text }

mixedDocument :: Text
mixedDocument = T.intercalate "\n"
  [ "#+TODO: NEXT | SKIP"
  , "* NEXT Task :x:"
  , "SCHEDULED: <2024-01-15 Mon 10:30>"
  , "some body text"
  , "[2024-06-01 Sat 09:00]"
  ]

clockDocument :: Text
clockDocument = T.intercalate "\n"
  [ "* Task"
  , "CLOCK: [2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10] =>  1:16"
  , "SCHEDULED: <2024-01-15 Mon>--<2024-01-19 Fri>"
  ]

-- | A planning line whose keywords run opposite to the record's field order,
-- so the span table has to sort them back into source order.
planningDocument :: Text
planningDocument = T.intercalate "\n"
  [ "* TODO Task :x:"
  , "CLOSED: [2024-01-02 Tue 10:00] DEADLINE: <2024-01-03 Wed> SCHEDULED: <2024-01-01 Mon>"
  ]

planningDrawerDocument :: Text
planningDrawerDocument = T.intercalate "\n"
  [ "* Task"
  , "DEADLINE: <2024-02-01 Thu>"
  , ":PROPERTIES:"
  , ":K: v"
  , ":END:"
  ]

cases :: [Case]
cases =
  [ Case "todo, priority, title and tags"   "** TODO [#A] Hello :a:b:c:"
  , Case "title only"                       "* Hello"
  , Case "todo without title"               "* TODO"
  , Case "stars only"                       "* "
  , Case "corrupted tags"                   "* Hello world :a:b:c"
  , Case "inactive todo"                    "* DONE [#B] Fix the bug :work:urgent:"
  , Case "property drawer"                  "* Hello\n:PROPERTIES:\n:TITLE: New title\n:END:"
  , Case "unicode title and tags"           "** TODO Привет мир :тег:"
  , Case "headline after a pragma"          "#+CATEGORY: cat\n* Task"
  , Case "two headlines"                    "* one\n** TODO two :t:"
  , Case "custom todo keyword"              "#+TODO: TODO | CANCELLED\n* CANCELLED Mess"
  , Case "mixed document"                   mixedDocument
  , Case "clock and scheduled ranges"       clockDocument
  , Case "planning keywords out of order"   planningDocument
  , Case "planning line before a drawer"    planningDrawerDocument
  , Case "planning with a stale weekday"    "* Task\nSCHEDULED: <2024-01-15 Fri>"
  , Case "planning with a compact range"    "* Task\nSCHEDULED: <2024-01-15 Mon 10:30-11:30 +1w>"
  , Case "date-only timestamp in a title"   "* Due <2026-07-08 Wed>"
  , Case "trailing spaces before a newline" "* Hello  \n* Two"
  , Case "trailing tab at eof"              "* Hello\t"
  , Case "pragma with trailing spaces"      "#+CATEGORY: cat  "
  , Case "drawer with trailing spaces"      "* H\n:PROPERTIES:\n:K: v  \n:END:"
  , Case "indented drawer"                  "* H\n  :PROPERTIES:\n  :K: v  \n  :END:  "
  ]

-- Helpers

-- | Run K over the elements and final context of CASE, failing on a parse error.
onDoc :: Case -> (Text -> Context -> [Spanned Element] -> Assertion) -> TestTree
onDoc c k = testCase (caseName c) $ case orgParse defaultContext (caseInput c) of
  (elems, ctx, Nothing) -> k (caseInput c) ctx elems
  (_, _, Just _err)     -> assertFailure ("parse error in " <> show (caseInput c))

-- | Sub-spans a headline actually carries, in source order.
presentSpans :: Headline -> [(String, Span)]
presentSpans h = [(T.unpack label, s) | (label, Just s, _ok) <- headlineSpanParts h]

propertyKeys :: Headline -> [Text]
propertyKeys h = case properties h of
  Properties ps -> [k | Property (Keyword k) _ <- ps]

-- | H's planning components: label, span, and the timestamp stored under it.
planningParts :: Headline -> [(Text, Maybe Span, Maybe Timestamp)]
planningParts h = [ ("hsSchedule", hsSchedule hs, schedule h)
                  , ("hsDeadline", hsDeadline hs, deadline h)
                  , ("hsClosed",   hsClosed hs,   closed h) ]
  where hs = spans h

-- | Describe LABEL of headline H inside INPUT for an assertion message.
about :: Text -> Headline -> String -> String
about input h label =
  label <> " of " <> show (TS.showt h) <> " in " <> show input

-- | Parse SLICE in CTX, expecting exactly EXPECTED modulo spans.
assertReparse :: String -> Context -> Element -> Text -> Assertion
assertReparse label ctx expected slice = case orgParse ctx slice of
  (_, _, Just _err) -> assertFailure (label <> ": parse error reparsing " <> show slice)
  (elems, _, Nothing) ->
    assertEqual (label <> ": reparse of " <> show slice)
                [stripSpans expected]
                (map (stripSpans . valueOf) elems)

-- Assertions

-- | Every present sub-span slices back to the component it stands for, and
-- every component the headline carries has a sub-span: a dropped one would
-- leave the slice assertions with nothing to check.  Planning spans get the
-- exact check 'headlineSpanParts' cannot state, having no parser to hand: the
-- slice re-parses to the very timestamp the headline stores.
assertSlices :: Text -> Headline -> Assertion
assertSlices input h = do
  sequence_ [ assertBool (say (T.unpack label <> " sliced " <> show slice)) (ok slice)
            | (label, Just s, ok) <- headlineSpanParts h
            , let slice = sliceSpan input s ]
  sequence_ [ assertBool (say (T.unpack label <> " is missing")) (isJust s)
            | (label, s, _ok) <- headlineSpanParts h
            , carried label ]
  sequence_ [ assertEqual (say (T.unpack label <> " reparse"))
                          [ETimestamp ts] (bareParse defaultContext (sliceSpan input s))
            | (label, Just s, Just ts) <- planningParts h ]
  maybe (pure ()) assertKeys (hsProperties (spans h))
  where say = about input h
        carried "hsTodo"       = isJust (todo h)
        carried "hsPriority"   = isJust (priority h)
        carried "hsTitle"      = title h /= Title []
        carried "hsTags"       = tags h /= Tags []
        carried "hsSchedule"   = isJust (schedule h)
        carried "hsDeadline"   = isJust (deadline h)
        carried "hsClosed"     = isJust (closed h)
        carried "hsProperties" = properties h /= mempty
        carried _              = False
        assertKeys s = mapM_ (assertKey (sliceSpan input s)) (propertyKeys h)
        assertKey slice k =
          assertBool (say ("hsProperties covers key " <> show k))
                     (T.toUpper k `T.isInfixOf` T.toUpper slice)

-- | 'hsFull' slices back to a headline equal to the one it came from.
assertFullReparse :: Text -> Context -> Headline -> Assertion
assertFullReparse input ctx h =
  assertReparse (about input h "hsFull") ctx (EHeadline h) (sliceSpan input (hsFull (spans h)))

-- | An element's own span slices back to that element.
assertElementReparse :: Text -> Context -> Spanned Element -> Assertion
assertElementReparse input ctx e =
  assertReparse label ctx (valueOf e) (sliceSpan input (spanOf e))
  where label = "element span in " <> show input

-- | Spans stay inside the input, nest inside 'hsFull', and never overlap.
assertInvariants :: Text -> Spanned Element -> Assertion
assertInvariants input e = do
  assertWellFormed "element span" (spanOf e)
  case valueOf e of
    EHeadline h -> do
      let hs = spans h
          full = hsFull hs
          inFull (label, s) = do
            assertBool (about input h (label <> " starts inside hsFull"))
                       (spanStart s >= spanStart full)
            assertBool (about input h (label <> " ends inside hsFull"))
                       (spanEnd s <= spanEnd full)
      assertWellFormed (about input h "hsFull") full
      mapM_ (\(label, s) -> assertWellFormed (about input h label) s) (presentSpans h)
      mapM_ inFull (presentSpans h)
      maybe (pure ()) (assertPropertiesPlacement input h) (hsProperties hs)
      assertOrdered (about input h "sub-spans") (presentSpans h)
    _element -> pure ()
  where
    assertWellFormed label s = case spanFaults (T.length input) s of
      []     -> pure ()
      faults -> assertFailure (label <> ": " <> show faults)

-- | The drawer closes the headline and follows the title.
assertPropertiesPlacement :: Text -> Headline -> Span -> Assertion
assertPropertiesPlacement input h props = do
  assertBool (say "hsProperties ends where hsFull ends")
             (spanEnd props == spanEnd (hsFull (spans h)))
  maybe (pure ()) titleFirst (hsTitle (spans h))
  where say = about input h
        titleFirst t = assertBool (say "hsProperties starts after hsTitle")
                                  (spanStart props > spanEnd t)

-- | Consecutive sub-spans must not overlap.
assertOrdered :: String -> [(String, Span)] -> Assertion
assertOrdered label parts = mapM_ check (zip parts (drop 1 parts))
  where check ((na, a), (nb, b)) =
          assertBool (label <> ": " <> na <> " must end before " <> nb <> " starts")
                     (spanEnd a <= spanStart b)

-- Spec

spec :: TestTree
spec = testGroup "Spans"
  [ testGroup "Exact slices"
    [ onDoc c (\input _ctx elems -> mapM_ (assertSlices input) (headlinesOf elems))
    | c <- cases ]

  , testGroup "Headline sub-spans" [customTodoSpan, starsOnlySpans]

  , testGroup "Timestamp ranges" [clockRangeSpans]

  , trailingWhitespaceSpec

  , testGroup "Full-span reparse"
    [ onDoc c (\input ctx elems -> mapM_ (assertFullReparse input ctx) (headlinesOf elems))
    | c <- cases ]

  , testGroup "Element-span reparse"
    [ onDoc c (\input ctx elems -> mapM_ (assertElementReparse input ctx) elems)
    | c <- cases ]

  , testGroup "Invariants"
    [ onDoc c (\input _ctx elems -> mapM_ (assertInvariants input) elems)
    | c <- cases ]
  ]

-- | Trailing horizontal space ends an element without derailing or widening it.
trailingWhitespaceSpec :: TestTree
trailingWhitespaceSpec = testGroup "Trailing whitespace"
  [ testCase "headline before a newline" $
      assertEqual "elements"
                  [EHeadline (titled "Hello"), EHeadline (titled "Two")]
                  (bareParse defaultContext "* Hello  \n* Two")

  , testCase "headline before eof" $
      assertEqual "elements" [EHeadline (titled "Hello")]
                  (bareParse defaultContext "* Hello\t")

  , testCase "pragma value stays tight" $ do
      let input = "#+CATEGORY: cat  "
      assertEqual "elements"
                  [EPragma (Pragma (Keyword "CATEGORY") (OrgLine [OrgLineToken "cat"]))]
                  (bareParse defaultContext input)
      case orgParse defaultContext input of
        (elems, ctx, _err) -> do
          assertEqual "category" "cat" (metaCategory ctx)
          assertEqual "element slices" ["#+CATEGORY: cat"]
                      (map (sliceSpan input . spanOf) elems)

  , testCase "property drawer" $
      assertEqual "elements"
                  [EHeadline (titled "H") { properties = drawer }]
                  (bareParse defaultContext "* H\n:PROPERTIES:\n:K: v  \n:END:")

  , testCase "indented property drawer" $
      assertEqual "elements"
                  [EHeadline (titled "H") { properties = drawer }]
                  (bareParse defaultContext "* H\n  :PROPERTIES:\n  :K: v  \n  :END:  ")

  , testCase "spans stop before the trailing space" $ do
      let input = "* Hello  \n* Two"
      case orgParse defaultContext input of
        (elems, _ctx, _err) -> do
          assertEqual "element slices" ["* Hello", "* Two"]
                      (map (sliceSpan input . spanOf) elems)
          assertEqual "hsFull slices" ["* Hello", "* Two"]
                      (map (sliceSpan input . hsFull . spans) (headlinesOf elems))
  ]
  where drawer = Properties [Property (Keyword "K") (OrgLine [OrgLineToken "v"])]

-- | Each range is one element whose span covers both halves and reparses.
clockRangeSpans :: TestTree
clockRangeSpans = onDoc (Case "ranges span both halves" clockDocument) check
  where
    check doc _ctx elems = case ranges elems of
      [inactive, active] -> do
        assertEqual "clock range slice"
                    "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"
                    (sliceSpan doc (spanOf inactive))
        assertEqual "scheduled range slice"
                    "<2024-01-15 Mon>--<2024-01-19 Fri>"
                    (sliceSpan doc (spanOf active))
      es -> assertFailure ("expected two ranges, got " <> show (length es))
    ranges elems = [e | e <- elems, ETimestamp t <- [valueOf e], isJust (tsEnd t)]

-- | Stars alone carry no component: every sub-span is absent and 'hsFull'
-- covers the stars, not the space after them.
starsOnlySpans :: TestTree
starsOnlySpans = onDoc (Case "stars alone carry no sub-span" "* ") check
  where
    check doc _ctx elems = case headlinesOf elems of
      [h] -> do
        assertEqual "sub-spans" [] (presentSpans h)
        assertEqual "hsFull slice" "*" (sliceSpan doc (hsFull (spans h)))
      hs -> assertFailure ("expected one headline, got " <> show (length hs))

customTodoSpan :: TestTree
customTodoSpan = onDoc (Case "keyword declared by a #+TODO pragma" input) check
  where
    input = "#+TODO: TODO | CANCELLED\n* CANCELLED Mess"
    check doc _ctx elems = case headlinesOf elems of
      [h] -> case hsTodo (spans h) of
        Nothing -> assertFailure "expected hsTodo for the CANCELLED keyword"
        Just s  -> do
          assertEqual "hsTodo slice" "CANCELLED" (sliceSpan doc s)
          assertEqual "todo name" (Just "CANCELLED") (name <$> todo h)
      hs -> assertFailure ("expected one headline, got " <> show (length hs))
