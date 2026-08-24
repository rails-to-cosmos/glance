module TestParser (spec) where

import Data.Maybe (isNothing)
import Data.Org
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults

data Result = Result { elements :: ![Element]
                     , context :: !Context
                     } deriving (Eq, Show)

data TestCase = TestCase { description :: !String
                         , inputs :: ![Text]
                         , expected :: !Result }

-- | A case whose parse leaves the context untouched.
plain :: String -> [Text] -> [Element] -> TestCase
plain desc ls es = TestCase desc ls (Result es initialState)

-- | A case whose parse ends in CTX.
ending :: String -> [Text] -> [Element] -> Context -> TestCase
ending desc ls es ctx = TestCase desc ls (Result es ctx)

testCases :: [TestCase]
testCases =
  [ plain "Headline" ["** TODO [#A] Hello :a:b:c:"]
      [ EHeadline defaultHeadline { indent = Indent 2
                                  , todo = Just Todo { name = "TODO", active = True }
                                  , priority = Just (Priority 'A')
                                  , title = Title [OrgLineToken "Hello"]
                                  , tags = Tags ["a", "b", "c"] } ]

  , plain "Corrupted tags" ["* Hello world :a:b:c"]
      [ EHeadline defaultHeadline { title = Title [ OrgLineToken "Hello"
                                                  , OrgLineToken "world"
                                                  , OrgLineToken ":a:b:c" ] } ]

  , plain "Property block" ["* Hello", ":PROPERTIES:", ":TITLE: New title", ":END:"]
      [ EHeadline (titled "Hello")
          { properties = Properties [ Property (Keyword "TITLE")
                                               (OrgLine [OrgLineToken "New", OrgLineToken "title"]) ] } ]

  , plain "Drawer" [":DRAWER:"] [EToken ":DRAWER:"]

  , ending "Category pragma" ["#+CATEGORY: foo bar"]
      [ EPragma (Pragma (Keyword "CATEGORY") (OrgLine [OrgLineToken "foo", OrgLineToken "bar"])) ]
      (initialState `withCategory` "foo bar")

  , ending "Category property" ["* Hello", ":PROPERTIES:", ":CATEGORY: Updated category", ":END:"]
      [ EHeadline (titled "Hello")
          { properties = Properties [ Property (Keyword "CATEGORY")
                                               (OrgLine [OrgLineToken "Updated", OrgLineToken "category"]) ] } ]
      (initialState `withCategory` "Updated category")

  , ending "TODO pragma" ["#+TODO: TODO | CANCELLED", "* CANCELLED Mess"]
      [ EPragma (PTodo ["TODO"] ["CANCELLED"])
      , EHeadline (titled "Mess") { todo = Just Todo { name = "CANCELLED", active = False } } ]
      (initialState `withTodo` (["TODO"], ["DONE", "CANCELLED"]))

  , ending "TODO pragma (active only)" ["#+TODO: foo"]
      [ EPragma (PTodo ["foo"] []) ]
      (initialState `withTodo` (["TODO", "foo"], ["DONE"]))

    -- org's two older spellings of the same line (test-org/test-org-todo):
    -- both configure the cycle exactly as #+TODO: does.
  , ending "SEQ_TODO pragma is the TODO line" ["#+SEQ_TODO: NEXT | SHIPPED", "* NEXT Foo"]
      [ EPragma (PTodo ["NEXT"] ["SHIPPED"])
      , EHeadline (titled "Foo") { todo = Just Todo { name = "NEXT", active = True } } ]
      (initialState `withTodo` (["TODO", "NEXT"], ["DONE", "SHIPPED"]))

  , ending "TYP_TODO pragma is the TODO line" ["#+TYP_TODO: Fred | DONE"]
      [ EPragma (PTodo ["Fred"] ["DONE"]) ]
      (initialState `withTodo` (["TODO", "Fred"], ["DONE"]))

    -- A warning cookie must not fail the timestamp: the LINE would demote to
    -- body and take the drawer behind it out of the headline.
  , plain "Planning line survives a warning cookie"
      [ "* Task", "SCHEDULED: <2024-01-01 Mon +1m -3d>"
      , ":PROPERTIES:", ":MARKER: kept", ":END:" ]
      [ EHeadline (titled "Task")
          { schedule = Just (plainTs TimestampActive (on "2024-01-01 00:00:00"))
              { tsInterval = Just (TimestampRepeaterInterval Restart 1 Months TRSPlus)
              , tsWarning  = Just (TimestampWarningInterval False 3 Days) }
          , properties = Properties [ Property (Keyword "MARKER")
                                               (OrgLine [OrgLineToken "kept"]) ] } ]

    -- org-tag-re carries `%' (test-org/tags): the run parses whole.
  , plain "Percent in a tag" ["* Hello :50%:done:"]
      [ EHeadline (titled "Hello") { tags = Tags ["50%", "done"] } ]

  , plain "Multiline" ["* foo", "* bar"]
      [EHeadline (titled "foo"), EHeadline (titled "bar")]

  , plain "Empty text" [""] []

  , plain "Timestamp" ["<2024-01-01>", "<2024-01-01 Mon>"]
      [ETimestamp day2024, ETimestamp day2024]

  , plain "Timestamp range" ["[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"]
      [ ETimestamp (plainTs TimestampInactive (at "2023-07-15 15:54:00"))
                     { tsEnd = Just (at "2023-07-15 17:10:00") } ]

  , plain "Planning: SCHEDULED" ["* Task", "SCHEDULED: <2024-01-01 Mon>"]
      [EHeadline (titled "Task") { schedule = Just day2024 }]

  , plain "Planning: DEADLINE" ["* Task", "DEADLINE: <2024-01-01 Mon>"]
      [EHeadline (titled "Task") { deadline = Just day2024 }]

  , plain "Planning: CLOSED" ["* DONE Task", "CLOSED: [2024-01-01 Mon 10:30]"]
      [ EHeadline (titled "Task")
          { todo = Just Todo { name = "DONE", active = False }
          , closed = Just (plainTs TimestampInactive (at "2024-01-01 10:30:00")) } ]

  , plain "Planning: two keywords on one line"
      ["* Task", "SCHEDULED: <2024-01-01 Mon> DEADLINE: <2024-06-01 Sat>"]
      [EHeadline (titled "Task") { schedule = Just day2024
                                 , deadline = Just (jun2024 TimestampActive) }]

  , plain "Planning: keywords in the reverse order"
      ["* Task", "DEADLINE: <2024-06-01 Sat> SCHEDULED: <2024-01-01 Mon>"]
      [EHeadline (titled "Task") { schedule = Just day2024
                                 , deadline = Just (jun2024 TimestampActive) }]

  , plain "Planning: a repeated keyword keeps the last"
      ["* Task", "SCHEDULED: <2024-01-01 Mon> SCHEDULED: <2024-06-01 Sat>"]
      [EHeadline (titled "Task") { schedule = Just (jun2024 TimestampActive) }]

  , plain "Planning: range and repeater timestamps"
      ["* Task", "SCHEDULED: <2024-01-15 Mon>--<2024-01-19 Fri> DEADLINE: <2024-01-01 Mon +1w>"]
      [ EHeadline (titled "Task")
          { schedule = Just (plainTs TimestampActive (on "2024-01-15 00:00:00"))
                              { tsEnd = Just (on "2024-01-19 00:00:00") }
          , deadline = Just day2024
                              { tsInterval = Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus) } } ]

  , plain "Planning: a compact same-day range"
      ["* Task", "SCHEDULED: <2024-01-15 Mon 10:30-11:30 +1w>"]
      [ EHeadline (titled "Task")
          { schedule = Just (compactTs TimestampActive (at "2024-01-15 10:30:00")
                                                       (at "2024-01-15 11:30:00"))
                              { tsInterval = Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus) } } ]

  , plain "Planning: an indented line still attaches"
      ["* Task", "  SCHEDULED: <2024-01-01 Mon>"]
      [EHeadline (titled "Task") { schedule = Just day2024 }]

  , plain "Planning: a drawer may follow the planning line"
      ["* Task", "SCHEDULED: <2024-01-01 Mon>", ":PROPERTIES:", ":K: v", ":END:"]
      [ EHeadline (titled "Task")
          { schedule = Just day2024
          , properties = Properties [Property (Keyword "K") (OrgLine [OrgLineToken "v"])] } ]

  , plain "Planning: a blank line detaches it" ["* Task", "", "SCHEDULED: <2024-01-01 Mon>"]
      [EHeadline (titled "Task"), EToken "SCHEDULED:", ETimestamp day2024]

  , plain "Planning: a later body line stays body"
      ["* Task", "note", "SCHEDULED: <2024-01-01 Mon>"]
      [ EHeadline (titled "Task"), EToken "note"
      , EToken "SCHEDULED:", ETimestamp day2024 ]

  , plain "Planning: lowercase keywords do not match"
      ["* Task", "scheduled: <2024-01-01 Mon>"]
      [EHeadline (titled "Task"), EToken "scheduled:", ETimestamp day2024]

  , plain "Planning: CLOCK is not a planning keyword"
      ["* Task", "CLOCK: [2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"]
      [ EHeadline (titled "Task"), EToken "CLOCK:"
      , ETimestamp (plainTs TimestampInactive (at "2023-07-15 15:54:00"))
                     { tsEnd = Just (at "2023-07-15 17:10:00") } ]

  , plain "Planning: text after the last timestamp stays body"
      ["* Task", "SCHEDULED: <2024-01-01 Mon> note"]
      [EHeadline (titled "Task") { schedule = Just day2024 }, EToken "note"]

  , plain "Single token" ["a"] [EToken "a"]
  , plain "Multiple tokens" ["a", "b"] [EToken "a", EToken "b"]
  , plain "Skip spaces" [" "] []
  , plain "Skip leading spaces" [" ", "a", "b"] [EToken "a", EToken "b"]
  , plain "Skip trailing spaces" ["a", "b", " "] [EToken "a", EToken "b"]
  , plain "Skip multiple spaces" ["a", " ", " ", "b"] [EToken "a", EToken "b"]
  ]

jan2024 :: TimestampStatus -> Timestamp
jan2024 status = plainTs status (on "2024-01-01 00:00:00")

day2024 :: Timestamp
day2024 = jan2024 TimestampActive

-- | A second date, to catch a misfiled planning entry.
jun2024 :: TimestampStatus -> Timestamp
jun2024 status = plainTs status junMoment

-- | The day 'jun2024' names, for the range that ENDS on it.
junMoment :: TsMoment
junMoment = on "2024-06-01 00:00:00"

-- | The follower truth table.  A timestamp closes on its OWN bracket, so prose
-- abuts it with nothing between and each follower is a token of its own; the
-- element loop used to demand whitespace there and failed the WHOLE file on the
-- character behind the bracket.  Both statuses, because the bug was reported on
-- an inactive one and the two share 'tsBodyParser'.
abuttedCases :: [TestCase]
abuttedCases =
  [ plain (T.unpack follower <> " abuts an " <> label <> " timestamp")
          ["note " <> open <> "2024-01-01 Mon" <> close <> follower <> " tail"]
          [ EToken "note", ETimestamp (jan2024 status)
          , EToken (Token follower), EToken "tail" ]
  | (label, status, open, close) <- [ ("active",   TimestampActive,   "<", ">")
                                    , ("inactive", TimestampInactive, "[", "]") ]
  , follower <- [".", ",", ")", ":", ";", "!", "?", "s", "]", ">", "*"]
  ]

-- | The rest of the table: where the follower sits, and the spellings a
-- follower must NOT swallow — the range's @--@ and a second bracket.
abuttedEdgeCases :: [TestCase]
abuttedEdgeCases =
  [ plain "A follower at the line's end"
      ["note [2024-01-01 Mon].", "tail"]
      [EToken "note", ETimestamp (jan2024 TimestampInactive), EToken ".", EToken "tail"]

  , plain "A follower at the end of the document"
      ["note <2024-01-01 Mon>."]
      [EToken "note", ETimestamp day2024, EToken "."]

    -- The abutting run is mid-line however the line opened, so the star is ink.
  , plain "A star behind a timestamp at column 1 opens no headline"
      ["[2024-01-01 Mon]* not a headline"]
      [ ETimestamp (jan2024 TimestampInactive), EToken "*"
      , EToken "not", EToken "a", EToken "headline" ]

  , plain "Two timestamps with nothing between them"
      ["[2024-01-01 Mon][2024-06-01 Sat]"]
      [ ETimestamp (jan2024 TimestampInactive)
      , ETimestamp (jun2024 TimestampInactive) ]

    -- A `--' is read as the range's only when a second timestamp opens behind
    -- it; otherwise it is the follower, and the timestamp stands alone.
  , plain "A `--' that opens no timestamp is a token"
      ["[2024-01-01 Mon]--and on"]
      [ETimestamp (jan2024 TimestampInactive), EToken "--and", EToken "on"]

  , plain "A range keeps its `--' and takes a follower"
      ["[2024-01-01 Mon]--[2024-06-01 Sat]. done"]
      [ ETimestamp (jan2024 TimestampInactive)
          { tsEnd = Just junMoment }
      , EToken ".", EToken "done" ]

    -- The line the bug was filed on, shortened: an org-glance registry blob's
    -- body sentence.  The headline behind it is what the whole-file failure hid.
  , plain "Body prose behind a timestamp keeps the headline"
      ["* Registry", "Research [2024-01-01 Mon]. Use case: music."]
      [ EHeadline (titled "Registry"), EToken "Research"
      , ETimestamp (jan2024 TimestampInactive), EToken "."
      , EToken "Use", EToken "case:", EToken "music." ]
  ]

spec :: TestTree
spec = testGroup "Parser" $
  map assert testCases
  <> [testGroup "A timestamp is abutted" (map assert (abuttedCases <> abuttedEdgeCases))]
  where assert tc = testCase (description tc) $ do
          let input = intercalate "\n" (inputs tc)
          case orgParse defaultContext input of
            (elems, ctx, err) -> do
              assertBool ("unexpected parse error in " <> show input) (isNothing err)
              assertEqual [] (expected tc) (Result (bare elems) ctx)
