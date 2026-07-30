module TestParser (spec) where

import Data.Maybe (isNothing)
import Data.Org
import qualified Data.Set as Set
import Data.Text (Text, intercalate)
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
      [ EPragma (PTodo (Set.fromList ["TODO"]) (Set.fromList ["CANCELLED"]))
      , EHeadline (titled "Mess") { todo = Just Todo { name = "CANCELLED", active = False } } ]
      (initialState `withTodo` (["TODO"], ["DONE", "CANCELLED"]))

  , ending "TODO pragma (active only)" ["#+TODO: foo"]
      [ EPragma (PTodo (Set.fromList ["foo"]) Set.empty) ]
      (initialState `withTodo` (["TODO", "foo"], ["DONE"]))

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
      [EHeadline (titled "Task") { schedule = Just day2024, deadline = Just jun2024 }]

  , plain "Planning: keywords in the reverse order"
      ["* Task", "DEADLINE: <2024-06-01 Sat> SCHEDULED: <2024-01-01 Mon>"]
      [EHeadline (titled "Task") { schedule = Just day2024, deadline = Just jun2024 }]

  , plain "Planning: a repeated keyword keeps the last"
      ["* Task", "SCHEDULED: <2024-01-01 Mon> SCHEDULED: <2024-06-01 Sat>"]
      [EHeadline (titled "Task") { schedule = Just jun2024 }]

  , plain "Planning: range and repeater timestamps"
      ["* Task", "SCHEDULED: <2024-01-15 Mon>--<2024-01-19 Fri> DEADLINE: <2024-01-01 Mon +1w>"]
      [ EHeadline (titled "Task")
          { schedule = Just (plainTs TimestampActive (on "2024-01-15 00:00:00"))
                              { tsEnd = Just (on "2024-01-19 00:00:00") }
          , deadline = Just day2024
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

-- | 2024-01-01, date only.
day2024 :: Timestamp
day2024 = plainTs TimestampActive (on "2024-01-01 00:00:00")

-- | 2024-06-01, date only: a second date, to catch a misfiled planning entry.
jun2024 :: Timestamp
jun2024 = plainTs TimestampActive (on "2024-06-01 00:00:00")

spec :: TestTree
spec = testGroup "Parser" (map assert testCases)
  where assert tc = testCase (description tc) $ do
          let input = intercalate "\n" (inputs tc)
          case orgParse defaultContext input of
            (elems, ctx, err) -> do
              assertBool ("unexpected parse error in " <> show input) (isNothing err)
              assertEqual [] (expected tc) (Result (bare elems) ctx)
