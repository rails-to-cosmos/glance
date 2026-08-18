module TestTextShow (spec) where

import Data.Org
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestDefaults (at, on, plainTs, titled)
import TextShow (showt)

spec :: TestTree
spec = testGroup "TextShow"
  [ testGroup group [ testCase name (assertEqual "" want got) | (name, got, want) <- cases ]
  | (group, cases) <- showCases ]

-- | Group by group: the case name, what 'showt' made of the value, and the text
-- it has to be.  Rendering at the element site is what lets headlines, pragmas,
-- timestamps and components share one list.
showCases :: [(String, [(String, Text, Text)])]
showCases =
  [ ( "Headline rendering"
    , [ ("Minimal headline", showt (defaultHeadline { title = Title [] }), "* ")
      , ( "Headline with TODO"
        , showt ((titled "Hello") { todo = Just (Todo "TODO" True) }), "* TODO Hello" )
      , ( "Headline with priority"
        , showt ((titled "Hello") { todo = Just (Todo "TODO" True)
                                  , priority = Just (Priority 'A') })
        , "* TODO [#A] Hello" )
      , ( "Headline with tags"
        , showt ((titled "Hello") { tags = Tags ["a", "b"] }), "* Hello :a:b:" )
      , ("Deep indent", showt ((titled "Hello") { indent = Indent 3 }), "*** Hello")
      ] )

  , ( "Pragma rendering"
    , [ ( "Category pragma"
        , showt (Pragma (Keyword "CATEGORY") (OrgLine [OrgLineToken (Token "mycat")]))
        , "#+CATEGORY: mycat" )
      , ("TODO pragma", showt (PTodo ["TODO"] ["DONE"]), "#+TODO: TODO | DONE")
        -- The cycle re-emits as the line SPELLS it, never in the keyword Set's order.
      , ( "and its keywords keep the order they were declared in"
        , showt (PTodo ["TODO", "WAITING"] ["DONE", "CANCELLED"])
        , "#+TODO: TODO WAITING | DONE CANCELLED" )
      , ( "Generic pragma"
        , showt (Pragma (Keyword "TITLE")
                        (OrgLine [OrgLineToken (Token "My"), OrgLineToken (Token "Doc")]))
        , "#+TITLE: My Doc" )
      ] )

  , ( "Timestamp rendering"
    , [ ( "Active timestamp"
        , showt (plainTs TimestampActive (at "2024-01-01 00:00:00"))
        , "<2024-01-01 Mon 00:00>" )
      , ( "Inactive timestamp"
        , showt (plainTs TimestampInactive (at "2024-06-15 00:00:00"))
        , "[2024-06-15 Sat 00:00]" )
      , ( "Date-only timestamp"
        , showt (plainTs TimestampActive (on "2024-01-01 00:00:00"))
        , "<2024-01-01 Mon>" )
      , ( "Timestamp range"
        , showt ((plainTs TimestampInactive (at "2024-01-01 09:00:00"))
                   { tsEnd = Just (at "2024-01-01 17:30:00") })
        , "[2024-01-01 Mon 09:00]--[2024-01-01 Mon 17:30]" )
      , ( "Timestamp with repeater"
        , showt ((plainTs TimestampActive (at "2024-01-01 00:00:00"))
                   { tsInterval = Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus) })
        , "<2024-01-01 Mon 00:00 +1w>" )
      , ( "Timestamp with cumulative repeater"
        , showt ((plainTs TimestampActive (at "2024-01-01 00:00:00"))
                   { tsInterval = Just (TimestampRepeaterInterval Cumulative 3 Days TRSPlus) })
        , "<2024-01-01 Mon 00:00 .+3d>" )
      ] )

  , ( "Component rendering"
    , [ ("Token", showt (Token "hello"), "hello")
      , ("Keyword", showt (Keyword "TODO"), "TODO")
      , ("Indent", showt (Indent 3), "***")
      , ("Priority", showt (Priority 'A'), "[#A]")
      , ("Tags", showt (Tags ["work", "urgent"]), ":work:urgent:")
      , ("Empty tags", showt (Tags []), "")
      , ( "OrgLine with spaces"
        , showt (OrgLine [OrgLineToken (Token "hello"), OrgLineToken (Token "world")])
        , "hello world" )
      , ( "Property"
        , showt (Property (Keyword "KEY") (OrgLine [OrgLineToken (Token "value")]))
        , ":KEY: value" )
      ] )
  ]
