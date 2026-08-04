module TestTextShow (spec) where

import Data.Org
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestDefaults (at, on, plainTs, titled)
import TextShow (showt)

spec :: TestTree
spec = testGroup "TextShow"
  [ testGroup "Headline rendering"
    [ testCase "Minimal headline" $
        assertEqual "" "* "
          (showt $ defaultHeadline { title = Title [] })

    , testCase "Headline with TODO" $
        assertEqual "" "* TODO Hello"
          (showt $ (titled "Hello") { todo = Just (Todo "TODO" True) })

    , testCase "Headline with priority" $
        assertEqual "" "* TODO [#A] Hello"
          (showt $ (titled "Hello") { todo = Just (Todo "TODO" True)
                                    , priority = Just (Priority 'A') })

    , testCase "Headline with tags" $
        assertEqual "" "* Hello :a:b:"
          (showt $ (titled "Hello") { tags = Tags ["a", "b"] })

    , testCase "Deep indent" $
        assertEqual "" "*** Hello"
          (showt $ (titled "Hello") { indent = Indent 3 })
    ]

  , testGroup "Pragma rendering"
    [ testCase "Category pragma" $
        assertEqual "" "#+CATEGORY: mycat"
          (showt $ Pragma (Keyword "CATEGORY") (OrgLine [OrgLineToken (Token "mycat")]))

    , testCase "TODO pragma" $
        assertEqual "" "#+TODO: TODO | DONE"
          (showt $ PTodo ["TODO"] ["DONE"])

      -- The cycle re-emits as the line SPELLS it.  It used to re-emit in Set
      -- order, which put WAITING before the TODO it follows and CANCELLED
      -- before the DONE it follows -- one of this re-serializer's documented
      -- losses, and the one that stopped being one when the keyword lists
      -- became ordered.
    , testCase "and its keywords keep the order they were declared in" $
        assertEqual "" "#+TODO: TODO WAITING | DONE CANCELLED"
          (showt $ PTodo ["TODO", "WAITING"] ["DONE", "CANCELLED"])

    , testCase "Generic pragma" $
        assertEqual "" "#+TITLE: My Doc"
          (showt $ Pragma (Keyword "TITLE") (OrgLine [OrgLineToken (Token "My"), OrgLineToken (Token "Doc")]))
    ]

  , testGroup "Timestamp rendering"
    [ testCase "Active timestamp" $
        assertEqual "" "<2024-01-01 Mon 00:00>"
          (showt $ plainTs TimestampActive (at "2024-01-01 00:00:00"))

    , testCase "Inactive timestamp" $
        assertEqual "" "[2024-06-15 Sat 00:00]"
          (showt $ plainTs TimestampInactive (at "2024-06-15 00:00:00"))

    , testCase "Date-only timestamp" $
        assertEqual "" "<2024-01-01 Mon>"
          (showt $ plainTs TimestampActive (on "2024-01-01 00:00:00"))

    , testCase "Timestamp range" $
        assertEqual "" "[2024-01-01 Mon 09:00]--[2024-01-01 Mon 17:30]"
          (showt $ (plainTs TimestampInactive (at "2024-01-01 09:00:00"))
                     { tsEnd = Just (at "2024-01-01 17:30:00") })

    , testCase "Timestamp with repeater" $
        assertEqual "" "<2024-01-01 Mon 00:00 +1w>"
          (showt $ (plainTs TimestampActive (at "2024-01-01 00:00:00"))
                     { tsInterval = Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus) })

    , testCase "Timestamp with cumulative repeater" $
        assertEqual "" "<2024-01-01 Mon 00:00 .+3d>"
          (showt $ (plainTs TimestampActive (at "2024-01-01 00:00:00"))
                     { tsInterval = Just (TimestampRepeaterInterval Cumulative 3 Days TRSPlus) })
    ]

  , testGroup "Component rendering"
    [ testCase "Token" $
        assertEqual "" "hello" (showt $ Token "hello")

    , testCase "Keyword" $
        assertEqual "" "TODO" (showt $ Keyword "TODO")

    , testCase "Indent" $
        assertEqual "" "***" (showt $ Indent 3)

    , testCase "Priority" $
        assertEqual "" "[#A]" (showt $ Priority 'A')

    , testCase "Tags" $
        assertEqual "" ":work:urgent:" (showt $ Tags ["work", "urgent"])

    , testCase "Empty tags" $
        assertEqual "" "" (showt $ Tags [])

    , testCase "OrgLine with spaces" $
        assertEqual "" "hello world" (showt $ OrgLine [OrgLineToken (Token "hello"), OrgLineToken (Token "world")])

    , testCase "Property" $
        assertEqual "" ":KEY: value" (showt $ Property (Keyword "KEY") (OrgLine [OrgLineToken (Token "value")]))
    ]
  ]
