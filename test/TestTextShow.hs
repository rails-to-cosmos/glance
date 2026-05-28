module TestTextShow (spec) where

import Data.Org
import Data.Text (Text)
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TextShow (showt)

spec :: TestTree
spec = testGroup "TextShow"
  [ testGroup "Headline rendering"
    [ testCase "Minimal headline" $
        assertEqual "" "* "
          (showt $ defaultHeadline { title = Title [] })

    , testCase "Headline with TODO" $
        assertEqual "" "* TODO Hello"
          (showt $ defaultHeadline { todo = Just (Todo "TODO" True)
                                   , title = Title [OrgLineToken (Token "Hello")] })

    , testCase "Headline with priority" $
        assertEqual "" "* TODO [#A] Hello"
          (showt $ defaultHeadline { todo = Just (Todo "TODO" True)
                                   , priority = Just (Priority 'A')
                                   , title = Title [OrgLineToken (Token "Hello")] })

    , testCase "Headline with tags" $
        assertEqual "" "* Hello :a:b:"
          (showt $ defaultHeadline { title = Title [OrgLineToken (Token "Hello")]
                                   , tags = Tags ["a", "b"] })

    , testCase "Deep indent" $
        assertEqual "" "*** Hello"
          (showt $ defaultHeadline { indent = Indent 3
                                   , title = Title [OrgLineToken (Token "Hello")] })
    ]

  , testGroup "Pragma rendering"
    [ testCase "Category pragma" $
        assertEqual "" "#+CATEGORY: mycat"
          (showt $ PCategory (OrgLine [OrgLineToken (Token "mycat")]))

    , testCase "TODO pragma" $
        assertEqual "" "#+TODO: TODO | DONE"
          (showt $ PTodo (Set.fromList ["TODO"]) (Set.fromList ["DONE"]))

    , testCase "Generic pragma" $
        assertEqual "" "#+TITLE: My Doc"
          (showt $ Pragma (Keyword "TITLE") (OrgLine [OrgLineToken (Token "My"), OrgLineToken (Token "Doc")]))
    ]

  , testGroup "Timestamp rendering"
    [ testCase "Active timestamp" $
        assertEqual "" "<2024-01-01 Mon 00:00>"
          (showt $ Timestamp TimestampActive Nothing (read "2024-01-01 00:00:00 UTC"))

    , testCase "Inactive timestamp" $
        assertEqual "" "[2024-06-15 Sat 00:00]"
          (showt $ Timestamp TimestampInactive Nothing (read "2024-06-15 00:00:00 UTC"))

    , testCase "Timestamp with repeater" $
        assertEqual "" "<2024-01-01 Mon 00:00 +1w>"
          (showt $ Timestamp TimestampActive
                    (Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus))
                    (read "2024-01-01 00:00:00 UTC"))

    , testCase "Timestamp with cumulative repeater" $
        assertEqual "" "<2024-01-01 Mon 00:00 .+3d>"
          (showt $ Timestamp TimestampActive
                    (Just (TimestampRepeaterInterval Cumulative 3 Days TRSPlus))
                    (read "2024-01-01 00:00:00 UTC"))
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
