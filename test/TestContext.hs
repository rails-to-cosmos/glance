module TestContext (spec) where

import Data.Org
import qualified Data.Org as Org
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)

strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t) :: UTCTime

spec :: TestTree
spec = testGroup "Context"
  [ testGroup "IAS registration"
    [ testCase "Headline with ORG_GLANCE_ID is registered" $ do
        let input = T.intercalate "\n"
              [ "* My Headline"
              , ":PROPERTIES:"
              , ":ORG_GLANCE_ID: unique-id-123"
              , ":END:" ]
        let (_elems, ctx, _err) = orgParse mempty input
        let IAS m = ias ctx
        assertBool "Should have one registered headline" (Map.size m == 1)
        assertBool "Should be keyed by ORG_GLANCE_ID" (Map.member "unique-id-123" m)

    , testCase "Headline without ORG_GLANCE_ID is not registered" $ do
        let (_elems, ctx, _err) = orgParse mempty "* Anonymous Headline"
        let IAS m = ias ctx
        assertEqual "IAS should be empty" 0 (Map.size m)

    , testCase "Multiple headlines with IDs" $ do
        let input = T.intercalate "\n"
              [ "* First"
              , ":PROPERTIES:"
              , ":ORG_GLANCE_ID: id-1"
              , ":END:"
              , "* Second"
              , ":PROPERTIES:"
              , ":ORG_GLANCE_ID: id-2"
              , ":END:" ]
        let (_elems, ctx, _err) = orgParse mempty input
        let IAS m = ias ctx
        assertEqual "Should have two registered headlines" 2 (Map.size m)

    , testCase "Duplicate IDs use last-writer-wins" $ do
        let input = T.intercalate "\n"
              [ "* First version"
              , ":PROPERTIES:"
              , ":ORG_GLANCE_ID: same-id"
              , ":END:"
              , "* Second version"
              , ":PROPERTIES:"
              , ":ORG_GLANCE_ID: same-id"
              , ":END:" ]
        let (_elems, ctx, _err) = orgParse mempty input
        let IAS m = ias ctx
        assertEqual "Should have one entry" 1 (Map.size m)
        case Map.lookup "same-id" m of
          Just h -> assertEqual "Should keep latest" (Title [OrgLineToken (Token "Second"), OrgLineToken (Token "version")]) (title h)
          Nothing -> assertBool "Should find the headline" False
    ]

  , testGroup "Context accumulation across parses"
    [ testCase "Category persists across calls" $ do
        let (_, ctx1, _) = orgParse mempty "#+CATEGORY: first"
        let (_, ctx2, _) = orgParse ctx1 "* Hello"
        assertEqual "Category should persist" "first" (metaCategory ctx2)

    , testCase "TODO keywords persist across calls" $ do
        let (_, ctx1, _) = orgParse mempty "#+TODO: WAITING | CANCELLED"
        let (elems, _, _) = orgParse ctx1 "* WAITING Something"
        case elems of
          [EHeadline h] -> assertEqual "Should recognize WAITING as todo" (Just (Todo "WAITING" True)) (todo h)
          _ -> assertBool "Expected single headline" False

    , testCase "IAS accumulates across calls" $ do
        let input1 = T.intercalate "\n"
              [ "* First"
              , ":PROPERTIES:"
              , ":ORG_GLANCE_ID: id-1"
              , ":END:" ]
        let input2 = T.intercalate "\n"
              [ "* Second"
              , ":PROPERTIES:"
              , ":ORG_GLANCE_ID: id-2"
              , ":END:" ]
        let (_, ctx1, _) = orgParse mempty input1
        let (_, ctx2, _) = orgParse ctx1 input2
        let IAS m = ias ctx2
        assertEqual "Should have both headlines" 2 (Map.size m)
    ]

  , testGroup "resolveHeadline"
    [ testCase "Keeps headline with later schedule" $ do
        let h1 = defaultHeadline { title = Title [OrgLineToken (Token "Later")]
                                 , schedule = Just (Timestamp TimestampActive Nothing (strptime "2024-06-01 00:00:00")) }
        let h2 = defaultHeadline { title = Title [OrgLineToken (Token "Earlier")]
                                 , schedule = Just (Timestamp TimestampActive Nothing (strptime "2024-01-01 00:00:00")) }
        assertEqual "Should keep h1 (later schedule)" (title h1) (title (resolveHeadline h1 h2))

    , testCase "Keeps h2 when h1 has no schedule" $ do
        let h1 = defaultHeadline { title = Title [OrgLineToken (Token "No schedule")] }
        let h2 = defaultHeadline { title = Title [OrgLineToken (Token "Has schedule")]
                                 , schedule = Just (Timestamp TimestampActive Nothing (strptime "2024-01-01 00:00:00")) }
        assertEqual "Should keep h2" (title h2) (title (resolveHeadline h1 h2))

    , testCase "Keeps h2 when both have no schedule" $ do
        let h1 = defaultHeadline { title = Title [OrgLineToken (Token "First")] }
        let h2 = defaultHeadline { title = Title [OrgLineToken (Token "Second")] }
        assertEqual "Should keep h2" (title h2) (title (resolveHeadline h1 h2))
    ]

  , testGroup "TODO keyword management"
    [ testCase "Default has TODO and DONE" $ do
        let ctx = mempty :: Context
        assertBool "TODO should be active" (Set.member "TODO" (todoActive ctx))
        assertBool "DONE should be inactive" (Set.member "DONE" (todoInactive ctx))

    , testCase "setTodo adds to existing" $ do
        let ctx = setTodo (Set.fromList ["WAITING"]) (Set.fromList ["CANCELLED"]) mempty
        assertBool "WAITING should be active" (Set.member "WAITING" (todoActive ctx))
        assertBool "TODO should still be active" (Set.member "TODO" (todoActive ctx))
        assertBool "CANCELLED should be inactive" (Set.member "CANCELLED" (todoInactive ctx))

    , testCase "Multiple TODO pragmas accumulate" $ do
        let input = T.intercalate "\n"
              [ "#+TODO: TODO STARTED | DONE"
              , "#+TODO: WAITING | CANCELLED" ]
        let (_, ctx, _) = orgParse mempty input
        assertBool "STARTED should be active" (Set.member "STARTED" (todoActive ctx))
        assertBool "WAITING should be active" (Set.member "WAITING" (todoActive ctx))
        assertBool "CANCELLED should be inactive" (Set.member "CANCELLED" (todoInactive ctx))

    , testCase "Unregistered keyword is not parsed as TODO" $ do
        let (elems, _, _) = orgParse mempty "* CUSTOM Something"
        case elems of
          [EHeadline h] -> assertEqual "Should not have todo" Nothing (todo h)
          _ -> assertBool "Expected single headline" False
    ]

  , testGroup "Category management"
    [ testCase "Category pragma updates context" $ do
        let (_, ctx, _) = orgParse mempty "#+CATEGORY: myproject"
        assertEqual "Category should be set" "myproject" (metaCategory ctx)

    , testCase "Category property updates context" $ do
        let input = T.intercalate "\n"
              [ "* Headline"
              , ":PROPERTIES:"
              , ":CATEGORY: fromprops"
              , ":END:" ]
        let (_, ctx, _) = orgParse mempty input
        assertEqual "Category should be set from property" "fromprops" (metaCategory ctx)
    ]
  ]
