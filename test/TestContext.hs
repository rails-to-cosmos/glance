module TestContext (spec) where

import Data.Org
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, intercalate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestDefaults

-- | An active date-only schedule at T.
scheduled :: Text -> Maybe Timestamp
scheduled t = Just (plainTs TimestampActive (on t))

-- | 'resolveHeadline' over headlines scheduled at S1 and S2 keeps WINNER.
-- Compared WHOLE: resolution picks a headline rather than building one, so a
-- winner carrying a field of the loser's is a failure the title cannot show.
resolves :: String -> Maybe Timestamp -> Maybe Timestamp -> Text -> TestTree
resolves desc s1 s2 winner = testCase desc $
    assertEqual desc expected (resolveHeadline h1 h2)
  where h1 = (titled "h1") { schedule = s1 }
        h2 = (titled "h2") { schedule = s2 }
        expected | winner == "h1" = h1
                 | otherwise      = h2

-- | A keyword-matching case: an optional registering pragma, the headline it
-- governs, and the todo and title words that headline must carry.
data TodoCase = TodoCase !String !(Maybe Text) !Text !(Maybe Todo) ![Text]

todoCases :: [TodoCase]
todoCases =
  [ TodoCase "Unregistered keyword stays in the title"
      Nothing "* CUSTOM Something" Nothing ["CUSTOM", "Something"]

  , TodoCase "Keyword casing must match: Done stays in the title"
      Nothing "* Done with it" Nothing ["Done", "with", "it"]

  , TodoCase "Keywords registered by a pragma survive into a later parse"
      (Just "#+TODO: WAITING | CANCELLED") "* WAITING Something"
      (Just (Todo "WAITING" True)) ["Something"]

  , TodoCase "Lowercase keyword matches as written"
      (Just "#+TODO: wip | done") "* wip Task" (Just (Todo "wip" True)) ["Task"]

  , TodoCase "Uppercase spelling of a lowercase keyword does not match"
      (Just "#+TODO: wip | done") "* WIP Task" Nothing ["WIP", "Task"]
  ]

todoCase :: TodoCase -> TestTree
todoCase (TodoCase desc pragma input expectedTodo titleWords) = testCase desc $
    withHeadlineIn ctx input $ \h -> do
      assertEqual "todo" expectedTodo (todo h)
      assertEqual "title" (Title (map (OrgLineToken . Token) titleWords)) (title h)
  where ctx = maybe defaultContext register pragma
        register p = case orgParse defaultContext p of (_, c, _) -> c

spec :: TestTree
spec = testGroup "Context"
  [ testGroup "IAS registration"
    [ testCase "Headline with ORG_GLANCE_ID is registered" $ do
        let (_, ctx, _) = orgParse defaultContext (withId "My Headline" "unique-id-123")
        assertEqual "registered ids" ["unique-id-123"] (Map.keys (ias ctx))

    , testCase "Headline without ORG_GLANCE_ID is not registered" $ do
        let (_, ctx, _) = orgParse defaultContext "* Anonymous Headline"
        assertEqual "registered ids" [] (Map.keys (ias ctx))

    , testCase "Multiple headlines with IDs" $ do
        let input = intercalate "\n" [withId "First" "id-1", withId "Second" "id-2"]
        let (_, ctx, _) = orgParse defaultContext input
        assertEqual "registered ids" ["id-1", "id-2"] (Map.keys (ias ctx))

    , testCase "Duplicate IDs use last-writer-wins" $ do
        let input = intercalate "\n" [ withId "First version" "same-id"
                                     , withId "Second version" "same-id" ]
        let (_, ctx, _) = orgParse defaultContext input
        assertEqual "registered ids" ["same-id"] (Map.keys (ias ctx))
        assertEqual "keeps the last"
          (Just (Title [OrgLineToken "Second", OrgLineToken "version"]))
          (title <$> Map.lookup "same-id" (ias ctx))
    ]

  , testGroup "Context accumulation across parses"
    [ testCase "Category persists across calls" $ do
        let (_, ctx1, _) = orgParse defaultContext "#+CATEGORY: first"
        let (_, ctx2, _) = orgParse ctx1 "* Hello"
        assertEqual "Category should persist" "first" (metaCategory ctx2)

    , testCase "IAS accumulates across calls" $ do
        let (_, ctx1, _) = orgParse defaultContext (withId "First" "id-1")
        let (_, ctx2, _) = orgParse ctx1 (withId "Second" "id-2")
        assertEqual "registered ids" ["id-1", "id-2"] (Map.keys (ias ctx2))
    ]

  , testGroup "resolveHeadline"
    [ resolves "Keeps h1 when its schedule is later"
        (scheduled "2024-06-01 00:00:00") (scheduled "2024-01-01 00:00:00") "h1"

    , resolves "Keeps h2 when h1 has no schedule"
        Nothing (scheduled "2024-01-01 00:00:00") "h2"

    , resolves "Keeps h2 when h2 has no schedule"
        (scheduled "2024-01-01 00:00:00") Nothing "h2"

    , resolves "Keeps h2 when h1's schedule is earlier"
        (scheduled "2024-01-01 00:00:00") (scheduled "2024-06-01 00:00:00") "h2"

    , resolves "Keeps h2 when the schedules are equal"
        (scheduled "2024-01-01 00:00:00") (scheduled "2024-01-01 00:00:00") "h2"
    ]

  , testGroup "TODO keyword management" $
    [ testCase "Default has TODO and DONE" $ do
        assertEqual "active" (Set.fromList ["TODO"]) (todoActive defaultContext)
        assertEqual "inactive" (Set.fromList ["DONE"]) (todoInactive defaultContext)

    , testCase "setTodo adds to existing" $ do
        let ctx = setTodo (Set.fromList ["WAITING"]) (Set.fromList ["CANCELLED"]) defaultContext
        assertEqual "active" (Set.fromList ["TODO", "WAITING"]) (todoActive ctx)
        assertEqual "inactive" (Set.fromList ["DONE", "CANCELLED"]) (todoInactive ctx)

    , testCase "Multiple TODO pragmas accumulate" $ do
        let input = intercalate "\n" [ "#+TODO: TODO STARTED | DONE"
                                     , "#+TODO: WAITING | CANCELLED" ]
        let (_, ctx, _) = orgParse defaultContext input
        assertEqual "active" (Set.fromList ["TODO", "STARTED", "WAITING"]) (todoActive ctx)
        assertEqual "inactive" (Set.fromList ["DONE", "CANCELLED"]) (todoInactive ctx)

    , testCase "Lowercase keywords register as written" $ do
        let (_, ctx, _) = orgParse defaultContext "#+TODO: wip | done"
        assertEqual "active" (Set.fromList ["TODO", "wip"]) (todoActive ctx)
        assertEqual "inactive" (Set.fromList ["DONE", "done"]) (todoInactive ctx)
    ] ++ map todoCase todoCases
  ]
