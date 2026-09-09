module TestGit (spec) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Glance.Web.Git
  ( GitStatus (..), SyncAction (..), actionFor, emptyStatus, parsePorcelain, syncActionOf )

-- | The porcelain=v2 --branch lines a status is folded from.
porc :: [Text] -> GitStatus
porc = parsePorcelain

clean, dirty, staged, behind, ahead, diverged, detached, noUpstream :: [Text]
clean =
  [ "# branch.oid abc123"
  , "# branch.head main"
  , "# branch.upstream origin/main"
  , "# branch.ab +0 -0" ]
dirty = init clean ++
  [ "# branch.ab +0 -0"
  , "1 .M N... 100644 100644 100644 aaa bbb notes.org"
  , "? new.txt" ]
staged =
  [ "# branch.head main", "# branch.upstream origin/main", "# branch.ab +0 -0"
  , "1 MM N... 100644 100644 100644 aaa bbb both.org" ]
behind   = init clean ++ ["# branch.ab +0 -3"]
ahead    = init clean ++ ["# branch.ab +2 -0"]
diverged = init clean ++ ["# branch.ab +1 -2"]
detached = ["# branch.oid abc", "# branch.head (detached)"]
noUpstream = ["# branch.head main"]

spec :: TestTree
spec = testGroup "Git"
  [ testGroup "parsePorcelain"
    [ testCase "clean, up to date" $ do
        let s = porc clean
        assertEqual "branch"   (Just "main") (gsBranch s)
        assertEqual "upstream" (Just "origin/main") (gsUpstream s)
        assertEqual "ahead"    0 (gsAhead s)
        assertEqual "behind"   0 (gsBehind s)
        assertEqual "counts"   (0, 0, 0) (gsStaged s, gsUnstaged s, gsUntracked s)
        assertEqual "repo"     True (gsRepo s)

    , testCase "dirty: one modified, one untracked" $ do
        let s = porc dirty
        assertEqual "staged"    0 (gsStaged s)
        assertEqual "unstaged"  1 (gsUnstaged s)
        assertEqual "untracked" 1 (gsUntracked s)

    , testCase "a changed entry counts on both columns" $ do
        let s = porc staged
        assertEqual "staged"   1 (gsStaged s)
        assertEqual "unstaged" 1 (gsUnstaged s)

    , testCase "behind" $ do
        let s = porc behind
        assertEqual "behind" 3 (gsBehind s)
        assertEqual "ahead"  0 (gsAhead s)

    , testCase "ahead" $ do
        let s = porc ahead
        assertEqual "ahead"  2 (gsAhead s)
        assertEqual "behind" 0 (gsBehind s)

    , testCase "diverged" $ do
        let s = porc diverged
        assertEqual "ahead"  1 (gsAhead s)
        assertEqual "behind" 2 (gsBehind s)

    , testCase "detached HEAD" $ do
        let s = porc detached
        assertEqual "detached" True (gsDetached s)
        assertEqual "branch"   Nothing (gsBranch s)

    , testCase "no upstream" $ do
        let s = porc noUpstream
        assertEqual "branch"   (Just "main") (gsBranch s)
        assertEqual "upstream" Nothing (gsUpstream s)
    ]

  , testGroup "actionFor — the one obvious step per state"
    [ testCase "clean fetches"          $ eq (Just Fetch)      (actionFor (porc clean))
    , testCase "dirty commits + pushes" $ eq (Just CommitPush) (actionFor (porc dirty))
    , testCase "behind pulls"           $ eq (Just Pull)       (actionFor (porc behind))
    , testCase "ahead pushes"           $ eq (Just Push)       (actionFor (porc ahead))
    , testCase "diverged syncs"         $ eq (Just Sync)       (actionFor (porc diverged))
    , testCase "detached — no one-click" $ eq Nothing          (actionFor (porc detached))
    , testCase "no upstream — no one-click" $ eq Nothing       (actionFor (porc noUpstream))
    , testCase "not a repo — no one-click" $ eq Nothing        (actionFor emptyStatus)
    ]

  , testGroup "syncActionOf"
    [ testCase "fetch"        $ eq (Just Fetch)      (syncActionOf "fetch")
    , testCase "commit-push"  $ eq (Just CommitPush) (syncActionOf "commit-push")
    , testCase "sync"         $ eq (Just Sync)       (syncActionOf "sync")
    , testCase "autosync-on"  $ eq (Just AutoOn)     (syncActionOf "autosync-on")
    , testCase "arm"          $ eq (Just Arm)        (syncActionOf "arm")
    , testCase "unknown"      $ eq Nothing           (syncActionOf "nope")
    ]
  ]
  where
    eq :: (Eq a, Show a) => a -> a -> IO ()
    eq = assertEqual "="
