{-# LANGUAGE OverloadedStrings #-}

-- | @GET \/git@ and @POST \/git\/sync@: a one-glance git status of the served
-- dir, the one safe action per state, and Model B auto-sync, which fires only once BOTH @glance.autosync@ is set AND a push is armed in the UI.
module Glance.Web.Git
  ( GitStatus (..)
  , emptyStatus
  , parsePorcelain
  , gitStatus
  , SyncStep (..)
  , AutoSet (..)
  , GitPost (..)
  , syncActionOf
  , actionFor
  , stepsFor
  , AutoSync
  , newAutoSync
  , stopAutoSync
  , autoSyncPoke
  , gitStatusView
  , gitSyncRoute
  ) where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar, tryTakeMVar)
import Control.Monad (unless, void, when)
import Data.Aeson (Value, decode, object, (.=))
import Data.Char (isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.HTTP.Types (status200, status400)
import Network.Wai (Request, Response)
import System.Directory (canonicalizePath, doesFileExist, getModificationTime)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (isAbsolute, (</>))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Glance.Web.Base (ServeOptions (..), jsonError, jsonType, sized, withBody)


-- | A magit-style one-glance read of the work tree; @gsRepo = False@ leaves every other field 0/Nothing.
data GitStatus = GitStatus
  { gsRepo      :: !Bool
  , gsBranch    :: !(Maybe Text)  -- ^ 'Nothing' when detached or on an unborn HEAD.
  , gsUpstream  :: !(Maybe Text)
  , gsDetached  :: !Bool
  , gsAhead     :: !Int
  , gsBehind    :: !Int
  , gsStaged    :: !Int
  , gsUnstaged  :: !Int
  , gsUntracked :: !Int
  , gsLocked    :: !(Maybe UTCTime) -- ^ an index.lock exists; its modification time.
  } deriving (Eq, Show)

emptyStatus :: GitStatus
emptyStatus = GitStatus False Nothing Nothing False 0 0 0 0 0 Nothing

-- | Fold @git status --porcelain=v2 --branch@ lines into a 'GitStatus'; the caller has confirmed the dir is a repo, so 'gsRepo' starts 'True'.
parsePorcelain :: [Text] -> GitStatus
parsePorcelain = foldl' step emptyStatus { gsRepo = True }
  where
    step s ln = case T.words ln of
      ("#" : "branch.head" : name : _)
        | name == "(detached)" -> s { gsDetached = True, gsBranch = Nothing }
        | otherwise            -> s { gsBranch = Just name }
      ("#" : "branch.upstream" : up : _) -> s { gsUpstream = Just up }
      ("#" : "branch.ab" : a : b : _)    -> s { gsAhead = count a, gsBehind = count b }
      ("1" : xy : _) -> tally xy s
      ("2" : xy : _) -> tally xy s
      ("u" : _)      -> s { gsUnstaged = gsUnstaged s + 1 }  -- unmerged: the user's to resolve.
      ("?" : _)      -> s { gsUntracked = gsUntracked s + 1 }
      _              -> s
    -- @branch.ab@ is @+A -B@; each side's magnitude is the count that side moves.
    count = fromMaybe 0 . readMaybe . filter isDigit . T.unpack
    -- A changed entry's XY: X is the index (staged) column, Y the work tree.
    tally xy s = case T.unpack xy of
      (x : y : _) -> s { gsStaged   = gsStaged s   + fromEnum (x /= '.')
                       , gsUnstaged = gsUnstaged s + fromEnum (y /= '.') }
      _           -> s

-- | Read the work tree's status in DIR; a non-work-tree answers 'emptyStatus' (@gsRepo = False@).
gitStatus :: FilePath -> IO GitStatus
gitStatus dir = do
  inside <- gitBool dir ["rev-parse", "--is-inside-work-tree"]
  if not inside
    then pure emptyStatus
    else do
      (_, out, _) <- readGit dir ["status", "--porcelain=v2", "--branch"]
      locked <- indexLockTime dir
      pure (parsePorcelain (T.lines (T.pack out))) { gsLocked = locked }

-- | The index lock Git itself names for DIR, and when it last moved.  Glance
-- never deletes it: an active git and a stale lock have the same pathname, so
-- the status tells the reader and leaves the recovery decision to them.
indexLockTime :: FilePath -> IO (Maybe UTCTime)
indexLockTime dir = do
  (code, out, _) <- readGit dir ["rev-parse", "--git-path", "index.lock"]
  let named = T.unpack (T.strip (T.pack out))
      path = if isAbsolute named then named else dir </> named
  exists <- doesFileExist path
  if code == ExitSuccess && exists then Just <$> getModificationTime path else pure Nothing


-- | A git job: one or more commands run in order, stopping at the first failure.
data SyncStep = Fetch | Pull | Push | CommitPush | Sync
  deriving (Eq, Show, Enum, Bounded)

-- | A change to the auto-sync handle.  No git command runs.
data AutoSet = AutoOn | AutoOff | Arm
  deriving (Eq, Show, Enum, Bounded)

-- | What POST /git/sync was asked for.  The route's two roads are the type's.
data GitPost = Step !SyncStep | Auto !AutoSet
  deriving (Eq, Show)

postWord :: GitPost -> Text
postWord (Step Fetch)      = "fetch"
postWord (Step Pull)       = "pull"
postWord (Step Push)       = "push"
postWord (Step CommitPush) = "commit-push"
postWord (Step Sync)       = "sync"
postWord (Auto AutoOn)     = "autosync-on"
postWord (Auto AutoOff)    = "autosync-off"
postWord (Auto Arm)        = "arm"

gitPosts :: [GitPost]
gitPosts = map Step [minBound .. maxBound] <> map Auto [minBound .. maxBound]

syncActionOf :: Text -> Maybe GitPost
syncActionOf word = listToMaybe [ post | post <- gitPosts, postWord post == word ]

-- | The one safe step for a state, which auto-sync runs; 'Nothing' when none is safe (detached, no upstream).
actionFor :: GitStatus -> Maybe SyncStep
actionFor s
  | not (gsRepo s)                           = Nothing
  | isJust (gsLocked s)                      = Nothing
  | gsDetached s || isNothing (gsUpstream s) = Nothing
  | dirty > 0 && ahead == 0 && behind == 0   = Just CommitPush
  | ahead > 0 && behind > 0                  = Just Sync
  | dirty > 0                                = Just CommitPush
  | behind > 0                               = Just Pull
  | ahead > 0                                = Just Push
  | otherwise                                = Just Fetch
  where
    dirty  = gsStaged s + gsUnstaged s + gsUntracked s
    ahead  = gsAhead s
    behind = gsBehind s

data SyncResult = SyncResult
  { srOk     :: !Bool
  , srSteps  :: ![Text]  -- ^ the git commands run, in order.
  , srOutput :: !Text
  }

-- | Perform a 'SyncStep' in DIR command by command, stopping at the first failure.
runSync :: FilePath -> SyncStep -> IO SyncResult
runSync dir action = go (stepsFor action) (SyncResult True [] "")
  where
    go [] acc = pure acc
    go (args : rest) acc = do
      (code, out, err) <- readGit dir args
      let label = T.pack (unwords ("git" : args))
          acc'  = acc { srSteps = srSteps acc ++ [label]
                      , srOutput = srOutput acc <> label <> "\n" <> T.pack (out <> err) }
      if code == ExitSuccess || tolerable args (out <> err)
        then go rest acc'
        else pure acc' { srOk = False }
    -- @commit@ with nothing staged exits non-zero; auto-sync must carry past it.
    tolerable ("commit" : _) msg = "nothing to commit" `T.isInfixOf` T.pack msg
    tolerable _              _   = False

stepsFor :: SyncStep -> [[String]]
stepsFor Fetch      = [["fetch"]]
stepsFor Pull       = [["pull", "--ff-only"]]
stepsFor Push       = [["push"]]
stepsFor CommitPush =
  [ [ "add", "-A", "--", "."
    , ":(exclude).org-glance/meta/EXTERNAL.jsonl"
    , ":(exclude).org-glance/meta/COMPLETIONS.jsonl" ]
  , ["commit", "-m", "glance: sync"]
  , ["push"]
  ]
stepsFor Sync       = [["pull", "--rebase"], ["push"]]


-- | Model B state: the on/armed flags mirror @git config@ so they survive a restart;
-- the single-slot poke channel debounces a burst of edits into one commit + push.
data AutoSync = AutoSync
  { asDir   :: !FilePath
  , asOn    :: !(IORef Bool)
  , asArmed :: !(IORef Bool)
  , asPoke  :: !(MVar ())
  , asStopping :: !(IORef Bool)
  , asDone  :: !(MVar ())
  }

-- | Build the handle and fork its debounced worker.
newAutoSync :: FilePath -> IO AutoSync
newAutoSync dir = do
  on    <- newIORef =<< configBool dir "glance.autosync"
  armed <- newIORef =<< configBool dir "glance.autosync-armed"
  poke  <- newEmptyMVar
  stopping <- newIORef False
  done <- newEmptyMVar
  let as = AutoSync dir on armed poke stopping done
  _ <- forkFinally (worker as) (const (putMVar done ()))
  pure as
  where
    worker as = do
      takeMVar (asPoke as)            -- wait for a write to land.
      stopping <- readIORef (asStopping as)
      unless stopping $ do
        threadDelay 3000000           -- 3 s: coalesce a burst into one sync.
        _ <- tryTakeMVar (asPoke as)   -- drop pokes gathered during the wait.
        stopping' <- readIORef (asStopping as)
        unless stopping' $ do
          on    <- readIORef (asOn as)
          armed <- readIORef (asArmed as)
          when (on && armed) (void (runSync (asDir as) CommitPush))
          worker as

-- | Stop accepting work and join the worker.  If git is already running the
-- daemon waits for that child, so process shutdown cannot abandon index.lock.
stopAutoSync :: AutoSync -> IO ()
stopAutoSync as = do
  writeIORef (asStopping as) True
  void (tryPutMVar (asPoke as) ())
  takeMVar (asDone as)

-- | Non-blocking nudge from the write path: a full slot means a sync is already owed.
autoSyncPoke :: AutoSync -> IO ()
autoSyncPoke as = do
  stopping <- readIORef (asStopping as)
  unless stopping (void (tryPutMVar (asPoke as) ()))

autoSyncState :: AutoSync -> IO (Bool, Bool)  -- ^ (on, armed)
autoSyncState as = (,) <$> readIORef (asOn as) <*> readIORef (asArmed as)

autoSyncSet :: AutoSync -> Bool -> IO ()
autoSyncSet as on = do
  setConfigBool (asDir as) "glance.autosync" on
  writeIORef (asOn as) on

autoSyncArm :: AutoSync -> IO ()
autoSyncArm as = do
  setConfigBool (asDir as) "glance.autosync-armed" True
  writeIORef (asArmed as) True


-- | @GET \/git@: the status plus the served dir and the two Model B flags.
gitStatusView :: ServeOptions -> Maybe AutoSync -> IO Response
gitStatusView opts mas = do
  dir <- canonicalizePath (soDir opts)
  st  <- gitStatus (soDir opts)
  (on, armed) <- maybe (pure (False, False)) autoSyncState mas
  pure . sized status200 [jsonType] . A.encode $ statusJSON dir st on armed

-- | @POST \/git\/sync@: body @{"action":"…"}@ runs that one action; unknown or missing is a 400.
gitSyncRoute :: ServeOptions -> Maybe AutoSync -> Request -> IO Response
gitSyncRoute opts mas request = withBody request $ \raw ->
  case actionOf raw of
    Nothing -> pure (jsonError status400 "git sync: unknown or missing action")
    Just post -> handle post
  where
    actionOf raw = do
      obj <- decode raw
      A.Object o <- pure (obj :: Value)
      A.String t <- KM.lookup "action" o
      syncActionOf t
    handle (Step step) = do
      r <- runSync (soDir opts) step
      pure . sized status200 [jsonType] . A.encode
           $ object ["ok" .= srOk r, "steps" .= srSteps r, "output" .= srOutput r]
    handle (Auto setting) = case mas of
      Nothing -> pure (jsonError status400 "auto-sync unavailable in this mode")
      Just as -> do
        case setting of
          AutoOn  -> autoSyncSet as True
          AutoOff -> autoSyncSet as False
          Arm     -> autoSyncArm as
        (on, armed) <- autoSyncState as
        pure . sized status200 [jsonType] . A.encode
             $ object ["ok" .= True, "autosync" .= on, "armed" .= armed]

statusJSON :: FilePath -> GitStatus -> Bool -> Bool -> Value
statusJSON dir s on armed
  | not (gsRepo s) = object ["repo" .= False, "dir" .= dir]
  | otherwise = object
      [ "repo" .= True, "dir" .= dir
      , "branch" .= gsBranch s, "upstream" .= gsUpstream s, "detached" .= gsDetached s
      , "ahead" .= gsAhead s, "behind" .= gsBehind s
      , "staged" .= gsStaged s, "unstaged" .= gsUnstaged s, "untracked" .= gsUntracked s
      , "locked" .= gsLocked s
      , "action" .= fmap (postWord . Step) (actionFor s)
      , "glyph" .= glyphOf s, "cls" .= classOf s, "label" .= labelOf s
      , "autosync" .= on, "armed" .= armed
      ]

glyphOf :: GitStatus -> Text
glyphOf s
  | isJust (gsLocked s)           = "⚠"
  | gsDetached s || noUpstream    = "⚠"
  | dirty > 0 && gsAhead s == 0 && gsBehind s == 0 = "●"
  | gsAhead s > 0 && gsBehind s > 0 = "↕"
  | dirty > 0                     = "●"
  | gsBehind s > 0                = "↓"
  | gsAhead s > 0                 = "↑"
  | otherwise                     = "✓"
  where dirty = gsStaged s + gsUnstaged s + gsUntracked s
        noUpstream = isNothing (gsUpstream s)

classOf :: GitStatus -> Text
classOf s
  | isJust (gsLocked s)              = "g-detached"
  | gsDetached s || isNothing (gsUpstream s) = "g-detached"
  | dirty > 0 && gsAhead s == 0 && gsBehind s == 0 = "g-dirty"
  | gsAhead s > 0 && gsBehind s > 0  = "g-diverged"
  | dirty > 0                        = "g-dirty"
  | gsBehind s > 0                   = "g-behind"
  | gsAhead s > 0                    = "g-ahead"
  | otherwise                        = "g-clean"
  where dirty = gsStaged s + gsUnstaged s + gsUntracked s

labelOf :: GitStatus -> Text
labelOf s = T.intercalate " · " (base : facts)
  where
    dirty = gsStaged s + gsUnstaged s + gsUntracked s
    headName | gsDetached s = "detached HEAD"
             | otherwise = fromMaybe "no branch" (gsBranch s)
    base = maybe headName (\up -> headName <> " tracking " <> up) (gsUpstream s)
    facts = [ "index.lock exists since " <> T.pack (show at) | Just at <- [gsLocked s] ]
         <> [ "no upstream" | isNothing (gsUpstream s) ]
         <> [ T.pack (show dirty) <> " uncommitted (" <> T.pack (show (gsStaged s))
              <> " staged, " <> T.pack (show (gsUnstaged s)) <> " unstaged, "
              <> T.pack (show (gsUntracked s)) <> " untracked)" | dirty > 0 ]
         <> [ T.pack (show (gsBehind s)) <> " behind" | gsBehind s > 0 ]
         <> [ T.pack (show (gsAhead s)) <> " ahead" | gsAhead s > 0 ]
         <> [ "up to date" | isNothing (gsLocked s), isJust (gsUpstream s)
                          , dirty == 0, gsAhead s == 0, gsBehind s == 0 ]


-- Shelling git, all through one door: @-C dir@ so the cwd never matters.

readGit :: FilePath -> [String] -> IO (ExitCode, String, String)
readGit dir args = readProcessWithExitCode "git" (["-C", dir] <> args) ""

gitBool :: FilePath -> [String] -> IO Bool
gitBool dir args = do
  (code, out, _) <- readGit dir args
  pure (code == ExitSuccess && T.strip (T.pack out) == "true")

configBool :: FilePath -> String -> IO Bool
configBool dir key = do
  (code, out, _) <- readGit dir ["config", "--bool", "--get", key]
  pure (code == ExitSuccess && T.strip (T.pack out) == "true")

setConfigBool :: FilePath -> String -> Bool -> IO ()
setConfigBool dir key on =
  void (readGit dir ["config", key, if on then "true" else "false"])
