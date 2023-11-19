{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Repl.Org (runRepl) where

import Repl.State
import           Data.Text (Text, pack, intercalate, isPrefixOf)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.State (StateT)
import           Data.Text.Lazy.Builder ()
import           Database.Persist.Monad (runSqlQueryT, runMigration, SqlQueryT)
import           Database.Persist.Sqlite (createSqlitePool)
import           System.Console.Haskeline (Settings (autoAddHistory, historyFile), defaultSettings, getInputLine, runInputT, InputT)
import           UnliftIO ()

import Data.Org.Generic (OrgGeneric)
import           Data.Org.Context (OrgContext)
import           Persist.Org (migrateAll)

import qualified Control.Monad.State as State

type CommandProcessor = OrgContext -> Text -> (Maybe OrgGeneric, OrgContext)
type Repl a = StateT OrgContext (SqlQueryT (InputT IO)) a

settings :: Settings IO
settings = defaultSettings
  { autoAddHistory = True
  , historyFile = Just ".history"
  }

getInput :: Repl Text
getInput = do
  input <- State.lift $ State.lift $ getInputLine "> "
  case input of
    Nothing -> return ""
    Just cmd -> return (pack cmd)

repl :: CommandProcessor -> Repl ()
repl fn = do
  ctx <- State.get
  liftIO (print ctx)
  input <- getInput

  case input of
    ":q" -> return ()
    "exit" -> return ()
    "quit" -> return ()
    cmd -> do
      let (el, ctx') = applyCommand ctx cmd
      liftIO $ print el
      State.put ctx'
      repl fn

      -- | input `elem` [":PROPERTIES:", ":LOGBOOK:"] -> do  -- open drawer stack
      --     case metaStack ctx of
      --       OrgDrawer xs -> State.modify $ \c -> c { metaStack = OrgDrawer (xs ++ [input]) }
      --       EmptyStack -> State.modify $ \c -> c { metaStack = OrgDrawer [input] }
      --       OrgBabel _ -> return ()
      --     repl fn
      -- | input == ":END:" -> do  -- close and apply stack instructions to drawer
      --     case metaStack ctx of
      --       OrgDrawer xs -> do
      --         let stack = xs ++ [input]
      --         State.modify $ flip fn $ intercalate "\n" stack
      --       _ -> return ()
      --     repl fn
      -- | "#+begin_src" `isPrefixOf` input -> do  -- open babel stack
      --     case metaStack ctx of
      --       OrgBabel xs -> State.modify $ \c -> c { metaStack = OrgBabel (xs ++ [input]) }
      --       EmptyStack -> State.modify $ \c -> c { metaStack = OrgBabel [input] }
      --       OrgDrawer _ -> return ()
      --     repl fn
      -- | input == "#+end_src" -> do  -- close and apply stack instructions to babel code block
      --     case metaStack ctx of
      --       OrgBabel xs -> do
      --         let stack = xs ++ [input]
      --         State.modify $ flip fn $ intercalate "\n" stack
      --       _ -> return ()
      --     repl fn
      -- | otherwise -> do
      --     case metaStack ctx of
      --       EmptyStack -> State.modify $ flip fn input
      --       OrgDrawer xs -> State.modify $ \c -> c { metaStack = OrgDrawer (xs ++ [input]) }
      --       OrgBabel xs -> State.modify $ \c -> c { metaStack = OrgBabel (xs ++ [input]) }
      --     repl fn

runRepl :: Text -> Int -> OrgContext -> CommandProcessor -> IO ()
runRepl connectionString poolSize initialState fn = do
  pool <- runStderrLoggingT createPool

  runSqlQueryT pool $ runMigration migrateAll

  runInputT settings
    $ runSqlQueryT pool
    $ runStateT initialState
    $ repl fn

  where runStateT = flip State.evalStateT
        createPool = createSqlitePool connectionString poolSize
