{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Repl.Org (runRepl) where

import           Data.Text (Text, pack, intercalate)
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Control.Monad.Logger ( runStderrLoggingT )
import           Control.Monad.State ( StateT )
import           Data.Text.Lazy.Builder ()
import           Database.Persist.Monad ( runSqlQueryT, runMigration, SqlQueryT )
import           Database.Persist.Sqlite ( createSqlitePool )
import           System.Console.Haskeline (Settings (autoAddHistory, historyFile), defaultSettings, getInputLine, runInputT, InputT)
import           UnliftIO ()

import           Data.Org.Context
import           Persist.Org

import qualified Control.Monad.State as State

type CommandProcessor = OrgContext -> Text -> OrgContext
type Repl a = StateT OrgContext (SqlQueryT (InputT IO)) a

settings :: Settings IO
settings = defaultSettings { autoAddHistory = True
                           , historyFile = Just ".history"
                           }

getInput :: Repl Text
getInput = do
  input <- State.lift $ State.lift $ getInputLine "> "
  case input of
    Nothing -> return ""
    Just cmd -> return $ pack cmd

repl :: CommandProcessor -> Repl ()
repl fn = do
  ctx <- State.get
  liftIO (print ctx)
  input <- getInput
  case input of
    _ | input `elem` ["", ":END:"] -> do
          State.modify $ flip fn $ intercalate "\n" $ metaCommand ctx ++ [input]
          repl fn
      | input == ":q" -> return ()
      | otherwise -> do
          State.modify $ \c -> c { metaCommand = metaCommand c ++ [input] }
          repl fn

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
