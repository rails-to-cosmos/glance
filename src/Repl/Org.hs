{-# LANGUAGE TypeFamilies #-}

module Repl.Org (runRepl) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.State (StateT)
import Control.Monad.State qualified as State
import Data.Org.Context (OrgContext)
import Data.Org.Generic
import Data.Text (Text, pack)
import Data.Text.IO as TIO
import Data.Text.Lazy.Builder ()
import Database.Persist.Monad (SqlQueryT, runMigration, runSqlQueryT)
import Database.Persist.Sqlite (createSqlitePool)
import Persist.Org (migrateAll)
import Repl.State
import System.Console.Haskeline (InputT, Settings (autoAddHistory, historyFile), defaultSettings, getInputLine, runInputT)
import TextShow
import UnliftIO ()

type CommandProcessor = OrgContext -> Text -> ([OrgGenericElement], OrgContext)

type Repl a = StateT OrgContext (SqlQueryT (InputT IO)) a

settings :: Settings IO
settings = defaultSettings { autoAddHistory = True
                           , historyFile = Just ".history" }

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
      let (elements, ctx') = parseOrgElements ctx cmd
      liftIO $ do
        TIO.putStrLn $ "Repr: " <> pack (show elements)
        TIO.putStrLn $ "Str: \"" <> showt elements <> "\""
      State.put ctx'
      repl fn

runRepl :: Text -> Int -> OrgContext -> CommandProcessor -> IO ()
runRepl connectionString poolSize state fn = do
  pool <- runStderrLoggingT createPool
  runSqlQueryT pool (runMigration migrateAll)
  runInputT settings (runSqlQueryT pool (runStateT state (repl fn)))
  where runStateT = flip State.evalStateT
        createPool = createSqlitePool connectionString poolSize
