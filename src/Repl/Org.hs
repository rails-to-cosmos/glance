{-# LANGUAGE TypeFamilies #-}

module Repl.Org (runRepl) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.State (StateT)
import Control.Monad.State qualified as State

import Data.Org qualified as Org
import Data.Config qualified as Config
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO as TIO
import Data.Text.Lazy.Builder ()
import Database.Persist.Monad (SqlQueryT, runMigration, runSqlQueryT)
import Database.Persist.Sqlite (createSqlitePool)
import Persist.Org (migrateAll)
import System.Console.Haskeline (InputT, getInputLine, runInputT)
import TextShow qualified as TS
import UnliftIO ()

type CommandProcessor = Org.St -> Text -> ([Org.Element], Org.St)
type Repl a = StateT Org.St (SqlQueryT (InputT IO)) a

getInput :: Repl Text
getInput = do
  input <- State.lift $ State.lift $ getInputLine "> "
  return $ maybe "" Text.pack input

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
      let (elements, ctx') = Org.parse ctx cmd
      liftIO $ do
        TIO.putStrLn $ "Repr: " <> Text.pack (show elements)
        TIO.putStrLn $ "Str: \"" <> Text.intercalate "" (map TS.showt elements) <> "\""
      State.put ctx'
      repl fn

runRepl :: Config.Config -> Org.St -> CommandProcessor -> IO ()
runRepl config state fn = do
  pool <- createPool
  runSqlQueryT pool (runMigration migrateAll)
  runInputT haskelineSettings (runSqlQueryT pool (evalStateT state (repl fn)))
  where dbPoolSize = Config.dbPoolSize config
        dbConnectionString = Config.dbConnectionString config
        haskelineSettings = Config.haskelineSettings config
        evalStateT = flip State.evalStateT
        createPool = runStderrLoggingT (createSqlitePool dbConnectionString dbPoolSize)
