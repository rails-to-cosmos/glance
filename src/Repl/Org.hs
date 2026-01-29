{-# LANGUAGE TypeFamilies #-}

module Repl.Org (runRepl) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT)
import Control.Monad.State qualified as State

import Data.Org qualified as Org
import Data.Config qualified as Config
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO as TIO
import Data.Text.Lazy.Builder ()
import System.Console.Haskeline (InputT, getInputLine, runInputT)
import TextShow qualified as TS
import UnliftIO ()

type CommandProcessor = Org.Context -> Text -> ([Org.Element], Org.Context)
type Repl a = StateT Org.Context (InputT IO) a

getInput :: Repl Text
getInput = do
  input <- State.lift $ getInputLine "> "
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

runRepl :: Config.Config -> Org.Context -> CommandProcessor -> IO ()
runRepl (Config.Config {..}) state fn = do
  -- conn <- createPool
  -- runSqlQueryT conn (runMigration migrateAll)
  runInputT haskelineSettings
    $ evalStateT state
    $ repl fn
  where evalStateT = flip State.evalStateT
