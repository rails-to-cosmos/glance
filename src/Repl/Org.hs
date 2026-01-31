{-# LANGUAGE TypeFamilies #-}

module Repl.Org (runRepl) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Config as Config
import Data.Org (orgParse)
import qualified Data.Org as Org
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as TIO
import System.Console.Haskeline (InputT, getInputLine, runInputT)
import qualified TextShow as TS

type CommandProcessor = Org.Context -> Text -> ([Org.Element], Org.Context)
type Repl a = StateT Org.Context (InputT IO) a

getInput :: Repl Text
getInput = do
  input <- State.lift $ getInputLine "> "
  return $ maybe "" Text.pack input

repl :: CommandProcessor -> Repl ()
repl fn = do
  ctx <- State.get
  liftIO $ TIO.putStrLn $ Org.display ctx
  input <- getInput

  case input of
    ":q" -> return ()
    "exit" -> return ()
    "quit" -> return ()
    cmd -> do
      let (elements, ctx') = orgParse ctx cmd
      liftIO $ do
        TIO.putStrLn $ "Repr: " <> Text.pack (show elements)
        TIO.putStrLn $ "Str: \"" <> Text.intercalate "" (map TS.showt elements) <> "\""
        TIO.putStrLn $ "Display: " <> Text.intercalate "" (map Org.display elements)
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
