{-# LANGUAGE TypeFamilies #-}

module Repl.Org (runRepl) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Config as Config
import Data.Maybe
import Data.Org (orgParse, OrgParser)
import qualified Data.Org as Org
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as TIO
import System.Console.Haskeline (InputT, getInputLine, runInputT)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import qualified TextShow as TS

type Repl a = StateT Org.Context (InputT IO) a

getInput :: Repl Text
getInput = do
  input <- State.lift $ getInputLine "> "
  return $ maybe "" Text.pack input

repl :: OrgParser -> Repl ()
repl fn = do
  ctx <- State.get
  liftIO $ TIO.putStrLn $ Org.display ctx
  input <- getInput

  unless (input `elem` [":q", "exit", "quit"]) $ do
    let (elements, ctx', maybeErr) = orgParse ctx input
    liftIO $ do
      TIO.putStrLn $ "Repr: " <> Text.pack (show elements)
      TIO.putStrLn $ "Str: \"" <> Text.intercalate "" (map TS.showt elements) <> "\""
      TIO.putStrLn $ "Display:\n\n" <> Text.intercalate "" (map Org.display elements)
      case maybeErr of
        Nothing  -> return ()
        Just err -> TIO.putStrLn $ "Errors:\n" <> Text.pack (errorBundlePretty err)
    State.put ctx'
    repl fn

runRepl :: Config.Config -> Org.Context -> OrgParser -> IO ()
runRepl (Config.Config {..}) state fn = do
  -- conn <- createPool
  -- runSqlQueryT conn (runMigration migrateAll)
  runInputT haskelineSettings
    $ evalStateT state
    $ repl fn
  where evalStateT = flip State.evalStateT
