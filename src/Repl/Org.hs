module Repl.Org (runRepl) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Org as Org
import qualified Data.Text as Text
import Data.Text.IO as TIO
import System.Console.Haskeline (InputT, Settings, getInputLine, runInputT)
import Text.Megaparsec (errorBundlePretty)
import qualified TextShow as TS

type Repl a = StateT Org.Context (InputT IO) a

repl :: Repl ()
repl = do
  ctx <- State.get
  liftIO $ TIO.putStrLn $ Org.display ctx
  input <- maybe "" Text.pack <$> State.lift (getInputLine "> ")

  unless (input `elem` [":q", "exit", "quit"]) $ do
    let (elements, ctx', maybeErr) = Org.orgParse ctx input
    liftIO $ do
      TIO.putStrLn $ "Repr: " <> Text.pack (show elements)
      TIO.putStrLn $ "Str: \"" <> Text.intercalate "" (map TS.showt elements) <> "\""
      TIO.putStrLn $ "Display:\n\n" <> Text.intercalate "" (map Org.display elements)
      case maybeErr of
        Nothing  -> return ()
        Just err -> TIO.putStrLn $ "Errors:\n" <> Text.pack (errorBundlePretty err)
    State.put ctx'
    repl

-- | Read org text from SETTINGS' prompt, threading STATE across inputs.
runRepl :: Settings IO -> Org.Context -> IO ()
runRepl settings state = runInputT settings (State.evalStateT repl state)
