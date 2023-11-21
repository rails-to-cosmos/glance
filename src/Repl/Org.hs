{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Repl.Org (runRepl) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.State (StateT)
import Control.Monad.State qualified as State
import Data.Org.Context (OrgContext)
import Data.Org.Generic
import Data.Text (Text, pack)
import Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder ()
import Database.Persist.Monad (SqlQueryT, runMigration, runSqlQueryT)
import Database.Persist.Sqlite (createSqlitePool)
import Persist.Org (migrateAll)
import Repl.State
import System.Console.Haskeline (InputT, Settings (autoAddHistory, historyFile), defaultSettings, getInputLine, runInputT)
import TextShow
import UnliftIO ()

type CommandProcessor = OrgContext -> Text -> (OrgGenericElement, OrgContext)

type Repl a = StateT OrgContext (SqlQueryT (InputT IO)) a

settings :: Settings IO
settings =
  defaultSettings
    { autoAddHistory = True,
      historyFile = Just ".history"
    }

getInput :: Repl Text
getInput = do
  input <- State.lift $ State.lift $ getInputLine "> "
  case input of
    Nothing -> return ""
    Just cmd -> return (pack cmd)

printTextShow :: (TextShow a) => a -> IO ()
printTextShow = TIO.putStrLn . toStrict . toLazyText . showb

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
      liftIO $ do
        TIO.putStrLn $ "Repr: " <> pack (show el)
        TIO.putStrLn $ "Str: \"" <> showt el <> "\""
      State.put ctx'
      repl fn

-- \| input `elem` [":PROPERTIES:", ":LOGBOOK:"] -> do  -- open drawer stack
--     case metaStack ctx of
--       OrgDrawer xs -> State.modify $ \c -> c { metaStack = OrgDrawer (xs ++ [input]) }
--       EmptyStack -> State.modify $ \c -> c { metaStack = OrgDrawer [input] }
--       OrgBabel _ -> return ()
--     repl fn
-- \| input == ":END:" -> do  -- close and apply stack instructions to drawer
--     case metaStack ctx of
--       OrgDrawer xs -> do
--         let stack = xs ++ [input]
--         State.modify $ flip fn $ intercalate "\n" stack
--       _ -> return ()
--     repl fn
-- \| "#+begin_src" `isPrefixOf` input -> do  -- open babel stack
--     case metaStack ctx of
--       OrgBabel xs -> State.modify $ \c -> c { metaStack = OrgBabel (xs ++ [input]) }
--       EmptyStack -> State.modify $ \c -> c { metaStack = OrgBabel [input] }
--       OrgDrawer _ -> return ()
--     repl fn
-- \| input == "#+end_src" -> do  -- close and apply stack instructions to babel code block
--     case metaStack ctx of
--       OrgBabel xs -> do
--         let stack = xs ++ [input]
--         State.modify $ flip fn $ intercalate "\n" stack
--       _ -> return ()
--     repl fn
-- \| otherwise -> do
--     case metaStack ctx of
--       EmptyStack -> State.modify $ flip fn input
--       OrgDrawer xs -> State.modify $ \c -> c { metaStack = OrgDrawer (xs ++ [input]) }
--       OrgBabel xs -> State.modify $ \c -> c { metaStack = OrgBabel (xs ++ [input]) }
--     repl fn

runRepl :: Text -> Int -> OrgContext -> CommandProcessor -> IO ()
runRepl connectionString poolSize initialState fn = do
  pool <- runStderrLoggingT createPool

  runSqlQueryT pool $ runMigration migrateAll

  runInputT settings $
    runSqlQueryT pool $
      runStateT initialState $
        repl fn
  where
    runStateT = flip State.evalStateT
    createPool = createSqlitePool connectionString poolSize
