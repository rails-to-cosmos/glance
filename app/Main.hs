module Main (main) where

import Data.List (intercalate)
import System.Environment
import System.Exit

import Data.Org (Context, defaultContext, orgParse)
import Data.Org.Edit (readDocument)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty)

import Repl.Org
import Scan (runScan)

import Data.Org.Walk (WalkOptions (..), defaultWalk)
import Glance.Desktop (DesktopOptions (..))
import Glance.Desktop.Native (desktopWith)
import Glance.Desktop.WebKit (nativeAvailable, nativeWindow)
import Glance.Web (ServeOptions (..), defaultPort, serve)

import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified System.Console.Haskeline as Haskeline

-- | Haskeline settings backed by a history file under ~/.config/glance.
replSettings :: IO (Haskeline.Settings IO)
replSettings = do
  homeDir <- getHomeDirectory

  let configDir = homeDir </> ".config" </> "glance"

  createDirectoryIfMissing True configDir

  return Haskeline.defaultSettings { Haskeline.autoAddHistory = True
                                   , Haskeline.historyFile = Just (configDir </> ".history") }

main :: IO ()
main = getArgs >>= parse

-- | The REPL banner, with MESSAGES under it.
greetings :: [Text] -> IO ()
greetings messages = do
  TIO.putStrLn "---"
  TIO.putStrLn "Hello, fellow hacker!\n"
  mapM_ TIO.putStrLn messages
  TIO.putStrLn "---"
  TIO.putStrLn ""

parse :: [String] -> IO a

-- | NO ARGUMENT IS A QUESTION, so the bare command answers it; the REPL is
-- @glance repl@ and is asked for by name like every other command.
parse [] = putStrLn glanceUsage >> exitSuccess

parse args | wantsHelp args = do
  putStrLn $ case args of
    ("repl":_)    -> replUsage
    ("serve":_)   -> serveUsage
    ("desktop":_) -> desktopUsage
    ("scan":_)    -> scanUsage
    _everything   -> glanceUsage
  exitSuccess

parse ("repl":[])           = repl [] [] defaultContext

parse ("repl":filename:_) = do
  -- A latin-1 round trip gave mojibake titles and byte offsets where spans are chars.
  content <- readDocument filename

  case content of
    Nothing -> do
      hPutStrLn stderr ("glance repl: cannot read " <> filename <> " as UTF-8 org")
      exitFailure
    Just (text, _digest) ->
      repl ["Additional context provided: " <> Text.pack filename]
           [ Text.pack (errorBundlePretty err) | Just err <- [maybeErr] ]
           context
      where (_elements, context, maybeErr) = orgParse defaultContext text

parse ("scan":args) = do
  let derived = "--include-derived" `elem` args
      dirs = filter (/= "--include-derived") args
  runScan (WalkOptions derived) (if null dirs then ["."] else dirs)
  exitSuccess

parse ("serve":args) = run "serve" serveUsage serve (serveOptions args)

parse ("desktop":args) = run "desktop" desktopUsage runDesktop (desktopOptions args)

parse (arg:_) = do
  hPutStrLn stderr ("glance: unknown command: " <> arg)
  hPutStrLn stderr glanceUsage
  exitFailure

-- | Greet with MESSAGES, print NOTES, hand the terminal to the REPL under CONTEXT.
repl :: [Text] -> [Text] -> Context -> IO a
repl messages notes context = do
  settings <- replSettings
  greetings messages
  mapM_ TIO.putStrLn notes
  runRepl settings context
  exitSuccess

-- | OPTIONS through ACT, or NAME's complaint and USAGE on stderr.
run :: String -> String -> (a -> IO ()) -> Either String a -> IO b
run name usage act parsed = case parsed of
  Left err -> do
    hPutStrLn stderr ("glance " <> name <> ": " <> err)
    hPutStrLn stderr usage
    exitFailure
  Right opts -> do
    act opts
    exitSuccess

-- | Does ARGS ask for help?  A VALUE POSITION IS STEPPED OVER, so @--dir help@
-- names a directory and asks nothing.
wantsHelp :: [String] -> Bool
wantsHelp []       = False
wantsHelp (a:rest)
  | a `elem` ["--help", "-h", "help"] = True
  | otherwise                         = wantsHelp (drop (valued a) rest)
  where valued x = length [ () | Flag n (Val _ _) _ _ <- flags, n == x ]

-- | Every command in one screen, each flag-taking one spelled in full under it.
glanceUsage :: String
glanceUsage = intercalate "\n"
  [ "usage: glance serve    serve an org tree over HTTP"
  , "       glance desktop  the same daemon in an app window"
  , "       glance scan     parse a corpus and report what drifted"
  , "       glance repl     the org parser at a prompt"
  , ""
  , serveUsage
  , ""
  , desktopUsage
  , ""
  , scanUsage
  , ""
  , replUsage
  ]

replUsage :: String
replUsage = intercalate "\n" $
  [ "usage: glance repl [FILE]"
  , described "FILE" "an org document to parse and hold as context" ]
  <> [ ""
     , "A prompt over the org parser.  History lands in ~/.config/glance/.history." ]

serveUsage :: String
serveUsage = intercalate "\n" $
  [ "usage: glance serve --dir DIR [options]" ]
  <> flagLines (filter fServed flags)
  <> [ ""
     , "Binds 127.0.0.1 only, and binds BEFORE the walk, so store routes answer 503"
     , "with a loading body until the tree lands." ]

desktopUsage :: String
desktopUsage = intercalate "\n" $
  [ "usage: glance desktop --dir DIR [options]" ]
  <> flagLines flags
  <> [ ""
     , "Opens this build's WebKitGTK window when it has one and neither --browser"
     , "nor GLANCE_BROWSER names another." ]

scanUsage :: String
scanUsage = intercalate "\n" $
  [ "usage: glance scan [DIR...] [--include-derived]"
  , described "DIR..." "the trees to parse (default .)" ]
  <> flagLines [ f | f <- flags, fName f == "--include-derived" ]
  <> [ ""
     , "Reports parse coverage, span-invariant violations, and how far each"
     , "org-glance index has drifted from the blobs it indexes." ]

-- | What a flag does to the options it lands in; 'Val' names its value for the
-- usage line.
data Take = Nul (Desktop -> Desktop)
          | Val String (String -> Desktop -> Either String Desktop)

-- | ONE FLAG, spelled once.  'fServed' is whether @serve@ takes it too.
data Flag = Flag { fName :: String, fTake :: Take, fServed :: Bool, fHelp :: String }

-- | Every flag @serve@ and @desktop@ take.  THE PARSER AND THE USAGE BOTH FOLD
-- THIS, so a flag cannot be taken without being documented.
flags :: [Flag]
flags =
  [ Flag "--dir" (Val "DIR" (\v -> Right . onServe (\s -> s { soDir = v }))) True
      "the org tree to serve; required"
  , Flag "--port" (Val "N" port) True
      ("the port to listen on (default " <> show defaultPort <> ")")
  , Flag "--assets" (Val "PATH" (\v -> Right . onServe (\s -> s { soAssets = Just v }))) True
      "serve the front end from PATH instead of the embedded copy"
  , Flag "--include-derived" (Nul (onServe (\s -> s { soDerived = True }))) True
      "take org-glance's derived areas as row sources too"
  , Flag "--browser" (Val "CMD" (\v -> Right . onWindow (\w -> w { doBrowser = Just v }))) False
      "open the window with CMD, declining this build's own one"
  , Flag "--dry-run" (Nul (onWindow (\w -> w { doDryRun = True }))) False
      "print the resolved window command, binding nothing"
  , Flag "--keep-serving" (Nul (\d -> d { dKeepServing = True })) False
      "leave the daemon running after the window closes"
  ]
  where
    port v d = case readMaybe v of
      Just n | n > 0 && n < 65536 -> Right (onServe (\s -> s { soPort = n }) d)
      _notAPort                   -> Left ("not a port number: " <> v)

onWindow :: (DesktopOptions -> DesktopOptions) -> Desktop -> Desktop
onWindow f d = d { dWindow = f (dWindow d) }

onServe :: (ServeOptions -> ServeOptions) -> Desktop -> Desktop
onServe f = onWindow (\w -> w { doServe = f (doServe w) })

-- | FLAGS as usage lines, one each.
flagLines :: [Flag] -> [String]
flagLines fs = [ described (spelling f) (fHelp f) | f <- fs ]

spelling :: Flag -> String
spelling f = fName f <> case fTake f of
  Val name _ -> " " <> name
  Nul _      -> ""

-- | NAME, then WHAT it does at the column the widest flag sets.
described :: String -> String -> String
described name what = "  " <> name <> replicate (gutter - length name) ' ' <> what
  where gutter = 2 + maximum (map (length . spelling) flags)

-- | One desktop session: this build's own window, else stage 1's app-mode browser.
runDesktop :: Desktop -> IO ()
runDesktop d = desktopWith nativeAvailable nativeWindow (dKeepServing d) (dWindow d)

-- | Everything @glance desktop@ takes.
data Desktop = Desktop
  { dWindow      :: !DesktopOptions  -- ^ the daemon and the two stage-1 window flags.
  , dKeepServing :: !Bool            -- ^ @--keep-serving@: outlive the window.
  }

-- | What DESKTOP serves.
serveOf :: Desktop -> ServeOptions
serveOf = doServe . dWindow

-- | ARGS as desktop options, or what is wrong with them.  Hand-rolled: seven
-- flags do not earn an option-parsing dependency.
desktopOptions :: [String] -> Either String Desktop
desktopOptions = go (Desktop (DesktopOptions bare Nothing False) False)
  where
    bare = ServeOptions "" defaultPort Nothing (woIncludeDerived defaultWalk)
    go d [] | null (soDir (serveOf d)) = Left "--dir is required"
            | otherwise                = Right d
    go d (arg:rest) = case [ fTake f | f <- flags, fName f == arg ] of
      [Nul set]              -> go (set d) rest
      [Val _ set] | (v:more) <- rest -> set v d >>= \taken -> go taken more
                  | otherwise        -> Left (arg <> " needs a value")
      _unknown               -> Left ("unknown argument: " <> arg)

-- | ARGS as serve options: the desktop parser, minus the window-only flags.
serveOptions :: [String] -> Either String ServeOptions
serveOptions args = do
  d <- desktopOptions args
  case [ fName f | a <- args, f <- flags, fName f == a, not (fServed f) ] of
    (name:_) -> Left (name <> " is a desktop flag; serve opens no window")
    _serving -> Right (serveOf d)
