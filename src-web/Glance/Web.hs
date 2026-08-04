-- | The M1 web layer: headlines out of a directory, into a browser tab, and
-- kept current there.  This module is the daemon — bind, walk, watch — and the
-- door the rest of the program comes through.
--
-- This component's build-depends names the public @glance@ library and the HTTP
-- packages.  @glance-internal@ is absent, so @Data.Org.*@ is out of scope here
-- and reaching for it means writing the dependency down where anyone reading
-- the stanza sees it.  That is the facade invariant (docs\/invariants.md,
-- Architecture), kept where the solver can check it.
--
-- The layer below, in dependency order: 'Glance.Web.Base' is what one server
-- serves and how it answers, 'Glance.Web.Keymap' the shell's one key map,
-- 'Glance.Web.Page' the served documents (@.Style@ the stylesheet they wear,
-- @.Glue@ the script the shell runs), 'Glance.Web.Commands' the structured
-- writes, and 'Glance.Web.Routes' the HTTP surface over all of it.
--
-- The walk does not happen first.  Warp binds, the banner prints, and the one
-- full parse runs on its own thread; the store routes answer 503 until it lands
-- and the watch starts after it, so it never sees a store the walk has not
-- finished writing.  A 15-second walk used to be 15 seconds of refused
-- connections.
--
-- The directory is parsed once into 'Glance.Web.Store.Store' and a file watcher
-- re-parses one file per edit.  Org files stay the single source of truth — the
-- store is a projection that dies with the process.
--
-- The listener binds 127.0.0.1 and nothing else.  Read, write and automate
-- tiers arrive at S7 (docs\/plan-org-console-web.md); until an unauthenticated
-- connection is a read-only one by construction, the loopback interface is the
-- whole access-control story.
module Glance.Web ( ServeOptions (..)
                  , defaultPort
                  , application
                  , bannerLines
                  , bootstrapWanted
                  , serve
                  , serveAs
                  , viewTitleFor
                  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (finally)
import Control.Monad (unless, void)
import GHC.Clock (getMonotonicTime)
import System.Directory (doesDirectoryExist)
import System.Exit (die)

import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

import Glance.Query (IdCollision (..), QueryResult (..), captureTargetIn)
import Glance.Web.Base ( ServeOptions (..), defaultPort, tenths, viewTitleFor
                       , walkFor )
import Glance.Web.Routes (application, bootstrapWanted, hasRenderer)
import Glance.Web.Store ( Hub, Store (stConfig), finishLoading, loadStoreWith
                        , newLoadingHub, storeResult )
import Glance.Web.Watch (say, watchOrgTree)

-- Server

-- | Serve OPTS until killed.
serve :: ServeOptions -> IO ()
serve opts = serveAs "serve" opts (pure ())

-- | Serve OPTS until killed, running LISTENING once the socket is bound and
-- accepting.  A missing org directory fails here rather than per request: the
-- operator learns at startup, not from a 500.
--
-- MODE is the subcommand that asked — one daemon either way, and what the
-- banner and the startup failure call it, so @glance desktop@ does not report
-- itself as @glance serve@ in the one place a reader looks to see which they
-- started.
--
-- The walk does not happen first.  Warp binds, @LISTENING@ runs, and the one
-- full parse runs in its own thread — over @~\/sync@ that is 15 seconds during
-- which a request would otherwise be a refused connection instead of a page
-- saying what the server is doing.  The store routes answer 503 until the
-- parse lands ('indexing'), and the watch starts after it, on the same thread,
-- so it never sees a store the walk has not finished writing.
--
-- LISTENING is what @glance desktop@ opens its window from, and the socket is
-- where a window is wanted: the indexing page is the point of serving before the
-- load.  It runs on its own thread — the accept loop waits for no window.
serveAs :: String -> ServeOptions -> IO () -> IO ()
serveAs mode opts listening = do
  ok <- doesDirectoryExist (soDir opts)
  unless ok (die ("glance " <> mode <> ": no such directory: " <> soDir opts))
  assets <- hasRenderer opts
  started <- getMonotonicTime
  hub <- newLoadingHub started
  loader <- forkIO (indexTree opts hub started)
  Warp.runSettings (settings (announce assets)) (application opts hub)
    `finally` killThread loader
  where
    settings ready = Warp.setHost "127.0.0.1"
                   . Warp.setPort (soPort opts)
                   . Warp.setBeforeMainLoop ready
                   $ Warp.defaultSettings
    announce assets = do
      say (bannerLines mode opts assets)
      void (forkIO listening)

-- | What OPTS announces at startup under MODE, ASSETS saying whether the
-- renderer was found.  Pure, the way @Glance.Desktop@'s @--dry-run@ lines are:
-- what the operator is told is worth a test that runs no server.
bannerLines :: String -> ServeOptions -> Bool -> [String]
bannerLines mode opts assets =
  [ "glance " <> mode <> " — http://127.0.0.1:" <> show (soPort opts) <> "/"
  , "  org dir: " <> soDir opts
  , "  assets:  " <> case soAssets opts of
      Nothing  -> "compiled in (--assets serves a directory instead)"
      Just dir -> dir <> if assets then "" else "  (missing — /headlines only)"
  , "  live:    ws://127.0.0.1:" <> show (soPort opts) <> "/ws, watching " <> soDir opts
  , "  bound to 127.0.0.1; no auth tier before S7."
  , "  indexing — /headlines, /headline and /ws answer 503 until the walk lands."
  ]

-- | Walk and parse OPTS's directory into HUB, then watch it.  Runs off the
-- main thread from STARTED, and keeps the two in order: the watch's first
-- event must land on a store the walk has finished building, or a re-parse
-- would be folded into a store that is about to be replaced wholesale.
indexTree :: ServeOptions -> Hub -> Double -> IO ()
indexTree opts hub started = do
  store <- loadStoreWith (walkFor opts) (soDir opts)
  loaded <- getMonotonicTime
  finishLoading hub store
  let stats = storeResult store
  say
    [ "  loaded:  " <> show (length (qrRecords stats)) <> " rows from "
        <> show (qrFiles stats) <> " files in " <> seconds (loaded - started)
        <> collisionNote (qrIdCollisions stats)
    -- Where `+' would write, said once at startup rather than discovered on the
    -- first capture: a target this daemon will not write to is a misconfigured
    -- tree, and the operator learns it here.
    , "  capture: " <> either (\why -> T.unpack why <> " — + is refused until it moves")
                              id (captureTargetIn (soDir opts) (stConfig store))
    ]
  watchOrgTree (walkFor opts) (soDir opts) hub

-- | What to say about CLASHES on the startup banner: nothing when there are
-- none, and the count with one example when there are — two files claiming one
-- id is a tree worth looking at, and the header carries the number for a client.
collisionNote :: [IdCollision] -> String
collisionNote [] = ""
collisionNote (c : rest) =
  ", " <> show (length rest + 1) <> " id collision" <> plural
    <> " (" <> T.unpack (icId c) <> ": kept " <> icKept c <> ", dropped " <> icDropped c <> ")"
  where plural = if null rest then "" else "s"

-- | S to a tenth of a second, which is the resolution a startup banner earns.
seconds :: Double -> String
seconds s = show (tenths s) <> " s"
