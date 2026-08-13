-- | The web layer: the daemon — bind, walk, watch — and the door the rest of the program comes through.  It binds 127.0.0.1 alone.
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
import Glance.Web.Store ( Hub, finishLoading, loadStoreWith
                        , newLoadingHub, storeResult )
import Glance.Web.Watch (say, watchOrgTree)


-- | Serve OPTS until killed.
serve :: ServeOptions -> IO ()
serve opts = serveAs "serve" opts (pure ())

-- | Serve OPTS until killed, running LISTENING once the socket is bound.  MODE is the subcommand that asked; a missing org directory fails here.
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

-- | What OPTS announces at startup under MODE, ASSETS saying whether the renderer was found.
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

-- | Walk and parse OPTS's directory into HUB, then watch it: the watch's first event must land on a store the walk has finished building.
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
    , "  capture: " <> captureTargetIn (soDir opts)
    ]
  watchOrgTree (walkFor opts) (soDir opts) hub

-- | What to say about CLASHES on the startup banner: the count with one example.
collisionNote :: [IdCollision] -> String
collisionNote [] = ""
collisionNote (c : rest) =
  ", " <> show (length rest + 1) <> " id collision" <> plural
    <> " (" <> T.unpack (icId c) <> ": kept " <> icKept c <> ", dropped " <> icDropped c <> ")"
  where plural = if null rest then "" else "s"

-- | S to a tenth of a second, which is the resolution a startup banner earns.
seconds :: Double -> String
seconds s = show (tenths s) <> " s"
