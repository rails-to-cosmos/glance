-- | The M1 web layer: headlines out of a directory, into a browser tab, and
-- kept current there.
--
-- This component's build-depends names the public @glance@ library and the
-- HTTP packages.  @glance-internal@ is absent, so @Data.Org.*@ is out of scope
-- here and reaching for it means writing the dependency down where anyone
-- reading the stanza sees it.  That is the facade invariant
-- (docs/invariants.md, Architecture), kept where the solver can check it.
--
-- Four routes: @GET \/headlines@ is the view JSON, @GET \/@ a demo shell that
-- fetches it, @GET \/ws@ the live row stream, and @GET \/NAME@ an asset out of
-- the @--assets@ directory.  The view's field set is the contract
-- (@table-view\/SCHEMA.md@), so the load counts ride along as @X-Glance-*@
-- response headers and leave the body's shape alone.
--
-- The directory is parsed once, at startup, into 'Glance.Web.Store.Store';
-- every request renders that, and a file watcher re-parses one file per edit
-- (S5).  Org files stay the single source of truth — the store is a projection
-- that dies with the process.
--
-- A page mounts in two steps: @\/headlines@ for the columns and the sort, then
-- the socket's opening @set-rows@ for the rows.  The bootstrap frame is taken
-- in the transaction that subscribes, so an edit landing between the fetch and
-- the socket is in the bootstrap rather than lost, and the server needs no
-- journal to catch a client up.
--
-- The listener binds 127.0.0.1 and nothing else.  Read, write and automate
-- tiers arrive at S7 (docs\/plan-org-console-web.md); until an unauthenticated
-- connection is a read-only one by construction, the loopback interface is the
-- whole access-control story.
module Glance.Web ( ServeOptions (..)
                  , defaultAssetsDir
                  , defaultPort
                  , application
                  , serve
                  , viewTitleFor
                  ) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (forever, unless, void)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( Header, Status, hContentType, methodGet, methodHead
                          , status200, status400, status404, status405, status500 )
import Network.Wai ( Application, Request (pathInfo, requestMethod), Response
                   , responseFile, responseLBS )
import Network.Wai.Handler.WebSockets (websocketsOr)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import System.FilePath (takeExtension, (</>))
import System.IO (hFlush, stdout)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

import Glance.Query (QueryResult (..), viewJSONText)
import Glance.Web.Store ( Client, Frame (ViewChanged), Hub, frameText, hubStore
                        , loadStore, newHub, nextFrame, storeResult, subscribe
                        , unsubscribe )
import Glance.Web.Watch (watchOrgTree)

-- Options

-- | What one server serves.
data ServeOptions = ServeOptions
  { soDir    :: !FilePath  -- ^ org root, walked once at startup and watched after.
  , soPort   :: !Int       -- ^ loopback port to listen on.
  , soAssets :: !FilePath  -- ^ directory holding @table-view.js@; see 'defaultAssetsDir'.
  } deriving (Eq, Show)

defaultPort :: Int
defaultPort = 7777

-- | Where @table-view.js@ lives when @--assets@ says nothing: the sibling
-- checkout.  A missing directory costs the demo page and nothing else, so the
-- absolute path is a convenience rather than a requirement.  S4 gives the
-- renderer a fixtures dir of its own and this default a better home.
defaultAssetsDir :: FilePath
defaultAssetsDir = "/home/akatovda/sync/stuff/table-view/web"

-- | The asset the demo shell loads; its presence decides which page @\/@ serves.
rendererAsset :: FilePath
rendererAsset = "table-view.js"

-- Server

-- | Serve OPTS until killed.  A missing org directory fails here rather than
-- per request: the operator learns at startup, not from a 500.  The one full
-- parse happens here too, before the socket opens, so a request never waits
-- for a walk.
serve :: ServeOptions -> IO ()
serve opts = do
  ok <- doesDirectoryExist (soDir opts)
  unless ok (die ("glance serve: no such directory: " <> soDir opts))
  assets <- hasRenderer opts
  started <- getMonotonicTime
  store <- loadStore (soDir opts)
  loaded <- getMonotonicTime
  let stats = storeResult store
  hub <- newHub store
  mapM_ putStrLn
    [ "glance serve — http://127.0.0.1:" <> show (soPort opts) <> "/"
    , "  org dir: " <> soDir opts
    , "  assets:  " <> soAssets opts <> if assets then "" else "  (missing — /headlines only)"
    , "  loaded:  " <> show (length (qrRecords stats)) <> " rows from "
                    <> show (qrFiles stats) <> " files in " <> seconds (loaded - started)
    , "  live:    ws://127.0.0.1:" <> show (soPort opts) <> "/ws, watching " <> soDir opts
    , "  bound to 127.0.0.1; no auth tier before S7."
    ]
  -- Redirected stdout is block-buffered, and the process then blocks in warp
  -- until it is killed: without this the banner never reaches the log.
  hFlush stdout
  watcher <- forkIO (watchOrgTree (soDir opts) hub)
  Warp.runSettings settings (application opts hub) `finally` killThread watcher
  where settings = Warp.setHost "127.0.0.1"
                 . Warp.setPort (soPort opts)
                 $ Warp.defaultSettings

-- | S to a tenth of a second, which is the resolution a startup banner earns.
seconds :: Double -> String
seconds s = show (fromIntegral (round (s * 10) :: Int) / 10 :: Double) <> " s"

-- | Does the assets directory hold the renderer?  Checked per request as well
-- as at startup, so pointing @--assets@ at a directory that fills up later
-- needs no restart.
hasRenderer :: ServeOptions -> IO Bool
hasRenderer opts = doesFileExist (soAssets opts </> rendererAsset)

-- | OPTS over HUB as a WAI application.  Exported for the suite: the routes
-- are tested through this, with no socket bound.  A request that is not a
-- websocket upgrade never reaches 'liveSocket', which is why
-- 'Network.Wai.Test' can drive this unchanged.
application :: ServeOptions -> Hub -> Application
application opts hub =
  websocketsOr WS.defaultConnectionOptions (liveSocket hub) (httpApp opts hub)

-- | Everything that is not a websocket upgrade: the three GET routes and the
-- 405 for anything that would write.
httpApp :: ServeOptions -> Hub -> Application
httpApp opts hub request respond
  | requestMethod request `notElem` [methodGet, methodHead] =
      respond (plain status405 "method not allowed; this view is read-only until S7")
  | otherwise = route (pathInfo request) >>= respond
  where
    route []             = shellPage opts
    route ["headlines"]  = headlines opts hub
    route ["ws"]         = pure (plain status400 wsHint)
    route [name]
      | safeName name    = asset opts (T.unpack name)
    route _              = pure (plain status404 "not found: /, /headlines, /ws, or an asset name")
    wsHint = "/ws is a websocket endpoint; connect with Upgrade: websocket"

-- | Is NAME a plain file name, safe to look up inside the assets directory?
-- 'pathInfo' has split the separators away already; what is left to reject is
-- the traversal pair and the empty segment.
safeName :: Text -> Bool
safeName name = not (T.null name)
             && name `notElem` [".", ".."]
             && not (T.any (`elem` ("/\\" :: String)) name)

-- Routes

-- | The view JSON for the configured directory, with the load counts as
-- headers.  Rendered from the store rather than from a fresh walk: the rows
-- are the ones the startup parse produced, kept current by the watcher, so the
-- response costs an encode instead of a directory walk.
headlines :: ServeOptions -> Hub -> IO Response
headlines opts hub = do
  qr <- storeResult <$> readTVarIO (hubStore hub)
  let body = TLE.encodeUtf8 (viewJSONText (viewTitleFor dir) (qrRecords qr))
  -- The encode is lazy, so it needs its own 'try': an exception raised inside
  -- warp's sender would truncate a 200 that has already gone out.
  forced <- try (evaluate (BL.length body))
  pure $ case forced of
    Left err -> plain status500 (renderError err)
    Right _n -> responseLBS status200 (jsonType : statsHeaders qr) body
  where dir = soDir opts
        renderError :: SomeException -> Text
        renderError e = "headline render failed: " <> T.pack (displayException e)

-- Live socket

-- | @\/ws@: one @set-rows@ with everything the store holds, then a frame per
-- change.  Anything else is refused, so a mistyped path fails loudly rather
-- than sitting open sending nothing.
liveSocket :: Hub -> WS.ServerApp
liveSocket hub pending
  | wsPath == "/ws" = do
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $ do
        (cid, client, boot) <- atomically (subscribe hub)
        send conn boot
        pump conn client `finally` unsubscribe hub cid
  | otherwise = WS.rejectRequest pending "glance streams rows at /ws"
  where wsPath = BSC.takeWhile (/= '?') (WS.requestPath (WS.pendingRequest pending))

-- | Feed CLIENT's mailbox to CONN until one of them ends it.  The socket is
-- also drained in the background: nothing arrives on it (the view is read-only
-- until S7), but a reader is what notices a browser closing the tab, and it is
-- what answers the protocol's own control frames.
pump :: WS.Connection -> Client -> IO ()
pump conn client = do
  ended <- newEmptyMVar
  reader <- forkIO (drainSocket conn `finally` void (tryPutMVar ended ()))
  writer <- forkIO (feed `finally` void (tryPutMVar ended ()))
  takeMVar ended `finally` (killThread reader >> killThread writer)
  where
    feed = do
      next <- atomically (nextFrame client)
      case next of
        -- The mailbox filled: this client stopped reading, and the watcher
        -- will not wait for it (Glance.Web.Store).  Closing makes it resync.
        Nothing          -> WS.sendClose conn ("dropped-slow-client" :: Text)
        -- The columns moved, and SCHEMA.md streams rows only.  Reconnecting
        -- re-fetches /headlines, which is where columns come from.
        Just ViewChanged -> WS.sendClose conn ("view-changed" :: Text)
        Just frame       -> send conn frame >> feed

-- | FRAME down CONN, when it is one of the ops that travels as a message.
send :: WS.Connection -> Frame -> IO ()
send conn = mapM_ (WS.sendTextData conn) . frameText

-- | Read and discard, until the peer goes away.
drainSocket :: WS.Connection -> IO ()
drainSocket conn = forever (void (WS.receiveDataMessage conn))

-- | The view title for DIR: what the browser tab and the table heading show.
-- Exported so the suite renders the same document the server does.
viewTitleFor :: FilePath -> Text
viewTitleFor dir = T.pack dir <> " — glance"

-- | What the load covered, for a client that wants to know how much of the
-- directory the rows account for.  Headers rather than a @meta@ sibling: the
-- View object's fields are SCHEMA.md's, and a producer does not add to them.
statsHeaders :: QueryResult -> [Header]
statsHeaders qr =
  [ count "X-Glance-Rows"            (length (qrRecords qr))
  , count "X-Glance-Files"           (qrFiles qr)
  , count "X-Glance-Parse-Failures"  (qrParseFailures qr)
  , count "X-Glance-Decode-Failures" (qrDecodeFailures qr)
  , count "X-Glance-Read-Failures"   (qrReadFailures qr)
  ]
  where count name n = (name, BSC.pack (show n))

-- | An asset out of the configured directory, or a 404 naming what was looked
-- for.  Only files directly in it are reachable — one segment, no traversal.
-- Every one-segment path lands here, so the miss doubles as the route list:
-- @\/graph@ is a mistyped route rather than a missing file, and reads better
-- when told so.
asset :: ServeOptions -> FilePath -> IO Response
asset opts name = do
  ok <- doesFileExist path
  pure $ if ok
    then responseFile status200 [(hContentType, mimeOf name)] path Nothing
    else plain status404 (T.intercalate "\n"
           [ "no such asset: " <> T.pack path
           , "this server serves /, /headlines, and file names under "
               <> T.pack (soAssets opts) ])
  where path = soAssets opts </> name

-- | Content types for what the renderer ships with.  Everything else is
-- served as bytes; guessing wider would be guessing.
mimeOf :: FilePath -> BSC.ByteString
mimeOf name = case takeExtension name of
  ".js"    -> "text/javascript; charset=utf-8"
  ".mjs"   -> "text/javascript; charset=utf-8"
  ".css"   -> "text/css; charset=utf-8"
  ".html"  -> "text/html; charset=utf-8"
  ".json"  -> "application/json; charset=utf-8"
  ".svg"   -> "image/svg+xml"
  ".png"   -> "image/png"
  ".ico"   -> "image/x-icon"
  ".woff2" -> "font/woff2"
  _        -> "application/octet-stream"

-- Pages

-- | @\/@: the demo shell when the renderer is on disk, and an explanation when
-- it is not.  A missing renderer leaves @\/headlines@ untouched — the server
-- is a JSON server that happens to ship a page.
shellPage :: ServeOptions -> IO Response
shellPage opts = do
  ok <- hasRenderer opts
  pure (html (if ok then demoShell opts else assetsMissing opts))

-- | The page a browser gets: load the renderer, fetch the view, mount it, then
-- hold a socket open and apply what it sends.  The glue is inline so the shell
-- has exactly one asset to find.
--
-- The mount is two-step on purpose.  @\/headlines@ gives the columns, the sort
-- and a set of rows; the socket then opens with a @set-rows@ of its own, which
-- is what the store held at the moment of subscription.  Applying the second
-- over the first closes the gap between the two requests without the server
-- keeping a journal.
--
-- Every close leads back through the same door: re-fetch, re-mount, reconnect.
-- That covers a daemon restart, a dropped slow client, and @view-changed@ —
-- the columns moving, which SCHEMA.md's row ops cannot express.
demoShell :: ServeOptions -> Text
demoShell opts = page (viewTitleFor (soDir opts)) $ T.unlines
  [ "  <h1>" <> escape (viewTitleFor (soDir opts)) <> "<span id=\"dot\"></span></h1>"
  , "  <div id=\"app\"></div>"
  , "  <div id=\"log\">loading …</div>"
  , "  <script src=\"" <> T.pack rendererAsset <> "\"></script>"
  , "  <script>"
  , "    const log = (m) => (document.getElementById(\"log\").textContent = m);"
  , "    const dot = (state) => (document.getElementById(\"dot\").className = state);"
  , "    let table = null, socket = null, backoff = 1000;"
  , "    function mount(view) {"
  , "      table = TableView.mount(document.getElementById(\"app\"), view, {"
  , "        onAction: (command, id) => log(`action: ${command}  id=${id}`),"
  , "        onLink: (target) => log(`link: ${target}`),"
  , "      });"
  , "      log(`${(view.rows || []).length} headlines · click a column to sort`);"
  , "    }"
  , "    function apply(frame) {"
  , "      if (!table) return;"
  , "      if (frame.op === \"set-rows\") table.setRows(frame.rows);"
  , "      else if (frame.op === \"upsert-row\") table.upsertRow(frame.row);"
  , "      else if (frame.op === \"delete-row\") table.deleteRow(frame.id);"
  , "      else if (frame.op === \"apply-delta\") table.applyDelta(frame.ops);"
  , "      log(`${table.getRows().length} headlines · live`);"
  , "    }"
  , "    function listen() {"
  , "      const scheme = location.protocol === \"https:\" ? \"wss\" : \"ws\";"
  , "      socket = new WebSocket(`${scheme}://${location.host}/ws`);"
  , "      socket.onopen = () => { backoff = 1000; dot(\"live\"); };"
  , "      socket.onmessage = (e) => apply(JSON.parse(e.data));"
  , "      socket.onclose = () => { socket = null; dot(\"down\"); again(); };"
  , "    }"
  , "    function again() {"
  , "      log(`disconnected · retrying in ${Math.round(backoff / 1000)}s`);"
  , "      setTimeout(start, backoff);"
  , "      backoff = Math.min(backoff * 2, 30000);"
  , "    }"
  , "    function start() {"
  , "      fetch(\"/headlines\")"
  , "        .then((r) => r.ok ? r.json()"
  , "                          : r.text().then((t) => { throw new Error(t); }))"
  , "        .then((view) => { mount(view); listen(); })"
  , "        .catch((e) => { dot(\"down\"); log(`load failed: ${e.message}`); again(); });"
  , "    }"
  , "    start();"
  , "  </script>"
  ]

-- | The page a browser gets when the renderer is missing: what still works,
-- and the flag that fixes it.
assetsMissing :: ServeOptions -> Text
assetsMissing opts = page "glance — JSON only" $ T.unlines
  [ "  <h1>glance — JSON-only mode</h1>"
  , "  <p>No <code>" <> T.pack rendererAsset <> "</code> under <code>"
      <> escape (T.pack (soAssets opts)) <> "</code>, so there is no table to"
      <> " render here. The server is otherwise complete:</p>"
  , "  <p><code>curl -s localhost:" <> T.pack (show (soPort opts))
      <> "/headlines | jq '.rows | length'</code></p>"
  , "  <p>Point <code>--assets</code> at a directory holding <code>"
      <> T.pack rendererAsset <> "</code> (the <code>web/</code> directory of a"
      <> " table-view checkout) and reload:</p>"
  , "  <p><code>glance serve --dir " <> escape (T.pack (soDir opts))
      <> " --assets /path/to/table-view/web</code></p>"
  ]

-- | BODY wrapped in a document titled TITLE.  Styles inline, no asset but the
-- renderer, dark and light both.
page :: Text -> Text -> Text
page title body = T.unlines
  [ "<!doctype html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>" <> escape title <> "</title>"
  , "<style>"
  , "  body{margin:0;font:14px/1.5 system-ui,sans-serif;background:#eceef2;color:#1c1e26;"
  , "    padding:24px;display:flex;flex-direction:column;gap:14px}"
  , "  @media (prefers-color-scheme:dark){body{background:#13141c;color:#c8ccd4}}"
  , "  h1{font-size:16px;margin:0}"
  , "  p{margin:0;max-width:70ch}"
  , "  code{font:12px ui-monospace,monospace;opacity:.9}"
  , "  #app{height:80vh}"
  , "  #log{font:12px ui-monospace,monospace;opacity:.75;min-height:1.4em}"
  , "  #dot{display:inline-block;width:7px;height:7px;border-radius:50%;"
  , "    margin-left:8px;vertical-align:middle;background:#9aa0ad;transition:background .3s}"
  , "  #dot.live{background:#9ece6a}"
  , "  #dot.down{background:#9aa0ad}"
  , "</style>"
  , "</head>"
  , "<body>"
  , body <> "</body>"
  , "</html>"
  ]

-- | T with the five characters that would leave text mode escaped.
escape :: Text -> Text
escape = T.concatMap esc
  where esc '&'  = "&amp;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '"'  = "&quot;"
        esc '\'' = "&#39;"
        esc c    = T.singleton c

-- Responses

jsonType :: Header
jsonType = (hContentType, "application/json; charset=utf-8")

html :: Text -> Response
html body = responseLBS status200 [(hContentType, "text/html; charset=utf-8")] (utf8 body)

-- | STATUS with MSG as its whole body — errors read in a terminal as well as
-- in a browser.
plain :: Status -> Text -> Response
plain status msg =
  responseLBS status [(hContentType, "text/plain; charset=utf-8")] (utf8 (msg <> "\n"))

utf8 :: Text -> BL.ByteString
utf8 = BL.fromStrict . TE.encodeUtf8
