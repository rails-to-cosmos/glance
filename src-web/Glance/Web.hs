-- | The M0 web layer: headlines out of a directory, into a browser tab.
--
-- This component's build-depends names the public @glance@ library and the
-- HTTP packages.  @glance-internal@ is absent, so @Data.Org.*@ is out of scope
-- here and reaching for it means writing the dependency down where anyone
-- reading the stanza sees it.  That is the facade invariant
-- (docs/invariants.md, Architecture), kept where the solver can check it.
--
-- Three routes: @GET \/headlines@ is the view JSON, @GET \/@ a demo shell that
-- fetches it, and @GET \/NAME@ an asset out of the @--assets@ directory.  The
-- view's field set is the contract (@table-view\/SCHEMA.md@), so the load
-- counts ride along as @X-Glance-*@ response headers and leave the body's
-- shape alone.
--
-- The store is parsed per request.  M0 wants no second authoritative store,
-- and persistence returns only on the plan's trigger metric (full-store parse
-- > 1 s); until then a request costs one walk.
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

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Text (Text)
import Network.HTTP.Types ( Header, Status, hContentType, methodGet, methodHead
                          , status200, status404, status405, status500 )
import Network.Wai ( Application, Request (pathInfo, requestMethod), Response
                   , responseFile, responseLBS )
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

import Glance.Query (QueryResult (..), loadDir, viewJSONText)

-- Options

-- | What one server serves.
data ServeOptions = ServeOptions
  { soDir    :: !FilePath  -- ^ org root, walked afresh per request.
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
-- per request: the operator learns at startup, not from a 500.
serve :: ServeOptions -> IO ()
serve opts = do
  ok <- doesDirectoryExist (soDir opts)
  unless ok (die ("glance serve: no such directory: " <> soDir opts))
  assets <- hasRenderer opts
  mapM_ putStrLn
    [ "glance serve — http://127.0.0.1:" <> show (soPort opts) <> "/"
    , "  org dir: " <> soDir opts
    , "  assets:  " <> soAssets opts <> if assets then "" else "  (missing — /headlines only)"
    , "  bound to 127.0.0.1; no auth tier before S7."
    ]
  -- Redirected stdout is block-buffered, and the process then blocks in warp
  -- until it is killed: without this the banner never reaches the log.
  hFlush stdout
  Warp.runSettings settings (application opts)
  where settings = Warp.setHost "127.0.0.1"
                 . Warp.setPort (soPort opts)
                 $ Warp.defaultSettings

-- | Does the assets directory hold the renderer?  Checked per request as well
-- as at startup, so pointing @--assets@ at a directory that fills up later
-- needs no restart.
hasRenderer :: ServeOptions -> IO Bool
hasRenderer opts = doesFileExist (soAssets opts </> rendererAsset)

-- | OPTS as a WAI application.  Exported for the suite: the routes are tested
-- through this, with no socket bound.
application :: ServeOptions -> Application
application opts request respond
  | requestMethod request `notElem` [methodGet, methodHead] =
      respond (plain status405 "method not allowed; this view is read-only until S7")
  | otherwise = route (pathInfo request) >>= respond
  where
    route []             = shellPage opts
    route ["headlines"]  = headlines opts
    route [name]
      | safeName name    = asset opts (T.unpack name)
    route _              = pure (plain status404 "not found: /, /headlines, or an asset name")

-- | Is NAME a plain file name, safe to look up inside the assets directory?
-- 'pathInfo' has split the separators away already; what is left to reject is
-- the traversal pair and the empty segment.
safeName :: Text -> Bool
safeName name = not (T.null name)
             && name `notElem` [".", ".."]
             && not (T.any (`elem` ("/\\" :: String)) name)

-- Routes

-- | The view JSON for the configured directory, with the load counts as
-- headers.  A load that throws becomes a 500 carrying the reason: the walk
-- swallows per-file failures into counts, so anything arriving here is the
-- directory itself going wrong mid-flight.
headlines :: ServeOptions -> IO Response
headlines opts = do
  loaded <- try (loadDir dir)
  case loaded of
    Left err -> pure (plain status500 (loadError err))
    Right qr -> do
      let body = TLE.encodeUtf8 (viewJSONText (viewTitleFor dir) (qrRecords qr))
      -- The encode is lazy, so it needs its own 'try': an exception raised
      -- inside warp's sender would truncate a 200 that has already gone out.
      forced <- try (evaluate (BL.length body))
      pure $ case forced of
        Left err -> plain status500 (loadError err)
        Right _n -> responseLBS status200 (jsonType : statsHeaders qr) body
  where dir = soDir opts
        loadError :: SomeException -> Text
        loadError e = "headline load failed: " <> T.pack (displayException e)

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

-- | The page a browser gets: load the renderer, fetch the view, mount it.
-- The glue is inline so the shell has exactly one asset to find.
demoShell :: ServeOptions -> Text
demoShell opts = page (viewTitleFor (soDir opts)) $ T.unlines
  [ "  <h1>" <> escape (viewTitleFor (soDir opts)) <> "</h1>"
  , "  <div id=\"app\"></div>"
  , "  <div id=\"log\">loading …</div>"
  , "  <script src=\"" <> T.pack rendererAsset <> "\"></script>"
  , "  <script>"
  , "    const log = (m) => (document.getElementById(\"log\").textContent = m);"
  , "    fetch(\"/headlines\")"
  , "      .then((r) => r.ok ? r.json()"
  , "                        : r.text().then((t) => { throw new Error(t); }))"
  , "      .then((view) => {"
  , "        TableView.mount(document.getElementById(\"app\"), view, {"
  , "          onAction: (command, id) => log(`action: ${command}  id=${id}`),"
  , "          onLink: (target) => log(`link: ${target}`),"
  , "        });"
  , "        log(`${(view.rows || []).length} headlines · click a column to sort`);"
  , "      })"
  , "      .catch((e) => log(`load failed: ${e.message}`));"
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
