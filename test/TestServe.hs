-- | The server, driven as a WAI 'Application'.  No socket is bound: every case
-- here is a request handed straight to the app, so the suite stays free of
-- ports and of the races that come with them.  The websocket route is the one
-- thing an upgrade-less request cannot reach, and the frames it would carry
-- are TestStore's subject.
module TestServe (spec) where

import Data.Aeson ( Value (Bool, Number, Object, String)
                  , eitherDecode, encode, object, parseJSON, (.=) )
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.List (find, nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( HeaderName, RequestHeaders, methodDelete, methodPost
                          , renderQuery, statusCode )
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SRequest (SRequest)
                        , SResponse (simpleBody, simpleHeaders, simpleStatus)
                        , request, runSession, setPath, srequest )
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( document, field, intAt, listAt, maybeTextAt, membersAt, orgFile
                    , textAt, textsAt, viewDir, withTempDir )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Data.Org.Edit (snapDigest, snapshotOf)
import Glance.Query (QueryResult (qrRecords), loadDir, loadFile, viewJSON)
import Glance.Web ( ServeOptions (..), application, bootstrapWanted, defaultPort
                  , viewTitleFor )
import Glance.Web.Store ( Hub, applyFile, finishLoading, loadStore, newHub
                       , newLoadingHub, publish )

-- Fixtures
--
-- 'viewDir' is the directory TestQuery loads: one sample document and one that
-- is not UTF-8, six headlines between them.

-- | The document those six headlines come from.
sampleFile :: FilePath
sampleFile = viewDir <> "/sample.org"

-- | @sha256sum test\/fixtures\/view\/sample.org@.  Written down rather than
-- computed here: the digest the server hands out is what a client pins its
-- edit to, and an oracle that runs the same code as the server proves nothing
-- about it.
sampleDigest :: T.Text
sampleDigest = "0de46a0cceb1b1b30364c0bba0107e63bbd2c9b504d1e2bf31f29321f1ff2493"

-- | A file whose first headline is materialized, edited and written back.  The
-- id is in the drawer, so it stays the same across the temp directory's name
-- and across the edit.
committable :: T.Text
committable = T.unlines
  [ "#+CATEGORY: notes"
  , "* TODO First :one:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: first"
  , ":END:"
  , "body of first"
  , "* TODO Second"
  , "tail"
  ]

-- | An assets directory holding a stub renderer.
assetsDir :: FilePath
assetsDir = "test/fixtures/assets"

-- | A path with no renderer under it, and no directory either.
missingAssetsDir :: FilePath
missingAssetsDir = "test/fixtures/assets-not-here"

served :: FilePath -> ServeOptions
served assets = ServeOptions { soDir = viewDir, soPort = defaultPort, soAssets = assets
                             , soDerived = False }

-- | The app a server with ASSETS runs, over a store loaded the way 'serve'
-- loads one.  A fresh store per request is the suite's convenience; the server
-- keeps one for its lifetime.
app :: FilePath -> IO Application
app assets = application (served assets) <$> (newHub =<< loadStore viewDir)

-- | A server over DIR: the app, and the hub whose store it answers from — the
-- write cases look at that store afterwards to show the route left it alone.
serverOver :: FilePath -> IO (Application, Hub)
serverOver dir = do
  hub <- newHub =<< loadStore dir
  pure (application (served assetsDir) { soDir = dir } hub, hub)

-- | GET PATH from a server configured with ASSETS.
get :: FilePath -> ByteString -> IO SResponse
get assets path = do
  application' <- app assets
  getFrom application' path

-- | GET PATH from APPLICATION'.
getFrom :: Application -> ByteString -> IO SResponse
getFrom application' path = getWith application' path []

-- | GET PATH from APPLICATION', sending HEADERS — the conditional and the
-- content-negotiation cases are all one request header apart.
getWith :: Application -> ByteString -> RequestHeaders -> IO SResponse
getWith application' path headers =
  runSession (request (setPath defaultRequest path) { requestHeaders = headers }) application'

-- | POST PAYLOAD to PATH on APPLICATION', as JSON.
postTo :: Application -> ByteString -> BL.ByteString -> IO SResponse
postTo application' path payload = runSession (srequest (SRequest req payload)) application'
  where req = (setPath defaultRequest path)
                { requestMethod  = methodPost
                , requestHeaders = [("Content-Type", "application/json")] }

-- | @\/headline?id=…@ with ID percent-encoded, the way a client builds it: a
-- row id is @FILE:START@ and carries both separators the path would fight over.
headlinePath :: T.Text -> ByteString
headlinePath rid = "/headline" <> renderQuery True [("id", Just (TE.encodeUtf8 rid))]

-- | A commit body: the subtree text and the digest it was materialized with.
commitBody :: T.Text -> T.Text -> BL.ByteString
commitBody org digest = encode (object ["org" .= org, "digest" .= digest])

-- | Run K over a server holding 'committable' with its first headline already
-- materialized: the app, the file on disk, and the materialize response a
-- commit to it has to present back.  Six cases opened with those five lines.
withCommitted :: (Application -> FilePath -> Value -> Assertion) -> Assertion
withCommitted k = withTempDir $ \dir -> do
  path <- orgFile dir "notes.org" committable
  (a, _hub) <- serverOver dir
  v <- getFrom a (headlinePath "first") >>= decoded
  k a path v

-- Assertions

status :: SResponse -> Int
status = statusCode . simpleStatus

header :: HeaderName -> SResponse -> Maybe ByteString
header name r = lookup name (simpleHeaders r)

body :: SResponse -> T.Text
body = TE.decodeUtf8 . BL.toStrict . simpleBody

assertContains :: String -> T.Text -> T.Text -> Assertion
assertContains what needle haystack =
  assertBool (what <> ": no " <> show needle <> " in " <> show (T.take 400 haystack))
             (needle `T.isInfixOf` haystack)

-- | WHAT: every one of NEEDLES is in HAYSTACK.
holdsAll :: String -> [T.Text] -> T.Text -> Assertion
holdsAll what needles haystack = mapM_ (\n -> assertContains what n haystack) needles

-- | WHAT: none of NEEDLES is in HAYSTACK.  Each of them names a design the page
-- superseded, so one coming back means two are live at once.
holdsNone :: String -> [T.Text] -> T.Text -> Assertion
holdsNone what needles haystack =
  mapM_ (\n -> assertBool (what <> ": " <> show n <> " survives in the page")
                          (not (n `T.isInfixOf` haystack)))
        needles

-- | R's body as a JSON 'Value', or the decode error as a test failure.
decoded :: SResponse -> IO Value
decoded r = either (\e -> assertFailure ("response JSON: " <> e)) pure
                   (eitherDecode (simpleBody r))

-- | R's @rows@ array.
rowsOf :: SResponse -> IO [Value]
rowsOf r = listAt "rows" =<< decoded r

-- | ROW's @id@, or the whole row when it has none — a failure that reads.
rowId :: Value -> T.Text
rowId row = case row of
  Object o -> case KM.lookup "id" o of
    Just (String i) -> i
    _noId           -> T.pack (show row)
  _notARow -> T.pack (show row)

-- | ROW's @scheduled@ cell, empty when it has none.  The key the view declares
-- its sort on, so a page has to come out of this order.
scheduledOf :: Value -> T.Text
scheduledOf row = case row of
  Object o -> case KM.lookup "cells" o of
    Just (Object cells) -> case KM.lookup "scheduled" cells of
      Just (String s) -> s
      _unscheduled    -> ""
    _noCells -> ""
  _notARow -> ""

-- | The state column's badge values, in palette order.
badgeValues :: Value -> IO [T.Text]
badgeValues view = do
  cols <- listAt "columns" view
  state <- maybe (assertFailure "no state column") pure
                 (find (keyIs "state") cols)
  traverse (textAt "value") =<< listAt "badges" state
  where keyIs k (Object o) = KM.lookup "key" o == Just (String k)
        keyIs _ _notAColumn = False

-- | What sits between OPEN and CLOSE in HAYSTACK, when both are in it.
between :: T.Text -> T.Text -> T.Text -> Maybe T.Text
between open close haystack
  | T.null after = Nothing
  | T.null rest  = Nothing
  | otherwise    = Just inner
  where (_before, after) = T.breakOn open haystack
        (inner, rest)    = T.breakOn close (T.drop (T.length open) after)

-- | The SHA-256 of the bytes PATH holds now.  Taken off the file rather than
-- through a load: the digest a receipt reports is what a client pins its next
-- edit to, and an oracle that re-runs the loader agrees with it whatever the
-- loader digested.
digestOnDisk :: FilePath -> IO T.Text
digestOnDisk path = snapDigest . snapshotOf path <$> document path

-- Spec
--
-- One shell is rendered for the whole group and handed to every case that reads
-- it ('withResource'): the page is a pure function of the options and the
-- store, and thirty-odd cases were each loading the fixture directory again to
-- get the same string back.

spec :: TestTree
spec = withResource (body <$> get assetsDir "/") (const (pure ())) $ \shell ->
  testGroup "Serve"
    [ headlineSpec, statsSpec, cacheSpec, gzipSpec, querySpec, bootstrapSpec
    , materializeSpec, commitSpec, indexingSpec, pageSpec shell, keymapSpec shell
    , glueSpec shell, touchSpec shell, shellFontSpec shell, assetSpec, errorSpec ]

-- | A claim about a page this server serves: strings it must carry, and strings
-- it must not.
data Glue = Glue { glLabel :: String, glHas :: [T.Text], glGone :: [T.Text] }

-- | A claim with nothing to forbid.
glue :: String -> [T.Text] -> Glue
glue label has = Glue label has []

-- | The shell's inline glue, checked as the data it is.  Every row is a case
-- that read the same rendered page and asserted a list of strings into it; as
-- rows they share the render and the two assertion shapes, and adding one is
-- adding a line.
glueSpec :: IO T.Text -> TestTree
glueSpec shell = testGroup "Shell glue"
  [ testCase glLabel $ do
      b <- shell
      holdsAll glLabel glHas b
      holdsNone glLabel glGone b
  | Glue{..} <- shellGlue ]

shellGlue :: [Glue]
shellGlue =
  [ glue "paints a page and loads the rest behind it"
      [ "const PAGE = 100;", "load(`${narrow}limit=${PAGE}`)"
      , "r.headers.get(\"X-Glance-Total\")", "a.total > (a.view.rows || []).length"
      , "query === asked && paint(b)" ]

  , glue "hands the filter to the server and aborts stale fetches"
      [ "onFilter: filter", "new AbortController()", "inflight.abort()"
      , "signal: inflight.signal", "?q=${encodeURIComponent(query)}"
      , "e.name !== \"AbortError\""
      -- The string as typed: the grammar is the server's to parse.
      , "const filter = (q) => commit(q.trim());" ]

  -- A key the columns do not name is a producer virtual key, which is the one
  -- place the renderer's suggestions and the server's parser can be different
  -- versions.  The check reads the rows the page already holds.
  , glue "an empty answer to a virtual key is checked locally"
      [ "function parity(total)", "if (total !== 0 || !query || !all.length) return;"
      , "TableView.parseQuery(query, keys)"
      , "t.key === null && !t.quoted && !t.negated"
      , "filter parity divergence — asset/daemon version skew"
      , "console.warn(note, { query, server: total, local })"
      , "if (!query) all = rows;" ]

  , glue "the applied query lives in the URL"
      [ "history.replaceState(null, \"\"", "p.set(\"q\", q); else p.delete(\"q\")"
      -- `keys' rides in the same query string and has to survive a commit.
      , "new URLSearchParams(location.search)"
      , "const urlQuery = () => params().get(\"q\") || \"\";"
      -- A ?q= in the address bar is applied on load.
      , "const asked = (query = urlQuery());" ]

  -- `/' asks the renderer to raise its palette instead of reaching for a box on
  -- the page: `openFilter' is mode-agnostic, so the one call covers an asset in
  -- any of them.  The renderer keeps `omnibox' for consumers that want the
  -- control resident; this shell is off it.
  , Glue "the filter is summoned rather than resident"
      [ "palette: true,"
      , "const summons = () => !!table && typeof table.openFilter === \"function\";"
      , "if (summons()) { table.openFilter(); return; }"
      -- An asset predating the call has a resident box; focus that.
      , "const box = document.querySelector(\"#app .tv-filter\");"
      , "if (box) { box.focus(); box.select(); }"
      -- And the map says what the key does now, which is what the echo pill
      -- prints when it runs.
      , "summon the filter palette" ]
      ["omnibox: true,"]

  -- The overlay is raised and dissolved by the renderer, whose own input stops
  -- ESC and DEL before this page's document handler sees them.  What keeps the
  -- shell's rows off the palette either way is `typing()': every `table' row is
  -- dead while a field has focus, and the one `any' row — ESC — closes the sheet
  -- and otherwise only blurs whatever is typing.
  , Glue "the palette's lifecycle stays the renderer's"
      [ "const live = (b) => b.scope === \"any\""
      , "|| (b.scope === \"table\" && !typing());"
      , "a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
      , "cancel: () => {"
      , "else if (typing()) document.activeElement.blur();" ]
      ["closeFilter", "tv-veil", "tv-panel"]

  -- With `bootstrap=off' no `set-rows' frame can arrive, so the branch that
  -- would have applied one is gone rather than left unreachable.
  , Glue "opens a socket and applies the streaming ops"
      [ "new WebSocket(", "/ws?bootstrap=off", "table.setRows("
      , "\"upsert-row\"", "table.upsertRow(", "\"delete-row\"", "table.deleteRow("
      -- Under a filter the rows are the server's answer to a query, so a row
      -- frame is re-asked for rather than spliced into them.
      , "setTimeout(fetchRows, 250)" ]
      ["\"set-rows\""]

  , glue "re-fetches and remounts after a close"
      [ "socket.onclose", "setTimeout(start,", "Math.min(backoff * 2, 30000)" ]

  -- A cold daemon answers the boot fetch with 503 while it walks the tree; the
  -- page it is answering is this one, so it says so and asks again.
  , glue "shows the indexing state and polls out of it"
      [ "r.status === 503", "{ indexing: b }", "if (e.indexing) return indexing("
      , "indexing … ${b.elapsed}s", "setTimeout(start, 1000)"
      , "dot(\"wait\")", "#dot.wait{" ]

  , glue "materializes a row and syncs it back"
      [ "\"materialize\"", "/headline?id=${encodeURIComponent(", "<textarea id=\"mtext\""
      , "method: \"POST\"", "flush(editing.digest)", "a.status === 409"
      -- The sheet's exits are keymap rows: ESC closes it, C-x C-s syncs it from
      -- inside the textarea.
      , "keyboard-quit", "C-x C-s" ]

  -- The author's Emacs theme in one set of custom properties: white on true
  -- black in the dark variant, black on white in the light one.  The hairline is
  -- the renderer's own `--tv-border', so the page draws one weight of chrome;
  -- danneskjold's own border faces frame instead.
  , Glue "the page wears danneskjold and the sheet wears Hack"
      [ "--g-bg:#FFFFFF;--g-fg:#000000;--g-border:#E3E6EA"
      , "@media (prefers-color-scheme:dark){:root{--g-bg:#000000;--g-fg:#FFFFFF;"
      , "--g-border:#2A2D3D;--g-mute:#A4C2EB;--g-surface:#21252B;--g-sel:#373D4F;"
      , "background:var(--g-bg);color:var(--g-fg)"
      , "#mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
      , "#mnote.conflict,#mnote.error{color:var(--g-bad)}"
      , "border:1px solid var(--g-border)"
      -- The sheet asks for the author's Emacs font by name; the page keeps the
      -- stack it had.
      , "--dk-mono:\"Hack\", var(--glance-mono)"
      , "font:14px/1.5 var(--glance-mono)" ]
      ["--g-border:#BDC3C7", "--g-border:#223959"]

  -- One rule sets both widths, so the strip cannot drift from the table above
  -- it; the hairline, the radius and the surface tint are `.tv-root''s, which is
  -- what makes it read as the same thing.  The frame is resident, so an arriving
  -- event cannot shift the key line under it: the collapse, the hand-reserved
  -- line and the ten-line cap are all superseded designs the flex rule replaced,
  -- and a second limit here could only fight the column.
  , Glue "the log wears the table's container under it"
      [ "#app,#log{width:100%;box-sizing:border-box}"
      , "border:1px solid var(--g-border);border-radius:8px;"
      -- It takes the height the table and the key line leave, and scrolls
      -- inside it rather than at a cap of its own.
      , "background:var(--g-surface);flex:1 1 auto;overflow-y:auto}"
      -- The end of a long message is scrolled to unless the reader has scrolled
      -- up to hold a place.
      , "box.scrollTop + box.clientHeight >= box.scrollHeight - 4"
      , "if (end) box.scrollTop = box.scrollHeight;" ]
      ["#log:empty", "min-height:1.4em", "max-height:10em"]

  -- Connection, sync outcomes, the parity warning and errors: what a reader
  -- could not have seen otherwise.  The row count is the renderer's hint line
  -- and the profile is the corner's and the key line's; the strip repeated both.
  , Glue "the log carries events and nothing the page shows anyway"
      [ "log(`disconnected · retrying in ${Math.round(backoff / 1000)}s`)"
      , "log(`indexing … ${b.elapsed}s"
      , "log(`load failed: ${e.message}`)"
      , "log(\"closed without writing — the file is as it was\")"
      , "filter parity divergence — asset/daemon version skew"
      -- The boot placeholder is cleared by the mount: the frame stays, and a
      -- loaded page with nothing to report shows it empty rather than still
      -- saying it is loading.
      , "<div id=\"log\">loading …</div>"
      , "log(\"\");"
      -- The transient echo on a manual switch is not a log line and stays.
      , "echo(`movement: ${profile}`)" ]
      [ "const say = () =>", "say();", "getRows().length"
      , "matching ${query}", "${profile} keys" ]

  -- `table-view.js' gives its sticky header `z-index:1' and its completion list
  -- `5'; an unnumbered backdrop painted under both.  The page's own corner and
  -- echo stay below the backdrop and dim with everything else.
  , glue "the sheet's backdrop covers the renderer's chrome"
      [ "position:fixed;inset:0;z-index:100;", "position:relative;z-index:101;"
      , "#corner{position:fixed;top:12px;right:14px;z-index:3;"
      , "#echo{position:fixed;right:14px;bottom:12px;z-index:2;" ]

  , glue "the theme is a three-way switch the page honours"
      -- The selector and its three options.
      [ "<label for=\"themesel\">theme:</label>"
      , "<option value=\"auto\">auto</option><option value=\"light\">light</option>"
      , "<option value=\"dark\">dark</option>"
      -- `auto' is the media query; the other two pin the attribute the
      -- renderer's own overrides read.
      , ":root[data-theme=\"light\"]{--g-bg:#FFFFFF"
      , ":root[data-theme=\"dark\"]{--g-bg:#000000"
      , "if (name === \"auto\") delete document.documentElement.dataset.theme;"
      , "else document.documentElement.dataset.theme = name;"
      , "localStorage.getItem(\"glance-theme\")"
      , "localStorage.setItem(\"glance-theme\", v)"
      , "el(\"themesel\").addEventListener(\"change\""
      -- And the head applies it before anything paints.
      , "<script>try{var t=localStorage.getItem(\"glance-theme\");" ]

  , glue "the dispatch and the echo widget read that blob and no other map"
      [ "<script id=\"keys\" type=\"application/json\">"
      , "JSON.parse(el(\"keys\").textContent)"
      , "MAPS.shared.concat(MAPS.profiles[profile]).filter(live)"
      , "HANDLERS[b.handler]" ]

  , glue "the profile is remembered, askable, and switchable in place"
      [ "localStorage.getItem(\"glance-keys\")", "localStorage.setItem(\"glance-keys\""
      , "new URLSearchParams(location.search).get(\"keys\")"
      , "movement: ${profile}" ]

  -- Native, so Tab reaches it and the arrows walk it without a chord of its own
  -- — and `typing()' hands those arrows over while it has focus.  The pill it
  -- replaced needed one.
  , Glue "the profile picker is a native select filled from the blob"
      [ "<select id=\"keysel\"", "<label for=\"keysel\">keys:</label>"
      , "for (const name of Object.keys(MAPS.profiles))"
      , "document.createElement(\"option\")"
      , "el(\"keysel\").addEventListener(\"change\""
      , "a.tagName === \"SELECT\"" ]
      [ "<button id=\"keyset\"", "#keyset{", "keys: ${name}" ]

  , glue "a binding with no handler names what it is waiting for"
      [ "arrives with daemon commands (M4)" ]

  , glue "the echo widget is mounted, in Emacs wording"
      [ "<div id=\"echo\"", "#echo{position:fixed", "is undefined", "timed out"
      , "Enter: \"RET\"", "Escape: \"ESC\"", "ArrowUp: \"<up>\"" ]

  -- A row step is `selectStep': it carries the column and turns the page at
  -- either end, and `getVisible()' is one page's worth, so arithmetic over it
  -- here would stop dead at a boundary.  The index walk stays as the fallback
  -- for an asset predating the call — which has no pages either.  The selected
  -- row is then marked once, by the renderer's own secondary-highlight
  -- background: the accent stripe this page drew over it is a superseded design
  -- (#26), a second mark for the same fact.
  , Glue "row movement drives the renderer's own selection"
      [ "const steps = () => !!table && typeof table.selectStep === \"function\";"
      , "if (visible().length) table.selectStep(step);"
      -- Which row is on is the renderer's answer too, with the DOM read left as
      -- the fallback for an asset predating that call.
      , "tbody tr.tv-sel", "table.getVisible()", "table.select(id, column())", ".tv-filter"
      , "if (cells()) return table.getSelection().id;" ]
      [ "tr.click()", "scrollIntoView", "rowEls("
      , "box-shadow:inset 2px 0 0 var(--tv-accent)", "tr.tv-sel{box-shadow" ]

  , glue "the set is paged, and the brackets turn one"
      -- One number for the boot's limit and the renderer's page, so the first
      -- paint is exactly page one.
      [ "const PAGE = 100;   // rows in the first paint, and rows to a page"
      , "pageSize: PAGE,"
      , "load(`${narrow}limit=${PAGE}`)"
      -- The turn is the renderer's, and the echo says where it landed:
      -- `] → page 3/129'.
      , "nextPage: (b) => turnPage(b, 1),"
      , "previousPage: (b) => turnPage(b, -1),"
      , "if (step > 0) table.nextPage(); else table.previousPage();"
      , "echo(`${b.seq} → page ${at.page}/${at.pages}`);"
      -- An asset without a pager says so rather than throwing.
      , "typeof table.nextPage === \"function\""
      , "typeof table.pageInfo === \"function\""
      , "this table-view.js has no pager" ]

  -- The column is the renderer's to hold: the shell reads it back out of
  -- `getSelection()' every time, which is why it survives a profile switch and
  -- goes when the selection does.  No second copy of it lives here.
  , Glue "cell movement is that selection with a column, and no state here"
      [ "const column = () => (cells() ? table.getSelection().col : null);"
      , "nextColumn: (b) => moveCol(b, 1),"
      , "previousColumn: (b) => moveCol(b, -1),"
      -- A whole-row selection has no column, and either direction lands on the
      -- first one from there.
      , "const at = column(), want = at === null ? 0 : at + step;"
      , "table.select(id, want)"
      -- An asset without cell selection says so rather than throwing.
      , "typeof table.getSelection === \"function\""
      , "this table-view.js has no cell selection"
      -- The row is handed to its handler so the echo can open the same way.
      , "if (handler) handler(b);" ]
      ["let col = ", "selCol", "lastColumn"]

  -- `f → next-column (Headline)', and `f → next-column (at last)' where the walk
  -- ran out of columns.
  , glue "the landing column is echoed by its header, or the edge it stopped at"
      [ "const say = (what) => echo(`${b.seq} → ${b.command} (${what})`);"
      , "say(cols[want].header || cols[want].key);"
      , "say(want < 0 ? \"at first\" : \"at last\")"
      , "say(\"no row\")"
      -- The headers are the mounted view's, and parity cuts the keys out of the
      -- same list where it needs them.
      , "cols = view.columns || [];"
      , "const keys = cols.map((c) => c.key);" ]

  -- The rules: a finger's 44px, and a word saying what the row is while no chip
  -- has filled it.  The renderer hides an empty row with an inline
  -- `display:none', which `!important' outranks.
  , glue "a coarse pointer taps the chip row to summon the filter"
      [ "@media (pointer:coarse){"
      , "#app .tv-chips{min-height:44px;cursor:pointer}"
      , "#app .tv-chips:empty{display:flex!important;align-items:center}"
      , "content:\"filter …\""
      -- The handler: delegated from #app so it survives a re-mount, and through
      -- the same `focusFilter' the key runs.
      , "el(\"app\").addEventListener(\"click\""
      , "matchMedia(\"(pointer: coarse)\").matches"
      , "if (!coarse()) return;"
      , "t.closest(\".tv-chips\")"
      -- A tap on a chip is that chip's own removal and stays the renderer's.
      , "t.closest(\".tv-chip\")"
      , "focusFilter();" ]

  -- Under 16px, focusing a field zooms the page in and nothing zooms it back
  -- out.  The renderer's own input is the renderer's problem; #mtext is this
  -- page's, and it keeps its 12px everywhere else.
  , glue "a coarse pointer gets a sheet iOS will not zoom into"
      [ "#mtext{font-size:16px}}", "font:12px/1.5 var(--dk-mono)" ]

  , glue "asks for one font stack, everywhere in the page"
      [ "--glance-mono:\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"
      -- The renderer injects `.tv-root{font:…}' from its own script, which lands
      -- after this page's style element; the extra selector step wins.
      , "#app .tv-root{font-family:var(--glance-mono)}"
      , "font:14px/1.5 var(--glance-mono)", "font:12px/1.5 var(--dk-mono)"
      -- The sheet asks for the author's Emacs font first and falls back through
      -- the page's own stack, so there is still one list.
      , "--dk-mono:\"Hack\", var(--glance-mono)" ]

  -- The assets directory this shell is rendered against holds no font file, so
  -- the declaration must not be there to point at one.
  , Glue "with no font file to serve, says nothing about one" [] ["@font-face"]
  ]

-- | The window between @bind@ and the end of the startup walk.  The server
-- listens through it, so every route has an answer: the three that read the
-- store say they cannot yet, and the page that says so is served.
indexingSpec :: TestTree
indexingSpec = testGroup "Indexing (bind before load)"
  [ testCase "/headlines is a 503 that says when to come back" $ do
      application' <- indexingApp
      r <- getFrom application' "/headlines"
      assertEqual "status" 503 (status r)
      assertEqual "retry" (Just "1") (header "Retry-After" r)
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      loading <- decoded r
      assertEqual "loading" (Bool True) =<< field "loading" loading
      -- Seconds, rounded to a tenth: the shell prints them as `indexing … 0.3s',
      -- so a raw double would put fifteen digits on the page.
      elapsed <- field "elapsed" loading
      case elapsed of
        Number n -> do
          assertBool ("elapsed runs backwards: " <> show n) (n >= 0)
          assertEqual "elapsed is not rounded to a tenth"
                      (fromInteger (round (n * 10)) / 10) n
        other -> assertFailure ("expected a number of seconds, got " <> show other)
      -- And no query parameter makes the store readable early.
      q <- getFrom application' "/headlines?q=meeting&limit=10&offset=5"
      assertEqual "with parameters" 503 (status q)

  , testCase "materialize and commit wait for the load too" $ do
      application' <- indexingApp
      r <- getFrom application' (headlinePath "sample.org:0")
      assertEqual "GET /headline" 503 (status r)
      -- A commit before the load would be refused as a headline the file does
      -- have: the 503 is the honest answer, and the retriable one.
      w <- postTo application' (headlinePath "sample.org:0") (commitBody "* x\n" "deadbeef")
      assertEqual "POST /headline" 503 (status w)
      assertEqual "retry" (Just "1") (header "Retry-After" w)

  , testCase "/ws says the same, so a client reconnects rather than mounts" $ do
      application' <- indexingApp
      r <- getFrom application' "/ws"
      assertEqual "status" 503 (status r)

  , testCase "the shell and its assets are served the whole time" $ do
      application' <- indexingApp
      r <- getFrom application' "/"
      assertEqual "status" 200 (status r)
      assertContains "the shell itself" "TableView.mount" (body r)
      js <- getFrom application' "/table-view.js"
      assertEqual "the renderer" 200 (status js)

  , testCase "the load landing opens the store routes, on the same server" $ do
      hub <- newLoadingHub =<< getMonotonicTime
      let application' = application (served assetsDir) hub
      before <- getFrom application' "/headlines"
      assertEqual "before" 503 (status before)
      finishLoading hub =<< loadStore viewDir
      after <- getFrom application' "/headlines"
      assertEqual "after" 200 (status after)
      -- That the body is the view the load produced is 'headlineSpec''s claim
      -- over the same directory; what is this case's is that the routes opened
      -- onto a loaded store rather than an empty one.
      assertEqual "the rows the walk found" (Just "6") (header "X-Glance-Rows" after)
      -- The generation starts where a store loaded at startup starts it.
      assertEqual "etag" (Just "\"g0\"") (header "ETag" after)
  ]

-- | A server whose startup walk has not finished — the state 'Glance.Web.serve'
-- binds its socket in.
indexingApp :: IO Application
indexingApp = application (served assetsDir) <$> (newLoadingHub =<< getMonotonicTime)

-- | @\/headlines@ is the facade's view document — the same 'Value' 'viewJSON'
-- builds from the same directory, so the server adds nothing to the wire.
headlineSpec :: TestTree
headlineSpec = testGroup "GET /headlines"
  [ testCase "is the view JSON for the served directory, rendered from the store" $ do
      r <- get assetsDir "/headlines"
      expected <- viewJSON (viewTitleFor viewDir) . qrRecords <$> loadDir viewDir
      assertEqual "status" 200 (status r)
      got <- decoded r
      assertEqual "view" expected got

  , testCase "is UTF-8 JSON, and says so" $ do
      r <- get assetsDir "/headlines"
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      assertContains "unicode cell" "Привет мир" (body r)

  , testCase "carries one row per headline" $ do
      r <- get assetsDir "/headlines"
      rows <- length . qrRecords <$> loadDir viewDir
      assertEqual "fixture rows" 6 rows
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)

  , testCase "serves JSON with the assets directory missing" $ do
      r <- get missingAssetsDir "/headlines"
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)
  ]

-- | The load counts ride in headers; the body stays SCHEMA.md's field set.
statsSpec :: TestTree
statsSpec = testGroup "Load stats"
  [ testCase "report what the walk covered" $ do
      r <- get assetsDir "/headlines"
      assertEqual "files" (Just "2") (header "X-Glance-Files" r)
      assertEqual "parse failures" (Just "0") (header "X-Glance-Parse-Failures" r)
      assertEqual "decode failures" (Just "1") (header "X-Glance-Decode-Failures" r)
      assertEqual "read failures" (Just "0") (header "X-Glance-Read-Failures" r)
      assertEqual "id collisions" (Just "0") (header "X-Glance-Id-Collisions" r)

  , testCase "count the rows two files claimed one id for" $ withTempDir $ \dir -> do
      -- The org-glance shape: a canonical store and a mirror of it.  Both are
      -- named here, so the walk's own exclusion is not what is under test.
      let shared = "* TODO one\n:PROPERTIES:\n:ORG_GLANCE_ID: shared-id\n:END:\n"
      _ <- orgFile dir "canonical.org" shared
      _ <- orgFile dir "mirror.org" shared
      (a, _hub) <- serverOver dir
      r <- getFrom a "/headlines"
      assertEqual "one row per id" 1 . length =<< rowsOf r
      assertEqual "and it says how many it chose between"
                  (Just "1") (header "X-Glance-Id-Collisions" r)

  , testCase "leave the view document's field set alone" $ do
      v <- get assetsDir "/headlines" >>= decoded
      case v of
        Object o -> assertEqual "top-level keys"
                                ["actions", "columns", "rows", "sort", "title"]
                                (sort (map Key.toText (KM.keys o)))
        _        -> assertFailure ("expected an object, got " <> show v)
  ]

-- | The @ETag@ is the store's generation, and the store's generation is what
-- the watcher moves.  Every query variant shares it — the parameters are in
-- the URL, and an HTTP cache is keyed by URL, so each variant revalidates
-- against the tag it was itself given.
cacheSpec :: TestTree
cacheSpec = testGroup "GET /headlines cache validation"
  [ testCase "carries a generation tag, and says to revalidate every time" $ do
      r <- get assetsDir "/headlines"
      assertEqual "ETag" (Just "\"g0\"") (header "ETag" r)
      assertEqual "Cache-Control" (Just "no-cache") (header "Cache-Control" r)

  , testCase "the tag it just gave out is a 304 with no body" $ do
      a <- app assetsDir
      first' <- getFrom a "/headlines"
      let tag = fromMaybe "" (header "ETag" first')
      again <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 304 (status again)
      assertEqual "body" "" (simpleBody again)
      assertEqual "the tag comes back" (Just tag) (header "ETag" again)
      -- Nothing else is owed on a 304, and Content-Type least of all.
      assertEqual "no content type" Nothing (header "Content-Type" again)

  , testCase "a weak tag, or one in a list, still matches" $ do
      a <- app assetsDir
      weak <- getWith a "/headlines" [("If-None-Match", "W/\"g0\"")]
      listed <- getWith a "/headlines" [("If-None-Match", "\"g9\", \"g0\"")]
      assertEqual "weak" 304 (status weak)
      assertEqual "listed" 304 (status listed)

  , testCase "a tag from another generation is the whole document again" $ do
      a <- app assetsDir
      r <- getWith a "/headlines" [("If-None-Match", "\"g7\"")]
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)

  , testCase "a store the watch moved is a fresh tag" $ withTempDir $ \dir -> do
      path <- orgFile dir "notes.org" committable
      (a, hub) <- serverOver dir
      before <- getFrom a "/headlines"
      let tag = fromMaybe "" (header "ETag" before)
      -- The watch's own step, taken here without a watcher: re-load the file
      -- and publish it, which is the one path that updates the store.
      _ <- orgFile dir "notes.org" (committable <> "* TODO Third\n")
      outcome <- loadFile path
      _ <- publish hub (applyFile path outcome)
      after <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 200 (status after)
      assertBool "the tag moved with the store"
                 (header "ETag" after /= Just tag)
      assertEqual "and the new row is in it" (Just "3") (header "X-Glance-Rows" after)

  , testCase "a re-load that changes nothing leaves the tag where it was" $
      withTempDir $ \dir -> do
        path <- orgFile dir "notes.org" committable
        (a, hub) <- serverOver dir
        before <- getFrom a "/headlines"
        outcome <- loadFile path
        _ <- publish hub (applyFile path outcome)
        after <- getFrom a "/headlines"
        assertEqual "the tag" (header "ETag" before) (header "ETag" after)

  , testCase "one tag serves every variant, which the URL keeps apart" $ do
      a <- app assetsDir
      full <- getFrom a "/headlines"
      paged <- getFrom a "/headlines?limit=2"
      filtered <- getFrom a "/headlines?q=table"
      assertEqual "the paged tag" (header "ETag" full) (header "ETag" paged)
      assertEqual "the filtered tag" (header "ETag" full) (header "ETag" filtered)
      -- The bodies differ, which is what the distinct URLs are for.
      assertBool "one URL's answer served for another"
                 (simpleBody full /= simpleBody paged)
  ]

-- | Compression: on for the text this server sends, off for a body too small
-- to gain by it, and always with the @Vary@ that keeps the two encodings from
-- being confused for each other.
gzipSpec :: TestTree
gzipSpec = testGroup "Compression"
  [ testCase "the view JSON is gzipped for a client that asks" $ do
      a <- app assetsDir
      plain' <- getFrom a "/headlines"
      zipped <- getWith a "/headlines" [("Accept-Encoding", "gzip")]
      assertEqual "status" 200 (status zipped)
      assertEqual "Content-Encoding" (Just "gzip") (header "Content-Encoding" zipped)
      assertBool "compressed to no less than it was"
                 (BL.length (simpleBody zipped) < BL.length (simpleBody plain'))

  , testCase "and left alone for a client that does not" $ do
      r <- get assetsDir "/headlines"
      assertEqual "Content-Encoding" Nothing (header "Content-Encoding" r)
      assertContains "the body is JSON" "\"rows\"" (body r)

  , testCase "every answer varies on the encoding, 304s included" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines"
      notModified <- getWith a "/headlines"
        [("If-None-Match", fromMaybe "" (header "ETag" r))]
      assertEqual "on the 200" (Just "Accept-Encoding") (header "Vary" r)
      assertEqual "on the 304" (Just "Accept-Encoding") (header "Vary" notModified)

  , testCase "a body under the threshold is not worth compressing" $ do
      a <- app assetsDir
      r <- getWith a "/headline" [("Accept-Encoding", "gzip")]
      assertEqual "status" 400 (status r)
      assertBool "the error JSON is small" (BL.length (simpleBody r) < 860)
      assertEqual "Content-Encoding" Nothing (header "Content-Encoding" r)

  , testCase "the renderer is compressed too, though it is a file" $ do
      a <- app assetsDir
      r <- getWith a "/table-view.js" [("Accept-Encoding", "gzip")]
      assertEqual "status" 200 (status r)
      assertEqual "Content-Encoding" (Just "gzip") (header "Content-Encoding" r)
  ]

-- | @q@, @limit@ and @offset@: filter first, then page, and report what the
-- page covers in the header family the load counts already use.
querySpec :: TestTree
querySpec = testGroup "GET /headlines filter and paging"
  [ testCase "no parameters is the whole set, as it always was" $ do
      r <- get assetsDir "/headlines"
      assertEqual "X-Glance-Total" (Just "6") (header "X-Glance-Total" r)
      assertEqual "X-Glance-Has-Next" (Just "false") (header "X-Glance-Has-Next" r)
      assertEqual "rows" 6 . length =<< rowsOf r

  , testCase "q narrows on the row as it displays, case-insensitively" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=SHIP%20THE%20TABLE"
      assertEqual "X-Glance-Total" (Just "1") (header "X-Glance-Total" r)
      ids <- map rowId <$> rowsOf r
      assertEqual "the matching row" ["ship-table-view"] ids

  , testCase "q matches a bracket link by its description, not its target" $
      withTempDir $ \dir -> do
        -- What the row shows is what a filter searches, the way the renderer
        -- searches its own cached display text (table-view.js `displayText').
        _ <- orgFile dir "links.org"
               "* TODO Read [[file:table-view/SCHEMA.md][the schema]]\n"
        (a, _hub) <- serverOver dir
        shown <- getFrom a "/headlines?q=the%20schema"
        target <- getFrom a "/headlines?q=SCHEMA.md"
        assertEqual "the description matches" (Just "1") (header "X-Glance-Total" shown)
        assertEqual "the target does not" (Just "0") (header "X-Glance-Total" target)

  , testCase "q matching nothing is an empty page under a 200" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=no-such-headline-anywhere"
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Total" (Just "0") (header "X-Glance-Total" r)
      assertEqual "rows" 0 . length =<< rowsOf r

  , testCase "q reaches the filter grammar intact" $ do
      a <- app assetsDir
      let total path = fmap TE.decodeUtf8 . header "X-Glance-Total" <$> getFrom a path
      -- This route's subject is transport: TestFilter is the grammar's home and
      -- states every rule over the same fixture.  What is this route's own
      -- contract is that a `?q=' arrives at the parser as it was typed, so three
      -- shapes are enough — a predicate, a negation of one, and org cell text
      -- carrying the separator that must not become one.
      assertEqual "a predicate" (Just "1") =<< total "/headlines?q=state:DONE"
      assertEqual "a negation drops what it matches" (Just "5")
        =<< total "/headlines?q=-state:DONE"
      assertEqual "a tag string stays text" (Just "2") =<< total "/headlines?q=:web:"

  , testCase "a filtered OR query pages out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines?q=state:active"
      one <- getFrom a "/headlines?q=state:active&limit=2&offset=0"
      two <- getFrom a "/headlines?q=state:active&limit=2&offset=2"
      assertEqual "the union" 3 (length whole)
      assertEqual "the total is the match count, not the page" (Just "3")
                  (header "X-Glance-Total" one)
      let sorted = map rowId (sortOn scheduledOf whole)
      assertEqual "page one" (take 2 sorted) . map rowId =<< rowsOf one
      assertEqual "page two" (drop 2 sorted) . map rowId =<< rowsOf two
      assertEqual "more follows page one" (Just "true") (header "X-Glance-Has-Next" one)
      assertEqual "nothing follows page two" (Just "false") (header "X-Glance-Has-Next" two)

  , testCase "limit cuts a page out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines"
      page <- rowsOf =<< getFrom a "/headlines?limit=3"
      assertEqual "page size" 3 (length page)
      -- The page is the first three by scheduled ascending, which is what the
      -- view declares — not the first three the walk found.
      assertEqual "the sort the view declares"
                  (take 3 (map rowId (sortOn scheduledOf whole)))
                  (map rowId page)

  , testCase "offset walks the pages, and has-next says when to stop" $ do
      a <- app assetsDir
      whole <- map rowId . sortOn scheduledOf <$> (rowsOf =<< getFrom a "/headlines")
      one <- getFrom a "/headlines?limit=4&offset=0"
      two <- getFrom a "/headlines?limit=4&offset=4"
      past <- getFrom a "/headlines?limit=4&offset=6"
      assertEqual "page one" (take 4 whole) . map rowId =<< rowsOf one
      assertEqual "page two" (drop 4 whole) . map rowId =<< rowsOf two
      assertEqual "more follows page one" (Just "true") (header "X-Glance-Has-Next" one)
      assertEqual "nothing follows page two" (Just "false") (header "X-Glance-Has-Next" two)
      assertEqual "past the end is empty" 0 . length =<< rowsOf past
      assertEqual "and says so" (Just "false") (header "X-Glance-Has-Next" past)

  , testCase "the filter runs before the page, so the total is the match count" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=e&limit=2&offset=1"
      matched <- length <$> (rowsOf =<< getFrom a "/headlines?q=e")
      assertEqual "the total is what matched" (Just (T.pack (show matched)))
                  (fmap TE.decodeUtf8 (header "X-Glance-Total" r))
      assertBool "the fixture would not exercise the arithmetic" (matched > 3)
      page <- rowsOf r
      assertEqual "the page is a slice of it" 2 (length page)
      assertEqual "and more follows" (Just "true") (header "X-Glance-Has-Next" r)

  , testCase "the state palette is the store's, whatever the page holds" $ do
      a <- app assetsDir
      whole <- badgeValues =<< decoded =<< getFrom a "/headlines"
      page <- badgeValues =<< decoded =<< getFrom a "/headlines?limit=1"
      none <- badgeValues =<< decoded =<< getFrom a "/headlines?q=no-such-headline"
      assertEqual "the paged palette" whole page
      assertEqual "the empty page's palette" whole none
      assertBool "the fixture declares keywords" (length whole > 2)

  , testCase "a limit past the cap is refused, and named" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?limit=20001"
      ok <- getFrom a "/headlines?limit=20000"
      assertEqual "over" 400 (status r)
      assertContains "the cap" "20000" (body r)
      assertEqual "at the cap" 200 (status ok)

  , testCase "a parameter that is not a number is a 400 saying which" $ do
      a <- app assetsDir
      mapM_ (\(path, named) -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the parameter" named (body r))
            [ ("/headlines?limit=lots", "limit")
            , ("/headlines?limit=-1", "limit")
            , ("/headlines?offset=x", "offset")
            , ("/headlines?offset=-3", "offset") ]

  , testCase "a bare parameter reads as an absent one" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?limit&q"
      assertEqual "status" 200 (status r)
      assertEqual "rows" 6 . length =<< rowsOf r
  ]

-- | @\/ws?bootstrap=off@: the opening @set-rows@ dropped for a client that
-- fetched the rows over HTTP.  Checked on the parser, since the suite binds no
-- socket — the decision is the whole of what the query controls.
bootstrapSpec :: TestTree
bootstrapSpec = testGroup "Socket bootstrap control"
  [ testCase "is wanted by default, and by every query but the one" $
      mapM_ (\path -> assertBool (show path <> " skipped the bootstrap")
                                 (bootstrapWanted path))
            ["/ws", "/ws?", "/ws?keys=vim", "/ws?bootstrap=on", "/ws?bootstrap="]

  , testCase "bootstrap=off drops it, wherever it sits in the query" $
      mapM_ (\path -> assertBool (show path <> " still sent the bootstrap")
                                 (not (bootstrapWanted path)))
            ["/ws?bootstrap=off", "/ws?keys=vim&bootstrap=off", "/ws?bootstrap=off&x=1"]
  ]

-- | @GET \/headline@: one subtree out of the read model, with the coordinates
-- a write back to it needs.
materializeSpec :: TestTree
materializeSpec = testGroup "GET /headline"
  [ testCase "is the raw subtree, the file it came from and its digest" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a (headlinePath "ship-table-view")
      assertEqual "status" 200 (status r)
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      v <- decoded r
      org <- textAt "org" v
      rid <- textAt "id" v
      file <- textAt "file" v
      digest <- textAt "digest" v
      assertEqual "id" "ship-table-view" rid
      assertEqual "file" (T.pack sampleFile) file
      assertEqual "digest" sampleDigest digest
      assertEqual "org" (T.unlines [ "* NEXT [#A] Ship the table view :web:glance:"
                                   , "SCHEDULED: <2026-08-01 Sat 09:30> DEADLINE: <2026-08-05 Wed>"
                                   , ":PROPERTIES:"
                                   , ":ORG_GLANCE_ID: ship-table-view"
                                   , ":END:" ])
                  org

  , testCase "the org text is exactly the span the response reports" $ do
      (a, _hub) <- serverOver viewDir
      v <- getFrom a (headlinePath "ship-table-view") >>= decoded
      org <- textAt "org" v
      extent <- field "span" v
      start <- intAt "start" extent
      end <- intAt "end" extent
      doc <- document sampleFile
      assertEqual "slice" (T.take (end - start) (T.drop start doc)) org
      -- The extent starts at the stars, so the file's own header belongs to no
      -- subtree and a commit cannot take it with the headline.
      assertEqual "the preamble sits ahead of the first subtree"
                  "#+CATEGORY: sample\n#+TODO: NEXT WAITING | CANCELLED\n\n" (T.take start doc)

  , testCase "an id carrying a colon and slashes round-trips" $ do
      (a, _hub) <- serverOver viewDir
      let rid = T.pack sampleFile <> ":210"
      r <- getFrom a (headlinePath rid)
      assertEqual "status" 200 (status r)
      v <- decoded r
      back <- textAt "id" v
      org <- textAt "org" v
      assertEqual "id" rid back
      assertContains "subtree" "Привет мир" org

  , testCase "an id no row carries is a 404" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a (headlinePath "no-such-headline")
      assertEqual "status" 404 (status r)
      assertContains "hint" "no headline with id" (body r)

  , testCase "no id at all says what the route wants" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a "/headline"
      assertEqual "status" 400 (status r)
      -- The whole hint: `id=' on its own rides every id-bearing URL the page
      -- builds, so a body naming the method and the parameter is what tells a
      -- client with the wrong URL what to write.
      assertContains "hint" "GET /headline?id=<row id>" (body r)
  ]

-- | @POST \/headline@: the subtree written back, and every way that is refused.
commitSpec :: TestTree
commitSpec = testGroup "POST /headline"
  [ testCase "writes the edited subtree and leaves the rest of the file alone" $
      withCommitted $ \a path v -> do
        org <- textAt "org" v
        digest <- textAt "digest" v
        before <- document path
        extent <- field "span" v
        start <- intAt "start" extent
        end <- intAt "end" extent
        let edited = T.replace "TODO First" "DONE First" org <> "an added line\n"
        r <- postTo a (headlinePath "first") (commitBody edited digest)
        assertEqual "status" 200 (status r)
        after <- document path
        assertEqual "the file is prefix + new subtree + suffix"
                    (T.take start before <> edited <> T.drop end before) after
        assertContains "the edit landed" "* DONE First" after
        assertContains "the next headline is untouched" "* TODO Second\ntail\n" after
        fresh <- textAt "digest" =<< decoded r
        expected <- digestOnDisk path
        assertEqual "the reported digest is the file's" expected fresh

  , testCase "leaves the store alone — the watch is what updates rows" $
      withCommitted $ \a path before -> do
        org <- textAt "org" before
        digest <- textAt "digest" before
        r <- postTo a (headlinePath "first") (commitBody (org <> "a line\n") digest)
        assertEqual "status" 200 (status r)
        -- No watcher runs in this suite, so the store still holds the load it
        -- started with: the route wrote to the file and to nothing else.
        after <- decoded =<< getFrom a (headlinePath "first")
        assertEqual "the store's subtree" (Just org) . Just =<< textAt "org" after
        assertEqual "the store's digest" (Just digest) . Just =<< textAt "digest" after
        onDisk <- digestOnDisk path
        assertBool "the file was not written" (onDisk /= digest)

  , testCase "a file rewritten behind the client is a conflict, and stays as it is" $
      withCommitted $ \a path v -> do
        org <- textAt "org" v
        digest <- textAt "digest" v
        let meddled = committable <> "* TODO Someone else\n"
        TIO.writeFile path meddled
        r <- postTo a (headlinePath "first") (commitBody (org <> "mine\n") digest)
        assertEqual "status" 409 (status r)
        conflict <- decoded r
        reason <- textAt "reason" conflict
        assertEqual "reason" "drift" reason
        assertContains "the message says to materialize again" "materialize" (body r)
        after <- document path
        assertEqual "the file is the meddler's" meddled after

  , testCase "a digest the store no longer holds is a conflict too" $
      withCommitted $ \a path v -> do
        org <- textAt "org" v
        let stale = T.replicate 64 "0"
        r <- postTo a (headlinePath "first") (commitBody org stale)
        assertEqual "status" 409 (status r)
        conflict <- decoded r
        reason <- textAt "reason" conflict
        current <- textAt "digest" conflict
        assertEqual "reason" "stale" reason
        assertEqual "the digest to re-materialize with" (Just current) . Just
          =<< textAt "digest" v
        after <- document path
        assertEqual "untouched" committable after

  , testCase "a body that is not the two fields is a 400" $
      withCommitted $ \a _path _v -> do
        broken <- postTo a (headlinePath "first") "{not json"
        missing <- postTo a (headlinePath "first") (encode (object ["org" .= ("x" :: T.Text)]))
        assertEqual "malformed" 400 (status broken)
        assertEqual "incomplete" 400 (status missing)
        -- The parse error names the missing field, rather than the word
        -- appearing anywhere in a body that also carries the digest itself.
        assertContains "says which" "key \\\"digest\\\" not found" (body missing)

  , testCase "a body over the cap is refused before it is read" $
      withCommitted $ \a _path _v -> do
        let huge = BL.fromStrict (BS.replicate (1024 * 1024 + 1) 0x78)
        r <- postTo a (headlinePath "first") huge
        assertEqual "status" 413 (status r)
        assertContains "the cap" "body over" (body r)

  , testCase "an id no row carries is a 404, and no id a 400" $
      withCommitted $ \a _path _v -> do
        unknown <- postTo a (headlinePath "no-such-headline") (commitBody "* x\n" "d")
        anonymous <- postTo a "/headline" (commitBody "* x\n" "d")
        assertEqual "unknown id" 404 (status unknown)
        assertEqual "no id" 400 (status anonymous)
        assertContains "the hint" "POST /headline?id=<row id>" (body anonymous)
  ]

-- | @\/@ in both modes: a shell that mounts the renderer, and a page that
-- explains where the renderer went.
pageSpec :: IO T.Text -> TestTree
pageSpec shell = testGroup "GET /"
  [ testCase "with assets, is a shell that fetches and mounts" $ do
      r <- get assetsDir "/"
      assertEqual "status" 200 (status r)
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "renderer" "src=\"table-view.js\"" (body r)
      assertContains "fetch glue" "fetch(`/headlines${params}`" (body r)
      assertContains "mount" "TableView.mount(" (body r)

  , testCase "with assets, the restored query is the renderer's own chips" $ do
      b <- shell
      -- The mount is handed the applied query; the renderer tokenizes it into
      -- committed chips and delivers nothing, since the rows in hand are
      -- already the server's answer to it.
      holdsAll "restore glue"
            [ "initialQuery: query,"
            -- An asset predating the option drops it silently, so the mount
            -- asks whether it took and stuffs the box when it did not.
            , "const holds = (q) => typeof table.getQuery === \"function\""
            , "&& table.getQuery() === q;"
            , "if (query && !holds(query)) showQuery();"
            , "function showQuery() {" ] b
      -- One restoration point: `start' re-fetches and re-mounts for every way
      -- back in, so it does not restore the query a second time itself.
      assertEqual "showQuery is called from the mount alone" 1
                  (T.count "!holds(query)) showQuery();" b)
      assertEqual "showQuery is defined once" 1 (T.count "function showQuery()" b)

  , testCase "with assets, DEL takes the last token off through the renderer" $ do
      b <- shell
      -- The chips are the renderer's, so the strip is too: the shell asks and
      -- then follows, rather than recomposing a query the chips would outlive.
      mapM_ (\needle -> assertContains "DEL glue" needle b)
            [ "table.stripLastToken()", "table.getQuery().trim()"
            , "filterDrop: () => {", "echo(\"DEL → no filter\")"
            , "DEL → filter: ${JSON.stringify(left)}", "DEL → filter cleared"
            -- An asset without the pair says so instead of guessing.
            , "typeof table.stripLastToken === \"function\""
            , "this table-view.js has no filter tokens"
            -- One press, one token: a held DEL claims the key and runs once,
            -- where held movement keeps repeating.  The table is the blob's.
            , "if (!(e.repeat && MAPS.once.indexOf(hit.command) !== -1)) run(hit);" ]
      onceOf b >>= assertEqual "the commands auto-repeat is off for" ["filter-drop-token"]
      -- The guard is per command, so it cannot take auto-repeat off movement.
      assertBool "the repeat guard is blanket rather than per command"
                 (not ("if (e.repeat) return" `T.isInfixOf` b))
      -- Neither of the two designs this replaced survives.
      holdsNone "a superseded filter path" ["glance-filter-history", "function withoutLast"] b

  , testCase "with assets, the sheet is buttonless and syncs on the way out" $ do
      b <- shell
      holdsAll "sheet glue"
            -- Dirty against the materialized original decides everything: a
            -- pristine close is no request at all.
            [ "const dirty = () => editing !== null && el(\"mtext\").value !== base;"
            , "if (!dirty()) { shut(); return; }"
            , "flush(editing.digest).then((ok) => ok && shut());"
            -- The backdrop is the mouse's ESC.
            , "if (e.target === el(\"modal\")) leave()"
            -- The receipt chains: the 200's digest is the next flush's lock.
            , "h.digest = a.body.digest; base = text;"
            -- A conflict keeps the sheet open and names the two keys.
            , "if (a.status === 409) sync(\"conflict\");"
            , "conflict — C-x C-s overwrite · ESC discard"
            , "if (troubled()) { shut();"
            -- And a tab closing on an edited sheet still owes the file.
            , "addEventListener(\"beforeunload\""
            , "post(editing.id, el(\"mtext\").value, editing.digest, { keepalive: true })" ] b
      -- One word carries the sheet's state, `sync' is its only writer, and the
      -- states that wait for a key say which key.  No buttons to reach them with.
      holdsAll "sync status"
            [ "synced: \"synced\"", "syncing: \"syncing…\"", "id=\"mnote\""
            , "error: \"error — C-x C-s retry · ESC discard\" };"
            , "const troubled = () => state === \"conflict\" || state === \"error\";"
            , "const flushing = () => state === \"syncing\";"
            , "function sync(next, message) {", "state = next;" ] b
      -- Nothing else writes the word, so the header cannot disagree with it.
      assertEqual "sync is the only writer" 1 (T.count "      state = next;" b)
      holdsNone "a sheet button"
        [ "id=\"msave\"", "id=\"mcancel\"", "id=\"mredo\"", "id=\"mfoot\"", "Re-materialize" ] b

  , testCase "with assets, the page is one column the viewport tall" $ do
      b <- shell
      holdsAll "column"
            [ "height:100vh;box-sizing:border-box;overflow:hidden;"
            , "padding:34px 24px 24px;display:flex;flex-direction:column;gap:14px}"
            -- The table asks for its height and can give it back; the key line
            -- never gives any of its own up, so a short window squeezes the
            -- table rather than clipping the line.
            , "#app{height:80vh;min-height:0}"
            , "#kbd{flex:none;" ] b
      -- Table, log, key line, in that order — the corner and the pill are
      -- fixed and out of the column, and the sheet is display:none until it
      -- is not.
      let at needle = T.length (fst (T.breakOn needle b))
      assertBool ("app, log, kbd in that order: " <> show (at "id=\"app\"", at "id=\"log\"", at "id=\"kbd\""))
                 (at "id=\"app\"" < at "id=\"log\"" && at "id=\"log\"" < at "id=\"kbd\"")

  , testCase "with assets, the last line is the active map, resident" $ do
      b <- shell
      holdsAll "key line"
            [ "<div id=\"kbd\"></div>"
            , "const rows = MAPS.profiles[profile].concat(MAPS.shared);"
            , "rows.find((x) => x.command === command && x.scope === \"table\")"
            -- A staged row has no handler and is no offer.
            , "return b && b.handler ? b.seq : null;"
            , "el(\"kbd\").textContent = MAPS.hints"
            -- And it is rewritten wherever the profile is set, which is the
            -- selector's own hook.
            , "hints();   // the line is the map's, so it moves with the profile" ] b
      -- Commands, not keys, in the order the line reads them: the table is the
      -- blob's and each spelling comes out of the active profile.
      hints <- hintsOf b
      assertEqual "the key line's table"
        [ (["next-row", "previous-row"], "rows")
        , (["next-column", "previous-column"], "cells")
        -- The page pair reads open-then-close, so the line says `[/]' where
        -- the two above it read forward first.
        , (["previous-page", "next-page"], "pages")
        , (["org-glance-overview:materialize"], "materialize")
        , (["filter-rows"], "filter")
        , (["org-glance-overview:refresh"], "refresh")
        , (["filter-drop-token"], "drop token")
        , (["quit-window"], "quit")
        ] hints
      -- And every command it names is one the map binds, in the table scope,
      -- with a handler behind it — under every profile, since the line is
      -- rewritten for each.  A hint for anything else is an empty offer.
      (shared, profiles) <- keymapOf b
      mapM_ (\(name, rows) ->
               let offered = [ c | (_k, _s, c, Just _h, "table", _help) <- shared <> rows ]
               in assertEqual (T.unpack name <> ": hinted but unbound") []
                    [ c | (cs, _label) <- hints, c <- cs, c `notElem` offered ])
            profiles
      -- No literal key in the line: `n/p' under emacs and `j/k' under vim are
      -- the same two commands, and only the blob knows which.  Nor does the
      -- transient log repeat what the resident line already says.
      holdsNone "the key line spells a key itself"
                ["\"n/p rows", "\"j/k rows", "RET materializes"] b

  , testCase "without assets, explains JSON-only mode" $ do
      r <- get missingAssetsDir "/"
      assertEqual "status" 200 (status r)
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "mode" "JSON-only" (body r)
      assertContains "flag" "--assets" (body r)
      assertContains "endpoint" "/headlines" (body r)
      assertBool "mounts a renderer it has not got"
                 (not ("TableView.mount(" `T.isInfixOf` body r))
  ]

-- | A keymap row as the suite spells it: the keys the dispatch matches, the
-- notation the echo widget shows, the command name, the handler behind it (or
-- none, for a binding no daemon command backs yet), the scope it is live in,
-- and the help line the echo widget adds where the command name is an Emacs
-- name for something narrower.
type Row = ([T.Text], T.Text, T.Text, Maybe T.Text, T.Text, Maybe T.Text)

-- | The shell's keymap, checked as the data it is.  The page carries the map
-- as a JSON blob and its own dispatch parses that blob, so reading it here
-- reads what the browser reads rather than what a grep for handler names would
-- find.
--
-- The expected map is written down rather than imported: the point of the
-- assertion is that the sequences and the org-glance command names are the
-- ones @org-glance-overview-mode-map@ spells, and an oracle taken from the
-- code under test would agree with any of them.
--
-- What both profiles carry — every command that is not movement, plus the
-- arrows, org-glance's own buffer-ends keys, and the brackets that turn a page,
-- which both editors spell alike.
expectedShared :: [Row]
expectedShared =
  [ (["<down>"],     "<down>",  "next-row",                        Just "nextRow",        "table", Nothing)
  , (["<up>"],       "<up>",    "previous-row",                    Just "previousRow",    "table", Nothing)
  , ([","],          ",",       "first-row",                       Just "firstRow",       "table", Nothing)
  , (["<"],          "<",       "first-row",                       Just "firstRow",       "table", Nothing)
  , (["."],          ".",       "last-row",                        Just "lastRow",        "table", Nothing)
  , ([">"],          ">",       "last-row",                        Just "lastRow",        "table", Nothing)
  , (["]"],          "]",       "next-page",                       Just "nextPage",       "table", Nothing)
  , (["["],          "[",       "previous-page",                   Just "previousPage",   "table", Nothing)
  , (["RET"],        "RET",     "org-glance-overview:materialize", Just "materializeRow", "table", Nothing)
  , (["/"],          "/",       "filter-rows",                     Just "focusFilter",    "table",
       Just "summon the filter palette")
  , (["DEL"],        "DEL",     "filter-drop-token",               Just "filterDrop",     "table",
       Just "drop the filter's last token")
  , (["q"],          "q",       "quit-window",                     Just "quitWindow",     "table", Nothing)
  , (["TAB"],        "TAB",     "org-cycle",                       Nothing,               "table", Nothing)
  , (["!"],          "!",       "org-glance-overview:open",        Nothing,               "table", Nothing)
  , (["a"],          "a",       "org-glance-agenda",               Nothing,               "table", Nothing)
  , (["@"],          "@",       "org-glance-overview:relations",   Nothing,               "table", Nothing)
  , (["+"],          "+",       "org-glance-overview:capture",     Nothing,               "table", Nothing)
  , (["D"],          "D",       "org-glance-overview:delete",      Nothing,               "table", Nothing)
  , (["C-c", "C-t"], "C-c C-t", "org-glance-overview:todo",        Nothing,               "table", Nothing)
  , (["C-c", "C-s"], "C-c C-s", "org-glance-overview:schedule",    Nothing,               "table", Nothing)
  , (["C-c", "C-d"], "C-c C-d", "org-glance-overview:deadline",    Nothing,               "table", Nothing)
  , (["C-x", "C-s"], "C-x C-s", "save-buffer",                     Just "save",           "modal",
       Just "sync the sheet now; again to overwrite a conflict")
  , (["ESC"],        "ESC",     "keyboard-quit",                   Just "cancel",         "any",
       Just "close the sheet, syncing an edited one; again to discard")
  ]

-- | The movement each profile adds, and what it displaces.  @j@ is the
-- overview's open-stub under emacs and down under vim; @g@ is refresh under
-- emacs and the opening of @gg@ under vim, which sends refresh to @R@.  Cell
-- movement is @f@\/@b@ under emacs — org-glance's same-level rhyme, one
-- granularity down — and @h@\/@l@ under vim, under the same two command names.
expectedProfiles :: [(T.Text, [Row])]
expectedProfiles =
  [ ("emacs",
      [ (["n"], "n", "next-row",                    Just "nextRow",         "table", Nothing)
      , (["p"], "p", "previous-row",                Just "previousRow",     "table", Nothing)
      , (["f"], "f", "next-column",                 Just "nextColumn",      "table", rightHelp)
      , (["b"], "b", "previous-column",             Just "previousColumn",  "table", leftHelp)
      , (["g"], "g", "org-glance-overview:refresh", Just "refresh",         "table", Nothing)
      , (["j"], "j", "org-glance-overview:open",    Nothing,                "table", Nothing)
      ])
  , ("vim",
      [ (["j"],      "j",  "next-row",                    Just "nextRow",        "table", Nothing)
      , (["k"],      "k",  "previous-row",                Just "previousRow",    "table", Nothing)
      , (["l"],      "l",  "next-column",                 Just "nextColumn",     "table", rightHelp)
      , (["h"],      "h",  "previous-column",             Just "previousColumn", "table", leftHelp)
      , (["g", "g"], "gg", "first-row",                   Just "firstRow",       "table", Nothing)
      , (["G"],      "G",  "last-row",                    Just "lastRow",        "table", Nothing)
      , (["R"],      "R",  "org-glance-overview:refresh", Just "refresh",        "table", Nothing)
      ])
  ]
  where rightHelp = Just "the cell to the right; row movement keeps the column"
        leftHelp  = Just "the cell to the left; from a whole row, the first column"

-- | The keymap blob out of SHELL, parsed.  Everything the dispatch reads is in
-- here, so the assertions below are over data rather than over the spelling of
-- a JS literal.
blobOf :: T.Text -> IO Value
blobOf shell = do
  raw <- maybe (assertFailure "no keymap blob in the shell") pure
               (between "<script id=\"keys\" type=\"application/json\">" "</script>" shell)
  either (\e -> assertFailure ("keymap JSON: " <> e)) pure
         (eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)))

-- | The resident key line's table out of SHELL: the commands it names, in the
-- order the line reads them, each with its label.
hintsOf :: T.Text -> IO [([T.Text], T.Text)]
hintsOf shell = traverse one =<< listAt "hints" =<< blobOf shell
  where one v = (,) <$> textsAt "commands" v <*> textAt "label" v

-- | The chords SHELL's blob declares never claimed.
reservedOf :: T.Text -> IO [T.Text]
reservedOf shell = textsAt "reserved" =<< blobOf shell

-- | The commands SHELL's blob declares auto-repeat is off for.
onceOf :: T.Text -> IO [T.Text]
onceOf shell = textsAt "once" =<< blobOf shell

-- | The keymap blob out of SHELL: the shared rows, and the profiles by name.
keymapOf :: T.Text -> IO ([Row], [(T.Text, [Row])])
keymapOf shell = do
  blob <- blobOf shell
  shared <- traverse row =<< listAt "shared" blob
  named <- traverse profile =<< membersAt "profiles" blob
  pure (shared, sortOn fst named)
  where
    profile (name, v) = do
      rows <- either (\e -> assertFailure (T.unpack name <> " profile: " <> e)) pure
                     (parseEither parseJSON v)
      (,) name <$> traverse row rows
    row v = (,,,,,) <$> textsAt "keys" v <*> textAt "seq" v <*> textAt "command" v
                    <*> maybeTextAt "handler" v <*> textAt "scope" v
                    <*> maybeTextAt "help" v

-- | The shell's inline glue, on its own — what a syntax check is run over.
glueOf :: T.Text -> IO T.Text
glueOf shell = maybe (assertFailure "no inline script in the shell") pure
                     (between "\n  <script>\n" "  </script>" shell)

keymapSpec :: IO T.Text -> TestTree
keymapSpec shell = testGroup "Shell keymap"
  [ testCase "is one JSON blob, in org-glance's own command names" $ do
      (shared, _profiles) <- keymapOf =<< shell
      assertEqual "the rows both profiles carry" expectedShared shared

  , testCase "carries a movement profile per editor, emacs by default" $ do
      b <- shell
      (_shared, profiles) <- keymapOf b
      assertEqual "profiles" expectedProfiles profiles
      assertContains "the default is named in the blob" "\"default\":\"emacs\"" b

  , testCase "no profile shadows a shared binding, or hides its own longer one" $ do
      (shared, profiles) <- keymapOf =<< shell
      let keysOf rows = [ k | (k, _, _, _, _, _) <- rows ]
      mapM_ (\(name, rows) -> do
               let bound = keysOf (shared <> rows)
                   twice = [ k | k <- nub bound, length (filter (== k) bound) > 1 ]
                   -- A complete sequence that also opens a longer one would
                   -- match first and leave the longer one unreachable.
                   eaten = [ (k, l) | k <- bound, l <- bound
                                    , k /= l, k == take (length k) l ]
               assertEqual (T.unpack name <> ": bound twice") [] twice
               assertEqual (T.unpack name <> ": swallows a longer sequence") [] eaten)
            profiles

  , testCase "the status corner carries the dot and the pickers, in that order" $ do
      b <- shell
      corner <- maybe (assertFailure "no status corner in the shell") pure
                      (between "<div id=\"corner\">" "</div>" b)
      holdsAll "corner" ["id=\"dot\"", "id=\"themesel\"", "id=\"keysel\""] corner
      assertContains "fixed in the corner" "#corner{position:fixed;top:12px;right:14px" b
      let at needle = T.length (fst (T.breakOn needle corner))
      assertBool ("dot, theme, keys in that order: " <> show corner)
                 (at "id=\"dot\"" < at "id=\"themesel\"" && at "id=\"themesel\"" < at "id=\"keysel\"")

  , testCase "the view title is the tab's alone, and nothing on the page repeats it" $ do
      b <- shell
      -- The table is the top of the page; a heading repeating the tab title put
      -- the same string on screen twice.
      assertContains "palette" "palette: true," b
      assertBool ("a heading survives in the shell: " <> show (between "<h1>" "</h1>" b))
                 (not ("<h1>" `T.isInfixOf` b))
      -- Written down rather than taken from the code that writes it into the
      -- page: an oracle calling 'viewTitleFor' agrees with whatever it returns.
      assertEqual "the tab title" "test/fixtures/view — glance" (viewTitleFor viewDir)
      assertEqual "the title, once in the document" 1
                  (T.count "test/fixtures/view — glance" b)

  , testCase "the prefix keys are claimed only where they are ours" $ do
      b <- shell
      holdsAll "chord policy"
        -- A selection keeps C-c and C-x as copy and cut; the reserved chords
        -- reach the browser when they abandon a claimed prefix, which is why
        -- neither profile moves on C-n or C-p.
        [ "if (!selecting()) { e.preventDefault();"
        , "if (MAPS.reserved.indexOf(k) === -1) e.preventDefault();" ] b
      reservedOf b >>= assertEqual "the chords never claimed on their own"
        ["C-l", "C-r", "C-t", "C-w", "C-n", "C-p", "<f5>"]
      -- None of them is bound, so the guard is the only thing that decides.
      (shared, profiles) <- keymapOf b
      reserved <- reservedOf b
      mapM_ (\(name, rows) ->
               assertEqual (T.unpack name <> ": a reserved chord is bound") []
                 [ k | (k, _s, _c, _h, _scope, _help) <- shared <> rows
                     , k `elem` map pure reserved ])
            profiles

  , testCase "the page keys are shared, and no profile claims one" $ do
      b <- shell
      -- The rows are the blob's, shared by both profiles, and neither shadows
      -- them: a bracket is spelled the same in either editor.
      (shared, profiles) <- keymapOf b
      assertEqual "the page keys, shared"
        [(["]"], "next-page"), (["["], "previous-page")]
        [ (k, c) | (k, _s, c, _h, _scope, _help) <- shared
                 , c `elem` ["next-page", "previous-page"] ]
      mapM_ (\(name, rows) ->
               assertEqual (T.unpack name <> ": a profile claims a page key") []
                 [ k | (k, _s, _c, _h, _scope, _help) <- rows
                     , k `elem` [["["], ["]"]] ])
            profiles

  , testCase "the inline glue is JavaScript, where there is a node to say so" $ do
      node <- findExecutable "node"
      case node of
        -- No node on this machine: the syntax of the glue is checked wherever
        -- there is one, and the rest of this group still reads it as text.
        Nothing  -> pure ()
        Just exe -> withTempDir $ \dir -> do
          inline <- glueOf =<< shell
          let path = dir </> "shell.js"
          TIO.writeFile path inline
          (code, _out, err) <- readProcessWithExitCode exe ["--check", path] ""
          assertEqual ("node --check said: " <> err) ExitSuccess code
  ]

-- | What a coarse pointer gets, and what a fine one is spared.  Keys are the
-- interface wherever there are keys; a touch device is the one place they
-- cannot reach, so the filter earns a tap target there and nowhere else.
touchSpec :: IO T.Text -> TestTree
touchSpec shell = testGroup "Touch"
  [ testCase "every page this server serves lays out at the device's own width" $ do
      withAssets <- shell
      bare <- body <$> get missingAssetsDir "/"
      -- Without it a phone lays the page out at 980px and scales it down, so
      -- the table renders at a third of the size it asked for.
      mapM_ (\(what, page') ->
               assertContains what
                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                 page')
            [("the shell", withAssets), ("the JSON-only page", bare)]

  , testCase "a fine pointer sees none of it" $ do
      b <- shell
      -- Everything above is inside the query, and the handler asks the same
      -- query before it runs: with a mouse the page is what it always was.
      let (before, coarse') = T.breakOn "@media (pointer:coarse){" b
      assertBool "no coarse block in the page" (not (T.null coarse'))
      mapM_ (\needle -> assertBool ("a touch rule outside the query: " <> show needle)
                                   (not (needle `T.isInfixOf` before)))
            ["min-height:44px", "#mtext{font-size:16px}", "tv-chips:empty"]
      assertEqual "one coarse block, and one gate on it" 1
                  (T.count "@media (pointer:coarse){" b)
  ]

-- | The shell is monospace, and gets there without asking the network for it.
shellFontSpec :: IO T.Text -> TestTree
shellFontSpec shell = testGroup "Shell type"
  [ testCase "a font in the assets directory is declared and served" $
      withTempDir $ \dir -> do
        TIO.writeFile (dir </> "table-view.js") ""
        BS.writeFile (dir </> "JetBrainsMono-Regular.woff2") "wOF2"
        b <- body <$> get dir "/"
        assertContains "declared" "@font-face{font-family:\"JetBrains Mono\"" b
        assertContains "from this server, by name"
                       "src:url(\"JetBrainsMono-Regular.woff2\") format(\"woff2\")" b
        r <- get dir "/JetBrainsMono-Regular.woff2"
        assertEqual "status" 200 (status r)
        assertEqual "content type" (Just "font/woff2") (header "Content-Type" r)

  , testCase "no page this server serves reaches off it" $ do
      withAssets <- shell
      bare <- body <$> get missingAssetsDir "/"
      mapM_ (\(what, page') -> holdsNone (what <> " fetches")
                                         ["http://", "https://", "@import"] page')
            [("the shell", withAssets), ("the JSON-only page", bare)]
  ]

-- | Assets come out of the configured directory, and only from there.
assetSpec :: TestTree
assetSpec = testGroup "Assets"
  [ testCase "the renderer is served as JavaScript" $ do
      r <- get assetsDir "/table-view.js"
      assertEqual "status" 200 (status r)
      assertEqual "content type"
                  (Just "text/javascript; charset=utf-8") (header "Content-Type" r)

  , testCase "a file the assets directory lacks is a 404" $ do
      r <- get assetsDir "/table-view.css"
      assertEqual "status" 404 (status r)

  , testCase "a traversal segment is not a file name" $ do
      r <- get assetsDir "/.."
      assertEqual "status" 404 (status r)
  ]

errorSpec :: TestTree
errorSpec = testGroup "Errors"
  [ testCase "an unknown path is a 404" $ do
      r <- get assetsDir "/graph"
      assertEqual "status" 404 (status r)
      assertContains "hint" "/headlines" (body r)

  , testCase "a write to a read route is refused" $ do
      let req = (setPath defaultRequest "/headlines") { requestMethod = methodPost }
      application' <- app assetsDir
      r <- runSession (request req) application'
      assertEqual "status" 405 (status r)
      assertContains "hint" "/headline" (body r)

  , testCase "/headline takes GET and POST, and nothing else" $ do
      let req = (setPath defaultRequest "/headline") { requestMethod = methodDelete }
      application' <- app assetsDir
      r <- runSession (request req) application'
      assertEqual "status" 405 (status r)

  , testCase "/ws without an upgrade says what it wants" $ do
      r <- get assetsDir "/ws"
      assertEqual "status" 400 (status r)
      assertContains "hint" "websocket" (body r)
  ]
