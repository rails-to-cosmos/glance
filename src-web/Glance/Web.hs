{-# LANGUAGE TemplateHaskell #-}

-- | The M1 web layer: headlines out of a directory, into a browser tab, and
-- kept current there.
--
-- This component's build-depends names the public @glance@ library and the
-- HTTP packages.  @glance-internal@ is absent, so @Data.Org.*@ is out of scope
-- here and reaching for it means writing the dependency down where anyone
-- reading the stanza sees it.  That is the facade invariant
-- (docs/invariants.md, Architecture), kept where the solver can check it.
--
-- The routes: @GET \/headlines@ is the view JSON, @GET \/@ a demo shell that
-- fetches it, @GET \/ws@ the live row stream, @GET \/NAME@ an asset (the
-- renderer this binary carries, or a file under @--assets@), @\/headline@ the
-- materialize round-trip — @GET@ for
-- one headline's raw subtree, @POST@ to write an edited one back —
-- @POST \/command@ the structured writes, which name rows and let the server
-- compute the spans, @GET \/keywords@ the states those rows may be set to and
-- which scope declares each, @GET \/links@ where one row points,
-- @GET \/tags@ what those rows are tagged with and what else the tree has, and
-- @\/config@ the keyword layers themselves.
-- The view's field set is the contract
-- (@table-view\/SCHEMA.md@), so the load counts ride along as @X-Glance-*@
-- response headers and leave the body's shape alone.
--
-- @\/headlines@ takes @q@, @limit@ and @offset@, filters before it pages, and
-- reports the match count and whether more follows in that same header family.
-- It carries an @ETag@ of the tree's fingerprint and the store's generation
-- under @Cache-Control: no-cache@, so a browser revalidates every time and pays
-- for bytes only when something in the tree moved; @gzip@ sits over the whole
-- HTTP app.  See 'headlines' for why one tag covers every query variant, and
-- 'etagOf' for why the generation alone would not survive a restart.
--
-- Materialize is the one route that writes, and it writes through the store's
-- own coordinates: the subtree extent and the digest of the text it was
-- measured in, both taken at load.  A commit that does not present that digest
-- is refused, and a commit that does never touches the store — the file watch
-- re-parses the file it wrote and streams the rows, so a browser save and an
-- editor save reach open tabs by the same single channel.
--
-- The directory is parsed once, at startup, into 'Glance.Web.Store.Store';
-- every request renders that, and a file watcher re-parses one file per edit
-- (S5).  Org files stay the single source of truth — the store is a projection
-- that dies with the process.
--
-- The socket opens ahead of that parse.  @serve@ binds, hands the walk to a
-- background thread and starts the watch when it lands; until then the three
-- routes that read the store answer @503@ with @Retry-After: 1@ and
-- @{"loading": true, "elapsed": S}@, while @\/@ and the assets serve normally
-- so the shell can render the indexing state and poll.  A 15-second walk used
-- to be 15 seconds of refused connections.
--
-- A page mounts in two steps: @\/headlines@ for the columns and the sort, then
-- the socket's opening @set-rows@ for the rows.  The bootstrap frame is taken
-- in the transaction that subscribes, so an edit landing between the fetch and
-- the socket is in the bootstrap rather than lost, and the server needs no
-- journal to catch a client up.
--
-- The page's keys are 'keyBindings' — ONE map, org-glance's @overview-mode@
-- under org-glance's own command names — and the page carries it as JSON for
-- its own dispatch to parse, so the map and the handlers cannot drift apart.
-- Everything the shell needs comes from this server: inline styles, inline
-- glue, one script by name, and a font only when @--assets@ names a directory
-- holding one (docs\/invariants.md).  That script is 'embeddedRenderer'.
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

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (filterM, forever, join, unless, void, when)
import Data.Aeson ( Object, Value, eitherDecode', encode, object, toJSON, withObject
                  , (.:), (.:!), (.:?), (.=) )
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.Bifunctor (first)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( Header, Status, hCacheControl, hContentType, methodGet
                          , methodHead, methodPost, parseQuery, status200, status304
                          , status400, status404, status405, status409, status413
                          , status500, status503 )
import Network.HTTP.Types.Header (hContentLength, hETag, hIfNoneMatch)
import Network.Wai ( Application, Request (pathInfo, queryString, requestHeaders, requestMethod)
                   , Response, getRequestBodyChunk, responseFile, responseLBS )
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Gzip ( GzipFiles (GzipCompress), defaultGzipSettings
                                   , gzip, gzipFiles )
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import System.FilePath (takeExtension, (</>))
import System.IO (hFlush, stdout)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified Data.Time as Time
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

import Glance.Query ( ConfigLayerFile (..), ConfigLayers (clDirs)
                    , HeadlineParts (..)
                    , HeadlineRecord (hrDigest, hrFile, hrId, hrSubtree, hrTags)
                    , IdCollision (..), QueryResult (..), SortChain
                    , Span (spanEnd, spanStart)
                    , TodoKeywords (..), WalkOptions (..)
                    , WriteFailure (..), addTagEdits, archiveEdits, archived, builtinFilter
                    , captureEdits, captureStamp, captureTargetIn, captureTargetOf
                    , configDirIn, configEdits, currentDocument, defaultCaptureFile
                    , defaultFilter, defaultFilterOf, defaultSortChain
                    , followableTypes
                    , headlineParts, keywordSources, linkColumns, linkType
                    , planningKeywords
                    , planningTimestamp, readConfigLayers, readsAsTimestamp
                    , recomposedSubtree, removeTagEdits, renameTagEdits
                    , replaceSpans, setPlanningEdits, setStateEdits, sortedForViewWith
                    , subtreeLinks
                    , subtreeText, tagColumns, tagText, tagsOfCell
                    , todoLines, viewJSONTextWith )
import Glance.Web.Filter (archiveKey, matchesFilter, namesArchive, storeEnv)
import Glance.Web.Sort (sortChainIn)
import Glance.Web.Store ( Client, Frame (ViewChanged), Hub, LoadState (..)
                        , Store (stConfig, stGen, stPrint), finishLoading, frameText
                        , hubLoad, hubStore, loadStoreWith, newLoadingHub, nextFrame
                        , storeDocument, storeHeadline, storeHeadlines, storeKeywords
                        , storeRecords, storeResult
                        , storeTags, subscribe, unsubscribe )
import Glance.Web.Watch (watchOrgTree)

-- Options

-- | What one server serves.
data ServeOptions = ServeOptions
  { soDir     :: !FilePath          -- ^ org root, walked once at startup and watched after.
  , soPort    :: !Int               -- ^ loopback port to listen on.
  , soAssets  :: !(Maybe FilePath)  -- ^ @--assets@ directory; 'Nothing' serves 'embeddedRenderer'.
  , soDerived :: !Bool              -- ^ serve org-glance's mirror directories too; see 'Data.Org.Walk'.
  } deriving (Eq, Show)

-- | How OPTS wants the tree walked, for the load and for the watch alike: a
-- file the walk passed over must not come back through an inotify event.
walkFor :: ServeOptions -> WalkOptions
walkFor opts = WalkOptions { woIncludeDerived = soDerived opts }

defaultPort :: Int
defaultPort = 7777

-- | The asset the demo shell loads.  Served from 'embeddedRenderer' by default;
-- under @--assets@ its presence in that directory decides which page @\/@ serves.
rendererAsset :: FilePath
rendererAsset = "table-view.js"

-- | The renderer, read at COMPILE time out of the repo's own @assets\/@ and
-- carried in the binary.  This is what makes a built @glance@ self-contained:
-- there is no directory it has to be started beside, no path off this repo in
-- the source, and a clone plus a compiler is the whole of what serving the page
-- takes.  @assets\/table-view.js@ is vendored from the sibling table-view
-- checkout by @make sync-renderer@ and committed like any other file, so the
-- bytes a build embeds are the bytes in the tree.
--
-- @--assets@ overrides it (see 'assetSource'), which is the renderer-hacking
-- loop: point it at @..\/table-view\/web@ and reload instead of rebuilding.
embeddedRenderer :: BS.ByteString
embeddedRenderer = $(makeRelativeToProject "assets/table-view.js" >>= embedFile)

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
      mapM_ putStrLn (bannerLines mode opts assets)
      -- Redirected stdout is block-buffered, and the process then blocks in warp
      -- until it is killed: without this the banner never reaches the log.
      hFlush stdout
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
  putStrLn ("  loaded:  " <> show (length (qrRecords stats)) <> " rows from "
              <> show (qrFiles stats) <> " files in " <> seconds (loaded - started)
              <> collisionNote (qrIdCollisions stats))
  -- Where `+' would write, said once at startup rather than discovered on the
  -- first capture: a target this daemon will not write to is a misconfigured
  -- tree, and the operator learns it here.
  putStrLn ("  capture: " <> either (\why -> T.unpack why <> " — + is refused until it moves")
                                    id (captureTargetIn (soDir opts) (stConfig store)))
  hFlush stdout
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

-- | S rounded the way a banner and an indexing body both want it.
tenths :: Double -> Double
tenths s = fromIntegral (round (s * 10) :: Int) / 10

-- | Is there a renderer to serve?  The route's own question ('assetSource'),
-- asked of the renderer's name, so what the banner and @\/@ report cannot
-- disagree with what @\/table-view.js@ answers.  Without @--assets@ it is
-- always yes; with it, whether that directory holds the file — asked per
-- request as well as at startup, so a directory that fills up later needs no
-- restart.
hasRenderer :: ServeOptions -> IO Bool
hasRenderer opts = isJust <$> assetSource opts rendererAsset

-- | OPTS over HUB as a WAI application.  Exported for the suite: the routes
-- are tested through this, with no socket bound.  A request that is not a
-- websocket upgrade never reaches 'liveSocket', which is why
-- 'Network.Wai.Test' can drive this unchanged.
application :: ServeOptions -> Hub -> Application
application opts hub =
  websocketsOr WS.defaultConnectionOptions (liveSocket hub) (compressed (httpApp opts hub))

-- | The HTTP app under @gzip@.  Inside the websocket branch rather than around
-- it: an upgrade is not a response to rewrite.
--
-- Everything this server sends is text — JSON, HTML, one script — and
-- @defaultGzipSettings@ already names those types and skips a body under 860
-- bytes, below which the header costs more than the compression saves.  What
-- it does not do by default is compress a @responseFile@, and the renderer is
-- one: 'GzipCompress' puts the asset route in too.  The middleware adds
-- @Vary: Accept-Encoding@ to every response it passes, 304s included, which is
-- what keeps a shared cache from serving one encoding for the other.
compressed :: Application -> Application
compressed = gzip defaultGzipSettings { gzipFiles = GzipCompress }

-- | Everything that is not a websocket upgrade: the read routes, the one route
-- that writes, and the 405 for every other method.
--
-- While the startup walk is still running the three routes that read the store
-- answer 'indexing' whatever the method — a commit against a store that is not
-- the directory yet would 404 on a headline the file does have.  @\/@ and the
-- assets are served the whole time: the page they carry is what shows the
-- indexing state and polls for the end of it.
httpApp :: ServeOptions -> Hub -> Application
httpApp opts hub request respond = route >>= respond
  where
    -- HEAD is GET's, once and explicitly: warp drops the body, so every route
    -- that answers a GET answers a HEAD with it and no entry says so twice.
    wanted | requestMethod request == methodHead = methodGet
           | otherwise                           = requestMethod request
    -- The named routes: the path, whether the answer comes out of the store —
    -- which is what makes it a 503 while the walk runs, whatever the method —
    -- the METHODS it takes with the handler for each, and how it spells a 405.
    -- The methods are the table's, so a route that takes a write says so here
    -- rather than in a guard of its own, and the refusal sentence is read off
    -- the same list ('takes').  Everything else is an asset name or a miss.
    named =
      [ ([],            False, textRefusal, [(methodGet, shellPage opts hub)])
      , (["headlines"], True,  textRefusal, [(methodGet, headlines opts hub request)])
      -- @/headline@ is the one read that also writes: materialize a subtree,
      -- and commit it back.
      , (["headline"],  True,  jsonRefusal,
          [ (methodGet, materialize hub (queryId request))
          , (methodPost, commit hub (queryId request) request) ])
      -- @/command@ writes and only writes: there is nothing to read back, since
      -- the rows a command moved arrive over the socket like any other edit.
      , (["command"],   True,  jsonRefusal, [(methodPost, runCommand opts hub request)])
      -- @/config@ is the settings sheet's pair, and reads and writes like
      -- @/headline@: the layers with their digests, and one of them back.
      , (["config"],    True,  jsonRefusal,
          [ (methodGet, configView opts hub)
          , (methodPost, configWrite opts hub request) ])
      , (["keywords"],  True,  textRefusal, [(methodGet, keywordsView hub request)])
      , (["links"],     True,  textRefusal, [(methodGet, linksView hub (queryId request))])
      , (["tags"],      True,  textRefusal, [(methodGet, tagsView hub request)])
      , (["ws"],        True,  textRefusal, [(methodGet, pure (plain status400 wsHint))])
      ]
    route = case [ r | r@(path, _, _, _) <- named, path == pathInfo request ] of
      ((path, needs, refuse, methods) : _) -> do
        load <- readTVarIO (hubLoad hub)
        case load of
          Loading since | needs -> indexing since
          _ready                -> fromMaybe (pure (refuse (takes path methods)))
                                             (lookup wanted methods)
      _noSuchRoute -> fallback
    -- What a route takes, out of the entry's own method names: a route that
    -- grows one says so with no sentence to keep in step.  HEAD is not among
    -- them — it is GET's answer rather than a method a handler declares.
    takes path methods = "/" <> T.intercalate "/" path <> " takes "
                           <> T.intercalate " and " [ TE.decodeUtf8 m | (m, _act) <- methods ]
    -- Two ways to spell a refusal, and which one is the route's own business:
    -- a route a client reads JSON off answers in JSON, naming its own methods,
    -- and a route a browser lands on answers in text, pointing at where a write
    -- goes instead of at what it does not take.
    jsonRefusal = jsonError status405
    textRefusal = const (plain status405 writeHint)
    -- Every one-segment path lands on the assets directory, so the miss below
    -- it doubles as the route list.
    fallback
      | wanted /= methodGet = pure (plain status405 writeHint)
      | otherwise = case pathInfo request of
          [name] | safeName name -> asset opts (T.unpack name)
          _other                 -> pure (plain status404 notFound)
    wsHint    = "/ws is a websocket endpoint; connect with Upgrade: websocket"
    writeHint = "method not allowed; POST /headline?id=… and POST /command write"
    -- Derived from the table above rather than spelled beside it, so a route
    -- added there cannot go missing here.
    notFound  = "not found: "
                  <> T.intercalate ", " [ "/" <> T.intercalate "/" p | (p, _, _, _) <- named ]
                  <> ", or an asset name"

-- | The answer a store route gives while the startup walk is still running: a
-- 503 that says when to come back and how long it has been going.
--
-- 503 with @Retry-After@ rather than an empty 200: an empty view is a tree with
-- no headlines in it, and a client that mounts one has to be told to throw it
-- away later.  The delay is a second because that is the poll the shell runs,
-- and the body carries the elapsed seconds so the page can show them — the walk
-- hands its files over in one batch, so there is no file count to report
-- ('Glance.Web.Store.LoadState').
indexing :: Double -> IO Response
indexing since = do
  now <- getMonotonicTime
  pure . sized status503 [jsonType, ("Retry-After", "1")] . encode
       $ object ["loading" .= True, "elapsed" .= tenths (now - since)]

-- | Is NAME a plain file name, safe to look up inside the assets directory?
-- 'pathInfo' has split the separators away already; what is left to reject is
-- the traversal pair and the empty segment.
safeName :: Text -> Bool
safeName name = not (T.null name)
             && name `notElem` [".", ".."]
             && not (T.any (`elem` ("/\\" :: String)) name)

-- Routes

-- | The view JSON for the configured directory, filtered and paged as REQUEST
-- asks, with the load counts and the page's metadata as headers.  Rendered
-- from the store rather than from a fresh walk: the rows are the ones the
-- startup parse produced, kept current by the watcher, so the response costs a
-- filter and an encode instead of a directory walk.
--
-- @q@ is SCHEMA.md's filter query — field predicates over the view's own
-- columns, free text, negation, and one combination rule: tokens AND,
-- a value's @|@-alternatives OR ('Glance.Web.Filter') — @limit@ a page
-- size, absent meaning the whole set,
-- which is what every client before this asked for, and @offset@ where the page
-- starts.  Filtering happens before paging, so @X-Glance-Total@ is the match
-- count and @X-Glance-Has-Next@ says whether a further page exists.
-- The body stays a View: SCHEMA.md fixes its fields, so paging metadata rides
-- in the same @X-Glance-*@ family the load counts already use.
--
-- Archived rows are left out unless the query says otherwise.  @D@ archives
-- rather than deletes ('runCommand'), so an org tree accumulates rows that are
-- done with rather than gone, and a view that showed them by default would
-- grow without bound.  The rule is one predicate, applied after @q@ and spelled
-- exactly as @-tag:*archive*@ would be: any query naming the archive META
-- through the @tag@ column turns it off ('Glance.Web.Filter.namesArchive') —
-- negated, quoted, whatever — and @X-Glance-Archived@ reports how many rows the
-- exclusion took.  The STARRED spelling alone: @tag:archive@ is the ordinary
-- substring predicate any other tag gets, so a tree using the word for
-- something of its own filters on it without lifting anything.
--
-- A page is cut out of the view's declared sort ('Glance.Query.sortedForView'),
-- never out of walk order — page two has to be the rows the table would show
-- after page one.  With no @limit@ the walk order stands and the client sorts
-- the whole set itself, which is the full-fidelity mode; under a limit a
-- client-side re-sort reorders the loaded page alone.
--
-- @order=document@ is the __experimental__ exception, and it moves both halves
-- together ('Glance.Query.ViewOrder'): the rows stay in walk order whatever the
-- limit, and the view carries no @sort@ field, so a renderer leaves them where
-- they landed.  Walk order is document order — file by file, top entry by top
-- entry down each.  Anything else under @order=@ is a 400; @order=scheduled@
-- names the default.  There is no UI for it: the shell never asks, so a reader
-- reaches it by typing the URL.
--
-- Caching.  The @ETag@ is the tree's fingerprint and the store's generation,
-- which the watcher moves whenever a response would change, and
-- @Cache-Control: no-cache@ makes every browser revalidate rather than guess a
-- lifetime ('etagOf').  One tag serves every query
-- variant: @q@, @limit@ and @offset@ are in the URL, and an HTTP cache is
-- keyed by URL, so @?q=foo@ and @?q=bar@ are separate entries that each
-- revalidate against their own stored tag.  A response is a function of
-- (generation, URL) and nothing else in the request, so no @Vary@ is owed for
-- them — the one header the answer does turn on is @Accept-Encoding@, and the
-- gzip middleware writes that @Vary@ itself.
headlines :: ServeOptions -> Hub -> Request -> IO Response
headlines opts hub request = case pageParams request of
  Left why -> pure (jsonError status400 why)
  Right (q, limit, offset, chain) -> do
    st <- readTVarIO (hubStore hub)
    let tag = etagOf st
    if tag `elem` ifNoneMatch request
      then pure (responseLBS status304 (cacheHeaders tag) "")
      else do
        let qr      = storeResult st
            -- `ref:' reads the link graph, so the query is matched against an
            -- environment carrying the store's rows.  They are the id-resolved
            -- ones the answer is drawn from, which is what makes a reference
            -- point where the table points.
            env     = storeEnv (qrRecords qr)
            asked   = filter (matchesFilter env q) (qrRecords qr)
            matched = if hiding then filter (not . archived) asked else asked
            -- A tree with nothing archived in it pays no pass over the answer:
            -- the WHOLE store's tags say whether the tag is anywhere, filtered
            -- set or not.  The query's half is its own ('namesArchive'), and it
            -- is asked second because the vocabulary is the cheaper refusal.
            hiding  = archiveKey `elem` storeTags st && not (namesArchive q)
            hidden  = length asked - length matched
            total   = length matched
            ordered = sortedForViewWith (storeKeywords st) chain matched
            shown   = maybe matched (\n -> take n (drop offset ordered)) limit
            hasNext = maybe False (\n -> offset + n < total) limit
            body    = TLE.encodeUtf8
                        (viewJSONTextWith chain (viewTitleFor dir) (storeKeywords st) shown)
        -- The encode is lazy, so it needs its own 'try': an exception raised
        -- inside warp's sender would truncate a 200 that has already gone out.
        forced <- try (evaluate (BL.length body))
        pure $ case forced of
          Left err -> plain status500 (renderError err)
          Right _n -> sized status200
            (jsonType : cacheHeaders tag <> statsHeaders qr <> pageHeaders total hasNext hidden)
            body
  where dir = soDir opts
        renderError :: SomeException -> Text
        renderError e = "headline render failed: " <> T.pack (displayException e)

-- | The largest page a client may ask for in one request.  Well past a
-- screenful and well short of a number that means the caller lost track: an
-- explicit @limit@ over this is a mistake worth naming rather than silently
-- trimming.  Asking for no limit at all still serves the whole store.
limitCap :: Int
limitCap = 20000

-- | ST as an entity tag: which tree it was loaded from, and how far that tree
-- has moved since.  Opaque to a client, which only ever compares it to the one
-- it was given — but it has to mean the same thing across a restart, and the
-- generation alone does not: it starts at zero in every process, so a client
-- holding @\"g0\"@ from a daemon since restarted over a rewritten tree would be
-- told 304 and keep a table that is no longer anywhere.  The fingerprint
-- ('Glance.Web.Store.fingerprintOf') moves with the tree and the generation
-- with the edits inside one process, so the pair revalidates only what is still
-- true.  Sixteen hex digits of it: a cache key, not a signature.
etagOf :: Store -> BSC.ByteString
etagOf st = "\"" <> TE.encodeUtf8 (T.take 16 (stPrint st))
              <> "-g" <> BSC.pack (show (stGen st)) <> "\""

-- | The tags REQUEST would accept as unchanged.  @If-None-Match@ is a
-- comma-separated list and each entry may be weak, so both are handled — a
-- browser echoes the one tag it holds, and a proxy in between may not.
ifNoneMatch :: Request -> [BSC.ByteString]
ifNoneMatch request =
  [ strong (BSC.dropWhile (== ' ') entry)
  | raw <- maybe [] pure (lookup hIfNoneMatch (requestHeaders request))
  , entry <- BSC.split ',' raw ]
  where strong t = fromMaybe t (BSC.stripPrefix "W/" t)

-- | What every @\/headlines@ answer carries, 304 included: revalidate always,
-- and the tag to revalidate with.
cacheHeaders :: BSC.ByteString -> [Header]
cacheHeaders tag = [(hETag, tag), (hCacheControl, "no-cache")]

-- | What the page covers of the filtered set, and what the answer left out.
-- @X-Glance-Archived@ is how many rows matched the query and were dropped for
-- carrying the archive tag: the count is the only trace of an exclusion nobody
-- asked for, and a client showing it can tell "nothing matches" from "the
-- matches are all archived".  It is zero whenever the query named the key, so a
-- reader who asked for archived rows is never told any were withheld.
pageHeaders :: Int -> Bool -> Int -> [Header]
pageHeaders total hasNext hidden =
  [ ("X-Glance-Total", BSC.pack (show total))
  , ("X-Glance-Has-Next", if hasNext then "true" else "false")
  , ("X-Glance-Archived", BSC.pack (show hidden)) ]

-- | @q@, @limit@, @offset@ and the CHAIN the rows are served in, out of
-- REQUEST's query string, or what is wrong with one of them.  An absent
-- parameter is its default — no filter, no limit, the top of the set,
-- 'defaultSortChain' — and a present one that is not a number is a 400 rather
-- than a silent fallback to it, since a mistyped page size that quietly serves
-- the whole store looks like a working request.  @order@ is spelled out for the
-- same reason: a misspelling that silently served the sorted view would look
-- exactly like a working one.
--
-- The chain is read TWICE from two places, in one order: @order=@ picks the
-- base — the default chain, or the empty one for document order — and the
-- query's own @sort:@ tokens override it where it names any
-- ('Glance.Web.Sort.sortChainIn'), so a reader who states an order states the
-- whole of it.  A sort token this producer cannot read is this request's 400,
-- naming the token.
pageParams :: Request -> Either Text (Text, Maybe Int, Int, SortChain)
pageParams request = do
  q      <- maybe (Right "") text (raw "q")
  limit  <- traverse count (raw "limit")
  offset <- maybe (Right 0) count (raw "offset")
  base   <- maybe (Right defaultSortChain) ordering (raw "order")
  chain  <- sortChainIn base q
  case limit of
    Just n | n > limitCap -> Left ("limit is at most " <> T.pack (show limitCap)
                                     <> "; page with offset for more")
    _within                -> Right (q, limit, offset, chain)
  where
    -- Experimental, and the only way to reach the empty chain — walk order,
    -- with no @sort@ declared.  @scheduled@ names the default so a client can be
    -- explicit about the ordinary case; the name is a fossil of the days the
    -- default was one @scheduled@ key.
    ordering named = do
      t <- text named
      case t of
        "document"  -> Right []
        "scheduled" -> Right defaultSortChain
        _unknown    -> Left "order must be scheduled or document"
    -- A parameter with no @=@ reads as absent, so @?limit@ is not a zero page.
    raw name = case lookup (TE.encodeUtf8 name) (queryString request) of
      Just (Just bytes) -> Just (name, bytes)
      _absent           -> Nothing
    text (name, bytes) = first (const (name <> " is not UTF-8")) (TE.decodeUtf8' bytes)
    -- Read as an 'Integer' first: a query string can spell a number no 'Int'
    -- holds, and wrapping one would page from a negative offset.
    count named@(name, _bytes) = do
      t <- text named
      case TR.decimal t :: Either String (Integer, Text) of
        Right (n, rest) | T.null rest, n >= 0, n <= toInteger (maxBound :: Int)
                            -> Right (fromInteger n)
        _notANumber         -> Left (name <> " must be a whole number, 0 or more")

-- Materialize

-- | The largest commit body this server reads, in bytes.  A subtree is org
-- text and a megabyte of it is an enormous one; past that the request is a
-- mistake or an attack, and either way the answer is a 413.
bodyLimit :: Int
bodyLimit = 1024 * 1024

-- | @GET \/headline?id=…@: one headline's subtree as its file spells it, plus
-- the digest a commit has to present and the extent the text was cut from.
--
-- The id travels in the query string rather than in the path.  A row id is
-- @FILE#K@ — slashes and a hash — so a path segment would have to be
-- percent-encoded by every client and decoded here, while WAI has already
-- decoded the query string by the time this runs.  The hash is the sharper
-- half: spelled into a URL raw it opens a FRAGMENT and the id never reaches the
-- server at all, which is why the shell builds this with @encodeURIComponent@.
--
-- Every field comes out of the store, which is the read model.  The offsets
-- and the digest then describe one document, the text this process parsed:
-- re-reading the file here would answer with a digest for bytes the extent was
-- never measured against, and the disagreement would only surface as a splice
-- landing in the wrong place.
--
-- The same subtree arrives twice, whole and split.  @org@ is what it has always
-- been; @body@, @properties@, @planning@ and @logbook@ are
-- 'Glance.Query.headlineParts' — the text with the headline's own regions lifted
-- out, and each region beside it — so a client can edit them apart without
-- holding an org parser of its own.  The split is the server's for exactly that
-- reason, and a client that wants neither ignores the lot.
--
-- @logbook@ rides out and never back: it is shown and not edited, and
-- 'Glance.Query.recomposedSubtree' takes it off the record whatever a commit
-- says.  The properties in 'Glance.Query.hiddenProperties' are not even shown —
-- @ORG_GLANCE_ID@ is the row id a client keys its updates off, so the drawer a
-- sheet edits is the drawer minus the thing that names it.
materialize :: Hub -> Maybe Text -> IO Response
materialize _hub Nothing = pure (jsonError status400 "GET /headline?id=<row id>")
materialize hub (Just rid) = do
  found <- storeHeadline rid <$> readTVarIO (hubStore hub)
  pure $ case found of
    Nothing -> jsonError status404 ("no headline with id " <> rid)
    Just r  -> let parts = headlineParts r in jsonResponse status200
      [ "id"         .= hrId r
      , "file"       .= hrFile r
      , "org"        .= subtreeText r
      , "body"       .= hpBody parts
      , "properties" .= [ [key, value] | (key, value) <- hpProperties parts ]
      , "planning"   .= [ [key, value] | (key, value) <- hpPlanning parts ]
      , "logbook"    .= hpLogbook parts
      , "digest"     .= hrDigest r
      , "span"       .= object [ "start" .= spanStart (hrSubtree r)
                               , "end"   .= spanEnd (hrSubtree r) ]
      ]

-- | @POST \/headline?id=…@ with body @{"org": …, "digest": …}@: the headline's
-- subtree replaced by the text the client edited.
--
-- Or @{"body": …, "properties": [[key, value], …], "digest": …}@, which is the
-- same write with the drawer named apart: the subtree is recomposed here
-- ('Glance.Query.recomposedSubtree') and everything past that point is
-- identical, the drift lock and the digest chain included.  A body naming both
-- shapes is a 400 — which of two texts to write is not a thing to guess at.
--
-- Two digest checks, one lock.  The client's digest must be the one the store
-- holds, or the file has been re-parsed since and the client is editing a
-- subtree measured at offsets that have moved; and 'replaceSpans' re-digests
-- the file itself, which catches a change that has not reached the store yet.
-- Both are a 409 with the file untouched, and both mean the same thing to a
-- client: materialize again, because the text it edited is not there any more.
--
-- Nothing here touches the store.  The write goes to the file, the watch sees
-- it, re-parses it and streams the rows — so a browser save reaches every open
-- tab by the path an editor's save already takes, and there is one update
-- channel rather than one plus a special case for the writer we happen to know
-- about.  The text itself is taken as given: org validity is the author's
-- business, and a file that stops parsing keeps the rows it had
-- (docs/invariants.md), exactly as when the text came from Emacs.
commit :: Hub -> Maybe Text -> Request -> IO Response
commit _hub Nothing _request = pure (jsonError status400 "POST /headline?id=<row id>")
commit hub (Just rid) request = do
  body <- takeBody bodyLimit request
  found <- storeHeadline rid <$> readTVarIO (hubStore hub)
  case prepare rid body found of
    Left refusal -> pure refusal
    Right (r, digest, org) ->
      answerWrite rewritten (\fresh -> ["digest" .= fresh])
        <$> replaceSpans (hrFile r) digest [(hrSubtree r, org)]

-- | What writing RID needs — the record, the digest to pin and the text to
-- splice — or the response refusing to.  Every refusal but the write's own is
-- decided here, so the IO above it is the write and nothing else.
prepare :: Text -> Maybe BL.ByteString -> Maybe HeadlineRecord
        -> Either Response (HeadlineRecord, Text, Text)
prepare rid body found = case (body, found) of
  (Nothing, _) -> Left (jsonError status413 ("body over " <> T.pack (show bodyLimit) <> " bytes"))
  (_, Nothing) -> Left (jsonError status404 ("no headline with id " <> rid))
  (Just raw, Just r) -> case parseCommit raw of
    Left why -> Left (jsonError status400 why)
    Right (asked, digest)
      | digest /= hrDigest r  -> Left (conflict "stale" (hrDigest r) reparsed)
      -- Named rather than counted: a sheet showing three planning rows has to
      -- say which one it will not write.  Its own shape rather than 'conflict''s,
      -- since nothing about it is a digest and a client reading @digest@ off a
      -- 409 is reading the lock its next write would present.
      | Just key <- badPlanning asked -> Left (jsonResponse status409
          [ "error" .= unreadable key, "reason" .= ("planning" :: Text), "field" .= key ])
      | otherwise             -> Right (r, digest, committed r asked)

-- | The subtree ASKED for, over R: the raw text as given, or the client's parts
-- composed back into one — with the server's own put back beside them.
committed :: HeadlineRecord -> Commitment -> Text
committed _r (WholeSubtree org)         = org
committed r  (SplitSubtree body ps pln) =
  recomposedSubtree r (HeadlineParts body ps pln "")

-- | The planning entry ASKED for that no timestamp parser would read back, if
-- any.  Checked ahead of the write and named in the refusal, because the cost of
-- letting one through is silent: the line stops being a planning line on the
-- next load and the entry the author set is body text.
badPlanning :: Commitment -> Maybe Text
badPlanning (WholeSubtree _org) = Nothing
badPlanning (SplitSubtree _body _ps pln) =
  listToMaybe ([ key | (key, _v) <- pln, key `notElem` planningKeywords ]
                 <> [ key | (key, value) <- pln, not (readsAsTimestamp value) ])

-- | A 409 spelling REASON, the digest the file carries now, and WHY.  The two
-- ways a materialized subtree goes stale are told apart for a client that has
-- to decide what to do next; both mean the same thing to one that does not.
-- Every write route answers a moved file this way, so WHY carries the whole
-- sentence rather than half of one this appends to.
conflict :: Text -> Text -> Text -> Response
conflict reason current why = jsonResponse status409
  [ "error"  .= why
  , "reason" .= reason
  , "digest" .= current
  ]

-- | Why KEY's planning entry was refused.
unreadable :: Text -> Text
unreadable key = key <> " is not a timestamp org would read back"
  <> "; spell it <2026-08-01 Sat> or clear the row"

reparsed, rewritten, configMoved :: Text
reparsed  = "the file was re-read since this subtree was materialized" <> again
rewritten = "the file changed on disk since this subtree was materialized" <> again
configMoved = "the config file changed on disk since it was read; open settings again"

captureMoved :: FilePath -> Text
captureMoved path =
  T.pack path <> " changed on disk while the entry was being written; capture it again"

-- | What a drift-locked write answers: the file's new digest with whatever else
-- OK wants beside it, the 409 MOVED spells, or the 500 the engine's own refusal
-- is.
--
-- Three routes write files and all three answer this way, so the sentence a
-- moved file gets is the only thing any of them chooses — which is what keeps a
-- client's handling of a refusal the same whichever route it asked.
answerWrite :: Text -> (Text -> [Pair]) -> Either WriteFailure Text -> Response
answerWrite moved ok written = case written of
  Right fresh              -> jsonResponse status200 (ok fresh)
  Left (WriteDrift onDisk) -> conflict "drift" onDisk moved
  Left (WriteRefused why)  -> jsonError status500 why

again :: Text
again = "; materialize it again and re-apply the edit"

-- Keywords

-- | @GET \/keywords?ids=A,B@: the states those rows may be set to, laid out as
-- the chain that classifies them.
--
-- @{"sources": [{"source": …, "active": […], "inactive": […]}], "unknown": […]}@.
-- One entry per SOURCE in precedence order — @default@, org's own cycle, then
-- @system.org@, then the rows' tags, then their own files — and a keyword
-- appears under the WIDEST source that declares it and nowhere below it
-- ('Glance.Query.keywordSources', which is the whole of the rule).  So the
-- answer classifies as well as enumerates: it is 'Data.Org.Config.classify' read
-- forwards, and a palette drawing it shows a reader why @READING@ is active here
-- and done-with two directories over.
--
-- FOUR sources and no union row.  What the recognition union adds is which
-- words PARSE as states, which is neither what classifies a row nor what a row
-- may be set to; the chain is both, and it is the chain
-- 'Glance.Query.setStateEdits' checks a write against, so what this offers for
-- one row is exactly what that accepts for it.
--
-- Resolved for the TARGET ROWS rather than for the tree, which is what makes it
-- worth a request: the store's badge palette is the union of every file loaded,
-- and this is the part of it that answers for the rows a command is about to
-- run over.  Several ids — the marked set — merge by source name.
--
-- Refusals follow the command route's, since the caller is the same key: no ids
-- at all is a 400, and an id the store has no row for is named in @unknown@ and
-- left out of the resolution, so a stale marked set still answers for the rows
-- that are there.  A read, so it writes nothing and pins nothing — the digest a
-- write presents is the row's, and @\/headlines@ already carries it.
keywordsView :: Hub -> Request -> IO Response
keywordsView hub request = do
  st <- readTVarIO (hubStore hub)
  let (found, unknown) = storeHeadlines asked st
  pure $ if null asked
    then jsonError status400 "GET /keywords?ids=<row id>,<row id>"
    else jsonResponse status200
           [ "sources" .= map sourceJSON (keywordSources (stConfig st) found)
           , "unknown" .= unknown
           ]
  where asked = queryIds request
        sourceJSON (source, kw) = object ("source" .= source : keywordsPair kw)

-- Tags

-- | @GET \/tags?ids=A,B@: what the rows a tag command is about to run over are
-- tagged with, and what else the tree has to offer.
--
-- @{"rows": [{"id": …, "tags": […]}], "vocabulary": […], "counts": {…},
-- "unknown": […]}@.
-- @rows@ is in the order the ids were named, each row's tags in the order its
-- FILE spells them, FOLDED — the same 'Glance.Query.tagsOfCell' reading the
-- filter vocabulary and 'Glance.Query.tagged' use, so what this reports about a
-- row is exactly what a write to it will find there.  @vocabulary@ is the whole
-- store's tag list ('Glance.Web.Store.storeTags'), which is what a completing
-- read has to narrow over: the rows a page holds are a fraction of the tree and
-- a palette offering only their tags could not reach the rest.
--
-- PER ROW rather than as one union, because the client needs to know WHICH rows
-- lack a tag: adding one over a marked set writes the rows that do not carry it
-- and no others, and the union cannot say which those are.  The union, its
-- coverage counts and their order are the popup's, computed off this.
--
-- @counts@ is how many ROWS the whole store has under each tag, which is the
-- one thing the popup's third column cannot work out from the rows in hand: a
-- reader deciding whether to drop a tag wants to know whether it is this set's
-- or the tree's.  It is counted per request rather than kept, because the
-- store's own 'Glance.Web.Store.stTags' counts FILES — a different question,
-- and one no arithmetic recovers a row count from.  One pass over the resolved
-- rows, at the cost of a keystroke.
--
-- Refusals follow @\/keywords@', since the caller is the same key: no ids at all
-- is a 400, and an id the store has no row for is named in @unknown@ and left
-- out, so a stale marked set still answers for the rows that are there.  A read
-- — it pins nothing, and the digest a write presents is the row's.
tagsView :: Hub -> Request -> IO Response
tagsView hub request = do
  st <- readTVarIO (hubStore hub)
  let (found, unknown) = storeHeadlines asked st
  pure $ if null asked
    then jsonError status400 "GET /tags?ids=<row id>,<row id>"
    else jsonResponse status200
           [ "rows"       .= [ object [ "id" .= hrId r, "tags" .= tagsOfCell (hrTags r) ]
                             | r <- found ]
           , "vocabulary" .= storeTags st
           , "counts"     .= tagRowCounts st
           , "unknown"    .= unknown
           ]
  where asked = queryIds request

-- | How many of ST's rows carry each tag, over every tag any of them carries.
--
-- A row counts ONCE per tag however often its file spells one, which is what
-- makes the number answer "how many rows would a @TAG:@ predicate reach" — the
-- question the popup's column asks.
--
-- The empty cell is skipped rather than read: `tagsOfCell` lowercases and
-- re-tokenises whatever it is given, and nearly half the corpus's rows carry no
-- tag at all.
tagRowCounts :: Store -> Map Text Int
tagRowCounts st = Map.fromListWith (+)
  [ (tag, 1 :: Int) | r <- storeRecords st, not (T.null (hrTags r))
                    , tag <- nub (tagsOfCell (hrTags r)) ]

-- Links

-- | @GET \/links?id=ROW@: where that row points.
--
-- @{"links": [{"target": …, "desc": …, "type": …}]}@, in order of appearance and
-- one entry per target ('Glance.Query.subtreeLinks').  The rule is the DISPLAY
-- rule the table already answers to: a bracket link is described by its @DESC@,
-- or by its target where it has none, and a bare @http(s)@ or @mailto:@ URL
-- describes itself.
--
-- @type@ is the target's SCHEME, folded ('Glance.Query.linkType'): the popup
-- draws it as a badge column and @o@ reads it to decide whether a browser tab
-- can be pointed at the target.  Derived here rather than in the page for the
-- reason the rest of this answer is — it is org text work, and a JS copy would
-- be a second rule to keep in step.
--
-- Extracted here rather than in the page because it is org text work.  The
-- shell holds no org parser and must not grow one, and the bracket grammar this
-- reads is the one 'Glance.Query.displayText' already holds — a second copy in
-- JavaScript would be a second grammar to keep in step with SCHEMA.md's link
-- rule.
--
-- The SUBTREE, not the cells: a reader pressing @o@ on a row means the entry,
-- and an entry keeps its references in its body.  A read, so it pins nothing
-- and 404s an id the store has no row for, the way materialize does.
linksView :: Hub -> Maybe Text -> IO Response
linksView _hub Nothing = pure (jsonError status400 "GET /links?id=<row id>")
linksView hub (Just rid) = do
  found <- storeHeadline rid <$> readTVarIO (hubStore hub)
  pure $ case found of
    Nothing -> jsonError status404 ("no headline with id " <> rid)
    Just r  -> jsonResponse status200
      [ "links" .= [ object [ "target" .= target, "desc" .= desc
                            , "type" .= linkType target ]
                   | (target, desc) <- subtreeLinks r ] ]

-- | The rows REQUEST names, deduplicated: every @ids@ parameter, each a comma
-- separated list, and every @id@ parameter, each ONE id — the way
-- @POST \/command@ takes either spelling.  An empty name is dropped rather than
-- looked up, so a trailing comma costs nothing.
--
-- The two keys differ on the comma deliberately.  @id@ has to mean on this
-- route what it means on @\/headline@ ('queryId'), and a row id that carries a
-- comma is ordinary — the fallback is @path#ordinal@ and a path may hold one.
-- Percent-encoding does not help, since the split happens after decoding and
-- @%2C@ arrives as a separator; the spelling that always works is one parameter
-- per id, which is what the shell writes.  @ids=a,b@ is for a caller typing one
-- out.
queryIds :: Request -> [Text]
queryIds request =
  nub [ rid
      | (key, Just raw) <- queryString request, key `elem` ["ids", "id"]
      , Right text <- [TE.decodeUtf8' raw]
      , rid <- if key == "ids" then T.splitOn "," text else [text], not (T.null rid) ]

-- Config

-- | The config directories a settings client edits: the ones the walk met, and
-- the one the served root WOULD hold when it met none.
--
-- The walk is what can answer the first half — an org-glance store is not
-- obliged to sit at the root being served, and in the author's own tree it does
-- not — and only the second half is a guess, which is the one case where there
-- is nothing to be right about yet.
configDirsOf :: ServeOptions -> Store -> [FilePath]
configDirsOf opts st = case clDirs (stConfig st) of
  []   -> [configDirIn (soDir opts)]
  dirs -> dirs

-- | @GET \/config@: the keyword layers a settings client edits, and the union
-- they add up to.
--
-- One entry per config file — where it is, which layer it is (@tag@ is null for
-- @system.org@), its @#+TODO:@ lines verbatim, and the digest a write to it is
-- pinned to.  A layer whose digest is EMPTY is not there yet; @system.org@ in a
-- tree that never had one is listed anyway, since posting to it is how it comes
-- to exist.
--
-- The files are read here rather than taken off the store's loaded
-- 'Glance.Query.ConfigLayers'.  What a client is handed is the lock its write
-- is checked against, so it has to be the digest of the very bytes it was
-- shown; the store supplies the one thing a read cannot, which is WHICH
-- directories.
--
-- @keywords@ is the store's own palette, so the preview is the badge list the
-- table is already showing rather than a second computation of it — and it is
-- wider than the layers on purpose: a file's own @#+TODO:@ is in it, which is
-- exactly the thing this page cannot edit and has to say so about.
configView :: ServeOptions -> Hub -> IO Response
configView opts hub = do
  st <- readTVarIO (hubStore hub)
  layers <- readConfigLayers (configDirsOf opts st)
  pure (jsonResponse status200
          [ "layers"   .= map layerJSON layers
          , "keywords" .= keywordsJSON (storeKeywords st)
          -- What the table opens on, and what a bare `g' applies: read off the
          -- files beside the lines, since it is a line of the same file and its
          -- write rides in the same request.  The capture target is the second
          -- line of that kind and travels the same way.
          , "filter"   .= servedFilter layers
          -- Empty rather than null where no layer names one: the fallback here
          -- is a PATH this server computes rather than a value to show, and the
          -- settings field's placeholder is what says so.
          , "capture"  .= fromMaybe "" (systemLine captureTargetOf layers)
          ])

-- | The default view LAYERS name, or the built-in where none does.  The system
-- layer's line, read off the same bytes the digests were taken from, so what a
-- settings sheet shows and what its write is pinned to describe one file.
--
-- 'Glance.Query.defaultFilter' answers the same question off the loaded config,
-- and the two cannot disagree: both take the first SYSTEM layer that names a
-- line and fall back to 'builtinFilter', and a file that is not there names
-- nothing either way.
servedFilter :: [ConfigLayerFile] -> Text
servedFilter layers = fromMaybe builtinFilter (systemLine defaultFilterOf layers)

-- | The value READ takes off the first SYSTEM layer that names one, or
-- 'Nothing' where none does.  Both tree-wide settings are read this way, off
-- the same bytes the digests were taken from, so what a settings sheet shows
-- and what its write is pinned to describe one file.
systemLine :: (Text -> Maybe Text) -> [ConfigLayerFile] -> Maybe Text
systemLine reader layers =
  listToMaybe [ v | l <- layers, Nothing <- [lfTag l], Just v <- [reader (lfText l)] ]

layerJSON :: ConfigLayerFile -> Value
layerJSON f = object
  [ "path"   .= lfPath f
  , "tag"    .= lfTag f
  , "lines"  .= todoLines (lfText f)
  , "digest" .= lfDigest f
  ]

keywordsJSON :: TodoKeywords -> Value
keywordsJSON = object . keywordsPair

-- | One keyword set as the two fields every answer spells it with.  Shared, so
-- the settings preview and one source of the resolution table cannot disagree
-- about what a cycle looks like on the wire.
keywordsPair :: TodoKeywords -> [Pair]
keywordsPair kw = ["active" .= tkActive kw, "inactive" .= tkInactive kw]

-- | @POST \/config@ with body @{"path": …, "lines": […], "digest": …}@: one
-- layer's @#+TODO:@ block replaced, and nothing else in the file touched.
--
-- @path@ has to be a layer @GET \/config@ would list, and that is the whole of
-- the traversal defence: the request names one of the files this server just
-- offered, never a path it is handed.  Looking the layer up is also the read
-- the edits are measured in, so the two cannot describe different files.
--
-- The write is every other write: 'Glance.Query.configEdits' turns the lines
-- into span edits over the file's own text and 'Glance.Query.replaceSpans'
-- splices them under the client's digest, temp file and rename, so a comment,
-- a @#+TITLE:@ and a capture template around the block come back byte for
-- byte.  A file that moved since the client read it is a 409 with nothing
-- written.  The EMPTY digest is the pin for a layer that does not exist yet:
-- the same lock says "nothing is there", the write creates it and the
-- directories over it, and a file that turned up meanwhile drifts.
--
-- Nothing here touches the store.  A config file is watched
-- ('Glance.Web.Watch.settle'), and a change to one is the event that reseeds
-- the whole tree — so the rows and the palette arrive by the path an editor
-- saving the same file already takes.
configWrite :: ServeOptions -> Hub -> Request -> IO Response
configWrite opts hub request = do
  body <- takeBody bodyLimit request
  st <- readTVarIO (hubStore hub)
  case body of
    Nothing  -> pure (jsonError status413 ("body over " <> T.pack (show bodyLimit) <> " bytes"))
    Just raw -> case parseConfigWrite raw of
      Left why   -> pure (jsonError status400 why)
      Right want -> writeLayer (configDirsOf opts st) want

-- | WANT written into one of DIRS' layers, or the refusal.  The lookup that
-- decides which file is the read the edits are then measured in, so the two
-- cannot be describing different bytes.
writeLayer :: [FilePath] -> (Text, [Text], Maybe Text, Maybe Text, Text) -> IO Response
writeLayer dirs (path, asked, want, target, digest) = do
  layers <- readConfigLayers dirs
  case find ((== path) . T.pack . lfPath) layers of
    Nothing -> pure (jsonError status400 (noSuchLayer path layers))
    Just f  -> case configEdits (lfText f) asked (systemOnly f want)
                                (systemOnly f target) of
      Left why    -> pure (jsonError status400 why)
      Right edits -> answerWrite configMoved written
                       <$> replaceSpans (lfPath f) digest edits
  where
    -- Both tree-wide lines are the SYSTEM layer's and no other's, so a tag
    -- layer's write leaves them alone whatever the request said.
    systemOnly f v = maybe v (const Nothing) (lfTag f)
    written fresh = ["path" .= path, "digest" .= fresh]

noSuchLayer :: Text -> [ConfigLayerFile] -> Text
noSuchLayer path layers =
  "no config layer at " <> path <> "; this tree has "
    <> T.intercalate ", " [ T.pack (lfPath f) | f <- layers ]

-- | RAW as a layer write, or what is wrong with it.  @lines@ is an array
-- because a layer can spell its cycle over more than one @#+TODO:@ line, and a
-- client editing them as text splits on its own newlines rather than asking
-- this server to.
-- @filter@ and @capture@ are the default view and the capture target the same
-- file names, and both are optional: absent leaves that line exactly as it is,
-- empty takes it away, and anything else writes it.  They ride in this one
-- request because they are lines of the same file — three requests would be
-- three writes under three digests, each of which the one before it had just
-- invalidated.
parseConfigWrite :: BL.ByteString -> Either Text (Text, [Text], Maybe Text, Maybe Text, Text)
parseConfigWrite raw = first (("body: " <>) . T.pack) $ do
  value <- eitherDecode' raw
  parseEither (withObject "config write" shape) value
  where shape o = (,,,,) <$> o .: "path" <*> o .: "lines"
                         <*> o .:? "filter" <*> o .:? "capture" <*> o .: "digest"

-- Commands

-- | What a command request asks for: the command its name resolved to, the rows
-- it names, its arguments, and any digests the client wants the write pinned to.
--
-- A 'Command' cannot be built without its 'CommandSpec' ('parseCommand'), which
-- is what puts the unknown-name refusal ahead of everything else: what reaches
-- the planner is a command this server implements, and the NAME has no reader
-- left past the lookup that resolved it.
data Command = Command
  { cmdSpec    :: !CommandSpec      -- ^ its entry in 'commands'.
  , cmdIds     :: ![Text]           -- ^ in the order named, deduplicated; empty for @capture@.
  , cmdArgs    :: !Args             -- ^ whatever @args@ carried.
  , cmdDigests :: !(Map Text Text)  -- ^ id to the digest the client holds for its file.
  }

-- | The @args@ object, read once for every command that takes one.  Six
-- fields between them, and a request naming one command leaves the rest absent:
-- @keyword@ is @set-state@'s state and @set-planning@'s planning keyword — one
-- field because the wire spells both that way — @date@ is the timestamp text,
-- @text@ is the line @capture@ writes, @tag@ is the one @add-tag@ and
-- @remove-tag@ move, and @from@\/@to@ are @rename-tag@'s pair.
--
-- The two nested 'Maybe's are the distinction the whole command layer turns on:
-- ABSENT is a request that said nothing, which is a 400, and NULL is a request
-- that asked for the value to come off.  The other four are flat, since none of
-- those commands has a value to clear: a tag comes off through @remove-tag@
-- rather than through a null.
--
-- @rename-tag@ spells its pair @from@\/@to@ rather than reusing @tag@ for one
-- half: the two are symmetric, and naming one of them @tag@ would leave a
-- reader of the request guessing which end it was.
data Args = Args
  { agKeyword :: !(Maybe (Maybe Text))
  , agDate    :: !(Maybe (Maybe Text))
  , agText    :: !(Maybe Text)
  , agTag     :: !(Maybe Text)
  , agFrom    :: !(Maybe Text)
  , agTo      :: !(Maybe Text)
  }

-- | One file's share of a command: the write it costs, and the ids it answers
-- for.  Every row of a file shares the file's digest, so the plan carries one.
data FilePlan = FilePlan
  { fpPath   :: !FilePath
  , fpDigest :: !Text
  , fpRows   :: ![(Text, [(Span, Text)])]  -- ^ row id, and the spans it moves.
  }

-- | The span edits a command asks for on ONE row, under the store's config and
-- the timestamp the request resolved to ('resolveDate').  A refusal here is the
-- WHOLE request's, since half a write over a marked set is worse than none of
-- one.
type RowEdits = ConfigLayers -> Maybe Text -> Args -> HeadlineRecord
              -> Either Text [(Span, Text)]

-- | One command: what its @args@ owe, whether its date is resolved against the
-- server's today, and what it does to a row.  'Nothing' for the edits is the
-- command that names no rows — it MAKES one, so there is no row function to
-- hold and no ids rule to apply.
data CommandSpec = CommandSpec
  { csArgs  :: Args -> Maybe Text  -- ^ why the request's shape is refused, where it is.
  , csDated :: Bool                -- ^ its @date@ is read against today, once per request.
  , csEdits :: Maybe RowEdits      -- ^ what it does to each named row.
  }

-- | The commands this route implements, which is also the whole of what @args@
-- can mean: @set-state@ takes @{"keyword": "DONE"}@ or @{"keyword": null}@,
-- @set-planning@ takes @{"keyword": "SCHEDULED", "date": "+3d"}@ or a null
-- date, @capture@ takes @{"text": "TODO Buy milk :errands:"}@, @add-tag@ and
-- @remove-tag@ take @{"tag": "work"}@, @rename-tag@ takes
-- @{"from": "work", "to": "projects"}@, and @archive@ takes nothing.
--
-- ONE table, so a command is one entry rather than a name spelled into a list,
-- two guards and a case arm: 'commandNames' is its keys, 'parseCommand' refuses
-- a name it does not carry and asks the entry's own 'csArgs' for the rest, and
-- 'runCommand' reads the edits off the entry the name resolved to.  A lookup
-- past the parse cannot miss, which is what leaves no arm for a command that is
-- not there.
commands :: [(Text, CommandSpec)]
commands =
  [ ("add-tag", CommandSpec (wantsTag "add-tag") False
      (Just (\_cfg _stamp args r -> Right (addTagEdits (tagOf args) r))))
    -- @archive@ is 'addTagEdits' at a fixed name, and it takes no @tag@ of its
    -- own: the name is org's and a client cannot aim it elsewhere.  Idempotent,
    -- so a row already tagged costs no edit and still answers @ok@.
  , ("archive", CommandSpec (const Nothing) False
      (Just (\_cfg _stamp _args r -> Right (archiveEdits r))))
  , ("capture", CommandSpec wantsText False Nothing)
  , ("remove-tag", CommandSpec (wantsTag "remove-tag") False
      (Just (\_cfg _stamp args r -> Right (removeTagEdits (tagOf args) r))))
    -- One command rather than a @remove-tag@ and an @add-tag@ a client fires in
    -- turn: the rename is ONE drift-locked write per file, and the pair it
    -- would compose from spells the tag onto the title
    -- ('Glance.Query.renameTagEdits').
  , ("rename-tag", CommandSpec wantsRename False
      (Just (\_cfg _stamp args r ->
               Right (renameTagEdits (word agFrom args) (word agTo args) r))))
    -- The keyword is there and the date has been resolved: 'csArgs' refuses a
    -- @set-planning@ without either, so neither can refuse per row.
  , ("set-planning", CommandSpec wantsPlanning True
      (Just (\_cfg stamp args r ->
               setPlanningEdits (fromMaybe "" (join (agKeyword args))) stamp r)))
  , ("set-state", CommandSpec wantsState False
      (Just (\cfg _stamp args r -> setStateEdits cfg (join (agKeyword args)) r)))
  ]
  where
    word field = fromMaybe "" . field
    tagOf = word agTag
    -- The one command whose keyword may be NULL: that is how a state comes off,
    -- so what it owes is the FIELD rather than a value ('Args').
    wantsState args
      | Nothing <- agKeyword args =
          Just "set-state wants args {\"keyword\": \"DONE\"}, or a null keyword to clear it"
      | otherwise = Nothing
    wantsPlanning args
      | Nothing <- join (agKeyword args) =
          Just "set-planning wants args {\"keyword\": \"SCHEDULED\", \"date\": \"+3d\"}"
      | Nothing <- agDate args =
          Just "set-planning wants a date, or a null one to take the entry off"
      | otherwise = Nothing
    wantsText args
      | Nothing <- agText args =
          Just "capture wants args {\"text\": \"TODO Buy milk :errands:\"}"
      | otherwise = Nothing
    -- The charset is a property of the STRING, so it is refused here with the
    -- rest of the request's shape rather than once per row: a word that is not
    -- a tag is not a tag for any of them.
    wantsTag name args = case agTag args of
      Nothing    -> Just (name <> " wants args {\"tag\": \"work\"}")
      Just given -> either Just (const Nothing) (tagText given)
    -- BOTH ends take the charset wall, for one reason: a string that is not a
    -- tag is not a tag for any row, so a @from@ org could not have written
    -- names nothing, and a @to@ it could not read would take the whole run down
    -- into title text on the next load.
    wantsRename args = case (agFrom args, agTo args) of
      (Just from, Just to) -> either Just (const Nothing) (tagText from >> tagText to)
      _absent -> Just "rename-tag wants args {\"from\": \"work\", \"to\": \"projects\"}"

-- | The names 'commands' carries, in the order it carries them — which is the
-- order a refusal lists them in.
commandNames :: [Text]
commandNames = map fst commands

-- | @POST \/command@ with body @{"name": …, "ids": […], "args": {…}}@: a
-- structured write over the rows the client names.  @"id"@ is accepted for an
-- @"ids"@ of one, since a command over the row at point is the common one.
--
-- The edits themselves are computed in 'Glance.Query'
-- ('Glance.Query.setStateEdits', 'Glance.Query.archiveEdits') — a headline's
-- spans are the private sublibrary's and this layer cannot see them, which is
-- the facade invariant doing its job rather than an inconvenience.  What this
-- route owns is which rows, which file, and what the answer says.
--
-- Batching is per FILE, and is the point of the route.  Ids are grouped by the
-- file their rows came from and each file is written ONCE
-- ('Glance.Query.replaceSpans') under that file's own digest, so a marked set
-- spanning three files is three atomic writes rather than one per row, and each
-- of the three is all-or-nothing — a rejected batch writes nothing.  There is
-- no rollback ACROSS files and none is possible: a rename that has happened
-- cannot be undone by a later failure.  The answer reports per id instead,
-- which is what a client showing @archived (5)@ and a line per refusal needs.
--
-- Refusals split by whose mistake they are.  A body that is not a command, a
-- name nothing implements, no ids at all, and a keyword some named row's own
-- classification chain does not declare are all 400 with nothing written — the
-- last one refuses the WHOLE request deliberately, since half a state change
-- over a marked set is worse than none of one.  Per id: an id the store has no
-- row for, and a file whose digest moved.  A 200 is therefore "the command ran",
-- never "every row moved"; the results say which did.
--
-- Nothing here touches the store, exactly as with @POST \/headline@: the write
-- goes to the file, the watch re-reads it and streams the rows, so a browser
-- command reaches every open tab by the path an editor's save takes.
runCommand :: ServeOptions -> Hub -> Request -> IO Response
runCommand opts hub request = do
  body <- takeBody bodyLimit request
  st <- readTVarIO (hubStore hub)
  -- The cap outranks every other refusal, the way it does on the other write
  -- route: this server declines to read a megabyte to find out what it says.
  case body of
    Nothing  -> pure (jsonError status413 tooBig)
    Just raw -> case parseCommand raw of
      Left why -> pure (jsonError status400 why)
      -- The one command with no row function makes a row rather than editing
      -- one, and the 'Maybe' is destructured HERE, once: what goes down to the
      -- planner is the edits themselves, so nothing below has an arm for a
      -- command that edits nothing.
      Right cmd -> case csEdits (cmdSpec cmd) of
        Nothing -> captureInto opts st cmd
        -- The clock is read ONCE per request, ahead of any row, so a marked set
        -- crossing midnight cannot land on two days.  Everything that can refuse
        -- is then decided before a file is opened, so what is left is either the
        -- 400 or the IO that writes.
        Just edits -> do
          stamp <- resolveDate cmd
          either (pure . jsonError status400) id
                 (stamp >>= \at -> overRows st at edits cmd)
  where
    tooBig = "body over " <> T.pack (show bodyLimit) <> " bytes"

-- | CMD's rows written, as the IO that writes them or the 400 that stops it.
-- STAMP is @set-planning@'s date already worked out, and is nothing to every
-- other name.
overRows :: Store -> Maybe Text -> RowEdits -> Command -> Either Text (IO Response)
overRows st stamp edits cmd = do
  (plans, said) <- planCommand st stamp edits cmd
  pure $ do
    written <- mapM writeOne plans
    -- Answered in the order the client named the ids, so a caller can zip the
    -- results against what it asked for.
    let outcomes = said <> concat written
    pure (jsonResponse status200
            ["results" .= [ v | rid <- cmdIds cmd, Just v <- [lookup rid outcomes] ]])

-- | The timestamp CMD's rows are to carry: its date text rendered against the
-- server's today ('Glance.Query.planningTimestamp'), 'Nothing' where the
-- request asked for the entry to come off, and 'Nothing' for a command that has
-- no date at all.
--
-- Worked out ONCE, before any row is planned, for two reasons: a marked set
-- crossing midnight would otherwise land on two days, and a text no date parser
-- reads is the WHOLE request's 400 the way an undeclared keyword is — half a
-- reschedule over a marked set is worse than none of one.  The answer is handed
-- on as a value rather than written back into 'Args', so @agDate@ means the text
-- the client typed everywhere and at every point in the request.
resolveDate :: Command -> IO (Either Text (Maybe Text))
resolveDate cmd = case join (agDate (cmdArgs cmd)) of
  Just text | csDated (cmdSpec cmd) -> do
    today <- Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime
    pure (Just <$> planningTimestamp today text)
  _nothingToResolve -> pure (Right Nothing)

-- | @capture@: CMD's line appended to the tree's capture target as a top entry.
--
-- The target comes out of the config ('Glance.Query.captureTargetIn'), which is
-- also where a target this daemon will not write to is refused — so a misspelled
-- pragma is a 400 naming itself rather than a file written outside the tree.
--
-- The write is every other write: the document and the digest come off the STORE
-- where it holds the file, since that is the text this server last read, and off
-- a fresh read where it does not — a target that is not there yet reads as the
-- empty document under the empty digest, which is what
-- 'Data.Org.Edit.editFile' creates under.  Either way the offset and the lock
-- describe one text.  Nothing here touches the store: the watch re-reads the
-- file and streams the new row, exactly as it does for a capture out of Emacs.
captureInto :: ServeOptions -> Store -> Command -> IO Response
captureInto opts st cmd = case captureTargetIn (soDir opts) (stConfig st) of
  Left why   -> pure (jsonError status400 why)
  Right path -> do
    (doc, digest) <- maybe (currentDocument path) pure (storeDocument path st)
    now <- Time.getZonedTime
    case captureEdits doc (captureStamp now) (fromMaybe "" (agText (cmdArgs cmd))) of
      Left why    -> pure (jsonError status400 why)
      Right edits -> answerWrite (captureMoved path) (landed path)
                       <$> replaceSpans path digest edits
  where landed path fresh = ["ok" .= True, "file" .= path, "digest" .= fresh]

-- | PLAN's file written once, and what that came to for each of its ids.  Both
-- outcomes are shared by the whole group, because the write is: the batch lands
-- or the file is untouched.
writeOne :: FilePlan -> IO [(Text, Value)]
writeOne plan = report <$> replaceSpans (fpPath plan) (fpDigest plan) spliced
  where
    spliced = concatMap snd (fpRows plan)
    report written = [ (rid, either (refused rid . why) (done rid) written)
                     | (rid, _edits) <- fpRows plan ]
    why (WriteDrift found) = T.pack (fpPath plan) <> " changed on disk (it digests to "
                               <> T.take 12 found <> "… now); nothing was written to it"
    why (WriteRefused spelled) = spelled

-- | CMD as the files to write and the ids refused without opening one, or as
-- the 400 that stops it.  Two refusals are decided here
-- rather than in the IO above: an id the store has no row for, and a digest the
-- client pinned that the store no longer holds, which is the same @stale@ check
-- @POST \/headline@ makes and is per file because a digest is per file.
planCommand :: Store -> Maybe Text -> RowEdits -> Command
            -> Either Text ([FilePlan], [(Text, Value)])
planCommand st stamp rowEdits cmd = do
  rows <- mapM withEdits held
  let groups = groupOn (hrFile . fst) rows
  pure ( [ FilePlan path (hrDigest r0) [ (hrId r, edits) | (r, edits) <- rs ]
         | (path, rs@((r0, _) : _)) <- groups, not (stale rs) ]
       , missing <> [ (hrId r, refused (hrId r) (staleWhy path))
                    | (path, rs) <- groups, stale rs, (r, _edits) <- rs ] )
  where
    -- One resolution for the whole set rather than one per id, which is what
    -- keeps a marked set of a hundred rows off a hundred passes of the store.
    (held, absent) = storeHeadlines (cmdIds cmd) st
    withEdits r = (,) r <$> rowEdits (stConfig st) stamp (cmdArgs cmd) r
    missing = [ (rid, refused rid ("no headline with id " <> rid)) | rid <- absent ]
    stale rs = or [ pinned /= hrDigest r
                  | (r, _edits) <- rs, Just pinned <- [Map.lookup (hrId r) (cmdDigests cmd)] ]
    staleWhy path = T.pack path
                      <> " has been re-read since these rows were fetched; ask for them again"

-- | One row's outcome: the file's new digest, so a caller can pin its next
-- write without re-reading.
done :: Text -> Text -> Value
done rid digest = object [ "id" .= rid, "ok" .= True, "digest" .= digest ]

-- | One row's refusal, shaped like the success it replaces.
refused :: Text -> Text -> Value
refused rid why = object [ "id" .= rid, "ok" .= False, "error" .= why ]

-- | XS grouped by KEY: each group in arrival order, the groups in first-seen
-- order.  Quadratic in the number of distinct files, which is the marked set's
-- size and not the store's.
groupOn :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOn key xs = [ (k, [ x | x <- xs, key x == k ]) | k <- nub (map key xs) ]

-- | RAW as a command, or what is wrong with it.  Every refusal that is the
-- request's shape rather than the tree's state is decided here, so what reaches
-- 'planCommand' is a name it implements with rows to run it over.
--
-- The name is resolved to its entry in 'commands' FIRST and the 'Command' is
-- built out of that entry, so an unimplemented name never reaches a rule that
-- would have to guess what it takes.  What is left is the entry's own two:
-- whether it names rows, and what its @args@ owe ('csArgs').
parseCommand :: BL.ByteString -> Either Text Command
parseCommand raw =
  first (("body: " <>) . T.pack) (eitherDecode' raw >>= parseEither command) >>= checked
  where
    command = withObject "command" $ \o -> do
      name <- o .: "name"
      one <- o .:? "id"
      several <- o .:? "ids"
      digests <- o .:? "digests"
      -- @.:!@ rather than @.:?@ for the two nullable fields, and that is the
      -- whole of how ABSENT is told from NULL: @.:?@ folds a null into an
      -- absence, which would make @{"args": {}}@ an instruction to clear.
      -- A request with no @args@ at all reads as an empty one, so there is no
      -- second shape to carry.
      a <- fromMaybe mempty <$> (o .:? "args" :: Parser (Maybe Object))
      parsed <- Args <$> a .:! "keyword" <*> a .:! "date" <*> a .:? "text" <*> a .:? "tag"
                     <*> a .:? "from" <*> a .:? "to"
      pure ( name :: Text, nub (maybe [] pure one <> fromMaybe [] several)
           , parsed, fromMaybe Map.empty digests )
    checked (name, ids, args, digests) = case lookup name commands of
      Nothing -> Left ("no such command: " <> name <> "; this server runs "
                         <> T.intercalate " and " commandNames)
      Just spec
        | isJust (csEdits spec), null ids ->
            Left "a command names rows: {\"ids\": [\"…\"]}, or {\"id\": \"…\"} for one"
        | Just why <- csArgs spec args -> Left why
        | otherwise -> Right (Command spec ids args digests)

-- | The @id@ parameter of REQUEST, when it carries one with a value.
queryId :: Request -> Maybe Text
queryId request = case lookup "id" (queryString request) of
  Just (Just raw) -> either (const Nothing) Just (TE.decodeUtf8' raw)
  _absent         -> Nothing

-- | The subtree a commit body asks to write.  Two spellings of one thing: the
-- text whole, or the parts a client edits apart for one editing them as panes
-- ('Glance.Query.recomposedSubtree' puts them back together).
data Commitment
  = WholeSubtree !Text  -- ^ @org@: the subtree as it is to be written.
  | SplitSubtree !Text ![(Text, Text)] ![(Text, Text)]
      -- ^ @body@, @properties@ and @planning@, to be composed.
  deriving (Eq, Show)

-- | What a commit body asks for and the digest it pins the write to, or what is
-- wrong with it.  @digest@ is read first, so a body missing it says so whichever
-- of the two shapes it was reaching for; naming both shapes is refused rather
-- than resolved, and @body@ owes @properties@ and @planning@ beside it — an
-- absent one would read as "and drop that region", which is too much to infer
-- from a field a client forgot.
--
-- The two regions the SERVER owns — the hidden properties and the logbook — are
-- in neither shape, and a split body naming them writes nothing: they are taken
-- off the record on the way back in.
parseCommit :: BL.ByteString -> Either Text (Commitment, Text)
parseCommit raw = first (("body: " <>) . T.pack) $ do
  value <- eitherDecode' raw
  parseEither (withObject "commit" shape) value
  where
    shape o = do
      digest <- o .: "digest"
      org <- o .:? "org"
      body <- o .:? "body"
      asked <- case (org, body) of
        (Just _, Just _)   -> fail "name either \"org\" or \"body\", not both"
        (Just text, _)     -> pure (WholeSubtree text)
        (_, Just text)     -> SplitSubtree text <$> (traverse pair =<< o .: "properties")
                                                <*> (traverse pair =<< o .: "planning")
        (Nothing, Nothing) ->
          fail "no \"org\", and no \"body\" with \"properties\" and \"planning\" either"
      pure (asked, digest)
    pair [key, value] = pure (key, value)
    pair _other       = fail "each property is a [key, value] pair"

-- | At most LIMIT bytes of REQUEST's body, or 'Nothing' when there are more of
-- them.  Chunk by chunk rather than through 'Network.Wai.strictRequestBody': a
-- cap that measures the body once it is all in memory has already paid for
-- what it means to refuse.
takeBody :: Int -> Request -> IO (Maybe BL.ByteString)
takeBody limit request = go 0 []
  where
    go seen chunks = do
      chunk <- getRequestBodyChunk request
      let taken = seen + BS.length chunk
      if BS.null chunk        then pure (Just (BL.fromChunks (reverse chunks)))
        else if taken > limit then pure Nothing
        else go taken (chunk : chunks)

-- Live socket

-- | @\/ws@: one @set-rows@ with everything the store holds, then a frame per
-- change.  Anything else is refused, so a mistyped path fails loudly rather
-- than sitting open sending nothing.
--
-- @?bootstrap=off@ drops that opening frame for a client that already has the
-- rows — the shell fetches @\/headlines@ and would otherwise be sent the whole
-- store a second time.  The subscription is unchanged: the mailbox is
-- registered in the same transaction, so the snapshot is still taken and only
-- thrown away.  What such a client gives up is the gap the snapshot closes —
-- an edit landing between its fetch and its subscribe reaches it on the next
-- write to that file rather than at once — which is why the default stands.
--
-- An upgrade arriving before the startup walk lands is refused with the same
-- 503 and @Retry-After@ the HTTP routes give, rather than accepted onto a store
-- that is not the directory yet — a @set-rows@ of an empty store is a claim
-- about the tree.  Refusing is also what the shell already handles: it opens
-- its socket only after @\/headlines@ has answered.
liveSocket :: Hub -> WS.ServerApp
liveSocket hub pending
  | wsPath /= "/ws" = WS.rejectRequest pending "glance streams rows at /ws"
  | otherwise = do
      load <- readTVarIO (hubLoad hub)
      case load of
        Loading _since -> WS.rejectRequestWith pending WS.defaultRejectRequest
          { WS.rejectCode    = 503
          , WS.rejectMessage = "Service Unavailable"
          , WS.rejectHeaders = [("Retry-After", "1")]
          , WS.rejectBody    = "{\"loading\":true}"
          }
        Loaded -> do
          conn <- WS.acceptRequest pending
          WS.withPingThread conn 30 (pure ()) $ do
            (cid, client, boot) <- atomically (subscribe hub)
            when (bootstrapWanted requested) (send conn boot)
            pump conn client `finally` unsubscribe hub cid
  where requested = WS.requestPath (WS.pendingRequest pending)
        wsPath    = BSC.takeWhile (/= '?') requested

-- | Does the socket opened at PATH want the @set-rows@ bootstrap?  Everything
-- but @bootstrap=off@ does, so the default survives a typo.
bootstrapWanted :: BSC.ByteString -> Bool
bootstrapWanted path = ("bootstrap", Just "off") `notElem` parseQuery query
  where query = BSC.dropWhile (/= '?') path

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
        -- The mailbox filled: this client fell behind a write storm and the
        -- watcher will not wait for it (Glance.Web.Store).  The backlog is
        -- gone, so what the close owes the client is the ONE thing that
        -- replaces any backlog — re-ask for rows.  Named for that rather than
        -- for the client's speed: the shell answers it by revalidating
        -- /headlines and re-attaching, keeping the page it had.
        Nothing          -> WS.sendClose conn ("resync" :: Text)
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
  -- Two files claiming one row id: the view carries one of them
  -- ('Glance.Query.resolveIds') and this is how many it had to choose between.
  , count "X-Glance-Id-Collisions"   (length (qrIdCollisions qr))
  ]
  where count name n = (name, BSC.pack (show n))

-- | Where NAME's bytes come from under OPTS, and 'Nothing' when nothing serves
-- it.  The two cases are exclusive: @--assets@ names the whole set of assets or
-- the binary does, so a directory without a renderer in it does not silently
-- fall back on the compiled one — that is the case 'assetsMissing' reports.
--
-- The one oracle for what this server has: 'asset' serves what it returns,
-- 'hasRenderer' and 'localFont' ask it whether a name is there at all.  A page
-- that declares a resource this would decline is the drift it exists to
-- prevent.
assetSource :: ServeOptions -> FilePath -> IO (Maybe (Either FilePath BS.ByteString))
assetSource opts name = case soAssets opts of
  Nothing  -> pure (if name == rendererAsset then Just (Right embeddedRenderer) else Nothing)
  Just dir -> let path = dir </> name
              in (\ok -> if ok then Just (Left path) else Nothing) <$> doesFileExist path

-- | An asset out of 'assetSource', or a 404 naming what was looked for.  Under
-- @--assets@ only files directly in that directory are reachable — one segment,
-- no traversal.  Every one-segment path lands here, so the miss doubles as the
-- route list: @\/graph@ is a mistyped route rather than a missing file, and
-- reads better when told so.
--
-- One response either way, so the wire cannot tell a compiled-in renderer from
-- a file: the same 'mimeOf' content type, and 'compressed' compresses both.  A
-- @responseFile@ because 'GzipCompress' says to and the middleware takes the
-- length off the file; the compiled bytes through 'sized', which is what gives
-- the middleware a @Content-Length@ to compare against its threshold — a
-- @responseLBS@ without one is left uncompressed.
asset :: ServeOptions -> FilePath -> IO Response
asset opts name = maybe missing serveFrom <$> assetSource opts name
  where
    serveFrom (Left onDisk)      = responseFile status200 [contentType] onDisk Nothing
    serveFrom (Right compiledIn) = sized status200 [contentType] (BL.fromStrict compiledIn)
    contentType = (hContentType, mimeOf name)
    missing = plain status404 (T.intercalate "\n"
      [ "no such asset: " <> T.pack name
      , "this server serves /, /headlines, and " <> case soAssets opts of
          Nothing  -> "the " <> T.pack rendererAsset <> " it carries"
          Just dir -> "file names under " <> T.pack dir ])

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
  ".woff"  -> "font/woff"
  ".ttf"   -> "font/ttf"
  _        -> "application/octet-stream"

-- Type

-- | The type stack the shell asks for, in the table, the sheet and the widgets
-- alike.  Nothing is fetched for it: these are names looked up on the machine,
-- and 'fontFace' adds an @\@font-face@ only when an @--assets@ directory holds
-- a file to point at.  A page that reaches the network for a font is a page
-- that renders differently offline, and this one is served over loopback to a
-- machine that may have none (docs\/invariants.md).
monoStack :: Text
monoStack = "\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"

-- | The font files the shell will use when the assets directory holds one,
-- best first.  With neither there, 'monoStack' falls through to whatever is
-- installed and the page says nothing about it.
fontAssets :: [FilePath]
fontAssets = ["JetBrainsMono-Regular.woff2", "JetBrainsMono-Regular.ttf"]

-- | The first of 'fontAssets' this server can serve.  Asked of 'assetSource',
-- the way the renderer is, so a font is declared exactly when the route would
-- answer for it: per request, and never for a name the binary does not carry.
--
-- No font is embedded and none is invented — 'embeddedRenderer' is the whole of
-- what the binary carries — so with no @--assets@ there is no @\@font-face@ and
-- 'monoStack' is the whole story, exactly as a build without a font file beside
-- the renderer behaved before.
localFont :: ServeOptions -> IO (Maybe FilePath)
localFont opts = listToMaybe <$> filterM (fmap isJust . assetSource opts) fontAssets

-- | An @\@font-face@ for NAME, which the asset route serves out of the same
-- directory the renderer comes from.  The @src@ is a bare file name, resolved
-- against this server the way the renderer's own @src@ is.
fontFace :: Maybe FilePath -> Text
fontFace Nothing     = ""
fontFace (Just name) = T.concat
  [ "  @font-face{font-family:\"JetBrains Mono\";font-display:swap;"
  , "src:url(\"", T.pack name, "\") format(\"", format, "\")}" ]
  where format | takeExtension name == ".woff2" = "woff2"
               | otherwise                      = "truetype"

-- Keymap

-- | One row of the shell's keymap.
data KeyBinding = KeyBinding
  { kbKeys    :: ![Text]        -- ^ the keys in order; what the dispatch matches.
  , kbCommand :: !Text          -- ^ the command name the echo widget shows.
  , kbHandler :: !(Maybe Text)  -- ^ the shell function running it; 'Nothing' is staged.
  , kbScope   :: !Text          -- ^ @table@, @modal@ or @any@ — where it is live.
  , kbHelp    :: !(Maybe Text)  -- ^ what it does, when the command name does not say; see 'helps'.
  }

-- | KEYS bound to a command.  The notation the echo widget shows is derived
-- rather than stored — the keys with one space between them, the way Emacs
-- spells a sequence ('keyBindingsJSON').
bind :: [Text] -> Text -> Maybe Text -> Text -> KeyBinding
bind keys command handler scope = KeyBinding keys command handler scope Nothing

-- | B with the one line the echo widget shows past its command name.  A row
-- earns one where the name is the Emacs name for a key whose behaviour here is
-- narrower than the name — @save-buffer@ on a sheet that syncs itself, and the
-- @keyboard-quit@ that flushes on the way out.
helps :: KeyBinding -> Text -> KeyBinding
helps b text' = b { kbHelp = Just text' }

-- | The map, whole.  There is ONE, and every row in it is live wherever its
-- scope is: the movement profiles this used to carry are gone, and with them
-- the question of which keys a reader has.  @n@\/@p@ and @j@\/@k@ both step a
-- row, @f@\/@b@ and @l@\/@h@ both step a cell — the spellings cost a row each
-- and nothing else, where a profile cost a selector, a stored choice, a URL
-- parameter and a key line that had to be rewritten whenever it moved.
--
-- These are org-glance's command names (@org-glance-overview-mode-map@, plus
-- @C-x C-s@ for the sheet, which is Emacs's) wherever org-glance has one, and a
-- descriptive name where it does not.  A row with no handler is recognized in
-- full and then says what it is waiting for: the map is complete ahead of the
-- daemon commands that will back it (M4), which reads better than a key that
-- silently does nothing.
--
-- Claimed chords, and only these.  @C-c@ becomes a prefix while no text field
-- has focus and the selection is collapsed, so a copy is still a copy; @C-x@
-- likewise, and only while the sheet is open, which is the only place @C-x
-- C-s@ means anything.  @RET@, @TAB@, @\/@ and @DEL@ are taken while the table
-- has focus — @DEL@ is the filter's own undo, and a field with focus keeps its
-- backspace.  @C-l@, @C-r@, @C-t@, @C-w@, @C-n@, @C-p@ and @\<f5\>@ are never
-- claimed on their own, which is why nothing here moves on @C-n@ or @C-p@;
-- what the reserved list buys is the /abandoned/ prefix — @C-x C-l@ reaches the
-- browser rather than being swallowed as undefined.  Completing a bound
-- sequence still claims them, which is what makes @C-c C-t@ work.
keyBindings :: [KeyBinding]
keyBindings =
  -- Movement.  Two spellings of each, and the arrows over both.  The order
  -- matters in one place only: the resident key line shows the FIRST row bound
  -- to a command ('keyHints'), so the letters lead and the line reads
  -- @n\/p rows@ rather than @\<down\>\/\<up\> rows@.
  [ bind ["n"]          "next-row"                        (Just "nextRow")        "table"
  , bind ["p"]          "previous-row"                    (Just "previousRow")    "table"
  , bind ["j"]          "next-row"                        (Just "nextRow")        "table"
  , bind ["k"]          "previous-row"                    (Just "previousRow")    "table"
  , bind ["<down>"]     "next-row"                        (Just "nextRow")        "table"
  , bind ["<up>"]       "previous-row"                    (Just "previousRow")    "table"
  , bind ["f"]          "next-column"                     (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["b"]          "previous-column"                 (Just "previousColumn") "table"
      `helps` previousColumnHelp
  , bind ["l"]          "next-column"                     (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["h"]          "previous-column"                 (Just "previousColumn") "table"
      `helps` previousColumnHelp
  -- The arrows ride with the letters on BOTH axes, and silently: the key line
  -- shows a command's FIRST binding, so `<down>' has always sat behind `n' and
  -- these sit behind `f' and `b' the same way.  Same handler, so walking off
  -- either end is the landing it is for the letters rather than a wall.
  , bind ["<right>"]    "next-column"                     (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["<left>"]     "previous-column"                 (Just "previousColumn") "table"
      `helps` previousColumnHelp
  -- The ends of the buffer, org-glance's own pair, plus vi's @G@ beside @>@.
  -- Progressive: the page's end row, then the previous or next page's, so the
  -- pair reaches the ends of the whole set without reaching for the brackets.
  , bind ["<"]          "first-row"                       (Just "firstRow")       "table"
      `helps` firstRowHelp
  , bind [">"]          "last-row"                        (Just "lastRow")        "table"
      `helps` lastRowHelp
  , bind ["G"]          "last-row"                        (Just "lastRow")        "table"
      `helps` lastRowHelp
  , bind ["]"]          "next-page"                       (Just "nextPage")       "table"
  , bind ["["]          "previous-page"                   (Just "previousPage")   "table"
  -- The order the rows are in, over the column the cell keys are standing in:
  -- table-view's own @^@ (@table-view-sort-cycle@ there), which is why the key
  -- is that one.  It REVERSES rather than cycling off — the handle states an
  -- order and offers no way back to none — and it refuses a whole-row selection
  -- rather than guessing which column was meant.
  , bind ["^"]          "toggle-sort"                     (Just "toggleSort")     "table"
      `helps` "put this column at the head of the order; again reverses it"
  , bind ["RET"]        "org-glance-overview:materialize" (Just "materializeRow") "table"
  , bind ["/"]          "filter-rows"                     (Just "focusFilter")    "table"
      `helps` "summon the filter palette"
  , bind ["DEL"]        "filter-drop-token"               (Just "filterDrop")     "table"
      `helps` "drop the filter's last token"
  -- The default view, as the tree configures it (@#+GLANCE_DEFAULT_FILTER:@).
  , bind ["g"]          "apply-default-filter"            (Just "applyDefault")   "table"
      `helps` "the view this tree opens on"
  , bind ["m"]          "mark-toggle"                     (Just "markToggle")     "table"
      `helps` "toggle this row's mark, then step down"
  , bind ["u"]          "unmark"                          (Just "unmarkRow")      "table"
      `helps` "take this row's archive flag off, else its mark, then step down"
  , bind ["U"]          "unmark-all"                      (Just "unmarkAll")      "table"
      `helps` "every mark and every archive flag off"
  , bind ["M"]          "mark-all"                        (Just "markAll")        "table"
      `helps` "mark every row loaded"
  , bind ["q"]          "quit-window"                     (Just "quitWindow")     "table"
  , bind ["TAB"]        "org-cycle"                       Nothing                 "table"
    -- Where the row points, out of its own subtree: one link opens, several
    -- raise the popup that lists them, none says so.  Two spellings,
    -- org-glance's own.
  , bind ["o"]          "org-glance-overview:open"        (Just "openLinks")      "table"
      `helps` openHelp
  , bind ["!"]          "org-glance-overview:open"        (Just "openLinks")      "table"
      `helps` openHelp
    -- A canned VIEW rather than a mode: the active rows carrying a date,
    -- earliest first.  `g' is the way back.
  , bind ["a"]          "org-glance-agenda"               (Just "applyAgenda")    "table"
      `helps` "the active rows carrying a date, earliest first"
    -- The drill: the rows pointing AT the one at point, applied as a `ref:'
    -- view with a crumb left behind.  A look rather than a bulk act, so it
    -- takes the row at point and never the marked set.
  , bind ["@"]          "org-glance-overview:relations"   (Just "relations")      "table"
      `helps` "the rows referring to this one; DEL walks back"
  -- The one command that names no row: it writes a new entry into the tree's
  -- capture target, which is a line of the system config.
  , bind ["+"]          "org-glance-overview:capture"     (Just "capture")        "table"
      `helps` "a headline for the inbox, typed as org"
  -- dired's flag, and dired's @dd@: the first press flags the row and the second
  -- archives every flagged row at once — @D@'s own job, reached through @D@'s own
  -- handler, so a lone flag is a set of one and the single-row flow is the
  -- general one.  The flag IS the confirmation, so there is no prompt and no
  -- undo to build — @u@ takes it off.  Plain @d@ is never a write on its own,
  -- which is what makes a mis-key cost a keystroke.
  , bind ["d"]          "archive-flag"                    (Just "archiveFlag")    "table"
      `helps` "flag for archive; d again archives all flagged"
  , bind ["D"]          "org-glance-overview:delete"      (Just "archiveRows")    "table"
      `helps` "archive the flagged rows, or the row at point — never a delete"
      -- The user's own spelling; Chromium owns Ctrl+T above the document, so
      -- the org chord stays as the secondary for browsers that deliver it.
  , bind ["t"]          "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "set the state of the marked rows, or the row at point"
  , bind ["C-c", "C-t"] "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "the org spelling, where the browser lets it through"
      -- The agenda's own key for the same question over there.  One palette, and
      -- it STAYS UP: managing tags is several ops, where setting a state is one.
  , bind [":"]          "org-agenda-set-tags"             (Just "manageTags")     "table"
      `helps` "add or drop tags over the marked rows, or the row at point"
      -- Both chords survive the browser, where @C-c C-t@ does not: @Ctrl+S@ and
      -- @Ctrl+D@ are page default actions rather than chrome shortcuts, so
      -- @preventDefault@ on the completing chord is the whole of what they need.
  , bind ["C-c", "C-s"] "org-glance-overview:schedule"    (Just "schedulePlan")   "table"
      `helps` planningHelp
  , bind ["C-c", "C-d"] "org-glance-overview:deadline"    (Just "deadlinePlan")   "table"
      `helps` planningHelp
  , bind [","]          "customize"                       (Just "openSettings")   "table"
      `helps` "the settings sheet: general, theme, keyword cycles"
  , bind ["C-x", "C-s"] "save-buffer"                     (Just "save")           "modal"
      `helps` "sync the sheet now; again to overwrite a conflict"
  , bind ["C-c", "'"]   "org-edit-special"                (Just "toggleRaw")      "modal"
      `helps` "the sheet as raw org, or as body and properties; sync an edited one first"
  , bind ["ESC"]        "keyboard-quit"                   (Just "cancel")         "any"
      `helps` "close the sheet, syncing an edited one; again to discard"
  ]

-- | The cell-movement help lines, one pair for the two spellings of each: the
-- keys differ, what they do does not.  Between them they say the whole rule —
-- the column rides along with row movement, and a whole-row selection starts at
-- the first column whichever direction asks for one.
nextColumnHelp, previousColumnHelp :: Text
nextColumnHelp     = "the cell to the right; row movement keeps the column"
previousColumnHelp = "the cell to the left; from a whole row, the first column"

-- | The buffer-end help lines: each key takes the page's end row, and taking it
-- again turns the page onto the SAME end of the next one, which is what makes
-- the pair walk the whole set.  The two spellings of @last-row@ share a line.
firstRowHelp, lastRowHelp :: Text
firstRowHelp = "first row, again = page up"
lastRowHelp  = "last row, again = page down"

-- | The reschedule help line, shared by the two keys: what they take differs by
-- one word and what a reader has to know does not.
planningHelp :: Text
planningHelp = "a date over the marked rows, or the row at point; empty clears it"

-- | The open help line, shared by the two spellings of the one command.
openHelp :: Text
openHelp = "follow this row's link; several list them"

-- | Chords the browser needs more than this page does: never claimed as the key
-- that abandons a prefix this map had entered, which is what leaves @C-x C-l@
-- to the browser.  One completing a bound sequence is still claimed — that is
-- what makes @C-c C-t@ work — and none of them is bound on its own.
reservedChords :: [Text]
reservedChords = ["C-l", "C-r", "C-t", "C-w", "C-n", "C-p", "<f5>"]

-- | The commands auto-repeat is taken off: one press, one token.  Movement
-- wants the repeat — a held @n@ is how you cross a table, and the renderer
-- coalesces those to a frame — but a held @DEL@ would walk the whole query away
-- between one glance at the chips and the next.  By command name, so a command
-- two keys spell is off under both.
--
-- @m@ and @u@ stay off it: both advance, so a held one walks a column rather
-- than working one row twice (docs\/invariants.md).  The writes are on it for a
-- different reason — a held key must not be a hundred @\/command@ requests — and
-- @d@ needs it most of all: a repeat that survived here would flag a row and
-- archive it from ONE press, which is exactly the confirmation the two-press
-- shape exists to be.
onceCommands :: [Text]
onceCommands = [ "filter-drop-token", "unmark-all", "mark-all"
               , "archive-flag", "org-glance-overview:delete"
                 -- Neither writes a file, and both are ruinous held down: a
                 -- leaned-on `o' is a browser tab per repeat, and a leaned-on
                 -- `a' is a remount per repeat.  `@' is a remount per repeat
                 -- too, and each one leaves a crumb: a held key would build a
                 -- trail of identical steps for DEL to walk back one at a time.
               , "org-glance-overview:open", "org-glance-agenda"
               , "org-glance-overview:relations"
                 -- A reversing key is the one kind a repeat cannot help: a held
                 -- `^' re-sorts the whole set per repeat and lands on whichever
                 -- direction the parity of the count leaves it, so what a reader
                 -- gets is the order they asked for or its opposite, at random.
               , "toggle-sort" ]

-- | The resident key line, in the order it reads: the commands worth naming
-- ahead of the echo pill, each with the word the line shows for it.  Commands
-- rather than keys, so the page looks each one up in the map — the line cannot
-- advertise a key nothing is bound to.  These are the rows a reader needs in
-- front of them; the rest is the echo pill's to name as it runs.
--
-- The page pair is listed backwards on purpose: a bracket pair reads open then
-- close, so the line says @[\/]@ where the row and cell pairs say forward first.
keyHints :: [([Text], Text)]
keyHints =
  [ (["next-row", "previous-row"],         "rows")
  , (["next-column", "previous-column"],   "cells")
  , (["previous-page", "next-page"],       "pages")
  -- The one row whose label carries a second sentence, because the second press
  -- is the whole point: without it a reader takes `<' for a within-page key and
  -- never finds out that it climbs.
  , (["first-row", "last-row"],            "first/last row, again = page up/down")
  -- Beside the cell keys in spirit: what it sorts by is the column they picked.
  , (["toggle-sort"],                      "sort")
  , (["org-glance-overview:materialize"],  "materialize")
  , (["org-glance-overview:open"],         "open link")
  , (["mark-toggle", "unmark", "unmark-all", "mark-all"], "mark")
  -- The two structured commands, beside the keys that pick what they run over.
  -- `state' runs over the MARKED set; archiving runs over the FLAGGED one, and
  -- is named as the two steps it is — `d' puts a flag on, and either key takes
  -- the flagged rows off.
  , (["org-glance-overview:todo"],         "state")
  , (["org-agenda-set-tags"],              "tags")
  , (["org-glance-overview:schedule", "org-glance-overview:deadline"], "schedule/deadline")
  , (["org-glance-overview:capture"],      "capture")
  , (["archive-flag"],                     "flag for archive")
  , (["archive-flag", "org-glance-overview:delete"], "archive flagged")
  , (["filter-rows"],                      "filter")
  , (["apply-default-filter"],             "default view")
  , (["org-glance-agenda"],                "agenda")
  -- Named beside the key that walks back out of it: the drill and its undo are
  -- one gesture, and a reader who sees only the way in has no way home.
  , (["org-glance-overview:relations"],    "references")
  , (["filter-drop-token"],                "drop token/back")
  , (["customize"],                        "settings")
  , (["quit-window"],                      "quit")
  ]

-- | The keymap as the page carries it: the one row list, and the three tables
-- the dispatch reads off the same blob — the key line's hints, the chords never
-- claimed, and the commands auto-repeat is off for.  The angle brackets are
-- escaped because five of these sequences are angle brackets — a blob that
-- cannot spell a tag cannot open one, whatever element it sits in, and
-- @JSON.parse@ undoes them.
--
-- @seq@ is derived here rather than carried by a row: it is the keys with one
-- space between them, which is how Emacs spells a sequence and the only
-- notation left now that no row runs two keys together.
--
-- The shell parses this instead of holding a second copy, so a key cannot be
-- bound and undocumented, and a hint cannot name a command this map does not
-- carry.
keyBindingsJSON :: Text
keyBindingsJSON = jsonLiteral $ object
  [ "rows"     .= map row keyBindings
  , "hints"    .= [ object [ "commands" .= cs, "label" .= label ] | (cs, label) <- keyHints ]
  , "reserved" .= reservedChords
  , "once"     .= onceCommands
  ]
  where row b = object [ "keys"    .= kbKeys b
                       , "seq"     .= T.unwords (kbKeys b)
                       , "command" .= kbCommand b
                       , "handler" .= kbHandler b
                       , "scope"   .= kbScope b
                       , "help"    .= kbHelp b ]

-- Pages

-- | @\/@: the demo shell, and an explanation in the one case there is no
-- renderer to mount — @--assets@ naming a directory without one.  With no
-- @--assets@ the shell always renders, since the binary carries the renderer.
-- A missing one leaves @\/headlines@ untouched either way: the server is a JSON
-- server that happens to ship a page.
--
-- The tree's default view is embedded at REQUEST time rather than at startup: a
-- config change reseeds the store ('Glance.Web.Watch.settle'), so the next page
-- served carries what the file says now.  This route does not wait on the load
-- — the shell has to render while the walk runs — and a store that is still
-- loading carries no config, which is exactly the built-in fallback.
shellPage :: ServeOptions -> Hub -> IO Response
shellPage opts hub = do
  ok <- hasRenderer opts
  font <- localFont opts
  st <- readTVarIO (hubStore hub)
  pure . html $ case soAssets opts of
    Just dir | not ok -> assetsMissing opts dir
    _rendererInHand   -> demoShell opts font (defaultFilter (stConfig st))

-- | The page a browser gets: load the renderer, fetch a page of the view,
-- mount it, then hold a socket open and apply what it sends.  The glue is
-- inline so the shell has exactly one asset to find.
--
-- The boot is two fetches, and both are @\/headlines@.  The first asks for
-- 100 rows — a window's worth, so the table paints without waiting on the
-- whole store and without encoding rows nobody can see yet; the
-- response's @X-Glance-Total@ says whether there are more, and the rest is
-- fetched behind the painted table.  The full local set is what keeps @n@,
-- @p@, sorting and materialize coherent — the renderer virtualizes, so holding
-- 13k rows costs memory and no DOM.  The socket then opens with
-- @?bootstrap=off@: the rows are already here and the server's opening
-- @set-rows@ would only send them again.
--
-- The page opens on a view rather than on everything: with no @q@ in the
-- address bar the applied query is @state:*active*@, org-glance's own name for
-- the keyword group a @#+TODO:@ line declares before the bar.  It is a query
-- like any other — written into the URL, mounted as the renderer's chip, asked
-- of the server — so @DEL@ takes it off and the whole store is one keystroke
-- away.  A @q@ that IS in the address bar is the reader's own, empty or not,
-- and nothing is injected over it.  Under either, the parity check gets its
-- unfiltered baseline from a third fetch taken once behind the table (@arm@),
-- since a filtered paint can never be its own control.
--
-- A cold daemon answers that first fetch with 503 while it walks the tree.
-- The boot reads it as the state it is — amber dot, @indexing …@ with the
-- elapsed seconds the body carries — and asks again a second later, so the
-- page a browser opened at once is the page that fills in when the walk lands.
--
-- Filtering is the server's.  The renderer's @onFilter@ hands over the
-- debounced query instead of narrowing its own list, the shell asks
-- @\/headlines?q=@ for it — the string exactly as typed, since the grammar is
-- 'Glance.Web.Filter''s to parse — and the answer replaces the rows.  One fetch
-- is in flight at a time and a new one aborts the last, so a fast typist's
-- earlier answers cannot land after a later one.  While a filter is on, a row
-- frame off the socket is answered by re-asking rather than by splicing: the
-- loaded rows are the server's answer to a query and only it knows whether the
-- changed row still matches.
--
-- Every view swaps ON ITS ANSWER.  The table that is up stands until the new
-- rows are in hand and then goes in one mount, so no view is ever shown
-- half-arrived: a re-application asks for the whole answer rather than for a
-- page, since a page-sized mount over a full table is a complete view replaced
-- by a partial one and reflowed a moment later.  The two-phase fetch is the
-- BOOT's, where the first page is the difference between a table and a blank
-- page.  While the answer is out — or while the socket that would deliver a
-- change is gone — the page wears the stale wash: one class on the document
-- element fading back the table and every overlay open over it, never blurring
-- them, so the rows stay readable while they are known to be
-- out of date.  The corner, the event strip and the key line are exempt,
-- being where a reader finds out why.  Each half arms on a delay — a fetch
-- that answers quickly and a socket that blips and comes back dim nothing.
--
-- The filter is summoned rather than resident.  The mount asks for @palette@,
-- so an unfiltered table carries no filter chrome but its chip row, and @\/@
-- raises the overlay through @openFilter@ — the renderer's one entry point for
-- it whatever mode it is in, so the shell asks and never reaches into the
-- chrome.  An asset predating the call has a box on the page, which the old
-- path focuses.  The lifecycle past that is the renderer's: its input stops
-- @ESC@ and @DEL@ before this page's dispatch sees them, and every @table@ row
-- is inert while a field has focus anyway.  A coarse pointer has no @\/@ to
-- press, which is the one exception the keyboard-first rule makes: there the
-- chip row is 44px of tap target — labelled while it is empty — and summons
-- the same palette through the same call.  A fine pointer sees none of it.
--
-- The applied query is page state.  It goes into the URL on every commit
-- (@replaceState@, leaving @keys@ where it is) and on the boot that injected
-- the default, so a filtered view is a link, a reload keeps it and a reconnect
-- comes back to it.  An EMPTY applied query is written too, as a @q@ that is
-- present and empty: absent means nobody has filtered this page yet and gets
-- the default, present-and-empty means a reader took the filter off and gets
-- left alone.  Deleting the parameter instead is what made @DEL@ on the last
-- chip come back filtered on the next remount.  It is restored by handing
-- it to @mount@ as @initialQuery@, which tokenizes it into the renderer's own
-- committed chips and delivers nothing — the rows in hand are already the
-- server's answer to it.  Every return through this door restores it the same
-- way, since a reload, @view-changed@ and @g@ all re-fetch and
-- re-mount.  An asset predating that option drops it silently, so the mount
-- asks @getQuery@ whether it took and falls back to writing the query into the
-- filter box, which is how this worked before chips could carry it.  @DEL@ over
-- the table is that query's own backspace: it takes the last token off — quotes
-- and all — and commits what is left, which clears the filter once the last
-- token goes.
--
-- The materialize sheet has no buttons.  @ESC@ or a click on the backdrop
-- closes it, flushing first when either pane has moved and closing on the 200; a
-- pristine sheet closes with no request at all, so opening one and reading it
-- never touches the file.  @C-x C-s@ flushes mid-edit, and the receipt's digest
-- becomes the next flush's lock.  A 409 leaves the sheet open saying
-- @conflict@: @C-x C-s@ then re-reads the file's digest and posts the author's
-- text over it — last writer wins, on a deliberate keystroke — and @ESC@
-- discards.  A tab closing on an edited sheet flushes with @keepalive@.  The
-- header carries the state in one word, @synced@ \/ @syncing…@ \/ @conflict@ \/
-- @error@, and the sheet wears the author's Emacs theme (danneskjold) while the
-- table keeps the page's.
--
-- It is two panes over one subtree.  The textarea holds the body and a panel
-- beside it holds the property drawer, a row of two fields per property, with
-- an empty row at the bottom that grows the next one as soon as it is typed in
-- — the add affordance is that row, since a key-first page has nothing to
-- press.  A row whose key is emptied is a property deleted, and @ORG_GLANCE_ID@
-- is shown like any other with a line under it saying what editing it costs:
-- the row id IS that value.  The cut between the panes is the SERVER's
-- (@GET \/headline@ hands @body@ and @properties@ beside @org@, and @POST@ takes
-- them back), because finding a drawer in org text is a parser's job and there
-- is none in this page.  @C-c '@ — org's @edit-special@ rhyme — swaps the sheet
-- between the two panes and the raw subtree, and does it by re-materializing:
-- an edited sheet is refused with the key that would let it through, since a
-- re-read cannot carry unsaved work and a local conversion would need the parser
-- this design exists to avoid.  Narrow windows and coarse pointers stack the
-- panel under the text.
--
-- Two keys write without a sheet.  @D@ archives the FLAGGED set, else the row
-- at point; @C-c C-t@ sets a state over the MARKED set, else the row at point.
-- The split is deliberate: a mark is the generic bulk selection and a flag is a
-- selection made for archiving, so the destructive-looking key inherits
-- nothing.  They are @POST \/command@ ('runCommand'):
-- the page sends row ids and a name, the server computes the spans, and the
-- table is not touched at all, since the rows come back over the socket once
-- the watch has re-read the files.  There is no confirmation step; the drift
-- lock is the safety, and @D@ archives rather than deletes, so the worst a
-- mis-key costs is a tag and another keystroke.  @C-c C-t@ raises a value
-- palette of this page's own — the state column's badges plus @*empty*@, typed
-- to narrow, @C-n@\/@C-p@ or the arrows to walk, @RET@ to commit — because the
-- renderer's overlay is the filter's and this page does not reach into it.  The
-- pill counts what landed and the log carries a line per refusal.
--
-- A lost socket costs rows, and only @view-changed@ costs the mount.  The
-- reconnect asks @\/headlines@ for the applied query with the tag the last
-- answer carried: 304 and the rows on screen are still current, 200 and they
-- are replaced in place, and either way the socket is re-attached under the
-- same table — the sheet, the palette, the selection and the URL all stand.
-- That is what an editor writing a whole tree looks like from here, since the
-- server abandons a backlog it cannot deliver and closes with @resync@ rather
-- than making the page rebuild itself.  The columns are the exception: they are
-- what a row op cannot carry, so @view-changed@ tears the mount down and puts
-- it back — and because a daemon restarted while this page was away had no
-- socket to say so on, the reconnect compares the columns it fetched to the
-- mounted ones and takes the same door when they differ.  Across a real
-- remount, an unsaved sheet and a half-typed palette are stashed and restored;
-- the sheet's digest is re-read rather than remembered, so a file that moved
-- underneath opens the conflict flow instead of being overwritten.
--
-- The @materialize@ action opens the subtree over the table: a @textarea@ for
-- the body and a panel for the properties, both filled by @GET \/headline@.  A
-- commit never touches the table — the row arrives over the socket when the
-- watch has re-read the file, which is the same way it would arrive had the
-- edit come from an editor.  A real editor component is M3.5; a textarea is
-- what proves the round-trip, and the panel is what makes the drawer editable
-- as the structure it is.
--
-- The keys are 'keyBindingsJSON', which the glue parses: row movement is the
-- renderer's @selectStep@, which carries the column and crosses a page
-- boundary the shell is not told about, and a sequence with no handler echoes
-- its org-glance command name and what it is waiting for.  The set is shown a
-- page at a time (@pageSize@), and @[@ and @]@ turn one, echoing the page they
-- landed on.  Cell movement is @select@ with a column: the column lives in the
-- renderer's selection rather than here, so it rides along with row movement
-- and goes when the selection does.  A
-- whole-row selection has no column and keeps the look it always had until a
-- horizontal key lands on the first one; the echo names the column it arrived
-- at by the header over it, or says which edge it stopped at.  The pill in
-- the bottom corner is the echo area — the pending prefix while one is open,
-- the command and its help line on completion, @is undefined@ otherwise.  The
-- top corner holds the connection dot alone — a readout, with nothing in it to
-- focus; every preference, the theme included, is a panel of the settings sheet
-- @,@ raises.  The page's last line is the same blob resident: the map's core
-- rows as @keys label@ pairs, named by command, so the pill says what just ran
-- and the line says what can.
--
-- The page is one column the height of the viewport — table, log, key line —
-- and it does not scroll.  The table keeps the height it asks for, the log
-- takes what is left, and both scroll inside themselves, so the corner and the
-- key line hold their places whatever arrives.
--
-- The log is an event strip: connection, sync outcomes, the parity warning,
-- errors.  What is loaded is the renderer's own hint line and what the keys are
-- is the resident key line's, so neither is repeated there.  The
-- frame is resident: with nothing to report it is an empty strip holding its
-- place, so the first event to arrive does not shift the key line under it.
demoShell :: ServeOptions -> Maybe FilePath -> Text -> Text
demoShell opts font wanted = page (fontFace font) (viewTitleFor (soDir opts)) $ T.unlines
  -- No heading: the view title is already the tab's, and printing it a second
  -- time here put it on screen twice.  In palette mode the renderer carries no
  -- bar either, so the page opens on the table itself.
  -- The corner is the CONNECTION DOT and nothing else a reader can focus: the
  -- theme selector moved into the settings sheet, where every other preference
  -- already lived.  What that buys is the blur rule going away — a control in
  -- the corner that keeps the focus eats `n' and `p' as its own type-ahead,
  -- and the answer used to be a hand-written `blur()' every new control owed.
  -- Inside a popup that problem does not exist: the popup is a legitimate
  -- focus holder, so its own focus rules apply and nothing has to be given
  -- back.
  [ "  <div id=\"corner\"><span id=\"dot\" title=\"live connection\"></span>"
      -- The keyboard-first exception, and the same one the chip row is: a
      -- coarse pointer has no `,' to press.  Hidden outside the
      -- pointer:coarse block, so a mouse never sees it and the key is the
      -- whole of the offer there.  It hands the focus straight to the sheet it
      -- opens, so it keeps none of its own.
      <> "<button id=\"gear\" title=\"settings\">\9881</button></div>"
  , "  <div id=\"app\"></div>"
  , "  <div id=\"log\"></div>"
  , "  <div id=\"kbd\"></div>"
  , "  <div id=\"modal\">"
  , "    <div id=\"sheet\">"
  , "      <div id=\"mhead\"><span id=\"mfile\"></span><span id=\"mnote\"></span></div>"
  -- Two panes over one subtree: the body on the left, the property drawer on
  -- the right.  The cut between them is the server's (`GET /headline' hands
  -- both), so neither pane is derived here.
  , "      <div id=\"mpanes\">"
  , "        <textarea id=\"mtext\" spellcheck=\"false\"></textarea>"
  -- The panel is a table-view MOUNT, so the pane is the mount's host plus the
  -- one thing a mount cannot hold: the edit overlay.  The renderer owns its
  -- rows and rewrites them as it scrolls, so an open row's fields sit OVER the
  -- table rather than inside it, anchored to the row the cursor is on.
  , "        <div id=\"mprops\"><div id=\"mptable\"></div>"
      <> "<div id=\"pedit\"><input id=\"pkey\" spellcheck=\"false\">"
      <> "<input id=\"pval\" spellcheck=\"false\"></div></div>"
  , "      </div>"
  -- The logbook, read-only and full width under both panes.  It is the
  -- server's: shown so a reader can see what the row has been through, never
  -- sent back, and out of Tab and out of `dirty()' because nothing here can
  -- move it.
  , "      <pre id=\"mlog\"></pre>"
  , "    </div>"
  , "  </div>"
  -- The tags popup, hosting the page's FOURTH table-view mount and the only
  -- MUTABLE one.  What a set of rows is tagged with is a list of RECORDS — a
  -- name, a coverage over the set, a weight in the tree — so it is the link
  -- popup's shape rather than the value palette's, and it carries the one thing
  -- a read-only mount does not: the rename overlay, which is the property
  -- panel's edit model over one cell.  `#tpane' is the overlay's positioning
  -- parent, exactly as `#mprops' is the panel's.
  , "  <div id=\"tags\">"
  , "    <div id=\"tbox\">"
  , "      <div id=\"thead\"></div>"
  , "      <div id=\"tpane\"><div id=\"ttable\"></div>"
      <> "<div id=\"tedit\"><input id=\"tname\" spellcheck=\"false\"></div></div>"
  , "      <div id=\"tfoot\"></div>"
  , "    </div>"
  , "  </div>"
  -- The value palette.  Letter mode is the resident one and its field is
  -- hidden, so the box carries the mode: `#pbox.narrow' is the completing-read
  -- `/' falls back to, and `+' over the tags popup is its other door.  The foot
  -- names the keys the list itself cannot draw.
  , "  <div id=\"prompt\">"
  , "    <div id=\"pbox\">"
  , "      <div id=\"phead\"></div>"
  , "      <input id=\"pinput\" spellcheck=\"false\" autocomplete=\"off\">"
  , "      <div id=\"plist\"></div>"
  , "      <div id=\"pfoot\"></div>"
  , "    </div>"
  , "  </div>"
  -- The link popup, and it hosts the page's THIRD table-view mount.  A list of
  -- links is a TABLE — what each one is, what the entry calls it, where it
  -- points — and this page has one list widget, so it draws that table rather
  -- than a hand-made list under hand-assigned letters.  A sibling of `#app'
  -- sharing the two z levels with the sheets and the value palette, so the four
  -- values still stand.
  , "  <div id=\"links\">"
  , "    <div id=\"lbox\">"
  , "      <div id=\"lhead\"></div>"
  , "      <div id=\"ltable\"></div>"
  , "      <div id=\"lfoot\"></div>"
  , "    </div>"
  , "  </div>"
  -- The settings sheet: the page's ONE place for a preference, in PANELS.
  -- General, theme, keywords — a header over the rows that belong to it, drawn
  -- from the `SECTIONS' list below, so a fourth panel is one entry there and
  -- the markup it wraps.  A sibling of `#app' like the other two overlays, so a
  -- remount leaves it standing — which this one needs more than the others,
  -- since writing a layer is itself what moves the columns.
  --
  -- The panel BODIES are declared here, each wearing `cpart', and `SECTIONS'
  -- wraps them at boot: the list owns the headers and the order, the markup
  -- owns what is under them, and the stylesheet reads the class rather than a
  -- roll of ids.
  , "  <div id=\"config\">"
  , "    <div id=\"cbox\">"
  , "      <div id=\"chead\"><span id=\"ctitle\">settings</span>"
      <> "<span id=\"cnote\"></span></div>"
  , "      <div id=\"csecs\"></div>"
  -- GENERAL: the two tree-wide lines of `system.org'.  They are that layer's
  -- own bytes and ride in that layer's own write, so the panel they are shown
  -- in is a matter of reading rather than of where the splice goes.  What each
  -- field falls back to when it is emptied is written into its placeholder
  -- here, off the same Haskell constants the server answers with — the value
  -- cannot change while the page is up, so nothing has to carry it into the
  -- glue and interpolate it back out per sheet-open.
  , "      <div id=\"cgen\" class=\"cpart\">"
  , "        <div class=\"crow\"><div class=\"clab\">default view</div>"
      <> "<input id=\"cfilter\" class=\"cview\" spellcheck=\"false\" placeholder=\""
      <> escape ("the view g applies; empty is " <> builtinFilter) <> "\"></div>"
  , "        <div class=\"crow\"><div class=\"clab\">capture target</div>"
      <> "<input id=\"ctarget\" class=\"cview\" spellcheck=\"false\" placeholder=\""
      <> escape ("where + captures; empty is " <> T.pack defaultCaptureFile)
      <> "\"></div>"
  , "      </div>"
  -- THEME: the selector that used to sit in the corner.  Same three values,
  -- same `localStorage' key, same pre-paint boot — a different place to reach
  -- it from.  It wears `cview' with the fields above it, so it takes their
  -- border, radius and font, and the one coarse-pointer rule that stops iOS
  -- zooming in on a focused control reaches it too.
  , "      <div id=\"ctheme\" class=\"cpart\">"
  , "        <div class=\"crow\"><div class=\"clab\">colour theme</div>"
      <> "<select id=\"themesel\" class=\"cview\" title=\"colour theme\">"
      <> "<option value=\"auto\">auto</option><option value=\"light\">light</option>"
      <> "<option value=\"dark\">dark</option></select></div>"
  , "      </div>"
  -- KEYWORDS: one box per layer, then the union they come to and the note
  -- saying what that union is.
  , "      <div id=\"clayers\" class=\"cpart\"></div>"
  , "      <div id=\"ceff\"></div>"
  , "      <div id=\"cfoot\">read-only: the union every file is parsed with."
      <> " A file's own #+TODO: line adds to it and outranks these for that"
      <> " file's own headlines.</div>"
  , "    </div>"
  , "  </div>"
  , "  <div id=\"echo\" role=\"status\" aria-live=\"polite\"></div>"
  , "  <script id=\"keys\" type=\"application/json\">" <> keyBindingsJSON <> "</script>"
  , "  <script src=\"" <> T.pack rendererAsset <> "\"></script>"
  , "  <script>"
  -- The strip is an event log, and an APPEND-ONLY one: nothing clears it, so
  -- what a reader missed is still there to scroll back to.  A line is
  -- `HH:MM:SS SEV scope message' — the stamp muted, the severity coloured, the
  -- scope one word out of a fixed set (ws, sync, cmd, filter, config, boot)
  -- saying which part of the page is talking.  The parts are spans so each can
  -- carry its own colour, and the message is one line: control characters in it
  -- collapse to spaces rather than breaking the shape.
  --
  -- Two rules keep it bounded without taking anything back: past LOGCAP the
  -- OLDEST line is dropped, and a line identical to the one before it bumps a
  -- counter instead of repeating itself.  That counter is the only mutation an
  -- append-only strip allows, and it is what keeps a retry loop from filling the
  -- ring with one message.
  --
  -- The strip is capped in height, so its end can be out of sight.  Keep it in
  -- sight — unless the reader has scrolled up, which is a place they are holding
  -- on purpose.
  -- One appended child: TAG under INTO, wearing CLS and holding TEXT when there
  -- is any.  Both trees this page builds — the event strip's lines and the value
  -- palette's entries — are rows of these.
  , "    const part = (into, tag, cls, text) => {"
  , "      const e = document.createElement(tag);"
  , "      e.className = cls;"
  , "      if (text !== undefined) e.textContent = text;"
  , "      into.appendChild(e);"
  , "      return e;"
  , "    };"
  , "    const LOGCAP = 500;"
  , "    let logLast = null;"
  , "    function append(scope, sev, message) {"
  , "      const box = document.getElementById(\"log\");"
  , "      const text = String(message).replace(/[\\x00-\\x1f]+/g, \" \");"
  , "      const end = box.scrollTop + box.clientHeight >= box.scrollHeight - 4;"
  , "      if (logLast && logLast.scope === scope && logLast.sev === sev"
  , "          && logLast.text === text) {"
  , "        logLast.count.textContent = `×${(logLast.n += 1)}`;"
  , "      } else {"
  , "        const line = document.createElement(\"div\");"
  , "        line.className = sev;"
  , "        part(line, \"span\", \"lt\", new Date().toTimeString().slice(0, 8));"
      -- The severity is SPELLED uppercase and WORN lowercase: the word is what
      -- a reader scans a screenful of chatter for, and the class is what the
      -- stylesheet and the suite name.  One value, two cases, and the display
      -- is the only place the fold happens.
  , "        part(line, \"span\", \"lv\", sev.toUpperCase());"
  , "        part(line, \"span\", \"lc\", scope);"
  , "        part(line, \"span\", \"lm\", text);"
  , "        logLast = { scope, sev, text, n: 1, count: part(line, \"span\", \"ln\", \"\") };"
  , "        box.appendChild(line);"
  , "        while (box.children.length > LOGCAP) box.removeChild(box.children[0]);"
  , "      }"
  , "      if (end) box.scrollTop = box.scrollHeight;"
  , "    }"
  , "    const dot = (name) => (document.getElementById(\"dot\").className = name);"
  , "    const el = (id) => document.getElementById(id);"
    -- THE WASH.  What is on screen stops being known to be current in exactly
    -- two ways: the view is being replaced and its answer has not landed, or
    -- the socket that would deliver a change is gone.  A reader cannot tell
    -- either of those from a page that is simply quiet, so both wear ONE look
    -- — faded back, and never blurred, since stale rows are still the rows and
    -- have to stay readable while the answer is on its way —
    -- carried by ONE class on the document element.  What EXPLAINS the state
    -- is exempt: the corner's dot, the event strip and the key line are where
    -- a reader finds out why, and washing the answer along with the question
    -- would leave nothing to read.
    --
    -- Each reason arms on a DELAY, which is the whole of what keeps the wash
    -- off a page that is working: a fetch answering inside its grace and a
    -- socket that blips and comes back dim nothing at all.  Whoever arms a
    -- reason is who clears it.
  , "    const WASH = { view: 300, socket: 400 };"
  , "    const wash = {"
  , "      n: { view: 0, socket: 0 }, at: { view: 0, socket: 0 },"
  , "      on: { view: false, socket: false },"
      -- Reason WHY now stands COUNT times over.  Arming and clearing are one
      -- discipline for both reasons; what differs is who counts.  A view fetch
      -- STEPS the count, because `load' overlaps an abort with the fetch that
      -- replaced it and a boolean would clear the wash the replacement still
      -- wants.  The socket SETS it, because a connection closing before it
      -- ever opened would otherwise arm twice against one open.
  , "      want(why, count) {"
  , "        const was = this.n[why];"
  , "        this.n[why] = Math.max(0, count);"
  , "        if (this.n[why] === was) return;"
  , "        if (this.n[why]) this.arm(why); else this.off(why);"
  , "      },"
  , "      step(why, by) { this.want(why, this.n[why] + by); },"
  , "      arm(why) {"
  , "        if (this.on[why] || this.at[why]) return;"
  , "        this.at[why] = setTimeout(() => {"
  , "          this.at[why] = 0; this.on[why] = true; this.show();"
  , "        }, WASH[why]);"
  , "      },"
  , "      off(why) {"
  , "        clearTimeout(this.at[why]); this.at[why] = 0;"
  , "        this.on[why] = false; this.show();"
  , "      },"
  , "      show() {"
  , "        document.documentElement.classList.toggle(\"stale\","
  , "          this.on.view || this.on.socket);"
  , "      },"
  , "    };"
    -- Does MOUNT carry the optional call NAME?  Every renderer capability this
    -- page uses is detected before it is used, and there are TWO mounts now —
    -- the table and the sheet's property panel — so the question is asked of a
    -- handle rather than of the one this page used to have.
  , "    const can = (mount, name) => !!mount && typeof mount[name] === \"function\";"
    -- ROW MOVEMENT, ONCE.  Both spellings and the arrows, as a direction or
    -- zero for a key that is not one — read by the two MODAL mounts, the
    -- property panel and the link popup, whose keys live in listeners of their
    -- own behind the dispatch.  Written here rather than twice, so a third
    -- spelling is one edit and the map's own `n'/`p'/`j'/`k' rows cannot drift
    -- from what a modal surface answers to.  The table's own movement stays the
    -- map's, since it walks two axes and pages.
  , "    const rowStep = (k) => (k === \"<down>\" || k === \"n\" || k === \"j\" ? 1"
  , "                          : k === \"<up>\" || k === \"p\" || k === \"k\" ? -1 : 0);"
  , "    const stepIn = (mount, step) =>"
  , "      can(mount, \"selectStep\") && mount.selectStep(step);"
    -- The archive/delete flags, which are the one capability both mounts want:
    -- the table flags a row for archiving and the panel flags one for deleting,
    -- and an asset predating either says so once.
  , "    const flagsOn = (mount) => can(mount, \"flagRow\") && can(mount, \"getFlagged\");"
    -- On the next frame, or now where there are no frames.  What the panel's
    -- edit overlay waits for: the renderer stamps its selection in a frame of
    -- its own, so a row selected in this tick has no marked element yet.
  , "    const soon = (fn) =>"
  , "      (typeof requestAnimationFrame === \"function\" ? requestAnimationFrame(fn)"
  , "                                                    : setTimeout(fn, 0));"
  , "    let table = null, socket = null, backoff = 1000, editing = null;"
  , "    // The sheet's own baselines: the two panes as the file holds them as"
  , "    // far as this page knows, and whether the drawer is a panel or spelled"
  , "    // out in the text.  Where it STANDS is `subtreeSheet.state'."
  , "    let base = \"\", baseProps = null, raw = false;"
  , "    // The server filters and pages; these hold the query it was last asked"
  , "    // with, the fetch still in flight for it, and the timer that re-asks"
  , "    // when a row frame lands while one is on."
  , "    let query = \"\", inflight = null, requeryAt = 0;"
  , "    // The tag the last answer carried, which is what makes a reconnect"
  , "    // cheap: an unmoved store answers the revalidation 304 and no rows"
  , "    // cross the wire at all."
  , "    let etag = null;"
    -- One number, two jobs: the boot asks for this many rows and the renderer
    -- shows this many at a time, so the first paint is exactly page one and
    -- the set arriving behind it only adds pages to turn to.
  , "    const PAGE = 100;   // rows in the first paint, and rows to a page"
  , "    function mount(view) {"
      -- The trail comes off the URL before the mount, because `chipLabel' can
      -- be asked for a label during the first paint: the map has to be standing
      -- when the renderer draws the chip it aliases.
  , "      const was = bootTrail();"
  , "      crumbLabels = was.labels;"
  , "      crumbSels = was.sels;"
  , "      table = TableView.mount(document.getElementById(\"app\"), view, {"
  , "        palette: true,     // the filter is summoned, never resident"
        -- The set is shown a page at a time: the renderer keeps the window,
        -- the spacers and the pager in its own status line, and movement
        -- crosses the boundary without this page knowing where one is.
  , "        pageSize: PAGE,"
        -- Marking is the renderer's chrome and the renderer's state: a
        -- checkbox column it draws and a set of ids it keys, which is why a
        -- mark outlives a filter that hides its row and a page it is not on.
        -- This page owns the keys and nothing else.
  , "        marks: true,       // dired's m/u/U/M, drawn and counted by the renderer"
        -- A flagged row's hint is the two keys that answer the flag, spelled the
        -- way the key line spells them.  The renderer draws it; an asset
        -- predating the option drops it the way it drops any other it has no
        -- field for.
  , "        flagHelp: \"d/D archive · u unflag\","
        -- The renderer's per-row hint says RET materializes, which the key line
        -- under the table already says and says for every command.  One place.
  , "        actionHints: false,"
  , "        // The applied query, restored as the renderer's own committed"
  , "        // chips. It tokenizes them and delivers nothing — the rows in"
  , "        // hand are already the server's answer to this query, and a"
  , "        // delivery here would ask for them a second time."
  , "        initialQuery: query,"
  , "        // A `ref:' chip shows what the drill was FOR, never the row id it"
  , "        // is spelled with. The query is untouched — the renderer aliases"
  , "        // the display alone — so DEL still strips the token as written."
  , "        chipLabel: (tok) => crumbLabels[tok] || null,"
  , "        onAction: (command, id) =>"
  , "          command === \"materialize\" ? materialize(id)"
  , "                                     : append(\"cmd\", \"info\","
      <> " `action: ${command}  id=${id}`),"
  , "        onLink: (target) => append(\"cmd\", \"info\", `link: ${target}`),"
  , "        onFilter: filter,   // the server narrows; the renderer shows what it is given"
  , "      });"
  , "      // An asset older than `initialQuery' drops it silently, which would"
  , "      // leave the page showing no filter over rows that are filtered."
  , "      // `getQuery()' says whether it took: when it did not, put the query"
  , "      // back in the box the way this did before chips could carry it."
  , "      if (query && !holds(query)) showQuery();"
      -- The strip goes back up the way the query did.  `setCrumbs' keeps only
      -- what parses as a crumb, so a hand-edited parameter costs the trail and
      -- nothing else.  An asset with no crumbs draws none and the labels sit
      -- unread until one arrives — a drill is refused before it starts.
  , "      if (crumbing() && was.trail.length) table.setCrumbs(was.trail);"
  , "      // The columns are the view's: both halves of a filter read the keys"
  , "      // out of them (`parity'), and cell movement names its landing column"
  , "      // by the header sitting over it."
  , "      cols = view.columns || [];"
      -- A mount takes its sort keys from the view, so the record of them starts
  , "      // Whatever the remount that led here took down goes back up over the"
  , "      // new table; on a first boot there is nothing stashed and nothing to do."
  , "      restore();"
  , "    }"
  , "    // One /headlines at a time: a keystroke aborts the fetch before it, so"
  , "    // an earlier answer can never land over a later one.  TAG makes it a"
  , "    // revalidation: the browser's own cache is stepped around, so the tag"
  , "    // that goes out is this page's and the 304 comes back as the answer it"
  , "    // is rather than as a body the cache filled in behind it."
  , "    function load(params, tag) {"
  , "      if (inflight) inflight.abort();"
  , "      inflight = new AbortController();"
  , "      const init = { signal: inflight.signal };"
  , "      if (tag) { init.headers = { \"if-none-match\": tag }; init.cache = \"no-store\"; }"
  , "      return fetch(`/headlines${params}`, init).then((r) =>"
  , "        // 304: the store has not moved, so there is no view to read and the"
  , "        // rows already on screen are the current answer to this query."
  , "        r.status === 304 ? { view: null, total: 0 }"
  , "        : r.ok ? r.json().then((view) => {"
  , "            etag = r.headers.get(\"ETag\") || etag;"
  , "            return { view, total: +r.headers.get(\"X-Glance-Total\") };"
  , "          })"
  , "        // 503 is the startup walk: the server is listening and says so"
  , "        // in the body.  `start' polls it; nothing else can see it."
  , "        : r.status === 503 ? r.json().then((b) => { throw Object.assign(new Error(\"indexing\"), { indexing: b }); })"
  , "             : r.text().then((t) => { throw new Error(t); }));"
  , "    }"
  , "    const quiet = (e) => {"
  , "      if (e.name !== \"AbortError\") append(\"ws\", \"error\", `load failed: ${e.message}`);"
  , "    };"
    -- A fetch whose answer REPLACES what is on screen, marked as one: it holds
    -- the wash's view reason for as long as it is out, so a swap that takes
    -- longer than the grace says so rather than leaving stale rows looking
    -- current.  The parity baseline and the probe behind `@' go through `load'
    -- without this, because neither replaces anything and dimming a page for a
    -- fetch that will not change it is the same lie the other way round.  A
    -- boot holds nothing either: a page with no table on it has no stale
    -- content to wash.
  , "    const viewing = (p) => {"
  , "      if (!table) return p;"
  , "      wash.step(\"view\", 1);"
  , "      return p.finally(() => wash.step(\"view\", -1));"
  , "    };"
  , "    // The unfiltered answer is kept: with a filter on, the loaded rows are"
  , "    // the server's answer to it and cannot be used to check that answer."
  , "    let all = [], cols = [];"
  , "    const paint = (a) => {"
  , "      const rows = a.view.rows || [];"
  , "      table.setRows(rows);"
  , "      if (!query) all = rows;"
  , "      parity(a.total);"
  , "    };"
  , "    // The check needs an unfiltered set to check a filtered answer against,"
  , "    // and this page can open filtered — a `?q=' link, or the default view"
  , "    // below.  A paint under a query arms nothing, so a filtered session"
  , "    // would keep the check dark for as long as it lasted.  Ask for the"
  , "    // unfiltered set once, behind everything else, keep it as the baseline"
  , "    // without touching the table, and re-run the check that had nothing to"
  , "    // run against when TOTAL was painted."
  , "    function arm(total) {"
  , "      if (!query || all.length) return;"
  , "      load(\"\").then((a) => { all = a.view.rows || []; parity(total); }).catch(quiet);"
  , "    }"
  , "    // A suggestion must never silently offer what the applied path cannot"
  , "    // evaluate.  The keys that can differ between the two halves are the"
  , "    // producer's virtual ones — the columns are in the view both read — so"
  , "    // when the server answers a query carrying one with nothing at all and"
  , "    // the words are in the rows this page already holds, say so.  Loose and"
  , "    // one-directional on purpose: it reports a suspicion and corrects"
  , "    // nothing, since guessing which half is right is how they drift."
  , "    function parity(total) {"
  , "      if (total !== 0 || !query || !all.length) return;"
  , "      if (typeof TableView.parseQuery !== \"function\") return;"
  , "      const keys = cols.map((c) => c.key);"
  , "      const loose = TableView.parseQuery(query, keys).filter((t) =>"
  , "        t.key === null && !t.quoted && !t.negated && /^[^:=]+[:=]./.test(t.value));"
  , "      if (!loose.length) return;"
  , "      const wants = loose.map((t) => t.value.slice(t.value.search(/[:=]/) + 1).toLowerCase());"
  , "      const text = (r) => keys.map((k) => TableView.displayText((r.cells || {})[k]))"
  , "        .join(\"\\x1f\").toLowerCase();"
  , "      const local = all.filter((r) => wants.every((v) => text(r).includes(v))).length;"
  , "      if (!local) return;"
  , "      const note = \"filter parity divergence — asset/daemon version skew\";"
  , "      console.warn(note, { query, server: total, local });"
  , "      append(\"filter\", \"warn\", note);"
  , "      echo(note);"
  , "    }"
  , ""
  , "    // The applied query is page state.  It rides in the URL, so a filtered"
  , "    // view is a link and a reconnect comes back to it; DEL takes its last"
  , "    // token off through the renderer.  The shell sends the string as typed"
  , "    // — the grammar is the server's to parse (SCHEMA.md)."
  , "    const params = () => new URLSearchParams(location.search);"
  , "    const urlQuery = () => params().get(\"q\") || \"\";"
  , "    // What the page opens on when the address bar says nothing, and what"
  , "    // `g' applies.  The daemon embeds it at request time out of the tree's"
  , "    // own `#+GLANCE_DEFAULT_FILTER:', falling back to org-glance's spelling"
  , "    // of the active group.  A `?q=' is the user's intent whatever it holds,"
  , "    // an empty one included, so the default is injected only where there is"
  , "    // no `q' at all — and then it is a query like any other, committed to"
  , "    // the URL, shown as the renderer's chip and asked of the server."
  , "    const DEFAULT_QUERY = " <> jsonText wanted <> ";"
  , "    const bootQuery = () => (params().has(\"q\") ? urlQuery() : DEFAULT_QUERY);"
    -- The drill-down trail.  The STACK is the renderer's — it draws the crumbs,
    -- and `setView' drops them with the world they described — so this page
    -- keeps no copy of it and reads it back whenever it needs one, the way it
    -- keeps no copy of the marks or of the selected column.
    --
    -- What this page does keep is the LABEL a `ref:' token wears, because no
    -- lookup can recover it: the title belongs to the row referred TO, and that
    -- row is very rarely among its own referrers, so by the time the drill has
    -- landed the title is nowhere in the rows on screen.  Keyed by the token, so
    -- one map answers both readers — `chipLabel' aliasing the live chip, and the
    -- crumb a further drill leaves behind.
  , "    let crumbLabels = {};"
  , "    const crumbing = () => !!table && typeof table.pushCrumb === \"function\""
  , "      && typeof table.popCrumb === \"function\""
  , "      && typeof table.getCrumbs === \"function\""
  , "      && typeof table.setCrumbs === \"function\";"
  , "    const trail = () => (crumbing() ? table.getCrumbs() : []);"
    -- The selection each crumb was pushed FROM, one entry per crumb.  It rides
    -- BESIDE the trail rather than inside it because the renderer's `crumbOf'
    -- keeps a crumb's `label' and `query' and drops everything else — so a
    -- selection put in a crumb would never come back out of `getCrumbs()'.
    -- The renderer's DEPTH is still the truth: a side table that has fallen out
    -- of step with it is dropped whole rather than pairing a crumb with another
    -- crumb's row.
  , "    let crumbSels = [];"
  , "    const selsFit = () => crumbSels.length === trail().length;"
    -- Where an applied view lands the cursor.  ONE rule, at one door: a POP puts
    -- back the row its drill was launched from, and every other application —
    -- a palette commit, `g', `a', `@' — lands on the FIRST row of the answer.
    -- An empty answer selects nothing, which is what it did before.
    --
    -- `select' answers false for a row the view no longer holds, so a
    -- remembered row that an edit or a narrower filter took away falls through
    -- to the same first-row landing rather than being forced back.
    -- The COLUMN rides across either landing: a commit repaints the same mount,
    -- so the cell the reader was reading in is still there to land in — and `^',
    -- which is a commit now, would otherwise take the selection it needs away
    -- from the next press of itself.  After a REMOUNT there is no column to
    -- keep and `column()' answers null, which is the whole-row look this landed
    -- on before.
  , "    function land(sel) {"
  , "      if (!table || typeof table.select !== \"function\") return;"
  , "      const rows = visible();"
  , "      if (!rows.length) return;"
  , "      if (sel && sel.id"
  , "          && table.select(sel.id, sel.col === null ? undefined : sel.col)) return;"
  , "      const at = column();"
  , "      table.select(rows[0].id, at === null ? undefined : at);"
  , "    }"
    -- A row as the `ref:' token naming it.  The value is quoted where the id
    -- carries a token separator: the fallback row id is `PATH#K' and a path may
    -- hold a space, which the grammar would otherwise cut the token at.  An id
    -- carrying a QUOTE is beyond this — the scanner drops quote characters
    -- rather than unescaping them — and no id spelling seen in the corpus does.
  , "    const refToken = (id) => `ref:${/[\\s&\"]/.test(id) ? `\"${id}\"` : id}`;"
    -- What the view being LEFT is called, for the crumb that stands in for it.
    -- A labelled jump chains honestly: drilling out of a drill leaves the first
    -- drill's own name behind rather than its `ref:' spelling, and any other
    -- query is its own best name.
  , "    const hereLabel = () => crumbLabels[query] || query || \"all rows\";"
  , "    // Every applied query is written, the EMPTY one included: a `q' that is"
  , "    // present and empty is a reader who took the filter off, where an absent"
  , "    // one is a page nobody has filtered yet.  Only the second has the default"
  , "    // injected over it, so DEL'ing the last chip survives a reload and every"
  , "    // remount after it — deleting the parameter here is what made a cleared"
  , "    // view come back filtered."
  , "    //"
  , "    // The trail rides beside it, and the URL is the ONLY channel it crosses"
  , "    // a remount by: every mutation of the stack — a drill, a pop, `g' — is"
  , "    // followed by a `remember', so the address bar is current whenever"
  , "    // `mount' reads it back.  That is why `stash'/`restore' say nothing"
  , "    // about crumbs: what they carry is work the reader has NOT committed,"
  , "    // and there is no such thing as a half-applied crumb."
  , "    function remember(q) {"
  , "      const p = params();"
  , "      p.set(\"q\", q);   // `keys' and anything else in the URL survives"
  , "      const t = trail(), labels = Object.keys(crumbLabels).length ? crumbLabels : null;"
  , "      if (!t.length && !labels) p.delete(\"crumbs\");"
  , "      else p.set(\"crumbs\", JSON.stringify("
      <> " { trail: t, labels: crumbLabels, sels: selsFit() ? crumbSels : [] }));"
  , "      history.replaceState(null, \"\", `?${p.toString()}`);"
  , "    }"
    -- The trail as the address bar carries it.  A parameter a hand has been in
    -- is not worth a diagnostic: anything that does not parse into the two
    -- fields is one boot without a trail, which is where a reader starts anyway.
  , "    function bootTrail() {"
  , "      try {"
  , "        const was = JSON.parse(params().get(\"crumbs\") || \"null\");"
  , "        if (!was || typeof was !== \"object\")"
  , "          return { trail: [], labels: {}, sels: [] };"
  , "        return {"
  , "          trail: Array.isArray(was.trail) ? was.trail : [],"
  , "          labels: was.labels && typeof was.labels === \"object\" ? was.labels : {},"
  , "          sels: Array.isArray(was.sels) ? was.sels : [],"
  , "        };"
  , "      } catch (e) { return { trail: [], labels: {}, sels: [] }; }"
  , "    }"
  , "    // A query as the `/headlines' query string asking it, spelled once for"
  , "    // the four callers that want it — the boot, a commit, the arming fetch"
  , "    // and the reconnect.  A second spelling is how a revalidation comes to"
  , "    // be answered 304 against rows answering some other question."
  , "    const asking = (q) => (q ? `?q=${encodeURIComponent(q)}` : \"\");"
  , "    // One place asks the server for rows: `query' is already what to ask."
      -- A commit REPAINTS rather than remounting, so the cursor would otherwise
      -- stay wherever it was — on a row the new answer may not hold.  The same
      -- landing rule `applyView' uses closes that: the first row of what came
      -- back, and nothing when nothing did.
  , "    const fetchRows = () =>"
  , "      viewing(load(asking(query)))"
  , "        .then((a) => { if (table) { paint(a); land(null); } })"
  , "        .catch(quiet);"
  , "    // A commit is the moment a NEW query goes to the server — a settled"
  , "    // debounce, a committed token, an accepted completion."
  , "    function commit(q) {"
  , "      if (q === query) return;"
  , "      query = q;"
  , "      remember(q);"
  , "      fetchRows();"
  , "    }"
  , "    const filter = (q) => commit(q.trim());"
  , "    // The query's last token comes off in the renderer, which owns the"
  , "    // chips showing it: a shell-side strip would leave them on screen"
  , "    // spelling a filter that is no longer applied.  An asset too old to"
  , "    // have the pair says so rather than growing a second implementation."
  , "    const strips = () => table && typeof table.stripLastToken === \"function\""
  , "      && typeof table.getQuery === \"function\";"
  , "    // Whether the mounted renderer is carrying Q as its own query."
  , "    const holds = (q) => typeof table.getQuery === \"function\""
  , "      && table.getQuery() === q;"
  , "    // The renderer's filter field, wherever its mode puts it: the palette's"
  , "    // input in palette mode, the resident box in an asset predating one."
  , "    // Named once, since three callers want it and none of them may reach"
  , "    // further into the chrome than this."
  , "    const filterBox = () => document.querySelector(\"#app .tv-filter\");"
  , "    // The fallback for an asset without `initialQuery': the query goes in"
  , "    // the box rather than into chips.  The box is the renderer's, and"
  , "    // setting its value fires no input event, so a restored query shown"
  , "    // there is not committed a second time."
  , "    function showQuery() {"
  , "      const box = filterBox();"
  , "      if (box) box.value = query;"
  , "    }"
  , ""
  , "    // A GET that unwraps the JSON and turns the server's own error into a"
  , "    // throw.  Three routes are read this way — the subtree, the resolution"
  , "    // behind the state palette, and the config layers — and all three want"
  , "    // one handling of a refusal, so the shape sits here once."
  , "    const getJSON = (url) =>"
  , "      fetch(url).then((r) => r.json().then((b) => {"
  , "        if (!r.ok) throw new Error(b.error || r.status);"
  , "        return b;"
  , "      }));"
  , ""
  , "    // The two shapes of /headline, each written once.  `post' pins"
  , "    // the write to DIGEST, and EXTRA is what a page closing on an edited"
  , "    // sheet adds — `keepalive', being the one caller that cannot wait."
  , "    const headline = (id) => getJSON(`/headline?id=${encodeURIComponent(id)}`);"
  , "    const post = (id, digest, asked, extra) =>"
  , "      fetch(`/headline?id=${encodeURIComponent(id)}`, {"
  , "        method: \"POST\","
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify({ ...asked, digest }),"
  , "        ...extra,"
  , "      });"
  , "    function materialize(id) {"
  , "      headline(id).then((h) => show(h, false))"
  , "        .catch((e) => append(\"sync\", \"error\", `materialize failed: ${e.message}`));"
  , "    }"
  , "    // The sheet is buttonless: it syncs on the way out.  It is also two"
  , "    // panes over one subtree — the body in the textarea, the property drawer"
  , "    // as a panel of rows — and the cut between them is the server's, since"
  , "    // finding a drawer in org text is a parser's job and there is none here."
  , "    // `base' and `baseProps' are what the file holds as far as this page"
  , "    // knows: the materialized original, then whatever the last 200 wrote."
  , "    // Either pane moving is `dirty()', which is the whole of what decides"
  , "    // whether closing costs a POST."
  , "    function show(h, asRaw) {"
  , "      editing = h; raw = !!asRaw;"
  , "      el(\"mfile\").textContent = `${h.file}  ·  ${h.id}`;"
  , "      fill(h);"
  , "      sync(\"synced\");"
  , "      el(\"modal\").className = \"on\";"
  , "      el(\"mtext\").focus();"
  , "    }"
  , "    // Both panes filled from H, and the baselines taken off what landed in"
  , "    // them rather than off H — so a value the panel shows trimmed is not"
  , "    // dirty the moment it appears."
  , "    function fill(h) {"
  , "      base = raw ? h.org : h.body;"
  , "      el(\"mtext\").value = base;"
  , "      el(\"sheet\").className = raw ? \"raw\" : \"\";"
  , "      drawProps(raw ? [] : h.properties || [], raw ? [] : h.planning || []);"
  , "      drawLog(raw ? \"\" : h.logbook || \"\");"
  , "      baseProps = raw ? null : edited();"
  , "    }"
    -- Everything the panel holds, as one string to compare against.  Two lists
    -- rather than one, so a property and a planning entry spelling the same
    -- pair cannot cancel out.
  , "    const edited = () => JSON.stringify([props(), planning()]);"
    -- The logbook strip: shown, never sent, and taken off the sheet outright
    -- when there is none rather than left as a labelled blank.
    --
    -- The drawer's INTERIOR alone.  `:LOGBOOK:' and `:END:' are the delimiters
    -- of the thing the widget already is, so showing them spends two of the
    -- strip's lines saying what the strip is.  The cut is display-only: what
    -- goes back into the file is the whole drawer, delimiters and all, and this
    -- page never sends it at all.
  , "    function drawLog(text) {"
  , "      const inner = text.replace(/\\n$/, \"\").split(\"\\n\").slice(1, -1).join(\"\\n\");"
  , "      el(\"mlog\").textContent = inner;"
  , "      el(\"mlog\").className = inner ? \"on\" : \"\";"
  , "    }"
  , "    const dirty = () => editing !== null"
  , "      && (el(\"mtext\").value !== base"
  , "          || (!raw && edited() !== baseProps));"
    -- The property panel is a table-view MOUNT.  The renderer is this page's
    -- ONE list widget: it draws the table, and a drawer is a list, so it draws
    -- the drawer too — which is what leaves this page with no rows of its own to
    -- style, no cursor of its own to move and no second answer to what a flagged
    -- row looks like.
    --
    -- MODEL AND VIEW.  `prows' is the model — a key, a value, and whether org
    -- owns the key — and the mount is a view of it, re-set on every change.
    -- What a row HOLDS is its COMMITTED text; the open row's two fields are the
    -- edit in progress and nothing else reads them, which is what makes a commit
    -- the only thing that can make the sheet dirty.  The cursor, the flags and
    -- the scrolling are the renderer's, so none of them is kept here.
    --
    -- It stays MODAL, dired's shape rather than a form's: in NAV nothing is
    -- focusable, which is what leaves the plain letters free to be movement, and
    -- RET is what puts fields on screen.
    --
    -- The planning entries are rows of this same list rather than a second one
    -- of their own — three FIXED ones, in org's own order, ahead of the drawer's
    -- properties.  Fixed means the key is org's and not the author's: RET opens
    -- the value alone, an empty value is the entry absent, and a delete CLEARS
    -- the entry where it would drop a property.
    --
    -- A row's ID is stable for the life of the sheet — the planning key, or `P'
    -- and a number handed out once — so a flag, a selection and a deletion all
    -- name the same row after any number of edits above it.
    --
    -- The identity property is in neither pane: `ORG_GLANCE_ID' is the row id
    -- the table keys its updates off, and the server keeps it out of what it
    -- hands over and puts it back verbatim afterwards
    -- ('Glance.Query.hiddenProperties').  There is nothing here to warn about
    -- and nothing to filter — and nothing rowed is nothing flaggable.
  , "    const PLANNING = " <> jsonList planningKeywords <> ";"
  , "    const PCOLS = [ { key: \"key\", header: \"Key\" },"
  , "                    { key: \"value\", header: \"Value\" } ];"
  , "    let pmount = null, prows = [], pseq = 0;"
    -- Mounted once and kept: a mount per sheet would leave a theme listener
    -- behind each time the reader opened one.  `setRows' is how a new drawer
    -- arrives.
  , "    function mounted() {"
  , "      if (pmount) return pmount;"
  , "      pmount = TableView.mount(el(\"mptable\"), { columns: PCOLS, rows: [] }, {"
        -- No bar and no resident filter: five rows of a drawer are not something
        -- a reader narrows, and the overlay this leaves behind is never raised.
  , "        palette: true,"
        -- Flags alone: the gutter carries the flag's edge, no checkbox is
        -- drawn, and nothing here reads a mark.
  , "        flags: true,"
        -- The key line under the table already names every key, once.
  , "        actionHints: false,"
  , "        flagHelp: \"d/D delete · u unflag\","
  , "      });"
        -- The overlay is anchored to the row it opened over, so everything that
        -- can move that row's box has to say so: the mount's own scrolling —
        -- caught in the CAPTURE phase, which reaches it without this page
        -- naming the element that scrolls — and the window resizing, since the
        -- panes wrap rather than querying a width.
  , "      el(\"mprops\").addEventListener(\"scroll\", place, true);"
  , "      window.addEventListener(\"resize\", place);"
  , "      return pmount;"
  , "    }"
  , "    const prowsOf = () =>"
  , "      prows.map((r) => ({ id: r.id, cells: { key: r.key, value: r.val } }));"
    -- Every change to the model ends here.  AT is the row to land the cursor on
    -- and is left out where it should stay where it is.
  , "    function repaint(at) {"
  , "      const m = mounted();"
  , "      m.setRows(prowsOf());"
  , "      if (at) m.select(at);"
  , "    }"
  , "    function drawProps(list, plan) {"
  , "      mounted();"
  , "      prows = []; pseq = 0;"
  , "      shutEdit();"
  , "      el(\"mprops\").className = \"\";   // and the panel gives the keys back"
  , "      const held = new Map(plan || []);"
  , "      for (const key of PLANNING)"
  , "        prows.push({ id: `PLN:${key}`, key, val: held.get(key) || \"\", fixed: true });"
  , "      for (const p of list)"
  , "        prows.push({ id: `P${pseq++}`, key: p[0], val: p[1], fixed: false });"
      -- A different drawer: these flags were about the last one.  `setRows'
      -- deliberately keeps them, so taking them off is this page's to ask for.
  , "      pmount.clearFlags();"
  , "      repaint(prows[0].id);"
  , "    }"
    -- Where the cursor is, in the model's terms.  The renderer's answer is the
    -- one that decides; this page keeps no copy of it.
  , "    const patAt = () =>"
  , "      (can(pmount, \"getSelection\")"
  , "        ? prows.findIndex((r) => r.id === pmount.getSelection().id) : -1);"
    -- The add affordance, and the whole of it: `+' puts an empty property at the
    -- end of the drawer and opens it.  Keyboard-first means the KEY is the offer,
    -- where a row that is always empty was chrome every reader of the panel had
    -- to filter back out.  A row whose key is emptied is still a property
    -- deleted, which is what `d' spells as a key press.
  , "    function addProperty() {"
  , "      const id = `P${pseq++}`;"
  , "      prows.push({ id, key: \"\", val: \"\", fixed: false });"
  , "      repaint(id);"
  , "      openRow();"
  , "    }"
    -- What the panel would write: every property row carrying a key, in the
    -- order they sit in.  A row whose key has been emptied is a deletion.  Both
    -- fields are trimmed, because the server hands them over trimmed: what the
    -- panel can show is then exactly what it can write, and a space nobody could
    -- ever see again cannot be typed into a file.
  , "    const props = () => prows"
  , "      .filter((r) => !r.fixed)"
  , "      .map((r) => [r.key.trim(), r.val.trim()])"
  , "      .filter((p) => p[0] !== \"\");"
    -- And the planning line: the fixed rows carrying a value, in org's order.
    -- An empty row is that entry absent, so clearing all three is how the line
    -- comes off — the server drops it rather than writing a bare keyword.
  , "    const planning = () => prows"
  , "      .filter((r) => r.fixed && r.val.trim() !== \"\")"
  , "      .map((r) => [r.key, r.val.trim()]);"
    -- Crossing the panes, and the two modes.  Entering the panel BLURS the
    -- textarea: nav holds the keys with nothing focused, so a field left focused
    -- behind it would take every letter as text.  `pnav' is that state, and
    -- `typing()' counts it as a focus of its own so the table's keys stay dead
    -- under it; the mouse can undo it by clicking back into the text, which is
    -- what the focus listener is for.
  , "    const pnav = () => el(\"mprops\").className === \"on\";"
  , "    function enterPanel() {"
  , "      el(\"mprops\").className = \"on\"; el(\"mtext\").blur();"
  , "    }"
  , "    function leavePanel() {"
  , "      el(\"mprops\").className = \"\"; el(\"mtext\").focus();"
  , "    }"
  , "    el(\"mtext\").addEventListener(\"focus\", () => pnav() && leavePanel());"
    -- Movement is the mount's `selectStep', the same call the table's own `n'
    -- and `p' make, so the cursor a reader moves here is the renderer's cursor
    -- and there is nothing to keep in step with it.
  , "    const moveCur = (step) => stepIn(pmount, step);"
    -- THE EDIT OVERLAY.  The renderer owns its rows and rewrites them as it
    -- scrolls, so an edit cannot live inside one: the two fields sit OVER the
    -- panel, anchored to the row the cursor is on.  The value takes the focus,
    -- since editing an existing property is almost always editing its value —
    -- except where there is no key yet, which is the add-row, and there the key
    -- is the thing being typed.  A planning row's key is org's, so its field is
    -- read-only text with a caret in it.
  , "    const pediting = () => el(\"pedit\").className === \"on\";"
  , "    function openRow() {"
  , "      const at = patAt();"
  , "      if (at === -1) return;"
  , "      const r = prows[at];"
  , "      el(\"pedit\").className = \"on\";"
  , "      el(\"pkey\").value = r.key;"
  , "      el(\"pval\").value = r.val;"
  , "      el(\"pkey\").readOnly = r.fixed;"
      -- The renderer stamps `tv-sel' on its own frame, so a row selected in
      -- THIS tick has no marked element yet: `+' would measure the row the
      -- cursor was on before it.  One frame later there is one.
  , "      soon(place);"
  , "      (r.fixed || r.key ? el(\"pval\") : el(\"pkey\")).focus();"
  , "    }"
  , "    function shutEdit() {"
  , "      el(\"pedit\").className = \"\";"
  , "      el(\"pkey\").blur(); el(\"pval\").blur();"
  , "    }"
    -- Where the overlay sits: over the row the renderer has selected.  Its
    -- GEOMETRY is the only thing this page reads out of the mount's own DOM, and
    -- it reads nothing about the row but where it is — a page with no layout
    -- (the suite's) simply leaves the overlay where it was put.
    -- The row's box, read through the handle's own `el' rather than by
    -- querying the pane: the mount publishes its root, so the one geometry read
    -- this page makes goes through a published door.
  , "    function place() {"
  , "      if (!pediting()) return;"
  , "      const tr = pmount.el.querySelector(\"tbody tr.tv-sel\");"
  , "      if (!tr) return;"
  , "      const a = tr.getBoundingClientRect();"
  , "      const b = el(\"mprops\").getBoundingClientRect();"
  , "      el(\"pedit\").style.top = `${a.top - b.top}px`;"
  , "      el(\"pedit\").style.height = `${a.height}px`;"
  , "    }"
    -- Committing: the row takes the text the fields are holding and the overlay
    -- goes.  This is the one thing that can make the sheet dirty from the panel
    -- — an edit nobody committed was never in `props()'.  A fixed row keeps its
    -- key, which is org's rather than the author's.
  , "    function commitRow() {"
  , "      const r = prows[patAt()];"
  , "      if (!r.fixed) r.key = el(\"pkey\").value;"
  , "      r.val = el(\"pval\").value;"
  , "      shutEdit();"
  , "      repaint();"
  , "    }"
    -- ESC over an open row is the ROW's: the overlay goes and the text the row
    -- is holding stands, which is the text it was opened on.  The sheet's own
    -- ESC ladder therefore only ever sees the key from nav — that is why this
    -- runs from the keymap's `cancel' rather than from a listener of its own.
  , "    function cancelRow() {"
  , "      shutEdit();"
  , "      echo(\"ESC → keyboard-quit (row unchanged)\");"
  , "    }"
    -- DELETION IS THE TABLE'S GESTURE, over the renderer's own flags: `d' flags
    -- the row at point, `d' again — or `D' — takes every flagged row, and `u'
    -- takes a flag off.  One implementation of the gesture in this page, since
    -- the set, the wash and the count are all the mount's.
    --
    -- WHAT "taken" MEANS is the row's.  A property is dropped, which is the
    -- emptied key spelled as a key press.  A planning entry is CLEARED and its
    -- row stands: the three are org's keys rather than the author's, and an
    -- empty value is already how an entry is absent.
    -- IDS is the set the key worked out, HOW the word the pill calls it: a
    -- caller that has already found the row and read the flags does not make
    -- this look for them again.
  , "    function pdelete(ids, how) {"
  , "      const gone = new Set(ids);"
  , "      const cleared = prows.filter((r) => gone.has(r.id) && r.fixed);"
  , "      for (const r of cleared) r.val = \"\";"
  , "      prows = prows.filter((r) => r.fixed || !gone.has(r.id));"
  , "      pmount.clearFlags();"
  , "      repaint();"
      -- The command name is the BINDING's and the brackets carry what it did:
      -- org has no one function for taking a planning entry off — it is
      -- `org-schedule' or `org-deadline' under a prefix — so the line names the
      -- keys it cleared rather than claiming a property function did it.
  , "      const also = cleared.map((r) => r.key).join(\", \");"
  , "      echo(`D → org-delete-property (${how}${also ? ` · ${also} cleared` : \"\"})`);"
  , "    }"
    -- The panel's own keys, behind the dispatch and for the reason the value
    -- palette's are: while the panel holds them `typing()' is true, so every
    -- `table' row is dead and nothing here takes a key the map wanted — `d'
    -- flags a property rather than an org row, and `n' moves no table row.
    --
    -- TAB crosses the panes — out of the body into the panel's cursor, out of
    -- nav back into the body — and the cursor is where it was left.  Two stops,
    -- so both directions are one toggle and S-TAB is the same line.  In nav the
    -- keys are movement: n/p and j/k both, unconditionally, because a row with
    -- no field in it leaves every printable key free and both spellings cost
    -- nothing to satisfy at once; the arrows are the pair that needs neither.
    -- RET opens the row at point, @+@ adds one at the end, and @d@/@D@/@u@ are
    -- the deletion gesture.
    --
    -- In edit TAB is the hop between the row's two fields — one row, two fields,
    -- and nothing else for it to mean — so the crossing is suspended for as long
    -- as a row is open.  RET commits.  Raw mode has one pane and nowhere to
    -- cross to, so TAB is the browser's there.
    --
    -- A HELD `d' must not flag a row and delete it from one press, which is the
    -- confirmation the two-press shape exists to be — the dispatch's own ONCE
    -- list cannot reach a key this listener owns, so the guard is spelled here.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!editing) return;"
  , "      const k = keyName(e), crossing = k === \"TAB\" || k === \"S-TAB\";"
  , "      const step = rowStep(k);"
  , "      if (pediting()) {"
  , "        if (crossing)"
  , "          (document.activeElement === el(\"pkey\") ? el(\"pval\") : el(\"pkey\")).focus();"
  , "        else if (k === \"RET\") commitRow();"
  , "        else return;   // ESC is the keymap's, and puts the row back"
  , "      } else if (!pnav()) {"
  , "        if (raw || !crossing || document.activeElement !== el(\"mtext\")) return;"
  , "        enterPanel();"
  , "      } else if (crossing) leavePanel();"
  , "      else if (k === \"RET\") openRow();"
  , "      else if (k === \"+\") addProperty();"
  , "      else if (step) moveCur(step);"
    -- The panel's arrows are VERTICAL ONLY, where the table's walk both axes.
    -- The mount has two columns here, but a column selection would say nothing
    -- about the edit: `RET' opens the WHOLE row — both fields, whichever cell a
    -- cursor sat in — and `TAB' is what crosses between them.  So a horizontal
    -- key would move a highlight and change nothing a reader can act on.
  , "      else if (k === \"d\" || k === \"D\" || k === \"u\") { if (!e.repeat) pflag(k); }"
  , "      else return;"
  , "      e.preventDefault();"
  , "    });"
    -- dired's `d', over the panel's rows: the first press flags the row at point
    -- and a second `d' on an already-flagged row IS `D' — it calls the same
    -- handler, so it deletes EVERY flagged row rather than the one under it.
    -- `u' takes a flag off and walks on, the way it does over the table.
  , "    function pflag(k) {"
  , "      if (!flagsOn(pmount)) { echo(`${k} → this table-view.js has no delete flags`); return; }"
  , "      const at = patAt();"
  , "      if (at === -1) { echo(`${k} → org-delete-property (no row)`); return; }"
  , "      const id = prows[at].id, flags = pmount.getFlagged();"
  , "      if (k === \"D\" || (k === \"d\" && flags.indexOf(id) !== -1)) {"
  , "        pdelete(flags.length ? flags : [id],"
  , "                flags.length ? `${flags.length} flagged` : \"row\");"
  , "        return;"
  , "      }"
  , "      if (k === \"u\") {"
  , "        pmount.unflagRow(id);"
  , "        echo(\"u → delete-unflag (flag cleared)\");"
  , "        moveCur(1);"
  , "        return;"
  , "      }"
  , "      pmount.flagRow(id);"
  , "      echo(\"d → delete-flag (d again deletes)\");"
  , "    }"
    -- What a flush sends: the subtree whole in raw mode, the two panes apart
    -- otherwise.  The server joins them, so this page never spells a drawer.
  , "    const asked = () => raw"
  , "      ? { org: el(\"mtext\").value }"
  , "      : { body: el(\"mtext\").value, properties: props(), planning: planning() };"
    -- ONE BUTTONLESS SHEET, twice over.  The subtree sheet and the settings
    -- sheet are the same flow over different files — a state word, a flush, and
    -- a close that syncs on the way out — so the ladder is written once and each
    -- sheet supplies the verbs it differs in: `dirty', `flush', `refresh' (the
    -- digests a conflict overwrites under), `shut', and the log `scope' its own
    -- lines are filed under.  Never both up at once, since `openSettings'
    -- refuses over an open sheet, which is what makes `activeSheet' total.
    --
    -- Where a sheet stands is ONE word, and `note' is its only writer: the
    -- header wears it as text and as a class, and everything that asks reads it
    -- back off the sheet.  With no buttons the keys are the whole of the offer,
    -- so the states that wait for one say which key — and the retry line is
    -- spelled once, three copies of it being three chances to drift.
  , "    const RETRY = \" — C-x C-s retry · ESC discard\";"
  , "    const WORDS = { synced: \"synced\", syncing: \"syncing…\","
  , "      conflict: \"conflict — C-x C-s overwrite · ESC discard\","
  , "      error: \"error\" + RETRY };"
  , "    function note(s, next, message) {"
  , "      s.state = next;"
  , "      el(s.noteId).className = next;"
  , "      el(s.noteId).textContent = message || WORDS[next];"
  , "    }"
  , "    const stuck = (s, why) => note(s, \"error\", why && `${why}${RETRY}`);"
  , "    const subtreeSheet = {"
  , "      noteId: \"mnote\", scope: \"sync\", state: \"synced\","
  , "      closed: \"closed without writing — the file is as it was\","
  , "      dirty: () => dirty(),"
  , "      flush: () => flush(editing.digest),"
    -- What a conflict overwrites under: the digest the file carries NOW, unless
    -- the sheet moved on to another headline while the read was out.
  , "      refresh: () => {"
  , "        const h = editing;"
  , "        return headline(h.id).then((b) => {"
  , "          if (editing !== h) return false;"
  , "          h.digest = b.digest;"
  , "          return true;"
  , "        });"
  , "      },"
  , "      shut: () => shut(),"
  , "    };"
  , "    const activeSheet = () => (editing ? subtreeSheet : settings ? configSheet : null);"
  , "    const sync = (next, message) => note(subtreeSheet, next, message);"
  , "    function shut() {"
  , "      el(\"modal\").className = \"\"; editing = null; base = \"\"; baseProps = null;"
  , "      shutEdit();"
  , "      el(\"mprops\").className = \"\";   // and the keys go back to the table"
  , "    }"
  , "    // POST the sheet over the subtree, pinned to DIGEST.  A 200 carries"
  , "    // the file's new digest — the receipt chains, so the next flush needs no"
  , "    // re-materialize — and both baselines move with it."
  , "    function flush(digest) {"
  , "      const h = editing, sent = asked();"
  , "      sync(\"syncing\");"
  , "      return post(h.id, digest, sent)"
  , "        .then((r) => r.json().then((b) => ({ status: r.status, body: b })))"
  , "        .then((a) => {"
  , "          if (a.status === 200) {"
  , "            h.digest = a.body.digest;"
  , "            base = raw ? sent.org : sent.body;"
  , "            baseProps = raw ? null : JSON.stringify([sent.properties, sent.planning]);"
  , "            sync(\"synced\");"
  , "            return true;"
  , "          }"
    -- A refused planning entry is a 409 like a moved file, and it waits for a
    -- keystroke the same way — but it names the field rather than the file, so
    -- it goes through `stuck' and says so.
  , "          if (a.status === 409 && a.body.reason !== \"planning\") sync(\"conflict\");"
  , "          else stuck(subtreeSheet, a.body.error || `sync failed (${a.status})`);"
  , "          return false;"
  , "        })"
  , "        .catch((e) => { stuck(subtreeSheet, e.message); return false; });"
  , "    }"
  , "    // C-x C-s, over whichever sheet is up.  Mid-edit it is a manual flush;"
  , "    // on a conflict it is the deliberate keystroke that overwrites — ask for"
  , "    // the digests the files carry now and post what the author is looking at"
  , "    // over them."
  , "    function saveSheet() {"
  , "      const s = activeSheet();"
  , "      if (!s || s.state === \"syncing\") return;"
  , "      if (s.state !== \"conflict\") { s.flush(); return; }"
  , "      s.refresh().then((ok) => ok && s.flush()).catch((e) => stuck(s, e.message));"
  , "    }"
  , "    // The way out — ESC, the backdrop, q.  Pristine costs no request and"
  , "    // touches no file; dirty flushes and closes on the 200; a sheet with"
  , "    // trouble in it discards, which is what a second ESC is."
  , "    function leaveSheet() {"
  , "      const s = activeSheet();"
  , "      if (!s) return;"
  , "      if (s.state === \"conflict\" || s.state === \"error\") {"
  , "        s.shut();"
  , "        append(s.scope, \"info\", s.closed);"
  , "        return;"
  , "      }"
  , "      if (!s.dirty()) { s.shut(); return; }"
  , "      if (s.state !== \"syncing\") s.flush().then((ok) => ok && s.shut());"
  , "    }"
    -- The backdrop is the mouse's ESC, for both sheets: a click that lands on
    -- the veil itself rather than on the box over it.
  , "    for (const id of [\"modal\", \"config\"])"
  , "      el(id).addEventListener(\"click\","
  , "        (e) => { if (e.target === el(id)) leaveSheet(); });"
    -- C-c ' — org's `edit-special' rhyme, one subtree seen two ways: body and
    -- panel, or the raw org the panes were cut out of.  The cut is the server's,
    -- so the toggle RE-READS the headline rather than splitting or joining
    -- anything here; that is what keeps an org parser out of this page.  A
    -- re-read cannot carry unsaved work, so a dirty sheet is refused and told
    -- which key would let it through.  The re-read is also a fresh materialize,
    -- which is why it lands at `synced' whatever it was at before.
  , "    function toggleRaw(b) {"
  , "      if (!editing) return;"
  , "      if (dirty()) { said(b, \"sync first — C-x C-s\"); return; }"
  , "      const h = editing, want = !raw;"
  , "      headline(h.id).then((fresh) => {"
  , "        if (editing !== h) return;   // the sheet moved on while this was out"
  , "        editing = fresh; raw = want;"
  , "        fill(fresh);"
  , "        sync(\"synced\");"
  , "        el(\"mtext\").focus();"
  , "        said(b, raw ? \"raw org\" : \"properties panel\");"
  , "      }).catch((e) => stuck(subtreeSheet, e.message));"
  , "    }"
  , "    // A tab closing on an edited sheet still owes the file the text:"
  , "    // `keepalive' outlives the document, and a pristine sheet sends nothing."
  , "    addEventListener(\"beforeunload\", () => {"
  , "      if (!dirty()) return;"
  , "      post(editing.id, editing.digest, asked(), { keepalive: true }).catch(() => {});"
  , "    });"
  , ""
  , "    // Rows.  The renderer virtualizes, so a row outside the window has no"
  , "    // element: movement is ids out of `getVisible()' handed to `select(id)'."
  , "    // Which row is on is the renderer's too — it answers with the column,"
  , "    // and a click moves both without telling us — so the DOM read is the"
  , "    // fallback for an asset predating the call, and nothing is kept here."
  , "    const visible = () => (table ? table.getVisible() : []);"
  , "    const focusedId = () => {"
  , "      if (cells()) return table.getSelection().id;"
  , "      const tr = document.querySelector(\"#app .tv-table tbody tr.tv-sel\");"
  , "      return tr ? tr.dataset.id : null;"
  , "    };"
  , "    function pick(list, i) {"
  , "      if (!list.length) { append(\"cmd\", \"info\", \"no rows to move through\"); return; }"
  , "      const id = list[Math.max(0, Math.min(list.length - 1, i))].id;"
  , "      // Row movement carries the column along: null until a horizontal key"
  , "      // picks one, so a page nobody has moved sideways in keeps whole rows."
  , "      table.select(id, column());"
  , "    }"
    -- A row step is the renderer's `selectStep': it carries the column, and it
    -- turns the page at either end, which only the renderer knows there is —
    -- `getVisible()' is one page's worth, so index arithmetic here would stop
    -- dead at a boundary.  An asset predating the call has no pages either, so
    -- the old walk over the visible ids is exactly right for it.
  , "    const steps = () => !!table && typeof table.selectStep === \"function\";"
  , "    function move(step) {"
  , "      if (steps()) {"
  , "        if (visible().length) table.selectStep(step);"
  , "        else append(\"cmd\", \"info\", \"no rows to move through\");"
  , "        return;"
  , "      }"
  , "      const list = visible(), at = list.findIndex((r) => r.id === focusedId());"
  , "      pick(list, at === -1 ? (step > 0 ? 0 : list.length - 1) : at + step);"
  , "    }"
    -- What a key says when it has run: the sequence, the COMMAND, and what
    -- happened in brackets after it.  The command is the blob's own identifier
    -- and is spoken verbatim — `> → last-row', never `> → last row' — because
    -- these names are the handle a rebinding config will address a function by,
    -- and a reader who learns one off the echo has to be able to type it. The
    -- prose goes in the brackets, where it names an outcome rather than a
    -- function.  Every key echoes through here, so there is one shape and one
    -- place the rule can be broken.
  , "    const said = (b, what) =>"
  , "      echo(`${b.seq} → ${b.command}${what ? ` (${what})` : \"\"}`);"
    -- Pages.  The turn is the renderer's, and the bracket says where it landed
    -- rather than repeating the key: `] → next-page (page 3/129)' reads the
    -- same at a stop as at a turn.
  , "    const pager = () => !!table && typeof table.nextPage === \"function\""
  , "      && typeof table.pageInfo === \"function\";"
    -- The sort, which is `^''s alone now: an ORDER IS A QUERY, so the agenda
    -- states its own by carrying a `sort:' token and no page here calls
    -- `sortBy'.  Named with the rest of the optional calls, since this is where
    -- a reader greps for which renderer calls are feature-detected.
    --
    -- `sortPromote' composes the chain and WRITES IT INTO THE QUERY as `sort:'
    -- tokens, which comes back through `onFilter' like any other query change —
    -- so the URL carries the order, DEL takes a key off it, and the server is
    -- asked for the order it is about to be sent.  This page keeps no record of
    -- the chain: the handle publishes it (getSort) and the query spells it.
  , "    const sorts = () => !!table && typeof table.sortPromote === \"function\";"
  , "    function turnPage(b, step) {"
  , "      if (!pager()) { said(b, \"this table-view.js has no pager\"); return; }"
  , "      if (step > 0) table.nextPage(); else table.previousPage();"
  , "      const at = table.pageInfo();"
  , "      said(b, `page ${at.page}/${at.pages}`);"
  , "    }"
    -- The ends of the buffer, progressively.  `<' takes the page's first row;
    -- pressed AGAIN, already on it, it turns back a page and lands on THAT
    -- page's first row, and `>' mirrors it — so the pair reaches the ends of
    -- the SET rather than of the page, and a reader who wants one page turned
    -- still has the brackets.  Page one's first row and the last page's last
    -- row are stops: the turn declines and nothing moves.
    --
    -- Both climbs land at the wrong end and need a select of their own: the
    -- renderer puts the cursor on the end it ARRIVES at — `nextPage' on the
    -- new page's first row, `previousPage' on its last — which is the opposite
    -- end from the one the key is named for, in both directions.  The column
    -- comes back out of the renderer: a turn re-selects with the column it
    -- had, so reading `column()' after one reads what it kept.
    --
    -- A turn is an explicit page action, so the renderer snaps out of
    -- continuous presentation back to paged at the page it turned to.  That is
    -- what a key named for an end of the buffer means — the reader asked for a
    -- boundary, and paged is the presentation that has them.
  , "    function endStop(b, last) {"
  , "      const list = visible();"
  , "      if (!list.length) { append(\"cmd\", \"info\", \"no rows to move through\"); return; }"
  , "      const end = (rows) => rows[last ? rows.length - 1 : 0].id;"
    -- Not there yet — or an asset with no pages, where there is nowhere to
    -- climb to and the within-page jump is the whole of the key.
  , "      if (!pager() || focusedId() !== end(list)) {"
  , "        table.select(end(list), column());"
  , "        said(b, \"\");"
  , "        return;"
  , "      }"
  , "      if (!(last ? table.nextPage() : table.previousPage())) { said(b, \"\"); return; }"
  , "      const turned = visible();"
  , "      if (turned.length) table.select(end(turned), column());"
  , "      const at = table.pageInfo();"
  , "      said(b, `page ${at.page}/${at.pages}`);"
  , "    }"
  , "    // Cells.  The column is part of the renderer's selection, so it needs no"
  , "    // state here: it rides along with row"
  , "    // movement, and goes when the selection that holds it goes.  A whole-row"
  , "    // selection has none, and the first horizontal key lands on the first"
  , "    // column whichever direction asked."
  , "    const cells = () => !!table && typeof table.getSelection === \"function\";"
  , "    const column = () => (cells() ? table.getSelection().col : null);"
  , "    function moveCol(b, step) {"
  , "      if (!cells()) { said(b, \"this table-view.js has no cell selection\"); return; }"
  , "      const at = column(), want = at === null ? 0 : at + step;"
      -- Walking off the cells LANDS rather than bumping: a column index outside
      -- the table is no column at all to the renderer, which nulls it and gives
      -- back the whole-row look.  So the step is handed over out of range and
      -- the exit is a real move — where a clamp here used to swallow the key
      -- and say `at last' at a wall the renderer does not have.  The column
      -- comes back out of `column()' rather than off `want', since the
      -- renderer's answer is the one that decides.
  , "      const id = focusedId();"
  , "      if (!id || !table.select(id, want)) { said(b, \"no row\"); return; }"
  , "      const now = column();"
  , "      said(b, now === null ? \"row mode\" : (cols[now].header || cols[now].key));"
  , "    }"
    -- Marks.  The renderer holds them, keyed by id, so nothing about them is
    -- kept here: which rows are marked, how many there are and what a mark
    -- survives are all its answers.  Dired's advance is this page's, though —
    -- the key that marks is the key that walks, which is what makes a held `m'
    -- a run down a column.
  , "    const marking = () => !!table && typeof table.toggleMark === \"function\";"
    -- Archive flags are the renderer's for the same reason marks are: a flag has
    -- to outlive a `setRows', a filter that hides its row and a page it is not
    -- on, and only the thing that draws the rows can do that.  An asset predating
    -- the calls says so rather than growing a shell-side set the next paint would
    -- lose.
  , "    const flagging = () => flagsOn(table);"
  , "    const isFlagged = (id) => flagging() && table.getFlagged().indexOf(id) !== -1;"
      -- The same question of the other set, and asked the same way: the renderer
      -- is consulted at the moment it matters rather than copied into a set here.
  , "    const isMarked = (id) => marking() && table.getMarked().indexOf(id) !== -1;"
    -- The log names a row the way the table does: by its title, out of the rows
    -- in hand — the page on screen, and the unfiltered baseline behind it.  A
    -- row in neither is named by its id, which is a lookup failure a reader can
    -- still act on.  `displayText' is the renderer's own link rule, so what the
    -- line spells is what the cell shows.
  , "    const titleOf = (id) => {"
  , "      const row = visible().concat(all).find((r) => r.id === id);"
  , "      const cell = row && (row.cells || {}).title;"
  , "      const shown = typeof TableView.displayText === \"function\""
  , "        ? TableView.displayText(cell) : String(cell || \"\");"
  , "      return shown || id;"
  , "    };"
    -- One wording for every write a key makes: the pill counts what landed, the
    -- log says which rows they were.  Bulk is one line per row, since a set
    -- spanning three files can come back two-thirds applied.
  , "    const noted = (id, what) =>"
  , "      append(\"cmd\", \"info\", `headline ${JSON.stringify(titleOf(id))} ${what}`);"
    -- TOGGLING is `m', which flips the way dired's does and takes the
    -- renderer's word for where it landed.  `u' is never a toggle: it flips
    -- too, then puts back anything it just laid down, so walking a column of
    -- marks clears it rather than laying it again.  Both calls are one
    -- statement apart and the renderer coalesces its painting to a frame, so
    -- the flip is never drawn.
  , "    function mark(b, toggling) {"
  , "      if (!marking()) { said(b, \"this table-view.js has no marks\"); return; }"
  , "      const id = focusedId();"
  , "      if (!id) { said(b, \"no row\"); return; }"
    -- `u' takes the archive FLAG off first: it is the more recent thing a
    -- reader put on the row and the one that would otherwise write a file.
    -- One key for both, which is what dired does, and the echo says which.
  , "      if (!toggling && isFlagged(id)) {"
  , "        table.unflagRow(id);"
  , "        noted(id, \"unmarked for deletion\");"
  , "        said(b, \"flag cleared\");"
  , "        move(1);"
  , "        return;"
  , "      }"
  , "      let on = table.toggleMark(id);"
  , "      if (on && !toggling) on = table.toggleMark(id);"
  , "      said(b, `${on ? \"marked\" : \"unmarked\"} · ${table.markedCount()}`);"
  , "      move(1);"
  , "    }"
    -- Commands.  A structured write names ROWS and lets the server compute the
    -- spans, so nothing here knows what a headline looks like — and nothing
    -- here touches the table afterwards either: the rows arrive over the socket
    -- once the watch has re-read the files, the way an editor's save arrives.
    --
    -- Which rows a command runs over is per COMMAND, and the two answers are
    -- deliberately different.  `set-state' takes the MARKED set, which is the
    -- generic bulk selection — mark a run of rows, set them all. Archiving takes
    -- the FLAGGED set, which is a selection made for archiving and nothing else
    -- (`flagged' below): the destructive-looking command must not inherit a
    -- selection a reader built for some other purpose.  Either way the set is
    -- the renderer's and is asked for when the command runs rather than tracked
    -- here.
  , "    const targets = () => {"
  , "      const marked = marking() ? table.getMarked() : [];"
  , "      if (marked.length) return marked;"
  , "      const id = focusedId();"
  , "      return id ? [id] : [];"
  , "    };"
    -- A partial answer is ordinary here: each file is its own write, so one
    -- that moved on disk refuses its rows while the rest land.  The count goes
    -- in the pill and every refusal in the log.
    -- HOW names what the pill says inside the parentheses, and is given the
    -- number of rows that LANDED so a partial answer cannot read as a whole one:
    -- the count alone is the default, and a key that ran over a named set says
    -- which set it was — falling back to the bare count when nothing landed,
    -- since "row" over zero rows would be a lie.
    -- The route, and the only place this page spells it: a body in, the answer
    -- out, and the server's own words thrown where it refused.  Both writing
    -- keys go through it — the one that names rows and the one that makes one —
    -- so what a refusal looks like is decided once.
  , "    const postCommand = (body) =>"
  , "      fetch(\"/command\", {"
  , "        method: \"POST\","
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify(body),"
  , "      }).then((r) => r.json().then((answer) => {"
  , "        if (!r.ok) throw new Error(answer.error || r.status);"
  , "        return answer;"
  , "      }));"
    -- And the one shape a failed write takes: the pill says what went wrong and
    -- the strip keeps it, named by the command that was asked for.
  , "    const failed = (b, name) => (e) => {"
  , "      said(b, e.message);"
  , "      append(\"cmd\", \"error\", `${name} failed: ${e.message}`);"
  , "    };"
    -- And the shape a palette raised over an unanswered request takes: a
    -- palette with nothing in it is no offer, so the overlay comes down and the
    -- reason goes to the strip.  It takes the prompt it was raised FOR, since a
    -- reader who left and raised another must not have that one closed.
  , "    const askFailed = (mine, name) => (e) => {"
  , "      if (prompting === mine) unask();"
  , "      append(\"cmd\", \"error\", `${name} failed: ${e.message}`);"
  , "    };"
    -- The results come back out, undefined where the request failed: a palette
    -- that stays open has to fold what landed into the state it is drawing, and
    -- it is the only caller that reads them.  Every other one ignores the
    -- answer, which is the pill and the log this already wrote.
  , "    function fire(b, name, ids, args, verb, how) {"
  , "      return postCommand({ name, ids, args }).then((answer) => {"
  , "        const results = answer.results || [];"
  , "        const bad = results.filter((x) => !x.ok);"
  , "        const landed = results.length - bad.length;"
  , "        said(b, `${verb} · ${how ? how(landed) : landed}`);"
    -- What one landed write did, per row.  The names are the route's whole
    -- vocabulary, so the wording sits here rather than at each key that fires.
  , "        const what = name === \"archive\" ? \"archived\""
  , "          : name === \"add-tag\" ? `tagged :${args.tag}:`"
  , "          : name === \"remove-tag\" ? `untagged :${args.tag}:`"
  , "          : name === \"rename-tag\" ? `retagged ${args.from}→${args.to}`"
  , "          : name === \"set-planning\""
  , "            ? `${args.keyword.toLowerCase()} ${args.date || \"cleared\"}`"
  , "          : args.keyword ? `→ ${args.keyword}` : \"state cleared\";"
  , "        for (const x of results) if (x.ok) noted(x.id, what);"
      -- AN ARCHIVED ROW SPENDS ITS MARK, the way it spends its flag.  The mark
      -- is the renderer's and survives a `setRows' and a filter that hides its
      -- row — which is what makes it useful, and what would otherwise leave an
      -- archived row marked INVISIBLY: `markedCount()' would count it, `M' and
      -- `U' would answer about it, and it would come back marked the moment a
      -- reader looked at `tag:*archive*'.  Only the rows that LANDED, since a
      -- refused one was not archived.
  , "        if (name === \"archive\") unmark(results);"
  , "        if (bad.length)"
  , "          append(\"cmd\", \"error\", bad.map((x) => `${x.id}: ${x.error}`).join(\" · \"));"
  , "        return results;"
  , "      }).catch(failed(b, name));"
  , "    }"
    -- The marks the rows RESULTS landed on were carrying, taken off.  `toggleMark'
    -- is the only door the renderer offers, so a membership test comes first,
    -- and it is `isMarked' — the renderer asked at the moment it matters, never
    -- a set kept here.  Feature-detected through it, so an asset with no marks
    -- has none to spend.
  , "    function unmark(results) {"
  , "      for (const x of results)"
  , "        if (x.ok && isMarked(x.id)) table.toggleMark(x.id);"
  , "    }"
    -- Archiving: ONE implementation, reached by both keys.  The tag goes on, the
    -- headline stays, and the default view stops showing it.  It runs over the
    -- FLAGGED set when there is one and the row at point otherwise, and never
    -- over the marked one — a mark is the generic bulk selection a reader lays
    -- down to set a state over a run of rows, and letting the archive key
    -- inherit it makes every mark a loaded gun.
    --
    -- `d' on an already-flagged row calls THIS, so dired's `dd' is flag then
    -- mass-confirm: one flag archives one row, three flags archive three, from
    -- the same second press.  `D' is the same gesture without the first half.
    --
    -- The flags are SPENT here.  They have to be: the renderer keeps a flag
    -- whose row a filter has hidden — which is what makes a flag survive the
    -- refetch this write causes — so a set left standing would be re-archived by
    -- the next press, and the row at point would never be reachable again.
  , "    function archive(b) {"
  , "      const flags = flagging() ? table.getFlagged() : [];"
  , "      if (flags.length) {"
  , "        table.clearFlags();"
  , "        fire(b, \"archive\", flags, {}, \"archived\", (n) => `${n} flagged`);"
  , "        return;"
  , "      }"
  , "      const id = focusedId();"
  , "      if (id) fire(b, \"archive\", [id], {}, \"archived\", (n) => (n ? \"row\" : n));"
  , "      else said(b, \"no row\");"
  , "    }"
    -- Capture: the one write that names no row, so it takes none of the
    -- selection machinery above.  The line is raw org — `TODO Buy milk
    -- :errands:' captures a keyword, a title and a tag — and the server decides
    -- WHERE, out of the tree's own `#+GLANCE_CAPTURE_TARGET:'.  The row comes
    -- back over the socket once the watch has read the file it was written to,
    -- like every other write here.
  , "    function captureRow(b, text) {"
  , "      const typed = text.trim();"
  , "      if (!typed) { said(b, \"nothing to capture\"); return; }"
  , "      postCommand({ name: \"capture\", args: { text: typed } }).then((a) => {"
  , "        said(b, `captured · ${a.file}`);"
  , "        append(\"cmd\", \"info\","
      <> " `headline ${JSON.stringify(typed)} captured into ${a.file}`);"
  , "      }).catch(failed(b, \"capture\"));"
  , "    }"
    -- Reschedule: the same shape as the state palette, over the same rows —
    -- marked set, else the row at point — with a line of text where that one has
    -- a list.  The server parses the date (ISO, `+3d', `today', or org's own
    -- bracketed form) and refuses anything else as the whole request, so an
    -- unreadable line moves no row rather than some of them.  An EMPTY line
    -- clears the entry, which is how the planning line comes off.
    -- A count of rows, pluralised: every surface that names a set of them says
    -- it the same way, so the rule sits here rather than at each of them.
  , "    const rowsWord = (n) => `${n} row${n === 1 ? \"\" : \"s\"}`;"
    -- The rows a keyed write runs over, and the title that names them: the two
    -- keys that ask something before writing count them the same way.
  , "    function overTargets(b, label, k) {"
  , "      const ids = targets();"
  , "      if (!ids.length) { said(b, \"no row\"); return; }"
  , "      k(ids, `${label} · ${rowsWord(ids.length)}`);"
  , "    }"
  , "    function planRows(b, keyword) {"
  , "      overTargets(b, keyword.toLowerCase(), (ids, title) =>"
  , "        askText(title, \"RET sets it · empty clears it · ESC leaves\", \"\", (c) => {"
  , "          const date = c.text.trim();"
  , "          fire(b, \"set-planning\", ids, { keyword, date: date || null },"
  , "               date || \"cleared\");"
  , "        }));"
  , "    }"
    -- The value palette: a prompt of this page's own, since the renderer's
    -- overlay belongs to the filter and this page may not reach into it.  ESC is
    -- the keymap's own `keyboard-quit', which closes whichever overlay is up.
    --
    -- It opens in WHICH-KEY mode: every entry wears a letter and that letter
    -- commits on its own.  The palette IS the confirmation — a reader who
    -- pressed `t' has seen the list saying `t' sets TODO — so there is no
    -- second key, and the drift lock is what makes a mis-press cheap.  `/'
    -- falls back to the completing-read this used to be, for a cycle wide
    -- enough that some entry claimed nothing.
    --
    -- The keys are handled in a second document listener rather than on the
    -- field, and that is safe because it runs after the dispatch: while the
    -- palette is up `typing()' has already made every `table' row dead, so the
    -- only row that can fire ahead of this is ESC, which is the one that should.
  , "    let prompting = null;"
    -- The which-key assignment: each entry claims the first letter of its OWN
    -- spelling that no earlier entry took, over one a-z namespace in palette
    -- order.  What comes back is that letter's INDEX — the display bolds it
    -- there, which is what teaches why DELEGATED is `e' — and -1 for an
    -- entry whose every letter was taken.  Pure and order-only, so one tree's
    -- cycle always yields the same letters and the muscle memory holds.
  , "    function whichKeys(labels) {"
  , "      const taken = new Set();"
  , "      return labels.map((label) => {"
  , "        for (let i = 0; i < label.length; i += 1) {"
  , "          const c = label[i].toLowerCase();"
  , "          if (c >= \"a\" && c <= \"z\" && !taken.has(c)) { taken.add(c); return i; }"
  , "        }"
  , "        return -1;"
  , "      });"
  , "    }"
    -- A declaration rather than a `const', so a direct `eval' of this glue
    -- leaks it the way it leaks `whichKeys': the suite's harness reports the
    -- assignment through THIS function rather than re-spelling the rule.
  , "    function letterAt(label, at) {"
  , "      return at === -1 ? null : label[at].toLowerCase();"
  , "    }"
    -- The list palette, raised EMPTY on the keydown: `t' and `:' fill it from
    -- `/keywords' and `/tags', which is what makes the table the resolver's
    -- answer rather than this page's guess.  What a reader sees until the fill
    -- lands is the line saying so.
    --
    -- `raising' is that keydown, still in flight: this listener sits behind the
    -- dispatch, so the press that opened the palette is the next one it sees,
    -- and `t' is both the opener and a letter in what it opens.  Every palette
    -- here is raised that way, so it is set rather than passed.  The prompt
    -- itself is handed back, so a fill landing after an ESC can tell that the
    -- overlay it was asked for is gone.
  , "    function ask(title, commit, foot) {"
  , "      prompting = { choices: [], shown: [], at: 0, commit,"
  , "                    narrow: false, raising: true };"
  , "      el(\"phead\").textContent = title;"
  , "      el(\"pinput\").value = \"\";"
  , "      el(\"prompt\").className = \"on\";"
  , "      mode(\"\", foot);"
  , "      return prompting;"
  , "    }"
    -- LIST under its letters, drawn.  The one place the which-key pool is
    -- spent, so the rule a reader learns by heart has one implementation: the
    -- state palette hands over its table flattened in draw order, and the link
    -- palette its own flat list.  The letters are stamped IN PLACE, since the
    -- table's cells hold these very objects and a copy would leave them holding
    -- entries as they were before one was assigned.
    --
    -- An entry that came in with a key of its OWN (`fixed') is out of the pool
    -- and out of the assignment: `*empty*' answers to DEL, which is no letter,
    -- so the a-z namespace is spent on KEYWORDS alone and the cycle that used
    -- to lose one to the meta keeps it.
  , "    function offer(list) {"
  , "      const pool = list.filter((c) => !c.fixed);"
  , "      whichKeys(pool.map((c) => c.label)).forEach((cut, i) => {"
  , "        pool[i].cut = cut;"
  , "        pool[i].key = letterAt(pool[i].label, cut);"
  , "      });"
  , "      prompting.choices = list;"
  , "      prompting.shown = list;"
    -- A reader who pressed `/' and typed while an answer was out is narrowing
    -- an empty list; the fill lands in the mode it finds rather than throwing
    -- the typing away.
  , "      if (prompting.narrow) narrowTo(el(\"pinput\").value);"
  , "      else drawChoices();"
  , "    }"
    -- SOURCES as the palette holds them: the labels down the table's first
    -- column, and the flat ordered list everything else reads.  The flattening
    -- is the draw order — each source row's active cell and then its inactive
    -- one, `*empty*' last — so the letters are assigned ONCE over the whole
    -- table and a letter is the reader's wherever in it the keyword sits.  It is
    -- also the list `/' narrows, so both modes offer the same entries under the
    -- same names.
    --
    -- The letter is folded into each entry HERE, once, and the entry an
    -- OBJECT both halves hold: `table' keeps the cells and `choices' the flat
    -- list, and they are the same objects, so the drawing and the dispatch read
    -- one field of one thing rather than agreeing on a parallel array's indices
    -- — `shown' is narrowed and `choices' is not.  Which is also why the
    -- letters are stamped in place: a copy would leave the cells holding the
    -- entries as they were before one was assigned.
  , "    function setChoices(sources) {"
  , "      const flat = [];"
  , "      const held = (word) => {"
  , "        const c = { label: word, keyword: word, color: badgeColor(word) };"
  , "        flat.push(c);"
  , "        return c;"
  , "      };"
    -- Every source is drawn under the name it arrives under: `default',
    -- `system', `file' and a tag all read as they are, so this page holds no
    -- table of labels to keep in step with the resolver's names.
  , "      prompting.table = (sources || []).map((s) => ({"
  , "        source: s.source,"
  , "        cells: [s.active || [], s.inactive || []].map((ws) => ws.map(held)),"
  , "      }));"
  , "      prompting.meta = { label: EMPTY, keyword: null, meta: true,"
  , "                         fixed: true, key: \"DEL\", cut: -1 };"
  , "      flat.push(prompting.meta);"
  , "      offer(flat);"
  , "    }"
    -- The same overlay with no list in it: one line of text, typed and
    -- committed with RET.  It is the minibuffer the filter palette set the
    -- pattern for, and it is this one rather than a widget of its own because
    -- everything a prompt owes — the band it paints in, the blur on the way
    -- out, ESC through the keymap's `cancel' — is already here.  `text' is what
    -- the key listener reads to know there is nothing to narrow and nothing a
    -- letter would commit; the drawing reads it too and leaves the list empty,
    -- so this prompt carries no entries at all.
  , "    function askText(title, foot, initial, commit) {"
  , "      prompting = { commit, text: true, raising: true };"
  , "      el(\"phead\").textContent = title;"
  , "      el(\"pinput\").value = initial;"
  , "      el(\"prompt\").className = \"on\";"
  , "      mode(\"narrow\", foot);"
  , "      el(\"pinput\").focus();"
  , "    }"
    -- THE FIELD: the completing-read `/' falls back to over the state palette,
    -- and the whole of what `+' raises over the tags popup.  What it completes
    -- over is `wider' — a THUNK rather than a list, so it is current after a
    -- commit moved what it should offer — and a line matching nothing commits as
    -- written (`freely'), the charset wall that refuses garbage being the
    -- server's.
  , "    function fieldMode() {"
  , "      prompting.narrow = true;"
  , "      prompting.text = false;"
  , "      el(\"pinput\").value = \"\";"
  , "      if (prompting.wider) offer(prompting.wider());"
  , "      mode(\"narrow\", prompting.narrowFoot"
  , "        || \"RET sets it · C-n/C-p walks · ESC leaves\");"
  , "      el(\"pinput\").focus();"
  , "    }"
    -- The same overlay raised STRAIGHT into that field, with no letters behind
    -- it: `+' over the tags popup, whose list is the addable vocabulary and
    -- whose commit adds.  One widget for both doors into typing, so what a
    -- prompt looks like and how ESC leaves it are decided once.
    --
    -- The raising guard is CLEARED rather than left set.  It declines the
    -- keydown that OPENED a palette, which is only ever a problem for a key the
    -- DISPATCH raised — the press that reaches here came from another modal
    -- surface's own listener and has been handled already, so leaving the guard
    -- up would decline the next real key instead.
  , "    function askFrom(title, list, foot, commit) {"
  , "      const mine = ask(title, commit, foot);"
  , "      mine.raising = false;"
  , "      mine.wider = () => list;"
  , "      mine.narrowFoot = foot;"
  , "      fieldMode();"
  , "      return mine;"
  , "    }"
    -- The chrome the mode owns — the box's class, which is what shows the
    -- field, and the foot naming the keys the list cannot draw for itself.
    -- Written at the two transitions, so `drawChoices' stays a list renderer
    -- and a keystroke that narrows invalidates nothing outside the list.
  , "    function mode(cls, foot) {"
  , "      el(\"pbox\").className = cls;"
  , "      el(\"pfoot\").textContent = foot;"
  , "      drawChoices();"
  , "    }"
    -- Blurred as well as hidden: a focused field nobody can see would leave
    -- `typing()' true and swallow every key after it.
  , "    function unask() {"
  , "      prompting = null;"
  , "      el(\"prompt\").className = \"\";"
  , "      el(\"pinput\").blur();"
  , "    }"
    -- The palette is the RESOLUTION, drawn as the layered table it is: one row
    -- per source in precedence order, widest first, the source named down the
    -- first column and its keywords in the Active and Inactive cells.  What a
    -- reader learns from it is why — `TODO' under `default' and `READING' under
    -- `book' is the classify chain saying which scope answered.  The hairlines
    -- are the rows'
    -- own borders and the old active/done split is the two COLUMNS; `*empty*'
    -- keeps a spanning row of its own at the foot, in the muted italic every
    -- starred meta wears, since no scope declares taking a keyword off.
    --
    -- Three shapes, and the mode picks: the text prompt has no list at all, the
    -- fallback is the flat list under a cursor, and a table not back yet is the
    -- line saying so.
  , "    function drawChoices() {"
  , "      const list = el(\"plist\");"
  , "      list.textContent = \"\";"
  , "      if (prompting.text) return;"
  , "      if (prompting.narrow) {"
  , "        prompting.shown.forEach((c, i) => entry(list, \"pe\""
  , "          + (c.meta ? \" pm\" : \"\") + (i === prompting.at ? \" pat\" : \"\"), c));"
  , "        return;"
  , "      }"
    -- A list that is empty in LETTER mode is a resolution that has not landed:
    -- the overlay goes up on the keydown and the answer fills it, and this is
    -- the line a reader sees until it does.
  , "      if (!prompting.choices.length) {"
  , "        part(list, \"div\", \"pnone\", \"resolving…\");"
  , "        return;"
  , "      }"
    -- Past those two the list IS the resolution table: letter mode is the state
    -- palette's alone now, and `setChoices' is the only thing that fills one.
  , "      const head = part(list, \"div\", \"pr ph\");"
  , "      part(head, \"div\", \"ps\", \"source\");"
  , "      part(head, \"div\", \"pc\", \"active\");"
  , "      part(head, \"div\", \"pc\", \"inactive\");"
  , "      prompting.table.forEach((src) => {"
  , "        const row = part(list, \"div\", \"pr\");"
  , "        part(row, \"div\", \"ps\", src.source);"
  , "        src.cells.forEach((cell) => {"
  , "          const box = part(row, \"div\", \"pc\");"
  , "          cell.forEach((c) => entry(box, \"pe\", c));"
  , "        });"
  , "      });"
  , "      entry(part(list, \"div\", \"pr pm\"), \"pe\", prompting.meta);"
  , "    }"
    -- One entry: the key token, then the keyword in its badge colour with the
    -- claimed letter BOLD where it sits.  The token column goes in the fallback
    -- mode, and the bolding with it: no letter commits there, and drawing one
    -- would be a lie about what typing it does.
  , "    function entry(into, cls, c) {"
  , "      const row = part(into, \"div\", cls);"
    -- The letter is marked IN the word, so there is no token beside it — and
    -- one exception: a FIXED key names no position in a word (`*empty*' answers
    -- to DEL), so that entry alone keeps a token to be told by.  In the
    -- fallback mode nothing commits by key at all, so nothing is marked.
  , "      const marked = !prompting.narrow && c.cut >= 0;"
  , "      if (!prompting.narrow && c.fixed) part(row, \"span\", \"pk\", c.key);"
  , "      const word = part(row, \"span\", \"pw\");"
  , "      if (c.color) word.style.color = c.color;"
  , "      if (!marked) word.textContent = c.label;"
  , "      else {"
  , "        part(word, \"span\", \"\", c.label.slice(0, c.cut));"
    -- The rule under the letter takes the keyword's own hue, which only the
    -- entry knows; the weight and the thickness are the stylesheet's.
  , "        const hot = part(word, \"b\", \"\", c.label[c.cut]);"
  , "        if (c.color) hot.style.textDecorationColor = c.color;"
  , "        part(word, \"span\", \"\", c.label.slice(c.cut + 1));"
  , "      }"
    -- The muted aside beside an entry: the tag palette's partial count (`2/3'),
    -- which is what says a tag only some of the set carries.  A keyword has
    -- none, and neither has a levelled tag.
  , "      if (c.hint) part(row, \"span\", \"pt\", c.hint);"
  , "    }"
  , "    function narrowTo(text) {"
  , "      const want = text.trim().toLowerCase();"
    -- Over the LABEL alone.  The aside is drawn rather than searched: the add
    -- field writes a coverage count into it and a digit must not narrow the
    -- list to the entries that happen to be 2-of-3.
  , "      prompting.shown = prompting.choices.filter((c) =>"
  , "        c.label.toLowerCase().includes(want));"
  , "      prompting.at = 0;"
  , "      drawChoices();"
  , "    }"
  , "    function walkChoices(step) {"
  , "      const n = prompting.shown.length;"
  , "      if (n) prompting.at = Math.max(0, Math.min(n - 1, prompting.at + step));"
  , "      drawChoices();"
  , "    }"
    -- ONE COMMIT for every mode: a letter, and the field's own RET.  The overlay
    -- comes down FIRST, so what the commit sees is a page with no prompt on it —
    -- a palette that stayed open over its own write is the shape the tags
    -- palette took, and that list is a mount now.
  , "    function takeChoice(chosen) {"
  , "      if (!chosen) return;"
  , "      const act = prompting.commit;"
  , "      unask();"
  , "      act(chosen);"
  , "    }"
    -- The typed line as an entry, for a field whose typing REACHES PAST its
    -- list — which is what `wider' says: a tag the tree has never held has to be
    -- committable, since that is the only way a first one is ever written.
  , "    const freely = () => {"
  , "      if (!prompting.wider) return null;"
  , "      const typed = el(\"pinput\").value.trim();"
  , "      return typed ? { tag: typed } : null;"
  , "    };"
    -- The two fields that hold a LINE rather than a filter narrow nothing: the
    -- text prompt has no list, and `+'\''s field is a name being written rather
    -- than one being looked for.
  , "    el(\"pinput\").addEventListener(\"input\", (e) =>"
  , "      prompting && !prompting.text && narrowTo(e.target.value));"
  , "    el(\"prompt\").addEventListener(\"click\", (e) =>"
  , "      { if (e.target === el(\"prompt\")) unask(); });"
    -- What C-c C-t offers: the states the SERVER says those rows may be set to,
    -- with the scope that declares each — org's own cycle under `default',
    -- `system.org', the row's tags' configs, its file's own `#+TODO:' — plus the
    -- entry that takes a keyword off.  Resolved per request because the
    -- answer is per ROW: the state column's badges are the union of every file
    -- loaded, which is a superset and says nothing about where a keyword came
    -- from.  The column's `values' are the filter's group meta-values
    -- (`*active*') and are absent from both — no file declares one, so the
    -- server refuses every one of them, and offering a value that cannot be set
    -- is worse than not offering it.
    -- `*empty*' wears the stars every reserved meta wears (docs/invariants.md),
    -- and it is the filter's own word for a cell holding nothing — the entry
    -- takes the state column's cell to exactly what `state:*empty*' then finds.
    -- The starred form is the page's mark for a value with semantics rather than
    -- a word a file could hold, and the server refuses a starred string as a
    -- keyword from the other side.  What it commits is a null keyword, and the
    -- key it answers to is DEL — a key that already MEANS take-it-off wherever
    -- this page binds one, and no letter, so the a-z pool is spent on KEYWORDS
    -- alone.  A cycle wide enough to run the pool dry keeps the letter the meta
    -- used to take.  In the typing mode DEL is the field's own and `*empty*' is
    -- reached the way every other entry is, by narrowing to it.
  , "    const EMPTY = \"*empty*\";"
    -- The colour is the badge's own, so a keyword reads in the palette as it
    -- reads in the table.  Looked up rather than carried: the resolution names
    -- keywords, and the hues are the producer's and ride on the state column
    -- where every other reader of them finds them.
  , "    const badgeColor = (keyword) =>"
  , "      (((cols.find((c) => c.key === \"state\") || {}).badges || [])"
  , "        .find((b) => b.value === keyword) || {}).color || \"\";"
    -- ONE parameter per id rather than the comma list a caller types by hand:
    -- the fallback row id is a path and a comma in one would split it, and
    -- percent-encoding cannot help — the server splits after decoding.
  , "    const keywordSources = (ids) =>"
  , "      getJSON(\"/keywords?\""
  , "        + ids.map((i) => \"ids=\" + encodeURIComponent(i)).join(\"&\"));"
    -- Where a row points, out of the server's reading of its subtree.  This
    -- page holds no org parser, so the bracket grammar stays where the display
    -- rule already lives — one link is `[[TARGET][DESC]]' shown as DESC, and a
    -- bare URL is its own description.
  , "    const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);"
    -- What the rows a tag command names are tagged with, and what else the tree
    -- holds.  Per row rather than as a union, because WHICH rows lack a tag is
    -- what decides where an add is sent; the union and its partial counts are
    -- worked out here, off that.  One parameter per id, for `/keywords'' reason.
  , "    const tagsOf = (ids) =>"
  , "      getJSON(\"/tags?\""
  , "        + ids.map((i) => \"ids=\" + encodeURIComponent(i)).join(\"&\"));"
    -- What a browser tab can be pointed at, which is http(s) and nothing else.
    -- Org writes plenty of other link types and `/links' reports them all —
    -- `mailto:', `file:', org's `id:', org-glance's own protocols, a bare
    -- `[[Title]]' naming a headline — and each names something a tab is not.
    -- Following one needs a handler this page does not have yet, so it says so
    -- instead of opening a tab on a string a browser will make nothing of.
    --
    -- The TYPE decides, which is the server's own word for the target
    -- (`Glance.Query.linkType'): the scheme, folded.  A regex over the target
    -- here would be this page deriving a second time what the answer already
    -- carries, and the popup's badge column and this test would then be two
    -- readings of one string.  The LIST is the server's too
    -- (`Glance.Query.followableTypes'), spliced the way `PLANNING' is: it is the
    -- same list the badge palette gives the warm hues to, so what reads as
    -- followable and what opens cannot come apart.
  , "    const FOLLOWABLE = " <> jsonList followableTypes <> ";"
  , "    const followable = (l) => FOLLOWABLE.indexOf(l.type) !== -1;"
    -- A target in a log line, kept to a width the strip can show: an org link
    -- target runs to a hash and a path, and the line has other words in it.
  , "    const shortly = (t) => {"
  , "      const s = String(t || \"\");"
  , "      return s.length > 80 ? s.slice(0, 79) + \"…\" : s;"
  , "    };"
    -- One tab, and the log keeps what was followed: a link opened is the one
    -- thing a key here does that leaves no trace on the page it was pressed on.
    -- `noopener' because the opened page must not reach back into this one.
    --
    -- The COMMIT is where a link type is judged, which is why this is one
    -- function and not a filter over the rows: the popup lists everything the
    -- row points at, since that is what teaches a reader what is in the entry,
    -- and a single link takes this same door without a popup at all.  So `o' on
    -- a row holding one `mailto:' warns and opens nothing.
  , "    function openLink(b, link) {"
  , "      if (!followable(link)) {"
  , "        said(b, \"link type not implemented\");"
  , "        append(\"cmd\", \"warn\","
      <> " `link type not implemented: ${shortly(link.target)}`);"
  , "        return;"
  , "      }"
  , "      window.open(link.target, \"_blank\", \"noopener\");"
    -- The description, and never a fallback to the target: `/links' has already
    -- applied the display rule, so a bare URL arrives described by itself and an
    -- empty `desc' is a shape the route cannot produce.
  , "      said(b, link.desc);"
  , "      append(\"cmd\", \"info\", `link ${JSON.stringify(link.target)} opened`);"
  , "    }"
    -- THE LINK POPUP, and it is a table-view MOUNT — the page's third, after the
    -- table and the sheet's property panel.  What a row points at is a LIST OF
    -- RECORDS rather than a set of commands: each link has a kind, a name and a
    -- destination, and reading them is how a reader decides which one they
    -- meant.  A which-key letter is the right shape for a fixed vocabulary you
    -- commit from memory (a keyword, a tag); it is the wrong one for a list you
    -- have to READ, where the letters are noise over the columns that carry the
    -- answer.  So this one browses: move, look, RET.
    --
    -- READ-ONLY, and stated rather than inherited: no marks, no flags, no
    -- pageSize and no hint line.  Nothing here writes, so a gutter, a wash and a
    -- per-row hint would each be chrome about a gesture the popup does not have.
    -- `palette: true' for the property panel's reason — a handful of links is
    -- not something a reader narrows, and the filter overlay it leaves behind is
    -- never raised.
    --
    -- Mounted once and kept, like the panel: a mount per press would leave a
    -- theme listener behind every time the reader followed a row.
  , "    const LCOLS = " <> jsonLiteral (toJSON linkColumns) <> ";"
    -- `opening' is the BINDING that raised this — `o' or `!' — and it is also
    -- WHETHER the popup is up: a raise sets it, `shutLinks' clears it, and one
    -- value answers both questions rather than two that have to move in step.
    -- It is what lets the pill name the command that ran when the commit finally
    -- lands, the way it does for a link the row held only one of.
  , "    let lmount = null, lrows = [], opening = null;"
  , "    const linking = () => !!opening;"
  , "    function linksMounted() {"
  , "      if (lmount) return lmount;"
  , "      lmount = TableView.mount(el(\"ltable\"), { columns: LCOLS, rows: [] },"
  , "        { palette: true, marks: false, flags: false, actionHints: false });"
  , "      return lmount;"
  , "    }"
    -- The answer as rows, under B — the binding that asked.  The id is the
    -- link's PLACE in the answer, which is the only identity a link has here:
    -- two entries may share a description, and the targets are deduplicated
    -- upstream.  The model keeps it beside the link, so finding the link the
    -- cursor is on is a lookup by id like the property panel's rather than this
    -- page taking an id apart.
  , "    function showLinks(b, links) {"
  , "      lrows = links.map((l, i) => ({ id: `L${i}`, link: l }));"
  , "      const m = linksMounted();"
  , "      m.setRows(lrows.map((r) => ({ id: r.id,"
  , "        cells: { type: r.link.type, title: r.link.desc, url: r.link.target } })));"
  , "      el(\"lhead\").textContent = `open · ${links.length} links`;"
  , "      el(\"lfoot\").textContent = \"o opens it · ESC leaves\";"
  , "      el(\"links\").className = \"on\";"
  , "      opening = b;"
  , "      if (lrows.length) m.select(lrows[0].id);"
  , "    }"
    -- Nothing to blur: the popup holds the keys with no field in it, the way
    -- the property panel's nav does, so closing is the class coming off and the
    -- value `linking()' reads going null.
  , "    function shutLinks() {"
  , "      opening = null;"
  , "      el(\"links\").className = \"\";"
  , "    }"
    -- The link the cursor is on, out of the renderer's own selection — this
    -- page keeps no copy of where the popup is standing, the same rule the
    -- table and the panel follow.
  , "    function pointedLink() {"
  , "      if (!can(lmount, \"getSelection\")) return null;"
  , "      const at = (lmount.getSelection() || {}).id;"
  , "      return (lrows.find((r) => r.id === at) || {}).link || null;"
  , "    }"
    -- THE TAGS POPUP, the page's FOURTH table-view mount and the first one that
    -- WRITES.  What a set of rows is tagged with is a list of RECORDS — a name,
    -- a coverage over the set, a weight in the tree — and a reader deciding
    -- whether to drop one is READING those three.  That is the link popup's
    -- shape rather than the value palette's, so the which-key letters that used
    -- to carry this list went with the list: a letter is right for a fixed
    -- vocabulary committed from memory, which is what a KEYWORD is and what a
    -- tag over a set of rows is not.
    --
    -- MUTABLE, which is the whole of what makes it a fourth mount rather than a
    -- second link popup.  `d'/`D' remove, `+' adds, `RET' renames — three
    -- gestures this page already spells elsewhere, borrowed rather than
    -- invented: dired's flag-then-confirm from the table and the property panel,
    -- the value palette's completing field, and the panel's edit overlay.
    --
    -- Marks are OFF: the set a tag command runs over is the TABLE's and was
    -- decided before this went up, so a second selection here would be a second
    -- answer to a settled question.  Flags are ON, since the removal is the
    -- two-press gesture and the flag is its confirmation.
  , "    const TCOLS = " <> jsonLiteral (toJSON tagColumns) <> ";"
    -- The popup's whole state: the target rows as the server described them
    -- (each with its own folded tag list), the tree's vocabulary and its
    -- store-wide row counts, the rows the mount is showing, and the binding that
    -- raised this — which is also WHETHER it is up, the way `opening' is for the
    -- link popup.
  , "    let tmount = null, trows = [], ttargets = [], tvocab = [], tcount = {};"
  , "    let tagging = null;"
  , "    const managing = () => !!tagging;"
    -- Mounted once and kept, like the panel and the link popup: a mount per
    -- press would leave a theme listener behind every time a reader tagged.
  , "    function tagsMounted() {"
  , "      if (tmount) return tmount;"
  , "      tmount = TableView.mount(el(\"ttable\"), { columns: TCOLS, rows: [] },"
  , "        { palette: true, marks: false, flags: true, actionHints: false,"
  , "          flagHelp: \"d/D remove · u unflag\" });"
      -- The rename overlay is anchored to the row it opened over, so everything
      -- that can move that row's box has to say so — the property panel's two
      -- listeners, for the property panel's reasons.
  , "      el(\"tpane\").addEventListener(\"scroll\", placeTag, true);"
  , "      window.addEventListener(\"resize\", placeTag);"
  , "      return tmount;"
  , "    }"
    -- THE UNION over the target rows, FIRST-SEEN: each row's tags in the order
    -- its file spells them, the rows in the order the server named them.
    -- Alphabetical would be no harder and strictly worse — the cursor sits on a
    -- row, and an insert in the middle moves the row out from under it, where an
    -- append cannot.
  , "    function tagUnion() {"
  , "      const seen = [];"
  , "      for (const r of ttargets) for (const t of r.tags)"
  , "        if (seen.indexOf(t) === -1) seen.push(t);"
  , "      return seen;"
  , "    }"
    -- Which of the targets carry TAG, which is what every write here is aimed
    -- at: a removal goes to the rows carrying it, an add to the rows lacking it,
    -- so what an answer counts is rows that MOVED.
  , "    const carriers = (tag) => ttargets.filter((r) => r.tags.indexOf(tag) !== -1);"
    -- COVERAGE: `all' where every target carries it, `k/n' where some do.  It is
    -- what the letter palette wrote into a muted aside, promoted to a column of
    -- its own now that there is a table to put it in.
  , "    const coverage = (tag) => {"
  , "      const on = carriers(tag).length;"
  , "      return on === ttargets.length ? \"all\" : `${on}/${ttargets.length}`;"
  , "    };"
    -- The rows as the mount holds them, and a tag IS its own id: one entry per
    -- tag per popup, so a flag, the cursor and a rename all name the same thing
    -- after any number of writes.
  , "    const tagRows = () => tagUnion().map((tag) =>"
  , "      ({ id: tag, cells: { title: tag, on: coverage(tag),"
  , "                           rows: tcount[tag] === undefined ? \"\" : tcount[tag] } }));"
    -- Every change to the model ends here.  AT is the tag to land the cursor on
    -- and is left out where it should stay where it is.
  , "    function repaintTags(at) {"
  , "      const m = tagsMounted();"
  , "      trows = tagRows();"
  , "      m.setRows(trows);"
      -- The foot names what a reader can do, and a popup with nothing in it
      -- names the one key that still can: an untagged set is honest rather than
      -- empty, and `+' is the way in.
  , "      el(\"tfoot\").textContent = trows.length"
  , "        ? \"RET renames · d flags · D removes · + adds · ESC leaves\""
  , "        : \"nothing tagged here · + adds one · ESC leaves\";"
  , "      if (at && trows.some((r) => r.id === at)) m.select(at);"
  , "    }"
    -- Raised on the ANSWER, like the link popup and unlike the state palette:
    -- no key inside this list is also the key that opens it, so nothing is
    -- gained by putting an empty mount up first and no raising guard is owed.
    -- TITLE is the count of the ids the command was aimed at, which is what the
    -- reader asked for; the coverage denominator is the rows the store actually
    -- answered for.
  , "    function showTags(b, title, answer) {"
  , "      ttargets = (answer.rows || []).map((r) =>"
  , "        ({ id: r.id, tags: (r.tags || []).slice() }));"
  , "      tvocab = answer.vocabulary || [];"
  , "      tcount = answer.counts || {};"
  , "      tagging = b;"
      -- Written ONCE, here: the title is the count of the ids the command was
      -- aimed at and cannot move while the popup is up, so a repaint has no
      -- business restating it.
  , "      el(\"thead\").textContent = title;"
  , "      el(\"tags\").className = \"on\";"
  , "      repaintTags(tagUnion()[0]);"
  , "    }"
    -- Nothing to blur once the rename is shut: the popup holds the keys with no
    -- field in it, the way the link popup and the property panel's nav do.
  , "    function shutTags() {"
  , "      shutRename();"
  , "      tagging = null;"
  , "      el(\"tags\").className = \"\";"
  , "      ttargets = []; trows = [];"
  , "    }"
    -- The tag the cursor is on, out of the renderer's own selection — this page
    -- keeps no copy of where the popup is standing, the same rule the table, the
    -- panel and the link popup follow.
  , "    const tagAt = () => {"
  , "      if (!can(tmount, \"getSelection\")) return null;"
  , "      const at = (tmount.getSelection() || {}).id;"
  , "      return trows.some((r) => r.id === at) ? at : null;"
  , "    };"
    -- THE ADDABLE VOCABULARY, which is what `+' completes over: every tag this
    -- tree holds LESS the ones already on every target.  Adding one of those
    -- writes nothing, so offering it is offering a no-op — where one only SOME
    -- of them carry stays offered, wearing its `2/3', since adding it levels the
    -- set up.  The set's own partial tags lead and the rest of the TREE follows:
    -- the rows a page shows are a fraction of the store, so the vocabulary is
    -- the server's answer rather than a scan of what is in hand.
  , "    function addable() {"
  , "      const union = tagUnion();"
      -- `all' is exactly the coverage of a tag every target carries, so the
      -- filter reads that cell rather than counting the carriers a second time.
  , "      return union.map((t) => ({ label: t, tag: t, hint: coverage(t) }))"
  , "        .filter((c) => c.hint !== \"all\")"
  , "        .concat(tvocab.filter((t) => union.indexOf(t) === -1)"
  , "          .map((t) => ({ label: t, tag: t, hint: \"\" })));"
  , "    }"
    -- The ids one answer landed on, which is where every fold below starts.
    -- What the list shows next comes out of the command's OWN per-id answer,
    -- never a re-read: `/command' does not write the store — the watch does, a
    -- debounce later — so asking `/tags' again here would answer with what the
    -- files said BEFORE the write.
  , "    const landedIds = (results) =>"
  , "      new Set((results || []).filter((x) => x.ok).map((x) => x.id));"
    -- And the store-wide count stepped by what landed.  Arithmetic on the answer
    -- for the same reason the tag sets are, and corrected by the next
    -- resolution: the number is the tree's and only the tree can be right about
    -- it, but a column standing still while the rows under it moved would read
    -- as a stale answer rather than as a different question.
  , "    const stepCount = (tag, by) =>"
  , "      (tcount[tag] = Math.max(0, (tcount[tag] || 0) + by));"
    -- `+' — the add flow, one field over the addable vocabulary and the only
    -- door into it.  It only ever ADDS, so the write goes to the rows LACKING
    -- the tag; a tag every target already carries costs a line in the pill and
    -- no round trip.  FOLDED at commit, because presence is: `/tags' reports
    -- what `tagsOfCell' reads, and a popup that wrote `Work' would go on showing
    -- `work' and offering to add it again.
  , "    const addFlow = () => askFrom(`add a tag · ${rowsWord(ttargets.length)}`,"
  , "      addable(), \"RET adds it · C-n/C-p walks · ESC leaves\", addTag);"
  , "    function addTag(c) {"
  , "      const tag = String(c.tag || \"\").trim().toLowerCase();"
  , "      if (!managing() || !tag) return;"
  , "      const over = ttargets.filter((r) => r.tags.indexOf(tag) === -1);"
  , "      if (!over.length) { said(tagging, `:${tag}: is on every row already`); return; }"
  , "      fire(tagging, \"add-tag\", over.map((r) => r.id), { tag },"
  , "           `tagged :${tag}:`).then((results) => {"
  , "        if (!managing()) return;"
  , "        const landed = landedIds(results);"
  , "        for (const r of ttargets)"
  , "          if (landed.has(r.id) && r.tags.indexOf(tag) === -1) r.tags.push(tag);"
      -- A tag written for the first time joins the tree's vocabulary here, so
      -- the field offers it before the watch has told this page anything.
  , "        if (landed.size && tvocab.indexOf(tag) === -1) tvocab.push(tag);"
  , "        stepCount(tag, landed.size);"
  , "        repaintTags(tag);"
  , "      });"
  , "    }"
    -- `D', and the second `d' that reaches the same handler: every FLAGGED tag
    -- comes off every target carrying it.  ONE COMMAND PER TAG, since a command
    -- names one — each its own per-file batch of atomic writes — and the flags
    -- are SPENT before the first goes out, the way the table's archive flags
    -- are: a set left standing would be removed again by the next press.
  , "    function removeTags(list) {"
  , "      tmount.clearFlags();"
  , "      for (const tag of list) untag(tag);"
  , "    }"
  , "    function untag(tag) {"
  , "      const over = carriers(tag);"
  , "      if (!over.length) return;"
  , "      fire(tagging, \"remove-tag\", over.map((r) => r.id), { tag },"
  , "           `untagged :${tag}:`).then((results) => {"
  , "        if (!managing()) return;"
  , "        const landed = landedIds(results);"
  , "        for (const r of ttargets)"
  , "          if (landed.has(r.id)) r.tags = r.tags.filter((t) => t !== tag);"
  , "        stepCount(tag, -landed.size);"
  , "        repaintTags();"
  , "      });"
  , "    }"
    -- `RET' — the rename, and it is ONE command.  `rename-tag' replaces the
    -- entry where the author put it, under one drift lock per file; a remove and
    -- an add fired in turn would be two writes, two locks, and a tag that moved
    -- to the end of every run it was in.  It goes to the targets carrying FROM,
    -- which is the set the write is for.
  , "    function renameTag(from, typed) {"
  , "      const to = String(typed || \"\").trim().toLowerCase();"
  , "      shutRename();"
  , "      if (!from || !to || to === from) { said(tagging, \"unchanged\"); return; }"
  , "      const over = carriers(from);"
  , "      fire(tagging, \"rename-tag\", over.map((r) => r.id), { from, to },"
  , "           `renamed :${from}:→:${to}:`).then((results) => {"
  , "        if (!managing()) return;"
  , "        const landed = landedIds(results);"
      -- A row carrying BOTH ends loses `from' and gains nothing — the server
      -- cuts rather than renames there — so counting it would leave the tree
      -- count one high for as long as the popup stands.
  , "        const gained = ttargets.filter((r) =>"
  , "          landed.has(r.id) && r.tags.indexOf(to) === -1).length;"
  , "        for (const r of ttargets)"
  , "          if (landed.has(r.id)) r.tags = renamedTags(r.tags, from, to);"
  , "        if (landed.size && tvocab.indexOf(to) === -1) tvocab.push(to);"
  , "        stepCount(to, gained);"
  , "        stepCount(from, -landed.size);"
  , "        repaintTags(to);"
  , "      });"
  , "    }"
    -- One row's tags after the rename, IN PLACE and deduplicated — the server's
    -- own rule ('Glance.Query.renameTagEdits'): the entry stays where it was, so
    -- the union's first-seen order does not shuffle under the cursor, and a row
    -- that carried both ends comes out carrying one.
  , "    const renamedTags = (tags, from, to) =>"
  , "      [...new Set(tags.map((t) => (t === from ? to : t)))];"
    -- THE RENAME OVERLAY, which is the property panel's edit model over one
    -- cell: the tag cell becomes a field over itself, `RET' commits and `ESC'
    -- restores.  The other two columns are DERIVED — a coverage and a count — so
    -- there is nothing in them to edit and they never open, exactly as the link
    -- popup's type cell would not.
  , "    const renaming = () => el(\"tedit\").className === \"on\";"
      -- The tag the overlay OPENED on, kept for the commit: no key can move the
      -- cursor while a row is open, but a mouse click can, and RET would then
      -- rename the tag the reader landed on with the text typed for another.
  , "    let renamingFrom = null;"
  , "    function openRename() {"
  , "      const at = tagAt();"
  , "      if (!at) { echo(\"RET → org-rename-tag (no tag)\"); return; }"
  , "      renamingFrom = at;"
  , "      el(\"tedit\").className = \"on\";"
  , "      el(\"tname\").value = at;"
      -- The renderer stamps `tv-sel' on its own frame, so a row selected in THIS
      -- tick has no marked element yet; one frame later there is one.
  , "      soon(placeTag);"
  , "      el(\"tname\").focus();"
  , "      el(\"tname\").select();"
  , "    }"
  , "    function shutRename() {"
  , "      el(\"tedit\").className = \"\";"
  , "      el(\"tname\").blur();"
  , "    }"
  , "    function cancelRename() {"
  , "      shutRename();"
  , "      echo(\"ESC → keyboard-quit (tag unchanged)\");"
  , "    }"
    -- Where the overlay sits: over the TAG CELL of the row the renderer has
    -- selected — the row's box for the vertical, the cell's for the horizontal.
    -- The row comes through the mount's published root, which is the one door
    -- this page reads a mount's DOM through; a page with no layout (the suite's)
    -- finds no row and leaves the overlay where it was put.
    --
    -- The cell is the first that is not the GUTTER, which `flags: true' puts
    -- there: naming a column index would be this page counting the renderer's
    -- chrome, where the class it already stamps says which cell that is.
  , "    function placeTag() {"
  , "      if (!renaming()) return;"
  , "      const tr = tmount.el.querySelector(\"tbody tr.tv-sel\");"
  , "      const td = tr && tr.querySelector(\"td:not(.tv-box)\");"
  , "      if (!td) return;"
  , "      const a = tr.getBoundingClientRect(), c = td.getBoundingClientRect();"
  , "      const b = el(\"tpane\").getBoundingClientRect();"
  , "      el(\"tedit\").style.top = `${a.top - b.top}px`;"
  , "      el(\"tedit\").style.height = `${a.height}px`;"
  , "      el(\"tedit\").style.left = `${c.left - b.left}px`;"
  , "      el(\"tedit\").style.width = `${c.width}px`;"
  , "    }"
    -- dired's `d', over the popup's rows: the first press flags the tag at
    -- point, a second `d' on an already-flagged one IS `D' — it calls the same
    -- handler, so it removes EVERY flagged tag rather than the one under it —
    -- and `u' takes a flag off and walks on.  The flag is the confirmation, so
    -- there is no prompt.  A lone flag is a set of one, which is what leaves the
    -- single-tag flow unchanged.
  , "    function tflag(k) {"
  , "      if (!flagsOn(tmount))"
  , "        { echo(`${k} → this table-view.js has no delete flags`); return; }"
  , "      const at = tagAt();"
  , "      if (!at) { echo(`${k} → org-toggle-tag (no tag)`); return; }"
  , "      const flags = tmount.getFlagged();"
  , "      if (k === \"D\" || (k === \"d\" && flags.indexOf(at) !== -1)) {"
  , "        removeTags(flags.length ? flags : [at]);"
  , "        return;"
  , "      }"
  , "      if (k === \"u\") {"
  , "        tmount.unflagRow(at);"
  , "        echo(\"u → tag-unflag (flag cleared)\");"
  , "        stepIn(tmount, 1);"
  , "        return;"
  , "      }"
  , "      tmount.flagRow(at);"
  , "      echo(\"d → tag-flag (d again removes)\");"
  , "    }"
    -- Settings, in PANELS.  The general preferences, the theme, then one box
    -- per keyword layer — a layer being one config file and its `#+TODO:' lines
    -- VERBATIM.  The line is the contract org itself reads, so it is what is
    -- edited: a chip UI here would be this page guessing at a grammar it has no
    -- parser for, and the guess would be what gets written.
    --
    -- The sheet is the materialize sheet's pattern, down to the words: no
    -- buttons, ESC or the backdrop syncs and closes, `C-x C-s' syncs mid-edit,
    -- and the header carries one of the same four states.  What it does NOT
    -- share is a request: `/config' is its own pair of routes, and the rows
    -- arrive the way every other write's do — the file watch sees the config
    -- change and reseeds the tree.  The theme panel asks nobody: it is a
    -- `localStorage' preference and applies as it is picked.
    --
    -- ONE STRUCTURE for the panels: a header and the elements under it, in
    -- order.  A panel joins by adding an entry and the markup it names — the
    -- bodies wear `cpart' so the stylesheet needs no entry of its own — and
    -- native tabbing walks the sheet in exactly this order.
    --
    -- The list wraps markup rather than building it because the bodies are
    -- heterogeneous — two labelled inputs, a select, a list the server fills —
    -- and a builder for that shape would be a template language this page has
    -- no use for.  So `SECTIONS' owns the headers and the order, and the markup
    -- owns what is under them, joined by id.  A `parts' id the markup does not
    -- carry throws here, at boot, which is where a join like that should fail.
  , "    const SECTIONS = ["
  , "      { title: \"general\", parts: [\"cgen\"] },"
  , "      { title: \"theme\", parts: [\"ctheme\"] },"
  , "      { title: \"keywords\", parts: [\"clayers\", \"ceff\", \"cfoot\"] },"
  , "    ];"
  , "    const csecs = el(\"csecs\");"
  , "    for (const s of SECTIONS) {"
  , "      const sec = part(csecs, \"div\", \"csec\");"
  , "      part(sec, \"div\", \"chdr\", s.title);"
  , "      for (const id of s.parts) sec.appendChild(el(id));"
  , "    }"
  , "    let settings = false, crows = [];"
    -- The settings sheet's half of the pair the ladder drives ('subtreeSheet'
    -- is the other): the same four verbs, over the config layers and their own
    -- digests, filed under its own log scope.
  , "    const configSheet = {"
  , "      noteId: \"cnote\", scope: \"config\", state: \"synced\","
  , "      closed: \"settings closed — the files are as they were\","
  , "      dirty: () => cdirty(),"
  , "      flush: () => flushConfig(),"
  , "      refresh: () => config().then((b) => {"
  , "        for (const r of crows) {"
  , "          const fresh = (b.layers || []).find((l) => l.path === r.path);"
  , "          if (fresh) r.digest = fresh.digest;"
  , "        }"
  , "        return true;"
  , "      }),"
  , "      shut: () => shutSettings(),"
  , "    };"
    -- Claimed before the fetch, and refused over the other sheet.  `typing()'
    -- is not enough to keep the two apart: clicking the materialize sheet's own
    -- header blurs its textarea, and a `table' row is live again the moment it
    -- does — so the rule is stated here rather than left to the focus.
  , "    function openSettings() {"
  , "      if (activeSheet()) return;"
  , "      settings = true;"
  , "      config().then((b) => {"
  , "        if (!settings) return;   // an ESC arrived while the layers were out"
  , "        drawLayers(b);"
  , "        cnote(\"synced\");"
  , "        el(\"config\").className = \"on\";"
    -- The top of the sheet, which is the general panel's first field: the sheet
    -- opens where a reader reads it from, and Tab walks down from there.
  , "        el(\"cfilter\").focus();"
  , "      }).catch((e) => {"
  , "        settings = false;"
  , "        append(\"config\", \"error\", `settings failed: ${e.message}`);"
  , "      });"
  , "    }"
  , "    const config = () => getJSON(\"/config\");"
  , "    function drawLayers(b) {"
  , "      el(\"clayers\").textContent = \"\";"
  , "      crows = (b.layers || []).map(layerRow);"
    -- The general panel's two fields are `system.org''s two tree-wide LINES, so
    -- they are bound to the system layer's row and go out in its write: one
    -- file, one digest, one splice, wherever on the sheet they are drawn.  The
    -- FIRST system layer, which `/config' always serves and always leads with —
    -- a tree with none is a server that broke its own contract, and the throw
    -- lands in `openSettings''s catch as a settings failure rather than as a
    -- sheet that silently drops what a reader types into these two.
  , "      const view = el(\"cfilter\"), cap = el(\"ctarget\");"
  , "      view.value = b.filter || \"\"; cap.value = b.capture || \"\";"
  , "      const sys = crows.find((r) => r.tag === null);"
  , "      sys.view = view; sys.viewBase = view.value;"
  , "      sys.cap = cap; sys.capBase = cap.value;"
  , "      const kw = b.keywords || {};"
  , "      el(\"ceff\").textContent ="
  , "        `${(kw.active || []).join(\" \")} | ${(kw.inactive || []).join(\" \")}`;"
  , "    }"
    -- What one layer shows: which scope it is and where it lives, its lines,
    -- and a line for whatever the server said about the last write to it.  A
    -- layer with no digest is not a file yet — saying so is what makes creating
    -- the first one an edit rather than a mystery.
  , "    function layerRow(layer) {"
  , "      const row = document.createElement(\"div\");"
  , "      row.className = \"crow\";"
  , "      const lab = document.createElement(\"div\");"
  , "      lab.className = \"clab\";"
  , "      lab.textContent = `${layer.tag ? `tag · ${layer.tag}` : \"system\"} · ${layer.path}`"
  , "        + (layer.digest ? \"\" : \" · not created yet\");"
  , "      const box = document.createElement(\"textarea\");"
  , "      box.className = \"ctext\";"
  , "      box.spellcheck = false;"
  , "      box.value = (layer.lines || []).join(\"\\n\");"
  , "      box.placeholder = \"#+TODO: TODO STARTED | DONE\";"
  , "      const note = document.createElement(\"div\");"
  , "      note.className = \"cerr\";"
  , "      row.appendChild(lab); row.appendChild(box); row.appendChild(note);"
    -- The two tree-wide fields are the general panel's and are bound to the
    -- system layer by `drawLayers'; every layer's row carries the slots so one
    -- shape answers `cmoved' and one shape is posted.
  , "      const r = { path: layer.path, tag: layer.tag, digest: layer.digest,"
  , "                  base: box.value, box, note,"
  , "                  view: null, viewBase: null, cap: null, capBase: null };"
  , "      el(\"clayers\").appendChild(row);"
  , "      return r;"
  , "    }"
    -- The same four words the other sheet wears, through the same writer.
  , "    const cnote = (next, message) => note(configSheet, next, message);"
  , "    const cdirty = () => crows.some(cmoved);"
  , "    const cmoved = (r) => r.box.value !== r.base"
  , "      || (r.view !== null && r.view.value !== r.viewBase)"
  , "      || (r.cap !== null && r.cap.value !== r.capBase);"
    -- Every layer that moved, one POST each and each awaited.  A config file is
    -- its own write and its own lock, so one that drifted refuses on its own
    -- line while the rest land — there is no batch to roll back and none to
    -- want.
  , "    async function flushConfig() {"
  , "      cnote(\"syncing\");"
  , "      let ok = true, clashed = false;"
  , "      for (const r of crows) {"
  , "        if (!cmoved(r)) continue;"
  , "        // What was SENT, taken before the await: a keystroke landing while"
  , "        // the write is in flight would otherwise be marked as the file's"
  , "        // and never written, and the sheet would close on it silently."
  , "        const sent = r.box.value, view = r.view && r.view.value;"
  , "        const cap = r.cap && r.cap.value;"
  , "        const a = await fetch(\"/config\", {"
  , "          method: \"POST\","
  , "          headers: { \"content-type\": \"application/json\" },"
  , "          body: JSON.stringify({ path: r.path, lines: sent.split(\"\\n\"),"
  , "                                 ...(r.view ? { filter: view } : {}),"
  , "                                 ...(r.cap ? { capture: cap } : {}),"
  , "                                 digest: r.digest }),"
  , "        }).then((x) => x.json().then((b) => ({ status: x.status, body: b })))"
  , "          .catch((e) => ({ status: 0, body: { error: e.message } }));"
  , "        if (a.status === 200) {"
  , "          r.digest = a.body.digest; r.base = sent; r.note.textContent = \"\";"
  , "          if (r.view) r.viewBase = view;"
  , "          if (r.cap) r.capBase = cap;"
  , "        } else {"
  , "          ok = false;"
  , "          if (a.status === 409) clashed = true;"
  , "          r.note.textContent = a.body.error || `sync failed (${a.status})`;"
  , "        }"
  , "      }"
  , "      cnote(ok ? \"synced\" : clashed ? \"conflict\" : \"error\");"
  , "      return ok;"
  , "    }"
    -- C-x C-s and the way out are the ladder's, over `configSheet': the
    -- refresh above is what a conflict overwrites under, and the close is
    -- pristine-costs-nothing, dirty-syncs-and-closes, trouble-discards.
  , "    function shutSettings() {"
  , "      el(\"config\").className = \"\"; settings = false; crows = [];"
  , "      configSheet.state = \"synced\";"
    -- And the keys go back to the table, in ONE place.  A control of the sheet
    -- holds the focus while it is up — which is what keeps the table's own keys
    -- dead under it — so the close is what has to give it up.  A browser drops
    -- the focus anyway when the box goes to `display:none'; saying it makes it
    -- the sheet's rule rather than a side effect, and it covers every control
    -- the sheet will ever hold rather than costing one `blur()' per control.
  , "      if (typing()) document.activeElement.blur();"
  , "    }"
    -- The gear is the coarse pointer's `C-c C-,'.  It needs no media query of
    -- its own: the rules hide it outside the one block, and an element that is
    -- not displayed is one nobody can tap.
  , "    el(\"gear\").addEventListener(\"click\", openSettings);"
  , "    // `/' summons the filter.  `openFilter' is the renderer's one entry point"
  , "    // for it whatever mode it is in — in palette mode it raises the overlay,"
  , "    // elsewhere it takes the box already on the page — so the shell asks for"
  , "    // it rather than reaching into the chrome.  An asset predating the call"
  , "    // has a resident box; focusing that is how this worked before."
  , "    const summons = () => !!table && typeof table.openFilter === \"function\";"
  , "    const focusFilter = () => {"
  , "      if (summons()) { table.openFilter(); return; }"
  , "      const box = filterBox();"
  , "      if (box) { box.focus(); box.select(); }"
  , "    };"
  -- The one exception to keyboard-first, and the reason it is one: a coarse
  -- pointer has no `/' to press.  The chip row is the whole of the filter
  -- chrome a palette-mode page carries, so it doubles as the palette's button
  -- there — the same `focusFilter' the key runs, feature detection included.
  -- Delegated from @#app@, so it survives every re-mount, and gated on the
  -- media query the rules are in, so a mouse sees nothing new.  A tap on a
  -- chip is that chip's own removal and stays the renderer's.
  , "    const coarse = () => typeof matchMedia === \"function\""
  , "      && matchMedia(\"(pointer: coarse)\").matches;"
  , "    el(\"app\").addEventListener(\"click\", (e) => {"
  , "      if (!coarse()) return;"
  , "      const t = e.target;"
  , "      if (!t.closest || !t.closest(\".tv-chips\") || t.closest(\".tv-chip\")) return;"
  , "      focusFilter();"
  , "    });"
  , "    // What a remount takes with it.  The table is `#app''s and goes when"
  , "    // the mount is replaced; the palette is the renderer's chrome inside it"
  , "    // and goes with it.  The sheet is a SIBLING of `#app' and survives by"
  , "    // where it sits, which is a fact about the layout rather than a promise"
  , "    // — so both are carried across by hand and neither depends on it."
  , "    let stashed = null;"
  , "    // The palette's lifecycle is the renderer's and this page does not reach"
  , "    // into its chrome past the field.  A field with focus is a palette the"
  , "    // reader is typing in; anything else is a query already committed, which"
  , "    // the URL is carrying anyway."
  , "    function typedFilter() {"
  , "      const box = filterBox();"
  , "      return box && document.activeElement === box ? box.value || \"\" : null;"
  , "    }"
  , "    function stash() {"
  , "      stashed = {"
  , "        // A pristine sheet is the file, which the remount can re-read; what"
  , "        // cannot be re-read is work the reader has not saved yet — in either"
  , "        // pane, and in whichever of the two shapes the sheet was showing."
  , "        sheet: editing && dirty()"
  , "          ? { id: editing.id, raw, text: el(\"mtext\").value, props: props(),"
  , "              plan: planning(), digest: editing.digest }"
  , "          : null,"
  , "        palette: typedFilter(),"
  , "      };"
  , "    }"
  , "    function restore() {"
  , "      const was = stashed;"
  , "      stashed = null;"
  , "      if (!was) return;"
  , "      if (was.palette !== null) {"
  , "        focusFilter();"
  , "        // Assigning fires no input event, so the renderer is not asked to"
  , "        // complete or commit a query the reader has not finished typing."
  , "        const box = filterBox();"
  , "        if (box) { box.value = was.palette; box.focus(); }"
  , "      }"
  , "      if (was.sheet) reopen(was.sheet);"
  , "    }"
  , "    // The sheet, back open on what was in it — both panes, in the shape it"
  , "    // was showing.  The digest is re-asked for rather than carried over: a"
  , "    // file that moved while the mount was rebuilt is the conflict flow, and"
  , "    // flushing against a digest this page merely remembers is the silent"
  , "    // overwrite that flow exists to stop.  The reader's work is put back"
  , "    // either way — a restore never decides that an edit is worth less than"
  , "    // the file.  The baselines stay the file's, so what was dirty stays dirty."
  , "    function reopen(s) {"
  , "      headline(s.id).then((h) => {"
  , "        show(h, s.raw);   // which opens the sheet on the file and focuses it"
  , "        el(\"mtext\").value = s.text;   // dirty again, against the file as it now is"
  , "        if (!s.raw) drawProps(s.props, s.plan);"
  , "        if (h.digest !== s.digest) sync(\"conflict\");"
  , "      }).catch((e) => append(\"sync\", \"error\", `sheet restore failed: ${e.message}`));"
  , "    }"
  , "    // The one door that throws the mount away and builds a new one: a"
  , "    // `view-changed' close, and `g'.  Everything else that loses the socket"
  , "    // goes through `resync', which keeps the page it has."
  , "    function remount(after) { stash(); start(after); }"
    -- `g': the view this tree configures, applied the way every other query is
    -- — written into the URL and asked of the server.  It goes through the
    -- mount because the chips are the renderer's and only a mount can be handed
    -- a query it did not commit itself; `start' then reads the URL this just
    -- wrote.  Dropping onclose first stops the reconnect timer opening a second
    -- socket behind this one.
    -- SEL is where the cursor should end up, which only a POP has an opinion
    -- about; every other caller leaves it out and takes the first row.  The
    -- landing rule lives HERE rather than in each caller, so a view applied
    -- through this door lands the same way whoever asked for it.
  , "    function applyView(b, q, landing, sel) {"
  , "      said(b, q ? `filter: ${JSON.stringify(q)}` : \"filter cleared\");"
  , "      if (socket) { socket.onclose = null; socket.close(); socket = null; }"
  , "      backoff = 1000;"
  , "      remember(q);"
  , "      remount((total) => { land(sel || null); if (landing) landing(total); });"
  , "    }"
    -- `g' is HOME, so it is not a step on the trail: it throws the crumbs away
    -- with the labels that named them.  Walking back out of a drill is DEL's,
    -- one rung at a time; `g' is the door, not the ladder.
  , "    function applyDefault(b) {"
  , "      if (crumbing()) table.setCrumbs([]);"
  , "      crumbLabels = {};"
  , "      crumbSels = [];"
  , "      applyView(b, DEFAULT_QUERY);"
  , "    }"
    -- `@': the rows pointing AT the one at point.  A drill is a LOOK, so it
    -- takes the row at point and never the marked set — a mark is what a reader
    -- lays down to write over a run of rows, and inheriting it here would make
    -- every mark change what `@' means.
    --
    -- The crumb goes down BEFORE the view changes, so what it records is where
    -- the reader was standing rather than where they landed; `applyView' then
    -- writes both into the URL in one `remember'.  `ref:' is the server's own
    -- term (SCHEMA.md) — the renderer reads it as free text and would narrow
    -- further, which is why the drill re-fetches like every other query.
    -- A drill out of the EMPTY query leaves no crumb, which is the absence of a
    -- special case rather than one: `all rows' IS the empty filter, and
    -- DEL already lands there — the first rung strips the `ref:' token, the
    -- query goes empty, and with no trail behind it the key clears the filter,
    -- which is the very view the crumb would have restored.  So the crumb, its
    -- label and its remembered row would be bookkeeping for a step the ladder
    -- takes anyway.  What goes with it is the cursor: DEL back out of that one
    -- drill lands on the first row like every other applied view, rather than
    -- on the row the drill was launched from.
    --
    -- ZERO REFERENCES IS NO JUMP, and the answer is what says so: the drill is
    -- PROBED first — the same query under `limit=1', which is a count and one
    -- row — and a total of nothing leaves the table, the filter and the trail
    -- exactly where they were.  A view with no rows in it is the one landing a
    -- reader cannot read anything off, and walking back out of it costs a
    -- keystroke to undo a keystroke.  The cost is a second fetch on a key that
    -- was already going to refetch, which is one keypress either way.
  , "    function relations(b) {"
  , "      const id = focusedId();"
  , "      if (!id) { said(b, \"no row\"); return; }"
  , "      if (!crumbing()) { said(b, \"this table-view.js has no crumbs\"); return; }"
  , "      const token = refToken(id), name = titleOf(id);"
  , "      load(`${asking(token)}&limit=1`).then((a) => {"
  , "        if (!a.total) {"
  , "          said(b, `no references to ${JSON.stringify(name)}`);"
  , "          append(\"cmd\", \"info\", `no references to headline ${JSON.stringify(name)}`);"
  , "          return;"
  , "        }"
  , "        drill(b, token, name);"
  , "      }).catch((e) => {"
  , "        if (e.name !== \"AbortError\") failed(b, \"relations\")(e);"
  , "      });"
  , "    }"
  , "    function drill(b, token, name) {"
  , "      if (query.trim()) {"
      -- The crumb records where the reader was STANDING: the query being left,
      -- and the row and column they were on, so walking back puts the cursor
      -- where it was rather than at the top of a view they had scrolled into.
  , "        const at = cells() ? table.getSelection() : null;"
  , "        const n = table.pushCrumb({ label: hereLabel(), query: query });"
  , "        crumbSels[n - 1] = at && at.id ? { id: at.id, col: at.col } : null;"
  , "        crumbSels.length = n;"
  , "      }"
  , "      crumbLabels[token] = `references of «${name}»`;"
  , "      applyView(b, token, (total) =>"
  , "        said(b, `references of ${JSON.stringify(name)} · ${total}`));"
  , "    }"
    -- `a' is the second canned view and the only one this page spells itself:
    -- the active rows carrying a date, which is `planned' — the virtual key
    -- over the two date cells, decidable by either side of the wire.  It is a
    -- VIEW rather than a mode, so `g' is the way home and every other key means
    -- what it always meant while it is applied.
    -- The sort is part of the view — earliest first — so it is part of the
    -- QUERY: `sort:scheduled' says it to the server, which answers page one in
    -- that order, and to the renderer, which shows it on the header. A canned
    -- view is one string again, where it used to be a query plus a call behind
    -- the answer, and `DEL' walks out of the order the way it walks out of the
    -- filter.
  , "    const AGENDA_QUERY = \"state:*active* -planned:*empty* sort:scheduled\";"
    -- What `a' says once its rows are on screen.  The count is the server's
    -- answer to the query, which is the one number a first page cannot give.
  , "    function landedAgenda(b, total) {"
  , "      said(b, `agenda · ${rowsWord(total)}`);"
  , "    }"
  , ""
  , "    // Keys.  The map is the JSON above — dispatch and echo read the one blob,"
  , "    // and there is one map: `n'/`j' both step a row, `f'/`l' both step a cell."
  , "    const MAPS = JSON.parse(el(\"keys\").textContent);"
  , "    // The theme selector.  `auto' is the media query — the attribute comes"
  , "    // off — and light and dark pin `data-theme' on the root, which is what"
  , "    // this page's own variables and the renderer's overrides both key off."
  , "    // The head has already applied the stored choice; this keeps the"
  , "    // control and the storage in step with it."
  , "    const themed = {"
  , "      get() { try { return localStorage.getItem(\"glance-theme\") || \"auto\"; }"
  , "              catch (e) { return \"auto\"; } },"
  , "      set(v) { try { localStorage.setItem(\"glance-theme\", v); } catch (e) { /* denied */ } },"
  , "    };"
  , "    function setTheme(name) {"
  , "      if (name === \"auto\") delete document.documentElement.dataset.theme;"
  , "      else document.documentElement.dataset.theme = name;"
  , "      themed.set(name);"
  , "      el(\"themesel\").value = name;"
  , "    }"
  , "    setTheme(themed.get());"
    -- The theme applies as it is picked and the sheet stays where it is: this
    -- is a preference rather than a write, so there is nothing to sync and no
    -- reason to close over it.  The select KEEPS the focus, which is the whole
    -- of the rule now that it lives inside a popup — a popup is a legitimate
    -- focus holder, `typing()' is true while one of its controls has the focus,
    -- and the table's own keys are dead underneath either way.  The corner's
    -- copy owed a `blur()' precisely because the corner is not one.
  , "    el(\"themesel\").addEventListener(\"change\", (e) => {"
  , "      setTheme(e.target.value);"
  , "      echo(`theme: ${e.target.value}`);"
  , "    });"
  , ""
  -- The resident key line, under the log: what can run, where the echo pill
  -- says what just did.  The table is the blob's ('keyHints'), naming commands
  -- rather than keys, so the spelling comes out of the same rows the dispatch
  -- reads.  A command two keys spell shows the FIRST of them, which is the
  -- order 'keyBindings' lists it in.
  , "    function hints() {"
  , "      const seq = (command) => {"
  , "        const b = MAPS.rows.find((x) => x.command === command && x.scope === \"table\");"
  , "        return b && b.handler ? b.seq : null;   // a staged row is no offer"
  , "      };"
  , "      el(\"kbd\").textContent = MAPS.hints"
  , "        .map((h) => [h.commands.map(seq).filter(Boolean), h.label])"
  , "        .filter(([keys]) => keys.length)"
  , "        .map(([keys, label]) => `${keys.join(\"/\")} ${label}`)"
  , "        .join(\" · \");"
  , "    }"
  , "    hints();"
  , "    const NAMED = { Enter: \"RET\", Tab: \"TAB\", \" \": \"SPC\", Escape: \"ESC\","
  , "      Backspace: \"DEL\", Delete: \"<delete>\", ArrowUp: \"<up>\", ArrowDown: \"<down>\","
  , "      ArrowLeft: \"<left>\", ArrowRight: \"<right>\", Home: \"<home>\", End: \"<end>\","
  , "      PageUp: \"<prior>\", PageDown: \"<next>\" };"
  , "    function keyName(e) {"
  , "      let base = NAMED[e.key], special = base !== undefined;"
  , "      if (!special && /^F\\d{1,2}$/.test(e.key))"
  , "        { base = `<${e.key.toLowerCase()}>`; special = true; }"
  , "      if (!special) { base = e.key; if (base.length !== 1) return null; }"
  , "      let mods = \"\";"
  , "      if (e.ctrlKey) mods += \"C-\";"
  , "      if (e.altKey || e.metaKey) mods += \"M-\";"
  , "      if (special && e.shiftKey) mods += \"S-\";"
  , "      return mods + base;"
  , "    }"
  , "    let echoAt = null, pending = [], pendingAt = null;"
  , "    function echo(text, hold) {"
  , "      const pill = el(\"echo\");"
  , "      pill.textContent = text;"
  , "      pill.style.opacity = \"1\";"
  , "      clearTimeout(echoAt);"
  , "      if (!hold) echoAt = setTimeout(() => (pill.style.opacity = \"0\"), 1500);"
  , "    }"
  , "    function prefix(keys) {"
  , "      pending = keys;"
  , "      clearTimeout(pendingAt);"
  , "      if (!keys.length) return;"
  , "      const shown = keys.join(\" \");"
  , "      echo(`${shown} -`, true);"
  , "      pendingAt = setTimeout(() => { pending = []; echo(`${shown} - timed out`); }, 2000);"
  , "    }"
  , "    // A focus that keeps its own keys: the filter box, the sheet, and the"
  , "    // keys select, which navigates on the arrows this map would otherwise"
  , "    // take for row movement — and the four modal things that hold the keys"
  , "    // with nothing focused at all: the property panel in nav, the value"
  , "    // palette in letter mode, whose whole offer is single letters the table"
  , "    // also binds, and the two popups, which browse on the table's own"
  , "    // movement keys and write on its own `d'/`D'/`u'.  Any of them would"
  , "    // otherwise leave the table's own keys live underneath it — for the link"
  , "    // popup that is the whole of what makes it read-only, and for the tags"
  , "    // popup it is what keeps `d' off a row while it flags a tag."
  , "    const typing = () => {"
  , "      const a = document.activeElement;"
  , "      return pnav() || !!prompting || linking() || managing()"
  , "        || (!!a && (a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
  , "                     || a.tagName === \"SELECT\" || a.isContentEditable));"
  , "    };"
    -- `modal' is "a sheet is up", and there are two of them: the subtree's and
    -- the settings'.  Never both — `openSettings' refuses over an open sheet,
    -- which is what keeps `C-x C-s' and `ESC' from having to guess which one
    -- they meant.
  , "    const live = (b) => b.scope === \"any\""
  , "      || (b.scope === \"modal\" && (editing !== null || settings))"
  , "      || (b.scope === \"table\" && !typing());"
  , "    // A live selection means C-c and C-x are copy and cut, and the browser"
  , "    // decides that on this keydown — so the prefix does not claim them."
  , "    function selecting() {"
  , "      const a = document.activeElement;"
  , "      if (a && typeof a.selectionStart === \"number\")"
  , "        return a.selectionStart !== a.selectionEnd;"
  , "      const s = document.getSelection();"
  , "      return !!s && !s.isCollapsed;"
  , "    }"
  , "    const HANDLERS = {"
  , "      nextRow: () => move(1),"
  , "      previousRow: () => move(-1),"
  , "      nextColumn: (b) => moveCol(b, 1),"
  , "      previousColumn: (b) => moveCol(b, -1),"
  , "      nextPage: (b) => turnPage(b, 1),"
  , "      previousPage: (b) => turnPage(b, -1),"
  , "      firstRow: (b) => endStop(b, false),"
  , "      lastRow: (b) => endStop(b, true),"
    -- `^' sorts by the column the CELL selection is standing in, which is the
    -- whole of how it picks one: a whole-row selection names no column, and
    -- guessing one — the primary, the first, the last one sorted — would be
    -- this page inventing a rule the renderer's own `^' does not have (there
    -- the answer is where point is).  So it refuses and says which key picks a
    -- column.
    --
    -- `sortable' is the RENDERER's opt-in and `sortBy' ignores it — the flag
    -- gates what a reader may reach, where a producer's own call is the
    -- producer's business — so a page driving a reader's key has to honour it
    -- here or it would sort a column the header click will not.
    --
    -- `^' PROMOTES: the column at point becomes the chain's head ascending (the
    -- rest shift down, deduped); on the column already leading it flips that key
    -- alone.  Composing a chain = pressing over columns in reverse priority
    -- order — the web's spelling of table-view.el's C-u ^.
    --
    -- IT IS A QUERY EDIT.  The renderer writes the new chain into the applied
    -- query as `sort:' tokens and delivers it, so the press lands here as an
    -- ordinary filter commit: the rows in hand re-order at once, the URL is
    -- rewritten, the server is asked for that order and answers page one in it,
    -- and DEL walks the keys back off one at a time.  Nothing on this page
    -- remembers a sort.
  , "      toggleSort: (b) => {"
  , "        if (!sorts()) { said(b, \"this table-view.js has no sort\"); return; }"
  , "        const at = column(), c = at === null ? null : cols[at];"
  , "        if (!c) { said(b, \"no column selected — f/l to pick one\"); return; }"
  , "        const named = c.header || c.key;"
    -- `sortable' is the renderer's opt-in and `sortPromote' is where it is
    -- enforced, so the refusal is READ OFF the call rather than derived a
    -- second time here — the key still has to SPEAK it.
  , "        if (!table.sortPromote(c.key)) { said(b, `${named} does not sort`); return; }"
  , "        const chain = table.getSort() || [], head = chain[0];"
  , "        said(b, head ? `${named} ${head.ascending !== false ? \"▲\" : \"▼\"}`"
      <> " + (chain.length > 1 ? ` · ${chain.length} keys` : \"\") : named);"
  , "      },"
  , "      materializeRow: () => {"
  , "        const id = focusedId();"
  , "        if (id) materialize(id);"
  , "        else append(\"cmd\", \"info\", \"no row focused — n or p picks one\");"
  , "      },"
  , "      markToggle: (b) => mark(b, true),"
  , "      unmarkRow: (b) => mark(b, false),"
  , "      unmarkAll: (b) => {"
  , "        if (!marking()) { said(b, \"this table-view.js has no marks\"); return; }"
  , "        table.clearMarks();"
  , "        if (flagging()) table.clearFlags();"
  , "        said(b, \"all marks and flags cleared\");"
  , "      },"
    -- `M' marks the whole loaded set, which is the renderer's call because the
    -- set is the renderer's: a page it is not showing is still marked.
  , "      markAll: (b) => {"
  , "        if (!marking() || typeof table.markAll !== \"function\")"
  , "          { said(b, \"this table-view.js has no mark-all\"); return; }"
  , "        table.markAll();"
  , "        said(b, `marked · ${table.markedCount()}`);"
  , "      },"
    -- dired's `d', in two presses: the first flags the row and the second is
    -- `D' — `archive' over every flagged row, this one included.  The flag IS
    -- the confirmation, so there is no prompt — and the command is in ONCE, so a
    -- HELD `d' delivers exactly one press and can never flag and archive from
    -- one keystroke.  `u' takes a flag off.
  , "      archiveFlag: (b) => {"
  , "        if (!flagging()) { said(b, \"this table-view.js has no archive flags\"); return; }"
  , "        const id = focusedId();"
  , "        if (!id) { said(b, \"no row\"); return; }"
  , "        if (isFlagged(id)) { archive(b); return; }"
  , "        table.flagRow(id);"
  , "        noted(id, \"marked for deletion\");"
  , "        said(b, \"flagged — d again archives\");"
  , "      },"
  , "      applyDefault, relations, focusFilter, toggleRaw, openSettings,"
    -- One `save-buffer' over two sheets: `saveSheet' asks `activeSheet' which
    -- is up, so there is nothing to choose between here.
  , "      save: saveSheet,"
    -- D is dired's key and org-glance's `delete', and it is `archive' with no
    -- flagging step in front of it — the same function the second `d' reaches.
  , "      archiveRows: archive,"
    -- C-c C-t asks which state, over whatever the command would run on.  The
    -- overlay goes up on the press and the answer fills it: the same server
    -- that refuses a keyword the row's own file does not declare is the one
    -- that says which keywords those are, so the offer and the refusal cannot
    -- disagree.  A fill that lands after the reader has left finds another
    -- prompt or none, and drops.
  , "      setState: (b) => overTargets(b, \"set state\", (ids, title) => {"
  , "        const mine = ask(title,"
  , "          (c) => fire(b, \"set-state\", ids, { keyword: c.keyword },"
  , "                      c.keyword === null ? EMPTY : c.keyword),"
  , "          \"a letter sets it · / to search · ESC leaves\");"
  , "        keywordSources(ids).then((answer) => {"
  , "          if (prompting === mine) setChoices(answer.sources);"
  , "        }).catch(askFailed(mine, \"keywords\"));"
  , "      }),"
    -- `:' is the agenda's own key for the same question, over the same rows as
    -- `t' — the marked set, else the row at point.  It raises the POPUP, which
    -- STAYS up under every write it carries: managing tags is several ops over
    -- one set, where setting a state is one, and closing after each would make
    -- the second op a fresh press and a fresh resolution.
    --
    -- Raised LATE, behind the fetch, for the link popup's reason: `:' is no key
    -- inside the list it opens, so an empty mount put up on the press would buy
    -- nothing and cost a raising guard.  Every named row unknown to the store
    -- leaves nothing to tag at all, which is a refusal rather than an empty
    -- popup.
  , "      manageTags: (b) => overTargets(b, \"tags\", (ids, title) => {"
  , "        tagsOf(ids).then((answer) => {"
  , "          if (!(answer.rows || []).length) { said(b, \"no such row\"); return; }"
  , "          showTags(b, title, answer);"
  , "        }).catch(failed(b, \"tags\"));"
  , "      }),"
    -- `+' is the minibuffer and nothing else: what it collects goes straight to
    -- the server, which knows the file.
  , "      capture: (b) =>"
  , "        askText(\"capture · a headline for the inbox\","
  , "                \"RET captures it · ESC leaves\", \"\", (c) => captureRow(b, c.text)),"
    -- `o' follows the row rather than editing it, and how many links the row
    -- holds decides the whole gesture: none is a refusal, one opens, several
    -- raise the popup.  The count is the server's answer, so the popup can only
    -- go up behind the request — which is why this one is raised late where the
    -- state palette is raised on the press.  By then the `o' that asked has been
    -- dispatched and gone, so nothing is travelling and no press is declined.
  , "      openLinks: (b) => {"
  , "        const id = focusedId();"
  , "        if (!id) { said(b, \"no row\"); return; }"
  , "        linksOf(id).then((a) => {"
  , "          const links = a.links || [];"
  , "          if (!links.length) { said(b, \"no links\"); return; }"
  , "          if (links.length === 1) { openLink(b, links[0]); return; }"
  , "          showLinks(b, links);"
  , "        }).catch(failed(b, \"open\"));"
  , "      },"
  , "      applyAgenda: (b) => applyView(b, AGENDA_QUERY, (total) => landedAgenda(b, total)),"
  , "      schedulePlan: (b) => planRows(b, \"SCHEDULED\"),"
  , "      deadlinePlan: (b) => planRows(b, \"DEADLINE\"),"
    -- `q' is the SUBTREE sheet's door alone, which is why it asks after
    -- `editing' rather than after whichever sheet is up.
  , "      quitWindow: () => (editing ? leaveSheet()"
  , "        : append(\"cmd\", \"info\", \"q closes the sheet; there is no window to quit\")),"
    -- One key out of whichever overlay is up: the prompt first, since it is the
    -- one that can be raised over an open sheet.
  , "      cancel: () => {"
    -- The prompt is the top rung wherever it is raised: over the table, or over
    -- the tags popup as its `+' field, which is what leaves the popup standing
    -- when a reader backs out of adding.
  , "        if (prompting) unask();"
    -- The two popups are rungs of their own beside the palette's: each is raised
    -- over the table alone, never over a sheet, so their place in the ladder is
    -- decided by nothing but reading order.  The tags popup's open rename is a
    -- rung UNDER it, the way the panel's open row is under the sheet: ESC puts
    -- the tag back and only then does the key reach the popup.
  , "        else if (linking()) shutLinks();"
  , "        else if (renaming()) cancelRename();"
  , "        else if (managing()) shutTags();"
    -- The panel's open row is a rung of its own, under the sheet's: while one
    -- is open ESC puts it back, and only from nav does the key reach the sheet
    -- — whichever of the two is up, which is `leaveSheet''s own question.
  , "        else if (pediting()) cancelRow();"
  , "        else if (activeSheet()) leaveSheet();"
  , "        else if (typing()) document.activeElement.blur();"
  , "      },"
  , "      // The filter's own backspace: the renderer drops the token and the"
  , "      // shell follows it — one commit, one URL, focus left on the table."
  , "      //"
  , "      // A LADDER, in two rungs.  While the query has tokens in it, DEL takes"
  , "      // the last one off, as it always has.  When the strip EMPTIES the"
  , "      // query and there is a trail behind it, the same key walks back out of"
  , "      // the drill that built the view — and it applies the crumb's query"
  , "      // INSTEAD of the empty one, so `@' and `DEL' are one step out and one"
  , "      // step back rather than a step and a half.  With no trail the second"
  , "      // rung is not there and the key does what it did before."
  , "      filterDrop: (b) => {"
  , "        if (!strips()) { said(b, \"this table-view.js has no filter tokens\"); return; }"
  , "        if (!table.stripLastToken()) { said(b, \"no filter\"); return; }"
  , "        const left = table.getQuery().trim();"
  , "        if (!left && crumbing() && trail().length) {"
  , "          // The row this crumb was pushed from, when the side table is"
  , "          // still in step with the trail the renderer is holding."
  , "          const sel = selsFit() ? crumbSels.pop() : null;"
  , "          const back = table.popCrumb();"
  , "          // The view being left takes its label with it; a crumb further"
  , "          // down the trail keeps its own, since the map is keyed by token."
  , "          delete crumbLabels[query];"
  , "          applyView(b, back.query, () => said(b, `back to ${back.label}`), sel);"
  , "          return;"
  , "        }"
  , "        commit(left);"
  , "        said(b, left ? `filter: ${JSON.stringify(left)}` : \"filter cleared\");"
  , "      },"
  , "    };"
  , "    // The row is handed to its handler: one that names what it landed on"
  , "    // — the filter left, the column arrived at — echoes over this line with"
  , "    // the same `seq → command' opening."
  , "    function run(b) {"
  , "      echo(`${b.seq} → ${b.command}${b.help ? ` · ${b.help}` : \"\"}`);"
  , "      const handler = b.handler && HANDLERS[b.handler];"
  , "      if (handler) handler(b);"
  , "      else append(\"cmd\", \"info\","
      <> " `${b.seq} (${b.command}) — arrives with daemon commands (M4)`);"
  , "    }"
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      const k = keyName(e);"
  , "      if (!k) return;"
  , "      const keys = pending.concat([k]);"
  , "      const here = MAPS.rows.filter(live);"
  , "      // A row is in play while its keys open with the ones typed so far."
  , "      const opens = (b) => keys.every((key, i) => b.keys[i] === key);"
  , "      const hit = here.find((b) => b.keys.length === keys.length && opens(b));"
  , "      // A held key still belongs to this map — it is claimed either way —"
  , "      // but a destructive one runs once per press."
  , "      if (hit) {"
  , "        prefix([]);"
  , "        e.preventDefault();"
  , "        if (!(e.repeat && MAPS.once.indexOf(hit.command) !== -1)) run(hit);"
  , "        return;"
  , "      }"
  , "      if (here.some((b) => b.keys.length > keys.length && opens(b))) {"
  , "        if (!selecting()) { e.preventDefault(); prefix(keys); }"
  , "        return;"
  , "      }"
  , "      if (!pending.length) return;   // not ours; the browser keeps it"
  , "      prefix([]);"
  , "      if (MAPS.reserved.indexOf(k) === -1) e.preventDefault();"
  , "      echo(`${keys.join(\" \")} is undefined`);"
  , "    });"
    -- The prompt's own keys, behind the dispatch above: while it is up
    -- `typing()' is true, so the only row that can have fired already is ESC,
    -- which is the one that should.  C-n and C-p are reserved chords the map
    -- never claims, and claiming them HERE is the palette's business rather
    -- than the map's — the same way a focused select keeps its arrows.
    --
    -- Letter mode is bare letters only: `keyName' spells a chord `C-t' and a
    -- held shift `T', neither of which is a claimed letter, so both fall
    -- through to whatever else wants them.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!prompting) return;"
  , "      if (prompting.raising) { prompting.raising = false; return; }"
  , "      const k = keyName(e);"
    -- A bare modifier spells no key, and an unbound entry claims no letter:
    -- without this the two nulls would meet and Shift would commit whatever
    -- came out of the pool empty.
  , "      if (!k) return;"
    -- The text mode has no list, so no letter commits and nothing narrows: RET
    -- takes the line as typed and every other key is the field's own.
    -- The mode that holds a LINE rather than a list — `askText'\''s prompt, and
    -- `+'\''s field over a sticky palette — takes RET and leaves every other key
    -- to the field.  Nothing narrows and no letter commits.  A palette whose
    -- typing reaches past its list takes the line as an ENTRY (`freely'), and
    -- one with no list at all takes it as text.
  , "      if (prompting.text) {"
  , "        if (k !== \"RET\") return;"
  , "        takeChoice(freely() || { text: el(\"pinput\").value });"
  , "        e.preventDefault();"
  , "        return;"
  , "      }"
    -- A letter writes, so it runs once per press — the `ONCE' rule, owed here
    -- rather than by the map because the key that OPENS this palette is a
    -- letter too, and a held one would raise it and commit through it.  The
    -- repeat is claimed either way, the way the dispatch claims one it declines
    -- to run.  DEL arrives here as an ordinary entry key, since `*empty*' holds
    -- it as its own; a palette with no such entry — the tag one — leaves the
    -- press to nobody, `typing()' having already killed the map's own DEL.
  , "      if (!prompting.narrow) {"
  , "        const hit = prompting.choices.find((c) => c.key === k);"
  , "        if (k === \"/\") fieldMode();"
  , "        else if (!hit) return;"
  , "        else if (!e.repeat) takeChoice(hit);"
  , "        e.preventDefault();"
  , "        return;"
  , "      }"
  , "      const step = k === \"<down>\" || k === \"C-n\" ? 1"
  , "                 : k === \"<up>\" || k === \"C-p\" ? -1 : 0;"
  , "      if (step) walkChoices(step);"
  , "      else if (k === \"RET\") takeChoice(prompting.shown[prompting.at] || freely());"
  , "      else return;"
  , "      e.preventDefault();"
  , "    });"
    -- The link popup's keys, the THIRD listener behind the dispatch and safe for
    -- the reason the other two are: while it is up `typing()' has already made
    -- every `table' row dead, so the only row that can have fired ahead of this
    -- is `ESC' — which is the one that should, `cancel' closing whichever
    -- overlay is up.
    --
    -- MOVE, LOOK, OPEN, and that is the whole surface today.  Row movement is
    -- `rowStep', the property panel's own — both spellings and the arrows, bound
    -- unconditionally the way the panel's are, since the popup holds no field
    -- and every printable key is free.
    --
    -- `o' is the OPEN key, which is the key that raised this — the table's own
    -- `o' carried inside, over the link the cursor is on rather than over the
    -- row.  It opens and CLOSES, both outcomes alike (the tab and the refusal),
    -- since picking one link is what the popup was raised to do and a popup that
    -- stayed up on the refusal would be a second rule for the same key.
    --
    -- `RET' is reserved for editing the link at point IN PLACE — the row's own
    -- title and url cells becoming fields over themselves, `TAB' between them,
    -- `RET' committing and `ESC' restoring, which is the property panel's edit
    -- model exactly.  ONE edit vocabulary across the page: a panel row and a
    -- link row are edited alike, and the type cell is derived and never opens.
    -- It is not here yet — the write needs a per-link SPAN out of `GET /links'
    -- and an `edit-link' command to splice it, neither of which this build has —
    -- so it says what it is waiting for, the way a bound key with no handler
    -- does, and leaves the popup standing.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!linking()) return;"
  , "      const k = keyName(e);"
  , "      if (!k) return;"
  , "      const step = rowStep(k);"
  , "      if (step) stepIn(lmount, step);"
  , "      else if (k === \"o\") {"
  , "        const link = pointedLink();"
  , "        const b = opening;"
  , "        shutLinks();"
  , "        if (link) openLink(b, link);"
  , "      }"
  , "      else if (k === \"RET\") append(\"cmd\", \"info\","
      <> " \"RET (edit-link) — arrives with the link span and the edit-link command\");"
  , "      else return;"
  , "      e.preventDefault();"
  , "    });"
    -- The tags popup's keys, the FOURTH listener behind the dispatch and safe
    -- for the reason the other three are: while it is up `typing()' has already
    -- made every `table' row dead, so the only row that can have fired ahead of
    -- this is `ESC' — which is the one that should.
    --
    -- TWO GUARDS, both about the value palette `+' raises OVER this one.  While
    -- it is up this listener declines outright, or a reader narrowing the field
    -- would be flagging tags underneath it — `typing()' having killed the map's
    -- rows, there is nothing else between the two surfaces.  And a key the
    -- palette has just CLAIMED is declined too: the palette's listener runs
    -- ahead of this one and closes the overlay as it commits, so the very `RET'
    -- that added a tag would arrive here over a popup with no prompt on it and
    -- open the rename.  `defaultPrevented' is the DOM's own word for "handled",
    -- which is what every listener here already says by calling it.
    --
    -- MOVE, RENAME, FLAG, REMOVE, ADD.  Row movement is `rowStep', the property
    -- panel's own and the link popup's; `RET' opens the rename; `d'/`D'/`u' are
    -- the deletion gesture, spelled here as they are there and guarded against a
    -- HELD key the same way — a repeat that survived would flag a tag and remove
    -- it from ONE press, which is the confirmation the two-press shape exists to
    -- be.  With the overlay open the keys are the rename's: `RET' commits, and
    -- `ESC' is the keymap's, which puts the tag back.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!managing() || prompting || e.defaultPrevented) return;"
  , "      const k = keyName(e);"
  , "      if (!k) return;"
  , "      if (renaming()) {"
  , "        if (k !== \"RET\") return;   // ESC is the keymap's, and puts the tag back"
  , "        renameTag(renamingFrom, el(\"tname\").value);"
  , "        e.preventDefault();"
  , "        return;"
  , "      }"
  , "      const step = rowStep(k);"
  , "      if (step) stepIn(tmount, step);"
  , "      else if (k === \"RET\") openRename();"
  , "      else if (k === \"+\") addFlow();"
  , "      else if (k === \"d\" || k === \"D\" || k === \"u\") { if (!e.repeat) tflag(k); }"
  , "      else return;"
  , "      e.preventDefault();"
  , "    });"
  , ""
  , "    function apply(frame) {"
  , "      if (!table) return;"
  , "      // Under a filter the loaded rows are the server's answer to a query,"
  , "      // and only it knows whether the changed row still matches: ask again."
  , "      if (query) return void (clearTimeout(requeryAt),"
  , "        requeryAt = setTimeout(fetchRows, 250));"
  , "      if (frame.op === \"upsert-row\") table.upsertRow(frame.row);"
  , "      else if (frame.op === \"delete-row\") table.deleteRow(frame.id);"
  , "    }"
  , "    function listen() {"
  , "      const scheme = location.protocol === \"https:\" ? \"wss\" : \"ws\";"
  , "      // The rows came over HTTP; the socket's own set-rows would resend them."
  , "      socket = new WebSocket(`${scheme}://${location.host}/ws?bootstrap=off`);"
      -- The other half of the wash, and the only one a reader can sit in for
      -- minutes: a page whose socket is gone goes on showing rows nothing can
      -- correct.  Set rather than stepped — a connection refused closes
      -- without ever opening — and the delay is what keeps a reconnect that
      -- costs one revalidation from dimming anything.
  , "      socket.onopen = () => {"
  , "        backoff = 1000; dot(\"live\"); wash.want(\"socket\", 0);"
  , "      };"
  , "      socket.onmessage = (e) => apply(JSON.parse(e.data));"
  , "      socket.onclose = (e) => {"
  , "        socket = null;"
  , "        dot(\"down\");"
  , "        wash.want(\"socket\", 1);"
  , "        // The columns moved, which SCHEMA.md's row ops cannot say: the"
  , "        // mount has to go.  Every other close — a backlog abandoned under"
  , "        // a write storm (`resync'), a restarted daemon, a dead network —"
  , "        // costs rows and nothing else, and the page stays where it was."
  , "        if (e && e.reason === \"view-changed\") remount(); else resync();"
  , "      };"
  , "    }"
  , "    // A lost socket costs rows and keeps the page.  Ask"
  , "    // /headlines for the applied query with the tag the last answer carried:"
  , "    // an unmoved store answers 304 and costs a header exchange, a moved one"
  , "    // answers with rows that drop into the table standing here.  The mount"
  , "    // stays through both — the sheet, the palette, the selection and the URL"
  , "    // with it — which is what makes an editor's write storm a row refresh"
  , "    // rather than the page reloading under a reader's hands."
  , "    function resync() {"
  , "      if (!table) { start(); return; }   // nothing mounted yet: this is a boot"
  , "      const asked = query;"
  , "      load(asking(asked), etag).then((a) => {"
  , "        // The close reason is not trusted for this: a daemon restarted while"
  , "        // this page was away had no socket to send `view-changed' down, and"
  , "        // its columns can still have moved."
  , "        if (a.view && !sameColumns(a.view.columns || [])) { remount(); return; }"
  , "        if (a.view && query === asked) paint(a);"
  , "        backoff = 1000;"
  , "        listen();"
  , "        append(\"ws\", \"info\", a.view ? \"reconnected · rows refreshed\" : \"reconnected\");"
  , "      }).catch((e) => {"
  , "        if (e.indexing) return indexing(e.indexing);"
  , "        // A newer query is already fetching and will paint what it gets;"
  , "        // the socket is all this call still owed."
  , "        if (e.name === \"AbortError\") { listen(); return; }"
  , "        quiet(e); again();"
  , "      });"
  , "    }"
  , "    // The columns are the one part of a view rows cannot carry, so they are"
  , "    // compared whole: the state column's badge palette rides inside them,"
  , "    // and a key-by-key check would let it move unnoticed."
  , "    const sameColumns = (next) => JSON.stringify(next) === JSON.stringify(cols);"
  , "    function again() {"
  , "      append(\"ws\", \"warn\", `disconnected · retrying in ${Math.round(backoff / 1000)}s`);"
  , "      setTimeout(resync, backoff);"
  , "      backoff = Math.min(backoff * 2, 30000);"
  , "    }"
  , "    // The server binds before it walks the tree, so the first fetch of a"
  , "    // cold daemon is a 503: show what it is doing and ask again in a second."
  , "    // A daemon that restarts under a live page lands here too, and comes"
  , "    // back through `resync' — the page it left is still on screen."
  , "    function indexing(b) {"
  , "      dot(\"wait\");"
  , "      append(\"boot\", \"info\","
      <> " `indexing … ${b.elapsed}s · the table opens when the walk lands`);"
  , "      setTimeout(resync, 1000);"
  , "    }"
    -- AFTER is what a canned view wants doing once its own rows are up, given
    -- the server's match count.  An argument rather than a variable this arms
    -- and disarms, so it belongs to the boot it was passed to and a boot that
    -- never lands cannot leave one behind for the next.
  , "    function start(after) {"
  , "      // A `?q=' in the address bar is a filtered view, and so is a bare"
  , "      // boot: the boot asks for whichever it is and `mount' opens the"
  , "      // filter showing it.  Every return through this door — a reload,"
  , "      // `view-changed', `g' — restores it the same way, since they all"
  , "      // re-fetch and re-mount; a reconnect never comes here at all."
  , "      // The default is written into the URL where it was injected, so what"
  , "      // the page shows and what the address bar says are the same query"
  , "      // from the first paint on."
  , "      const asked = (query = bootQuery());"
  , "      if (!params().has(\"q\")) remember(asked);"
      -- SWAP ON THE ANSWER.  A boot has nothing on screen, so it takes the
      -- first page it can get and pulls the rest in behind the painted table.
      -- A RE-APPLICATION has a whole table standing — `g', `a', `@', a pop, a
      -- `view-changed' remount — and asks for the WHOLE answer once, because a
      -- page-sized mount here replaces a complete table with a partial one and
      -- reflows the pager and the hint under the reader a moment later.  Under
      -- either, the rows that are up STAND until the new ones are in hand and
      -- the swap is one mount; the wash is what says they are on their way.
  , "      const swap = !!table;"
  , "      const narrow = asking(asked) + (asked ? \"&\" : \"?\");"
  , "      viewing(load(swap ? asking(asked) : `${narrow}limit=${PAGE}`)).then((a) => {"
  , "        mount(a.view);"
  , "        if (after) after(a.total);"
  , "        listen();"
  , "        // The rest behind the painted table: n/p, sort and materialize all"
  , "        // want the whole answer, and the renderer holds it without the DOM."
  , "        if (!swap && a.total > (a.view.rows || []).length)"
  , "          load(asking(asked))"
  , "            .then((b) => { if (table && query === asked) paint(b); arm(a.total); })"
  , "            .catch(quiet);"
  , "        else arm(a.total);"
  , "      }).catch((e) => {"
  , "        if (e.indexing) return indexing(e.indexing);"
  , "        dot(\"down\"); quiet(e); if (e.name !== \"AbortError\") again();"
  , "      });"
  , "    }"
    -- The first line of the log, and an ordinary one: the strip is never
    -- cleared, so the boot stays in the scrollback under everything that
    -- follows it rather than being a placeholder something has to take away.
  , "    append(\"boot\", \"info\", \"loading …\");"
  , "    start();"
  , "  </script>"
  ]

-- | The page a browser gets when DIR — the @--assets@ directory — holds no
-- renderer: what still works, and the two ways out.  Reachable under that flag
-- alone; drop it and 'embeddedRenderer' serves, so a default run never sees
-- this page.
assetsMissing :: ServeOptions -> FilePath -> Text
assetsMissing opts dir = page "" "glance — JSON only" $ T.unlines
  [ "  <h1>glance — JSON-only mode</h1>"
  , "  <p>No <code>" <> T.pack rendererAsset <> "</code> under <code>"
      <> escape (T.pack dir) <> "</code>, and <code>--assets</code> replaces the"
      <> " renderer this binary carries, so there is no table to render here."
      <> " The server is otherwise complete:</p>"
  , "  <p><code>curl -s localhost:" <> T.pack (show (soPort opts))
      <> "/headlines | jq '.rows | length'</code></p>"
  , "  <p>Drop <code>--assets</code> to get the built-in renderer back, or point"
      <> " it at a directory holding <code>" <> T.pack rendererAsset
      <> "</code> (the <code>web/</code> directory of a table-view checkout):</p>"
  , "  <p><code>glance serve --dir " <> escape (T.pack (soDir opts))
      <> " --assets /path/to/table-view/web</code></p>"
  ]

-- | BODY wrapped in a document titled TITLE, with HEAD opening the style block.
-- Styles inline, no asset but the renderer and whatever HEAD names on this same
-- server, dark and light both.
page :: Text -> Text -> Text -> Text
page head' title body = T.unlines
  [ "<!doctype html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>" <> escape title <> "</title>"
  , "<style>" <> (if T.null head' then "" else "\n" <> head')
  -- The palette is danneskjold, the theme this author's Emacs runs, in one set
  -- of custom properties the whole page reads.  Source:
  -- danneskjold-theme.el (../danneskjold-theme,
  -- github.com/rails-to-cosmos/danneskjold-theme).  Dark variant: `default'
  -- #FFFFFF on #000000, `region' #373D4F, `font-lock-comment-face' #A4C2EB,
  -- `company-tooltip' #21252B, `org-done'/success #B6E63E, `error' #E74C3C,
  -- `accent' #4CB5F5.  Light variant: #000000 on #FFFFFF, `light-comment'
  -- #7F8C8D, `light-surface' #F8F8FF, `light-golden' #FFD600 for selection,
  -- and `green-dark' #27AE60 where a white background needs the darker green.
  --
  -- @--g-border@ is the one value taken off palette, and it is the renderer's:
  -- @table-view.js@ draws every rule it owns in @--tv-border@, #E3E6EA light
  -- and #2a2d3d dark, so the page's own hairlines are now the same weight as
  -- the table's instead of a second, heavier chrome around it.  danneskjold's
  -- `vertical-border' #223959 and `light-dim' #BDC3C7 frame what they edge —
  -- 1.8:1 against their own ground, where these are 1.25:1 and 1.5:1.  Text
  -- contrast is untouched; only the rules recede.
  --
  -- Three ways, the renderer's own pattern: the media query is the default and
  -- @data-theme@ on the root pins it, which is the attribute the @theme:@
  -- selector writes and the renderer's overrides key off too.
  , "  :root{--glance-mono:" <> monoStack <> ";"
  , "    --g-bg:#FFFFFF;--g-fg:#000000;--g-border:#E3E6EA;--g-mute:#7F8C8D;"
  , "    --g-surface:#F8F8FF;--g-sel:#FFD600;--g-accent:#4CB5F5;"
  , "    --g-ok:#27AE60;--g-warn:#FFA500;--g-bad:#E74C3C}"
  , "  @media (prefers-color-scheme:dark){:root{--g-bg:#000000;--g-fg:#FFFFFF;"
  , "    --g-border:#2A2D3D;--g-mute:#A4C2EB;--g-surface:#21252B;--g-sel:#373D4F;"
  , "    --g-ok:#B6E63E}}"
  , "  :root[data-theme=\"light\"]{--g-bg:#FFFFFF;--g-fg:#000000;--g-border:#E3E6EA;"
  , "    --g-mute:#7F8C8D;--g-surface:#F8F8FF;--g-sel:#FFD600;--g-ok:#27AE60}"
  , "  :root[data-theme=\"dark\"]{--g-bg:#000000;--g-fg:#FFFFFF;--g-border:#2A2D3D;"
  , "    --g-mute:#A4C2EB;--g-surface:#21252B;--g-sel:#373D4F;--g-ok:#B6E63E}"
  , "  body{margin:0;font:14px/1.5 var(--glance-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);"
  -- One column, exactly the viewport tall: the table at the height it asks
  -- for, the log taking whatever that leaves, the key line last.  The page
  -- itself never scrolls — the two boxes that can outgrow their room scroll
  -- inside themselves — so the key line stays on screen and the fixed corner
  -- keeps its place with no scrollbar under it.
  , "    height:100vh;box-sizing:border-box;overflow:hidden;"
  -- The extra top padding is the fixed status corner's room: with no heading
  -- above it, the table's own top edge would otherwise sit under the corner.
  , "    padding:34px 24px 24px;display:flex;flex-direction:column;gap:14px}"
  , "  h1{font-size:16px;margin:0}"
  , "  p{margin:0;max-width:70ch}"
  , "  code{font-size:12px;color:var(--g-mute)}"
  -- The height that is LEFT, and none it cannot give back: the table takes the
  -- room the log gives up under its cap, and a window shorter than the column's
  -- parts takes the difference out of the table — which scrolls inside itself —
  -- rather than off the key line, which does not.
  , "  #app{flex:1 1 auto;min-height:0}"
  -- The renderer injects its own `.tv-root' font, and injects it from a script,
  -- so its rule lands after this element and ties on specificity.  One more
  -- selector step settles it, and leaves the size and the leading it set.
  , "  #app .tv-root{font-family:var(--glance-mono)}"
  -- The selected row is marked once, by the renderer's own secondary-highlight
  -- background.  The accent stripe this page drew over it was a second mark for
  -- the same fact, and the two disagreed about where the row began.
  -- The log is the table's own container repeated under it: same width,
  -- because this rule is the one place either width is set; same hairline,
  -- radius and surface tint as @.tv-root@.  It takes the height the table and
  -- the key line leave and scrolls inside it, so a long message cannot push
  -- either of them off the page.  The frame is resident: it holds its place
  -- with nothing to say, so an arriving event never moves the key line under it.
  , "  #app,#log{width:100%;box-sizing:border-box}"
  --
  -- SEVEN LINES and no more.  The strip still fills what is there for smaller
  -- content, and stops growing at seven of its own line boxes — past that a
  -- quiet page was giving half the window to a log nobody was reading, and the
  -- table took the loss.  The cap is computed rather than eyeballed: `#log' is
  -- 12px over the body's unitless 1.5, so a line box is 18px, and
  -- `box-sizing:border-box' puts the 6px padding twice and the 1px border twice
  -- inside the figure.  A reader who changes the font or the padding above has
  -- to change the arithmetic below, which is why it is spelled as the sum
  -- rather than as `140px'.
  --
  -- SEVEN is a constant here.  Making it a preference is #56's, which owns the
  -- settings-sheet field and the storage behind it.
  , "  #log{font-size:12px;color:var(--g-mute);padding:6px 10px;"
  , "    border:1px solid var(--g-border);border-radius:8px;"
  , "    max-height:calc(7 * 1.5 * 12px + 2 * 6px + 2 * 1px);"
  , "    background:var(--g-surface);flex:1 1 auto;overflow-y:auto}"
  -- A line is spans so its parts can be told apart at a glance: the stamp and
  -- the scope recede into the strip's own colour, the message carries the page's
  -- text colour, and the severity is the one part that changes colour — which is
  -- what makes a warning findable in a screenful of chatter.  The repeat counter
  -- is empty until a line repeats, and an empty span occupies nothing.
  , "  #log div>span{margin-right:6px}"
  , "  #log .lt{opacity:.65}"
  , "  #log .lm{color:var(--g-fg)}"
  , "  #log .warn .lv{color:var(--g-warn)}"
  , "  #log .error .lv{color:var(--g-bad)}"
  -- The resident key line, and the page's last: what can run, where the echo
  -- pill says what just did.  Slim and muted, so it reads as chrome rather
  -- than as content; one line that scrolls sideways instead of wrapping, so a
  -- narrow window cannot grow it into the table's room.
  , "  #kbd{flex:none;font-size:11px;color:var(--g-mute);white-space:nowrap;"
  , "    overflow-x:auto;padding:0 2px}"
  -- The status corner: the connection dot, clear of the table and out of the
  -- heading.  It is a READOUT and holds nothing focusable — the theme selector
  -- that used to sit beside the dot is a preference and lives with the rest of
  -- them, in the settings sheet.
  , "  #corner{position:fixed;top:12px;right:14px;z-index:3;display:flex;gap:6px;"
  , "    align-items:center;font-size:11px;color:var(--g-mute)}"
  , "  #corner:hover{color:var(--g-fg)}"
  , "  #dot{display:inline-block;width:7px;height:7px;border-radius:50%;"
  , "    background:var(--g-mute);transition:background .3s}"
  , "  #dot.live{background:var(--g-ok)}"
  , "  #dot.wait{background:var(--g-warn)}"
  , "  #dot.down{background:var(--g-mute)}"
  -- The sheet is the one place the author's Emacs font is asked for by name:
  -- the subtree reads there as it reads in the buffer it came out of.  The
  -- colours are the page's, which are already danneskjold's.
  --
  -- The backdrop is a direct child of the body, which is neither transformed
  -- nor positioned, so these two levels are the root stacking context's and
  -- clear the renderer's chrome outright — its sticky @th@ carries
  -- @z-index:1@ and its completion list @5@, and an unnumbered backdrop paints
  -- under both.  The sheet is a flex item, so its own level would apply
  -- anyway; @position@ says so without relying on that.
  -- The two overlays share the backdrop and the two levels: the sheet is the
  -- subtree's and the prompt is a command's, and a reader has one of them open
  -- at a time.  The prompt sits high rather than centred — a list that grows
  -- downward should not move the line above it.
  , "  #modal,#prompt,#config,#links,#tags{--dk-mono:\"Hack\", var(--glance-mono);"
  , "    display:none;position:fixed;inset:0;z-index:100;padding:24px;background:#0009;"
  , "    align-items:center;justify-content:center}"
  , "  #modal.on,#prompt.on,#config.on,#links.on,#tags.on{display:flex}"
  , "  #prompt{align-items:flex-start;padding-top:15vh}"
  -- Four fifths of the window, in both directions: two panes of monospace want
  -- the room, and the fifth left over is what says there is a table under this
  -- rather than a page of its own.  The `min' keeps it inside the backdrop's
  -- padding on a window too narrow for the share to fit.
  , "  #sheet{display:flex;flex-direction:column;gap:8px;padding:14px;border-radius:6px;"
  , "    position:relative;z-index:101;"
  , "    width:min(80vw,100%);height:min(80vh,100%);font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #mhead{display:flex;justify-content:space-between;gap:12px;font-size:12px}"
  , "  #mfile{color:var(--g-mute)}"
  , "  #mnote{text-align:right;color:var(--g-ok)}"
  , "  #mnote.syncing{color:var(--g-mute)}"
  , "  #mnote.conflict,#mnote.error{color:var(--g-bad)}"
  -- Two panes, wrapping rather than querying: a window too narrow to hold both
  -- side by side puts the panel under the text, which is the same answer a
  -- width breakpoint would give and costs no second place to keep it.  C-c '
  -- takes the panel off the sheet outright, and then the text has the width.
  , "  #mpanes{flex:1;min-height:0;display:flex;flex-wrap:wrap;gap:10px}"
      -- The panes are one frame language, so they wear ONE radius: the panel's
      -- comes from `.tv-root' and is 8px, which is the page's own — the log
      -- strip and the sheet's logbook already wear it — so the textarea takes
      -- it too rather than being the one corner cut tighter than its neighbour.
  , "  #mtext{flex:2 1 320px;min-width:0;font:12px/1.5 var(--dk-mono);padding:8px;"
  , "    border-radius:8px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit;resize:none}"
  , "  #mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- ONE FOCUS LANGUAGE ACROSS THE SHEET: whichever pane holds the keys says so
  -- on its own FRAME, in the accent, and neither pane wears it otherwise.  The
  -- panes are two different kinds of thing — a textarea takes a real focus, the
  -- panel holds the keys with nothing focused at all — so the browser's own
  -- ring can only ever dress one of them, and a reader crossing with TAB would
  -- watch the mark disappear.  Declared here rather than left to the UA so the
  -- two are the same treatment rather than two that resemble each other; the
  -- ring goes with it, the border being the mark.
  , "  #mtext:focus{outline:none;border-color:var(--g-accent)}"
  -- The property panel: a table-view MOUNT and the overlay an open row wears.
  -- The rows, the stripe, the cursor and the flag wash are the renderer's — the
  -- whole reason the panel is a mount is that this page keeps no second answer
  -- to any of them — so what is left here is the PANE: how much room it takes,
  -- that it is the overlay's positioning parent, and that raw mode takes it off
  -- the sheet.
  --
  -- No frame of its own: `.tv-root' brings the hairline and the shared radius,
  -- which is the same frame the panel drew for itself before.
  , "  #mprops{flex:1 1 240px;min-width:0;min-height:0;position:relative;"
  , "    overflow:hidden;display:flex;flex-direction:column}"
  , "  #mptable{flex:1;min-height:0;display:flex}"
  -- The sheet's own face, the way `#app .tv-root' is the page's: one selector
  -- step past the renderer's injected rule, and the sheet's monospace rather
  -- than the page's, since both panes of a sheet read as one.
  , "  #mptable .tv-root,#ltable .tv-root,#ttable .tv-root{flex:1;min-width:0;"
  , "    font-family:var(--dk-mono)}"
  -- The panel's half of that language.  The frame is `.tv-root'\''s — the mount
  -- brings it — and `#mprops.on' is the panel holding the keys, which is the
  -- same state `pnav' reads.
  , "  #mprops.on .tv-root{border-color:var(--g-accent)}"
  -- The mark column is the renderer's PRICE for the flag wash — `isFlagged' is
  -- gated on `marks' — and nothing in the panel reads a mark.  So the gutter is
  -- left standing, since it carries the flag's second channel (an inset edge
  -- the renderer draws on this cell, which is what keeps a flag readable under
  , "  #sheet.raw #mprops{display:none}"
  -- The open row's two fields, laid over the row they belong to.  Absolute
  -- because the row underneath is virtualized: the mount rewrites its own rows
  -- as it scrolls, so an edit that lived inside one would be thrown away by the
  -- next frame.  `top' and `height' are the glue's, off the row's own box.
  --
  -- The rhythm is the table's, so the fields land on the text they replace:
  -- `5px 12px' per cell is `.tv-table td' exactly, and the column split is the
  -- renderer's two columns.  No z-index: it is a positioned LATER sibling of the
  -- mount, so paint order puts it over the rows already, and the page's four
  -- bands stay four.
  --
  -- The tags popup's rename overlay is the same thing over ONE cell, so the two
  -- share every declaration but the geometry: `#pedit' spans its row and
  -- `#tedit' is laid over the tag cell alone, whose left and width the glue
  -- reads off that cell.  One edit vocabulary across the page means one look.
  , "  #pedit,#tedit{display:none;position:absolute;background:var(--g-sel)}"
  , "  #pedit{left:0;right:0}"
  , "  #pedit.on,#tedit.on{display:flex;align-items:center}"
      -- The mount's own cell metrics, so the fields land on the text they
      -- replace: `.tv-table td' is `5px 12px' at the root's 13px/1.5, and a
      -- coarse pointer stretches the row rather than the padding.
  , "  #pedit input,#tedit input{font:13px/1.5 var(--dk-mono);padding:5px 12px;"
  , "    border:none;border-bottom:1px solid transparent;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
  , "  #pedit input:focus,#tedit input:focus{outline:none;"
  , "    border-bottom-color:var(--g-border)}"
  , "  #pedit input::selection,#tedit input::selection{background:var(--g-sel);"
  , "    color:var(--g-fg)}"
  , "  #tname{flex:1 1 auto}"
  -- A planning key is org's rather than the author's, so its field is muted and
  -- takes no typing — a label with a caret in it.
  , "  #pkey{flex:1 1 40%}"
  , "  #pkey[readonly]{color:var(--g-mute)}"
  , "  #pval{flex:2 1 50%}"
  -- The logbook: full width under both panes, muted, read-only and out of the
  -- tab order — it is the server's, and there is nothing here to press.
      -- The same dress as the page's log strip: 12px muted on the surface
      -- tint, one hairline, the shared 8px radius, 6px 10px padding.
  , "  #mlog{display:none;flex:0 0 auto;max-height:22vh;overflow:auto;margin:0;"
  , "    font-size:12px;font-family:var(--dk-mono);color:var(--g-mute);"
  , "    white-space:pre-wrap;padding:6px 10px;background:var(--g-surface);"
  , "    border:1px solid var(--g-border);border-radius:8px}"
  , "  #mlog.on{display:block}"
  , "  #sheet.raw #mlog{display:none}"
  -- The value palette and the link popup are ONE BOX: a heading naming what is
  -- being decided, a body, and a foot naming the keys the body cannot draw for
  -- itself.  Declared once so a restyle is one edit and a fifth overlay joins by
  -- adding a selector, which is what `#modal,#prompt,#config,#links' above does
  -- for the band.  They differ in WIDTH alone — the palette holds a resolution
  -- table three columns wide and no wider, the popup holds three real columns
  -- and a URL in one of them.  The palette's field is its fallback mode's and is
  -- hidden until `/' asks for it; in letter mode there is nothing to type.
  , "  #pbox,#lbox,#tbox{display:flex;flex-direction:column;gap:6px;padding:10px;"
  , "    border-radius:6px;position:relative;z-index:101;"
  , "    font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #pbox,#tbox{width:min(560px,100%)}"
  , "  #lbox{width:min(760px,100%)}"
  , "  #phead,#lhead,#thead{font-size:12px;color:var(--g-mute)}"
  , "  #pfoot,#lfoot,#tfoot,#cfoot{font-size:11px;color:var(--g-mute)}"
  , "  #pinput{font:12px/1.5 var(--dk-mono);padding:5px 7px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  , "  #pbox:not(.narrow) #pinput{display:none}"
  , "  #plist{max-height:40vh;overflow-y:auto;font-size:12px}"
  -- The resolution table: source, active, inactive.  A row is a source and the
  -- hairline between two of them is that row's own top border — the table's
  -- border language, where the old flat list needed a divider element of its
  -- own.  The source column is the muted small lowercase a tag wears
  -- everywhere else on this page, whether it holds a tag or one of the reserved
  -- labels; a long tag breaks rather than widening the box.  `*empty*' spans.
  , "  .pr{display:grid;grid-template-columns:6.5em 1fr 1fr;gap:4px 8px;padding:4px 7px}"
  , "  .pr+.pr{border-top:1px solid var(--g-border)}"
  , "  .ph,.ps{font-size:11px;color:var(--g-mute)}"
  , "  .ps{overflow-wrap:anywhere}"
  , "  .pc{display:flex;flex-wrap:wrap;gap:2px 10px}"
  , "  .pr.pm{grid-template-columns:1fr}"
  , "  .pnone{padding:4px 7px;color:var(--g-mute)}"
  -- An entry is its key token and its word.  The token is boxed in the accent
  -- so the letters read as a column of their own, and the word keeps its badge
  -- colour so a keyword looks the same here as it does in the table; the
  -- claimed letter is marked INSIDE the word — bold and underlined, the
  -- underline in that state's own badge hue — which is what says why DELEGATED
  -- answers to `e'.  There is no key-token column: an entry IS its keyword, and
  -- a boxed letter beside it said the same thing twice while pushing every word
  -- rightwards.  An entry that claimed nothing is drawn bare and is reachable
  -- through `/' alone; a reader learns "unmarked means untyped" from the
  -- marked ones beside it.  The padding is the FLAT list's, where an entry is a
  -- row of its own; inside a cell the gaps do that work.
  , "  .pe{display:flex;align-items:center;gap:6px;border-radius:4px}"
  , "  #plist>.pe{padding:3px 7px}"
      -- The one entry that keeps a token: `*empty*' answers to DEL, which has
      -- no position inside a word to be marked at.
  , "  .pk{flex:none;min-width:1.6em;text-align:center;padding:1px 5px;border-radius:3px;"
  , "    font:11px/1.4 var(--dk-mono);"
  , "    border:1px solid var(--g-accent);color:var(--g-accent)}"
      -- Weight AND a rule under it, and the rule takes the keyword's own badge
      -- hue (written inline per entry, since only the entry knows it): two
      -- marks rather than one, because the token column that used to carry the
      -- letter is gone and this is now the whole of what says which key
      -- commits.  Thick enough to read at 11px and offset clear of the
      -- descenders, which is what made a rule chrome the first time.
  , "  .pw b{font-weight:700;text-decoration:underline;"
  , "    text-decoration-thickness:2px;text-underline-offset:2px}"
  , "  .pm .pw{font-style:italic;color:var(--g-mute)}"
  -- The muted aside beside an entry, truncated: the tag palette's partial
  -- count, and the only thing that goes in it.  It is a note ABOUT the entry
  -- rather than part of it, so it recedes and gives the word the left edge.
  , "  .pt{flex:1 1 0;min-width:0;overflow:hidden;text-overflow:ellipsis;"
  , "    white-space:nowrap;text-align:right;font-size:11px;color:var(--g-mute)}"
  -- The fallback's cursor row wears the page's selection, which in the light
  -- theme is a bright yellow — a badge hue written inline reads badly on it,
  -- and this is the one place a declaration has to beat one.
  , "  #plist .pat{background:var(--g-sel);color:var(--g-fg)}"
  , "  #plist .pat .pw{color:var(--g-fg)!important}"
  -- The settings sheet, third in the same two bands: panels down a column,
  -- each a header over its rows.  High rather than centred, since the panels
  -- grow downward and the header over them should not move when they do.
  , "  #config{align-items:flex-start;padding-top:8vh}"
  , "  #cbox{display:flex;flex-direction:column;gap:10px;padding:14px;border-radius:6px;"
  , "    position:relative;z-index:101;"
  , "    width:min(720px,100%);max-height:84vh;overflow-y:auto;font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #chead{display:flex;justify-content:space-between;gap:12px;font-size:12px}"
  , "  #ctitle{color:var(--g-mute)}"
  , "  #cnote{text-align:right;color:var(--g-ok)}"
  , "  #cnote.syncing{color:var(--g-mute)}"
  , "  #cnote.conflict,#cnote.error{color:var(--g-bad)}"
  -- A panel: a header naming it, then a stack of rows.  The hairline is the
  -- header's own top border, the same border language the value palette's
  -- source rows use, and the first panel is under the sheet's own head so it
  -- wears none.  Panel bodies are matched by CLASS — a panel added to
  -- `SECTIONS' is laid out without an entry here.
  , "  #csecs{display:flex;flex-direction:column;gap:14px}"
  , "  .csec,.cpart{display:flex;flex-direction:column;gap:8px}"
  , "  .chdr{font-size:11px;letter-spacing:.08em;text-transform:uppercase;"
  , "    color:var(--g-mute);border-top:1px solid var(--g-border);padding-top:8px}"
  , "  .csec:first-child .chdr{border-top:none;padding-top:0}"
  , "  .crow{display:flex;flex-direction:column;gap:4px}"
  -- A path is long and has nowhere to wrap, so it is told it may break
  -- anywhere rather than widening the sheet past the viewport.
  , "  .clab{font-size:11px;color:var(--g-mute);overflow-wrap:anywhere}"
  -- Every control the sheet edits with, in ONE rule: the layer boxes, the two
  -- general fields and the theme select, which wears `cview' for exactly this.
  -- The coarse-pointer block names the same two classes, so a control that
  -- takes its look here takes its 16px there and iOS never zooms in on it.
  , "  .ctext,.cview{font:12px/1.5 var(--dk-mono);padding:6px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  , "  .ctext{height:3.4em;resize:vertical}"
  , "  .ctext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- The select is the one control with a popup of its own, which has to paint
  -- on something, and the one that would stretch to the sheet's width.
  , "  #themesel{background:var(--g-bg);align-self:flex-start;min-width:10em}"
  , "  #themesel option{background:var(--g-bg);color:var(--g-fg)}"
  -- What the server said about the last write to that layer, and nothing when
  -- it said nothing.
  , "  .cerr{font-size:11px;color:var(--g-bad)}"
  , "  .cerr:empty{display:none}"
  , "  #ceff{font-size:12px;padding-top:8px;border-top:1px solid var(--g-border)}"
  -- The link popup, fourth in the same two bands and sharing the palette's box
  -- above.  High rather than centred, for the palette's reason — a list that
  -- grows downward should not move the line above it — and the table is capped
  -- and scrolls inside itself, since a row can point at a great many places.
  --
  -- The mount brings its own frame (`.tv-root''s hairline and radius), so the
  -- box declares none of the table's look past the sheet's monospace, which is
  -- the one rule `#mptable' takes too and the one rule they share.
  , "  #links,#tags{align-items:flex-start;padding-top:12vh}"
  , "  #ltable,#ttable{max-height:52vh;min-height:0;display:flex;overflow:hidden}"
  -- The tags popup, fifth in the same two bands and in the same box.  The pane
  -- is the rename overlay's positioning parent, which is the one thing this one
  -- needs and the link popup does not — `#mprops' does the same job in the
  -- sheet, and for the same reason: the mount rewrites its rows as it scrolls,
  -- so an edit that lived inside one would be thrown away by the next frame.
  , "  #tpane{position:relative;min-height:0;display:flex;flex-direction:column;"
  , "    overflow:hidden}"
  -- The gear is the coarse pointer's only way in, so a fine pointer never sees
  -- it: the rule that shows it is in the one media block below.
  , "  #gear{display:none}"
  -- THE WASH.  One class on the document element, over the table and the whole
  -- modal band: a sheet open on rows that have gone stale is stale with them,
  -- so the overlays go under the same wash rather than floating over it looking
  -- current.  Eased, so the state arrives as a change rather than as a flicker,
  -- and the transition sits on the elements themselves so coming back is eased
  -- too.
  --
  -- ONE PROPERTY, and it is `opacity'.  Never BLUR — a reader has to be able to
  -- read a stale row while its replacement is on the way, and blur is what takes
  -- that away.  Never `filter' either, which is the cross-repo constraint here:
  -- ANY filter makes its element the containing block for `position:fixed'
  -- descendants, and the renderer's summoned filter palette is one — a
  -- `.tv-veil' inside `#app', which would stop covering the viewport and start
  -- being clipped by `.tv-root''s own `overflow:hidden' the moment a fetch went
  -- past its grace.  `opacity' creates a stacking context and no containing
  -- block, so it is the one declaration that dims everything and re-anchors
  -- nothing; over the page's own ground it takes the colour out of a badge as
  -- it goes.
  --
  -- The corner, the event strip and the key line are EXEMPT by omission: they
  -- are where a reader finds out what the page is waiting on, and dimming the
  -- explanation with the thing it explains leaves the page saying nothing.
  , "  #app,#modal,#prompt,#config,#links,#tags{transition:opacity .18s ease}"
  , "  html.stale #app,html.stale #modal,html.stale #prompt,html.stale #config,"
  , "  html.stale #links,html.stale #tags{opacity:.55}"
  -- The echo area and the status corner are the page's, and the backdrop dims
  -- the page: both sit under it (2 and 3 against the modal's 100) and grey out
  -- with everything else while the sheet is open.  They stay above the table.
  , "  #echo{position:fixed;right:14px;bottom:12px;z-index:2;padding:4px 10px;"
  , "    border-radius:999px;border:1px solid var(--g-border);font-size:12px;"
  , "    white-space:pre;background:var(--g-surface);color:var(--g-fg);opacity:0;"
  , "    transition:opacity .35s;pointer-events:none}"
  -- Touch.  Keyboard-first holds wherever there are keys; a coarse pointer is
  -- where they cannot reach, so the filter earns the one tap target on the
  -- page.  The chip row is what a palette-mode page carries of the filter, and
  -- it becomes that target: 44px of it, and a word saying so while no chip has
  -- filled it.  The renderer hides an empty row with an inline @display:none@,
  -- which only @!important@ outranks from a stylesheet.  Every rule is inside
  -- the query, so a mouse sees exactly what it saw before.
  --
  -- iOS zooms the page in on a focused field under 16px and does not zoom back
  -- out; the sheet's textarea and its property fields are the shell's own, and
  -- the renderer's input is the renderer's.  The panes stack here whatever the
  -- width: a thumb wants the text full-width and the drawer under it, where the
  -- wrap rule would keep them side by side on a wide tablet.
  , "  @media (pointer:coarse){"
  , "    #app .tv-chips{min-height:44px;cursor:pointer}"
  , "    #app .tv-chips:empty{display:flex!important;align-items:center}"
  , "    #app .tv-chips:empty::after{content:\"filter …\";color:var(--g-mute);"
  , "      font-size:12px}"
  , "    #mpanes{flex-direction:column}"
  -- The settings gear: the one control a coarse pointer gets that a mouse does
  -- not, since `C-c C-,' is the way in everywhere there are keys.  44px, like
  -- the chip row.
  , "    #gear{display:inline-block;font:inherit;font-family:var(--glance-mono);"
  , "      min-width:44px;min-height:44px;border-radius:4px;"
  , "      border:1px solid var(--g-border);background:var(--g-bg);color:inherit}"
  , "    #mtext,#pinput,#pedit input,#tedit input,.ctext,.cview{font-size:16px}}"
  , "</style>"
  -- The stored theme, applied before anything paints: a page that renders in
  -- the wrong one and corrects itself a frame later is a flash the selector
  -- exists to avoid.  One line, so the suite's glue extractor still finds the
  -- one inline script it checks.
  , "<script>" <> themeBoot <> "</script>"
  , "</head>"
  , "<body>"
  , body <> "</body>"
  , "</html>"
  ]

-- | The head script: the remembered theme pinned on the root element ahead of
-- the first paint.  @auto@ and anything unrecognised leave the attribute off,
-- which is the media query's business.
themeBoot :: Text
themeBoot = T.concat
  [ "try{var t=localStorage.getItem(\"glance-theme\");"
  , "if(t===\"light\"||t===\"dark\")document.documentElement.dataset.theme=t}"
  , "catch(e){}" ]

-- | TEXT as a JavaScript string literal, escaped through the JSON encoder so
-- the glue can carry a value the tree supplies without a quoting rule of its
-- own.  The angle brackets go the way 'keyBindingsJSON' sends them: the literal
-- sits inside a @\<script\>@ element, where @\<\/@ closes it whatever the JSON
-- says.
jsonText :: Text -> Text
jsonText = jsonLiteral . toJSON

-- | XS as a JavaScript array literal, escaped the same way.
jsonList :: [Text] -> Text
jsonList = jsonLiteral . toJSON

jsonLiteral :: Value -> Text
jsonLiteral = T.replace "<" "\\u003c" . T.replace ">" "\\u003e"
            . TE.decodeUtf8 . BL.toStrict . encode

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

-- | STATUS with HEADERS and BODY, the body's length among them.  Warp writes a
-- @Content-Length@ too, but downstream of every middleware; the gzip threshold
-- reads that header off the response, so a body too small to be worth
-- compressing is only recognisable as one when the length is written here.
-- 'Network.Wai.Middleware.Gzip.gzip' drops it again on the responses it does
-- compress.
sized :: Status -> [Header] -> BL.ByteString -> Response
sized status headers body =
  responseLBS status ((hContentLength, BSC.pack (show (BL.length body))) : headers) body

-- | STATUS with FIELDS as its JSON body.  Hand-built the way the view document
-- is: these objects are a contract with the shell, not a projection of a type.
jsonResponse :: Status -> [Pair] -> Response
jsonResponse status fields = sized status [jsonType] (encode (object fields))

-- | STATUS carrying MSG as @{"error": …}@, so a refusal parses the way the
-- success it replaces does.
jsonError :: Status -> Text -> Response
jsonError status msg = jsonResponse status ["error" .= msg]

html :: Text -> Response
html body = sized status200 [(hContentType, "text/html; charset=utf-8")] (utf8 body)

-- | STATUS with MSG as its whole body — errors read in a terminal as well as
-- in a browser.
plain :: Status -> Text -> Response
plain status msg =
  sized status [(hContentType, "text/plain; charset=utf-8")] (utf8 (msg <> "\n"))

utf8 :: Text -> BL.ByteString
utf8 = BL.fromStrict . TE.encodeUtf8
