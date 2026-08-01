-- | The M1 web layer: headlines out of a directory, into a browser tab, and
-- kept current there.
--
-- This component's build-depends names the public @glance@ library and the
-- HTTP packages.  @glance-internal@ is absent, so @Data.Org.*@ is out of scope
-- here and reaching for it means writing the dependency down where anyone
-- reading the stanza sees it.  That is the facade invariant
-- (docs/invariants.md, Architecture), kept where the solver can check it.
--
-- Six routes: @GET \/headlines@ is the view JSON, @GET \/@ a demo shell that
-- fetches it, @GET \/ws@ the live row stream, @GET \/NAME@ an asset out of the
-- @--assets@ directory, @\/headline@ the materialize round-trip — @GET@ for
-- one headline's raw subtree, @POST@ to write an edited one back — and
-- @POST \/command@ the structured writes, which name rows and let the server
-- compute the spans.  The view's field set is the contract
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
-- glue, one script by name, and a font only when the assets directory has one
-- (docs\/invariants.md).
--
-- The listener binds 127.0.0.1 and nothing else.  Read, write and automate
-- tiers arrive at S7 (docs\/plan-org-console-web.md); until an unauthenticated
-- connection is a read-only one by construction, the loopback interface is the
-- whole access-control story.
module Glance.Web ( ServeOptions (..)
                  , defaultAssetsDir
                  , defaultPort
                  , application
                  , bootstrapWanted
                  , serve
                  , serveWith
                  , viewTitleFor
                  ) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (filterM, forever, join, unless, void, when)
import Data.Aeson ( Value, eitherDecode', encode, object, toJSON, withObject
                  , (.:), (.:?), (.=) )
import Data.Aeson.Types (Pair, parseEither)
import Data.Bifunctor (first)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
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
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

import Glance.Query ( ConfigLayerFile (..), ConfigLayers (clDirs)
                    , HeadlineParts (..)
                    , HeadlineRecord (hrDigest, hrFile, hrId, hrSubtree)
                    , IdCollision (..), QueryResult (..), Span (spanEnd, spanStart)
                    , TodoKeywords (..), ViewOrder (..), WalkOptions (..)
                    , WriteFailure (..), archiveEdits, archived, builtinFilter
                    , configDirIn, configEdits, defaultFilter, defaultFilterOf
                    , headlineParts, orderedForView, planningKeywords
                    , readConfigLayers, readsAsTimestamp, recomposedSubtree
                    , replaceSpans, setStateEdits, subtreeText, todoLines
                    , viewJSONTextWith )
import Glance.Web.Filter (archiveKey, matchesFilter, namesArchive)
import Glance.Web.Store ( Client, Frame (ViewChanged), Hub, LoadState (..)
                        , Store (stConfig, stGen, stPrint), finishLoading, frameText
                        , hubLoad, hubStore, loadStoreWith, newLoadingHub, nextFrame
                        , storeHeadline, storeKeywords, storeResult, storeTags
                        , subscribe, unsubscribe )
import Glance.Web.Watch (watchOrgTree)

-- Options

-- | What one server serves.
data ServeOptions = ServeOptions
  { soDir     :: !FilePath  -- ^ org root, walked once at startup and watched after.
  , soPort    :: !Int       -- ^ loopback port to listen on.
  , soAssets  :: !FilePath  -- ^ directory holding @table-view.js@; see 'defaultAssetsDir'.
  , soDerived :: !Bool      -- ^ serve org-glance's mirror directories too; see 'Data.Org.Walk'.
  } deriving (Eq, Show)

-- | How OPTS wants the tree walked, for the load and for the watch alike: a
-- file the walk passed over must not come back through an inotify event.
walkFor :: ServeOptions -> WalkOptions
walkFor opts = WalkOptions { woIncludeDerived = soDerived opts }

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

-- | Serve OPTS until killed.
serve :: ServeOptions -> IO ()
serve opts = serveWith opts (pure ())

-- | Serve OPTS until killed, running LISTENING once the socket is bound and
-- accepting.  A missing org directory fails here rather than per request: the
-- operator learns at startup, not from a 500.
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
serveWith :: ServeOptions -> IO () -> IO ()
serveWith opts listening = do
  ok <- doesDirectoryExist (soDir opts)
  unless ok (die ("glance serve: no such directory: " <> soDir opts))
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
      mapM_ putStrLn
        [ "glance serve — http://127.0.0.1:" <> show (soPort opts) <> "/"
        , "  org dir: " <> soDir opts
        , "  assets:  " <> soAssets opts <> if assets then "" else "  (missing — /headlines only)"
        , "  live:    ws://127.0.0.1:" <> show (soPort opts) <> "/ws, watching " <> soDir opts
        , "  bound to 127.0.0.1; no auth tier before S7."
        , "  indexing — /headlines, /headline and /ws answer 503 until the walk lands."
        ]
      -- Redirected stdout is block-buffered, and the process then blocks in warp
      -- until it is killed: without this the banner never reaches the log.
      hFlush stdout
      void (forkIO listening)

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
    method  = requestMethod request
    reading = method `elem` [methodGet, methodHead]
    -- The named routes: the path, whether the answer comes out of the store —
    -- which is what makes it a 503 while the walk runs, whatever the method —
    -- and the handler.  Everything else is an asset name or a miss.
    named =
      [ ([],            False, readOnly (shellPage opts hub))
      , (["headlines"], True,  readOnly (headlines opts hub request))
      , (["headline"],  True,  headline)
      , (["command"],   True,  commandRoute)
      , (["config"],    True,  configRoute)
      , (["ws"],        True,  readOnly (pure (plain status400 wsHint)))
      ]
    route = case [ (needs, act) | (path, needs, act) <- named, path == pathInfo request ] of
      ((needs, act) : _) -> do
        load <- readTVarIO (hubLoad hub)
        case load of
          Loading since | needs -> indexing since
          _ready                -> act
      _noSuchRoute -> fallback
    -- @/headline@ is the one route that writes; a write anywhere else is a 405
    -- naming it.
    headline | reading              = materialize hub (queryId request)
             | method == methodPost = commit hub (queryId request) request
             | otherwise            = pure (jsonError status405 "/headline takes GET and POST")
    -- @/command@ writes and only writes: there is nothing to read back, since
    -- the rows a command moved arrive over the socket like any other edit.
    commandRoute | method == methodPost = runCommand hub request
                 | otherwise            = pure (jsonError status405 "/command takes POST")
    -- @/config@ is the settings sheet's pair, and reads and writes like
    -- @/headline@: the layers with their digests, and one of them back.
    configRoute | reading              = configView opts hub
                | method == methodPost = configWrite opts hub request
                | otherwise            = pure (jsonError status405 "/config takes GET and POST")
    readOnly act | reading   = act
                 | otherwise = pure (plain status405 writeHint)
    -- Every one-segment path lands on the assets directory, so the miss below
    -- it doubles as the route list.
    fallback = readOnly $ case pathInfo request of
      [name] | safeName name -> asset opts (T.unpack name)
      _other                 -> pure (plain status404 notFound)
    wsHint    = "/ws is a websocket endpoint; connect with Upgrade: websocket"
    writeHint = "method not allowed; POST /headline?id=… and POST /command write"
    notFound  = "not found: /, /headlines, /headline, /command, /config, /ws, or an asset name"

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
-- columns and over every org tag the store carries, free text, negation, and
-- same-key predicates combining by the field's arity, a single-valued one
-- ORing and a multi-valued one ANDing ('Glance.Web.Filter') — @limit@ a page
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
-- exactly as @-archive:@ would be: any query naming the @archive@ key at all
-- turns it off ('Glance.Web.Filter.namesArchive'), and @X-Glance-Archived@
-- reports how many rows the exclusion took.  The vocabulary a query is parsed
-- against stays the WHOLE store's, so hiding the rows never hides the key that
-- reaches them.
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
-- they landed.  Walk order is document order — file by file, headline by
-- headline down each — which is what makes the @depth@ each row carries
-- readable as a tree.  Anything else under @order=@ is a 400; @order=scheduled@
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
  Right (q, limit, offset, order) -> do
    st <- readTVarIO (hubStore hub)
    let tag = etagOf st
    if tag `elem` ifNoneMatch request
      then pure (responseLBS status304 (cacheHeaders tag) "")
      else do
        let qr      = storeResult st
            -- The vocabulary is the whole store's, filtered set or not: the
            -- exclusion below must not be able to take `archive:' out of the
            -- keys a query may name, or the only way back to what it hides
            -- would be gone with it.
            vocab   = storeTags st
            asked   = filter (matchesFilter vocab q) (qrRecords qr)
            matched = if hiding then filter (not . archived) asked else asked
            -- A tree with nothing archived in it pays no pass over the answer:
            -- the vocabulary already knows whether the tag is anywhere, and
            -- without it the query could not have named the key either.
            hiding  = archiveKey `elem` vocab && not (namesArchive vocab q)
            hidden  = length asked - length matched
            total   = length matched
            shown   = maybe matched
                            (\n -> take n (drop offset (orderedForView order matched))) limit
            hasNext = maybe False (\n -> offset + n < total) limit
            body    = TLE.encodeUtf8
                        (viewJSONTextWith order (viewTitleFor dir) (storeKeywords st) shown)
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

-- | @q@, @limit@, @offset@ and @order@ out of REQUEST's query string, or what
-- is wrong with one of them.  An absent parameter is its default — no filter,
-- no limit, the top of the set, the view's declared sort — and a present one
-- that is not a number is a 400 rather than a silent fallback to it, since a
-- mistyped page size that quietly serves the whole store looks like a working
-- request.  @order@ is spelled out for the same reason: a misspelling that
-- silently served the sorted view would look exactly like a working one.
pageParams :: Request -> Either Text (Text, Maybe Int, Int, ViewOrder)
pageParams request = do
  q      <- maybe (Right "") text (raw "q")
  limit  <- traverse count (raw "limit")
  offset <- maybe (Right 0) count (raw "offset")
  order  <- maybe (Right ScheduledOrder) ordering (raw "order")
  case limit of
    Just n | n > limitCap -> Left ("limit is at most " <> T.pack (show limitCap)
                                     <> "; page with offset for more")
    _within                -> Right (q, limit, offset, order)
  where
    -- Experimental, and the only way to reach 'DocumentOrder'.  @scheduled@
    -- names the default so a client can be explicit about the ordinary case.
    ordering named = do
      t <- text named
      case t of
        "document"  -> Right DocumentOrder
        "scheduled" -> Right ScheduledOrder
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
-- @FILE:START@ — slashes and a colon — so a path segment would have to be
-- percent-encoded by every client and decoded here, while WAI has already
-- decoded the query string by the time this runs.
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
    Right (r, digest, org) -> do
      written <- replaceSpans (hrFile r) digest [(hrSubtree r, org)]
      pure $ case written of
        Right fresh              -> jsonResponse status200 ["digest" .= fresh]
        Left (WriteDrift onDisk) -> conflict "drift" onDisk rewritten
        Left (WriteRefused why)  -> jsonError status500 why

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

again :: Text
again = "; materialize it again and re-apply the edit"

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
          -- write rides in the same request.
          , "filter"   .= servedFilter layers
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
servedFilter layers =
  fromMaybe builtinFilter
    (listToMaybe [ f | l <- layers, Nothing <- [lfTag l], Just f <- [defaultFilterOf (lfText l)] ])

layerJSON :: ConfigLayerFile -> Value
layerJSON f = object
  [ "path"   .= lfPath f
  , "tag"    .= lfTag f
  , "lines"  .= todoLines (lfText f)
  , "digest" .= lfDigest f
  ]

keywordsJSON :: TodoKeywords -> Value
keywordsJSON kw = object ["active" .= tkActive kw, "inactive" .= tkInactive kw]

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
writeLayer :: [FilePath] -> (Text, [Text], Maybe Text, Text) -> IO Response
writeLayer dirs (path, asked, want, digest) = do
  layers <- readConfigLayers dirs
  case find ((== path) . T.pack . lfPath) layers of
    Nothing -> pure (jsonError status400 (noSuchLayer path layers))
    Just f  -> case configEdits (lfText f) asked (systemOnly f) of
      Left why    -> pure (jsonError status400 why)
      Right edits -> answer <$> replaceSpans (lfPath f) digest edits
  where
    -- The default view is the SYSTEM layer's line and no other's, so a tag
    -- layer's write leaves it alone whatever the request said.
    systemOnly f = maybe want (const Nothing) (lfTag f)
    answer written = case written of
      Right fresh              -> jsonResponse status200 ["path" .= path, "digest" .= fresh]
      Left (WriteDrift onDisk) -> conflict "drift" onDisk configMoved
      Left (WriteRefused why)  -> jsonError status500 why

noSuchLayer :: Text -> [ConfigLayerFile] -> Text
noSuchLayer path layers =
  "no config layer at " <> path <> "; this tree has "
    <> T.intercalate ", " [ T.pack (lfPath f) | f <- layers ]

-- | RAW as a layer write, or what is wrong with it.  @lines@ is an array
-- because a layer can spell its cycle over more than one @#+TODO:@ line, and a
-- client editing them as text splits on its own newlines rather than asking
-- this server to.
-- @filter@ is the default view the same file names, and it is optional: absent
-- leaves the @#+GLANCE_DEFAULT_FILTER:@ line exactly as it is, empty takes it
-- away, and anything else writes it.  It rides in this one request because it
-- is a line of the same file — two requests would be two writes under two
-- digests, the second of which the first had just invalidated.
parseConfigWrite :: BL.ByteString -> Either Text (Text, [Text], Maybe Text, Text)
parseConfigWrite raw = first (("body: " <>) . T.pack) $ do
  value <- eitherDecode' raw
  parseEither (withObject "config write" shape) value
  where shape o = (,,,) <$> o .: "path" <*> o .: "lines"
                        <*> o .:? "filter" <*> o .: "digest"

-- Commands

-- | What a command request asks for: a name, the rows it names, its arguments,
-- and any digests the client wants the write pinned to.
data Command = Command
  { cmdName    :: !Text
  , cmdIds     :: ![Text]                -- ^ in the order named, deduplicated.
  , cmdArgs    :: !(Maybe (Maybe Text))  -- ^ @set-state@'s keyword: absent, present, or null.
  , cmdDigests :: !(Map Text Text)       -- ^ id to the digest the client holds for its file.
  }

-- | One file's share of a command: the write it costs, and the ids it answers
-- for.  Every row of a file shares the file's digest, so the plan carries one.
data FilePlan = FilePlan
  { fpPath   :: !FilePath
  , fpDigest :: !Text
  , fpRows   :: ![(Text, [(Span, Text)])]  -- ^ row id, and the spans it moves.
  }

-- | The commands this route implements, which is also the whole of what @args@
-- can mean: @set-state@ takes @{"keyword": "DONE"}@ or @{"keyword": null}@,
-- @archive@ takes nothing.
commandNames :: [Text]
commandNames = ["archive", "set-state"]

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
-- name nothing implements, no ids at all, and a keyword some named row's file
-- does not declare are all 400 with nothing written — the last one refuses the
-- WHOLE request deliberately, since half a state change over a marked set is
-- worse than none of one.  Per id: an id the store has no row for, and a file
-- whose digest moved.  A 200 is therefore "the command ran", never "every row
-- moved"; the results say which did.
--
-- Nothing here touches the store, exactly as with @POST \/headline@: the write
-- goes to the file, the watch re-reads it and streams the rows, so a browser
-- command reaches every open tab by the path an editor's save takes.
runCommand :: Hub -> Request -> IO Response
runCommand hub request = do
  body <- takeBody bodyLimit request
  st <- readTVarIO (hubStore hub)
  -- The cap outranks every other refusal, the way it does on the other write
  -- route: this server declines to read a megabyte to find out what it says.
  case body of
    Nothing  -> pure (jsonError status413 tooBig)
    -- Everything that can refuse the request is decided before a file is
    -- opened, so what the plan hands back is either the 400 or the IO that
    -- writes: one Left branch, and the ordering of the answer stays beside the
    -- command that named the ids.
    Just raw -> either (pure . jsonError status400) id (planned st raw)
  where
    tooBig = "body over " <> T.pack (show bodyLimit) <> " bytes"
    planned st raw = do
      cmd <- parseCommand raw
      (plans, said) <- planCommand st cmd
      pure $ do
        written <- mapM writeOne plans
        -- Answered in the order the client named the ids, so a caller can zip
        -- the results against what it asked for.
        let outcomes = said <> concat written
        pure (jsonResponse status200
                ["results" .= [ v | rid <- cmdIds cmd, Just v <- [lookup rid outcomes] ]])

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
planCommand :: Store -> Command -> Either Text ([FilePlan], [(Text, Value)])
planCommand st cmd = do
  rows <- mapM withEdits [ r | rid <- cmdIds cmd, Just r <- [storeHeadline rid st] ]
  let groups = groupOn (hrFile . fst) rows
  pure ( [ FilePlan path (hrDigest r0) [ (hrId r, edits) | (r, edits) <- rs ]
         | (path, rs@((r0, _) : _)) <- groups, not (stale rs) ]
       , missing <> [ (hrId r, refused (hrId r) (staleWhy path))
                    | (path, rs) <- groups, stale rs, (r, _edits) <- rs ] )
  where
    withEdits r = (,) r <$> commandEdits cmd r
    missing = [ (rid, refused rid ("no headline with id " <> rid))
              | rid <- cmdIds cmd, Nothing <- [storeHeadline rid st] ]
    stale rs = or [ pinned /= hrDigest r
                  | (r, _edits) <- rs, Just pinned <- [Map.lookup (hrId r) (cmdDigests cmd)] ]
    staleWhy path = T.pack path
                      <> " has been re-read since these rows were fetched; ask for them again"

-- | The span edits CMD asks for on R, or why the request cannot be served at
-- all.  Only 'setStateEdits' refuses, and its refusal is the whole request's:
-- a keyword one named row's file does not declare stops the command rather than
-- moving the rows whose files do declare it.
commandEdits :: Command -> HeadlineRecord -> Either Text [(Span, Text)]
commandEdits cmd r
  | cmdName cmd == "set-state" = setStateEdits (join (cmdArgs cmd)) r
  | otherwise                  = Right (archiveEdits r)

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
parseCommand :: BL.ByteString -> Either Text Command
parseCommand raw =
  first (("body: " <>) . T.pack) (eitherDecode' raw >>= parseEither command) >>= checked
  where
    command = withObject "command" $ \o -> do
      name <- o .: "name"
      one <- o .:? "id"
      several <- o .:? "ids"
      args <- o .:? "args"
      digests <- o .:? "digests"
      keyword <- traverse (withObject "args" (.:? "keyword")) args
      pure (Command name (nub (maybe [] pure one <> fromMaybe [] several))
                    keyword (fromMaybe Map.empty digests))
    checked cmd
      | cmdName cmd `notElem` commandNames =
          Left ("no such command: " <> cmdName cmd <> "; this server runs "
                  <> T.intercalate " and " commandNames)
      | null (cmdIds cmd) =
          Left "a command names rows: {\"ids\": [\"…\"]}, or {\"id\": \"…\"} for one"
      | cmdName cmd == "set-state", Nothing <- cmdArgs cmd =
          Left "set-state wants args {\"keyword\": \"DONE\"}, or a null keyword to clear it"
      | otherwise = Right cmd

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
  ".woff"  -> "font/woff"
  ".ttf"   -> "font/ttf"
  _        -> "application/octet-stream"

-- Type

-- | The type stack the shell asks for, in the table, the sheet and the widgets
-- alike.  Nothing is fetched for it: these are names looked up on the machine,
-- and 'fontFace' adds an @\@font-face@ only when the assets directory holds a
-- file to point at.  A page that reaches the network for a font is a page that
-- renders differently offline, and this one is served over loopback to a
-- machine that may have none (docs\/invariants.md).
monoStack :: Text
monoStack = "\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"

-- | The font files the shell will use when the assets directory holds one,
-- best first.  With neither there, 'monoStack' falls through to whatever is
-- installed and the page says nothing about it.
fontAssets :: [FilePath]
fontAssets = ["JetBrainsMono-Regular.woff2", "JetBrainsMono-Regular.ttf"]

-- | The first of 'fontAssets' under OPTS's assets directory.  Looked up per
-- request, the way the renderer is: dropping the file in needs no restart.
localFont :: ServeOptions -> IO (Maybe FilePath)
localFont opts = listToMaybe <$> filterM (doesFileExist . (soAssets opts </>)) fontAssets

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
  -- The ends of the buffer, org-glance's own pair, plus vi's @G@ beside @>@.
  , bind ["<"]          "first-row"                       (Just "firstRow")       "table"
  , bind [">"]          "last-row"                        (Just "lastRow")        "table"
  , bind ["G"]          "last-row"                        (Just "lastRow")        "table"
  , bind ["]"]          "next-page"                       (Just "nextPage")       "table"
  , bind ["["]          "previous-page"                   (Just "previousPage")   "table"
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
  , bind ["o"]          "org-glance-overview:open"        Nothing                 "table"
  , bind ["!"]          "org-glance-overview:open"        Nothing                 "table"
  , bind ["a"]          "org-glance-agenda"               Nothing                 "table"
  , bind ["@"]          "org-glance-overview:relations"   Nothing                 "table"
  , bind ["+"]          "org-glance-overview:capture"     Nothing                 "table"
  -- dired's flag, and dired's rule for it: the first press marks the row for
  -- archiving and the second one does it.  The flag IS the confirmation, so
  -- there is no prompt and no undo to build — @u@ takes it off.  Plain @d@ is
  -- never a write on its own, which is what makes a mis-key cost a keystroke.
  , bind ["d"]          "archive-flag"                    (Just "archiveFlag")    "table"
      `helps` "flag this row; d again archives it"
  , bind ["D"]          "org-glance-overview:delete"      (Just "archiveRows")    "table"
      `helps` "archive the marked rows, or the row at point — never a delete"
      -- The user's own spelling; Chromium owns Ctrl+T above the document, so
      -- the org chord stays as the secondary for browsers that deliver it.
  , bind ["t"]          "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "set the state of the marked rows, or the row at point"
  , bind ["C-c", "C-t"] "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "the org spelling, where the browser lets it through"
  , bind ["C-c", "C-s"] "org-glance-overview:schedule"    Nothing                 "table"
  , bind ["C-c", "C-d"] "org-glance-overview:deadline"    Nothing                 "table"
  , bind [","]          "customize"                       (Just "openSettings")   "table"
      `helps` "the keyword cycles and the default view, a config layer at a time"
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
               , "archive-flag", "org-glance-overview:delete" ]

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
  , (["org-glance-overview:materialize"],  "materialize")
  , (["mark-toggle", "unmark", "unmark-all", "mark-all"], "mark")
  -- The two structured commands, beside the keys that pick what they run over.
  , (["org-glance-overview:todo"],         "state")
  , (["archive-flag"],                     "archive")
  , (["org-glance-overview:delete"],       "archive marked")
  , (["filter-rows"],                      "filter")
  , (["apply-default-filter"],             "default view")
  , (["filter-drop-token"],                "drop token")
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

-- | @\/@: the demo shell when the renderer is on disk, and an explanation when
-- it is not.  A missing renderer leaves @\/headlines@ untouched — the server
-- is a JSON server that happens to ship a page.
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
  pure (html (if ok then demoShell opts font (defaultFilter (stConfig st))
                    else assetsMissing opts))

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
-- Two keys write without a sheet.  @D@ archives and @C-c C-t@ sets a state,
-- both over the marked set when there is one and the row at point otherwise —
-- dired's rule, and org-glance's.  They are @POST \/command@ ('runCommand'):
-- the page sends row ids and a name, the server computes the spans, and the
-- table is not touched at all, since the rows come back over the socket once
-- the watch has re-read the files.  There is no confirmation step; the drift
-- lock is the safety, and @D@ archives rather than deletes, so the worst a
-- mis-key costs is a tag and another keystroke.  @C-c C-t@ raises a value
-- palette of this page's own — the state column's badges plus @clear@, typed
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
-- top corner holds the connection dot and the theme selector; a native control
-- because Tab, the arrows and Enter already navigate one and no new chord is
-- owed for it.  The page's last line is the same blob resident: the map's core
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
  [ "  <div id=\"corner\"><span id=\"dot\" title=\"live connection\"></span>"
      <> "<label for=\"themesel\">theme:</label>"
      <> "<select id=\"themesel\" title=\"colour theme\">"
      <> "<option value=\"auto\">auto</option><option value=\"light\">light</option>"
      <> "<option value=\"dark\">dark</option></select>"
      -- The keyboard-first exception, and the same one the chip row is: a
      -- coarse pointer has no `,' to press.  Hidden outside the
      -- pointer:coarse block, so a mouse never sees it and the key is the
      -- whole of the offer there.
      <> "<button id=\"gear\" title=\"settings\">\9881</button></div>"
  , "  <div id=\"app\"></div>"
  , "  <div id=\"log\">loading …</div>"
  , "  <div id=\"kbd\"></div>"
  , "  <div id=\"modal\">"
  , "    <div id=\"sheet\">"
  , "      <div id=\"mhead\"><span id=\"mfile\"></span><span id=\"mnote\"></span></div>"
  -- Two panes over one subtree: the body on the left, the property drawer on
  -- the right.  The cut between them is the server's (`GET /headline' hands
  -- both), so neither pane is derived here.
  , "      <div id=\"mpanes\">"
  , "        <textarea id=\"mtext\" spellcheck=\"false\"></textarea>"
  , "        <div id=\"mprops\"></div>"
  , "      </div>"
  -- The logbook, read-only and full width under both panes.  It is the
  -- server's: shown so a reader can see what the row has been through, never
  -- sent back, and out of Tab and out of `dirty()' because nothing here can
  -- move it.
  , "      <pre id=\"mlog\"></pre>"
  , "    </div>"
  , "  </div>"
  , "  <div id=\"prompt\">"
  , "    <div id=\"pbox\">"
  , "      <div id=\"phead\"></div>"
  , "      <input id=\"pinput\" spellcheck=\"false\" autocomplete=\"off\">"
  , "      <div id=\"plist\"></div>"
  , "    </div>"
  , "  </div>"
  -- The settings sheet: one section per keyword layer, then the union they
  -- come to.  A sibling of `#app' like the other two overlays, so a remount
  -- leaves it standing — which this one needs more than the others, since
  -- writing a layer is itself what moves the columns.
  , "  <div id=\"config\">"
  , "    <div id=\"cbox\">"
  , "      <div id=\"chead\"><span id=\"ctitle\">keyword cycles · default view</span>"
      <> "<span id=\"cnote\"></span></div>"
  , "      <div id=\"clayers\"></div>"
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
  -- The strip is capped, so the end of a long message can be out of sight.
  -- Keep it in sight — unless the reader has scrolled up, which is a place
  -- they are holding on purpose.
  , "    function log(m) {"
  , "      const box = document.getElementById(\"log\");"
  , "      const end = box.scrollTop + box.clientHeight >= box.scrollHeight - 4;"
  , "      box.textContent = m;"
  , "      if (end) box.scrollTop = box.scrollHeight;"
  , "    }"
  , "    const dot = (name) => (document.getElementById(\"dot\").className = name);"
  , "    const el = (id) => document.getElementById(id);"
  , "    let table = null, socket = null, backoff = 1000, editing = null;"
  , "    // The sheet's own state: the two panes as the file holds them as far as"
  , "    // this page knows, whether the drawer is a panel or spelled out in the"
  , "    // text, and the one word saying where the sheet stands with the file."
  , "    let base = \"\", baseProps = null, raw = false, state = \"synced\";"
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
        -- The renderer's per-row hint says RET materializes, which the key line
        -- under the table already says and says for every command.  One place.
  , "        actionHints: false,"
  , "        // The applied query, restored as the renderer's own committed"
  , "        // chips. It tokenizes them and delivers nothing — the rows in"
  , "        // hand are already the server's answer to this query, and a"
  , "        // delivery here would ask for them a second time."
  , "        initialQuery: query,"
  , "        onAction: (command, id) =>"
  , "          command === \"materialize\" ? materialize(id)"
  , "                                     : log(`action: ${command}  id=${id}`),"
  , "        onLink: (target) => log(`link: ${target}`),"
  , "        onFilter: filter,   // the server narrows; the renderer shows what it is given"
  , "      });"
  , "      // An asset older than `initialQuery' drops it silently, which would"
  , "      // leave the page showing no filter over rows that are filtered."
  , "      // `getQuery()' says whether it took: when it did not, put the query"
  , "      // back in the box the way this did before chips could carry it."
  , "      if (query && !holds(query)) showQuery();"
  , "      // The columns are the view's: both halves of a filter read the keys"
  , "      // out of them (`parity'), and cell movement names its landing column"
  , "      // by the header sitting over it."
  , "      cols = view.columns || [];"
  , "      // The boot placeholder has done its work.  The strip is an event log"
  , "      // — connection, sync, warnings, errors — and it says nothing about"
  , "      // what is loaded: the renderer's own hint line already counts the"
  , "      // rows, and the key line under it says what the keys are."
  , "      log(\"\");"
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
  , "    const quiet = (e) => { if (e.name !== \"AbortError\") log(`load failed: ${e.message}`); };"
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
  , "      log(note);"
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
  , "    // Every applied query is written, the EMPTY one included: a `q' that is"
  , "    // present and empty is a reader who took the filter off, where an absent"
  , "    // one is a page nobody has filtered yet.  Only the second has the default"
  , "    // injected over it, so DEL'ing the last chip survives a reload and every"
  , "    // remount after it — deleting the parameter here is what made a cleared"
  , "    // view come back filtered."
  , "    function remember(q) {"
  , "      const p = params();"
  , "      p.set(\"q\", q);   // `keys' and anything else in the URL survives"
  , "      history.replaceState(null, \"\", `?${p.toString()}`);"
  , "    }"
  , "    // A query as the `/headlines' query string asking it, spelled once for"
  , "    // the four callers that want it — the boot, a commit, the arming fetch"
  , "    // and the reconnect.  A second spelling is how a revalidation comes to"
  , "    // be answered 304 against rows answering some other question."
  , "    const asking = (q) => (q ? `?q=${encodeURIComponent(q)}` : \"\");"
  , "    // One place asks the server for rows: `query' is already what to ask."
  , "    const fetchRows = () =>"
  , "      load(asking(query)).then((a) => table && paint(a)).catch(quiet);"
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
  , "    // The two shapes of /headline, each written once.  `headline' unwraps"
  , "    // the JSON and turns the server's own error into a throw; `post' pins"
  , "    // the write to DIGEST, and EXTRA is what a page closing on an edited"
  , "    // sheet adds — `keepalive', being the one caller that cannot wait."
  , "    const headline = (id) =>"
  , "      fetch(`/headline?id=${encodeURIComponent(id)}`).then((r) =>"
  , "        r.json().then((b) => {"
  , "          if (!r.ok) throw new Error(b.error || r.status);"
  , "          return b;"
  , "        }));"
  , "    const post = (id, digest, asked, extra) =>"
  , "      fetch(`/headline?id=${encodeURIComponent(id)}`, {"
  , "        method: \"POST\","
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify({ ...asked, digest }),"
  , "        ...extra,"
  , "      });"
  , "    function materialize(id) {"
  , "      headline(id).then((h) => show(h, false))"
  , "        .catch((e) => log(`materialize failed: ${e.message}`));"
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
    -- The property panel.  A row is a key and a value — so the drawer is edited
    -- as what it is rather than as text that happens to look like a drawer — and
    -- the rows sit in the order the file writes them.
    --
    -- It is MODAL, dired's shape rather than a form's.  In NAV the rows are
    -- read-only text with a cursor on one of them and nothing focusable at all,
    -- which is what leaves the plain letters free to be movement; RET opens the
    -- row at point, and only then are there fields to type into.  A panel of
    -- boxes would have taken n and j away and given a keystroke two meanings.
    --
    -- The rows are held in this array rather than read back out of the DOM: the
    -- panel is this page's own chrome, and a selector over it would be a second
    -- description of a structure written three lines above.  What a row HOLDS is
    -- its committed text; the open row's two fields are the edit in progress and
    -- nothing else reads them, which is what makes a commit the only thing that
    -- can make the sheet dirty.
    --
    -- The planning entries are the same two modes over the same kind of row, so
    -- they are rows in this list rather than a second one of their own — three
    -- FIXED ones, in org's own order, ahead of the drawer's properties.  Fixed
    -- means the key is org's and not the author's: RET opens the value alone,
    -- and an empty value is the entry absent.
  , "    const PLANNING = " <> jsonList planningKeywords <> ";"
  , "    let prows = [], pcur = 0, pedit = -1, pnav = false;"
  , "    function drawProps(list, plan) {"
  , "      el(\"mprops\").textContent = \"\";"
  , "      el(\"mprops\").className = \"\";"
  , "      prows = []; pcur = 0; pedit = -1; pnav = false;"
  , "      const held = new Map(plan || []);"
  , "      for (const key of PLANNING) addRow(key, held.get(key) || \"\", true);"
  , "      for (const p of list) addRow(p[0], p[1], false);"
  , "    }"
  , "    function addRow(key, value, fixed) {"
  , "      const row = document.createElement(\"div\");"
  , "      el(\"mprops\").appendChild(row);"
  , "      prows.push({ key, val: value, row, fixed: !!fixed });"
  , "      drawRow(prows.length - 1);"
  , "    }"
    -- The add-row affordance, and the whole of it: `+' puts an empty property
    -- at the end of the drawer and opens it.  Keyboard-first means no button
    -- here, and a KEY rather than a row that is always empty — the trailing row
    -- was chrome that had to be filtered out of everything the panel says.  A
    -- row whose key is emptied is still a property deleted.
  , "    function addProperty() {"
  , "      const was = pcur;"
  , "      addRow(\"\", \"\", false);"
  , "      pcur = prows.length - 1;"
  , "      cls(was); cls(pcur);"
  , "      openRow();"
  , "    }"
    -- One row, in whichever mode it is in.  The cells are spans until the row is
    -- opened and fields while it is, which is the whole of the difference: an
    -- empty span wears its hint through the stylesheet, so what the panel SAYS
    -- an empty cell is stays out of what it holds.
    --
    -- The identity property is in neither mode, because it is in neither pane:
    -- `ORG_GLANCE_ID' is the row id the table keys its updates off, and the
    -- server keeps it out of what it hands over and puts it back verbatim
    -- afterwards ('Glance.Query.hiddenProperties').  There is nothing for this
    -- page to warn about and nothing for it to filter.
  , "    const cls = (i) =>"
  , "      (prows[i].row.className ="
  , "        (prows[i].fixed ? \"prow pln\" : \"prow\") + (i === pcur ? \" pat\" : \"\"));"
  , "    function drawRow(i) {"
  , "      const r = prows[i], open = i === pedit;"
  , "      r.row.textContent = \"\";"
  , "      cls(i);"
  , "      const k = cell(open && !r.fixed, \"pkey\", r.key, \"property\");"
  , "      const v = cell(open, \"pval\", r.val, r.fixed ? \"<2026-08-01 Sat>\" : \"value\");"
  , "      r.row.appendChild(k); r.row.appendChild(v);"
  , "      return [k, v];"
  , "    }"
  , "    function cell(open, kind, value, hint) {"
  , "      const e = document.createElement(open ? \"input\" : \"span\");"
  , "      e.className = kind;"
  , "      if (open) { e.value = value; e.placeholder = hint; e.spellcheck = false; }"
  , "      else { e.textContent = value; e.dataset.hint = hint; }"
  , "      return e;"
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
  , "    function enterPanel() {"
  , "      pnav = true; el(\"mprops\").className = \"on\"; el(\"mtext\").blur();"
  , "    }"
  , "    function leavePanel() {"
  , "      pnav = false; el(\"mprops\").className = \"\"; el(\"mtext\").focus();"
  , "    }"
  , "    el(\"mtext\").addEventListener(\"focus\", () => pnav && leavePanel());"
  , "    function moveCur(step) {"
  , "      const at = pcur + step;"
  , "      if (at < 0 || at >= prows.length) return;"
  , "      const was = pcur; pcur = at; cls(was); cls(at);"
  , "    }"
    -- Opening a row: the value takes the focus, since editing an existing
    -- property is almost always editing its value — except where there is no key
    -- yet, which is the add-row, and there the key is the thing being typed.
  , "    function openRow() {"
  , "      pedit = pcur;"
  , "      const [k, v] = drawRow(pcur);"
  , "      (prows[pcur].fixed || prows[pcur].key ? v : k).focus();"
  , "    }"
    -- Committing: the row takes the text its fields are holding and goes back
    -- to being text.  This is the one thing that can make the sheet dirty from
    -- the panel — an edit nobody committed was never in `props()'.  A fixed
    -- row keeps its key, which is org's rather than the author's.
  , "    function commitRow() {"
  , "      const at = pedit, r = prows[at];"
  , "      if (!r.fixed) r.key = r.row.children[0].value;"
  , "      r.val = r.row.children[1].value;"
  , "      pedit = -1;"
  , "      drawRow(at);"
  , "    }"
    -- ESC over an open row is the ROW's: the fields go and the text the row is
    -- holding comes back, which is the text it was opened on.  The sheet's own
    -- ESC ladder therefore only ever sees the key from nav — that is why this
    -- runs from the keymap's `cancel' rather than from a listener of its own.
  , "    function cancelRow() {"
  , "      const at = pedit; pedit = -1; drawRow(at);"
  , "      echo(\"ESC → row unchanged\");"
  , "    }"
    -- The panel's own keys, behind the dispatch and for the reason the value
    -- palette's are: while the panel holds them `typing()' is true, so every
    -- `table' row is dead and nothing here takes a key the map wanted.
    --
    -- TAB crosses the panes — out of the body into the panel's cursor, out of
    -- nav back into the body — and the cursor is where it was left.  Two stops,
    -- so both directions are one toggle and S-TAB is the same line.  In nav the
    -- keys are movement: n/p and j/k both, unconditionally, because a row with
    -- no field in it leaves every printable key free and both spellings cost
    -- nothing to satisfy at once; the arrows are the pair that needs neither.
    -- RET opens the row at point, and @+@ adds one at the end.
    --
    -- In edit TAB is the hop between the row's two fields — one row, two fields,
    -- and nothing else for it to mean — so the crossing is suspended for as long
    -- as a row is open.  RET commits.  Raw mode has one pane and nowhere to
    -- cross to, so TAB is the browser's there.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!editing) return;"
  , "      const k = keyName(e), crossing = k === \"TAB\" || k === \"S-TAB\";"
  , "      if (pedit !== -1) {"
  , "        const two = prows[pedit].row.children;"
  , "        if (crossing)"
  , "          (document.activeElement === two[0] ? two[1] : two[0]).focus();"
  , "        else if (k === \"RET\") commitRow();"
  , "        else return;   // ESC is the keymap's, and puts the row back"
  , "      } else if (!pnav) {"
  , "        if (raw || !crossing || document.activeElement !== el(\"mtext\")) return;"
  , "        enterPanel();"
  , "      } else if (crossing) leavePanel();"
  , "      else if (k === \"RET\") openRow();"
  , "      else if (k === \"+\") addProperty();"
  , "      else if (k === \"<down>\" || k === \"n\" || k === \"j\") moveCur(1);"
  , "      else if (k === \"<up>\" || k === \"p\" || k === \"k\") moveCur(-1);"
  , "      else return;"
  , "      e.preventDefault();"
  , "    });"
    -- What a flush sends: the subtree whole in raw mode, the two panes apart
    -- otherwise.  The server joins them, so this page never spells a drawer.
  , "    const asked = () => raw"
  , "      ? { org: el(\"mtext\").value }"
  , "      : { body: el(\"mtext\").value, properties: props(), planning: planning() };"
  , "    // Where the sheet stands is one word, and `sync' is its only writer:"
  , "    // the header wears it as text and as a class, and everything that asks"
  , "    // reads it back.  With no buttons the keys are the whole of the offer,"
  , "    // so the states that wait for one say which key."
  , "    const WORDS = { synced: \"synced\", syncing: \"syncing…\","
  , "      conflict: \"conflict — C-x C-s overwrite · ESC discard\","
  , "      error: \"error — C-x C-s retry · ESC discard\" };"
  , "    function sync(next, message) {"
  , "      state = next;"
  , "      el(\"mnote\").className = next;"
  , "      el(\"mnote\").textContent = message || WORDS[next];"
  , "    }"
  , "    const troubled = () => state === \"conflict\" || state === \"error\";"
  , "    const flushing = () => state === \"syncing\";"
  , "    const stuck = (why) => sync(\"error\", why && `${why} — C-x C-s retry · ESC discard`);"
  , "    function shut() {"
  , "      el(\"modal\").className = \"\"; editing = null; base = \"\"; baseProps = null;"
  , "      pedit = -1; pnav = false;   // the keys go back to the table"
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
  , "          else stuck(a.body.error || `sync failed (${a.status})`);"
  , "          return false;"
  , "        })"
  , "        .catch((e) => { stuck(e.message); return false; });"
  , "    }"
  , "    // C-x C-s.  Mid-edit it is a manual flush; on a conflict it is the"
  , "    // deliberate keystroke that overwrites — ask for the digest the file"
  , "    // carries now and post the text the author is looking at over it."
  , "    function saveSheet() {"
  , "      if (!editing || flushing()) return;"
  , "      if (state !== \"conflict\") { flush(editing.digest); return; }"
  , "      const h = editing;"
  , "      headline(h.id)"
  , "        .then((b) => editing === h && flush(b.digest))"
  , "        .catch((e) => stuck(e.message));"
  , "    }"
  , "    // The way out — ESC, the backdrop, q.  Pristine costs no request and"
  , "    // touches no file; dirty flushes and closes on the 200; a sheet with"
  , "    // trouble in it discards, which is what a second ESC is."
  , "    function leave() {"
  , "      if (!editing) return;"
  , "      if (troubled()) { shut(); log(\"closed without writing — the file is as it was\"); return; }"
  , "      if (!dirty()) { shut(); return; }"
  , "      if (!flushing()) flush(editing.digest).then((ok) => ok && shut());"
  , "    }"
  , "    el(\"modal\").addEventListener(\"click\", (e) => { if (e.target === el(\"modal\")) leave(); });"
    -- C-c ' — org's `edit-special' rhyme, one subtree seen two ways: body and
    -- panel, or the raw org the panes were cut out of.  The cut is the server's,
    -- so the toggle RE-READS the headline rather than splitting or joining
    -- anything here; that is what keeps an org parser out of this page.  A
    -- re-read cannot carry unsaved work, so a dirty sheet is refused and told
    -- which key would let it through.  The re-read is also a fresh materialize,
    -- which is why it lands at `synced' whatever it was at before.
  , "    function toggleRaw(b) {"
  , "      if (!editing) return;"
  , "      if (dirty()) { echo(`${b.seq} → sync first — C-x C-s`); return; }"
  , "      const h = editing, want = !raw;"
  , "      headline(h.id).then((fresh) => {"
  , "        if (editing !== h) return;   // the sheet moved on while this was out"
  , "        editing = fresh; raw = want;"
  , "        fill(fresh);"
  , "        sync(\"synced\");"
  , "        el(\"mtext\").focus();"
  , "        echo(`${b.seq} → ${raw ? \"raw org\" : \"properties panel\"}`);"
  , "      }).catch((e) => stuck(e.message));"
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
  , "      if (!list.length) { log(\"no rows to move through\"); return; }"
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
  , "        else log(\"no rows to move through\");"
  , "        return;"
  , "      }"
  , "      const list = visible(), at = list.findIndex((r) => r.id === focusedId());"
  , "      pick(list, at === -1 ? (step > 0 ? 0 : list.length - 1) : at + step);"
  , "    }"
    -- Pages.  The turn is the renderer's, and the echo says where it landed
    -- rather than which key ran: `] → page 3/129' is the only thing a reader
    -- wants back from a page key, and it reads the same at a stop as at a turn.
  , "    const pager = () => !!table && typeof table.nextPage === \"function\""
  , "      && typeof table.pageInfo === \"function\";"
  , "    function turnPage(b, step) {"
  , "      if (!pager()) {"
  , "        echo(`${b.seq} → ${b.command} (this table-view.js has no pager)`);"
  , "        return;"
  , "      }"
  , "      if (step > 0) table.nextPage(); else table.previousPage();"
  , "      const at = table.pageInfo();"
  , "      echo(`${b.seq} → page ${at.page}/${at.pages}`);"
  , "    }"
  , "    // Cells.  The column is part of the renderer's selection, so it needs no"
  , "    // state here: it rides along with row"
  , "    // movement, and goes when the selection that holds it goes.  A whole-row"
  , "    // selection has none, and the first horizontal key lands on the first"
  , "    // column whichever direction asked."
  , "    const cells = () => !!table && typeof table.getSelection === \"function\";"
  , "    const column = () => (cells() ? table.getSelection().col : null);"
  , "    function moveCol(b, step) {"
  , "      const say = (what) => echo(`${b.seq} → ${b.command} (${what})`);"
  , "      if (!cells()) { say(\"this table-view.js has no cell selection\"); return; }"
  , "      const at = column(), want = at === null ? 0 : at + step;"
  , "      // Clamped, never wrapped: walking off an edge stays on it and says so."
  , "      if (want < 0 || want >= cols.length) { say(want < 0 ? \"at first\" : \"at last\"); return; }"
  , "      const id = focusedId();"
  , "      if (!id || !table.select(id, want)) { say(\"no row\"); return; }"
  , "      say(cols[want].header || cols[want].key);"
  , "    }"
    -- Marks.  The renderer holds them, keyed by id, so nothing about them is
    -- kept here: which rows are marked, how many there are and what a mark
    -- survives are all its answers.  Dired's advance is this page's, though —
    -- the key that marks is the key that walks, which is what makes a held `m'
    -- a run down a column.
  , "    const marking = () => !!table && typeof table.toggleMark === \"function\";"
  , "    const said = (b, what) => echo(`${b.seq} → ${b.command} (${what})`);"
    -- Archive flags are the renderer's for the same reason marks are: a flag has
    -- to outlive a `setRows', a filter that hides its row and a page it is not
    -- on, and only the thing that draws the rows can do that.  An asset predating
    -- the calls says so rather than growing a shell-side set the next paint would
    -- lose.
  , "    const flagging = () => !!table && typeof table.flagRow === \"function\""
  , "      && typeof table.getFlagged === \"function\";"
  , "    const isFlagged = (id) => flagging() && table.getFlagged().indexOf(id) !== -1;"
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
  , "        echo(`${b.seq} → flag cleared`);"
  , "        move(1);"
  , "        return;"
  , "      }"
  , "      let on = table.toggleMark(id);"
  , "      if (on && !toggling) on = table.toggleMark(id);"
  , "      echo(`${b.seq} → ${on ? \"marked\" : \"unmarked\"} (${table.markedCount()})`);"
  , "      move(1);"
  , "    }"
    -- Commands.  A structured write names ROWS and lets the server compute the
    -- spans, so nothing here knows what a headline looks like — and nothing
    -- here touches the table afterwards either: the rows arrive over the socket
    -- once the watch has re-read the files, the way an editor's save arrives.
    --
    -- Which rows is dired's rule, and the same one `D' has in an org-glance
    -- overview: the marked set when there is one, the row at point otherwise.
    -- The marks are the renderer's, so they are asked for when the command runs
    -- rather than tracked here.
  , "    const targets = () => {"
  , "      const marked = marking() ? table.getMarked() : [];"
  , "      if (marked.length) return marked;"
  , "      const id = focusedId();"
  , "      return id ? [id] : [];"
  , "    };"
    -- A partial answer is ordinary here: each file is its own write, so one
    -- that moved on disk refuses its rows while the rest land.  The count goes
    -- in the pill and every refusal in the log.
  , "    function fire(b, name, ids, args, verb) {"
  , "      fetch(\"/command\", {"
  , "        method: \"POST\","
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify({ name, ids, args }),"
  , "      }).then((r) => r.json().then((answer) => {"
  , "        if (!r.ok) throw new Error(answer.error || r.status);"
  , "        const results = answer.results || [];"
  , "        const bad = results.filter((x) => !x.ok);"
  , "        echo(`${b.seq} → ${verb} (${results.length - bad.length})`);"
  , "        if (bad.length) log(bad.map((x) => `${x.id}: ${x.error}`).join(\" · \"));"
  , "      })).catch((e) => { said(b, e.message); log(`${name} failed: ${e.message}`); });"
  , "    }"
    -- The value palette: a prompt of this page's own, since the renderer's
    -- overlay belongs to the filter and this page may not reach into it.  Type
    -- to narrow, C-n/C-p or the arrows to walk, RET to commit; ESC is the
    -- keymap's own `keyboard-quit', which closes whichever overlay is up.
    --
    -- The keys are handled in a second document listener rather than on the
    -- field, and that is safe because it runs after the dispatch: with a field
    -- focused `typing()' has already made every `table' row dead, so the only
    -- row that can fire ahead of this is the one that should.
  , "    let prompting = null;"
  , "    function ask(title, choices, commit) {"
  , "      prompting = { choices, shown: choices, at: 0, commit };"
  , "      el(\"phead\").textContent = title;"
  , "      el(\"pinput\").value = \"\";"
  , "      el(\"prompt\").className = \"on\";"
  , "      drawChoices();"
  , "      el(\"pinput\").focus();"
  , "    }"
    -- Blurred as well as hidden: a focused field nobody can see would leave
    -- `typing()' true and swallow every key after it.
  , "    function unask() {"
  , "      prompting = null;"
  , "      el(\"prompt\").className = \"\";"
  , "      el(\"pinput\").blur();"
  , "    }"
  , "    function drawChoices() {"
  , "      const list = el(\"plist\");"
  , "      list.textContent = \"\";"
  , "      prompting.shown.forEach((c, i) => {"
  , "        const row = document.createElement(\"div\");"
  , "        row.className = i === prompting.at ? \"pat\" : \"\";"
  , "        row.textContent = c.label;"
  , "        list.appendChild(row);"
  , "      });"
  , "    }"
  , "    function narrowTo(text) {"
  , "      const want = text.trim().toLowerCase();"
  , "      prompting.shown = prompting.choices.filter((c) => c.label.toLowerCase().includes(want));"
  , "      prompting.at = 0;"
  , "      drawChoices();"
  , "    }"
  , "    function walkChoices(step) {"
  , "      const n = prompting.shown.length;"
  , "      if (n) prompting.at = Math.max(0, Math.min(n - 1, prompting.at + step));"
  , "      drawChoices();"
  , "    }"
  , "    function takeChoice() {"
  , "      const chosen = prompting.shown[prompting.at];"
  , "      if (!chosen) return;"
  , "      const act = prompting.commit;"
  , "      unask();"
  , "      act(chosen);"
  , "    }"
  , "    el(\"pinput\").addEventListener(\"input\", (e) => prompting && narrowTo(e.target.value));"
  , "    el(\"prompt\").addEventListener(\"click\", (e) =>"
  , "      { if (e.target === el(\"prompt\")) unask(); });"
    -- What C-c C-t offers: the state column's badges, which are the keyword
    -- union of every file loaded, plus the entry that takes a keyword off.  The
    -- column's `values' are the filter's group meta-values (`*active*') and are
    -- deliberately absent — no file declares one, so the server refuses every
    -- one of them, and offering a value that cannot be set is worse than not
    -- offering it.
    -- `*clear*' wears the stars every reserved meta wears (docs/invariants.md):
    -- the starred form is the page's mark for a value with semantics rather than
    -- a word a file could hold, and the server refuses a starred string as a
    -- keyword from the other side.  What it commits is a null keyword.
  , "    const CLEAR = \"*clear*\";"
    -- What the tree falls back to with no `#+GLANCE_DEFAULT_FILTER:' line, which
    -- is what the settings field says an empty box means.
  , "    const BUILTIN_QUERY = " <> jsonText builtinFilter <> ";"
  , "    const stateChoices = () =>"
  , "      ((cols.filter((c) => c.key === \"state\")[0] || {}).badges || [])"
  , "        .map((x) => ({ label: x.value, keyword: x.value }))"
  , "        .concat([{ label: CLEAR, keyword: null }]);"
    -- Settings.  One section per keyword layer, and a layer is one config file
    -- and one box holding its `#+TODO:' lines VERBATIM.  The line is the
    -- contract org itself reads, so it is what is edited: a chip UI here would
    -- be this page guessing at a grammar it has no parser for, and the guess
    -- would be what gets written.
    --
    -- The sheet is the materialize sheet's pattern, down to the words: no
    -- buttons, ESC or the backdrop syncs and closes, `C-x C-s' syncs mid-edit,
    -- and the header carries one of the same four states.  What it does NOT
    -- share is a request: `/config' is its own pair of routes, and the rows
    -- arrive the way every other write's do — the file watch sees the config
    -- change and reseeds the tree.
  , "    let settings = false, cstate = \"synced\", crows = [];"
    -- Claimed before the fetch, and refused over the other sheet.  `typing()'
    -- is not enough to keep the two apart: clicking the materialize sheet's own
    -- header blurs its textarea, and a `table' row is live again the moment it
    -- does — so the rule is stated here rather than left to the focus.
  , "    function openSettings() {"
  , "      if (settings || editing) return;"
  , "      settings = true;"
  , "      config().then((b) => {"
  , "        if (!settings) return;   // an ESC arrived while the layers were out"
  , "        drawLayers(b);"
  , "        cnote(\"synced\");"
  , "        el(\"config\").className = \"on\";"
  , "        if (crows.length) crows[0].box.focus();"
  , "      }).catch((e) => { settings = false; log(`settings failed: ${e.message}`); });"
  , "    }"
  , "    const config = () =>"
  , "      fetch(\"/config\").then((r) => r.json().then((b) => {"
  , "        if (!r.ok) throw new Error(b.error || r.status);"
  , "        return b;"
  , "      }));"
  , "    function drawLayers(b) {"
  , "      el(\"clayers\").textContent = \"\";"
  , "      crows = (b.layers || []).map((l) => layerRow(l, b.filter || \"\"));"
  , "      const kw = b.keywords || {};"
  , "      el(\"ceff\").textContent ="
  , "        `${(kw.active || []).join(\" \")} | ${(kw.inactive || []).join(\" \")}`;"
  , "    }"
    -- What one layer shows: which scope it is and where it lives, its lines,
    -- and a line for whatever the server said about the last write to it.  A
    -- layer with no digest is not a file yet — saying so is what makes creating
    -- the first one an edit rather than a mystery.
  , "    function layerRow(layer, viewText) {"
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
  , "      row.appendChild(lab); row.appendChild(box);"
  , "      const r = { path: layer.path, digest: layer.digest, base: box.value,"
  , "                  box, note, view: null, viewBase: null };"
    -- The default view is a LINE of `system.org' and of no other file, so its
    -- field sits under that layer and rides in that layer's write: one file,
    -- one digest, one splice.  Emptying it takes the line away, which is the
    -- tree going back to the built-in default rather than to no filter at all.
  , "      if (layer.tag === null) {"
  , "        const view = document.createElement(\"input\");"
  , "        view.className = \"cview\";"
  , "        view.spellcheck = false;"
  , "        view.placeholder = `the view g applies; empty is ${BUILTIN_QUERY}`;"
  , "        view.value = r.viewBase = viewText;"
  , "        row.appendChild(view);"
  , "        r.view = view;"
  , "      }"
  , "      row.appendChild(note);"
  , "      el(\"clayers\").appendChild(row);"
  , "      return r;"
  , "    }"
    -- The same four words the other sheet wears, and the same two keys clear
    -- the two that wait for one.
  , "    const cnote = (next, message) => {"
  , "      cstate = next;"
  , "      el(\"cnote\").className = next;"
  , "      el(\"cnote\").textContent = message || WORDS[next];"
  , "    };"
  , "    const cdirty = () => crows.some(cmoved);"
  , "    const cmoved = (r) => r.box.value !== r.base"
  , "      || (r.view !== null && r.view.value !== r.viewBase);"
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
  , "        const a = await fetch(\"/config\", {"
  , "          method: \"POST\","
  , "          headers: { \"content-type\": \"application/json\" },"
  , "          body: JSON.stringify({ path: r.path, lines: sent.split(\"\\n\"),"
  , "                                 ...(r.view ? { filter: view } : {}),"
  , "                                 digest: r.digest }),"
  , "        }).then((x) => x.json().then((b) => ({ status: x.status, body: b })))"
  , "          .catch((e) => ({ status: 0, body: { error: e.message } }));"
  , "        if (a.status === 200) {"
  , "          r.digest = a.body.digest; r.base = sent; r.note.textContent = \"\";"
  , "          if (r.view) r.viewBase = view;"
  , "        } else {"
  , "          ok = false;"
  , "          if (a.status === 409) clashed = true;"
  , "          r.note.textContent = a.body.error || `sync failed (${a.status})`;"
  , "        }"
  , "      }"
  , "      cnote(ok ? \"synced\" : clashed ? \"conflict\" : \"error\");"
  , "      return ok;"
  , "    }"
    -- C-x C-s.  Mid-edit a flush; on a conflict the deliberate keystroke that
    -- overwrites — ask for the digests the files carry NOW and post the text
    -- the author is looking at over them, which is the sheet's own rule.
  , "    function saveConfig() {"
  , "      if (cstate === \"syncing\") return;"
  , "      if (cstate !== \"conflict\") { flushConfig(); return; }"
  , "      config().then((b) => {"
  , "        for (const r of crows) {"
  , "          const fresh = (b.layers || []).find((l) => l.path === r.path);"
  , "          if (fresh) r.digest = fresh.digest;"
  , "        }"
  , "        flushConfig();"
  , "      }).catch((e) => cnote(\"error\", `${e.message} — C-x C-s retry · ESC discard`));"
  , "    }"
    -- The way out, and the sheet's own rule: pristine closes with no request,
    -- dirty syncs and closes on success, and one with trouble in it discards —
    -- which is what a second ESC is.
  , "    function leaveSettings() {"
  , "      if (!settings) return;"
  , "      if (cstate === \"conflict\" || cstate === \"error\") {"
  , "        shutSettings(); log(\"settings closed — the files are as they were\"); return;"
  , "      }"
  , "      if (!cdirty()) { shutSettings(); return; }"
  , "      if (cstate !== \"syncing\") flushConfig().then((ok) => ok && shutSettings());"
  , "    }"
  , "    function shutSettings() {"
  , "      el(\"config\").className = \"\"; settings = false; crows = []; cstate = \"synced\";"
  , "    }"
  , "    el(\"config\").addEventListener(\"click\", (e) =>"
  , "      { if (e.target === el(\"config\")) leaveSettings(); });"
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
  , "      }).catch((e) => log(`sheet restore failed: ${e.message}`));"
  , "    }"
  , "    // The one door that throws the mount away and builds a new one: a"
  , "    // `view-changed' close, and `g'.  Everything else that loses the socket"
  , "    // goes through `resync', which keeps the page it has."
  , "    function remount() { stash(); start(); }"
    -- `g': the view this tree configures, applied the way every other query is
    -- — written into the URL and asked of the server.  It goes through the
    -- mount because the chips are the renderer's and only a mount can be handed
    -- a query it did not commit itself; `start' then reads the URL this just
    -- wrote.  Dropping onclose first stops the reconnect timer opening a second
    -- socket behind this one.
  , "    function applyDefault(b) {"
  , "      echo(`${b.seq} → ${DEFAULT_QUERY"
  , "        ? `filter: ${JSON.stringify(DEFAULT_QUERY)}` : \"filter cleared\"}`);"
  , "      if (socket) { socket.onclose = null; socket.close(); socket = null; }"
  , "      backoff = 1000;"
  , "      remember(DEFAULT_QUERY);"
  , "      remount();"
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
  , "    // take for row movement — and the property panel in nav, which holds"
  , "    // the keys with nothing focused and would otherwise leave the table's"
  , "    // own letters live under its movement."
  , "    const typing = () => {"
  , "      const a = document.activeElement;"
  , "      return pnav || (!!a && (a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
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
  , "      firstRow: () => pick(visible(), 0),"
  , "      lastRow: () => pick(visible(), visible().length - 1),"
  , "      materializeRow: () => {"
  , "        const id = focusedId();"
  , "        if (id) materialize(id); else log(\"no row focused — n or p picks one\");"
  , "      },"
  , "      markToggle: (b) => mark(b, true),"
  , "      unmarkRow: (b) => mark(b, false),"
  , "      unmarkAll: (b) => {"
  , "        if (!marking()) { said(b, \"this table-view.js has no marks\"); return; }"
  , "        table.clearMarks();"
  , "        if (flagging()) table.clearFlags();"
  , "        echo(`${b.seq} → all marks and flags cleared`);"
  , "      },"
    -- `M' marks the whole loaded set, which is the renderer's call because the
    -- set is the renderer's: a page it is not showing is still marked.
  , "      markAll: (b) => {"
  , "        if (!marking() || typeof table.markAll !== \"function\")"
  , "          { said(b, \"this table-view.js has no mark-all\"); return; }"
  , "        table.markAll();"
  , "        echo(`${b.seq} → marked (${table.markedCount()})`);"
  , "      },"
    -- dired's `d', in two presses: the first flags the row and the second
    -- archives it.  The flag IS the confirmation, so there is no prompt — and
    -- the command is in ONCE, so a HELD `d' delivers exactly one press and can
    -- never flag and archive from one keystroke.  `u' takes a flag off.
  , "      archiveFlag: (b) => {"
  , "        if (!flagging()) { said(b, \"this table-view.js has no archive flags\"); return; }"
  , "        const id = focusedId();"
  , "        if (!id) { said(b, \"no row\"); return; }"
  , "        if (!isFlagged(id)) {"
  , "          table.flagRow(id);"
  , "          echo(`${b.seq} → flagged — d again archives`);"
  , "          return;"
  , "        }"
  , "        table.unflagRow(id);"
  , "        fire(b, \"archive\", [id], {}, \"archived\");"
  , "      },"
  , "      applyDefault, focusFilter, toggleRaw, openSettings,"
    -- One `save-buffer' over two sheets: whichever is up is what it syncs.
  , "      save: () => (settings ? saveConfig() : saveSheet()),"
    -- D is dired's key and org-glance's `delete', and here it archives: the tag
    -- goes on, the headline stays, and the default view stops showing it.
  , "      archiveRows: (b) => {"
  , "        const ids = targets();"
  , "        if (ids.length) fire(b, \"archive\", ids, {}, \"archived\");"
  , "        else said(b, \"no row\");"
  , "      },"
    -- C-c C-t asks which state, over whatever the command would run on, and
    -- the server refuses a keyword the row's own file does not declare.
  , "      setState: (b) => {"
  , "        const ids = targets();"
  , "        if (!ids.length) { said(b, \"no row\"); return; }"
  , "        ask(`set state · ${ids.length} row${ids.length === 1 ? \"\" : \"s\"}`,"
  , "            stateChoices(),"
  , "            (c) => fire(b, \"set-state\", ids, { keyword: c.keyword },"
  , "                        c.keyword === null ? CLEAR : c.keyword));"
  , "      },"
  , "      quitWindow: () =>"
  , "        (editing ? leave() : log(\"q closes the sheet; there is no window to quit\")),"
    -- One key out of whichever overlay is up: the prompt first, since it is the
    -- one that can be raised over an open sheet.
  , "      cancel: () => {"
  , "        if (prompting) unask();"
    -- The panel's open row is a rung of its own, under the sheet's: while one
    -- is open ESC puts it back, and only from nav does the key reach the sheet.
  , "        else if (pedit !== -1) cancelRow();"
  , "        else if (editing) leave();"
  , "        else if (settings) leaveSettings();"
  , "        else if (typing()) document.activeElement.blur();"
  , "      },"
  , "      // The filter's own backspace: the renderer drops the token and the"
  , "      // shell follows it — one commit, one URL, focus left on the table."
  , "      filterDrop: () => {"
  , "        if (!strips()) { echo(\"DEL → this table-view.js has no filter tokens\"); return; }"
  , "        if (!table.stripLastToken()) { echo(\"DEL → no filter\"); return; }"
  , "        const left = table.getQuery().trim();"
  , "        commit(left);"
  , "        echo(left ? `DEL → filter: ${JSON.stringify(left)}` : \"DEL → filter cleared\");"
  , "      },"
  , "    };"
  , "    // The row is handed to its handler: one that names what it landed on"
  , "    // — the filter left, the column arrived at — echoes over this line with"
  , "    // the same `seq → command' opening."
  , "    function run(b) {"
  , "      echo(`${b.seq} → ${b.command}${b.help ? ` · ${b.help}` : \"\"}`);"
  , "      const handler = b.handler && HANDLERS[b.handler];"
  , "      if (handler) handler(b);"
  , "      else log(`${b.seq} (${b.command}) — arrives with daemon commands (M4)`);"
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
    -- The prompt's own keys, behind the dispatch above: while its field has
    -- focus the only row that can have fired already is ESC, which is the one
    -- that should.  C-n and C-p are reserved chords the map never claims, and
    -- claiming them HERE is the field's business rather than the map's — the
    -- same way a focused select keeps its arrows.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!prompting) return;"
  , "      const k = keyName(e);"
  , "      const step = k === \"<down>\" || k === \"C-n\" ? 1"
  , "                 : k === \"<up>\" || k === \"C-p\" ? -1 : 0;"
  , "      if (step) walkChoices(step);"
  , "      else if (k === \"RET\") takeChoice();"
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
  , "      socket.onopen = () => { backoff = 1000; dot(\"live\"); };"
  , "      socket.onmessage = (e) => apply(JSON.parse(e.data));"
  , "      socket.onclose = (e) => {"
  , "        socket = null;"
  , "        dot(\"down\");"
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
  , "        log(a.view ? \"reconnected · rows refreshed\" : \"reconnected\");"
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
  , "      log(`disconnected · retrying in ${Math.round(backoff / 1000)}s`);"
  , "      setTimeout(resync, backoff);"
  , "      backoff = Math.min(backoff * 2, 30000);"
  , "    }"
  , "    // The server binds before it walks the tree, so the first fetch of a"
  , "    // cold daemon is a 503: show what it is doing and ask again in a second."
  , "    // A daemon that restarts under a live page lands here too, and comes"
  , "    // back through `resync' — the page it left is still on screen."
  , "    function indexing(b) {"
  , "      dot(\"wait\");"
  , "      log(`indexing … ${b.elapsed}s · the table opens when the walk lands`);"
  , "      setTimeout(resync, 1000);"
  , "    }"
  , "    function start() {"
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
  , "      const narrow = asking(asked) + (asked ? \"&\" : \"?\");"
  , "      load(`${narrow}limit=${PAGE}`).then((a) => {"
  , "        mount(a.view);"
  , "        listen();"
  , "        // The rest behind the painted table: n/p, sort and materialize all"
  , "        // want the whole answer, and the renderer holds it without the DOM."
  , "        if (a.total > (a.view.rows || []).length)"
  , "          load(asking(asked))"
  , "            .then((b) => { if (table && query === asked) paint(b); arm(a.total); })"
  , "            .catch(quiet);"
  , "        else arm(a.total);"
  , "      }).catch((e) => {"
  , "        if (e.indexing) return indexing(e.indexing);"
  , "        dot(\"down\"); quiet(e); if (e.name !== \"AbortError\") again();"
  , "      });"
  , "    }"
  , "    start();"
  , "  </script>"
  ]

-- | The page a browser gets when the renderer is missing: what still works,
-- and the flag that fixes it.
assetsMissing :: ServeOptions -> Text
assetsMissing opts = page "" "glance — JSON only" $ T.unlines
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
  -- The height it asks for, and none it cannot give back: a window shorter
  -- than the column's parts takes the difference out of the table, which
  -- scrolls inside itself, rather than off the key line, which does not.
  , "  #app{height:80vh;min-height:0}"
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
  , "  #log{font-size:12px;color:var(--g-mute);padding:6px 10px;"
  , "    border:1px solid var(--g-border);border-radius:8px;"
  , "    background:var(--g-surface);flex:1 1 auto;overflow-y:auto}"
  -- The resident key line, and the page's last: what can run, where the echo
  -- pill says what just did.  Slim and muted, so it reads as chrome rather
  -- than as content; one line that scrolls sideways instead of wrapping, so a
  -- narrow window cannot grow it into the table's room.
  , "  #kbd{flex:none;font-size:11px;color:var(--g-mute);white-space:nowrap;"
  , "    overflow-x:auto;padding:0 2px}"
  -- The status corner: the connection dot and the theme, together, clear of the
  -- table and out of the heading.
  , "  #corner{position:fixed;top:12px;right:14px;z-index:3;display:flex;gap:6px;"
  , "    align-items:center;font-size:11px;color:var(--g-mute)}"
  , "  #corner:hover,#corner:focus-within{color:var(--g-fg)}"
  , "  #dot{display:inline-block;width:7px;height:7px;border-radius:50%;"
  , "    background:var(--g-mute);transition:background .3s}"
  , "  #dot.live{background:var(--g-ok)}"
  , "  #dot.wait{background:var(--g-warn)}"
  , "  #dot.down{background:var(--g-mute)}"
  , "  #corner select{font:inherit;font-family:var(--glance-mono);padding:1px 4px;"
  , "    border-radius:4px;border:1px solid var(--g-border);background:var(--g-bg);"
  , "    color:inherit}"
  , "  #corner option{background:var(--g-bg);color:var(--g-fg)}"
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
  , "  #modal,#prompt,#config{--dk-mono:\"Hack\", var(--glance-mono);"
  , "    display:none;position:fixed;inset:0;z-index:100;padding:24px;background:#0009;"
  , "    align-items:center;justify-content:center}"
  , "  #modal.on,#prompt.on,#config.on{display:flex}"
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
  , "  #mtext{flex:2 1 320px;min-width:0;font:12px/1.5 var(--dk-mono);padding:8px;"
  , "    border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit;resize:none}"
  , "  #mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- The property panel: a row per property, the last one empty so there is
  -- always somewhere to type the next.  They sit in file order because that is
  -- the order they are in — nothing here sets a tab index, and the keys that
  -- move over them and open one are the glue's.
  , "  #mprops{flex:1 1 240px;min-width:0;overflow-y:auto;"
  , "    display:flex;flex-direction:column;gap:4px}"
  , "  #sheet.raw #mprops{display:none}"
  , "  .prow{display:flex;flex-wrap:wrap;gap:4px;border-radius:4px}"
      -- Minimal chrome: a cell is bare text whichever mode it is in — the same
      -- box either way, so opening a row moves nothing — the 4px row gap is the
      -- separation, and the border exists only under the focused field so the
      -- keyboard's place stays visible.  The row at point wears the page's own
      -- selection, and only while the panel is the thing holding the keys.
  , "  .prow input,.prow span{font:12px/1.5 var(--dk-mono);padding:4px 6px;"
  , "    border:none;border-bottom:1px solid transparent;"
  , "    background:transparent;color:inherit;min-width:0}"
  , "  .prow span{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
  , "  .prow span:empty::after{content:attr(data-hint);color:var(--g-mute)}"
  , "  .prow input:focus{outline:none;border-bottom-color:var(--g-border)}"
  , "  #mprops.on .pat{background:var(--g-sel);color:var(--g-fg)}"
  , "  .pkey{flex:1 1 40%}"
  , "  .pval{flex:2 1 50%}"
  , "  .prow input::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- The three planning rows: org's keys rather than the author's, so their key
  -- cell is muted to say it is a label and not a field.  They sit above the
  -- properties, which is where the file writes them.
  , "  .pln .pkey{color:var(--g-mute)}"
  -- The logbook: full width under both panes, muted, read-only and out of the
  -- tab order — it is the server's, and there is nothing here to press.
  , "  #mlog{display:none;flex:0 0 auto;max-height:22vh;overflow:auto;margin:0;"
  , "    font:11px/1.5 var(--dk-mono);color:var(--g-mute);white-space:pre-wrap;"
  , "    padding:6px 8px;border:1px solid var(--g-border);border-radius:4px}"
  , "  #mlog.on{display:block}"
  , "  #sheet.raw #mlog{display:none}"
  -- The value palette.  Narrow, since what it holds is a word: the title says
  -- what is being set and over how many rows, the field narrows the list, and
  -- the row under point wears the page's own selection colour.
  , "  #pbox{display:flex;flex-direction:column;gap:6px;padding:10px;border-radius:6px;"
  , "    position:relative;z-index:101;"
  , "    width:min(420px,100%);font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #phead{font-size:12px;color:var(--g-mute)}"
  , "  #pinput{font:12px/1.5 var(--dk-mono);padding:5px 7px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  , "  #plist{max-height:40vh;overflow-y:auto;font-size:12px}"
  , "  #plist div{padding:2px 7px;border-radius:4px}"
  , "  #plist .pat{background:var(--g-sel);color:var(--g-fg)}"
  -- The settings sheet, third in the same two bands: one section per keyword
  -- layer, each a label saying which file it is and a box holding that file's
  -- `#+TODO:' lines.  High rather than centred, since the sections grow
  -- downward and the header over them should not move when they do.
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
  , "  #clayers,.crow{display:flex;flex-direction:column;gap:4px}"
  -- A path is long and has nowhere to wrap, so it is told it may break
  -- anywhere rather than widening the sheet past the viewport.
  , "  .clab{font-size:11px;color:var(--g-mute);overflow-wrap:anywhere}"
  , "  .ctext{font:12px/1.5 var(--dk-mono);padding:6px;border-radius:4px;height:3.4em;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit;resize:vertical}"
  , "  .ctext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- The default view, one line under the system layer's cycle.
  , "  .cview{font:12px/1.5 var(--dk-mono);padding:6px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  -- What the server said about the last write to that layer, and nothing when
  -- it said nothing.
  , "  .cerr{font-size:11px;color:var(--g-bad)}"
  , "  .cerr:empty{display:none}"
  , "  #ceff{font-size:12px;padding-top:8px;border-top:1px solid var(--g-border)}"
  , "  #cfoot{font-size:11px;color:var(--g-mute)}"
  -- The gear is the coarse pointer's only way in, so a fine pointer never sees
  -- it: the rule that shows it is in the one media block below.
  , "  #gear{display:none}"
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
  , "    #mtext,#pinput,.prow input,.ctext,.cview{font-size:16px}}"
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
