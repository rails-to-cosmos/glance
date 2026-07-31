-- | The M1 web layer: headlines out of a directory, into a browser tab, and
-- kept current there.
--
-- This component's build-depends names the public @glance@ library and the
-- HTTP packages.  @glance-internal@ is absent, so @Data.Org.*@ is out of scope
-- here and reaching for it means writing the dependency down where anyone
-- reading the stanza sees it.  That is the facade invariant
-- (docs/invariants.md, Architecture), kept where the solver can check it.
--
-- Five routes: @GET \/headlines@ is the view JSON, @GET \/@ a demo shell that
-- fetches it, @GET \/ws@ the live row stream, @GET \/NAME@ an asset out of the
-- @--assets@ directory, and @\/headline@ the materialize round-trip — @GET@ for
-- one headline's raw subtree, @POST@ to write an edited one back.  The view's
-- field set is the contract (@table-view\/SCHEMA.md@), so the load counts ride
-- along as @X-Glance-*@ response headers and leave the body's shape alone.
--
-- @\/headlines@ takes @q@, @limit@ and @offset@, filters before it pages, and
-- reports the match count and whether more follows in that same header family.
-- It carries an @ETag@ of the store's generation under @Cache-Control:
-- no-cache@, so a browser revalidates every time and pays for bytes only when
-- something in the tree moved; @gzip@ sits over the whole HTTP app.  See
-- 'headlines' for why one generation covers every query variant.
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
-- The page's keys are 'sharedKeys' — org-glance's @overview-mode@ map under
-- org-glance's own command names — over a movement profile out of
-- 'keyProfiles', and it carries both as JSON for its own dispatch to parse, so
-- the map and the handlers cannot drift apart.
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
                  , limitCap
                  , serve
                  , serveWith
                  , viewTitleFor
                  ) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (filterM, forever, unless, void, when)
import Data.Aeson (eitherDecode', encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (Pair, parseEither)
import Data.Bifunctor (first)
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

import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

import Glance.Query ( HeadlineRecord (hrDigest, hrFile, hrId, hrSubtree)
                    , IdCollision (..), QueryResult (..), Span (spanEnd, spanStart)
                    , WalkOptions (..), WriteFailure (..), replaceSpan, sortedForView
                    , subtreeText, viewJSONTextWith )
import Glance.Web.Filter (matchesFilter)
import Glance.Web.Store ( Client, Frame (ViewChanged), Hub, LoadState (..)
                        , Store (stGen), finishLoading, frameText, hubLoad, hubStore
                        , loadStoreWith, newLoadingHub, nextFrame, storeHeadline
                        , storeKeywords, storeResult, storeTags, subscribe
                        , unsubscribe )
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
httpApp opts hub request respond = route (pathInfo request) >>= respond
  where
    method  = requestMethod request
    reading = method `elem` [methodGet, methodHead]
    route path = do
      load <- readTVarIO (hubLoad hub)
      case load of
        Loading since | path `elem` storeRoutes -> indexing since
        _ready                                  -> ready path
    storeRoutes = [["headlines"], ["headline"], ["ws"]]
    ready ["headline"]
      | reading              = materialize hub (queryId request)
      | method == methodPost = commit hub (queryId request) request
      | otherwise            = pure (jsonError status405 "/headline takes GET and POST")
    ready _
      | not reading          = pure (plain status405 writeHint)
    ready []                 = shellPage opts
    ready ["headlines"]      = headlines opts hub request
    ready ["ws"]             = pure (plain status400 wsHint)
    ready [name]
      | safeName name        = asset opts (T.unpack name)
    ready _                  = pure (plain status404 notFound)
    wsHint    = "/ws is a websocket endpoint; connect with Upgrade: websocket"
    writeHint = "method not allowed; POST /headline?id=… is the one route that writes"
    notFound  = "not found: /, /headlines, /headline, /ws, or an asset name"

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
-- columns and over every org tag the store carries, free text, negation,
-- same-key predicates ORing ('Glance.Web.Filter') — @limit@ a page size,
-- absent meaning the whole set,
-- which is what every client before this asked for, and @offset@ where the page
-- starts.  Filtering happens before paging, so @X-Glance-Total@ is the match
-- count and @X-Glance-Has-Next@ says whether a further page exists.
-- The body stays a View: SCHEMA.md fixes its fields, so paging metadata rides
-- in the same @X-Glance-*@ family the load counts already use.
--
-- A page is cut out of the view's declared sort ('Glance.Query.sortedForView'),
-- never out of walk order — page two has to be the rows the table would show
-- after page one.  With no @limit@ the walk order stands and the client sorts
-- the whole set itself, which is the full-fidelity mode; under a limit a
-- client-side re-sort reorders the loaded page alone.
--
-- Caching.  The @ETag@ is the store's generation, which the watcher moves
-- whenever a response would change, and @Cache-Control: no-cache@ makes every
-- browser revalidate rather than guess a lifetime.  One tag serves every query
-- variant: @q@, @limit@ and @offset@ are in the URL, and an HTTP cache is
-- keyed by URL, so @?q=foo@ and @?q=bar@ are separate entries that each
-- revalidate against their own stored tag.  A response is a function of
-- (generation, URL) and nothing else in the request, so no @Vary@ is owed for
-- them — the one header the answer does turn on is @Accept-Encoding@, and the
-- gzip middleware writes that @Vary@ itself.
headlines :: ServeOptions -> Hub -> Request -> IO Response
headlines opts hub request = case pageParams request of
  Left why -> pure (jsonError status400 why)
  Right (q, limit, offset) -> do
    st <- readTVarIO (hubStore hub)
    let tag = etagOf (stGen st)
    if tag `elem` ifNoneMatch request
      then pure (responseLBS status304 (cacheHeaders tag) "")
      else do
        let qr      = storeResult st
            matched = filter (matchesFilter (storeTags st) q) (qrRecords qr)
            total   = length matched
            shown   = maybe matched (\n -> take n (drop offset (sortedForView matched))) limit
            hasNext = maybe False (\n -> offset + n < total) limit
            body    = TLE.encodeUtf8
                        (viewJSONTextWith (viewTitleFor dir) (storeKeywords st) shown)
        -- The encode is lazy, so it needs its own 'try': an exception raised
        -- inside warp's sender would truncate a 200 that has already gone out.
        forced <- try (evaluate (BL.length body))
        pure $ case forced of
          Left err -> plain status500 (renderError err)
          Right _n -> sized status200
            (jsonType : cacheHeaders tag <> statsHeaders qr <> pageHeaders total hasNext) body
  where dir = soDir opts
        renderError :: SomeException -> Text
        renderError e = "headline render failed: " <> T.pack (displayException e)

-- | The largest page a client may ask for in one request.  Well past a
-- screenful and well short of a number that means the caller lost track: an
-- explicit @limit@ over this is a mistake worth naming rather than silently
-- trimming.  Asking for no limit at all still serves the whole store.
limitCap :: Int
limitCap = 20000

-- | GEN as an entity tag.  Opaque to a client, which only ever compares it to
-- the one it was given.
etagOf :: Int -> BSC.ByteString
etagOf gen = "\"g" <> BSC.pack (show gen) <> "\""

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

-- | What the page covers of the filtered set.
pageHeaders :: Int -> Bool -> [Header]
pageHeaders total hasNext =
  [ ("X-Glance-Total", BSC.pack (show total))
  , ("X-Glance-Has-Next", if hasNext then "true" else "false") ]

-- | @q@, @limit@ and @offset@ out of REQUEST's query string, or what is wrong
-- with one of them.  An absent parameter is its default — no filter, no limit,
-- the top of the set — and a present one that is not a number is a 400 rather
-- than a silent fallback to it, since a mistyped page size that quietly serves
-- the whole store looks like a working request.
pageParams :: Request -> Either Text (Text, Maybe Int, Int)
pageParams request = do
  q      <- maybe (Right "") text (raw "q")
  limit  <- traverse count (raw "limit")
  offset <- maybe (Right 0) count (raw "offset")
  case limit of
    Just n | n > limitCap -> Left ("limit is at most " <> T.pack (show limitCap)
                                     <> "; page with offset for more")
    _within                -> Right (q, limit, offset)
  where
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
materialize :: Hub -> Maybe Text -> IO Response
materialize _hub Nothing = pure (jsonError status400 "GET /headline?id=<row id>")
materialize hub (Just rid) = do
  found <- storeHeadline rid <$> readTVarIO (hubStore hub)
  pure $ case found of
    Nothing -> jsonError status404 ("no headline with id " <> rid)
    Just r  -> jsonResponse status200
      [ "id"     .= hrId r
      , "file"   .= hrFile r
      , "org"    .= subtreeText r
      , "digest" .= hrDigest r
      , "span"   .= object [ "start" .= spanStart (hrSubtree r)
                           , "end"   .= spanEnd (hrSubtree r) ]
      ]

-- | @POST \/headline?id=…@ with body @{"org": …, "digest": …}@: the headline's
-- subtree replaced by the text the client edited.
--
-- Two digest checks, one lock.  The client's digest must be the one the store
-- holds, or the file has been re-parsed since and the client is editing a
-- subtree measured at offsets that have moved; and 'replaceSpan' re-digests
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
  case (body, found) of
    (Nothing, _) -> pure (jsonError status413 ("body over " <> T.pack (show bodyLimit) <> " bytes"))
    (_, Nothing) -> pure (jsonError status404 ("no headline with id " <> rid))
    (Just raw, Just r) -> case parseCommit raw of
      Left why -> pure (jsonError status400 why)
      Right (org, digest)
        | digest /= hrDigest r -> pure (conflict "stale" (hrDigest r) reparsed)
        | otherwise -> do
            written <- replaceSpan (hrFile r) digest (hrSubtree r) org
            pure $ case written of
              Right fresh             -> jsonResponse status200 ["digest" .= fresh]
              Left (WriteDrift onDisk) -> conflict "drift" onDisk rewritten
              Left (WriteRefused why)  -> jsonError status500 why
  where
    reparsed, rewritten :: Text
    reparsed  = "the file was re-read since this subtree was materialized"
    rewritten = "the file changed on disk since this subtree was materialized"
    conflict :: Text -> Text -> Text -> Response
    conflict reason current why = jsonResponse status409
      [ "error"  .= (why <> "; materialize it again and re-apply the edit")
      , "reason" .= reason
      , "digest" .= current
      ]

-- | The @id@ parameter of REQUEST, when it carries one with a value.
queryId :: Request -> Maybe Text
queryId request = case lookup "id" (queryString request) of
  Just (Just raw) -> either (const Nothing) Just (TE.decodeUtf8' raw)
  _absent         -> Nothing

-- | The @org@ and @digest@ a commit body carries, or what is wrong with it.
parseCommit :: BL.ByteString -> Either Text (Text, Text)
parseCommit raw = first (("body: " <>) . T.pack) $ do
  value <- eitherDecode' raw
  parseEither (withObject "commit" (\o -> (,) <$> o .: "org" <*> o .: "digest")) value

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
  , kbSeq     :: !Text          -- ^ how the echo widget and the docs spell them.
  , kbCommand :: !Text          -- ^ the command name the echo widget shows.
  , kbHandler :: !(Maybe Text)  -- ^ the shell function running it; 'Nothing' is staged.
  , kbScope   :: !Text          -- ^ @table@, @modal@ or @any@ — where it is live.
  , kbHelp    :: !(Maybe Text)  -- ^ what it does, when the command name does not say; see 'helps'.
  }

-- | KEYS bound to a command, spelled the way Emacs spells a sequence: one
-- space between the keys.
bind :: [Text] -> Text -> Maybe Text -> Text -> KeyBinding
bind keys command handler scope = KeyBinding keys (T.unwords keys) command handler scope Nothing

-- | 'bind', spelled SHOWN.  vi runs @gg@ together where Emacs would write
-- @g g@, and the echo widget owes the reader the notation they typed in.
bindAs :: Text -> [Text] -> Text -> Maybe Text -> Text -> KeyBinding
bindAs shown keys command handler scope = KeyBinding keys shown command handler scope Nothing

-- | B with the one line the echo widget shows past its command name.  A row
-- earns one where the name is the Emacs name for a key whose behaviour here is
-- narrower than the name — @save-buffer@ on a sheet that syncs itself, and the
-- @keyboard-quit@ that flushes on the way out.
helps :: KeyBinding -> Text -> KeyBinding
helps b text' = b { kbHelp = Just text' }

-- | The rows both profiles carry: every command that is not movement, plus the
-- movement no editor argues about — the arrows, and org-glance's own
-- buffer-ends keys.
--
-- These are org-glance's command names (@org-glance-overview-mode-map@, plus
-- @C-x C-s@ for the sheet, which is Emacs's).  A row with no handler is
-- recognized in full and then says what it is waiting for: the map is complete
-- ahead of the daemon commands that will back it (M4), which reads better than
-- a key that silently does nothing.
--
-- Claimed chords, and only these.  @C-c@ becomes a prefix while no text field
-- has focus and the selection is collapsed, so a copy is still a copy; @C-x@
-- likewise, and only while the sheet is open, which is the only place @C-x
-- C-s@ means anything.  @RET@, @TAB@, @\/@ and @DEL@ are taken while the table
-- has focus — @DEL@ is the filter's own undo, and a field with focus keeps its
-- backspace.  Everything else reaches the browser — @C-l@, @C-r@, @C-t@,
-- @C-w@, @C-n@, @C-p@ and @\<f5\>@ even as the continuation of a prefix this
-- map entered, which is why neither profile moves on @C-n@ or @C-p@.
sharedKeys :: [KeyBinding]
sharedKeys =
  [ bind ["<down>"]     "next-row"                        (Just "nextRow")        "table"
  , bind ["<up>"]       "previous-row"                    (Just "previousRow")    "table"
  , bind [","]          "first-row"                       (Just "firstRow")       "table"
  , bind ["<"]          "first-row"                       (Just "firstRow")       "table"
  , bind ["."]          "last-row"                        (Just "lastRow")        "table"
  , bind [">"]          "last-row"                        (Just "lastRow")        "table"
  , bind ["RET"]        "org-glance-overview:materialize" (Just "materializeRow") "table"
  , bind ["/"]          "filter-rows"                     (Just "focusFilter")    "table"
  , bind ["DEL"]        "filter-drop-token"               (Just "filterDrop")     "table"
      `helps` "drop the filter's last token"
  , bind ["q"]          "quit-window"                     (Just "quitWindow")     "table"
  , bind ["TAB"]        "org-cycle"                       Nothing                 "table"
  , bind ["!"]          "org-glance-overview:open"        Nothing                 "table"
  , bind ["a"]          "org-glance-agenda"               Nothing                 "table"
  , bind ["@"]          "org-glance-overview:relations"   Nothing                 "table"
  , bind ["+"]          "org-glance-overview:capture"     Nothing                 "table"
  , bind ["D"]          "org-glance-overview:delete"      Nothing                 "table"
  , bind ["C-c", "C-t"] "org-glance-overview:todo"        Nothing                 "table"
  , bind ["C-c", "C-s"] "org-glance-overview:schedule"    Nothing                 "table"
  , bind ["C-c", "C-d"] "org-glance-overview:deadline"    Nothing                 "table"
  , bind ["C-x", "C-s"] "save-buffer"                     (Just "save")           "modal"
      `helps` "sync the sheet now; again to overwrite a conflict"
  , bind ["ESC"]        "keyboard-quit"                   (Just "cancel")         "any"
      `helps` "close the sheet, syncing an edited one; again to discard"
  ]

-- | Movement as org-glance's overview binds it, and what a page starts on.
-- @j@ is the overview's open-stub here, where vi needs it for down.
--
-- @f@ and @b@ are the same-level rhyme one granularity down: where org-glance
-- walks headlines with them, a table walks the cells of a row.
emacsKeys :: [KeyBinding]
emacsKeys =
  [ bind ["n"] "next-row"                    (Just "nextRow")        "table"
  , bind ["p"] "previous-row"                (Just "previousRow")    "table"
  , bind ["f"] "next-column"                 (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["b"] "previous-column"             (Just "previousColumn") "table"
      `helps` previousColumnHelp
  , bind ["g"] "org-glance-overview:refresh" (Just "refresh")        "table"
  , bind ["j"] "org-glance-overview:open"    Nothing                 "table"
  ]

-- | Movement as vi binds it.  Two rows move to make room, and both moves are
-- the reason this profile is data rather than a second dispatch: @j@ is down
-- here, so the open-stub is left to @\!@ — which the overview map already
-- carries, as its dired-execute rhyme — and @g@ is a prefix rather than a
-- command, so refresh goes to @R@ and reads as vi's reload.  Nothing in this
-- profile binds a bare @g@: a complete sequence that also opens a longer one
-- would make the longer one unreachable.
--
-- @h@ and @l@ are the cell movement @f@ and @b@ are under @emacs@; the command
-- names are shared, so only the keys differ.
vimKeys :: [KeyBinding]
vimKeys =
  [ bind   ["j"]           "next-row"                    (Just "nextRow")        "table"
  , bind   ["k"]           "previous-row"                (Just "previousRow")    "table"
  , bind   ["l"]           "next-column"                 (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind   ["h"]           "previous-column"             (Just "previousColumn") "table"
      `helps` previousColumnHelp
  , bindAs "gg" ["g", "g"] "first-row"                   (Just "firstRow")       "table"
  , bind   ["G"]           "last-row"                    (Just "lastRow")        "table"
  , bind   ["R"]           "org-glance-overview:refresh" (Just "refresh")        "table"
  ]

-- | The cell-movement help lines, one pair for both profiles: the keys differ,
-- what they do does not.  Between them they say the whole rule — the column
-- rides along with row movement, and a whole-row selection starts at the first
-- column whichever direction asks for one.
nextColumnHelp, previousColumnHelp :: Text
nextColumnHelp     = "the cell to the right; row movement keeps the column"
previousColumnHelp = "the cell to the left; from a whole row, the first column"

-- | The movement profiles, by the name @?keys=@ and the toggle use.  Adding
-- one is adding a row: the shell reads them out of the blob and its toggle
-- cycles whatever it finds.
keyProfiles :: [(Text, [KeyBinding])]
keyProfiles = [("emacs", emacsKeys), ("vim", vimKeys)]

-- | The profile a page starts on with nothing stored and nothing asked for.
defaultProfile :: Text
defaultProfile = "emacs"

-- | The keymap as the page carries it: the shared rows once, the movement
-- profiles beside them, and the name to start on.  The angle brackets are
-- escaped because four of these sequences are angle brackets — a blob that
-- cannot spell a tag cannot open one, whatever element it sits in, and
-- @JSON.parse@ undoes them.
--
-- The shell parses this instead of holding a second copy, so a key cannot be
-- bound and undocumented, and a profile cannot be offered and unbound.
keyBindingsJSON :: Text
keyBindingsJSON = T.replace "<" "\\u003c" . T.replace ">" "\\u003e"
                . TE.decodeUtf8 . BL.toStrict . encode $ object
  [ "shared"   .= map row sharedKeys
  , "default"  .= defaultProfile
  , "profiles" .= object [ Key.fromText name .= map row rows | (name, rows) <- keyProfiles ]
  ]
  where row b = object [ "keys"    .= kbKeys b
                       , "seq"     .= kbSeq b
                       , "command" .= kbCommand b
                       , "handler" .= kbHandler b
                       , "scope"   .= kbScope b
                       , "help"    .= kbHelp b ]

-- Pages

-- | @\/@: the demo shell when the renderer is on disk, and an explanation when
-- it is not.  A missing renderer leaves @\/headlines@ untouched — the server
-- is a JSON server that happens to ship a page.
shellPage :: ServeOptions -> IO Response
shellPage opts = do
  ok <- hasRenderer opts
  font <- localFont opts
  pure (html (if ok then demoShell opts font else assetsMissing opts))

-- | The page a browser gets: load the renderer, fetch a page of the view,
-- mount it, then hold a socket open and apply what it sends.  The glue is
-- inline so the shell has exactly one asset to find.
--
-- The boot is two fetches, and both are @\/headlines@.  The first asks for
-- 1000 rows so the table paints without waiting on the whole store; the
-- response's @X-Glance-Total@ says whether there are more, and the rest is
-- fetched behind the painted table.  The full local set is what keeps @n@,
-- @p@, sorting and materialize coherent — the renderer virtualizes, so holding
-- 13k rows costs memory and no DOM.  The socket then opens with
-- @?bootstrap=off@: the rows are already here and the server's opening
-- @set-rows@ would only send them again.
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
-- The applied query is page state.  It goes into the URL on every commit
-- (@replaceState@, leaving @keys@ where it is), so a filtered view is a link, a
-- reload keeps it and a reconnect comes back to it.  It is restored by handing
-- it to @mount@ as @initialQuery@, which tokenizes it into the renderer's own
-- committed chips and delivers nothing — the rows in hand are already the
-- server's answer to it.  Every return through this door restores it the same
-- way, since a reload, a reconnect, @view-changed@ and @g@ all re-fetch and
-- re-mount.  An asset predating that option drops it silently, so the mount
-- asks @getQuery@ whether it took and falls back to writing the query into the
-- filter box, which is how this worked before chips could carry it.  @DEL@ over
-- the table is that query's own backspace: it takes the last token off — quotes
-- and all — and commits what is left, which clears the filter once the last
-- token goes.
--
-- The materialize sheet has no buttons.  @ESC@ or a click on the backdrop
-- closes it, flushing first when the text has moved and closing on the 200; a
-- pristine sheet closes with no request at all, so opening one and reading it
-- never touches the file.  @C-x C-s@ flushes mid-edit, and the receipt's digest
-- becomes the next flush's lock.  A 409 leaves the sheet open saying
-- @conflict@: @C-x C-s@ then re-reads the file's digest and posts the author's
-- text over it — last writer wins, on a deliberate keystroke — and @ESC@
-- discards.  A tab closing on an edited sheet flushes with @keepalive@.  The
-- header carries the state in one word, @synced@ \/ @syncing…@ \/ @conflict@,
-- and the sheet wears the author's Emacs theme (danneskjold) while the table
-- keeps the page's.
--
-- Every close leads back through the same door: re-fetch, re-mount, reconnect.
-- That covers a daemon restart, a dropped slow client, and @view-changed@ —
-- the columns moving, which SCHEMA.md's row ops cannot express.
--
-- The @materialize@ action opens the subtree over the table in a plain
-- @textarea@ filled by @GET \/headline@.  A commit never touches the table —
-- the row arrives over the socket when the watch has re-read the file, which is
-- the same way it would arrive had the edit come from an editor.  A real editor
-- component is M3.5; a textarea is what proves the round-trip.
--
-- The keys are 'keyBindingsJSON', which the glue parses: row movement runs
-- over the renderer's @getVisible@ and @select@, since a virtualized row
-- outside the window has no element to click, and a sequence with no handler
-- echoes its org-glance command name and what it is waiting for.  Cell
-- movement is the same @select@ with a column: the column lives in the
-- renderer's selection rather than here, so it survives a profile switch,
-- rides along with row movement, and goes when the selection does.  A
-- whole-row selection has no column and keeps the look it always had until a
-- horizontal key lands on the first one; the echo names the column it arrived
-- at by the header over it, or says which edge it stopped at.  The pill in
-- the bottom corner is the echo area — the pending prefix while one is open,
-- the command and its help line on completion, @is undefined@ otherwise.  The
-- top corner holds the connection dot and a @select@ of the movement profiles
-- the blob declares, which rebinds in place and remembers the choice; a native
-- control because Tab, the arrows and Enter already navigate one and no new
-- chord is owed for it.
demoShell :: ServeOptions -> Maybe FilePath -> Text
demoShell opts font = page (fontFace font) (viewTitleFor (soDir opts)) $ T.unlines
  -- No heading: the renderer's omnibox is the top of the page and the view
  -- title is already the tab's.  Printing it a second time here put it on
  -- screen twice.
  [ "  <div id=\"corner\"><span id=\"dot\" title=\"live connection\"></span>"
      <> "<label for=\"themesel\">theme:</label>"
      <> "<select id=\"themesel\" title=\"colour theme\">"
      <> "<option value=\"auto\">auto</option><option value=\"light\">light</option>"
      <> "<option value=\"dark\">dark</option></select>"
      <> "<label for=\"keysel\">keys:</label>"
      <> "<select id=\"keysel\" title=\"movement profile\"></select></div>"
  , "  <div id=\"app\"></div>"
  , "  <div id=\"log\">loading …</div>"
  , "  <div id=\"modal\">"
  , "    <div id=\"sheet\">"
  , "      <div id=\"mhead\"><span id=\"mfile\"></span><span id=\"mnote\"></span></div>"
  , "      <textarea id=\"mtext\" spellcheck=\"false\"></textarea>"
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
  , "    const dot = (state) => (document.getElementById(\"dot\").className = state);"
  , "    const el = (id) => document.getElementById(id);"
  , "    let table = null, socket = null, backoff = 1000, editing = null;"
  , "    // The sheet's own state: the text the file holds as far as this page"
  , "    // knows, what the last flush ran into, and the flush still in flight."
  , "    let base = \"\", trouble = null, flushing = null;"
  , "    // The server filters and pages; these hold the query it was last asked"
  , "    // with, the fetch still in flight for it, and the selected row's id."
  , "    let query = \"\", inflight = null, cursor = null, requeryAt = 0;"
  , "    const PAGE = 1000;   // rows in the first paint; the rest follows it"
  , "    function mount(view) {"
  , "      table = TableView.mount(document.getElementById(\"app\"), view, {"
  , "        omnibox: true,     // the filter is the page's one hero input"
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
  , "      cursor = null;"
  , "      // The columns are the view's: both halves of a filter read the keys"
  , "      // out of them (`parity'), and cell movement names its landing column"
  , "      // by the header sitting over it."
  , "      cols = view.columns || [];"
  , "      columnKeys = cols.map((c) => c.key);"
  , "      say();"
  , "    }"
  , "    const say = () => log(`${table ? table.getRows().length : 0}`"
  , "      + ` ${query ? `matching ${query}` : \"headlines\"} · ${profile} keys`"
  , "      + \" · RET materializes · / filters\");"
  , "    // One /headlines at a time: a keystroke aborts the fetch before it, so"
  , "    // an earlier answer can never land over a later one."
  , "    function load(params) {"
  , "      if (inflight) inflight.abort();"
  , "      inflight = new AbortController();"
  , "      return fetch(`/headlines${params}`, { signal: inflight.signal }).then((r) =>"
  , "        r.ok ? r.json().then((view) => ({ view, total: +r.headers.get(\"X-Glance-Total\") }))"
  , "        // 503 is the startup walk: the server is listening and says so"
  , "        // in the body.  `start' polls it; nothing else can see it."
  , "        : r.status === 503 ? r.json().then((b) => { throw Object.assign(new Error(\"indexing\"), { indexing: b }); })"
  , "             : r.text().then((t) => { throw new Error(t); }));"
  , "    }"
  , "    const quiet = (e) => { if (e.name !== \"AbortError\") log(`load failed: ${e.message}`); };"
  , "    // The unfiltered answer is kept: with a filter on, the loaded rows are"
  , "    // the server's answer to it and cannot be used to check that answer."
  , "    let all = [], cols = [], columnKeys = [];"
  , "    const paint = (a) => {"
  , "      const rows = a.view.rows || [];"
  , "      table.setRows(rows);"
  , "      if (!query) all = rows;"
  , "      parity(a.total);"
  , "      say();"
  , "    };"
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
  , "      const loose = TableView.parseQuery(query, columnKeys).filter((t) =>"
  , "        t.key === null && !t.quoted && !t.negated && /^[^:=]+[:=]./.test(t.value));"
  , "      if (!loose.length) return;"
  , "      const wants = loose.map((t) => t.value.slice(t.value.search(/[:=]/) + 1).toLowerCase());"
  , "      const text = (r) => columnKeys.map((k) => TableView.displayText((r.cells || {})[k]))"
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
  , "    // view is a link and a reconnect comes back to it; the query it"
  , "    // replaces goes on a stack DEL walks back.  The shell sends the string"
  , "    // as typed — the grammar is the server's to parse (SCHEMA.md)."
  , "    const params = () => new URLSearchParams(location.search);"
  , "    const urlQuery = () => params().get(\"q\") || \"\";"
  , "    function remember(q) {"
  , "      const p = params();"
  , "      if (q) p.set(\"q\", q); else p.delete(\"q\");"
  , "      const s = p.toString();   // `keys' and anything else in the URL survives"
  , "      history.replaceState(null, \"\", s ? `?${s}` : location.pathname);"
  , "    }"
  , "    // One place asks the server for rows: `query' is already what to ask."
  , "    const fetchRows = () =>"
  , "      load(query ? `?q=${encodeURIComponent(query)}` : \"\")"
  , "        .then((a) => table && paint(a)).catch(quiet);"
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
  , "    // The fallback for an asset without `initialQuery': the query goes in"
  , "    // the box rather than into chips.  The box is the renderer's, and"
  , "    // setting its value fires no input event, so a restored query shown"
  , "    // there is not committed a second time."
  , "    function showQuery() {"
  , "      const box = document.querySelector(\"#app .tv-filter\");"
  , "      if (box) box.value = query;"
  , "    }"
  , ""
  , "    function materialize(id) {"
  , "      fetch(`/headline?id=${encodeURIComponent(id)}`)"
  , "        .then((r) => r.json().then((b) => {"
  , "          if (!r.ok) throw new Error(b.error || r.status);"
  , "          return b;"
  , "        }))"
  , "        .then(show)"
  , "        .catch((e) => log(`materialize failed: ${e.message}`));"
  , "    }"
  , "    // The sheet is buttonless: it syncs on the way out.  `base' is the text"
  , "    // the file holds as far as this page knows — the materialized original,"
  , "    // then whatever the last 200 wrote — so `dirty()' is the whole of what"
  , "    // decides whether closing costs a POST at all."
  , "    function show(h) {"
  , "      editing = h; base = h.org; trouble = null;"
  , "      el(\"mfile\").textContent = `${h.file}  ·  ${h.id}`;"
  , "      el(\"mtext\").value = h.org;"
  , "      sync(\"synced\");"
  , "      el(\"modal\").className = \"on\";"
  , "      el(\"mtext\").focus();"
  , "    }"
  , "    const dirty = () => editing !== null && el(\"mtext\").value !== base;"
  , "    // With no buttons, the keys are the whole of the offer, so the two"
  , "    // states that wait for one say which key."
  , "    const WORDS = { synced: \"synced\", syncing: \"syncing…\","
  , "      conflict: \"conflict — C-x C-s overwrite · ESC discard\" };"
  , "    function sync(state, message) {"
  , "      el(\"mnote\").className = state;"
  , "      el(\"mnote\").textContent = message || WORDS[state];"
  , "    }"
  , "    const stuck = (why) => {"
  , "      trouble = \"error\";"
  , "      sync(\"error\", `${why} — C-x C-s retry · ESC discard`);"
  , "    };"
  , "    function shut() { el(\"modal\").className = \"\"; editing = null; base = \"\"; trouble = null; }"
  , "    // POST the textarea over the subtree, pinned to DIGEST.  A 200 carries"
  , "    // the file's new digest — the receipt chains, so the next flush needs no"
  , "    // re-materialize — and the baseline moves with it."
  , "    function flush(digest) {"
  , "      const h = editing, text = el(\"mtext\").value;"
  , "      sync(\"syncing\");"
  , "      flushing = fetch(`/headline?id=${encodeURIComponent(h.id)}`, {"
  , "        method: \"POST\","
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify({ org: text, digest }),"
  , "      })"
  , "        .then((r) => r.json().then((b) => ({ status: r.status, body: b })))"
  , "        .then((a) => {"
  , "          if (a.status === 200) {"
  , "            h.digest = a.body.digest; base = text; trouble = null; sync(\"synced\");"
  , "            return true;"
  , "          }"
  , "          if (a.status === 409) { trouble = \"conflict\"; sync(\"conflict\"); }"
  , "          else stuck(a.body.error || `sync failed (${a.status})`);"
  , "          return false;"
  , "        })"
  , "        .catch((e) => { stuck(e.message); return false; })"
  , "        .finally(() => (flushing = null));"
  , "      return flushing;"
  , "    }"
  , "    // C-x C-s.  Mid-edit it is a manual flush; on a conflict it is the"
  , "    // deliberate keystroke that overwrites — ask for the digest the file"
  , "    // carries now and post the text the author is looking at over it."
  , "    function save() {"
  , "      if (!editing || flushing) return;"
  , "      if (trouble !== \"conflict\") { flush(editing.digest); return; }"
  , "      const h = editing;"
  , "      fetch(`/headline?id=${encodeURIComponent(h.id)}`)"
  , "        .then((r) => r.json().then((b) => {"
  , "          if (!r.ok) throw new Error(b.error || r.status);"
  , "          return b;"
  , "        }))"
  , "        .then((b) => editing === h && flush(b.digest))"
  , "        .catch((e) => stuck(e.message));"
  , "    }"
  , "    // The way out — ESC, the backdrop, q.  Pristine costs no request and"
  , "    // touches no file; dirty flushes and closes on the 200; a sheet with"
  , "    // trouble in it discards, which is what a second ESC is."
  , "    function leave() {"
  , "      if (!editing) return;"
  , "      if (trouble) { shut(); log(\"closed without writing — the file is as it was\"); return; }"
  , "      if (!dirty()) { shut(); return; }"
  , "      if (!flushing) flush(editing.digest).then((ok) => ok && shut());"
  , "    }"
  , "    el(\"modal\").addEventListener(\"click\", (e) => { if (e.target === el(\"modal\")) leave(); });"
  , "    // A tab closing on an edited sheet still owes the file the text:"
  , "    // `keepalive' outlives the document, and a pristine sheet sends nothing."
  , "    addEventListener(\"beforeunload\", () => {"
  , "      if (!dirty()) return;"
  , "      fetch(`/headline?id=${encodeURIComponent(editing.id)}`, {"
  , "        method: \"POST\", keepalive: true,"
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify({ org: el(\"mtext\").value, digest: editing.digest }),"
  , "      }).catch(() => {});"
  , "    });"
  , ""
  , "    // Rows.  The renderer virtualizes, so a row outside the window has no"
  , "    // element: movement is ids out of `getVisible()' handed to `select(id)'."
  , "    // The class is still read — a click moves the selection without telling"
  , "    // us — and `cursor' carries it while the row is scrolled out of sight."
  , "    const visible = () => (table ? table.getVisible() : []);"
  , "    const focusedId = () => {"
  , "      const tr = document.querySelector(\"#app .tv-table tbody tr.tv-sel\");"
  , "      return (cursor = tr ? tr.dataset.id : cursor);"
  , "    };"
  , "    function pick(list, i) {"
  , "      if (!list.length) { log(\"no rows to move through\"); return; }"
  , "      const id = list[Math.max(0, Math.min(list.length - 1, i))].id;"
  , "      // Row movement carries the column along: null until a horizontal key"
  , "      // picks one, so a page nobody has moved sideways in keeps whole rows."
  , "      if (table.select(id, column())) cursor = id;"
  , "    }"
  , "    function move(step) {"
  , "      const list = visible(), at = list.findIndex((r) => r.id === focusedId());"
  , "      pick(list, at === -1 ? (step > 0 ? 0 : list.length - 1) : at + step);"
  , "    }"
  , "    // Cells.  The column is part of the renderer's selection, so it needs no"
  , "    // state here: it survives a profile switch, rides along with row"
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
  , "      cursor = id;"
  , "      say(cols[want].header || cols[want].key);"
  , "    }"
  , "    const focusFilter = () => {"
  , "      const box = document.querySelector(\"#app .tv-filter\");"
  , "      if (box) { box.focus(); box.select(); }"
  , "    };"
  , "    // `start' is the fetch, the mount and the socket; reuse it whole."
  , "    // Dropping onclose first stops the reconnect timer opening a second one."
  , "    function refresh() {"
  , "      if (socket) { socket.onclose = null; socket.close(); socket = null; }"
  , "      backoff = 1000;"
  , "      log(\"refreshing …\");"
  , "      start();"
  , "    }"
  , ""
  , "    // Keys.  The map is the JSON above — dispatch and echo read the one blob."
  , "    // Movement comes from the active profile; the rest is shared by all."
  , "    const MAPS = JSON.parse(el(\"keys\").textContent);"
  , "    const kept = {"
  , "      get() { try { return localStorage.getItem(\"glance-keys\"); } catch (e) { return null; } },"
  , "      set(v) { try { localStorage.setItem(\"glance-keys\", v); } catch (e) { /* denied */ } },"
  , "    };"
  , "    // `?keys=NAME' picks a profile and is remembered; otherwise what was"
  , "    // remembered; otherwise the name the blob calls its default."
  , "    function wanted() {"
  , "      const asked = new URLSearchParams(location.search).get(\"keys\");"
  , "      if (asked && MAPS.profiles[asked]) { kept.set(asked); return asked; }"
  , "      const saved = kept.get();"
  , "      return saved && MAPS.profiles[saved] ? saved : MAPS.default;"
  , "    }"
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
  , "    let profile = wanted(), KEYS = [];"
  , "    function setProfile(name) {"
  , "      profile = name;"
  , "      KEYS = MAPS.shared.concat(MAPS.profiles[name]);"
  , "      kept.set(name);"
  , "      el(\"keysel\").value = name;"
  , "    }"
  , "    // The options are the blob's own profiles — a profile cannot be offered"
  , "    // and unbound — and a native select is keyboard-reachable as it stands:"
  , "    // Tab to it, arrows through it, Enter or ESC out, no chord of its own."
  , "    for (const name of Object.keys(MAPS.profiles)) {"
  , "      const o = document.createElement(\"option\");"
  , "      o.value = o.textContent = name;"
  , "      el(\"keysel\").appendChild(o);"
  , "    }"
  , "    setProfile(profile);"
  , "    el(\"keysel\").addEventListener(\"change\", (e) => {"
  , "      setProfile(e.target.value);"
  , "      prefix([]);"
  , "      echo(`movement: ${profile}`);"
  , "    });"
  , "    const NAMED = { Enter: \"RET\", Tab: \"TAB\", \" \": \"SPC\", Escape: \"ESC\","
  , "      Backspace: \"DEL\", Delete: \"<delete>\", ArrowUp: \"<up>\", ArrowDown: \"<down>\","
  , "      ArrowLeft: \"<left>\", ArrowRight: \"<right>\", Home: \"<home>\", End: \"<end>\","
  , "      PageUp: \"<prior>\", PageDown: \"<next>\" };"
  , "    // Chords the browser needs more than we do: never claimed, not even as"
  , "    // the continuation of a prefix this map has already entered."
  , "    const RESERVED = [\"C-l\", \"C-r\", \"C-t\", \"C-w\", \"C-n\", \"C-p\", \"<f5>\"];"
  , "    // Commands that take auto-repeat off: one press, one token.  Movement"
  , "    // wants the repeat — a held n is how you cross a table, and the renderer"
  , "    // coalesces those to a frame — but a held DEL would walk the whole query"
  , "    // away between one glance at the chips and the next.  By command name,"
  , "    // so it holds under every profile that binds it."
  , "    const ONCE = [\"filter-drop-token\"];"
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
  , "    // take for row movement."
  , "    const typing = () => {"
  , "      const a = document.activeElement;"
  , "      return !!a && (a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
  , "                     || a.tagName === \"SELECT\" || a.isContentEditable);"
  , "    };"
  , "    const live = (b) => b.scope === \"any\""
  , "      || (b.scope === \"modal\" && editing !== null)"
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
  , "      firstRow: () => pick(visible(), 0),"
  , "      lastRow: () => pick(visible(), visible().length - 1),"
  , "      materializeRow: () => {"
  , "        const id = focusedId();"
  , "        if (id) materialize(id); else log(\"no row focused — n or p picks one\");"
  , "      },"
  , "      refresh, focusFilter, save,"
  , "      quitWindow: () =>"
  , "        (editing ? leave() : log(\"q closes the sheet; there is no window to quit\")),"
  , "      cancel: () => {"
  , "        if (editing) leave();"
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
  , "      const here = KEYS.filter(live);"
  , "      // A row is in play while its keys open with the ones typed so far."
  , "      const opens = (b) => keys.every((key, i) => b.keys[i] === key);"
  , "      const hit = here.find((b) => b.keys.length === keys.length && opens(b));"
  , "      // A held key still belongs to this map — it is claimed either way —"
  , "      // but a destructive one runs once per press."
  , "      if (hit) {"
  , "        prefix([]);"
  , "        e.preventDefault();"
  , "        if (!(e.repeat && ONCE.indexOf(hit.command) !== -1)) run(hit);"
  , "        return;"
  , "      }"
  , "      if (here.some((b) => b.keys.length > keys.length && opens(b))) {"
  , "        if (!selecting()) { e.preventDefault(); prefix(keys); }"
  , "        return;"
  , "      }"
  , "      if (!pending.length) return;   // not ours; the browser keeps it"
  , "      prefix([]);"
  , "      if (RESERVED.indexOf(k) === -1) e.preventDefault();"
  , "      echo(`${keys.join(\" \")} is undefined`);"
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
  , "      say();"
  , "    }"
  , "    function listen() {"
  , "      const scheme = location.protocol === \"https:\" ? \"wss\" : \"ws\";"
  , "      // The rows came over HTTP; the socket's own set-rows would resend them."
  , "      socket = new WebSocket(`${scheme}://${location.host}/ws?bootstrap=off`);"
  , "      socket.onopen = () => { backoff = 1000; dot(\"live\"); };"
  , "      socket.onmessage = (e) => apply(JSON.parse(e.data));"
  , "      socket.onclose = () => { socket = null; dot(\"down\"); again(); };"
  , "    }"
  , "    function again() {"
  , "      log(`disconnected · retrying in ${Math.round(backoff / 1000)}s`);"
  , "      setTimeout(start, backoff);"
  , "      backoff = Math.min(backoff * 2, 30000);"
  , "    }"
  , "    // The server binds before it walks the tree, so the first fetch of a"
  , "    // cold daemon is a 503: show what it is doing and ask again in a second."
  , "    function indexing(b) {"
  , "      dot(\"wait\");"
  , "      log(`indexing … ${b.elapsed}s · the table opens when the walk lands`);"
  , "      setTimeout(start, 1000);"
  , "    }"
  , "    function start() {"
  , "      // A `?q=' in the address bar is a filtered view: the boot asks for it"
  , "      // and `mount' opens the filter showing it.  Every return through this"
  , "      // door — a reload, a reconnect, `view-changed', `g' — restores it the"
  , "      // same way, since they all re-fetch and re-mount."
  , "      const asked = (query = urlQuery());"
  , "      const narrow = asked ? `?q=${encodeURIComponent(asked)}&` : \"?\";"
  , "      load(`${narrow}limit=${PAGE}`).then((a) => {"
  , "        mount(a.view);"
  , "        listen();"
  , "        // The rest behind the painted table: n/p, sort and materialize all"
  , "        // want the whole set, and the renderer holds it without the DOM."
  , "        if (a.total > (a.view.rows || []).length)"
  , "          load(asked ? `?q=${encodeURIComponent(asked)}` : \"\")"
  , "            .then((b) => table && query === asked && paint(b)).catch(quiet);"
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
  -- The extra top padding is the fixed status corner's room: with no heading
  -- above it, the omnibox would otherwise open underneath the corner.
  , "    padding:34px 24px 24px;display:flex;flex-direction:column;gap:14px}"
  , "  h1{font-size:16px;margin:0}"
  , "  p{margin:0;max-width:70ch}"
  , "  code{font-size:12px;color:var(--g-mute)}"
  , "  #app{height:80vh}"
  -- The renderer injects its own `.tv-root' font, and injects it from a script,
  -- so its rule lands after this element and ties on specificity.  One more
  -- selector step settles it, and leaves the size and the leading it set.
  , "  #app .tv-root{font-family:var(--glance-mono)}"
  , "  #app .tv-table tbody tr.tv-sel{box-shadow:inset 2px 0 0 var(--tv-accent)}"
  -- The log is the table's own container repeated under it: same width,
  -- because this rule is the one place either width is set and they are the
  -- body column's two items; same hairline, radius and surface tint as
  -- @.tv-root@.  It is capped at ten lines and scrolls inside that, so a long
  -- message cannot push the table up the page, and it collapses outright when
  -- there is nothing to say rather than leaving an empty frame behind.
  , "  #app,#log{width:100%;box-sizing:border-box}"
  , "  #log{font-size:12px;color:var(--g-mute);padding:6px 10px;"
  , "    border:1px solid var(--g-border);border-radius:8px;"
  , "    background:var(--g-surface);max-height:10em;overflow-y:auto}"
  , "  #log:empty{display:none}"
  -- The status corner: the connection dot, the theme and the movement profile,
  -- together, clear of the table and out of the heading.
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
  , "  #modal{--dk-mono:\"Hack\", var(--glance-mono);"
  , "    display:none;position:fixed;inset:0;z-index:100;padding:24px;background:#0009;"
  , "    align-items:center;justify-content:center}"
  , "  #modal.on{display:flex}"
  , "  #sheet{display:flex;flex-direction:column;gap:8px;padding:14px;border-radius:6px;"
  , "    position:relative;z-index:101;"
  , "    width:min(900px,100%);height:min(80vh,100%);font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #mhead{display:flex;justify-content:space-between;gap:12px;font-size:12px}"
  , "  #mfile{color:var(--g-mute)}"
  , "  #mnote{text-align:right;color:var(--g-ok)}"
  , "  #mnote.syncing{color:var(--g-mute)}"
  , "  #mnote.conflict,#mnote.error{color:var(--g-bad)}"
  , "  #mtext{flex:1;font:12px/1.5 var(--dk-mono);padding:8px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit;resize:none}"
  , "  #mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- The echo area and the status corner are the page's, and the backdrop dims
  -- the page: both sit under it (2 and 3 against the modal's 100) and grey out
  -- with everything else while the sheet is open.  They stay above the table.
  , "  #echo{position:fixed;right:14px;bottom:12px;z-index:2;padding:4px 10px;"
  , "    border-radius:999px;border:1px solid var(--g-border);font-size:12px;"
  , "    white-space:pre;background:var(--g-surface);color:var(--g-fg);opacity:0;"
  , "    transition:opacity .35s;pointer-events:none}"
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
