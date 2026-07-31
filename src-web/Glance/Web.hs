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
                          , status500 )
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
                    , QueryResult (..), Span (spanEnd, spanStart)
                    , WriteFailure (..), matchesSearch, replaceSpan, sortedForView
                    , subtreeText, viewJSONTextWith )
import Glance.Web.Store ( Client, Frame (ViewChanged), Hub, Store (stGen), frameText
                        , hubStore, loadStore, newHub, nextFrame, storeHeadline
                        , storeKeywords, storeResult, subscribe, unsubscribe )
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
httpApp :: ServeOptions -> Hub -> Application
httpApp opts hub request respond = route (pathInfo request) >>= respond
  where
    method  = requestMethod request
    reading = method `elem` [methodGet, methodHead]
    route ["headline"]
      | reading              = materialize hub (queryId request)
      | method == methodPost = commit hub (queryId request) request
      | otherwise            = pure (jsonError status405 "/headline takes GET and POST")
    route _
      | not reading          = pure (plain status405 writeHint)
    route []                 = shellPage opts
    route ["headlines"]      = headlines opts hub request
    route ["ws"]             = pure (plain status400 wsHint)
    route [name]
      | safeName name        = asset opts (T.unpack name)
    route _                  = pure (plain status404 notFound)
    wsHint    = "/ws is a websocket endpoint; connect with Upgrade: websocket"
    writeHint = "method not allowed; POST /headline?id=… is the one route that writes"
    notFound  = "not found: /, /headlines, /headline, /ws, or an asset name"

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
-- @q@ is a case-insensitive substring of the row as it displays
-- ('Glance.Query.matchesSearch'), @limit@ a page size — absent means the whole
-- set, which is what every client before this asked for — and @offset@ where
-- the page starts.  Filtering happens before paging, so @X-Glance-Total@ is
-- the match count and @X-Glance-Has-Next@ says whether a further page exists.
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
            matched = filter (matchesSearch q) (qrRecords qr)
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
liveSocket :: Hub -> WS.ServerApp
liveSocket hub pending
  | wsPath == "/ws" = do
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $ do
        (cid, client, boot) <- atomically (subscribe hub)
        when (bootstrapWanted requested) (send conn boot)
        pump conn client `finally` unsubscribe hub cid
  | otherwise = WS.rejectRequest pending "glance streams rows at /ws"
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
  }

-- | KEYS bound to a command, spelled the way Emacs spells a sequence: one
-- space between the keys.
bind :: [Text] -> Text -> Maybe Text -> Text -> KeyBinding
bind keys = KeyBinding keys (T.unwords keys)

-- | 'bind', spelled SHOWN.  vi runs @gg@ together where Emacs would write
-- @g g@, and the echo widget owes the reader the notation they typed in.
bindAs :: Text -> [Text] -> Text -> Maybe Text -> Text -> KeyBinding
bindAs shown keys = KeyBinding keys shown

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
-- C-s@ means anything.  @RET@, @TAB@ and @\/@ are taken while the table has
-- focus.  Everything else reaches the browser — @C-l@, @C-r@, @C-t@, @C-w@,
-- @C-n@, @C-p@ and @\<f5\>@ even as the continuation of a prefix this map
-- entered, which is why neither profile moves on @C-n@ or @C-p@.
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
  , bind ["ESC"]        "keyboard-quit"                   (Just "cancel")         "any"
  ]

-- | Movement as org-glance's overview binds it, and what a page starts on.
-- @j@ is the overview's open-stub here, where vi needs it for down.
emacsKeys :: [KeyBinding]
emacsKeys =
  [ bind ["n"] "next-row"                    (Just "nextRow")     "table"
  , bind ["p"] "previous-row"                (Just "previousRow") "table"
  , bind ["g"] "org-glance-overview:refresh" (Just "refresh")     "table"
  , bind ["j"] "org-glance-overview:open"    Nothing              "table"
  ]

-- | Movement as vi binds it.  Two rows move to make room, and both moves are
-- the reason this profile is data rather than a second dispatch: @j@ is down
-- here, so the open-stub is left to @\!@ — which the overview map already
-- carries, as its dired-execute rhyme — and @g@ is a prefix rather than a
-- command, so refresh goes to @R@ and reads as vi's reload.  Nothing in this
-- profile binds a bare @g@: a complete sequence that also opens a longer one
-- would make the longer one unreachable.
vimKeys :: [KeyBinding]
vimKeys =
  [ bind   ["j"]           "next-row"                    (Just "nextRow")     "table"
  , bind   ["k"]           "previous-row"                (Just "previousRow") "table"
  , bindAs "gg" ["g", "g"] "first-row"                   (Just "firstRow")    "table"
  , bind   ["G"]           "last-row"                    (Just "lastRow")     "table"
  , bind   ["R"]           "org-glance-overview:refresh" (Just "refresh")     "table"
  ]

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
                       , "scope"   .= kbScope b ]

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
-- Filtering is the server's.  The renderer's @onFilter@ hands over the
-- debounced query instead of narrowing its own list, the shell asks
-- @\/headlines?q=@ for it, and the answer replaces the rows — the store holds
-- the search text, so a query costs a substring scan there rather than 3 MB of
-- JSON here.  One fetch is in flight at a time and a new one aborts the last,
-- so a fast typist's earlier answers cannot land after a later one.  While a
-- filter is on, a row frame off the socket is answered by re-asking rather
-- than by splicing: the loaded rows are the server's answer to a query and
-- only it knows whether the changed row still matches.
--
-- Every close leads back through the same door: re-fetch, re-mount, reconnect.
-- That covers a daemon restart, a dropped slow client, and @view-changed@ —
-- the columns moving, which SCHEMA.md's row ops cannot express.
--
-- The @materialize@ action opens the subtree over the table in a plain
-- @textarea@: @GET \/headline@ fills it, Save posts it back with the digest it
-- came with, and a 409 says so and offers to fetch the subtree again.  A save
-- closes the sheet without touching the table — the row arrives over the socket
-- when the watch has re-read the file, which is the same way it would arrive
-- had the edit come from an editor.  A real editor component is M3.5; a
-- textarea is what proves the round-trip.
--
-- The keys are 'keyBindingsJSON', which the glue parses: row movement runs
-- over the renderer's @getVisible@ and @select@, since a virtualized row
-- outside the window has no element to click, and a sequence with no handler
-- echoes its org-glance command name
-- and what it is waiting for.  The pill in the corner is the echo area — the
-- pending prefix while one is open, the command on completion, @is undefined@
-- otherwise.  A second pill by the heading names the movement profile and
-- switches it in place, remembering the choice where the page can find it
-- again.
demoShell :: ServeOptions -> Maybe FilePath -> Text
demoShell opts font = page (fontFace font) (viewTitleFor (soDir opts)) $ T.unlines
  [ "  <h1>" <> escape (viewTitleFor (soDir opts)) <> "<span id=\"dot\"></span>"
      <> "<button id=\"keyset\" type=\"button\""
      <> " title=\"movement profile — click to switch\"></button></h1>"
  , "  <div id=\"app\"></div>"
  , "  <div id=\"log\">loading …</div>"
  , "  <div id=\"modal\">"
  , "    <div id=\"sheet\">"
  , "      <div id=\"mhead\"><span id=\"mfile\"></span><span id=\"mnote\"></span></div>"
  , "      <textarea id=\"mtext\" spellcheck=\"false\"></textarea>"
  , "      <div id=\"mfoot\">"
  , "        <button id=\"msave\">Save</button>"
  , "        <button id=\"mredo\" hidden>Re-materialize</button>"
  , "        <button id=\"mcancel\">Cancel</button>"
  , "      </div>"
  , "    </div>"
  , "  </div>"
  , "  <div id=\"echo\" role=\"status\" aria-live=\"polite\"></div>"
  , "  <script id=\"keys\" type=\"application/json\">" <> keyBindingsJSON <> "</script>"
  , "  <script src=\"" <> T.pack rendererAsset <> "\"></script>"
  , "  <script>"
  , "    const log = (m) => (document.getElementById(\"log\").textContent = m);"
  , "    const dot = (state) => (document.getElementById(\"dot\").className = state);"
  , "    const el = (id) => document.getElementById(id);"
  , "    let table = null, socket = null, backoff = 1000, editing = null;"
  , "    // The server filters and pages; these hold the query it was last asked"
  , "    // with, the fetch still in flight for it, and the selected row's id."
  , "    let query = \"\", inflight = null, cursor = null, requeryAt = 0;"
  , "    const PAGE = 1000;   // rows in the first paint; the rest follows it"
  , "    function mount(view) {"
  , "      table = TableView.mount(document.getElementById(\"app\"), view, {"
  , "        onAction: (command, id) =>"
  , "          command === \"materialize\" ? materialize(id)"
  , "                                     : log(`action: ${command}  id=${id}`),"
  , "        onLink: (target) => log(`link: ${target}`),"
  , "        onFilter: filter,   // the server narrows; the renderer shows what it is given"
  , "      });"
  , "      cursor = null;"
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
  , "             : r.text().then((t) => { throw new Error(t); }));"
  , "    }"
  , "    const quiet = (e) => { if (e.name !== \"AbortError\") log(`load failed: ${e.message}`); };"
  , "    const paint = (a) => { table.setRows(a.view.rows || []); say(); };"
  , "    const filter = (q) =>"
  , "      load((query = q.trim()) ? `?q=${encodeURIComponent(query)}` : \"\")"
  , "        .then((a) => table && paint(a)).catch(quiet);"
  , "    function materialize(id) {"
  , "      fetch(`/headline?id=${encodeURIComponent(id)}`)"
  , "        .then((r) => r.json().then((b) => {"
  , "          if (!r.ok) throw new Error(b.error || r.status);"
  , "          return b;"
  , "        }))"
  , "        .then(show)"
  , "        .catch((e) => log(`materialize failed: ${e.message}`));"
  , "    }"
  , "    function show(h) {"
  , "      editing = h;"
  , "      el(\"mfile\").textContent = `${h.file}  ·  ${h.id}`;"
  , "      el(\"mtext\").value = h.org;"
  , "      note(\"\", false);"
  , "      el(\"modal\").className = \"on\";"
  , "      el(\"mtext\").focus();"
  , "    }"
  , "    function note(message, again) {"
  , "      el(\"mnote\").textContent = message;"
  , "      el(\"mredo\").hidden = !again;"
  , "    }"
  , "    function shut() { el(\"modal\").className = \"\"; editing = null; }"
  , "    function save() {"
  , "      if (!editing) return;"
  , "      fetch(`/headline?id=${encodeURIComponent(editing.id)}`, {"
  , "        method: \"POST\","
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify({ org: el(\"mtext\").value, digest: editing.digest }),"
  , "      })"
  , "        .then((r) => r.json().then((b) => ({ status: r.status, body: b })))"
  , "        .then((a) => {"
  , "          if (a.status === 200) { shut(); log(\"saved · the watch streams the row\"); }"
  , "          else if (a.status === 409) note(\"File changed since materialize — re-open\", true);"
  , "          else note(a.body.error || `save failed (${a.status})`, false);"
  , "        })"
  , "        .catch((e) => note(`save failed: ${e.message}`, false));"
  , "    }"
  , "    el(\"msave\").addEventListener(\"click\", save);"
  , "    el(\"mcancel\").addEventListener(\"click\", shut);"
  , "    el(\"mredo\").addEventListener(\"click\", () => editing && materialize(editing.id));"
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
  , "      if (table.select(id)) cursor = id;"
  , "    }"
  , "    function move(step) {"
  , "      const list = visible(), at = list.findIndex((r) => r.id === focusedId());"
  , "      pick(list, at === -1 ? (step > 0 ? 0 : list.length - 1) : at + step);"
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
  , "    let profile = wanted(), KEYS = [];"
  , "    function setProfile(name) {"
  , "      profile = name;"
  , "      KEYS = MAPS.shared.concat(MAPS.profiles[name]);"
  , "      kept.set(name);"
  , "      el(\"keyset\").textContent = `keys: ${name}`;"
  , "    }"
  , "    setProfile(profile);"
  , "    el(\"keyset\").addEventListener(\"click\", (e) => {"
  , "      const names = Object.keys(MAPS.profiles);"
  , "      setProfile(names[(names.indexOf(profile) + 1) % names.length]);"
  , "      prefix([]);"
  , "      echo(`movement: ${profile}`);"
  , "      e.currentTarget.blur();"
  , "    });"
  , "    const NAMED = { Enter: \"RET\", Tab: \"TAB\", \" \": \"SPC\", Escape: \"ESC\","
  , "      Backspace: \"DEL\", Delete: \"<delete>\", ArrowUp: \"<up>\", ArrowDown: \"<down>\","
  , "      ArrowLeft: \"<left>\", ArrowRight: \"<right>\", Home: \"<home>\", End: \"<end>\","
  , "      PageUp: \"<prior>\", PageDown: \"<next>\" };"
  , "    // Chords the browser needs more than we do: never claimed, not even as"
  , "    // the continuation of a prefix this map has already entered."
  , "    const RESERVED = [\"C-l\", \"C-r\", \"C-t\", \"C-w\", \"C-n\", \"C-p\", \"<f5>\"];"
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
  , "    const typing = () => {"
  , "      const a = document.activeElement;"
  , "      return !!a && (a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
  , "                     || a.isContentEditable);"
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
  , "      firstRow: () => pick(visible(), 0),"
  , "      lastRow: () => pick(visible(), visible().length - 1),"
  , "      materializeRow: () => {"
  , "        const id = focusedId();"
  , "        if (id) materialize(id); else log(\"no row focused — n or p picks one\");"
  , "      },"
  , "      refresh, focusFilter, save,"
  , "      quitWindow: () =>"
  , "        (editing ? shut() : log(\"q closes the sheet; there is no window to quit\")),"
  , "      cancel: () => {"
  , "        if (editing) shut();"
  , "        else if (typing()) document.activeElement.blur();"
  , "      },"
  , "    };"
  , "    function run(b) {"
  , "      echo(`${b.seq} → ${b.command}`);"
  , "      const handler = b.handler && HANDLERS[b.handler];"
  , "      if (handler) handler();"
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
  , "      if (hit) { prefix([]); e.preventDefault(); run(hit); return; }"
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
  , "        requeryAt = setTimeout(() => filter(query), 250));"
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
  , "    function start() {"
  , "      query = \"\";"
  , "      load(`?limit=${PAGE}`).then((a) => {"
  , "        mount(a.view);"
  , "        listen();"
  , "        // The rest behind the painted table: n/p, sort and materialize all"
  , "        // want the whole set, and the renderer holds it without the DOM."
  , "        if (a.total > (a.view.rows || []).length)"
  , "          load(\"\").then((b) => table && !query && paint(b)).catch(quiet);"
  , "      }).catch((e) => { dot(\"down\"); quiet(e); if (e.name !== \"AbortError\") again(); });"
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
  , "  :root{--glance-mono:" <> monoStack <> "}"
  , "  body{margin:0;font:14px/1.5 var(--glance-mono);background:#eceef2;color:#1c1e26;"
  , "    padding:24px;display:flex;flex-direction:column;gap:14px}"
  , "  @media (prefers-color-scheme:dark){body{background:#13141c;color:#c8ccd4}}"
  , "  h1{font-size:16px;margin:0}"
  , "  p{margin:0;max-width:70ch}"
  , "  code{font-size:12px;opacity:.9}"
  , "  #app{height:80vh}"
  -- The renderer injects its own `.tv-root' font, and injects it from a script,
  -- so its rule lands after this element and ties on specificity.  One more
  -- selector step settles it, and leaves the size and the leading it set.
  , "  #app .tv-root{font-family:var(--glance-mono)}"
  , "  #app .tv-table tbody tr.tv-sel{box-shadow:inset 2px 0 0 var(--tv-accent)}"
  , "  #log{font-size:12px;opacity:.75;min-height:1.4em}"
  , "  #dot{display:inline-block;width:7px;height:7px;border-radius:50%;"
  , "    margin-left:8px;vertical-align:middle;background:#9aa0ad;transition:background .3s}"
  , "  #dot.live{background:#9ece6a}"
  , "  #dot.down{background:#9aa0ad}"
  , "  #keyset{font:inherit;font-size:11px;margin-left:10px;padding:1px 8px;"
  , "    border-radius:999px;border:1px solid #8884;background:transparent;color:inherit;"
  , "    opacity:.65;cursor:pointer;vertical-align:middle}"
  , "  #keyset:hover{opacity:1}"
  , "  #modal{display:none;position:fixed;inset:0;padding:24px;background:#0009;"
  , "    align-items:center;justify-content:center}"
  , "  #modal.on{display:flex}"
  , "  #sheet{display:flex;flex-direction:column;gap:8px;padding:14px;border-radius:6px;"
  , "    width:min(900px,100%);height:min(80vh,100%);background:#eceef2;color:#1c1e26}"
  , "  @media (prefers-color-scheme:dark){#sheet{background:#1b1d26;color:#c8ccd4}}"
  , "  #mhead{display:flex;justify-content:space-between;gap:12px;font-size:12px;opacity:.85}"
  , "  #mnote{color:#f7768e;text-align:right}"
  , "  #mtext{flex:1;font:12px/1.5 var(--glance-mono);padding:8px;border-radius:4px;"
  , "    border:1px solid #8884;background:transparent;color:inherit;resize:none}"
  , "  #mfoot{display:flex;gap:8px}"
  , "  #sheet button{font:12px var(--glance-mono);padding:5px 12px;border-radius:4px;"
  , "    border:1px solid #8884;background:transparent;color:inherit;cursor:pointer}"
  -- The echo area, over the sheet's backdrop: the sheet takes no z-index, so
  -- one is enough to keep the pending prefix readable while the sheet is open.
  , "  #echo{position:fixed;right:14px;bottom:12px;z-index:2;padding:4px 10px;"
  , "    border-radius:999px;border:1px solid #8884;font-size:12px;white-space:pre;"
  , "    background:#eceef2;color:#1c1e26;opacity:0;transition:opacity .35s;"
  , "    pointer-events:none}"
  , "  @media (prefers-color-scheme:dark){#echo{background:#1b1d26;color:#c8ccd4}}"
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
