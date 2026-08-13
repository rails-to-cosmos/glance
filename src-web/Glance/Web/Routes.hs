{-# LANGUAGE TemplateHaskell #-}

-- | The HTTP surface: a fixed route table, its handlers, and the live socket.
--
-- @GET \/headlines@ the view JSON, @\/@ the shell, @\/ws@ the row stream,
-- @\/NAME@ an asset, @\/headline@ the materialize round-trip, @POST
-- \/command@ the structured writes, @\/keywords@ @\/links@ @\/tags@
-- @\/capture@ the per-row questions, @\/config@ the keyword layers.  Each
-- entry declares whether it needs a loaded store and which methods it takes;
-- anything else is a 405.
--
-- The view's field set is the contract (@table-view\/SCHEMA.md@), so the load
-- counts ride as @X-Glance-*@ headers and leave the body alone.  Everything is
-- rendered from the 'Glance.Web.Store.Store', a projection that dies with the
-- process.
--
-- THE WRITE ROUTES NEVER TOUCH THE STORE: a commit goes to the file, the watch
-- re-parses and streams the rows, so a browser save and an editor save reach
-- open tabs by one channel.
--
-- While the startup walk runs, the store-reading routes answer 503 with
-- @Retry-After: 1@ while @\/@ and the assets serve.
module Glance.Web.Routes (application, bootstrapWanted, hasRenderer) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (filterM, forever, void, when)
import Data.Aeson (FromJSON (..), Value, encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser)
import Data.Bifunctor (first)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Language.Haskell.TH (listE)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( Header, hCacheControl, hContentType, methodGet, methodHead
                          , methodPost, parseQuery, status200, status304, status400
                          , status404, status405, status409, status500, status503 )
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai ( Application, Request (pathInfo, queryString, requestHeaders, requestMethod)
                   , Response, responseFile, responseLBS )
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Gzip ( GzipFiles (GzipCompress), defaultGzipSettings
                                   , gzip, gzipFiles )
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified Network.WebSockets as WS

import Glance.Query ( ConfigLayerFile (..), ConfigParts (..)
                    , HeadlineParts (..)
                    , HeadlineRecord ( hrDigest, hrFile, hrId, hrPriority, hrState
                                     , hrSubtree, hrTags, hrTitle )
                    , OrgLink (olSpan, olTarget)
                    , QueryResult (..), SortChain
                    , Span (spanEnd, spanStart)
                    , SubtreeEntry (..)
                    , TodoKeywords (..)
                    , SavedView (..), archived, captureCodes, configDirsIn
                    , captureTemplateIn, captureTemplateOf
                    , ConfigLayers (clTree), TreeSettings (..), treeSettings
                    , configEdits, viewQuery, viewQueryIn
                    , headlineParts, keywordSources, linkShown, linkType
                    , planningKeywords, readConfigLayers, readsAsTimestamp
                    , untrailed
                    , recomposedSubtree
                    , ownBodyLines, sortedForViewWith
                    , subtreeEntries, subtreeEntryAt, subtreeLinks
                    , subtreeText, tagsOfCell
                    , templatePrompts, titleSpan, todoPragmas
                    , resolveColumns, savedViews, todoLines, viewColumns
                    , viewJSONTextFor )
import Glance.Web.Base ( ServeOptions (..), answerWrite, bodyObject, configMoved
                       , conflict, glueAsset, gluePartFiles, html, jsonError
                       , elmAsset
                       , jsonResponse, jsonType
                       , noSuchRow
                       , plain, rendererAsset, reparsed, rewritten, sized, tenths
                       , unreadable, viewTitleFor, walkFor, withBody )
import Glance.Web.Commands (runCommand)
import Glance.Web.Filter (archiveKey, matchesFilter, namesArchive, storeEnv)
import Glance.Web.Page (assetsMissing, demoShell)
import Glance.Web.Page.Style (fontAssets)
import Glance.Web.Columns (columnNamesIn)
import Glance.Web.Sort (sortChainIn)
import Glance.Web.Theme (themeIds)
import Glance.Web.Store ( Client, CloseReason (Resync), Frame (Close), Hub
                        , LoadState (..), closeReason
                        , Store (stConfig, stGen, stPrint), frameText, layersFor
                        , hubLoad, hubStore, nextFrame
                        , headlinesIn
                        , storeKeywords
                        , storeRecords, storeResult
                        , storeTags, subscribe, unsubscribe )
import Glance.Web.Watch (writeSpans)

-- | The renderer, read at COMPILE time out of the repo's own @assets\/@ and
-- carried in the binary, which is what makes a built @glance@ self-contained:
-- no directory it has to be started beside, no path off this repo in the
-- source, a clone plus a compiler being the whole of what serving the page
-- takes.  @assets\/table-view.js@ is vendored from the sibling table-view
-- checkout by @make sync-renderer@ and committed like any other file, so the
-- bytes a build embeds are the bytes in the tree.  @--assets@ overrides it
-- ('assetSource') — the renderer-hacking loop: point it at
-- @..\/table-view\/web@ and reload instead of rebuilding.
embeddedRenderer :: BS.ByteString
embeddedRenderer = $(makeRelativeToProject "assets/table-view.js" >>= embedFile)

-- | The shell's own script, embedded the same way and in PARTS: one file per
-- widget under @assets\/glue\/@
-- (docs\/proposals\/2026-08-08-widget-files.partial.md), concatenated in the
-- order 'gluePartFiles' declares.  The parts are FRAGMENTS of one script
-- scope rather than modules, so the join is plain concatenation and the split
-- is byte-provable against the single file it came from.
--
-- The page still names ONE script beside the renderer's, and @--assets@ still
-- overrides both together — the directory is the whole asset set, and a dev
-- serving from one puts a whole @glue.js@ there.
embeddedGlue :: BS.ByteString
embeddedGlue = BS.concat
  $(listE [ makeRelativeToProject ("assets/glue/" <> part) >>= embedFile
          | part <- gluePartFiles ])

-- | THE ELM PROGRAMS, compiled from @assets\/elm@ by @make elm@ and committed
-- like the renderer, so the bytes a build embeds are the bytes in the tree.
embeddedElm :: BS.ByteString
embeddedElm = $(makeRelativeToProject "assets/elm.js" >>= embedFile)

-- | Is there a renderer to serve?  The route's own question ('assetSource'),
-- asked of the renderer's name, so the banner and @\/@ cannot disagree with
-- what @\/table-view.js@ answers.  Without @--assets@ it is always yes; with
-- it, whether that directory holds the file — asked per request as well as at
-- startup, so a directory that fills up later needs no restart.
hasRenderer :: ServeOptions -> IO Bool
hasRenderer opts = isJust <$> assetSource opts rendererAsset

-- | OPTS over HUB as a WAI application.  Exported for the suite, which tests
-- the routes through it with no socket bound: a request that is not a websocket
-- upgrade never reaches 'liveSocket', so 'Network.Wai.Test' drives this
-- unchanged.
application :: ServeOptions -> Hub -> Application
application opts hub =
  websocketsOr WS.defaultConnectionOptions (liveSocket hub) (compressed (httpApp opts hub))

-- | The HTTP app under @gzip@, inside the websocket branch rather than around
-- it: an upgrade is not a response to rewrite.
--
-- Everything this server sends is text — JSON, HTML, one script — and
-- @defaultGzipSettings@ names those types already and skips a body under 860
-- bytes, below which the header costs more than the compression saves.  It
-- declines a @responseFile@ by default and the renderer is one, so
-- 'GzipCompress' puts the asset route in too.  The middleware adds @Vary:
-- Accept-Encoding@ to every response it passes, 304s included, which keeps a
-- shared cache from serving one encoding for the other.
compressed :: Application -> Application
compressed = gzip defaultGzipSettings { gzipFiles = GzipCompress }

-- | Everything that is not a websocket upgrade: the read routes, the one route
-- that writes, and the 405 for every other method.
--
-- While the startup walk runs, every route the table marks as needing the store
-- answers 'indexing' whatever the method — a commit against a store that is not
-- the directory yet would 404 on a headline the file does have.  @\/@ and the
-- assets serve the whole time, since the page they carry shows the indexing
-- state and polls for the end of it.
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
    -- The methods are the table's, so a route taking a write says so here rather
    -- than in a guard of its own, and the refusal sentence reads off the same
    -- list ('takes').  Everything else is an asset name or a miss.
    named =
      [ ([],            False, textRefusal, [(methodGet, shellPage opts hub)])
      , (["headlines"], True,  textRefusal, [(methodGet, headlines opts hub request)])
      -- @/headline@ is the one read that also writes: materialize a subtree,
      -- and commit it back.
      , (["headline"],  True,  jsonRefusal,
          [ (methodGet, materialize hub (queryId request) (queryChild request))
          , (methodPost, commit opts hub (queryId request) (queryChild request) request) ])
      -- @/command@ writes and only writes: there is nothing to read back, since
      -- the rows a command moved arrive over the socket like any other edit.
      , (["command"],   True,  jsonRefusal, [(methodPost, runCommand opts hub request)])
      -- @/config@ is the settings sheet's pair, and reads and writes like
      -- @/headline@: the layers with their digests, and one of them back.
      , (["config"],    True,  jsonRefusal,
          [ (methodGet, configView opts hub)
          , (methodPost, configWrite opts hub request) ])
      -- @/capture@ is what a client has to read before it can ASK anything: the
      -- tag's template, its prompts, and the codes the settings box completes
      -- over.  A read, and the write is @POST /command capture@.
      , (["capture"],   True,  textRefusal, [(methodGet, captureView opts hub request)])
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
-- 503 with @Retry-After@ rather than an empty 200, since an empty view is a
-- tree with no headlines in it and a client that mounts one has to be told to
-- throw it away later.  The delay is a second because that is the shell's poll;
-- the body carries elapsed seconds rather than a file count because the walk
-- hands its files over in one batch ('Glance.Web.Store.LoadState').
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

-- | The view JSON for the served directory, filtered and paged as REQUEST asks,
-- with the load counts and the page's metadata as headers.  Rendered from the
-- store rather than a fresh walk, so a response costs a filter and an encode.
--
-- @q@ is SCHEMA.md's filter query ('Glance.Web.Filter'), @limit@ a page size
-- (absent = the whole set), @offset@ where the page starts.  Filtering precedes
-- paging, so @X-Glance-Total@ is the match count and @X-Glance-Has-Next@ says
-- whether another page exists; SCHEMA.md fixes the View's fields, so paging
-- metadata rides the @X-Glance-*@ family.
--
-- ARCHIVED ROWS are left out unless the query says otherwise, since @D@
-- archives rather than deletes and a view showing them would grow without
-- bound.  One predicate, spelled exactly as @-tag:*archive*@ would be: any
-- query naming the archive META turns it off ('namesArchive'), and
-- @X-Glance-Archived@ reports what it took.  The STARRED spelling alone, so a
-- tree using the word for something of its own lifts nothing.
--
-- A page is cut out of the view's declared sort, never out of walk order — page
-- two has to be the rows the table would show after page one.  With no @limit@
-- walk order stands and the client sorts the whole set itself.
--
-- CACHING: the @ETag@ is the tree's fingerprint and the store's generation
-- ('etagOf'), under @Cache-Control: no-cache@.  One tag serves every query
-- variant — the parameters are in the URL and an HTTP cache is keyed by URL, so
-- the response is a function of (generation, URL) and no @Vary@ is owed beyond
-- the @Accept-Encoding@ the gzip middleware writes itself.
headlines :: ServeOptions -> Hub -> Request -> IO Response
headlines opts hub request = case pageParams request of
  Left why -> pure (jsonError status400 why)
  Right PageAsk {..} -> do
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
            asked   = filter (matchesFilter env paQuery) (qrRecords qr)
            matched = if hiding then filter (not . archived) asked else asked
            -- A tree with nothing archived in it pays no pass over the answer:
            -- the WHOLE store's tags say whether the tag is anywhere, filtered
            -- set or not.  The query's half is its own ('namesArchive'), and it
            -- is asked second because the vocabulary is the cheaper refusal.
            hiding  = archiveKey `elem` storeTags st && not (namesArchive paQuery)
            hidden  = length asked - length matched
            total   = length matched
            ordered = sortedForViewWith (storeKeywords st) paChain matched
            shown   = maybe matched (\n -> take n (drop paOffset ordered)) paLimit
            hasNext = maybe False (\n -> paOffset + n < total) paLimit
            -- The column SET is the query's too ('columnNamesIn'): absent,
            -- the default view; named, the picked columns in written order,
            -- customs reading the rows' own drawers ('resolveColumns').
            cols    = maybe viewColumns resolveColumns paPicked
            body    = TLE.encodeUtf8
                        (viewJSONTextFor cols (savedViewsIn st) paChain
                                         (viewTitleFor dir) (storeKeywords st) shown)
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
-- has moved since.  Opaque to a client, which only compares it to the one it
-- holds, and it has to mean the same thing across a restart — which the
-- generation alone cannot, starting at zero in every process: a client holding
-- @\"g0\"@ from a daemon since restarted over a rewritten tree would be told 304
-- and keep a table that is no longer anywhere.  The fingerprint
-- ('Glance.Web.Store.fingerprintOf') moves with the tree and the generation
-- with the edits inside one process, so the pair revalidates only what is still
-- true.  Sixteen hex digits of it — cache-key strength, no more.
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

-- | What one @\/headlines@ request asks for, each view token a FIELD: the
-- next view token is a field here rather than a wider tuple at every caller.
data PageAsk = PageAsk
  { paQuery  :: !Text            -- ^ @q@, the filter query, view tokens and all.
  , paLimit  :: !(Maybe Int)     -- ^ @limit@; absent serves the whole store.
  , paOffset :: !Int             -- ^ @offset@ into the effective order.
  , paChain  :: !SortChain       -- ^ the order @q@'s @sort:@ tokens state.
  , paPicked :: !(Maybe [Text])  -- ^ the set @q@'s @columns:@ tokens state.
  }

-- | @q@, @limit@ and @offset@ out of REQUEST's query string, with the CHAIN the
-- rows are served in read off @q@, or what is wrong with one of them.  An
-- absent parameter is its default — no filter, no limit, the top of the set —
-- and a present one that is not a number is a 400 rather than a silent fallback
-- to it, since a mistyped page size that quietly serves the whole store looks
-- like a working request.
--
-- The chain is read from ONE place, the query ('Glance.Web.Sort.sortChainIn'):
-- @?q=sort:COL@ states an order and @?q=sort:*none*@ document order, so a
-- reader states the whole of it in the grammar the renderer also writes, and a
-- sort token this producer cannot read is this request's 400 naming the token.
-- @order=@ was the older half and is GONE, refused rather than ignored: a
-- parameter this server no longer reads would serve the default order and look
-- exactly like a working request, the failure it was spelled out to avoid.  The
-- refusal names its replacement.
pageParams :: Request -> Either Text PageAsk
pageParams request = do
  q      <- maybe (Right "") text (raw "q")
  limit  <- traverse count (raw "limit")
  offset <- maybe (Right 0) count (raw "offset")
  _order <- maybe (Right ()) (const (Left retired)) (raw "order")
  chain  <- sortChainIn q
  picked <- columnNamesIn q
  case limit of
    Just n | n > limitCap -> Left ("limit is at most " <> T.pack (show limitCap)
                                     <> "; page with offset for more")
    _within                -> Right PageAsk { paQuery = q, paLimit = limit
                                           , paOffset = offset, paChain = chain
                                           , paPicked = picked }
  where
    retired = "order= is gone; the order is the query's: ?q=sort:COL, \
              \or ?q=sort:*none* for document order"
    -- A parameter with no @=@ reads as absent, so @?limit@ is not a zero page.
    raw name = case lookup (TE.encodeUtf8 name) (queryString request) of
      Just (Just bytes) -> Just (name, bytes)
      _absent           -> Nothing
    text (name, bytes) = first (const (name <> " is not UTF-8")) (TE.decodeUtf8' bytes)
    count (name, bytes) = wholeNumber name bytes

-- | NAME's query value RAW as a whole number, or what is wrong with it.  Read
-- as an 'Integer' first: a query string can spell a number no 'Int' holds, and
-- wrapping one would page from a negative offset — or, on @child@, splice over
-- the wrong subtree.  One rule, so the two parameters that take a number refuse
-- the same values.
wholeNumber :: Text -> BS.ByteString -> Either Text Int
wholeNumber name raw = do
  t <- first (const (name <> " is not UTF-8")) (TE.decodeUtf8' raw)
  case TR.decimal t :: Either String (Integer, Text) of
    Right (n, rest) | T.null rest, n >= 0, n <= toInteger (maxBound :: Int)
                        -> Right (fromInteger n)
    _notANumber         -> Left (name <> " must be a whole number, 0 or more")

-- Materialize



-- | @GET \/headline?id=…@: one headline's subtree as its file spells it, plus
-- the digest a commit must present and the extent the text was cut from.
--
-- The id travels in the QUERY STRING: a row id is @FILE#K@, and a hash spelled
-- into a path opens a FRAGMENT, so the id never reaches the server — which is
-- why the shell builds this with @encodeURIComponent@.
--
-- Every field comes out of the store, so the offsets and the digest describe
-- ONE document.  Re-reading the file here would digest bytes the extent was
-- never measured against, and the disagreement would surface only as a splice
-- landing in the wrong place.
--
-- The subtree arrives TWICE, whole and split: @org@, and @body@ +
-- @properties@ + @planning@ + @logbook@ ('headlineParts'), so a client edits
-- them apart without an org parser.  @logbook@ rides out and never back, and
-- 'hiddenProperties' are not even shown — the drawer a sheet edits is the
-- drawer minus the thing that names the row.
materialize :: Hub -> Maybe Text -> Either Text (Maybe Int) -> IO Response
materialize _hub Nothing _child = pure (jsonError status400 "GET /headline?id=<row id>")
materialize hub (Just rid) child = do
  st <- readTVarIO (hubStore hub)
  pure $ either id (jsonResponse status200 . subtreeJSON) (focusIn st rid child)

-- | Which headline a @\/headline@ request is looking at: the ROW it named, every
-- entry inside that row's subtree, and which of them the sheet is standing on.
--
-- The STORE addresses the row and the lens is over the entry, so keeping both
-- is what answers for either: the extent and the digest come off the entry, the
-- id, the file and the trail back out off the row.  A row with no @child@ is
-- its own entry, so nothing below this has two shapes to read.  Where the lens
-- stands is DERIVED ('focusEntry', 'focusHere') rather than stored beside the
-- index that decides it: 'focusIn' has already refused an index the entries do
-- not hold, so the lookup cannot fail and a second copy of its answer would be
-- one more thing to keep in step.
data Focus = Focus
  { fcRow     :: !HeadlineRecord   -- ^ the row the id named.
  , fcEntries :: ![SubtreeEntry]   -- ^ every headline inside it, in document order.
  , fcAt      :: !(Maybe Int)      -- ^ the @child@ index; 'Nothing' is the row itself.
  }

-- | ST's answer to @?id=RID&child=K@, or the response refusing it.  Both write
-- and read go through this one resolution, so a commit cannot address a
-- headline a materialize would not have served.
focusIn :: Store -> Text -> Either Text (Maybe Int) -> Either Response Focus
focusIn st rid child = do
  at <- first (jsonError status400) child
  r  <- maybe (Left (jsonError status404 (noSuchRow rid))) Right
              (rowIn (storeRecords st) rid)
  let entries = subtreeEntries (stConfig st) r
  case at of
    Nothing -> Right (Focus r entries Nothing)
    Just k | Nothing <- subtreeEntryAt entries k ->
      Left (jsonError status404
             (rid <> " has no child " <> T.pack (show k)
                <> "; it holds " <> T.pack (show (length entries))))
    _held -> Right (Focus r entries at)

-- | The entry @child@ names, 'Nothing' being the row itself.  Total by
-- construction: 'focusIn' refuses an index 'fcEntries' does not hold.
focusEntry :: Focus -> Maybe SubtreeEntry
focusEntry f = fcAt f >>= subtreeEntryAt (fcEntries f)

-- | The record the lens is over: the entry @child@ named, else the row.
focusHere :: Focus -> HeadlineRecord
focusHere f = maybe (fcRow f) seRecord (focusEntry f)

-- | Which entry E hangs under, 'Nothing' being the row.  The outline walk
-- spells @-1@ for the row, and this is the one place that reading is made.
parentOf :: SubtreeEntry -> Maybe Int
parentOf e = if seParent e < 0 then Nothing else Just (seParent e)

-- | What @GET \/headline@ answers with: the subtree twice over, the cells of the
-- headline itself, the entries hanging under it, and the way back out.  The
-- three navigation fields are the whole of the sub-addressing contract: @child@
-- is the index this answer is FOR, @parent@ the one @DEL@ climbs to (null being
-- the row), and @children@ the direct descendants with the index each answers
-- to, so a client walks the outline by handing back numbers this server worked
-- out rather than counting stars of its own.
subtreeJSON :: Focus -> [Pair]
subtreeJSON f =
  [ "id"         .= hrId (fcRow f)
  , "file"       .= hrFile (fcRow f)
  , "child"      .= fcAt f
  , "parent"     .= upFrom f
  , "path"       .= trailTo f
  , "level"      .= levelOf f
  , "cells"      .= object (cells here)
  , "children"   .= [ childJSON i e | (i, e) <- under f ]
  , "org"        .= subtreeText here
  , "body"       .= hpBody parts
  , "ownLines"   .= ownBodyLines here (hpBody parts) (seRecord . snd <$> listToMaybe (under f))
  , "properties" .= [ [key, value] | (key, value) <- hpProperties parts ]
  , "planning"   .= [ [key, value] | (key, value) <- hpPlanning parts ]
  , "logbook"    .= hpLogbook parts
  , "digest"     .= hrDigest here
  , "span"       .= extentJSON here
    -- THE ROW'S LINKS RIDE THE ANSWER, in the same objects @\/links@ serves
    -- and the same FILE coordinates as @span@ and @titleAt@: the sheet draws
    -- link descriptions in place, and a second request for them opened an
    -- async gap every fill had to bridge -- the frames between draw and
    -- answer showed the brackets raw.  One answer, one moment: the links
    -- describe exactly the text beside them, by construction.  The ROW's
    -- whole subtree's, whatever @child@ the lens stands in, since the spans
    -- are the file's and the client intersects per element either way.
  , "links"      .= map linkJSON (subtreeLinks (fcRow f))
    -- WHERE THE TITLE CELL STARTS, in the same FILE coordinates @\/links@
    -- answers in.  A title may hold org links and the client draws that cell
    -- itself, so this is what lets it tell which of the row's links are inside
    -- the cell without a bracket parser of its own.  @null@ where there is no
    -- title, which is the blank entry's case.
  , "titleAt"    .= (spanStart <$> titleSpan here)
  ]
  where here  = focusHere f
        parts = headlineParts here

-- | Org's outline level for the headline in focus: the ROW's is 1 and an entry
-- inside it carries its own.  A client draws the stars off this and off each
-- child's, which is the one thing it cannot work out from the cells.
levelOf :: Focus -> Int
levelOf = maybe 1 seLevel . focusEntry

-- | The four cells a headline shows, under the column keys the table spells
-- them with.  A client draws the headline LINE out of these rather than the
-- first line of @body@, which is org text this page holds no parser for.
cells :: HeadlineRecord -> [Pair]
cells r = [ "state"    .= hrState r
          , "priority" .= hrPriority r
          , "title"    .= hrTitle r
          , "tags"     .= hrTags r ]

-- | One entry hanging under the headline in focus: the number that addresses
-- it, how deep it sits, its own extent in the FILE, and the same four cells the
-- headline itself carries — flat rather than nested, since a child line draws
-- exactly as the headline line above it does.  The SPAN is the currency
-- @\/links@ already answers in, which lets a client ask "which of this row's
-- links are inside THIS entry" without a second route and without a text match
-- that a repeated URL would fool.
childJSON :: Int -> SubtreeEntry -> Value
childJSON i e = object ([ "index" .= i, "level" .= seLevel e
                        , "span" .= extentJSON (seRecord e) ]
                        <> cells (seRecord e))

-- | R's subtree extent on the wire, in the FILE coordinates @\/links@ answers
-- in.  One spelling, so the headline in focus and each child under it describe
-- their extents the same way.
extentJSON :: HeadlineRecord -> Value
extentJSON r = object [ "start" .= spanStart (hrSubtree r), "end" .= spanEnd (hrSubtree r) ]

-- | The entries hanging DIRECTLY under the headline in focus, each with the
-- index @?child=@ names it by.
under :: Focus -> [(Int, SubtreeEntry)]
under f = [ (i, e) | (i, e) <- zip [0 ..] (fcEntries f), seParent e == mine ]
  where mine = fromMaybe (-1) (fcAt f)

-- | Where @DEL@ climbs to from here: the entry this one hangs under, 'Nothing'
-- being the row itself.  A focus already ON the row answers 'Nothing' too, and
-- the client tells the two apart by @child@ — null there, a number here.
upFrom :: Focus -> Maybe Int
upFrom f = focusEntry f >>= parentOf

-- | The titles from the ROW down to the headline in focus, inclusive: the
-- breadcrumb a sheet standing in a child shows.  Read up the parent chain and
-- reversed, so a client draws it left to right without walking anything.
trailTo :: Focus -> [Text]
trailTo f = hrTitle (fcRow f) : reverse (climb (fcAt f))
  where climb Nothing  = []
        climb (Just k) = case subtreeEntryAt (fcEntries f) k of
          Nothing -> []
          Just e  -> hrTitle (seRecord e) : climb (parentOf e)

-- | @POST \/headline?id=…@ with @{"org", "digest"}@: the subtree replaced by
-- the text the client edited.
--
-- Or @{"body", "properties", "planning", "digest"}@, the same write with the
-- drawer named apart — recomposed here and identical past that point.  A body
-- naming BOTH shapes is a 400: which of two texts to write is not a thing to
-- guess at.
--
-- TWO DIGEST CHECKS, ONE LOCK: the client's must be the one the store holds, or
-- the file was re-parsed since and the offsets have moved; and 'replaceSpans'
-- re-digests the file itself, catching a change that has not reached the store.
-- Both are a 409 with the file untouched, and both mean materialize again.
--
-- Nothing here touches the store — ONE update channel.  The text is taken as
-- given: org validity is the author's business, and a file that stops parsing
-- keeps the rows it had.
commit :: ServeOptions -> Hub -> Maybe Text -> Either Text (Maybe Int) -> Request
       -> IO Response
commit _opts _hub Nothing _child _request =
  pure (jsonError status400 "POST /headline?id=<row id>")
-- The cap outranks the lookup, so the id and the child index are resolved
-- behind the body rather than in front of it.
commit opts hub (Just rid) child request = withBody request $ \raw -> do
  st <- readTVarIO (hubStore hub)
  case focusIn st rid child >>= \f ->
         (,) (focusHere f) <$> prepare raw (focusHere f) of
    Left refusal -> pure refusal
    Right (here, (digest, org)) ->
      answerWrite rewritten (\fresh -> ["digest" .= fresh])
        <$> writeSpans (walkFor opts) hub (hrFile here) digest
                       [(hrSubtree here, org)]

-- | What writing R needs — the digest to pin and the text to splice — or the
-- response refusing to.  Every refusal but the write's own is decided here, so
-- the IO above it is the write and nothing else.
prepare :: BL.ByteString -> HeadlineRecord -> Either Response (Text, Text)
prepare raw r = case parseCommit raw of
  Left why -> Left (jsonError status400 why)
  Right (asked, digest)
    | digest /= hrDigest r  -> Left (conflict "stale" (hrDigest r) reparsed)
    -- Named rather than counted: a sheet showing three planning rows has to
    -- say which one it will not write.  Its own shape rather than 'conflict''s,
    -- since nothing about it is a digest and a client reading @digest@ off a
    -- 409 is reading the lock its next write would present.
    | Just key <- badPlanning asked -> Left (jsonResponse status409
        [ "error" .= unreadable key, "reason" .= ("planning" :: Text), "field" .= key ])
    | otherwise             -> Right (digest, committed r asked)

-- | The subtree ASKED for, over R: the raw text as given, or the client's parts
-- composed back into one — with the server's own put back beside them.
--
-- 'untrailed' EITHER WAY.  The composed shape is trimmed where it is composed;
-- the raw shape is a whole document the client hands back, so the trim is owed
-- here or `C-c '' is the one door a trailing run still gets in through.
committed :: HeadlineRecord -> Commitment -> Text
committed _r (WholeSubtree org)         = untrailed org
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



-- Keywords

-- | @GET \/keywords?ids=A,B@: the states those rows may be set to, laid out as
-- the chain that classifies them.
--
-- @{"sources": [{"source": …, "active": […], "inactive": […]}], "unknown": […]}@
-- — one entry per SOURCE in precedence order (@default@, @system@, the rows'
-- tags, @file@), each keyword under the WIDEST source that declares it and
-- nowhere below ('Glance.Query.keywordSources').  The answer classifies as well
-- as enumerates, being 'classify' read forwards.
--
-- FOUR sources and no union row: the recognition union says which words PARSE
-- as states, which is neither what classifies a row nor what it may be set to.
-- The chain is both, and it is what 'setStateEdits' checks a write against — so
-- what this offers for ONE row is what that accepts for it.  Several ids merge
-- by source name.
--
-- Resolved for the TARGET ROWS rather than the tree, which is what makes it
-- worth a request.  Refusals are the command route's: no ids is a 400, an
-- unknown id is named in @unknown@ and left out, so a stale marked set still
-- answers for the rows that are there.
keywordsView :: Hub -> Request -> IO Response
keywordsView hub request =
  idsView hub request "GET /keywords?ids=<row id>,<row id>" $ \st _rows found unknown ->
    [ "sources" .= map sourceJSON (keywordSources (stConfig st) found)
    , "unknown" .= unknown
    ]
  where sourceJSON (source, kw) = object ("source" .= source : keywordsPair kw)

-- | The shape both @ids@ reads answer in: the rows REQUEST named resolved ONCE
-- at the door, then FIELDS over the store, the rows found and the ids it holds
-- none for.  No ids at all is USAGE as a 400; an unknown id is reported rather
-- than refused, so a stale marked set still answers for the rows that are
-- there.  Written once, so @\/keywords@ and @\/tags@ cannot come to differ about
-- what a caller pressing one key twice is owed.
idsView :: Hub -> Request -> Text
        -> (Store -> [HeadlineRecord] -> [HeadlineRecord] -> [Text] -> [Pair])
        -> IO Response
idsView hub request usage fields = do
  st <- readTVarIO (hubStore hub)
  -- RESOLVED ONCE, and handed on.  `storeRecords' is `resolveIds' over the
  -- whole store, so a field closure asking the store for them again pays for a
  -- second full resolution on a route a key press reaches.
  let rows = storeRecords st
      (found, unknown) = headlinesIn rows asked
  pure $ if null asked then jsonError status400 usage
                       else jsonResponse status200 (fields st rows found unknown)
  where asked = queryIds request

-- | The single-row read's door: ROW RID names handed to FIELDS, or the 404
-- naming it.  'idsView' for the routes that take a set.
withRow :: Hub -> Text -> (HeadlineRecord -> [Pair]) -> IO Response
withRow hub rid fields = do
  st <- readTVarIO (hubStore hub)
  pure $ maybe (jsonError status404 (noSuchRow rid))
               (jsonResponse status200 . fields)
               (rowIn (storeRecords st) rid)

-- Tags

-- | @GET \/tags?ids=A,B@: what those rows are tagged with, and what else the
-- tree offers.
--
-- @{"rows": [{"id", "tags"}], "vocabulary", "counts", "unknown"}@.  @rows@ is
-- in the order the ids were named, each row's tags in its FILE's order and
-- folded through 'tagsOfCell' — so what this reports is what a write will find.
-- PER ROW rather than as one union, because the client needs WHICH rows lack a
-- tag: an add writes those and no others.
--
-- @vocabulary@ is the whole store's, which a completing read narrows over.
-- @counts@ is how many ROWS the store holds under each tag — the one thing the
-- popup cannot work out from the rows in hand, and not recoverable from
-- 'stTags', which counts FILES.  Counted per request.
--
-- Refusals are @\/keywords@' because the door is ('idsView').
tagsView :: Hub -> Request -> IO Response
tagsView hub request =
  idsView hub request "GET /tags?ids=<row id>,<row id>" $ \st rows found unknown ->
    [ "rows"       .= [ object [ "id" .= hrId r, "tags" .= tagsOfCell (hrTags r) ]
                      | r <- found ]
    , "vocabulary" .= storeTags st
    , "counts"     .= tagRowCounts rows
    , "unknown"    .= unknown
    ]

-- | How many of ROWS carry each tag, over every tag any of them carries.
--
-- Takes the RESOLVED rows rather than the store, so a caller already holding
-- them pays for no second resolution.  A row counts ONCE per tag however often
-- its file spells one, which is what makes the number answer "how many rows
-- would a @TAG:@ predicate reach" — the question the popup's column asks.
--
-- The empty cell is skipped rather than read: `tagsOfCell` lowercases and
-- re-tokenises whatever it is given, and nearly half the corpus's rows carry no
-- tag at all.
tagRowCounts :: [HeadlineRecord] -> Map Text Int
tagRowCounts rows = Map.fromListWith (+)
  [ (tag, 1 :: Int) | r <- rows, not (T.null (hrTags r))
                    , tag <- nub (tagsOfCell (hrTags r)) ]

-- Capture

-- | @GET \/capture[?tag=NAME]@: what a capture under that tag will ask for.
--
-- @{"template": BOOL, "prompts": […], "tags": […], "codes": [{"code","means"}]}@.
-- @template@ says whether a layer configures one — @false@ is the bare @* %?@ —
-- and @prompts@ its @%^{PROMPT}@ asks IN TEMPLATE ORDER, one spelled twice
-- asked once.
--
-- WITH NO TAG it is the untagged path's own shape (no template, no prompts):
-- the inbox capture stays bare, so the answer says so rather than refusing.
--
-- @tags@ is the tree's whole vocabulary — here rather than on @\/tags@ because
-- that route answers about ROWS a caller names and a capture names none.
-- @codes@ is the expansion subset with its meanings, served whatever the tag,
-- so the completion offers exactly what the server expands.
--
-- Needs a loaded store; read-only, so POST is 405.
captureView :: ServeOptions -> Hub -> Request -> IO Response
captureView opts hub request = do
  st <- readTVarIO (hubStore hub)
  template <- case queryText "tag" request of
    Nothing  -> pure Nothing
    Just tag -> captureTemplateIn tag <$> layersFor (soDir opts) st
  pure (jsonResponse status200
          [ "template" .= isJust template
          , "prompts"  .= maybe [] templatePrompts template
          , "tags"     .= storeTags st
          , "codes"    .= [ object ["code" .= code, "means" .= means]
                          | (code, means) <- captureCodes ]
          ])

-- Links

-- | @GET \/links?id=ROW@: where that row points.
--
-- @{"digest", "links": [{"target","desc","type","span"}]}@, in order of
-- appearance, one entry per (target, shown) pair.  The rule is the DISPLAY rule
-- the table answers to — a bracket link is described by its @DESC@, else by its
-- target — plus bare @http(s)@\/@mailto:@ URLs, which describe themselves.
-- @type@ is the folded SCHEME, which @o@ reads to decide whether a tab can be
-- pointed at it.
--
-- @span@ is the half-open CHAR range in the FILE, which is what makes the
-- answer WRITEABLE: @edit-link@ takes it back.  A target spelled twice is one
-- entry, so the span is the FIRST spelling's.  @digest@ is the LOCK: the spans
-- describe the text this store last read, so a client pinning them is refused
-- rather than spliced blind once the file has moved.
--
-- Server-side because it is org text work: the shell holds no bracket grammar
-- and must not grow one.  The SUBTREE rather than the cells, since a reader
-- pressing @o@ means the entry.
linksView :: Hub -> Maybe Text -> IO Response
linksView _hub Nothing = pure (jsonError status400 "GET /links?id=<row id>")
linksView hub (Just rid) = withRow hub rid $ \r ->
  [ "digest" .= hrDigest r
  , "links" .= map linkJSON (subtreeLinks r) ]

-- | One link as the wire spells it -- @\/links@' entry and the materialize's
-- @links@ rider are ONE builder, so the two answers cannot drift.
linkJSON :: OrgLink -> Value
linkJSON l = object [ "target" .= olTarget l, "desc" .= linkShown l
                    , "type" .= linkType (olTarget l)
                    , "span" .= [spanStart (olSpan l), spanEnd (olSpan l)] ]

-- | The one row RID names among ROWS, or 'Nothing'.  The single-id spelling of
-- 'Glance.Web.Store.headlinesIn', so the routes naming ONE row and the ones
-- naming a set answer out of the same lookup, and neither can reach the store
-- to resolve it a second time.
--
-- EVERY ROUTE RESOLVES AT ITS DOOR, once, and passes the rows down — a
-- structural rule rather than a convention: 'Glance.Web.Store' offers nothing
-- taking a 'Store' and answering about an id, so a route owing two folds over
-- the rows (@\/tags@ owes its ids and the store-wide tag counts behind the
-- popup's third column) cannot spell the second as another whole resolution.
rowIn :: [HeadlineRecord] -> Text -> Maybe HeadlineRecord
rowIn rows rid = listToMaybe (fst (headlinesIn rows [rid]))

-- | The rows REQUEST names, deduplicated: every @ids@ parameter, each a comma
-- separated list, and every @id@ parameter, each ONE id — the way
-- @POST \/command@ takes either spelling.  An empty name is dropped rather than
-- looked up, so a trailing comma costs nothing.
--
-- The two keys differ on the comma deliberately: @id@ has to mean on this route
-- what it means on @\/headline@ ('queryId'), and a row id carrying a comma is
-- ordinary — the fallback is @path#ordinal@ and a path may hold one.
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
-- 'Glance.Query.ConfigLayers': what a client is handed is the lock its write is
-- checked against, so it has to be the digest of the very bytes it was shown.
-- The store supplies the one thing a read cannot, WHICH directories.
--
-- @keywords@ is the store's own palette, so the preview is the badge list the
-- table already shows rather than a second computation of it — and it is wider
-- than the layers on purpose: a file's own @#+TODO:@ is in it, which is exactly
-- what this page cannot edit and has to say so about.
configView :: ServeOptions -> Hub -> IO Response
configView opts hub = do
  st <- readTVarIO (hubStore hub)
  layers <- layersFor (soDir opts) st
  -- THE SAME FOLD THE LOAD CACHES IN `clTree', over files read a line ago: what
  -- a sheet shows and what its write is pinned to describe one file, and a
  -- tree-wide member joins both answers by joining `TreeSettings'.
  let tree = treeSettings layers
  pure (jsonResponse status200
          [ "layers"   .= map layerJSON layers
          , "keywords" .= keywordsJSON (storeKeywords st)
          -- THE SAVED VIEWS, by id: what a bare `g' applies and what `a'
          -- does, read off the files beside the lines, each being a line of the
          -- same file whose write rides in the same request.  The capture
          -- target is a line of that kind and travels the same way.
          , "views"    .= [ object [ "id" .= svId v, "query" .= viewQueryIn (svId v) tree ]
                          | v <- savedViews ]
          -- THE THEMES THIS BUILD CARRIES and the tree's own hue for a keyword
          -- under each: a flat list, so the order is the file's and a client
          -- needs no key iteration to read it back.  Another line of the same
          -- system layer, so it rides the same request and the same digest.
          , "themes"   .= themeIds
          , "colors"   .= [ object [ "theme" .= theme, "keyword" .= kw, "hue" .= hue ]
                          | (theme, pairs) <- tsColors tree, (kw, hue) <- pairs ]
          ])

-- | One layer as a settings client holds it.  @template@ is the layer's capture
-- template, verbatim, empty where it has none — a REGION of the same file rather
-- than a line of it, and it rides in this one answer and this one write for the
-- reason the two settings lines do.
layerJSON :: ConfigLayerFile -> Value
layerJSON f = object
  [ "path"     .= lfPath f
  , "tag"      .= lfTag f
  , "lines"    .= todoLines (lfText f)
  -- The same lines PARSED, so a client editing the cycle as a table reads
  -- structure where one editing it as text reads the lines.  The grammar stays
  -- here: this page has no org parser and must not grow one.
  , "keywords" .= keywordsJSON (todoPragmas (lfText f))
  , "template" .= fromMaybe "" (captureTemplateOf (lfText f))
  , "digest"   .= lfDigest f
  ]

keywordsJSON :: TodoKeywords -> Value
keywordsJSON = object . keywordsPair

-- | One keyword set as the two fields every answer spells it with.  Shared, so
-- the settings preview and one source of the resolution table cannot disagree
-- about what a cycle looks like on the wire.
keywordsPair :: TodoKeywords -> [Pair]
keywordsPair kw = ["active" .= tkActive kw, "inactive" .= tkInactive kw]

-- | @POST \/config@ with @{"path", "lines", "digest"}@: one layer's @#+TODO:@
-- block replaced and nothing else in the file touched.
--
-- @path@ must be a layer @GET \/config@ would list, and that is the whole
-- traversal defence: the request names a file this server just offered.  The
-- lookup is also the read the edits are measured in, so the two cannot describe
-- different files.
--
-- The write is every other write: 'configEdits' turns the lines into span edits
-- and 'replaceSpans' splices them under the client's digest, so a comment, a
-- @#+TITLE:@ and a capture template come back byte for byte.  The EMPTY digest
-- is the pin for a layer that does not exist yet — the write creates it and the
-- directories over it, and a file that turned up meanwhile drifts.
--
-- Nothing here touches the store: a config change reseeds the whole tree by the
-- path an editor saving the same file takes.  A write that CREATES the layer
-- goes through 'writeSpans' like every other — the first @.org-glance\/config@
-- is two directories minted at once, which fsnotify arms without entering, so
-- that write used to reseed at the next restart and no sooner.
configWrite :: ServeOptions -> Hub -> Request -> IO Response
configWrite opts hub request = withBody request $ \raw -> do
  st <- readTVarIO (hubStore hub)
  case parseConfigWrite raw of
    Left why   -> pure (jsonError status400 why)
    Right want -> writeLayer opts hub (configDirsIn (soDir opts) (stConfig st)) want

-- | WANT written into one of DIRS' layers, or the refusal.  The lookup that
-- decides which file is the read the edits are then measured in, so the two
-- cannot be describing different bytes.
writeLayer :: ServeOptions -> Hub -> [FilePath] -> LayerWrite -> IO Response
writeLayer opts hub dirs want = do
  layers <- readConfigLayers dirs
  case find ((== path) . T.pack . lfPath) layers of
    Nothing -> pure (jsonError status400 (noSuchLayer path layers))
    -- THE SCOPE MASK RIDES THE FILE: `configEdits' reads the layer's tag and
    -- folds `configSettings', so a tree-wide setting is the SYSTEM layer's
    -- whatever a tag layer's request named, and a new member is masked by
    -- declaring its scope rather than by an edit here.
    Just f  -> case configEdits f (lwLines want) (lwParts want) of
      Left why    -> pure (jsonError status400 why)
      Right edits -> answerWrite configMoved written
                       <$> writeSpans (walkFor opts) hub (lfPath f) (lwDigest want) edits
  where
    path = lwPath want
    written fresh = ["path" .= path, "digest" .= fresh]

noSuchLayer :: Text -> [ConfigLayerFile] -> Text
noSuchLayer path layers =
  "no config layer at " <> path <> "; this tree has "
    <> T.intercalate ", " [ T.pack (lfPath f) | f <- layers ]

-- | RAW as a layer write, or what is wrong with it.  @lines@ is an array
-- because a layer can spell its cycle over more than one @#+TODO:@ line, and a
-- client editing them as text splits on its own newlines rather than asking
-- this server to.  @filter@, @capture@ and @template@ are the default view, the
-- capture target and the layer's capture template, all optional and all
-- three-valued the same way: absent leaves that part as it is, empty takes it
-- away, anything else writes it.  They ride in this one request because they are
-- regions of the same file — four requests would be four writes under four
-- digests, each invalidated by the one before it.
parseConfigWrite :: BL.ByteString -> Either Text LayerWrite
parseConfigWrite = bodyObject "config write" shape
  where shape o = LayerWrite <$> o .: "path" <*> o .:? "lines"
                             <*> (ConfigParts <$> views o <*> colours o <*> o .:? "template")
                             <*> o .: "digest"
        -- @views@ is an OBJECT keyed by view id, three-valued per view the way
        -- every other optional region is: an id absent leaves that view alone,
        -- empty takes its line off, anything else writes it.
        views o = maybe [] Map.toList <$> (o .:? "views" :: Parser (Maybe (Map Text Text)))
        -- @colors@ is the flat @{theme, keyword, hue}@ list the answer serves,
        -- gathered back into a line per theme in the order it arrived; ABSENT
        -- leaves the block, the EMPTY list deletes it.
        colours o = fmap (fmap gather) (o .:? "colors" :: Parser (Maybe [Hue]))
        gather hues = [ (theme, [ (huKeyword h, huHue h) | h <- hues, huTheme h == theme ])
                      | theme <- nub (map huTheme hues) ]

-- | One @{theme, keyword, hue}@ entry of a colour write.
data Hue = Hue { huTheme :: !Text, huKeyword :: !Text, huHue :: !Text }

instance FromJSON Hue where
  parseJSON = withObject "colour" $ \o ->
    Hue <$> o .: "theme" <*> o .: "keyword" <*> o .: "hue"

-- | One layer write as it arrives.  A record rather than the four-slot tuple it
-- was: three of the four are 'Text' and the pattern binding them is written in
-- another function, so a transposed pair would have been a path pinned by a
-- digest of the lines.
data LayerWrite = LayerWrite
  { lwPath   :: !Text          -- ^ which layer, and it must be one @GET \/config@ listed.
  , lwLines  :: !(Maybe [Text])
      -- ^ the @#+TODO:@ block, one entry per line; ABSENT leaves it standing
      -- (the optional regions' own rule — a pin writes the filter alone),
      -- and the EMPTY list is still the deletion.
  , lwParts  :: !ConfigParts   -- ^ the three optional regions riding in the same write.
  , lwDigest :: !Text          -- ^ the pin, empty for a layer that is not there yet.
  }

-- | The @id@ parameter of REQUEST, when it carries one with a value.
queryId :: Request -> Maybe Text
queryId = queryText "id"

-- | NAME's parameter in REQUEST, when it carries one with a value.
queryText :: BS.ByteString -> Request -> Maybe Text
queryText name request = case lookup name (queryString request) of
  Just (Just raw) -> either (const Nothing) Just (TE.decodeUtf8' raw)
  _absent         -> Nothing

-- | The @child@ parameter of REQUEST: which entry inside the row's subtree it
-- names, absent meaning the row itself.  A value that is not a whole number is
-- a 400 rather than a silent fall back to the row, for @limit=@'s reason: a
-- mistyped index that quietly serves the parent looks exactly like a working
-- request, and a write pinned to it would splice over the wrong subtree.
queryChild :: Request -> Either Text (Maybe Int)
queryChild request = case lookup "child" (queryString request) of
  Just (Just raw) -> first (const refusal) (Just <$> wholeNumber "child" raw)
  _absent         -> Right Nothing
  where refusal = "child must be a whole number, 0 or more: the entry's place \
                  \in the subtree, in document order"

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
-- absent one would read as "and drop that region", too much to infer from a
-- field a client forgot.  The two regions the SERVER owns — the hidden
-- properties and the logbook — are in neither shape, and a split body naming
-- them writes nothing: they are taken off the record on the way back in.
parseCommit :: BL.ByteString -> Either Text (Commitment, Text)
parseCommit = bodyObject "commit" shape
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


-- Live socket

-- | @\/ws@: one @set-rows@ with everything the store holds, then a frame per
-- change.  Anything else is refused, so a mistyped path fails loudly.
--
-- @?bootstrap=off@ drops that opening frame for a client that already fetched
-- the rows.  The subscription is unchanged and the mailbox registered in the
-- same transaction, so the snapshot is still taken and only thrown away.  Such
-- a client gives up the gap the snapshot closes, which is why the default
-- stands.
--
-- An upgrade before the startup walk lands is refused with the same 503 the
-- HTTP routes give: a @set-rows@ of an empty store is a claim about the tree.
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
        -- watcher will not wait for it (Glance.Web.Store).  The backlog is gone,
        -- so the close owes the ONE thing that replaces any backlog — re-ask for
        -- rows.  Named for that rather than for the client's speed: the shell
        -- answers it by revalidating /headlines and re-attaching, keeping the
        -- page it had.
        Nothing            -> WS.sendClose conn (closeReason Resync)
        -- The columns moved, and SCHEMA.md streams rows only.  Reconnecting
        -- re-fetches /headlines, which is where columns come from.  Every
        -- server-initiated close spells its reason ONE way ('closeReason'), so
        -- a third one is a constructor rather than a fourth string.
        Just (Close why)   -> WS.sendClose conn (closeReason why)
        Just frame         -> send conn frame >> feed

-- | FRAME down CONN, when it is one of the ops that travels as a message.
send :: WS.Connection -> Frame -> IO ()
send conn = mapM_ (WS.sendTextData conn) . frameText

-- | Read and discard, until the peer goes away.
drainSocket :: WS.Connection -> IO ()
drainSocket conn = forever (void (WS.receiveDataMessage conn))

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
-- the binary does, so a directory without a renderer in it serves no compiled
-- fallback — the case 'assetsMissing' reports.
--
-- The one oracle for what this server has: 'asset' serves what it returns,
-- 'hasRenderer' and 'localFont' ask it whether a name is there at all.  A page
-- declaring a resource this would decline is the drift it exists to prevent.
assetSource :: ServeOptions -> FilePath -> IO (Maybe (Either FilePath BS.ByteString))
assetSource opts name = case soAssets opts of
  Nothing  -> pure (if name == rendererAsset then Just (Right embeddedRenderer)
                    else if name == glueAsset then Just (Right embeddedGlue)
                    else if name == elmAsset then Just (Right embeddedElm)
                    else Nothing)
  -- THE SHELL IS ITS PARTS in a served directory too, read per request so an
  -- edit shows up on a reload: one source, `gluePartFiles`' order, and the same
  -- concatenation the splice makes — a whole `glue.js` in a dev directory would
  -- be a second copy to keep in step.
  Just dir | name == glueAsset -> devGlue dir
  Just dir -> let path = dir </> name
              in (\ok -> if ok then Just (Left path) else Nothing) <$> doesFileExist path

-- | DIR's shell: its PARTS joined where it carries a @glue\/@ directory, else a
-- whole @glue.js@ sitting in it.  Both shapes because a dev directory is
-- whatever a dev put there — the repo's own is parts, a copied-out one is a
-- file — and the page names one script either way.  Read per request, so an
-- edit shows up on a reload.
devGlue :: FilePath -> IO (Maybe (Either FilePath BS.ByteString))
devGlue dir = do
  parts <- filterM doesFileExist [ dir </> "glue" </> p | p <- gluePartFiles ]
  if null parts
    then let whole = dir </> glueAsset
         in (\ok -> if ok then Just (Left whole) else Nothing) <$> doesFileExist whole
    else Just . Right . BS.concat <$> mapM BS.readFile parts

-- | An asset out of 'assetSource', or a 404 naming what was looked for.  Under
-- @--assets@ only files directly in that directory are reachable — one segment,
-- no traversal.  Every one-segment path lands here, so the miss doubles as the
-- route list: @\/graph@ is a mistyped route rather than a missing file, and
-- reads better when told so.
--
-- One response either way, so the wire cannot tell a compiled-in renderer from
-- a file: the same 'mimeOf' content type, and 'compressed' compresses both.  A
-- @responseFile@ because 'GzipCompress' says to and the middleware takes the
-- length off the file; the compiled bytes through 'sized', which gives the
-- middleware a @Content-Length@ to compare against its threshold — a
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

-- | The first of 'fontAssets' this server can serve.  Asked of 'assetSource',
-- the way the renderer is, so a font is declared exactly when the route would
-- answer for it: per request, and never for a name the binary does not carry.
--
-- No font is embedded and none is invented — 'embeddedRenderer' is the whole of
-- what the binary carries — so with no @--assets@ there is no @\@font-face@ and
-- 'Glance.Web.Page.Style.monoStack' is the whole story, as a build without a font file beside the
-- renderer behaved before.
localFont :: ServeOptions -> IO (Maybe FilePath)
localFont opts = listToMaybe <$> filterM (fmap isJust . assetSource opts) fontAssets

-- Pages

-- | @\/@: the demo shell, and an explanation in the one case there is no
-- renderer to mount — @--assets@ naming a directory without one.  With no
-- @--assets@ the shell always renders, since the binary carries the renderer,
-- and a missing renderer leaves @\/headlines@ untouched either way.
--
-- The tree's default view is embedded at REQUEST time rather than at startup:
-- a config change reseeds the store ('Glance.Web.Watch.settle'), so the next
-- page served carries what the file says now.  This route does not wait on the
-- load — the shell has to render while the walk runs — and a store still
-- loading carries no config, which is exactly the built-in fallback.
-- | The tree's saved views as the wire carries them: the registry's order, each
-- with the query it holds NOW.  One fold, so the page's boot blob and the view
-- JSON's `views' vocabulary cannot name different views.
savedViewsIn :: Store -> [(Text, Text)]
savedViewsIn st = [ (svId v, viewQuery (svId v) (stConfig st)) | v <- savedViews ]

shellPage :: ServeOptions -> Hub -> IO Response
shellPage opts hub = do
  ok <- hasRenderer opts
  font <- localFont opts
  st <- readTVarIO (hubStore hub)
  pure . html $ case soAssets opts of
    Just dir | not ok -> assetsMissing opts dir
    _rendererInHand   -> demoShell opts font (tsColors (clTree (stConfig st)))
                                   (savedViewsIn st)
