{-# LANGUAGE TemplateHaskell #-}

-- | The HTTP surface: a fixed route table, its handlers, and the live socket.
-- Routes, caching, the 503 gate and what a write may touch are AGENTS.hs.
-- 'etagOf' is exported so a unit can state the DAY-folds-in law.
module Glance.Web.Routes (application, bootstrapWanted, etagOf, hasRenderer, mcpToolsFor) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (filterM, forever, void, when)
import Data.Aeson (FromJSON (..), Value (Null, Object), encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.Key as Key
import Data.Bifunctor (first)
import Data.List (find, foldl', nub, sortOn)
import Data.Map.Strict (Map)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Language.Haskell.TH (listE)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import qualified Data.Time as Time
import Network.HTTP.Types ( Header, hCacheControl, hContentType, methodGet, methodHead
                          , methodPost, parseQuery, status200, status304, status400
                          , status404, status405, status409, status500, status503 )
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai ( Application, Request (pathInfo, queryString, requestHeaders, requestMethod)
                   , Response, defaultRequest, responseFile, responseLBS )
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Gzip ( GzipFiles (GzipCompress), defaultGzipSettings
                                   , gzip, gzipFiles )
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeExtension, (</>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified Network.WebSockets as WS

import Glance.Query ( ConfigLayerFile (..), ConfigParts (..)
                    , HeadlineParts (..)
                    , HeadlineRecord ( hrDigest, hrFile, hrId, hrLinks, hrOrgId, hrSubtree
                                     , hrTags, hrTitle )
                    , rowProperties
                    , OrgLink (olSpan, olTarget)
                    , QueryResult (..), SortChain
                    , doctorClean
                    , doctorJSON
                    , summaryEnvelope
                    , WriteFailure (WriteDrift, WriteRefused)
                    , Span (spanEnd, spanStart)
                    , SubtreeEntry (..)
                    , TodoKeywords (..)
                    , SavedView (..), archived, configDirsIn, configPaths
                    , pinnedDocument, rowSnapshot
                    , captureTemplateIn, captureTemplateOf
                    , bareTemplate
                    , Inherited (..)
                    , draftKeywords, draftPointLine, draftRecord, draftSeeded, draftTemplate
                    , ConfigLayers (clTree), TreeSettings (..), treeSettings
                    , configEdits, viewQuery, viewQueryIn
                    , headlineParts, keywordSources, linkShown, linkType
                    , mintableLayer
                    , kindSlug, refKind
                    , edgePairs, neighborDepth, neighborDepthCap, neighborLimit
                    , neighborhood
                    , plannedEntry, plannedValue, readConfigLayers
                    , unplanned
                    , untrailed
                    , recomposedSubtree
                    , ownBodyLines, sortedForViewWith
                    , subtreeEntries, subtreeEntryAt, subtreeLinks
                    , subtreeText, tagText, tagsOfCell
                    , titleSpan, todoPragmas
                    , resolveColumns, savedViews, todoLines, viewColumns
                    , viewJSONFor )
import Glance.Web.Base ( Day, ServeOptions (..), answerWrite, bodyObject, configMoved
                       , conflict, docCells, glueAsset, gluePartFiles, html, jsonError
                       , elmAsset
                       , jsonResponse, jsonType
                       , noSuchRow
                       , plain, rendererAsset, reparsed, rewritten, sized, tenths, today
                       , viewTitleFor, walkFor, withBody, writeRefusal )
import Glance.Web.Commands (runCommand, runCommandRaw)
import Glance.Web.Git (gitStatusView, gitSyncRoute)
import Glance.Web.Mcp (McpTools (..), mcpRoute)
import Glance.Web.Filter (archiveKey, matchesFilter, namesArchive, onDay, storeEnv, viewAddedIn)
import Glance.Web.Page (assetsMissing, demoShell)
import Glance.Web.Page.Style (fontAssets)
import Glance.Web.Columns (columnNamesIn)
import Glance.Web.Sort (sortChainIn)
import Glance.Web.Theme (themeIds)
import Glance.Web.Store ( Client, CloseReason (Resync), Frame (Close), Hub
                        , LoadState (..), closeReason
                        , Store (stConfig, stEdges, stGen, stPrint), frameText, layersFor
                        , hubAutoSync, hubDoctor, hubLoad, hubStore, nextFrame
                        , headlinesIn
                        , storeKeywords
                        , storeRecords, storeResult
                        , storeTags, subscribe, unsubscribe )
import Glance.Web.Watch (reload, writeSpans)

-- | The renderer, embedded at COMPILE time; @make sync-renderer@ vendors the file.
embeddedRenderer :: BS.ByteString
embeddedRenderer = $(makeRelativeToProject "assets/table-view.js" >>= embedFile)

-- | The shell, embedded in PARTS: FRAGMENTS of one script scope, joined in 'gluePartFiles' order.
embeddedGlue :: BS.ByteString
embeddedGlue = BS.concat
  $(listE [ makeRelativeToProject ("frontend/glue/" <> part) >>= embedFile
          | part <- gluePartFiles ])

embeddedElm :: BS.ByteString
embeddedElm = $(makeRelativeToProject "assets/elm.js" >>= embedFile)

-- | The @GET \/mcp@ explorer: reads the tool catalog off @POST \/mcp@ and invokes a tool or raw JSON-RPC.
embeddedMcpUi :: BS.ByteString
embeddedMcpUi = $(makeRelativeToProject "assets/mcp.html" >>= embedFile)

mcpUiResponse :: Response
mcpUiResponse = sized status200 [(hContentType, "text/html; charset=utf-8")]
                      (BL.fromStrict embeddedMcpUi)

hasRenderer :: ServeOptions -> IO Bool
hasRenderer opts = isJust <$> assetSource opts rendererAsset

application :: ServeOptions -> Hub -> Application
application opts hub =
  websocketsOr WS.defaultConnectionOptions (liveSocket hub) (compressed (httpApp opts hub))

-- | The HTTP app under @gzip@, INSIDE the websocket branch: an upgrade is no response to rewrite.
compressed :: Application -> Application
compressed = gzip defaultGzipSettings { gzipFiles = GzipCompress }

httpApp :: ServeOptions -> Hub -> Application
httpApp opts hub request respond = route >>= respond
  where
    -- HEAD is GET's, once and explicitly: warp drops the body, so no entry names it.
    wanted | requestMethod request == methodHead = methodGet
           | otherwise                           = requestMethod request
    named =
      [ ([],             False, textRefusal, [(methodGet, shellPage opts hub)])
      , (["headlines"],  True,  textRefusal, [(methodGet, headlines opts hub request)])
      , (["refer"],      True,  textRefusal, [(methodGet, refer opts hub request)])
      , (["headline"],   True,  jsonRefusal,
          [ (methodGet, materialize hub (queryEdges request) (queryId request) (queryChild request))
          , (methodPost, commit opts hub (queryId request) (queryChild request) request) ])
      , (["command"],    True,  jsonRefusal, [(methodPost, runCommand opts hub request)])
      , (["config"],     True,  jsonRefusal,
          [ (methodGet, configView opts hub)
          , (methodPost, configWrite opts hub request) ])
      , (["capture"],    True,  textRefusal, [(methodGet, captureView opts hub request)])
      , (["keywords"],   True,  textRefusal, [(methodGet, keywordsView hub request)])
      , (["links"],      True,  textRefusal, [(methodGet, linksView hub (queryId request))])
      , (["neighbors"],  True,  jsonRefusal, [(methodGet, neighborsView hub request)])
      , (["tags"],       True,  textRefusal, [(methodGet, tagsView hub request)])
      , (["properties"], True,  textRefusal, [(methodGet, propertiesView hub)])
      , (["ws"],         True,  textRefusal, [(methodGet, pure (plain status400 wsHint))])
      , (["status"],     False, jsonRefusal, [(methodGet, statusView opts hub)])
      , (["doctor"],     True,  jsonRefusal, [(methodGet, doctorView hub)])
      , (["mcp"],        True,  jsonRefusal, [ (methodGet, pure mcpUiResponse)
                                             , (methodPost, mcpRoute (mcpToolsFor opts hub) request) ])
      -- git is independent of the org walk, so it answers while the store loads.
      , (["git"],        False, jsonRefusal,
          [ (methodGet,  readTVarIO (hubAutoSync hub) >>= gitStatusView opts)
          , (methodPost, readTVarIO (hubAutoSync hub) >>= \mas -> gitSyncRoute opts mas request) ])
      ]
    route = case [ r | r@(path, _, _, _) <- named, path == pathInfo request ] of
      ((path, needs, refuse, methods) : _) -> do
        load <- readTVarIO (hubLoad hub)
        case load of
          Loading since | needs -> indexing since
          _ready                -> fromMaybe (pure (refuse (takes path methods)))
                                             (lookup wanted methods)
      _noSuchRoute -> fallback
    takes path methods = "/" <> T.intercalate "/" path <> " takes "
                           <> T.intercalate " and " [ TE.decodeUtf8 m | (m, _act) <- methods ]
    jsonRefusal = jsonError status405
    textRefusal = const (plain status405 writeHint)
    fallback
      | wanted /= methodGet = pure (plain status405 writeHint)
      | otherwise = case pathInfo request of
          [name] | safeName name -> asset opts (T.unpack name)
          _other                 -> pure (plain status404 notFound)
    wsHint    = "/ws is a websocket endpoint; connect with Upgrade: websocket"
    -- DERIVED from the table, like `notFound' below: no hand-written list to drift.
    writeHint = "method not allowed; " <> T.intercalate " and "
                  [ "POST /" <> T.intercalate "/" p
                  | (p, _, _, ms) <- named, isJust (lookup methodPost ms) ]
                  <> " write"
    -- Derived from the table above, so a route added there cannot go missing here.
    notFound  = "not found: "
                  <> T.intercalate ", " [ "/" <> T.intercalate "/" p | (p, _, _, _) <- named ]
                  <> ", or an asset name"

-- | The 503 a store route gives while the startup walk runs; an empty 200 would be a claim about the tree.
indexing :: Double -> IO Response
indexing since = do
  now <- getMonotonicTime
  pure . sized status503 [jsonType, ("Retry-After", "1")] . encode
       $ object ["loading" .= True, "elapsed" .= tenths (now - since)]

-- | @GET \/status@: LIVENESS is the 200 itself (no store needed), READINESS the @ready@ flag.
statusView :: ServeOptions -> Hub -> IO Response
statusView opts hub = do
  dir <- canonicalizePath (soDir opts)
  load <- readTVarIO (hubLoad hub)
  fields <- case load of
    Loading since -> do
      now <- getMonotonicTime
      pure ["ready" .= False, "loading" .= True, "elapsed" .= tenths (now - since)]
    Loaded -> do
      st <- readTVarIO (hubStore hub)
      pure ["ready" .= True, "loading" .= False, "rows" .= length (storeRecords st)]
  -- The served TREE, so `glance mcp' proxies only to a daemon that owns its --dir.
  pure . sized status200 [jsonType] . encode $ object (("ok" .= True) : ("dir" .= dir) : fields)

-- | The MCP door's handlers on the live Hub: 'runCommandRaw' to write,
-- 'materialize' and 'headlines' to read — a list reuses the table's rules.
mcpToolsFor :: ServeOptions -> Hub -> McpTools
mcpToolsFor opts hub = McpTools
  { mtWrite     = runCommandRaw opts hub
  , mtHeadline  = \rid edges -> materialize hub (Right edges) (Just rid) (Right Nothing)
  , mtHeadlines = \q limit edges -> headlines opts hub (listRequest q limit edges)
  , mtNeighbors = \rid depth limit kind ->
                    neighborsView hub (neighborRequest rid depth limit kind)
  , mtDoctor    = doctorView hub
  }

-- | The @\/headlines@ request an MCP @list-headlines@ synthesizes: query, cap,
-- @shape=rows@ and the @edges@ it was asked for — the browser sends neither, so
-- an agent gets 'summaryEnvelope' and the table gets the page it always did.
listRequest :: Maybe Text -> Maybe Int -> Bool -> Request
listRequest q limit edges = defaultRequest
  { queryString = [ ("q", Just (TE.encodeUtf8 t)) | Just t <- [q] ]
               <> [ ("limit", Just (BSC.pack (show n))) | Just n <- [limit] ]
               <> [ ("shape", Just "rows") ]
               <> [ ("edges", Just "true") | edges ] }

-- | The @\/neighbors@ request an MCP @neighbors@ synthesizes.  ONE DOOR, so the
-- walls the query string meets — a depth over the cap, a negative one — are the
-- walls the tool meets.
neighborRequest :: Text -> Maybe Int -> Maybe Int -> Maybe Text -> Request
neighborRequest rid depth limit kind = defaultRequest
  { queryString = [ ("id", Just (TE.encodeUtf8 rid)) ]
               <> [ ("depth", Just (BSC.pack (show n))) | Just n <- [depth] ]
               <> [ ("limit", Just (BSC.pack (show n))) | Just n <- [limit] ]
               <> [ ("kind", Just (TE.encodeUtf8 t)) | Just t <- [kind] ] }

-- | @GET \/neighbors?id=…@: the subgraph around a row, DEPTH hops either way,
-- narrowed to a KIND and capped at LIMIT nodes.  OFF THE STORE'S OWN GRAPH
-- ('stEdges'), which is built once per store version — the @from:@\/@ref:@ round
-- trips an agent would otherwise pay per hop.  Both caps refuse rather than trim.
neighborsView :: Hub -> Request -> IO Response
neighborsView hub request = case neighborParams request of
  Left why -> pure (jsonError status400 why)
  Right ask -> do
    st <- readTVarIO (hubStore hub)
    pure (either (jsonError status404) (sized status200 [jsonType] . encode)
            (neighborhood (naKind ask) (naDepth ask) (naLimit ask) (stEdges st) (naId ask)))

data NeighborAsk = NeighborAsk
  { naId    :: !Text
  , naDepth :: !Int
  , naLimit :: !Int
  , naKind  :: !(Maybe Text)
  }

-- | What @\/neighbors@ was asked for, or what is wrong with the asking.  The two
-- numbers meet 'wholeNumber' and 'cappedAt', the walls @\/headlines@' own @limit@ meets.
neighborParams :: Request -> Either Text NeighborAsk
neighborParams request = do
  rid   <- queryWord request "id"
  depth <- cappedAt neighborDepthCap "depth" fromTheFarEnd =<< queryCount request "depth"
  limit <- queryLimit request
  kind  <- queryWord request "kind"
  case rid of
    Nothing -> Left "GET /neighbors?id=<row id>"
    Just i  -> Right NeighborAsk { naId = i, naDepth = fromMaybe neighborDepth depth
                                 , naLimit = fromMaybe neighborLimit limit, naKind = kind }
  where fromTheFarEnd = "ask the far rows from their own end"

-- | @GET \/doctor@: the startup scan's health, read O(1) off the hub and never
-- recomputed — one 'doctorJSON' the boot log and @glance doctor@ share too.
doctorView :: Hub -> IO Response
doctorView hub = do
  doctor <- readTVarIO (hubDoctor hub)
  pure (sized status200 [jsonType] (encode (doctorJSON doctor)))

safeName :: Text -> Bool
safeName name = not (T.null name)
             && name `notElem` [".", ".."]
             && not (T.any (`elem` ("/\\" :: String)) name)

-- Routes

-- | The view JSON, filtered and paged as REQUEST asks.  Archive exclusion, the order and the @ETag@ are AGENTS.hs.
headlines :: ServeOptions -> Hub -> Request -> IO Response
headlines opts hub request = viewPage opts hub request (const True) (const [])

-- | @GET \/refer?q=…[&row=ID]@: 'headlines'' view cut to addressable rows, never the one asked from.
refer :: ServeOptions -> Hub -> Request -> IO Response
refer opts hub request = viewPage opts hub request keep (referExtra asked)
  where
    self = queryText request "row"
    asked = queryText request "kind"
    keep r = isJust (hrOrgId r) && Just (hrId r) /= self

-- | What the picker completes from, over every row THE QUERY MATCHED rather than the page served.
referExtra :: Maybe Text -> [HeadlineRecord] -> [Pair]
referExtra asked rows = referVocabulary rows <> referKinds rows <> echo
  where
    -- THE SLUG IS THE SERVER'S ('kindSlug'): a kind comes back canonical, so no second spelling lives on the page.
    echo = [ "kind" .= kindSlug k | Just k <- [asked], not (T.null (kindSlug k)) ]

-- | The @tag:@ vocabulary: tags of every row THE QUERY MATCHED, commonest first.
-- @state@ and @priority@ ride their columns already, so tags is all this owes.
referVocabulary :: [HeadlineRecord] -> [Pair]
referVocabulary rows =
  [ "vocabulary" .= object [ "tag" .= map fst (commonest (tagRowCounts rows)) ] ]

-- | The kinds the tree uses, commonest first, counted in ROWS like @\/tags@.
-- The count tells an established spelling from a typo, or the vocabulary forks.
referKinds :: [HeadlineRecord] -> [Pair]
referKinds rows =
  [ "kinds" .= [ object ["kind" .= k, "rows" .= n] | (k, n) <- kinds ] ]
  where kinds = commonest (countedBy (\r -> [ k | Just k <- map refKind (hrLinks r) ]) rows)

-- | Commonest first, ties by name -- the one ordering every counted list answers in.
commonest :: Ord k => Map k Int -> [(k, Int)]
commonest = sortOn (\(k, n) -> (negate n, k)) . Map.toList

-- | ONE PIPELINE, and KEEP is all a door may add: every caller answers the same
-- shape with the same headers, so a mount cannot tell two doors apart.
viewPage :: ServeOptions
         -> Hub
         -> Request
         -> (HeadlineRecord -> Bool)
         -> ([HeadlineRecord] -> [Pair])
         -> IO Response
viewPage opts hub request keep extra = case pageParams request of
  Left why -> pure (jsonError status400 why)
  Right PageAsk {..} -> do
    st <- readTVarIO (hubStore hub)
    -- GLOBAL, not per-view: the startup verdict, read O(1) and never recomputed.
    doctor <- readTVarIO (hubDoctor hub)
    -- The request's one clock read ('Base.today'), ABOVE the revalidation: the
    -- day rides in the tag, so a 304 cannot answer with yesterday's rows.
    day <- today
    let tag = etagOf day st
    if tag `elem` ifNoneMatch request
      then pure (responseLBS status304 (cacheHeaders tag) "")
      else do
        let qr      = storeResult st
            -- The reference keys read the store's OWN graph, built once per store version.
            env     = onDay day (storeEnv (stEdges st))
            -- THE FILTER IS COMPILED ONCE, AND THIS BINDING IS WHAT MAKES IT SO: applied
            -- inside the row lambda, `matchesFilter''s parse and `ref:' resolution rerun per row.
            passes  = matchesFilter env paQuery
            asked   = filter (\r -> keep r && passes r) (qrRecords qr)
            matched = if hiding then filter (not . archived) asked else asked
            -- The WHOLE store's tags answer first: the cheaper refusal.
            hiding  = archiveKey `elem` storeTags st && not (namesArchive paQuery)
            hidden  = length asked - length matched
            total   = length matched
            ordered = sortedForViewWith (storeKeywords st) paChain matched
            shown   = maybe matched (\n -> take n (drop paOffset ordered)) paLimit
            hasNext = maybe False (\n -> paOffset + n < total) paLimit
            cols    = maybe viewColumns resolveColumns paPicked
            -- OFF THE STORE'S OWN GRAPH, built once per store version: a `ref:'
            -- pass per row served would be one pass over the tree per row.
            edges   = if paEdges then edgePairs (stEdges st) else const []
            -- EXTRA rides the ONE encoding, over every row the query MATCHED.
            view    = viewJSONFor cols edges (savedViewsIn st) paChain
                                  (viewTitleFor dir) (storeKeywords st) shown
            -- @shape=rows@ (an MCP caller) gets the compact answer; TOTAL stays the
            -- uncapped match count so @limit@ is honest.
            body
              | paRows    = TLE.encodeUtf8 (encodeToLazyText
                              (summaryEnvelope edges total (doctorClean doctor) shown))
              | otherwise = TLE.encodeUtf8 (encodeToLazyText
                              (merged view (("doctor" .= doctorJSON doctor) : extra matched)))
        -- The encode is lazy: an exception in warp's sender would truncate a sent 200.
        forced <- try (evaluate (BL.length body))
        pure $ case forced of
          Left err -> plain status500 (renderError err)
          Right _n -> sized status200
            (jsonType : cacheHeaders tag <> statsHeaders qr <> pageHeaders total hasNext hidden)
            body
  where dir = soDir opts
        renderError :: SomeException -> Text
        renderError e = "headline render failed: " <> T.pack (displayException e)

-- | VIEW with MORE members added here, so there is one encoding and one shape to read.
merged :: Value -> [Pair] -> Value
merged (Object o) more = Object (o <> KM.fromList more)
merged v _more = v

-- | The largest page one request may ask for; over it is a 400 rather than a silent trim.
limitCap :: Int
limitCap = 20000

-- | ST as an entity tag ON DAY.  The generation resets each process, so the
-- fingerprint is what survives one.  THE DAY RIDES ALONG: a store untouched across
-- midnight revalidates rather than 304 yesterday's rows, at one extra hit a day.
etagOf :: Day -> Store -> BSC.ByteString
etagOf day st = "\"" <> TE.encodeUtf8 (T.take 16 (stPrint st))
                  <> "-g" <> BSC.pack (show (stGen st))
                  <> "-d" <> BSC.pack (show day) <> "\""

ifNoneMatch :: Request -> [BSC.ByteString]
ifNoneMatch request =
  [ strong (BSC.dropWhile (== ' ') entry)
  | raw <- maybe [] pure (lookup hIfNoneMatch (requestHeaders request))
  , entry <- BSC.split ',' raw ]
  where strong t = fromMaybe t (BSC.stripPrefix "W/" t)

cacheHeaders :: BSC.ByteString -> [Header]
cacheHeaders tag = [(hETag, tag), (hCacheControl, "no-cache")]

pageHeaders :: Int -> Bool -> Int -> [Header]
pageHeaders total hasNext hidden =
  [ ("X-Glance-Total", BSC.pack (show total))
  , ("X-Glance-Has-Next", if hasNext then "true" else "false")
  , ("X-Glance-Archived", BSC.pack (show hidden)) ]

data PageAsk = PageAsk
  { paQuery  :: !Text            -- ^ @q@, the filter query, view tokens and all.
  , paLimit  :: !(Maybe Int)     -- ^ @limit@; absent serves the whole store.
  , paOffset :: !Int             -- ^ @offset@ into the effective order.
  , paChain  :: !SortChain       -- ^ the order @q@'s @sort:@ tokens state.
  , paPicked :: !(Maybe [Text])  -- ^ the set @q@'s @columns:@ tokens state.
  , paRows   :: !Bool            -- ^ @shape=rows@: the compact 'summaryEnvelope', never the table's own.
  , paEdges  :: !Bool            -- ^ @edges=true@: 'edgePairs' on every row served, under either shape.
  }

-- | @q@, @limit@ and @offset@ out of REQUEST, or what is wrong with one.  @order=@ is refused rather than ignored.
pageParams :: Request -> Either Text PageAsk
pageParams request = do
  q      <- fromMaybe "" <$> queryWord request "q"
  limit  <- queryLimit request
  offset <- fromMaybe 0 <$> queryCount request "offset"
  _order <- maybe (Right ()) (const (Left retired)) =<< queryWord request "order"
  chain  <- sortChainIn q
  picked <- columnNamesIn q
  -- The third view key's own refusal: it has no reader of its own to carry one.
  _added <- viewAddedIn q
  rows   <- queryFlag request "shape" "rows"
  edges  <- queryEdges request
  Right PageAsk { paQuery = q, paLimit = limit, paOffset = offset, paChain = chain
                , paPicked = picked, paRows = rows, paEdges = edges }
  where
    retired = "order= is gone; the order is the query's: ?q=sort:COL, \
              \or ?q=sort:*none* for document order"

-- | What a caller over the page cap is told to do instead.
pageOn :: Text
pageOn = "page with offset for more"

-- | @limit@ under 'limitCap'.  ONE READING for the two doors that take one.
queryLimit :: Request -> Either Text (Maybe Int)
queryLimit request = cappedAt limitCap "limit" pageOn =<< queryCount request "limit"

-- | N under CAP, or the refusal naming the cap and the way round it.  ONE
-- SENTENCE for every capped number, so @limit@ and @depth@ refuse alike.
cappedAt :: Int -> Text -> Text -> Maybe Int -> Either Text (Maybe Int)
cappedAt cap name hint n
  | any (> cap) n = Left (name <> " is at most " <> T.pack (show cap) <> "; " <> hint)
  | otherwise     = Right n

-- | NAME's value in REQUEST, or why it is no text.  A parameter with no @=@
-- reads as absent, so @?limit@ is not a zero page.
queryWord :: Request -> Text -> Either Text (Maybe Text)
queryWord request name = traverse decode' (rawParam request name)
  where decode' bytes = first (const (name <> " is not UTF-8")) (TE.decodeUtf8' bytes)

-- | NAME's value in REQUEST as a whole number, or why it is none.
queryCount :: Request -> Text -> Either Text (Maybe Int)
queryCount request name = traverse (wholeNumber name) (rawParam request name)

rawParam :: Request -> Text -> Maybe BS.ByteString
rawParam request name = case lookup (TE.encodeUtf8 name) (queryString request) of
  Just (Just bytes) -> Just bytes
  _absent           -> Nothing

-- | NAME's value RAW as a whole number.  Read as 'Integer' first: a wrapped 'Int' would page from a negative offset.
wholeNumber :: Text -> BS.ByteString -> Either Text Int
wholeNumber name raw = do
  t <- first (const (name <> " is not UTF-8")) (TE.decodeUtf8' raw)
  case TR.decimal t :: Either String (Integer, Text) of
    Right (n, rest) | T.null rest, n >= 0, n <= toInteger (maxBound :: Int)
                        -> Right (fromInteger n)
    _notANumber         -> Left (name <> " must be a whole number, 0 or more")

-- Materialize



-- | @GET \/headline?id=…@: a subtree, over the file as it stands.  The id rides the query string; a @#@ in a path opens a fragment.
-- EDGES adds the row's own references and the rows pointing at it, off the store the read resolved in.
materialize :: Hub -> Either Text Bool -> Maybe Text -> Either Text (Maybe Int) -> IO Response
materialize _hub _edges Nothing _child = pure (jsonError status400 "GET /headline?id=<row id>")
materialize hub edges (Just rid) child = case edges of
  Left why   -> pure (jsonError status400 why)
  Right want -> either id (answered want) <$> focused Reading hub rid child
  where
    answered want (doc, f) = jsonResponse status200 (subtreeJSON doc f <> rider want f)
    -- OFF THE STORE THE READ RESOLVED IN, never a second resolution of its rows.
    rider want f
      | want      = edgePairs (stEdges (fcStore f)) (fcRow f)
      | otherwise = []

data Focus = Focus
  { fcStore   :: !Store            -- ^ the store version the id resolved in; its graph is 'stEdges'.
  , fcRow     :: !HeadlineRecord   -- ^ the row the id named.
  , fcEntries :: ![SubtreeEntry]   -- ^ every headline inside it, in document order.
  , fcAt      :: !(Maybe Int)      -- ^ the @child@ index; 'Nothing' is the row itself.
  }

-- | What a pinned read does where the STORE LAGS THE FILE.
data Pin
  = Reading  -- ^ RELOAD: the store lagging its own tree is this server's signal.
  | Writing  -- ^ REFUSE: the drift lock, so no write re-targets bytes unseen.

-- | HUB's answer to @?id=RID&child=K@, over the file AS IT STANDS.  A READ
-- RELOADS ON DRIFT ('Glance.Web.Watch.reload') and re-addresses under the fresh
-- digest, a SECOND drift being the genuine race that takes the 409; a WRITE takes
-- it the first time ('Pin', docs\/invariants.md).
onRow :: Pin -> Hub -> Text -> Either Text (Maybe Int)
      -> (Store -> Text -> (HeadlineRecord, Maybe Int) -> Either Response a)
      -> IO (Either Response a)
onRow pin hub rid child k = do
  first' <- turn
  case (pin, first') of
    (Reading, Left (Just path, _refusal)) -> reload hub path >> (settled <$> turn)
    _once                                 -> pure (settled first')
  where
    settled = either (Left . snd) Right
    -- ONE TURN: the id addressed in the store as it stands; a refusal carries the path that DRIFTED.
    turn = do
      st <- readTVarIO (hubStore hub)
      case addressed st of
        Left refusal -> pure (Left (Nothing, refusal))
        Right at -> either (unread (hrFile (fst at))) (standing st at)
                      <$> pinnedDocument (rowSnapshot (fst at))
    standing st at doc = either (\why -> Left (Nothing, why)) Right (k st doc at)
    unread path failed = Left (drifted failed path, writeRefusal rewritten failed)
    drifted (WriteDrift _found) path  = Just path
    drifted (WriteRefused _why) _path = Nothing
    -- The ROW and CHILD the query addresses, BEFORE ANY DISK READ; coarsest refusal first.
    addressed st = do
      at <- first (jsonError status400) child
      r  <- maybe (Left (jsonError status404 (noSuchRow rid))) Right
                  (rowIn (storeRecords st) rid)
      pure (r, at)

-- | The file as it stands and what @?id=RID&child=K@ focuses.  ONE PIPELINE for
-- read and write, so a commit cannot address what a materialize would refuse.
focused :: Pin -> Hub -> Text -> Either Text (Maybe Int)
        -> IO (Either Response (Text, Focus))
focused pin hub rid child = onRow pin hub rid child $ \st doc (r, at) ->
  let entries = subtreeEntries (stConfig st) doc r
  in case at of
       Just k | Nothing <- subtreeEntryAt entries k ->
         Left (jsonError status404
                (hrId r <> " has no child " <> T.pack (show k)
                   <> "; it holds " <> T.pack (show (length entries))))
       _held -> Right (doc, Focus st r entries at)

focusEntry :: Focus -> Maybe SubtreeEntry
focusEntry f = fcAt f >>= subtreeEntryAt (fcEntries f)

focusHere :: Focus -> HeadlineRecord
focusHere f = maybe (fcRow f) seRecord (focusEntry f)

-- | Which entry E hangs under.  The outline walk spells @-1@ for the row, and this is the one place that reading is made.
parentOf :: SubtreeEntry -> Maybe Int
parentOf e = if seParent e < 0 then Nothing else Just (seParent e)

subtreeJSON :: Text -> Focus -> [Pair]
subtreeJSON doc f =
  [ "id"         .= hrId (fcRow f)
  , "file"       .= hrFile (fcRow f)
  , "child"      .= fcAt f
  , "parent"     .= upFrom f
  , "path"       .= trailTo f
  , "level"      .= levelOf f
  , "cells"      .= object (cells here)
  ] <> docPairs doc here f <>
  [ "digest"     .= hrDigest here
  , "span"       .= extentJSON here
    -- The ROW's whole scan, in FILE coordinates: one request, so no async gap to bridge.
  , "links"      .= map linkJSON (subtreeLinks doc (fcRow f))
  , "titleAt"    .= (spanStart <$> titleSpan here)
  ]
  where here = focusHere f

-- | THE DOC PANE'S ENVELOPE: the text, the parts lifted out of it and the brood
-- under it — the members a materialize and a draft both owe, spelled once.  DOC
-- ARRIVES AS A 'Text', never off the row: a record retains no document bytes.
docPairs :: Text -> HeadlineRecord -> Focus -> [Pair]
docPairs doc here f =
  [ "children"   .= [ childJSON here subtree (hpBody parts) i e | (i, e) <- beneath f ]
  , "org"        .= subtree
  , "body"       .= hpBody parts
  , "ownLines"   .= ownBodyLines doc here (hpBody parts) (firstUnder f)
  , "properties" .= [ [key, value] | (key, value) <- hpProperties parts ]
  , "planning"   .= [ [key, value] | (key, value) <- hpPlanning parts ]
  , "logbook"    .= hpLogbook parts
  ]
  where subtree = subtreeText doc here
        parts   = headlineParts doc here

levelOf :: Focus -> Int
levelOf = maybe 1 seLevel . focusEntry

cells :: HeadlineRecord -> [Pair]
cells r = [ Key.fromText k .= f r | (k, f) <- docCells ]

-- | A descendant's line in the lifted body, by the same subtraction 'ownBodyLines'
--   makes.  The SUBTREE is passed in, rendered once for the whole brood.
childJSON :: HeadlineRecord -> Text -> Text -> Int -> SubtreeEntry -> Value
childJSON root subtree body i e = object
  ([ "index" .= i, "level" .= seLevel e
   , "line"  .= bodyLine
   , "span" .= extentJSON (seRecord e) ]
   <> cells (seRecord e))
  where
    -- Newlines COUNTED on both sides, so any convention cancels: the difference is the child's line.
    bodyLine = T.count "\n" body - T.count "\n" (T.drop cut subtree)
    cut = spanStart (hrSubtree (seRecord e)) - spanStart (hrSubtree root)

extentJSON :: HeadlineRecord -> Value
extentJSON r = object [ "start" .= spanStart (hrSubtree r), "end" .= spanEnd (hrSubtree r) ]

firstUnder :: Focus -> Maybe HeadlineRecord
firstUnder f = listToMaybe [ seRecord e | e <- fcEntries f, seParent e == mine ]
  where mine = fromMaybe (-1) (fcAt f)

-- | EVERY descendant of the focus, document order.  ONE LEFT FOLD: a parent precedes
--   its child, so no per-entry chain and a malformed pointer cannot loop.
beneath :: Focus -> [(Int, SubtreeEntry)]
beneath f = [ (i, e) | (i, e) <- zip [0 ..] entries, i `IntSet.member` hit ]
  where
    entries = fcEntries f
    mine = fromMaybe (-1) (fcAt f)
    hit = foldl' claim IntSet.empty (zip [0 ..] entries)
    claim seen (i, e)
      | seParent e == mine || seParent e `IntSet.member` seen = IntSet.insert i seen
      | otherwise = seen

upFrom :: Focus -> Maybe Int
upFrom f = focusEntry f >>= parentOf

trailTo :: Focus -> [Text]
trailTo f = hrTitle (fcRow f) : reverse (climb (fcAt f))
  where climb Nothing  = []
        climb (Just k) = case subtreeEntryAt (fcEntries f) k of
          Nothing -> []
          Just e  -> hrTitle (seRecord e) : climb (parentOf e)

-- | @POST \/headline?id=…@: the subtree replaced, whole or by parts.  TWO DIGEST CHECKS, ONE LOCK; nothing here touches the store.
commit :: ServeOptions -> Hub -> Maybe Text -> Either Text (Maybe Int) -> Request
       -> IO Response
commit _opts _hub Nothing _child _request =
  pure (jsonError status400 "POST /headline?id=<row id>")
-- The cap outranks the lookup, so the id resolves behind the body.
commit opts hub (Just rid) child request = withBody request $ \raw -> do
  -- The request's one clock read ('Base.today'), above the row: one commit is ONE day.
  day <- today
  got <- focused Writing hub rid child
  case got >>= \(doc, f) -> (,) (focusHere f) <$> prepare day raw doc (focusHere f) of
    Left refusal -> pure refusal
    Right (here, (digest, org)) ->
      answerWrite rewritten (\fresh -> ["digest" .= fresh])
        <$> writeSpans (walkFor opts) hub (hrFile here) digest
                       [(hrSubtree here, org)]

-- | The CLIENT's pin against the row's.  A SECOND LAW beyond the drift lock: the
-- client names the digest it materialized at, and a re-read store is a @stale@ 409.
prepare :: Day -> BL.ByteString -> Text -> HeadlineRecord -> Either Response (Text, Text)
prepare day raw doc r = case parseCommit raw of
  Left why -> Left (jsonError status400 why)
  Right (asked, digest)
    | digest /= hrDigest r  -> Left (conflict "stale" (hrDigest r) reparsed)
    | otherwise             -> case settledPlanning day asked of
        Left (key, why) -> Left (jsonResponse status409
          [ "error" .= why, "reason" .= ("planning" :: Text), "field" .= key ])
        Right settled   -> Right (digest, committed doc r settled)

-- | The subtree ASKED for, over R in DOC.  'untrailed' EITHER WAY: the raw shape is a whole document the client hands back.
committed :: Text -> HeadlineRecord -> Commitment -> Text
committed _doc _r (WholeSubtree org)         = untrailed org
committed doc  r  (SplitSubtree body ps pln) =
  recomposedSubtree doc r (HeadlineParts body ps pln "")

-- | The commitment with its planning entries READ AND REWRITTEN, or the KEY that
-- stops the write and WHY.  Each entry meets 'plannedValue', the wall @set-planning@ meets.
--
-- THE RAW HALF TRANSFORMS NOTHING: rewriting bytes in a 'WholeSubtree' would be the server editing a client's buffer.
settledPlanning :: Day -> Commitment -> Either (Text, Text) Commitment
settledPlanning _day whole@(WholeSubtree _org) = Right whole
-- An unknown KEY outranks every value.
settledPlanning day (SplitSubtree body ps pln)
  | Just refusal <- listToMaybe refused = Left refusal
  | otherwise = SplitSubtree body ps <$> traverse (plannedEntry day) pln
  where refused = [ (key, why) | (key, _v) <- pln, Just why <- [unplanned key] ]



-- Keywords

-- | @GET \/keywords?ids=A,B@: the states those rows may be set to, laid out as the chain that classifies them (AGENTS.hs).
keywordsView :: Hub -> Request -> IO Response
keywordsView hub request =
  idsView hub request "GET /keywords?ids=<row id>,<row id>" $ \st _rows found unknown ->
    [ "sources" .= map sourceJSON (keywordSources (stConfig st) found)
    , "unknown" .= unknown
    ]

-- | One scope as the wire spells it.  @\/keywords@ and the draft @cycle@ are ONE builder.
sourceJSON :: (Text, TodoKeywords) -> Value
sourceJSON (source, kw) = object ("source" .= source : keywordsPair kw)

idsView :: Hub -> Request -> Text
        -> (Store -> [HeadlineRecord] -> [HeadlineRecord] -> [Text] -> [Pair])
        -> IO Response
idsView hub request usage fields = do
  st <- readTVarIO (hubStore hub)
  -- RESOLVED ONCE and handed on: 'storeRecords' is a full resolution per call.
  let rows = storeRecords st
      (found, unknown) = headlinesIn rows asked
  pure $ if null asked then jsonError status400 usage
                       else jsonResponse status200 (fields st rows found unknown)
  where asked = queryIds request

withRow :: Hub -> Text -> (Text -> HeadlineRecord -> [Pair]) -> IO Response
withRow hub rid fields =
  either id (jsonResponse status200)
    <$> onRow Reading hub rid (Right Nothing) (\_st doc (r, _at) -> Right (fields doc r))

-- Tags

-- | @GET \/tags?ids=A,B@: what those rows are tagged with.  PER ROW, since the client needs WHICH rows lack a tag.
tagsView :: Hub -> Request -> IO Response
tagsView hub request =
  idsView hub request "GET /tags?ids=<row id>,<row id>" $ \st rows found unknown ->
    [ "rows"       .= [ object [ "id" .= hrId r, "tags" .= tagsOfCell (hrTags r) ]
                      | r <- found ]
    , "vocabulary" .= storeTags st
    , "counts"     .= tagRowCounts rows
    , "unknown"    .= unknown
    ]

tagRowCounts :: [HeadlineRecord] -> Map Text Int
tagRowCounts = countedBy (tagsOfCell . hrTags)

-- | ROWS per key, a row counted ONCE however often it names one.
{-# INLINE countedBy #-}
countedBy :: Ord k => (a -> [k]) -> [a] -> Map k Int
countedBy keys rows =
  Map.fromListWith (+) [ (k, 1 :: Int) | r <- rows, k <- nub (keys r) ]

-- Properties

-- | @GET \/properties@: the drawer vocabulary — every key, every value, and the ROWS
-- each sits on.  NAMES NO ROW, so the whole store answers, recomputed per request.
propertiesView :: Hub -> IO Response
propertiesView hub = do
  st <- readTVarIO (hubStore hub)
  -- ONE drawer walk, counted twice: a key's rows are no arithmetic on its values' —
  -- a row spelling one key twice is ONE row under the key, one under each value.
  let drawers = map completable (storeRecords st)
  pure (jsonResponse status200
          [ "keys"   .= countedBy (map fst) drawers
          , "values" .= valuesUnder drawers ])

-- | R's pairs a client may COMPLETE from.  'rowProperties' drops the hidden keys —
-- completing one would write it — and reserved keys are absent by construction.
-- A drawer line that parses to no key is no vocabulary.
completable :: HeadlineRecord -> [(Text, Text)]
completable r = [ p | p <- rowProperties r, not (T.null (fst p)) ]

-- | ROWS per VALUE under its key, nested from ONE 'countedBy' so a value counts by its key's rule.
valuesUnder :: [[(Text, Text)]] -> Map Text (Map Text Int)
valuesUnder drawers = Map.fromListWith (Map.unionWith (+))
  [ (key, Map.singleton value n)
  | ((key, value), n) <- Map.toList (countedBy id drawers) ]

-- Capture

-- | @GET \/capture[?tag=NAME]@: the DRAFT a capture under that tag opens on.
--
-- THE SHAPE @\/headline@ SERVES, field for field, off bytes that exist only in this
-- answer, so the pane draws a draft as it draws any doc.  Three fields ride beside it:
-- the @cycle@ (a draft has no ROW), the @point@ @%?@ stood at, and the tree's @tags@.
-- NO FILE IS CREATED.
captureView :: ServeOptions -> Hub -> Request -> IO Response
captureView opts hub request = do
  st <- readTVarIO (hubStore hub)
  layers <- layersFor (soDir opts) st
  -- One read, above the expansion ('Base.today''s rule): template stamps and a lent day name ONE instant.
  now <- Time.getZonedTime
  let cfg = stConfig st
      tag = fromMaybe "" (queryText request "tag")
      -- WHAT THE DRAFT WEARS.  THE DESTINATION LEADS; lent tags follow, each through
      -- the CHARSET wall below — a lent tag org cannot read is filter noise, worn no
      -- more here than written there.  Deduplicated: one tag named twice is one tag.
      worn = nub ([ T.toLower tag | not (T.null tag) ] <> lent)
      lent = [ T.toLower raw | raw <- inheritedTags request
                             , Right _ <- [tagText raw] ]
      day = Time.localDay (Time.zonedTimeToLocalTime now)
      drafted = do
        (expanded, at) <- draftTemplate now (fromMaybe bareTemplate (captureTemplateIn tag layers))
        -- The point is read off the EXPANDED doc: seeding edits the headline and planning,
        -- not body lines, so the line index survives the seeding measured before it.
        opens <- draftPointLine expanded <$> draftRecord cfg expanded <*> pure at
        seeded <- draftSeeded cfg worn (inheritedIn day request) expanded
        r <- draftRecord cfg seeded
        pure (draftJSON st worn seeded r opens)
  pure (either (jsonError status400) (jsonResponse status200) drafted)

-- | A DRAFT as the wire carries it: 'subtreeJSON''s members plus the three a
-- fileless doc owes.  The empty digest is the CREATE PIN, walling the commit like a materialize's.
draftJSON :: Store -> [Text] -> Text -> HeadlineRecord -> Maybe Int -> [Pair]
draftJSON st worn doc r opens =
  [ "id"         .= Null
  , "file"       .= ("" :: Text)
  , "child"      .= Null
  , "parent"     .= Null
  , "path"       .= [hrTitle r]
  , "level"      .= (1 :: Int)
  , "cells"      .= object (draftCells worn r)
  ] <> docPairs doc r f <>
  [ "digest"     .= ("" :: Text)
  , "span"       .= Null
  , "links"      .= ([] :: [Value])
  , "titleAt"    .= Null
  , "cycle"      .= map sourceJSON (draftKeywords (stConfig st) worn)
  , "point"      .= opens
  , "tags"       .= storeTags st
  ]
  where f = Focus st r (subtreeEntries (stConfig st) doc r) Nothing

-- | A DRAFT'S DISPLAY CELLS: 'cells', with the tag run saying WHERE THIS LANDS.
-- A DISPLAY CELL IS CONSTRUCTED, owing no round trip through the org line: a
-- title-less headline spells no run here, and the reader must see the destination.
draftCells :: [Text] -> HeadlineRecord -> [Pair]
draftCells worn r =
  [ if k == "tags" then Key.fromText k .= draftTagsCell worn r
                   else Key.fromText k .= f r
  | (k, f) <- docCells ]

-- | WORN — destination and lent — then the draft's own line beyond them, as an org
-- tag cell.  THE COMMIT WEARS EACH ONCE: minting folds idempotently, so no twin.
draftTagsCell :: [Text] -> HeadlineRecord -> Text
draftTagsCell worn r
  | null run  = ""
  | otherwise = ":" <> T.intercalate ":" run <> ":"
  where run = worn <> [ t | t <- tagsOfCell (hrTags r), t `notElem` worn ]

-- | What the standing filter LENDS this draft.  NEVER A REFUSAL: an unreadable
-- inherited fact is filter noise, so it fills the gap or not and @+@ opens either way.
inheritedIn :: Day -> Request -> Inherited
inheritedIn day request = Inherited
  { inhState    = queryText request "state"
  , inhPriority = queryText request "priority"
  , inhTags     = inheritedTags request
  , inhPlanning = [ (key, stamp)
                  | (key, name) <- [("SCHEDULED", "scheduled"), ("DEADLINE", "deadline")]
                  , Just value <- [queryText request name]
                  , Right stamp <- [plannedValue day key value] ]
  }

-- | @?tags=a,b@: the positive filter tags a draft wears beyond the template's.
inheritedTags :: Request -> [Text]
inheritedTags request =
  [ t | raw <- maybe [] (T.splitOn ",") (queryText request "tags")
      , let t = T.strip raw, not (T.null t) ]

-- Links

-- | @GET \/links?id=ROW@: where that row points.  @span@ is the FILE range @edit-link@ takes back, under @digest@ as the lock.
linksView :: Hub -> Maybe Text -> IO Response
linksView _hub Nothing = pure (jsonError status400 "GET /links?id=<row id>")
linksView hub (Just rid) = withRow hub rid $ \doc r ->
  [ "digest" .= hrDigest r
  , "links" .= map linkJSON (subtreeLinks doc r) ]

-- | One link as the wire spells it -- @\/links@' entry and the materialize rider are ONE builder.
linkJSON :: OrgLink -> Value
linkJSON l = object [ "target" .= olTarget l, "desc" .= linkShown l
                    , "type" .= linkType (olTarget l)
                    , "span" .= [spanStart (olSpan l), spanEnd (olSpan l)] ]

-- | The one row RID names among ROWS.  EVERY ROUTE RESOLVES AT ITS DOOR, once, and passes the rows down.
rowIn :: [HeadlineRecord] -> Text -> Maybe HeadlineRecord
rowIn rows rid = listToMaybe (fst (headlinesIn rows [rid]))

-- | The rows REQUEST names, deduplicated.  @ids@ splits on commas and @id@ does not: a row id may carry one.
queryIds :: Request -> [Text]
queryIds request =
  nub [ rid
      | (key, Just raw) <- queryString request, key `elem` ["ids", "id"]
      , Just text <- [silentText raw]
      , rid <- if key == "ids" then T.splitOn "," text else [text], not (T.null rid) ]

-- Config

-- | @GET \/config@: the keyword layers a settings client edits.  Read from the FILES: the digest handed out is the write's lock.
configView :: ServeOptions -> Hub -> IO Response
configView opts hub = do
  st <- readTVarIO (hubStore hub)
  layers <- layersFor (soDir opts) st
  let tree = treeSettings layers
  pure (jsonResponse status200
          [ "layers"   .= map layerJSON layers
            -- WHERE A TAG LAYER GOES that has no file yet.  The path rule is the
            -- server's, served here and applied again inbound by 'mintableLayer'.
          , "tagsDir"  .= maybe "" (snd . configPaths)
                                (listToMaybe (configDirsIn (soDir opts) (stConfig st)))
          , "keywords" .= keywordsJSON (storeKeywords st)
          , "views"    .= [ object [ "id" .= svId v, "query" .= viewQueryIn (svId v) tree ]
                          | v <- savedViews ]
          , "themes"   .= themeIds
          , "colors"   .= [ object [ "theme" .= theme, "keyword" .= kw, "hue" .= hue ]
                          | (theme, pairs) <- tsColors tree, (kw, hue) <- pairs ]
          ])

layerJSON :: ConfigLayerFile -> Value
layerJSON f = object
  [ "path"     .= lfPath f
  , "tag"      .= lfTag f
  , "lines"    .= todoLines (lfText f)
  , "keywords" .= keywordsJSON (todoPragmas (lfText f))
  , "template" .= fromMaybe "" (captureTemplateOf (lfText f))
  , "digest"   .= lfDigest f
  ]

keywordsJSON :: TodoKeywords -> Value
keywordsJSON = object . keywordsPair

keywordsPair :: TodoKeywords -> [Pair]
keywordsPair kw = ["active" .= tkActive kw, "inactive" .= tkInactive kw]

-- | @POST \/config@: one layer's PARTS replaced.  @path@ must be a layer @GET \/config@ listed, which is the whole traversal defence.
configWrite :: ServeOptions -> Hub -> Request -> IO Response
configWrite opts hub request = withBody request $ \raw -> do
  st <- readTVarIO (hubStore hub)
  case parseConfigWrite raw of
    Left why   -> pure (jsonError status400 why)
    Right want -> writeLayer opts hub (configDirsIn (soDir opts) (stConfig st)) want

writeLayer :: ServeOptions -> Hub -> [FilePath] -> LayerWrite -> IO Response
writeLayer opts hub dirs want = do
  layers <- readConfigLayers dirs
  -- A TAG LAYER IS MINTED BY BEING WRITTEN TO, only under the FIRST config dir's
  -- `tags/', so a write cannot name a path this tree does not own.
  case find ((== path) . T.pack . lfPath) layers
         <|> (listToMaybe dirs >>= \d -> mintableLayer d (T.unpack path)) of
    Nothing -> pure (jsonError status400 (noSuchLayer path layers))
    -- THE SCOPE MASK RIDES THE FILE: 'configEdits' folds 'configSettings' off the layer's tag.
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

-- | RAW as a layer write.  Every optional PART is three-valued and rides one request: they are parts of one file.
parseConfigWrite :: BL.ByteString -> Either Text LayerWrite
parseConfigWrite = bodyObject "config write" shape
  where shape o = LayerWrite <$> o .: "path" <*> o .:? "lines"
                             <*> (ConfigParts <$> views o <*> colours o <*> o .:? "template")
                             <*> o .: "digest"
        views o = maybe [] Map.toList <$> (o .:? "views" :: Parser (Maybe (Map Text Text)))
        colours o = fmap (fmap gather) (o .:? "colors" :: Parser (Maybe [Hue]))
        gather hues = [ (theme, [ (huKeyword h, huHue h) | h <- hues, huTheme h == theme ])
                      | theme <- nub (map huTheme hues) ]

data Hue = Hue { huTheme :: !Text, huKeyword :: !Text, huHue :: !Text }

instance FromJSON Hue where
  parseJSON = withObject "colour" $ \o ->
    Hue <$> o .: "theme" <*> o .: "keyword" <*> o .: "hue"

-- | One layer write as it arrives.  A record: three of the four are 'Text', so a transposed pair would compile.
data LayerWrite = LayerWrite
  { lwPath   :: !Text          -- ^ which layer, and it must be one @GET \/config@ listed.
  , lwLines  :: !(Maybe [Text])
      -- ^ the @#+TODO:@ block, one per line; ABSENT leaves it standing (a pin writes
      -- the filter alone), and the EMPTY list is the deletion.
  , lwParts  :: !ConfigParts   -- ^ the three optional parts riding in the same write.
  , lwDigest :: !Text          -- ^ the pin, empty for a layer that is not there yet.
  }

queryId :: Request -> Maybe Text
queryId request = queryText request "id"

-- | @?edges=true@: does this read owe each row's references and the rows pointing
-- at it?  THE ONE READER, for the subtree door and the table's own.
queryEdges :: Request -> Either Text Bool
queryEdges request = queryFlag request "edges" "true"

-- | @?NAME=WORD@ as a flag: WORD is 'True', absent is 'False', and every other
-- value is REFUSED rather than read as unasked.  ONE SENTENCE for both flags, so
-- @edges@ and @shape@ cannot grow two refusal vocabularies.
queryFlag :: Request -> Text -> Text -> Either Text Bool
queryFlag request name word = queryWord request name >>= \given -> case given of
  Nothing    -> Right False
  Just given'
    | given' == word -> Right True
    | otherwise      -> Left (name <> " is " <> word <> ", or absent")

-- | NAME's value in REQUEST, bytes that are no UTF-8 reading as ABSENT.  THE
-- SILENT WRAPPER, deliberately: these parameters lend rather than command, so an
-- undecodable one fills no gap and refuses nothing ('queryWord' is the 400).
queryText :: Request -> Text -> Maybe Text
queryText request name = silentText =<< rawParam request name

silentText :: BS.ByteString -> Maybe Text
silentText = either (const Nothing) Just . TE.decodeUtf8'

-- | The @child@ parameter.  A non-number is a 400: a write pinned to a mistyped index would splice the wrong subtree.
queryChild :: Request -> Either Text (Maybe Int)
queryChild request = first (const refusal) (queryCount request "child")
  where refusal = "child must be a whole number, 0 or more: the entry's place \
                  \in the subtree, in document order"

data Commitment
  = WholeSubtree !Text  -- ^ @org@: the subtree as it is to be written.
  | SplitSubtree !Text ![(Text, Text)] ![(Text, Text)]
      -- ^ @body@, @properties@ and @planning@, to be composed.
  deriving (Eq, Show)

-- | What a commit body asks for and the digest it pins to.  Naming both shapes is refused rather than resolved.
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

-- | @\/ws@: a @set-rows@ snapshot, then a frame per change.  @?bootstrap=off@ drops the snapshot; an upgrade mid-walk is 503.
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

bootstrapWanted :: BSC.ByteString -> Bool
bootstrapWanted path = ("bootstrap", Just "off") `notElem` parseQuery query
  where query = BSC.dropWhile (/= '?') path

-- | Feed CLIENT's mailbox to CONN.  The read side is what notices a closed tab and answers the protocol's control frames.
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
        -- The mailbox filled: the backlog is gone, so the close asks for rows.
        Nothing            -> WS.sendClose conn (closeReason Resync)
        Just (Close why)   -> WS.sendClose conn (closeReason why)
        Just frame         -> send conn frame >> feed

send :: WS.Connection -> Frame -> IO ()
send conn = mapM_ (WS.sendTextData conn) . frameText

drainSocket :: WS.Connection -> IO ()
drainSocket conn = forever (void (WS.receiveDataMessage conn))

-- | What the load covered.  Headers because the View object's fields are SCHEMA.md's.
statsHeaders :: QueryResult -> [Header]
statsHeaders qr =
  [ count "X-Glance-Rows"            (length (qrRecords qr))
  , count "X-Glance-Files"           (qrFiles qr)
  , count "X-Glance-Parse-Failures"  (qrParseFailures qr)
  , count "X-Glance-Decode-Failures" (qrDecodeFailures qr)
  , count "X-Glance-Read-Failures"   (qrReadFailures qr)
  , count "X-Glance-Id-Collisions"   (length (qrIdCollisions qr))
  ]
  where count name n = (name, BSC.pack (show n))

-- | Where NAME's bytes come from under OPTS.  The two cases are exclusive, and this is the one oracle for what the server has.
assetSource :: ServeOptions -> FilePath -> IO (Maybe (Either FilePath BS.ByteString))
assetSource opts name = case soAssets opts of
  Nothing  -> pure (Right <$> lookup name [ (rendererAsset, embeddedRenderer)
                                          , (glueAsset, embeddedGlue)
                                          , (elmAsset, embeddedElm) ])
  -- THE SHELL IS ITS PARTS in a served directory too, read per request; a whole @glue.js@ would be a second copy.
  Just dir | name == glueAsset -> devGlue dir
  Just dir -> fileAt (dir </> name)

fileAt :: FilePath -> IO (Maybe (Either FilePath BS.ByteString))
fileAt path = (\there -> if there then Just (Left path) else Nothing) <$> doesFileExist path

devGlue :: FilePath -> IO (Maybe (Either FilePath BS.ByteString))
devGlue dir = do
  parts <- filterM doesFileExist [ dir </> "glue" </> p | p <- gluePartFiles ]
  if null parts
    then fileAt (dir </> glueAsset)
    else Just . Right . BS.concat <$> mapM BS.readFile parts

-- | An asset, or a 404 doubling as the route list.  Compiled bytes go through 'sized': no @Content-Length@, no compression.
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

localFont :: ServeOptions -> IO (Maybe FilePath)
localFont opts = listToMaybe <$> filterM (fmap isJust . assetSource opts) fontAssets

-- Pages

-- | The tree's saved views as the wire carries them.  ONE fold, so the page's boot blob and the view JSON cannot name different views.
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
