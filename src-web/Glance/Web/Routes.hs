{-# LANGUAGE TemplateHaskell #-}

-- | The HTTP surface: a fixed route table, its handlers, and the live socket.
-- Routes, caching, the 503 gate and what a write may touch are AGENTS.hs.
module Glance.Web.Routes (application, bootstrapWanted, hasRenderer) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Control.Monad (filterM, forever, void, when)
import Data.Aeson (FromJSON (..), Value (Object), encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.Key as Key
import Data.Bifunctor (first)
import Data.List (find, nub, sortOn)
import Data.Map.Strict (Map)
import Control.Applicative ((<|>))
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
                    , HeadlineRecord ( hrDigest, hrFile, hrId, hrLinks, hrSubtree, hrTags
                                     , hrTitle )
                    , rowOrgId
                    , OrgLink (olSpan, olTarget)
                    , QueryResult (..), SortChain
                    , Span (spanEnd, spanStart)
                    , SubtreeEntry (..)
                    , TodoKeywords (..)
                    , SavedView (..), archived, configDirsIn, configPaths
                    , captureTemplateIn, captureTemplateOf
                    , ConfigLayers (clTree), TreeSettings (..), treeSettings
                    , configEdits, viewQuery, viewQueryIn
                    , headlineParts, keywordSources, linkShown, linkType
                    , mintableLayer
                    , kindSlug, refKind
                    , planningKeywords, readConfigLayers, readsAsTimestamp
                    , untrailed
                    , recomposedSubtree
                    , ownBodyLines, sortedForViewWith
                    , subtreeEntries, subtreeEntryAt, subtreeLinks
                    , subtreeText, tagsOfCell
                    , templatePrompts, titleSpan, todoPragmas
                    , resolveColumns, savedViews, todoLines, viewColumns
                    , viewJSONFor )
import Glance.Web.Base ( ServeOptions (..), answerWrite, bodyObject, codeList, configMoved
                       , conflict, docCells, glueAsset, gluePartFiles, html, jsonError
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
      [ ([],            False, textRefusal, [(methodGet, shellPage opts hub)])
      , (["headlines"], True,  textRefusal, [(methodGet, headlines opts hub request)])
      , (["refer"],     True,  textRefusal, [(methodGet, refer opts hub request)])
      , (["headline"],  True,  jsonRefusal,
          [ (methodGet, materialize hub (queryId request) (queryChild request))
          , (methodPost, commit opts hub (queryId request) (queryChild request) request) ])
      , (["command"],   True,  jsonRefusal, [(methodPost, runCommand opts hub request)])
      , (["config"],    True,  jsonRefusal,
          [ (methodGet, configView opts hub)
          , (methodPost, configWrite opts hub request) ])
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
    -- DERIVED, like `notFound' under it: the table above knows which entries
    -- carry `methodPost', and a hand-written sentence had missed /config.
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

safeName :: Text -> Bool
safeName name = not (T.null name)
             && name `notElem` [".", ".."]
             && not (T.any (`elem` ("/\\" :: String)) name)

-- Routes

-- | The view JSON, filtered and paged as REQUEST asks.  Archive exclusion, the order and the @ETag@ are AGENTS.hs.
headlines :: ServeOptions -> Hub -> Request -> IO Response
headlines opts hub request = viewPage opts hub request (const True) (const [])

-- | @GET \/refer?q=…[&row=ID]@: the rows a REFERENCE may name — 'headlines''
-- own view, cut to the addressable rows and never the one asked from.
refer :: ServeOptions -> Hub -> Request -> IO Response
refer opts hub request = viewPage opts hub request keep (referExtra asked)
  where
    self = queryText "row" request
    asked = queryText "kind" request
    keep r = isJust (rowOrgId r) && Just (hrId r) /= self

-- | What the picker completes from, over every row THE QUERY MATCHED rather than
-- the page served.  A reader narrowing the picker narrows what it offers, which
-- is the same rule the @tag@ half has always had.
referExtra :: Maybe Text -> [HeadlineRecord] -> [Pair]
referExtra asked rows = referVocabulary rows <> referKinds rows <> echo
  where
    -- THE SLUG IS THE SERVER'S, said once: a kind typed into the picker comes
    -- back canonical, so the page writes what org-glance would have written and
    -- no second spelling of the rule lives on the page ('kindSlug').
    echo = [ "kind" .= kindSlug k | Just k <- [asked], not (T.null (kindSlug k)) ]

-- | What the picker may complete a @tag:@ from: the tags of every row THE QUERY
-- MATCHED rather than the page served, commonest first.  The @state@ and
-- @priority@ domains ride their COLUMNS already ('columnsFor'), so the one
-- column that declares none is all this owes — and the count is
-- 'tagRowCounts'', the same rule @\/tags@ answers with.
referVocabulary :: [HeadlineRecord] -> [Pair]
referVocabulary rows =
  [ "vocabulary" .= object [ "tag" .= map fst (sortOn rank counted) ] ]
  where
    counted = Map.toList (tagRowCounts rows)
    rank (tag, n) = (negate n, tag)

-- | THE KINDS THE TREE ALREADY USES, commonest first, counted in ROWS the way
-- @\/tags@ counts them.  The COUNT is what the picker shows beside each: free
-- text is how a kind is minted, so an established spelling has to be tellable
-- from a typo made once, or the vocabulary forks.
referKinds :: [HeadlineRecord] -> [Pair]
referKinds rows =
  [ "kinds" .= [ object ["kind" .= k, "rows" .= n] | (k, n) <- sortOn rank counted ] ]
  where
    counted = Map.toList (countedBy (\r -> [ k | Just k <- map refKind (hrLinks r) ]) rows)
    rank (k, n) = (negate n, k)

-- | ONE PIPELINE, and KEEP is all a door may add: every caller answers the same
-- shape with the same headers, so a mount cannot tell two doors apart.
viewPage :: ServeOptions -> Hub -> Request -> (HeadlineRecord -> Bool)
         -> ([HeadlineRecord] -> [Pair]) -> IO Response
viewPage opts hub request keep extra = case pageParams request of
  Left why -> pure (jsonError status400 why)
  Right PageAsk {..} -> do
    st <- readTVarIO (hubStore hub)
    let tag = etagOf st
    if tag `elem` ifNoneMatch request
      then pure (responseLBS status304 (cacheHeaders tag) "")
      else do
        let qr      = storeResult st
            -- `ref:' reads the link graph, so the match runs over the store's own rows.
            env     = storeEnv (qrRecords qr)
            asked   = filter (\r -> keep r && matchesFilter env paQuery r) (qrRecords qr)
            matched = if hiding then filter (not . archived) asked else asked
            -- The WHOLE store's tags answer first: the cheaper refusal.
            hiding  = archiveKey `elem` storeTags st && not (namesArchive paQuery)
            hidden  = length asked - length matched
            total   = length matched
            ordered = sortedForViewWith (storeKeywords st) paChain matched
            shown   = maybe matched (\n -> take n (drop paOffset ordered)) paLimit
            hasNext = maybe False (\n -> paOffset + n < total) paLimit
            cols    = maybe viewColumns resolveColumns paPicked
            -- EXTRA rides the ONE encoding, over every row the query MATCHED.
            view    = viewJSONFor cols (savedViewsIn st) paChain
                                  (viewTitleFor dir) (storeKeywords st) shown
            body    = TLE.encodeUtf8 (encodeToLazyText (merged view (extra matched)))
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

-- | VIEW with MORE members: a door that answers more than the table does adds
-- them here, so there is one encoding and one shape to read.
merged :: Value -> [Pair] -> Value
merged (Object o) more = Object (o <> KM.fromList more)
merged v _more = v

-- | The largest page one request may ask for; over it is a 400 rather than a silent trim.
limitCap :: Int
limitCap = 20000

-- | ST as an entity tag.  The generation restarts at zero each process, so the fingerprint is what survives one.
etagOf :: Store -> BSC.ByteString
etagOf st = "\"" <> TE.encodeUtf8 (T.take 16 (stPrint st))
              <> "-g" <> BSC.pack (show (stGen st)) <> "\""

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
  }

-- | @q@, @limit@ and @offset@ out of REQUEST, or what is wrong with one.  @order=@ is refused rather than ignored.
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

-- | NAME's value RAW as a whole number.  Read as 'Integer' first: a wrapped 'Int' would page from a negative offset.
wholeNumber :: Text -> BS.ByteString -> Either Text Int
wholeNumber name raw = do
  t <- first (const (name <> " is not UTF-8")) (TE.decodeUtf8' raw)
  case TR.decimal t :: Either String (Integer, Text) of
    Right (n, rest) | T.null rest, n >= 0, n <= toInteger (maxBound :: Int)
                        -> Right (fromInteger n)
    _notANumber         -> Left (name <> " must be a whole number, 0 or more")

-- Materialize



-- | @GET \/headline?id=…@: a subtree from the store.  The id rides the query string; a @#@ in a path opens a fragment.
materialize :: Hub -> Maybe Text -> Either Text (Maybe Int) -> IO Response
materialize _hub Nothing _child = pure (jsonError status400 "GET /headline?id=<row id>")
materialize hub (Just rid) child = do
  st <- readTVarIO (hubStore hub)
  pure $ either id (jsonResponse status200 . subtreeJSON) (focusIn st rid child)

data Focus = Focus
  { fcRow     :: !HeadlineRecord   -- ^ the row the id named.
  , fcEntries :: ![SubtreeEntry]   -- ^ every headline inside it, in document order.
  , fcAt      :: !(Maybe Int)      -- ^ the @child@ index; 'Nothing' is the row itself.
  }

-- | ST's answer to @?id=RID&child=K@.  Read and write share it, so a commit cannot address what a materialize would refuse.
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

focusEntry :: Focus -> Maybe SubtreeEntry
focusEntry f = fcAt f >>= subtreeEntryAt (fcEntries f)

focusHere :: Focus -> HeadlineRecord
focusHere f = maybe (fcRow f) seRecord (focusEntry f)

-- | Which entry E hangs under.  The outline walk spells @-1@ for the row, and this is the one place that reading is made.
parentOf :: SubtreeEntry -> Maybe Int
parentOf e = if seParent e < 0 then Nothing else Just (seParent e)

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
    -- The ROW's whole scan, in FILE coordinates: a second request opened an async gap every fill had to bridge.
  , "links"      .= map linkJSON (subtreeLinks (fcRow f))
  , "titleAt"    .= (spanStart <$> titleSpan here)
  ]
  where here  = focusHere f
        parts = headlineParts here

levelOf :: Focus -> Int
levelOf = maybe 1 seLevel . focusEntry

cells :: HeadlineRecord -> [Pair]
cells r = [ Key.fromText k .= f r | (k, f) <- docCells ]

childJSON :: Int -> SubtreeEntry -> Value
childJSON i e = object ([ "index" .= i, "level" .= seLevel e
                        , "span" .= extentJSON (seRecord e) ]
                        <> cells (seRecord e))

extentJSON :: HeadlineRecord -> Value
extentJSON r = object [ "start" .= spanStart (hrSubtree r), "end" .= spanEnd (hrSubtree r) ]

under :: Focus -> [(Int, SubtreeEntry)]
under f = [ (i, e) | (i, e) <- zip [0 ..] (fcEntries f), seParent e == mine ]
  where mine = fromMaybe (-1) (fcAt f)

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
  st <- readTVarIO (hubStore hub)
  case focusIn st rid child >>= \f ->
         (,) (focusHere f) <$> prepare raw (focusHere f) of
    Left refusal -> pure refusal
    Right (here, (digest, org)) ->
      answerWrite rewritten (\fresh -> ["digest" .= fresh])
        <$> writeSpans (walkFor opts) hub (hrFile here) digest
                       [(hrSubtree here, org)]

prepare :: BL.ByteString -> HeadlineRecord -> Either Response (Text, Text)
prepare raw r = case parseCommit raw of
  Left why -> Left (jsonError status400 why)
  Right (asked, digest)
    | digest /= hrDigest r  -> Left (conflict "stale" (hrDigest r) reparsed)
    | Just key <- badPlanning asked -> Left (jsonResponse status409
        [ "error" .= unreadable key, "reason" .= ("planning" :: Text), "field" .= key ])
    | otherwise             -> Right (digest, committed r asked)

-- | The subtree ASKED for, over R.  'untrailed' EITHER WAY: the raw shape is a whole document the client hands back.
committed :: HeadlineRecord -> Commitment -> Text
committed _r (WholeSubtree org)         = untrailed org
committed r  (SplitSubtree body ps pln) =
  recomposedSubtree r (HeadlineParts body ps pln "")

-- | The planning entry no timestamp parser reads back.  Letting one through is silent: the line stops being a planning line.
badPlanning :: Commitment -> Maybe Text
badPlanning (WholeSubtree _org) = Nothing
badPlanning (SplitSubtree _body _ps pln) =
  listToMaybe ([ key | (key, _v) <- pln, key `notElem` planningKeywords ]
                 <> [ key | (key, value) <- pln, not (readsAsTimestamp value) ])



-- Keywords

-- | @GET \/keywords?ids=A,B@: the states those rows may be set to, laid out as the chain that classifies them (AGENTS.hs).
keywordsView :: Hub -> Request -> IO Response
keywordsView hub request =
  idsView hub request "GET /keywords?ids=<row id>,<row id>" $ \st _rows found unknown ->
    [ "sources" .= map sourceJSON (keywordSources (stConfig st) found)
    , "unknown" .= unknown
    ]
  where sourceJSON (source, kw) = object ("source" .= source : keywordsPair kw)

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

withRow :: Hub -> Text -> (HeadlineRecord -> [Pair]) -> IO Response
withRow hub rid fields = do
  st <- readTVarIO (hubStore hub)
  pure $ maybe (jsonError status404 (noSuchRow rid))
               (jsonResponse status200 . fields)
               (rowIn (storeRecords st) rid)

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

-- | ROWS per key, a row counted ONCE however often it names one.  The count
-- @\/tags@ answers with, and the picker's two vocabularies ride it too.
{-# INLINE countedBy #-}
countedBy :: Ord k => (HeadlineRecord -> [k]) -> [HeadlineRecord] -> Map k Int
countedBy keys rows =
  Map.fromListWith (+) [ (k, 1 :: Int) | r <- rows, k <- nub (keys r) ]

-- Capture

-- | @GET \/capture[?tag=NAME]@: what a capture under that tag will ask for.  @tags@ rides here because a capture names no rows.
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
          , "codes"    .= codeList
          ])

-- Links

-- | @GET \/links?id=ROW@: where that row points.  @span@ is the FILE range @edit-link@ takes back, under @digest@ as the lock.
linksView :: Hub -> Maybe Text -> IO Response
linksView _hub Nothing = pure (jsonError status400 "GET /links?id=<row id>")
linksView hub (Just rid) = withRow hub rid $ \r ->
  [ "digest" .= hrDigest r
  , "links" .= map linkJSON (subtreeLinks r) ]

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
      , Right text <- [TE.decodeUtf8' raw]
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
            -- WHERE A TAG LAYER GOES that has no file yet.  Served rather than
            -- composed on the page: the path rule is this server's, and
            -- 'mintableLayer' is the same rule again on the way back in.
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
  -- A TAG LAYER IS MINTED BY BEING WRITTEN TO.  Only under the FIRST config
  -- dir's own `tags/', so a write still cannot name a path this tree does not own.
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
      -- ^ the @#+TODO:@ block, one entry per line; ABSENT leaves it standing
      -- (the optional parts' own rule — a pin writes the filter alone),
      -- and the EMPTY list is still the deletion.
  , lwParts  :: !ConfigParts   -- ^ the three optional parts riding in the same write.
  , lwDigest :: !Text          -- ^ the pin, empty for a layer that is not there yet.
  }

queryId :: Request -> Maybe Text
queryId = queryText "id"

queryText :: BS.ByteString -> Request -> Maybe Text
queryText name request = case lookup name (queryString request) of
  Just (Just raw) -> either (const Nothing) Just (TE.decodeUtf8' raw)
  _absent         -> Nothing

-- | The @child@ parameter.  A non-number is a 400: a write pinned to a mistyped index would splice the wrong subtree.
queryChild :: Request -> Either Text (Maybe Int)
queryChild request = case lookup "child" (queryString request) of
  Just (Just raw) -> first (const refusal) (Just <$> wholeNumber "child" raw)
  _absent         -> Right Nothing
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

-- | PATH as a source, where there is a file there at all.
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
