-- | The query facade: load org files into rows, render them as table-view
-- JSON, and write one headline's raw subtree back.  The parser and its AST
-- stay in a private sublibrary; the wire is hand-built 'Value's (AGENTS.hs).
module Glance.Query ( BlobSeed (..)
                    , ConfigLayerFile (..)
                    , ConfigLayers (..)
                    , ConfigParts (..)
                    , ConfigSetting (..)
                    , SettingScope (..)
                    , configSettings
                    , TreeSettings (..)
                    , noTreeSettings
                    , treeSettings
                    , HeadlineParts (..)
                    , HeadlineRecord (..)
                    , IdCollision (..)
                    , LinkShape (..)
                    , LoadFailure (..)
                    , OrgLink (..)
                    , QueryResult (..)
                    , Span (..)
                    , SortChain
                    , SubtreeEntry (..)
                    , TodoKeywords (..)
                    , WalkOptions (..)
                    , WriteFailure (..)
                    , activeMeta
                    , Meta (..)
                    , metaWord
                    , metas
                    , starred
                    , addTagEdits
                    , archiveEdits
                    , archiveTag
                    , archived
                    , bareTemplate
                    , blobDocument
                    , blobPathIn
                    , builtinFilter
                    , captureCodes
                    , captureEdits
                    , captureStamp
                    , captureText
                    , captureTargetIn
                    , captureTemplateEdits
                    , captureTemplateIn
                    , captureTemplateOf
                    , cellSep
                    , configDirsIn
                    , configEdits
                    , configPath
                    , configPaths
                    , currentDocument
                    , dayOf
                    , dayNamed
                    , defaultCaptureFile
                    , SavedView (..)
                    , defaultFilter
                    , savedView
                    , savedViews
                    , viewOf
                    , viewQuery
                    , viewQueryIn
                    , defaultWalk
                    , derivedPath
                    , digestOfText
                    , displayText
                    , documentPath
                    , editLinkEdits
                    , englishDay
                    , englishSpan
                    , monthWords
                    , expandTemplate
                    , filterKeys
                    , fingerprint
                    , firstBy
                    , groupOn
                    , followableTypes
                    , materialTypes
                    , headlineParts
                    , hiddenProperties
                    , inactiveMeta
                    , isoDay
                    , keywordSources
                    , linkColumns
                    , linkShown
                    , linkType
                    , loadDir
                    , loadDirFilesSerially
                    , loadDirFilesWith
                    , loadDirWith
                    , loadDirWithConfig
                    , loadFile
                    , loadFileWith
                    , matchesSearch
                    , mergeKeywords
                    , mintBlobId
                    , mintableLayer
                    , noConfig
                    , noKeywords
                    , noParts
                    , orgLinks
                    , planningKeywords
                    , planningTimestamp
                    , priorityLetter
                    , priorityText
                    , readConfigLayers
                    , readsAsTimestamp
                    , recognizedKeywords
                    , untrailed
                    , recomposedSubtree
                    , Ref (..)
                    , RefVia (..)
                    , carriesKind
                    , idPropertyOf
                    , kindCut
                    , kindSlug
                    , namesRow
                    , pointedAtBy
                    , pointsAt
                    , refNames
                    , refSpellings
                    , refTargetOf
                    , refTargets
                    , refsCarrying
                    , removeTagEdits
                    , renameTagEdits
                    , replaceSpans
                    , resolveIds
                    , rowIdIn
                    , rowJSON
                    , rowProperties
                    , setPlanningEdits
                    , setPriorityEdits
                    , setStateEdits
                    , Repeat (..)
                    , repeatOn
                    , rowOrgId
                    , Completion (..)
                    , noteCompletion
                    , shiftRepeat
                    , setTitleEdits
                    , settableKeywords
                    , settableStates
                    , shiftDay
                    , shiftIn
                    , shiftUnits
                    , Sign (..)
                    , signOf
                    , ownBodyLines
                    , subtreeEntries
                    , subtreeEntryAt
                    , titleSpan
                    , titleText
                    , defaultSortChain
                    , sortedForView
                    , sortedForViewWith
                    , sortedTagsCell
                    , storeRootIn
                    , trashBlob
                    , trashDirIn
                    , trashPathFor
                    , subtreeLinks
                    , subtreeText
                    , systemSetting
                    , tagColumns
                    , tagRunEntries
                    , tagText
                    , tagged
                    , stateColorsOf
                    , prioritySlots
                    , stateSlots
                    , tagsOfCell
                    , templatePrompts
                    , todoLines
                    , todoPragmas
                    , uuidFrom
                    , viewJSON
                    , resolveColumns
                    , viewColumns
                    , viewJSONFor
                    , viewJSONTextFor
                    ) where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isLetter, isSpace)
import Data.Either (fromRight)
import Data.List (foldl', nub, partition, sort, sortBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import TextShow (showt)

import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import qualified Data.Time as Time

import Data.Org ( Context, Element (EHeadline), Headline
                , HeadlineSpans ( hsClosed, hsDeadline, hsPriority, hsProperties
                                , hsSchedule, hsStars, hsTags, hsTitle, hsTodo )
                , Priority (Priority), Span (..), Spanned (valueOf)
                , Timestamp (tsInterval, tsStart)
                , TimestampRepeaterInterval (repeaterType, repeaterUnit, repeaterValue)
                , TimestampRepeaterType (CatchUp, Cumulative, Restart)
                , TimestampStatus (TimestampActive, TimestampInactive)
                , Todo (name)
                , TsMoment (tsmHasTime, tsmTime), archiveTag, deadline, defaultContext
                , firstHeadlineOf, headlineIdProperty, headlinesOf, hsFull, identity
                , isKeywordChar, isTagChar, levelOf
                , metaCategory
                , orgIdentity, orgParse, priority, schedule, shiftSpan, sliceSpan, spans, spelled
                , addUnit, relativeForms, repeaterFormat, tags, title, todo
                , tsBrackets, unitChar, unitOf )
import Data.Org.Config ( ConfigLayerFile (..), ConfigLayers (..), TodoKeywords (..)
                       , builtinFilter, captureTargetIn
                       , classify, configDirsIn, configPaths
                       , declaredKeywords
                       , SavedView (..), defaultCaptureFile, defaultFilter
                       , isTodoPragma, savedView, savedViews, stateColorsEdits
                       , stateColorsOf
                       , TreeSettings (..), noTreeSettings, treeSettings
                       , viewEdits, viewOf
                       , viewQuery, viewQueryIn
                       , fingerprint, firstBy, groupOn, keywordScopes
                       , loadConfigDirs, mergeKeywords, mintableLayer, noConfig, noKeywords
                       , readConfigLayers, recognizedKeywords, seedContext
                       , systemSetting, todoLineEdits, todoLines, todoPragmas )
import Data.Org.External (Completion (..), noteCompletion)
import Data.Org.Blob (blobPathIn, mintBlobId, storeRootIn, uuidFrom)
import Data.Org.Trash (trashBlob, trashDirIn, trashPathFor)
import Data.Org.Walk ( Found (..), LoadFailure (..), WalkOptions (..), claimById
                     , defaultWalk, findOrgFilesWith, isConfig, isDerived, isDocument
                     , mapFilesConcurrently )

import Data.Org.Edit (digestOfText, eolOf, lineSpansIn, linesWith, openingFor)

import qualified Data.Org.Edit as Edit
import qualified Data.Org.External as External


data HeadlineRecord = HeadlineRecord
  { hrFile      :: !FilePath        -- ^ path the headline was read from, as walked.
  , hrId        :: !Text            -- ^ row identity; see 'rowId'.
  , hrCategory  :: !Text            -- ^ the file's final @#+CATEGORY@, empty when unset.
  , hrHeadline  :: !Headline        -- ^ the parsed headline; its type stays private.
  , hrKeywords  :: !TodoKeywords    -- ^ every keyword the file's parse recognized — config seed included; one value shared per file.
  , hrDeclared  :: !TodoKeywords    -- ^ what the file's OWN @#+TODO:@ lines declare; the nearest scope, see 'keywordSources'.
  , hrDoc       :: !Text            -- ^ the file's text as parsed; shared with 'hrHeadline', not copied.
  , hrDigest    :: !Text            -- ^ SHA-256 of that text's bytes, lowercase hex; one value shared per file.
  , hrSubtree   :: !Span            -- ^ the headline's outline extent in 'hrDoc'; see 'subtreeSpans'.
  , hrState     :: !(Maybe Text)    -- ^ TODO keyword verbatim.
  , hrPriority  :: !(Maybe Text)    -- ^ priority letter, brackets dropped.
  , hrTitle     :: !Text            -- ^ title text as the file spells it.
  , hrTags      :: !Text            -- ^ @":a:b:"@ in FILE order, empty when untagged; the COLUMN sorts ('sortedTagsCell').
  , hrScheduled :: !(Maybe Text)    -- ^ ISO date, see 'isoStamp'.
  , hrDeadline  :: !(Maybe Text)    -- ^ ISO date, see 'isoStamp'.
  , hrSearch    :: !Text            -- ^ the cells as they display, lowercased; see 'searchTextOf'.
  , hrLinks     :: ![Ref]           -- ^ the references this subtree makes, normalized; see 'refTargets'.
  , hrLinked    :: !Bool            -- ^ does the subtree hold a link at all — what @o@ follows; see 'subtreeLinks'.
  , hrActive    :: !(Maybe Bool)    -- ^ whether 'hrState' is an active state HERE; see 'Data.Org.Config.classify'.
  } deriving (Show)

data QueryResult = QueryResult
  { qrRecords        :: ![HeadlineRecord]  -- ^ rows in walk order, one per id; paths sorted, headlines in file order.
  , qrFiles          :: !Int               -- ^ .org files visited.
  , qrParseFailures  :: !Int               -- ^ files 'orgParse' rejected; they contribute no rows.
  , qrDecodeFailures :: !Int               -- ^ files that are not valid UTF-8.
  , qrReadFailures   :: !Int               -- ^ files that could not be read, plus unlistable directories.
  , qrIdCollisions   :: ![IdCollision]     -- ^ rows 'resolveIds' dropped, and what they lost to.
  } deriving (Show)

data IdCollision = IdCollision
  { icId      :: !Text      -- ^ the id both files claim.
  , icKept    :: !FilePath  -- ^ the file whose row the view carries.
  , icDropped :: !FilePath  -- ^ the file whose row it does not.
  } deriving (Eq, Show)

emptyResult :: QueryResult
emptyResult = QueryResult [] 0 0 0 0 []


loadDir :: FilePath -> IO QueryResult
loadDir = loadDirWith defaultWalk

loadDirWith :: WalkOptions -> FilePath -> IO QueryResult
loadDirWith opts dir = do
  (files, dirErrs) <- loadDirFilesWith opts dir
  pure (summarise dirErrs files)

loadDirFilesWith :: WalkOptions -> FilePath
                 -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesWith opts dir = withoutConfig <$> loadDirWithConfig opts dir

loadDirWithConfig :: WalkOptions -> FilePath
                  -> IO (ConfigLayers, [(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirWithConfig = loadDirFilesUsing mapFilesConcurrently

loadDirFilesSerially :: WalkOptions -> FilePath
                     -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesSerially opts dir = withoutConfig <$> loadDirFilesUsing mapM opts dir

withoutConfig :: (ConfigLayers, [a], Int) -> ([a], Int)
withoutConfig (_cfg, files, dirErrs) = (files, dirErrs)

loadDirFilesUsing :: ((FilePath -> IO (Either LoadFailure [HeadlineRecord]))
                      -> [FilePath] -> IO [Either LoadFailure [HeadlineRecord]])
                  -> WalkOptions -> FilePath
                  -> IO (ConfigLayers, [(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesUsing over opts dir = do
  found <- findOrgFilesWith opts [dir]
  cfg <- loadConfigDirs (sort (foundConfig found))
  let paths = sort (foundFiles found)
  outcomes <- over (loadFileWith cfg) paths
  pure (cfg, zip paths outcomes, length (foundDirErrs found))

loadFile :: FilePath -> IO (Either LoadFailure [HeadlineRecord])
loadFile = loadFileWith noConfig

-- | PATH's top entries under CFG, or why it has none.  The rows come back
-- FORCED, so the document is dropped rather than retained (AGENTS.hs).
loadFileWith :: ConfigLayers -> FilePath -> IO (Either LoadFailure [HeadlineRecord])
loadFileWith cfg path = do
  parsed <- Edit.readParsed (seedContext cfg) path
  evaluate $ case parsed of
    Left (fault, _why) -> Left fault
    Right pd -> forcing rs (Right rs)
      where rs = recordsOf cfg path (Edit.pdText pd) (Edit.pdDigest pd)
                           (Edit.pdContext pd) (Edit.pdElements pd)

summarise :: Int -> [(FilePath, Either LoadFailure [HeadlineRecord])] -> QueryResult
summarise dirErrs files =
  (foldl' count (emptyResult { qrReadFailures = dirErrs }) files)
    { qrRecords = forcing rows rows, qrIdCollisions = clashes }
  where
    (rows, clashes) = resolveIds (concatMap (fromRight [] . snd) files)
    count acc (_path, outcome) = case outcome of
      Left ReadFailed   -> seen { qrReadFailures = qrReadFailures seen + 1 }
      Left DecodeFailed -> seen { qrDecodeFailures = qrDecodeFailures seen + 1 }
      Left ParseFailed  -> seen { qrParseFailures = qrParseFailures seen + 1 }
      Right _rs         -> seen
      where seen = acc { qrFiles = qrFiles acc + 1 }

-- | The rows FILE contributes.  The extents are computed over the WHOLE
-- sequence and the filters applied after: filtering first would end a row
-- at the next KEPT headline.
recordsOf :: ConfigLayers -> FilePath -> Text -> Text -> Context -> [Spanned Element]
          -> [HeadlineRecord]
recordsOf cfg path doc digest ctx elems =
  [ recordOf cfg declared path ordinal doc digest category keywords h subtree
  | (ordinal, (h, subtree)) <- zip [0 ..] entries ]
  where category = detach (metaCategory ctx)
        -- Off the config CHAIN rather than CTX's sets: the org files' own order.
        keywords = forcedKeywords (recognizedKeywords cfg declared)
        -- Forced once per file: it is STORED, and an unforced set thunks over ELEMS.
        declared = forcedKeywords (declaredKeywords elems)
        -- BOTH filters run before the numbering, so a dropped entry spends no ordinal.
        entries  = [ e | e@(h, _sub) <- outlineEntries doc elems
                       , topLevel h, not (blankEntry h) ]

outlineEntries :: Text -> [Spanned Element] -> [(Headline, Span)]
outlineEntries doc elems = zip heads (subtreeSpans (T.length doc) heads)
  where heads = headlinesOf elems

topLevel :: Headline -> Bool
topLevel h = levelOf h == 1

-- | Has H nothing the table can show?  The RECORD's rule at the HEADLINE's
-- layer: each span is 'Nothing' exactly where 'recordOf' cuts an empty cell.
blankEntry :: Headline -> Bool
blankEntry h = all isNothing [ hsTodo sp
                             , hsPriority sp
                             , hsTitle sp
                             , hsTags sp
                             , hsSchedule sp
                             , hsDeadline sp ]
  where sp = spans h

recordOf :: ConfigLayers -> TodoKeywords -> FilePath -> Int -> Text -> Text -> Text
         -> TodoKeywords -> Headline -> Span -> HeadlineRecord
recordOf cfg declared path ordinal doc digest category keywords h subtree =
  forceRecord (row { hrSearch = searchTextOf (viewCells row) })
  where
        -- The haystack is the view's columns by construction: 'viewCells' reads ROW.
        row = HeadlineRecord
          { hrFile      = path
          , hrId        = rowId path ordinal h
          , hrCategory  = category
          , hrHeadline  = h
          , hrKeywords  = keywords
          , hrDeclared  = declared
          , hrDoc       = doc
          , hrDigest    = digest
          , hrSubtree   = subtree
          , hrState     = state
          , hrPriority  = pri
          , hrTitle     = titleCell
          , hrTags      = tagsCell
          , hrScheduled = scheduled
          , hrDeadline  = due
          , hrSearch    = ""
          , hrLinks     = refTargetsOf links
          , hrLinked    = not (null links)
          , hrActive    = classify cfg declared (tagsOfCell tagsCell) <$> state
          }
        sp = spans h
        links = orgLinks (sliceSpan doc subtree)
        cut mspan render = detach (maybe render (sliceSpan doc) mspan)
        state     = detach . name <$> todo h
        pri       = (\(Priority c) -> priorityCell (T.singleton c)) <$> priority h
        titleCell = cut (hsTitle sp) (showt (title h))
        tagsCell  = cut (hsTags sp) (showt (tags h))
        scheduled = isoStamp <$> schedule h
        due       = isoStamp <$> deadline h


cellSep :: Char
cellSep = '\US'

-- | CELLS as one lowercase haystack — @table-view.js@'s own row text, to the byte.
searchTextOf :: [Text] -> Text
searchTextOf = detach . T.toLower . T.intercalate (T.singleton cellSep) . map displayText

displayText :: Text -> Text
displayText = squashControls . showLinks

showLinks :: Text -> Text
showLinks s | not ("[[" `T.isInfixOf` s) = s   -- the common cell, scanned once
            | otherwise                  = T.concat (map (either snd linkShown) (linkParts s))

data LinkShape = Bare | Bracketed !(Maybe Text)
  deriving (Eq, Show)

data OrgLink = OrgLink
  { olTarget :: !Text       -- ^ where it points, as the source spells it.
  , olShape  :: !LinkShape  -- ^ how the source spells it.
  , olSpan   :: !Span       -- ^ its extent in the text scanned.
  } deriving (Eq, Show)

linkShown :: OrgLink -> Text
linkShown l = case olShape l of
  Bracketed (Just desc) | not (T.null desc) -> desc
  _itsTarget                                -> olTarget l

linkParts :: Text -> [Either (Int, Text) OrgLink]
linkParts = go 0
  where
    go at rest
      | T.null after = [Left (at, before)]
      | otherwise    = Left (at, before) : case linkAt after of
          Just (target, desc, width) ->
            Right (OrgLink target (Bracketed desc) (Span opens (opens + width)))
              : go (opens + width) (T.drop width after)
          Nothing -> Left (opens, "[[") : go (opens + 2) (T.drop 2 after)
      where (before, after) = T.breakOn "[[" rest
            opens           = at + T.length before

linkAt :: Text -> Maybe (Text, Maybe Text, Int)
linkAt text
  | T.null target || T.null rest = Nothing
  | otherwise = case T.uncons (T.drop 1 rest) of
      Just (']', _more) -> Just (target, Nothing, 4 + T.length target)  -- [[TARGET]]
      Just ('[', more) | "]]" `T.isPrefixOf` after'                     -- [[TARGET][DESC]]
                       -> Just (target, Just desc, 6 + T.length target + T.length desc)
        where (desc, after') = T.break (== ']') more
      _notALink        -> Nothing
  where (target, rest) = T.break (== ']') (T.drop 2 text)


-- | Every link R's subtree points at, spanned in the DOCUMENT ('Data.Org.Edit').
subtreeLinks :: HeadlineRecord -> [OrgLink]
subtreeLinks r = map (shiftLink (spanStart (hrSubtree r))) (orgLinks (subtreeText r))

shiftLink :: Int -> OrgLink -> OrgLink
shiftLink by l = l { olSpan = shiftSpan by (olSpan l) }

-- | The links TEXT holds, one per (target, shown) PAIR — the key a reader can see.
orgLinks :: Text -> [OrgLink]
orgLinks = firstBy (\l -> (olTarget l, linkShown l))
         . concatMap (either (uncurry plainLinks) pure) . linkParts

linkSchemes :: [Text]
linkSchemes = ["https://", "http://", "mailto:"]

plainLinks :: Int -> Text -> [OrgLink]
plainLinks at s =
  [ OrgLink url Bare (Span from (from + T.length url))
  | (start, word) <- spacedWords s
  , Just (opens, url) <- [urlIn word]
  , let from = at + start + opens ]

spacedWords :: Text -> [(Int, Text)]
spacedWords = go 0
  where
    go at text
      | T.null word = []
      | otherwise   = (opens, word) : go (opens + T.length word) rest
      where (spaces, body) = T.span isSpace text
            (word, rest)   = T.break isSpace body
            opens          = at + T.length spaces

urlIn :: Text -> Maybe (Int, Text)
urlIn word
  | not (T.any (== ':') word) = Nothing
  | otherwise = case mapMaybe opensAt linkSchemes of
      []  -> Nothing
      ats -> let at  = minimum ats
                 url = T.dropWhileEnd trailing (T.drop at word)
             in if T.null url then Nothing else Just (at, url)
  where
    opensAt scheme
      | T.null after               = Nothing
      | T.null before              = Just 0
      | isAlphaNum (T.last before) = Nothing
      | otherwise                  = Just (T.length before)
      where (before, after) = T.breakOn scheme word
    trailing c = c `elem` (".,;:!?'\"()[]{}<>" :: String)

-- | What KIND of place a target names.  An unheard-of scheme keeps its own name.
linkType :: Text -> Text
linkType target
  | T.null rest                           = "other"
  | not (schemeShaped word)               = "other"
  | "org-glance-" `T.isPrefixOf` word     = "glance"
  | otherwise                             = word
  where
    (before, rest) = T.breakOn ":" target
    word           = T.toLower before
    schemeShaped t = case T.uncons t of
      Nothing      -> False
      Just (c, cs) -> isAsciiLower c && T.all part cs
    part c = isAsciiLower c || isDigit c || c == '+' || c == '-' || c == '.'

followableTypes :: [Text]
followableTypes = ["https", "http"]

-- | The SCHEMES whose target is a HEADLINE ID: following one opens the
--   material doc.  The colon form is org's syntax; 'linkType' folds the whole
--   family to "glance", which is what a row's wire type carries -- so the
--   wire-facing 'materialTypes' derives to that one word.
materialSchemes :: [Text]
materialSchemes = ["glance", "org-glance-material", "org-glance-visit", "org-glance-open"]

materialTypes :: [Text]
materialTypes = nub (map (linkType . (<> ":")) materialSchemes)

linkTypes :: [Text]
linkTypes = followableTypes <> materialTypes <> ["mailto", "id", "file"]

linkTypeBadges :: [Value]
linkTypeBadges =
  zipWith (badge Nothing) (followable <> unreachable) linkTypes
  where followable  = map (stateSlot "a") [0 .. length followableTypes - 1]
        unreachable = map (stateSlot "i") [0 ..]

linkColumns :: [Value]
linkColumns =
  [ column "type"  "Type"     "badge" ["badges" .= linkTypeBadges]
  , column "title" "Title"    "text"  []
  , column "url"   "Target"   "text"  []
  ]

tagColumns :: [Value]
tagColumns =
  [ column "title" "Tag"  "text"   []
  , column "on"    "On"   "text"   []
  , column "rows"  "Rows" "number" []
  ]


-- | The protocols naming a row, EACH BOUND TO ITS NAMESPACE, so a prefix
--   cannot land without declaring where it resolves.
--   @org-glance-overview:@/@-state:@ name a tag and a keyword.
refPrefixes :: [(Text, RefVia)]
refPrefixes = [ (s <> ":", ViaRow) | s <- materialSchemes ]
           <> [("id:", ViaOrgId)]

-- | The NAMESPACE a reference resolves in.  @id:@ is org-id's protocol and
--   names the @:ID:@ PROPERTY; everything else names the row itself.
--   Resolving @id:@ over @ORG_GLANCE_ID@ would conflict with org-mode.
data RefVia = ViaRow | ViaOrgId
  deriving (Eq, Ord, Show)

-- | A reference AS RESOLVED: the row it names, and the KIND its author declared
-- on the edge.  The kind is the EDGE's, never the row's — which is why it rides
-- here rather than on 'HeadlineRecord' — and 'Nothing' is a plain mention.
data Ref = Ref
  { refTarget :: !Text          -- ^ the id or title the link names, normalized.
  , refKind   :: !(Maybe Text)  -- ^ @?kind=SLUG@, as the author spelled it.
  , refVia    :: !RefVia        -- ^ the namespace the target lives in.
  } deriving (Eq, Ord, Show)

refTargets :: Text -> [Ref]
refTargets = refTargetsOf . orgLinks

-- | DEDUP IS ON THE PAIR, which is the peer's own rule: two typed edges to one
-- row are two references, where two plain mentions are one.
refTargetsOf :: [OrgLink] -> [Ref]
refTargetsOf = nub . map detachRef . mapMaybe (refTargetOf . olTarget)

-- | The document is a slice of the file's text, so a reference kept out of it
-- retains the whole file; 'detach' copies, and the KIND must not reopen that.
detachRef :: Ref -> Ref
detachRef (Ref t k v) = Ref (detach t) (detach <$> k) v

refTargetOf :: Text -> Maybe Ref
refTargetOf target
    -- A KIND RIDES ON THE EDGE, not on the row: org-glance writes
    -- @?kind=SLUG@ after the id, so the id alone names the row and the kind is
    -- KEPT BESIDE IT.  A TITLE is text, so its own @?@ stays.  The NAMESPACE
    -- rides the prefix ('refPrefixes').
  | Just (via, rest) <- stripped = let (row, kind) = kindCut rest in plain row kind via
  | Just rest <- T.stripPrefix "*" target                       = plain rest Nothing ViaRow
  | T.any (\c -> c == ':' || c == '/') target                   = Nothing
  | otherwise                                                   = plain target Nothing ViaRow
  where
    stripped = listToMaybe [ (via, rest) | (p, via) <- refPrefixes
                                         , Just rest <- [T.stripPrefix p target] ]
    plain t k v = if T.null t then Nothing else Just (Ref t k v)

-- | A target cut into the ROW it names and the KIND its @?@ declares: the cut
-- at the FIRST @?@, the peer's own key read behind it.  ONE READING, TWO
-- CALLERS — 'refTargetOf' cuts a LINK's target with it, and the @ref:@\/@from:@
-- value reader cuts a TOKEN's value with it — so an edge and the token that
-- tests it can never spell a kind two ways.  WHERE THE CUT IS TAKEN is the
-- caller's: a link's target is the peer's URL and its @?@ always opens a query,
-- where a row id is no URL and the filter leaves an unproductive @?@ alone.
kindCut :: Text -> (Text, Maybe Text)
kindCut target = (row, kindIn (T.drop 1 query))
  where (row, query) = T.breakOn "?" target

-- | The @kind@ of a target's query string; an EMPTY one is no kind at all.
-- Only the peer's own key is read — anything else it may write is not a kind.
kindIn :: Text -> Maybe Text
kindIn query =
  listToMaybe [ slug | part <- T.splitOn "&" query
                     , Just v <- [T.stripPrefix "kind=" part]
                     , let slug = kindSlug v, not (T.null slug) ]

-- | A kind CANONICALIZED, and the rule is the PEER's: downcased, trimmed, runs
-- of whitespace folded to one @-@ (org-glance's @org-glance--kind-slug@,
-- @src\/data\/org-glance-utils.el:183-187@).  It slugs on encode AND on read —
-- its own "invariant 13" — so a hand-typed @Roasted By@ and a written
-- @roasted-by@ are ONE kind.  Reading them as two would fork the dedup rule and
-- count one vocabulary twice.
kindSlug :: Text -> Text
kindSlug = T.intercalate "-" . T.words . T.toLower

refSpellings :: HeadlineRecord -> [Text]
refSpellings r = maybe id (:) (identity (hrHeadline r)) [hrTitle r]

-- | R's @:ID:@ property — org-id's own, the one spelling a 'ViaOrgId'
--   reference matches.  Named after the PROPERTY: 'rowOrgId' nearby reads
--   @ORG_GLANCE_ID@, the other namespace.
idPropertyOf :: HeadlineRecord -> Maybe Text
idPropertyOf = orgIdentity . hrHeadline

-- | THE NAMES R ANSWERS TO, each bound to the namespace it answers in:
-- 'refSpellings' — @ORG_GLANCE_ID@ and the title, what @[[Title]]@ and
-- @[[*Title]]@ resolve against — answer 'ViaRow', and the @:ID:@ property alone
-- answers 'ViaOrgId', @id:@ being org-id's protocol.  ONE EQUATION FOR BOTH
-- DIRECTIONS: @ref:@ tests a link against it and @from:@ indexes rows by it, so
-- the two keys cannot drift into two namespace rules.
refNames :: HeadlineRecord -> [(RefVia, Text)]
refNames r = [ (ViaRow, s) | s <- refSpellings r ]
          <> [ (ViaOrgId, o) | Just o <- [idPropertyOf r] ]

-- | Does a reference name ROW?  The link's own namespace decides ('refNames').
-- PARTIALLY APPLIED AT COMPILE where the row is the one fixed end, which is
-- what keeps @ref:@ reading its target's names once and never per row.
namesRow :: HeadlineRecord -> Ref -> Bool
namesRow row = \l -> (refVia l, refTarget l) `elem` names
  where names = refNames row

-- | Does an edge carrying E answer a token asking for KIND?  'Nothing' IS THE
-- KIND-BLIND READING and every edge answers it — today's law, and what keeps a
-- bare @ref:@ byte for byte the test it was.  BOTH SIDES ARRIVE SLUGGED —
-- 'refTargetOf' slugs the edge's kind and 'kindIn' the token's — so this
-- compares canon to canon and slugs nothing twice.
carriesKind :: Maybe Text -> Maybe Text -> Bool
carriesKind Nothing  _ = True
carriesKind (Just k) e = e == Just k

-- | R's references narrowed to the ones carrying KIND.  THE KIND-BLIND ARM
-- TAKES THE LIST WHOLE, so a bare @ref:@ walks exactly the list it always did.
refsCarrying :: Maybe Text -> HeadlineRecord -> [Ref]
refsCarrying Nothing        = hrLinks
refsCarrying k@(Just _kind) = filter (carriesKind k . refKind) . hrLinks

-- | Does a row POINT AT T over an edge carrying KIND?  @ref:T@'s own test, and
-- an unresolvable T never reaches it: the caller answers that with no rows.  T
-- IS THE FIXED END, so its names are read once at compile and the rows run
-- against them; A ROW IS NEVER ITS OWN REFERENCE, which is what puts the
-- materialize footer's self-link outside every answer.
pointsAt :: Maybe Text -> HeadlineRecord -> HeadlineRecord -> Bool
pointsAt kind t = \r -> hrId r /= hrId t && any names (refsCarrying kind r)
  where names = namesRow t

-- | Is a row POINTED AT BY T over an edge carrying KIND?  @from:T@'s own test —
-- 'pointsAt' read from the other end, and THE SAME EDGE under both: T's own
-- references are taken once at compile and resolved against each row's names,
-- where 'pointsAt' fixes the names and walks each row's references.  The rows T
-- points at are @filter (pointedAtBy kind t) rows@, which is what a graph route
-- would ask for.
pointedAtBy :: Maybe Text -> HeadlineRecord -> HeadlineRecord -> Bool
pointedAtBy kind t = \r -> hrId r /= hrId t && any (namesRow r) out
  where out = refsCarrying kind t

squashControls :: Text -> Text
squashControls = T.concat . go
  where
    go s | T.null s    = []
         | T.null rest = [keep]
         | otherwise   = keep : " " : go (T.dropWhile control rest)
      where (keep, rest) = T.break control s
    control c = c < ' ' || c == '\DEL'

-- | CELL with its tags in case-folded order.  DISPLAY ONLY: 'hrTags' order
-- DECIDES which tag's config governs a row, so sorting it moves a resolution.
sortedTagsCell :: Text -> Text
sortedTagsCell cell
  | sorted == entries = cell
  | otherwise         = ":" <> T.intercalate ":" sorted <> ":"
  where entries = tagRunEntries cell
        sorted  = sortOn T.toCaseFold entries

-- | The entries of a tag RUN.  'tagEntries' keeps INTERIOR positions and is not this.
tagRunEntries :: Text -> [Text]
tagRunEntries = filter (not . T.null) . T.splitOn ":"

tagsOfCell :: Text -> [Text]
tagsOfCell = tagRunEntries . T.toLower . displayText

matchesSearch :: Text -> HeadlineRecord -> Bool
matchesSearch q
  | T.null needle = const True
  | otherwise     = T.isInfixOf needle . hrSearch
  where needle = T.toLower (T.strip q)


-- | RECORDS with one row per id ('claimById'), and the losers it reports.
resolveIds :: [HeadlineRecord] -> ([HeadlineRecord], [IdCollision])
resolveIds records = (kept, reverse clashes)
  where
    indexed = zip [0 :: Int ..] records
    (winners, clashes) = foldl' pick (Map.empty, []) indexed
    pick (best, out) (i, r) = case Map.lookup (hrId r) best of
      Nothing -> (taken, out)
      Just (_j, held) -> case claimById (hrFile r) held of
        (True, (win, lose))  -> (taken, collision win lose : out)
        (False, (win, lose)) -> (best, collision win lose : out)
      where taken     = Map.insert (hrId r) (i, hrFile r) best
            collision = IdCollision (hrId r)
    kept = [ r | (i, r) <- indexed, fmap fst (Map.lookup (hrId r) winners) == Just i ]

derivedPath :: FilePath -> Bool
derivedPath = isDerived

documentPath :: FilePath -> Bool
documentPath = isDocument

configPath :: FilePath -> Bool
configPath = isConfig

type SortChain = [(Text, Bool)]

-- | The default sort chain.  ONE list 'declaredSort' spells and 'sortedForViewWith' obeys.
defaultSortChain :: SortChain
defaultSortChain =
  [ ("state", True), ("title", True), ("deadline", True), ("scheduled", True) ]

-- | R's value for the column KEY: (palette POSITION, folded TEXT), built ONCE per sort.
sortCell :: TodoKeywords -> Text -> Maybe (HeadlineRecord -> Maybe (Int, Text))
sortCell palette key = read' <$> lookup key [(k, cell) | (k, _, _, cell) <- viewColumns]
  where
    ranked = paletteRank palette
    read' cell r = case cell r of
      Just value | not (T.null value) -> Just (rank value, text' value)
      _empty                          -> Nothing
    -- The priority cell wears org's brackets, so the comparator reads through them.
    rank value  = if key == "state" then ranked value else 0
    text' value | key == "state"    = ""
                | key == "priority" = priorityLetter value
                | otherwise         = T.toCaseFold value

paletteRank :: TodoKeywords -> Text -> Int
paletteRank (TodoKeywords actives inactives) =
  let ordered = actives <> filter (`notElem` actives) inactives
      places  = zip ordered [0 ..]
  in \value -> fromMaybe (length ordered) (lookup value places)

sortedForViewWith :: TodoKeywords -> SortChain -> [HeadlineRecord]
                  -> [HeadlineRecord]
sortedForViewWith _       []    = id
sortedForViewWith palette chain = sortBy (mconcat (mapMaybe key chain))
  where
    key (k, asc) = compareBy asc <$> sortCell palette k
    -- Nulls last, OUTSIDE the direction.  ONE EXTRACTION A SIDE: pairing
    -- `comparing (isNothing . value)' with `comparing value' ran `value' twice.
    compareBy asc value a b = case (value a, value b) of
      (Nothing, Nothing) -> EQ
      (Nothing, Just _)  -> GT
      (Just _,  Nothing) -> LT
      (Just x,  Just y)  -> if asc then compare x y else compare y x

-- | Over the palette RECORDS imply; pass the STORE's where a caller has one.
sortedForView :: [HeadlineRecord] -> [HeadlineRecord]
sortedForView records =
  sortedForViewWith (mergeKeywords (map hrKeywords records)) defaultSortChain records


subtreeText :: HeadlineRecord -> Text
subtreeText r = sliceSpan (hrDoc r) (hrSubtree r)

data SubtreeEntry = SubtreeEntry
  { seLevel  :: !Int             -- ^ org's outline level; the row's own is 1.
  , seParent :: !Int             -- ^ the index it hangs under, @-1@ being the row itself.
  , seRecord :: !HeadlineRecord  -- ^ the entry as a record: cells, extent, digest.
  } deriving (Show)

-- | R's descendants: one re-parse per call, from the load's own seed.
subtreeEntries :: ConfigLayers -> HeadlineRecord -> [SubtreeEntry]
subtreeEntries cfg r = case orgParse (seedContext cfg) doc of
  (_elems, _ctx, Just _err) -> []
  (elems, _ctx, Nothing)    -> parented (zip [0 ..] (inside elems))
  where
    doc   = hrDoc r
    outer = hrSubtree r
    inside elems =
      [ (levelOf h, h, sub)
      | (h, sub) <- outlineEntries doc elems
      , spanStart sub > spanStart outer, spanStart sub < spanEnd outer ]
    made k (_lvl, h, sub) =
      (recordOf cfg (hrDeclared r) (hrFile r) k doc (hrDigest r)
                (hrCategory r) (hrKeywords r) h sub)
        { hrId = detach (hrId r <> "/" <> T.pack (show k)) }
    -- Org permits a level jump, so the parent is the nearest SHALLOWER entry.
    parented = go []
      where
        go _open [] = []
        go open ((k, e@(lvl, _h, _sub)) : rest) =
          SubtreeEntry lvl parent (made k e) : go ((k, lvl) : still) rest
          where still  = dropWhile ((>= lvl) . snd) open
                parent = case still of
                  ((j, _l) : _rest) -> j
                  []                -> -1

subtreeEntryAt :: [SubtreeEntry] -> Int -> Maybe SubtreeEntry
subtreeEntryAt entries k
  | k < 0     = Nothing
  | otherwise = listToMaybe (drop k entries)

-- | How many lines of BODY are R's OWN.  Counted by DIFFERENCE rather than by
-- reading a leading @*@, which would cut the entry short at a @*bold*@ line.
ownBodyLines :: HeadlineRecord -> Text -> Maybe HeadlineRecord -> Int
ownBodyLines r body first' = case first' of
  Nothing     -> whole
  Just deeper -> whole - length (linesWith (T.drop (cut deeper) (subtreeText r)))
  where whole = length (linesWith body)
        cut deeper = spanStart (hrSubtree deeper) - spanStart (hrSubtree r)

-- Lens
--
-- ONE OWNER PER BYTE: the planning line, the headline's OWN drawer and its OWN
-- logbook are lifted; every byte left is the body's, a child's drawer included.

hiddenProperties :: [Text]
hiddenProperties = [headlineIdProperty, captureProperty]

hiddenProperty :: Text -> Bool
hiddenProperty key = T.toUpper (T.strip key) `elem` hiddenProperties

-- | PAIRS less the server's own.  ONE spelling of the filter: every reader that
-- shows a drawer, reads one back or completes from one drops the same keys.
shownPairs :: [(Text, Text)] -> [(Text, Text)]
shownPairs ps = [ p | p <- ps, not (hiddenProperty (fst p)) ]

planningKeywords :: [Text]
planningKeywords = ["SCHEDULED", "DEADLINE", "CLOSED"]

data HeadlineParts = HeadlineParts
  { hpBody       :: !Text            -- ^ the subtree with all three regions lifted out.
  , hpProperties :: ![(Text, Text)]  -- ^ the drawer's pairs in file order, 'hiddenProperties' dropped.
  , hpPlanning   :: ![(Text, Text)]  -- ^ the planning keywords present and each one's timestamp text, in line order.
  , hpLogbook    :: !Text            -- ^ the headline's own @:LOGBOOK:@ drawer verbatim; @""@ when it has none.
  } deriving (Eq, Show)

headlineParts :: HeadlineRecord -> HeadlineParts
headlineParts r = HeadlineParts
  { hpBody       = withoutSpans subtree (regionSpans [planAt, drawAt, logAt])
  , hpProperties = shownPairs (drawerPairs subtree drawAt)
  , hpPlanning   = [ (key, sliceSpan subtree sp) | (key, sp) <- entries ]
  , hpLogbook    = maybe "" (sliceSpan subtree) logAt
  }
  where (subtree, entries, planAt, drawAt, logAt) = regionsOf r

regionsOf :: HeadlineRecord
          -> (Text, [(Text, Span)], Maybe Span, Maybe Span, Maybe Span)
regionsOf r = (subtree, entries, planAt, drawAt, logAt)
  where subtree = subtreeText r
        entries = planningEntries r subtree
        planAt  = planningSlice entries subtree
        drawAt  = drawerSlice r subtree
        logAt   = logbookSlice drawAt subtree

recomposedSubtree :: HeadlineRecord -> HeadlineParts -> Text
recomposedSubtree r parts = untrailed (spliceRegions (hpBody parts) regions)
  where
    (subtree, entries, planAt, drawAt, logAt) = regionsOf r
    -- Body coordinates: the subtree line less the lines every region ahead of it
    -- took out.  Subtree indices leave a GAP where a region was cleared.
    cut = catMaybes [planAt, drawAt, logAt]
    lineOf sp = T.count "\n" (T.take (spanStart sp) subtree)
    height sp = length (linesWith (sliceSpan subtree sp))
    bodyLine fallback = maybe fallback (\sp -> lineOf sp - taken sp)
      where taken sp = sum [ height q | q <- cut, spanStart q < spanStart sp ]
    regions = [ Region at text | (at, text) <- [plan, props, logs], not (T.null text) ]
    plan  = ( bodyLine 1 planAt
            , planningText (planningStyle subtree (hpBody parts) entries planAt)
                           (hpPlanning parts) )
    props = ( bodyLine 1 drawAt
            , drawerText (drawerStyle subtree (hpBody parts) drawAt)
                         (shownPairs (hpProperties parts)) )
    logs  = ( bodyLine 0 logAt, maybe "" (sliceSpan subtree) logAt )

data Region = Region !Int !Text

spliceRegions :: Text -> [Region] -> Text
spliceRegions body regions = knit (go 0 (linesWith body) (sortOn above regions))
  where
    above (Region line _text) = line
    go _seen ls [] = ls
    go seen ls (Region at block : rest) =
      -- 'splitAt' clamps, so a region past the body's last line takes all of it.
      taken <> linesWith block <> go (seen + length taken) left rest
      where (taken, left) = splitAt (at - seen) ls

knit :: [Text] -> Text
knit ls = T.concat (zipWith close ls [1 :: Int ..])
  where n = length ls
        close l i | i == n || "\n" `T.isSuffixOf` l = l
                  | otherwise                       = l <> "\n"

withoutSpans :: Text -> [Span] -> Text
withoutSpans subtree sps =
  fromRight subtree (Edit.applyEdits subtree [ Edit.Edit sp "" | sp <- sps ])

regionSpans :: [Maybe Span] -> [Span]
regionSpans = sortOn spanStart . catMaybes


drawerSlice :: HeadlineRecord -> Text -> Maybe Span
drawerSlice r subtree = do
  sp <- hsProperties (spans (hrHeadline r))
  Span from to <- localSpan r subtree sp
  pure (Span from (pastLine subtree to))

planningSlice :: [(Text, Span)] -> Text -> Maybe Span
planningSlice entries subtree = case map snd entries of
  []  -> Nothing
  sps -> Just (Span (lineStart subtree (minimum (map spanStart sps)))
                    (pastLine subtree (maximum (map spanEnd sps))))

planningEntries :: HeadlineRecord -> Text -> [(Text, Span)]
planningEntries r subtree = sortOn (spanStart . snd)
  [ (key, local)
  | (key, sp) <- presentPlanning (headlineSpans r)
  , Just local <- [localSpan r subtree sp] ]

presentPlanning :: HeadlineSpans -> [(Text, Span)]
presentPlanning hs =
  [ (key, sp)
  | (key, Just sp) <- zip planningKeywords [hsSchedule hs, hsDeadline hs, hsClosed hs] ]

-- | Where R's OWN logbook sits, SKIP being the property drawer.  Located
-- TEXTUALLY, a @:LOGBOOK:@ being no part of a headline's parse.
logbookSlice :: Maybe Span -> Text -> Maybe Span
logbookSlice skip subtree = case break (opens . snd) own of
  (_before, (sp, _line) : rest) -> Just (Span (spanStart sp) (closes sp rest))
  _none                         -> Nothing
  where
    own = filter outside (takeWhile (not . child . snd) (drop 1 (lineSpansIn subtree)))
    outside (sp, _line) =
      maybe True (\s -> spanEnd sp <= spanStart s || spanStart sp >= spanEnd s) skip
    child line = "*" `T.isPrefixOf` line
    opens line = T.toUpper (T.strip line) == ":LOGBOOK:"
    ends  line = T.toUpper (T.strip line) == ":END:"
    -- An unterminated drawer owns every line it may own, as the parser would.
    closes sp rest = case break (ends . snd) rest of
      (_before, (e, _line) : _after) -> spanEnd e
      (before, [])                   -> foldl' max (spanEnd sp) (map (spanEnd . fst) before)

localSpan :: HeadlineRecord -> Text -> Span -> Maybe Span
localSpan r subtree sp
  | from < 0 || to > T.length subtree || from > to = Nothing
  | otherwise                                      = Just local
  where local@(Span from to) = shiftSpan (negate (spanStart (hrSubtree r))) sp


data PlanningStyle = PlanningStyle
  { psIndent :: !Text                    -- ^ what a written line is indented by.
  , psEol    :: !Text                    -- ^ what it ends with.
  , psRaw    :: ![((Text, Text), Text)]  -- ^ each entry already there, and its own text.
  }

planningStyle :: Text -> Text -> [(Text, Span)] -> Maybe Span -> PlanningStyle
planningStyle _subtree body _entries Nothing = PlanningStyle "" (eolOf body) []
planningStyle subtree _body entries (Just sp) = PlanningStyle (indentOf line) (eolOf line) raws
  where
    line = sliceSpan subtree sp
    raws = [ ((key, sliceSpan subtree at), raw)
           | (key, at) <- entries
           , Just raw <- [rawEntry key line (shifted at)] ]
    shifted = shiftSpan (negate (spanStart sp))

rawEntry :: Text -> Text -> Span -> Maybe Text
rawEntry key line at =
  (\from -> T.take (spanEnd at - from) (T.drop from line)) <$> entryOpening line key (spanStart at)

-- | Where the entry KEY opens in TEXT: the LAST @KEY:@ ahead of AT, only
-- horizontal space being allowed between one and its timestamp.  Line-bounded.
entryOpening :: Text -> Text -> Int -> Maybe Int
entryOpening text key at
  | T.null ahead = Nothing
  | otherwise    = Just (from + T.length ahead - T.length marker)
  where marker = key <> ":"
        from   = lineStart text at
        ahead  = fst (T.breakOnEnd marker (sliceSpan text (Span from at)))

planningText :: PlanningStyle -> [(Text, Text)] -> Text
planningText style want
  | null entries = ""
  | otherwise    = psIndent style <> T.unwords (map spell entries) <> psEol style
  where
    entries = kept <> added
    kept    = [ p | (p, _raw) <- psRaw style, p `elem` want ]
    added   = [ p | key <- planningKeywords, p <- want, fst p == key, p `notElem` kept ]
    spell p = fromMaybe (fst p <> ": " <> snd p) (lookup p (psRaw style))

-- | Is VALUE a timestamp org reads back?  Asked of the line the write would
-- produce: a value that does not reparse becomes body text on the next load.
readsAsTimestamp :: Text -> Bool
readsAsTimestamp value = either (const False) (isJust . timestampOf) (oneLine () () value)


drawerPairs :: Text -> Maybe Span -> [(Text, Text)]
drawerPairs subtree slice = case slice of
  Nothing -> []
  Just sp -> [ (key, value) | (key, value, _raw) <- drawerRows (sliceSpan subtree sp) ]

-- | R's OWN drawer pairs in file order, the hidden keys dropped.  The same
-- pairs 'headlineParts' answers under @hpProperties@, READ ALONE: a caller that
-- wants the vocabulary and not the write pays for no other region.
rowProperties :: HeadlineRecord -> [(Text, Text)]
rowProperties r = shownPairs (drawerPairs subtree (drawerSlice r subtree))
  where subtree = subtreeText r

data DrawerStyle = DrawerStyle
  { dsOpen   :: !Text                    -- ^ the @:PROPERTIES:@ line, terminator and all.
  , dsClose  :: !Text                    -- ^ the @:END:@ line, which ends the block.
  , dsIndent :: !Text                    -- ^ what a rendered line is indented by.
  , dsRaw    :: ![((Text, Text), Text)]  -- ^ each pair a client may write, and its line.
  , dsHidden :: ![(Int, Text)]           -- ^ the server's own lines, and where in the block they sat.
  }

dsEol :: DrawerStyle -> Text
dsEol = eolOf . dsClose

drawerStyle :: Text -> Text -> Maybe Span -> DrawerStyle
drawerStyle _subtree body Nothing =
  DrawerStyle (":PROPERTIES:" <> eol) (":END:" <> eol) "" [] []
  where eol = eolOf body
drawerStyle subtree body (Just sp) =
  DrawerStyle open close (indentOf (firstOr open [ raw | (_k, _v, raw) <- rows ]))
              [ ((key, value), raw) | (key, value, raw) <- rows
                                    , not (hiddenProperty key) ]
              [ (at, raw) | (at, (key, _value, raw)) <- zip [0 ..] rows
                          , hiddenProperty key ]
  where block = sliceSpan subtree sp
        ls    = linesWith block
        open  = firstOr (":PROPERTIES:" <> eolOf body) ls
        close = firstOr (":END:" <> eolOf body) (reverse ls)
        rows  = drawerRows block

drawerText :: DrawerStyle -> [(Text, Text)] -> Text
drawerText style props
  | null props && null (dsHidden style) = ""
  | otherwise = T.concat (dsOpen style : weave (dsHidden style) written <> [dsClose style])
  where
    written = go (dsRaw style) props
    go _raws []      = []
    go raws (p : ps) = case taking p raws of
      Just (raw, rest) -> raw : go rest ps
      Nothing          -> rendered p : go raws ps
    -- Consumed rather than looked up: one pair spelled twice keeps both lines.
    taking p raws = case break ((== p) . fst) raws of
      (before, (_p, raw) : after) -> Just (raw, before <> after)
      _absent                     -> Nothing
    rendered (key, value) =
      dsIndent style <> ":" <> key <> ":"
        <> (if T.null value then "" else " " <> value) <> dsEol style

weave :: [(Int, Text)] -> [Text] -> [Text]
weave kept ls = foldl' put ls (sortOn fst kept)
  where put acc (at, line) = before <> [line] <> after
          where (before, after) = splitAt at acc

-- | BLOCK's property lines, key, value and raw.  Split by line rather than
-- through 'Properties', which uppercases keys and re-tokenises values.
drawerRows :: Text -> [(Text, Text, Text)]
drawerRows block = [ (key, value, raw) | raw <- inner (linesWith block)
                                       , let (key, value) = propertyOf raw ]
  where inner ls = drop 1 (take (length ls - 1) ls)

propertyOf :: Text -> (Text, Text)
propertyOf line = case T.uncons (T.stripStart line) of
  Just (':', rest) | (key, closed) <- T.breakOn ":" rest, not (T.null closed)
                   -> (key, T.strip (T.drop 1 closed))
  _notAProperty    -> ("", T.strip line)

pastLine :: Text -> Int -> Int
pastLine t at = maybe (T.length t) (\i -> at + i + 1) (T.findIndex (== '\n') (T.drop at t))

lineStart :: Text -> Int -> Int
lineStart t at = T.length (fst (T.breakOnEnd "\n" (T.take at t)))

indentOf :: Text -> Text
indentOf = T.takeWhile horizontal

-- | TEXT with the horizontal run ending each line taken off.  ONLY THE LINE
-- END — space inside a line is content.  The TERMINATOR is stepped over: a
-- 'T.stripEnd' would take a CRLF line's @\\r@ with the spaces in front of it.
untrailed :: Text -> Text
untrailed = T.concat . map trim . linesWith
  where
    trim line = case ends line of
      Just (body, end) -> T.dropWhileEnd horizontal body <> end
      Nothing          -> T.dropWhileEnd horizontal line
    ends line = listToMaybe [ (body, end) | end <- ["\r\n", "\n"]
                                          , Just body <- [T.stripSuffix end line] ]

horizontal :: Char -> Bool
horizontal c = c == ' ' || c == '\t'

runWidth :: Text -> Int
runWidth = T.length . T.takeWhile horizontal

runWidthEnd :: Text -> Int
runWidthEnd = T.length . T.takeWhileEnd horizontal

firstOr :: a -> [a] -> a
firstOr fallback xs = case xs of { (x : _rest) -> x; [] -> fallback }

subtreeSpans :: Int -> [Headline] -> [Span]
subtreeSpans len heads = snd (foldl' place ([], []) (reverse (map extent heads)))
  where
    extent h = (levelOf h, spanStart (hsFull (spans h)))
    place (open, ends) (lvl, start) = ((lvl, start) : closers, Span start end : ends)
      where closers = dropWhile ((> lvl) . fst) open
            end = case closers of
              ((_lvl, next) : _rest) -> next
              []                     -> len

-- | KW forced, which is what makes a keyword set safe to STORE: a strict field
-- buys WHNF alone, so an unforced set pins its file's whole element tree.
forcedKeywords :: TodoKeywords -> TodoKeywords
forcedKeywords kw = forcing (tkActive kw <> tkInactive kw) kw

-- | H's row identity: @ORG_GLANCE_ID@, else @"FILE#K"@ with K its place among
-- the file's EMITTED ROWS.  One namespace, resolved by exact string.
rowId :: FilePath -> Int -> Headline -> Text
rowId path ordinal h = maybe (rowIdIn path ordinal) detach (identity h)

rowIdIn :: FilePath -> Int -> Text
rowIdIn path ordinal = T.pack path <> "#" <> T.pack (show ordinal)

isoStamp :: Timestamp -> Text
isoStamp ts = spelled fmt (tsmTime moment)
  where moment = tsStart ts
        fmt | tsmHasTime moment = "%Y-%m-%d %H:%M"
            | otherwise         = "%Y-%m-%d"

-- | DAY as the date a cell carries.  ONE FORMATTER for both sides of a date
-- comparison: a literal spelled here and a cell spelled by 'isoStamp' are the
-- same shape, so no reader can compare two spellings of one day.
isoDay :: Time.Day -> Text
isoDay = spelled "%Y-%m-%d"

-- | The day L spells, 'isoDay' READ BACKWARDS.  Same formatter, one shape: a
-- literal a reader types and a day the server holds are one value or neither.
-- 'Nothing' where L names no day, a month or a timed stamp included.
dayOf :: Text -> Maybe Time.Day
dayOf = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" . T.unpack

-- | The day W names against TODAY: THE EMPTY TEXT and @*today*@ are the
-- request's own day, anything else the ISO day it spells ('dayOf').
--
-- THE ONE BASE READER, for the filter's date literals
-- ('Glance.Web.Filter.dayIn') and the planning wall's own dates
-- ('planningTimestamp') alike, so the bare shift is today-relative at both.  A
-- surface owed a word of its own layers it OVER this, never inside it.
dayNamed :: Time.Day -> Text -> Maybe Time.Day
dayNamed today w
  | T.null w || w == metaWord MToday = Just today
  | otherwise                        = dayOf w

-- | The unit LETTERS a date shift may carry — ORG'S WHOLE CHARSET, read off the
-- parser's own map, so a unit org grows is offered the day it lands.
shiftUnits :: [Char]
shiftUnits = map unitChar [minBound .. maxBound]

-- | DAY moved N of the unit C names, ORG'S OWN CALENDAR ARITHMETIC ('addUnit'):
-- a week is seven days, and a month or a year is CLIPPED to the target month's
-- last day, so Jan 31 moved one month lands on February's last.  'Nothing'
-- where C names no unit.
shiftDay :: Char -> Integer -> Time.Day -> Maybe Time.Day
shiftDay c n day = (\u -> addUnit u n day) <$> unitOf c

-- | THE SIGN A TOKEN OPENS WITH, and a token wears one: the scanner reads the
-- FIRST CHARACTER alone, so a second sign is body text.
data Sign
  = Unsigned  -- ^ the token opened with neither sign.
  | Neg       -- ^ the token opened with @-@.
  | Add       -- ^ the token opened with @+@.
  deriving (Eq, Show)

-- | The sign C opens a token with, or 'Nothing' where C is body text.  ONE
-- CHARSET: 'shiftIn' reads a shift's sign here and the filter's scanner reads a
-- token's ('Glance.Web.Filter.scanQuery', which re-exports this), so @+@ and
-- @-@ are spelled in exactly one place.
signOf :: Char -> Maybe Sign
signOf '-' = Just Neg
signOf '+' = Just Add
signOf _   = Nothing

-- | HOW FAR A SIGN CARRIES a shifted day.  ONE EQUATION PER CONSTRUCTOR and no
-- wildcard; 'signOf' spells no character for 'Unsigned', whose day stands still.
shiftWay :: Sign -> Integer
shiftWay Unsigned = 0
shiftWay Add      = 1
shiftWay Neg      = -1

-- | A trailing SHIFT read off L: the BASE ahead of it, the SIGNED count and the
-- unit letter.  READ FROM THE END, so the sign that opens a shift is the one
-- before the unit and a date's own hyphens are never mistaken for it:
-- @2026-09-15-7d@ is the week before that day.
--
-- THE ONE SHIFT GRAMMAR.  The filter compiles its shifted literals through this
-- ('Glance.Web.Filter.literalIn', docs/query.md "A date can be shifted") and
-- the planning wall reads its dates through it ('planningTimestamp'), so a
-- shift the table serves is a shift a date-owed field takes, sign for sign.
shiftIn :: Text -> Maybe (Text, Integer, Char)
shiftIn l = case T.unsnoc l of
  Just (run, unit)
    | unit `elem` shiftUnits
    , digits            <- T.takeWhileEnd isDigit run
    , not (T.null digits)
    , Just (base, mark) <- T.unsnoc (T.dropWhileEnd isDigit run)
    , Just way          <- signOf mark
    , Just n            <- digitsOnly digits
    -> Just (base, shiftWay way * n, unit)
  _noShift -> Nothing

detach :: Text -> Text
detach = T.copy

forcing :: [a] -> b -> b
forcing ts x = foldr seq x ts

-- | R with every cell evaluated.  'hrLinks' is a LIST, so its SPINE is forced
-- beside its elements: a lazy tail retains the document it was cut from.
forceRecord :: HeadlineRecord -> HeadlineRecord
forceRecord r =
  forcing (hrId r : hrCategory r : hrTitle r : hrTags r : hrDigest r : hrSearch r
             : optional)
          (forcing (hrLinks r) (foldr seq r (hrActive r)))
  where optional = catMaybes [hrState r, hrPriority r, hrScheduled r, hrDeadline r]


-- | Why a 'replaceSpans' did not land.  Either way the file is byte-identical
-- to what it held before the call (AGENTS.hs).
data WriteFailure
  = WriteDrift !Text    -- ^ the digest the file holds now, which is not the pinned one.
  | WriteRefused !Text  -- ^ read, decode, splice or rename trouble, spelled for a caller to show.
  deriving (Eq, Show)

-- | PATH's text and digest, or @("", "")@ — the EMPTY pin, under which a write creates.
currentDocument :: FilePath -> IO (Text, Text)
currentDocument = fmap (fromMaybe ("", "")) . Edit.readDocument

-- | Replace each span of FILE, provided it still digests to DIGEST.  THE DOOR
-- every write leaves through, so the note to org-glance is taken here (AGENTS.hs).
replaceSpans :: FilePath -> Text -> [(Span, Text)] -> IO (Either WriteFailure Text)
replaceSpans path digest edits = do
  written <- Edit.editFile (Edit.Snapshot path digest) [ Edit.Edit sp new | (sp, new) <- edits ]
  either (pure . Left . failure) noted written
  where
    noted receipt = do
      External.noteExternalWrite path (Edit.receiptText receipt)
      pure (Right (Edit.snapDigest (Edit.receiptSnapshot receipt)))
    failure err = case err of
      Edit.Drift _path _pinned found -> WriteDrift found
      Edit.ReadFailed _path why      -> WriteRefused ("cannot read " <> named <> ": " <> why)
      Edit.DecodeFailed _path        -> WriteRefused (named <> " is not valid UTF-8")
      Edit.Rejected editError        -> WriteRefused ("the edit does not apply to " <> named
                                                       <> ": " <> T.pack (show editError))
      Edit.WriteFailed _path why     -> WriteRefused ("cannot write " <> named <> ": " <> why)
    named = T.pack path

-- Commands
--
-- Span edits over the text a record was parsed from.  Nothing here reads or
-- writes a file: a caller hands the result to 'replaceSpans'.


tagged :: Text -> HeadlineRecord -> Bool
tagged tag = \r -> want `elem` tagsOfCell (hrTags r)
  where want = T.toLower tag

archived :: HeadlineRecord -> Bool
archived = tagged archiveTag

-- | TEXT as an org tag.  The charset is the PARSER's ('isTagChar'): what this
-- writes has to reparse HERE, or the run falls into title text on the next load.
tagText :: Text -> Either Text Text
tagText text
  | T.null text            = Left "a tag is at least one character"
  | T.all isTagChar text   = Right text
  | otherwise              = Left (text <> " is not an org tag: a tag is letters,"
                                     <> " digits, and _ - @ # or %")

-- | TEXT as a TODO keyword.  The charset is the PARSER's ('isKeywordChar'): a
-- word org will not read back declares nothing, and the writer is told WHICH
-- word rather than that the block came to nothing.
keywordText :: Text -> Either Text Text
keywordText text
  | T.null text              = Left "a state is at least one character"
  | T.all isKeywordChar text = Right text
  | otherwise                = Left (text <> " is not a TODO state: a state is"
                                       <> " letters and _")

-- | The classification chain behind ROWS, one entry per SOURCE.  DEDUP IS THE
-- CLASSIFICATION RULE.  Rows merge by source NAME, so a keyword one reaches by
-- file and another by tag lands in the WIDER (AGENTS.hs).
keywordSources :: ConfigLayers -> [HeadlineRecord] -> [(Text, TodoKeywords)]
keywordSources cfg rows = widest Set.empty (sortOn fst chain)
  where
    filed   = mergeKeywords (map hrDeclared rows)
    chain   = [ (rank, (source, kw))
              | r <- rows
              , (rank, source, kw) <- keywordScopes cfg filed (tagsOfCell (hrTags r)) ]
    widest _seen [] = []
    widest seen ((_rank, (source, kw)) : rest)
      | null actives && null inactives = widest seen rest
      | otherwise = (source, TodoKeywords actives inactives) : widest taken rest
      where actives   = filter unseen (tkActive kw)
            inactives = filter unseen (tkInactive kw)
            unseen w  = not (Set.member w seen)
            taken     = foldr Set.insert seen (actives <> inactives)

-- | @set-state@'s edits.  KEYWORD is refused unless R's OWN CHAIN declares it.
setStateEdits :: ConfigLayers -> Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setStateEdits _cfg Nothing r = Right (tokenEdits hsTodo (spanEnd . hsStars) Nothing r)
setStateEdits cfg (Just keyword) r
  | keyword `notElem` settable =
      Left (keyword <> " is not a TODO keyword for " <> hrId r <> " in " <> T.pack (hrFile r)
              <> "; that row may be set to " <> T.intercalate ", " settable)
  | otherwise = Right (tokenEdits hsTodo (spanEnd . hsStars) (Just keyword) r)
  where settable = settableStates cfg r

-- | The token AT set to TOKEN, PLACE saying where one goes on a headline with
-- none.  'Nothing' deletes it WITH the horizontal run — so a line's end survives.
tokenEdits :: (HeadlineSpans -> Maybe Span) -> (HeadlineSpans -> Int)
           -> Maybe Text -> HeadlineRecord -> [(Span, Text)]
tokenEdits at place token r = case (at hs, token) of
  (Just sp, Just new) -> [(sp, new)]
  (Just sp, Nothing)  -> [(Span (spanStart sp) (spanEnd sp + trailing sp), "")]
  (Nothing, Just new) -> [(insertAt (place hs), " " <> new)]
  (Nothing, Nothing)  -> []
  where hs = headlineSpans r
        trailing sp = runWidth (T.drop (spanEnd sp) (hrDoc r))

-- | The states R may be set to: 'keywordSources' flattened, so offer and wall agree.
settableStates :: ConfigLayers -> HeadlineRecord -> [Text]
settableStates cfg r =
  [ word | (_source, kw) <- keywordSources cfg [r], word <- tkActive kw <> tkInactive kw ]


-- | R's `ORG_GLANCE_ID`.  The ledger's key: an ordinal names another row a week on.
rowOrgId :: HeadlineRecord -> Maybe Text
rowOrgId = identity . hrHeadline

data Repeat = Repeat
  { rpState   :: !Text            -- ^ the keyword the entry lands on.
  , rpShifted :: !Text            -- ^ its next occurrence, cookie and all.
  , rpEdits   :: ![(Span, Text)]  -- ^ the shift and the reset, as one set.
  } deriving (Eq, Show)

-- | R completed into KEYWORD, else 'Nothing'.  ORG'S OWN CONDITION, both
-- halves: an INACTIVE keyword AND a stamp carrying a repeater.  ONE EDIT SET,
-- so one write and one event; the reset is the chain's first ACTIVE word.
repeatOn :: ConfigLayers -> Time.Day -> Text -> HeadlineRecord -> Maybe Repeat
repeatOn cfg today keyword r
  | keyword `notElem` chainOf tkInactive = Nothing
  | null shifts                          = Nothing
  | otherwise = Just Repeat { rpState = fromMaybe "" reset
                            , rpShifted = snd (head shifts)
                            , rpEdits = shifts <> tokenEdits hsTodo (spanEnd . hsStars) reset r }
  where
    shifts = [ (sp, rewriteDates (repeatDay today i) text)
           | (sp, text, i) <- repeatingSpans r ]
    reset  = listToMaybe (chainOf tkActive)
    chain  = keywordSources cfg [r]
    chainOf half = [ word | (_source, kw) <- chain, word <- half kw ]

timestampOf :: Text -> Maybe Timestamp
timestampOf text = case orgParse defaultContext ("* probe\nSCHEDULED: " <> text <> "\n") of
  (elems, _ctx, Nothing) -> listToMaybe [ ts | e <- elems, EHeadline h <- [valueOf e]
                                             , Just ts <- [schedule h] ]
  _failed                -> Nothing

repeatsOf :: HeadlineRecord -> Maybe Text
repeatsOf r = listToMaybe [ repeaterFormat i | (_sp, _text, i) <- repeatingSpans r ]

repeatingSpans :: HeadlineRecord -> [(Span, Text, TimestampRepeaterInterval)]
repeatingSpans r =
  [ (sp, sliceSpan (hrDoc r) sp, i)
  | (at, stamp) <- [ (hsSchedule, schedule), (hsDeadline, deadline) ]
  , Just sp <- [at (headlineSpans r)]
  , Just ts <- [stamp (hrHeadline r)]
  , Just i  <- [tsInterval ts] ]

-- | DAY one repeat on under INTERVAL.  A zero-width interval takes the `+N`
-- arm, since the `++` loop over one would not end.
repeatDay :: Time.Day -> TimestampRepeaterInterval -> Time.Day -> Time.Day
repeatDay today interval day
  | repeaterValue interval <= 0   = day
  | otherwise = case repeaterType interval of
      Restart    -> once day
      Cumulative -> once today
      CatchUp    -> until (> today) once day
  where
    once = addUnit (repeaterUnit interval) (fromIntegral (repeaterValue interval))

-- | TEXT with every date moved one repeat on.  TEXTUAL: the time, the cookies
-- and a range's second half stay the author's bytes.
shiftRepeat :: Time.Day -> Text -> Maybe Text
shiftRepeat today text = do
  ts <- timestampOf text
  interval <- tsInterval ts
  pure (rewriteDates (repeatDay today interval) text)

rewriteDates :: (Time.Day -> Time.Day) -> Text -> Text
rewriteDates move = go
  where
    go text = case dateAt text of
      Just (day, rest) -> let moved = move day
                              (had, after) = weekdayAt rest
                          in spelled "%Y-%m-%d" moved
                          <> (if had then " " <> spelled "%a" moved else "")
                          <> go after
      Nothing | T.null text -> text
              | otherwise   -> T.take 1 text <> go (T.drop 1 text)
    -- VARIABLE WIDTH, because `tsDayParser' is: `<2026-08-8 Sat>' parses, and a
    -- fixed ten-character window would cut it short and eat the space behind it.
    dateAt text = do
      (y, afterY) <- digitsOf text
      afterYDash  <- T.stripPrefix "-" afterY
      (m, afterM) <- digitsOf afterYDash
      afterMDash  <- T.stripPrefix "-" afterM
      (d, rest)   <- digitsOf afterMDash
      day <- Time.fromGregorianValid y (fromInteger m) (fromInteger d)
      pure (day, rest)
    digitsOf text = case TR.decimal text of
      Right (n, rest) | n >= 0 -> Just (n :: Integer, rest)
      _notANumber              -> Nothing
    weekdayAt rest = case T.uncons rest of
      Just (' ', body) | (word, after) <- T.span isLetter body, not (T.null word)
                         -> (True, after)
      _noWeekday         -> (False, rest)

titleSpan :: HeadlineRecord -> Maybe Span
titleSpan = hsTitle . headlineSpans

oneLine :: e -> e -> Text -> Either e Text
oneLine empty many text
  | T.null want          = Left empty
  | T.any (== '\n') want = Left many
  | otherwise            = Right want
  where want = T.strip text

titleText :: Text -> Either Text Text
titleText = oneLine "a headline needs a title: the text after the keyword"
                    "a title is one line: the rest of the headline's own line"

-- | @set-title@'s edits.  'titleLineEnd' cannot serve — its answer includes
-- 'hsTags', past which a title reads back as tag text.
setTitleEdits :: Text -> HeadlineRecord -> Either Text [(Span, Text)]
setTitleEdits text r = do
  want <- titleText text
  pure $ case hsTitle hs of
    Just sp -> [(sp, want)]
    Nothing -> case [ spanEnd sp | Just sp <- [hsPriority hs, hsTodo hs] ] of
      (at : _rest) -> [(insertAt at, " " <> want)]
      []           -> [(insertAt (pastRun (spanEnd (hsStars hs))), want)]
  where hs = headlineSpans r
        pastRun at = at + runWidth (T.drop at (hrDoc r))

priorityText :: Text -> Either Text Text
priorityText text
  | T.length want == 1, T.all isAsciiUpper want = Right want
  | otherwise = Left (text <> " is not a priority: org spells one as a single"
                        <> " letter, A to C in its own cycle")
  where want = T.toUpper (T.strip text)

setPriorityEdits :: Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setPriorityEdits Nothing r = Right (tokenEdits hsPriority afterKeyword Nothing r)
setPriorityEdits (Just letter) r = do
  want <- priorityText letter
  pure (tokenEdits hsPriority afterKeyword (Just (priorityCell want)) r)

afterKeyword :: HeadlineSpans -> Int
afterKeyword hs = maybe (spanEnd (hsStars hs)) spanEnd (hsTodo hs)

-- | @add-tag@'s edits.  With no tags the run joins the TITLE LINE: 'hsFull' ends
-- at a timestamp on the NEXT line for a scheduled headline.
addTagEdits :: Text -> HeadlineRecord -> [(Span, Text)]
addTagEdits tag r = addTagEditsIn (hrTags r) tag (headlineSpans r)

addTagEditsIn :: Text -> Text -> HeadlineSpans -> [(Span, Text)]
addTagEditsIn cell tag hs
  | T.toLower tag `elem` tagsOfCell cell = []
  | Just sp <- hsTags hs = [ (insertAt (spanEnd sp), tag <> ":") ]
  | otherwise            = [ (insertAt (titleLineEnd hs), " :" <> tag <> ":") ]

-- | @remove-tag@'s edits.  The LAST entry takes the whole run and the space in
-- front of it, a lone @:@ not being a tag list.  Matching is FOLDED.
removeTagEdits :: Text -> HeadlineRecord -> [(Span, Text)]
removeTagEdits tag r = case tagRun r of
  Nothing -> []
  Just (run, separator, entries)
    | null hit  -> []
    | null left -> [ (Span (spanStart run - separator) (spanEnd run), "") ]
    | otherwise -> map cutEntry hit
    where (hit, left) = partition (spells tag) entries

-- | @rename-tag@'s edits, in place.  A remove plus an add is wrong twice over —
-- the addition's anchor is measured BEFORE the removal and it appends at the
-- RUN'S END — and is two writes under two digests.  FROM is FOLDED.
renameTagEdits :: Text -> Text -> HeadlineRecord -> [(Span, Text)]
renameTagEdits from to r = case tagRun r of
  Nothing -> []
  Just (_run, _separator, entries) -> case partition (spells from) entries of
    ([], _left) -> []
    (hit@(first : rest), left)
      | any (spells to) left -> map cutEntry hit
      | otherwise            -> renamed first <> map cutEntry rest
  where renamed (at, entry)
          -- A byte-identical rewrite is still a temp-and-rename, an event and a re-parse.
          | entry == to = []
          | otherwise   = [(Span at (at + T.length entry), to)]

-- | R's tag RUN, read once.  A headline parses at column 1, so its stars ARE its line's start.
tagRun :: HeadlineRecord -> Maybe (Span, Int, [(Int, Text)])
tagRun r = case hsTags hs of
  Nothing  -> Nothing
  Just run -> let line  = sliceSpan (hrDoc r) (Span from (spanEnd run))
                  ahead = spanStart run - from
              in Just ( run
                      , runWidthEnd (T.take ahead line)
                      , [ (spanStart run + at, entry)
                        | (at, entry) <- tagEntries (T.drop ahead line) ] )
  where hs   = headlineSpans r
        from = spanStart (hsStars hs)

spells :: Text -> (Int, Text) -> Bool
spells tag = \(_at, entry) -> T.toLower entry == want
  where want = T.toLower tag

cutEntry :: (Int, Text) -> (Span, Text)
cutEntry (at, entry) = (Span at (at + T.length entry + 1), "")

tagEntries :: Text -> [(Int, Text)]
tagEntries run = case offsets 0 (T.splitOn ":" run) of
  pieces@(_ : _ : _) -> drop 1 (init pieces)
  _notARun           -> []
  where offsets _ []          = []
        offsets at (p : rest) = (at, p) : offsets (at + T.length p + 1) rest

archiveEdits :: HeadlineRecord -> [(Span, Text)]
archiveEdits = addTagEdits archiveTag

-- | @edit-link@'s edits.  THE FORM IS PRESERVED and ABSENT IS NOT NULL.  TWO
-- WALLS ('linkAtSpan', 'spelling'), the write engine being content-agnostic by law.
editLinkEdits :: Span -> Text -> Maybe (Maybe Text) -> HeadlineRecord
              -> Either Text [(Span, Text)]
editLinkEdits sp target desc r = do
  found <- linkAtSpan sp r
  written <- spelling target (reshaped (olShape found) desc)
  pure [(sp, written)]

-- | TARGET in SHAPE, or why that text is not that link.  REPARSE AND COMPARE:
-- @a][b@ renders one link pointing somewhere the request never named.
spelling :: Text -> LinkShape -> Either Text Text
spelling target shape
  | T.any newline target || any (T.any newline) (described shape) =
      Left "a link is one line: neither its target nor its description may carry a newline"
  | Just l <- onlyLink written, olTarget l == target, olShape l == shape = Right written
  | otherwise = Left (written <> " does not read as one link pointing at " <> target)
  where written = renderLink target shape
        newline c = c == '\n' || c == '\r'
        described (Bracketed d) = maybe [] pure d
        described Bare          = []

-- | The one link SP covers.  It must sit inside the ROW's own subtree — a digest
-- is per file — and cover the link EDGE TO EDGE.
linkAtSpan :: Span -> HeadlineRecord -> Either Text OrgLink
linkAtSpan sp r
  | spanStart sp >= spanEnd sp =
      Left (spanned sp <> " covers no characters")
  | spanStart sp < spanStart sub || spanEnd sp > spanEnd sub =
      Left (spanned sp <> " is not inside " <> hrId r <> "'s subtree " <> spanned sub)
  | otherwise = maybe (Left (spanned sp <> " does not read as one link")) Right
                      (onlyLink (sliceSpan (hrDoc r) sp))
  where sub = hrSubtree r

onlyLink :: Text -> Maybe OrgLink
onlyLink text = case orgLinks text of
  [l] | olSpan l == Span 0 (T.length text) -> Just l
  _notOneLink                              -> Nothing

reshaped :: LinkShape -> Maybe (Maybe Text) -> LinkShape
reshaped shape Nothing      = shape
reshaped shape (Just given) = case given of
  Just desc | not (T.null (T.strip desc)) -> Bracketed (Just desc)
  _takeItOff                              -> case shape of
    Bare        -> Bare
    Bracketed _ -> Bracketed Nothing

renderLink :: Text -> LinkShape -> Text
renderLink target Bare                 = target
renderLink target (Bracketed Nothing)  = "[[" <> target <> "]]"
renderLink target (Bracketed (Just d)) = "[[" <> target <> "][" <> d <> "]]"

spanned :: Span -> Text
spanned sp = "[" <> offset (spanStart sp) <> "," <> offset (spanEnd sp) <> ")"
  where offset = T.pack . show

-- | Where HS's title LINE ends.  'hsFull' cannot serve — its end is the last
-- part in SPAN ORDER, a timestamp on the NEXT line for a scheduled headline.
titleLineEnd :: HeadlineSpans -> Int
titleLineEnd hs = foldl' max (spanEnd (hsStars hs))
  [ spanEnd sp | Just sp <- [hsTodo hs, hsPriority hs, hsTitle hs, hsTags hs] ]

settableKeywords :: [Text]
settableKeywords = filter (/= "CLOSED") planningKeywords

setPlanningEdits :: Text -> Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setPlanningEdits keyword stamp r
  | keyword `notElem` settableKeywords =
      Left (keyword <> " is not a planning keyword; this server sets "
              <> T.intercalate " and " settableKeywords)
  | otherwise = Right $ case (lookup keyword present, stamp) of
      (Just sp, Just ts) -> [(sp, ts)]
      (Just sp, Nothing) -> [(cleared sp, "")]
      (Nothing, Just ts) -> [added ts]
      (Nothing, Nothing) -> []
  where
    hs      = headlineSpans r
    doc     = hrDoc r
    present = presentPlanning hs
    others  = [ sp | (key, sp) <- present, key /= keyword ]

    -- The line is cut ONCE and every scan runs inside it, not down the document.
    cleared sp
      | null others  = Span from (pastLine doc (spanEnd sp))
      | trailing > 0 = Span at (spanEnd sp + trailing)
      | otherwise    = Span (at - leading) (spanEnd sp)
      where from     = lineStart doc (spanStart sp)
            at       = fromMaybe (spanStart sp) (entryOpening doc keyword (spanStart sp))
            line     = sliceSpan doc (Span from (pastLine doc (spanEnd sp)))
            trailing = runWidth (T.drop (spanEnd sp - from) line)
            leading  = runWidthEnd (T.take (at - from) line)

    added ts
      | null others = (insertAt (titleLineEnd hs), eolOf doc <> entry)
      | otherwise   = (insertAt (maximum (map spanEnd others)), " " <> entry)
      where entry = keyword <> ": " <> ts

-- | TEXT as a planning timestamp against TODAY.  Org's own spelling is kept
-- verbatim once it REPARSES; the rest render with the weekday COMPUTED.
--
-- ONE GRAMMAR, ONE DOOR.  Every surface that owes a date reads TEXT here —
-- @set-planning@'s argument, and the planning line's own wall — so a form this
-- reader takes is a form the pane's date widget may type and a form it declines
-- is refused the same way at both.
planningTimestamp :: Time.Day -> Text -> Either Text Text
planningTimestamp today text
  | T.null want            = refusal
  | bracketed              = if readsAsTimestamp want then Right want else refusal
  | Just answer <- english = answer
  | otherwise = maybe refusal Right (withTime <$> asLocal <|> (`stamped` Nothing) <$> dated)
  where
    want      = T.strip text
    bracketed = any (`T.isPrefixOf` want) timestampOpeners
    refusal   = Left (text <> " is not a date: spell it 2026-08-05, 2026-08-05 09:30, "
                        <> relativeForms
                        <> ", today, tomorrow, " <> metaWord MToday
                        <> ", 18 aug, 18 august 2027, from 18 to 19 aug"
                        <> ", or org's own <2026-08-05 Wed>")

    -- THE ENGLISH PHRASE IS READ BEHIND ORG'S OWN BRACKETS AND AHEAD OF THE
    -- REST, because it is the one reading with a refusal of its own to spend:
    -- an inverted interval names two perfectly good days in the wrong order and
    -- "is not a date" reads oddly of it.
    english = case englishSpan today want of
      Just (Left why)         -> Just (Left why)
      Just (Right (from, to)) -> Just (Right (orgRange from to))
      Nothing                 -> Right . (`stamped` Nothing) <$> englishDay today want

    -- THE ONE SHIFT GRAMMAR ('shiftIn') over THE ONE BASE READER ('dayNamed'):
    -- what the filter's table serves and what a date-owed field takes are one
    -- grammar, sign for sign, and the planning words compose because a word
    -- legal alone is legal shifted.  Lower-casing leaves an ISO day's digits and
    -- hyphens where they were, so the bare day needs no branch of its own.
    dated = case T.toLower want of
      w | Just d <- baseDay w             -> Just d
        | Just (base, n, u) <- shiftIn w  -> shiftDay u n =<< baseDay base
        | otherwise                       -> Nothing
    -- @today@ and @tomorrow@ are THIS WALL'S OWN WORDS, layered over the shared
    -- reader: a query spells @*today*@, so the filter is owed no English here.
    baseDay w
      | w == "today"    = Just today
      | w == "tomorrow" = Just (Time.addDays 1 today)
      | otherwise       = dayNamed today w

    -- @%k@ rather than @%H@: it reads one digit as well as two, so @9:05@ is the
    -- time a reader meant rather than a refusal over a zero.
    asLocal :: Maybe Time.LocalTime
    asLocal = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %k:%M" (T.unpack want)
    withTime = timedStamp activeBrackets
    stamped  = orgStamp activeBrackets

-- | The MONTH WORDS an English date may spell: org's three-letter form and the
-- full one, lower case, matched after 'T.toLower'.  @may@ is ONE entry, its two
-- forms coinciding; @sept@ and any form carrying a full stop are outside the
-- table on purpose.
--
-- THE ONLY LANGUAGE-BEARING DATUM in the grammar: a second language is a second
-- table and a selector — plus @from@ and @to@, which the same table can carry —
-- and nothing else in the parser moves.
monthWords :: [(Text, Int)]
monthWords =
  [ ("jan", 1),  ("january", 1),   ("feb", 2),  ("february", 2)
  , ("mar", 3),  ("march", 3),     ("apr", 4),  ("april", 4)
  , ("may", 5)
  , ("jun", 6),  ("june", 6),      ("jul", 7),  ("july", 7)
  , ("aug", 8),  ("august", 8),    ("sep", 9),  ("september", 9)
  , ("oct", 10), ("october", 10),  ("nov", 11), ("november", 11)
  , ("dec", 12), ("december", 12) ]

-- | The day TEXT names in English against TODAY: @18 aug@ or @aug 18@, either
-- arrangement optionally carrying a year.  'Nothing' where TEXT names no day —
-- THE WHOLE FIELD IS THE PHRASE, so one word outside the grammar leaves the
-- value text rather than guessing at a date inside it.
--
-- THE YEAR IS THE CLOCK'S, FLAT: @18 aug@ typed in December means that August,
-- and a typist meaning next year writes the year.  'Time.fromGregorianValid' is
-- the wall @31 feb@ and @29 feb@ in a common year meet.  THE WEEKDAY IS NEVER
-- READ — it is computed on render — so @thu 18 aug@ is text even when Thursday
-- is right.
englishDay :: Time.Day -> Text -> Maybe Time.Day
englishDay today text = case englishFields (wsWords (T.toLower text)) of
  Just (d, Just m, y) -> Time.fromGregorianValid (fromMaybe (yearOf today) y) m d
  _noEnglishDate      -> Nothing

-- | The two days an English INTERVAL names against TODAY.  @from@ is optional
-- and @to@ is not, so @18 to 19 aug@ is the interval where @18 19 aug@ is text.
--
-- THE LEFT END INHERITS EVERY FIELD IT ELIDES from the right — the English idiom
-- says one month once — which is why @from 18 to 19 august 2027@ is two days in
-- 2027 rather than a twelve-month span.  'Nothing' means TEXT spells no interval
-- at all; 'Left' is the ONE refusal this parser spends a word on, an interval
-- whose END FALLS BEFORE ITS START, and the remedy it names is a typed year.
-- The degenerate pair is no refusal: it comes back with both ends equal and
-- COLLAPSES at the renderer.
englishSpan :: Time.Day -> Text -> Maybe (Either Text (Time.Day, Time.Day))
englishSpan today text = do
  (leftWs, rightWs) <- cut (dropFrom (wsWords (T.toLower text)))
  (rd, rmonth, ry)  <- englishFields rightWs
  -- THE RIGHT END SPELLS ITS OWN MONTH: it has nothing to inherit from.
  rm                <- rmonth
  (ld, lm, ly)      <- englishFields leftWs
  let year = fromMaybe (yearOf today) ry
  to   <- Time.fromGregorianValid year rm rd
  from <- Time.fromGregorianValid (fromMaybe year ly) (fromMaybe rm lm) ld
  pure (if from > to then Left (inverted text) else Right (from, to))
  where
    dropFrom ("from" : rest) = rest
    dropFrom ws              = ws
    cut ws = case break (== "to") ws of
      (before, _to : after) | not (null before), not (null after) -> Just (before, after)
      _noKeyword                                                  -> Nothing
    inverted phrase =
      phrase <> " ends before it starts: spell a year at each end,"
             <> " as in from 30 dec 2026 to 2 jan 2027"

-- | The DAY, the MONTH and the YEAR a phrase's WORDS name, the last two
-- 'Nothing' where the words elide them — which only an interval's LEFT END may
-- do.  A BARE DAY AND A BARE MONTH ARE NO DATE on their own; 'englishDay' is
-- what refuses them, by demanding the month.
englishFields :: [Text] -> Maybe (Int, Maybe Int, Maybe Integer)
englishFields [d]       = (\n -> (n, Nothing, Nothing)) <$> dayWord d
englishFields [a, b]    = dayAndMonth a b Nothing
englishFields [a, b, y] = dayAndMonth a b . Just =<< yearWord y
englishFields _noPhrase = Nothing

-- | The day and month two words name, EITHER ARRANGEMENT — @18 aug@ and
-- @aug 18@ — carrying the year Y its caller already read.
dayAndMonth :: Text -> Text -> Maybe Integer -> Maybe (Int, Maybe Int, Maybe Integer)
dayAndMonth a b y = shaped <$> dayWord a <*> monthWord b
                <|> flip shaped <$> monthWord a <*> dayWord b
  where shaped d m = (d, Just m, y)

-- | A DAY NUMBER: one digit or two, naming 1..31.  @18th@ keeps its ordinal
-- suffix and @018@ its third character, so neither is a day here.
dayWord :: Text -> Maybe Int
dayWord w
  | T.length w <= 2, Just n <- digitsOnly w, n >= 1, n <= 31 = Just (fromInteger n)
  | otherwise                                                = Nothing

-- | A YEAR: FOUR DIGITS AND NEVER TWO, which is what keeps @18 aug 18@ text
-- where a fuzzy reader answers 2018.
yearWord :: Text -> Maybe Integer
yearWord w | T.length w == 4 = digitsOnly w
           | otherwise       = Nothing

monthWord :: Text -> Maybe Int
monthWord w = lookup w monthWords

-- | W as a WHOLE decimal run, or 'Nothing': a suffix, a sign or a separator left
-- over means W was never a number.
digitsOnly :: Text -> Maybe Integer
digitsOnly w = case TR.decimal w of
  Right (n, "") -> Just n
  _notANumber   -> Nothing

-- | TEXT's words over THE GRAMMAR'S OWN SEPARATOR — a run of spaces and tabs and
-- nothing else, so @18  aug@ is @18 aug@.  A NEWLINE IS NO SEPARATOR HERE: it
-- stays inside its word, no phrase carrying one reads as a date, and none can
-- reach a planning line to break it in two.
wsWords :: Text -> [Text]
wsWords = filter (not . T.null) . T.split (\c -> c == ' ' || c == '\t')

yearOf :: Time.Day -> Integer
yearOf day = y where (y, _month, _day) = Time.toGregorian day

-- | The days FROM..TO as org spells them: the @--@ pair joining two stamps,
-- each computing its OWN WEEKDAY off its date.  NO SECOND STAMP RENDERER —
-- 'TextShow' is the lossy REPL re-serializer and never a write-back channel
-- (docs/invariants.md), so the bytes a write lands are spelled through this
-- module's own 'orgStamp', beside every other stamp this wall writes.
--
-- A DEGENERATE PAIR COLLAPSES to the single stamp: equal ENDS write one stamp
-- and no @--@, so @from 18 to 18 aug@ and @18 aug@ land on the same bytes and
-- the law stays "refuse end before start" rather than growing a second clause.
orgRange :: Time.Day -> Time.Day -> Text
orgRange from to = one from <> (if to == from then "" else "--" <> one to)
  where one d = orgStamp activeBrackets d Nothing

-- | The brackets org writes a timestamp in, DERIVED from the pair the parser
-- matches on: a bracket it declines would reach the disk uncaught.
activeBrackets, inactiveBrackets :: (Text, Text)
activeBrackets   = bracketsOf TimestampActive
inactiveBrackets = bracketsOf TimestampInactive

bracketsOf :: TimestampStatus -> (Text, Text)
bracketsOf status = (T.singleton open, T.singleton close)
  where (open, close) = tsBrackets status

timestampOpeners :: [Text]
timestampOpeners = map fst [activeBrackets, inactiveBrackets]

orgStamp :: (Text, Text) -> Time.Day -> Maybe Text -> Text
orgStamp (open, close) day time =
  open <> spelled "%Y-%m-%d %a" day <> maybe "" (" " <>) time <> close

timedStamp :: (Text, Text) -> Time.LocalTime -> Text
timedStamp brackets at = orgStamp brackets (Time.localDay at) (Just (spelled "%H:%M" at))

captureProperty :: Text
captureProperty = "ORG_GLANCE_CREATION_TIME"

zonedStamp :: TimestampStatus -> Time.ZonedTime -> Text
zonedStamp status = timedStamp (bracketsOf status) . Time.zonedTimeToLocalTime

captureStamp :: Time.ZonedTime -> Text
captureStamp = zonedStamp TimestampInactive

-- | @capture@'s edits: ONE insertion at the END, lines ending the target's own
-- way.  'untrailed' here ENFORCES the no-trailing-space rule rather than applying it.
captureEdits :: Text -> Text -> Text -> Either Text [(Span, Text)]
captureEdits doc stamp text = written <$> captureText text
  where
    written typed = [(insertAt (T.length doc), openingFor doc eol <> untrailed (entry typed))]
    eol   = eolOf doc
    entry typed = T.concat [ line <> eol
                           | line <- [ "* " <> typed
                                     , ":PROPERTIES:"
                                     , ":" <> captureProperty <> ": " <> stamp
                                     , ":END:" ] ]

-- | TEXT as the one headline a capture promises.  The wall BOTH paths take: a
-- newline lands a column-1 star the parser reads as a second entry.
captureText :: Text -> Either Text Text
captureText = oneLine "a capture needs a headline: the text that goes after the star"
                      "a captured entry is one headline, so its text is one line"


-- | The @%@-codes served.  The scanner never consults it; @TestQuery@ keeps the two in step.
captureCodes :: [(Text, Text)]
captureCodes =
  [ ("%?", "where the text you type lands — a template without it cannot be filled")
  , ("%U", "the moment of capture, inactive: [2026-08-04 Tue 09:30]")
  , ("%T", "the moment of capture, active: <2026-08-04 Tue 09:30>")
  , ("%^{PROMPT}", "asks PROMPT before capturing and writes the answer here")
  ]

data TemplatePart
  = TplText !Text               -- ^ written as it stands.
  | TplPoint                    -- ^ @%?@: the line the reader typed.
  | TplStamp !TimestampStatus   -- ^ @%T@ and @%U@, the server's clock in org's two bracket kinds.
  | TplAsk !Text                -- ^ @%^{PROMPT}@: the answer @fields@ carries for PROMPT.
  deriving (Eq, Show)

templateParts :: Text -> [TemplatePart]
templateParts = go
  where
    go t = case T.breakOn "%" t of
      (before, rest)
        | T.null rest -> [ TplText before | not (T.null before) ]
        | otherwise   -> [ TplText before | not (T.null before) ] <> code (T.drop 1 rest)
    code rest = case T.uncons rest of
      Nothing       -> [TplText "%"]
      Just ('?', t) -> TplPoint : go t
      Just ('U', t) -> TplStamp TimestampInactive : go t
      Just ('T', t) -> TplStamp TimestampActive : go t
      Just ('^', t) -> ask t
      Just (c, t)   -> TplText (T.pack ['%', c]) : go t
    ask t = case T.stripPrefix "{" t of
      Just body | (want, closed) <- T.breakOn "}" body, not (T.null closed)
                  -> TplAsk want : go (T.drop 1 closed)
      _notAnAsk   -> TplText "%^" : go t

templatePrompts :: Text -> [Text]
templatePrompts t = nub [ want | TplAsk want <- templateParts t ]

expandTemplate :: Time.ZonedTime -> [(Text, Text)] -> Text -> Text -> Either Text Text
expandTemplate now answers text template
  | TplPoint `notElem` parts = Left noPoint
  | otherwise                = T.concat <$> traverse piece parts
  where
    parts = templateParts template
    piece part = case part of
      TplText t       -> Right t
      TplPoint        -> Right text
      TplStamp status -> Right (zonedStamp status now)
      TplAsk want     -> maybe (Left (unanswered want)) Right (lookup want answers)
    noPoint = "this capture template has no %?, so there is nowhere for the text to go"
    unanswered want = "this capture template asks " <> want
                        <> "; name it in args {\"fields\": {" <> want <> ": \"…\"}}"

-- | Where DOC's capture template sits: first heading to EOF, which is
-- @org-glance-tag-config--entry@'s rule verbatim rather than the outline extent.
captureTemplateSpan :: Text -> Maybe Span
captureTemplateSpan doc = (\from -> Span from (T.length (T.stripEnd doc))) <$> headingAt doc

headingAt :: Text -> Maybe Int
headingAt doc = listToMaybe [ spanStart sp | (sp, line) <- lineSpansIn doc
                            , isJust (headingStars line) ]

-- | How many stars LINE opens a heading with.  org-glance's own @^\\*+ @, so a
-- bare star run is body text here where the PARSER reads an empty headline.
headingStars :: Text -> Maybe Int
headingStars line = case T.span (== '*') line of
  (stars, rest) | not (T.null stars), maybe False (horizontal . fst) (T.uncons rest)
                  -> Just (T.length stars)
  _notAHeading    -> Nothing

captureTemplateOf :: Text -> Maybe Text
captureTemplateOf doc = sliceSpan doc <$> captureTemplateSpan doc

captureTemplateIn :: Text -> [ConfigLayerFile] -> Maybe Text
captureTemplateIn tag layers = mine <|> systemSetting captureTemplateOf layers
  where
    mine = captureTemplateOf . lfText =<< listToMaybe [ f | f <- layers, lfTag f == Just folded ]
    folded = T.toLower tag

captureTemplateEdits :: Text -> Text -> Either Text [(Span, Text)]
captureTemplateEdits doc want
  | T.null value         = Right [ (Span from (T.length doc), "") | Just from <- [headingAt doc] ]
  | not (topEntry value) = Left notATemplate
  | otherwise            = Right [ maybe appended written (captureTemplateSpan doc) ]
  where
    value = T.stripEnd want
    written sp = (sp, value)
    appended = (insertAt (T.length doc), openingFor doc eol <> value <> eol)
    eol = eolOf doc
    notATemplate = "a capture template is one top entry: its first line opens with a\
                   \ single star, as \"* %?\" does"

data BlobSeed = BlobSeed
  { bsTag   :: !Text  -- ^ the org tag the entry wears.
  , bsId    :: !Text  -- ^ its @ORG_GLANCE_ID@.
  , bsStamp :: !Text  -- ^ its 'captureProperty' stamp, as 'captureStamp' spells one.
  }

-- | ENTRY as the document a blob holds.  The tag's rule is 'addTagEditsIn', the
-- very function @add-tag@ runs, so capture and command cannot disagree.
blobDocument :: BlobSeed -> Text -> Either Text Text
blobDocument seed given = case firstHeadlineOf elems of
  Nothing -> Left "this capture template expands to no headline, so there is no entry to store"
  Just h  -> spliced (spans h)
  where
    eol = eolOf given
    -- ENDED FIRST: a template is stored right-trimmed, so a title line with no
    -- newline of its own would take the drawer onto the end of itself.
    entry = given <> openingFor given eol
    (elems, _ctx, _err) = orgParse defaultContext entry
    spliced hs = either (Left . refused) (Right . untrailed)
                        (Edit.applyEdits entry [ Edit.Edit sp new | (sp, new) <- edits hs ])
    edits hs = addTagEditsIn (cellOf (hsTags hs)) (bsTag seed) hs <> drawerEdits hs
    refused err = "this capture template does not splice: " <> T.pack (show err)
    cellOf = maybe "" (sliceSpan entry)

    -- INSIDE an existing drawer, else under the PLANNING LINE.  Measuring from the
    -- title line splices it BETWEEN the headline and its @SCHEDULED:@, where the
    -- planning line stops being read as one.
    drawerEdits hs = case hsProperties hs of
      Just sp -> [ (insertAt (pastLine entry (spanStart sp))
                   , rows (indentOf (T.drop (lineStart entry (spanStart sp)) entry))) ]
      Nothing -> [ (insertAt (pastLine entry (planningEnd hs))
                   , T.concat [ ":PROPERTIES:" <> eol, rows "", ":END:" <> eol ]) ]

    -- The three planning spans permute freely, so this is a maximum over the ends.
    planningEnd hs = foldl' max (titleLineEnd hs)
                       [ spanEnd sp | (_key, sp) <- presentPlanning hs ]
    rows indent = T.concat [ indent <> ":" <> key <> ": " <> value <> eol
                           | (key, value) <- [ (headlineIdProperty, bsId seed)
                                             , (captureProperty, bsStamp seed) ] ]

bareTemplate :: Text
bareTemplate = "* %?"

topEntry :: Text -> Bool
topEntry text = headingStars (T.takeWhile (/= '\n') text) == Just 1

-- | LINES as a config file's @#+TODO:@ block; an EMPTY block is the DELETION.
-- PARTS rides the SAME call, these being regions of one file: four calls would
-- be four writes under four digests each invalidating the last.
configEdits :: ConfigLayerFile -> Maybe [Text] -> ConfigParts -> Either Text [(Span, Text)]
configEdits layer asked parts
  | not (null strange) = Left ("not a #+TODO: line: " <> T.intercalate " · " strange)
    -- ABSENT lines leave the block standing; an EMPTY list is still the deletion.
  | isNothing asked    = partEdits
  | null lines'        = block []
    -- THE SPELLING IS CHECKED BEFORE THE COUNT, so a word org cannot read is
    -- named rather than reported as a block that came to nothing.
  | Left why <- spelt  = Left why
  | null declared      = Left declaresNothing
  | otherwise          = block lines'
  where
    doc      = lfText layer
    block ls = (todoLineEdits doc ls <>) <$> partEdits
    partEdits = concat <$> traverse (\s -> csEdits s doc parts) (settingsFor layer)
    lines'   = filter (not . T.null . T.strip) (fromMaybe [] asked)
    -- A LINE: the pragma test is a prefix one, so an entry carrying a newline of
    -- its own would write everything past it into the file unread.
    strange  = filter (\l -> not (isTodoPragma l) || T.isInfixOf "\n" l) lines'
    keywords = todoPragmas (T.unlines lines')
    declared = tkActive keywords <> tkInactive keywords
    -- Over the WORDS AS WRITTEN: `todoPragmas' answers what org would READ, which
    -- silently drops the very word the writer got wrong.
    spelt    = traverse keywordText (concatMap todoWords lines')

data ConfigParts = ConfigParts
  { cpViews    :: ![(Text, Text)]  -- ^ saved views by id, the system layer's alone; an id absent leaves that view.
  , cpColors   :: !(Maybe [(Text, [(Text, Text)])])
      -- ^ @#+GLANCE_STATE_COLORS:@ by theme, likewise; the empty list deletes the block.
  , cpTemplate :: !(Maybe Text)  -- ^ the capture template, which EVERY layer may carry.
  } deriving (Eq, Show)

noParts :: ConfigParts
noParts = ConfigParts [] Nothing Nothing

data SettingScope = TreeWide | PerLayer
  deriving (Eq, Show)

data ConfigSetting = ConfigSetting
  { csName  :: !Text          -- ^ the field a write names it by, per @SCHEMA.md@.
  , csScope :: !SettingScope
  , csEdits :: !(Text -> ConfigParts -> Either Text [(Span, Text)])
  }

-- | EVERY setting beside the cycle.  ORDER IS DATA: two absent pragmas insert
-- at one offset and 'Data.Org.Edit.applyEdits' resolves that in list order.
configSettings :: [ConfigSetting]
configSettings =
  [ ConfigSetting "views"    TreeWide viewPartEdits
  , ConfigSetting "colors"   TreeWide (\doc p -> Right (maybe [] (stateColorsEdits doc) (cpColors p)))
  , ConfigSetting "template" PerLayer (\doc p -> maybe (Right []) (captureTemplateEdits doc) (cpTemplate p))
  ]

settingsFor :: ConfigLayerFile -> [ConfigSetting]
settingsFor layer
  | isNothing (lfTag layer) = configSettings
  | otherwise               = [ s | s <- configSettings, csScope s == PerLayer ]

viewPartEdits :: Text -> ConfigParts -> Either Text [(Span, Text)]
viewPartEdits doc parts = concat <$> traverse one (cpViews parts)
  where
    one (vid, want) = case savedView vid of
      Just v  -> Right (viewEdits v doc want)
      Nothing -> Left ("no view is called " <> vid <> "; this build has "
                        <> T.intercalate ", " (map svId savedViews))

-- | The words a @#+TODO:@ line DECLARES, as WRITTEN.  The key and the bar are
-- structure — and the bar needs no space around it — where a @(k)@ fast key is
-- org's own and no part of the word.
todoWords :: Text -> [Text]
todoWords line =
  [ T.takeWhile (/= '(') w
  | chunk <- T.split (== '|') (T.drop 1 (T.dropWhile (/= ':') line))
  , w <- T.words chunk ]

declaresNothing :: Text
declaresNothing =
  "#+TODO: declares no keyword org would read: a keyword is letters and underscores, "
    <> "active states before the bar and done-like ones after it. "
    <> "*active* and *inactive* are the filter's group names, not keywords."

headlineSpans :: HeadlineRecord -> HeadlineSpans
headlineSpans = spans . hrHeadline

insertAt :: Int -> Span
insertAt at' = Span at' at'


viewJSON :: Text -> [HeadlineRecord] -> Value
viewJSON viewTitle records =
  viewJSONWith defaultSortChain viewTitle
               (mergeKeywords (map hrKeywords records)) records

-- | 'viewJSON' declaring CHAIN with PALETTE given.  A server passes the whole
-- store's palette: this page's rows would move the badge list on every page.
viewJSONWith :: SortChain -> Text -> TodoKeywords -> [HeadlineRecord] -> Value
viewJSONWith = viewJSONFor viewColumns builtinViews

builtinViews :: [(Text, Text)]
builtinViews = [ (svId v, viewQuery (svId v) noConfig) | v <- savedViews ]

viewJSONFor :: [ViewColumn] -> [(Text, Text)] -> SortChain -> Text -> TodoKeywords
            -> [HeadlineRecord] -> Value
viewJSONFor cols views chain viewTitle palette records = object
  (  [ "title" .= viewTitle, "columns" .= columnsFor cols palette
     , "actions" .= actions ]
  <> declaredSort chain
  <> declaredViews views
  <> [ "rows" .= map (rowJSONFor cols) records ])

declaredViews :: [(Text, Text)] -> [Pair]
declaredViews [] = []
declaredViews vs = ["views" .= [object ["name" .= n, "query" .= q] | (n, q) <- vs]]

declaredSort :: SortChain -> [Pair]
declaredSort []    = []
declaredSort chain =
  [ "sort" .= [ object [ "column" .= key, "ascending" .= asc ]
              | (key, asc) <- chain ] ]

actions :: [Value]
actions =
  [ object [ "key"     .= ("RET" :: Text)
           , "command" .= ("materialize" :: Text)
           , "label"   .= ("Materialize" :: Text) ] ]

viewJSONTextFor :: [ViewColumn] -> [(Text, Text)] -> SortChain -> Text
                -> TodoKeywords -> [HeadlineRecord] -> TL.Text
viewJSONTextFor cols views chain viewTitle palette =
  encodeToLazyText . viewJSONFor cols views chain viewTitle palette

-- | The view's columns in draw order.  ONE TABLE, so the four that must agree
-- cannot drift — 'columnsFor' declares, 'rowJSONFor' fills, 'filterKeys' names,
-- 'viewCells' joins into 'hrSearch'.  Every index downstream resolves by KEY.
viewColumns :: [ViewColumn]
viewColumns =
  [ ("state",     "State",     "badge", hrState)
  , ("priority",  "#",         "badge", hrPriority)
  , ("title",     "Title",     "text",  Just . hrTitle)
  , ("scheduled", "Scheduled", "text",  hrScheduled)
  , ("deadline",  "Deadline",  "text",  hrDeadline)
  , ("tag",       "Tags",      "text",  Just . sortedTagsCell . hrTags)
  ]

type ViewColumn = (Text, Text, Text, HeadlineRecord -> Maybe Text)

-- | NAMES as columns, matched CASE-INSENSITIVELY against the default view's
-- keys and headers; an unknown name is a CUSTOM column.  THE MINIMAL SET IS TITLE.
resolveColumns :: [Text] -> [ViewColumn]
resolveColumns names = withTitle (map pick names)
  where
    withTitle cols
      | any (\(key, _h, _k, _c) -> key == "title") cols = cols
      | otherwise = [ col | col@("title", _h, _k, _c) <- viewColumns ] <> cols
    pick wanted = fromMaybe (custom wanted) (lookup (T.toCaseFold wanted) builtins)
    builtins    = concat [ [ (T.toCaseFold key, col), (T.toCaseFold header, col) ]
                         | col@(key, header, _kind, _cell) <- viewColumns ]
    custom wanted = ( T.toCaseFold wanted, wanted, "text"
                    , \r -> customCell r (T.toCaseFold wanted) )

-- | R's value under a custom column NAME, folded.  The hidden properties are
-- NOT hidden here — a read-only cell rewrites nothing.
customCell :: HeadlineRecord -> Text -> Maybe Text
customCell r wanted
  | wanted == "closed" = sliceSpan (hrDoc r) <$> hsClosed (headlineSpans r)
  | otherwise          =
      listToMaybe [ v | (k, v) <- drawerPairs subtree (drawerSlice r subtree)
                      , T.toCaseFold k == wanted ]
  where subtree = subtreeText r

viewCells :: HeadlineRecord -> [Text]
viewCells r = [ fromMaybe "" (cell r) | (_key, _header, _kind, cell) <- viewColumns ]

filterKeys :: [Text]
filterKeys = [ key | (key, _header, _kind, _cell) <- viewColumns ]

columnsFor :: [ViewColumn] -> TodoKeywords -> [Value]
columnsFor cols palette =
  [ column key header kind (extra key) | (key, header, kind, _cell) <- cols ]
  where
    extra key = case key of
      "state"    -> [ "badges" .= badges palette, "values" .= stateValues ]
      "priority" -> [ "badges" .= priorityBadges, "values" .= priorityValues ]
      -- Declared rather than sampled: the renderer reads at most 40 cells, so a page
      -- with fewer than two tagged rows would find no list column at all.
      "tag"      -> [ "multi" .= True ]
      _          -> []

priorityCell :: Text -> Text
priorityCell letter = "[#" <> letter <> "]"

-- | And back, folded.  DISPLAY WEARS THE DECORATION, MATCHING READS THROUGH IT.
priorityLetter :: Text -> Text
priorityLetter value = T.toCaseFold (fromMaybe folded stripped)
  where folded   = T.strip value
        stripped = T.stripSuffix "]" =<< T.stripPrefix "[#" folded

priorityValues :: [Text]
priorityValues = map priorityCell ["A", "B", "C"]

priorityBadges :: [Value]
priorityBadges =
  [ badge Nothing (overridable "priority" letter
                               ("var(--g-priority-" <> showt i <> ")")) v
  | (i, v) <- zip [0 :: Int ..] priorityValues
  , let letter = T.filter isAsciiUpper v ]

-- | THE RESERVED METAS, WHOLE, and no BARE word is reserved.  The other half is
-- closed by two charset walls — 'keywordTextP' and 'tagText' — so none arrives as data.
data Meta = MActive | MInactive | MEmpty | MArchive | MNone | MToday | MAny
  deriving (Eq, Show, Enum, Bounded)

metas :: [Meta]
metas = [minBound .. maxBound]

metaWord :: Meta -> Text
metaWord = starred . bare
  where
    bare MActive   = "active"
    bare MInactive = "inactive"
    bare MEmpty    = "empty"
    bare MArchive  = T.toLower archiveTag
    bare MNone     = "none"
    -- A DATE VALUE rather than a cell predicate: it stands wherever a date
    -- literal stands and resolves to the request's own day ('Filter.onDay').
    bare MToday    = "today"
    -- AN ANCHOR rather than a cell predicate: it stands where a @ref:@\/@from:@
    -- row id stands and names EVERY ANCHOR AT ONCE, so @ref:*any*@ is the union
    -- of @ref:T@ over every row T ('Filter.anyMeta').
    bare MAny      = "any"

starred :: Text -> Text
starred word = "*" <> word <> "*"

activeMeta, inactiveMeta :: Text
activeMeta = metaWord MActive
inactiveMeta = metaWord MInactive

stateValues :: [Text]
stateValues = [activeMeta, inactiveMeta]


-- | A column object.  @sortable@ gates what a READER may sort by, so the shell honours it.
column :: Text -> Text -> Text -> [Pair] -> Value
column key header kind extra =
  object ([ "key" .= key, "header" .= header, "type" .= kind
          , "sortable" .= True ] <> extra)

-- | One row for the wire.  @linked@ is SPARSE — @true@ or absent — which keeps
-- it an addition to SCHEMA.md's Row rather than a field every row now owes.
rowJSON :: HeadlineRecord -> Value
rowJSON = rowJSONFor viewColumns

rowJSONFor :: [ViewColumn] -> HeadlineRecord -> Value
rowJSONFor cols r = object
  (  [ "id" .= hrId r
     , "cells" .= object [ Key.fromText key .= toJSON (cell r)
                         | (key, _header, _kind, cell) <- cols ] ]
  <> [ "linked" .= True | hrLinked r ]
  -- SPARSE like `linked`, so SCHEMA.md's Row stays additive.
  <> [ "repeats" .= cookie | Just cookie <- [repeatsOf r] ])

badges :: TodoKeywords -> [Value]
badges (TodoKeywords actives inactives) =
  group "active" "a" actives <> group "inactive" "i" inactives
  where group g slot ws =
          [ badge (Just g) (overridable "state" w (stateSlot slot i)) w
          | (i, w) <- zip [0 ..] ws ]

badge :: Maybe Text -> Text -> Text -> Value
badge group color value =
  object ([ "value" .= value, "color" .= color ] <> [ "group" .= g | Just g <- [group] ])

stateSlots :: Int
stateSlots = 4

prioritySlots :: Int
prioritySlots = length priorityValues

-- | The slot token.  A @var()@ rather than a hex: a theme switches without refetching.
stateSlot :: Text -> Int -> Text
stateSlot group i = "var(--g-state-" <> group <> showt (i `mod` stateSlots) <> ")"

-- | The colour a badge names, as a CSS fallback chain.  Keywords are letters and
-- underscores and priority values one letter, so neither can spell a slot's name.
overridable :: Text -> Text -> Text -> Text
overridable prefix value fallback =
  "var(--g-" <> prefix <> "-" <> value <> ", " <> fallback <> ")"
