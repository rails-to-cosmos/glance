-- | The query facade: load org files into rows, render them as table-view JSON,
-- write one headline's raw subtree back; wire is hand-built 'Value's (AGENTS.hs).
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
                    , Doctor (..)
                    , diagnose
                    , cleanDoctor
                    , doctorClean
                    , doctorJSON
                    , doctorWarnings
                    , Span (..)
                    , SortChain
                    , SubtreeEntry (..)
                    , TodoKeywords (..)
                    , WalkOptions (..)
                    , WriteFailure (..)
                    , writeRefusalText
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
                    , captureProperty
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
                    , pinnedDocument
                    , dayOf
                    , dayNamed
                    , dayWordIn
                    , dayWords
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
                    , eolOf
                    , DraftCargo (..)
                    , Inherited (..)
                    , noInheritance
                    , draftEntry
                    , draftKeywords
                    , draftPointLine
                    , draftRecord
                    , draftSeeded
                    , draftStates
                    , draftTemplate
                    , addLinkEdits
                    , editLinkEdits
                    , englishDay
                    , englishSpan
                    , keywordText
                    , monthWords
                    , expandTemplate
                    , filterKeys
                    , sortKeys
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
                    , plannedValue
                    , planningKeywords
                    , planningTimestamp
                    , unplanned
                    , priorityLetter
                    , priorityText
                    , readConfigLayers
                    , readsAsTimestamp
                    , recognizedKeywords
                    , untrailed
                    , recomposedSubtree
                    , drawerInsertEdit
                    , outlineEntries
                    , Ref (..)
                    , RefVia (..)
                    , Edge (..)
                    , EdgeIndex
                    , carriesKind
                    , edgeIndex
                    , edgePairs
                    , edgesInto
                    , edgesOutOf
                    , glanceLink
                    , kindCut
                    , kindSlug
                    , LinkPlace (..)
                    , linkPlaceOf
                    , linkPlaceWord
                    , linkPlaces
                    , linkTargetIn
                    , nameClaims
                    , namesRow
                    , neighborDepth
                    , neighborDepthCap
                    , neighborLimit
                    , neighborhood
                    , ownBody
                    , pointedAtBy
                    , pointsAt
                    , refNames
                    , referrersIn
                    , refSpellings
                    , refTargetOf
                    , refTargets
                    , refsCarrying
                    , removeTagEdits
                    , renameTagEdits
                    , replaceSpans
                    , rowSnapshot
                    , resolveIds
                    , rowIdIn
                    , rowJSON
                    , rowProperties
                    , rowSummaryJSON
                    , rowSummaryPairs
                    , summaryEnvelope
                    , setPlanningEdits
                    , setPriorityEdits
                    , setStateEdits
                    , Repeat (..)
                    , repeatOn
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
import Control.Monad (foldM)
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
                , firstHeadlineOf, forcedSpans, headlineIdProperty, headlinesOf, hsFull
                , identityOf
                , isKeywordChar, isTagChar, levelOf
                , metaCategory
                , orgIdentity, orgParse, priority, propertyLine, schedule, shiftSpan
                , sliceSpan, spans, spelled
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
import Data.Org.Doctor ( Doctor (..), cleanDoctor, corpusDoctor, doctorClean
                       , doctorJSON, doctorWarnings, scanCorpus )
import Data.Org.External (Completion (..), noteCompletion)
import Data.Org.Blob (blobPathIn, mintBlobId, storeRootIn, uuidFrom)
import Data.Org.Trash (trashBlob, trashDirIn, trashPathFor)
import Data.Org.Walk ( Found (..), LoadFailure (..), WalkOptions (..), claimById
                     , defaultWalk, findOrgFilesWith, isConfig, isDerived, isDocument
                     , mapFilesConcurrently )

import Data.Org.Edit (digestOfText, eolOf, lineSpansIn, linesWith, openingFor)

import qualified Data.Org.Edit as Edit
import qualified Data.Org.External as External


-- | One row: a headline's cells, and the spans that cut them out of its file.
--
-- A RECORD RETAINS NO DOCUMENT BYTES: text is read by path at request, pinned
-- against 'hrDigest' ('pinnedDocument'); every 'Text' kept is copied and forced.
data HeadlineRecord = HeadlineRecord
  { hrFile       :: !FilePath        -- ^ path the headline was read from, as walked.
  , hrId         :: !Text            -- ^ row identity: 'hrOrgId' where the headline carries one, else 'rowIdIn'.
  , hrOrgId      :: !(Maybe Text)    -- ^ the @ORG_GLANCE_ID@ property, or the one a broken drawer still spells ('identityOf').  THE LEDGER'S KEY: an ordinal names another row a week on.
  , hrIdProperty :: !(Maybe Text)    -- ^ org-id's own @:ID:@ property, the OTHER namespace — what an @id:@ link resolves against.
  , hrCategory   :: !Text            -- ^ the file's final @#+CATEGORY@, empty when unset.
  , hrLevel      :: !Int             -- ^ org's outline level; a stored row is 1, a subtree entry its own.
  , hrKeywords   :: !TodoKeywords    -- ^ every keyword the file's parse recognized — config seed included; one value shared per file.
  , hrDeclared   :: !TodoKeywords    -- ^ what the file's OWN @#+TODO:@ lines declare; the nearest scope, see 'keywordSources'.
  , hrDigest     :: !Text            -- ^ SHA-256 of the file's bytes as parsed, lowercase hex; one value shared per file.
  , hrSubtree    :: !Span            -- ^ the headline's outline extent in that text; see 'subtreeSpans'.
  , hrSpans      :: !HeadlineSpans   -- ^ where the headline's own parts sit in it; every edit's span math starts here.
  , hrState      :: !(Maybe Text)    -- ^ TODO keyword verbatim.
  , hrPriority   :: !(Maybe Text)    -- ^ priority letter, brackets dropped.
  , hrTitle      :: !Text            -- ^ title text as the file spells it.
  , hrTags       :: !Text            -- ^ @":a:b:"@ in FILE order, empty when untagged; the COLUMN sorts ('sortedTagsCell').
  , hrScheduled  :: !(Maybe Text)    -- ^ ISO date, see 'isoStamp'.
  , hrDeadline   :: !(Maybe Text)    -- ^ ISO date, see 'isoStamp'.
  , hrClosed     :: !(Maybe Text)    -- ^ the @CLOSED@ timestamp verbatim; a CUSTOM COLUMN reads it, so it is cut at parse.
  , hrDrawer     :: ![(Text, Text)]  -- ^ the headline's OWN drawer pairs in file order, the hidden keys KEPT; see 'drawerPairs'.
  , hrRepeats    :: ![(Span, TimestampRepeaterInterval)]  -- ^ where each repeating planning stamp sits and what it repeats by; the @repeats@ cookie rides every wire row.
  , hrSearch     :: !Text            -- ^ the cells as they display, lowercased; see 'searchTextOf'.
  , hrLinks      :: ![Ref]           -- ^ the references this subtree makes, normalized; see 'refTargets'.
  , hrLinked     :: !Bool            -- ^ does the subtree hold a link at all — what @o@ follows; see 'subtreeLinks'.
  , hrActive     :: !(Maybe Bool)    -- ^ whether 'hrState' is an active state HERE; see 'Data.Org.Config.classify'.
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

-- | The daemon's startup verdict on ROOTS.  Failures and id collisions come from
-- the store's own QR; span violations and index drift from a fresh scan.  The CLI
-- folds the SAME 'Doctor' off 'scanCorpus' ('corpusDoctor'), so the two never disagree.
diagnose :: WalkOptions -> [FilePath] -> QueryResult -> IO Doctor
diagnose opts roots qr = do
  corpus <- scanCorpus opts roots
  pure (corpusDoctor corpus)
    { docParseFailures  = qrParseFailures qr
    , docDecodeFailures = qrDecodeFailures qr
    , docReadFailures   = qrReadFailures qr
    , docIdCollisions   = length (qrIdCollisions qr)
    }


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

-- | PATH's top entries under CFG, or why it has none.  Rows come back FORCED, so the document is dropped (AGENTS.hs).
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

-- | The rows FILE contributes.  Extents over the WHOLE sequence, filters after — else a row ends at the next KEPT headline.
recordsOf :: ConfigLayers -> FilePath -> Text -> Text -> Context -> [Spanned Element]
          -> [HeadlineRecord]
recordsOf cfg path doc digest ctx elems =
  [ recordOf cfg declared path ordinal doc digest category keywords h subtree
  | (ordinal, (h, subtree)) <- zip [0 ..] entries ]
  where category = metaCategory ctx
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

-- | Has H nothing the table can show?  A span is 'Nothing' exactly where 'recordOf' cuts an empty cell.
blankEntry :: Headline -> Bool
blankEntry h = all isNothing [ hsTodo sp
                             , hsPriority sp
                             , hsTitle sp
                             , hsTags sp
                             , hsSchedule sp
                             , hsDeadline sp ]
  where sp = spans h

-- | One STORED row: 'recordWith' COPYING every 'Text' it keeps.
recordOf :: ConfigLayers -> TodoKeywords -> FilePath -> Int -> Text -> Text -> Text
         -> TodoKeywords -> Headline -> Span -> HeadlineRecord
recordOf = recordWith detach

-- | H at EXTENT in DOC as a row.  COPY keeps a 'Text' past the parse: 'detach' for a stored row, 'id' for a transient one.
recordWith :: (Text -> Text) -> ConfigLayers -> TodoKeywords -> FilePath -> Int
           -> Text -> Text -> Text -> TodoKeywords -> Headline -> Span
           -> HeadlineRecord
recordWith copy cfg declared path ordinal doc digest category keywords h extent =
  forceRecord (row { hrSearch = searchTextOf (viewCells row) })
  where
        -- The haystack is the view's columns by construction: 'viewCells' reads ROW.
        row = HeadlineRecord
          { hrFile       = path
          , hrId         = fromMaybe (rowIdIn path ordinal) orgId
          , hrOrgId      = orgId
          , hrIdProperty = orgIdentity h
          , hrCategory   = category
          , hrLevel      = levelOf h
          , hrKeywords   = keywords
          , hrDeclared   = declared
          , hrDigest     = digest
          , hrSubtree    = extent
          , hrSpans      = forcedSpans sp
          , hrState      = state
          , hrPriority   = pri
          , hrTitle      = titleCell
          , hrTags       = tagsCell
          , hrScheduled  = scheduled
          , hrDeadline   = due
          , hrClosed     = copy <$> cutting (hsClosed sp)
          , hrDrawer     = [ (copy key, copy value) | (key, value) <- drawer ]
          , hrRepeats    = repeats
          , hrSearch     = ""
          , hrLinks      = refTargetsOf copy links
          , hrLinked     = not (null links)
          , hrActive     = classify cfg declared (tagsOfCell tagsCell) <$> state
          }
        sp = spans h
        orgId = copy <$> identityOf h subtree
        -- CUT OUT OF THE SUBTREE, never the file: 'sliceSpan' walks from the
        -- text's start, so a cell cut at a file offset is one long scan each.
        subtree = sliceSpan doc extent
        cutting mspan = sliceSpan subtree <$> (mspan >>= localSpan extent subtree)
        cut mspan render = maybe render copy (cutting mspan)
        links = orgLinks subtree
        -- READ AT PARSE: a custom column reads it PER ROW PER REQUEST, and no query may touch the disk.
        drawer = drawerPairs subtree (drawerSlice extent subtree sp)
        repeats = [ (stampAt, i)
                  | (at, stamp) <- [ (hsSchedule, schedule), (hsDeadline, deadline) ]
                  , Just stampAt <- [at sp]
                  , Just ts      <- [stamp h]
                  , Just i       <- [tsInterval ts] ]
        state     = name <$> todo h
        pri       = (\(Priority c) -> priorityCell (T.singleton c)) <$> priority h
        titleCell = cut (hsTitle sp) (showt (title h))
        tagsCell  = cut (hsTags sp) (showt (tags h))
        scheduled = isoStamp <$> schedule h
        due       = isoStamp <$> deadline h


cellSep :: Char
cellSep = '\US'

-- | CELLS as one lowercase haystack — @table-view.js@'s own row text, to the byte.
searchTextOf :: [Text] -> Text
searchTextOf = T.toLower . T.intercalate (T.singleton cellSep) . map displayText

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


-- | Every link R's subtree points at in DOC, spanned in the DOCUMENT ('Data.Org.Edit').
subtreeLinks :: Text -> HeadlineRecord -> [OrgLink]
subtreeLinks doc r = map (shiftLink (spanStart (hrSubtree r))) (orgLinks (subtreeText doc r))

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

-- | The SCHEMES whose target is a HEADLINE ID: following one opens the material
--   doc.  'linkType' folds the family to "glance", so 'materialTypes' derives to that one word.
materialSchemes :: [Text]
materialSchemes = [writtenScheme, "org-glance-material", "org-glance-visit", "org-glance-open"]

-- | The scheme this server WRITES, first of the family it reads: org-glance's
-- own, so a link @add-link@ lands is a link org-glance follows.
writtenScheme :: Text
writtenScheme = "glance"

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


-- | The protocols naming a row, EACH BOUND TO ITS NAMESPACE, so a prefix cannot land without declaring where it resolves.
refPrefixes :: [(Text, RefVia)]
refPrefixes = [ (s <> ":", ViaRow) | s <- materialSchemes ]
           <> [("id:", ViaOrgId)]

-- | The NAMESPACE a reference resolves in.  @id:@ is org-id's protocol and names
--   the @:ID:@ PROPERTY; everything else names the row.  @id:@ over @ORG_GLANCE_ID@ would conflict with org-mode.
data RefVia = ViaRow | ViaOrgId
  deriving (Eq, Ord, Show)

-- | A reference AS RESOLVED: the row it names, and the KIND its author declared on
-- the edge.  The kind is the EDGE's, so it rides here not on 'HeadlineRecord'; 'Nothing' is a plain mention.
data Ref = Ref
  { refTarget :: !Text          -- ^ the id or title the link names, normalized.
  , refKind   :: !(Maybe Text)  -- ^ @?kind=SLUG@, as the author spelled it.
  , refVia    :: !RefVia        -- ^ the namespace the target lives in.
  } deriving (Eq, Ord, Show)

refTargets :: Text -> [Ref]
refTargets = refTargetsOf detach . orgLinks

-- | DEDUP IS ON THE PAIR, the peer's rule: two typed edges to a row are two references, two mentions one.  COPY is 'recordWith''s.
refTargetsOf :: (Text -> Text) -> [OrgLink] -> [Ref]
refTargetsOf copy = nub . map (refWith copy) . mapMaybe (refTargetOf . olTarget)

-- | A reference kept past the parse.  TARGET is a file slice, so COPIED; KIND is
-- 'kindSlug''s fresh text, so only FORCED — a thunk over it would retain the slice.
refWith :: (Text -> Text) -> Ref -> Ref
refWith copy (Ref t k v) = foldr seq (Ref (copy t) k v) k

refTargetOf :: Text -> Maybe Ref
refTargetOf target
    -- A KIND RIDES ON THE EDGE: @?kind=SLUG@ after the id, so the id alone names
    -- the row.  A TITLE is text, so its own @?@ stays; the NAMESPACE rides the prefix.
  | Just (via, rest) <- stripped = let (row, kind) = kindCut rest in plain row kind via
  | Just rest <- T.stripPrefix "*" target                       = plain rest Nothing ViaRow
  | T.any (\c -> c == ':' || c == '/') target                   = Nothing
  | otherwise                                                   = plain target Nothing ViaRow
  where
    stripped = listToMaybe [ (via, rest) | (p, via) <- refPrefixes
                                         , Just rest <- [T.stripPrefix p target] ]
    plain t k v = if T.null t then Nothing else Just (Ref t k v)

-- | A target cut into the ROW it names and the KIND its @?@ declares, at the FIRST
-- @?@.  ONE READING, TWO CALLERS ('refTargetOf' a link's target, the @ref:@\/@from:@
-- reader a token's value), so an edge and the token testing it never spell a kind two ways.
kindCut :: Text -> (Text, Maybe Text)
kindCut target = (row, kindIn (T.drop 1 query))
  where (row, query) = T.breakOn "?" target

-- | The @kind@ of a target's query string; an EMPTY one is no kind.  Only the peer's own key is read, nothing else.
kindIn :: Text -> Maybe Text
kindIn query =
  listToMaybe [ slug | part <- T.splitOn "&" query
                     , Just v <- [T.stripPrefix "kind=" part]
                     , let slug = kindSlug v, not (T.null slug) ]

-- | A kind CANONICALIZED to the PEER's rule (downcased, whitespace runs to one @-@;
-- @org-glance--kind-slug@, @src\/data\/org-glance-utils.el:183-187@, "invariant 13"), slugging on encode AND read.
kindSlug :: Text -> Text
kindSlug = T.intercalate "-" . T.words . T.toLower

refSpellings :: HeadlineRecord -> [Text]
refSpellings r = maybe id (:) (hrOrgId r) [hrTitle r]

-- | THE NAMES R ANSWERS TO, each bound to its namespace: 'refSpellings' answer
-- 'ViaRow', the @:ID:@ property answers 'ViaOrgId'.  ONE EQUATION FOR BOTH
-- DIRECTIONS: @ref:@ tests a link, @from:@ indexes rows — no drift into two rules.
refNames :: HeadlineRecord -> [(RefVia, Text)]
refNames r = [ (ViaRow, s) | s <- refSpellings r ]
          <> [ (ViaOrgId, o) | Just o <- [hrIdProperty r] ]

-- | Does a reference name ROW?  The link's namespace decides ('refNames').
-- PARTIALLY APPLIED AT COMPILE with the row fixed, so @ref:@ reads names once, not per row.
namesRow :: HeadlineRecord -> Ref -> Bool
namesRow row = \l -> (refVia l, refTarget l) `elem` names
  where names = refNames row

-- | Does an edge carrying E answer a token asking for KIND?  'Nothing' IS THE KIND-BLIND
-- READING every edge answers.  BOTH SIDES ARRIVE SLUGGED, so this compares canon to canon.
carriesKind :: Maybe Text -> Maybe Text -> Bool
carriesKind Nothing  _ = True
carriesKind (Just k) e = e == Just k

-- | R's references narrowed to those carrying KIND.  THE KIND-BLIND ARM TAKES THE LIST WHOLE, so a bare @ref:@ walks the same list.
refsCarrying :: Maybe Text -> HeadlineRecord -> [Ref]
refsCarrying Nothing        = hrLinks
refsCarrying k@(Just _kind) = filter (carriesKind k . refKind) . hrLinks

-- | Does a row POINT AT T over an edge carrying KIND?  @ref:T@'s test; T IS THE FIXED
-- END, read once at compile.  A ROW IS NEVER ITS OWN REFERENCE (the materialize self-link).
pointsAt :: Maybe Text -> HeadlineRecord -> HeadlineRecord -> Bool
pointsAt kind t = \r -> hrId r /= hrId t && any names (refsCarrying kind r)
  where names = namesRow t

-- | Is a row POINTED AT BY T over an edge carrying KIND?  @from:T@'s test, 'pointsAt'
-- from the other end and THE SAME EDGE under both.
pointedAtBy :: Maybe Text -> HeadlineRecord -> HeadlineRecord -> Bool
pointedAtBy kind t = \r -> hrId r /= hrId t && any (namesRow r) out
  where out = refsCarrying kind t

-- | ONE RESOLVED EDGE: the row that wrote the link, the row the link names, the
-- kind the edge declares ('Nothing' is a plain mention) and the namespace it
-- resolved through.  A LINK NAMING NO ROW IS NO EDGE and a row is never its own
-- reference, so both cuts are made once, here, and @refs@, @referrers@ and a
-- walk are one relation rather than three readings.
data Edge = Edge
  { edFrom :: !Text
  , edTo   :: !Text
  , edKind :: !(Maybe Text)
  , edVia  :: !RefVia
  } deriving (Eq, Ord, Show)

-- | The graph RECORDS spell, resolved ONCE per store version: the rows by id and
-- the edges read from both ends.  ONE PASS over every link, where a 'pointedAtBy'
-- per row is one pass per row.
data EdgeIndex = EdgeIndex
  { exRows :: Map.Map Text HeadlineRecord    -- ^ the rows the edges join, by id.
  , exOut  :: Map.Map Text (Set.Set Edge)    -- ^ a row, and what it points at.
  , exInto :: Map.Map Text (Set.Set Edge)    -- ^ a row, and what points at it.
  }

edgeIndex :: [HeadlineRecord] -> EdgeIndex
edgeIndex records = EdgeIndex
  { exRows = Map.fromList [ (hrId r, r) | r <- records ]
  , exOut  = Map.fromListWith (<>) [ (edFrom e, Set.singleton e) | e <- es ]
  , exInto = Map.fromListWith (<>) [ (edTo e, Set.singleton e) | e <- es ]
  }
  where es = resolvedEdges records

-- | EVERY LINK IN RECORDS RESOLVED THROUGH 'nameClaims': one edge per row the
-- link's own namespace claims, the self-edge dropped.
resolvedEdges :: [HeadlineRecord] -> [Edge]
resolvedEdges records =
  [ Edge (hrId r) to (refKind l) (refVia l)
  | r <- records, l <- hrLinks r
  , to <- Set.toList (Map.findWithDefault Set.empty (refVia l, refTarget l) claims)
  , to /= hrId r ]
  where claims = nameClaims records

-- | A NAME → THE ROWS CLAIMING IT, each in its own namespace ('refNames'): the
-- forward half, which is what a link resolves THROUGH.  ONE INDEX for the graph
-- and for @ref:*any*@, so "a row is never its own reference" has one spelling.
nameClaims :: [HeadlineRecord] -> Map.Map (RefVia, Text) (Set.Set Text)
nameClaims records = Map.fromListWith (<>)
  [ (n, Set.singleton (hrId r)) | r <- records, n <- refNames r ]

edgesOutOf, edgesInto :: EdgeIndex -> Text -> Set.Set Edge
edgesOutOf ix i = Map.findWithDefault Set.empty i (exOut ix)
edgesInto  ix i = Map.findWithDefault Set.empty i (exInto ix)

-- | The rows pointing at R, @ref:ID@'s answer read off IX.
referrersIn :: EdgeIndex -> HeadlineRecord -> [Text]
referrersIn ix r = Set.toAscList (Set.map edFrom (edgesInto ix (hrId r)))

-- | R's edges as the @edges@ rider carries them: @refs@ out and @referrers@ in,
-- both RESOLVED, so every id here addresses a row this server serves.
edgePairs :: EdgeIndex -> HeadlineRecord -> [Pair]
edgePairs ix r =
  [ "refs" .= [ refJSON e | e <- Set.toAscList (edgesOutOf ix (hrId r)) ]
  , "referrers" .= referrersIn ix r ]

-- | ONE reference for the wire: the row it points at, the kind the edge declares
-- (@null@ for a plain mention) and the namespace it resolved in.
refJSON :: Edge -> Value
refJSON e = object ["to" .= edTo e, "kind" .= edKind e, "via" .= viaWord (edVia e)]

-- | The wire's word for a namespace.  ONE spelling, so every reader agrees.
viaWord :: RefVia -> Text
viaWord ViaRow   = "row"
viaWord ViaOrgId = "org-id"

-- | THE THREE NUMBERS A @neighbors@ WALK IS BOUND BY, beside the walk itself:
-- the most hops one may take, the hops it takes when the caller names none, and
-- the nodes it serves when the caller caps none.
neighborDepthCap, neighborDepth, neighborLimit :: Int
neighborDepthCap = 3
neighborDepth = 1
neighborLimit = 200

-- | The subgraph around IDENT: the rows within DEPTH hops of it either way, and
-- every edge carrying KIND between them.  Breadth-first over IX, which the store
-- built once, so a walk costs no pass over the records and no round trip a hop.
--
-- DEPTH IS WHAT BOUNDS THE WORK and CAP only what is drawn, so @total@ is the
-- count before the cap and a @limit@ is honest — 'summaryEnvelope''s own rule.
-- The edges are every one BETWEEN HELD NODES, the last layer's to each other
-- included: a subgraph, not a tree.
neighborhood :: Maybe Text -> Int -> Int -> EdgeIndex -> Text -> Either Text Value
neighborhood kind depth cap ix ident
  | Map.notMember ident (exRows ix) = Left (ident <> " is no row in this tree")
  | otherwise = Right (object
      [ "total" .= Set.size reached
      , "nodes" .= [ rowSummaryJSON r | i <- Set.toAscList held
                                      , Just r <- [Map.lookup i (exRows ix)] ]
      , "edges" .= [ edgeJSON e | e <- Set.toAscList drawn ] ])
  where
    reached = walk depth (Set.singleton ident) (Set.singleton ident)
    held    = Set.fromList (take cap (Set.toAscList reached))
    drawn   = Set.filter (\e -> Set.member (edTo e) held)
                         (foldMap (kinded . edgesOutOf ix) (Set.toList held))
    -- BOTH ENDS EVERY HOP: `from:' out of the row and `ref:' into it, the two
    -- keys the query language already reads, iterated here instead of over HTTP.
    hops i  = Set.map edTo (kinded (edgesOutOf ix i))
                <> Set.map edFrom (kinded (edgesInto ix i))
    kinded  = Set.filter (carriesKind kind . edKind)
    walk d frontier seen
      | d <= 0 || Set.null fresh = seen
      | otherwise                = walk (d - 1) fresh (seen <> fresh)
      where fresh = Set.filter (`Set.notMember` seen) (foldMap hops (Set.toList frontier))

edgeJSON :: Edge -> Value
edgeJSON e = object ["from" .= edFrom e, "to" .= edTo e, "kind" .= edKind e]

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
sortCell palette key = read' <$> lookup key [(k, cell) | (k, _, _, cell) <- sortColumns]
  where
    ranked = paletteRank palette
    read' cell r = case cell r of
      Just value | not (T.null value) -> Just (rank value, text' value)
      _empty                          -> Nothing
    -- The priority cell wears org's brackets, so the comparator reads through them.
    rank value  = if key == "state" then ranked value else 0
    -- 'created' needs no case of its own: its inactive-stamp cell wears a constant
    -- '[' affix, so the plain-text branch sorts it chronologically like bare ISO.
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


-- | R's outline extent cut out of DOC — read by path and pinned ('pinnedDocument'), since a record keeps none of it.
subtreeText :: Text -> HeadlineRecord -> Text
subtreeText doc r = sliceSpan doc (hrSubtree r)

data SubtreeEntry = SubtreeEntry
  { seLevel  :: !Int             -- ^ org's outline level; the row's own is 1.
  , seParent :: !Int             -- ^ the index it hangs under, @-1@ being the row itself.
  , seRecord :: !HeadlineRecord  -- ^ the entry as a record: cells, extent, digest.
  } deriving (Show)

-- | R's descendants in DOC: one re-parse per call from the load's seed.  The entries are TRANSIENT, built with no copy ('recordWith').
subtreeEntries :: ConfigLayers -> Text -> HeadlineRecord -> [SubtreeEntry]
subtreeEntries cfg doc r = case orgParse (seedContext cfg) doc of
  (_elems, _ctx, Just _err) -> []
  (elems, _ctx, Nothing)    -> parented (zip [0 ..] (inside elems))
  where
    outer = hrSubtree r
    inside elems =
      [ (levelOf h, h, sub)
      | (h, sub) <- outlineEntries doc elems
      , spanStart sub > spanStart outer, spanStart sub < spanEnd outer ]
    made k (_lvl, h, sub) =
      (recordWith id cfg (hrDeclared r) (hrFile r) k doc (hrDigest r)
                  (hrCategory r) (hrKeywords r) h sub)
        { hrId = hrId r <> "/" <> T.pack (show k) }
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

-- | How many lines of BODY are R's OWN.  By DIFFERENCE, not a leading @*@ (which cuts short at a @*bold*@ line).
ownBodyLines :: Text -> HeadlineRecord -> Text -> Maybe HeadlineRecord -> Int
ownBodyLines doc r body first' = case first' of
  Nothing     -> whole
  Just deeper -> whole - length (linesWith (T.drop (cut deeper) (subtreeText doc r)))
  where whole = length (linesWith body)
        cut deeper = spanStart (hrSubtree deeper) - spanStart (hrSubtree r)

-- Lens
--
-- ONE OWNER PER BYTE: planning line, OWN drawer and OWN logbook are lifted; every byte left is the body's.

hiddenProperties :: [Text]
hiddenProperties = [headlineIdProperty, captureProperty]

hiddenProperty :: Text -> Bool
hiddenProperty key = T.toUpper (T.strip key) `elem` hiddenProperties

-- | PAIRS less the server's own.  ONE spelling of the filter: every drawer reader drops the same keys.
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

headlineParts :: Text -> HeadlineRecord -> HeadlineParts
headlineParts doc r = HeadlineParts
  { hpBody       = withoutSpans subtree (regionSpans [planAt, drawAt, logAt])
  , hpProperties = shownPairs (drawerPairs subtree drawAt)
  , hpPlanning   = [ (key, sliceSpan subtree sp) | (key, sp) <- entries ]
  , hpLogbook    = maybe "" (sliceSpan subtree) logAt
  }
  where (subtree, entries, planAt, drawAt, logAt) = regionsOf doc r

regionsOf :: Text -> HeadlineRecord
          -> (Text, [(Text, Span)], Maybe Span, Maybe Span, Maybe Span)
regionsOf doc r = (subtree, entries, planAt, drawAt, logAt)
  where subtree = subtreeText doc r
        entries = planningEntries r subtree
        planAt  = planningSlice entries subtree
        drawAt  = drawerSlice (hrSubtree r) subtree (hrSpans r)
        logAt   = logbookSlice drawAt subtree

recomposedSubtree :: Text -> HeadlineRecord -> HeadlineParts -> Text
recomposedSubtree doc r parts = untrailed (spliceRegions (hpBody parts) regions)
  where
    (subtree, entries, planAt, drawAt, logAt) = regionsOf doc r
    -- Body coordinates: subtree line less the lines regions ahead cleared; indices leave a GAP where one was removed.
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


-- | Where the OWN drawer sits inside SUBTREE (extent OUTER, spans HS).  SPANS not a ROW: 'recordWith' cuts pairs pre-record.
drawerSlice :: Span -> Text -> HeadlineSpans -> Maybe Span
drawerSlice outer subtree hs = do
  sp <- hsProperties hs
  Span from to <- localSpan outer subtree sp
  pure (Span from (pastLine subtree to))

planningSlice :: [(Text, Span)] -> Text -> Maybe Span
planningSlice entries subtree = case map snd entries of
  []  -> Nothing
  sps -> Just (Span (lineStart subtree (minimum (map spanStart sps)))
                    (pastLine subtree (maximum (map spanEnd sps))))

planningEntries :: HeadlineRecord -> Text -> [(Text, Span)]
planningEntries r subtree = sortOn (spanStart . snd)
  [ (key, local)
  | (key, sp) <- presentPlanning (hrSpans r)
  , Just local <- [localSpan (hrSubtree r) subtree sp] ]

presentPlanning :: HeadlineSpans -> [(Text, Span)]
presentPlanning hs =
  [ (key, sp)
  | (key, Just sp) <- zip planningKeywords [hsSchedule hs, hsDeadline hs, hsClosed hs] ]

-- | Where R's OWN logbook sits (SKIP the property drawer), located TEXTUALLY: a @:LOGBOOK:@ is no part of the parse.
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

-- | SP (a document span) in SUBTREE's coordinates, OUTER where SUBTREE sits; 'Nothing' where it does not fit.
localSpan :: Span -> Text -> Span -> Maybe Span
localSpan outer subtree sp
  | from < 0 || to > T.length subtree || from > to = Nothing
  | otherwise                                      = Just local
  where local@(Span from to) = shiftSpan (negate (spanStart outer)) sp


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

-- | Where the entry KEY opens in TEXT: the LAST @KEY:@ ahead of AT, only horizontal space between.  Line-bounded.
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

-- | Is VALUE a timestamp org reads back?  A value that does not reparse becomes body text on the next load.
readsAsTimestamp :: Text -> Bool
readsAsTimestamp value = either (const False) (isJust . timestampOf) (oneLine () () value)


drawerPairs :: Text -> Maybe Span -> [(Text, Text)]
drawerPairs subtree slice = case slice of
  Nothing -> []
  Just sp -> [ (key, value) | (key, value, _raw) <- drawerRows (sliceSpan subtree sp) ]

-- | R's OWN drawer pairs in file order, hidden keys dropped — the pairs
-- 'headlineParts' answers under @hpProperties@, READ OFF THE ROW.  NO QUERY
-- TOUCHES THE DISK, so they were cut at parse ('hrDrawer').
rowProperties :: HeadlineRecord -> [(Text, Text)]
rowProperties = shownPairs . hrDrawer

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

-- | BLOCK's property lines: key, value, raw.  Split by line, not 'Properties', which uppercases keys and re-tokenises.
drawerRows :: Text -> [(Text, Text, Text)]
drawerRows block = [ (key, value, raw) | raw <- inner (linesWith block)
                                       , let (key, value) = propertyOf raw ]
  where inner ls = drop 1 (take (length ls - 1) ls)

-- | 'propertyLine' with a total answer: a line spelling no property is all value.
propertyOf :: Text -> (Text, Text)
propertyOf line = fromMaybe ("", T.strip line) (propertyLine line)

pastLine :: Text -> Int -> Int
pastLine t at = maybe (T.length t) (\i -> at + i + 1) (T.findIndex (== '\n') (T.drop at t))

lineStart :: Text -> Int -> Int
lineStart t at = T.length (fst (T.breakOnEnd "\n" (T.take at t)))

indentOf :: Text -> Text
indentOf = T.takeWhile horizontal

-- | TEXT with the horizontal run ending each line taken off — ONLY THE LINE END (space inside a line is content).
-- 'T.stripEnd' would take a CRLF line's @\\r@ too.
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

-- | KW forced, which makes a keyword set safe to STORE: an unforced set pins its file's whole element tree.
forcedKeywords :: TodoKeywords -> TodoKeywords
forcedKeywords kw = forcing (tkActive kw <> tkInactive kw) kw

-- | H's row identity: @ORG_GLANCE_ID@, else @"FILE#K"@ with K its place among EMITTED ROWS.  One namespace, one string.
rowIdIn :: FilePath -> Int -> Text
rowIdIn path ordinal = T.pack path <> "#" <> T.pack (show ordinal)

isoStamp :: Timestamp -> Text
isoStamp ts = spelled fmt (tsmTime moment)
  where moment = tsStart ts
        fmt | tsmHasTime moment = "%Y-%m-%d %H:%M"
            | otherwise         = "%Y-%m-%d"

-- | DAY as the date a cell carries.  ONE FORMATTER for both sides of a comparison, so no reader compares two spellings.
isoDay :: Time.Day -> Text
isoDay = spelled "%Y-%m-%d"

-- | The day L spells, 'isoDay' READ BACKWARDS.  'Nothing' where L names no day, a month or a timed stamp.
dayOf :: Text -> Maybe Time.Day
dayOf = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" . T.unpack

-- | THE DAY WORDS, each offset from the request's day, off the CLOCK not ISO's digits.  @*today*@ is @today@'s OLD
-- spelling, READ AND NEVER OFFERED so stored views survive the rename ('Glance.Web.Filter.todayMeta').
dayWords :: [(Text, Integer)]
dayWords = [("today", 0), ("tomorrow", 1), (metaWord MToday, 0)]

-- | The day W names against the CLOCK read; 'Nothing' where W is no day word, or NO CLOCK WAS READ ('emptyEnv').
dayWordIn :: Maybe Time.Day -> Text -> Maybe Time.Day
dayWordIn today w = do
  n   <- lookup w dayWords
  day <- today
  pure (Time.addDays n day)

-- | The day W names against TODAY: EMPTY TEXT and 'dayWords' off the clock, else the ISO day ('dayOf').  THE ONE
-- BASE READER for the filter's date literals ('Glance.Web.Filter.dayIn') and the planning wall ('planningTimestamp'),
-- a word one takes the other takes.
dayNamed :: Time.Day -> Text -> Maybe Time.Day
dayNamed today w
  | T.null w  = Just today
  | otherwise = dayWordIn (Just today) w <|> dayOf w

-- | The unit LETTERS a date shift may carry — ORG'S WHOLE CHARSET off the parser's map, so a unit org grows is offered.
shiftUnits :: [Char]
shiftUnits = map unitChar [minBound .. maxBound]

-- | DAY moved N of the unit C names, ORG'S OWN CALENDAR ARITHMETIC ('addUnit'):
-- a month or year is CLIPPED to the target month's last day.  'Nothing' where C names no unit.
shiftDay :: Char -> Integer -> Time.Day -> Maybe Time.Day
shiftDay c n day = (\u -> addUnit u n day) <$> unitOf c

-- | THE SIGN A TOKEN OPENS WITH: the scanner reads the FIRST CHARACTER alone, so a second sign is body text.
data Sign
  = Unsigned  -- ^ the token opened with neither sign.
  | Neg       -- ^ the token opened with @-@.
  | Add       -- ^ the token opened with @+@.
  deriving (Eq, Show)

-- | The sign C opens a token with, or 'Nothing' for body text.  ONE CHARSET: 'shiftIn' and the filter's scanner
-- ('Glance.Web.Filter.scanQuery', re-exporting this) both read it, so @+@ and @-@ are spelled in one place.
signOf :: Char -> Maybe Sign
signOf '-' = Just Neg
signOf '+' = Just Add
signOf _   = Nothing

-- | HOW FAR A SIGN CARRIES a shifted day.  ONE EQUATION PER CONSTRUCTOR, no wildcard; 'Unsigned' stands still.
shiftWay :: Sign -> Integer
shiftWay Unsigned = 0
shiftWay Add      = 1
shiftWay Neg      = -1

-- | A trailing SHIFT read off L: the BASE, SIGNED count, unit.  READ FROM THE END, so a date's own hyphens are never
-- mistaken for the sign (@2026-09-15-7d@ is the week before).  THE ONE SHIFT GRAMMAR — the filter's shifted literals
-- ('Glance.Web.Filter.literalIn', docs/query.md) and the planning wall ('planningTimestamp') both read it, sign for sign.
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

-- | R with every cell evaluated.  'hrLinks' is a LIST, so its SPINE is forced too — a lazy tail retains the document.
forceRecord :: HeadlineRecord -> HeadlineRecord
forceRecord r =
  forcing texts (forcing (hrLinks r) (forcing repeats (foldr seq r (hrActive r))))
  where
    -- THE RESIDENCY LAW'S ENFORCEMENT: a thunk over the parse retains the whole document, so a field a row KEEPS belongs here.
    texts    = hrId r : hrCategory r : hrTitle r : hrTags r : hrDigest r : hrSearch r
                 : optional <> drawer
    optional = catMaybes [ hrState r, hrPriority r, hrScheduled r, hrDeadline r
                         , hrClosed r, hrOrgId r, hrIdProperty r ]
    drawer   = concat [ [key, value] | (key, value) <- hrDrawer r ]
    repeats  = [ at `seq` i | (at, i) <- hrRepeats r ]


-- | Why a 'replaceSpans' did not land.  Either way the file is byte-identical to before the call (AGENTS.hs).
data WriteFailure
  = WriteDrift !Text    -- ^ the digest the file holds now, which is not the pinned one.
  | WriteRefused !Text  -- ^ read, decode, splice or rename trouble, spelled for a caller to show.
  deriving (Eq, Show)

-- | PATH's text and digest, or @("", "")@ — the EMPTY pin, under which a write creates.
currentDocument :: FilePath -> IO (Text, Text)
currentDocument = fmap (fromMaybe ("", "")) . Edit.readDocument

-- | R's file as the drift lock names it: the path, and the digest its parse took.
rowSnapshot :: HeadlineRecord -> Edit.Snapshot
rowSnapshot r = Edit.Snapshot (hrFile r) (hrDigest r)

-- | SNAP's file as it stands, or why it gave nothing.  ONE PIN CHECK, shared with 'replaceSpans''s opening read, so
-- reader and writer agree to the byte on when a file has moved — drift told apart from unreadable or undecodable.
pinnedDocument :: Edit.Snapshot -> IO (Either WriteFailure Text)
pinnedDocument snap =
  either (Left . writeFailure (Edit.snapPath snap)) Right <$> Edit.currentText snap

-- | An 'Data.Org.Edit' trouble as a caller shows it.
-- | A 'WriteFailure' spelled for a person: the drift digest and do-nothing, or the
-- refusal's sentence.  ONE renderer, so command and migration cannot word one event two ways.
writeRefusalText :: FilePath -> WriteFailure -> Text
writeRefusalText path (WriteDrift found) =
  T.pack path <> " changed on disk (it digests to " <> T.take 12 found
    <> "\8230 now); nothing was written to it"
writeRefusalText _path (WriteRefused why) = why

writeFailure :: FilePath -> Edit.EditIOError -> WriteFailure
writeFailure path err = case err of
  Edit.Drift _path _pinned found -> WriteDrift found
  Edit.ReadFailed _path why      -> WriteRefused ("cannot read " <> named <> ": " <> why)
  Edit.DecodeFailed _path        -> WriteRefused (named <> " is not valid UTF-8")
  Edit.Rejected editError        -> WriteRefused ("the edit does not apply to " <> named
                                                   <> ": " <> T.pack (show editError))
  Edit.WriteFailed _path why     -> WriteRefused ("cannot write " <> named <> ": " <> why)
  where named = T.pack path

-- | Replace each span of FILE, provided it still digests to DIGEST.  THE DOOR
-- every write leaves through, so the note to org-glance is taken here (AGENTS.hs).
replaceSpans :: FilePath -> Text -> [(Span, Text)] -> IO (Either WriteFailure Text)
replaceSpans path digest edits = do
  written <- Edit.editFile (Edit.Snapshot path digest) [ Edit.Edit sp new | (sp, new) <- edits ]
  either (pure . Left . writeFailure path) noted written
  where
    noted receipt = do
      External.noteExternalWrite path (Edit.receiptText receipt)
      pure (Right (Edit.snapDigest (Edit.receiptSnapshot receipt)))

-- Commands
--
-- Span edits over the text a record was parsed from; nothing here reads or writes a file (a caller hands 'replaceSpans').


tagged :: Text -> HeadlineRecord -> Bool
tagged tag = \r -> want `elem` tagsOfCell (hrTags r)
  where want = T.toLower tag

archived :: HeadlineRecord -> Bool
archived = tagged archiveTag

-- | TEXT as an org tag.  The charset is the PARSER's ('isTagChar'): a bad run falls into title text on the next load.
tagText :: Text -> Either Text Text
tagText text
  | T.null text            = Left "a tag is at least one character"
  | T.all isTagChar text   = Right text
  | otherwise              = Left (text <> " is not an org tag: a tag is letters,"
                                     <> " digits, and _ - @ # or %")

-- | TEXT as a TODO keyword.  The charset is the PARSER's ('isKeywordChar'): a word org won't read back declares nothing.
keywordText :: Text -> Either Text Text
keywordText text
  | T.null text              = Left "a state is at least one character"
  | T.all isKeywordChar text = Right text
  | otherwise                = Left (text <> " is not a TODO state: a state is"
                                       <> " letters and _")

-- | The chain classifying ROWS, one per SOURCE.  DEDUP IS THE CLASSIFICATION RULE: file and tag land in the WIDER (AGENTS.hs).
keywordSources :: ConfigLayers -> [HeadlineRecord] -> [(Text, TodoKeywords)]
keywordSources cfg rows = keywordChain
  [ scope | r <- rows, scope <- keywordScopes cfg filed (tagsOfCell (hrTags r)) ]
  where filed = mergeKeywords (map hrDeclared rows)

-- | The chain a capture under TAGS stands in — @\/keywords@' shape for a rowless draft; the FILE scope is empty (no file yet).
draftKeywords :: ConfigLayers -> [Text] -> [(Text, TodoKeywords)]
draftKeywords cfg worn = keywordChain (keywordScopes cfg noKeywords worn)

-- | SCOPES ranked and deduplicated, one entry per source.  DEDUP IS THE
-- CLASSIFICATION RULE (AGENTS.hs): both callers classify by one fold.
keywordChain :: [(Int, Text, TodoKeywords)] -> [(Text, TodoKeywords)]
keywordChain scopes = widest Set.empty (sortOn fst [ (rank, (source, kw))
                                                   | (rank, source, kw) <- scopes ])
  where
    widest _seen [] = []
    widest seen ((_rank, (source, kw)) : rest)
      | null actives && null inactives = widest seen rest
      | otherwise = (source, TodoKeywords actives inactives) : widest taken rest
      where actives   = filter unseen (tkActive kw)
            inactives = filter unseen (tkInactive kw)
            unseen w  = not (Set.member w seen)
            taken     = foldr Set.insert seen (actives <> inactives)

-- | @set-state@'s edits.  KEYWORD is refused unless R's OWN CHAIN declares it.
setStateEdits :: ConfigLayers -> Maybe Text -> Text -> HeadlineRecord
              -> Either Text [(Span, Text)]
setStateEdits _cfg Nothing doc r = Right (tokenEdits hsTodo (spanEnd . hsStars) Nothing doc r)
setStateEdits cfg (Just keyword) doc r
  | keyword `notElem` settable =
      Left (keyword <> " is not a TODO keyword for " <> hrId r <> " in " <> T.pack (hrFile r)
              <> "; that row may be set to " <> T.intercalate ", " settable)
  | otherwise = Right (tokenEdits hsTodo (spanEnd . hsStars) (Just keyword) doc r)
  where settable = settableStates cfg r

-- | The token AT set to TOKEN, PLACE where one goes on a bare headline.  'Nothing' deletes it WITH the horizontal run.
tokenEdits :: (HeadlineSpans -> Maybe Span) -> (HeadlineSpans -> Int)
           -> Maybe Text -> Text -> HeadlineRecord -> [(Span, Text)]
tokenEdits at place token doc r = case (at hs, token) of
  (Just sp, Just new) -> [(sp, new)]
  (Just sp, Nothing)  -> [(Span (spanStart sp) (spanEnd sp + trailing sp), "")]
  (Nothing, Just new) -> [(insertAt (place hs), " " <> new)]
  (Nothing, Nothing)  -> []
  where hs = hrSpans r
        trailing sp = runWidth (T.drop (spanEnd sp) doc)

-- | The states R may be set to: 'keywordSources' flattened, so offer and wall agree.
settableStates :: ConfigLayers -> HeadlineRecord -> [Text]
settableStates cfg r = flatKeywords (keywordSources cfg [r])

-- | The states a capture under TAGS may be set to: 'draftKeywords' flattened by
-- the fold 'settableStates' uses, so the draft door OFFERS what the commit door WALLS with.
draftStates :: ConfigLayers -> [Text] -> [Text]
draftStates cfg worn = flatKeywords (draftKeywords cfg worn)

flatKeywords :: [(Text, TodoKeywords)] -> [Text]
flatKeywords chain = [ word | (_source, kw) <- chain, word <- tkActive kw <> tkInactive kw ]


data Repeat = Repeat
  { rpState   :: !Text            -- ^ the keyword the entry lands on.
  , rpShifted :: !Text            -- ^ its next occurrence, cookie and all.
  , rpEdits   :: ![(Span, Text)]  -- ^ the shift and the reset, as one set.
  } deriving (Eq, Show)

-- | R completed into KEYWORD, else 'Nothing'.  ORG'S OWN CONDITION: an INACTIVE
-- keyword AND a stamp with a repeater.  ONE EDIT SET; the reset is the chain's first ACTIVE word.
repeatOn :: ConfigLayers -> Time.Day -> Text -> Text -> HeadlineRecord -> Maybe Repeat
repeatOn cfg today keyword doc r
  | keyword `notElem` chainOf tkInactive = Nothing
  | null shifts                          = Nothing
  | otherwise = Just Repeat { rpState = fromMaybe "" reset
                            , rpShifted = snd (head shifts)
                            , rpEdits = shifts <> tokenEdits hsTodo (spanEnd . hsStars) reset doc r }
  where
    shifts = [ (sp, rewriteDates (repeatDay today i) (sliceSpan doc sp))
           | (sp, i) <- hrRepeats r ]
    reset  = listToMaybe (chainOf tkActive)
    chain  = keywordSources cfg [r]
    chainOf half = [ word | (_source, kw) <- chain, word <- half kw ]

timestampOf :: Text -> Maybe Timestamp
timestampOf text = case orgParse defaultContext ("* probe\nSCHEDULED: " <> text <> "\n") of
  (elems, _ctx, Nothing) -> listToMaybe [ ts | e <- elems, EHeadline h <- [valueOf e]
                                             , Just ts <- [schedule h] ]
  _failed                -> Nothing

repeatsOf :: HeadlineRecord -> Maybe Text
repeatsOf r = listToMaybe [ repeaterFormat i | (_sp, i) <- hrRepeats r ]

-- | DAY one repeat on under INTERVAL.  A zero-width interval takes the `+N` arm, else the `++` loop would not end.
repeatDay :: Time.Day -> TimestampRepeaterInterval -> Time.Day -> Time.Day
repeatDay today interval day
  | repeaterValue interval <= 0   = day
  | otherwise = case repeaterType interval of
      Restart    -> once day
      Cumulative -> once today
      CatchUp    -> until (> today) once day
  where
    once = addUnit (repeaterUnit interval) (fromIntegral (repeaterValue interval))

-- | TEXT with every date moved one repeat on.  TEXTUAL: time, cookies and a range's second half stay the author's.
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
    -- VARIABLE WIDTH, like `tsDayParser': a fixed ten-char window would cut `<2026-08-8 Sat>' short and eat the space.
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
titleSpan = hsTitle . hrSpans

oneLine :: e -> e -> Text -> Either e Text
oneLine empty many text
  | T.null want          = Left empty
  | T.any (== '\n') want = Left many
  | otherwise            = Right want
  where want = T.strip text

titleText :: Text -> Either Text Text
titleText = oneLine "a headline needs a title: the text after the keyword"
                    "a title is one line: the rest of the headline's own line"

-- | @set-title@'s edits.  'titleLineEnd' cannot serve: past its 'hsTags' a title reads back as tag text.
setTitleEdits :: Text -> Text -> HeadlineRecord -> Either Text [(Span, Text)]
setTitleEdits text doc r = do
  want <- titleText text
  pure $ case hsTitle hs of
    Just sp -> [(sp, want)]
    Nothing -> case [ spanEnd sp | Just sp <- [hsPriority hs, hsTodo hs] ] of
      (at : _rest) -> [(insertAt at, " " <> want)]
      []           -> [(insertAt (pastRun (spanEnd (hsStars hs))), want)]
  where hs = hrSpans r
        pastRun at = at + runWidth (T.drop at doc)

priorityText :: Text -> Either Text Text
priorityText text
  | T.length want == 1, T.all isAsciiUpper want = Right want
  | otherwise = Left (text <> " is not a priority: org spells one as a single"
                        <> " letter, A to C in its own cycle")
  where want = T.toUpper (T.strip text)

setPriorityEdits :: Maybe Text -> Text -> HeadlineRecord -> Either Text [(Span, Text)]
setPriorityEdits Nothing doc r = Right (tokenEdits hsPriority afterKeyword Nothing doc r)
setPriorityEdits (Just letter) doc r = do
  want <- priorityText letter
  pure (tokenEdits hsPriority afterKeyword (Just (priorityCell want)) doc r)

afterKeyword :: HeadlineSpans -> Int
afterKeyword hs = maybe (spanEnd (hsStars hs)) spanEnd (hsTodo hs)

-- | @add-tag@'s edits.  With no tags the run joins the TITLE LINE ('hsFull' ends at a next-line scheduled stamp).
addTagEdits :: Text -> HeadlineRecord -> [(Span, Text)]
addTagEdits tag r = addTagEditsIn (hrTags r) tag (hrSpans r)

addTagEditsIn :: Text -> Text -> HeadlineSpans -> [(Span, Text)]
addTagEditsIn cell tag hs
  | T.toLower tag `elem` tagsOfCell cell = []
  | Just sp <- hsTags hs = [ (insertAt (spanEnd sp), tag <> ":") ]
  | otherwise            = [ (insertAt (titleLineEnd hs), " :" <> tag <> ":") ]

-- | @remove-tag@'s edits.  The LAST entry takes the whole run and the space ahead (a lone @:@ is no list).  FOLDED.
removeTagEdits :: Text -> Text -> HeadlineRecord -> [(Span, Text)]
removeTagEdits tag doc r = case tagRun doc r of
  Nothing -> []
  Just (run, separator, entries)
    | null hit  -> []
    | null left -> [ (Span (spanStart run - separator) (spanEnd run), "") ]
    | otherwise -> map cutEntry hit
    where (hit, left) = partition (spells tag) entries

-- | @rename-tag@'s edits, in place.  A remove plus an add is wrong twice over
-- (anchor measured BEFORE the removal, two writes under two digests).  FROM is FOLDED.
renameTagEdits :: Text -> Text -> Text -> HeadlineRecord -> [(Span, Text)]
renameTagEdits from to doc r = case tagRun doc r of
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
tagRun :: Text -> HeadlineRecord -> Maybe (Span, Int, [(Int, Text)])
tagRun doc r = case hsTags hs of
  Nothing  -> Nothing
  Just run -> let line  = sliceSpan doc (Span from (spanEnd run))
                  ahead = spanStart run - from
              in Just ( run
                      , runWidthEnd (T.take ahead line)
                      , [ (spanStart run + at, entry)
                        | (at, entry) <- tagEntries (T.drop ahead line) ] )
  where hs   = hrSpans r
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

-- | @edit-link@'s edits: THE FORM IS PRESERVED, ABSENT IS NOT NULL, TWO WALLS ('linkAtSpan', 'spelling'); engine content-agnostic.
editLinkEdits :: Span -> Text -> Maybe (Maybe Text) -> Text -> HeadlineRecord
              -> Either Text [(Span, Text)]
editLinkEdits sp target desc doc r = do
  found <- linkAtSpan sp doc r
  written <- spelling target (reshaped (olShape found) desc)
  pure [(sp, written)]

-- | WHERE @add-link@ WRITES.  A CLOSED WORD, like 'RefVia': the arg wall, the
-- sentence it refuses with and the two roads read one vocabulary.
data LinkPlace = InBody | InTitle
  deriving (Eq, Show, Enum, Bounded)

linkPlaces :: [LinkPlace]
linkPlaces = [minBound .. maxBound]

linkPlaceWord :: LinkPlace -> Text
linkPlaceWord InBody  = "body"
linkPlaceWord InTitle = "title"

-- | The place WORD names, 'Nothing' where it names none.
linkPlaceOf :: Text -> Maybe LinkPlace
linkPlaceOf word = listToMaybe [ p | p <- linkPlaces, linkPlaceWord p == word ]

-- | @add-link@'s edits: LINK appended in R's own BODY, or to its TITLE.  The
-- title road is 'setTitleEdits'' own, so a headline meets ONE wall whichever
-- door writes it.
addLinkEdits :: LinkPlace -> Text -> Text -> HeadlineRecord -> Either Text [(Span, Text)]
addLinkEdits InBody  link doc r = Right [bodyLinkEdit link doc r]
addLinkEdits InTitle link doc r = setTitleEdits (hrTitle r <> " " <> link) doc r

-- | R's OWN BODY LINES in DOC: the subtree's lines past its title, less every
-- lifted region, and only as far as its first child.  THE CHILD TEST IS
-- 'headingStars', org-glance's own @^\*+ @, so a @*bold*@ line is body text.
--
-- 'logbookSlice' cannot read this — it is what decides the logbook region this
-- subtracts — and 'draftPointLine' counts over EVERY line, a child's included,
-- which is the opposite cut.
ownBody :: Text -> HeadlineRecord -> [(Span, Text)]
ownBody doc r = filter outside (takeWhile (isNothing . headingStars . snd) (drop 1 rows))
  where
    subtree = subtreeText doc r
    rows    = lineSpansIn subtree
    (_sub, _entries, planAt, drawAt, logAt) = regionsOf doc r
    cut     = regionSpans [planAt, drawAt, logAt]
    outside (sp, _line) =
      not (any (\q -> spanStart sp >= spanStart q && spanEnd sp <= spanEnd q) cut)

-- | Where an appended link lands in R's own body: at the end of its last written
-- line, else on a line opened at the body's start.
bodyLinkEdit :: Text -> Text -> HeadlineRecord -> (Span, Text)
bodyLinkEdit link doc r = case reverse [ e | e@(_sp, line) <- own, written line ] of
  ((sp, line) : _) -> (insertAt (base + spanStart sp + ends line), " " <> link)
  []               -> (insertAt (base + at), openingFor (T.take at subtree) eol <> link <> eol)
  where
    base    = spanStart (hrSubtree r)
    subtree = subtreeText doc r
    eol     = eolOf doc
    own     = ownBody doc r
    -- NOTHING WRITTEN TO JOIN: the link opens the body at its first line, and
    -- where the row owns no line at all, under every header line it carries.
    at      = maybe opens (spanStart . fst) (listToMaybe own)
    opens   = foldl' max (pastLine subtree (titleLineEnd (hrSpans r) - base))
                        [ spanEnd sp | sp <- regionSpans [planAt, drawAt, logAt] ]
    (_sub, _entries, planAt, drawAt, logAt) = regionsOf doc r
    written line = not (T.null (T.strip line))
    ends line    = T.length (T.dropWhileEnd isSpace line)

-- | The org link @add-link@ writes: @[[glance:TARGET?kind=KIND][DESC]]@, the
-- spelling org-glance writes and 'refTargetOf' reads back.  The KIND is slugged
-- to the peer's rule and a blank one declares none; the description meets
-- 'reshaped', the wall @edit-link@'s own rewrite meets.  Through 'spelling', so
-- what lands reparses as ONE link pointing where it says.
glanceLink :: Text -> Maybe Text -> Maybe Text -> Either Text Text
glanceLink target kind desc =
  spelling (writtenScheme <> ":" <> target <> query) (reshaped (Bracketed Nothing) (Just desc))
  where query = T.concat [ "?kind=" <> slug | Just given <- [kind]
                                            , let slug = kindSlug given, not (T.null slug) ]

-- | The name a @glance:@ link spells R by, or why none does.  THE ORG ID: the one
-- name 'refSpellings' answers to, and 'hrId' already IS it where a row carries one.
linkTargetIn :: HeadlineRecord -> Either Text Text
linkTargetIn r =
  maybe (Left (hrId r <> " carries no ORG_GLANCE_ID, so no link can name it"))
        Right (hrOrgId r)

-- | TARGET in SHAPE, or why that text is not that link.  REPARSE AND COMPARE: @a][b@ renders one wrong link.
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

-- | The one link SP covers: inside the ROW's own subtree (a digest is per file), covering it EDGE TO EDGE.
linkAtSpan :: Span -> Text -> HeadlineRecord -> Either Text OrgLink
linkAtSpan sp doc r
  | spanStart sp >= spanEnd sp =
      Left (spanned sp <> " covers no characters")
  | spanStart sp < spanStart sub || spanEnd sp > spanEnd sub =
      Left (spanned sp <> " is not inside " <> hrId r <> "'s subtree " <> spanned sub)
  | otherwise = maybe (Left (spanned sp <> " does not read as one link")) Right
                      (onlyLink (sliceSpan doc sp))
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

-- | Where HS's title LINE ends.  'hsFull' cannot serve: its end is the last part in SPAN ORDER, maybe a next line.
titleLineEnd :: HeadlineSpans -> Int
titleLineEnd hs = foldl' max (spanEnd (hsStars hs))
  [ spanEnd sp | Just sp <- [hsTodo hs, hsPriority hs, hsTitle hs, hsTags hs] ]

-- | The two whose value this server COMPOSES.  @CLOSED@ is never resolved for — VERBATIM or not at all, at both
-- write doors — so this is also the list asking whether a field OWES a date.
settableKeywords :: [Text]
settableKeywords = filter (/= "CLOSED") planningKeywords

-- | Why KEYWORD names no planning entry, or nothing.  ONE SENTENCE, TWO ASKS: @set-planning@ refuses with it (AN
-- UNKNOWN KEY OUTRANKS EVERY VALUE, the commit door's order) and the span math below asks again.
unplanned :: Text -> Maybe Text
unplanned keyword
  | keyword `elem` planningKeywords = Nothing
  | otherwise = Just (keyword <> " is not a planning keyword; this server writes "
                        <> T.intercalate " and " planningKeywords)

-- | KEY's planning value read the way ITS OWN KEY is written, or the refusal.  A value no timestamp parser reads back
-- may not land: the line stops being a planning line on the next load.  ONE ARM PER KEY, BOTH WRITE DOORS READ IT HERE:
-- @SCHEDULED@\/@DEADLINE@ take 'planningTimestamp' (also the TRANSFORM); @CLOSED@ takes REPARSE alone — verbatim or refused.
plannedValue :: Time.Day -> Text -> Text -> Either Text Text
plannedValue day key value
  | key `elem` settableKeywords = planningTimestamp day value
  | readsAsTimestamp value      = Right value
  | otherwise                   = Left (unreadable key)

unreadable :: Text -> Text
unreadable key = key <> " is not a timestamp org would read back"
  <> "; spell it <2026-08-01 Sat> or clear the row"

setPlanningEdits :: Text -> Maybe Text -> Text -> HeadlineRecord
                 -> Either Text [(Span, Text)]
setPlanningEdits keyword stamp doc r
  | Just why <- unplanned keyword = Left why
  | otherwise = Right $ case (lookup keyword present, stamp) of
      (Just sp, Just ts) -> [(sp, ts)]
      (Just sp, Nothing) -> [(cleared sp, "")]
      (Nothing, Just ts) -> [added ts]
      (Nothing, Nothing) -> []
  where
    hs      = hrSpans r
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

-- | TEXT as a planning timestamp against TODAY.  Org's own spelling kept verbatim once it REPARSES; the rest render
-- with the weekday COMPUTED.  ONE GRAMMAR, ONE DOOR: every surface owing a date reads TEXT here (@set-planning@'s
-- argument, the planning line's wall), so both take and decline one form alike.  A BRACKET CHOOSES THE ANSWER'S
-- ACTIVITY: behind the verbatim arm, a bracket that does not reparse wraps the WHOLE grammar (@[today]@ the clock
-- day INACTIVE, @<today>@ the bare word's bytes).
planningTimestamp :: Time.Day -> Text -> Either Text Text
planningTimestamp today text
  | T.null want = refusal
  | bracketed   = if readsAsTimestamp want then Right want else fromMaybe refusal wrapped
  | otherwise   = fromMaybe refusal (resolved activeBrackets want)
  where
    want      = T.strip text
    bracketed = any (`T.isPrefixOf` want) timestampOpeners
    refusal   = Left (text <> " is not a date: spell it 2026-08-05, 2026-08-05 09:30, "
                        <> relativeForms
                        <> ", today, tomorrow, 18 aug, 18 august 2027, from 18 to 19 aug"
                        <> ", or org's own <2026-08-05 Wed>")

    -- BEHIND THE VERBATIM ARM: only a bracket that REPARSES NOTHING reaches here.  The pair typed is the pair worn,
    -- the body read by the grammar a bare field reads; a MISMATCHED or EMPTY PAIR stays the refusal it was.
    wrapped = case [ (pair, T.strip inner)
                   | pair@(open, close) <- timestampBrackets
                   , Just rest  <- [T.stripPrefix open want]
                   , Just inner <- [T.stripSuffix close rest] ] of
      ((brackets, body) : _) | not (T.null body) -> resolved brackets body
      _noBodyAtAll                               -> Nothing

    -- THE WHOLE RESOLVING GRAMMAR over the BRACKETS the answer is spelled in.  'Nothing' is "no reading" (caller's own
    -- refusal), 'Left' the ONE refusal a reading spends a word on; THE ENGLISH PHRASE IS READ AHEAD (an inverted interval).
    resolved :: (Text, Text) -> Text -> Maybe (Either Text Text)
    resolved brackets phrase = case englishSpan today phrase of
      Just (Left why)         -> Just (Left why)
      Just (Right (from, to)) -> Just (Right (orgRange brackets from to))
      Nothing -> Right <$> (stamped <$> englishDay today phrase
                        <|> timedStamp brackets <$> asLocal phrase
                        <|> stamped <$> dated phrase)
      where stamped day = orgStamp brackets day Nothing

    -- THE ONE SHIFT GRAMMAR ('shiftIn') over THE ONE BASE READER ('dayNamed'): one grammar, sign for sign, day words
    -- composing.  Lower-casing leaves an ISO day's digits and hyphens alone, so the bare day needs no branch of its own.
    dated phrase = case T.toLower phrase of
      w | Just d <- dayNamed today w      -> Just d
        | Just (base, n, u) <- shiftIn w  -> shiftDay u n =<< dayNamed today base
        | otherwise                       -> Nothing

    -- @%k@ not @%H@: it reads one digit as well as two, so @9:05@ is a time rather than a refusal over a zero.
    asLocal :: Text -> Maybe Time.LocalTime
    asLocal phrase =
      Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %k:%M" (T.unpack phrase)

-- | The MONTH WORDS an English date may spell: org's three-letter and full forms, lower case.  @may@ is ONE entry;
-- @sept@ and any form with a full stop are outside the table on purpose.  THE ONLY LANGUAGE-BEARING DATUM in the
-- grammar: a second language is a second table and a selector, nothing else moves.
monthWords :: [(Text, Int)]
monthWords =
  [ ("jan", 1),  ("january", 1),   ("feb", 2),  ("february", 2)
  , ("mar", 3),  ("march", 3),     ("apr", 4),  ("april", 4)
  , ("may", 5)
  , ("jun", 6),  ("june", 6),      ("jul", 7),  ("july", 7)
  , ("aug", 8),  ("august", 8),    ("sep", 9),  ("september", 9)
  , ("oct", 10), ("october", 10),  ("nov", 11), ("november", 11)
  , ("dec", 12), ("december", 12) ]

-- | The day TEXT names in English against TODAY: @18 aug@ or @aug 18@, optionally with a year.  'Nothing' where TEXT
-- names no day — THE WHOLE FIELD IS THE PHRASE.  THE YEAR IS THE CLOCK'S, FLAT (@18 aug@ in December means that
-- August); THE WEEKDAY IS NEVER READ (computed on render), so @thu 18 aug@ is text even when Thursday is right.
englishDay :: Time.Day -> Text -> Maybe Time.Day
englishDay today text = case englishFields (wsWords (T.toLower text)) of
  Just (d, Just m, y) -> Time.fromGregorianValid (fromMaybe (yearOf today) y) m d
  _noEnglishDate      -> Nothing

-- | The two days an English INTERVAL names against TODAY.  @from@ optional, @to@ not (@18 to 19 aug@ is the interval,
-- @18 19 aug@ text).  THE LEFT END INHERITS EVERY FIELD IT ELIDES from the right (@from 18 to 19 august 2027@ is two
-- days in 2027).  'Nothing' spells no interval; 'Left' is the ONE refusal — an END BEFORE ITS START, remedied by a
-- typed year.  A degenerate pair COLLAPSES at the renderer.
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

-- | The DAY, MONTH and YEAR a phrase's WORDS name, the last two 'Nothing' where elided (only an interval's LEFT END
-- may).  A BARE DAY OR MONTH IS NO DATE alone; 'englishDay' refuses them by demanding the month.
englishFields :: [Text] -> Maybe (Int, Maybe Int, Maybe Integer)
englishFields [d]       = (\n -> (n, Nothing, Nothing)) <$> dayWord d
englishFields [a, b]    = dayAndMonth a b Nothing
englishFields [a, b, y] = dayAndMonth a b . Just =<< yearWord y
englishFields _noPhrase = Nothing

-- | The day and month two words name, EITHER ARRANGEMENT (@18 aug@ and @aug 18@), carrying the year Y its caller read.
dayAndMonth :: Text -> Text -> Maybe Integer -> Maybe (Int, Maybe Int, Maybe Integer)
dayAndMonth a b y = shaped <$> dayWord a <*> monthWord b
                <|> flip shaped <$> monthWord a <*> dayWord b
  where shaped d m = (d, Just m, y)

-- | A DAY NUMBER: one digit or two, naming 1..31.  @18th@ keeps its ordinal and @018@ its third char, so neither is a day.
dayWord :: Text -> Maybe Int
dayWord w
  | T.length w <= 2, Just n <- digitsOnly w, n >= 1, n <= 31 = Just (fromInteger n)
  | otherwise                                                = Nothing

-- | A YEAR: FOUR DIGITS AND NEVER TWO, which keeps @18 aug 18@ text where a fuzzy reader answers 2018.
yearWord :: Text -> Maybe Integer
yearWord w | T.length w == 4 = digitsOnly w
           | otherwise       = Nothing

monthWord :: Text -> Maybe Int
monthWord w = lookup w monthWords

-- | W as a WHOLE decimal run, or 'Nothing': a leftover suffix, sign or separator means W was never a number.
digitsOnly :: Text -> Maybe Integer
digitsOnly w = case TR.decimal w of
  Right (n, "") -> Just n
  _notANumber   -> Nothing

-- | TEXT's words over THE GRAMMAR'S OWN SEPARATOR — spaces and tabs only (@18  aug@ is @18 aug@).  A NEWLINE IS NO
-- SEPARATOR HERE: it stays inside its word, so no phrase carrying one reads as a date or reaches a planning line.
wsWords :: Text -> [Text]
wsWords = filter (not . T.null) . T.split (\c -> c == ' ' || c == '\t')

yearOf :: Time.Day -> Integer
yearOf day = y where (y, _month, _day) = Time.toGregorian day

-- | The days FROM..TO as org spells them in BRACKETS: the @--@ pair, each half computing its OWN WEEKDAY.  NO SECOND
-- STAMP RENDERER — 'TextShow' is the lossy REPL re-serializer, never a write-back channel (docs/invariants.md), so
-- writes go through this module's 'orgStamp'.  ONE BRACKET KIND, BOTH HALVES (the parser takes the opening bracket
-- again after @--@); a DEGENERATE PAIR COLLAPSES to one stamp, so the law stays "refuse end before start".
orgRange :: (Text, Text) -> Time.Day -> Time.Day -> Text
orgRange brackets from to = one from <> (if to == from then "" else "--" <> one to)
  where one d = orgStamp brackets d Nothing

-- | The brackets org writes a timestamp in, DERIVED from the pair the parser matches: a declined bracket would reach disk uncaught.
activeBrackets, inactiveBrackets :: (Text, Text)
activeBrackets   = bracketsOf TimestampActive
inactiveBrackets = bracketsOf TimestampInactive

bracketsOf :: TimestampStatus -> (Text, Text)
bracketsOf status = (T.singleton open, T.singleton close)
  where (open, close) = tsBrackets status

-- | BOTH PAIRS, ONCE: the openers a value is recognised by and the pairs a body is unwrapped from are one roster, one edit.
timestampBrackets :: [(Text, Text)]
timestampBrackets = [activeBrackets, inactiveBrackets]

timestampOpeners :: [Text]
timestampOpeners = map fst timestampBrackets

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

-- | @capture@'s edits: ONE insertion at the END, lines ending the target's own way, the creation stamp joined to
-- ENTRY's drawer by a blob's own splice.  NO ID AND NO TAG: the inbox files a jot, it mints no identity.
captureEdits :: Text -> Text -> Text -> Either Text [(Span, Text)]
captureEdits doc stamp entry = written <$> stampedEntry eol [(captureProperty, stamp)] Nothing entry
  where
    written body = [(insertAt (T.length doc), openingFor doc eol <> body)]
    eol = eolOf doc

-- | TEXT as the one headline a capture promises.  BOTH paths' wall: a newline lands a column-1 star, a second entry.
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

expandTemplate :: Time.ZonedTime -> [(Text, Text)] -> Text -> Text -> Either Text Text
expandTemplate now answers text template
  | TplPoint `notElem` parts = Left noPointRefusal
  | otherwise                = T.concat <$> traverse piece parts
  where
    parts = templateParts template
    piece part = case part of
      TplText t       -> Right t
      TplPoint        -> Right text
      TplStamp status -> Right (zonedStamp status now)
      TplAsk want     -> maybe (Left (unanswered want)) Right (lookup want answers)
    unanswered want = "this capture template asks " <> want
                        <> "; name it in args {\"fields\": {" <> want <> ": \"…\"}}"

noPointRefusal :: Text
noPointRefusal = "this capture template has no %?, so there is nowhere for the text to go"

-- | TEMPLATE expanded for a DRAFT, and where @%?@ stood in what came back.  THE PROMPTING ESCAPES OPEN EMPTY
-- (@%^{PROMPT}@ becomes an empty pair or slot the pane edits), so nothing is asked before the doc exists.  The
-- STAMPING escapes still take the server's clock — the page spells no org.
draftTemplate :: Time.ZonedTime -> Text -> Either Text (Text, Int)
draftTemplate now template
  | TplPoint `notElem` parts = Left noPointRefusal
  | otherwise = Right (T.concat (map piece parts), T.length (T.concat (map piece ahead)))
  where
    parts = templateParts template
    ahead = takeWhile (/= TplPoint) parts
    piece part = case part of
      TplText t       -> t
      TplPoint        -> ""
      TplStamp status -> zonedStamp status now
      TplAsk _want    -> ""

-- | Where DOC's capture template sits: first heading to EOF, which is
-- @org-glance-tag-config--entry@'s rule verbatim rather than the outline extent.
captureTemplateSpan :: Text -> Maybe Span
captureTemplateSpan doc = (\from -> Span from (T.length (T.stripEnd doc))) <$> headingAt doc

headingAt :: Text -> Maybe Int
headingAt doc = listToMaybe [ spanStart sp | (sp, line) <- lineSpansIn doc
                            , isJust (headingStars line) ]

-- | How many stars LINE opens a heading with.  org-glance's own @^\\*+ @, so a bare star run is body text here.
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
blobDocument seed given =
  stampedEntry (eolOf given)
               [ (headlineIdProperty, bsId seed), (captureProperty, bsStamp seed) ]
               (Just (bsTag seed)) given

-- | GIVEN with PAIRS joined to its drawer and TAG on its headline.  BOTH CAPTURE PATHS' composer: one splice rule.
stampedEntry :: Text -> [(Text, Text)] -> Maybe Text -> Text -> Either Text Text
stampedEntry eol pairs tag given = case firstHeadlineOf elems of
  Nothing -> Left noEntryRefusal
  Just h  -> spliced (spans h)
  where
    -- ENDED FIRST: a template is stored right-trimmed, so a title with no newline would take the drawer onto itself.
    entry = given <> openingFor given eol
    (elems, _ctx, _err) = orgParse defaultContext entry
    spliced hs = either (Left . refused) (Right . untrailed)
                        (Edit.applyEdits entry [ Edit.Edit sp new | (sp, new) <- edits hs ])
    edits hs = concat [ addTagEditsIn (cellOf (hsTags hs)) t hs | Just t <- [tag] ]
                 <> [ drawerInsertEdit entry eol pairs hs ]
    refused err = "this capture template does not splice: " <> T.pack (show err)
    cellOf = maybe "" (sliceSpan entry)

-- | The ONE edit adding PAIRS to HS's property drawer inside DOC, EOL ending each line.  INSIDE an existing drawer,
-- else a fresh one UNDER THE PLANNING LINE (so the planning line stays read as one).  HS is spanned in DOC, so
-- capture and the backfill splice by one rule.
drawerInsertEdit :: Text -> Text -> [(Text, Text)] -> HeadlineSpans -> (Span, Text)
drawerInsertEdit doc eol pairs hs = case hsProperties hs of
  Just sp -> ( insertAt (pastLine doc (spanStart sp))
             , rows (indentOf (T.drop (lineStart doc (spanStart sp)) doc)) )
  Nothing -> ( insertAt (pastLine doc (drawerAnchor hs))
             , T.concat [ ":PROPERTIES:" <> eol, rows "", ":END:" <> eol ] )
  where
    -- The three planning spans permute freely, so this is a maximum over the ends.
    drawerAnchor h = foldl' max (titleLineEnd h) [ spanEnd sp | (_key, sp) <- presentPlanning h ]
    rows indent = T.concat [ indent <> ":" <> key <> ": " <> value <> eol
                           | (key, value) <- pairs ]

noEntryRefusal :: Text
noEntryRefusal = "this capture template expands to no headline, so there is no entry to store"

bareTemplate :: Text
bareTemplate = "* %?"

topEntry :: Text -> Bool
topEntry text = headingStars (T.takeWhile (/= '\n') text) == Just 1

-- | DOC's first top entry as a record, off bytes with NO FILE BEHIND THEM.  Draft and commit doors read through the
-- SAME parse, so a draft meets the reader it will be read by.  A BLANK ENTRY IS KEPT, unlike 'recordsOf''s: @* @ is
-- what the bare template opens as.
draftRecord :: ConfigLayers -> Text -> Either Text HeadlineRecord
draftRecord cfg doc = maybe (Left noEntryRefusal) Right (listToMaybe entries)
  where
    (elems, ctx, _err) = orgParse (seedContext cfg) doc
    declared = forcedKeywords (declaredKeywords elems)
    entries = [ recordWith id cfg declared "" 0 doc "" (metaCategory ctx)
                           (forcedKeywords (recognizedKeywords cfg declared)) h extent
              | (h, extent) <- outlineEntries doc elems, topLevel h ]

-- | Which line of R's BODY the offset AT stands in — @point@'s answer, 'Nothing' the head row.  Lifted regions are
-- NOT the body's, so a @%?@ in the planning line or drawer lands on the head row too.
draftPointLine :: Text -> HeadlineRecord -> Int -> Maybe Int
draftPointLine doc r at
  | i > 0, not (lifted i) = Just (length [ j | j <- [0 .. i - 1], not (lifted j) ])
  | otherwise             = Nothing
  where
    subtree = subtreeText doc r
    here    = at - spanStart (hrSubtree r)
    rows    = lineSpansIn subtree
    -- The LAST line where the offset is the text's end: a right-trimmed template's trailing @%?@ has no line to open on.
    i = fromMaybe (length rows - 1)
                  (listToMaybe [ k | (k, (sp, _l)) <- zip [0 ..] rows, here < spanEnd sp ])
    (_sub, _entries, planAt, drawAt, logAt) = regionsOf doc r
    cut = regionSpans [planAt, drawAt, logAt]
    lifted k = case drop k rows of
      ((sp, _l) : _) -> any (\q -> spanStart sp >= spanStart q && spanEnd sp <= spanEnd q) cut
      []             -> True

-- | What the standing filter LENDS a draft.  Template-first: each fills a gap the template left, moves nothing.
data Inherited = Inherited
  { inhState    :: !(Maybe Text)       -- ^ one ordinary positive keyword the filter pins.
  , inhPriority :: !(Maybe Text)       -- ^ the bare letter, brackets the composer's.
  , inhTags     :: ![Text]             -- ^ positive filter tags beyond the template's own.
  , inhPlanning :: ![(Text, Text)]     -- ^ a settable key pinned to one day, ALREADY RESOLVED.
  } deriving (Eq, Show)

noInheritance :: Inherited
noInheritance = Inherited Nothing Nothing [] []

-- | DOC with WHAT the filter lends filled into the gaps it left.  ONE SEED AT A TIME, each off a fresh parse: two tag
-- runs against one record open runs the other cannot see, and the second lands inside the first.
draftSeeded :: ConfigLayers -> [Text] -> Inherited -> Text -> Either Text Text
draftSeeded cfg cycleTags inh doc0 = foldM step doc0 seeds
  where
    seeds = [state, letter] <> map tag (inhTags inh) <> map planned (inhPlanning inh)
    step doc seed = do
      r <- draftRecord cfg doc
      edits <- seed doc r
      either (Left . spliceRefused) Right
             (Edit.applyEdits doc [ Edit.Edit sp new | (sp, new) <- edits ])
    -- AN INHERITED FACT IS NEVER A REFUSAL: a state outside the cycle, a bad letter, a tag outside the charset, an
    -- unreadable day — each is the filter talking about other rows.  It fills the gap or not, and `+' opens either way.
    state doc r = Right $ case inhState inh of
      Just want | isNothing (hrState r), want `elem` draftStates cfg cycleTags ->
        tokenEdits hsTodo (spanEnd . hsStars) (Just want) doc r
      _spoken -> []
    letter doc r = Right $ case inhPriority inh of
      Just want | isNothing (hrPriority r) -> lends (setPriorityEdits (Just want) doc r)
      _spoken -> []
    -- A TAG RUN NEEDS A TITLE TO STAND AFTER: on a title-less draft @:work:@ reads as the title, so it waits for one.
    tag want _doc r
      | T.null (hrTitle r) = Right []
      | otherwise          = Right (lends ((`addTagEdits` r) <$> tagText want))
    planned (key, value) doc r
      | isJust (unplanned key) = Right []
      | key `elem` map fst (hpPlanning (headlineParts doc r)) = Right []
      | otherwise = Right (lends (setPlanningEdits key (Just value) doc r))
    lends = either (const []) id

spliceRefused :: Edit.EditError -> Text
spliceRefused err = "this capture draft does not splice: " <> T.pack (show err)

-- | The draft as the pane hands it back: the header the doc pane edits by list,
-- and the body it edits by span.
data DraftCargo = DraftCargo
  { dcTitle      :: !Text            -- ^ TITLE TEXT ALONE — no stars, no state, no tag run.
  , dcState      :: !(Maybe Text)
  , dcPriority   :: !(Maybe Text)    -- ^ the bare letter; the composer writes @[#A]@.
  , dcTags       :: ![Text]          -- ^ the headline's own run, the destination tag apart.
  , dcPlanning   :: ![(Text, Text)]  -- ^ already through 'plannedValue', as a row edit's are.
  , dcProperties :: ![(Text, Text)]
  , dcBody       :: !Text            -- ^ everything UNDER the headline, children and all.
  } deriving (Eq, Show)

-- | CARGO as the one entry a capture writes, EOL ending the line it composes.  The header is composed and READ BACK:
-- a title carrying a tag run or star is refused by the reparse, not written and misread later.  The lists are spliced
-- by 'recomposedSubtree', a materialize commit's own composer.
draftEntry :: ConfigLayers -> Text -> DraftCargo -> Either Text Text
draftEntry cfg eol cargo = do
  titled <- titleText (dcTitle cargo)
  state <- traverse keywordText (dcState cargo)
  letter <- traverse priorityText (dcPriority cargo)
  run <- traverse tagText (dcTags cargo)
  mapM_ (\(key, _v) -> maybe (Right ()) Left (unplanned key)) (dcPlanning cargo)
  let head' = T.intercalate " " (concat [ ["*"], maybe [] pure state
                                        , map priorityCell (maybe [] pure letter)
                                        , [titled], runOf run ])
      doc = head' <> eol <> reended (dcBody cargo)
  r <- draftRecord cfg doc
  reads' r titled state letter run
  alone doc
  Right (recomposedSubtree doc r (HeadlineParts doc (dcProperties cargo) (dcPlanning cargo) ""))
  where
    -- ONE FILE, ONE ENDING.  The pane speaks `\n', so a CRLF body is converted here — 'captureEdits''s own rule.
    reended body | eol == "\n" = body
                 | otherwise   = T.replace "\n" eol (T.replace "\r\n" "\n" body)
    runOf [] = []
    runOf ts = [":" <> T.intercalate ":" ts <> ":"]
    reads' r titled state letter run
      | hrTitle r /= titled = Left (misread "title" titled (hrTitle r))
      | hrState r /= state = Left (misread "state" (fold' state) (fold' (hrState r)))
      | hrPriority r /= (priorityCell <$> letter) =
          Left (misread "priority" (fold' letter) (fold' (hrPriority r)))
      | tagsOfCell (hrTags r) /= map T.toLower run = Left (misread "tags"
          (T.intercalate " " run) (T.intercalate " " (tagsOfCell (hrTags r))))
      | otherwise = Right ()
    fold' = fromMaybe ""
    misread part want got =
      "a capture's " <> part <> " is that part alone: " <> want
        <> " reads back as " <> got <> ", so it is refused rather than written"
    -- ONE TOP ENTRY, the template's own law: a body line opening a single star is a SECOND capture on the first.
    alone doc = case [ k | (k, line) <- zip [1 :: Int ..] (drop 1 (linesWith doc))
                         , headingStars line == Just 1 ] of
      (k : _rest) -> Left ("a capture is one headline, so its body opens no second\
                           \ entry; line " <> showt k <> " opens one")
      []          -> Right ()

-- | LINES as a config file's @#+TODO:@ block; an EMPTY block is the DELETION.  PARTS rides the SAME call (regions of
-- one file): four calls would be four writes under four digests, each invalidating the last.
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
    -- Over the WORDS AS WRITTEN: `todoPragmas' answers what org would READ, dropping the very word the writer got wrong.
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

-- | EVERY setting beside the cycle.  ORDER IS DATA: two absent pragmas insert at one offset, resolved in list order.
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
-- structure (the bar needs no space around it); a @(k)@ fast key is no part of the word.
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

insertAt :: Int -> Span
insertAt at' = Span at' at'


viewJSON :: Text -> [HeadlineRecord] -> Value
viewJSON viewTitle records =
  viewJSONWith defaultSortChain viewTitle
               (mergeKeywords (map hrKeywords records)) records

-- | 'viewJSON' declaring CHAIN with PALETTE given.  A server passes the whole
-- store's palette: this page's rows would move the badge list on every page.
viewJSONWith :: SortChain -> Text -> TodoKeywords -> [HeadlineRecord] -> Value
viewJSONWith = viewJSONFor viewColumns (const []) builtinViews

builtinViews :: [(Text, Text)]
builtinViews = [ (svId v, viewQuery (svId v) noConfig) | v <- savedViews ]

-- | The table's own envelope.  EXTRA rides every row served — 'edgePairs' where
-- @edges@ was asked, nothing where it was not; the browser asks for none.
viewJSONFor :: [ViewColumn] -> (HeadlineRecord -> [Pair]) -> [(Text, Text)] -> SortChain
            -> Text -> TodoKeywords -> [HeadlineRecord] -> Value
viewJSONFor cols extra views chain viewTitle palette records = object
  (  [ "title" .= viewTitle, "columns" .= columnsFor cols palette
     , "actions" .= actions ]
  <> declaredSort chain
  <> declaredViews views
  <> [ "rows" .= [ rowJSONFor cols (extra r) r | r <- records ] ])

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
  encodeToLazyText . viewJSONFor cols (const []) views chain viewTitle palette

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

-- | Columns beyond the default view: a friendly key/header over a drawer property.  OPT-IN — none is in 'viewColumns',
-- yet 'resolveColumns' picks one by name and 'sortCell' orders by it.  @created@ reads 'captureProperty', "Created".
-- | The creation-time property key, folded once for the alias cell.
createdKey :: Text
createdKey = T.toCaseFold captureProperty

aliasColumns :: [ViewColumn]
aliasColumns = [ ("created", "Created", "text", \r -> customCell r createdKey) ]

-- | The default view's columns and the aliases: the one roster a sort reads.
sortColumns :: [ViewColumn]
sortColumns = viewColumns <> aliasColumns

-- | NAMES as columns, matched CASE-INSENSITIVELY against the default view's keys,
-- headers and aliases; an unknown name is a CUSTOM column.  THE MINIMAL SET IS TITLE.
resolveColumns :: [Text] -> [ViewColumn]
resolveColumns names = withTitle (map pick names)
  where
    withTitle cols
      | any (\(key, _h, _k, _c) -> key == "title") cols = cols
      | otherwise = [ col | col@("title", _h, _k, _c) <- viewColumns ] <> cols
    pick wanted = fromMaybe (custom wanted) (lookup (T.toCaseFold wanted) builtins)
    builtins    = concat [ [ (T.toCaseFold key, col), (T.toCaseFold header, col) ]
                         | col@(key, header, _kind, _cell) <- sortColumns ]
    custom wanted = ( T.toCaseFold wanted, wanted, "text"
                    , \r -> customCell r (T.toCaseFold wanted) )

-- | R's value under a custom column NAME, folded.  The hidden properties are
-- NOT hidden here — a read-only cell rewrites nothing.
customCell :: HeadlineRecord -> Text -> Maybe Text
customCell r wanted
  | wanted == "closed" = hrClosed r
  | otherwise          = listToMaybe [ value | (key, value) <- hrDrawer r
                                             , T.toCaseFold key == wanted ]

viewCells :: HeadlineRecord -> [Text]
viewCells r = [ fromMaybe "" (cell r) | (_key, _header, _kind, cell) <- viewColumns ]

filterKeys :: [Text]
filterKeys = [ key | (key, _header, _kind, _cell) <- viewColumns ]

-- | Every key a sort may name: 'filterKeys' widened by the aliases.  A sort reads a
-- cell no filter indexes, so an alias orders without joining the filter's columns.
sortKeys :: [Text]
sortKeys = [ key | (key, _header, _kind, _cell) <- sortColumns ]

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
data Meta = MActive | MInactive | MEmpty | MNone | MToday | MAny
  deriving (Eq, Show, Enum, Bounded)

metas :: [Meta]
metas = [minBound .. maxBound]

metaWord :: Meta -> Text
metaWord = starred . bare
  where
    bare MActive   = "active"
    bare MInactive = "inactive"
    bare MEmpty    = "empty"
    bare MNone     = "none"
    -- A DATE VALUE rather than a cell predicate: it stands wherever a date
    -- literal stands and resolves to the request's own day ('Filter.onDay').
    bare MToday    = "today"
    -- AN ANCHOR rather than a cell predicate: it stands where a @ref:@\/@from:@ row
    -- id stands and names EVERY ANCHOR AT ONCE ('Filter.anyMeta').
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

-- | One row for the wire.  @linked@ is SPARSE (@true@ or absent), additive to SCHEMA.md's Row, not a field every row owes.
rowJSON :: HeadlineRecord -> Value
rowJSON = rowJSONFor viewColumns []

-- | A headline as the MCP @list-headlines@ rows shape: the fields an agent reads, no badge chrome or
-- @columns@\/@actions@\/@views@.  SPARSE: unset planning, state, priority and an empty tag run drop out.  @tags@ is
-- the file's own run, file-cased ('tagRunEntries'), never 'rowJSON''s sorted cell.
rowSummaryPairs :: HeadlineRecord -> [Pair]
rowSummaryPairs r =
     [ "id" .= hrId r, "title" .= hrTitle r ]
  <> [ "state"     .= s  | Just s <- [hrState r] ]
  <> [ "priority"  .= p  | Just p <- [hrPriority r] ]
  <> [ "scheduled" .= d  | Just d <- [hrScheduled r] ]
  <> [ "deadline"  .= d  | Just d <- [hrDeadline r] ]
  <> [ "tags"      .= ts | let ts = tagRunEntries (hrTags r), not (null ts) ]

rowSummaryJSON :: HeadlineRecord -> Value
rowSummaryJSON = object . rowSummaryPairs

-- | The @list-headlines@ answer @{total, clean, rows}@: TOTAL the uncapped match count so a @limit@ is honest; CLEAN
-- the startup verdict (no second @doctor@ call); ROWS the paged 'rowSummaryPairs', each carrying what EXTRA rides —
-- 'edgePairs' where @edges@ was asked, nothing where it was not.
summaryEnvelope :: (HeadlineRecord -> [Pair]) -> Int -> Bool -> [HeadlineRecord] -> Value
summaryEnvelope extra total clean rows = object
  [ "total" .= total, "clean" .= clean
  , "rows" .= [ object (rowSummaryPairs r <> extra r) | r <- rows ] ]

-- | One table row over COLS, EXTRA riding what the request asked for beyond the cells.
rowJSONFor :: [ViewColumn] -> [Pair] -> HeadlineRecord -> Value
rowJSONFor cols extra r = object
  (  [ "id" .= hrId r
     , "cells" .= object [ Key.fromText key .= toJSON (cell r)
                         | (key, _header, _kind, cell) <- cols ] ]
  <> [ "linked" .= True | hrLinked r ]
  -- SPARSE like `linked`, so SCHEMA.md's Row stays additive.
  <> [ "repeats" .= cookie | Just cookie <- [repeatsOf r] ]
  <> extra)

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

-- | The colour a badge names, as a CSS fallback chain.  Keywords and one-letter priorities can't spell a slot's name.
overridable :: Text -> Text -> Text -> Text
overridable prefix value fallback =
  "var(--g-" <> prefix <> "-" <> value <> ", " <> fallback <> ")"
