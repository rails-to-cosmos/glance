-- | The query facade: load org files into rows, render them as a table-view
-- JSON document, and write one headline's raw subtree back.  The whole public
-- surface of the package: the parser and its AST are a private sublibrary, so
-- a daemon linking against @glance@ cannot reach them.
--
-- THE WRITE PATH IS THE READ PATH BACKWARDS.  A record carries its subtree's
-- extent in the text it was parsed from and that text's digest;
-- 'replaceSpans' splices over spans of it and refuses unless the file still
-- digests to the pinned value.  A file that moved underneath costs a refusal
-- rather than a corrupted splice.
--
-- Structured commands are the other half, and the reason the span math lives
-- here: 'HeadlineSpans' is the private sublibrary's, so a web layer computing
-- its own insertion points would reach past this facade.  The @*Edits@
-- functions hand back edits in the currency 'replaceSpans' takes, and none of
-- them writes.
--
-- TWO RULES THE WIRE DEPENDS ON.  Cell text is cut from the source spans at
-- load, so the JSON carries what the file says rather than what the REPL
-- renderer would re-emit.  And the JSON is assembled out of 'Value'
-- combinators, never derived from the internal types — the wire is the
-- contract with @table-view\/SCHEMA.md@ and the AST must stay free under it.
--
-- RETENTION: cells are copied out of the document they were sliced from, so a
-- row never pins its file's text.  'hrHeadline' still holds the parser's own
-- slices, so a loaded store retains what it parsed; the lever, should
-- residency ever exceed the scan budget, is that field.
module Glance.Query ( BlobSeed (..)
                    , ConfigLayerFile (..)
                    , ConfigLayers (..)
                    , ConfigParts (..)
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
                    , captureTargetOf
                    , captureTemplateEdits
                    , captureTemplateIn
                    , captureTemplateOf
                    , cellSep
                    , configDirsIn
                    , configEdits
                    , configPath
                    , currentDocument
                    , defaultCaptureFile
                    , SavedView (..)
                    , defaultFilter
                    , savedView
                    , savedViews
                    , viewOf
                    , viewQuery
                    , defaultWalk
                    , derivedPath
                    , digestOfText
                    , displayText
                    , documentPath
                    , editLinkEdits
                    , expandTemplate
                    , filterKeys
                    , followableTypes
                    , headlineParts
                    , hiddenProperties
                    , inactiveMeta
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
                    , recomposedSubtree
                    , refSpellings
                    , refTargetOf
                    , refTargets
                    , removeTagEdits
                    , renameTagEdits
                    , replaceSpans
                    , resolveIds
                    , rowIdIn
                    , rowJSON
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
                    , settableStates
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
                    , subtreeLinks
                    , subtreeText
                    , systemSetting
                    , tagColumns
                    , tagRunEntries
                    , tagText
                    , tagged
                    , clStateColors
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
                , TimestampUnit (Days, Months, Weeks, Years), Todo (name)
                , TsMoment (tsmHasTime, tsmTime), archiveTag, deadline, defaultContext
                , firstHeadlineOf, headlineIdProperty, headlinesOf, hsFull, identity
                , isTagChar, levelOf
                , metaCategory
                , orgParse, priority, schedule, shiftSpan, sliceSpan, spans, spelled
                , addUnit, relativeForms, repeaterFormat, tags, title, todo
                , tsBrackets, unitOf )
import Data.Org.Config ( ConfigLayerFile (..), ConfigLayers (..), TodoKeywords (..)
                       , builtinAgenda, builtinFilter, captureTargetEdits, captureTargetIn
                       , captureTargetOf, classify, configDirIn, configDirsIn
                       , declaredKeywords
                       , SavedView (..), defaultCaptureFile, defaultFilter
                       , isTodoPragma, savedView, savedViews, stateColorsEdits
                       , stateColorsOf
                       , viewEdits, viewOf
                       , viewQuery
                       , firstBy, keywordScopes
                       , loadConfigDirs, mergeKeywords, noConfig, noKeywords
                       , readConfigLayers, recognizedKeywords, seedContext
                       , systemSetting, todoLineEdits, todoLines, todoPragmas )
import Data.Org.External (Completion (..))
import Data.Org.Blob (blobPathIn, mintBlobId, storeRootIn, uuidFrom)
import Data.Org.Walk ( Found (..), LoadFailure (..), WalkOptions (..), claimById
                     , defaultWalk, findOrgFilesWith, isConfig, isDerived, isDocument
                     , mapFilesConcurrently )

-- The line splitter and its span arithmetic are the write engine's: this
-- module's regions are cut by whole lines and spliced back by char span, which
-- is the currency 'Data.Org.Edit' owns.  Two spellings of it agreed by accident.
import Data.Org.Edit (digestOfText, eolOf, lineSpansIn, linesWith, openingFor)

import qualified Data.Org.Edit as Edit
import qualified Data.Org.External as External

-- Records

-- | One row's worth of a headline: where it came from, the cells the view
-- shows, and the parsed headline itself as an opaque passthrough — later
-- milestones read its spans (write-back) and its links (graph) from here.
--
-- A record is a LEVEL-ONE headline's ('topLevel') with something to show
-- ('blankEntry').  Its descendants have no records of their own and are reached
-- through 'hrSubtree', which covers them.
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
  , hrLinks     :: ![Text]          -- ^ the rows this subtree points AT, normalized; see 'refTargets'.
  , hrLinked    :: !Bool            -- ^ does the subtree hold a link at all — what @o@ follows; see 'subtreeLinks'.
  , hrActive    :: !(Maybe Bool)    -- ^ whether 'hrState' is an active state HERE; see 'Data.Org.Config.classify'.
  } deriving (Show)

-- | A load: the rows, and what did not make it into them.  The counts are the
-- coverage the web layer surfaces — a silently skipped file is a bug report
-- waiting to happen.
data QueryResult = QueryResult
  { qrRecords        :: ![HeadlineRecord]  -- ^ rows in walk order, one per id; paths sorted, headlines in file order.
  , qrFiles          :: !Int               -- ^ .org files visited.
  , qrParseFailures  :: !Int               -- ^ files 'orgParse' rejected; they contribute no rows.
  , qrDecodeFailures :: !Int               -- ^ files that are not valid UTF-8.
  , qrReadFailures   :: !Int               -- ^ files that could not be read, plus unlistable directories.
  , qrIdCollisions   :: ![IdCollision]     -- ^ rows 'resolveIds' dropped, and what they lost to.
  } deriving (Show)

-- | Two files claiming one @ORG_GLANCE_ID@, and which of them the view shows.
-- A row id is the identity a renderer keys updates off, so two rows cannot
-- share one: 'resolveIds' picks and this records the pick, since a duplicate id
-- is nearly always a file that should not have been walked.
data IdCollision = IdCollision
  { icId      :: !Text      -- ^ the id both files claim.
  , icKept    :: !FilePath  -- ^ the file whose row the view carries.
  , icDropped :: !FilePath  -- ^ the file whose row it does not.
  } deriving (Eq, Show)

emptyResult :: QueryResult
emptyResult = QueryResult [] 0 0 0 0 []

-- Loading

-- | Every top entry under DIR the table can show, one record each ('topLevel',
-- 'blankEntry').  Walks @*.org@ recursively and reads each file strictly.
-- org-glance's derived mirrors are not walked, and neither is its config
-- ('Data.Org.Walk') — the config is read instead, by path, and seeds every
-- parse.
loadDir :: FilePath -> IO QueryResult
loadDir = loadDirWith defaultWalk

-- | 'loadDir' over the tree OPTS asks for.
loadDirWith :: WalkOptions -> FilePath -> IO QueryResult
loadDirWith opts dir = do
  (files, dirErrs) <- loadDirFilesWith opts dir
  pure (summarise dirErrs files)

-- | DIR loaded one file at a time over the tree OPTS asks for: every @*.org@
-- path in walk order with its rows or its failure, plus the number of
-- directories the walk could not list (those count as read failures too, and
-- have no path of their own to report).  The per-file breakdown 'loadDir' folds
-- away is what a watcher needs to re-load a single file into a store built the
-- same way.
--
-- The files are read on a pool ('Data.Org.Walk.mapFilesConcurrently'), which is
-- sound because a file is parsed from one constant seed and shares no state
-- with any other.  The answer is the sorted path list zipped with its outcomes,
-- so it is the sequence 'loadDirFilesSerially' produces whatever order the pool
-- finished in — and everything downstream (id resolution, the store's walk
-- order, the counts) reads that sequence rather than the completion order.
loadDirFilesWith :: WalkOptions -> FilePath
                 -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesWith opts dir = withoutConfig <$> loadDirWithConfig opts dir

-- | 'loadDirFilesWith' keeping the config the walk found.
--
-- ONE walk answers both halves, and it has to: the config directories are
-- discovered by crossing the tree ('Data.Org.Walk.foundConfig') and every file
-- in it is parsed knowing them.  A caller that keeps the layers — the store
-- does, so its watch can re-read one file the way the load read it — takes them
-- from here rather than walking a second time.
loadDirWithConfig :: WalkOptions -> FilePath
                  -> IO (ConfigLayers, [(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirWithConfig = loadDirFilesUsing mapFilesConcurrently

-- | 'loadDirFilesWith' with the pool taken out — one file after another on the
-- calling thread.  It is the reference the parallel load is asserted equal to
-- (@TestQuery@), and it is exported for that: every other answer this library
-- gives over a directory is a fold of this pair, so two loads agreeing here
-- agree everywhere.
loadDirFilesSerially :: WalkOptions -> FilePath
                     -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesSerially opts dir = withoutConfig <$> loadDirFilesUsing mapM opts dir

-- | A load with its config dropped, for the callers that only want the files.
withoutConfig :: (ConfigLayers, [a], Int) -> ([a], Int)
withoutConfig (_cfg, files, dirErrs) = (files, dirErrs)

-- | 'loadDirWithConfig' with OVER deciding how the walk's files are crossed.
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

-- | PATH's top entries with no config in force, or why it has none.
loadFile :: FilePath -> IO (Either LoadFailure [HeadlineRecord])
loadFile = loadFileWith noConfig

-- | PATH's top entries under CFG, or why it has none.  Reads the file strictly
-- and parses it from CFG's seed — 'Data.Org.defaultContext' plus every keyword
-- the config layers name, one constant per load.  Nothing accumulates between
-- files: what a file's own @#+TODO:@ adds reaches that file's headlines and no
-- other's, whether it is loaded with a directory on a pool or on its own after
-- an edit.
--
-- The rows come back forced: a caller running this on a pool needs the work
-- done by the worker that took the file, and a caller of any kind needs the
-- document dropped rather than retained under an unevaluated cell
-- (docs\/invariants.md, Scan).
--
-- The ladder itself is 'Edit.readParsed', which the corpus scan climbs too, so
-- the digest a row is pinned by is of the very bytes the parse read.  Only the
-- rung is kept here: a reason belongs to a report, and this side counts.
loadFileWith :: ConfigLayers -> FilePath -> IO (Either LoadFailure [HeadlineRecord])
loadFileWith cfg path = do
  parsed <- Edit.readParsed (seedContext cfg) path
  evaluate $ case parsed of
    Left (fault, _why) -> Left fault
    Right pd -> forcing rs (Right rs)
      where rs = recordsOf cfg path (Edit.pdText pd) (Edit.pdDigest pd)
                           (Edit.pdContext pd) (Edit.pdElements pd)

-- | FILES folded into one result, with DIRERRS unlistable directories already
-- counted as read failures.
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

-- | The rows FILE contributes, cells cut out of DOC and DIGEST pinning it,
-- categorised by CTX — so a @#+CATEGORY@ anywhere labels the whole file.
--
-- A row is a LEVEL-ONE headline; everything deeper rides inside its ancestor's
-- subtree.  The extents are computed over the WHOLE sequence and the filter
-- applied afterwards: 'subtreeSpans' is org's outline rule over a DOCUMENT, and
-- widening 'topLevel' with the filter first would end a deeper row at the next
-- KEPT headline instead of the next shallower one — a subtree missing its own
-- children.
--
-- TWO KEYWORD VALUES come out of one parse and are not the same thing. The
-- file's own @#+TODO:@ ('hrDeclared') is the nearest scope a row is CLASSIFIED
-- by; RECOGNITION is CFG's chain closed over them ('hrKeywords'), the palette
-- contribution and what a command may write.  Neither recovers the other, so
-- both are kept.  The union is the CHAIN's rather than CTX's: the chain answers
-- in the order the org files spell it where CTX is a 'Data.Set' and answers
-- alphabetically, and palette order is sort order.
recordsOf :: ConfigLayers -> FilePath -> Text -> Text -> Context -> [Spanned Element]
          -> [HeadlineRecord]
recordsOf cfg path doc digest ctx elems =
  [ recordOf cfg declared path ordinal doc digest category keywords h subtree
  | (ordinal, (h, subtree)) <- zip [0 ..] entries ]
  where category = detach (metaCategory ctx)
        -- The recognized union, read off the CONFIG CHAIN rather than off CTX's
        -- sets: the same words either way, in the order the org files spell them
        -- rather than alphabetized ('recognizedKeywords').  Nothing is detached
        -- here, since neither half is a slice of DOC — CFG's seed is the load's
        -- own text and DECLARED is already copied out.
        keywords = forcedKeywords (recognizedKeywords cfg declared)
        -- Forced here, once per file: it is STORED now ('hrDeclared'), and an
        -- unforced set is a thunk over ELEMS.
        declared = forcedKeywords (declaredKeywords elems)
        -- The position in THIS list is the row's ordinal ('rowId'), so BOTH
        -- filters run before the numbering: a child or a blank entry between
        -- two rows would otherwise consume an ordinal and shift every row
        -- behind it.
        entries  = [ e | e@(h, _sub) <- outlineEntries doc elems
                       , topLevel h, not (blankEntry h) ]

-- | DOC's headlines with the subtree extent of each, in document order.
--
-- The outline rule over a WHOLE document, which is what makes the extents tile:
-- an entry runs to the next headline at its level or shallower.  Both readers
-- filter this rather than the headlines ahead of it — the rows keep the top
-- entries, the subtree lens keeps what lies inside one — because filtering
-- first would end a kept entry at the next KEPT headline instead of the next
-- shallower one.
outlineEntries :: Text -> [Spanned Element] -> [(Headline, Span)]
outlineEntries doc elems = zip heads (subtreeSpans (T.length doc) heads)
  where heads = headlinesOf elems

-- | Is H a top entry — one star, no ancestor?  Half of being a row; the other
-- half is having something to show ('blankEntry').
--
-- The table is a list of top entries, so a row is an entry rather than a line
-- of one: a child's title, tags and dates are part of what its parent's subtree
-- says, reachable by materializing that subtree, and they are not rows of their
-- own.  Three things follow and are the intended semantics rather than
-- oversights.  A word that appears only under a child matches nothing, since
-- 'hrSearch' is built out of the cells of the rows that exist.  An
-- @ORG_GLANCE_ID@ on a deeper headline is not a row id, so nothing addresses it
-- and it cannot collide.  And a file whose outline never reaches level one —
-- every headline written @**@ or deeper — contributes no rows at all, the same
-- answer a file with no headlines gives.
topLevel :: Headline -> Bool
topLevel h = levelOf h == 1

-- | Has H nothing the table can show?  Six sub-spans, one per column: a
-- headline carrying none renders six empty cells, and a row a reader can
-- neither read nor tell from the next is not a row.  The file keeps the entry;
-- the table skips it.
--
-- The RECORD's rule computed at the HEADLINE's layer, and it has to be — the
-- ordinal numbers emitted rows, so the filter runs before the numbering and
-- there is no record yet to ask.  The layers agree by construction: each span
-- is 'Nothing' exactly where 'recordOf' would cut an empty cell.
--
-- NOTHING WITHOUT A COLUMN RESCUES AN ENTRY: a @CLOSED:@ stamp, a drawer (an
-- @ORG_GLANCE_ID@ included, so a blank entry has no row id and no command can
-- address it), a body, children.  Reading the no-planning clause as the two
-- planning COLUMNS is the one place it could have gone the other way.
--
-- The tags clause never fires alone: org spells tags after a title and the
-- parser hands @* :tag:@ its colons as the title.
blankEntry :: Headline -> Bool
blankEntry h = all isNothing [ hsTodo sp, hsPriority sp, hsTitle sp, hsTags sp
                             , hsSchedule sp, hsDeadline sp ]
  where sp = spans h

recordOf :: ConfigLayers -> TodoKeywords -> FilePath -> Int -> Text -> Text -> Text
         -> TodoKeywords -> Headline -> Span -> HeadlineRecord
recordOf cfg declared path ordinal doc digest category keywords h subtree =
  forceRecord (row { hrSearch = searchTextOf (viewCells row) })
  where
        -- The record is tied through its own cells: 'viewCells' reads the six
        -- column accessors off ROW, so the haystack is the view's columns in
        -- the view's order by construction rather than by a second list here
        -- staying in step with 'viewColumns'.  ROW's own 'hrSearch' is never
        -- read on the way round.
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
        -- One scan of the subtree answers both questions: which ROWS it points
        -- at ('hrLinks') and whether it points ANYWHERE ('hrLinked').  The
        -- second is the wider set — every reference is a link and most links
        -- are not references (~/sync at 2026-08-02: 4976 rows carry a link,
        -- 1824 of them a reference).
        links = orgLinks (sliceSpan doc subtree)
        -- The span is the lossless channel; the render is what is left when a
        -- headline carries no span for a component, which is to say when the
        -- component is empty.
        cut mspan render = detach (maybe render (sliceSpan doc) mspan)
        state     = detach . name <$> todo h
        pri       = (\(Priority c) -> priorityCell (T.singleton c)) <$> priority h
        titleCell = cut (hsTitle sp) (showt (title h))
        tagsCell  = cut (hsTags sp) (showt (tags h))
        scheduled = isoStamp <$> schedule h
        due       = isoStamp <$> deadline h

-- Search text

-- | The separator cells are joined by, and the one character a cell cannot
-- hold: 'displayText' turns every control character into a space, so a query
-- can never span two cells.  Exported because a consumer cutting one field back
-- out of 'hrSearch' has to cut on the character that joined it.
cellSep :: Char
cellSep = '\US'

-- | CELLS as one lowercase haystack, in column order, joined by 'cellSep' and
-- copied out of the document like every other cell.  Built at load beside the
-- row's JSON so a filter is one 'T.isInfixOf' per row rather than a re-render.
--
-- This is @table-view.js@'s own row text: the renderer caches
-- @cells.map(displayText).map(toLowerCase).join(\"\\x1f\")@ and searches it with
-- the trimmed, lowercased filter box.  Server-side filtering has to agree with
-- it exactly, or the same query answers differently depending on who ran it.
searchTextOf :: [Text] -> Text
searchTextOf = detach . T.toLower . T.intercalate (T.singleton cellSep) . map displayText

-- | CELL as a table-view renderer displays it: org bracket links shown by
-- their description, and every run of control characters as one space.  The
-- JS mirror is @displayText@ (@web\/table-view.js@), which drives that
-- renderer's widths, sort and filter alike.
displayText :: Text -> Text
displayText = squashControls . showLinks

-- | S with each org bracket link replaced by what it shows: @[[T][D]]@ becomes
-- @D@, @[[T]]@ and @[[T][]]@ become @T@.  Text that does not close a link is
-- left exactly as it is, the way the renderer's regex leaves an unmatched
-- @[[@ alone.
showLinks :: Text -> Text
showLinks s | not ("[[" `T.isInfixOf` s) = s   -- the common cell, scanned once
            | otherwise                  = T.concat (map (either snd linkShown) (linkParts s))

-- | Which of org's two link SHAPES the source spells, which is what a rewrite
-- has to keep: @[[T]]@ is 'Bracketed' 'Nothing', @[[T][D]]@ is 'Bracketed' over
-- the description section it carries — the empty one included, since @[[T][]]@
-- is a section that is THERE — and a plain URL is 'Bare'.
data LinkShape = Bare | Bracketed !(Maybe Text)
  deriving (Eq, Show)

-- | One link as the scanner read it: where it points, the shape that spells it,
-- and the half-open CHAR span it occupies in the text scanned.
--
-- The span is what makes a link WRITEABLE: @GET \/links@ carries it out to the
-- page and @edit-link@ takes it back as the range to splice ('editLinkEdits'),
-- so the reader edits the very characters the scanner read.
data OrgLink = OrgLink
  { olTarget :: !Text       -- ^ where it points, as the source spells it.
  , olShape  :: !LinkShape  -- ^ how the source spells it.
  , olSpan   :: !Span       -- ^ its extent in the text scanned.
  } deriving (Eq, Show)

-- | What L SHOWS: its description where it carries one that says anything,
-- and its target otherwise — @[[T][D]]@ shows @D@, @[[T]]@, @[[T][]]@ and a
-- plain URL show @T@.
--
-- The display rule and the shape are therefore ONE fact: 'displayText' reads
-- this and so does @\/links@' @desc@, so what the table shows for a link and
-- what the popup calls it cannot come apart.
linkShown :: OrgLink -> Text
linkShown l = case olShape l of
  Bracketed (Just desc) | not (T.null desc) -> desc
  _itsTarget                                -> olTarget l

-- | S cut into the text between its bracket links and the links themselves, in
-- order, every piece carrying its offset into S.  Text that does not close a
-- link — an unmatched @[[@ — stays literal, the way the renderer's regex leaves
-- one alone.
--
-- ONE scanner for the three questions asked of a bracket link: 'showLinks' keeps
-- what each shows, 'orgLinks' keeps where each points, and 'editLinkEdits' keeps
-- where each SITS.  A second pass would be a second grammar to keep in step with
-- SCHEMA.md's link rule.
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

-- | The link TEXT opens with — TEXT standing at its @[[@ — as its target, the
-- description section it carries (absent for @[[T]]@) and the WIDTH it spends,
-- both pairs of brackets included.  'Nothing' when TEXT does not close one.
--
-- The width is read off the pieces rather than measured against what is left,
-- so a scan costs the links it finds rather than the tail behind each of them.
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

-- Links

-- | Every link R's subtree points at, spanned in the DOCUMENT.
--
-- Server-side because it is org text work: a page that extracted these would
-- need the bracket grammar 'displayText' already holds, and would then hold a
-- second copy of it.  The subtree rather than the cells, so a link in the body
-- of an entry is reachable from the row that carries it.
--
-- The scan runs over the subtree slice and every span is shifted by where that
-- slice starts, so what comes out is an offset into 'hrDoc' — the currency
-- 'Data.Org.Edit' splices in and the one @\/links@ hands to a client that means
-- to write.
subtreeLinks :: HeadlineRecord -> [OrgLink]
subtreeLinks r = map (shiftLink (spanStart (hrSubtree r))) (orgLinks (subtreeText r))

-- | L moved BY characters along the text it was scanned in.
shiftLink :: Int -> OrgLink -> OrgLink
shiftLink by l = l { olSpan = shiftSpan by (olSpan l) }

-- | The links TEXT holds, in order of appearance, one per (target, shown) pair.
--
-- Two forms, which is what org writes and what 'displayText' reads: the bracket
-- link, described by its @DESC@ else by its target, and the plain URL, which
-- describes itself.  The dedup key is the pair a reader can SEE — a target
-- respelled under the SAME description keeps the first occurrence, span and
-- all, while the same target under another description is another entry.  The
-- key was the target alone, and one @elisp:@ command under two descriptions
-- served the first and swallowed the second, which read as the link not parsing.
--
-- A plain URL can only be in the text BETWEEN bracket links ('linkParts'), so
-- @[[https://…][x]]@ never also reports its target as a bare one.
orgLinks :: Text -> [OrgLink]
orgLinks = firstBy (\l -> (olTarget l, linkShown l))
         . concatMap (either (uncurry plainLinks) pure) . linkParts

-- | The schemes a bare URL is recognized by.  org's plain-link set is wider;
-- these three are the ones a browser is asked to open, and a scheme this does
-- not name stays ordinary text rather than becoming a link nothing can follow.
linkSchemes :: [Text]
linkSchemes = ["https://", "http://", "mailto:"]

-- | The plain URLs S holds, each as its own description and each spanning where
-- it sits — AT being where S itself sits in the text being scanned, so the spans
-- come out in that text's offsets rather than in this piece's.  A URL cannot
-- carry whitespace, so the words of S are the candidates and one word holds at
-- most one link.
plainLinks :: Int -> Text -> [OrgLink]
plainLinks at s =
  [ OrgLink url Bare (Span from (from + T.length url))
  | (start, word) <- spacedWords s
  , Just (opens, url) <- [urlIn word]
  , let from = at + start + opens ]

-- | The words of S with their offsets into it.  'Data.Text.words' answers the
-- first question and loses the second, and a plain link's span is where the word
-- carrying it stands.
spacedWords :: Text -> [(Int, Text)]
spacedWords = go 0
  where
    go at text
      | T.null word = []
      | otherwise   = (opens, word) : go (opens + T.length word) rest
      where (spaces, body) = T.span isSpace text
            (word, rest)   = T.break isSpace body
            opens          = at + T.length spaces

-- | The plain URL WORD holds and where in WORD it opens, if any: from the
-- earliest scheme that opens at a non-word boundary — so @xhttp://a@ is not one
-- — to the end of the word, with the punctuation a sentence leaves behind taken
-- off the tail.  That last rule is what makes @see https://x.org.@ and
-- @(https://x.org)@ point where they read as pointing.
--
-- Every scheme carries its separator, so a word with no @:@ in it can hold no
-- link and is turned away by one cheap pass — which is nearly every word of
-- nearly every subtree.
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

-- | What KIND of place a link target names, as one word: its SCHEME, lowercased,
-- with the whole @org-glance-@ family folded into @glance@ and everything with
-- no scheme at all reading @other@.
--
-- One rule and one pass.  A scheme is what sits before the first @:@ and is
-- shaped like one — RFC 3986's letter followed by letters, digits, @+@, @-@ and
-- @.@ — so @https@, @http@, @mailto@, @id@, @file@ and org-glance's own
-- protocols all fall out of it rather than being named here.  The six words
-- 'linkTypes' declares are the ones ~\/sync spells; a scheme this has never seen
-- travels under its own name rather than being flattened into @other@, since the
-- word IS the answer and a popup listing it teaches more than a catch-all would.
--
-- Three honest consequences of deriving the type off the PREFIX alone.  Org's
-- internal links carry no scheme and read @other@, which is right — they name a
-- place rather than a protocol.  A relative file link without its prefix reads
-- @other@ where @[[file:…]]@ reads @file@: the type reports what the target
-- SAYS.  And a scheme-SHAPED word before a colon is taken at its word, so
-- @[[Meeting: notes]]@ reads @meeting@ — the alternative is a registry of known
-- schemes, and then an unheard-of scheme reads as prose, which is worse: the
-- popup exists to say what a link IS.
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

-- | The link types a browser tab CAN be pointed at.  Spelled once and read
-- three ways: the badge palette gives them the warm hues, the rest of
-- 'linkTypes' takes the cool ones, and the shell's @followable@ is this list
-- spliced into the page — so adding one is one edit rather than three that no
-- test ties together.
followableTypes :: [Text]
followableTypes = ["https", "http"]

-- | The link types the walked corpus spells, in the order the popup's badge
-- palette declares them: 'followableTypes' first, then the four a tab cannot
-- reach.  'linkType' does not consult this list — every one of these words falls
-- out of the scheme rule — so it is a VOCABULARY rather than a classifier, and a
-- type outside it is drawn with no badge hue rather than refused.
linkTypes :: [Text]
linkTypes = followableTypes <> ["glance", "mailto", "id", "file"]

-- | 'linkTypes' as SCHEMA.md badges, and the hue carries the one fact that
-- matters at the moment of pressing a key: the FOLLOWABLE types take the warm
-- keyword hues and the ones a tab cannot reach take the cool ones.  The page's
-- existing two lists, so the popup reads in the palette the table already uses
-- and this module grows no second colour language.
--
-- No @group@ field ('badge'): that one is the state column's own.
linkTypeBadges :: [Value]
linkTypeBadges =
  zipWith (badge Nothing) (followable <> unreachable) linkTypes
  where followable  = map (stateSlot "a") [0 .. length followableTypes - 1]
        unreachable = map (stateSlot "i") [0 ..]

-- | The link popup's columns: what a link IS, what the entry calls it, and where
-- it points.  SCHEMA.md Column objects through the same 'column' builder the
-- table's own use, so they sort like every other column of this page — the rows
-- arrive in the order the SUBTREE writes them, which is the order that means
-- something, and a reader who wants another may take one.
--
-- The @url@ column is plain text.  A muted aside is what the which-key palette
-- drew by hand, and no column KIND offers one — @text@, @number@ and @badge@ are
-- the whole of SCHEMA.md's set — so the target reads in the page's ordinary ink
-- and the column it sits in is what tells it from the title.  Inventing a kind
-- would be a renderer feature, and styling one from the shell would be this page
-- reaching into the table's cells.
linkColumns :: [Value]
linkColumns =
  [ column "type"  "Type"     "badge" ["badges" .= linkTypeBadges]
  , column "title" "Title"    "text"  []
  , column "url"   "Target"   "text"  []
  ]

-- | The manage-tags popup's columns: the tag itself, how much of the named set
-- carries it, and how many rows the whole tree has under it.
--
-- The tag column is keyed @title@, the way the link popup's description column
-- is: it is the readable NAME of the record a row stands for, and that is the
-- key this page's readers — the log line naming a row, a renderer's own display
-- rule — already look under.  A column keyed @tag@ would also invite the
-- renderer's multi-value sampling, which reads a cell as a whole tag RUN
-- (@:a:b:@); these cells are single words and there is nothing to split.
--
-- @on@ is the COVERAGE over the rows the command would run over — @all@, or
-- @k\/n@ — and is the client's arithmetic over @GET \/tags@' per-row answer.
-- @rows@ is the store-wide count that answer carries, so a reader deciding
-- whether to drop a tag can see whether it is this set's or the tree's.
tagColumns :: [Value]
tagColumns =
  [ column "title" "Tag"  "text"   []
  , column "on"    "On"   "text"   []
  , column "rows"  "Rows" "number" []
  ]

-- References
--
-- A REFERENCE is a link that points at another ROW, which is a narrower thing
-- than a link: most of what a subtree holds points out of the tree entirely.
-- The forms below are the ones ~\/sync actually spells, counted over the 6291
-- files the walk collects (2026-08-02) — the matcher implements what exists
-- rather than what org permits.

-- | The link protocols that name a row by its @ORG_GLANCE_ID@.  Each is
-- stripped and the rest is the id, case preserved, since a row id is
-- exact-string everywhere else in this library ('resolveIds').
--
-- Counted in the walked corpus: @org-glance-visit:@ 3867, @org-glance-open:@
-- 568, @org-glance-material:@ 28, and @id:@ zero — org's own earns its entry by
-- being org's own, at a cost of one list element.
--
-- Two org-glance protocols are deliberately absent, and both are common:
-- @org-glance-overview:@ (2726) names a TAG and @org-glance-state:@ (880) names
-- a keyword, so neither points at a row.  The census settles it — of their 52
-- and 6 distinct targets, exactly none is an @ORG_GLANCE_ID@.
refPrefixes :: [Text]
refPrefixes = ["org-glance-visit:", "org-glance-open:", "org-glance-material:", "id:"]

-- | TEXT's row references, normalized and deduplicated, in order of appearance
-- — a subtree's 'hrLinks'.  Read through 'orgLinks', so the bracket grammar
-- stays the one 'displayText' already holds.
--
-- The whole subtree rather than the cells: a reference is nearly always written
-- in the BODY of an entry, which is where a reader puts the sentence that
-- explains it.
refTargets :: Text -> [Text]
refTargets = refTargetsOf . orgLinks

-- | The references among LINKS, which 'orgLinks' already read.  Split off so a
-- caller wanting both answers about one subtree scans it once ('recordOf').
refTargetsOf :: [OrgLink] -> [Text]
refTargetsOf = nub . map detach . mapMaybe (refTargetOf . olTarget)

-- | TARGET as the row it names, or 'Nothing' where it names no row.  Three
-- shapes, and everything else — a @file:@ path, an @http@ URL, a protocol this
-- does not know — is a link that leaves the table and is dropped here rather
-- than kept for a matcher to skip over.
--
--   * one of 'refPrefixes', stripped: an @ORG_GLANCE_ID@.
--   * a leading @*@, stripped: org's @[[*Title]]@, 4 in the walked corpus.
--   * no @:@ and no @\/@ at all: org's bare @[[Title]]@, which cannot be a path
--     or a URL.  18 in the corpus and nearly all of them false — @[[key,asc]]@
--     and other bracketed prose — which is the cost of covering the form at all.
--
-- The two title shapes are one answer: what a link spells is compared against
-- 'hrTitle' as the file spells it ('refSpellings').  Org's own @[[Title]]@ is
-- a fuzzy search and this is exact, so a reference that only org would resolve
-- is one this does not.
refTargetOf :: Text -> Maybe Text
refTargetOf target
  | Just rest <- firstJust (`T.stripPrefix` target) refPrefixes = nonEmpty rest
  | Just rest <- T.stripPrefix "*" target                       = nonEmpty rest
  | T.any (\c -> c == ':' || c == '/') target                   = Nothing
  | otherwise                                                   = nonEmpty target
  where
    nonEmpty t = if T.null t then Nothing else Just t
    firstJust f = listToMaybe . mapMaybe f

-- | How a link may spell a reference to R: its @ORG_GLANCE_ID@ where it has
-- one, and its title, which is what the two bracket-title forms resolve
-- against.  The answer a @ref:@ predicate is matched over ('hrLinks').
--
-- The id comes off the headline rather than off 'hrId', which falls back to
-- @FILE#K@ for a row carrying no property: an ordinal is this view's own
-- invention and no file can hold a link to one.
refSpellings :: HeadlineRecord -> [Text]
refSpellings r = maybe id (:) (identity (hrHeadline r)) [hrTitle r]

-- | S with every run of C0 control characters, and DEL, standing as one space
-- — so a cell is one line and a multi-line one cannot be matched across the
-- break that is not there on screen.
squashControls :: Text -> Text
squashControls = T.concat . go
  where
    go s | T.null s    = []
         | T.null rest = [keep]
         | otherwise   = keep : " " : go (T.dropWhile control rest)
      where (keep, rest) = T.break control s
    control c = c < ' ' || c == '\DEL'

-- | CELL re-spelled with its tags in case-folded alphabetical order.
--
-- DISPLAY ONLY, applied at the @tag@ entry of 'viewColumns' alone.  Everything
-- else about a row's tags is the file's own order: the FILE (the span is never
-- touched), 'hrTags' (whose order DECIDES which tag's config governs the row —
-- 'keywordScopes' is first-wins, so sorting it would move a resolution rather
-- than a rendering), and @GET \/tags@, whose union is first-seen.
--
-- Membership readers are unaffected: 'tagged' and @tag:*archive*@ split the
-- cell, and a bare @tag:x@ is a substring of one tag.
--
-- Folded so a capital does not sort ahead of every lowercase tag; the sort is
-- STABLE, so two spellings folding alike keep the file's order.  A cell ALREADY
-- IN ORDER is handed straight back — the accessor runs per row per
-- @\/headlines@, and almost every row rebuilds nothing.
sortedTagsCell :: Text -> Text
sortedTagsCell cell
  | sorted == entries = cell
  | otherwise         = ":" <> T.intercalate ":" sorted <> ":"
  where entries = tagRunEntries cell
        sorted  = sortOn T.toCaseFold entries

-- | The entries of a tag RUN, org spelling it @:a:b:@: split on the colon and
-- drop the empties its two ends leave.  One spelling of that rule, since
-- 'tagsOfCell', 'sortedTagsCell' and the filter's own cell reader
-- ('Glance.Web.Filter') ask the same question of the same string and a second
-- copy would be a second reading of org's own syntax.  Exported for that third
-- caller.
--
-- 'tagEntries' is the near miss and is deliberately not this: it keeps the
-- INTERIOR positions, being what an edit measures a splice in.
tagRunEntries :: Text -> [Text]
tagRunEntries = filter (not . T.null) . T.splitOn ":"

-- | The tags CELL names, one per tag, lowercased through 'displayText' like the
-- search text — so a tag read off a row here is the same string a filter
-- compares against.
--
-- This is the vocabulary a producer's virtual filter keys come from
-- (@table-view\/SCHEMA.md@, Filter query): every distinct tag in the column is
-- a key, and a renderer deriving them from the rows it holds has to get the
-- same list out of the same cells.
tagsOfCell :: Text -> [Text]
tagsOfCell = tagRunEntries . T.toLower . displayText

-- | Does a row's display text contain Q?  Q is trimmed and lowercased the way
-- the renderer trims and lowercases its filter box, and an empty query matches
-- every row.
--
-- Written to take Q alone and hand back the test, so @filter (matchesSearch q)@
-- normalises the query once rather than once per row: over 13k rows that
-- rewrite is the difference between a 19 ms filter and a sub-millisecond one.
matchesSearch :: Text -> HeadlineRecord -> Bool
matchesSearch q
  | T.null needle = const True
  | otherwise     = T.isInfixOf needle . hrSearch
  where needle = T.toLower (T.strip q)

-- Identity

-- | RECORDS with one row per id, and what that cost.  A row id is what a
-- renderer keys updates off (@table-view\/SCHEMA.md@), so two rows sharing one
-- are not two rows: the second would overwrite the first on every frame, and
-- meanwhile the table shows the headline twice.
--
-- Which one stays is decided by the path ('Data.Org.Walk.claimById'):
-- org-glance's canonical store lives under @.org-glance\/data\/@ and everything
-- else claiming that id is a copy of it, so a canonical path wins; between two
-- paths of the same kind, walk order does, which is stable and is what the view
-- was showing before.  Every loser is reported rather than dropped quietly: a
-- duplicate id is nearly always a tree that should not have been walked, and
-- the count is a response header for exactly that reason.
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

-- | Is PATH inside one of org-glance's derived mirrors — the directories the
-- walk declines to enter ('Data.Org.Walk.isDerived')?  Re-exported so a watcher
-- can drop an event under one without reaching past this facade.
derivedPath :: FilePath -> Bool
derivedPath = isDerived

-- | Is PATH a file the walk reads ('Data.Org.Walk.isDocument') — an @.org@ name
-- that is not one of Emacs's sidecars?  Re-exported for the same reason: a
-- watcher decides what to re-read by the rule the walk decided what to read by,
-- rather than by a second copy of it.  One predicate is what keeps the two
-- sides from disagreeing in either direction — a file the walk skipped arriving
-- by inotify, or a file it read that no event can ever refresh.
documentPath :: FilePath -> Bool
documentPath = isDocument

-- | Is PATH inside org-glance's config area ('Data.Org.Walk.isConfig')?
-- Re-exported for the third answer a watcher owes an event: a file the walk
-- refused AND still has to act on.  A config file is never a row, and a change
-- to one changes how every OTHER file parses, so the watch answers it by
-- reseeding rather than by re-reading the path it was handed.
configPath :: FilePath -> Bool
configPath = isConfig

-- | A sort CHAIN, highest priority first: each key a column and whether it
-- ascends.  SCHEMA.md's @sort@ array, and what @?q=@'s @sort:@ tokens name.
--
-- The EMPTY chain is no sort at all: the rows stay in the order they arrived
-- and the view declares nothing, which is how SCHEMA.md reads an absent @sort@.
type SortChain = [(Text, Bool)]

-- | The view's default sort chain: what a query naming no @sort:@ token opens
-- on and is served in.
--
-- ONE list read twice — 'declaredSort' spells it onto the wire,
-- 'sortedForViewWith' arranges the rows by it — so the order a client is told
-- and the order it is served cannot disagree.  That pairing is the whole reason
-- a producer sorts: a page cut out of a different order than the one declared
-- is a different set of rows than the table would have put there.
--
-- STATE leads, by the badge PALETTE rather than alphabetically — the declared
-- @#+TODO:@ cycle, so the table opens in org's own order.  Title settles rows
-- sharing a state, the two dates settle the rest, every key ascending.
-- Priority is deliberately out: a fifth key behind four that have already
-- separated nearly every pair.  @sort:priority@ is how a reader asks for it.
defaultSortChain :: SortChain
defaultSortChain =
  [ ("state", True), ("title", True), ("deadline", True), ("scheduled", True) ]

-- | R's comparison value for the column KEY under PALETTE, or 'Nothing' for an
-- empty cell.
--
-- A 'Nothing' is SCHEMA.md's NULL: it sorts to one end of its own key, OUTSIDE
-- that key's direction.  An absent cell and a @\"\"@ one are the same null,
-- which is what @key:*empty*@ already reads.
--
-- The pair is (palette POSITION, folded TEXT) — the two ways SCHEMA.md orders a
-- column: a badge column by where its value sits in the palette, every other by
-- its text.  So the state column fills the first half and the rest the second.
--
-- Text is compared CASE-FOLDED: the browser collates with @localeCompare@,
-- case-insensitive at its primary strength, and raw code-point order would put
-- every capitalised title ahead of every lowercase one.  Folding is the closest
-- this side gets; the next key settles what it cannot.
--
-- Built ONCE per sort rather than per comparison, so a chain of four keys pays
-- four lookups instead of four per pair of rows.
sortCell :: TodoKeywords -> Text -> Maybe (HeadlineRecord -> Maybe (Int, Text))
sortCell palette key = read' <$> lookup key [(k, cell) | (k, _, _, cell) <- viewColumns]
  where
    ranked = paletteRank palette
    read' cell r = case cell r of
      Just value | not (T.null value) -> Just (rank value, text' value)
      _empty                          -> Nothing
    -- The state column orders by PALETTE POSITION and everything else by its
    -- text, folded.  The priority column is a third answer only in what it
    -- reads: its cell wears org's brackets, so the comparator reads the LETTER
    -- through them ('priorityLetter') and @[#A]@ still sorts ahead of @[#B]@ —
    -- which the bracketed text would do anyway, and would stop doing the moment
    -- a tree spelled one of them differently.
    rank value  = if key == "state" then ranked value else 0
    text' value | key == "state"    = ""
                | key == "priority" = priorityLetter value
                | otherwise         = T.toCaseFold value

-- | Where a value sits in PALETTE, or one past its end for a keyword it does
-- not name.  The renderers' rule for a badge column: palette order is sort
-- order, and everything unlisted ties at the back.
--
-- The order is worked out once per PALETTE and the value looked up in it, so a
-- caller holding the function pays for the flattening once however many rows it
-- ranks.
paletteRank :: TodoKeywords -> Text -> Int
paletteRank (TodoKeywords actives inactives) =
  let ordered = actives <> filter (`notElem` actives) inactives
      places  = zip ordered [0 ..]
  in \value -> fromMaybe (length ordered) (lookup value places)

-- | RECORDS in the order CHAIN states, with the state column's PALETTE given.
--
-- Each key compares by 'sortCell', empty cells last whatever the direction, and
-- 'Data.List.sortBy' is stable — so rows equal on every key keep walk order,
-- which is what both renderers do with the same chain.  A key naming no column
-- is dropped, the way both renderers drop one.  The EMPTY chain leaves the rows
-- exactly as they arrived, which is the view that declares no @sort@ at all —
-- spelled rather than left to a fold over no keys, so document order costs no
-- walk at all.
sortedForViewWith :: TodoKeywords -> SortChain -> [HeadlineRecord]
                  -> [HeadlineRecord]
sortedForViewWith _       []    = id
sortedForViewWith palette chain = sortBy (mconcat (mapMaybe key chain))
  where
    key (k, asc) = compareBy asc <$> sortCell palette k
    -- Nulls last, OUTSIDE the direction: the emptiness is settled first and the
    -- values only reach the second comparator once both cells are there.
    --
    -- ONE EXTRACTION A SIDE.  `Ordering''s `<>' short-circuits only when the
    -- first comparator answers non-EQ — exactly when one cell is empty — so
    -- pairing `comparing (isNothing . value)' with `comparing value' ran
    -- `value' TWICE per side in the common case where both are there, and
    -- `value' is a cell read plus a palette lookup or a case fold.  Over an
    -- unlimited answer that is 20000 rows times the chain's keys, twice.
    compareBy asc value a b = case (value a, value b) of
      (Nothing, Nothing) -> EQ
      (Nothing, Just _)  -> GT
      (Just _,  Nothing) -> LT
      (Just x,  Just y)  -> if asc then compare x y else compare y x

-- | 'sortedForViewWith' in 'defaultSortChain', over the palette RECORDS
-- themselves imply.
--
-- Sound for ordering RECORDS, since every state they hold is declared by one of
-- their own files and 'mergeKeywords' keeps a keyword where it was first seen.
-- It is not the STORE's palette, though, and a caller that has one should pass
-- it: two files declaring the same pair of keywords in opposite orders, and a
-- filter that hides every row of the first, leaves this reading the second's
-- order where the columns a client was served still carry the first's.
sortedForView :: [HeadlineRecord] -> [HeadlineRecord]
sortedForView records =
  sortedForViewWith (mergeKeywords (map hrKeywords records)) defaultSortChain records

-- Subtrees

-- | R's subtree as its file spells it: stars, planning, drawer, body and every
-- child, raw.  A slice, so it shares the document rather than copying it — the
-- caller encodes it into a response and drops it.
subtreeText :: HeadlineRecord -> Text
subtreeText r = sliceSpan (hrDoc r) (hrSubtree r)

-- | One headline INSIDE a row's subtree, past the row's own stars: where it
-- sits in the outline, which entry it hangs under, and the record that
-- addresses it.
--
-- The INDEX of an entry in 'subtreeEntries'' answer is what @?child=K@ names, so
-- the addressing is document order over the WHOLE subtree rather than a count
-- per level: a grandchild is one number away from the row it belongs to, and a
-- client is handed that number rather than working a path out of the levels.
data SubtreeEntry = SubtreeEntry
  { seLevel  :: !Int             -- ^ org's outline level; the row's own is 1.
  , seParent :: !Int             -- ^ the index it hangs under, @-1@ being the row itself.
  , seRecord :: !HeadlineRecord  -- ^ the entry as a record: cells, extent, digest.
  } deriving (Show)

-- | R's descendants, in document order.
--
-- A record keeps its OWN headline and nothing deeper, so the descendants are
-- read back out of the document R was parsed from — one parse per call, from the
-- seed the load used ('Data.Org.Config.seedContext'), which is what makes a
-- child's keyword the keyword the loader would have read there.  Their extents
-- come out of the same 'subtreeSpans' the rows do, so a child's slice is org's
-- outline rule over the whole document rather than a second rule over a
-- fragment, and a document that no longer parses yields none.
--
-- Ids are the ROW's with the index behind it (@ROW\/K@).  Nothing registers one
-- and no route resolves one: they exist so a refusal names something a reader
-- can place.
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
    -- One left-to-right pass with a stack of the entries still open: anything
    -- at this level or deeper is closed by it, and whatever is left on top is
    -- what it hangs under.  Org permits a level jump, so the parent is the
    -- nearest SHALLOWER entry rather than the one a level up.
    parented = go []
      where
        go _open [] = []
        go open ((k, e@(lvl, _h, _sub)) : rest) =
          SubtreeEntry lvl parent (made k e) : go ((k, lvl) : still) rest
          where still  = dropWhile ((>= lvl) . snd) open
                parent = case still of
                  ((j, _l) : _rest) -> j
                  []                -> -1

-- | ENTRIES' K-th, or 'Nothing' where there is no such descendant.  The bounds
-- rule is here rather than at each caller, since @?child=K@ is where every
-- out-of-range number arrives.
subtreeEntryAt :: [SubtreeEntry] -> Int -> Maybe SubtreeEntry
subtreeEntryAt entries k
  | k < 0     = Nothing
  | otherwise = listToMaybe (drop k entries)

-- | How many lines of BODY — R's, regions already lifted — are R's OWN: the
-- ones ahead of FIRST, its first descendant.
--
-- ONE OWNER PER BYTE, one level down.  The lens hands a client the whole
-- subtree's body, children and all, so a reader looking at the ENTRY needs to
-- know where its own text stops — else the same bytes are drawn twice, as this
-- entry's last paragraph and as the child that owns them.
--
-- Counted by DIFFERENCE rather than by looking for a star: the three regions
-- all sit above the first child, so everything from that child on is in the
-- body unmoved.  Reading a leading @*@ instead would spell the parser's
-- star-run rule a second time, and a body line opening @*bold*@ would cut the
-- entry short.
ownBodyLines :: HeadlineRecord -> Text -> Maybe HeadlineRecord -> Int
ownBodyLines r body first' = case first' of
  Nothing     -> whole
  Just deeper -> whole - length (linesWith (T.drop (cut deeper) (subtreeText r)))
  where whole = length (linesWith body)
        cut deeper = spanStart (hrSubtree deeper) - spanStart (hrSubtree r)

-- Lens
--
-- The parts of a subtree taken out and put back.  ONE OWNER PER BYTE: three
-- regions are lifted — the planning line, the headline's own property drawer,
-- its own logbook — and every byte left is the body's.  A part nobody edited
-- goes back as the very line it came in on, so editing one part does not
-- re-spell the others.
--
-- A headline's OWN regions only: a child's drawer is body text here.
--
-- Two of the four are the SERVER's and a client neither sees nor sends them —
-- 'hiddenProperties' (identity a rename would break) and the logbook (a record
-- nothing here edits).  They go back verbatim whatever a client says.

-- | The property keys a client is never shown and never writes.
--
-- @ORG_GLANCE_ID@ is the row id: renaming it renames the row the table keys its
-- updates off, and the sheet would be looking at a different headline
-- afterwards.  Hiding it is cheaper than a rule about which edits to a shown
-- value are allowed, and honest in a way a warning beside an editable field is
-- not.  'captureProperty' is the other half of the same argument from the other
-- end: a creation time is a fact about when a row was written, so a sheet that
-- let it be edited would be offering to make the record say something else.
--
-- ONE list, read by both halves of the lens: 'headlineParts' drops these pairs
-- and 'recomposedSubtree' puts their original lines back, so extending it is
-- one edit here and none anywhere else.
hiddenProperties :: [Text]
hiddenProperties = [headlineIdProperty, captureProperty]

-- | Is KEY one the server owns?  Folded and stripped, since a drawer spells its
-- keys however the file that holds it does.
hiddenProperty :: Text -> Bool
hiddenProperty key = T.toUpper (T.strip key) `elem` hiddenProperties

-- | The planning keywords, in the order org writes them where nothing says
-- otherwise.  A line may permute them freely, so this decides only where an
-- entry the file did not already carry goes.
planningKeywords :: [Text]
planningKeywords = ["SCHEDULED", "DEADLINE", "CLOSED"]

-- | A subtree split into what a client edits and what the server keeps.
--
-- With no region of a kind the corresponding field is empty, which is the shape
-- 'recomposedSubtree' reads back as "leave it without one".  'hpLogbook' is
-- carried OUTWARD only: the recompose takes it off the record rather than off
-- this value, so a client cannot write one by sending one.
data HeadlineParts = HeadlineParts
  { hpBody       :: !Text            -- ^ the subtree with all three regions lifted out.
  , hpProperties :: ![(Text, Text)]  -- ^ the drawer's pairs in file order, 'hiddenProperties' dropped.
  , hpPlanning   :: ![(Text, Text)]  -- ^ the planning keywords present and each one's timestamp text, in line order.
  , hpLogbook    :: !Text            -- ^ the headline's own @:LOGBOOK:@ drawer verbatim; @""@ when it has none.
  } deriving (Eq, Show)

-- | R's subtree split into 'HeadlineParts'.
--
-- Every cut is by WHOLE LINES, the newline that ends each one included, so the
-- body is left as the lines that were around them, byte for byte.  Anything
-- trailing a region's last line belongs to that region — it is on one of its
-- lines.
headlineParts :: HeadlineRecord -> HeadlineParts
headlineParts r = HeadlineParts
  { hpBody       = withoutSpans subtree (regionSpans [planAt, drawAt, logAt])
  , hpProperties = [ p | p <- drawerPairs subtree drawAt, not (hiddenProperty (fst p)) ]
  , hpPlanning   = [ (key, sliceSpan subtree sp) | (key, sp) <- entries ]
  , hpLogbook    = maybe "" (sliceSpan subtree) logAt
  }
  where (subtree, entries, planAt, drawAt, logAt) = regionsOf r

-- | R's subtree, its planning ENTRIES, and where each of the three regions
-- sits in it: the planning line, the headline's own drawer, its own logbook.
-- Each answer feeds the next ('logbookSlice' steps over the drawer), so this is
-- one pass rather than three questions.
--
-- ONE LOCATION, read by both halves of the lens: 'headlineParts' cuts on these
-- spans and 'recomposedSubtree' counts the body lines they took, so the two
-- cannot come to different answers about where a region was.
regionsOf :: HeadlineRecord
          -> (Text, [(Text, Span)], Maybe Span, Maybe Span, Maybe Span)
regionsOf r = (subtree, entries, planAt, drawAt, logAt)
  where subtree = subtreeText r
        entries = planningEntries r subtree
        planAt  = planningSlice entries subtree
        drawAt  = drawerSlice r subtree
        logAt   = logbookSlice drawAt subtree

-- | R's subtree as its file would hold PARTS.
--
-- Each region goes back on the line it was cut from — counted in lines from the
-- start of the subtree, so an edit further down the body cannot move it — and a
-- region the headline never had gets the place org puts one: the planning line
-- right after the title line, the drawer after that.
--
-- What each part costs is decided by whether it moved.  A property the drawer
-- already held is written as the line it already was; a planning entry the line
-- already carried is written as the text it already was, where it already was;
-- anything else is rendered.  The hidden properties and the logbook are taken
-- off R and are never rendered at all.
recomposedSubtree :: HeadlineRecord -> HeadlineParts -> Text
recomposedSubtree r parts = spliceRegions (hpBody parts) regions
  where
    (subtree, entries, planAt, drawAt, logAt) = regionsOf r
    -- Which line of the BODY each region goes back on: the line it sat on in the
    -- subtree, less the lines every region ahead of it took out.  Subtree
    -- indices would leave a GAP where a region was cleared — a drawer whose
    -- planning line has just come off would land a line late — and the body is
    -- the only text the arithmetic can be done in, since it is the only one
    -- that exists at this point.  A region the headline never had goes where org
    -- puts one, which in body coordinates is the line under the title.
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
                         [ p | p <- hpProperties parts, not (hiddenProperty (fst p)) ] )
    logs  = ( bodyLine 0 logAt, maybe "" (sliceSpan subtree) logAt )

-- | One region of a subtree: the body line it belongs above, and what goes back
-- there, terminated or not.  Positional, since 'spliceRegions' takes both apart
-- in one pattern and a selector for the text would be a name nothing reads.
data Region = Region !Int !Text

-- | BODY with each of REGIONS put back at the line it belongs above.
--
-- Ascending, counting only the BODY lines consumed, so two regions naming one
-- line land in list order rather than one displacing the other — which is what
-- a headline growing a planning line and a drawer in the same commit needs.  A
-- body with fewer lines than an index takes the region at the end, which is
-- where a client that deleted the lines above it has left room.
spliceRegions :: Text -> [Region] -> Text
spliceRegions body regions = knit (go 0 (linesWith body) (sortOn above regions))
  where
    above (Region line _text) = line
    go _seen ls [] = ls
    go seen ls (Region at block : rest) =
      -- 'splitAt' clamps at both ends, so a region naming a line already spent
      -- takes none and one past the body's last takes all of it.
      taken <> linesWith block <> go (seen + length taken) left rest
      where (taken, left) = splitAt (at - seen) ls

-- | LINES concatenated, each but the last closed with a newline.  A body whose
-- last line has none still ends without one; a region spliced behind it gets
-- the newline that keeps it a line of its own.
knit :: [Text] -> Text
knit ls = T.concat (zipWith close ls [1 :: Int ..])
  where n = length ls
        close l i | i == n || "\n" `T.isSuffixOf` l = l
                  | otherwise                       = l <> "\n"

-- | SUBTREE with every span in SPANS taken out.  They are disjoint and in
-- source order, which is what 'regionSpans' answers with.
--
-- A deletion is an edit with nothing in it, so the splice is
-- 'Edit.applyEdits'\'s — the same one pass, and the overlap and bounds checks
-- for free.  A batch it refuses cannot come out of 'regionSpans'; SUBTREE
-- unchanged is what a caller sees if one ever does, which is the region left
-- standing rather than a document cut at the wrong offsets.
withoutSpans :: Text -> [Span] -> Text
withoutSpans subtree sps =
  fromRight subtree (Edit.applyEdits subtree [ Edit.Edit sp "" | sp <- sps ])

-- | SLICES in source order, the ones a headline has no region for dropped.  The
-- three are located once by the caller, each answer feeding the next
-- ('logbookSlice' steps over the drawer), so this arranges rather than finds.
regionSpans :: [Maybe Span] -> [Span]
regionSpans = sortOn spanStart . catMaybes

-- Regions

-- | Where R's own drawer sits in SUBTREE, as a span of SUBTREE's own offsets
-- covering whole lines.  'Nothing' when the headline carries no drawer — and,
-- defensively, when the span is not inside the subtree, which the span
-- invariants forbid.
drawerSlice :: HeadlineRecord -> Text -> Maybe Span
drawerSlice r subtree = do
  sp <- hsProperties (spans (hrHeadline r))
  Span from to <- localSpan r subtree sp
  pure (Span from (pastLine subtree to))

-- | Where the planning line ENTRIES sit in SUBTREE, as a whole-line span.
-- 'Nothing' when the headline has no planning at all.
--
-- The three planning spans cover their timestamps alone and permute freely on
-- one line, so the region is the LINE the outermost of them sits on: the
-- keywords that open the entries and whatever spacing is between them belong to
-- it too, which is what lets an untouched line go back byte for byte.
planningSlice :: [(Text, Span)] -> Text -> Maybe Span
planningSlice entries subtree = case map snd entries of
  []  -> Nothing
  sps -> Just (Span (lineStart subtree (minimum (map spanStart sps)))
                    (pastLine subtree (maximum (map spanEnd sps))))

-- | R's planning entries as @(KEYWORD, timestamp span)@ in SUBTREE's offsets,
-- in the order the line writes them.
planningEntries :: HeadlineRecord -> Text -> [(Text, Span)]
planningEntries r subtree = sortOn (spanStart . snd)
  [ (key, local)
  | (key, sp) <- presentPlanning (headlineSpans r)
  , Just local <- [localSpan r subtree sp] ]

-- | HS's planning entries that are there, keyword by keyword, in
-- 'planningKeywords' order — which is org's default order and NOT necessarily
-- the line's, the three permuting freely.
presentPlanning :: HeadlineSpans -> [(Text, Span)]
presentPlanning hs =
  [ (key, sp)
  | (key, Just sp) <- zip planningKeywords [hsSchedule hs, hsDeadline hs, hsClosed hs] ]

-- | Where R's OWN logbook drawer sits in SUBTREE, as a whole-line span, SKIP
-- being the property drawer's extent.
--
-- Located TEXTUALLY, because a @:LOGBOOK:@ drawer is not part of a headline's
-- parse and what makes one this headline's is where it sits: past the title
-- line, ahead of the first child's stars.  The property drawer is stepped over
-- rather than searched, so a @:LOGBOOK:@ line that somehow sat inside one stays
-- the properties' — one byte, one owner, decided here rather than by whichever
-- finder ran first.
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
    -- An unterminated drawer owns every line it may own, which is where the
    -- parser reading one would have stopped as well.
    closes sp rest = case break (ends . snd) rest of
      (_before, (e, _line) : _after) -> spanEnd e
      (before, [])                   -> foldl' max (spanEnd sp) (map (spanEnd . fst) before)

-- | SP, a span of the document R was parsed from, in SUBTREE's own offsets, or
-- 'Nothing' where it does not lie inside it — which the span invariants forbid
-- and this checks anyway.
localSpan :: HeadlineRecord -> Text -> Span -> Maybe Span
localSpan r subtree sp
  | from < 0 || to > T.length subtree || from > to = Nothing
  | otherwise                                      = Just local
  where local@(Span from to) = shiftSpan (negate (spanStart (hrSubtree r))) sp

-- Planning

-- | How the planning line at SP in SUBTREE is spelled, so a rewritten one reads
-- like the file it goes back into, ENTRIES being the ones it carries.  BODY
-- supplies the line ending for a headline that has no line to copy one from.
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

-- | The text of the entry KEY opens in LINE, from its keyword to the end of
-- AT.  'Nothing' where the keyword is not in front of the timestamp, which the
-- parse forbids and this checks anyway.
rawEntry :: Text -> Text -> Span -> Maybe Text
rawEntry key line at =
  (\from -> T.take (spanEnd at - from) (T.drop from line)) <$> entryOpening line key (spanStart at)

-- | Where the entry KEY opens in TEXT, given that its timestamp starts at AT:
-- the offset of the last @KEY:@ ahead of AT ON AT'S OWN LINE, or 'Nothing'
-- where that line has none.
--
-- The LAST one, because only horizontal space may sit between a planning
-- keyword and its timestamp.  Bounded to the line so the answer cannot come
-- from a @SCHEDULED:@ further up the document — which is what lets a caller
-- hand this the whole file rather than a line it cut first, and is why no
-- caller owes a check afterwards.
entryOpening :: Text -> Text -> Int -> Maybe Int
entryOpening text key at
  | T.null ahead = Nothing
  | otherwise    = Just (from + T.length ahead - T.length marker)
  where marker = key <> ":"
        from   = lineStart text at
        ahead  = fst (T.breakOnEnd marker (sliceSpan text (Span from at)))

-- | STYLE's planning line carrying WANT, or @""@ where WANT is empty — which is
-- how the last entry coming off takes the line with it.
--
-- An entry the file already carried and nobody changed goes back as the very
-- text it was, in the place it was.  Everything else is rendered
-- @KEYWORD: value@ and joins behind them in 'planningKeywords' order, so the
-- entries that moved are canonical among themselves and the ones that did not
-- are untouched.
planningText :: PlanningStyle -> [(Text, Text)] -> Text
planningText style want
  | null entries = ""
  | otherwise    = psIndent style <> T.unwords (map spell entries) <> psEol style
  where
    entries = kept <> added
    kept    = [ p | (p, _raw) <- psRaw style, p `elem` want ]
    added   = [ p | key <- planningKeywords, p <- want, fst p == key, p `notElem` kept ]
    spell p = fromMaybe (fst p <> ": " <> snd p) (lookup p (psRaw style))

-- | Is VALUE a planning timestamp org would read back as one?
--
-- Asked of the very line the write would produce rather than of a timestamp
-- grammar spelled a second time here: a value that does not reparse turns the
-- planning line into body text on the next load, and the entry the author
-- thought they set is gone.  A value carrying a newline is refused outright —
-- it would be a second line, and a planning line is one.
readsAsTimestamp :: Text -> Bool
readsAsTimestamp value = either (const False) (isJust . timestampOf) (oneLine () () value)

-- Properties

-- | The drawer pairs at SLICE in SUBTREE, in file order and with nothing hidden.
drawerPairs :: Text -> Maybe Span -> [(Text, Text)]
drawerPairs subtree slice = case slice of
  Nothing -> []
  Just sp -> [ (key, value) | (key, value, _raw) <- drawerRows (sliceSpan subtree sp) ]

-- | How R's drawer is spelled, so a rewritten one reads like the file it goes
-- back into.  BODY supplies the line ending for a headline that has no drawer
-- to copy one from.
data DrawerStyle = DrawerStyle
  { dsOpen   :: !Text                    -- ^ the @:PROPERTIES:@ line, terminator and all.
  , dsClose  :: !Text                    -- ^ the @:END:@ line, which ends the block.
  , dsIndent :: !Text                    -- ^ what a rendered line is indented by.
  , dsRaw    :: ![((Text, Text), Text)]  -- ^ each pair a client may write, and its line.
  , dsHidden :: ![(Int, Text)]           -- ^ the server's own lines, and where in the block they sat.
  }

-- | What a line rendered into STYLE's drawer ends with: the closing line's own
-- ending, which is the drawer's, which is the file's.  Derived rather than
-- stored, since a stored copy could only ever be this.
dsEol :: DrawerStyle -> Text
dsEol = eolOf . dsClose

-- | How the drawer at SLICE in SUBTREE is spelled, BODY standing in for the
-- parts a headline with no drawer has nothing to copy.
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

-- | STYLE's drawer holding PROPS: the opening line, a line per property, the
-- closing line — and @""@ where there is nothing at all to hold, which is how a
-- client deletes a drawer.
--
-- The server's own lines are woven back in at the indices they sat at, so a
-- hidden property survives a client that never mentioned it, in its own place
-- and byte for byte.  Ascending, so each insertion finds the list already the
-- right length in front of it.
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
    -- Consumed rather than looked up: one pair written twice with two spacings
    -- keeps both, and a pair the client sent twice does not reuse one line.
    taking p raws = case break ((== p) . fst) raws of
      (before, (_p, raw) : after) -> Just (raw, before <> after)
      _absent                     -> Nothing
    rendered (key, value) =
      dsIndent style <> ":" <> key <> ":"
        <> (if T.null value then "" else " " <> value) <> dsEol style

-- | LINES with each of KEPT put back at the index it names, lowest first.
weave :: [(Int, Text)] -> [Text] -> [Text]
weave kept ls = foldl' put ls (sortOn fst kept)
  where put acc (at, line) = before <> [line] <> after
          -- 'splitAt' clamps, so an index past the end appends without this
          -- walking the list to find out how long it is.
          where (before, after) = splitAt at acc

-- | BLOCK's property lines: everything between its @:PROPERTIES:@ and @:END:@
-- lines, each as the key it names, the value it carries and the raw line it is.
--
-- Read by splitting lines rather than through the parser's own 'Properties': a
-- parsed property has its key uppercased and its value re-tokenised, and the
-- lens owes a client the file's own spelling of both.  A drawer that reached
-- here came out of a document that parsed, so every line between the two is a
-- property line; one that somehow is not comes back keyless, which a client
-- reads as a row to drop.
drawerRows :: Text -> [(Text, Text, Text)]
drawerRows block = [ (key, value, raw) | raw <- inner (linesWith block)
                                       , let (key, value) = propertyOf raw ]
  where inner ls = drop 1 (take (length ls - 1) ls)

-- | The key LINE names, as written between its colons, and the value it
-- carries with the space around it stripped.  A line that is not a property at
-- all comes back keyless.
propertyOf :: Text -> (Text, Text)
propertyOf line = case T.uncons (T.stripStart line) of
  Just (':', rest) | (key, closed) <- T.breakOn ":" rest, not (T.null closed)
                   -> (key, T.strip (T.drop 1 closed))
  _notAProperty    -> ("", T.strip line)

-- | The offset in T past the newline ending the line offset AT sits on, or T's
-- length when that line has none.
pastLine :: Text -> Int -> Int
pastLine t at = maybe (T.length t) (\i -> at + i + 1) (T.findIndex (== '\n') (T.drop at t))

-- | The offset in T where the line offset AT sits on begins.
lineStart :: Text -> Int -> Int
lineStart t at = T.length (fst (T.breakOnEnd "\n" (T.take at t)))

-- | The horizontal space LINE opens with.
indentOf :: Text -> Text
indentOf = T.takeWhile horizontal

-- | Is C horizontal space — the run a command deletes with a keyword, and the
-- run a line is indented by?  Org's own distinction: a newline ends a line and
-- these two do not, so a command taking "the space behind this word" must never
-- take the line's end with it.
horizontal :: Char -> Bool
horizontal c = c == ' ' || c == '\t'

-- | How wide the horizontal run T opens with is — the run a command deletes
-- with the token in front of it, measured where that token ends.
runWidth :: Text -> Int
runWidth = T.length . T.takeWhile horizontal

-- | How wide the horizontal run T ends with is, which is the same measurement
-- taken from the other side: the separator in front of a token.
runWidthEnd :: Text -> Int
runWidthEnd = T.length . T.takeWhileEnd horizontal

-- | XS's first element, or FALLBACK where it has none.
firstOr :: a -> [a] -> a
firstOr fallback xs = case xs of { (x : _rest) -> x; [] -> fallback }

-- | Where each of HEADS runs, in the source order they arrive in, over a
-- document of LEN characters.  A headline's subtree starts at its stars
-- ('hsFull'), which is where the headline itself starts, and ends where the
-- next headline at its own level or shallower begins — org's outline rule, so
-- the slice covers the headline, its body and every descendant, and nothing
-- past them.  The last headline of a file runs to the end of the document.
--
-- The slice may therefore end in blank lines: whatever sits between one
-- subtree's last body line and the next headline's stars belongs to the
-- subtree above it, the way an editor's outline command takes it.
--
-- One right-to-left pass with a stack of the headlines still open.  Entries
-- deeper than the one being placed are its descendants and are dropped; what
-- is left on top is the headline that closes it, and the stack is dropped down
-- to it, so each entry is pushed and popped once — linear whatever the nesting.
subtreeSpans :: Int -> [Headline] -> [Span]
subtreeSpans len heads = snd (foldl' place ([], []) (reverse (map extent heads)))
  where
    extent h = (levelOf h, spanStart (hsFull (spans h)))
    place (open, ends) (lvl, start) = ((lvl, start) : closers, Span start end : ends)
      where closers = dropWhile ((> lvl) . fst) open
            end = case closers of
              ((_lvl, next) : _rest) -> next
              []                     -> len

-- | KW with both lists' spines and elements forced, which is what makes a
-- keyword set safe to STORE.  A strict field buys WHNF and no more — the first
-- cons cell — so an unforced set is a thunk over the parse it was read from,
-- and a record holding one would pin its file's whole element tree for the life
-- of the process.  Both stored sets go through this: the recognized union and,
-- because the same trap is one field away, the file's own declarations.
forcedKeywords :: TodoKeywords -> TodoKeywords
forcedKeywords kw = forcing (tkActive kw <> tkInactive kw) kw

-- | H's row identity: its @ORG_GLANCE_ID@ property, else @"FILE#K"@ — the path
-- and ORDINAL, H's 0-based place among the file's EMITTED ROWS (numbered in
-- 'recordsOf' after both filters).
--
-- The ordinal is what a row's identity survives.  It moves only when the file's
-- rows are REORDERED, INSERTED into or REMOVED ahead of this one — so editing a
-- title, a state, a body, a drawer or a child renames nothing.  What churns is
-- the class it cannot absorb: a new first entry renumbers everything behind it,
-- and an entry going blank is that class wearing another hat.  An
-- @ORG_GLANCE_ID@ is immune, which is the reason to write one.  The character
-- offset this replaced moved on ANY edit above the headline.
--
-- The two forms share one namespace, resolved by exact string and never parsed
-- apart, so the separator carries no rule.  A headline WRITING an
-- @ORG_GLANCE_ID@ that spells another row's @FILE#K@ collides like any two
-- claims on one id: one row is kept and the other reported.
rowId :: FilePath -> Int -> Headline -> Text
rowId path ordinal h = maybe (rowIdIn path ordinal) detach (identity h)

-- | The id a row with no @ORG_GLANCE_ID@ answers to: PATH and its ORDINAL.
--
-- Exported because @capture@ has to NAME the row it just made and the store has
-- not read the file yet ('Glance.Web.Commands'), so the daemon spells an id the
-- next load will spell back.  ONE spelling of the separator either way.
rowIdIn :: FilePath -> Int -> Text
rowIdIn path ordinal = T.pack path <> "#" <> T.pack (show ordinal)

-- | TS's start as the wire spells a date: @"YYYY-MM-DD"@, plus @" HH:MM"@ when
-- the source carried a time of day.  A computed value rather than a slice: ISO
-- is the contract, and org's bracketed spelling stays in the file.
isoStamp :: Timestamp -> Text
isoStamp ts = spelled fmt (tsmTime moment)
  where moment = tsStart ts
        fmt | tsmHasTime moment = "%Y-%m-%d %H:%M"
            | otherwise         = "%Y-%m-%d"

-- | T detached from the document array it slices, so keeping the text does not
-- keep the file.
detach :: Text -> Text
detach = T.copy

-- | Force TS spine and elements, then yield X.  A strict field forces a
-- 'Maybe' to its constructor only, and a cell left as a thunk retains the file
-- it would have been cut from.
forcing :: [a] -> b -> b
forcing ts x = foldr seq x ts

-- | R with every cell evaluated, so the record can outlive its document.  The
-- digest is one of them: a thunk over it would retain the file's bytes, which
-- are the one thing a loaded record has no other reason to keep.
-- 'hrLinks' is a LIST, so its spine is forced beside its elements: a strict
-- field forces the outermost cons alone, and a tail left as a thunk retains the
-- document every target was cut from — the one thing 'detach' is there to stop.
forceRecord :: HeadlineRecord -> HeadlineRecord
forceRecord r =
  forcing (hrId r : hrCategory r : hrTitle r : hrTags r : hrDigest r : hrSearch r
             : hrLinks r <> optional)
          (foldr seq r (hrActive r))
  where optional = catMaybes [hrState r, hrPriority r, hrScheduled r, hrDeadline r]

-- Write-back

-- | Why a 'replaceSpans' did not land.  Either way the file is byte-identical
-- to what it held before the call (docs/invariants.md, Architecture).
data WriteFailure
  = WriteDrift !Text    -- ^ the digest the file holds now, which is not the pinned one.
  | WriteRefused !Text  -- ^ read, decode, splice or rename trouble, spelled for a caller to show.
  deriving (Eq, Show)

-- | PATH's text and the digest of the bytes it holds, or @("", "")@ where there
-- is nothing readable there.
--
-- ONE read answers both, so the offsets a caller measures in the text and the
-- digest it pins the write to describe one document — the rule materialize keeps
-- by taking both at load, kept here for a file the store never loaded.  The
-- EMPTY digest is 'Data.Org.Edit.editFile''s pin for a file that is not there,
-- so a capture into a tree with no inbox creates one.  An unreadable file that
-- IS there answers the empty pin too, which is safe rather than lossy: the write
-- re-reads, digests what it finds, and refuses as drift.
currentDocument :: FilePath -> IO (Text, Text)
currentDocument = fmap (fromMaybe ("", "")) . Edit.readDocument

-- | Replace each span of FILE with the text beside it, provided FILE still
-- digests to DIGEST; the new digest comes back, so a caller chains an edit
-- without re-reading.
--
-- THE LOCK IS THE POINT.  DIGEST is the one a record was loaded with and every
-- span indexes that same text, so either the file is still the document the
-- offsets were measured in or nothing is written.  The write is
-- 'Data.Org.Edit.editFile': one drift check, one pass whatever the batch, one
-- atomic replace — so a command over several rows of one file is ONE write and
-- either all its edits land or none.
--
-- It is also THE DOOR every write leaves through (a command, a commit, a
-- capture, a config edit), which is why the note to org-glance is taken here
-- and nowhere else.  It fires only for a blob under a store's @data\/@ and
-- cannot fail the write: by the time it runs the rename has happened.
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
-- What a structured command costs a file, as span edits over the text the
-- record was parsed from.  Nothing here reads or writes a file: a caller hands
-- the result to 'replaceSpans', which is where the drift lock is, and a caller
-- collecting several rows of one file hands it ONE batch.
--
-- The span math is here because 'Data.Org.HeadlineSpans' is the private
-- sublibrary's.  It is also the only place that can be right about it: the
-- insertion points below are not derivable from the cells the wire carries.

-- 'archiveTag' — the tag a headline wears once it is archived — is org's own
-- name for it and lives in 'Data.Org', re-exported here: the filter key that
-- hides an archived row is that literal folded ('Glance.Web.Filter'), and the
-- scan's index comparison reads a blob's archive flag off the same one, so the
-- write, the predicate over what it wrote and the oracle all spell it once.

-- | Does R carry TAG?  Read off the tags cell through the same 'tagsOfCell' the
-- filter vocabulary is built with, so presence here means exactly what a
-- @TAG:@ predicate means — FOLDED, so a row spelling @:Work:@ carries @work@ and
-- adding it again costs no edit.
--
-- It is what makes both tag commands idempotent, from opposite sides:
-- 'addTagEdits' owes nothing to a row that has it, 'removeTagEdits' nothing to a
-- row that does not.
--
-- The fold happens on the TAG, once per partial application, which is what
-- keeps 'archived' — a predicate over every row of every @\/headlines@ — from
-- re-folding a constant per row.
tagged :: Text -> HeadlineRecord -> Bool
tagged tag = \r -> want `elem` tagsOfCell (hrTags r)
  where want = T.toLower tag

-- | Does R carry 'archiveTag'?  Archiving is adding ONE tag, so this is
-- 'tagged' at that name and the served view's exclusion reads the same presence
-- rule a @-archive:@ predicate does.
archived :: HeadlineRecord -> Bool
archived = tagged archiveTag

-- | TEXT as an org tag, or why it is not one.  The wall @add-tag@ and
-- @remove-tag@ put up, and a whole-request refusal rather than a per-row one: a
-- string that is not a tag is not a tag for any row.
--
-- The charset is the PARSER's own ('Data.Org.isTagChar') rather than a copy of
-- org's @org-tag-re@, because what this server writes has to reparse HERE: a
-- tag carrying a character 'Data.Org.Parser.tagsP' declines does not end up in
-- the tags run, it takes the whole run down into title text on the next load.
-- The sets differ by @-@ alone — org's charset plus the hyphen the wild corpus
-- writes — and the parser's is the one that binds.
tagText :: Text -> Either Text Text
tagText text
  | T.null text            = Left "a tag is at least one character"
  | T.all isTagChar text   = Right text
  | otherwise              = Left (text <> " is not an org tag: a tag is letters,"
                                     <> " digits, and _ - @ # or %")

-- | The classification chain behind ROWS, made visible: one entry per SOURCE in
-- precedence order, each holding the keywords it is the WIDEST to declare.
--
-- 'Data.Org.Config.classify' turned inside out, over the very list it folds:
-- that takes the first scope with an opinion, this reports what each claims.
-- DEDUP IS THE CLASSIFICATION RULE — a keyword @default@ and a file both
-- declare appears under @default@ and nowhere below.  An emptied source is
-- dropped, so a @system.org@ redeclaring TODO\/DONE shows its other keywords
-- and no row when it has none.  Each entry's own active\/inactive split is that
-- source's, which is why the answer classifies as well as enumerates.
--
-- What this layer adds is the ROWS.  Several merge by source NAME: the file
-- entry is the union of their files' pragmas, the tags every tag any carries in
-- first-seen order.  The cost: a keyword one row reaches by file and another by
-- tag lands in the WIDER, so the table describes the SET rather than a member.
-- Over ONE row this IS what 'setStateEdits' accepts ('settableStates' is it
-- flattened, so offer and wall cannot come apart); over several the merge
-- outruns that, and committing such a keyword refuses the whole request naming
-- the row it does not fit.
keywordSources :: ConfigLayers -> [HeadlineRecord] -> [(Text, TodoKeywords)]
keywordSources cfg rows = widest Set.empty (sortOn fst chain)
  where
    -- The one scope whose value differs between rows, merged before the chain
    -- is built.  Every other entry a row contributes is a function of the tag
    -- or a constant, so a repeat carries the same set — and 'widest' drops a
    -- repeat by construction, everything it declares being seen already.
    filed   = mergeKeywords (map hrDeclared rows)
    chain   = [ (rank, (source, kw))
              | r <- rows
              , (rank, source, kw) <- keywordScopes cfg filed (tagsOfCell (hrTags r)) ]
    -- 'sortOn' is stable, so the scopes keep their order and the tags keep the
    -- order the rows named them in.
    widest _seen [] = []
    widest seen ((_rank, (source, kw)) : rest)
      | null actives && null inactives = widest seen rest
      | otherwise = (source, TodoKeywords actives inactives) : widest taken rest
      where actives   = filter unseen (tkActive kw)
            inactives = filter unseen (tkInactive kw)
            unseen w  = not (Set.member w seen)
            taken     = foldr Set.insert seen (actives <> inactives)

-- | The span edits @set-state@ makes to R: KEYWORD in place of the one it
-- carries, or the keyword taken off where KEYWORD is 'Nothing'.
--
-- The three shapes are 'tokenEdits'\'s, read at 'hsTodo' with the stars as the
-- place a headline carrying none takes one — org's own place, and the one
-- offset every headline has.
--
-- KEYWORD is refused unless R's OWN CHAIN declares it ('settableStates').  The
-- bar is the chain rather than the parse's recognized set because the chain is
-- what a reader is SHOWN: a state the palette does not offer is one this row
-- has no configuration for.  Recognition stays a superset — a word another
-- tag's cycle names still parses as a state rather than as a title's first
-- word.  The group metas are in no keyword set and are refused like any other
-- word that is not one.
setStateEdits :: ConfigLayers -> Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setStateEdits _cfg Nothing r = Right (tokenEdits hsTodo (spanEnd . hsStars) Nothing r)
setStateEdits cfg (Just keyword) r
  | keyword `notElem` settable =
      Left (keyword <> " is not a TODO keyword for " <> hrId r <> " in " <> T.pack (hrFile r)
              <> "; that row may be set to " <> T.intercalate ", " settable)
  | otherwise = Right (tokenEdits hsTodo (spanEnd . hsStars) (Just keyword) r)
  where settable = settableStates cfg r

-- | The span edits setting the token AT reads on R to TOKEN, or taking it off
-- where TOKEN is 'Nothing'.  PLACE says where one goes on a headline carrying
-- none.
--
-- THREE SHAPES, and @set-state@ and @set-priority@ are one accessor apart.  A
-- token already there is its own span, so everything around it keeps its bytes.
-- One where there is none is an insertion at PLACE behind a space.  'Nothing'
-- deletes the token WITH the horizontal run behind it, so @* TODO Title@ closes
-- up — horizontal only, so a token ending its line keeps the newline.  A
-- headline with no token asked to drop one costs no edit, which makes both
-- commands idempotent.
tokenEdits :: (HeadlineSpans -> Maybe Span) -> (HeadlineSpans -> Int)
           -> Maybe Text -> HeadlineRecord -> [(Span, Text)]
tokenEdits at place token r = case (at hs, token) of
  (Just sp, Just new) -> [(sp, new)]
  (Just sp, Nothing)  -> [(Span (spanStart sp) (spanEnd sp + trailing sp), "")]
  (Nothing, Just new) -> [(insertAt (place hs), " " <> new)]
  (Nothing, Nothing)  -> []
  where hs = headlineSpans r
        trailing sp = runWidth (T.drop (spanEnd sp) (hrDoc r))

-- | The states R may be set to: 'keywordSources' for that one row, flattened.
--
-- Derived from the palette's own function rather than folding
-- 'Data.Org.Config.keywordScopes' a second time, so "what a reader is offered
-- is what a write takes" holds by construction instead of by agreement between
-- two dedups.  For ONE row the layout loses nothing: an empty source carries no
-- word, and the per-source split is where a word sits rather than whether it is
-- there.  The coupling runs the other way too, and is the point — a change to
-- what the palette SHOWS a row is a change to what that row may be set to.
--
-- Which is why reordering the chain leaves this alone: every scope's words are
-- here either way, and the order only decides which entry a word came out of.
settableStates :: ConfigLayers -> HeadlineRecord -> [Text]
settableStates cfg r =
  [ word | (_source, kw) <- keywordSources cfg [r], word <- tkActive kw <> tkInactive kw ]

-- Repeating entries

-- | Record that IDENT repeated into STATE under ROOT, its next occurrence
-- SHIFTED. `Data.Org.External`'s own append, re-exported so the write route
-- reaches the ledger through this library like everything else.
noteCompletion :: FilePath -> External.Completion -> IO ()
noteCompletion = External.noteCompletion

-- | R's `ORG_GLANCE_ID`, when its headline claims one. The ledger's key: an
-- ordinal moves, so a record under one names a different row a week on.
rowOrgId :: HeadlineRecord -> Maybe Text
rowOrgId = identity . hrHeadline

-- | What completing R into KEYWORD does when R repeats: the state it resets to,
-- the next occurrence as the file will spell it, and the edits that do both.
data Repeat = Repeat
  { rpState   :: !Text            -- ^ the keyword the entry lands on.
  , rpShifted :: !Text            -- ^ its next occurrence, cookie and all.
  , rpEdits   :: ![(Span, Text)]  -- ^ the shift and the reset, as one set.
  } deriving (Eq, Show)

-- | R completed into KEYWORD against TODAY, or 'Nothing' where this is an
-- ordinary state change.
--
-- ORG'S OWN CONDITION, and both halves are needed: the keyword must be an
-- INACTIVE one — the entry is being closed — and a planning stamp must carry a
-- repeater. Either alone is a plain `set-state`.
--
-- ONE EDIT SET, so one write, one digest, one event: the timestamps and the
-- keyword sit in disjoint spans, and `applyEdits` rejects only overlap.
--
-- WHICH KEYWORD IT RESETS TO is the row's own chain — `settableStates` is
-- `keywordSources` flattened, so the first active word is the one the palette
-- offers first. A chain declaring none takes the keyword off.
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
    -- ONE fold: `keywordSources` walks the scopes and sorts, and both halves
    -- read the same answer.
    chain  = keywordSources cfg [r]
    chainOf half = [ word | (_source, kw) <- chain, word <- half kw ]

-- | The `Timestamp` TEXT spells, when it spells one.  Read through a probe
-- planning line rather than a second grammar here, `readsAsTimestamp`'s rule.
timestampOf :: Text -> Maybe Timestamp
timestampOf text = case orgParse defaultContext ("* probe\nSCHEDULED: " <> text <> "\n") of
  (elems, _ctx, Nothing) -> listToMaybe [ ts | e <- elems, EHeadline h <- [valueOf e]
                                             , Just ts <- [schedule h] ]
  _failed                -> Nothing

-- | The repeater cookie R carries on a planning timestamp, e.g. @+1w@.  The
-- FIRST of them: an entry repeating on two lines repeats, and the cookie is
-- shown rather than resolved.
--
-- Off the PARSED headline rather than a reparse: this is asked once per row per
-- answer, so a probe parse here would be one whole document parsed per stamp
-- per row.
repeatsOf :: HeadlineRecord -> Maybe Text
repeatsOf r = listToMaybe [ repeaterFormat i | (_sp, _text, i) <- repeatingSpans r ]

-- | R's repeating SCHEDULED and DEADLINE stamps: where each sits, how the file
-- spells it, and the cookie it carries.  `CLOSED:` is out — org repeats a plan
-- rather than a record of one — and a stamp with no repeater is not here.
--
-- The span and the timestamp come off ONE headline, so they describe the same
-- entry by construction; `Data.Org.schedule` and `hsSchedule` are the two
-- halves the parser already produced.
repeatingSpans :: HeadlineRecord -> [(Span, Text, TimestampRepeaterInterval)]
repeatingSpans r =
  [ (sp, sliceSpan (hrDoc r) sp, i)
  | (at, stamp) <- [ (hsSchedule, schedule), (hsDeadline, deadline) ]
  , Just sp <- [at (headlineSpans r)]
  , Just ts <- [stamp (hrHeadline r)]
  , Just i  <- [tsInterval ts] ]

-- | DAY one repeat on under INTERVAL, given TODAY.
--
-- Org's three kinds, and the difference is only where the count starts: `+N`
-- adds one interval to the stamp, so an entry three weeks overdue lands one
-- week on and stays overdue; `++N` adds intervals until it is past today; `.+N`
-- is today plus one.  The `++` loop is bounded by construction — each step
-- moves the day strictly forward — and a zero-width interval takes the `+N`
-- arm, since a loop over one would not end.
repeatDay :: Time.Day -> TimestampRepeaterInterval -> Time.Day -> Time.Day
repeatDay today interval day
  | repeaterValue interval <= 0   = day
  | otherwise = case repeaterType interval of
      Restart    -> once day
      Cumulative -> once today
      CatchUp    -> until (> today) once day
  where
    once = addUnit (repeaterUnit interval) (fromIntegral (repeaterValue interval))

-- | TEXT with every date in it moved one repeat on, or 'Nothing' where it
-- carries no repeater.  The rule as a function of TEXT ALONE, which is what the
-- suite exercises; `repeatOn` reads the interval off the parsed headline
-- instead of paying a probe parse per row.
--
-- TEXTUAL, and deliberately: only the @YYYY-MM-DD@ runs and the weekday behind
-- each are rewritten, so the time of day, the warning cookie, the repeater
-- itself, a range end and the brackets all ride through as the author wrote
-- them.  Re-rendering from `Timestamp` would spell them this library's way.
shiftRepeat :: Time.Day -> Text -> Maybe Text
shiftRepeat today text = do
  ts <- timestampOf text
  interval <- tsInterval ts
  pure (rewriteDates (repeatDay today interval) text)

-- | TEXT with MOVE applied to every @YYYY-MM-DD@ in it, each date's weekday
-- respelled behind it — the parser drops a weekday and the renderer computes
-- one, so a stale word here would outlive both.
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
    -- VARIABLE WIDTH, because `Data.Org.Parser.tsDayParser` is: it reads each
    -- part with `MPL.decimal`, so `<2026-08-8 Sat>` is a timestamp this library
    -- reads.  A fixed ten-character window cuts such a date short, eats the
    -- space behind it and leaves the weekday naming the day it moved off.
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
    -- The weekday org writes after a date: a run of LETTERS in any script, the
    -- parser's own charset, taken out so the computed one replaces it.
    weekdayAt rest = case T.uncons rest of
      Just (' ', body) | (word, after) <- T.span isLetter body, not (T.null word)
                         -> (True, after)
      _noWeekday         -> (False, rest)

-- | TEXT as a headline title, or why it is not one.  The wall @set-title@ puts
-- up, and a whole-request refusal the way the tag charset's is: a string that is
-- not a title is not a title for any row.
--
-- Two rules and no third.  A title is at least one character, since a headline
-- with none is a blank entry and no longer a row ('blankEntry'); and it is ONE
-- line, since the second one would be body text at best and a headline of its
-- own at worst.  What it may SAY is the author's: a title spelling @:word:@ at
-- its end reads back as a tag run, which is org's own grammar rather than
-- something to refuse here.
-- | Where R's TITLE sits in its file, when it has one.
--
-- The one sub-span a CLIENT needs by itself: a title may hold org links, and a
-- link's own range comes back from @\/links@ in FILE coordinates, so a renderer
-- has to know where the cell it is drawing starts before it can tell which
-- links are inside it.  Every other cell is drawn as the text it is.
--
-- A 'Span' rather than the whole 'HeadlineSpans': that type is
-- @glance-internal@'s and stays there.
titleSpan :: HeadlineRecord -> Maybe Span
titleSpan = hsTitle . headlineSpans

-- | TEXT stripped, refused with EMPTY where nothing is left of it and with MANY
-- where a newline is.
--
-- The wall three fields share — a title, a captured entry's text, a planning
-- value — spelled once.  The SENTENCES stay the callers': what a reader is being
-- told is about the field they typed into, and a shared one would name none of
-- them.
oneLine :: e -> e -> Text -> Either e Text
oneLine empty many text
  | T.null want          = Left empty
  | T.any (== '\n') want = Left many
  | otherwise            = Right want
  where want = T.strip text

titleText :: Text -> Either Text Text
titleText = oneLine "a headline needs a title: the text after the keyword"
                    "a title is one line: the rest of the headline's own line"

-- | The span edits @set-title@ makes to R: TITLE in place of the one it carries.
--
-- Two shapes.  A title already there is its own span and nothing else, so the
-- keyword, the priority and the tags around it keep their bytes.  A headline
-- with none — @* TODO@, @* [#A]@, bare stars — takes an insertion behind the
-- last part org writes AHEAD of a title, which is the priority, else the
-- keyword, else the stars themselves.
--
-- 'titleLineEnd' cannot serve here and the difference is the point: its answer
-- includes 'hsTags', and a title inserted past a tag run would be read back as
-- tag text on the next load, taking the entry the author typed with it.
setTitleEdits :: Text -> HeadlineRecord -> Either Text [(Span, Text)]
setTitleEdits text r = do
  want <- titleText text
  pure $ case hsTitle hs of
    Just sp -> [(sp, want)]
    -- Behind the priority, else behind the keyword, each of which owes its own
    -- separator.  With NEITHER there is only the stars, and the horizontal run
    -- org already writes after them is that separator — so the title goes past
    -- it rather than growing a second one.
    Nothing -> case [ spanEnd sp | Just sp <- [hsPriority hs, hsTodo hs] ] of
      (at : _rest) -> [(insertAt at, " " <> want)]
      []           -> [(insertAt (pastRun (spanEnd (hsStars hs))), want)]
  where hs = headlineSpans r
        pastRun at = at + runWidth (T.drop at (hrDoc r))

-- | TEXT as an org priority letter, or why it is not one.  The wall
-- @set-priority@ puts up, and a whole-request refusal like the tag charset's: a
-- string that is not a letter is not one for any row.
--
-- ONE ASCII LETTER, uppercased.  Org's own cycle is @A@ to @C@ and its
-- @org-highest-priority@ / @org-lowest-priority@ move that window rather than
-- changing what a priority IS, so the charset is the letter and the CYCLE is the
-- reader's — which is what leaves a tree using @D@ writable here and unbadged in
-- the table ('priorityBadges').
priorityText :: Text -> Either Text Text
priorityText text
  | T.length want == 1, T.all isAsciiUpper want = Right want
  | otherwise = Left (text <> " is not a priority: org spells one as a single"
                        <> " letter, A to C in its own cycle")
  where want = T.toUpper (T.strip text)

-- | The span edits @set-priority@ makes to R: LETTER in place of the token it
-- carries, or the token taken off where LETTER is 'Nothing'.
--
-- The three shapes are 'tokenEdits'\'s, read at 'hsPriority' — @set-state@'s own
-- one part along, so the keyword in front of the token and the title behind it
-- keep their bytes, a headline with no token takes one behind the KEYWORD
-- ('afterKeyword'), and a clear closes @* TODO [#A] Title@ up to
-- @* TODO Title@.
--
-- Clearing a headline that carries none costs no edit, which makes the command
-- idempotent the way @archive@ is — and is what lets the cycle's wrap through
-- NONE be pressed twice without a second write.
setPriorityEdits :: Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setPriorityEdits Nothing r = Right (tokenEdits hsPriority afterKeyword Nothing r)
setPriorityEdits (Just letter) r = do
  want <- priorityText letter
  pure (tokenEdits hsPriority afterKeyword (Just (priorityCell want)) r)

-- | Where a priority goes on a headline that has none: behind the keyword, else
-- behind the stars.  Org writes @* TODO [#A] Title@, so the token follows the
-- state and precedes the title — and the stars are the fallback because they are
-- the one part every headline has.
afterKeyword :: HeadlineSpans -> Int
afterKeyword hs = maybe (spanEnd (hsStars hs)) spanEnd (hsTodo hs)

-- | The span edits @add-tag@ makes to R: TAG joining its tag list.  A row
-- already carrying it costs no edit, which makes the command idempotent.
--
-- TWO SHAPES.  With tags present the tag joins as the last entry — the span
-- ends past the closing colon, so the insertion is the tag and one colon and
-- the tags already there stay byte-identical.  With none it is appended to the
-- TITLE LINE, after the last part that line carries.  'hsFull' cannot serve
-- there: its end is the last part in span order, which for a scheduled headline
-- is a timestamp on the NEXT line.
--
-- TAG is written as given; presence is folded, so a row spelling @:Work:@ is
-- not given a second @:work:@.
addTagEdits :: Text -> HeadlineRecord -> [(Span, Text)]
addTagEdits tag r = addTagEditsIn (hrTags r) tag (headlineSpans r)

-- | 'addTagEdits' over a headline's own two pieces — the tags CELL as the file
-- spells it, and the spans — rather than over a loaded record.
--
-- Exported to nobody and shared with 'blobDocument', which composes a blob out
-- of an expanded template and has no record to hand over.  ONE insertion rule,
-- the way 'archiveEdits' is one: a capture and an @add-tag@ that disagreed about
-- where a tag goes would be two spellings of org's grammar.
addTagEditsIn :: Text -> Text -> HeadlineSpans -> [(Span, Text)]
addTagEditsIn cell tag hs
  | T.toLower tag `elem` tagsOfCell cell = []
  | Just sp <- hsTags hs = [ (insertAt (spanEnd sp), tag <> ":") ]
  | otherwise            = [ (insertAt (titleLineEnd hs), " :" <> tag <> ":") ]

-- | The span edits @remove-tag@ makes to R: TAG cut out of its tag list.  A row
-- not carrying it costs no edit, the other half of the pair's idempotence.
--
-- TWO SHAPES, decided by what the run has LEFT.  An entry with neighbours is
-- cut as @TAG:@, so @:a:b:c:@ minus @b@ is @:a:c:@ and the survivors keep their
-- bytes.  The LAST entry takes the whole run and the horizontal space in front
-- of it: a lone @:@ is not a tag list.  That run is the parser's own separator
-- ('tagsP' opens on @hspace1@), so there is always one and it is on this line.
--
-- Matching is FOLDED and takes EVERY entry spelling the tag, which is what
-- makes "removed" mean the row stops answering to 'tagged'.
removeTagEdits :: Text -> HeadlineRecord -> [(Span, Text)]
removeTagEdits tag r = case tagRun r of
  Nothing -> []
  Just (run, separator, entries)
    | null hit  -> []
    | null left -> [ (Span (spanStart run - separator) (spanEnd run), "") ]
    | otherwise -> map cutEntry hit
    where (hit, left) = partition (spells tag) entries

-- | The span edits @rename-tag@ makes to R: FROM's entry becoming TO, in place.
--
-- A row not carrying FROM costs no edit, which makes a rename over a marked set
-- safe to send whole and idempotent.
--
-- The entry is replaced WITHOUT its closing colon, so the run's other entries
-- and both delimiters keep their bytes and the tag stays where the author put
-- it.  That is why rename is a command rather than a remove and an add
-- composed: those two APPLY, and what they write is wrong twice over — the
-- addition's anchor is measured BEFORE the removal (spelling @Ship itprojects:@
-- for a lone tag), and whatever the anchor it appends at the RUN'S END, so an
-- entry with neighbours survives having MOVED.  The pair is also two writes
-- under two digests where this is one splice.
--
-- ONE TAG ONCE: the FIRST entry spelling FROM becomes TO and further ones are
-- cut, so a file spelling one tag twice comes out clean.  Where the row ALREADY
-- carries TO, every FROM entry is cut instead — that branch cannot empty the
-- run, the entry carrying TO being one it leaves.
--
-- FROM is matched FOLDED and TO written as given, which makes a change of
-- SPELLING a rename like any other.
renameTagEdits :: Text -> Text -> HeadlineRecord -> [(Span, Text)]
renameTagEdits from to r = case tagRun r of
  Nothing -> []
  Just (_run, _separator, entries) -> case partition (spells from) entries of
    ([], _left) -> []
    (hit@(first : rest), left)
      | any (spells to) left -> map cutEntry hit
      | otherwise            -> renamed first <> map cutEntry rest
  where renamed (at, entry)
          -- An entry already spelling TO costs no edit, which is 'addTagEdits''
          -- rule reached from this side: a byte-identical rewrite is still a
          -- temp-and-rename, an inotify event and a re-parse.
          | entry == to = []
          | otherwise   = [(Span at (at + T.length entry), to)]

-- | R's tag RUN as the two commands that CUT one read it: the run's own span, the
-- width of the horizontal separator in front of it, and its entries as offsets
-- into the DOCUMENT with their text.  'Nothing' for a headline with no tags at
-- all.
--
-- Read once because both want the same answers about it, and because the
-- TITLE LINE is cut once and every scan runs inside it — 'setPlanningEdits''
-- own rule, reached more cheaply here: a headline parses at column 1, so its
-- stars ARE its line's start and no @lineStart@ walk down the document prefix
-- is owed.
tagRun :: HeadlineRecord -> Maybe (Span, Int, [(Int, Text)])
tagRun r = case hsTags hs of
  Nothing  -> Nothing
  Just run -> let line  = sliceSpan (hrDoc r) (Span from (spanEnd run))
                  ahead = spanStart run - from
              in Just ( run
                        -- The horizontal run between the title and the tags,
                        -- which comes off with the whole list.
                      , runWidthEnd (T.take ahead line)
                      , [ (spanStart run + at, entry)
                        | (at, entry) <- tagEntries (T.drop ahead line) ] )
  where hs   = headlineSpans r
        from = spanStart (hsStars hs)

-- | Does this entry of a tag run spell TAG?  FOLDED, the way presence is
-- ('tagged'), so a file writing @:Work:@ answers to @work@.
spells :: Text -> (Int, Text) -> Bool
spells tag = \(_at, entry) -> T.toLower entry == want
  where want = T.toLower tag

-- | The edit that cuts one entry out of a run: itself and the colon that closes
-- it, so @:a:b:c:@ minus @b@ is @:a:c:@ and the surviving entries keep their
-- bytes.  A run emptied by its last entry is the CALLER's case, since only it
-- knows what is left.
cutEntry :: (Int, Text) -> (Span, Text)
cutEntry (at, entry) = (Span at (at + T.length entry + 1), "")

-- | The entries of a tag RUN — @":a:b:"@ — as their offsets into it and their
-- text: @[(1, "a"), (3, "b")]@.  The empty pieces the opening and closing colons
-- leave are dropped, which is what makes an entry's own colon the character
-- after it.
tagEntries :: Text -> [(Int, Text)]
tagEntries run = case offsets 0 (T.splitOn ":" run) of
  pieces@(_ : _ : _) -> drop 1 (init pieces)
  _notARun           -> []
  where offsets _ []          = []
        offsets at (p : rest) = (at, p) : offsets (at + T.length p + 1) rest

-- | The span edits @archive@ makes to R: 'archiveTag' added to its tag list.
--
-- Archiving IS adding one tag, so this is 'addTagEdits' at that name and there
-- is one insertion rule rather than two that have to agree.  Its idempotence is
-- that function's: a row already carrying the tag costs no edit.
archiveEdits :: HeadlineRecord -> [(Span, Text)]
archiveEdits = addTagEdits archiveTag

-- | The span edits @edit-link@ makes to R: the link at SP rewritten to point at
-- TARGET, under whatever DESC says about its description.
--
-- THE FORM IS PRESERVED, which is what makes this a link edit rather than a
-- rewrite of the text around one: @[[T][D]]@ keeps its description under a
-- target-only edit, @[[T]]@ stays desc-less and takes brackets the moment a
-- description arrives, and a bare URL swaps its target and stays bare —
-- a description arriving BRACKETS it, having nowhere else to live.
--
-- ABSENT IS NOT NULL (@.:!@ rather than @.:?@): saying nothing about the
-- description leaves it, @null@ takes it OFF.  An empty description is the null
-- spelled another way, since @[[T][]]@ shows its target.
--
-- TWO WALLS ('linkAtSpan', 'spelling'): the span must cover exactly one link as
-- the document reads, and the replacement must read back as THE LINK IT CLAIMS
-- TO BE.  The write engine is content-agnostic by law, so this is the layer
-- that owes the check.
editLinkEdits :: Span -> Text -> Maybe (Maybe Text) -> HeadlineRecord
              -> Either Text [(Span, Text)]
editLinkEdits sp target desc r = do
  found <- linkAtSpan sp r
  written <- spelling target (reshaped (olShape found) desc)
  pure [(sp, written)]

-- | TARGET in SHAPE as the text to write, or why that text is not that link.
--
-- REPARSE AND COMPARE, the only honest form of this check: rendering and
-- scanning are one grammar, so a rendered link reading back with ANOTHER target
-- says the grammar was escaped rather than used.  @a][b@ renders @[[a][b]]@,
-- which IS one link — pointing somewhere the request never named — so a shape
-- check alone would bless it.
--
-- A NEWLINE is refused ahead of that, being the one thing reparsing cannot
-- catch: this scanner has no line rule, so @[[a\n* B]]@ reads back as the link
-- it claims to be and lands a column-1 star the ORG parser reads as a new
-- headline.
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

-- | The one link SP covers in R's document, or why it covers none.
--
-- The span must sit inside the ROW's own subtree: a row's links are its
-- subtree's ('subtreeLinks'), and a span outside it would let one row's write
-- reach bytes no reader of that row was ever shown — under that row's digest,
-- since a digest is per file.  And it must cover the link EDGE TO EDGE, so a
-- span a character short of the real one is refused rather than spliced into the
-- middle of a link.
linkAtSpan :: Span -> HeadlineRecord -> Either Text OrgLink
linkAtSpan sp r
  | spanStart sp >= spanEnd sp =
      Left (spanned sp <> " covers no characters")
  | spanStart sp < spanStart sub || spanEnd sp > spanEnd sub =
      Left (spanned sp <> " is not inside " <> hrId r <> "'s subtree " <> spanned sub)
  | otherwise = maybe (Left (spanned sp <> " does not read as one link")) Right
                      (onlyLink (sliceSpan (hrDoc r) sp))
  where sub = hrSubtree r

-- | TEXT as the ONE link it spells, edge to edge, or 'Nothing' where it spells
-- none, part of one, or more than one.  One reading for both walls, so what a
-- span must currently hold and what a replacement must come to are the same
-- question asked twice.
onlyLink :: Text -> Maybe OrgLink
onlyLink text = case orgLinks text of
  [l] | olSpan l == Span 0 (T.length text) -> Just l
  _notOneLink                              -> Nothing

-- | SHAPE under what a request said about the description: absent leaves it, a
-- value gives the link one — which BRACKETS a bare link, a plain URL having
-- nowhere to write a description — and a null, or a description that shows
-- nothing, takes it off.
--
-- The EMPTINESS test strips and the value is written verbatim, which is the
-- target's own rule ('wantsLink' refuses a target that is whitespace): neither
-- is content, and content is nobody's to trim.
reshaped :: LinkShape -> Maybe (Maybe Text) -> LinkShape
reshaped shape Nothing      = shape
reshaped shape (Just given) = case given of
  Just desc | not (T.null (T.strip desc)) -> Bracketed (Just desc)
  _takeItOff                              -> case shape of
    Bare        -> Bare
    Bracketed _ -> Bracketed Nothing

-- | TARGET spelled in SHAPE.  The one place this module writes a link, so the
-- bracket grammar is READ in 'linkAt' and WRITTEN here and nowhere else.
renderLink :: Text -> LinkShape -> Text
renderLink target Bare                 = target
renderLink target (Bracketed Nothing)  = "[[" <> target <> "]]"
renderLink target (Bracketed (Just d)) = "[[" <> target <> "][" <> d <> "]]"

-- | SP as a refusal spells it: @[START,END)@, the half-open range it is.
--
-- 'show' rather than 'TextShow.showt', which this module holds for the org
-- re-serializer: what goes out on the wire is never that one, and an offset
-- spelled through it would be the first exception.
spanned :: Span -> Text
spanned sp = "[" <> offset (spanStart sp) <> "," <> offset (spanEnd sp) <> ")"
  where offset = T.pack . show

-- | Where HS's title LINE ends: the greatest end among the parts org writes on
-- it — the stars, the keyword, the priority, the title and the tags.
--
-- 'hsFull' cannot serve, and its own invariant says why: its end is the last
-- part in SPAN ORDER, which for a scheduled headline is a timestamp on the NEXT
-- line and for one with a drawer is its @:END:@.  Two commands insert here: the
-- archive tag onto an untagged headline, and a planning line under a headline
-- that has none.
titleLineEnd :: HeadlineSpans -> Int
titleLineEnd hs = foldl' max (spanEnd (hsStars hs))
  [ spanEnd sp | Just sp <- [hsTodo hs, hsPriority hs, hsTitle hs, hsTags hs] ]

-- | The planning keywords a command may set: 'planningKeywords' less
-- @CLOSED:@, which is org's own bookkeeping — a state change is what writes one
-- — so the lens reads it and no key sets it.
--
-- DERIVED rather than listed, so a fourth keyword added to the trio has to be
-- excluded on purpose rather than being unsettable by omission.
settableKeywords :: [Text]
settableKeywords = filter (/= "CLOSED") planningKeywords

-- | The span edits @set-planning@ makes to R: KEYWORD's entry set to STAMP, or
-- taken off where STAMP is 'Nothing'.
--
-- FOUR SHAPES.  An entry already there is its own span, so a reschedule leaves
-- the keywords, the spacing and the other entries byte-identical.  One where
-- there is none joins the END of the planning line.  A headline with no
-- planning line grows one under its TITLE LINE at column 1, where org puts it.
-- A clear takes the entry plus the horizontal run beside it — trailing, or
-- leading where the entry ends its line — and the WHOLE LINE where it was the
-- last entry, a planning line with none not being one.
--
-- Clearing an entry that was never there costs no edit, which makes the command
-- idempotent.
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

    -- The line is cut ONCE and every scan below runs inside it: the entry's
    -- keyword, the run behind it and the run in front are all a few dozen
    -- characters away, where over the document each would be a pass down the
    -- whole prefix.
    cleared sp
      | null others  = Span from (pastLine doc (spanEnd sp))
      | trailing > 0 = Span at (spanEnd sp + trailing)
      | otherwise    = Span (at - leading) (spanEnd sp)
      where from     = lineStart doc (spanStart sp)
            -- The keyword is where the entry starts; a line that somehow spells
            -- none leaves the timestamp standing in, which the parse forbids
            -- and this survives anyway.
            at       = fromMaybe (spanStart sp) (entryOpening doc keyword (spanStart sp))
            line     = sliceSpan doc (Span from (pastLine doc (spanEnd sp)))
            trailing = runWidth (T.drop (spanEnd sp - from) line)
            leading  = runWidthEnd (T.take (at - from) line)

    added ts
      | null others = (insertAt (titleLineEnd hs), eolOf doc <> entry)
      | otherwise   = (insertAt (maximum (map spanEnd others)), " " <> entry)
      where entry = keyword <> ": " <> ts

-- | TEXT as the timestamp a planning entry carries, TODAY anchoring the
-- relative forms, or why it is not one.
--
-- Four spellings, tried in this order.  Org's own — anything opening on a
-- bracket — is taken exactly as written once it REPARSES, so a repeater, a range
-- or a warning period the author spelled out survives untouched.  @today@ and
-- @tomorrow@, and @+Nd@ \/ @+Nw@ \/ @+Nm@ from TODAY, work a date out.  A bare
-- ISO date carries an optional @HH:MM@.  The last three render as an ACTIVE
-- timestamp with the weekday computed rather than typed, which is the one thing
-- a reader cannot be asked to get right.
--
-- Anything else is refused by name: a value that does not reparse turns the
-- planning line into body text on the next load, and the entry the author set is
-- gone with it.
planningTimestamp :: Time.Day -> Text -> Either Text Text
planningTimestamp today text
  | T.null want = refusal
  | bracketed   = if readsAsTimestamp want then Right want else refusal
  | otherwise   = maybe refusal Right (withTime <$> asLocal <|> (`stamped` Nothing) <$> dated)
  where
    want      = T.strip text
    bracketed = any (`T.isPrefixOf` want) timestampOpeners
    refusal   = Left (text <> " is not a date: spell it 2026-08-05, 2026-08-05 09:30, "
                        <> relativeForms
                        <> ", today, tomorrow, or org's own <2026-08-05 Wed>")

    -- One rendering site: a relative form and a bare ISO date differ in how the
    -- DAY is worked out and in nothing else, and a @+3d@ can never parse as ISO,
    -- so the two feed one alternative rather than each rendering for itself.
    dated = relative <|> asDay
    relative = case T.toLower want of
      "today"    -> Just today
      "tomorrow" -> Just (Time.addDays 1 today)
      offset     -> shifted offset
    -- ORG'S WHOLE CHARSET, through the one reverse map: the parser reads four
    -- units and this read three, so `+1y' parsed everywhere and was refused
    -- here alone.
    shifted offset = do
      digits <- T.stripPrefix "+" offset
      (n, rest) <- either (const Nothing) Just (TR.decimal digits :: Either String (Integer, Text))
      (c, "") <- T.uncons rest
      (\u -> addUnit u n today) <$> unitOf c

    asDay :: Maybe Time.Day
    asDay = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" (T.unpack want)
    -- @%k@ rather than @%H@ for the hour: it reads one digit as well as two, so
    -- @9:05@ is the time a reader meant rather than a refusal over a zero.
    asLocal :: Maybe Time.LocalTime
    asLocal = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %k:%M" (T.unpack want)
    withTime = timedStamp activeBrackets
    stamped  = orgStamp activeBrackets

-- | The brackets org writes a timestamp in: @\<…\>@ for one an agenda picks up,
-- @[…]@ for one that is a record and nothing else.
--
-- DERIVED from the pair the parser matches on ('Data.Org.tsBrackets') rather
-- than respelled here, because nothing downstream would catch a disagreement:
-- only 'planningTimestamp'\'s already-bracketed branch reparses, so a computed
-- stamp and a 'captureStamp' written in a bracket the parser does not read reach
-- the disk and turn the planning line into body text on the next load.
activeBrackets, inactiveBrackets :: (Text, Text)
activeBrackets   = bracketsOf TimestampActive
inactiveBrackets = bracketsOf TimestampInactive

-- | STATUS's brackets as the text a stamp is spelled with.
bracketsOf :: TimestampStatus -> (Text, Text)
bracketsOf status = (T.singleton open, T.singleton close)
  where (open, close) = tsBrackets status

-- | What a value spelled in org's OWN timestamp grammar opens with, which is
-- how 'planningTimestamp' tells one from a date it has to work out.
timestampOpeners :: [Text]
timestampOpeners = map fst [activeBrackets, inactiveBrackets]

-- | DAY inside BRACKETS with its weekday, and TIME after it where there is one:
-- @\<2026-08-05 Wed 09:30\>@.  The one place this library spells a timestamp,
-- so a planning entry and a creation stamp cannot disagree about the shape.
--
-- The weekday is COMPUTED here, which is the same rule the renderer keeps
-- (docs\/invariants.md, Parser): a weekday is a function of the date and asking
-- anyone to type one is asking them to get it wrong.
orgStamp :: (Text, Text) -> Time.Day -> Maybe Text -> Text
orgStamp (open, close) day time =
  open <> spelled "%Y-%m-%d %a" day <> maybe "" (" " <>) time <> close

-- | AT inside BRACKETS with its time of day spelled out.  The shape both stamps
-- this library writes share, so a planning entry and a creation stamp differ in
-- their brackets and in nothing else.
timedStamp :: (Text, Text) -> Time.LocalTime -> Text
timedStamp brackets at = orgStamp brackets (Time.localDay at) (Just (spelled "%H:%M" at))

-- | The property a captured entry carries, org-glance's own spelling.
captureProperty :: Text
captureProperty = "ORG_GLANCE_CREATION_TIME"

-- | NOW in the server's own zone, inside the brackets STATUS names.  The one
-- reading of a wall clock into a stamp, so a creation time and a @%U@ in a
-- capture template cannot come to spell one moment two ways.
zonedStamp :: TimestampStatus -> Time.ZonedTime -> Text
zonedStamp status = timedStamp (bracketsOf status) . Time.zonedTimeToLocalTime

-- | NOW as 'captureProperty' spells a moment: org's INACTIVE timestamp,
-- @[YYYY-MM-DD Day HH:MM]@, in the server's own zone.  Inactive because a
-- creation time is a record of when a row was written rather than something to
-- turn up on an agenda.
captureStamp :: Time.ZonedTime -> Text
captureStamp = zonedStamp TimestampInactive

-- | The span edits @capture@ makes to DOC — the capture target's text, @\"\"@
-- for a file that is not there yet, where the entry is the whole file.
--
-- ONE insertion at the END, so every byte already in the file stays where it
-- was: TEXT as a level-one headline, then a drawer holding STAMP under
-- 'captureProperty'.  TEXT is raw org and is written as spelled, so
-- @TODO Buy milk :errands:@ captures a keyword, a title and a tag.  What it may
-- not be is empty, or more than one line — either makes the entry something
-- other than the one headline this command promises.
--
-- The drawer sits at column 1 like the stars: org's unindented layout, and what
-- the parser reads back with no rule about indentation.  Its lines end the way
-- the target's own do ('eolOf'), so a capture into a CRLF file leaves one.
captureEdits :: Text -> Text -> Text -> Either Text [(Span, Text)]
captureEdits doc stamp text = written <$> captureText text
  where
    written typed = [(insertAt (T.length doc), openingFor doc eol <> entry typed)]
    eol   = eolOf doc
    entry typed = T.concat [ line <> eol
                           | line <- [ "* " <> typed
                                     , ":PROPERTIES:"
                                     , ":" <> captureProperty <> ": " <> stamp
                                     , ":END:" ] ]

-- | TEXT as the one headline a capture promises, or why it is not one.
--
-- The wall BOTH capture paths take: the inbox entry is this text after a star,
-- and a tagged capture puts it at its template's @%?@ — where a newline lands a
-- column-1 star the parser reads as a second entry, and an empty line writes a
-- template with a hole in it.  Every @fields@ answer takes it too, for the same
-- reason: an answer is spliced into the same document.
captureText :: Text -> Either Text Text
captureText = oneLine "a capture needs a headline: the text that goes after the star"
                      "a captured entry is one headline, so its text is one line"

-- Capture templates

-- | The @%@-codes a capture template expands, each with the one line that says
-- what it does.
--
-- ONE LIST, and it is the contract's window: @GET \/capture@ serves it and the
-- settings box completes over what it was served.  The scanner never consults
-- it — 'templateParts' spells the same four codes out as a case — so the list
-- and the grammar are two spellings kept in step by @TestQuery@\'s zip of one
-- through the other.  What the list omits copies through as written:
-- org-capture's language is enormous and this is the sliver the corpus uses.
--
-- @%^{PROMPT}@ carries its own braces because that is how a reader types it;
-- the word inside is theirs.
captureCodes :: [(Text, Text)]
captureCodes =
  [ ("%?", "where the text you type lands — a template without it cannot be filled")
  , ("%U", "the moment of capture, inactive: [2026-08-04 Tue 09:30]")
  , ("%T", "the moment of capture, active: <2026-08-04 Tue 09:30>")
  , ("%^{PROMPT}", "asks PROMPT before capturing and writes the answer here")
  ]

-- | One piece of a capture template.  'TplText' is everything that copies
-- through, an unknown @%@-code included: a template using one captures it
-- literally, which is visible and refusable later where silently dropping it
-- would not be.
data TemplatePart
  = TplText !Text               -- ^ written as it stands.
  | TplPoint                    -- ^ @%?@: the line the reader typed.
  | TplStamp !TimestampStatus   -- ^ @%T@ and @%U@, the server's clock in org's two bracket kinds.
  | TplAsk !Text                -- ^ @%^{PROMPT}@: the answer @fields@ carries for PROMPT.
  deriving (Eq, Show)

-- | TEMPLATE as the pieces it expands to, in order.
--
-- ONE left-to-right pass, and the ONE grammar two answers are read off:
-- 'templatePrompts' asks what a template will want, and 'expandTemplate' writes
-- it.  A @%@ this knows nothing about — @%^@ with no brace, an unclosed @%^{@,
-- @%a@, a trailing @%@ — is text and the scan goes on past it, so no template is
-- unreadable.
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

-- | What TEMPLATE will ask for, in the order it asks, a prompt spelled twice
-- counted once — one question, both places filled.
templatePrompts :: Text -> [Text]
templatePrompts t = nub [ want | TplAsk want <- templateParts t ]

-- | TEMPLATE with TEXT at its point, ANSWERS at its asks and NOW at its stamps,
-- or why this request cannot fill it.
--
-- Two refusals, and both are the whole request's: a template with no @%?@ has
-- nowhere for the text to go, and an ask nobody answered would otherwise write
-- an entry with a hole in it.  The clock is read once by the caller, so a
-- template spelling @%U@ twice stamps one moment.
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

-- | Where DOC's capture template sits — from its FIRST heading LINE to the end
-- of the file, trailing whitespace out.
--
-- org-glance's convention and no new file class: a tag's template is the first
-- @*@ heading of its config file, the same one carrying its @#+TODO:@ cycle.
--
-- TO THE END, which is @org-glance-tag-config--entry@'s rule verbatim rather
-- than the outline extent: everything under the first heading is the template,
-- so @* Book@ over @*** Notes@ is ONE.  Everything ABOVE is the pragmas the
-- @#+TODO:@ splice and the settings lines own, so the regions cannot overlap.
--
-- The heading is found the way org-glance finds it (@^\*+ @): stars then a
-- SPACE, so a bare star run and a @*bold*@ line are body text here.
captureTemplateSpan :: Text -> Maybe Span
captureTemplateSpan doc = (\from -> Span from (T.length (T.stripEnd doc))) <$> headingAt doc

-- | Where DOC's first heading LINE begins, or 'Nothing' when it has none.
headingAt :: Text -> Maybe Int
headingAt doc = listToMaybe [ spanStart sp | (sp, line) <- lineSpansIn doc
                            , isJust (headingStars line) ]

-- | How many stars LINE opens a heading with, or 'Nothing' when it opens none.
--
-- org-glance's own rule verbatim (@^\\*+ @): a run of stars and then HORIZONTAL
-- SPACE, so a bare star run is body text here where the parser reads it as an
-- empty headline.  ONE predicate for the reader and the writer — with two, the
-- sheet was handed a @** Notes@ template it would then refuse to write back, and
-- a bare @*@ could be written and never read again.
headingStars :: Text -> Maybe Int
headingStars line = case T.span (== '*') line of
  (stars, rest) | not (T.null stars), maybe False (horizontal . fst) (T.uncons rest)
                  -> Just (T.length stars)
  _notAHeading    -> Nothing

-- | DOC's capture template, verbatim, or 'Nothing' where it holds no heading.
captureTemplateOf :: Text -> Maybe Text
captureTemplateOf doc = sliceSpan doc <$> captureTemplateSpan doc

-- | The template a capture under TAG expands, given the config LAYERS, or
-- 'Nothing' for the bare entry.
--
-- The tag's own layer first, then the system layer's, then nothing.  The tag's
-- layer is the FIRST file configuring it, which is 'Data.Org.Config.clTags''s
-- rule, and the system half is 'systemSetting''s, so a template is resolved
-- exactly the way the keywords beside it are.
captureTemplateIn :: Text -> [ConfigLayerFile] -> Maybe Text
captureTemplateIn tag layers = mine <|> systemSetting captureTemplateOf layers
  where
    mine = captureTemplateOf . lfText =<< listToMaybe [ f | f <- layers, lfTag f == Just folded ]
    folded = T.toLower tag

-- | The span edits setting DOC's capture template to WANT, or why WANT is not a
-- template.
--
-- The same whole-region splice a pragma line gets, one grain up: a template
-- already there is its own extent, one a file lacks joins the END, and an EMPTY
-- value takes it away.  The extent runs to the end of the file, so this is the
-- one region here whose write cannot disturb a byte of anything else — there is
-- nothing after it.
--
-- ONE WALL: a template is one top entry.  A blob's first headline is the entry
-- org-glance keys by id, so a template that is body text or opens at a deeper
-- level writes a blob with no entry in it, and the refusal is here rather than
-- at the capture that would discover it.
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

-- | What a stored entry is stamped with beside the text its template expanded
-- to: the tag it is filed under, the id org-glance keys it by, and when it was
-- captured.
--
-- A record rather than three positional 'Text' arguments, for 'ConfigParts'\'
-- reason: all three are text and two of them are opaque strings, so a
-- transposed pair composes a blob filed under a timestamp and nothing refuses
-- it.
data BlobSeed = BlobSeed
  { bsTag   :: !Text  -- ^ the org tag the entry wears.
  , bsId    :: !Text  -- ^ its @ORG_GLANCE_ID@.
  , bsStamp :: !Text  -- ^ its 'captureProperty' stamp, as 'captureStamp' spells one.
  }

-- | ENTRY as the document a blob holds: SEED's tag on its first headline, and
-- its id and stamp in that headline's own drawer.  The ONE place this repo
-- assembles what org-glance keys a stored entry by; everything else is the
-- template's, the expansion having already run.
--
-- TWO RULES, and the tag's is 'addTagEditsIn' — the very function @add-tag@
-- runs, so the insertion point cannot differ between a capture and a command.
-- The drawer joins an existing @:PROPERTIES:@ under its own indentation, else
-- is written whole under the PLANNING LINE, which is where the parser reads it
-- back.  Both properties are written whatever the template said: a template
-- spelling @ORG_GLANCE_ID@ would claim an identity the store hands out.
blobDocument :: BlobSeed -> Text -> Either Text Text
blobDocument seed given = case firstHeadlineOf elems of
  Nothing -> Left "this capture template expands to no headline, so there is no entry to store"
  Just h  -> spliced (spans h)
  where
    eol = eolOf given
    -- ENDED FIRST, because a template is stored right-trimmed: a title line with
    -- no newline of its own takes the drawer onto the end of itself, and every
    -- offset below is measured in the text that gets written.
    entry = given <> openingFor given eol
    (elems, _ctx, _err) = orgParse defaultContext entry
    spliced hs = either (Left . refused) Right
                        (Edit.applyEdits entry [ Edit.Edit sp new | (sp, new) <- edits hs ])
    edits hs = addTagEditsIn (cellOf (hsTags hs)) (bsTag seed) hs <> drawerEdits hs
    refused err = "this capture template does not splice: " <> T.pack (show err)
    cellOf = maybe "" (sliceSpan entry)

    -- INSIDE an existing drawer, under its own indentation, else a whole drawer
    -- on the line under the PLANNING LINE — which is where org puts one and
    -- where the parser looks for it.  Measuring from the title line instead
    -- would splice the drawer BETWEEN the headline and its @SCHEDULED:@, where
    -- the planning line is no longer the line after the title and stops being
    -- read as one at all.
    drawerEdits hs = case hsProperties hs of
      Just sp -> [ (insertAt (pastLine entry (spanStart sp))
                   , rows (indentOf (T.drop (lineStart entry (spanStart sp)) entry))) ]
      Nothing -> [ (insertAt (pastLine entry (planningEnd hs))
                   , T.concat [ ":PROPERTIES:" <> eol, rows "", ":END:" <> eol ]) ]

    -- Where the headline's last LINE ends before a drawer would go: its planning
    -- entries where it has any, else its title line.  The three planning spans
    -- permute freely, so this is a maximum over the ends rather than a position.
    planningEnd hs = foldl' max (titleLineEnd hs)
                       [ spanEnd sp | (_key, sp) <- presentPlanning hs ]
    rows indent = T.concat [ indent <> ":" <> key <> ": " <> value <> eol
                           | (key, value) <- [ (headlineIdProperty, bsId seed)
                                             , (captureProperty, bsStamp seed) ] ]

-- | The template a tagged capture expands when no layer names one — org-glance's
-- own default stub, and the whole of what "the bare entry" means here.
--
-- A CONSTANT rather than a branch: with it, a tag with no config, a tag whose
-- config has no heading and a tag whose config spells a template all take ONE
-- path through 'expandTemplate', and the entry a bare capture writes is
-- described by the same three lines of grammar as any other.
bareTemplate :: Text
bareTemplate = "* %?"

-- | Does TEXT open as a level-one heading?  'headingStars' over its first line,
-- so what a template may BE and where a template is FOUND are one rule with one
-- number between them.
topEntry :: Text -> Bool
topEntry text = headingStars (T.takeWhile (/= '\n') text) == Just 1

-- | The span edits writing LINES as the @#+TODO:@ block of a config file
-- holding DOC, or why LINES are not a block.
--
-- Checked ahead of the write: blank lines drop, every line left must be a
-- @#+TODO:@ pragma, and the block must declare at least one keyword — a pragma
-- declaring nothing would leave a layer looking configured and doing nothing.
-- An EMPTY block is the DELETION, which is how a layer is taken off.
--
-- The group metas need no rule here and a guard would be unreachable: a keyword
-- token is letters and underscores, so @#+TODO: *active* | DONE@ does not parse
-- as a cycle and is refused as declaring nothing.  The same wall
-- 'setStateEdits' puts up, reached earlier.
--
-- PARTS is everything besides the cycle, each three-valued: 'Nothing' leaves
-- the part, @Just ""@ takes it away, anything else writes it.  They ride one
-- call because they are regions of one file, and four calls would be four
-- writes under four digests each invalidating the last.
--
-- The spans are the file's own lines and its first heading, so the
-- @#+TITLE:@, the comments and a second heading are bytes this never names.
configEdits :: Text -> Maybe [Text] -> ConfigParts -> Either Text [(Span, Text)]
configEdits doc asked parts
  | not (null strange) = Left ("not a #+TODO: line: " <> T.intercalate " · " strange)
    -- ABSENT lines leave the block exactly as it stands — the rule every
    -- optional region already follows, and what lets a pin write the filter
    -- line without restating a cycle it never read.  An EMPTY list is still
    -- the deletion it always was.
  | isNothing asked    = partEdits
  | null lines'        = block []
  | null declared      = Left declaresNothing
  | otherwise          = block lines'
  where
    block ls = (todoLineEdits doc ls <>) <$> partEdits
    partEdits = (<>) <$> viewLines <*> maybe (Right []) (captureTemplateEdits doc) (cpTemplate parts)
    -- A view is named by its id, so a name no build carries refuses rather than
    -- writing a line nothing reads.
    viewLines = fmap ((<> otherLines) . concat) . traverse one $ cpViews parts
    otherLines = maybe [] (captureTargetEdits doc) (cpCapture parts)
              <> maybe [] (stateColorsEdits doc) (cpColors parts)
    one (vid, want) = case savedView vid of
      Just v  -> Right (viewEdits v doc want)
      Nothing -> Left ("no view is called " <> vid <> "; this build has "
                        <> T.intercalate ", " (map svId savedViews))
    lines'   = filter (not . T.null . T.strip) (fromMaybe [] asked)
    -- A LINE, and the pragma test is a prefix one: an entry carrying a newline
    -- of its own would pass it and write everything past that newline into the
    -- file unread.  One line per line is what makes this a #+TODO:-only splice.
    strange  = filter (\l -> not (isTodoPragma l) || T.isInfixOf "\n" l) lines'
    keywords = todoPragmas (T.unlines lines')
    declared = tkActive keywords <> tkInactive keywords

-- | Everything a @POST \/config@ asks of a layer BESIDES its @#+TODO:@ block —
-- three regions of one file, each three-valued the same way: absent leaves it,
-- empty takes it off, anything else writes it.
--
-- A record rather than three positional 'Maybe Text' arguments, since all three
-- have the same type and a caller swapping two would compile.
data ConfigParts = ConfigParts
  { cpViews    :: ![(Text, Text)]  -- ^ saved views by id, the system layer's alone; an id absent leaves that view.
  , cpColors   :: !(Maybe [(Text, [(Text, Text)])])
      -- ^ @#+GLANCE_STATE_COLORS:@ by theme, likewise; the empty list deletes the block.
  , cpCapture  :: !(Maybe Text)  -- ^ @#+GLANCE_CAPTURE_TARGET:@, likewise.
  , cpTemplate :: !(Maybe Text)  -- ^ the capture template, which EVERY layer may carry.
  } deriving (Eq, Show)

-- | A layer write asking for nothing but its cycle.
noParts :: ConfigParts
noParts = ConfigParts [] Nothing Nothing Nothing

declaresNothing :: Text
declaresNothing =
  "#+TODO: declares no keyword org would read: a keyword is letters and underscores, "
    <> "active states before the bar and done-like ones after it. "
    <> "*active* and *inactive* are the filter's group names, not keywords."

-- | R's headline spans, which is where every command's offsets come from.
headlineSpans :: HeadlineRecord -> HeadlineSpans
headlineSpans = spans . hrHeadline

-- | The zero-width span at AT: an edit over it inserts and deletes nothing.
insertAt :: Int -> Span
insertAt at' = Span at' at'

-- View JSON

-- | The table-view document for RECORDS under TITLE, per
-- @table-view/SCHEMA.md@, in 'defaultSortChain' with the state palette taken
-- from RECORDS themselves.
viewJSON :: Text -> [HeadlineRecord] -> Value
viewJSON viewTitle records =
  viewJSONWith defaultSortChain viewTitle
               (mergeKeywords (map hrKeywords records)) records

-- | 'viewJSON' declaring CHAIN and with the state column's PALETTE given rather
-- than derived.  A server answering a page has to pass the whole store's
-- palette: the badge list is what a client watches for a column change, and
-- deriving it from the rows that happen to be on this page would move it every
-- time the page did.
--
-- CHAIN is the EFFECTIVE order — the query's @sort:@ tokens where it names any,
-- else the default — and it is the chain 'sortedForViewWith' was given, since a
-- view whose declaration disagrees with its rows is one a renderer re-sorts out
-- from under the reader.
viewJSONWith :: SortChain -> Text -> TodoKeywords -> [HeadlineRecord] -> Value
viewJSONWith = viewJSONFor viewColumns builtinViews

-- | The registry's views under NO config: each id with the query its build
-- carries.  What a tree declaring none is served, so the convenience builder and
-- the route agree wherever a tree has no config to differ by.
builtinViews :: [(Text, Text)]
builtinViews = [ (svId v, viewQuery (svId v) noConfig) | v <- savedViews ]

-- | 'viewJSONWith' over COLS instead of the default view — what a query's
-- @columns:@ token serves ('resolveColumns').  The row cells are keyed by
-- COLS' own keys, so the columns declared and the cells filled cannot drift
-- however the set was chosen.
viewJSONFor :: [ViewColumn] -> [(Text, Text)] -> SortChain -> Text -> TodoKeywords
            -> [HeadlineRecord] -> Value
viewJSONFor cols views chain viewTitle palette records = object
  (  [ "title" .= viewTitle, "columns" .= columnsFor cols palette
     , "actions" .= actions ]
  <> declaredSort chain
  <> declaredViews views
  <> [ "rows" .= map (rowJSONFor cols) records ])

-- | The @views@ field VIEWS declares, or nothing at all where a tree has none —
-- SCHEMA.md reads an absent @views@ as the empty list, which is what a renderer
-- with no vocabulary to offer wants.
--
-- The vocabulary of the @view:@ token and the whole of what a renderer is told:
-- APPLYING one is this side's, since only it knows what a name holds.
declaredViews :: [(Text, Text)] -> [Pair]
declaredViews [] = []
declaredViews vs = ["views" .= [object ["name" .= n, "query" .= q] | (n, q) <- vs]]

-- | The @sort@ field CHAIN declares, or nothing at all for the empty one —
-- SCHEMA.md reads an absent @sort@ as the order the rows arrived in, which is
-- what an unsorted view serves.
--
-- SCHEMA.md's @sort@ takes an array for a CHAIN, highest priority first, and
-- this is the one place a chain becomes JSON, where 'sortedForViewWith' is the
-- one place it becomes an ordering.  Both renderers run every key of it and
-- both draw it: the browser over the headers of the columns it orders,
-- @table-view.el@ as words on its hint line.
declaredSort :: SortChain -> [Pair]
declaredSort []    = []
declaredSort chain =
  [ "sort" .= [ object [ "column" .= key, "ascending" .= asc ]
              | (key, asc) <- chain ] ]

-- | The commands the view dispatches.  One so far: @materialize@ on the row at
-- point, which asks the server for that headline's raw subtree and posts an
-- edited one back.  SCHEMA.md's Action object is @{key, command, label}@ and a
-- renderer never interprets @command@ itself — it hands the name to its
-- consumer, and @\"RET\"@ is the conventional default row action.
actions :: [Value]
actions =
  [ object [ "key"     .= ("RET" :: Text)
           , "command" .= ("materialize" :: Text)
           , "label"   .= ("Materialize" :: Text) ] ]

-- | 'viewJSONFor' encoded — the one the routes call, COLS being the query's
-- @columns:@ answer or the default view.
viewJSONTextFor :: [ViewColumn] -> [(Text, Text)] -> SortChain -> Text
                -> TodoKeywords -> [HeadlineRecord] -> TL.Text
viewJSONTextFor cols views chain viewTitle palette =
  encodeToLazyText . viewJSONFor cols views chain viewTitle palette

-- | The view's columns in draw order: the key a filter names, the header, the
-- SCHEMA.md type, and where the cell comes out of a row.  A cell is a 'Maybe':
-- 'Nothing' is the row JSON's @null@ and the empty field a filter reads.
--
-- ONE TABLE, so the FOUR things that must agree cannot drift: 'columns'
-- declares, 'rowJSON' fills, 'filterKeys' names, 'viewCells' joins into
-- 'hrSearch'.  A column appended here is one a filter can name the day it
-- lands.  Reordering is the same one edit — every index downstream is resolved
-- by KEY NAME — so the only lists moved by hand are the suites' deliberate
-- oracles.
--
-- TAGS SITS LAST because org writes it last, flush right past the title and the
-- planning.  The PRIORITY header is org's glyph @#@ rather than a word: the
-- cells are @[#A]@, and a spelled-out header makes the column as wide as the
-- word instead of as wide as what is in it.  Drawing decisions both — the KEYS
-- are untouched, so nothing starts or stops matching.
viewColumns :: [ViewColumn]
viewColumns =
  [ ("state",     "State",     "badge", hrState)
  , ("priority",  "#",         "badge", hrPriority)
  , ("title",     "Title",     "text",  Just . hrTitle)
  , ("scheduled", "Scheduled", "text",  hrScheduled)
  , ("deadline",  "Deadline",  "text",  hrDeadline)
  , ("tag",       "Tags",      "text",  Just . sortedTagsCell . hrTags)
  ]

-- | One column of a served view: the key a filter names, the header over the
-- cells, the SCHEMA.md kind, and where the cell comes out of a row.
type ViewColumn = (Text, Text, Text, HeadlineRecord -> Maybe Text)

-- | NAMES as the columns a @columns:@ token asks for, in written order.  A name
-- is matched CASE-INSENSITIVELY against the default view — its keys and its
-- headers alike, so @Tags@ finds the @tag@ column the way the drawn word
-- spells it — and a name the view does not carry is a CUSTOM column: key and
-- lookup folded, header as the reader wrote it, cells read from the row's own
-- subtree by 'customCell'.  Total, so the grammar layer refuses spellings and
-- this resolves every name that survives it.
--
-- THE MINIMAL SET IS TITLE: every view includes the title column, so a set
-- naming it anywhere keeps it there and a set naming it nowhere gets it FIRST
-- — a row is its headline, and a table of context columns with no headline in
-- it answers no question a reader can act on.
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

-- | R's value under NAME, a case-FOLDED custom column name: @closed@ is the
-- planning line's @CLOSED:@ timestamp verbatim, and anything else is the
-- headline's own property drawer read case-insensitively — the same raw-line
-- reader the materialize lens uses ('drawerPairs'), so the value is the file's
-- spelling rather than the parser's re-tokenised one.  The hidden properties
-- are NOT hidden here: the sheet withholds them so a panel sync cannot rewrite
-- them, and a read-only cell carries no such risk — a column asking for
-- @ORG_GLANCE_ID@ is a reader who wants exactly that.
customCell :: HeadlineRecord -> Text -> Maybe Text
customCell r wanted
  | wanted == "closed" = sliceSpan (hrDoc r) <$> hsClosed (headlineSpans r)
  | otherwise          =
      listToMaybe [ v | (k, v) <- drawerPairs subtree (drawerSlice r subtree)
                      , T.toCaseFold k == wanted ]
  where subtree = subtreeText r

-- | R's cells in column order, an absent one as the empty string: what
-- 'searchTextOf' joins into the row's haystack.  A column whose cell is
-- 'Nothing' and one whose cell is @\"\"@ are the same empty field to a filter,
-- which is what @key:*empty*@ reads.
viewCells :: HeadlineRecord -> [Text]
viewCells r = [ fromMaybe "" (cell r) | (_key, _header, _kind, cell) <- viewColumns ]

-- | The column keys a filter may name, in view order.  Matched
-- case-sensitively, the way a renderer matches its own column keys.
filterKeys :: [Text]
filterKeys = [ key | (key, _header, _kind, _cell) <- viewColumns ]

-- | COLS as SCHEMA.md's Column objects, PALETTE giving the state badges.
-- Every column sorts, so @sortable@ rides on the column itself ('column');
-- what a kind adds past that is the priority letters, the badge list and the
-- tags column's @multi@ declaration.  The extras ride the KEY, so a picked
-- @state@ keeps its badges and a picked @tag@ its @multi@ wherever in the set
-- they land, and a custom column gets none.
columnsFor :: [ViewColumn] -> TodoKeywords -> [Value]
columnsFor cols palette =
  [ column key header kind (extra key) | (key, header, kind, _cell) <- cols ]
  where
    extra key = case key of
      "state"    -> [ "badges" .= badges palette, "values" .= stateValues ]
      "priority" -> [ "badges" .= priorityBadges, "values" .= priorityValues ]
      -- Declared rather than left to be sampled: the renderer decides which
      -- column holds a LIST from up to 40 non-empty cells, so a page with fewer
      -- than two tagged rows finds none at all — and then @tag:*archive*@ is
      -- the literal it matches nothing with, where this producer reads it as
      -- the whole tag.  The declaration wins there.
      "tag"      -> [ "multi" .= True, "values" .= tagValues ]
      _          -> []

-- | A priority LETTER as the cell spells it, which is org's own @[#A]@ rather
-- than the bare letter.  The cell is what a reader sees, what a filter reads and
-- what a sort compares, so the decoration is applied ONCE, here, and every
-- reader of it goes through 'priorityLetter' rather than knowing the brackets.
priorityCell :: Text -> Text
priorityCell letter = "[#" <> letter <> "]"

-- | And back: a priority cell read through its brackets, folded.  The rule the
-- filter matches by and the comparator orders by — DISPLAY WEARS THE
-- DECORATION, MATCHING READS THROUGH IT, which is the star-blind precedent the
-- starred metas set from the other side.  A value that is not bracketed is its
-- own answer, so @priority:A@ and @priority:[#A]@ are one query.
priorityLetter :: Text -> Text
priorityLetter value = T.toCaseFold (fromMaybe folded stripped)
  where folded   = T.strip value
        stripped = T.stripSuffix "]" =<< T.stripPrefix "[#" folded

-- | The three priorities org's own cycle names, as the cells spell them.
priorityValues :: [Text]
priorityValues = map priorityCell ["A", "B", "C"]

-- | And their hues, which are the theme's own org-priority faces: the highest
-- in the theme's red, the medium in its yellow, the lowest in its green.  THREE
-- and no more — org's default cycle is @A@ to @C@ and a tree that spells @[#D]@
-- gets the badge-less default ink rather than a colour this file invented.
--
-- No @group@ field ('badge'): a priority has no such halves.
priorityBadges :: [Value]
priorityBadges =
  [ badge Nothing (overridable "priority" letter
                               ("var(--g-priority-" <> showt i <> ")")) v
  | (i, v) <- zip [0 :: Int ..] priorityValues
    -- The LETTER as org writes it, since that is what a config names it by;
    -- `priorityLetter' folds, which is the MATCHER's rule rather than a name.
  , let letter = T.filter isAsciiUpper v ]

-- | The two keyword groups a @#+TODO:@ line's bar divides: every keyword ahead
-- of it, and every one behind it.  Starred metas, so no file can declare either
-- as a keyword ('Data.Org.Parser.keywordTextP' spells one out of letters and
-- underscores) and no cell can hold one.
--
-- SPELLED HERE, where the view OFFERS them ('stateValues'), and read by the
-- predicate that EVALUATES them ('Glance.Web.Filter'), so the vocabulary a
-- renderer completes over and the words the filter answers to cannot come
-- apart.
activeMeta, inactiveMeta :: Text
activeMeta = "*active*"
inactiveMeta = "*inactive*"

-- | The state column's meta values: filter vocabulary rather than cell text.
-- SCHEMA.md lets a producer add values over a column's own domain, and this one
-- adds org-glance's two keyword groups ('Glance.Web.Filter').  No cell ever
-- holds either, which is why they travel beside the badges rather than among
-- them: a renderer completing the column offers the concrete keywords and these
-- two, and the starred spelling is what says that a group is not a badge.
stateValues :: [Text]
stateValues = [activeMeta, inactiveMeta]

-- | The tags column's meta value: @tag:*archive*@, the whole tag rather than
-- the substring @tag:archive@ matches, and the one query that reaches the rows
-- @\/headlines@ leaves out ('Glance.Web.Filter.archiveMeta').  Declared for the
-- same reason 'stateValues' is — it is how a renderer offers a value no cell
-- holds — and the tags column needs it more, its domain being derived from the
-- cells rather than declared.
tagValues :: [Text]
tagValues = ["*archive*"]

-- | A column object: KEY, HEADER and TYPE, then whatever EXTRA the kind needs.
--
-- @sortable@ is on every one of them, because order means something in all six:
-- a state cycle, a priority letter, a title alphabetically, the tags a row
-- carries, and the two dates.  SCHEMA.md makes the field opt-in and this
-- producer opts every column in, which is why it sits here rather than in a
-- per-kind list with one entry per column.  It gates what a READER may sort by
-- — @^@ and a header click read it, and a producer's own @sortBy@ ignores it —
-- so the shell honours it too before it asks for a sort.
column :: Text -> Text -> Text -> [Pair] -> Value
column key header kind extra =
  object ([ "key" .= key, "header" .= header, "type" .= kind
          , "sortable" .= True ] <> extra)

-- | One row: the identity a renderer keys updates off, its cells, and whether
-- there is anywhere to go from it.  Exported because a live producer streams
-- rows one at a time — a @upsert-row@ frame carries exactly this object, so the
-- streamed row and the row in the initial view are built by the same code.
--
-- @linked@ is SPARSE — @true@ or absent, never @false@ — which is what keeps it
-- an addition to SCHEMA.md's Row rather than a field every row now owes.  It
-- says the subtree holds a link, so @o@ has something to follow; @GET \/links@
-- ('subtreeLinks') is the answer itself, asked per row.  A renderer marks the
-- row's title with it, and one that never learns it renders as it always did.
rowJSON :: HeadlineRecord -> Value
rowJSON = rowJSONFor viewColumns

-- | 'rowJSON' over COLS, which is where a custom column's cells are read.
rowJSONFor :: [ViewColumn] -> HeadlineRecord -> Value
rowJSONFor cols r = object
  (  [ "id" .= hrId r
     , "cells" .= object [ Key.fromText key .= toJSON (cell r)
                         | (key, _header, _kind, cell) <- cols ] ]
  <> [ "linked" .= True | hrLinked r ]
  -- SPARSE like `linked`: a row that repeats carries its cookie, one that does
  -- not carries no field, so SCHEMA.md's Row stays additive and no renderer
  -- edit is owed.
  <> [ "repeats" .= cookie | Just cookie <- [repeatsOf r] ])

-- | The state palette: every TODO keyword the loaded files declared, actives
-- ahead of the done-like ones.  Palette order is also sort priority
-- (SCHEMA.md), so a sort on the state column puts work before its aftermath.
--
-- Each badge names its @group@ — @active@ or @inactive@, the halves a
-- @#+TODO:@ line's bar divides.  Order alone cannot say where the bar fell, and
-- the producer is the only side that knows: the two @stateValues@ metas filter
-- on exactly this split.  It is DECLARED rather than consumed here — the shell
-- reads the badges for their hues and takes its own active\/inactive split off
-- @\/keywords@, which answers per row where this palette is the whole store's —
-- and a renderer with no use for it ignores an extra field.
badges :: TodoKeywords -> [Value]
badges (TodoKeywords actives inactives) =
  group "active" "a" actives <> group "inactive" "i" inactives
  where group g slot ws =
          [ badge (Just g) (overridable "state" w (stateSlot slot i)) w
          | (i, w) <- zip [0 ..] ws ]

-- | A SCHEMA.md badge: VALUE drawn in COLOR, under GROUP where its column has
-- halves to name.  One builder for the three palettes this module declares —
-- the state cycle, org's priority letters and the link types — so a field added
-- to the object is added to all three.
--
-- The GROUP is the state column's own, saying which side of a @#+TODO:@ bar a
-- keyword fell on; the other two have no such halves and pass 'Nothing', which
-- leaves the field off the object entirely rather than spending the name on a
-- different question.
badge :: Maybe Text -> Text -> Text -> Value
badge group color value =
  object ([ "value" .= value, "color" .= color ] <> [ "group" .= g | Just g <- [group] ])

-- | HOW MANY HUES A GROUP CYCLES.  A keyword's slot is its place in the group
-- modulo this, so the number is the WIRE's and the same for every theme; a
-- theme fills exactly this many slots however many hues it declares
-- ('Glance.Web.Theme').
stateSlots :: Int
stateSlots = 4

-- | And how many the PRIORITY column names, which is org's own cycle length
-- rather than a number of this layer's choosing.
prioritySlots :: Int
prioritySlots = length priorityValues

-- | The slot token for INDEX in GROUP (@a@ ctive, @i@ nactive).  The wire
-- carries a `var()` rather than a hex because a reader switches theme without
-- refetching: a baked hue would go stale on the keystroke.
stateSlot :: Text -> Int -> Text
stateSlot group i = "var(--g-state-" <> group <> showt (i `mod` stateSlots) <> ")"

-- | The colour a badge names: the token for VALUE if some theme declares one,
-- else FALLBACK.  A CSS fallback chain, so the per-value override needs nothing
-- of this layer beyond the name — a theme (or a tree's config) declares
-- @--g-state-TODO@ and the badge takes it, declares none and the slot stands.
-- Keywords are letters and underscores and priority values are one letter, so
-- neither can spell a slot's own name.
overridable :: Text -> Text -> Text -> Text
overridable prefix value fallback =
  "var(--g-" <> prefix <> "-" <> value <> ", " <> fallback <> ")"
