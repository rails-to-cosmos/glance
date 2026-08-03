-- | The query facade: load org files into rows, render them as a table-view
-- JSON document, and write one headline's raw subtree back.  This is the whole
-- public surface of the package; the parser and its AST live in a private
-- sublibrary, so a daemon or web target linking against @glance@ cannot reach
-- them.
--
-- The write path is the read path run backwards.  A record carries the extent
-- of its subtree ('hrSubtree') in the text it was parsed from ('hrDoc') and
-- the digest of that text ('hrDigest'); 'replaceSpans' splices new text over
-- spans of that text and refuses unless the file still digests to the pinned
-- value.  So a client materializes what the load model holds, and a file that
-- moved underneath it costs a refusal rather than a corrupted splice.
--
-- Structured commands are the other half of it, and the reason the span math
-- lives here rather than in the daemon: 'HeadlineSpans' is the private
-- sublibrary's, so a web layer computing its own insertion points would have to
-- reach past this facade.  'setStateEdits' and 'archiveEdits' hand back span
-- edits in the same currency 'replaceSpans' takes, and neither of them writes.
--
-- Two rules the wire depends on.  Cell text comes from the source spans, cut
-- once at load time, so the JSON carries what the file says rather than what
-- the REPL renderer would re-emit; the 'TextShow' render is a fallback for
-- components a headline has no span for.  And the JSON is assembled here out
-- of 'Value' combinators, never derived from the internal types: the wire
-- shape is the contract with @table-view/SCHEMA.md@ and the AST must stay free
-- to change under it.
--
-- Retention: cells are copied out of the document they were sliced from, so a
-- row never pins its file's text.  'hrHeadline' still holds the parser's own
-- slices, so a loaded store retains the documents it parsed; should full-store
-- residency ever exceed the scan budget, the lever is that field — drop it or
-- copy it, and leave the cells where they are.  'hrDoc' names the same text
-- 'hrHeadline' already shares, so materialize costs a pointer per row and no
-- array: the file was retained before the field existed.
module Glance.Query ( ConfigLayerFile (..)
                    , ConfigLayers (..)
                    , HeadlineParts (..)
                    , HeadlineRecord (..)
                    , IdCollision (..)
                    , LoadFailure (..)
                    , QueryResult (..)
                    , Span (..)
                    , SortChain
                    , TodoKeywords (..)
                    , WalkOptions (..)
                    , WriteFailure (..)
                    , addTagEdits
                    , archiveEdits
                    , archiveTag
                    , archived
                    , builtinFilter
                    , captureEdits
                    , captureProperty
                    , captureStamp
                    , captureTargetIn
                    , captureTargetOf
                    , cellSep
                    , configDirIn
                    , configEdits
                    , configPath
                    , currentDocument
                    , defaultCaptureFile
                    , defaultFilter
                    , defaultFilterOf
                    , defaultWalk
                    , derivedPath
                    , digestOfText
                    , displayText
                    , documentPath
                    , filterKeys
                    , followableTypes
                    , headlineParts
                    , hiddenProperties
                    , keywordSources
                    , linkColumns
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
                    , noConfig
                    , orgLinks
                    , planningKeywords
                    , planningTimestamp
                    , readConfigLayers
                    , readsAsTimestamp
                    , recomposedSubtree
                    , refSpellings
                    , refTargetOf
                    , refTargets
                    , removeTagEdits
                    , renameTagEdits
                    , replaceSpans
                    , resolveIds
                    , rowJSON
                    , setPlanningEdits
                    , setStateEdits
                    , settableStates
                    , defaultSortChain
                    , sortedForView
                    , sortedForViewWith
                    , sortedTagsCell
                    , subtreeLinks
                    , subtreeText
                    , tagColumns
                    , tagText
                    , tagged
                    , tagsOfCell
                    , todoLines
                    , viewJSON
                    , viewJSONTextWith
                    , viewJSONWith
                    ) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, evaluate, try)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum, isAsciiLower, isDigit)
import Data.Either (fromRight)
import Data.List (foldl', nub, partition, sort, sortBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import TextShow (showt)

import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import qualified Data.Time as Time

import Data.Org ( Context, Element (EHeadline), Headline
                , HeadlineSpans ( hsClosed, hsDeadline, hsPriority, hsProperties
                                , hsSchedule, hsStars, hsTags, hsTitle, hsTodo )
                , Indent (Indent)
                , Priority (Priority), Span (..), Spanned (valueOf)
                , Timestamp (tsStart), Todo (name)
                , TsMoment (tsmHasTime, tsmTime), archiveTag, deadline, defaultContext
                , hsFull, identity, indent, isTagChar, metaCategory, orgParse, priority
                , schedule, sliceSpan, spans, tags, title, todo, todoActive
                , todoInactive )
import Data.Org.Config ( ConfigLayerFile (..), ConfigLayers (..), TodoKeywords (..)
                       , builtinFilter, captureTargetEdits, captureTargetIn
                       , captureTargetOf, classify, configDirIn, declaredKeywords
                       , defaultCaptureFile, defaultFilter
                       , defaultFilterEdits, defaultFilterOf, isTodoPragma
                       , firstBy, keywordScopes
                       , loadConfigDirs, mergeKeywords, noConfig, readConfigLayers
                       , seedContext, todoLineEdits, todoLines, todoPragmas )
import Data.Org.Walk ( Found (..), WalkOptions (..), beatsForId, defaultWalk
                     , findOrgFilesWith, isConfig, isDerived, isDocument
                     , mapFilesConcurrently )

import qualified Data.Org.Edit as Edit

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

-- | Why one file yielded no rows.  A load reports these as counts; a watcher
-- reports them per file, and decides what to keep on the strength of which one
-- it got.
data LoadFailure
  = ReadFailed    -- ^ the bytes could not be read.
  | DecodeFailed  -- ^ the bytes are not valid UTF-8.
  | ParseFailed   -- ^ 'orgParse' rejected the document, which is all-or-nothing.
  deriving (Eq, Show)

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
loadFileWith :: ConfigLayers -> FilePath -> IO (Either LoadFailure [HeadlineRecord])
loadFileWith cfg path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  evaluate $ case raw of
    Left _err -> Left ReadFailed
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _err -> Left DecodeFailed
      Right doc -> case orgParse (seedContext cfg) doc of
        (_elems, _ctx, Just _err) -> Left ParseFailed
        (elems, ctx, Nothing)     -> forcing rs (Right rs)
          -- The digest is of the very bytes these spans were computed against,
          -- taken here rather than by a later read: a write pinned to a digest
          -- read at some other moment would splice offsets into a document
          -- they were never measured in.
          where rs = recordsOf cfg path doc (Edit.digestOf bytes) ctx elems

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
-- categorised by CTX — the context the file parsed to, so a @#+CATEGORY@
-- anywhere in it labels the whole file.
--
-- A row is a LEVEL-ONE headline ('topLevel'); everything deeper is carried
-- inside its ancestor's subtree rather than beside it.  The extents are
-- computed over the WHOLE headline sequence and the filter applied to the zip
-- afterwards.  For THIS predicate the two orders happen to agree — a level-one
-- extent ends at the next headline at level one or shallower, which is another
-- level-one headline, so the ones dropped never decided anything.  The order is
-- kept anyway because 'subtreeSpans' is org's outline rule over a DOCUMENT, and
-- running it over a subsequence is a different function: widen 'topLevel' to
-- keep anything deeper and filtering first would end that row at the next KEPT
-- headline instead of the next shallower one, which is a subtree missing its
-- own children.
--
-- Two keyword values come out of one parse and they are not the same thing.
-- CTX's sets are what the parse RECOGNIZED, CFG's seed included, and they are
-- the file's palette contribution and the vocabulary a command may write.  The
-- file's own @#+TODO:@ declarations ('declaredKeywords' over the elements) are
-- the nearest scope a row's active-ness is CLASSIFIED by, are kept beside the
-- recognized set ('hrDeclared') because the two are not recoverable from each
-- other — a file redeclaring a seeded keyword the other way adds nothing to the
-- union it disagrees with — and they are read
-- over the whole file rather than positionally: a document declaring one
-- keyword two ways at two depths is not something org writes, and recognition
-- stays positional either way.
recordsOf :: ConfigLayers -> FilePath -> Text -> Text -> Context -> [Spanned Element]
          -> [HeadlineRecord]
recordsOf cfg path doc digest ctx elems =
  [ recordOf cfg declared path ordinal doc digest category keywords h subtree
  | (ordinal, (h, subtree)) <- zip [0 ..] entries ]
  where category = detach (metaCategory ctx)
        keywords = keywordsOf ctx
        -- Forced here, once per file: it is STORED now ('hrDeclared'), and an
        -- unforced set is a thunk over ELEMS.
        declared = forcedKeywords (declaredKeywords elems)
        heads    = [ h | e <- elems, EHeadline h <- [valueOf e] ]
        -- The position in THIS list is the row's ordinal ('rowId'), so BOTH
        -- filters run before the numbering: a child or a blank entry between
        -- two rows would otherwise consume an ordinal and shift every row
        -- behind it.
        entries  = [ e | e@(h, _sub) <- zip heads (subtreeSpans (T.length doc) heads)
                       , topLevel h, not (blankEntry h) ]

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
topLevel h = case indent h of Indent n -> n == 1

-- | Has H nothing the table can show?  Six sub-spans, one per column: a
-- headline carrying none of them renders six empty cells, and a row a reader
-- can neither read nor tell from the next one is not a row.  The file keeps the
-- entry — org is the source of truth and nothing here rewrites it — and the
-- table skips it.
--
-- This is the RECORD's rule computed at the HEADLINE's layer, and it has to be:
-- the ordinal numbers emitted rows, so the filter runs before the numbering and
-- there is no record yet to ask.  The layers agree by construction — each span
-- is 'Nothing' exactly where 'recordOf' would cut an empty cell, and one that is
-- there is tight, so it cuts a non-empty one.
--
-- What does NOT rescue an entry is everything the table has no column for: a
-- @CLOSED:@ stamp, a properties drawer — an @ORG_GLANCE_ID@ included, so a
-- blank entry carries no row id and no command can address it — a body, and
-- children, a blank parent taking its whole subtree out of the view the way a
-- file that never reaches level one already does.  Reading the rule's
-- no-planning clause as the two planning COLUMNS is the one place it could have
-- gone the other way: counting @CLOSED:@ would keep an entry whose every cell
-- is still empty.
--
-- The tags clause never fires alone.  Org spells tags after a title and the
-- parser hands @* :tag:@ its colons as the title, so no headline carries
-- 'hsTags' without 'hsTitle'.  It is written down because the rule is over the
-- columns rather than over what the parser happens to reach.
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
        pri       = (\(Priority c) -> T.singleton c) <$> priority h
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
            | otherwise                  = T.concat (map (either id snd) (linkParts s))

-- | S cut into the text between its bracket links and the links themselves, as
-- @(target, shown)@, in order.  Text that does not close a link — an unmatched
-- @[[@ — stays literal, the way the renderer's regex leaves one alone.
--
-- ONE scanner for the two questions asked of a bracket link: 'showLinks' keeps
-- what each shows and 'orgLinks' keeps where each points, and a second pass
-- would be a second grammar to keep in step with SCHEMA.md's link rule.
linkParts :: Text -> [Either Text (Text, Text)]
linkParts = go
  where
    go rest
      | T.null after = [Left before]
      | otherwise    = Left before : case linkAt (T.drop 2 after) of
          Just (target, shown, more) -> Right (target, shown) : go more
          Nothing                    -> Left "[[" : go (T.drop 2 after)
      where (before, after) = T.breakOn "[[" rest

-- | The link opening TEXT — which starts past its @[[@ — as its target, as it
-- displays, and whatever follows it.  'Nothing' when TEXT does not close one.
linkAt :: Text -> Maybe (Text, Text, Text)
linkAt text
  | T.null target || T.null rest = Nothing
  | otherwise = case T.uncons (T.drop 1 rest) of
      Just (']', more) -> Just (target, target, more)          -- [[TARGET]]
      Just ('[', more) | "]]" `T.isPrefixOf` after'            -- [[TARGET][DESC]]
                       -> Just (target, if T.null desc then target else desc, T.drop 2 after')
        where (desc, after') = T.break (== ']') more
      _notALink        -> Nothing
  where (target, rest) = T.break (== ']') text

-- Links

-- | Every link R's subtree points at, as @(target, description)@.
--
-- Server-side because it is org text work: a page that extracted these would
-- need the bracket grammar 'displayText' already holds, and would then hold a
-- second copy of it.  The subtree rather than the cells, so a link in the body
-- of an entry is reachable from the row that carries it.
subtreeLinks :: HeadlineRecord -> [(Text, Text)]
subtreeLinks = orgLinks . subtreeText

-- | The links TEXT holds, in order of appearance, one per target.
--
-- Two forms, which is what org writes and what 'displayText' already reads: the
-- bracket link, described by its @DESC@ where it has one and by its target
-- where it does not ('linkAt'), and the plain URL, which is its own description.
-- A target spelled twice keeps the FIRST description — the second is the same
-- destination under another name, and a palette offering it twice would be
-- offering one place two letters.
-- A plain URL can only be in the text BETWEEN bracket links, which is what
-- 'linkParts' hands over separately — so @[[https://…][x]]@ never also reports
-- its own target as a bare one.
orgLinks :: Text -> [(Text, Text)]
orgLinks = firstBy fst . concatMap (either plainLinks pure) . linkParts

-- | The schemes a bare URL is recognized by.  org's plain-link set is wider;
-- these three are the ones a browser is asked to open, and a scheme this does
-- not name stays ordinary text rather than becoming a link nothing can follow.
linkSchemes :: [Text]
linkSchemes = ["https://", "http://", "mailto:"]

-- | The plain URLs S holds, each as its own description.  A URL cannot carry
-- whitespace, so the words of S are the candidates and one word holds at most
-- one link.
plainLinks :: Text -> [(Text, Text)]
plainLinks = mapMaybe urlIn . T.words

-- | The plain URL WORD holds, if any: from the earliest scheme that opens at a
-- non-word boundary — so @xhttp://a@ is not one — to the end of the word, with
-- the punctuation a sentence leaves behind taken off the tail.  That last rule
-- is what makes @see https://x.org.@ and @(https://x.org)@ point where they
-- read as pointing.
--
-- Every scheme carries its separator, so a word with no @:@ in it can hold no
-- link and is turned away by one cheap pass — which is nearly every word of
-- nearly every subtree.
urlIn :: Text -> Maybe (Text, Text)
urlIn word
  | not (T.any (== ':') word) = Nothing
  | otherwise = case mapMaybe opensAt linkSchemes of
      []  -> Nothing
      ats -> let url = T.dropWhileEnd trailing (T.drop (minimum ats) word)
             in if T.null url then Nothing else Just (url, url)
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
-- Three honest consequences, all from deriving the type off the PREFIX alone.
-- Org's internal links — @[[Title]]@ and @[[*Title]]@ — carry no scheme and read
-- @other@, which is right: they name a place inside the tree rather than a
-- protocol.  A relative file link written without its prefix
-- (@[[.\/notes.org]]@) reads @other@ too, where @[[file:.\/notes.org]]@ reads
-- @file@ — the type reports what the target SAYS, and nothing here guesses at a
-- path.  And a scheme-SHAPED word before a colon is taken at its word, so
-- @[[Meeting: notes]]@ reads @meeting@.  The alternative is a registry of known
-- schemes, and then a scheme this list had never heard of would read as prose —
-- which is the worse failure, since the popup exists to say what a link IS.
--
-- Read by @GET \/links@, which is where the shell's popup gets its type column
-- and where @o@ decides whether a browser tab can be pointed at the target at
-- all.
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
-- No @group@ field.  That one is the state column's own, saying which side of a
-- @#+TODO:@ bar a keyword fell on, and spending it here on a different question
-- would be two meanings for one name.
linkTypeBadges :: [Value]
linkTypeBadges =
  zipWith badge (take n activeColors <> cycle inactiveColors) linkTypes
  where badge color value = object ["value" .= value, "color" .= color]
        n = length followableTypes

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
  , column "title" "Headline" "text"  []
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
refTargetsOf :: [(Text, Text)] -> [Text]
refTargetsOf = nub . map detach . mapMaybe (refTargetOf . fst)

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

-- | CELL re-spelled with its tags in case-folded alphabetical order —
-- @\":task:nl:finance:\"@ reads @\":finance:nl:task:\"@.
--
-- DISPLAY ONLY, and the one place it is applied is the @tag@ entry of
-- 'viewColumns' — the COLUMN, which is what the table draws and what
-- 'searchTextOf' joins into 'hrSearch'.  Everything else about a row's tags is
-- the file's own order:
--
--   * the FILE, because the span is never touched — materialize hands back the
--     author's bytes and 'addTagEdits' \/ 'removeTagEdits' splice into the run
--     as it is spelled.
--   * 'hrTags' itself, so 'classify' still reads the tags in the order the
--     headline writes them.  That order DECIDES which tag's config governs the
--     row ('keywordScopes' is first-wins), so sorting the field would move a
--     resolution rather than a rendering.
--   * @GET \/tags@ and the manage-tags palette behind it, whose union is
--     first-seen in the order the rows and their files introduce the tags.
--
-- Readers that ask about MEMBERSHIP are unaffected either way: 'tagged' and
-- @tag:*archive*@ split the cell, and a bare @tag:x@ is a substring of one tag
-- rather than of the run.
--
-- Folded rather than raw, so @:Work:admin:@ does not sort its capital ahead of
-- every lowercase tag; the sort is STABLE, so two spellings folding alike keep
-- the file's order between them.
--
-- A cell ALREADY IN ORDER is handed straight back, the very 'Text' that came in,
-- and that is the row this runs over: the accessor is read per row per
-- @\/headlines@ ('rowJSON') as well as once at load ('viewCells'), and ~/sync at
-- 2026-08-02 serves 10112 rows of which 4514 carry no tag, 5491 carry exactly
-- one, and 107 carry more.  So 99% of rows rebuild nothing.  A colon-count guard
-- ahead of the split would save more still and is deliberately not taken: it
-- would assume a well-formed @:a:b:@ run, which is a shape this function is
-- otherwise free of, for a fraction of a millisecond per render.
sortedTagsCell :: Text -> Text
sortedTagsCell cell
  | sorted == entries = cell
  | otherwise         = ":" <> T.intercalate ":" sorted <> ":"
  where entries = tagRunEntries cell
        sorted  = sortOn T.toCaseFold entries

-- | The entries of a tag RUN, org spelling it @:a:b:@: split on the colon and
-- drop the empties its two ends leave.  One spelling of that rule, since
-- 'tagsOfCell' and 'sortedTagsCell' ask the same question of the same string and
-- a second copy would be a second reading of org's own syntax.
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
-- Which one stays is decided by the path ('Data.Org.Walk.beatsForId'):
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
      Nothing -> (Map.insert (hrId r) (i, hrFile r) best, out)
      Just (_j, held)
        | beatsForId (hrFile r) held
                    -> (Map.insert (hrId r) (i, hrFile r) best, collision (hrFile r) held : out)
        | otherwise -> (best, collision held (hrFile r) : out)
        where collision = IdCollision (hrId r)
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
-- ONE list, read twice — 'declaredSort' spells it onto the wire and
-- 'sortedForViewWith' arranges the rows by it — so the order a client is told
-- about and the order it is served can never disagree.  That pairing is the
-- whole reason a producer sorts at all: a renderer re-sorts what it is given,
-- and a page cut out of a different order than the one declared is a different
-- set of rows than the table would have put there.
--
-- STATE leads, and by the badge PALETTE rather than alphabetically
-- ('sortCell'), which is the declared @#+TODO:@ cycle: the table opens with the
-- work in the order org itself names it, active states ahead of done-like ones.
-- Title settles rows sharing a state and the two dates settle the rest.  Every
-- key ascends; SCHEMA.md makes direction per key, and the default wants no
-- other one.
--
-- Priority is deliberately out of it: it is a fifth key behind four that have
-- already separated nearly every pair of rows, and a chain is read by whoever
-- has to hold it in mind.  @sort:priority@ is how a reader asks for it.
defaultSortChain :: SortChain
defaultSortChain =
  [ ("state", True), ("title", True), ("deadline", True), ("scheduled", True) ]

-- | R's comparison value for the column KEY under PALETTE, or 'Nothing' for an
-- empty cell.
--
-- A 'Nothing' is SCHEMA.md's NULL: it sorts to one end of its own key, outside
-- that key's direction, and says nothing about the row's other cells.  A cell
-- that is absent and one that is @\"\"@ are the same null, which is the rule
-- @key:*empty*@ already reads.
--
-- The pair is (palette POSITION, folded TEXT), which is the two ways SCHEMA.md
-- orders a column with one type: a badge column compares by where its value
-- sits in the palette and nothing else, every other column by its text.  So the
-- state column fills the first half and leaves the second empty, and the rest
-- do the reverse.
--
-- Text is compared CASE-FOLDED, the way 'sortedTagsCell' folds: the browser
-- renderer collates with @localeCompare@, which is case-insensitive at its
-- primary strength, and raw code-point order would put every capitalised title
-- ahead of every lowercase one where the table shows them interleaved.  Folding
-- is the closest this side gets; a title differing from another only by
-- punctuation or by script can still land elsewhere than @localeCompare@ would
-- put it, and the next key settles it here where the renderer would not have
-- asked one.
-- Built ONCE per sort rather than per comparison: the column is resolved out of
-- 'viewColumns' and the palette flattened into a rank where the KEY is read,
-- so a chain of four keys pays for four lookups instead of four per pair of
-- rows.
sortCell :: TodoKeywords -> Text -> Maybe (HeadlineRecord -> Maybe (Int, Text))
sortCell palette key = read' <$> lookup key [(k, cell) | (k, _, _, cell) <- viewColumns]
  where
    ranked = paletteRank palette
    read' cell r = case cell r of
      Just value | not (T.null value) ->
        Just (if key == "state" then ranked value else 0
             , if key == "state" then "" else T.toCaseFold value)
      _empty -> Nothing

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
    compareBy asc value a b = case (value a, value b) of
      (Nothing, Nothing) -> EQ
      (Nothing, Just _)  -> GT          -- nulls last, outside the direction
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

-- Lens
--
-- The parts of a subtree taken out of it and put back into it.  One rule holds
-- the pair together: every byte of a subtree has ONE OWNER.  Three regions are
-- lifted out of the text — the planning line, the headline's own property
-- drawer and its own logbook drawer — and every byte left is the body's.  A
-- part nobody edited goes back as the very line it came in on, odd spacing and
-- odd casing and all, so a client can edit one part without the ones it did not
-- touch being re-spelled underneath it.
--
-- A headline's OWN regions only.  A child's drawer is body text here, since it
-- belongs to the child's headline and this lens is over one.
--
-- Two of the four parts are the SERVER's, and a client neither sees nor sends
-- them: the properties named in 'hiddenProperties', and the whole logbook.
-- They are lifted out for different reasons — one is identity a rename would
-- break, the other is a record nothing here edits — and they go back verbatim
-- whatever a client says, which is the whole of what "server-preserved" means.

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
hiddenProperties = ["ORG_GLANCE_ID", captureProperty]

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
  { hpBody       = withoutSpans subtree (regionSpans r subtree)
  , hpProperties = [ p | p <- drawerPairs r subtree, not (hiddenProperty (fst p)) ]
  , hpPlanning   = [ (key, sliceSpan subtree sp) | (key, sp) <- planningEntries r subtree ]
  , hpLogbook    = maybe "" (sliceSpan subtree) (logbookSlice (drawerSlice r subtree) subtree)
  }
  where subtree = subtreeText r

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
    subtree = subtreeText r
    planAt  = planningSlice r subtree
    drawAt  = drawerSlice r subtree
    logAt   = logbookSlice drawAt subtree
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
            , planningText (planningStyle r subtree (hpBody parts) planAt) (hpPlanning parts) )
    props = ( bodyLine 1 drawAt
            , drawerText (drawerStyle r subtree (hpBody parts))
                         [ p | p <- hpProperties parts, not (hiddenProperty (fst p)) ] )
    logs  = ( bodyLine 0 logAt, maybe "" (sliceSpan subtree) logAt )

-- | One region of a subtree, and which line of the BODY it goes back on.
data Region = Region
  { rgLine :: !Int   -- ^ the body line it belongs above.
  , rgText :: !Text  -- ^ what goes back there, terminated or not.
  }

-- | BODY with each of REGIONS put back at the line it belongs above.
--
-- Ascending, counting only the BODY lines consumed, so two regions naming one
-- line land in list order rather than one displacing the other — which is what
-- a headline growing a planning line and a drawer in the same commit needs.  A
-- body with fewer lines than an index takes the region at the end, which is
-- where a client that deleted the lines above it has left room.
spliceRegions :: Text -> [Region] -> Text
spliceRegions body regions = knit (go 0 (linesWith body) (sortOn rgLine regions))
  where
    go _seen ls [] = ls
    go seen ls (Region at block : rest) =
      taken <> linesWith block <> go (seen + length taken) left rest
      where (taken, left) = splitAt (max 0 (at - seen)) ls

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
withoutSpans :: Text -> [Span] -> Text
withoutSpans subtree = T.concat . go 0
  where go at []         = [T.drop at subtree]
        go at (sp : sps) = slice at (spanStart sp) : go (spanEnd sp) sps
        slice from to = T.take (to - from) (T.drop from subtree)

-- | Every region of SUBTREE that is R's own, in source order.
regionSpans :: HeadlineRecord -> Text -> [Span]
regionSpans r subtree =
  sortOn spanStart (catMaybes [planningSlice r subtree, drawer, logbookSlice drawer subtree])
  where drawer = drawerSlice r subtree

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

-- | Where R's planning line sits in SUBTREE, as a whole-line span.  'Nothing'
-- when the headline has no planning at all.
--
-- The three planning spans cover their timestamps alone and permute freely on
-- one line, so the region is the LINE the outermost of them sits on: the
-- keywords that open the entries and whatever spacing is between them belong to
-- it too, which is what lets an untouched line go back byte for byte.
planningSlice :: HeadlineRecord -> Text -> Maybe Span
planningSlice r subtree = case map snd (planningEntries r subtree) of
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
  | otherwise                                      = Just (Span from to)
  where from = spanStart sp - spanStart (hrSubtree r)
        to   = spanEnd sp - spanStart (hrSubtree r)

-- Planning

-- | How R's planning line in SUBTREE is spelled, so a rewritten one reads like
-- the file it goes back into.  BODY supplies the line ending for a headline
-- that has no line to copy one from.
data PlanningStyle = PlanningStyle
  { psIndent :: !Text                    -- ^ what a written line is indented by.
  , psEol    :: !Text                    -- ^ what it ends with.
  , psRaw    :: ![((Text, Text), Text)]  -- ^ each entry already there, and its own text.
  }

planningStyle :: HeadlineRecord -> Text -> Text -> Maybe Span -> PlanningStyle
planningStyle _r _subtree body Nothing = PlanningStyle "" (eolOf body) []
planningStyle r subtree _body (Just sp) = PlanningStyle (indentOf line) (eolOf line) raws
  where
    line = sliceSpan subtree sp
    raws = [ ((key, sliceSpan subtree at), raw)
           | (key, at) <- planningEntries r subtree
           , Just raw <- [rawEntry key line (shifted at)] ]
    shifted (Span s e) = Span (s - spanStart sp) (e - spanStart sp)

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
readsAsTimestamp value = not (T.null trimmed) && not (T.any (== '\n') trimmed) && parses
  where
    trimmed = T.strip value
    parses  = case orgParse defaultContext ("* probe\nSCHEDULED: " <> trimmed <> "\n") of
      (elems, _ctx, Nothing) -> any planned elems
      _failed                -> False
    planned e = case valueOf e of
      EHeadline h -> isJust (schedule h)
      _other      -> False

-- Properties

-- | R's drawer pairs in SUBTREE, in file order and with nothing hidden.
drawerPairs :: HeadlineRecord -> Text -> [(Text, Text)]
drawerPairs r subtree = case drawerSlice r subtree of
  Nothing -> []
  Just sp -> [ (key, value) | (key, value, _raw) <- drawerRows (sliceSpan subtree sp) ]

-- | How R's drawer is spelled, so a rewritten one reads like the file it goes
-- back into.  BODY supplies the line ending for a headline that has no drawer
-- to copy one from.
data DrawerStyle = DrawerStyle
  { dsOpen   :: !Text                    -- ^ the @:PROPERTIES:@ line, terminator and all.
  , dsClose  :: !Text                    -- ^ the @:END:@ line, which ends the block.
  , dsIndent :: !Text                    -- ^ what a rendered line is indented by.
  , dsEol    :: !Text                    -- ^ what a rendered line ends with.
  , dsRaw    :: ![((Text, Text), Text)]  -- ^ each pair a client may write, and its line.
  , dsHidden :: ![(Int, Text)]           -- ^ the server's own lines, and where in the block they sat.
  }

-- | How R's drawer in SUBTREE is spelled, BODY standing in for the parts a
-- headline with no drawer has nothing to copy.
drawerStyle :: HeadlineRecord -> Text -> Text -> DrawerStyle
drawerStyle r subtree body = case drawerSlice r subtree of
  Nothing -> DrawerStyle (":PROPERTIES:" <> eol) (":END:" <> eol) "" eol [] []
    where eol = eolOf body
  Just sp -> DrawerStyle open close (indentOf (firstOr open [ raw | (_k, _v, raw) <- rows ]))
                         (eolOf close)
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
          where (before, after) = splitAt (min at (length acc)) acc

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

-- | T split into lines, each carrying the newline that ends it.  The last line
-- carries one only where T does, so @T.concat . linesWith@ is @id@.
linesWith :: Text -> [Text]
linesWith t
  | T.null t  = []
  | otherwise = case T.breakOn "\n" t of
      (line, rest) | T.null rest -> [line]
                   | otherwise   -> (line <> "\n") : linesWith (T.drop 1 rest)

-- | T's lines, each with the span covering it and the newline that ends it.
lineSpansIn :: Text -> [(Span, Text)]
lineSpansIn t = go 0 (linesWith t)
  where go _at []       = []
        go at (l : ls)  = (Span at (at + T.length l), l) : go (at + T.length l) ls

-- | The offset in T past the newline ending the line offset AT sits on, or T's
-- length when that line has none.
pastLine :: Text -> Int -> Int
pastLine t at = maybe (T.length t) (\i -> at + i + 1) (T.findIndex (== '\n') (T.drop at t))

-- | The offset in T where the line offset AT sits on begins.
lineStart :: Text -> Int -> Int
lineStart t at = T.length (fst (T.breakOnEnd "\n" (T.take at t)))

-- | The line ending T's first line uses, @"\\n"@ when it has none.
eolOf :: Text -> Text
eolOf t = case T.breakOn "\n" t of
  (before, rest) | not (T.null rest), "\r" `T.isSuffixOf` before -> "\r\n"
  _plain                                                         -> "\n"

-- | The horizontal space LINE opens with.
indentOf :: Text -> Text
indentOf = T.takeWhile horizontal

-- | Is C horizontal space — the run a command deletes with a keyword, and the
-- run a line is indented by?  Org's own distinction: a newline ends a line and
-- these two do not, so a command taking "the space behind this word" must never
-- take the line's end with it.
horizontal :: Char -> Bool
horizontal c = c == ' ' || c == '\t'

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
    extent h = (level (indent h), spanStart (hsFull (spans h)))
    level (Indent n) = n
    place (open, ends) (lvl, start) = ((lvl, start) : closers, Span start end : ends)
      where closers = dropWhile ((> lvl) . fst) open
            end = case closers of
              ((_lvl, next) : _rest) -> next
              []                     -> len

-- | CTX's keyword sets, forced and detached: one 'TodoKeywords' per file,
-- shared by every row that file contributes.  This is RECOGNITION — org's
-- TODO\/DONE, the config seed and the file's own @#+TODO:@ lines together —
-- which is why a row's active-ness is 'hrActive' rather than a lookup in here.
keywordsOf :: Context -> TodoKeywords
keywordsOf ctx = forcedKeywords (TodoKeywords (kept todoActive) (kept todoInactive))
  where kept f = map detach (Set.toAscList (f ctx))

-- | KW with both lists' spines and elements forced, which is what makes a
-- keyword set safe to STORE.  A strict field buys WHNF and no more — the first
-- cons cell — so an unforced set is a thunk over the parse it was read from,
-- and a record holding one would pin its file's whole element tree for the life
-- of the process.  Both stored sets go through this: the recognized union and,
-- because the same trap is one field away, the file's own declarations.
forcedKeywords :: TodoKeywords -> TodoKeywords
forcedKeywords kw = forcing (tkActive kw <> tkInactive kw) kw

-- | H's row identity: its @ORG_GLANCE_ID@ property, else @"FILE#K"@ — the path
-- and ORDINAL, which is H's 0-based position among the file's EMITTED ROWS
-- ('recordsOf', where the numbering happens after both filters, 'topLevel' and
-- 'blankEntry').
--
-- The ordinal is what a row's identity survives.  It moves only when the file's
-- rows are REORDERED, INSERTED into or REMOVED from ahead of this one — so
-- editing a title, a state, a body, a drawer, a child, or anything at all in
-- the entry above, renames nothing and the table keeps its selection.  What
-- still churns, honestly, is the class the ordinal cannot absorb: a new first
-- entry renumbers every row behind it, and so does deleting one or swapping two.
-- An entry going blank, or stopping being blank, is that same class wearing
-- another hat — clearing the last keyword off a title-less entry removes a row,
-- and every K behind it moves up one.  An @ORG_GLANCE_ID@ is immune to all of
-- it, which is the reason to write one.
--
-- The character offset this replaced moved on ANY edit above the headline,
-- which is most edits: a byte typed into the preamble renamed every row in the
-- file, and the store could not tell that from every row being deleted and
-- re-inserted.
--
-- The two forms share one namespace and are resolved by exact string
-- ('resolveIds'), never by parsing an id apart, so nothing turns on the
-- separator being unambiguous.  It is @#@ rather than @:@ because a path is
-- allowed to hold either, and a walked path always ends in its @.org@
-- extension: @FILE#K@ therefore recovers K at its last @#@ for every file this
-- library can reach.  A headline that WRITES an @ORG_GLANCE_ID@ spelling
-- another row's @FILE#K@ collides the way any two headlines claiming one id
-- collide — one row is kept and the other is reported, so a pathological tree
-- costs a row and never points an id at the wrong one.
rowId :: FilePath -> Int -> Headline -> Text
rowId path ordinal h = maybe fallback detach (identity h)
  where fallback = T.pack path <> "#" <> T.pack (show ordinal)

-- | TS's start as the wire spells a date: @"YYYY-MM-DD"@, plus @" HH:MM"@ when
-- the source carried a time of day.  A computed value rather than a slice: ISO
-- is the contract, and org's bracketed spelling stays in the file.
isoStamp :: Timestamp -> Text
isoStamp ts = T.pack (Time.formatTime Time.defaultTimeLocale fmt (tsmTime moment))
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

-- Digests

-- | The SHA-256 of TEXT's UTF-8 bytes, lowercase hex: the digest a record pins
-- its file with ('hrDigest'), over text a caller assembled itself.  Exported so
-- a consumer summarising a set of them — a store fingerprinting the tree it
-- loaded — hashes with the function that produced them rather than with one of
-- its own.
digestOfText :: Text -> Text
digestOfText = Edit.digestOf . TE.encodeUtf8

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
-- digests to DIGEST; the file's new digest comes back, so a caller chains an
-- edit without re-reading.
--
-- The lock is the point.  DIGEST is the one a record was loaded with
-- ('hrDigest'), and every span indexes that same text, so either the file is
-- still the document the offsets were measured in or nothing is written — a
-- browser and an editor writing the same file cannot silently splice over each
-- other.  The write itself is 'Data.Org.Edit.editFile': one drift check, one
-- pass over the document whatever the batch size, one atomic replace.  So a
-- command over several rows of one file is ONE write, and either all of its
-- edits land or none of them do.
replaceSpans :: FilePath -> Text -> [(Span, Text)] -> IO (Either WriteFailure Text)
replaceSpans path digest edits =
  report <$> Edit.editFile (Edit.Snapshot path digest) [ Edit.Edit sp new | (sp, new) <- edits ]
  where
    report = either (Left . failure) (Right . Edit.snapDigest . Edit.receiptSnapshot)
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
-- The two sets differ by @-@ and @%@ and the parser's is the one that binds.
tagText :: Text -> Either Text Text
tagText text
  | T.null text            = Left "a tag is at least one character"
  | T.all isTagChar text   = Right text
  | otherwise              = Left (text <> " is not an org tag: a tag is letters,"
                                     <> " digits, and _ - @ or #")

-- | The classification chain behind ROWS, made visible: one entry per SOURCE in
-- precedence order, each holding the keywords it is the WIDEST to declare.
--
-- This is 'Data.Org.Config.classify' turned inside out, over the very list that
-- one folds ('Data.Org.Config.keywordScopes'): that function takes the first
-- scope with an opinion about a keyword, and this one reports what each scope
-- claims.  Deduplication IS the classification rule — a keyword @default@ and a
-- file both declare belongs to @default@ alone, so it appears in that entry and
-- nowhere below it.  A source left with nothing after that is dropped rather
-- than shown empty, which is why a @system.org@ redeclaring TODO and DONE shows
-- its OTHER keywords and no row at all when it has none.
--
-- Each entry's own active\/inactive split is that source's, which is why the
-- answer classifies as well as enumerates: @system.org@ writing @| READING@
-- puts READING in the system entry's inactive half, and a @book@ config writing
-- it before the bar reaches a row only where the system layer said nothing.
-- The dedup decides which of the two a row is shown, and the wider one wins.
--
-- What this layer adds to the scopes is the ROWS.  A record supplies its file's
-- own declarations and its tags, and SEVERAL of them — the marked set — merge
-- by source NAME: the file entry is the union of those rows' files' own
-- pragmas, and the tags are every tag any of them carries, in first-seen order
-- across the rows as given.  Merging costs one property: a keyword one row
-- reaches through its file and another through a tag lands in the WIDER of the
-- two, so the table describes the set rather than any one member of it.  Rows
-- whose tag ORDER disagrees are resolved the same way, by the merged order.
--
-- Over ONE row this IS what 'setStateEdits' accepts: 'settableStates' is this
-- function flattened, so the offer and the wall cannot come apart.  Over
-- SEVERAL the merge outruns that — a keyword only one row's file or one row's
-- tag declares is offered for the set, and committing it refuses the whole
-- request rather than moving the rows it would have fitted.  That is the
-- merge's cost stated as what a reader sees: the table describes the set, and a
-- keyword belonging to part of it is a refusal naming the row it does not fit.
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

-- | The span edits @set-state@ makes to R.
--
-- Three shapes, decided by what the headline carries.  A keyword over one
-- already there is that keyword's own span and nothing else.  A keyword where
-- there is none is an insertion right after the stars — org's own place for it,
-- and the one offset every headline has, present or empty.  And 'Nothing'
-- deletes the keyword together with the horizontal space behind it, so
-- @* TODO Title@ closes up to @* Title@ rather than keeping the gap; the run
-- deleted is horizontal only, so a keyword that is the last thing on its line
-- keeps the newline that ends it.  A headline with no keyword asked to drop one
-- costs no edit.
--
-- KEYWORD is refused unless R's OWN CHAIN declares it ('settableStates'): org's
-- TODO\/DONE, @system.org@, the configs of the tags THIS row carries, the file's
-- own @#+TODO:@ lines.  The bar is the chain rather than the parse's
-- recognized set ('hrKeywords') because the chain is what a reader is shown: the
-- palette draws 'keywordSources' and a state it does not offer is one this row
-- has no configuration for.  Recognition stays a superset — a word another tag's
-- cycle names still parses as a state here rather than as the first word of a
-- title — and settability is the narrower question of what this row is
-- configured to be.  The state column's group meta-values (@*active*@,
-- @*inactive*@) are in no keyword set, so they are refused here like any other
-- word that is not one.
setStateEdits :: ConfigLayers -> Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setStateEdits _cfg Nothing r = Right [ (Span (spanStart sp) (spanEnd sp + trailing sp), "")
                                     | Just sp <- [hsTodo (headlineSpans r)] ]
  where trailing sp = T.length (T.takeWhile horizontal (T.drop (spanEnd sp) (hrDoc r)))
setStateEdits cfg (Just keyword) r
  | keyword `notElem` settable =
      Left (keyword <> " is not a TODO keyword for " <> hrId r <> " in " <> T.pack (hrFile r)
              <> "; that row may be set to " <> T.intercalate ", " settable)
  | otherwise = Right [placed (hsTodo hs)]
  where hs = headlineSpans r
        settable = settableStates cfg r
        placed (Just sp) = (sp, keyword)
        placed Nothing   = (insertAt (spanEnd (hsStars hs)), " " <> keyword)

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

-- | The span edits @add-tag@ makes to R: TAG joining its tag list.  A row
-- already carrying it ('tagged') costs no edit at all, which is what makes the
-- command idempotent — adding a tag over a marked set twice writes each file
-- twice and changes it once.
--
-- Two shapes.  With tags present the tag joins the list as its own last entry
-- (@:a:b:@ becomes @:a:b:TAG:@): the span ends past the closing colon, so
-- the whole insertion is the tag and one colon at that offset, which leaves the
-- tags already there byte-identical.  With none it is appended to the title
-- line, after the last
-- part that line carries — the title, or the priority, or the keyword, or the
-- stars themselves.  'hsFull' cannot serve there: its end is the last part in
-- span order, which for a scheduled headline is a timestamp on the NEXT line
-- and for one with a drawer is its @:END:@.
--
-- TAG is written as it was given.  Presence is folded, so a row spelling
-- @:Work:@ is not given a second @:work:@; a row with no tag at all takes the
-- spelling the caller sent.
addTagEdits :: Text -> HeadlineRecord -> [(Span, Text)]
addTagEdits tag r
  | tagged tag r         = []
  | Just sp <- hsTags hs = [ (insertAt (spanEnd sp), tag <> ":") ]
  | otherwise            = [ (insertAt (titleLineEnd hs), " :" <> tag <> ":") ]
  where hs = headlineSpans r

-- | The span edits @remove-tag@ makes to R: TAG cut out of its tag list.  A row
-- that does not carry it costs no edit, which is the other half of the pair's
-- idempotence.
--
-- Two shapes, and which one is decided by what the run has LEFT.  An entry with
-- neighbours is cut as @TAG:@ — itself and the colon that closes it — so
-- @:a:b:c:@ minus @b@ is @:a:c:@ and the surviving entries keep their bytes.
-- The LAST entry takes the whole run with it, together with the horizontal space
-- that separated the run from the title: a lone @:@ is not a tag list, and
-- @* Title :done:@ has to close up to @* Title@ rather than keep a trailing
-- space.  That run is the parser's own separator ('Data.Org.Parser.tagsP' opens
-- on @hspace1@), so there is always one and it is inside this line.
--
-- Matching is FOLDED and takes EVERY entry spelling the tag, which is what makes
-- "remove it" leave the row not carrying it under 'tagged' — a file spelling one
-- tag twice, or spelling it @:Work:@ where the caller said @work@, is still
-- clean afterwards.
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
-- A row that does not carry FROM costs no edit, which is what makes a rename
-- over a marked set safe to send whole — and what makes it idempotent, since a
-- second request finds nothing left spelling FROM.
--
-- The entry is replaced WITHOUT its closing colon, so the run's other entries
-- and both of its delimiters keep their bytes and the tag lands where the
-- author put it: @:a:work:b:@ renamed to @projects@ is @:a:projects:b:@, never
-- an entry cut from the middle and appended at the end.  This is the reason
-- rename is a command of its own rather than a remove and an add composed.
-- Those two edit sets APPLY — they touch, and 'Data.Org.Edit.applyEdits' allows
-- an edit to start where the last one ended — and what they write is wrong in
-- two INDEPENDENT ways.  The addition's anchor is 'spanEnd' of 'hsTags' measured
-- BEFORE the removal, so for a lone tag it is where the run's closing colon sat;
-- the removal takes the whole run and the space in front of it, and the
-- insertion lands flush against the title, spelling @* TODO Ship itprojects:@.
-- Separately, and whatever the anchor, this appends at the RUN'S END, so an
-- entry with neighbours survives having MOVED there — re-measuring after the
-- removal would not change that one.  The pair is also two writes under two
-- digests where this is one drift-locked splice per file.
--
-- ONE TAG ONCE, which is the invariant 'removeTagEdits' keeps by cutting every
-- entry that spells its tag.  Here the FIRST entry spelling FROM becomes TO and
-- any further ones are cut, so a file spelling one tag twice comes out clean.
-- And where the row ALREADY carries TO under some other entry, every FROM entry
-- is cut instead: the rename would otherwise write a duplicate.  That branch
-- can never empty the run, since the entry carrying TO is one of the ones it
-- leaves standing.
--
-- Matching FROM is FOLDED and TO is written as it was given, which is
-- 'addTagEdits'' rule and makes a change of SPELLING a rename like any other:
-- @:Work:@ renamed to @work@ is one replacement.
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
                      , T.length (T.takeWhileEnd horizontal (T.take ahead line))
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
-- Four shapes.  An entry already there is its own span and nothing else, so a
-- reschedule moves the timestamp and leaves the keywords, the spacing and the
-- other entries on the line byte-identical.  An entry where there is none joins
-- the END of the planning line, behind whatever it already carries, which is
-- the lens's own rule for an entry that moved.  A headline with no planning line
-- at all grows one under its TITLE LINE, where org puts one, at column 1 like
-- the stars.  And a clear takes the entry out together with the horizontal space
-- that separated it from its neighbour — the TRAILING run, or the leading one
-- where the entry ends its line — or takes the WHOLE LINE when it was the last
-- entry on it, since a planning line with no entries is not one.
--
-- Clearing an entry a headline never had costs no edit, which makes the command
-- idempotent the way @archive@ is.  KEYWORD is refused unless it is one a key
-- may set ('settableKeywords').
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
            trailing = runOf (T.drop (spanEnd sp - from) line)
            leading  = T.length (T.takeWhileEnd horizontal (T.take (at - from) line))
            runOf    = T.length . T.takeWhile horizontal

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
    bracketed = "<" `T.isPrefixOf` want || "[" `T.isPrefixOf` want
    refusal   = Left (text <> " is not a date: spell it 2026-08-05, 2026-08-05 09:30,"
                        <> " +3d, +2w, +1m, today, tomorrow, or org's own <2026-08-05 Wed>")

    -- One rendering site: a relative form and a bare ISO date differ in how the
    -- DAY is worked out and in nothing else, and a @+3d@ can never parse as ISO,
    -- so the two feed one alternative rather than each rendering for itself.
    dated = relative <|> asDay
    relative = case T.toLower want of
      "today"    -> Just today
      "tomorrow" -> Just (Time.addDays 1 today)
      offset     -> shifted offset
    shifted offset = do
      digits <- T.stripPrefix "+" offset
      (n, unit) <- either (const Nothing) Just (TR.decimal digits :: Either String (Integer, Text))
      case unit of
        "d" -> Just (Time.addDays n today)
        "w" -> Just (Time.addDays (7 * n) today)
        "m" -> Just (Time.addGregorianMonthsClip n today)
        _no -> Nothing

    asDay :: Maybe Time.Day
    asDay = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" (T.unpack want)
    -- @%k@ rather than @%H@ for the hour: it reads one digit as well as two, so
    -- @9:05@ is the time a reader meant rather than a refusal over a zero.
    asLocal :: Maybe Time.LocalTime
    asLocal = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %k:%M" (T.unpack want)
    withTime at = stamped (Time.localDay at) (Just (spelled "%H:%M" at))
    stamped = orgStamp activeBrackets

-- | The brackets org writes a timestamp in: @\<…\>@ for one an agenda picks up,
-- @[…]@ for one that is a record and nothing else.
activeBrackets, inactiveBrackets :: (Text, Text)
activeBrackets   = ("<", ">")
inactiveBrackets = ("[", "]")

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

-- | T under FMT, in the locale org writes.
spelled :: Time.FormatTime t => String -> t -> Text
spelled fmt = T.pack . Time.formatTime Time.defaultTimeLocale fmt

-- | The property a captured entry carries, org-glance's own spelling.
captureProperty :: Text
captureProperty = "ORG_GLANCE_CREATION_TIME"

-- | NOW as 'captureProperty' spells a moment: org's INACTIVE timestamp,
-- @[YYYY-MM-DD Day HH:MM]@, in the server's own zone.  Inactive because a
-- creation time is a record of when a row was written rather than something to
-- turn up on an agenda.
captureStamp :: Time.ZonedTime -> Text
captureStamp now = orgStamp inactiveBrackets (Time.localDay at) (Just (spelled "%H:%M" at))
  where at = Time.zonedTimeToLocalTime now

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
captureEdits doc stamp text
  | T.null typed          = Left "a capture needs a headline: the text that goes after the star"
  | T.any (== '\n') typed = Left "a captured entry is one headline, so its text is one line"
  | otherwise             = Right [(insertAt (T.length doc), opening <> entry)]
  where
    typed = T.strip text
    eol   = eolOf doc
    -- A file whose last line has no newline would otherwise take the stars onto
    -- the end of that line, where they are no headline at all.
    opening | T.null doc || "\n" `T.isSuffixOf` doc = ""
            | otherwise                             = eol
    entry = T.concat [ line <> eol
                     | line <- [ "* " <> typed
                               , ":PROPERTIES:"
                               , ":" <> captureProperty <> ": " <> stamp
                               , ":END:" ] ]

-- | The span edits writing LINES as the @#+TODO:@ block of a config file
-- holding DOC, or why LINES are not a block.
--
-- What a keyword layer may say is one small grammar, and it is checked ahead of
-- the write rather than discovered after it: blank lines are dropped, every
-- line left has to be a @#+TODO:@ pragma, and the block has to declare at least
-- one keyword — a pragma the parser reads as declaring nothing would leave a
-- layer looking configured and doing nothing.  An EMPTY block is always
-- allowed, and it is the deletion: posting nothing is how a layer is taken off,
-- and how a file that never had a line stays that way.
--
-- The state column's group meta-values need no rule of their own here, and a
-- guard against them would be unreachable: a keyword token is letters and
-- underscores ('Data.Org.Parser.keywordTextP'), so @#+TODO: *active* | DONE@
-- does not parse as a cycle at all and is refused as declaring nothing.  It is
-- the same wall 'setStateEdits' puts up from the other side, reached earlier —
-- the parser will not let the word into a keyword set, so nothing can write it
-- into one either.  The message says so, since that is the refusal a reader
-- typing the group name gets.
--
-- WANT and TARGET are the default view and the capture target the same file
-- names, both of which the system layer carries and a tag layer never does:
-- 'Nothing' leaves that line exactly as it is, @Just \"\"@ takes it away, and
-- anything else writes it.  They ride in this one call because they are lines of
-- the same file, and three calls would be three writes under three digests, each
-- of which the one before it had just invalidated.
--
-- The spans are the file's own lines ('Data.Org.Config.todoLineEdits'), so
-- everything a config file is besides its cycle — the @#+TITLE:@, the comments,
-- the capture template — is bytes this never names.
configEdits :: Text -> [Text] -> Maybe Text -> Maybe Text -> Either Text [(Span, Text)]
configEdits doc asked want target
  | not (null strange) = Left ("not a #+TODO: line: " <> T.intercalate " · " strange)
  | null lines'        = Right (todoLineEdits doc [] <> lineEdits)
  | null declared      = Left declaresNothing
  | otherwise          = Right (todoLineEdits doc lines' <> lineEdits)
  where
    lineEdits = maybe [] (defaultFilterEdits doc) want
             <> maybe [] (captureTargetEdits doc) target
    lines'   = filter (not . T.null . T.strip) asked
    -- A LINE, and the pragma test is a prefix one: an entry carrying a newline
    -- of its own would pass it and write everything past that newline into the
    -- file unread.  One line per line is what makes this a #+TODO:-only splice.
    strange  = filter (\l -> not (isTodoPragma l) || T.isInfixOf "\n" l) lines'
    keywords = todoPragmas (T.unlines lines')
    declared = tkActive keywords <> tkInactive keywords

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
viewJSONWith chain viewTitle palette records = object
  (  [ "title" .= viewTitle, "columns" .= columns palette, "actions" .= actions ]
  <> declaredSort chain
  <> [ "rows" .= map rowJSON records ])

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

-- | 'viewJSONWith' encoded.
viewJSONTextWith :: SortChain -> Text -> TodoKeywords -> [HeadlineRecord] -> TL.Text
viewJSONTextWith chain viewTitle palette =
  encodeToLazyText . viewJSONWith chain viewTitle palette

-- | The view's columns, in the order the table draws them: the key a filter
-- names, the header over the cells, the type @table-view\/SCHEMA.md@ declares,
-- and where the cell comes out of a row.  A cell is a 'Maybe': 'Nothing' is the
-- @null@ a row's JSON carries for a column it has no value for, and the empty
-- string a filter reads there ('viewCells').
--
-- One table, so the FOUR things that have to agree cannot drift: 'columns'
-- declares them, 'rowJSON' fills them, 'filterKeys' names them, and 'viewCells'
-- joins them into 'hrSearch' — which is what lets a predicate read one field of
-- that text by its key's position.  A column appended here is therefore a
-- column a filter can name the day it lands, with no second list to extend.
viewColumns :: [(Text, Text, Text, HeadlineRecord -> Maybe Text)]
viewColumns =
  [ ("state",     "State",     "badge", hrState)
  , ("priority",  "Pri",       "text",  hrPriority)
  , ("title",     "Headline",  "text",  Just . hrTitle)
  , ("tag",       "Tags",      "text",  Just . sortedTagsCell . hrTags)
  , ("scheduled", "Scheduled", "text",  hrScheduled)
  , ("deadline",  "Deadline",  "text",  hrDeadline)
  ]

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

-- | 'viewColumns' as SCHEMA.md's Column objects, PALETTE giving the state
-- badges.  Every column sorts, so @sortable@ rides on the column itself
-- ('column'); what a kind adds past that is the priority letters, the badge
-- list and the tags column's @multi@ declaration.
columns :: TodoKeywords -> [Value]
columns palette =
  [ column key header kind (extra key) | (key, header, kind, _cell) <- viewColumns ]
  where
    extra key = case key of
      "state"    -> [ "badges" .= badges palette, "values" .= stateValues ]
      "priority" -> [ "values" .= (["A", "B", "C"] :: [Text]) ]
      -- Declared rather than left to be sampled: the renderer decides which
      -- column holds a LIST from up to 40 non-empty cells, so a page with fewer
      -- than two tagged rows finds none at all — and then @tag:*archive*@ is
      -- the literal it matches nothing with, where this producer reads it as
      -- the whole tag.  The declaration wins there.
      "tag"      -> [ "multi" .= True, "values" .= tagValues ]
      _          -> []

-- | The state column's meta values: filter vocabulary rather than cell text.
-- SCHEMA.md lets a producer add values over a column's own domain, and this one
-- adds org-glance's two keyword groups — @*active*@ is every keyword a file's
-- @#+TODO:@ line declares before the bar, @*inactive*@ every one after it
-- ('Glance.Web.Filter').  No cell ever holds either, which is why they travel
-- beside the badges rather than among them: a renderer completing the column
-- offers the concrete keywords and these two, and the starred spelling is what
-- says that a group is not a badge.
stateValues :: [Text]
stateValues = ["*active*", "*inactive*"]

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
rowJSON r = object
  (  [ "id" .= hrId r
     , "cells" .= object [ Key.fromText key .= toJSON (cell r)
                         | (key, _header, _kind, cell) <- viewColumns ] ]
  <> [ "linked" .= True | hrLinked r ])

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
  group "active" activeColors actives <> group "inactive" inactiveColors inactives
  where group g hues = zipWith (badge g) (cycle hues)
        badge g color value =
          object [ "value" .= value, "color" .= color, "group" .= (g :: Text) ]

-- | Warm hues for keywords that still want work.
activeColors :: [Text]
activeColors = ["#e0af68", "#ff9e64", "#f7768e", "#ffc777"]

-- | Cool hues for the done-like ones.
inactiveColors :: [Text]
inactiveColors = ["#9ece6a", "#73daca", "#41a6b5", "#565f89"]
