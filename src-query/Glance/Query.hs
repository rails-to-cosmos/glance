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
                    , TodoKeywords (..)
                    , ViewOrder (..)
                    , WalkOptions (..)
                    , WriteFailure (..)
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
                    , headlineParts
                    , hiddenProperties
                    , keywordSources
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
                    , orderedForView
                    , planningKeywords
                    , planningTimestamp
                    , readConfigLayers
                    , readsAsTimestamp
                    , recomposedSubtree
                    , replaceSpans
                    , resolveIds
                    , rowJSON
                    , setPlanningEdits
                    , setStateEdits
                    , sortedForView
                    , subtreeText
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
import Data.Either (fromRight)
import Data.List (foldl', sort, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
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
                , TsMoment (tsmHasTime, tsmTime), deadline, defaultContext
                , hsFull, identity, indent, metaCategory, orgParse, priority
                , schedule, sliceSpan, spans, tags, title, todo, todoActive
                , todoInactive )
import Data.Org.Config ( ConfigLayerFile (..), ConfigLayers (..), TodoKeywords (..)
                       , builtinFilter, captureTargetEdits, captureTargetIn
                       , captureTargetOf, classify, configDirIn, declaredKeywords
                       , defaultCaptureFile, defaultFilter
                       , defaultFilterEdits, defaultFilterOf, isTodoPragma
                       , keywordScopes
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
  , hrTags      :: !Text            -- ^ @":a:b:"@, empty when untagged.
  , hrScheduled :: !(Maybe Text)    -- ^ ISO date, see 'isoStamp'.
  , hrDeadline  :: !(Maybe Text)    -- ^ ISO date, see 'isoStamp'.
  , hrSearch    :: !Text            -- ^ the cells as they display, lowercased; see 'searchTextOf'.
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
recordOf cfg declared path ordinal doc digest category keywords h subtree = forceRecord HeadlineRecord
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
  , hrSearch    = searchTextOf [ opt state, opt pri, titleCell, tagsCell
                               , opt scheduled, opt due ]
  , hrActive    = classify cfg declared (tagsOfCell tagsCell) <$> state
  }
  where sp = spans h
        -- The span is the lossless channel; the render is what is left when a
        -- headline carries no span for a component, which is to say when the
        -- component is empty.
        cut mspan render = detach (maybe render (sliceSpan doc) mspan)
        opt = fromMaybe ""
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
            | otherwise                  = T.concat (go s)
  where
    go rest
      | T.null after = [before]
      | otherwise    = case linkAt (T.drop 2 after) of
          Just (shown, more) -> before : shown : go more
          Nothing            -> before : "[[" : go (T.drop 2 after)
      where (before, after) = T.breakOn "[[" rest

-- | The link opening TEXT — which starts past its @[[@ — as it displays, with
-- whatever follows it.  'Nothing' when TEXT does not close one.
linkAt :: Text -> Maybe (Text, Text)
linkAt text
  | T.null target || T.null rest = Nothing
  | otherwise = case T.uncons (T.drop 1 rest) of
      Just (']', more) -> Just (target, more)                  -- [[TARGET]]
      Just ('[', more) | "]]" `T.isPrefixOf` after'            -- [[TARGET][DESC]]
                       -> Just (if T.null desc then target else desc, T.drop 2 after')
        where (desc, after') = T.break (== ']') more
      _notALink        -> Nothing
  where (target, rest) = T.break (== ']') text

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

-- | The tags CELL names, one per tag: org writes them @:a:b:@, so splitting on
-- the colon and dropping the empties is the whole of it.  Lowercased through
-- 'displayText' like the search text, so a tag read off a row here is the same
-- string a filter compares against.
--
-- This is the vocabulary a producer's virtual filter keys come from
-- (@table-view\/SCHEMA.md@, Filter query): every distinct tag in the column is
-- a key, and a renderer deriving them from the rows it holds has to get the
-- same list out of the same cells.
tagsOfCell :: Text -> [Text]
tagsOfCell = filter (not . T.null) . T.splitOn ":" . T.toLower . displayText

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

-- | RECORDS in the order 'viewJSON' declares them sorted: scheduled ascending,
-- an unscheduled row first, ties left in walk order.  A page has to be cut out
-- of this order rather than out of the walk, or page two is a different set of
-- rows than the table's own sort would put there.
sortedForView :: [HeadlineRecord] -> [HeadlineRecord]
sortedForView = sortOn (fromMaybe "" . hrScheduled)

-- | Which order a view's rows are in, and what it declares about them.
--
-- 'ScheduledOrder' is the view every client has had: the rows sorted by
-- 'sortedForView' and a @sort@ field saying so.  'DocumentOrder' leaves them in
-- walk order — which for this producer is document order, headline by headline
-- down each file — and emits NO @sort@ field at all, since SCHEMA.md reads an
-- absent one as "the order they arrived in".  The pair travels together on
-- purpose: 'orderedForView' arranges the rows and 'viewJSONWith' declares what
-- was done, and a view whose declaration disagrees with its rows is one a
-- renderer will re-sort out from under the reader.
--
-- __Experimental__: reached by @\/headlines?order=document@ alone.  Nothing
-- else in the wire contract turns on it, and a renderer is free to ignore it.
-- Rows being top entries, document order is the order the files list them in
-- rather than an outline the reader can see the shape of.
data ViewOrder = ScheduledOrder | DocumentOrder
  deriving (Eq, Show)

-- | RECORDS in ORDER: 'sortedForView' for 'ScheduledOrder', untouched walk
-- order for 'DocumentOrder'.
orderedForView :: ViewOrder -> [HeadlineRecord] -> [HeadlineRecord]
orderedForView ScheduledOrder = sortedForView
orderedForView DocumentOrder  = id

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
forceRecord :: HeadlineRecord -> HeadlineRecord
forceRecord r =
  forcing (hrId r : hrCategory r : hrTitle r : hrTags r : hrDigest r : hrSearch r : optional)
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

-- | The org tag a headline wears once it is archived, as org spells it.  The
-- filter key that hides it is this folded ('Glance.Web.Filter'), so one literal
-- serves the write and the predicate over what it wrote.
archiveTag :: Text
archiveTag = "ARCHIVE"

-- | Does R carry 'archiveTag'?  Read off the tags cell through the same
-- 'tagsOfCell' the filter vocabulary is built with, so "archived" means exactly
-- what the query @archive:@ means.  Two readers: 'archiveEdits', which owes
-- nothing to a row that is already there, and the served view, which hides one
-- unless asked.
archived :: HeadlineRecord -> Bool
archived r = T.toLower archiveTag `elem` tagsOfCell (hrTags r)

-- | The classification chain behind ROWS, made visible: one entry per SOURCE in
-- precedence order, each holding the keywords it is the NEAREST to declare.
--
-- This is 'Data.Org.Config.classify' turned inside out, over the very list that
-- one folds ('Data.Org.Config.keywordScopes'): that function takes the first
-- scope with an opinion about a keyword, and this one reports what each scope
-- claims.  Deduplication IS the classification rule — a keyword the file and a
-- tag both declare belongs to the file alone, so it appears in the file's entry
-- and nowhere below it.  A source left with nothing after that is dropped
-- rather than shown empty.
--
-- Each entry's own active\/inactive split is that source's, which is why the
-- answer classifies as well as enumerates: @system.org@ writing @| READING@
-- puts READING in the system entry's inactive half, and a @book@ config
-- writing it before the bar puts it in the book entry's active half — and the
-- dedup decides which of the two a given row is shown.
--
-- What this layer adds to the scopes is the ROWS.  A record supplies its file's
-- own declarations and its tags, and SEVERAL of them — the marked set — merge
-- by source NAME: the file entry is the union of those rows' files' own
-- pragmas, and the tags are every tag any of them carries, in first-seen order
-- across the rows as given.  Merging costs one property: a keyword one row
-- reaches through its file and another through a tag lands in the NEARER of the
-- two, so the table describes the set rather than any one member of it.  Rows
-- whose tag ORDER disagrees are resolved the same way, by the merged order.
--
-- Everything reported is settable on every row named: the reserved scopes are
-- all in the parse seed or in org's own cycle, and a file's own declarations
-- are its own, so 'setStateEdits' accepts each of them for the rows it came
-- from.
keywordSources :: ConfigLayers -> [HeadlineRecord] -> [(Text, TodoKeywords)]
keywordSources cfg rows = nearest Set.empty (sortOn fst chain)
  where
    -- The one scope whose value differs between rows, merged before the chain
    -- is built.  Every other entry a row contributes is a function of the tag
    -- or a constant, so a repeat carries the same set — and 'nearest' drops a
    -- repeat by construction, everything it declares being seen already.
    filed   = mergeKeywords (map hrDeclared rows)
    chain   = [ (rank, (source, kw))
              | r <- rows
              , (rank, source, kw) <- keywordScopes cfg filed (tagsOfCell (hrTags r)) ]
    -- 'sortOn' is stable, so the scopes keep their order and the tags keep the
    -- order the rows named them in.
    nearest _seen [] = []
    nearest seen ((_rank, (source, kw)) : rest)
      | null actives && null inactives = nearest seen rest
      | otherwise = (source, TodoKeywords actives inactives) : nearest taken rest
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
-- KEYWORD is refused unless R's own parse recognized it ('hrKeywords'): the
-- file's @#+TODO:@ lines, the config layers' keywords and org's TODO\/DONE.
-- The bar is recognition rather than declaration because that is what the
-- parser reads back — writing a word this file would parse as the first word of
-- a title makes a headline the reader sees differently than the writer meant,
-- and the config layer is precisely the thing that stops a word being that.
-- The state column's group meta-values (@*active*@, @*inactive*@) are in no
-- keyword set, so they are refused here like any other word that is not one.
setStateEdits :: Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setStateEdits Nothing r = Right [ (Span (spanStart sp) (spanEnd sp + trailing sp), "")
                                | Just sp <- [hsTodo (headlineSpans r)] ]
  where trailing sp = T.length (T.takeWhile horizontal (T.drop (spanEnd sp) (hrDoc r)))
setStateEdits (Just keyword) r
  | keyword `notElem` declared = Left (keyword <> " is not a TODO keyword of " <> T.pack (hrFile r)
                                        <> "; it declares " <> T.intercalate ", " declared)
  | otherwise = Right [placed (hsTodo hs)]
  where hs = headlineSpans r
        declared = tkActive (hrKeywords r) <> tkInactive (hrKeywords r)
        placed (Just sp) = (sp, keyword)
        placed Nothing   = (insertAt (spanEnd (hsStars hs)), " " <> keyword)

-- | The span edits @archive@ makes to R: 'archiveTag' added to its tag list.  A
-- row already carrying it costs no edit at all, which is what makes the command
-- idempotent — archiving a marked set twice writes each file twice and changes
-- it once.
--
-- Two shapes.  With tags present the tag joins the list as its own last entry
-- (@:a:b:@ becomes @:a:b:ARCHIVE:@): the span ends past the closing colon, so
-- the whole insertion is the tag and one colon at that offset, which leaves the
-- tags already there byte-identical.  With none it is appended to the title
-- line, after the last
-- part that line carries — the title, or the priority, or the keyword, or the
-- stars themselves.  'hsFull' cannot serve there: its end is the last part in
-- span order, which for a scheduled headline is a timestamp on the NEXT line
-- and for one with a drawer is its @:END:@.
archiveEdits :: HeadlineRecord -> [(Span, Text)]
archiveEdits r
  | archived r           = []
  | Just sp <- hsTags hs = [ (insertAt (spanEnd sp), archiveTag <> ":") ]
  | otherwise            = [ (insertAt (titleLineEnd hs), " :" <> archiveTag <> ":") ]
  where hs = headlineSpans r

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
-- @table-view/SCHEMA.md@, with the state palette taken from RECORDS themselves.
viewJSON :: Text -> [HeadlineRecord] -> Value
viewJSON viewTitle records =
  viewJSONWith ScheduledOrder viewTitle (mergeKeywords (map hrKeywords records)) records

-- | 'viewJSON' in ORDER and with the state column's PALETTE given rather than
-- derived.  A server answering a page has to pass the whole store's palette:
-- the badge list is what a client watches for a column change, and deriving it
-- from the rows that happen to be on this page would move it every time the
-- page did.  ORDER declares what 'orderedForView' did to RECORDS; see
-- 'ViewOrder' for why the two are one decision.
viewJSONWith :: ViewOrder -> Text -> TodoKeywords -> [HeadlineRecord] -> Value
viewJSONWith order viewTitle palette records = object
  (  [ "title" .= viewTitle, "columns" .= columns palette, "actions" .= actions ]
  <> declaredSort order
  <> [ "rows" .= map rowJSON records ])

-- | The @sort@ field ORDER declares, or nothing at all for 'DocumentOrder'.
declaredSort :: ViewOrder -> [Pair]
declaredSort DocumentOrder  = []
declaredSort ScheduledOrder =
  [ "sort" .= object [ "column" .= ("scheduled" :: Text), "ascending" .= True ] ]

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
viewJSONTextWith :: ViewOrder -> Text -> TodoKeywords -> [HeadlineRecord] -> TL.Text
viewJSONTextWith order viewTitle palette =
  encodeToLazyText . viewJSONWith order viewTitle palette

-- | The view's columns, in the order the table draws them: the key a filter
-- names, the header over the cells, the type @table-view\/SCHEMA.md@ declares,
-- and where the cell comes out of a row.
--
-- One table, so the three things that have to agree cannot drift: 'columns'
-- declares them, 'rowJSON' fills them, and 'filterKeys' names them.  It is also
-- the order 'searchTextOf' joins the cells in, which is what lets a predicate
-- read one field of 'hrSearch' by its key's position.
viewColumns :: [(Text, Text, Text, HeadlineRecord -> Value)]
viewColumns =
  [ ("state",     "State",     "badge", toJSON . hrState)
  , ("priority",  "Pri",       "text",  toJSON . hrPriority)
  , ("title",     "Headline",  "text",  toJSON . hrTitle)
  , ("tag",       "Tags",      "text",  toJSON . hrTags)
  , ("scheduled", "Scheduled", "text",  toJSON . hrScheduled)
  , ("deadline",  "Deadline",  "text",  toJSON . hrDeadline)
  ]

-- | The column keys a filter may name, in view order.  Matched
-- case-sensitively, the way a renderer matches its own column keys.
filterKeys :: [Text]
filterKeys = [ key | (key, _header, _kind, _cell) <- viewColumns ]

-- | 'viewColumns' as SCHEMA.md's Column objects, PALETTE giving the state
-- badges.  What a column carries past its key, header and type is the kind's:
-- which columns sort, the priority letters, and the badge list.
columns :: TodoKeywords -> [Value]
columns palette =
  [ column key header kind (extra key) | (key, header, kind, _cell) <- viewColumns ]
  where
    extra key = case key of
      "state"    -> sortable <> [ "badges" .= badges palette, "values" .= stateValues ]
      "priority" -> sortable <> [ "values" .= (["A", "B", "C"] :: [Text]) ]
      "title"    -> []
      -- Declared rather than left to be sampled: the renderer decides a
      -- column's arity from up to 40 non-empty cells, so a page with fewer
      -- than two tagged rows finds no multi-valued column at all and ORs
      -- @tag:a tag:b@ where this producer ANDs it.  The declaration wins there.
      "tag"      -> [ "multi" .= True ]
      _date      -> sortable
    sortable = [ "sortable" .= True ]

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

-- | A column object: KEY, HEADER and TYPE, then whatever EXTRA the kind needs.
column :: Text -> Text -> Text -> [Pair] -> Value
column key header kind extra =
  object ([ "key" .= key, "header" .= header, "type" .= kind ] <> extra)

-- | One row: the identity a renderer keys updates off, and its cells.  Exported
-- because a live producer streams rows one at a time — a @upsert-row@ frame
-- carries exactly this object, so the streamed row and the row in the initial
-- view are built by the same code.
rowJSON :: HeadlineRecord -> Value
rowJSON r = object
  [ "id" .= hrId r
  , "cells" .= object [ Key.fromText key .= cell r | (key, _header, _kind, cell) <- viewColumns ]
  ]

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
