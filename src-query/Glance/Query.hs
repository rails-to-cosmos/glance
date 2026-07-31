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
module Glance.Query ( HeadlineParts (..)
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
                    , cellSep
                    , defaultWalk
                    , derivedPath
                    , digestOfText
                    , displayText
                    , documentPath
                    , filterKeys
                    , headlineParts
                    , loadDir
                    , loadDirFilesSerially
                    , loadDirFilesWith
                    , loadDirWith
                    , loadFile
                    , matchesSearch
                    , mergeKeywords
                    , orderedForView
                    , recomposedSubtree
                    , replaceSpans
                    , resolveIds
                    , rowJSON
                    , setStateEdits
                    , sortedForView
                    , subtreeText
                    , tagsOfCell
                    , viewJSON
                    , viewJSONTextWith
                    , viewJSONWith
                    ) where

import Control.Exception (IOException, evaluate, try)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Pair)
import Data.Either (fromRight)
import Data.List (foldl', sort, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import TextShow (showt)

import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
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
import Data.Org.Walk ( Found (..), WalkOptions (..), beatsForId, defaultWalk
                     , findOrgFilesWith, isDerived, isDocument
                     , mapFilesConcurrently )

import qualified Data.Org.Edit as Edit

-- Records

-- | One row's worth of a headline: where it came from, the cells the view
-- shows, and the parsed headline itself as an opaque passthrough — later
-- milestones read its spans (write-back) and its links (graph) from here.
data HeadlineRecord = HeadlineRecord
  { hrFile      :: !FilePath        -- ^ path the headline was read from, as walked.
  , hrId        :: !Text            -- ^ row identity; see 'rowId'.
  , hrCategory  :: !Text            -- ^ the file's final @#+CATEGORY@, empty when unset.
  , hrHeadline  :: !Headline        -- ^ the parsed headline; its type stays private.
  , hrKeywords  :: !TodoKeywords    -- ^ the file's TODO keywords; one value shared per file.
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
  } deriving (Show)

-- | The TODO keywords one file's context declared, active ones apart from the
-- done-like ones, each set in the order 'Data.Set.Set' keeps it.
data TodoKeywords = TodoKeywords
  { tkActive   :: ![Text]
  , tkInactive :: ![Text]
  } deriving (Eq, Show)

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

-- | Every headline under DIR, one record each.  Walks @*.org@ recursively,
-- reads each file strictly and parses it from 'defaultContext' — per-file
-- context is an invariant: keywords declared in one file never reach another.
-- org-glance's derived mirrors are not walked ('Data.Org.Walk').
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
-- sound because a file is parsed from 'defaultContext' and shares no state with
-- any other.  The answer is the sorted path list zipped with its outcomes, so
-- it is the sequence 'loadDirFilesSerially' produces whatever order the pool
-- finished in — and everything downstream (id resolution, the store's walk
-- order, the counts) reads that sequence rather than the completion order.
loadDirFilesWith :: WalkOptions -> FilePath
                 -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesWith = loadDirFilesUsing mapFilesConcurrently

-- | 'loadDirFilesWith' with the pool taken out — one file after another on the
-- calling thread.  It is the reference the parallel load is asserted equal to
-- (@TestQuery@), and it is exported for that: every other answer this library
-- gives over a directory is a fold of this pair, so two loads agreeing here
-- agree everywhere.
loadDirFilesSerially :: WalkOptions -> FilePath
                     -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesSerially = loadDirFilesUsing mapM

-- | 'loadDirFilesWith' with OVER deciding how the walk's files are crossed.
loadDirFilesUsing :: ((FilePath -> IO (Either LoadFailure [HeadlineRecord]))
                      -> [FilePath] -> IO [Either LoadFailure [HeadlineRecord]])
                  -> WalkOptions -> FilePath
                  -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFilesUsing over opts dir = do
  found <- findOrgFilesWith opts [dir]
  let paths = sort (foundFiles found)
  outcomes <- over loadFile paths
  pure (zip paths outcomes, length (foundDirErrs found))

-- | PATH's headlines, or why it has none.  Reads the file strictly and parses
-- it from 'defaultContext': a file's own @#+TODO:@ lines are the only ones that
-- reach its headlines, whether it is loaded with a directory on a pool or on
-- its own after an edit.
--
-- The rows come back forced: a caller running this on a pool needs the work
-- done by the worker that took the file, and a caller of any kind needs the
-- document dropped rather than retained under an unevaluated cell
-- (docs\/invariants.md, Scan).
loadFile :: FilePath -> IO (Either LoadFailure [HeadlineRecord])
loadFile path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  evaluate $ case raw of
    Left _err -> Left ReadFailed
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _err -> Left DecodeFailed
      Right doc -> case orgParse defaultContext doc of
        (_elems, _ctx, Just _err) -> Left ParseFailed
        (elems, ctx, Nothing)     -> forcing rs (Right rs)
          -- The digest is of the very bytes these spans were computed against,
          -- taken here rather than by a later read: a write pinned to a digest
          -- read at some other moment would splice offsets into a document
          -- they were never measured in.
          where rs = recordsOf path doc (Edit.digestOf bytes) ctx elems

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
recordsOf :: FilePath -> Text -> Text -> Context -> [Spanned Element] -> [HeadlineRecord]
recordsOf path doc digest ctx elems =
  zipWith (recordOf path doc digest category keywords) heads
          (subtreeSpans (T.length doc) heads)
  where category = detach (metaCategory ctx)
        keywords = keywordsOf ctx
        heads    = [ h | e <- elems, EHeadline h <- [valueOf e] ]

recordOf :: FilePath -> Text -> Text -> Text -> TodoKeywords -> Headline -> Span
         -> HeadlineRecord
recordOf path doc digest category keywords h subtree = forceRecord HeadlineRecord
  { hrFile      = path
  , hrId        = rowId path h
  , hrCategory  = category
  , hrHeadline  = h
  , hrKeywords  = keywords
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
-- __Experimental__: reached by @\/headlines?order=document@ alone, paired with
-- the row @depth@ 'rowJSON' emits.  Nothing else in the wire contract turns on
-- it, and a renderer is free to ignore both.
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
-- The properties drawer taken out of the subtree and put back into it.  One
-- rule holds the pair together: every byte of a subtree has ONE owner.  The
-- drawer's lines are the properties', everything else is the body's, and a
-- property nobody edited goes back as the very line it came in on — odd
-- spacing, odd casing and all.  So a client can edit the two halves apart
-- without the half it did not touch being re-spelled underneath it.
--
-- A headline's OWN drawer only.  A child's drawer is body text here, since it
-- belongs to the child's headline and this lens is over one.

-- | A subtree split in two: its text without the headline's own property
-- drawer, and that drawer's pairs in file order.  With no drawer the body is
-- the whole subtree and the pairs are empty, which is the shape
-- 'recomposedSubtree' reads back as "leave it without one".
data HeadlineParts = HeadlineParts
  { hpBody       :: !Text            -- ^ the subtree, the headline's own drawer lines gone.
  , hpProperties :: ![(Text, Text)]  -- ^ the drawer's keys and values, in file order.
  } deriving (Eq, Show)

-- | R's subtree split into 'HeadlineParts'.
--
-- The cut is by WHOLE LINES: the drawer span starts at the beginning of the
-- @:PROPERTIES:@ line and ends right after @:END:@, so the newline closing that
-- line goes with it and the body is left as the lines that were around it,
-- byte for byte.  Anything trailing @:END:@ on its own line belongs to the
-- drawer too — it is on a drawer line.
headlineParts :: HeadlineRecord -> HeadlineParts
headlineParts r = case drawerSlice r subtree of
  Nothing -> HeadlineParts subtree []
  Just sp -> HeadlineParts (T.take (spanStart sp) subtree <> T.drop (spanEnd sp) subtree)
                           [ (key, value)
                           | (key, value, _raw) <- drawerRows (sliceSpan subtree sp) ]
  where subtree = subtreeText r

-- | BODY carrying PROPS, as the subtree R's file would then hold.
--
-- The drawer goes back on the line it was cut from — counted in lines from the
-- start of the subtree, so an edit further down the body cannot move it — and a
-- headline that never had one gets it where org puts it: after the title line,
-- or after the planning line when there is one.  An empty PROPS writes no
-- drawer at all, which is how a client deletes one.
--
-- What each property costs is decided by whether it moved.  A pair the drawer
-- already held is written as the line it already was ('drawerRows' keeps them,
-- one per pair, consumed in order so two spellings of one pair both survive);
-- anything else is rendered @:KEY: value@ under the indentation the drawer's own
-- lines carry.  The @:PROPERTIES:@ and @:END:@ lines are likewise the file's own
-- where there were any.
recomposedSubtree :: HeadlineRecord -> Text -> [(Text, Text)] -> Text
recomposedSubtree _r body [] = body
recomposedSubtree r body props = spliceLine (drawerLine r subtree) block body
  where subtree = subtreeText r
        block = drawerText (drawerStyle r subtree body) props

-- | Where R's own drawer sits in SUBTREE, as a span of SUBTREE's own offsets
-- covering whole lines.  'Nothing' when the headline carries no drawer — and,
-- defensively, when the span is not inside the subtree, which the span
-- invariants forbid.
drawerSlice :: HeadlineRecord -> Text -> Maybe Span
drawerSlice r subtree = do
  sp <- hsProperties (spans (hrHeadline r))
  let from = spanStart sp - spanStart (hrSubtree r)
      to   = spanEnd sp - spanStart (hrSubtree r)
  if from < 0 || to > T.length subtree || from > to then Nothing
    else Just (Span from (pastLine subtree to))

-- | Which line of SUBTREE R's drawer starts on, counted from zero.  With no
-- drawer it is where org would write one: the line after the title line, or the
-- one after that when the headline carries a planning line — which is the one
-- line that can sit between them.
drawerLine :: HeadlineRecord -> Text -> Int
drawerLine r subtree = case drawerSlice r subtree of
  Just sp -> T.count "\n" (T.take (spanStart sp) subtree)
  Nothing | planned   -> 2
          | otherwise -> 1
  where hs = headlineSpans r
        planned = any isJust [hsSchedule hs, hsDeadline hs, hsClosed hs]

-- | How R's drawer is spelled, so a rewritten one reads like the file it goes
-- back into.  BODY supplies the line ending for a headline that has no drawer
-- to copy one from.
data DrawerStyle = DrawerStyle
  { dsOpen   :: !Text                    -- ^ the @:PROPERTIES:@ line, terminator and all.
  , dsClose  :: !Text                    -- ^ the @:END:@ line, which ends the block.
  , dsIndent :: !Text                    -- ^ what a rendered line is indented by.
  , dsEol    :: !Text                    -- ^ what a rendered line ends with.
  , dsRaw    :: ![((Text, Text), Text)]  -- ^ each pair already there, and its line.
  }

-- | How R's drawer in SUBTREE is spelled, BODY standing in for the parts a
-- headline with no drawer has nothing to copy.
drawerStyle :: HeadlineRecord -> Text -> Text -> DrawerStyle
drawerStyle r subtree body = case drawerSlice r subtree of
  Nothing -> DrawerStyle (":PROPERTIES:" <> eol) (":END:" <> eol) "" eol []
    where eol = eolOf body
  Just sp -> DrawerStyle open close (indentOf (firstOr open [ raw | (_k, _v, raw) <- rows ]))
                         (eolOf close) [ ((key, value), raw) | (key, value, raw) <- rows ]
    where block = sliceSpan subtree sp
          ls    = linesWith block
          open  = firstOr (":PROPERTIES:" <> eolOf body) ls
          close = firstOr (":END:" <> eolOf body) (reverse ls)
          rows  = drawerRows block

-- | STYLE's drawer holding PROPS: the opening line, a line per property, the
-- closing line.
drawerText :: DrawerStyle -> [(Text, Text)] -> Text
drawerText style props = T.concat (dsOpen style : go (dsRaw style) props <> [dsClose style])
  where
    go _raws []       = []
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

-- | BODY with BLOCK put back as its AT'th line, counting from zero.  A body
-- with fewer lines than that takes it at the end, which is where a client that
-- deleted the lines above the drawer has left room for it.  BLOCK is terminated
-- only where something follows it, so a drawer that closed an unterminated file
-- still closes one.
spliceLine :: Int -> Text -> Text -> Text
spliceLine at block body = terminated (T.concat before) <> rest
  where (before, after) = splitAt at (linesWith body)
        rest | null after = block
             | otherwise  = terminated block <> T.concat after
        terminated t | T.null t || "\n" `T.isSuffixOf` t = t
                     | otherwise                         = t <> "\n"

-- | T split into lines, each carrying the newline that ends it.  The last line
-- carries one only where T does, so @T.concat . linesWith@ is @id@.
linesWith :: Text -> [Text]
linesWith t
  | T.null t  = []
  | otherwise = case T.breakOn "\n" t of
      (line, rest) | T.null rest -> [line]
                   | otherwise   -> (line <> "\n") : linesWith (T.drop 1 rest)

-- | The offset in T past the newline ending the line offset AT sits on, or T's
-- length when that line has none.
pastLine :: Text -> Int -> Int
pastLine t at = maybe (T.length t) (\i -> at + i + 1) (T.findIndex (== '\n') (T.drop at t))

-- | The line ending T's first line uses, @"\\n"@ when it has none.
eolOf :: Text -> Text
eolOf t = case T.breakOn "\n" t of
  (before, rest) | not (T.null rest), "\r" `T.isSuffixOf` before -> "\r\n"
  _plain                                                         -> "\n"

-- | The horizontal space LINE opens with.
indentOf :: Text -> Text
indentOf = T.takeWhile (\c -> c == ' ' || c == '\t')

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
-- shared by every row that file contributes.
keywordsOf :: Context -> TodoKeywords
keywordsOf ctx = forcing (actives <> inactives) (TodoKeywords actives inactives)
  where actives   = kept todoActive
        inactives = kept todoInactive
        kept f = map detach (Set.toAscList (f ctx))

-- | H's row identity: its @ORG_GLANCE_ID@ property, else @"FILE:START"@ — the
-- path and the character offset 'hsFull' starts at.  The fallback is stable
-- only while the bytes ahead of the headline stay put, so an edit above it
-- renames the row; S5's file watch is where that gets revisited.
rowId :: FilePath -> Headline -> Text
rowId path h = maybe fallback detach (identity h)
  where fallback = T.pack path <> ":" <> T.pack (show (spanStart (hsFull (spans h))))

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
  forcing (hrId r : hrCategory r : hrTitle r : hrTags r : hrDigest r : hrSearch r : optional) r
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
-- KEYWORD is refused unless R's own file declares it ('hrKeywords').  Legality
-- is per file because @#+TODO:@ is: a word is a keyword in one document and the
-- first word of a title in the next, and writing one a file does not declare
-- makes a headline the parser will read differently than the writer meant.  The
-- state column's group meta-values (@*active*@, @*inactive*@) are in no keyword
-- set, so they are refused here like any other word that is not one.
setStateEdits :: Maybe Text -> HeadlineRecord -> Either Text [(Span, Text)]
setStateEdits Nothing r = Right [ (Span (spanStart sp) (spanEnd sp + trailing sp), "")
                                | Just sp <- [hsTodo (headlineSpans r)] ]
  where trailing sp = T.length (T.takeWhile horizontal (T.drop (spanEnd sp) (hrDoc r)))
        horizontal c = c == ' ' || c == '\t'
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
  | otherwise            = [ (insertAt titleLineEnd, " :" <> archiveTag <> ":") ]
  where
    hs = headlineSpans r
    titleLineEnd = foldl' max (spanEnd (hsStars hs))
                     [ spanEnd sp | Just sp <- [hsTodo hs, hsPriority hs, hsTitle hs] ]

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
  , "depth" .= depthOf r
  , "cells" .= object [ Key.fromText key .= cell r | (key, _header, _kind, cell) <- viewColumns ]
  ]

-- | R's outline depth, counted from zero: a top-level headline is 0 and each
-- star past the first is one level down.  'Data.Org.Indent' counts the stars
-- themselves, so the wire is one less than the file spells — SCHEMA.md's
-- @depth@ is 0-based, and a renderer indenting by it wants the root flush left.
--
-- __Experimental__: SCHEMA.md marks the field a renderer hint, so it rides on
-- every row and a renderer that draws no tree ignores it.  Every other field of
-- the row is a cell; this one is about the row's place among the others, which
-- is why it sits beside @id@ rather than among them, and why no column, filter
-- key or search field answers to it.
depthOf :: HeadlineRecord -> Int
depthOf r = case indent (hrHeadline r) of Indent n -> max 0 (n - 1)

-- | The state palette: every TODO keyword the loaded files declared, actives
-- ahead of the done-like ones.  Palette order is also sort priority
-- (SCHEMA.md), so a sort on the state column puts work before its aftermath.
badges :: TodoKeywords -> [Value]
badges (TodoKeywords actives inactives) =
  zipWith badge (cycled activeColors actives) actives
    <> zipWith badge (cycled inactiveColors inactives) inactives
  where cycled hues ks = take (length ks) (cycle hues)
        badge color value = object [ "value" .= value, "color" .= color ]

-- | The keyword sets of several files as one palette: first-seen order across
-- the list, a keyword declared both ways anywhere counting as active.  This is
-- the only thing a view's columns vary on, so a caller watching for a column
-- change watches this value.  Deduplication makes runs irrelevant: passing one
-- record per file gives the same answer as passing every record.
mergeKeywords :: [TodoKeywords] -> TodoKeywords
mergeKeywords keywords = TodoKeywords actives inactives
  where actives   = declared tkActive
        inactives = filter (`notElem` actives) (declared tkInactive)
        declared f = firstSeen (concatMap f keywords)

-- | XS deduplicated, each element kept where it first appeared.
-- 'Data.List.nub' reads the same and costs O(n · distinct); this merge runs
-- over one entry per file on every @\/headlines@ request, and at 6300 files
-- that quadratic was most of the request.
firstSeen :: Ord a => [a] -> [a]
firstSeen = go Set.empty
  where go _ [] = []
        go seen (x : xs) | Set.member x seen = go seen xs
                         | otherwise         = x : go (Set.insert x seen) xs

-- | Warm hues for keywords that still want work.
activeColors :: [Text]
activeColors = ["#e0af68", "#ff9e64", "#f7768e", "#ffc777"]

-- | Cool hues for the done-like ones.
inactiveColors :: [Text]
inactiveColors = ["#9ece6a", "#73daca", "#41a6b5", "#565f89"]
