-- | The query facade: load org files into rows, render them as a table-view
-- JSON document, and write one headline's raw subtree back.  This is the whole
-- public surface of the package; the parser and its AST live in a private
-- sublibrary, so a daemon or web target linking against @glance@ cannot reach
-- them.
--
-- The write path is the read path run backwards.  A record carries the extent
-- of its subtree ('hrSubtree') in the text it was parsed from ('hrDoc') and
-- the digest of that text ('hrDigest'); 'replaceSpan' splices new text over
-- that extent and refuses unless the file still digests to the pinned value.
-- So a client materializes what the load model holds, and a file that moved
-- underneath it costs a refusal rather than a corrupted splice.
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
module Glance.Query ( HeadlineRecord (..)
                    , LoadFailure (..)
                    , QueryResult (..)
                    , Span (..)
                    , TodoKeywords (..)
                    , WriteFailure (..)
                    , loadDir
                    , loadDirFiles
                    , loadFile
                    , mergeKeywords
                    , replaceSpan
                    , rowJSON
                    , subtreeText
                    , viewJSON
                    , viewJSONText
                    ) where

import Control.Exception (IOException, evaluate, try)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Pair)
import Data.Either (fromRight)
import Data.List (foldl', nub, sort)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import TextShow (showt)

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time

import Data.Org ( Context, Element (EHeadline), Headline
                , HeadlineSpans (hsFull, hsTags, hsTitle), Indent (Indent)
                , Priority (Priority), Span (..), Spanned (valueOf)
                , Timestamp (tsStart), Todo (name)
                , TsMoment (tsmHasTime, tsmTime), deadline, defaultContext
                , identity, indent, metaCategory, orgParse, priority, schedule
                , sliceSpan, spans, tags, title, todo, todoActive
                , todoInactive )
import Data.Org.Walk (Found (..), findOrgFiles)

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
  { qrRecords        :: ![HeadlineRecord]  -- ^ rows in walk order: paths sorted, headlines in file order.
  , qrFiles          :: !Int               -- ^ .org files visited.
  , qrParseFailures  :: !Int               -- ^ files 'orgParse' rejected; they contribute no rows.
  , qrDecodeFailures :: !Int               -- ^ files that are not valid UTF-8.
  , qrReadFailures   :: !Int               -- ^ files that could not be read, plus unlistable directories.
  } deriving (Show)

-- | Why one file yielded no rows.  A load reports these as counts; a watcher
-- reports them per file, and decides what to keep on the strength of which one
-- it got.
data LoadFailure
  = ReadFailed    -- ^ the bytes could not be read.
  | DecodeFailed  -- ^ the bytes are not valid UTF-8.
  | ParseFailed   -- ^ 'orgParse' rejected the document, which is all-or-nothing.
  deriving (Eq, Show)

emptyResult :: QueryResult
emptyResult = QueryResult [] 0 0 0 0

-- Loading

-- | Every headline under DIR, one record each.  Walks @*.org@ recursively,
-- reads each file strictly and parses it from 'defaultContext' — per-file
-- context is an invariant: keywords declared in one file never reach another.
loadDir :: FilePath -> IO QueryResult
loadDir dir = do
  (files, dirErrs) <- loadDirFiles dir
  pure (summarise dirErrs files)

-- | DIR loaded one file at a time: every @*.org@ path in walk order with its
-- rows or its failure, plus the number of directories the walk could not list
-- (those count as read failures too, and have no path of their own to report).
-- The per-file breakdown 'loadDir' folds away is what a watcher needs to
-- re-load a single file into a store built the same way.
loadDirFiles :: FilePath -> IO ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
loadDirFiles dir = do
  found <- findOrgFiles [dir]
  files <- mapM withOutcome (sort (foundFiles found))
  pure (files, length (foundDirErrs found))
  where withOutcome path = (,) path <$> loadFile path

-- | PATH's headlines, or why it has none.  Reads the file strictly and parses
-- it from 'defaultContext': a file's own @#+TODO:@ lines are the only ones that
-- reach its headlines, whether it is loaded with a directory or on its own
-- after an edit.
loadFile :: FilePath -> IO (Either LoadFailure [HeadlineRecord])
loadFile path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  evaluate $ case raw of
    Left _err -> Left ReadFailed
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _err -> Left DecodeFailed
      Right doc -> case orgParse defaultContext doc of
        (_elems, _ctx, Just _err) -> Left ParseFailed
        (elems, ctx, Nothing)     -> Right (forcing rs rs)
          -- The digest is of the very bytes these spans were computed against,
          -- taken here rather than by a later read: a write pinned to a digest
          -- read at some other moment would splice offsets into a document
          -- they were never measured in.
          where rs = recordsOf path doc (Edit.digestOf bytes) ctx elems

-- | FILES folded into one result, with DIRERRS unlistable directories already
-- counted as read failures.
summarise :: Int -> [(FilePath, Either LoadFailure [HeadlineRecord])] -> QueryResult
summarise dirErrs files =
  (foldl' count (emptyResult { qrReadFailures = dirErrs }) files) { qrRecords = forcing rows rows }
  where
    rows = concatMap (fromRight [] . snd) files
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
  , hrState     = detach . name <$> todo h
  , hrPriority  = (\(Priority c) -> T.singleton c) <$> priority h
  , hrTitle     = cut (hsTitle sp) (showt (title h))
  , hrTags      = cut (hsTags sp) (showt (tags h))
  , hrScheduled = isoStamp <$> schedule h
  , hrDeadline  = isoStamp <$> deadline h
  }
  where sp = spans h
        -- The span is the lossless channel; the render is what is left when a
        -- headline carries no span for a component, which is to say when the
        -- component is empty.
        cut mspan render = detach (maybe render (sliceSpan doc) mspan)

-- Subtrees

-- | R's subtree as its file spells it: stars, planning, drawer, body and every
-- child, raw.  A slice, so it shares the document rather than copying it — the
-- caller encodes it into a response and drops it.
subtreeText :: HeadlineRecord -> Text
subtreeText r = sliceSpan (hrDoc r) (hrSubtree r)

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
  forcing (hrId r : hrCategory r : hrTitle r : hrTags r : hrDigest r : optional) r
  where optional = catMaybes [hrState r, hrPriority r, hrScheduled r, hrDeadline r]

-- Write-back

-- | Why a 'replaceSpan' did not land.  Either way the file is byte-identical
-- to what it held before the call (docs/invariants.md, Architecture).
data WriteFailure
  = WriteDrift !Text    -- ^ the digest the file holds now, which is not the pinned one.
  | WriteRefused !Text  -- ^ read, decode, splice or rename trouble, spelled for a caller to show.
  deriving (Eq, Show)

-- | Replace SP of FILE with NEW, provided FILE still digests to DIGEST; the
-- file's new digest comes back, so a caller chains an edit without re-reading.
--
-- The lock is the point.  DIGEST is the one a record was loaded with
-- ('hrDigest'), and SP indexes that same text, so either the file is still the
-- document the offsets were measured in or nothing is written — a browser and
-- an editor writing the same file cannot silently splice over each other.
-- The write itself is 'Data.Org.Edit.editFile': one span, atomic replace.
replaceSpan :: FilePath -> Text -> Span -> Text -> IO (Either WriteFailure Text)
replaceSpan path digest sp new =
  report <$> Edit.editFile (Edit.Snapshot path digest) [Edit.Edit sp new]
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

-- View JSON

-- | The table-view document for RECORDS under TITLE, per
-- @table-view/SCHEMA.md@.
viewJSON :: Text -> [HeadlineRecord] -> Value
viewJSON viewTitle records = object
  [ "title"   .= viewTitle
  , "columns" .= columns records
  , "actions" .= actions
  , "sort"    .= object [ "column" .= ("scheduled" :: Text), "ascending" .= True ]
  , "rows"    .= map rowJSON records
  ]

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

-- | 'viewJSON' encoded, ready to hand a renderer.
viewJSONText :: Text -> [HeadlineRecord] -> TL.Text
viewJSONText viewTitle = encodeToLazyText . viewJSON viewTitle

columns :: [HeadlineRecord] -> [Value]
columns records =
  [ column "state"     "State"     "badge" [ "sortable" .= True
                                           , "badges" .= badges records ]
  , column "priority"  "Pri"       "text"  [ "sortable" .= True
                                           , "values" .= (["A", "B", "C"] :: [Text]) ]
  , column "title"     "Headline"  "text"  []
  , column "tags"      "Tags"      "text"  []
  , column "scheduled" "Scheduled" "text"  [ "sortable" .= True ]
  , column "deadline"  "Deadline"  "text"  [ "sortable" .= True ]
  ]

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
  , "cells" .= object [ "state"     .= hrState r
                      , "priority"  .= hrPriority r
                      , "title"     .= hrTitle r
                      , "tags"      .= hrTags r
                      , "scheduled" .= hrScheduled r
                      , "deadline"  .= hrDeadline r
                      ]
  ]

-- | The state palette: every TODO keyword the loaded files declared, actives
-- ahead of the done-like ones.  Palette order is also sort priority
-- (SCHEMA.md), so a sort on the state column puts work before its aftermath.
badges :: [HeadlineRecord] -> [Value]
badges records = zipWith badge (cycled activeColors actives) actives
             <> zipWith badge (cycled inactiveColors inactives) inactives
  where TodoKeywords actives inactives = mergeKeywords (map hrKeywords records)
        cycled palette ks = take (length ks) (cycle palette)
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
        declared f = nub (concatMap f keywords)

-- | Warm hues for keywords that still want work.
activeColors :: [Text]
activeColors = ["#e0af68", "#ff9e64", "#f7768e", "#ffc777"]

-- | Cool hues for the done-like ones.
inactiveColors :: [Text]
inactiveColors = ["#9ece6a", "#73daca", "#41a6b5", "#565f89"]
