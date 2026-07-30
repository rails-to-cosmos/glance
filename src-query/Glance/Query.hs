-- | The query facade: load org files into rows and render them as a
-- table-view JSON document.  This is the whole public surface of the package;
-- the parser and its AST live in a private sublibrary, so a daemon or web
-- target linking against @glance@ cannot reach them.
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
-- copy it, and leave the cells where they are.
module Glance.Query ( HeadlineRecord (..)
                    , QueryResult (..)
                    , TodoKeywords (..)
                    , loadDir
                    , viewJSON
                    , viewJSONText
                    ) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (foldM)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Pair)
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
                , HeadlineSpans (hsFull, hsTags, hsTitle), Priority (Priority)
                , Spanned (valueOf), Timestamp (tsStart), Todo (name)
                , TsMoment (tsmHasTime, tsmTime), deadline, defaultContext
                , identity, metaCategory, orgParse, priority, schedule
                , sliceSpan, spanStart, spans, tags, title, todo, todoActive
                , todoInactive )
import Data.Org.Walk (Found (..), findOrgFiles)

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

emptyResult :: QueryResult
emptyResult = QueryResult [] 0 0 0 0

-- Loading

-- | Every headline under DIR, one record each.  Walks @*.org@ recursively,
-- reads each file strictly and parses it from 'defaultContext' — per-file
-- context is an invariant: keywords declared in one file never reach another.
loadDir :: FilePath -> IO QueryResult
loadDir dir = do
  found <- findOrgFiles [dir]
  loaded <- foldM loadFile (emptyResult { qrReadFailures = length (foundDirErrs found) })
                           (sort (foundFiles found))
  pure loaded { qrRecords = reverse (qrRecords loaded) }

-- | Add PATH's headlines to ACC, or count the way it failed.
loadFile :: QueryResult -> FilePath -> IO QueryResult
loadFile acc path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  evaluate $ case raw of
    Left _err -> seen { qrReadFailures = qrReadFailures seen + 1 }
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _err -> seen { qrDecodeFailures = qrDecodeFailures seen + 1 }
      Right doc -> case orgParse defaultContext doc of
        (_elems, _ctx, Just _err) -> seen { qrParseFailures = qrParseFailures seen + 1 }
        (elems, ctx, Nothing)     -> foldl' keep seen (recordsOf path doc ctx elems)
  where seen = acc { qrFiles = qrFiles acc + 1 }
        keep res r = r `seq` res { qrRecords = r : qrRecords res }

-- | The rows FILE contributes, cells cut out of DOC, categorised by CTX — the
-- context the file parsed to, so a @#+CATEGORY@ anywhere in it labels the
-- whole file.
recordsOf :: FilePath -> Text -> Context -> [Spanned Element] -> [HeadlineRecord]
recordsOf path doc ctx elems =
  [ recordOf path doc category keywords h | e <- elems, EHeadline h <- [valueOf e] ]
  where category = detach (metaCategory ctx)
        keywords = keywordsOf ctx

recordOf :: FilePath -> Text -> Text -> TodoKeywords -> Headline -> HeadlineRecord
recordOf path doc category keywords h = forceRecord HeadlineRecord
  { hrFile      = path
  , hrId        = rowId path h
  , hrCategory  = category
  , hrHeadline  = h
  , hrKeywords  = keywords
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

-- | Force every text in TS, then yield X.  A strict field forces a 'Maybe' to
-- its constructor only, and a cell left as a thunk retains the file it would
-- have been cut from.
forcing :: [Text] -> a -> a
forcing ts x = foldr seq x ts

-- | R with every cell evaluated, so the record can outlive its document.
forceRecord :: HeadlineRecord -> HeadlineRecord
forceRecord r = forcing (hrId r : hrCategory r : hrTitle r : hrTags r : optional) r
  where optional = catMaybes [hrState r, hrPriority r, hrScheduled r, hrDeadline r]

-- View JSON

-- | The table-view document for RECORDS under TITLE, per
-- @table-view/SCHEMA.md@.  No actions: the view is read-only until the server
-- owns the commands that go with them (M3).
viewJSON :: Text -> [HeadlineRecord] -> Value
viewJSON viewTitle records = object
  [ "title"   .= viewTitle
  , "columns" .= columns records
  , "sort"    .= object [ "column" .= ("scheduled" :: Text), "ascending" .= True ]
  , "rows"    .= map rowJSON records
  ]

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
-- Order within a group is first-seen across the walk; a keyword declared both
-- ways somewhere counts as active.
badges :: [HeadlineRecord] -> [Value]
badges records = zipWith badge (cycled activeColors actives) actives
             <> zipWith badge (cycled inactiveColors inactives) inactives
  where actives   = declared tkActive
        inactives = filter (`notElem` actives) (declared tkInactive)
        declared f = nub (concatMap (f . hrKeywords) records)
        cycled palette ks = take (length ks) (cycle palette)
        badge color value = object [ "value" .= value, "color" .= color ]

-- | Warm hues for keywords that still want work.
activeColors :: [Text]
activeColors = ["#e0af68", "#ff9e64", "#f7768e", "#ffc777"]

-- | Cool hues for the done-like ones.
inactiveColors :: [Text]
inactiveColors = ["#9ece6a", "#73daca", "#41a6b5", "#565f89"]
