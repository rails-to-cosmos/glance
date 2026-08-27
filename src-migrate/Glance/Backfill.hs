-- | The @glance backfill-created@ migration: stamp 'captureProperty'
-- (@ORG_GLANCE_CREATION_TIME@) on every headline that LACKS it, from the best
-- evidence available and HONEST about precision.  The ENGINE lives here and is
-- pure where it can be ('planDocument', 'earliestLogbookStamp'), so the evidence
-- tiers are testable without the process; @app/Main.hs@ holds only the thin arm.
--
-- EVIDENCE TIERS, in order: a headline already stamped is kept; else the earliest
-- @:LOGBOOK:@ inactive timestamp in its subtree; else the file's mtime; else the
-- run's own day, the last resort.  The report NAMES how many rows fell to each
-- tier, so a wall of run-day stamps is never mistaken for real creation times.
--
-- Every write leaves through 'replaceSpans' — the drift lock and the
-- @EXTERNAL.jsonl@ note org-glance adopts on — one atomic write per file.
module Glance.Backfill
  ( BackfillOptions (..)
  , Tier (..)
  , FilePlan (..)
  , FileResult (..)
  , WriteState (..)
  , Report (..)
  , emptyReport
  , planDocument
  , earliestLogbookStamp
  , backfillFile
  , backfillRoots
  , reportLines
  , runBackfill
  ) where

import Control.Exception (IOException, try)
import Data.Char (isDigit)
import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified TextShow as TS

import System.Directory (getModificationTime)

import Data.Org (Context, Element, Spanned, getProperty, properties, sliceSpan, spans)
import Data.Org.Config (loadConfigDirs, seedContext)
import Data.Org.Edit (ParsedDocument (..), eolOf, readParsed)
import Data.Org.Walk (Found (..), WalkOptions, findOrgFilesWith)

import Glance.Query ( Span, captureProperty, captureStamp
                    , writeRefusalText
                    , drawerInsertEdit, outlineEntries, replaceSpans )


data BackfillOptions = BackfillOptions
  { boWalk   :: !WalkOptions  -- ^ the walk 'doctor' and the store share.
  , boDryRun :: !Bool         -- ^ compute and report, write nothing.
  }

-- | Where a written stamp came from.  'Present' is a headline the walk found
-- already carrying one; the other three are the evidence a fresh stamp rests on.
data Tier = Present | Logbook | Mtime | RunDay
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A document's plan: the drawer edits to splice, and how many headlines fell
-- to each tier ('Present' among them, so the counts total the file's headlines).
data FilePlan = FilePlan
  { fpEdits :: ![(Span, Text)]
  , fpTiers :: !(Map Tier Int)
  } deriving (Eq, Show)

-- | DOC as a plan: for each headline lacking 'captureProperty', the one drawer
-- edit adding the earliest LOGBOOK stamp its subtree records, or FALLBACKSTAMP
-- (of provenance FALLBACKTIER — 'Mtime' or 'RunDay') where the subtree is
-- silent.  A stamped headline adds no edit and counts under 'Present'.
planDocument :: Tier -> Text -> Text -> [Spanned Element] -> FilePlan
planDocument fallbackTier fallbackStamp doc elems = FilePlan (reverse edits) counts
  where
    eol = eolOf doc
    (edits, counts) = foldl' step ([], Map.empty) (outlineEntries doc elems)
    step (es, cs) (h, sub) = case getProperty captureProperty (properties h) of
      Just _  -> (es, bump Present cs)
      Nothing -> let (tier, stamp) = evidence sub
                     e = drawerInsertEdit doc eol [(captureProperty, stamp)] (spans h)
                 in (e : es, bump tier cs)
    evidence sub = case earliestLogbookStamp (sliceSpan doc sub) of
      Just s  -> (Logbook, s)
      Nothing -> (fallbackTier, fallbackStamp)
    bump t = Map.insertWith (+) t 1

-- | The chronologically EARLIEST inactive timestamp inside any @:LOGBOOK:@
-- drawer in SUBTREE, as its own @[YYYY-MM-DD …]@ text; 'Nothing' when the
-- subtree logs nothing.  The date-led form sorts chronologically as a string, so
-- @minimum@ is the earliest.
earliestLogbookStamp :: Text -> Maybe Text
earliestLogbookStamp subtree = case concatMap inactiveStamps (logbookLines subtree) of
  []     -> Nothing
  stamps -> Just (minimum stamps)

-- | The lines inside SUBTREE's @:LOGBOOK:@ drawers, the delimiters dropped.  A
-- state machine, so a nested child's logbook is read the same as an own one.
logbookLines :: Text -> [Text]
logbookLines = go False . T.lines
  where
    go _ [] = []
    go inside (l : ls)
      | opensLog l             = go True ls
      | inside, closesDrawer l = go False ls
      | inside                 = l : go inside ls
      | otherwise              = go inside ls
    opensLog l     = T.toUpper (T.strip l) == ":LOGBOOK:"
    closesDrawer l = T.toUpper (T.strip l) == ":END:"

-- | Every @[YYYY-MM-DD …]@ inactive timestamp in LINE, in order.  A @[@ opening
-- a run that does not begin with a digit is no stamp and the scan steps past it.
inactiveStamps :: Text -> [Text]
inactiveStamps = go
  where
    go t = case T.breakOn "[" t of
      (_, rest) | T.null rest -> []
                | otherwise   -> stamp (T.drop 1 rest)
    stamp afterOpen = case T.breakOn "]" afterOpen of
      (inner, close)
        | not (T.null close), dateLed inner -> ("[" <> inner <> "]") : go (T.drop 1 close)
        | otherwise                         -> go afterOpen
    dateLed inner = maybe False (isDigit . fst) (T.uncons inner)


-- | What became of one file's write.
data WriteState
  = Wrote            -- ^ the splice landed.
  | DryRun           -- ^ edits computed, held back by @--dry-run@.
  | NoChange         -- ^ every headline already stamped; nothing to write.
  | Refused !Text    -- ^ a drift or write trouble, spelled for the report.
  | LoadFailed !Text -- ^ the file could not be read, decoded or parsed.
  deriving (Eq, Show)

data FileResult = FileResult
  { frPath  :: !FilePath
  , frTiers :: !(Map Tier Int)
  , frWrite :: !WriteState
  } deriving (Eq, Show)

-- | Backfill PATH under SEED, NOW the run's own clock.  A read, decode or parse
-- failure and a drift are each a REFUSAL the report carries; a file already fully
-- stamped writes nothing.  The fallback tier is the FILE's — its mtime, or NOW
-- when the mtime cannot be read — so a file's silent headlines share one stamp.
backfillFile :: Bool -> Context -> Time.ZonedTime -> FilePath -> IO FileResult
backfillFile dry seed now path = do
  parsed <- readParsed seed path
  case parsed of
    Left (_fault, why) -> pure (FileResult path Map.empty (LoadFailed why))
    Right pd -> do
      (fallbackTier, fallbackStamp) <- fallback
      let plan = planDocument fallbackTier fallbackStamp (pdText pd) (pdElements pd)
          result st = FileResult path (fpTiers plan) st
      if null (fpEdits plan) then pure (result NoChange)
      else if dry then pure (result DryRun)
      else do
        written <- replaceSpans path (pdDigest pd) (fpEdits plan)
        pure $ case written of
          Right _ -> result Wrote
          Left wf -> result (Refused (writeRefusalText path wf))
  where
    fallback = do
      mt <- try (getModificationTime path) :: IO (Either IOException Time.UTCTime)
      case mt of
        Right t -> (,) Mtime . captureStamp <$> Time.utcToLocalZonedTime t
        Left _  -> pure (RunDay, captureStamp now)

-- | The migration's tally: files walked, headlines seen, the tier counts, files
-- written and the refusals.
data Report = Report
  { rFiles     :: !Int
  , rHeadlines :: !Int
  , rTiers     :: !(Map Tier Int)
  , rWritten   :: !Int
  , rRefusals  :: ![(FilePath, Text)]
  } deriving (Eq, Show)

emptyReport :: Report
emptyReport = Report 0 0 Map.empty 0 []

tally :: Report -> FileResult -> Report
tally rep fr = rep
  { rHeadlines = rHeadlines rep + sum (Map.elems (frTiers fr))
  , rTiers     = Map.unionWith (+) (rTiers rep) (frTiers fr)
  , rWritten   = rWritten rep + wrote
  , rRefusals  = rRefusals rep <> refusal
  }
  where
    wrote = case frWrite fr of { Wrote -> 1; _ -> 0 }
    refusal = case frWrite fr of
      Refused why    -> [(frPath fr, why)]
      LoadFailed why -> [(frPath fr, why)]
      _              -> []

-- | Walk ROOTS as OPTS asks and backfill every @.org@ file the walk sources —
-- blobs and inbox, config and derived excluded, the walk 'doctor' shares.  ONE
-- CLOCK READ, before any file, so a run spanning midnight stamps one day.
backfillRoots :: BackfillOptions -> [FilePath] -> IO Report
backfillRoots opts roots = do
  found  <- findOrgFilesWith (boWalk opts) roots
  config <- loadConfigDirs (foundConfig found)
  now    <- Time.getZonedTime
  let paths = sort (foundFiles found)
  results <- mapM (backfillFile (boDryRun opts) (seedContext config) now) paths
  pure (foldl' tally emptyReport { rFiles = length paths } results)

-- | REPORT as the lines stdout shows, doctor's own shape.  DRY says whether the
-- writes were held back, and the tier rows keep the approximations visible.
reportLines :: [FilePath] -> Bool -> Report -> [Text]
reportLines roots dry rep =
  ("backfill-created " <> T.intercalate " " (map T.pack roots) <> dryTag)
    : map (uncurry row)
        [ ("files",           rFiles rep)
        , ("headlines",       rHeadlines rep)
        , ("already stamped", tier Present)
        , ("logbook",         tier Logbook)
        , ("mtime",           tier Mtime)
        , ("run day",         tier RunDay)
        , ("files written",   rWritten rep)
        , ("refused",         length (rRefusals rep)) ]
    <> refusalSection
  where
    dryTag = if dry then "  (dry run: nothing written)" else ""
    tier t = Map.findWithDefault 0 t (rTiers rep)
    row label n = "  " <> T.justifyLeft 16 ' ' label <> T.justifyRight 10 ' ' (TS.showt n)
    refusalSection
      | null (rRefusals rep) = []
      | otherwise = "" : "refused:"
          : [ "  " <> T.pack p <> ": " <> why | (p, why) <- rRefusals rep ]

-- | The whole run: walk, backfill and print.  The CLI arm is this call.
runBackfill :: BackfillOptions -> [FilePath] -> IO ()
runBackfill opts roots = do
  rep <- backfillRoots opts roots
  mapM_ TIO.putStrLn (reportLines roots (boDryRun opts) rep)
