-- | Corpus scan: parse every .org file under a set of roots and report parse coverage, span-invariant violations, and how far each org-glance store's index has drifted from the blobs it indexes.
module Scan (runScan) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM)
import Data.List (foldl', isPrefixOf, mapAccumL, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified TextShow as TS

import Data.Org
import Data.Org.Config ( ConfigLayers (clSeed), TodoKeywords (..), loadConfigDirs
                       , seedContext )
import Data.Org.Blob (metaDirIn, storeRootIn)
import Data.Org.Edit (ParsedDocument (..), readBytes, readParsed)
import Data.Org.Index ( BlobEntry (..), IndexDrift, blobEntryOf, driftOf
                      , foldSegments, indexReportLines, manifestFile, metaDir
                      , openSegment, segmentNames )
import Data.Org.Walk ( Found (..), LoadFailure (..), WalkOptions (..), claimById
                     , errText, findOrgFilesWith, isBlob, mapFilesConcurrently
                     , storeDir )

import qualified Data.Map.Strict as Map

sampleLimit :: Int
sampleLimit = 20


-- | Scan ROOTS for .org files as OPTS asks, parse each one, and print a summary report.
-- Files are read on the loader's own pool; the fold into 'Totals' stays serial, so every count is what a one-file-at-a-time run produced.
runScan :: WalkOptions -> [FilePath] -> IO ()
runScan opts roots = do
  started <- getCurrentTime
  found <- findOrgFilesWith opts roots
  let paths = sort (foundFiles found)
      dirErrs = sort (foundDirErrs found)
      derived = sort (foundDerived found)
      configDirs = sort (foundConfig found)
  config <- loadConfigDirs configDirs
  let seed = clSeed config
  walked <- getCurrentTime
  results <- mapFilesConcurrently (scanFile (seedContext config)) paths
  let totals = foldl' visitFile emptyTotals (zip paths results)
  finished <- totals `seq` getCurrentTime
  drifts <- indexDrifts roots derived (blobsOf totals)
  let elapsed from to = realToFrac (diffUTCTime to from) :: Double
  report roots (length paths) totals dirErrs derived configDirs seed
         (elapsed started walked) (elapsed started finished) drifts
  where visitFile t (path, result) = merge t path result


-- | What the report calls FAILURE — the loader's own three rungs, so the scan counts what the daemon counts.
failureLabel :: LoadFailure -> Text
failureLabel ReadFailed = "read failures"
failureLabel DecodeFailed = "decode failures"
failureLabel ParseFailed = "parse failures"

data Bucket = BOk | BFailed !LoadFailure !Text

data FileResult = FileResult
  { frBucket     :: !Bucket
  , frElements   :: !Int
  , frHeadlines  :: !Int
  , frViolations :: !Int
  , frSample     :: ![Text]
  , frIds        :: ![Text]              -- ^ the ORG_GLANCE_IDs the file claims, copied.
  , frBlob       :: !(Maybe BlobEntry)   -- ^ set only for a blob; see 'blobEntryOf'.
  }

-- | Read, decode and parse PATH from SEED, forcing the result before returning it.
-- 'readParsed' forces only far enough to know the parse succeeded; the fold below is what walks into every element.
scanFile :: Context -> FilePath -> IO FileResult
scanFile seed path = do
  parsed <- readParsed seed path
  case parsed of
    Left (fault, why) -> pure (bare (BFailed fault why))
    Right pd -> do
      outcome <- try (evaluate (forceResult (analyse path pd)))
      pure $ case outcome of
        Left e  -> bare (BFailed ParseFailed ("exception: " <> errText (e :: SomeException)))
        Right r -> r
  where bare b = FileResult b 0 0 0 [] [] Nothing

-- | Tally PARSED's elements, headlines and span violations.
analyse :: FilePath -> ParsedDocument -> FileResult
analyse path pd =
  FileResult BOk (accElements acc) (accHeadlines acc) (accViolations acc) (accSample acc)
             [ T.copy i | h <- heads, Just i <- [identity h] ]
             (if isBlob path then blobEntryOf path (map indexTerms heads) else Nothing)
  where doc   = pdText pd
        elems = pdElements pd
        acc   = foldl' (step path doc (T.length doc)) (Acc 0 0 0 [] (Cursor 0 doc)) elems
        heads = headlinesOf elems

-- | H as the index comparison reads it, each field copied out of the document so no blob entry pins the text it was sliced from.
indexTerms :: Headline -> (Maybe Text, Text, Bool)
indexTerms h = ( T.copy <$> identity h
               , maybe "" (T.copy . name) (todo h)
               , archiveTag `elem` tagList (tags h) )
  where tagList (Tags ts) = ts

data Acc = Acc
  { accElements   :: !Int
  , accHeadlines  :: !Int
  , accViolations :: !Int
  , accSample     :: ![Text]
  , accCursor     :: !Cursor
  }

step :: FilePath -> Text -> Int -> Acc -> Spanned Element -> Acc
step path doc len acc el = Acc
  { accElements   = accElements acc + 1
  , accHeadlines  = accHeadlines acc + headline
  , accViolations = accViolations acc + length violations
  , accSample     = capped (accSample acc) violations
  , accCursor     = cursor
  }
  where (violations, cursor) = elementViolations path doc len (accCursor acc) el
        headline = case valueOf el of
          EHeadline _ -> 1
          _other      -> 0

-- | Force RESULT so that no thunk outlives the document it came from.
forceResult :: FileResult -> FileResult
forceResult r =
  frBucket r `seq` frElements r `seq` frHeadlines r `seq` frViolations r
              `seq` foldr seq (foldr seq (blob `seq` r) (frIds r)) (frSample r)
-- To WHNF and no further: 'BlobEntry' has strict fields, so applying the constructor forces the cells out of the document.
  where blob = maybe () (`seq` ()) (frBlob r)


-- | A slicer that remembers where it stopped, so left-to-right slicing of one document stays linear in its length.
data Cursor = Cursor !Int !Text

-- | Slice SP out of DOC, reusing CUR when SP starts at or after it.
sliceWith :: Text -> Cursor -> Span -> (Text, Cursor)
sliceWith doc cur@(Cursor off rest) sp
  | start >= off = let rest' = T.drop (start - off) rest
                   in (T.take (spanEnd sp - start) rest', Cursor start rest')
  | otherwise    = (sliceSpan doc sp, cur)
  where start = spanStart sp

elementViolations :: FilePath -> Text -> Int -> Cursor -> Spanned Element -> ([Text], Cursor)
elementViolations path doc len cur el = case valueOf el of
  EHeadline h -> let (vs, cur') = headlineViolations path doc len cur h
                 in (own ++ vs, cur')
  _other      -> (own, cur)
  where own = wellFormed path len "element" (spanOf el)

-- | Report SP when it runs backwards or leaves [0, LEN].
wellFormed :: FilePath -> Int -> Text -> Span -> [Text]
wellFormed path len label sp = [note path sp (label <> "/" <> fault) | fault <- spanFaults len sp]

-- | Check H's sub-spans: shape, containment in 'hsFull', order, and slices.
headlineViolations :: FilePath -> Text -> Int -> Cursor -> Headline -> ([Text], Cursor)
headlineViolations path doc len cur h = (concat parts, cur')
  where
    full = hsFull (spans h)
    present = [ (label, sp, ok) | (label, Just sp, ok) <- headlineSpanParts h ]
    (sliced, cur') = sliceAll doc cur present

    parts =
      [ wellFormed path len "hsFull" full
      , concat [ wellFormed path len label sp | (label, sp, _ok) <- present ]
      , concat [ [ note path sp (label <> "/outside-hsFull") ]
               | (label, sp, _ok) <- present
               , spanStart sp < spanStart full || spanEnd sp > spanEnd full ]
      , [ note path b (nb <> "/overlaps-" <> na)
        | ((na, a, _), (nb, b, _)) <- zip present (drop 1 present)
        , spanEnd a > spanStart b ]
      , concat [ [ note path sp (label <> "/slice-mismatch") ]
               | (label, sp, ok, txt) <- sliced, not (ok txt) ]
      ]

sliceAll :: Text -> Cursor
         -> [(Text, Span, Text -> Bool)]
         -> ([(Text, Span, Text -> Bool, Text)], Cursor)
sliceAll doc cur parts = (sliced, cur')
  where (cur', sliced) = mapAccumL cut cur parts
        cut at (label, sp, ok) = let (txt, next) = sliceWith doc at sp
                                 in (next, (label, sp, ok, txt))

-- | Render a violation as "path:offset kind".
note :: FilePath -> Span -> Text -> Text
note path sp kind = T.pack path <> ":" <> TS.showt (spanStart sp) <> " " <> kind


-- | HOW MANY, and a capped sample of them: a count and its listing spelled apart can be stepped apart.
data Tally a = Tally !Int ![a]

emptyTally :: Tally a
emptyTally = Tally 0 []

-- | N more counted, with NEW offered to the sample as far as 'sampleLimit' allows.  N is separate from @length NEW@.
add :: Int -> [a] -> Tally a -> Tally a
add n new (Tally seen sample) = Tally (seen + n) (capped sample new)

tallyCount :: Tally a -> Int
tallyCount (Tally n _sample) = n

tallySample :: Tally a -> [a]
tallySample (Tally _n sample) = sample

data Totals = Totals
  { tOk         :: !Int
  , tFailed     :: !(Map.Map LoadFailure (Tally (FilePath, Text)))
  , tElements   :: !Int
  , tHeadlines  :: !Int
  , tViolations :: !(Tally Text)
  , tIds        :: !(Map.Map Text FilePath)  -- ^ every id seen, and the file that keeps it.
  , tCollisions :: !(Tally Text)
    -- Every blob the walk parsed and what was read out of it, in REVERSE walk order; undeduplicated, 'driftOf' owning the tie rule.
  , tBlobs      :: ![(FilePath, Maybe BlobEntry)]
  }

emptyTotals :: Totals
emptyTotals = Totals 0 Map.empty 0 0 emptyTally Map.empty emptyTally []

failed :: LoadFailure -> Totals -> Tally (FilePath, Text)
failed kind = Map.findWithDefault emptyTally kind . tFailed

merge :: Totals -> FilePath -> FileResult -> Totals
merge t path r = case frBucket r of
  BFailed kind why -> t { tFailed = Map.insert kind (add 1 [(path, why)] (failed kind t))
                                               (tFailed t) }
  BOk              -> ids (blob (t { tOk         = tOk t + 1
                                   , tElements   = tElements t + frElements r
                                   , tHeadlines  = tHeadlines t + frHeadlines r
                                   , tViolations = add (frViolations r) (frSample r)
                                                       (tViolations t) }))
  where ids acc = foldl' (claim path) acc (frIds r)
        blob acc | isBlob path = acc { tBlobs = (path, frBlob r) : tBlobs acc }
                 | otherwise   = acc

-- | ID from PATH folded into ACC's index, by the rule the rows are resolved by ('Glance.Query.resolveIds').
claim :: FilePath -> Totals -> Text -> Totals
claim path t i = case Map.lookup i (tIds t) of
  Nothing   -> t { tIds = Map.insert i path (tIds t) }
  Just held -> seen (snd (claimById path held))
    where seen (kept, dropped) = t
            { tIds        = Map.insert i kept (tIds t)
            , tCollisions = add 1 [ i <> ": kept " <> T.pack kept
                                      <> ", dropped " <> T.pack dropped ]
                                (tCollisions t) }


-- | Compare every org-glance index this run can see against BLOBS, one 'IndexDrift' per store in the order the stores were found.
-- Each root's own @.org-glance\/meta@ plus every @meta@ the WALK declined; a nested store under @--include-derived@ is the one shape this misses.
indexDrifts :: [FilePath] -> [FilePath] -> [(FilePath, Maybe BlobEntry)] -> IO [IndexDrift]
indexDrifts roots derived blobs = do
  metas <- filterM doesDirectoryExist (storeMetaDirs roots derived)
  mapM compare' metas
  where
    compare' meta = do
      manifest <- bytesOf (meta </> manifestFile)
      names <- filterM (doesFileExist . (meta </>)) (segmentNames manifest)
      segments <- mapM (\n -> (,) (n == openSegment) . orEmpty <$> bytesOf (meta </> n)) names
      pure (driftOf store (foldSegments segments) (under (store </> storeDir)))
      where store = takeDirectory meta
    under dataDir = [ b | b@(path, _) <- blobs, (dataDir <> "/") `isPrefixOf` path ]
    orEmpty = fromMaybe BS.empty

-- | T's blobs in walk order, which is the order 'driftOf' resolves a shared id in.
blobsOf :: Totals -> [(FilePath, Maybe BlobEntry)]
blobsOf = reverse . tBlobs

-- | The meta directories to compare, deduplicated and in the order they were named.  Textual, so a store reached two ways is compared twice.
storeMetaDirs :: [FilePath] -> [FilePath] -> [FilePath]
storeMetaDirs roots derived =
  nub ([ metaDirIn (storeRootIn root) | root <- roots ]
        ++ [ d | d <- derived, takeFileName d == metaDir ])

-- | PATH's bytes, or 'Nothing' when it cannot be read: an index it cannot open is an index it says nothing about.
bytesOf :: FilePath -> IO (Maybe BS.ByteString)
bytesOf path = either (const Nothing) Just <$> readBytes path

-- | OLD extended by NEW, truncated to 'sampleLimit' and forced.
capped :: [a] -> [a] -> [a]
capped old new
  | length old >= sampleLimit = old
  | otherwise = let kept = take sampleLimit (old ++ new) in length kept `seq` kept


-- | The run's summary.  WALKSECS is how much of SECS the serial directory walk took; SEED is the config's recognition union, the one input to these counts that is no file under a root.
report :: [FilePath] -> Int -> Totals -> [(FilePath, Text)] -> [FilePath] -> [FilePath]
       -> TodoKeywords -> Double -> Double -> [IndexDrift] -> IO ()
report roots files t dirErrs derived configDirs seed walkSecs secs drifts = do
  TIO.putStrLn ("scan " <> T.intercalate " " (map T.pack roots))
  mapM_ TIO.putStrLn
    [ row "dirs scanned"    (num (length roots))
    , row "files"           (num files)
    , row "ok"              (num (tOk t))
    , row (failureLabel ReadFailed)   (num (count ReadFailed))
    , row (failureLabel DecodeFailed) (num (count DecodeFailed))
    , row (failureLabel ParseFailed)  (num (count ParseFailed))
    , row "unreadable dirs" (num (length dirErrs))
    , row "derived skipped" (num (length derived))
    , row "config skipped"  (num (length configDirs))
    , row "config keywords" (num (length keywords))
    , row "elements"        (num (tElements t))
    , row "headlines"       (num (tHeadlines t))
    , row "span violations" (num (tallyCount (tViolations t)))
    , row "id collisions"   (num (tallyCount (tCollisions t)))
    , row "walk seconds"    (fixed 2 walkSecs)
    , row "wall seconds"    (fixed 2 secs)
    , row "files/sec"       (fixed 1 rate)
    ]
  -- The read-failure SECTION totals the unreadable directories with the files, where the rows above keep them apart.
  section (failureLabel ReadFailed) (count ReadFailed + length dirErrs)
          (paths (capped (tallySample (failed ReadFailed t)) dirErrs))
  section (failureLabel DecodeFailed) (count DecodeFailed)
          (paths (tallySample (failed DecodeFailed t)))
  section (failureLabel ParseFailed) (count ParseFailed)
          (paths (tallySample (failed ParseFailed t)))
  section "span violations" (tallyCount (tViolations t)) (tallySample (tViolations t))
  section "id collisions" (tallyCount (tCollisions t)) (tallySample (tCollisions t))
  section "derived skipped" (length derived) (map T.pack (take sampleLimit derived))
  section "config skipped" (length configDirs) (map T.pack (take sampleLimit configDirs))
  section "config keywords" (length keywords) [T.unwords keywords]
  mapM_ (\d -> TIO.putStrLn "" >> mapM_ TIO.putStrLn (indexReportLines d)) drifts
  where rate | secs > 0  = fromIntegral files / secs
             | otherwise = 0
        keywords = tkActive seed <> tkInactive seed
        count kind = tallyCount (failed kind t)
        paths entries = [ T.pack p <> ": " <> why | (p, why) <- entries ]

section :: Text -> Int -> [Text] -> IO ()
section title total entries
  | total == 0 = pure ()
  | otherwise = do
      TIO.putStrLn ""
      TIO.putStrLn (title <> " (" <> num total <> " total, showing "
                          <> num (length entries) <> "):")
      mapM_ (TIO.putStrLn . ("  " <>)) entries

row :: Text -> Text -> Text
row label value = "  " <> T.justifyLeft 17 ' ' label <> T.justifyRight 10 ' ' value

num :: Int -> Text
num = TS.showt

fixed :: Int -> Double -> Text
fixed digits x = T.pack (showFFloat (Just digits) x "")

