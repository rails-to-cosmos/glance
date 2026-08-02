-- | Corpus scan: parse every .org file under a set of roots and report parse
-- coverage together with span-invariant violations, plus — where a root holds
-- an org-glance store — how far that store's index has drifted from the blobs
-- it indexes.
module Scan (runScan) where

import Control.Exception (IOException, SomeException, evaluate, try)
import Control.Monad (filterM)
import Data.List (foldl', isPrefixOf, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Void (Void)
import Numeric (showFFloat)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified TextShow as TS

import Data.Org
import Data.Org.Config ( ConfigLayers (clSeed), TodoKeywords (..), loadConfigDirs
                       , seedContext )
import Data.Org.Index ( BlobEntry (..), IndexDrift, blobEntryOf, driftOf
                      , foldSegments, indexReportLines, manifestFile, openSegment
                      , segmentNames )
import Data.Org.Walk ( Found (..), WalkOptions (..), beatsForId, errText
                     , findOrgFilesWith, isBlob, mapFilesConcurrently )

import qualified Data.Map.Strict as Map

-- | How many entries of each failure listing to print.
sampleLimit :: Int
sampleLimit = 20

-- Entry point

-- | Scan ROOTS for .org files as OPTS asks, parse each one, and print a
-- summary report.
--
-- Every file is parsed from the same seed the loader uses — org's TODO\/DONE
-- plus every keyword the roots' @.org-glance\/config@ names ('loadConfigs') —
-- so the counts describe the tree the daemon serves rather than a stricter
-- reading of it.  Without it the scan would report a keyword the browser shows
-- as a state as the first word of a title.
--
-- The files are read on the loader's own pool ('mapFilesConcurrently'), which
-- forces each 'FileResult' in the worker that produced it; the fold that turns
-- them into 'Totals' stays here and stays serial, over the sorted path list, so
-- the id index, the capped failure listings and every count are what a
-- one-file-at-a-time run produced.  The walk ahead of it is serial too.
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

-- Per-file scan

-- | Which bucket a file landed in, with the reason when it failed.
data Bucket = BOk | BRead !Text | BDecode !Text | BParse !Text

-- | What one file contributed to the run.
data FileResult = FileResult
  { frBucket     :: !Bucket
  , frElements   :: !Int
  , frHeadlines  :: !Int
  , frViolations :: !Int
  , frSample     :: ![Text]
  , frIds        :: ![Text]              -- ^ the ORG_GLANCE_IDs the file claims, copied.
  , frBlob       :: !(Maybe BlobEntry)   -- ^ set only for a blob; see 'blobEntryOf'.
  }

-- | Read, decode and parse PATH from SEED, forcing the result before returning
-- it.
scanFile :: Context -> FilePath -> IO FileResult
scanFile seed path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case raw of
    Left e -> pure (bare (BRead (errText e)))
    Right bytes -> case TE.decodeUtf8' bytes of
      Left e -> pure (bare (BDecode (errText e)))
      Right doc -> do
        outcome <- try (evaluate (forceResult (analyse seed path doc)))
        pure $ case outcome of
          Left e  -> bare (BParse ("exception: " <> errText (e :: SomeException)))
          Right r -> r
  where bare b = FileResult b 0 0 0 [] [] Nothing

-- | Parse DOC from SEED and tally its elements, headlines and span violations.
analyse :: Context -> FilePath -> Text -> FileResult
analyse seed path doc = case orgParse seed doc of
  (_elems, _ctx, Just err) -> FileResult (BParse (errorReason err)) 0 0 0 [] [] Nothing
  (elems, _ctx, Nothing)   ->
    let acc = foldl' (step path doc (T.length doc)) (Acc 0 0 0 [] (Cursor 0 doc)) elems
        heads = [ h | EHeadline h <- map valueOf elems ]
    in FileResult BOk (accElements acc) (accHeadlines acc) (accViolations acc) (accSample acc)
                  [ T.copy i | h <- heads, Just i <- [identity h] ]
                  (if isBlob path then blobEntryOf path (map indexTerms heads) else Nothing)

-- | H as the index comparison reads it: its id, its TODO keyword and whether it
-- is archived, each copied out of the document so no blob entry pins the text
-- it was sliced from.
indexTerms :: Headline -> (Maybe Text, Text, Bool)
indexTerms h = ( T.copy <$> identity h
               , maybe "" (T.copy . name) (todo h)
               , archiveTag `elem` tagList (tags h) )
  where tagList (Tags ts) = ts

-- | Running tally over one file's elements.
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
  -- To WHNF and no further: 'BlobEntry' has strict fields, so applying the
  -- constructor is what forces the cells out of the document.
  where blob = maybe () (`seq` ()) (frBlob r)

-- Span checks

-- | A slicer that remembers where it stopped, so left-to-right slicing of one
-- document stays linear in its length.
data Cursor = Cursor !Int !Text

-- | Slice SP out of DOC, reusing CUR when SP starts at or after it.
sliceWith :: Text -> Cursor -> Span -> (Text, Cursor)
sliceWith doc cur@(Cursor off rest) sp
  | start >= off = let rest' = T.drop (start - off) rest
                   in (T.take (spanEnd sp - start) rest', Cursor start rest')
  | otherwise    = (sliceSpan doc sp, cur)
  where start = spanStart sp

-- | Span violations of EL, and the cursor left after slicing its parts.
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

-- | Slice each labelled span in source order, threading one cursor.
sliceAll :: Text -> Cursor
         -> [(Text, Span, Text -> Bool)]
         -> ([(Text, Span, Text -> Bool, Text)], Cursor)
sliceAll doc = go
  where go cur [] = ([], cur)
        go cur ((label, sp, ok) : rest) =
          let (txt, cur') = sliceWith doc cur sp
              (more, cur'') = go cur' rest
          in ((label, sp, ok, txt) : more, cur'')

-- | Render a violation as "path:offset kind".
note :: FilePath -> Span -> Text -> Text
note path sp kind = T.pack path <> ":" <> TS.showt (spanStart sp) <> " " <> kind

-- Totals

data Totals = Totals
  { tOk         :: !Int
  , tRead       :: !Int
  , tDecode     :: !Int
  , tParse      :: !Int
  , tElements   :: !Int
  , tHeadlines  :: !Int
  , tViolations :: !Int
  , tReadErrs   :: ![(FilePath, Text)]
  , tDecodeErrs :: ![(FilePath, Text)]
  , tParseErrs  :: ![(FilePath, Text)]
  , tViolSample :: ![Text]
  , tIds        :: !(Map.Map Text FilePath)  -- ^ every id seen, and the file that keeps it.
  , tCollisions :: !Int
  , tCollSample :: ![Text]
    -- Every blob the walk parsed and what was read out of it, in REVERSE walk
    -- order; 'blobsOf' turns it back.  Undeduplicated on purpose — 'driftOf'
    -- keys these by id and that is where the tie rule belongs.
  , tBlobs      :: ![(FilePath, Maybe BlobEntry)]
  }

emptyTotals :: Totals
emptyTotals = Totals 0 0 0 0 0 0 0 [] [] [] [] Map.empty 0 [] []

merge :: Totals -> FilePath -> FileResult -> Totals
merge t path r = case frBucket r of
  BRead why   -> t { tRead   = tRead t + 1
                   , tReadErrs = capped (tReadErrs t) [(path, why)] }
  BDecode why -> t { tDecode = tDecode t + 1
                   , tDecodeErrs = capped (tDecodeErrs t) [(path, why)] }
  BParse why  -> t { tParse  = tParse t + 1
                   , tParseErrs = capped (tParseErrs t) [(path, why)] }
  BOk         -> ids (blob (t { tOk         = tOk t + 1
                              , tElements   = tElements t + frElements r
                              , tHeadlines  = tHeadlines t + frHeadlines r
                              , tViolations = tViolations t + frViolations r
                              , tViolSample = capped (tViolSample t) (frSample r) }))
  where ids acc = foldl' (claim path) acc (frIds r)
        blob acc | isBlob path = acc { tBlobs = (path, frBlob r) : tBlobs acc }
                 | otherwise   = acc

-- | ID from PATH folded into ACC's index.  The same rule the rows are resolved
-- by ('Glance.Query.resolveIds'): a canonical path takes the id, otherwise the
-- first file in walk order keeps it, and the loser is reported.
claim :: FilePath -> Totals -> Text -> Totals
claim path t i = case Map.lookup i (tIds t) of
  Nothing   -> t { tIds = Map.insert i path (tIds t) }
  Just held -> seen (if beatsForId path held then (path, held) else (held, path))
    where seen (kept, dropped) = t
            { tIds        = Map.insert i kept (tIds t)
            , tCollisions = tCollisions t + 1
            , tCollSample = capped (tCollSample t)
                              [i <> ": kept " <> T.pack kept <> ", dropped " <> T.pack dropped] }

-- The org-glance index

-- | Compare every org-glance index this run can see against BLOBS, one
-- 'IndexDrift' per store in the order the stores were found.
--
-- WHICH STORES.  Each root's own @.org-glance\/meta@, plus every @meta@
-- directory the WALK declined — 'foundDerived' holds them already, so a store
-- nested anywhere under a root is compared without a second traversal.  The
-- roots are asked separately because @--include-derived@ walks the mirrors
-- instead of declining them, and a run pointed straight at a tree must still
-- answer for that tree's own store.  A nested store under that flag is the one
-- shape this misses.
--
-- A store with no meta directory is silently no store, which is what makes the
-- instrument free for a tree org-glance never touched.
indexDrifts :: [FilePath] -> [FilePath] -> [(FilePath, Maybe BlobEntry)] -> IO [IndexDrift]
indexDrifts roots derived blobs = do
  metas <- filterM doesDirectoryExist (storeMetaDirs roots derived)
  mapM compare' metas
  where
    compare' meta = do
      manifest <- bytesOf (meta </> manifestFile)
      names <- filterM (doesFileExist . (meta </>)) (segmentNames manifest)
      segments <- mapM (\n -> (,) (n == openSegment) . orEmpty <$> bytesOf (meta </> n)) names
      pure (driftOf store (foldSegments segments) (under (store </> "data")))
      where store = takeDirectory meta
    under dataDir = [ b | b@(path, _) <- blobs, (dataDir <> "/") `isPrefixOf` path ]
    orEmpty = fromMaybe BS.empty

-- | T's blobs in walk order, which is the order 'driftOf' resolves a shared id
-- in.  The fold prepends, so this is the one place that reverses.
blobsOf :: Totals -> [(FilePath, Maybe BlobEntry)]
blobsOf = reverse . tBlobs

-- | The meta directories to compare, deduplicated and in the order they were
-- named.  Textual, like every other rule the walk reads off a path: nothing
-- canonicalizes a root, so a store reached two ways is compared twice.
storeMetaDirs :: [FilePath] -> [FilePath] -> [FilePath]
storeMetaDirs roots derived =
  nub ([ root </> ".org-glance" </> "meta" | root <- roots ]
        ++ [ d | d <- derived, takeFileName d == "meta" ])

-- | PATH's bytes, or 'Nothing' when it cannot be read.  The instrument never
-- fails a scan: an index it cannot open is an index it says nothing about.
bytesOf :: FilePath -> IO (Maybe BS.ByteString)
bytesOf path = either (const Nothing) Just
           <$> (try (BS.readFile path) :: IO (Either IOException BS.ByteString))

-- | OLD extended by NEW, truncated to 'sampleLimit' and forced.
capped :: [a] -> [a] -> [a]
capped old new
  | length old >= sampleLimit = old
  | otherwise = let kept = take sampleLimit (old ++ new) in length kept `seq` kept

-- Reporting

-- | The run's summary.  WALKSECS is how much of SECS the serial directory walk
-- took, which is the half of the wall the pool cannot touch — worth a row of
-- its own now that the reads are parallel and the walk stays serial.  SEED is
-- the config's recognition union, reported because it is the one input to these
-- counts that is not a file under a root.
report :: [FilePath] -> Int -> Totals -> [(FilePath, Text)] -> [FilePath] -> [FilePath]
       -> TodoKeywords -> Double -> Double -> [IndexDrift] -> IO ()
report roots files t dirErrs derived configDirs seed walkSecs secs drifts = do
  TIO.putStrLn ("scan " <> T.intercalate " " (map T.pack roots))
  mapM_ TIO.putStrLn
    [ row "dirs scanned"    (num (length roots))
    , row "files"           (num files)
    , row "ok"              (num (tOk t))
    , row "read failures"   (num (tRead t))
    , row "decode failures" (num (tDecode t))
    , row "parse failures"  (num (tParse t))
    , row "unreadable dirs" (num (length dirErrs))
    , row "derived skipped" (num (length derived))
    , row "config skipped"  (num (length configDirs))
    , row "config keywords" (num (length keywords))
    , row "elements"        (num (tElements t))
    , row "headlines"       (num (tHeadlines t))
    , row "span violations" (num (tViolations t))
    , row "id collisions"   (num (tCollisions t))
    , row "walk seconds"    (fixed 2 walkSecs)
    , row "wall seconds"    (fixed 2 secs)
    , row "files/sec"       (fixed 1 rate)
    ]
  section "read failures" (tRead t + length dirErrs)
          [T.pack p <> ": " <> why | (p, why) <- capped (tReadErrs t) dirErrs]
  section "decode failures" (tDecode t)
          [T.pack p <> ": " <> why | (p, why) <- tDecodeErrs t]
  section "parse failures" (tParse t)
          [T.pack p <> ": " <> why | (p, why) <- tParseErrs t]
  section "span violations" (tViolations t) (tViolSample t)
  section "id collisions" (tCollisions t) (tCollSample t)
  section "derived skipped" (length derived) (map T.pack (take sampleLimit derived))
  section "config skipped" (length configDirs) (map T.pack (take sampleLimit configDirs))
  section "config keywords" (length keywords) [T.unwords keywords]
  -- No store, no line: a tree org-glance never indexed says nothing about one.
  mapM_ (\d -> TIO.putStrLn "" >> mapM_ TIO.putStrLn (indexReportLines d)) drifts
  where rate | secs > 0  = fromIntegral files / secs
             | otherwise = 0
        keywords = tkActive seed <> tkInactive seed

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

-- | Position plus the first diagnostic line of ERR's pretty rendering.
errorReason :: ParseErrorBundle Text Void -> Text
errorReason err = T.unwords (take 1 ls ++ take 1 diagnostics)
  where ls = map T.stripEnd (T.lines (T.pack (errorBundlePretty err)))
        diagnostics = [l | l <- ls, any (`T.isPrefixOf` l) ["unexpected", "expecting"]]
