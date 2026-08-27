-- | The @glance doctor@ report: run a corpus scan and print parse coverage, span-invariant violations, and how far each org-glance store's index has drifted from the blobs it indexes.  The scan ENGINE and the summary live in 'Data.Org.Doctor', so the CLI report and the daemon's cached 'Doctor' never disagree.
module Scan (runScan) where

import Data.Text (Text)
import Numeric (showFFloat)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified TextShow as TS

import Data.Org.Config (TodoKeywords (..))
import Data.Org.Doctor ( Corpus (..), Doctor (..), Tally (tallyCount, tallySample)
                       , Totals (..), capped, corpusDoctor, failed, sampleLimit
                       , scanCorpus )
import Data.Org.Index (indexReportLines)
import Data.Org.Walk (LoadFailure (..), WalkOptions)


-- | Scan ROOTS for .org files as OPTS asks and print the summary report.
runScan :: WalkOptions -> [FilePath] -> IO ()
runScan opts roots = scanCorpus opts roots >>= report


-- | What the report calls FAILURE — the loader's own three rungs, so the scan counts what the daemon counts.
failureLabel :: LoadFailure -> Text
failureLabel ReadFailed = "read failures"
failureLabel DecodeFailed = "decode failures"
failureLabel ParseFailed = "parse failures"


-- | A CORPUS as its summary report.  The numbers a 'Doctor' folds come off the
-- SAME 'Doctor' the daemon caches, so the CLI and the daemon cannot disagree.
report :: Corpus -> IO ()
report c = do
  TIO.putStrLn ("doctor " <> T.intercalate " " (map T.pack roots))
  mapM_ TIO.putStrLn
    [ row "dirs scanned"    (num (length roots))
    , row "files"           (num files)
    , row "ok"              (num (tOk t))
    , row (failureLabel ReadFailed)   (num (count ReadFailed))
    , row (failureLabel DecodeFailed) (num (docDecodeFailures doc))
    , row (failureLabel ParseFailed)  (num (docParseFailures doc))
    , row "unreadable dirs" (num (length dirErrs))
    , row "derived skipped" (num (length derived))
    , row "config skipped"  (num (length configDirs))
    , row "config keywords" (num (length keywords))
    , row "elements"        (num (tElements t))
    , row "headlines"       (num (tHeadlines t))
    , row "span violations" (num (docSpanViolations doc))
    , row "id collisions"   (num (docIdCollisions doc))
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
  where
    roots      = coRoots c
    files      = coFiles c
    t          = coTotals c
    dirErrs    = coDirErrs c
    derived    = coDerived c
    configDirs = coConfigDirs c
    seed       = coSeed c
    drifts     = coDrifts c
    walkSecs   = coWalkSecs c
    secs       = coWallSecs c
    doc        = corpusDoctor c
    rate | secs > 0  = fromIntegral files / secs
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
