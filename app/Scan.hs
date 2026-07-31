-- | Corpus scan: parse every .org file under a set of roots and report parse
-- coverage together with span-invariant violations.
module Scan (runScan) where

import Control.Exception (IOException, SomeException, evaluate, try)
import Control.Monad (foldM)
import Data.List (foldl', sort)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Void (Void)
import Numeric (showFFloat)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified TextShow as TS

import Data.Org
import Data.Org.Walk ( Found (..), WalkOptions (..), beatsForId, errText
                     , findOrgFilesWith )

import qualified Data.Map.Strict as Map

-- | How many entries of each failure listing to print.
sampleLimit :: Int
sampleLimit = 20

-- Entry point

-- | Scan ROOTS for .org files as OPTS asks, parse each one, and print a
-- summary report.
runScan :: WalkOptions -> [FilePath] -> IO ()
runScan opts roots = do
  started <- getCurrentTime
  found <- findOrgFilesWith opts roots
  let paths = sort (foundFiles found)
      dirErrs = sort (foundDirErrs found)
      derived = sort (foundDerived found)
  totals <- foldM visitFile emptyTotals paths
  finished <- getCurrentTime
  let secs = realToFrac (diffUTCTime finished started) :: Double
  report roots (length paths) totals dirErrs derived secs
  where visitFile t path = do
          result <- scanFile path
          let t' = merge t path result
          t' `seq` pure t'

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
  , frIds        :: ![Text]     -- ^ the ORG_GLANCE_IDs the file claims, copied.
  }

-- | Read, decode and parse PATH, forcing the result before returning it.
scanFile :: FilePath -> IO FileResult
scanFile path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case raw of
    Left e -> pure (bare (BRead (errText e)))
    Right bytes -> case TE.decodeUtf8' bytes of
      Left e -> pure (bare (BDecode (errText e)))
      Right doc -> do
        outcome <- try (evaluate (forceResult (analyse path doc)))
        pure $ case outcome of
          Left e  -> bare (BParse ("exception: " <> errText (e :: SomeException)))
          Right r -> r
  where bare b = FileResult b 0 0 0 [] []

-- | Parse DOC and tally its elements, headlines and span violations.
analyse :: FilePath -> Text -> FileResult
analyse path doc = case orgParse defaultContext doc of
  (_elems, _ctx, Just err) -> FileResult (BParse (errorReason err)) 0 0 0 [] []
  (elems, _ctx, Nothing)   ->
    let acc = foldl' (step path doc (T.length doc)) (Acc 0 0 0 [] (Cursor 0 doc)) elems
    in FileResult BOk (accElements acc) (accHeadlines acc) (accViolations acc) (accSample acc)
                  [ T.copy i | EHeadline h <- map valueOf elems, Just i <- [identity h] ]

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
              `seq` foldr seq (foldr seq r (frIds r)) (frSample r)

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
  }

emptyTotals :: Totals
emptyTotals = Totals 0 0 0 0 0 0 0 [] [] [] [] Map.empty 0 []

merge :: Totals -> FilePath -> FileResult -> Totals
merge t path r = case frBucket r of
  BRead why   -> t { tRead   = tRead t + 1
                   , tReadErrs = capped (tReadErrs t) [(path, why)] }
  BDecode why -> t { tDecode = tDecode t + 1
                   , tDecodeErrs = capped (tDecodeErrs t) [(path, why)] }
  BParse why  -> t { tParse  = tParse t + 1
                   , tParseErrs = capped (tParseErrs t) [(path, why)] }
  BOk         -> ids (t { tOk         = tOk t + 1
                        , tElements   = tElements t + frElements r
                        , tHeadlines  = tHeadlines t + frHeadlines r
                        , tViolations = tViolations t + frViolations r
                        , tViolSample = capped (tViolSample t) (frSample r) })
  where ids acc = foldl' (claim path) acc (frIds r)

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

-- | OLD extended by NEW, truncated to 'sampleLimit' and forced.
capped :: [a] -> [a] -> [a]
capped old new
  | length old >= sampleLimit = old
  | otherwise = let kept = take sampleLimit (old ++ new) in length kept `seq` kept

-- Reporting

report :: [FilePath] -> Int -> Totals -> [(FilePath, Text)] -> [FilePath] -> Double
       -> IO ()
report roots files t dirErrs derived secs = do
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
    , row "elements"        (num (tElements t))
    , row "headlines"       (num (tHeadlines t))
    , row "span violations" (num (tViolations t))
    , row "id collisions"   (num (tCollisions t))
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
  where rate | secs > 0  = fromIntegral files / secs
             | otherwise = 0

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
