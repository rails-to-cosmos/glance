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
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeExtension, (</>))
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified TextShow as TS

import Data.Org

-- | How many entries of each failure listing to print.
sampleLimit :: Int
sampleLimit = 20

-- Entry point

-- | Scan ROOTS for .org files, parse each one, and print a summary report.
runScan :: [FilePath] -> IO ()
runScan roots = do
  started <- getCurrentTime
  found <- foldM collect emptyFound roots
  let paths = sort (foundFiles found)
      dirErrs = sort (foundDirErrs found)
  totals <- foldM visitFile emptyTotals paths
  finished <- getCurrentTime
  let secs = realToFrac (diffUTCTime finished started) :: Double
  report roots (length paths) totals dirErrs secs
  where visitFile t path = do
          result <- scanFile path
          let t' = merge t path result
          t' `seq` pure t'

-- File discovery

-- | What a walk turned up: .org files and the directories it could not read.
-- Both accumulate in reverse; 'runScan' sorts them.
data Found = Found
  { foundFiles   :: ![FilePath]
  , foundDirErrs :: ![(FilePath, Text)]
  }

emptyFound :: Found
emptyFound = Found [] []

-- | Add ROOT's .org files to ACC, walking it when it is a directory.
collect :: Found -> FilePath -> IO Found
collect acc root = do
  isDir <- doesDirectoryExist root
  if isDir
    then walk acc root
    else do
      isFile <- doesFileExist root
      pure $! case (isFile, isOrg root) of
        (True, True)  -> keepFile root acc
        (True, False) -> acc
        (False, _)    -> keepDirErr root "no such file or directory" acc

-- | Collect .org files under DIR, recursing into real subdirectories only.
walk :: Found -> FilePath -> IO Found
walk acc dir = do
  listed <- try (listDirectory dir) :: IO (Either IOException [FilePath])
  case listed of
    Left e      -> pure $! keepDirErr dir (firstLine (T.pack (show e))) acc
    Right names -> foldM (visit dir) acc names

-- | Classify NAME inside DIR: recurse, keep, or ignore.  The accumulator is
-- forced at every entry: a thunk per entry would retain the whole tree.
visit :: FilePath -> Found -> FilePath -> IO Found
visit dir acc name = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      link <- try (pathIsSymbolicLink path) :: IO (Either IOException Bool)
      case link of
        Right False -> walk acc path
        _symlink    -> pure acc
    else pure $! if isOrg path then keepFile path acc else acc
  where path = dir </> name

keepFile :: FilePath -> Found -> Found
keepFile path acc = acc { foundFiles = path : foundFiles acc }

keepDirErr :: FilePath -> Text -> Found -> Found
keepDirErr path why acc = acc { foundDirErrs = (path, why) : foundDirErrs acc }

isOrg :: FilePath -> Bool
isOrg path = map Char.toLower (takeExtension path) == ".org"

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
  }

-- | Read, decode and parse PATH, forcing the result before returning it.
scanFile :: FilePath -> IO FileResult
scanFile path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case raw of
    Left e -> pure (bare (BRead (firstLine (T.pack (show e)))))
    Right bytes -> case TE.decodeUtf8' bytes of
      Left e -> pure (bare (BDecode (firstLine (T.pack (show e)))))
      Right doc -> do
        outcome <- try (evaluate (forceResult (analyse path doc)))
        pure $ case outcome of
          Left e  -> bare (BParse ("exception: " <> firstLine (T.pack (show (e :: SomeException)))))
          Right r -> r
  where bare b = FileResult b 0 0 0 []

-- | Parse DOC and tally its elements, headlines and span violations.
analyse :: FilePath -> Text -> FileResult
analyse path doc = case orgParse mempty doc of
  (_elems, _ctx, Just err) -> FileResult (BParse (errorReason err)) 0 0 0 []
  (elems, _ctx, Nothing)   ->
    let acc = foldl' (step path doc (T.length doc)) (Acc 0 0 0 [] (Cursor 0 doc)) elems
    in FileResult BOk (accElements acc) (accHeadlines acc) (accViolations acc) (accSample acc)

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
              `seq` foldr seq r (frSample r)

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
wellFormed path len label sp = concat
  [ [ note path sp (label <> "/negative-start")  | spanStart sp < 0 ]
  , [ note path sp (label <> "/start-after-end") | spanStart sp > spanEnd sp ]
  , [ note path sp (label <> "/end-past-eof")    | spanEnd sp > len ]
  ]

-- | Check H's sub-spans: shape, containment in 'hsFull', order, and slices.
headlineViolations :: FilePath -> Text -> Int -> Cursor -> Headline -> ([Text], Cursor)
headlineViolations path doc len cur h = (concat parts, cur')
  where
    hs = spans h
    full = hsFull hs
    present = [ (label, sp, ok) | (label, Just sp, ok) <- labelled ]
    (sliced, cur') = sliceAll doc cur present

    labelled :: [(Text, Maybe Span, Text -> Bool)]
    labelled =
      [ ("hsTodo",       hsTodo hs,       (== maybe "" name (todo h)))
      , ("hsPriority",   hsPriority hs,   (== maybe "" TS.showt (priority h)))
      , ("hsTitle",      hsTitle hs,      \t -> T.words t == T.words (TS.showt (title h)))
      , ("hsTags",       hsTags hs,       (== TS.showt (tags h)))
      , ("hsProperties", hsProperties hs, drawer)
      ]

    drawer t = ":PROPERTIES:" `T.isPrefixOf` stripped && ":END:" `T.isSuffixOf` stripped
      where stripped = T.strip t

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
  }

emptyTotals :: Totals
emptyTotals = Totals 0 0 0 0 0 0 0 [] [] [] []

merge :: Totals -> FilePath -> FileResult -> Totals
merge t path r = case frBucket r of
  BRead why   -> t { tRead   = tRead t + 1
                   , tReadErrs = capped (tReadErrs t) [(path, why)] }
  BDecode why -> t { tDecode = tDecode t + 1
                   , tDecodeErrs = capped (tDecodeErrs t) [(path, why)] }
  BParse why  -> t { tParse  = tParse t + 1
                   , tParseErrs = capped (tParseErrs t) [(path, why)] }
  BOk         -> t { tOk         = tOk t + 1
                   , tElements   = tElements t + frElements r
                   , tHeadlines  = tHeadlines t + frHeadlines r
                   , tViolations = tViolations t + frViolations r
                   , tViolSample = capped (tViolSample t) (frSample r) }

-- | OLD extended by NEW, truncated to 'sampleLimit' and forced.
capped :: [a] -> [a] -> [a]
capped old new
  | length old >= sampleLimit = old
  | otherwise = let kept = take sampleLimit (old ++ new) in length kept `seq` kept

-- Reporting

report :: [FilePath] -> Int -> Totals -> [(FilePath, Text)] -> Double -> IO ()
report roots files t dirErrs secs = do
  TIO.putStrLn ("scan " <> T.intercalate " " (map T.pack roots))
  mapM_ TIO.putStrLn
    [ row "dirs scanned"    (num (length roots))
    , row "files"           (num files)
    , row "ok"              (num (tOk t))
    , row "read failures"   (num (tRead t))
    , row "decode failures" (num (tDecode t))
    , row "parse failures"  (num (tParse t))
    , row "unreadable dirs" (num (length dirErrs))
    , row "elements"        (num (tElements t))
    , row "headlines"       (num (tHeadlines t))
    , row "span violations" (num (tViolations t))
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

-- | The first line of T, with trailing whitespace dropped.
firstLine :: Text -> Text
firstLine = T.stripEnd . T.takeWhile (/= '\n')

-- | Position plus the first diagnostic line of ERR's pretty rendering.
errorReason :: ParseErrorBundle Text Void -> Text
errorReason err = T.unwords (take 1 ls ++ take 1 diagnostics)
  where ls = map T.stripEnd (T.lines (T.pack (errorBundlePretty err)))
        diagnostics = [l | l <- ls, any (`T.isPrefixOf` l) ["unexpected", "expecting"]]
