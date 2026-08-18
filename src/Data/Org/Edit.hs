{-# LANGUAGE CPP #-}
-- | The write-back engine: replace half-open CHAR spans, temp+rename under an
-- optimistic lock.  The rename REPLACES A SYMLINK.
module Data.Org.Edit ( Edit (..)
                     , EditError (..)
                     , EditIOError (..)
                     , EditReceipt (..)
                     , ParsedDocument (..)
                     , Snapshot (..)
                     , applyEdits
                     , digestOfText
                     , editFile
                     , eolOf
                     , lineSpansIn
                     , linesWith
                     , openingFor
                     , readBytes
                     , readDocument
                     , readParsed
                     , snapshotOf
                     , takeSnapshot
                     , tempSuffix
                     ) where

import Control.Exception (IOException, SomeException, bracket, bracketOnError, evaluate, try)
import Control.Monad (unless, void, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
#ifdef PURE_CRYPTO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.SHA as SHA
#else
import Crypto.Hash (Digest, SHA256, hash)
#endif
import Data.List (sortOn)
import Data.Text (Text)
import Data.Void (Void)
import System.Directory ( copyPermissions, createDirectoryIfMissing, doesDirectoryExist
                        , doesFileExist, removeFile, renameFile )
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, hFlush, openBinaryTempFile)
import System.Posix.IO (OpenMode (ReadOnly), closeFd, defaultFileFlags, handleToFd, openFd)
import System.Posix.Unistd (fileSynchronise)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Org.Parser (orgParse)
import Data.Org.Types (Context, Element, Span (..), Spanned, spanFaults)
-- Qualified: 'Walk.LoadFailure' spells two constructors the way 'EditIOError' does.
import qualified Data.Org.Walk as Walk


-- | Replace 'editSpan' with 'editText'; zero-width inserts, empty text deletes.
data Edit = Edit { editSpan :: !Span, editText :: !Text }
  deriving (Eq, Show)

data EditError
  = OutOfBounds !Span !Int  -- ^ the span, and the length of the document it does not fit.
  | Backwards !Span         -- ^ 'spanStart' past 'spanEnd'.
  | Overlap !Span !Span     -- ^ the earlier span, and the one starting inside it.
  deriving (Eq, Show)

-- | DOC with EDITS applied, every offset read against DOC itself.  Any order,
-- pairwise non-overlapping; the stable sort lands two INSERTIONS at one offset
-- in LIST order.
applyEdits :: Text -> [Edit] -> Either EditError Text
applyEdits doc edits = do
  mapM_ (checkSpan (T.length doc) . editSpan) edits
  ordered <- disjoint (sortOn key edits)
  pure (T.concat (splice 0 doc ordered))
  where key e = (spanStart (editSpan e), spanEnd (editSpan e))

-- | Reject SP against LEN characters; the backwards arm reads 'spanFaults'\'s LABEL.
checkSpan :: Int -> Span -> Either EditError ()
checkSpan len sp = case spanFaults len sp of
  []     -> Right ()
  faults | faults == ["start-after-end"] -> Left (Backwards sp)
         | otherwise                     -> Left (OutOfBounds sp len)

linesWith :: Text -> [Text]
linesWith t
  | T.null t  = []
  | otherwise = case T.breakOn "\n" t of
      (line, rest) | T.null rest -> [line]
                   | otherwise   -> (line <> "\n") : linesWith (T.drop 1 rest)

lineSpansIn :: Text -> [(Span, Text)]
lineSpansIn t = go 0 (linesWith t)
  where go _at []      = []
        go at (l : ls) = (Span at (at + T.length l), l) : go (at + T.length l) ls

-- | The line ending T's first line uses, @"\\n"@ when it has none.
eolOf :: Text -> Text
eolOf t = case T.breakOn "\n" t of
  (before, rest) | not (T.null rest), "\r" `T.isSuffixOf` before -> "\r\n"
  _plain                                                         -> "\n"

-- | What an append to DOC owes ahead of it, so @* @ does not join a live line.
openingFor :: Text -> Text -> Text
openingFor doc eol
  | T.null doc || "\n" `T.isSuffixOf` doc = ""
  | otherwise                             = eol

disjoint :: [Edit] -> Either EditError [Edit]
disjoint edits = mapM_ check (zip edits (drop 1 edits)) >> pure edits
  where check (a, b)
          | spanEnd (editSpan a) <= spanStart (editSpan b) = Right ()
          | otherwise = Left (Overlap (editSpan a) (editSpan b))

splice :: Int -> Text -> [Edit] -> [Text]
splice _at rest [] = [rest]
splice at rest (Edit (Span s e) new : es) = kept : new : splice e dropped es
  where (kept, from) = T.splitAt (s - at) rest
        dropped      = T.drop (e - s) from


data Snapshot = Snapshot { snapPath :: !FilePath, snapDigest :: !Text }
  deriving (Eq, Show)

data EditReceipt = EditReceipt
  { receiptSnapshot :: !Snapshot  -- ^ the file as written.
  , receiptText     :: !Text      -- ^ the document as written.
  } deriving (Eq, Show)

data EditIOError
  = ReadFailed !FilePath !Text   -- ^ path, and the first line of the IO error.
  | DecodeFailed !FilePath       -- ^ the bytes on disk are not valid UTF-8.
  | Drift !FilePath !Text !Text  -- ^ path, the snapshot's digest, the digest found.
  | Rejected !EditError          -- ^ the edits do not apply to the current text.
  | WriteFailed !FilePath !Text  -- ^ the temp write, the permission copy or the rename failed.
  deriving (Eq, Show)

takeSnapshot :: FilePath -> IO (Either EditIOError Snapshot)
takeSnapshot path =
  either (Left . ReadFailed path) (Right . Snapshot path . digestOf) <$> readBytes path

-- | PATH's text and the digest of the SAME bytes; 'Nothing' is "create it".
readDocument :: FilePath -> IO (Maybe (Text, Text))
readDocument path = do
  raw <- readBytes path
  pure $ case raw of
    Left _err    -> Nothing
    Right bytes  -> either (const Nothing) (\doc -> Just (doc, digestOf bytes))
                           (TE.decodeUtf8' bytes)

snapshotOf :: FilePath -> Text -> Snapshot
snapshotOf path doc = Snapshot path (digestOfText doc)

digestOf :: BS.ByteString -> Text
#ifdef PURE_CRYPTO
digestOf bytes = T.pack (SHA.showDigest (SHA.sha256 (BSL.fromStrict bytes)))
#else
digestOf bytes = T.pack (show (hash bytes :: Digest SHA256))
#endif

digestOfText :: Text -> Text
digestOfText = digestOf . TE.encodeUtf8


data ParsedDocument = ParsedDocument
  { pdText     :: !Text                -- ^ the decoded document, which spans are offsets into.
  , pdDigest   :: !Text                -- ^ 'digestOf' the bytes it was decoded from.
  , pdElements :: ![Spanned Element]   -- ^ the parse, kept lazy: a caller forces what it counts.
  , pdContext  :: !Context             -- ^ the context the document's own pragmas left behind.
  }

-- | PATH read, decoded and parsed from SEED; 'evaluate' inside 'try' fails this file alone.
readParsed :: Context -> FilePath -> IO (Either (Walk.LoadFailure, Text) ParsedDocument)
readParsed seed path = do
  raw <- readBytes path
  case raw of
    Left why -> pure (Left (Walk.ReadFailed, why))
    Right bytes -> case TE.decodeUtf8' bytes of
      Left err -> pure (Left (Walk.DecodeFailed, Walk.errText err))
      Right doc -> do
        outcome <- try (evaluate (parsed bytes doc))
        pure $ case outcome of
          Left e  -> Left (Walk.ParseFailed
                          , "exception: " <> Walk.errText (e :: SomeException))
          Right r -> r
  where
    parsed bytes doc = case orgParse seed doc of
      (_elems, _ctx, Just err) -> Left (Walk.ParseFailed, parseReason err)
      (elems, ctx, Nothing)    -> Right (ParsedDocument doc (digestOf bytes) elems ctx)

parseReason :: ParseErrorBundle Text Void -> Text
parseReason err = T.unwords (take 1 ls ++ take 1 diagnostics)
  where ls = map T.stripEnd (T.lines (T.pack (errorBundlePretty err)))
        diagnostics = [l | l <- ls, any (`T.isPrefixOf` l) ["unexpected", "expecting"]]


-- | Apply EDITS to SNAP's file: drift or a rejected edit leaves it untouched.
editFile :: Snapshot -> [Edit] -> IO (Either EditIOError EditReceipt)
editFile snap edits = runExceptT $ do
  doc <- ExceptT (currentText snap)
  edited <- either (throwError . Rejected) pure (applyEdits doc edits)
  let written = TE.encodeUtf8 edited
  ExceptT (writeAtomically path written)
  pure (EditReceipt (Snapshot path (digestOf written)) edited)
  where path = snapPath snap

-- | The document SNAP's edits are measured against; the EMPTY digest is "create it".
currentText :: Snapshot -> IO (Either EditIOError Text)
currentText snap = do
  there <- doesFileExist path
  if not there && T.null (snapDigest snap) then pure (Right "") else runExceptT $ do
    bytes <- ExceptT (either (Left . ReadFailed path) Right <$> readBytes path)
    let found = digestOf bytes
    unless (found == snapDigest snap) $ throwError (Drift path (snapDigest snap) found)
    either (const (throwError (DecodeFailed path))) pure (TE.decodeUtf8' bytes)
  where path = snapPath snap

readBytes :: FilePath -> IO (Either Text BS.ByteString)
readBytes path = report <$> (try (BS.readFile path) :: IO (Either IOException BS.ByteString))
  where report = either (Left . Walk.errText) Right

-- | What a half-written document is called, and it is LOAD-BEARING:
-- 'openBinaryTempFile' splits its template at the LAST dot, so without this the
-- leftover is @notes\<rand\>.org@, which the walk COLLECTS, PARSES and SERVES AS ROWS.
tempSuffix :: String
tempSuffix = ".glance-tmp"

writeAtomically :: FilePath -> BS.ByteString -> IO (Either EditIOError ())
writeAtomically path bytes = report <$> attempt
  where
    dir = takeDirectory path
    attempt = try landing :: IO (Either IOException ())
    -- THE DIRECTORIES ARE SYNCED AFTER THE RENAME, and one call is not enough
    -- where the write had to create the directory it lands in.
    landing = do
      missing <- unmadeAncestors dir
      bracketOnError open discard write
      mapM_ syncDirectory (dir : map takeDirectory missing)
    open = do createDirectoryIfMissing True dir
              openBinaryTempFile dir (takeFileName path <> tempSuffix)
    write (tmp, h) = do
      BS.hPut h bytes
      hFlush h
      fd <- handleToFd h  -- flushes and closes the handle, keeping the fd.
      fileSynchronise fd
      closeFd fd
      there <- doesFileExist path
      when there (copyPermissions path tmp)
      renameFile tmp path
    discard (tmp, h) = do
      ignoring (hClose h)  -- a no-op once 'handleToFd' has run.
      ignoring (removeFile tmp)
    report = either (Left . WriteFailed path . Walk.errText) Right

-- | @fsync@ DIR, so a rename into it survives a crash.  A rename is atomic and
-- NOT durable: without this the entry can be lost and the file come back
-- holding what it held before a write that answered 200.  A failure is
-- DROPPED — the bytes are already visible, and refusing a write that landed
-- would be the worse lie.
syncDirectory :: FilePath -> IO ()
syncDirectory dir = ignoring (bracket (openFd dir ReadOnly defaultFileFlags) closeFd fileSynchronise)

-- | The ancestors of DIR that do not exist yet, deepest first.  Their PARENTS
-- are what needs the sync: a created directory is itself an entry, and one lost
-- takes the document under it.
-- | ACT, with an IO failure dropped.
ignoring :: IO a -> IO ()
ignoring act = void (try (void act) :: IO (Either IOException ()))

unmadeAncestors :: FilePath -> IO [FilePath]
unmadeAncestors dir
  | up == dir = pure []
  | otherwise = do
      there <- doesDirectoryExist dir
      if there then pure [] else (dir :) <$> unmadeAncestors up
  where up = takeDirectory dir
