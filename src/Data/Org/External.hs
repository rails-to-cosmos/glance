-- | The notes this daemon leaves for org-glance under a store's @meta@ —
-- @EXTERNAL.jsonl@ and @COMPLETIONS.jsonl@.  Contract and cursor: AGENTS.hs.
module Data.Org.External ( Completion (..)
                         , blobIdOf
                         , completionLine
                         , completionsFile
                         , completionsPathOf
                         , externalFile
                         , externalLine
                         , externalPathOf
                         , noteCompletion
                         , noteExternalDelete
                         , noteExternalWrite
                         , tombstoneLine
                         ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.Text (Text)
import Foreign.Ptr (castPtr)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.IO ( OpenFileFlags (append, creat), OpenMode (WriteOnly)
                       , closeFd, defaultFileFlags, fdWriteBuf, openFd )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Time as Time

import Data.Org (defaultContext, firstHeadlineOf, identity, orgParse, spelled)
import Data.Org.Blob (metaDirIn, storeRootIn)
import Data.Org.Walk (isBlob, orgGlanceRoot)

externalFile :: FilePath
externalFile = "EXTERNAL.jsonl"

-- | Purely textual: a delete asks after the bytes are already gone.
externalPathOf :: FilePath -> Maybe FilePath
externalPathOf path
  | not (isBlob path) = Nothing
  | otherwise = (\store -> metaDirIn store </> externalFile) <$> orgGlanceRoot path

completionsFile :: FilePath
completionsFile = "COMPLETIONS.jsonl"

completionsPathOf :: FilePath -> IO (Maybe FilePath)
completionsPathOf root = do
  there <- doesDirectoryExist store
  pure (if there then Just (metaDirIn store </> completionsFile) else Nothing)
  where store = storeRootIn root

data Completion = Completion
  { coIdent   :: !Text  -- ^ the entry's @ORG_GLANCE_ID@, the ledger's key.
  , coState   :: !Text  -- ^ the keyword it landed on.
  , coShifted :: !Text  -- ^ its next occurrence, cookie and all.
  } deriving (Eq, Show)

completionLine :: Completion -> Time.UTCTime -> BS.ByteString
completionLine c at = BL.toStrict
  (  "{\"id\":" <> encode (coIdent c)
  <> ",\"at\":" <> encode (spelled "%Y-%m-%dT%H:%M:%SZ" at)
  <> ",\"state\":" <> encode (coState c)
  <> ",\"shifted\":" <> encode (coShifted c) <> "}\n" )

noteCompletion :: FilePath -> Completion -> IO ()
noteCompletion root c =
  completionsPathOf root >>= mapM_ (\note -> appendNote note (completionLine c))

-- | Hand-assembled so the field ORDER is the contract's; only values are encoded.
noteLine :: [BL.ByteString] -> Text -> Time.UTCTime -> BS.ByteString
noteLine extra ident at = BL.toStrict
  ("{\"id\":" <> encode ident <> ",\"at\":" <> encode (stamp at) <> mconcat extra <> "}\n")
  where stamp = spelled "%Y-%m-%dT%H:%M:%SZ"

externalLine :: Text -> Time.UTCTime -> BS.ByteString
externalLine = noteLine []

-- | @true@ is a literal — the field's one legal spelling, so no @false@ escapes.
tombstoneLine :: Text -> Time.UTCTime -> BS.ByteString
tombstoneLine = noteLine [",\"tombstone\":true"]

-- | The blob's entry is its FIRST headline; a child's own id is not it.
blobIdOf :: Text -> Maybe Text
blobIdOf doc = firstHeadlineOf elems >>= identity
  where (elems, _ctx, _err) = orgParse defaultContext doc

noteExternalWrite :: FilePath -> Text -> IO ()
noteExternalWrite = noteBlob externalLine

noteExternalDelete :: FilePath -> Text -> IO ()
noteExternalDelete = noteBlob tombstoneLine

noteBlob :: (Text -> Time.UTCTime -> BS.ByteString) -> FilePath -> Text -> IO ()
noteBlob render path doc = case (externalPathOf path, blobIdOf doc) of
  (Just note, Just ident) -> appendNote note (render ident)
  _noBlobOrNoId           -> pure ()

-- | Best effort — the org write has already landed, so no failure reaches the caller.
appendNote :: FilePath -> (Time.UTCTime -> BS.ByteString) -> IO ()
appendNote note render = do
  now <- Time.getCurrentTime
  void (try (write now) :: IO (Either IOException ()))
  where write at = do createDirectoryIfMissing True (takeDirectory note)
                      appendLine note (render at)

-- | @O_APPEND@ and ONE @write(2)@: a 'System.IO.AppendMode' handle remembers
-- the offset it opened at, so concurrent writers overwrite each other's lines.
appendLine :: FilePath -> BS.ByteString -> IO ()
appendLine path line = bracket (openFd path WriteOnly flags) closeFd $ \fd ->
  BU.unsafeUseAsCStringLen line $ \(bytes, len) ->
    void (fdWriteBuf fd (castPtr bytes) (fromIntegral len))
  where flags = defaultFileFlags { append = True, creat = Just 0o666 }
