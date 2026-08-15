-- | WHERE A DELETED BLOB GOES, and the move that puts it there:
-- @\<store\>\/.org-glance\/trash\/\<shard\>\/\<rest\>\/data.org.gz@.  See AGENTS.hs.
module Data.Org.Trash ( trashDirIn
                      , trashPathFor
                      , trashBlob
                      ) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
                       , listDirectory, removeDirectoryRecursive )
import System.FilePath ((</>), makeRelative, splitDirectories, takeDirectory)

import Data.Org.Blob (storeRootIn)
import Data.Org.External (noteExternalDelete)
import Data.Org.Walk (Entry (..), entryOf, isBlob, storeDir, trashDir)

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

trashDirIn :: FilePath -> FilePath
trashDirIn root = storeRootIn root </> trashDir

-- | Where the blob at PATH is to be kept, or 'Nothing' where PATH is not a blob.
trashPathFor :: FilePath -> FilePath -> Maybe FilePath
trashPathFor root path
  | not (isBlob path) = Nothing
  | otherwise         = (\shard -> trashDirIn root </> shard <> ".gz")
                          <$> afterData (splitDirectories path)
  where
    afterData parts = case break (== storeDir) parts of
      (_, _store:rest) | not (null rest) -> Just (foldr1 (</>) rest)
      _                                  -> Nothing

-- | Move the blob at PATH into ROOT's trash, compressed, and take the original
-- out of the live tree.  THE COPY LANDS BEFORE THE ORIGINAL GOES, and the
-- tombstone's id is read while the document the move takes away is still here.
trashBlob :: FilePath -> FilePath -> IO (Either Text FilePath)
trashBlob root path = case trashPathFor root path of
  Nothing   -> pure (Left (T.pack path <> " is not a blob: only a blob is deleted"))
  Just dest -> do
    taken <- doesFileExist dest
    if taken
      then pure (Left ("the trash already holds " <> T.pack dest))
      else do
        let blobDir = takeDirectory path
            mirror  = takeDirectory dest
        doc <- documentAt path
        moved <- try (do here <- filesUnder blobDir
                         mapM_ (keep blobDir mirror) here
                         removeDirectoryRecursive blobDir)
        case moved :: Either IOException () of
          Left err -> pure (Left (T.pack (show err)))
          Right () -> do mapM_ (noteExternalDelete path) doc
                         pure (Right dest)
  where
    keep from to file = do
      let dest = to </> makeRelative from file <> ".gz"
      createDirectoryIfMissing True (takeDirectory dest)
      BL.readFile file >>= BL.writeFile dest . GZip.compress

documentAt :: FilePath -> IO (Maybe Text)
documentAt path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case raw of
    Left _unreadable -> Nothing
    Right bytes      -> either (const Nothing) Just (TE.decodeUtf8' bytes)

-- | Every regular file under DIR, at any depth.  A symlinked DIRECTORY is
-- DECLINED: following one would copy a foreign tree into the trash.
filesUnder :: FilePath -> IO [FilePath]
filesUnder dir = do
  names <- listDirectory dir
  fmap concat . mapM one $ map (dir </>) names
  where
    one path = do
      what <- entryOf path
      case what of
        Dir     -> filesUnder path
        Regular -> pure [path]
        Linked  -> do
          away <- doesDirectoryExist path   -- FOLLOWS, and only here
          pure [path | not away]
