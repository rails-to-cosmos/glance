-- | WHERE A DELETED BLOB GOES, and the move that puts it there:
-- @\<store\>\/.org-glance\/trash\/\<shard\>\/\<rest\>\/data.org.gz@.
--
-- DELETION IS A MOVE, never an unlink.  A blob is the canonical document — the
-- index is its projection and can be rebuilt, the document cannot — so the one
-- destructive command this daemon has takes the bytes out of the live tree and
-- keeps them.  Compressed because a trash that costs nothing is one nobody
-- empties, and the shard path is preserved because an id is what a restore
-- would be asked for.
--
-- THE TRASH IS NOT WALKED, and the DENYLIST is what says so:
-- 'Data.Org.Walk.derivedDirs' names it beside @overviews@ and @meta@.  The
-- @.gz@ would keep it out of 'Data.Org.Walk.isDocument' anyway, but resting on
-- that would make the rule an accident of how the bytes are stored rather than
-- a fact about the directory.
module Data.Org.Trash ( trashDirIn
                      , trashPathFor
                      , trashBlob
                      ) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import System.Directory ( createDirectoryIfMissing, doesFileExist, removeFile )
import System.FilePath ((</>), splitDirectories, takeDirectory)

import Data.Org.Blob (storeRootIn)
import Data.Org.Walk (isBlob, trashDir)

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | The trash directory of the store at ROOT.  Beside @data@ and @meta@ rather
-- than inside one: what is in it is no longer data, and 'Data.Org.Index' reads
-- @meta@ line by line.
trashDirIn :: FilePath -> FilePath
trashDirIn root = storeRootIn root </> trashDir

-- | Where the blob at PATH is to be kept, or 'Nothing' where PATH is not a blob
-- at all — the only thing this command deletes.
--
-- The SHARD is carried over verbatim: @data\/a7\/92f0…\/data.org@ becomes
-- @trash\/a7\/92f0…\/data.org.gz@, so the id a restore names is still spelled by
-- the path.  Read off the path rather than off an id, since the path is what
-- the walk found and an id is what a headline claims.
trashPathFor :: FilePath -> FilePath -> Maybe FilePath
trashPathFor root path
  | not (isBlob path) = Nothing
  | otherwise         = (\shard -> trashDirIn root </> shard <> ".gz")
                          <$> afterData (splitDirectories path)
  where
    afterData parts = case break (== "data") parts of
      (_, "data":rest) | not (null rest) -> Just (foldr1 (</>) rest)
      _                                  -> Nothing

-- | Move the blob at PATH into ROOT's trash, compressed, and take the original
-- out of the live tree.  Answers where it was put, or why it was not.
--
-- THE COPY LANDS BEFORE THE ORIGINAL GOES, so a failure at either step leaves
-- the document somewhere: a write that cannot finish is a blob still in the
-- tree, never one in neither place.  A destination that already exists is a
-- second deletion of the same id — the first one's bytes are what is kept, and
-- saying so is better than overwriting them.
trashBlob :: FilePath -> FilePath -> IO (Either Text FilePath)
trashBlob root path = case trashPathFor root path of
  Nothing   -> pure (Left (T.pack path <> " is not a blob: only a blob is deleted"))
  Just dest -> do
    taken <- doesFileExist dest
    if taken
      then pure (Left ("the trash already holds " <> T.pack dest))
      else do
        moved <- try (do createDirectoryIfMissing True (takeDirectory dest)
                         BL.readFile path >>= BL.writeFile dest . GZip.compress
                         removeFile path)
        pure $ case moved :: Either IOException () of
          Left err -> Left (T.pack (show err))
          Right () -> Right dest
