{-# LANGUAGE CPP #-}
-- | org-glance's blob store from the writing side: where an entry's document
-- sits, and the identity it is keyed by.
--
-- 'Data.Org.Walk' classifies a path that is already there ('Data.Org.Walk.isBlob',
-- 'Data.Org.Walk.isCanonical'); this constructs one.  Both read the layout out
-- of the same three names, so a store directory renamed in one place cannot go
-- on matching in the other.
--
-- THE CONVENTION IS org-glance's, verified against its source and this corpus
-- (2026-08-04): @org-glance-graph:make-id@ is a bare @org-id-uuid@ and
-- @org-glance-graph:headline-data-path@ shards it by its FIRST TWO CHARACTERS,
-- verbatim and uncased, with the WHOLE remainder as the next component —
-- @04a14d10-41c1-4a3d-91b4-ea5c09364015@ lives at
-- @data\/04\/a14d10-41c1-4a3d-91b4-ea5c09364015\/data.org@.  An id of two
-- characters or fewer is not sharded at all, which is the case
-- 'Data.Org.Walk.isOccurrence' leaves its depth open for.
--
-- READING an id is a different question from writing one.  ~\/sync's 6073 blobs
-- carry four generations of the scheme — @Article-20210511-\<md5\>@,
-- @\<tag\>-\<time\>-\<md5\>@, @\<tag\>-\<md5\>@, 128-char hex — and 45 modern
-- UUIDs, so an @ORG_GLANCE_ID@ is an OPAQUE STRING everywhere it is read.  This
-- module only ever mints the current form.
module Data.Org.Blob ( blobPathIn
                     , metaDirIn
                     , mintBlobId
                     , storeRootIn
                     , uuidFrom
                     ) where

import Data.Bits ((.&.), (.|.))
import Data.Text (Text)
import Data.Word (Word8)
import Numeric (showHex)
import System.FilePath ((</>))

#ifdef PURE_CRYPTO
import qualified System.Random as Random
#else
import qualified Crypto.Random.Entropy as Entropy
#endif
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Org.Index (metaDir)
import Data.Org.Walk (blobFile, orgGlanceDir, storeDir)

-- | The store directory ROOT keeps, whether or not it is there.  One tree, one
-- store: a capture into a served root writes that root's own, never a nested
-- one the walk happens to have found.
storeRootIn :: FilePath -> FilePath
storeRootIn root = root </> orgGlanceDir

-- | Where IDENT's document sits under STORE — @data\/\<2\>\/\<rest\>\/data.org@,
-- and @data\/\<id\>\/data.org@ for an id of two characters or fewer.
--
-- The shard is the first two characters of the WHOLE id and is not folded:
-- org-glance's own store carries @Pa@, @Pe@ and @al@ shards side by side.
blobPathIn :: FilePath -> Text -> FilePath
blobPathIn store ident = under </> blobFile
  where
    under | T.length ident > 2 = data' </> unpack (T.take 2 ident) </> unpack (T.drop 2 ident)
          | otherwise          = data' </> unpack ident
    data' = store </> storeDir
    unpack = T.unpack

-- | Where STORE keeps its index — the directory the scan folds and the one
-- 'Data.Org.External' notes a blob write in.  Beside 'blobPathIn' and reading
-- STORE the same way, so the two addressing rules take the same argument.
metaDirIn :: FilePath -> FilePath
metaDirIn store = store </> metaDir

-- | A fresh @ORG_GLANCE_ID@: a random version-4 UUID, the form
-- @org-id-uuid@ writes.
--
-- No reservation.  org-glance mints by rejection against the directory it then
-- creates; this side lets the WRITE decide, since a blob is created under the
-- EMPTY digest and a path that already holds a file drifts rather than being
-- overwritten.  122 random bits make the collision unreachable either way.
mintBlobId :: IO Text
#ifdef PURE_CRYPTO
-- The pure seam: splitmix seeded by the system, which is randomness enough
-- for an IDENTIFIER — nothing here is a secret, and the 122 bits do the work.
mintBlobId = uuidFrom . fst . Random.uniformByteString 16 <$> Random.initStdGen
#else
mintBlobId = uuidFrom <$> Entropy.getEntropy 16
#endif

-- | BYTES as a version-4 UUID: 36 characters, lowercase hex, @8-4-4-4-12@.
--
-- Pure so the shape can be pinned by a test rather than by a running clock.
-- The version nibble and the variant bits are stamped the way RFC 4122 asks and
-- @org-id-uuid@ writes them, so a byte string short of sixteen is padded out
-- with zeros rather than answering a string of the wrong length.
uuidFrom :: BS.ByteString -> Text
uuidFrom bytes = T.intercalate "-" (map hex [ take' 0 4, take' 4 2, take' 6 2
                                            , take' 8 2, take' 10 6 ])
  where
    padded = BS.take 16 (bytes <> BS.replicate 16 0)
    stamped = [ stamp i b | (i, b) <- zip [0 :: Int ..] (BS.unpack padded) ]
    stamp 6 b = (b .&. 0x0f) .|. 0x40  -- version 4
    stamp 8 b = (b .&. 0x3f) .|. 0x80  -- variant 10xx
    stamp _ b = b
    take' from n = take n (drop from stamped)
    hex = T.concat . map byte
    byte :: Word8 -> Text
    byte b = T.justifyRight 2 '0' (T.pack (showHex b ""))
