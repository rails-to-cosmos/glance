{-# LANGUAGE CPP #-}
-- | org-glance's blob store from the writing side: the layout and the id.
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

storeRootIn :: FilePath -> FilePath
storeRootIn root = root </> orgGlanceDir

-- | Where IDENT's document sits under STORE — @data\/\<2\>\/\<rest\>\/data.org@,
-- and @data\/\<id\>\/data.org@ for an id of two characters or fewer.
blobPathIn :: FilePath -> Text -> FilePath
blobPathIn store ident = under </> blobFile
  where
    under | T.length ident > 2 = data' </> unpack (T.take 2 ident) </> unpack (T.drop 2 ident)
          | otherwise          = data' </> unpack ident
    data' = store </> storeDir
    unpack = T.unpack

metaDirIn :: FilePath -> FilePath
metaDirIn store = store </> metaDir

-- | A fresh @ORG_GLANCE_ID@: a random version-4 UUID, @org-id-uuid@'s form.
mintBlobId :: IO Text
#ifdef PURE_CRYPTO
mintBlobId = uuidFrom . fst . Random.uniformByteString 16 <$> Random.initStdGen
#else
mintBlobId = uuidFrom <$> Entropy.getEntropy 16
#endif

-- | BYTES as a version-4 UUID: 36 characters, lowercase hex, @8-4-4-4-12@,
-- padded with zeros when BYTES is short of sixteen.
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
