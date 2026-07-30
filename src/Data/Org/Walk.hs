-- | Discovery of .org files under a set of roots.  Shared by the CLI scan and
-- the 'Glance.Query' loader so both see the same file set; each adds its own
-- reporting on top.
module Data.Org.Walk ( Found (..)
                     , errText
                     , findOrgFiles
                     , isOrg
                     ) where

import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.Text (Text)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeExtension, (</>))

import qualified Data.Char as Char
import qualified Data.Text as T

-- | What a walk turned up: .org files and the directories it could not read.
-- Both accumulate in reverse; callers sort them.
data Found = Found
  { foundFiles   :: ![FilePath]
  , foundDirErrs :: ![(FilePath, Text)]
  }

emptyFound :: Found
emptyFound = Found [] []

-- | The .org files under ROOTS, with the directories the walk could not list.
-- A root that is a file is kept when its extension says .org; a root that is
-- neither file nor directory is reported as unreadable.
findOrgFiles :: [FilePath] -> IO Found
findOrgFiles = foldM collect emptyFound

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
    Left e      -> pure $! keepDirErr dir (errText e) acc
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

-- | E's rendering, cut to its first line, as a one-line diagnostic.
errText :: Show e => e -> Text
errText = T.stripEnd . T.takeWhile (/= '\n') . T.pack . show
