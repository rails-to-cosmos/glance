-- | Discovery of .org files under a set of roots, and the pool that reads them.
-- What it refuses is CLAUDE.md (Walk): mirrors, blob history, @config@ always.
module Data.Org.Walk ( Found (..)
                     , LoadFailure (..)
                     , WalkOptions (..)
                     , blobFile
                     , claimById
                     , configDir
                     , defaultWalk
                     , errText
                     , findOrgFiles
                     , findOrgFilesWith
                     , isBlob
                     , isConfig
                     , isDerived
                     , isDocument
                     , isWalked
                     , mapFilesConcurrently
                     , orgGlanceDir
                     , orgGlanceRoot
                     , storeDir
                     ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (replicateConcurrently)
import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isSuffixOf, sortOn, tails)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (joinPath, splitDirectories, takeExtension, takeFileName, (</>))
import System.Posix.Files (FileStatus, getFileStatus, getSymbolicLinkStatus, isDirectory, isSymbolicLink)

import Data.Org.Index (metaDir)

import qualified Data.Char as Char
import qualified Data.Text as T

data Found = Found
  { foundFiles   :: ![FilePath]
  , foundDirErrs :: ![(FilePath, Text)]
  , foundDerived :: ![FilePath]
  , foundConfig  :: ![FilePath]
  }

newtype WalkOptions = WalkOptions
  { woIncludeDerived :: Bool  -- ^ enter org-glance's mirror directories too.
  } deriving (Eq, Show)

defaultWalk :: WalkOptions
defaultWalk = WalkOptions False

-- | Which rung of the read-decode-parse ladder a file fell off.
data LoadFailure
  = ReadFailed    -- ^ the bytes could not be read.
  | DecodeFailed  -- ^ the bytes are not valid UTF-8.
  | ParseFailed   -- ^ 'Data.Org.Parser.orgParse' rejected the document, which is all-or-nothing.
  deriving (Eq, Ord, Show)

-- | Derived buffers under a @.org-glance@ directory.  @data@ is absent on purpose.
derivedDirs :: [FilePath]
derivedDirs = ["overviews", metaDir]

-- | org-glance's store layout, spelled once for the rules and callers below.
orgGlanceDir, storeDir, configDir, occurrenceDir, blobFile :: FilePath
orgGlanceDir = ".org-glance"
storeDir = "data"
configDir = "config"
occurrenceDir = "occurrences"
blobFile = "data.org"

-- | What sits under each @.org-glance@ component of PATH, one entry each.  The
-- 'namesOrgGlance' guard is what makes it affordable over ~703k walk entries.
orgGlanceTails :: FilePath -> [[FilePath]]
orgGlanceTails path
  | not (namesOrgGlance path) = []
  | otherwise = [ rest | d : rest <- tails (splitDirectories path), d == orgGlanceDir ]

orgGlanceRoot :: FilePath -> Maybe FilePath
orgGlanceRoot path
  | not (namesOrgGlance path) = Nothing
  | otherwise = listToMaybe [ joinPath (reverse below)
                            | below@(d : _above) <- tails (reverse parts), d == orgGlanceDir ]
  where parts = splitDirectories path

-- | Does PATH spell @.org-glance@ anywhere, as characters?  Spelled out rather
-- than 'Data.List.isInfixOf' for the @Char#@ comparisons — ~0.4 s of the walk.
namesOrgGlance :: FilePath -> Bool
namesOrgGlance ('.' : 'o' : 'r' : 'g' : '-' : 'g' : 'l' : 'a' : 'n' : 'c' : 'e' : _rest) = True
namesOrgGlance (_c : rest) = namesOrgGlance rest
namesOrgGlance [] = False

-- | Is PATH a mirror or a blob's history?  ONE predicate for walk and watch.
isDerived :: FilePath -> Bool
isDerived = any derivedTail . orgGlanceTails

derivedTail :: [FilePath] -> Bool
derivedTail t = mirrorTail t || occurrenceTail t

mirrorTail :: [FilePath] -> Bool
mirrorTail (d : _rest) = d `elem` derivedDirs
mirrorTail []          = False

occurrenceTail :: [FilePath] -> Bool
occurrenceTail (d : rest) = d == storeDir && occurrenceDir `elem` rest
occurrenceTail []         = False

isBlob :: FilePath -> Bool
isBlob path = takeFileName path == blobFile && isCanonical path

-- | Is PATH in org-glance's @config@ area?  @--include-derived@ never lifts it.
isConfig :: FilePath -> Bool
isConfig = any configTail . orgGlanceTails

configTail :: [FilePath] -> Bool
configTail (d : _rest) = d == configDir
configTail []          = False

-- | Is PATH in the canonical store, a blob's history excluded?  An occurrence
-- carries the LIVE entry's id, so ranking it canonical made the pair a tie.
isCanonical :: FilePath -> Bool
isCanonical path = not (any occurrenceTail ts)
                && or [ d == storeDir | d : _ : _rest <- ts ]
  where ts = orgGlanceTails path

beatsForId :: FilePath -> FilePath -> Bool
beatsForId a b = isCanonical a && not (isCanonical b)

claimById :: FilePath -> FilePath -> (Bool, (FilePath, FilePath))
claimById challenger held
  | beatsForId challenger held = (True, (challenger, held))
  | otherwise                  = (False, (held, challenger))

emptyFound :: Found
emptyFound = Found [] [] [] []

findOrgFiles :: [FilePath] -> IO Found
findOrgFiles = findOrgFilesWith defaultWalk

findOrgFilesWith :: WalkOptions -> [FilePath] -> IO Found
findOrgFilesWith opts = foldM (collect opts) emptyFound

collect :: WalkOptions -> Found -> FilePath -> IO Found
collect opts acc root = do
  isDir <- doesDirectoryExist root
  if isDir
    then walk opts acc root
    else do
      isFile <- doesFileExist root
      pure $! case (isFile, isDocument root) of
        (True, True)  -> keepFile root acc
        (True, False) -> acc
        (False, _)    -> keepDirErr root "no such file or directory" acc

walk :: WalkOptions -> Found -> FilePath -> IO Found
walk opts acc dir = do
  listed <- try (listDirectory dir) :: IO (Either IOException [FilePath])
  case listed of
    Left e      -> pure $! keepDirErr dir (errText e) acc
    Right names -> foldM (visit opts dir) acc names

-- | Classify NAME inside DIR: recurse, keep, or ignore.  The accumulator is
-- forced per entry, and ONE @lstat@ classifies it — a symlink pays a second.
visit :: WalkOptions -> FilePath -> Found -> FilePath -> IO Found
visit opts dir acc name = do
  probe <- try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus)
  case probe of
    Right st | isDirectory st    -> maybe (walk opts acc path) pure declined
             | isSymbolicLink st -> linked
    _notADirectory               -> pure $! file
  where
    path = dir </> name
    -- ONE split per entry, read by both decline rules.
    ts = orgGlanceTails path
    config = any configTail ts
    derived = not (woIncludeDerived opts) && any derivedTail ts
    document = isDocument path
    -- A symlinked directory is never followed: link loops, and one tree twice.
    declined | config    = Just $! keepConfig path acc
             | derived   = Just $! keepDerived path acc
             | otherwise = Nothing
    linked | config || derived || document = do
               target <- try (getFileStatus path) :: IO (Either IOException FileStatus)
               case target of
                 Right st | isDirectory st -> pure $! fromMaybe acc declined
                 _missingOrFile            -> pure $! file
           | otherwise = pure acc
    -- 'isWalked' is this conjunction; asking it here rescans the path per entry.
    file = if document && not (config || derived) then keepFile path acc else acc

keepFile :: FilePath -> Found -> Found
keepFile path acc = acc { foundFiles = path : foundFiles acc }

keepDirErr :: FilePath -> Text -> Found -> Found
keepDirErr path why acc = acc { foundDirErrs = (path, why) : foundDirErrs acc }

keepDerived :: FilePath -> Found -> Found
keepDerived path acc = acc { foundDerived = path : foundDerived acc }

keepConfig :: FilePath -> Found -> Found
keepConfig path acc = acc { foundConfig = path : foundConfig acc }

-- Reading what the walk found

-- | ACT over PATHS on a pool of 'getNumCapabilities' workers, answering in
-- PATHS' order.  Every file parses from 'defaultContext'; ACT must force.
mapFilesConcurrently :: (FilePath -> IO a) -> [FilePath] -> IO [a]
mapFilesConcurrently act paths = case paths of
  []    -> pure []
  [one] -> (: []) <$> act one
  _more -> do
    width <- getNumCapabilities
    if width <= 1 then mapM act paths else pooled width
  where
    pooled width = do
      queue <- newIORef (zip [0 :: Int ..] paths)
      taken <- replicateConcurrently width (drain queue)
      pure (map snd (sortOn fst (concat taken)))
    drain queue = go []
      where go acc = do
              next <- atomicModifyIORef' queue pop
              case next of
                Nothing        -> pure acc
                Just (i, path) -> do y <- act path
                                     go ((i, y) : acc)
    pop []            = ([], Nothing)
    pop (item : more) = (more, Just item)

-- | Would the walk collect PATH?  ALL THREE predicates matter (CLAUDE.md, Walk).
isWalked :: FilePath -> Bool
isWalked path = isDocument path && not (any declined (orgGlanceTails path))
  where declined t = configTail t || derivedTail t

-- | Is PATH a file this walk reads?  The watch asks the same of an inotify event.
isDocument :: FilePath -> Bool
isDocument path = isOrg path && not (isSidecar path)

isOrg :: FilePath -> Bool
isOrg path = map Char.toLower (takeExtension path) == ".org"

-- | Is PATH an Emacs sidecar?  BOTH SHAPES EXACT — a bare @#@ hid @#inbox.org@.
isSidecar :: FilePath -> Bool
isSidecar path = take 2 name == ".#" || (take 1 name == "#" && "#" `isSuffixOf` name)
  where name = takeFileName path

errText :: Show e => e -> Text
errText = T.stripEnd . T.takeWhile (/= '\n') . T.pack . show
