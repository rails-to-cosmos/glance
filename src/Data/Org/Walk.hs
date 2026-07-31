-- | Discovery of .org files under a set of roots.  Shared by the CLI scan and
-- the 'Glance.Query' loader so both see the same file set; each adds its own
-- reporting on top.
--
-- One thing the walk refuses by default: org-glance's own derived mirrors.
-- @org-glance@ keeps a canonical store under @.org-glance\/data\/@ and writes
-- overview and agenda buffers beside it under @.org-glance\/overviews\/@ and
-- @.org-glance\/meta\/@, which repeat the same headlines — same
-- @ORG_GLANCE_ID@, different file.  Serving those is serving a derived artifact
-- as truth: one headline turns up twice in a table, and its second copy comes
-- out of a file nobody edits.  So the two mirror directories are skipped and
-- @data@ is kept, and @--include-derived@ turns the whole exclusion off for
-- someone who wants to look at them.
module Data.Org.Walk ( Found (..)
                     , WalkOptions (..)
                     , beatsForId
                     , defaultWalk
                     , errText
                     , findOrgFiles
                     , findOrgFilesWith
                     , isCanonical
                     , isDerived
                     , isOrg
                     ) where

import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.List (tails)
import Data.Text (Text)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (splitDirectories, takeExtension, (</>))

import qualified Data.Char as Char
import qualified Data.Text as T

-- | What a walk turned up: .org files, the directories it could not read, and
-- the derived directories it declined to enter.  All three accumulate in
-- reverse; callers sort them.
data Found = Found
  { foundFiles   :: ![FilePath]
  , foundDirErrs :: ![(FilePath, Text)]
  , foundDerived :: ![FilePath]
  }

-- | What a walk covers besides the plain tree.
newtype WalkOptions = WalkOptions
  { woIncludeDerived :: Bool  -- ^ enter org-glance's mirror directories too.
  } deriving (Eq, Show)

-- | Org files as the source of truth: the mirrors stay out.
defaultWalk :: WalkOptions
defaultWalk = WalkOptions False

-- | The directories under a @.org-glance@ one that hold derived buffers.
-- @data@ is absent on purpose — it is the canonical store.
derivedDirs :: [FilePath]
derivedDirs = ["overviews", "meta"]

-- | What sits under each @.org-glance@ directory PATH passes through: one entry
-- per such directory, already past it.  The two rules below differ only in the
-- arity of the pattern they ask of a tail.
orgGlanceTails :: FilePath -> [[FilePath]]
orgGlanceTails path = [ rest | ".org-glance" : rest <- tails (splitDirectories path) ]

-- | Is PATH inside an org-glance mirror — one of 'derivedDirs' sitting directly
-- under a @.org-glance@ directory?  Takes the whole path, so it answers for a
-- directory the walk is about to enter and for a file the watch was told about
-- alike.
isDerived :: FilePath -> Bool
isDerived path = or [ d `elem` derivedDirs | d : _rest <- orgGlanceTails path ]

-- | Is PATH inside org-glance's canonical store?  The one directory under
-- @.org-glance@ holding documents rather than renders of them, so a headline
-- from it outranks one from anywhere else claiming its @ORG_GLANCE_ID@
-- ('beatsForId').  One component deeper than 'isDerived' asks for: the store
-- directory is not itself a document in it.
isCanonical :: FilePath -> Bool
isCanonical path = or [ d == "data" | d : _ : _rest <- orgGlanceTails path ]

-- | Does A outrank B as the file that keeps an @ORG_GLANCE_ID@ both claim?
-- Only a canonical path beats a non-canonical one; every other pairing leaves
-- the incumbent, which is walk order and is what the view was showing before.
-- The scan report and 'Glance.Query.resolveIds' read this one rule, so the two
-- name the same winner.
beatsForId :: FilePath -> FilePath -> Bool
beatsForId a b = isCanonical a && not (isCanonical b)

emptyFound :: Found
emptyFound = Found [] [] []

-- | The .org files under ROOTS, org-glance's mirrors excluded.
findOrgFiles :: [FilePath] -> IO Found
findOrgFiles = findOrgFilesWith defaultWalk

-- | The .org files under ROOTS as OPTS asks for them, with the directories the
-- walk could not list and the derived ones it skipped.  A root that is a file
-- is kept when its extension says .org; a root that is neither file nor
-- directory is reported as unreadable.
findOrgFilesWith :: WalkOptions -> [FilePath] -> IO Found
findOrgFilesWith opts = foldM (collect opts) emptyFound

-- | Add ROOT's .org files to ACC, walking it when it is a directory.
collect :: WalkOptions -> Found -> FilePath -> IO Found
collect opts acc root = do
  isDir <- doesDirectoryExist root
  if isDir
    then walk opts acc root
    else do
      isFile <- doesFileExist root
      pure $! case (isFile, isOrg root) of
        (True, True)  -> keepFile root acc
        (True, False) -> acc
        (False, _)    -> keepDirErr root "no such file or directory" acc

-- | Collect .org files under DIR, recursing into real subdirectories only.
walk :: WalkOptions -> Found -> FilePath -> IO Found
walk opts acc dir = do
  listed <- try (listDirectory dir) :: IO (Either IOException [FilePath])
  case listed of
    Left e      -> pure $! keepDirErr dir (errText e) acc
    Right names -> foldM (visit opts dir) acc names

-- | Classify NAME inside DIR: recurse, keep, or ignore.  The accumulator is
-- forced at every entry: a thunk per entry would retain the whole tree.  A
-- named root is walked whatever it is, so pointing the walk straight at a
-- mirror still reads it — the exclusion is about what a tree contains.
visit :: WalkOptions -> FilePath -> Found -> FilePath -> IO Found
visit opts dir acc name = do
  isDir <- doesDirectoryExist path
  if isDir
    then if skip then pure $! keepDerived path acc else do
      link <- try (pathIsSymbolicLink path) :: IO (Either IOException Bool)
      case link of
        Right False -> walk opts acc path
        _symlink    -> pure acc
    else pure $! if isOrg path && not skip then keepFile path acc else acc
  where path = dir </> name
        skip = not (woIncludeDerived opts) && isDerived path

keepFile :: FilePath -> Found -> Found
keepFile path acc = acc { foundFiles = path : foundFiles acc }

keepDirErr :: FilePath -> Text -> Found -> Found
keepDirErr path why acc = acc { foundDirErrs = (path, why) : foundDirErrs acc }

keepDerived :: FilePath -> Found -> Found
keepDerived path acc = acc { foundDerived = path : foundDerived acc }

isOrg :: FilePath -> Bool
isOrg path = map Char.toLower (takeExtension path) == ".org"

-- | E's rendering, cut to its first line, as a one-line diagnostic.
errText :: Show e => e -> Text
errText = T.stripEnd . T.takeWhile (/= '\n') . T.pack . show
