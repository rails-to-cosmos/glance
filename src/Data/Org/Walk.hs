-- | Discovery of .org files under a set of roots, and the pool that reads them.
-- Shared by the CLI scan and the 'Glance.Query' loader so both see one file set
-- and cross it the same way.
--
-- THREE THINGS THE WALK REFUSES.
--
-- org-glance's derived MIRRORS, by default: @.org-glance\/overviews\/@ and
-- @\/meta\/@ repeat the same headlines under the same @ORG_GLANCE_ID@, so
-- serving them serves a derived artifact as truth — one headline twice, its
-- second copy from a file nobody edits.  @data@ is kept.  @--include-derived@
-- turns the exclusion off.
--
-- A blob's own HISTORY, one level deeper:
-- @data\/\<id\>\/occurrences\/\<STAMP\>.org@ carries the LIVE entry's id,
-- so keeping @data@ kept it and 'beatsForId' called the pair a tie — walk order
-- then decided which copy a table showed and a command wrote to.
--
-- And @.org-glance\/config\/@, ALWAYS: those files are INPUT to a parse rather
-- than content in a table, and @--include-derived@ does not reach them —
-- nothing about them is derived, so nothing becomes truth by asking louder.
module Data.Org.Walk ( Found (..)
                     , LoadFailure (..)
                     , WalkOptions (..)
                     , beatsForId
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

-- | What a walk turned up: .org files, the directories it could not read, and
-- the derived and config directories it declined to enter.  All four
-- accumulate in reverse; callers sort them.
data Found = Found
  { foundFiles   :: ![FilePath]
  , foundDirErrs :: ![(FilePath, Text)]
  , foundDerived :: ![FilePath]
  , foundConfig  :: ![FilePath]
  }

-- | What a walk covers besides the plain tree.
newtype WalkOptions = WalkOptions
  { woIncludeDerived :: Bool  -- ^ enter org-glance's mirror directories too.
  } deriving (Eq, Show)

-- | Org files as the source of truth: the mirrors stay out.
defaultWalk :: WalkOptions
defaultWalk = WalkOptions False

-- | Which rung of the read-decode-parse ladder a file fell off, and so why it
-- yielded no elements.  Here rather than beside either reader because both of
-- them climb 'Data.Org.Edit.readParsed': a load reports these as counts and the
-- corpus scan as counted samples, and the two must be counting the same three
-- things.  No reason travels in the constructor — the store compares load
-- outcomes for equality to decide whether a file MOVED, and two unreadable
-- moments differing only in their @errno@ text are the same outcome.
data LoadFailure
  = ReadFailed    -- ^ the bytes could not be read.
  | DecodeFailed  -- ^ the bytes are not valid UTF-8.
  | ParseFailed   -- ^ 'Data.Org.Parser.orgParse' rejected the document, which is all-or-nothing.
  deriving (Eq, Ord, Show)

-- | The directories under a @.org-glance@ one that hold derived buffers.
-- @data@ is absent on purpose — it is the canonical store.
--
-- The index directory is 'Data.Org.Index.metaDir' rather than a second spelling
-- of the same name: that module owns the store layout, and a walk that declined
-- a directory the index no longer wrote to would be excluding nothing while
-- reading as though it were.
derivedDirs :: [FilePath]
derivedDirs = ["overviews", metaDir]

-- | org-glance's store layout, as the five names the rules below ask for: the
-- directory a store puts everything under, the canonical store directory inside
-- it, the directory of keyword and template configuration beside it, the one
-- directory inside a blob that is history rather than the blob, and the file a
-- blob keeps its entry in.
--
-- 'orgGlanceDir', 'storeDir', 'configDir' and 'blobFile' are exported for the
-- callers that ADDRESS a store rather than classify a path — the settings
-- writer naming a config directory, the scan selecting the blobs an index
-- covers, 'Data.Org.Blob' composing the path a capture writes.  A second
-- spelling there is a green lie: rename the directory here and a hardcoded
-- @data@ keeps matching nothing while every predicate below still collects, so
-- the comparison reports zero disagreements over zero blobs.
orgGlanceDir, storeDir, configDir, occurrenceDir, blobFile :: FilePath
orgGlanceDir = ".org-glance"
storeDir = "data"
configDir = "config"
occurrenceDir = "occurrences"
blobFile = "data.org"

-- | What sits under each @.org-glance@ directory PATH passes through: one entry
-- per such directory, already past it.  Three of the four rules below differ
-- only in the arity of the pattern they ask of a tail; 'isOccurrence' asks for
-- a name anywhere along one, for the reason its own note gives.
--
-- The guard is what makes this affordable in the walk, which asks it twice
-- per entry over ~703k of them — 'isConfig', then 'isDerived', which folds the
-- occurrence question into its one call: splitting a path allocates a list of
-- strings and
-- 'tails' allocates a list of those, and a tree with no @.org-glance@ component
-- anywhere pays that for an answer that is always @[]@.  'namesOrgGlance' is a
-- character scan with no allocation at all, and a path it rejects cannot
-- contain the component the split is looking for — so the fast exit answers
-- exactly what the split would have.
orgGlanceTails :: FilePath -> [[FilePath]]
orgGlanceTails path
  | not (namesOrgGlance path) = []
  | otherwise = [ rest | d : rest <- tails (splitDirectories path), d == orgGlanceDir ]

-- | The @.org-glance@ directory PATH sits inside, or 'Nothing' when it sits in
-- none.  'orgGlanceTails' with the other half kept: that one answers what is
-- BELOW the component, this one names the component itself, so a caller holding
-- a blob can address its store's own @meta@ directory
-- ('Data.Org.External.externalPathOf' is the caller).
--
-- The INNERMOST one wins where a path spells the component twice, which is the
-- store a file inside it belongs to; and the answer is textual, over the path as
-- the walk built it, exactly as every other rule here — nothing canonicalizes.
orgGlanceRoot :: FilePath -> Maybe FilePath
orgGlanceRoot path
  | not (namesOrgGlance path) = Nothing
  | otherwise = listToMaybe [ joinPath (reverse below)
                            | below@(d : _above) <- tails (reverse parts), d == orgGlanceDir ]
  where parts = splitDirectories path

-- | Does PATH spell @.org-glance@ anywhere in it, as characters rather than as
-- a component?  A necessary condition for 'orgGlanceTails' to find anything,
-- and the cheap half of it.
--
-- Spelled out rather than as @\".org-glance\" \`isInfixOf\` path@, and the
-- reason is speed rather than allocation: at @-O1@ GHC fuses
-- 'Data.List.isInfixOf' well enough that both allocate nothing per call, but
-- its loop still reaches @isPrefixOf@ through the @Eq Char@ dictionary at every
-- position, where the literal match above compiles to direct @Char#@
-- comparisons.  Measured ~1.8x per call in isolation, and end to end over
-- ~\/sync's 702k entries: 9.08 s of walk this way, 9.51 s through
-- 'Data.List.isInfixOf', 10.64 s with no guard at all.  Put the library call
-- back and a third of the guard's value goes with it.
namesOrgGlance :: FilePath -> Bool
namesOrgGlance ('.' : 'o' : 'r' : 'g' : '-' : 'g' : 'l' : 'a' : 'n' : 'c' : 'e' : _rest) = True
namesOrgGlance (_c : rest) = namesOrgGlance rest
namesOrgGlance [] = False

-- | Is PATH inside an org-glance mirror — one of 'derivedDirs' sitting directly
-- under a @.org-glance@ directory, or a blob's 'isOccurrence' history?  Takes
-- the whole path, so it answers for a directory the walk is about to enter and
-- for a file the watch was told about alike.
--
-- The occurrences half rides here rather than in a rule of its own so that ONE
-- predicate answers for the walk and for the watch — @Glance.Query.derivedPath@
-- is this function, and a file the walk never collected must never arrive by
-- inotify.  It follows @--include-derived@ with the mirrors, which is the flag
-- meaning what it says: on, the walk reads everything org-glance wrote, history
-- included, and an occurrence ties with its live blob again.
isDerived :: FilePath -> Bool
isDerived = any derivedTail . orgGlanceTails

-- | 'isDerived' over ONE tail: a mirror directory or a blob's history.  The
-- rule itself, so the two callers that already hold a split — 'visit' and
-- 'isWalked' — read it instead of splitting the path again.
derivedTail :: [FilePath] -> Bool
derivedTail t = mirrorTail t || occurrenceTail t

-- | The mirror half of 'derivedTail' over ONE tail.
mirrorTail :: [FilePath] -> Bool
mirrorTail (d : _rest) = d `elem` derivedDirs
mirrorTail []          = False

-- | Is PATH one of a blob's occurrence snapshots — anything under an
-- @occurrences@ directory inside the canonical store?
--
-- Depth is left open on purpose.  org-glance shards a blob directory by the
-- id's first two characters (@data\/\<2\>\/\<rest\>@) and falls back to
-- @data\/\<id\>@ for an id of two characters or fewer, so the history sits one
-- component deeper in the usual case than in the degenerate one, and no
-- position test covers both.  The cost of asking for the NAME anywhere under
-- @data@ is that a blob whose sharded remainder spells exactly @occurrences@
-- would be declined as history; that path is indistinguishable from a
-- two-character id's history by the path alone, so no rule can tell them apart.
isOccurrence :: FilePath -> Bool
isOccurrence = any occurrenceTail . orgGlanceTails

-- | 'isOccurrence' over ONE tail: the rule itself, factored so 'isDerived' and
-- 'isCanonical' can ask it of a tails list they already hold — each used to
-- call 'isOccurrence' whole and pay 'orgGlanceTails' a second time for it.
occurrenceTail :: [FilePath] -> Bool
occurrenceTail (d : rest) = d == storeDir && occurrenceDir `elem` rest
occurrenceTail []         = False

-- | Is PATH a blob's stored document — @data.org@ inside 'isCanonical'?  The
-- one file per entry org-glance writes its contents to, and what the scan's
-- index comparison reads a blob's state off.
isBlob :: FilePath -> Bool
isBlob path = takeFileName path == blobFile && isCanonical path

-- | Is PATH inside org-glance's config area — a @config@ directory sitting
-- directly under a @.org-glance@ one?  The same shape as 'isDerived' and a
-- different question: these files are read, by @Data.Org.Config@ and by path,
-- and they are never rows.  Unconditional, so @--include-derived@ leaves it
-- standing.
isConfig :: FilePath -> Bool
isConfig = any configTail . orgGlanceTails

-- | 'isConfig' over ONE tail, beside 'derivedTail' and for the same callers.
configTail :: [FilePath] -> Bool
configTail (d : _rest) = d == configDir
configTail []          = False

-- | Is PATH inside org-glance's canonical store?  The one directory under
-- @.org-glance@ holding documents rather than renders of them, so a headline
-- from it outranks one from anywhere else claiming its @ORG_GLANCE_ID@
-- ('beatsForId').  One component deeper than 'isDerived' asks for: the store
-- directory is not itself a document in it.
--
-- A blob's history is inside the store and is not it: an occurrence carries the
-- LIVE entry's id, so ranking it canonical made the pair a tie and handed the
-- id to whichever the walk saw first.  The walk no longer offers one, and this
-- says what the answer would be under @--include-derived@, which does.
isCanonical :: FilePath -> Bool
isCanonical path = not (any occurrenceTail ts)
                && or [ d == storeDir | d : _ : _rest <- ts ]
  where ts = orgGlanceTails path

-- | Does A outrank B as the file that keeps an @ORG_GLANCE_ID@ both claim?
-- Only a canonical path beats a non-canonical one; every other pairing leaves
-- the incumbent, which is walk order and is what the view was showing before.
-- The scan report and 'Glance.Query.resolveIds' read this one rule, so the two
-- name the same winner.
beatsForId :: FilePath -> FilePath -> Bool
beatsForId a b = isCanonical a && not (isCanonical b)

-- | CHALLENGER against the INCUMBENT holding an id both claim: whether the
-- challenger took it, and the pair a report names — @(kept, dropped)@.
--
-- 'beatsForId' read as the fold step both id indexes are built by, so the scan's
-- collision count and 'Glance.Query.resolveIds' cannot come to disagree about a
-- winner or about which file a report blames.  A file against itself leaves the
-- incumbent, which is what keeps the FIRST of two headlines in one file.
claimById :: FilePath -> FilePath -> (Bool, (FilePath, FilePath))
claimById challenger held
  | beatsForId challenger held = (True, (challenger, held))
  | otherwise                  = (False, (held, challenger))

emptyFound :: Found
emptyFound = Found [] [] [] []

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
      pure $! case (isFile, isDocument root) of
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
-- forced at every entry — a thunk per entry would retain the whole tree.  A
-- NAMED root is walked whatever it is: the exclusion is about what a tree
-- contains.  Config outranks derived where a path is both, which it cannot be;
-- they are asked apart so each declined directory lands in the count that
-- explains it.
--
-- ONE STAT AN ENTRY, and it is @lstat@, which never follows — the entry count
-- is what a walk costs, and the pair this replaced was most of the wall.  A
-- symlink pays a second, following stat to classify its target, and only when
-- the answer could change the accumulator: a link neither named like a document
-- nor inside a declined directory adds nothing whatever it points at, which is
-- where Emacs's lock exits.
--
-- What the two say: a real directory is entered, a symlinked one never is, a
-- symlinked FILE is kept on its name, and a link whose target is missing reads
-- as a non-directory — so a dangling @.org@ link is walked and fails later as
-- @ReadFailed@.  A failed @lstat@ lands in the same branch, silently.
visit :: WalkOptions -> FilePath -> Found -> FilePath -> IO Found
visit opts dir acc name = do
  probe <- try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus)
  case probe of
    Right st | isDirectory st    -> maybe (walk opts acc path) pure declined
             | isSymbolicLink st -> linked
    _notADirectory               -> pure $! file
  where
    path = dir </> name
    -- ONE split per entry, read by both decline rules: the walk asks this of
    -- ~703k entries and 'orgGlanceTails' allocates a list of lists.
    ts = orgGlanceTails path
    config = any configTail ts
    derived = not (woIncludeDerived opts) && any derivedTail ts
    document = isDocument path
    -- What a directory's NAME alone decides, counted where it is declined and
    -- 'Nothing' where the walk may go in.  A symlinked directory reads the same
    -- answer and then stops whatever it says, never being followed: the reason
    -- is a link loop, and counting one tree twice.
    declined | config    = Just $! keepConfig path acc
             | derived   = Just $! keepDerived path acc
             | otherwise = Nothing
    linked | config || derived || document = do
               target <- try (getFileStatus path) :: IO (Either IOException FileStatus)
               case target of
                 Right st | isDirectory st -> pure $! fromMaybe acc declined
                 _missingOrFile            -> pure $! file
           | otherwise = pure acc
    -- 'isWalked' is this same conjunction for a caller holding only a path.
    -- It is not called HERE because the three predicates are already in hand:
    -- asking again would scan the path three more times per entry, which is the
    -- cost docs/invariants.md declines to pay for sharing one of them.
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

-- | ACT over each of PATHS on a pool of 'getNumCapabilities' workers, answering
-- in the order PATHS gave them whatever order they finished.  The loader and
-- the CLI scan share this one pool; the walk stays serial, and a single path
-- skips the pool, which is the file watch's re-read.
--
-- WHY THIS PARALLELIZES: there is no shared parse state to race over.  Every
-- file is parsed from 'defaultContext' — the per-file context invariant — so
-- one file's @#+TODO:@ cannot reach another's headlines whichever thread reads
-- it, and nothing in the parser or the AST is mutable.
--
-- ORDER: workers pull from one queue and tag what they took, so the answer is
-- reassembled by index and is record for record a serial run's.  Id resolution,
-- the served row order and the scan's capped listings all read that sequence.
--
-- FORCING: ACT must return a value it has already forced, so a worker's
-- transient document dies with the file rather than in the caller's fold, and
-- in-flight memory is the pool's width times one document.
mapFilesConcurrently :: (FilePath -> IO a) -> [FilePath] -> IO [a]
mapFilesConcurrently act paths = case paths of
  []    -> pure []
  [one] -> (: []) <$> act one
  _more -> do
    width <- getNumCapabilities
    if width <= 1 then mapM act paths else pooled width
  where
    -- Workers past the last path find the queue empty and stop, so a tree with
    -- fewer files than capabilities needs no special case.
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

-- | Would the walk collect PATH — a document that is neither org-glance's
-- config nor one of its derived mirrors?
--
-- 'visit''s own conjunction, for a caller that holds a path and nothing else.
-- The one difference is the option: @--include-derived@ widens the walk and
-- cannot widen this, since a caller asking of a bare path has no options to
-- consult, so the answer here is the strict one.
--
-- Written down because ALL THREE predicates matter and stopping at 'isDocument'
-- gets it wrong in a way nothing reports: @.org-glance\/config\/x.org@ and
-- @.org-glance\/overviews\/x.org@ are org files the walk never collects, so a
-- caller that wrote to one would be writing where no row ever comes from
-- ('Data.Org.Config.captureTargetIn' is that caller).
isWalked :: FilePath -> Bool
isWalked path = isDocument path && not (any declined (orgGlanceTails path))
  where declined t = configTail t || derivedTail t

-- | Is PATH a file this walk reads?  An @.org@ name that is not one of Emacs's
-- sidecars.  The watch asks this same question of an inotify event, through the
-- facade re-export 'Glance.Query.documentPath', so the two sides cannot
-- disagree about what a document is — a file the walk skips can never arrive by
-- inotify, and a file it reads is never one no event can refresh.
isDocument :: FilePath -> Bool
isDocument path = isOrg path && not (isSidecar path)

isOrg :: FilePath -> Bool
isOrg path = map Char.toLower (takeExtension path) == ".org"

-- | Is PATH one of the two sidecars Emacs writes beside a buffer — the lock
-- @.#name.org@ and the auto-save @#name.org#@?  Neither is a document, and the
-- lock is the one that has to be named here: it is a symlink to
-- @user\@host.pid@ that usually dangles, and its extension is @.org@, so a walk
-- keeping files on 'isOrg' alone reads it as a document and 'Glance.Query.loadFile'
-- answers @ReadFailed@ — for the life of the process, since the watch filters
-- the path out and no event ever arrives to clear it.  The auto-save fails
-- 'isOrg' already and is named for the pair to be one rule.
--
-- BOTH SHAPES ARE EXACT.  The auto-save wants the closing @#@ as well as the
-- opening one: a leading @#@ alone took every @.org@ file whose name starts with
-- one, so a hand-written @#inbox.org@ was silently invisible to the walk, the
-- watch and a capture target naming it.
isSidecar :: FilePath -> Bool
isSidecar path = take 2 name == ".#" || (take 1 name == "#" && "#" `isSuffixOf` name)
  where name = takeFileName path

-- | E's rendering, cut to its first line, as a one-line diagnostic.
errText :: Show e => e -> Text
errText = T.stripEnd . T.takeWhile (/= '\n') . T.pack . show
