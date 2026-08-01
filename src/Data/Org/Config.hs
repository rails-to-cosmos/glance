-- | org-glance's keyword configuration: the files beside a tree that say which
-- words are TODO states, and how a parse and a display each read them.
--
-- org-glance keeps two kinds of file under @\<root\>\/.org-glance\/config@ —
-- @system.org@ for the whole tree and @tags\/TAG.org@ per tag, the file name
-- being the tag.  Both are ordinary org documents and both are mostly an
-- org-capture template; the only thing read here is their @#+TODO:@ lines.
--
-- Two questions come out of that, answered differently on purpose.
--
-- RECOGNITION is a UNION.  A word any layer names parses as a TODO keyword in
-- every file under the root, because the alternative is what the tree did
-- before this existed: @* READING War and Peace@ is a state in a file carrying
-- the pragma and the first word of a title in the file beside it.  The union is
-- computed once per load and threaded into every parse as the seed context
-- ('seedContext'); it accumulates nothing, so the per-file context rule
-- (docs\/invariants.md, Parser) still holds — no file's parse can reach
-- another's.
--
-- CLASSIFICATION is NEAREST SCOPE ('classify').  Whether a recognized keyword
-- is active or done-like is asked of the file's own @#+TODO:@ first, then of
-- the headline's tags in order, then of @system.org@, then of org's own
-- TODO\/DONE, and last of the union itself.  So @READING@ is active because
-- @book.org@ declares it before the bar, and a file that redeclares it after
-- one makes it done-like for its own headlines and nobody else's.
module Data.Org.Config ( ConfigLayers (..)
                       , TodoKeywords (..)
                       , classify
                       , configDirIn
                       , configPaths
                       , declaredKeywords
                       , loadConfigDirs
                       , mergeKeywords
                       , noConfig
                       , noKeywords
                       , seedContext
                       , todoPragmas
                       ) where

import Control.Exception (IOException, try)
import Data.Foldable (asum)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Org.Edit (digestOf)
import Data.Org.Parser (orgParse)
import Data.Org.Types ( Context, Element (EPragma), Pragma (PTodo), Spanned (valueOf)
                      , defaultContext, setTodo )
import Data.Org.Walk (isDocument)

-- Keywords

-- | One layer's TODO keywords: what a @#+TODO:@ line names before the bar and
-- what it names after it, each deduplicated and kept where it first appeared.
--
-- This is the currency the whole feature trades in — a config layer, a file's
-- own declarations, the recognition union and the served badge palette are all
-- one of these.
data TodoKeywords = TodoKeywords
  { tkActive   :: ![Text]
  , tkInactive :: ![Text]
  } deriving (Eq, Show)

-- | A layer that declares nothing.
noKeywords :: TodoKeywords
noKeywords = TodoKeywords [] []

-- | Several layers' keyword sets as one: first-seen order across the list, a
-- keyword declared both ways anywhere counting as active.  Deduplication makes
-- runs irrelevant, so passing one entry per file gives the same answer as
-- passing every row's.
--
-- The active-wins rule is what makes the union answerable: a word @book.org@
-- puts before the bar and @system.org@ puts after it is work as far as the
-- union is concerned, and 'classify' is where the nearer scope gets to
-- disagree.
mergeKeywords :: [TodoKeywords] -> TodoKeywords
mergeKeywords keywords = TodoKeywords actives inactives
  where actives   = declared tkActive
        inactives = filter (`notElem` actives) (declared tkInactive)
        declared f = firstSeen (concatMap f keywords)

-- | XS deduplicated, each element kept where it first appeared.
-- 'Data.List.nub' reads the same and costs O(n · distinct); this merge runs
-- over one entry per file on every @\/headlines@ request, and at 6300 files
-- that quadratic was most of the request.
firstSeen :: Ord a => [a] -> [a]
firstSeen = go Set.empty
  where go _ [] = []
        go seen (x : xs) | Set.member x seen = go seen xs
                         | otherwise         = x : go (Set.insert x seen) xs

-- Reading the pragmas

-- | The keyword sets ELEMS' own @#+TODO:@ pragmas declare.
--
-- A file's OWN declarations, which is what makes this different from the
-- context a parse ends in: that context also holds the seed, and a layer
-- claiming keywords it never wrote would answer for scopes it knows nothing
-- about.
declaredKeywords :: [Spanned Element] -> TodoKeywords
declaredKeywords elems = mergeKeywords
  [ TodoKeywords (copies active) (copies inactive)
  | EPragma (PTodo active inactive) <- map valueOf elems ]
  where copies = map T.copy . Set.toAscList

-- | The keyword sets DOC's @#+TODO:@ lines declare.
--
-- The lines are picked out before the parser sees them, and that is the whole
-- robustness of this module.  A config file is mostly an org-capture template:
-- two of the three tag configs in ~\/sync's own tree fail 'orgParse' outright,
-- over a hyphen inside a COMMENTED @#+TODO:@, and a whole-file parse would drop
-- their keywords without saying so.  Parsing the pragma lines alone still reads
-- the grammar org writes — fast-access keys, the bar, either casing — off a
-- document that cannot fail for a reason having nothing to do with keywords.
--
-- A commented pragma is excluded by construction: @#   #+TODO: …@ opens with
-- @#@ rather than with @#+@.
todoPragmas :: Text -> TodoKeywords
todoPragmas doc = case orgParse defaultContext (T.unlines lines') of
  (_elems, _ctx, Just _err) -> noKeywords
  (elems, _ctx, Nothing)    -> declaredKeywords elems
  where lines' = filter isTodoPragma (T.lines doc)

-- | Does LINE open a @#+TODO:@ pragma?  Folded, since org takes either casing
-- and the parser uppercases the key.
isTodoPragma :: Text -> Bool
isTodoPragma line = "#+todo:" `T.isPrefixOf` T.toLower (T.stripStart line)

-- The layers

-- | What one root's config declares: the system layer, the per-tag layers, the
-- union the parser is seeded with, and which config this is.
--
-- 'clSeed' is derived from the other two and carried rather than recomputed:
-- every file's parse asks for it, and it is a fold over every layer.
data ConfigLayers = ConfigLayers
  { clSystem :: !TodoKeywords            -- ^ @config\/system.org@'s sets; empty when there is no such file.
  , clTags   :: ![(Text, TodoKeywords)]  -- ^ @config\/tags\/TAG.org@'s sets, tag lowercased, in file-name order.
  , clSeed   :: !TodoKeywords            -- ^ the recognition union: every keyword any layer names.
  , clPrint  :: !Text                    -- ^ digest over the config files read, @\"\"@ when none were.
  } deriving (Eq, Show)

-- | No config at all — what a tree with no @.org-glance\/config@ loads as, and
-- what every caller that does not want one passes.  Parsing under it is
-- byte-identical to parsing from 'defaultContext'.
noConfig :: ConfigLayers
noConfig = ConfigLayers noKeywords [] noKeywords ""

-- | Where ROOT would keep its config directory.  For a writer — the settings UI
-- creating @system.org@ — rather than for a reader: a reader is given the
-- directories the walk found, because an org-glance store is not obliged to sit
-- at the root that is being served.  In ~\/sync's own tree it does not: the
-- walk root is @~\/sync@ and the store is @~\/sync\/views\/.org-glance@.
configDirIn :: FilePath -> FilePath
configDirIn root = root </> ".org-glance" </> "config"

-- | What a config directory DIR holds: the system file and the per-tag
-- directory.
configPaths :: FilePath -> (FilePath, FilePath)
configPaths dir = (dir </> "system.org", dir </> "tags")

-- | The layers DIRS declare, as one config.
--
-- DIRS are @.org-glance\/config@ directories, which the walk hands over having
-- declined to enter them ('Data.Org.Walk.foundConfig'): config is input to a
-- parse and never content in a table, so it is read by path, and reading the
-- ones the walk MET is what finds a store nested under the root rather than
-- sitting at it.  Several of them union — recognition is a superset — and where
-- two configure one tag the first in walk order keeps it.
--
-- Every failure is an absence: a missing file, an unreadable one, one that is
-- not UTF-8 and one whose pragma lines do not parse all declare nothing.  A
-- config that cannot be read must not stop a tree from loading, and the tree
-- then loads exactly as it did before there was a config layer.
loadConfigDirs :: [FilePath] -> IO ConfigLayers
loadConfigDirs dirs = combine . concat <$> mapM layersIn dirs
  where
    combine entries = ConfigLayers
      { clSystem = mergeKeywords [ kw | (Nothing, _p, _d, kw) <- entries ]
      , clTags   = firstPerTag [ (tag, kw) | (Just tag, _p, _d, kw) <- entries ]
      , clSeed   = mergeKeywords [ kw | (_tag, _p, _d, kw) <- entries ]
      , clPrint  = fingerprint [ (p, d) | (_tag, p, d, _kw) <- entries ]
      }
    firstPerTag = go Set.empty
      where go _ [] = []
            go seen (e@(tag, _kw) : rest)
              | Set.member tag seen = go seen rest
              | otherwise           = e : go (Set.insert tag seen) rest

-- | What one config directory declares: the system layer and each tag layer,
-- as @(tag, path, digest, keywords)@ with the system layer tagged 'Nothing'.
layersIn :: FilePath -> IO [(Maybe Text, FilePath, Text, TodoKeywords)]
layersIn dir = do
  system <- readLayer systemFile
  names  <- listOrg tagsDir
  tagged <- mapM (\n -> (,) n <$> readLayer (tagsDir </> n)) names
  pure $ [ (Nothing, systemFile, d, kw) | Just (d, kw) <- [system] ]
      <> [ (Just (tagOf n), tagsDir </> n, d, kw) | (n, Just (d, kw)) <- tagged ]
  where (systemFile, tagsDir) = configPaths dir

-- | The @.org@ file names directly in DIR, sorted; none when it is not there.
-- Filtered by the walk's own document rule, so Emacs's sidecars are not layers
-- — the tags directory in ~\/sync holds a live @#alberblanc.org#@ autosave.
listOrg :: FilePath -> IO [FilePath]
listOrg dir = do
  listed <- try (listDirectory dir) :: IO (Either IOException [FilePath])
  pure (either (const []) (sort . filter isDocument) listed)

-- | PATH's keywords and the digest of the bytes they were read from, or
-- 'Nothing' when there is nothing readable there.
readLayer :: FilePath -> IO (Maybe (Text, TodoKeywords))
readLayer path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case raw of
    Left _err -> Nothing
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _err  -> Nothing
      Right text -> Just (digestOf bytes, todoPragmas text)

-- | The tag NAME configures: its base name, folded, since a tag is matched
-- folded everywhere else.
tagOf :: FilePath -> Text
tagOf = T.toLower . T.pack . takeBaseName

-- | Which config ENTRIES are, as one digest over each name and what it held.
-- Empty for no entries at all, so a tree with no config prints as one.
--
-- This is the half of a store's fingerprint that says the PARSE has not moved:
-- the same bytes under a different config are different rows, and a client
-- revalidating across a daemon restart has no generation to tell it so.
fingerprint :: [(FilePath, Text)] -> Text
fingerprint [] = ""
fingerprint entries =
  digestOf (TE.encodeUtf8 (T.unlines [ T.pack p <> "\t" <> d | (p, d) <- entries ]))

-- Using the layers

-- | The context a file under CFG parses from: 'defaultContext' plus every
-- keyword any layer names.
--
-- ONE immutable value per load, threaded into every parse.  The per-file
-- context invariant is about ACCUMULATION — one file's @#+TODO:@ reaching the
-- next file's headlines — and a constant reaches nothing: two files parsed from
-- this seed cannot influence each other, the parallel read stays sound, and a
-- watch re-parsing one file lands where the full load left it.
seedContext :: ConfigLayers -> Context
seedContext cfg
  | null (tkActive seed) && null (tkInactive seed) = defaultContext
  | otherwise = setTodo (Set.fromList (tkActive seed)) (Set.fromList (tkInactive seed))
                        defaultContext
  where seed = clSeed cfg

-- | Is KEYWORD an active state on a headline carrying TAGS, in a file whose own
-- @#+TODO:@ lines declare FILEKW?
--
-- Nearest scope wins, and the chain is the whole of the rule: the file's own
-- declarations, then the headline's tags IN ORDER (the first tag with anything
-- to say about the keyword answers, and a later one disagreeing is ignored),
-- then @system.org@, then org's built-in TODO\/DONE, then the recognition union
-- — which is what answers for a keyword no scope here claims, @READING@ on an
-- untagged headline being the ordinary case of that.
--
-- Total by construction: a keyword that parsed at all came from the union, the
-- file or the built-ins, so the final 'True' is unreachable and defensive.
classify :: ConfigLayers -> TodoKeywords -> [Text] -> Text -> Bool
classify cfg fileKw tags keyword = fromMaybe True (asum scopes)
  where
    scopes  = [ says fileKw, byTag, says (clSystem cfg), builtin, says (clSeed cfg) ]
    byTag   = asum [ says kw | t <- tags, Just kw <- [lookup t (clTags cfg)] ]
    builtin | keyword == "TODO" = Just True
            | keyword == "DONE" = Just False
            | otherwise         = Nothing
    says (TodoKeywords active inactive)
      | keyword `elem` active   = Just True
      | keyword `elem` inactive = Just False
      | otherwise               = Nothing
