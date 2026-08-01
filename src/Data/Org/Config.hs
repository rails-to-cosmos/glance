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
module Data.Org.Config ( ConfigLayerFile (..)
                       , ConfigLayers (..)
                       , TodoKeywords (..)
                       , classify
                       , configDirIn
                       , configPaths
                       , declaredKeywords
                       , isTodoPragma
                       , loadConfigDirs
                       , mergeKeywords
                       , noConfig
                       , noKeywords
                       , readConfigLayers
                       , seedContext
                       , todoLineEdits
                       , todoLines
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
import Data.Org.Types ( Context, Element (EPragma), Pragma (PTodo), Span (..), Spanned (valueOf)
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
firstSeen = firstBy id

-- | XS with the first entry under each KEY kept and every later one dropped.
-- Two callers want the same fold under different keys: the keyword merge
-- deduplicates by the word itself, and the tag layers by the tag, where a
-- second config directory naming a tag the first already configured loses it.
firstBy :: Ord k => (a -> k) -> [a] -> [a]
firstBy key = go Set.empty
  where go _ [] = []
        go seen (x : xs) | Set.member k seen = go seen xs
                         | otherwise         = x : go (Set.insert k seen) xs
          where k = key x

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
todoPragmas doc = case orgParse defaultContext (T.unlines (todoLines doc)) of
  (_elems, _ctx, Just _err) -> noKeywords
  (elems, _ctx, Nothing)    -> declaredKeywords elems

-- | DOC's @#+TODO:@ lines, verbatim and in file order.  What 'todoPragmas'
-- reads and what a settings client edits are the same lines, so both go
-- through this.
todoLines :: Text -> [Text]
todoLines = filter isTodoPragma . T.lines

-- | Does LINE open a @#+TODO:@ pragma?  Folded, since org takes either casing
-- and the parser uppercases the key.
isTodoPragma :: Text -> Bool
isTodoPragma line = "#+todo:" `T.isPrefixOf` T.toLower (T.stripStart line)

-- Editing the pragmas

-- | The span edits putting LINES where DOC's @#+TODO:@ lines are: char spans
-- into DOC and the text to write over each, the currency
-- 'Data.Org.Edit.applyEdits' takes.
--
-- One block, wherever the first pragma line was.  A document already carrying
-- them keeps that offset and loses every later one, so a file spelling its
-- cycle over two lines comes back as whatever LINES spells and nothing is left
-- behind further down.  A document carrying none takes the block after its
-- leading @#@ run — the @#+TITLE:@ and the comments org-glance's own templates
-- open with — which is where org would have put it, and at the top when there
-- is no such run.  An empty LINES writes nothing and deletes what is there,
-- which is how a layer is taken off.
--
-- Every span covers a WHOLE line, the newline that ends it included, so
-- nothing outside the lines this rewrites can move.
todoLineEdits :: Text -> [Text] -> [(Span, Text)]
todoLineEdits doc new = case [ sp | (sp, line) <- lines', isTodoPragma line ] of
  []          -> [ (Span at at, opening <> block) | not (null new) ]
  (sp : rest) -> (sp, block) : [ (r, "") | r <- rest ]
  where
    lines' = lineSpans doc
    block  = if null new then "" else T.unlines new
    -- Past the header the file opens with, or at the very top when it opens
    -- with content; a file that is nothing but header takes it at the end.
    at = case dropWhile (T.isPrefixOf "#" . snd) lines' of
      ((sp, _line) : _rest) -> spanStart sp
      []                    -> T.length doc
    -- Every other insertion point is the start of a line, so the character
    -- before it is a newline by construction.  The all-header case is the
    -- exception: it lands at the end of a document that need not close with
    -- one, and a block appended to a live line is not a pragma at all.
    opening | at > 0, not ("\n" `T.isSuffixOf` T.take at doc) = "\n"
            | otherwise                                       = ""

-- | DOC's lines, each with the span covering it and the newline that ends it.
-- A final line with no newline still gets a span, ending at the document.
lineSpans :: Text -> [(Span, Text)]
lineSpans = go 0
  where
    go at rest
      | T.null rest = []
      | otherwise   = (Span at end, line) : go end more
      where (line, tailed) = T.break (== '\n') rest
            more = T.drop 1 tailed
            end  = at + T.length line + (if T.null tailed then 0 else 1)

-- The layers

-- | What one root's config declares: the system layer, the per-tag layers, the
-- union the parser is seeded with, and which config this is.
--
-- 'clSeed' is stored rather than derived from the other two, and it is not
-- recoverable from them: 'clTags' keeps the FIRST configuration of each tag
-- across the config directories the walk met, while the seed unions every entry
-- read, the shadowed ones included.  Recognition is a superset — a keyword a
-- losing tag file names still parses as a state — and only classification
-- picks a winner.
data ConfigLayers = ConfigLayers
  { clSystem :: !TodoKeywords            -- ^ @config\/system.org@'s sets; empty when there is no such file.
  , clTags   :: ![(Text, TodoKeywords)]  -- ^ @config\/tags\/TAG.org@'s sets, tag lowercased, in file-name order.
  , clSeed   :: !TodoKeywords            -- ^ the recognition union: every keyword any layer names.
  , clPrint  :: !Text                    -- ^ digest over the config files read, @\"\"@ when none were.
  , clDirs   :: ![FilePath]              -- ^ the config directories these were read from, in walk order.
  } deriving (Eq, Show)

-- | No config at all — what a tree with no @.org-glance\/config@ loads as, and
-- what every caller that does not want one passes.  Parsing under it is
-- byte-identical to parsing from 'defaultContext'.
noConfig :: ConfigLayers
noConfig = ConfigLayers noKeywords [] noKeywords "" []

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
      , clDirs   = dirs
      }
    firstPerTag = firstBy fst

-- | What one config directory declares: the system layer and each tag layer,
-- as @(tag, path, digest, keywords)@ with the system layer tagged 'Nothing'.
layersIn :: FilePath -> IO [(Maybe Text, FilePath, Text, TodoKeywords)]
layersIn dir = declaring <$> filesIn dir
  where declaring files = [ (lfTag f, lfPath f, lfDigest f, todoPragmas (lfText f))
                          | f <- files, not (T.null (lfDigest f)) ]

-- Reading the layers as files

-- | One config file as a settings client sees it: where it is, which layer it
-- is, what it holds and the digest a write to it is pinned to.
--
-- A file that is not there is a layer all the same — @system.org@ is the one a
-- client creates — and comes back empty on both counts.  That is the only
-- "does it exist" this carries: an empty file digests to something, so the
-- empty digest means absent (or unreadable, which a create is then refused
-- for) and nothing else has to be kept in step with it.  It is also the pin
-- 'Data.Org.Edit.editFile' reads as "nothing is there", so the record a reader
-- is handed is the lock a writer presents back.
--
-- The text is whole rather than the @#+TODO:@ lines alone, because both
-- readers want a different cut of it and neither wants a stored one:
-- 'todoLines' is what a settings client is shown and the write measures its
-- spans in the rest.
data ConfigLayerFile = ConfigLayerFile
  { lfPath   :: !FilePath      -- ^ the file, present or not.
  , lfTag    :: !(Maybe Text)  -- ^ the tag it configures; 'Nothing' is the system layer.
  , lfDigest :: !Text          -- ^ digest of its bytes, @\"\"@ when there are none to read.
  , lfText   :: !Text          -- ^ its text, @\"\"@ when there is no file.
  } deriving (Eq, Show)

-- | Every layer file DIRS hold, read now: each directory's @system.org@ and
-- every tag config beside it, in the order the directories were given and then
-- by file name.
--
-- Read at the moment it is asked for rather than taken off a loaded
-- 'ConfigLayers': the digest a client is handed is the lock its write is
-- checked against, so it has to be of the very bytes it was shown.  The system
-- file is listed whether or not it is there; a tag layer is only ever a file
-- that is, since a tag nobody has configured has no name to offer.
readConfigLayers :: [FilePath] -> IO [ConfigLayerFile]
readConfigLayers dirs = concat <$> mapM filesIn dirs

filesIn :: FilePath -> IO [ConfigLayerFile]
filesIn dir = do
  system <- layerAt Nothing systemFile
  names  <- listOrg tagsDir
  tags   <- mapM (\n -> layerAt (Just (tagOf n)) (tagsDir </> n)) names
  pure (system : tags)
  where
    (systemFile, tagsDir) = configPaths dir
    layerAt tag path = held <$> readLayer path
      where held = maybe (ConfigLayerFile path tag "" "") (uncurry (ConfigLayerFile path tag))

-- | The @.org@ file names directly in DIR, sorted; none when it is not there.
-- Filtered by the walk's own document rule, so Emacs's sidecars are not layers
-- — the tags directory in ~\/sync holds a live @#alberblanc.org#@ autosave.
listOrg :: FilePath -> IO [FilePath]
listOrg dir = do
  listed <- try (listDirectory dir) :: IO (Either IOException [FilePath])
  pure (either (const []) (sort . filter isDocument) listed)

-- | PATH's text and the digest of the bytes it was decoded from, or 'Nothing'
-- when there is nothing readable there.
readLayer :: FilePath -> IO (Maybe (Text, Text))
readLayer path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case raw of
    Left _err   -> Nothing
    Right bytes -> either (const Nothing) (Just . (,) (digestOf bytes)) (TE.decodeUtf8' bytes)

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
--
-- 'noConfig' needs no case of its own: 'setTodo' unions into the context it is
-- given, so empty sets give back 'defaultContext' itself.
seedContext :: ConfigLayers -> Context
seedContext cfg = setTodo (Set.fromList (tkActive seed)) (Set.fromList (tkInactive seed))
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
