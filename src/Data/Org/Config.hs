-- | org-glance's keyword configuration: the files beside a tree that say which
-- words are TODO states.
--
-- Under @\<root\>\/.org-glance\/config@: @system.org@ for the tree,
-- @tags\/TAG.org@ per tag.  Both are ordinary org documents and mostly a
-- capture template; what is read here is their @#+TODO:@ lines.  Three
-- questions come out of that, answered differently on purpose.
--
-- RECOGNITION is a UNION.  A word any layer names parses as a keyword in every
-- file under the root — else @* READING War and Peace@ is a state in one file
-- and a title in the file beside it.  Computed once per load and threaded in as
-- the seed context ('seedContext'); it accumulates nothing, so no file's parse
-- can reach another's.
--
-- CLASSIFICATION is WIDEST SCOPE ('classify'): org's own TODO\/DONE, then
-- @system.org@, then the headline's tags in order, then the file's own line.  A
-- narrower scope ADDS words and shadows none — the vocabulary a reader shares
-- across a tree is decided at the top, and a leaf cannot make a shared word
-- mean something else under it.
--
-- The union is NOT a scope.  A keyword only another tag's config names is
-- recognized here and classified by the fallback, and is settable on no row
-- that does not reach it: recognition keeps the word out of a title, the chain
-- is what a reader is offered.
--
-- ORDER is the org files' own ('recognizedKeywords').  A @#+TODO:@ line is a
-- CYCLE, and that order is the tree's whole say over how its state column sorts
-- and how a palette draws.  Sets answer recognition and nothing else — the
-- moment one reaches a palette a tree's cycle comes back alphabetized.
module Data.Org.Config ( ConfigLayerFile (..)
                       , ConfigLayers (..)
                       , TodoKeywords (..)
                       , builtinAgenda
                       , builtinFilter
                       , captureTargetEdits
                       , captureTargetIn
                       , captureTargetOf
                       , stateColorsEdits
                       , stateColorsKey
                       , stateColorsOf
                       , classify
                       , configDirIn
                       , configDirsIn
                       , configPaths
                       , declaredKeywords
                       , defaultCaptureFile
                       , SavedView (..)
                       , defaultFilter
                       , savedView
                       , savedViews
                       , viewEdits
                       , viewOf
                       , viewQuery
                       , firstBy
                       , isTodoPragma
                       , keywordScopes
                       , loadConfigDirs
                       , mergeKeywords
                       , noConfig
                       , noKeywords
                       , readConfigLayers
                       , recognizedKeywords
                       , seedContext
                       , systemSetting
                       , todoLineEdits
                       , todoLines
                       , todoPragmas
                       ) where

import Control.Exception (IOException, try)
import Data.Foldable (asum)
import Data.List (find, foldl', sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath (isAbsolute, splitDirectories, takeBaseName, (</>))

import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Org.Edit (digestOfText, eolOf, lineSpansIn, openingFor, readDocument)
import Data.Org.Parser (orgParse)
import Data.Org.Types ( Context, Element (EPragma), Pragma (PTodo), Span (..), Spanned (valueOf)
                      , defaultContext, setTodo, todoActive, todoInactive )
import Data.Org.Walk (configDir, isDocument, isWalked, orgGlanceDir)

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
-- Three callers want the same fold under different keys: the keyword merge
-- deduplicates by the word itself, the tag layers by the tag, where a second
-- config directory naming a tag the first already configured loses it, and
-- 'Glance.Query.orgLinks' by a link's target.
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
  -- In the order the lines spell them, which is the whole of what an org file
  -- says about how its states sort.  'mergeKeywords' does the deduplicating, so
  -- a word repeated on one line or across two keeps its FIRST place.
  where copies = map T.copy

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
isTodoPragma = opensPragma "#+todo:"

-- The default view

-- | The query a table opens on when nobody has said otherwise.  org-glance's
-- own name for the keyword group a @#+TODO:@ line declares before the bar, so a
-- tree with no configuration at all still opens on its unfinished work.
builtinFilter :: Text
builtinFilter = "state:*active*"

-- | And the query @a@ applies: the active rows carrying a date, earliest
-- first.  A VIEW like the default rather than a mode, so the order rides in the
-- query with the filter.
builtinAgenda :: Text
builtinAgenda = "state:*active* -planned:*empty* sort:scheduled"

-- | A SAVED VIEW: the query a key applies, kept in @system.org@ under a line of
-- its own.  A third view is one entry here — the wire, the settings selector
-- and the write all fold this list.
data SavedView = SavedView
  { svId      :: !Text  -- ^ what the wire and the selector spell.
  , svPragma  :: !Text  -- ^ its pragma key.
  , svBuiltin :: !Text  -- ^ the query where no layer names one.
  } deriving (Eq, Show)

-- | Every view a tree may configure, in the order a selector offers them.
savedViews :: [SavedView]
savedViews =
  [ SavedView "default" "GLANCE_DEFAULT_FILTER" builtinFilter
  , SavedView "agenda"  "GLANCE_AGENDA_FILTER"  builtinAgenda
  ]

-- | The view ID names, or 'Nothing' where no build carries one.
savedView :: Text -> Maybe SavedView
savedView vid = find ((== vid) . svId) savedViews

-- | The TREE-WIDE settings @system.org@ carries beside its cycle and its views,
-- as the pragma keys they are spelled with.  Each name is written ONCE: a
-- reader folds it and a writer renders it, and the pair drifting apart is a
-- line a settings sheet rewrites into a line nothing reads.
captureTargetKey, stateColorsKey :: Text
captureTargetKey = "GLANCE_CAPTURE_TARGET"
stateColorsKey   = "GLANCE_STATE_COLORS"

-- | Does a line open KEY's pragma?  The reader's half of a setting name, folded
-- once so the writer below can render the same name in org's own casing.
settingPragma :: Text -> Text -> Bool
settingPragma key = opensPragma ("#+" <> T.toLower key <> ":")

-- | The value DOC's @#+KEY:@ line names, or 'Nothing' when it carries none.
--
-- LAST line wins, the way a reader scrolling a config file would read it, and a
-- line naming nothing at all is a line naming nothing: it comes back as an
-- empty value, which for the default view is a table opening on the whole
-- store.
settingOf :: Text -> Text -> Maybe Text
settingOf = lastPragmaValue . settingPragma

-- | The span edits setting DOC's @#+KEY:@ line to WANT: rewritten, written or
-- taken away.  The same whole-line splice the @#+TODO:@ block gets, under a
-- per-setting predicate; an EMPTY value deletes the line, which is the tree
-- going back to that setting's built-in.
--
-- Two absent pragmas insert at the same offset, which 'Data.Org.Edit.applyEdits'
-- resolves in list order rather than refusing, so a caller writing both settings
-- into a file that had neither gets them in the order it named them.
settingEdits :: Text -> Text -> Text -> [(Span, Text)]
settingEdits key doc want =
  pragmaLineEdits (settingPragma key) doc
    [ "#+" <> key <> ": " <> value | not (T.null value) ]
  where value = T.strip want

-- | The query DOC's line for VIEW names, or 'Nothing' when it carries none —
-- which is what makes 'svBuiltin' the fallback rather than a value written into
-- every tree.
viewOf :: SavedView -> Text -> Maybe Text
viewOf = settingOf . svPragma

-- The capture target

-- | Where a capture lands when no layer names a file: one entry point per tree,
-- at the root, under the name org-capture templates have always used.
defaultCaptureFile :: FilePath
defaultCaptureFile = "inbox.org"

-- | The file DOC's @#+GLANCE_CAPTURE_TARGET:@ line names, or 'Nothing' when it
-- carries none — which is what makes 'defaultCaptureFile' the fallback rather
-- than a value written into every tree.
captureTargetOf :: Text -> Maybe Text
captureTargetOf = settingOf captureTargetKey

-- State colours

-- | The per-keyword hues DOC's @#+GLANCE_STATE_COLORS:@ lines name, by THEME.
--
-- @#+GLANCE_STATE_COLORS: light TODO=#9A3412 DONE=#166534@ — the theme first,
-- then @KEYWORD=VALUE@ pairs.  EVERY such line is read rather than the last
-- alone: a tree names one line per theme, and a keyword named twice takes its
-- LAST spelling, which is how the rest of this file reads a repeated setting.
--
-- Nothing is validated here beyond the shape.  A theme name no build carries
-- declares tokens nothing reads, and a value that is not a colour is a value
-- CSS ignores — both are the author's business, and refusing either would put
-- this parser in the way of a config it does not own.
stateColorsOf :: Text -> [(Text, [(Text, Text)])]
stateColorsOf doc = foldl' add [] (mapMaybe entryOf (T.lines doc))
  where
    entryOf line
      | not (settingPragma stateColorsKey line) = Nothing
      | otherwise = case T.words (T.strip (T.drop 1 (T.dropWhile (/= ':') line))) of
          theme : pairs -> Just (T.toLower theme, mapMaybe pairOf pairs)
          _nothingNamed -> Nothing
    pairOf w = case T.breakOn "=" w of
      (key, value) | not (T.null key), not (T.null (T.drop 1 value))
                     -> Just (key, T.strip (T.drop 1 value))
      _notAPair      -> Nothing
    add acc (theme, pairs) =
      [ (t, if t == theme then merge ps pairs else ps) | (t, ps) <- acc ]
        <> [ (theme, pairs) | theme `notElem` map fst acc ]
    merge held fresh =
      [ (k, v) | (k, v) <- held, k `notElem` map fst fresh ] <> fresh

-- | The span edits putting COLOURS where DOC's @#+GLANCE_STATE_COLORS:@ lines
-- are: one line per theme, in the order given, where the first of them stood.
-- A theme naming no keyword writes no line, so an emptied theme is a theme
-- taken off; an empty list deletes the block, which is a tree going back to its
-- palette.
--
-- The render is 'stateColorsOf' backwards and shares its key, so a fold that
-- drifted from a render can no longer rewrite a line nothing reads.
stateColorsEdits :: Text -> [(Text, [(Text, Text)])] -> [(Span, Text)]
stateColorsEdits doc colours =
  pragmaLineEdits (settingPragma stateColorsKey) doc
    [ T.unwords ([ "#+" <> stateColorsKey <> ":", theme ]
                   <> [ k <> "=" <> v | (k, v) <- pairs ])
    | (theme, pairs) <- colours, not (null pairs) ]

-- | Where a capture under ROOT lands given CFG, or why this daemon will not
-- write there.
--
-- Absent, the target is @\<root\>\/inbox.org@; named, it resolves against the
-- SERVED ROOT rather than the config directory, because a nested store still
-- captures into the tree being served.
--
-- CHECKED HERE, where the config is read, rather than when a capture arrives: a
-- tree misconfigured in January should say so at startup.  Three textual
-- refusals: an absolute path, one climbing out through @..@, and a name the
-- walk would not collect — a capture there writes a file no watch delivers a
-- row for.  That third is 'isWalked' rather than 'isDocument', and the
-- difference is load-bearing: @.org-glance\/config\/x.org@ is an org file the
-- walk declines, so stopping at the extension would bless exactly the paths
-- this refusal exists for.
captureTargetIn :: FilePath -> ConfigLayers -> Either Text FilePath
captureTargetIn root cfg = case fmap T.strip (clCapture cfg) of
  Just want | not (T.null want) -> checked want
  _unconfigured                 -> Right (root </> defaultCaptureFile)
  where
    checked want
      | isAbsolute path = Left (refused want "an absolute path; name one under the served root")
      | ".." `elem` splitDirectories path = Left (refused want "outside the served root")
      | not (isWalked target) = Left (refused want "not a file this tree walks, so no watch\
                                                   \ would ever deliver the rows it captured")
      | otherwise = Right target
      where path   = T.unpack want
            target = root </> path
    refused want why = "#+" <> captureTargetKey <> ": " <> want <> " is " <> why

-- | The span edits setting DOC's capture target to WANT.  An EMPTY value
-- deletes the line, which is the tree going back to 'defaultCaptureFile'.
captureTargetEdits :: Text -> Text -> [(Span, Text)]
captureTargetEdits = settingEdits captureTargetKey

-- | The value of the LAST line of DOC that MINE accepts, or 'Nothing' when it
-- has none.  Last wins, the way a reader scrolling a config file reads it, and
-- the value is whatever follows the pragma's colon, stripped.
lastPragmaValue :: (Text -> Bool) -> Text -> Maybe Text
lastPragmaValue mine doc =
  case [ T.strip (T.drop 1 (T.dropWhile (/= ':') line)) | line <- T.lines doc, mine line ] of
    [] -> Nothing
    vs -> Just (last vs)

-- | Does LINE open the pragma KEY names?  Folded, since org takes either casing
-- and the parser uppercases the key.
opensPragma :: Text -> Text -> Bool
opensPragma key line = key `T.isPrefixOf` T.toLower (T.stripStart line)

-- Editing the pragmas

-- | The span edits putting LINES where DOC's @#+TODO:@ lines are: char spans
-- into DOC and the text to write over each, the currency
-- 'Data.Org.Edit.applyEdits' takes.  'pragmaLineEdits' is the rule.
todoLineEdits :: Text -> [Text] -> [(Span, Text)]
todoLineEdits = pragmaLineEdits isTodoPragma

-- | The span edits setting DOC's VIEW to WANT.  An EMPTY query deletes the
-- line, which is how a tree goes back to that view's built-in — a line naming
-- nothing would be a different answer (the whole store) and a settings sheet
-- has no way to spell the difference.
viewEdits :: SavedView -> Text -> Text -> [(Span, Text)]
viewEdits = settingEdits . svPragma

-- | The span edits putting LINES where DOC's MINE lines are.
--
-- One block, wherever the first matching line was.  A document already carrying
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
pragmaLineEdits :: (Text -> Bool) -> Text -> [Text] -> [(Span, Text)]
pragmaLineEdits mine doc new = case [ sp | (sp, line) <- lines', mine line ] of
  []          -> [ (Span at at, opening <> block) | not (null new) ]
  (sp : rest) -> (sp, block) : [ (r, "") | r <- rest ]
  where
    lines' = lineSpansIn doc
    -- The file's OWN line ending, for both the block and the opening: a CRLF
    -- @system.org@ took an LF block spliced into it, so one write left the file
    -- speaking two conventions and the line the reader typed was the odd one.
    eol    = eolOf doc
    block  = T.concat [ l <> eol | l <- new ]
    -- Past the header the file opens with, or at the very top when it opens
    -- with content; a file that is nothing but header takes it at the end.
    at = case dropWhile (T.isPrefixOf "#" . snd) lines' of
      ((sp, _line) : _rest) -> spanStart sp
      []                    -> T.length doc
    -- Every other insertion point is the start of a line, so the character
    -- before it is a newline by construction.  The all-header case is the
    -- exception: it lands at the end of a document that need not close with
    -- one, and a block appended to a live line is not a pragma at all.
    opening = openingFor (T.take at doc) eol

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
--
-- It feeds 'seedContext' and nothing else here: it is not a scope of
-- 'keywordScopes', so no headline is classified by it and no row is settable to
-- a word it alone reaches.
data ConfigLayers = ConfigLayers
  { clSystem  :: !TodoKeywords            -- ^ @config\/system.org@'s sets; empty when there is no such file.
  , clTags    :: ![(Text, TodoKeywords)]  -- ^ @config\/tags\/TAG.org@'s sets, tag lowercased, in file-name order.
  , clSeed    :: !TodoKeywords            -- ^ the recognition union: every keyword any layer names.
  , clViews   :: ![(Text, Text)]          -- ^ the views @system.org@ names, by id; see 'viewQuery'.
  , clCapture :: !(Maybe Text)            -- ^ the capture target it names; see 'captureTargetIn'.
  , clStateColors :: ![(Text, [(Text, Text)])]  -- ^ per-theme keyword hues; see 'stateColorsOf'.
  , clPrint   :: !Text                    -- ^ digest over the config files read, @\"\"@ when none were.
  , clDirs    :: ![FilePath]              -- ^ the config directories these were read from, in walk order.
  } deriving (Eq, Show)

-- | The query view ID under CFG applies: what @system.org@'s line for it names,
-- or that view's built-in where no layer names one.  A view no build carries is
-- the empty query.
--
-- The SYSTEM layer alone, and only the first config directory that has anything
-- to say: a saved view is a property of a tree rather than of a tag, and two
-- stores nested under one root would otherwise take turns deciding what the
-- table opens on.
viewQuery :: Text -> ConfigLayers -> Text
viewQuery vid cfg =
  fromMaybe (maybe "" svBuiltin (savedView vid)) (lookup vid (clViews cfg))

-- | The query a table under CFG opens on.
defaultFilter :: ConfigLayers -> Text
defaultFilter = viewQuery "default"

-- | No config at all — what a tree with no @.org-glance\/config@ loads as, and
-- what every caller that does not want one passes.  Parsing under it is
-- byte-identical to parsing from 'defaultContext'.
noConfig :: ConfigLayers
noConfig = ConfigLayers noKeywords [] noKeywords [] Nothing [] "" []

-- | Where ROOT would keep its config directory.  For a writer — the settings UI
-- creating @system.org@ — rather than for a reader: a reader is given the
-- directories the walk found, because an org-glance store is not obliged to sit
-- at the root that is being served.  In ~\/sync's own tree it does not: the
-- walk root is @~\/sync@ and the store is @~\/sync\/views\/.org-glance@.
configDirIn :: FilePath -> FilePath
configDirIn root = root </> orgGlanceDir </> configDir

-- | Which config directories a reader of ROOT under CFG should read: the ones
-- the walk MET, else the one 'configDirIn' would put there.
--
-- Only the walk can answer the first half — an org-glance store is not obliged
-- to sit at the root being served, and in the author's own tree it does not.
-- The second is a guess, the one case with nothing yet to be right about, and
-- it is what makes @POST \/config@ able to create the first layer a tree has.
--
-- Beside 'configDirIn' rather than beside a caller: the @\/config@ route and
-- @POST \/command capture@ both ask it, and one answer is what makes the layer a
-- template is READ from the layer a settings sheet WRITES.
configDirsIn :: FilePath -> ConfigLayers -> [FilePath]
configDirsIn root cfg = case clDirs cfg of
  []   -> [configDirIn root]
  dirs -> dirs

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
loadConfigDirs dirs = combine <$> readConfigLayers dirs
  where
    combine files = ConfigLayers
      { clSystem  = mergeKeywords [ keywordsIn f | f <- entries, isSystem f ]
      , clTags    = firstBy fst [ (tag, keywordsIn f) | f <- entries, Just tag <- [lfTag f] ]
      , clSeed    = mergeKeywords (map keywordsIn entries)
      , clViews   = [ (svId v, q) | v <- savedViews
                                  , Just q <- [systemSetting (viewOf v) files] ]
      , clCapture = systemSetting captureTargetOf files
      -- The SYSTEM layer's, like the other two tree-wide settings: a colour for
      -- a keyword is a property of the tree rather than of one tag.
      , clStateColors = concat [ stateColorsOf (lfText f) | f <- files, isSystem f ]
      , clPrint   = fingerprint [ (lfPath f, lfDigest f) | f <- entries ]
      , clDirs    = dirs
      }
      -- A file that is not there is still a layer, and declares nothing: the
      -- empty digest is what says so, and the settings routes read the very
      -- same list off 'readConfigLayers'.
      where entries = [ f | f <- files, not (T.null (lfDigest f)) ]
    keywordsIn = todoPragmas . lfText

-- | The value READ takes off the first SYSTEM layer of LAYERS that names one,
-- or 'Nothing' where none does.  A tree-wide setting belongs to a tree rather
-- than to a tag, so a tag layer is never asked.
--
-- Every tree-wide setting is read this way and both readers share it — the
-- load, which fills 'clViews' and 'clCapture', and the settings route, which reads the
-- same bytes the digests it hands out were taken from, so what a sheet shows
-- and what its write is pinned to describe one file.
systemSetting :: (Text -> Maybe Text) -> [ConfigLayerFile] -> Maybe Text
systemSetting reader layers =
  listToMaybe [ v | l <- layers, isSystem l, Just v <- [reader (lfText l)] ]

-- | Is LAYER the tree's own @system.org@ rather than a tag's file?
isSystem :: ConfigLayerFile -> Bool
isSystem = null . lfTag

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
    layerAt tag path = held <$> readDocument path
      where held = maybe (ConfigLayerFile path tag "" "")
                         (\(text, digest) -> ConfigLayerFile path tag digest text)

-- | The @.org@ file names directly in DIR, sorted; none when it is not there.
-- Filtered by the walk's own document rule, so Emacs's sidecars are not layers
-- — the tags directory in ~\/sync holds a live @#alberblanc.org#@ autosave.
listOrg :: FilePath -> IO [FilePath]
listOrg dir = do
  listed <- try (listDirectory dir) :: IO (Either IOException [FilePath])
  pure (either (const []) (sort . filter isDocument) listed)

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
  digestOfText (T.unlines [ T.pack p <> "\t" <> d | (p, d) <- entries ])

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

-- | Every keyword a file under CFG declaring FILEKW recognizes, IN THE ORDER A
-- PALETTE SHOWS THEM: org's pair, @system.org@'s cycle, the tag configs in walk
-- order, the file's own lines — a repeat keeping its first place.
--
-- The same set 'seedContext' parses with, ORDERED.  A parse asks membership and
-- answers out of 'Context'\'s sets; everything a READER meets asks which word
-- comes first, and the answer is the org files' own spelling.  Deriving this
-- from the parse's ending context is what lost it — 'Data.Set' had already
-- alphabetized the tree's cycle.
--
-- The segment order is 'keywordScopes'\' precedence, so the chain a keyword is
-- CLASSIFIED by and the order it is SHOWN in are one list.
recognizedKeywords :: ConfigLayers -> TodoKeywords -> TodoKeywords
recognizedKeywords cfg fileKw = mergeKeywords [builtinKeywords, clSeed cfg, fileKw]

-- | The scopes that answer for a headline carrying TAGS in a file declaring
-- FILEKW, WIDEST FIRST: org's TODO\/DONE, @system.org@, the headline's tags IN
-- ORDER, the file's own.  Each entry is its RANK, its name and what it declares.
--
-- WIDEST FIRST IS THE DEFERRED BOUNDARY: the scope every reader shares gets the
-- first word, and a narrower one may ADD vocabulary without redefining what a
-- wider one settled.  The old order ran the other way, and a file redeclaring a
-- shared word made it classify differently from its neighbours' — a private
-- opinion about a public word.
--
-- FOUR SCOPES AND NO FIFTH.  The recognition union is not one: it says which
-- words PARSE under this root, a superset of what any headline is configured
-- for, and letting it close the chain made another tag's cycle both classified
-- and settable on a headline carrying no such tag.
--
-- ONE list, two readers, three answers: 'classify' takes the first scope with
-- an opinion, 'keywordSources' reports what each claims, and 'settableStates'
-- is THAT flattened rather than a third fold — so the active-ness shown, the
-- palette picked from and the words a write allows describe one chain.  Only
-- the BUCKET a word sits in moves with the order.  The rank travels beside the
-- name because a tree may configure a tag called @system@.
keywordScopes :: ConfigLayers -> TodoKeywords -> [Text] -> [(Int, Text, TodoKeywords)]
keywordScopes cfg fileKw tags =
  (0, defaultSource, builtinKeywords)
    : (1, systemSource, clSystem cfg)
    : [ (2, tag, kw) | tag <- tags, Just kw <- [lookup tag (clTags cfg)] ]
   <> [ (3, fileSource, fileKw) ]

-- | The names 'keywordScopes' gives the three scopes that are not tags.
-- Reserved by convention alone: a tag spelled like one of them is still a scope
-- of its own, at its own rank.
fileSource, systemSource, defaultSource :: Text
fileSource    = "file"
systemSource  = "system"
defaultSource = "default"

-- | Org's own two states, which every parse recognizes whatever a tree
-- configures and which answer under the name @default@ at the head of the
-- chain.  Read off 'defaultContext' rather than spelled again, since that
-- is where org's built-in cycle is declared — and read HERE rather than at each
-- of the two callers, so the scope 'classify' consults and the scope a palette
-- shows cannot come to hold different words.
--
-- The one place a set still becomes a keyword LIST, and the one place that
-- costs nothing: org's cycle is a single word each side of the bar, so
-- ascending order IS declaration order.  A second built-in would need a list
-- spelled here.
builtinKeywords :: TodoKeywords
builtinKeywords = TodoKeywords (Set.toAscList (todoActive defaultContext))
                               (Set.toAscList (todoInactive defaultContext))

-- | Is KEYWORD an active state on a headline carrying TAGS, in a file whose own
-- @#+TODO:@ lines declare FILEKW?  The first scope of 'keywordScopes' with
-- anything to say about it answers, and a NARROWER one disagreeing is ignored:
-- org's pair outranks the tree's cycle, which outranks a tag's, which outranks
-- the file's own line.
--
-- The final 'True' is REACHED, by exactly the keywords the recognition union
-- put in reach of this file and no scope of this headline's claims — another
-- tag's cycle on an untagged row.  Active is the honest default for one: a word
-- nothing has called done-like is unfinished work, so it stays in the default
-- view rather than disappearing out of it.
classify :: ConfigLayers -> TodoKeywords -> [Text] -> Text -> Bool
classify cfg fileKw tags keyword =
  fromMaybe True (asum [ says kw | (_rank, _source, kw) <- keywordScopes cfg fileKw tags ])
  where
    says (TodoKeywords active inactive)
      | keyword `elem` active   = Just True
      | keyword `elem` inactive = Just False
      | otherwise               = Nothing
