-- | @system.org@ and @tags\/TAG.org@ under @\<root\>\/.org-glance\/config@, read
-- for their @#+TODO:@ lines; recognition, classification and order in AGENTS.hs.
module Data.Org.Config ( ConfigLayerFile (..)
                       , ConfigLayers (..)
                       , TreeSettings (..)
                       , noTreeSettings
                       , treeSettings
                       , TodoKeywords (..)
                       , builtinFilter
                       , captureTargetIn
                       , stateColorsEdits
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
                       , viewQueryIn
                       , fingerprint
                       , firstBy
                       , groupOn
                       , isTodoPragma
                       , keywordScopes
                       , loadConfigDirs
                       , mergeKeywords
                       , mintableLayer
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
import Data.List (find, foldl', nub, sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))

import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Org.Edit (digestOfText, eolOf, lineSpansIn, openingFor, readDocument)
import Data.Org.Parser (isTagChar, orgParse)
import Data.Org.Types ( Context, Element (EPragma), Pragma (PTodo), Span (..), Spanned (valueOf)
                      , defaultContext, setTodo, todoActive, todoInactive )
import Data.Org.Walk (configDir, isDocument, orgGlanceDir)


data TodoKeywords = TodoKeywords
  { tkActive   :: ![Text]
  , tkInactive :: ![Text]
  } deriving (Eq, Show)

noKeywords :: TodoKeywords
noKeywords = TodoKeywords [] []

mergeKeywords :: [TodoKeywords] -> TodoKeywords
mergeKeywords keywords = TodoKeywords actives inactives
  where actives   = declared tkActive
        inactives = filter (`notElem` actives) (declared tkInactive)
        declared f = firstSeen (concatMap f keywords)

-- | 'Data.List.nub' costs O(n · distinct), most of a @\/headlines@ request here.
firstSeen :: Ord a => [a] -> [a]
firstSeen = firstBy id

firstBy :: Ord k => (a -> k) -> [a] -> [a]
firstBy key = go Set.empty
  where go _ [] = []
        go seen (x : xs) | Set.member k seen = go seen xs
                         | otherwise         = x : go (Set.insert k seen) xs
          where k = key x

-- | XS by KEY, each group in the order its key was first written.
groupOn :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOn key xs = [ (k, [ x | x <- xs, key x == k ]) | k <- nub (map key xs) ]


declaredKeywords :: [Spanned Element] -> TodoKeywords
declaredKeywords elems = mergeKeywords
  [ TodoKeywords (copies active) (copies inactive)
  | EPragma (PTodo active inactive) <- map valueOf elems ]
  where copies = map T.copy

-- | Picked out before the parser sees them: a config file is mostly a capture
-- template and may fail 'orgParse', losing its keywords to a whole-file parse.
todoPragmas :: Text -> TodoKeywords
todoPragmas doc = case orgParse defaultContext (T.unlines (todoLines doc)) of
  (_elems, _ctx, Just _err) -> noKeywords
  (elems, _ctx, Nothing)    -> declaredKeywords elems

todoLines :: Text -> [Text]
todoLines = filter isTodoPragma . T.lines

isTodoPragma :: Text -> Bool
isTodoPragma = opensPragma "#+todo:"


builtinFilter :: Text
builtinFilter = "state:*active*"

builtinAgenda :: Text
builtinAgenda = "state:*active* -planned:*empty* sort:scheduled"

builtinArchive :: Text
builtinArchive = "tag:archive"

data SavedView = SavedView
  { svId      :: !Text  -- ^ what the wire and the selector spell.
  , svPragma  :: !Text  -- ^ its pragma key.
  , svBuiltin :: !Text  -- ^ the query where no layer names one.
  } deriving (Eq, Show)

savedViews :: [SavedView]
savedViews =
  [ SavedView "default" "GLANCE_DEFAULT_FILTER" builtinFilter
  , SavedView "agenda"  "GLANCE_AGENDA_FILTER"  builtinAgenda
  , SavedView "archive" "GLANCE_ARCHIVE_FILTER" builtinArchive
  ]

savedView :: Text -> Maybe SavedView
savedView vid = find ((== vid) . svId) savedViews

stateColorsKey :: Text
stateColorsKey = "GLANCE_STATE_COLORS"

settingPragma :: Text -> Text -> Bool
settingPragma key = opensPragma ("#+" <> T.toLower key <> ":")

settingOf :: Text -> Text -> Maybe Text
settingOf = lastPragmaValue . settingPragma

-- | Two absent pragmas insert at one offset, which 'applyEdits' takes in list order.
settingEdits :: Text -> Text -> Text -> [(Span, Text)]
settingEdits key doc want =
  pragmaLineEdits (settingPragma key) doc
    [ "#+" <> key <> ": " <> value | not (T.null value) ]
  where value = T.strip want

viewOf :: SavedView -> Text -> Maybe Text
viewOf = settingOf . svPragma


defaultCaptureFile :: FilePath
defaultCaptureFile = "inbox.org"


-- | Only the SHAPE is validated: an unknown theme or a bad colour is the author's.
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

-- | 'stateColorsOf' backwards, sharing its key so fold and render cannot drift.
stateColorsEdits :: Text -> [(Text, [(Text, Text)])] -> [(Span, Text)]
stateColorsEdits doc colours =
  pragmaLineEdits (settingPragma stateColorsKey) doc
    [ T.unwords ([ "#+" <> stateColorsKey <> ":", theme ]
                   <> [ k <> "=" <> v | (k, v) <- pairs ])
    | (theme, pairs) <- colours, not (null pairs) ]

captureTargetIn :: FilePath -> FilePath
captureTargetIn root = root </> defaultCaptureFile

lastPragmaValue :: (Text -> Bool) -> Text -> Maybe Text
lastPragmaValue mine doc =
  case [ T.strip (T.drop 1 (T.dropWhile (/= ':') line)) | line <- T.lines doc, mine line ] of
    [] -> Nothing
    vs -> Just (last vs)

opensPragma :: Text -> Text -> Bool
opensPragma key line = key `T.isPrefixOf` T.toLower (T.stripStart line)


todoLineEdits :: Text -> [Text] -> [(Span, Text)]
todoLineEdits = pragmaLineEdits isTodoPragma

viewEdits :: SavedView -> Text -> Text -> [(Span, Text)]
viewEdits = settingEdits . svPragma

-- | One block wherever the first match was; every span covers a WHOLE line.
pragmaLineEdits :: (Text -> Bool) -> Text -> [Text] -> [(Span, Text)]
pragmaLineEdits mine doc new = case [ sp | (sp, line) <- lines', mine line ] of
  []          -> [ (Span at at, opening <> block) | not (null new) ]
  (sp : rest) -> (sp, block) : [ (r, "") | r <- rest ]
  where
    lines' = lineSpansIn doc
    -- The file's OWN ending: an LF block in a CRLF file left it speaking two.
    eol    = eolOf doc
    block  = T.concat [ l <> eol | l <- new ]
    at = case dropWhile (T.isPrefixOf "#" . snd) lines' of
      ((sp, _line) : _rest) -> spanStart sp
      []                    -> T.length doc
    -- The all-header case lands at an end that need not carry a newline.
    opening = openingFor (T.take at doc) eol


-- | 'clSeed' is stored: 'clTags' keeps each tag's FIRST config, the seed unions all.
data ConfigLayers = ConfigLayers
  { clSystem  :: !TodoKeywords            -- ^ @config\/system.org@'s sets; empty when there is no such file.
  , clTags    :: ![(Text, TodoKeywords)]  -- ^ @config\/tags\/TAG.org@'s sets, tag lowercased, in file-name order.
  , clSeed    :: !TodoKeywords            -- ^ the recognition union: every keyword any layer names.
  , clTree    :: !TreeSettings            -- ^ what @system.org@ says about every tree-wide setting.
  , clPrint   :: !Text                    -- ^ digest over the config files read, @\"\"@ when none were.
  , clDirs    :: ![FilePath]              -- ^ the config directories these were read from, in walk order.
  } deriving (Eq, Show)

data TreeSettings = TreeSettings
  { tsViews  :: ![(Text, Text)]                 -- ^ the saved views @system.org@ names, by id; see 'viewQueryIn'.
  , tsColors :: ![(Text, [(Text, Text)])]       -- ^ per-theme keyword hues; see 'stateColorsOf'.
  } deriving (Eq, Show)

noTreeSettings :: TreeSettings
noTreeSettings = TreeSettings [] []

-- | @GET \/config@ recomputes it over files just read: the digest is a write's lock.
treeSettings :: [ConfigLayerFile] -> TreeSettings
treeSettings files = TreeSettings
  { tsViews  = [ (svId v, q) | v <- savedViews, Just q <- [systemSetting (viewOf v) files] ]
  -- EVERY system layer's lines: a tree names one line per theme.
  , tsColors = concat [ stateColorsOf (lfText f) | f <- files, isSystem f ]
  }

viewQueryIn :: Text -> TreeSettings -> Text
viewQueryIn vid ts =
  fromMaybe (maybe "" svBuiltin (savedView vid)) (lookup vid (tsViews ts))

viewQuery :: Text -> ConfigLayers -> Text
viewQuery vid = viewQueryIn vid . clTree

defaultFilter :: ConfigLayers -> Text
defaultFilter = viewQuery "default"

noConfig :: ConfigLayers
noConfig = ConfigLayers noKeywords [] noKeywords noTreeSettings "" []

-- | For a WRITER: a store is under no obligation to sit at the root served.
configDirIn :: FilePath -> FilePath
configDirIn root = root </> orgGlanceDir </> configDir

configDirsIn :: FilePath -> ConfigLayers -> [FilePath]
configDirsIn root cfg = case clDirs cfg of
  []   -> [configDirIn root]
  dirs -> dirs

configPaths :: FilePath -> (FilePath, FilePath)
configPaths dir = (dir </> "system.org", dir </> "tags")

-- | Every failure is an absence: an unreadable config must not stop a load.
loadConfigDirs :: [FilePath] -> IO ConfigLayers
loadConfigDirs dirs = combine <$> readConfigLayers dirs
  where
    combine files = ConfigLayers
      { clSystem  = mergeKeywords [ keywordsIn f | f <- entries, isSystem f ]
      , clTags    = firstBy fst [ (tag, keywordsIn f) | f <- entries, Just tag <- [lfTag f] ]
      , clSeed    = mergeKeywords (map keywordsIn entries)
      , clTree    = treeSettings files
      , clPrint   = fingerprint [ (lfPath f, lfDigest f) | f <- entries ]
      , clDirs    = dirs
      }
      -- A file that is not there is still a layer: the empty digest says so.
      where entries = [ f | f <- files, not (T.null (lfDigest f)) ]
    keywordsIn = todoPragmas . lfText

systemSetting :: (Text -> Maybe Text) -> [ConfigLayerFile] -> Maybe Text
systemSetting reader layers =
  listToMaybe [ v | l <- layers, isSystem l, Just v <- [reader (lfText l)] ]

isSystem :: ConfigLayerFile -> Bool
isSystem = null . lfTag


-- | The empty digest means absent, and is the pin 'editFile' reads as nothing there.
data ConfigLayerFile = ConfigLayerFile
  { lfPath   :: !FilePath      -- ^ the file, present or not.
  , lfTag    :: !(Maybe Text)  -- ^ the tag it configures; 'Nothing' is the system layer.
  , lfDigest :: !Text          -- ^ digest of its bytes, @\"\"@ when there are none to read.
  , lfText   :: !Text          -- ^ its text, @\"\"@ when there is no file.
  } deriving (Eq, Show)

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

-- | PATH as a TAG LAYER THAT IS NOT THERE YET, under DIR's own @tags\/@.
-- 'filesIn' can only list what exists, and @system.org@ is listed absent so a
-- writer can create it ('loadConfigDirs'); this is that same rule for a tag,
-- and it is the ONLY way a tag layer comes into being.  The empty digest is
-- what 'editFile' reads as "create it".  A basename org cannot read back as a
-- tag is refused HERE, or 'tagOf' would name a layer nothing ever selects.
mintableLayer :: FilePath -> FilePath -> Maybe ConfigLayerFile
mintableLayer dir path
  | takeDirectory path == tagsDir
  , isDocument (takeFileName path)
  , not (T.null tag)
  , T.all isTagChar tag
  = Just (ConfigLayerFile path (Just tag) "" "")
  | otherwise = Nothing
  where tagsDir = snd (configPaths dir)
        tag     = tagOf (takeFileName path)

listOrg :: FilePath -> IO [FilePath]
listOrg dir = do
  listed <- try (listDirectory dir) :: IO (Either IOException [FilePath])
  pure (either (const []) (sort . filter isDocument) listed)

tagOf :: FilePath -> Text
tagOf = T.toLower . T.pack . takeBaseName

fingerprint :: [(FilePath, Text)] -> Text
fingerprint [] = ""
fingerprint entries =
  digestOfText (T.unlines [ T.pack p <> "\t" <> d | (p, d) <- entries ])


-- | ONE immutable value per load, so two files cannot influence each other.
seedContext :: ConfigLayers -> Context
seedContext cfg = setTodo (Set.fromList (tkActive seed)) (Set.fromList (tkInactive seed))
                          defaultContext
  where seed = clSeed cfg

-- | Deriving this from the parse's ending context alphabetized the tree's cycle.
recognizedKeywords :: ConfigLayers -> TodoKeywords -> TodoKeywords
recognizedKeywords cfg fileKw = mergeKeywords [builtinKeywords, clSeed cfg, fileKw]

-- | WIDEST FIRST; four scopes and no fifth, see AGENTS.hs.  The rank travels
-- beside the name because a tree may configure a tag called @system@.
keywordScopes :: ConfigLayers -> TodoKeywords -> [Text] -> [(Int, Text, TodoKeywords)]
keywordScopes cfg fileKw tags =
  (0, defaultSource, builtinKeywords)
    : (1, systemSource, clSystem cfg)
    : [ (2, tag, kw) | tag <- tags, Just kw <- [lookup tag (clTags cfg)] ]
   <> [ (3, fileSource, fileKw) ]

-- | Reserved by convention alone: a tag spelled like one is still its own scope.
fileSource, systemSource, defaultSource :: Text
fileSource    = "file"
systemSource  = "system"
defaultSource = "default"

-- | A set becomes a LIST here at no cost: one word each side of the bar.
builtinKeywords :: TodoKeywords
builtinKeywords = TodoKeywords (Set.toAscList (todoActive defaultContext))
                               (Set.toAscList (todoInactive defaultContext))

-- | The final 'True' is REACHED, by a word no scope of this headline's claims.
classify :: ConfigLayers -> TodoKeywords -> [Text] -> Text -> Bool
classify cfg fileKw tags keyword =
  fromMaybe True (asum [ says kw | (_rank, _source, kw) <- keywordScopes cfg fileKw tags ])
  where
    says (TodoKeywords active inactive)
      | keyword `elem` active   = Just True
      | keyword `elem` inactive = Just False
      | otherwise               = Nothing
