-- | The filter query language: @?q=@ as SCHEMA.md's micro-syntax.
--
-- A port term by term of @table-view.js@'s @scanQuery@, @parseQuery@ and
-- @tokenTest@ — parity is the contract.  The grammar, the starred metas and
-- every known divergence are in AGENTS.hs and @table-view\/SCHEMA.md@.
module Glance.Web.Filter ( FilterEnv
                         , Term (..)
                         , Token (..)
                         , alternatives
                         , archiveKey
                         , archiveMeta
                         , cellAt
                         , emptyEnv
                         , emptyMeta
                         , filterKeys
                         , matchesFilter
                         , metaOf
                         , namesArchive
                         , parseFilter
                         , plannedKey
                         , refKey
                         , refusedOn
                         , scanQuery
                         , sortKey
                         , substringKey
                         , columnsKey
                         , storeEnv
                         , tagsKey
                         ) where

import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (hrActive, hrId, hrLinks, hrSearch)
                    , Meta (..), activeMeta, archiveTag, cellSep, filterKeys
                    , inactiveMeta, metaWord
                    , priorityLetter, refSpellings, tagRunEntries )


dateKeys :: [Text]
dateKeys = ["scheduled", "deadline"]

-- | @ref:ROWID@ — rows whose subtree points at the row named.  Producer-only,
-- and the one predicate whose value is NOT folded: a row id is exact-string.
refKey :: Text
refKey = "ref"

-- | @planned@ — either date cell holding anything; renderer-decidable too.
plannedKey :: Text
plannedKey = "planned"

substringKey :: Text
substringKey = "substring"

-- | The ORDER token ('Glance.Web.Sort'), no predicate: it narrows nothing.
sortKey :: Text
sortKey = "sort"

columnsKey :: Text
columnsKey = "columns"

viewKeys :: [Text]
viewKeys = [sortKey, columnsKey, viewKey]

-- | @view:NAME@ — the saved view a query names.  A VIEW TOKEN like its two
-- siblings, so it narrows nothing here: what a name MEANS is the shell's, which
-- expands it before the fetch, and a query that reaches this side still holding
-- one is answered with every row rather than with a guess.
viewKey :: Text
viewKey = "view"

dateColumns :: [Int]
dateColumns = mapMaybe (`elemIndex` filterKeys) dateKeys

data Token = Token
  { tkNegated :: !Bool  -- ^ the token opened with @-@.
  , tkQuoted  :: !Bool  -- ^ the token opened with @"@, so it is free text whatever it spells.
  , tkBody    :: !Text  -- ^ the token itself, unquoted and un-negated.
  } deriving (Eq, Show)

-- | WHY a view-token reader refuses T under KEY; one sentence for both readers.
refusedOn :: Text -> Term -> Text -> Text
refusedOn key t why = why <> ": '" <> spellingOf key t <> "'"

-- | T as the reader wrote it under KEY, negation and all.
spellingOf :: Text -> Term -> Text
spellingOf key t = (if tmNegated t then "-" else "") <> key <> ":" <> tmValue t

data Term = Term
  { tmNegated :: !Bool          -- ^ the row fails when this term matches.
  , tmKey     :: !(Maybe Text)  -- ^ the column a predicate names; 'Nothing' is free text.
  , tmValue   :: !Text          -- ^ the predicate's value, or the free text itself.
  } deriving (Eq, Show)

isSep :: Char -> Bool
isSep c = c == '&' || c == ' ' || c == '\t' || c == '\n'

-- | Q cut into tokens; an unclosed quote runs to the end of Q.
scanQuery :: Text -> [Token]
scanQuery q = reverse (flush final out)
  where
    (final, out) = T.foldl' step (fresh, []) q
    step (s, acc) c
      | c == '"'                 = (s { seen     = True
                                      , hasBody  = True
                                      , quoted   = quoted s || not (hasBody s)
                                      , inQuotes = not (inQuotes s) }, acc)
      | not (inQuotes s), isSep c = (fresh, flush s acc)
      | not (seen s), c == '-'    = (s { seen = True, negated = True }, acc)
      | otherwise                 = (s { body = c : body s, seen = True, hasBody = True }, acc)
    flush s acc
      | seen s    = Token (negated s) (quoted s) (T.pack (reverse (body s))) : acc
      | otherwise = acc
    fresh = Scan [] False False False False False

data Scan = Scan
  { body     :: [Char]
  , negated  :: !Bool
  , quoted   :: !Bool
  , seen     :: !Bool
  , hasBody  :: !Bool
  , inQuotes :: !Bool
  }

-- | Q's tokens resolved by 'fieldOf', so grammar and matcher cannot disagree.
parseFilter :: Text -> [Term]
parseFilter = map resolve . scanQuery
  where
    resolve t
      | tkQuoted t = free t
      | otherwise  = case splitKey (tkBody t) of
          Just (key, value) | isJust (fieldOf key) -> Term (tkNegated t) (Just key) value
          _notAPredicate                           -> free t
    free t = Term (tkNegated t) Nothing (tkBody t)

tagsKey :: Text
tagsKey = "tag"

archiveKey :: Text
archiveKey = T.toLower archiveTag

-- | The archive tag as the META — the one query that lifts the exclusion.
archiveMeta :: Text
archiveMeta = metaWord MArchive

emptyMeta :: Text
emptyMeta = metaWord MEmpty

-- | VALUE's word where VALUE is a starred meta; a bare word is never one.
metaOf :: Text -> Maybe Text
metaOf value = do
  inner <- T.stripSuffix "*" =<< T.stripPrefix "*" value
  if T.null inner then Nothing else Just inner

-- | Does Q name 'archiveMeta' through the @tag@ column?  Any spelling counts,
-- alternatives included, and the STARRED spelling alone.
namesArchive :: Text -> Bool
namesArchive = any names . parseFilter
  where names t = tmKey t == Just tagsKey
                    && archiveMeta `elem` alternatives (T.toLower (tmValue t))

alternatives :: Text -> [Text]
alternatives = filter (not . T.null) . T.splitOn "|"

splitKey :: Text -> Maybe (Text, Text)
splitKey text'
  | T.null key || T.null rest = Nothing
  | otherwise                 = Just (key, T.drop 1 rest)
  where (key, rest) = T.break (\c -> c == ':' || c == '=') text'


data RefRow = RefRow
  { rrId      :: !Text    -- ^ the row's own id, so a row is not its own reference.
  , rrTargets :: ![Text]  -- ^ 'Glance.Query.refSpellings' of it.
  }

newtype FilterEnv = FilterEnv
  { feRef :: Text -> Maybe RefRow      -- ^ a row id resolved, or 'Nothing' where no row claims it.
  }

emptyEnv :: FilterEnv
emptyEnv = FilterEnv (const Nothing)

-- | The environment ROWS answer as; already id-resolved, so first match wins.
storeEnv :: [HeadlineRecord] -> FilterEnv
storeEnv rows = FilterEnv resolve
  where resolve rid = (\r -> RefRow (hrId r) (refSpellings r))
                        <$> find ((== rid) . hrId) rows

-- | Does a row match Q in ENV?  Compiled once per request, never per row.
matchesFilter :: FilterEnv -> Text -> HeadlineRecord -> Bool
matchesFilter env q = case compile env (parseFilter q) of
  []     -> const True
  [test] -> test
  tests  -> \r -> all ($ r) tests

data Field = Col !Int | Planned | Ref | Order | Whole

fieldOf :: Text -> Maybe Field
fieldOf key | key == plannedKey     = Just Planned
            | key == refKey         = Just Ref
            | key == substringKey   = Just Whole
            | key `elem` viewKeys   = Just Order
            | otherwise             = Col <$> elemIndex key filterKeys

fieldCells :: Field -> [Int]
fieldCells (Col i) = [i]
fieldCells Planned = dateColumns
fieldCells Ref     = []
fieldCells Order   = []
fieldCells Whole   = []

-- | TERMS as the tests a row must all pass.  A view token is dropped HERE,
-- above the inverter: a match-all under it would make @-sort:x@ empty the table.
compile :: FilterEnv -> [Term] -> [HeadlineRecord -> Bool]
compile env = map inverted . filter ((`notElem` map Just viewKeys) . tmKey)
  where inverted t | tmNegated t = not . termTest env t
                   | otherwise   = termTest env t

-- ONE EQUATION PER CONSTRUCTOR and no wildcard, so a fifth key is named HERE by
-- the compiler rather than folded into the column arm and silently case-folded.
valueFor :: Field -> Term -> Text
valueFor Ref       = tmValue
valueFor (Col _)   = T.toLower . tmValue
valueFor Planned   = T.toLower . tmValue
valueFor Order     = T.toLower . tmValue
valueFor Whole     = T.toLower . tmValue

folded :: Term -> Text
folded = T.toLower . tmValue

termTest :: FilterEnv -> Term -> HeadlineRecord -> Bool
termTest env t = fromMaybe (freeTest (folded t)) $ do
  key   <- tmKey t
  field <- fieldOf key
  pure (predTest env key field (valueFor field t))

predTest :: FilterEnv -> Text -> Field -> Text -> HeadlineRecord -> Bool
predTest env key field value = case map (keyTest env key field) (alternatives value) of
  []    -> const True
  tests -> \r -> any ($ r) tests

freeTest :: Text -> HeadlineRecord -> Bool
freeTest value | T.null value = const True
               | otherwise    = T.isInfixOf value . hrSearch

keyTest :: FilterEnv -> Text -> Field -> Text -> HeadlineRecord -> Bool
-- An unresolvable id matches nothing; a row is not its own reference.
keyTest env _key Ref value = case feRef env value of
  Nothing  -> const False
  Just row -> \r -> hrId r /= rrId row && any (`elem` hrLinks r) (rrTargets row)
keyTest _env _key Order _value = const True
keyTest _env _key Whole value = freeTest value
-- The two that read a row's CELLS, spelled rather than left to a wildcard: a
-- fifth key falling in here would read `fieldCells'' empty list and match
-- nothing, with no warning.
keyTest _env key field@(Col _) value = cellsTest key field value
keyTest _env key field@Planned value = cellsTest key field value

-- | The cell reading `Col' and `Planned' share: every cell the key names, the
-- empty meta asking whether all of them are empty.
cellsTest :: Text -> Field -> Text -> HeadlineRecord -> Bool
cellsTest key field value
  | value == emptyMeta = \r -> all (T.null . (`cellOf` r)) cells
  | otherwise          = \r -> any ($ r) tests
  where
    cells = fieldCells field
    tests = map cellTest cells
    cellTest i
      | Just word <- tagMeta i = \r -> word `elem` tagRunEntries (cell r)
      | key == "state"         = state cell
      -- Matching reads THROUGH org's brackets: @priority:A@ = @priority:[#A]@.
      | key == "priority"      = (== priorityLetter value) . priorityLetter . cell
      | prefixed               = T.isPrefixOf value . cell
      | otherwise              = T.isInfixOf value . cell
      where cell = cellOf i
    prefixed = key `elem` dateKeys || key == plannedKey
    -- Keyed by the CELL's index, so @planned@ can never reach this meta.
    tagMeta i | i == tagsColumn = metaOf value
              | otherwise       = Nothing
    -- @*active*@ ORs in the EMPTY cell and @*inactive*@ does not, so the two
    -- do not partition the column.
    state cell r | value == activeMeta   = hrActive r == Just True || T.null (cell r)
                 | value == inactiveMeta = hrActive r == Just False
                 | otherwise             = priorityLetter (cell r) == priorityLetter value

cellOf :: Int -> HeadlineRecord -> Text
cellOf n = cellAt n . hrSearch

tagsColumn :: Int
tagsColumn = length (takeWhile (/= tagsKey) filterKeys)

-- | Field N of HAY ('Glance.Query.hrSearch'); cut rather than split.
cellAt :: Int -> Text -> Text
cellAt n hay = T.takeWhile (/= cellSep) (skip n hay)
  where skip k t | k <= 0    = t
                 | otherwise = skip (k - 1) (T.drop 1 (T.dropWhile (/= cellSep) t))
