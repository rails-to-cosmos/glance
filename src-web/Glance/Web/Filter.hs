-- | The filter query language: @?q=@ as SCHEMA.md's micro-syntax rather than
-- one substring.
--
-- @table-view\/SCHEMA.md@ ("Filter query") is the contract, and the renderer
-- implements the same grammar locally, so a query has to mean the same thing on
-- both sides of the wire — a producer that narrowed differently would answer a
-- filtered page the renderer would not have drawn.  This module is that
-- grammar, ported term by term from @web\/table-view.js@'s @scanQuery@,
-- @parseQuery@ and @tokenTest@.
--
-- Tokens separate on whitespace and @&@.  @key:value@ is a field predicate only
-- when KEY names a column ('Glance.Query.filterKeys') or one of the two keys
-- that are no column, which is what keeps org cell text — @:work:@, @=code=@ —
-- from turning into one by accident; @=@ is an alias for @:@, a leading @-@
-- negates either form, and a token that /opens/ with a quote is free text
-- whatever it spells.  Everything else is free text: a case-insensitive
-- substring of the row as it displays.
--
-- Those two have no vocabulary behind them and no renderer branch: @planned@,
-- which the renderer can still answer from the row it holds, and @ref:ROWID@
-- ('refKey'), which it cannot — resolving a reference needs the store, so the
-- renderer reads the token as free text and narrows further than this does.
--
-- An org TAG is not a key.  @tag:course@ is the one spelling, and the facet
-- then search a tag tree gives an org user is the two tokens
-- @tag:course text@ — what @course:text@ used to be, since a predicate reads
-- one cell and free text reads the row.  The bare form cost more than it
-- bought: the keys a query could name were the loaded rows' tags on one side of
-- the wire and the whole store's on the other, so one token was a predicate
-- here and free text there.
--
-- Same-key predicates combine by the field's arity: a single-valued one ORs
-- (@state:TODO state:DONE tanik@ is either state and the text — ANDing a badge
-- with itself is always empty), a multi-valued one ANDs (@tag:a tag:b@ is a
-- row carrying both).  Distinct keys and free text AND; negations AND
-- regardless.
--
-- Three rules are uniform across the column types: @key:none@ matches the empty
-- cell (so a literal cell reading @none@ is unreachable by predicate — the
-- accepted cost of one spelling for "unset"), @key:@ with nothing after it
-- narrows nothing, and a predicate's value may be quoted (@tag:"two words"@).
--
-- The haystack is 'Glance.Query.hrSearch', built at load: the cells as they
-- display, lowercased and @\\x1f@-joined in column order.  Free text searches
-- the whole string and a predicate searches one field of it ('cellAt'), which
-- is the renderer's own @search@ and @cells@ — so the two agree by construction
-- rather than by two implementations of @displayText@ staying in step.
module Glance.Web.Filter ( FilterEnv (..)
                         , Term (..)
                         , Token (..)
                         , archiveKey
                         , cellAt
                         , emptyEnv
                         , filterKeys
                         , matchesFilter
                         , namesArchive
                         , parseFilter
                         , plannedKey
                         , refKey
                         , scanQuery
                         , storeEnv
                         , tagsKey
                         ) where

import Data.List (elemIndex, find, nub)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (hrActive, hrId, hrLinks, hrSearch)
                    , archiveTag, cellSep, filterKeys, refSpellings )

-- Grammar
--
-- The keys a predicate may name are the view's own columns
-- ('Glance.Query.filterKeys'), re-exported here because the grammar is this
-- module's: a key's position in that list is its field's position in the
-- haystack, since the columns are declared and the cells are joined in one
-- order.

-- | The cells matched by prefix rather than by substring: an ISO date, so
-- @scheduled:2026-08@ is the month.  The renderer decides this per column by
-- sampling its cells; here the two date columns are known by name.
dateKeys :: [Text]
dateKeys = ["scheduled", "deadline"]

-- | The virtual key that reads the link graph: @ref:ROWID@ is every row whose
-- subtree POINTS AT the row named, which is the drill-down @\@@ applies.
--
-- Producer-only, and in that respect the starred metas' relative rather than
-- @planned@'s: a row cannot answer it alone, since deciding it needs the target
-- row's @ORG_GLANCE_ID@ and title and only the store holds those.  The renderer
-- has no branch for it and reads @ref:x@ as free text, which is narrower —
-- SCHEMA.md's blessed direction for a divergence.
--
-- Values are NOT folded here, alone among the predicates: the value is a row
-- id, and a row id is exact-string everywhere else in this library
-- ('Glance.Query.resolveIds').  The corpus settles it — ~\/sync carries ids
-- spelled @Password-…@ and @Pets-…@, which a fold would put beyond reach.
refKey :: Text
refKey = "ref"

-- | The virtual key over the two date columns together: a row is @planned@ when
-- either of them holds anything, so @planned:none@ is an entry nobody has put a
-- day on and @-planned:none@ is the agenda's half of its query.
--
-- Decidable from the row alone, which is what makes it a key both sides can
-- carry: the renderer has the same two cells and needs no keyword set, no
-- vocabulary and no clock to answer it.  It is not a column — nothing renders a
-- @planned@ cell — and it is not a tag either, so it shadows an org tag of that
-- name the way a column does (SCHEMA.md, Filter query).
plannedKey :: Text
plannedKey = "planned"

-- | The date columns' cells, one reader each: where they sit in 'filterKeys' is
-- where their fields sit in the search text.
dateCells :: [HeadlineRecord -> Text]
dateCells = map cellOf (mapMaybe (`elemIndex` filterKeys) dateKeys)

-- | One token of a query, as 'scanQuery' cuts it: the quotes and the leading
-- @-@ are gone from 'tkBody', and what they meant is recorded beside it.  (The
-- renderer's token carries its offsets too — they place a caret inside a token
-- for autocomplete, and nothing here needs them.)
data Token = Token
  { tkNegated :: !Bool  -- ^ the token opened with @-@.
  , tkQuoted  :: !Bool  -- ^ the token opened with @"@, so it is free text whatever it spells.
  , tkBody    :: !Text  -- ^ the token itself, unquoted and un-negated.
  } deriving (Eq, Show)

-- | A token resolved against 'filterKeys'.
data Term = Term
  { tmNegated :: !Bool          -- ^ the row fails when this term matches.
  , tmKey     :: !(Maybe Text)  -- ^ the column a predicate names; 'Nothing' is free text.
  , tmValue   :: !Text          -- ^ the predicate's value, or the free text itself.
  } deriving (Eq, Show)

-- | Is C a token separator?  Whitespace and @&@ — the renderer's own @isSep@,
-- which is why a carriage return is not one.
isSep :: Char -> Bool
isSep c = c == '&' || c == ' ' || c == '\t' || c == '\n'

-- | Q cut into tokens.  Quotes suppress separators and are dropped; a quote
-- ahead of any body character marks the token free text; a @-@ ahead of
-- everything negates it.  An unclosed quote runs to the end of Q, so a query
-- being typed one character at a time never loses the token it is in.
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

-- | The scanner's state: the token being read, and what has been seen of it.
-- 'body' accumulates reversed.
data Scan = Scan
  { body     :: [Char]
  , negated  :: !Bool
  , quoted   :: !Bool
  , seen     :: !Bool
  , hasBody  :: !Bool
  , inQuotes :: !Bool
  }

-- | Q's tokens resolved against the keys a predicate may name: a field
-- predicate where the token names one, free text everywhere else.  One
-- resolution, 'fieldOf', decides both whether a key is a key and what it reads,
-- so the grammar and the matcher cannot disagree about a token.
parseFilter :: Text -> [Term]
parseFilter = map resolve . scanQuery
  where
    resolve t
      | tkQuoted t = free t
      | otherwise  = case splitKey (tkBody t) of
          Just (key, value) | isJust (fieldOf key) -> Term (tkNegated t) (Just key) value
          _notAPredicate                           -> free t
    free t = Term (tkNegated t) Nothing (tkBody t)

-- | The tags column's key, singular where its header is plural.  Spelled once:
-- it is the column an archive query names ('namesArchive') and the one field of
-- this view that holds a list ('multiValued').
tagsKey :: Text
tagsKey = "tag"

-- | The archive tag as a query spells it: 'Glance.Query.archiveTag' folded, the
-- way every cell was folded into the haystack at load.  It is an ordinary value
-- of the @tag@ column in every respect — @tag:archive@ and @-tag:archive@ match
-- as they would for @:work:@ — and the one thing that is not ordinary about it
-- is who names it: @\/headlines@ hides archived rows unless the query does
-- ('namesArchive'), so this is the value that turns the default view off.
archiveKey :: Text
archiveKey = T.toLower archiveTag

-- | Does Q name 'archiveKey' through the @tag@ column, given VOCABULARY?  Any
-- spelling counts — @tag:archive@, a negated one, a quoted one — because all of
-- them are a reader who has said something about archived rows, and a default
-- exclusion layered under any of them would answer a different question than
-- the one asked.
--
-- The value is matched WHOLE, where the predicate itself reads the cell by
-- substring: @tag:arch@ finds an archived row and does not turn the exclusion
-- off, so it answers empty.  The alternative is a prefix of a prefix deciding
-- what the default view shows.
--
-- VOCABULARY is the tree's tags, and the word only counts where the tree
-- carries one — sound, since with nothing archived there is nothing to hide.
namesArchive :: [Text] -> Text -> Bool
namesArchive vocabulary q =
  archiveKey `elem` vocabulary && any names (parseFilter q)
  where names t = tmKey t == Just tagsKey && T.toLower (tmValue t) == archiveKey

-- | BODY at its first @:@ or @=@, when the separator has a key ahead of it and
-- is there at all.  A body opening with the separator has none, which is what
-- leaves @:work:@ and @=code=@ as the org text they are.
splitKey :: Text -> Maybe (Text, Text)
splitKey text'
  | T.null key || T.null rest = Nothing
  | otherwise                 = Just (key, T.drop 1 rest)
  where (key, rest) = T.break (\c -> c == ':' || c == '=') text'

-- Matching

-- | A row a @ref:@ term names, reduced to what the rest of the store can say
-- about it: its id, and every spelling a link to it may carry.
data RefRow = RefRow
  { rrId      :: !Text    -- ^ the row's own id, so a row is not its own reference.
  , rrTargets :: ![Text]  -- ^ 'Glance.Query.refSpellings' of it.
  }

-- | What a query needs beyond the row in hand, which is one key's worth:
-- @ref:@, since resolving a reference needs the store.  Every other predicate
-- is decided from the row's own cells.
newtype FilterEnv = FilterEnv
  { feRef :: Text -> Maybe RefRow      -- ^ a row id resolved, or 'Nothing' where no row claims it.
  }

-- | An environment with no store behind it: @ref:@ resolves nothing, so a
-- @ref:@ term parses as a predicate and matches no row.  What a caller holding
-- rows but no index answers with.
emptyEnv :: FilterEnv
emptyEnv = FilterEnv (const Nothing)

-- | The environment ROWS answer as: @ref:@ resolved by exact row id over the
-- rows themselves.
--
-- The rows are the store's, which is to say already id-resolved
-- ('Glance.Query.resolveIds'), so the first match IS the resolution and a
-- loser's row can never be what a @ref:@ points at.  The scan is linear and
-- runs once per @ref:@ term per request rather than once per row, since
-- 'compile' builds each term's test before the rows are walked.
--
-- The rows are the whole of the environment: an org tag is not a key, so no
-- query depends on which tags a tree carries and the matcher asks for none.
storeEnv :: [HeadlineRecord] -> FilterEnv
storeEnv rows = FilterEnv resolve
  where resolve rid = (\r -> RefRow (hrId r) (refSpellings r))
                        <$> find ((== rid) . hrId) rows

-- | Does a row match Q in ENV?  Q is parsed and compiled once, so
-- @filter (matchesFilter env q)@ pays for the query per request rather than per
-- row — the same reason 'Glance.Query.matchesSearch' takes its needle first.
matchesFilter :: FilterEnv -> Text -> HeadlineRecord -> Bool
matchesFilter env q = case compile env (parseFilter q) of
  []      -> const True
  [test]  -> test
  tests   -> \r -> all ($ r) tests

-- | What a predicate's key turned out to name: a column, at its field of the
-- search text, the two date columns together ('plannedKey'), or the link graph
-- ('refKey').  Resolved once per term, so the arity and the test read one
-- answer rather than looking the key up again for each.
data Field = Col !Int | Planned | Ref
  deriving (Eq)

-- | KEY as the field it names, or 'Nothing' where it names none — which is the
-- test 'parseFilter' makes, so a token is a predicate exactly where there is a
-- field for it to read.
fieldOf :: Text -> Maybe Field
fieldOf key | key == plannedKey = Just Planned
            | key == refKey     = Just Ref
            | otherwise         = Col <$> elemIndex key filterKeys

-- | Does FIELD hold a list of values rather than one?  The @tag@ column does;
-- the rest of this view holds one value per cell.  This is the split SCHEMA.md
-- makes: @state:TODO state:DONE@ has to be either state, since a row with both
-- does not exist, while @tag:a tag:b@ is a row carrying both, the way a label
-- filter reads.
--
-- @planned@ reads one of two dates, so it ORs like the date columns it stands
-- over: @planned:2026-08 planned:2026-09@ is either month.  @ref@ reads a LIST
-- — the targets a subtree points at — so it ANDs like the tags do, and
-- @ref:a ref:b@ is a row referring to both.
multiValued :: Field -> Bool
multiValued Ref     = True
multiValued Planned = False
multiValued (Col i) = i == tagsColumn

-- | TERMS as the tests a row must all pass.  Positive predicates sharing a key
-- collapse into one test, and which one depends on the field's arity
-- ('multiValued'): a cell holding one value can only be one of them, so they
-- OR, while a cell holding a list can hold all of them, so they AND.  A
-- negation and a free-text token each stand on their own, so
-- @-state:TODO -state:DONE@ is neither rather than either.
compile :: FilterEnv -> [Term] -> [HeadlineRecord -> Bool]
compile env terms = singles <> groups
  where
    singles = [ inverted t | t <- terms, tmNegated t || isNothing (tmKey t) ]
    inverted t | tmNegated t = not . termTest env t
               | otherwise   = termTest env t
    keyed   = [ (key, field, keyTest env key field (valueFor field t))
              | t <- terms, not (tmNegated t), Just key <- [tmKey t]
              , Just field <- [fieldOf key] ]
    groups  = [ joining field [ test | (k, _field, test) <- keyed, k == key ]
              | (key, field) <- nub [ (k, f) | (k, f, _test) <- keyed ] ]
    joining field | multiValued field = \tests r -> all ($ r) tests
                  | otherwise         = \tests r -> any ($ r) tests

-- | T's value as FIELD reads it.  Every field but 'Ref' folds it the way the
-- haystack was folded at load, so only the value ever needs folding; 'Ref'
-- takes a row id and keeps its case ('refKey').
valueFor :: Field -> Term -> Text
valueFor Ref = tmValue
valueFor _   = T.toLower . tmValue

-- | T's value as FREE text, which is always folded — a token that resolved to
-- no key is searched against the haystack whatever it spells.
folded :: Term -> Text
folded = T.toLower . tmValue

-- | T as a row test, its negation aside — 'compile' applies that, since where a
-- term lands in the AND\/OR shape depends on it.  Kept for the one list that
-- mixes the two kinds: the negations and the free text, which stand alone.
termTest :: FilterEnv -> Term -> HeadlineRecord -> Bool
termTest env t = fromMaybe (freeTest (folded t)) $ do
  key   <- tmKey t
  field <- fieldOf key
  pure (keyTest env key field (valueFor field t))

-- | VALUE as free text: a substring of the row as it displays, an empty value
-- matching every row.
freeTest :: Text -> HeadlineRecord -> Bool
freeTest value | T.null value = const True
               | otherwise    = T.isInfixOf value . hrSearch

-- | @KEY:VALUE@ as a row test, FIELD being what KEY resolved to.  KEY is passed
-- beside it because a column's semantics are its own — a badge is whole-value,
-- a date is a prefix — and the field is only where the cell is.
keyTest :: FilterEnv -> Text -> Field -> Text -> HeadlineRecord -> Bool
-- @ref@ over the link targets a subtree carries ('Glance.Query.hrLinks'),
-- matched against how a link may spell the row named ('refSpellings').
--
-- An id NO row claims matches nothing, and that is the whole of the refusal:
-- this is a filter rather than a command, so an unresolvable id narrows to the
-- empty table the way @tag:nosuchtag@ does, and nothing 400s.  A stale @ref:@
-- in a bookmarked URL therefore opens an empty view rather than an error page.
--
-- A row is not its own reference: an entry whose body links to itself — which
-- org-glance's own materialize footer writes — would otherwise be the one row
-- every drill-down was guaranteed to find, and a list of references that always
-- holds the row you came from is a list with one useless entry in it.
keyTest env _key Ref value
  | T.null value = const True                             -- half-typed: narrows nothing
  | otherwise    = case feRef env value of
      Nothing  -> const False
      Just row -> \r -> hrId r /= rrId row
                        && any (`elem` hrLinks r) (rrTargets row)
-- @planned@ over its two cells: unplanned is neither of them holding anything,
-- and a value is the date prefix @scheduled:@ and @deadline:@ each take, asked
-- of both at once.  A cell that prefix-matches is a cell with something in it,
-- so a value never needs the presence test spelled beside it.
keyTest _env _key Planned value
  | T.null value    = const True                        -- half-typed: narrows nothing
  | value == "none" = \r -> all (T.null . ($ r)) dateCells
  | otherwise       = \r -> any (T.isPrefixOf value . ($ r)) dateCells
keyTest _env key (Col i) value
  | T.null value        = const True                    -- half-typed: narrows nothing
  | value == "none"     = T.null . cell
  | key == "state"      = state
  | key == "priority"   = (== value) . cell             -- one letter, so exact
  | key `elem` dateKeys = T.isPrefixOf value . cell
  | otherwise           = T.isInfixOf value . cell
  where
    cell = cellOf i
    -- The two meta-values SCHEMA.md lets a producer add.  Group membership is
    -- resolved at LOAD, per row, by the widest scope that classifies the
    -- keyword — org's TODO/DONE, then the system layer, then the row's tags'
    -- configs, then its file ('Data.Org.Config.classify') — and arrives here as
    -- 'hrActive'.  Each answers to two spellings — org-glance writes the groups
    -- `*active*' and `*inactive*', and the view offers those
    -- ('Glance.Query.stateValues') — so the stars come off before the
    -- comparison and `state:active' stays the alias it was.
    --
    -- The groups are ASYMMETRIC over the row no scope classifies, whose
    -- 'hrActive' is 'Nothing': `*active*' takes it, a stateless entry being
    -- live work the default view would otherwise hide, and `*inactive*' does
    -- not, an entry nobody marked done not being done.  So the two do not
    -- partition the column, `-state:*active*' drops the empty cell, and
    -- `state:none' — still the only way to ask for that cell alone — is a
    -- subset of `*active*'.  The empty half is spelled over the CELL rather
    -- than over 'hrActive': it is the predicate `none' reads, and it is the one
    -- half a renderer can answer without knowing a keyword set.
    state r | meta == "active"   = hrActive r == Just True || T.null (cell r)
            | meta == "inactive" = hrActive r == Just False
            | otherwise          = cell r == value      -- badge: whole value
    meta = starless value

-- | VALUE with one matched pair of asterisks taken off it.  The alias reaches
-- the two state meta-values alone, where it is asked for the group names
-- org-glance itself writes: @state:*active*@ and @state:active@ are one query.
-- There is no glob here — @state:*TODO*@ comes out as the literal badge text
-- @*todo*@, which no cell holds, and matches nothing.
starless :: Text -> Text
starless value = fromMaybe value (T.stripSuffix "*" =<< T.stripPrefix "*" value)

-- | Field N of R's search text.
cellOf :: Int -> HeadlineRecord -> Text
cellOf n = cellAt n . hrSearch

-- | Where the tag column sits in 'filterKeys', which is where its field sits
-- in the search text.
tagsColumn :: Int
tagsColumn = length (takeWhile (/= tagsKey) filterKeys)

-- | Field N of HAY, which is 'Glance.Query.hrSearch' — the display cells,
-- lowercased and joined by @\\x1f@ in 'filterKeys' order.  Cut rather than
-- split: a predicate reads one field and a row is not worth a list for it.
cellAt :: Int -> Text -> Text
cellAt n hay = T.takeWhile (/= cellSep) (skip n hay)
  where skip k t | k <= 0    = t
                 | otherwise = skip (k - 1) (T.drop 1 (T.dropWhile (/= cellSep) t))
