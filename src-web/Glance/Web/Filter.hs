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
-- Three keys are no column: @planned@, which the renderer answers from the row
-- it holds; @ref:ROWID@ ('refKey'), which it cannot — resolving a reference
-- needs the store, so the renderer reads the token as free text and narrows
-- further than this does; and @sort@ ('sortKey'), which is no predicate at all.
-- A sort token states the ORDER and narrows NOTHING: it is a key here so that
-- it is never read as free text, and what it says about the order is
-- 'Glance.Web.Sort's answer to the same query.
--
-- An org TAG is not a key.  @tag:course@ is the one spelling, and the facet
-- then search a tag tree gives an org user is the two tokens
-- @tag:course text@ — what @course:text@ used to be, since a predicate reads
-- one cell and free text reads the row.  The bare form cost more than it
-- bought: the keys a query could name were the loaded rows' tags on one side of
-- the wire and the whole store's on the other, so one token was a predicate
-- here and free text there.
--
-- Combination is ONE rule: TOKENS AND, ALTERNATIVES OR.  Every token narrows,
-- whether or not another token names its key, so @state:TODO state:DONE@ is a
-- row in both states — which for a cell holding one value is no row — and
-- @tag:a tag:b@ is a row carrying both.  A row in EITHER state is the one token
-- @state:TODO|DONE@: a predicate's VALUE splits on @|@ and each alternative is
-- read as that key's own value, the results OR'd ('alternatives').  A negation
-- covers the whole token, so @-tag:a|b@ is a row carrying neither.
--
-- Alternation is a PREDICATE's rule.  A free-text token is the text it spells,
-- bar and all, and a token opening with a quote is free text whatever it
-- spells; a predicate's value has had its quotes taken out by the scanner, so a
-- bar inside one is always the operator and a literal bar is free text's alone.
--
-- Two rules are uniform across the column types: a predicate with no
-- alternative left narrows nothing (@key:@, and @key:|@ with it), and a
-- predicate's value may be quoted (@tag:"two words"@).
--
-- A value between asterisks is a META — a value with semantics of its own,
-- never cell text — and a bare word is never one, so every word a cell can hold
-- stays reachable as itself.  @key:*empty*@ is the empty cell on every key,
-- @tag:*archive*@ is the whole tag where @tag:archive@ is a substring of the
-- cell, and @state:*active*@\/@state:*inactive*@ are the keyword groups only
-- this producer can resolve ('metaOf').
--
-- The haystack is 'Glance.Query.hrSearch', built at load: the cells as they
-- display, lowercased and @\\x1f@-joined in column order.  Free text searches
-- the whole string and a predicate searches one field of it ('cellAt'), which
-- is the renderer's own @search@ and @cells@ — so the two agree by construction
-- rather than by two implementations of @displayText@ staying in step.
module Glance.Web.Filter ( FilterEnv (..)
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
                         , scanQuery
                         , sortKey
                         , storeEnv
                         , tagsKey
                         ) where

import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (hrActive, hrId, hrLinks, hrSearch)
                    , activeMeta, archiveTag, cellSep, filterKeys, inactiveMeta
                    , priorityLetter, refSpellings, tagRunEntries )

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
-- either of them holds anything, so @planned:*empty*@ is an entry nobody has
-- put a day on and @-planned:*empty*@ is the agenda's half of its query.
--
-- Decidable from the row alone, which is what makes it a key both sides can
-- carry: the renderer has the same two cells and needs no keyword set, no
-- vocabulary and no clock to answer it.  It is not a column — nothing renders a
-- @planned@ cell — and it is not a tag either, so it shadows an org tag of that
-- name the way a column does (SCHEMA.md, Filter query).
plannedKey :: Text
plannedKey = "planned"

-- | The key that states the ORDER: @sort:COL@, @sort:COL:desc@
-- ('Glance.Web.Sort').  A key here so that a sort token is never read as free
-- text, and no predicate at all — it NARROWS NOTHING, whatever it names and
-- whatever its polarity ('compile' drops the term).
--
-- Which is the whole of what this module does with it: what the chain says is
-- 'Glance.Web.Sort.sortChainIn's answer, read off the tokens this module's own
-- 'parseFilter' produced, so a token cannot be a predicate for one of them and
-- an ordering for the other.
sortKey :: Text
sortKey = "sort"

-- | The date columns, by where they sit in 'filterKeys' — which is where their
-- fields sit in the search text.
dateColumns :: [Int]
dateColumns = mapMaybe (`elemIndex` filterKeys) dateKeys

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
-- this view whose cell holds a list, which is what the whole-tag meta reads
-- ('Glance.Query.tagRunEntries').
tagsKey :: Text
tagsKey = "tag"

-- | The archive tag as a query spells it: 'Glance.Query.archiveTag' folded, the
-- way every cell was folded into the haystack at load.  It is an ordinary value
-- of the @tag@ column — @tag:archive@ is the substring the column matches by,
-- and it says nothing about which rows are served.
archiveKey :: Text
archiveKey = T.toLower archiveTag

-- | The archive tag as the META that names it: @tag:*archive*@, which matches
-- the WHOLE tag ('metaOf') and, alone among the tag values, decides what
-- @\/headlines@ serves ('namesArchive').
--
-- Two readings of one word, told apart by the stars: a tree that genuinely
-- carries a tag called @archive@ on rows it wants to see is filtered by
-- @tag:archive@ like any other tag, and only the starred spelling reaches past
-- the default view.
archiveMeta :: Text
archiveMeta = "*" <> archiveKey <> "*"

-- | The meta every key answers: the empty cell.  Uniform across the columns and
-- 'plannedKey', decided from the cell alone, and read before any column's own
-- semantics — so @state:*empty*@ is the stateless row where @state:empty@ is a
-- keyword spelled @EMPTY@.
emptyMeta :: Text
emptyMeta = "*empty*"

-- | VALUE's word where VALUE is a starred meta: one matched pair of asterisks
-- with something between them, which is @table-view.js@'s @META@ and
-- @starless@ in one answer.  'Nothing' is an ordinary value, and that is the
-- whole of the rule — a bare word is never a meta, so no spelling a cell can
-- hold is reserved.
metaOf :: Text -> Maybe Text
metaOf value = do
  inner <- T.stripSuffix "*" =<< T.stripPrefix "*" value
  if T.null inner then Nothing else Just inner

-- | Does Q name 'archiveMeta' through the @tag@ column?  Any spelling counts —
-- @tag:*archive*@, a negated one, a quoted one — because all of them are a
-- reader who has said something about archived rows, and a default exclusion
-- layered under any of them would answer a different question than the one
-- asked.
--
-- The STARRED spelling alone.  The bare @tag:archive@ is an ordinary substring
-- predicate over the tags cell and leaves the exclusion where it is, so a tree
-- that uses the word for something of its own keeps it filterable; and there is
-- no prefix question to answer, a meta being matched whole by construction.
--
-- An ALTERNATIVE counts as naming it: @tag:*archive*|web@ asks for the archived
-- rows as much as @tag:*archive*@ does, so the value is read through
-- 'alternatives' rather than whole.
--
-- The QUERY is the whole of the question.  Whether the tree carries the tag at
-- all is the caller's half — @\/headlines@ asks its vocabulary first and only
-- hides where there is something to hide — and asking it here too would be the
-- same conjunct twice: @V && not (V && N)@ is @V && not N@.
namesArchive :: Text -> Bool
namesArchive = any names . parseFilter
  where names t = tmKey t == Just tagsKey
                    && archiveMeta `elem` alternatives (T.toLower (tmValue t))

-- | VALUE's alternatives — @A|B@ is either, each read as that key's own value.
-- An EMPTY alternative is dropped, so @a|@ is @a@ and @a||b@ is @a|b@;
-- a value spelled with bars alone is left with none, and a predicate with no
-- alternative has nothing to narrow by, which is the @key:@ rule.  One answer
-- for the whole half-typed family: @key:@, @key:|@, @key:||@.
--
-- The split runs over the value the scanner produced, whose quotes are already
-- gone, so a bar inside a predicate is always the operator.  A literal one is
-- free text's — @\"a|b\"@ and the bare @a|b@ are the text they spell.
alternatives :: Text -> [Text]
alternatives = filter (not . T.null) . T.splitOn "|"

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
--
-- An empty query compiles to no test and 'all' over none passes, so a reader
-- who has said nothing is served every row.
matchesFilter :: FilterEnv -> Text -> HeadlineRecord -> Bool
matchesFilter env q = case compile env (parseFilter q) of
  []     -> const True
  [test] -> test
  tests  -> \r -> all ($ r) tests

-- | What a token's key turned out to name: a column, at its field of the search
-- text, the two date columns together ('plannedKey'), the link graph
-- ('refKey'), or the ORDER ('sortKey'), which is no field and narrows nothing.
-- Resolved once per term, so the grammar's question — is this a key — and the
-- matcher's read one answer.
data Field = Col !Int | Planned | Ref | Order

-- | KEY as the field it names, or 'Nothing' where it names none — which is the
-- test 'parseFilter' makes, so a token is a predicate exactly where there is a
-- field for it to read.
fieldOf :: Text -> Maybe Field
fieldOf key | key == plannedKey = Just Planned
            | key == refKey     = Just Ref
            | key == sortKey    = Just Order
            | otherwise         = Col <$> elemIndex key filterKeys

-- | The cells FIELD reads, by their position in 'filterKeys'.  A column is its
-- own one cell and @planned@ is the two date columns, which is the whole of
-- what makes the virtual key a column predicate over a SET of cells rather than
-- a matcher of its own: @*empty*@ is every named cell empty and a value is any
-- of them passing.  'Ref' reads no cell — it reads the link graph.
fieldCells :: Field -> [Int]
fieldCells (Col i) = [i]
fieldCells Planned = dateColumns
fieldCells Ref     = []
fieldCells Order   = []

-- | TERMS as the tests a row must all pass.  One rule, so there is nothing to
-- group: every token narrows, and two tokens naming one key are read as the AND
-- they are written as — @tag:a tag:b@ carries both, @ref:a ref:b@ points at
-- both, and @state:TODO state:DONE@ asks a cell holding one value to hold two,
-- which is no row.  What ORs is a value's alternatives, inside one token
-- ('predTest'), and a negation is that token's whole answer inverted.
--
-- A @sort@ token contributes NO test at all, and that is why it is dropped HERE
-- rather than answered inside 'keyTest': it states the order and narrows
-- nothing in EITHER polarity, where a match-all under the inverter below would
-- make @-sort:x@ the query that empties the table.  The renderer drops it at
-- the same place, above its own negation.
compile :: FilterEnv -> [Term] -> [HeadlineRecord -> Bool]
compile env = map inverted . filter ((/= Just sortKey) . tmKey)
  where inverted t | tmNegated t = not . termTest env t
                   | otherwise   = termTest env t

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

-- | T as a row test, its negation aside — 'compile' applies that.
termTest :: FilterEnv -> Term -> HeadlineRecord -> Bool
termTest env t = fromMaybe (freeTest (folded t)) $ do
  key   <- tmKey t
  field <- fieldOf key
  pure (predTest env key field (valueFor field t))

-- | @KEY:VALUE@ as a row test: VALUE's alternatives, each read as KEY's own
-- single value ('keyTest'), and a row passes on ANY of them.  With no
-- alternative left the predicate narrows nothing, which is what @key:@ means —
-- and it is the one arm that has to be spelled out, 'any' over no test failing
-- where the rule passes.
--
-- The alternatives' tests are built here, before the rows are walked, so an
-- alternation costs its alternatives once per request rather than once per row.
predTest :: FilterEnv -> Text -> Field -> Text -> HeadlineRecord -> Bool
predTest env key field value = case map (keyTest env key field) (alternatives value) of
  []    -> const True
  tests -> \r -> any ($ r) tests

-- | VALUE as free text: a substring of the row as it displays, an empty value
-- matching every row.
freeTest :: Text -> HeadlineRecord -> Bool
freeTest value | T.null value = const True
               | otherwise    = T.isInfixOf value . hrSearch

-- | @KEY:VALUE@ as a row test for ONE alternative, FIELD being what KEY
-- resolved to.  KEY is passed beside it because a column's semantics are its
-- own — a badge is whole-value, a date is a prefix — and the field is only
-- where the cell is.  VALUE is folded ('valueFor') and NON-EMPTY: 'predTest'
-- dropped the empty alternatives before this ran.
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
keyTest env _key Ref value = case feRef env value of
  Nothing  -> const False
  Just row -> \r -> hrId r /= rrId row && any (`elem` hrLinks r) (rrTargets row)
-- @sort@ is no predicate at all and never reaches this: 'compile' drops the
-- term before a test is built for it.  The arm is here for totality, and the
-- answer it gives is the one that arm means — every row.
keyTest _env _key Order _value = const True
-- Every other key is a predicate over the CELLS it names ('fieldCells'), which
-- is one for a column and the two dates for @planned@.  The two metas the set
-- decides are @*empty*@ — every named cell empty, so an unplanned row is one
-- with neither date — and a value, which ANY of them may pass.  A cell that
-- matches is a cell with something in it, so a value never needs the presence
-- test spelled beside it.
keyTest _env key field value
  | value == emptyMeta = \r -> all (T.null . (`cellOf` r)) cells
  | otherwise          = \r -> any ($ r) tests
  where
    cells = fieldCells field
    -- One test per cell, built here rather than per row: 'predTest' has already
    -- paid for the alternative, and this pays for the column once beside it.
    tests = map cellTest cells
    cellTest i
      | Just word <- tagMeta i = \r -> word `elem` tagRunEntries (cell r)
      | key == "state"         = state cell
      -- One letter, so exact — but the CELL wears org's own `[#A]' and the
      -- match reads THROUGH the brackets, on both sides: display wears the
      -- decoration and matching reads through it, which is the rule the starred
      -- metas set from the other side.  So `priority:A' and `priority:[#A]' are
      -- one query, and a renderer folding the same way answers alike.
      | key == "priority"      = (== priorityLetter value) . priorityLetter . cell
      | prefixed               = T.isPrefixOf value . cell
      | otherwise              = T.isInfixOf value . cell
      where cell = cellOf i
    -- An ISO date is matched by prefix wherever it is read, so @planned@ takes
    -- the rule with the two cells it borrowed.
    prefixed = key `elem` dateKeys || key == plannedKey
    -- A starred word on the MULTI-VALUED column is that whole entry, where the
    -- bare word is a substring of the cell: `tag:*archive*' is the tag ARCHIVE
    -- and `tag:arch' is any tag holding those letters.  It is the whole-tag
    -- reading the tag keys took with them, back as a meta on the one spelling,
    -- and the renderer's own @tagsIn@ decides it identically off the same cell —
    -- `*empty*' is read first, so a tree tagged `:empty:' reaches that tag by
    -- its bare name alone.  Keyed by the CELL's index, so @planned@ — which
    -- names the date cells — can never reach it.
    tagMeta i | i == tagsColumn = metaOf value
              | otherwise       = Nothing
    -- The two PRODUCER meta-values SCHEMA.md lets a producer add.  Group
    -- membership is resolved at LOAD, per row, by the widest scope that
    -- classifies the keyword — org's TODO/DONE, then the system layer, then the
    -- row's tags' configs, then its file ('Data.Org.Config.classify') — and
    -- arrives here as 'hrActive'.  The starred spelling is the whole of it, and
    -- the two words are the DECLARATION's ('Glance.Query.activeMeta',
    -- 'Glance.Query.inactiveMeta', which 'Glance.Query.stateValues' offers), so
    -- what the view completes over and what this answers to are one string
    -- each: `state:active' is the literal keyword `ACTIVE', which is what makes
    -- every word a file could declare reachable.
    --
    -- The groups are ASYMMETRIC over the row no scope classifies, whose
    -- 'hrActive' is 'Nothing': `*active*' takes it, a stateless entry being
    -- live work the default view would otherwise hide, and `*inactive*' does
    -- not, an entry nobody marked done not being done.  So the two do not
    -- partition the column, `-state:*active*' drops the empty cell, and
    -- `state:*empty*' — still the only way to ask for that cell alone — is a
    -- subset of `*active*'.  The empty half is spelled over the CELL rather
    -- than over 'hrActive': it is the predicate `*empty*' reads, and it is the
    -- one half a renderer can answer without knowing a keyword set.
    -- The whole-value arm folds org's priority decoration on BOTH sides,
    -- because the renderer's badge matching does (its dispatch is per column
    -- TYPE, never per key): `state:[#TODO]' matches a TODO cell there, so it
    -- must here — a query nobody writes, closed for parity's sake.
    state cell r | value == activeMeta   = hrActive r == Just True || T.null (cell r)
                 | value == inactiveMeta = hrActive r == Just False
                 | otherwise             = priorityLetter (cell r) == priorityLetter value

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
