-- | The filter query language: @?q=@ as SCHEMA.md's micro-syntax.
-- A port term for term of @table-view.js@; the grammar, the starred metas and every known divergence are in AGENTS.hs.
module Glance.Web.Filter ( Cmp (..)
                         , FilterEnv
                         , Sign (..)
                         , Term (..)
                         , Token (..)
                         , alternatives
                         , anyMeta
                         , archiveKey
                         , cellAt
                         -- Exported for the SPEC PIN alone (TestSpec): the model
                         -- spells this comparison table too, and the two readings
                         -- of `<' and `>=' must be one.
                         , cmpMark
                         , cmpTest
                         , emptyEnv
                         , emptyMeta
                         , filterKeys
                         , fromKey
                         -- Exported for the SPEC PIN alone (TestSpec), with
                         -- 'shiftIn' and 'unspaced' below: the model spells the
                         -- shift grammar and the quoted form's fold too, and the
                         -- two readings of each must be one.
                         , halfShift
                         , matchesFilter
                         , metaOf
                         , namesArchive
                         , onDay
                         , parseFilter
                         , plannedKey
                         , refKey
                         , refusedOn
                         , scanQuery
                         , shiftIn     -- SPEC PIN, see 'halfShift'
                         , signOf
                         , sortKey
                         , substringKey
                         , columnsKey
                         , storeEnv
                         , tagsKey
                         , todayMeta
                         , unspaced    -- SPEC PIN, see 'halfShift'
                         , viewAddedIn
                         ) where

import Data.Char (isDigit)
import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time (Day)

import qualified Data.Set as Set
import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (hrActive, hrId, hrSearch)
                    , EdgeIndex, edFrom, edKind, edTo, edgesInto, edgesOutOf
                    , Meta (..), Sign (..), activeMeta, archiveTag, carriesKind, cellSep
                    , dayNamed, dayOf, dayWordIn, dayWords, filterKeys, groupOn
                    , inactiveMeta, isoDay, kindCut, metaWord, priorityLetter
                    , shiftDay, shiftIn, signOf, tagRunEntries )


dateKeys :: [Text]
dateKeys = ["scheduled", "deadline"]

-- | @ref:ROWID@ — rows whose subtree points at the row named.  Producer-only, and the one predicate value that is NOT folded.
refKey :: Text
refKey = "ref"

-- | @from:ROWID@ — THE REVERSE: the rows the row named points at.  Producer-only
-- and unfolded, @ref:@'s two laws it shares; the direction is all that differs.
fromKey :: Text
fromKey = "from"

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

-- | @view:NAME@ — the saved view a query names.  A VIEW TOKEN, so it narrows nothing here: the shell expands it ahead of the fetch.
viewKey :: Text
viewKey = "view"

-- | Q refused when it ADDS a view token.  @view:@ narrows nothing, so it has
-- nothing to widen; @-view:NAME@ is left as it stands, being no @+@ query.
viewAddedIn :: Text -> Either Text ()
viewAddedIn q = case find (\t -> tmKey t == Just viewKey && tmSign t == Add) (parseFilter q) of
  Nothing -> Right ()
  Just t  -> Left (refusedOn viewKey t "a view key cannot be added")

dateColumns :: [Int]
dateColumns = mapMaybe (`elemIndex` filterKeys) dateKeys

data Token = Token
  { tkSign   :: !Sign  -- ^ the sign the token opened with.
  , tkQuoted :: !Bool  -- ^ the token opened with @"@, so it is free text whatever it spells.
  , tkBody   :: !Text  -- ^ the token itself, unquoted and unsigned.
  } deriving (Eq, Show)

-- | WHY a view-token reader refuses T under KEY; one sentence for all three readers.
refusedOn :: Text -> Term -> Text -> Text
refusedOn key t why = why <> ": '" <> spellingOf key t <> "'"

-- | T as it was written, sign and all, so a refusal quotes the reader's own token.
spellingOf :: Text -> Term -> Text
spellingOf key t = signMark (tmSign t) <> key <> ":" <> tmValue t

-- | How a sign is written.  ONE EQUATION PER CONSTRUCTOR and no wildcard, the
-- discipline 'valueFor' states: a fourth sign is named HERE by the compiler.
signMark :: Sign -> Text
signMark Unsigned = ""
signMark Neg      = "-"
signMark Add      = "+"

data Term = Term
  { tmSign  :: !Sign          -- ^ 'Neg' fails the row it matches; 'Add' joins its axis as an alternative.
  , tmKey   :: !(Maybe Text)  -- ^ the column a predicate names; 'Nothing' is free text.
  , tmValue :: !Text          -- ^ the predicate's value, or the free text itself.
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
      -- SEEN GUARDS THE SIGN, so a second one lands in the body: @+-x@ is an
      -- added free-text token spelling @-x@, the resolver's usual fallthrough.
      | not (seen s), Just sg <- signOf c = (s { seen = True, sign = sg }, acc)
      | otherwise                 = (s { body = c : body s, seen = True, hasBody = True }, acc)
    flush s acc
      | seen s    = Token (sign s) (quoted s) (T.pack (reverse (body s))) : acc
      | otherwise = acc
    fresh = Scan [] Unsigned False False False False

data Scan = Scan
  { body     :: [Char]
  , sign     :: !Sign
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
          Just (key, value) | isJust (fieldOf key) ->
            Term (tkSign t) (Just key) value
          _notAPredicate                           -> free t
    free t = Term (tkSign t) Nothing (tkBody t)

tagsKey :: Text
tagsKey = "tag"

archiveKey :: Text
archiveKey = T.toLower archiveTag

emptyMeta :: Text
emptyMeta = metaWord MEmpty

-- | @*today*@ — @today@'s OLD spelling, READ AND NEVER OFFERED, so a stored view
-- and a typed habit survive the rename.  The canonical word is the bare @today@
-- ('Glance.Query.dayWords'); the roster law is AGENTS.hs (@metaHome MToday@).
todayMeta :: Text
todayMeta = metaWord MToday

-- | @*any*@ — the starred family's ANCHOR: legal wherever a @ref:@\/@from:@ row
-- id stands and NOWHERE ELSE, and the UNION over that slot (AGENTS.hs,
-- @anyTest@ and @metaHome MAny@).  A MEMBER OF 'metas' rather than a word this
-- key reads privately, so the roster's own census sees it.
anyMeta :: Text
anyMeta = metaWord MAny

metaOf :: Text -> Maybe Text
metaOf value = do
  inner <- T.stripSuffix "*" =<< T.stripPrefix "*" value
  if T.null inner then Nothing else Just inner

-- | Does Q name the archive through the @tag@ column?  THE PLAIN WORD alone, in
-- any spelling of the token (AGENTS.hs, @archiveExclusion@).
namesArchive :: Text -> Bool
namesArchive = any names . parseFilter
  where names t = tmKey t == Just tagsKey
                    && archiveKey `elem` alternatives (T.toLower (tmValue t))

alternatives :: Text -> [Text]
alternatives = filter (not . T.null) . T.splitOn "|"

splitKey :: Text -> Maybe (Text, Text)
splitKey text'
  | T.null key || T.null rest = Nothing
  | otherwise                 = Just (key, T.drop 1 rest)
  where (key, rest) = T.break (\c -> c == ':' || c == '=') text'


-- * A timestamp key's value, as GRAMMAR
--
-- Reading alone: what the text spells, with no row and no clock in reach.  What
-- a 'Stamp' MEANS is below, beside the cell it is asked of.

-- | THE COMPARISONS a timestamp key's value may open with.  DECLARED LONGEST
-- FIRST, so 'operatorIn' reads @>=@ before @>@ and a literal @=@ is never taken
-- for an operator's tail.
data Cmp = CGe | CLe | CGt | CLt deriving (Eq, Show, Enum, Bounded)

-- | How a comparison is written.  ONE EQUATION PER CONSTRUCTOR and no wildcard,
-- the discipline 'valueFor' states: a fifth operator is named HERE by the compiler.
cmpMark :: Cmp -> Text
cmpMark CGe = ">="
cmpMark CLe = "<="
cmpMark CGt = ">"
cmpMark CLt = "<"

cmps :: [Cmp]
cmps = [minBound ..]

-- | @A..B@ — the closed interval, and the ONE thing two tokens cannot say: on a
-- multi-cell key it asks ONE CELL to lie inside where two tokens ask the axis twice.
rangeMark :: Text
rangeMark = ".."

-- | What a timestamp key's VALUE spells.  Read at COMPILE TIME and never per
-- row: 'stampTest' turns it into a cell test the rows then run.
data Stamp
  = SPrefix !Text       -- ^ the bare literal, naming the interval every stamp it prefixes reaches.
  | SCmp !Cmp !Text     -- ^ an operator and its literal.
  | SRange !Text !Text  -- ^ @A..B@: ONE cell inside the closed interval.
  deriving (Eq, Show)

-- | V as a timestamp atom, or 'Nothing' where a literal is owed and missing —
-- the HALF-TYPED tokens, which narrow nothing (AGENTS.hs, @stampOf@).
stampOf :: Text -> Maybe Stamp
stampOf v = case operatorIn v of
  Just (cmp, lit) -> SCmp cmp <$> typed lit
  Nothing         -> case T.breakOn rangeMark v of
    (lo, rest) | Just hi <- T.stripPrefix rangeMark rest -> SRange <$> typed lo <*> typed hi
    _noRange                                             -> SPrefix <$> typed v
  -- THE LONG UNIT WORD IS FOLDED AT THE LITERAL, which is the one place a
  -- literal's end is known: a range carries two of them.
  where typed t | T.null w    = Nothing
                | halfShift w = Nothing
                | otherwise   = Just w
          where w = unitFolded t

operatorIn :: Text -> Maybe (Cmp, Text)
operatorIn v = listToMaybe
  [ (cmp, rest) | cmp <- cmps, Just rest <- [T.stripPrefix (cmpMark cmp) v] ]


-- ** THE SHIFT a date literal may carry, as GRAMMAR
--
-- @BASE(+|-)N UNIT@, read below the forms at the literal, so a shifted value is
-- ONE MORE SPELLING of a day literal and 'Stamp' gains no constructor.  The
-- grammar, and the sign's two readers, are AGENTS.hs (@shiftIn@, @shiftWay@).

-- | Does L END MID-SHIFT — a @+@ with nothing but digits behind it?  THE PLUS
-- FAMILY ALONE, @-@ being ISO's own separator (AGENTS.hs, @halfShift@).
halfShift :: Text -> Bool
halfShift l = case T.unsnoc (T.dropWhileEnd isDigit l) of
  Just (_base, mark) -> signOf mark == Just Add
  Nothing            -> False

-- | THE LONG UNIT WORDS the quoted value form admits, each folded onto org's own
-- letter, LONGEST FIRST so @days@ is read before @day@ (AGENTS.hs, @unitWords@).
unitWords :: [(Text, Char)]
unitWords = [ (stem <> plural, letter)
            | (stem, letter) <- [("day", 'd'), ("week", 'w'), ("month", 'm'), ("year", 'y')]
            , plural          <- ["s", ""] ]

-- | V with every space dropped BUT THE ONE BETWEEN TWO DIGITS, the timed stamp's
-- own.  ONE PARSER, TWO SPELLINGS: the quoted form folds here, ABOVE every form
-- read, and an unquoted value carries no space for the scanner to have left
-- (AGENTS.hs, @unspaced@).
unspaced :: Text -> Text
unspaced = T.pack . go . T.unpack
  where go (a : ' ' : b : rest) | isDigit a, isDigit b = a : ' ' : go (b : rest)
                                | otherwise            = go (a : b : rest)
        go (' ' : rest)         = go rest
        go (c : rest)           = c : go rest
        go []                   = []

-- | LITERAL L's long unit word cut to org's letter, AND ONLY WHERE A SHIFT
-- COMES OUT OF IT: @today@ ends in a unit word and is THE DAY WORD ITSELF,
-- never a @to@ moved one day.
unitFolded :: Text -> Text
unitFolded l = fromMaybe l (listToMaybe
  [ short | (word, letter) <- unitWords
          , Just base      <- [T.stripSuffix word l]
          , let short = base <> T.singleton letter
          , isJust (shiftIn short) ])


-- | What a predicate may ask OUTSIDE the row it is matching: the store, which
-- the reference keys resolve an id against and read the edge map off, and the
-- request's own DAY, which @today@ names and a shift moves.  ONE CLOCK READ
-- PER REQUEST, taken before any row: the day arrives here already read, so a
-- query asked across midnight cannot mean two days.
data FilterEnv = FilterEnv
  { feSenders :: Maybe Text -> Text -> Set.Set Text  -- ^ @ref:ANCHOR@: the ids pointing AT the anchor over an edge of that kind; empty where no row claims the anchor.
  , feTargets :: Maybe Text -> Text -> Set.Set Text  -- ^ @from:ANCHOR@: THE REVERSE — the ids the anchor points at.
  , feRefAny  :: Maybe Text -> HeadlineRecord -> Bool  -- ^ @ref:*any*@: does the row point at ANOTHER row over an edge of that kind?
  , feFromAny :: Maybe Text -> HeadlineRecord -> Bool  -- ^ @from:*any*@: does ANOTHER row point at it over an edge of that kind?
  , feToday   :: Maybe Day                     -- ^ the request's own day; 'Nothing' where no clock was read, and a day word then names no day — with a shift behind it or without.
  }

-- | No store and no clock.  The reference keys then serve NO ROW in either
-- direction and under either anchor, which is what a locally-filtered path
-- answers: an id it cannot resolve, and an edge map it cannot read.
emptyEnv :: FilterEnv
emptyEnv = FilterEnv noIds noIds noEdge noEdge Nothing
  where noIds  _kind _anchor = Set.empty
        noEdge _kind _r      = False

-- | The store's own graph as the reference keys read it.  THE INDEX IS BOUND
-- LAZILY and forced at most once per request, never per row: a query naming no
-- reference key forces it not at all.
--
-- ONE RELATION, 'edgeIndex''s: a link naming no row is no reference and a row is
-- never its own, and both cuts are made where the edges are resolved rather than
-- a second time here.  An anchor no row claims carries no edge either way, so
-- the id resolution IS the lookup and needs no second reading of the rows.
storeEnv :: EdgeIndex -> FilterEnv
storeEnv ix = FilterEnv
  { feSenders = \kind anchor -> ends edFrom kind (edgesInto ix anchor)
  , feTargets = \kind anchor -> ends edTo kind (edgesOutOf ix anchor)
  , feRefAny  = \kind r -> carries kind (edgesOutOf ix (hrId r))
  , feFromAny = \kind r -> carries kind (edgesInto ix (hrId r))
  , feToday   = Nothing
  }
  where
    ends end kind = Set.map end . Set.filter (carriesKind kind . edKind)
    carries kind  = any (carriesKind kind . edKind)

-- | ENV with the request's own day on it.  THE DAY IS CARRIED AS A DAY and
-- spelled only where a literal is owed ('literalIn'), which is what lets a
-- shift move it: arithmetic on the spelling would be a second reading of one
-- date.
onDay :: Day -> FilterEnv -> FilterEnv
onDay day env = env { feToday = Just day }

-- | Does a row match Q in ENV?  Compiled once per request, never per row.
matchesFilter :: FilterEnv -> Text -> HeadlineRecord -> Bool
matchesFilter env q | null tests = const True
                    | otherwise  = \r -> all ($ r) tests
  where tests = compile env (parseFilter q)

-- | THE AXIS A KEY JOINS, and @ref@ and @from@ STAND AS TWO: one edge read from
-- its two ends, never one predicate, so every axis law reads them exactly as it
-- reads @tag@ beside @state@ (AGENTS.hs, @readsAs@).
data Field = Col !Int | Planned | Ref | From | Order | Whole deriving Eq

fieldOf :: Text -> Maybe Field
fieldOf key | key == plannedKey     = Just Planned
            | key == refKey         = Just Ref
            | key == fromKey        = Just From
            | key == substringKey   = Just Whole
            | key `elem` viewKeys   = Just Order
            | otherwise             = Col <$> elemIndex key filterKeys

fieldCells :: Field -> [Int]
fieldCells (Col i) = [i]
fieldCells Planned = dateColumns
fieldCells Ref     = []
fieldCells From    = []
fieldCells Order   = []
fieldCells Whole   = []

-- | Which fields narrow at all.  A VIEW TOKEN narrows nothing in either
-- polarity, which is why 'compile' drops it above the inverter.
narrows :: Field -> Bool
narrows Order   = False
narrows (Col _) = True
narrows Planned = True
narrows Ref     = True
narrows From    = True
narrows Whole   = True

-- | Do KEY's cells hold ISO stamps?  THE COMPARISON FORMS ARE READ ON THESE
-- KEYS AND NOWHERE ELSE, so @title:>x@ is the substring it always was.
stamped :: Text -> Bool
stamped = maybe False stampedField . fieldOf

-- | Does FIELD name date cells, and ONLY date cells?  Read off the cells the
-- field carries rather than off a key list, so a field that grows a second date
-- cell takes the operator with it.
stampedField :: Field -> Bool
stampedField field = not (null cells) && all (`elem` dateColumns) cells
  where cells = fieldCells field

-- | TERMS as the tests a row must all pass, ONE PER AXIS (AGENTS.hs,
-- @queryTest@).  A view token is dropped HERE, above the inverter: a match-all
-- under it would make @-sort:x@ empty the table.
compile :: FilterEnv -> [Term] -> [HeadlineRecord -> Bool]
compile env terms = map (axisTest . snd) (groupOn axisOf narrowing)
  where
    narrowing = [ t | t <- terms, narrows (axisOf t), not (vacuous t) ]
    -- WITHIN ONE AXIS: plain and negated AND, added OR (AGENTS.hs, @axisTest@).
    axisTest ts = \r -> (some && all ($ r) base) || any ($ r) wide
      where
        some = not (null base)
        base = [ inverted t | t <- ts, tmSign t /= Add ]
        wide = [ termTest env t | t <- ts, tmSign t == Add ]
    inverted t | tmSign t == Neg = not . termTest env t
               | otherwise       = termTest env t

-- | The axis T joins: its key's field, and 'Whole' for free text and @substring:@ alike.
axisOf :: Term -> Field
axisOf t = fromMaybe Whole (tmKey t >>= fieldOf)

-- | Does T narrow nothing and establish no axis?  AN UNSIGNED OR ADDED TERM
-- NAMING NO ATOM, dropped ahead of the grouping; a NEGATED one keeps its own law
-- and a lone @-@ still empties the table (AGENTS.hs, @vacuous@).
vacuous :: Term -> Bool
vacuous t = tmSign t /= Neg && null (atoms t)

-- | The atoms T offers its axis: a predicate's alternatives, or free text's own
-- word.  The bar is a PREDICATE's, so @+|@ is one literal atom rather than none.
atoms :: Term -> [Text]
atoms t = case tmKey t of
  Just key -> atomsUnder key (tmValue t)
  Nothing | T.null (tmValue t) -> []
          | otherwise          -> [tmValue t]

-- | KEY's alternatives as the ATOMS its predicate offers.  ON A TIMESTAMP KEY A
-- HALF-TYPED COMPARISON IS NO ATOM, so it rides 'vacuous' (AGENTS.hs, @stampOf@).
-- THE QUOTED SPELLING'S SPACES GO HERE, above every form read ('unspaced').  ONE
-- LAW, SPELLED HERE ALONE: 'vacuous' asks it of the whole term and 'predTest'
-- tests what it leaves, each calling this.
atomsUnder :: Text -> Text -> [Text]
atomsUnder key value | stamped key = filter (isJust . stampOf) (map unspaced (alternatives value))
                     | otherwise   = alternatives value

-- ONE EQUATION PER CONSTRUCTOR and no wildcard, so a sixth key is named HERE by the compiler.
valueFor :: Field -> Term -> Text
-- The two reference keys keep their case, alone among the predicates: a row id
-- is exact, and the kind half behind the `?' is slugged rather than folded.
valueFor Ref       = tmValue
valueFor From      = tmValue
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

-- | A predicate's alternatives, ORed.  THE EMPTY ARM IS THE NEGATED TERM'S
-- ALONE: 'vacuous' drops every other term naming no atom, so @-state:@ is what
-- still reaches it — every row, and inverted above, none.
predTest :: FilterEnv -> Text -> Field -> Text -> HeadlineRecord -> Bool
predTest env key field value = case map (keyTest env key field) (atomsUnder key value) of
  []    -> const True
  tests -> \r -> any ($ r) tests

freeTest :: Text -> HeadlineRecord -> Bool
freeTest value | T.null value = const True
               | otherwise    = T.isInfixOf value . hrSearch

keyTest :: FilterEnv -> Text -> Field -> Text -> HeadlineRecord -> Bool
-- THE TWO ENDS OF ONE EDGE, each its own key and its own axis, through ONE
-- reader ('edgeTest'), so no wall of theirs can come apart between them.
keyTest env _key Ref value  = edgeTest env feSenders feRefAny value
keyTest env _key From value = edgeTest env feTargets feFromAny value
keyTest _env _key Order _value = const True
keyTest _env _key Whole value = freeTest value
-- The two that read a row's CELLS, spelled out: a fifth key falling in here would read an empty cell list and match nothing, with no warning.
keyTest env key field@(Col _) value = cellsTest env key field value
keyTest env key field@Planned value = cellsTest env key field value

-- | One reference atom, ANCHORED being the direction's id set and STARRED its
-- starred anchor's test.  @*any*@ is the union over the slot; every other anchor
-- is a row id, and one no row claims matches nothing rather than 400 — this is a
-- filter, not a command.  COMPILED ONCE PER PREDICATE, so the rows run a
-- 'Set.member'.
edgeTest :: FilterEnv
         -> (FilterEnv -> Maybe Text -> Text -> Set.Set Text)
         -> (FilterEnv -> Maybe Text -> HeadlineRecord -> Bool)
         -> Text -> HeadlineRecord -> Bool
edgeTest env anchored starred value
  | anchor == anyMeta = starred env kind
  | otherwise         = \r -> Set.member (hrId r) ids
  where (anchor, kind) = anchorIn value
        ids            = anchored env kind anchor

-- | A reference value as the ANCHOR it names and the KIND it tests for.  THE CUT
-- IS THE LINK TARGET'S OWN ('kindCut') and is taken ONLY WHERE A KIND COMES OUT
-- OF IT, so a @?@ declaring no kind stays in the id and a title's own question
-- mark is text (AGENTS.hs, @anchorIn@).
anchorIn :: Text -> (Text, Maybe Text)
anchorIn value = case kindCut value of
  (row, Just kind) -> (row, Just kind)
  (_row, Nothing)  -> (value, Nothing)

-- * A 'Stamp' over a CELL, which is where the clock and the row arrive

-- | The DATE LITERAL L names, resolved HERE — once per predicate, off the one
-- clock read the request took, never per row.  THE SHIFT RESOLVES TO A PLAIN DAY
-- LITERAL, so every law below applies untouched (AGENTS.hs, @litOf@).  ONE
-- FORMATTER SPELLS BOTH SIDES: 'isoDay' the literal, @isoStamp@ the cells.
literalIn :: FilterEnv -> Text -> Maybe Text
literalIn env l = case shiftIn l of
  Just (base, n, unit) -> isoDay <$> (shiftDay unit n =<< dayIn env base)
  Nothing | namesDay l -> isoDay <$> dayWordIn (feToday env) l
          | otherwise  -> Just l

-- | Does L spell a DAY WORD rather than a date?  THE CLOCK WORDS ALONE, off the
-- one roster ('Glance.Query.dayWords'); every other literal is left byte for byte
-- what it was.  THE PREDICATE IS OWED SEPARATELY from the resolution, which is
-- what leaves @today@ matching no row under 'emptyEnv' (AGENTS.hs, @namesDay@).
namesDay :: Text -> Bool
namesDay l = isJust (lookup l dayWords)

-- | The DAY a shift's BASE names, THROUGH 'Glance.Query.dayNamed' — the base
-- reader the planning wall goes through too.  A DAY WORD and THE EMPTY BASE are
-- both read off the clock, the bare shift being today-relative by
-- @set-planning@'s own precedent; any other base is the day it spells, and one
-- naming none leaves the whole value naming none (AGENTS.hs, @dayIn@).
dayIn :: FilterEnv -> Text -> Maybe Day
dayIn env base = maybe (dayOf base) (`dayNamed` base) (feToday env)

-- | L as a literal BYTE ORDER may be asked about, which owes an opening digit:
-- the guard sits on the COMPARED forms alone, the bare form staying byte for byte
-- the prefix arm it was (AGENTS.hs, @dateOf@).
comparableIn :: FilterEnv -> Text -> Maybe Text
comparableIn env l = do
  d     <- literalIn env l
  (c,_) <- T.uncons d
  if isDigit c then Just d else Nothing

-- | S as ONE cell's test, built once per predicate.  The bare arm carries no
-- 'dated' guard and needs none — a non-empty literal is the prefix of no empty
-- cell — which is what keeps it BYTE FOR BYTE the arm it was.
stampTest :: FilterEnv -> Stamp -> Text -> Bool
stampTest env (SPrefix lit)  = maybe (const False) T.isPrefixOf (literalIn env lit)
stampTest env (SCmp cmp lit) = maybe (const False) (dated . cmpTest cmp) (comparableIn env lit)
stampTest env (SRange lo hi) = case (comparableIn env lo, comparableIn env hi) of
  (Just a, Just b) -> dated (\c -> cmpTest CGe a c && cmpTest CLe b c)
  _noDate          -> const False

-- | THE EMPTY CELL SITS OUTSIDE EVERY COMPARISON AND EVERY RANGE, so NEGATION IS
-- NO MIRROR and @-k:\<D@ serves rows @k:>=D@ does not (docs\/invariants.md).
dated :: (Text -> Bool) -> Text -> Bool
dated p c = not (T.null c) && p c

-- | THE GRANULARITY LAW, one equation per constructor: @<@ and @>=@ cut at the
-- literal's FIRST instant, @<=@ and @>@ at its LAST, spelled as the prefix test
-- the bare form already runs, so NO DATE ARITHMETIC is owed (AGENTS.hs).
cmpTest :: Cmp -> Text -> Text -> Bool
cmpTest CLt d c = c < d
cmpTest CGe d c = c >= d
cmpTest CLe d c = c < d || d `T.isPrefixOf` c
cmpTest CGt d c = c > d && not (d `T.isPrefixOf` c)

-- | The cell reading `Col' and `Planned' share: every cell the key names, the empty meta asking whether all of them are empty.
cellsTest :: FilterEnv -> Text -> Field -> Text -> HeadlineRecord -> Bool
cellsTest env key field value
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
      -- THE VALUE FORM IS READ HERE, above the rows: the operator is split off
      -- and @today@ resolved at compile time, never per row.  WHERE A KEY
      -- NAMES SEVERAL CELLS the stamp is asked of each and ORed, so a RANGE on
      -- @planned@ is ONE CELL INSIDE THE INTERVAL — the reading no pair of
      -- tokens has, two tokens ANDing at the axis instead.
      | stamped key, Just s <- stampOf value = stampTest env s . cell
      -- 'atomsUnder' drops the half-typed values, so no value reaching here
      -- fails 'stampOf'; spelled out all the same, since falling through to the
      -- substring arm would read @>=2026-09@ as text.
      | stamped key            = const False
      | otherwise              = T.isInfixOf value . cell
      where cell = cellOf i
    -- Keyed by the CELL's index, so @planned@ can never reach this meta.
    tagMeta i | i == tagsColumn = metaOf value
              | otherwise       = Nothing
-- @*active*@ ORs in the EMPTY cell where @*inactive*@ does not, so the two do not partition the column.
    state cell r | value == activeMeta   = hrActive r == Just True || T.null (cell r)
                 | value == inactiveMeta = hrActive r == Just False
                 | otherwise             = priorityLetter (cell r) == priorityLetter value

cellOf :: Int -> HeadlineRecord -> Text
cellOf n = cellAt n . hrSearch

tagsColumn :: Int
tagsColumn = length (takeWhile (/= tagsKey) filterKeys)

cellAt :: Int -> Text -> Text
cellAt n hay = T.takeWhile (/= cellSep) (skip n hay)
  where skip k t | k <= 0    = t
                 | otherwise = skip (k - 1) (T.drop 1 (T.dropWhile (/= cellSep) t))
