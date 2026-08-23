-- | The filter query language: @?q=@ as SCHEMA.md's micro-syntax.
-- A port term for term of @table-view.js@; the grammar, the starred metas and every known divergence are in AGENTS.hs.
module Glance.Web.Filter ( Cmp (..)
                         , FilterEnv
                         , Sign (..)
                         , Term (..)
                         , Token (..)
                         , alternatives
                         , archiveKey
                         , archiveMeta
                         , cellAt
                         -- Exported for the SPEC PIN alone (TestSpec): the model
                         -- spells this comparison table too, and the two readings
                         -- of `<' and `>=' must be one.
                         , cmpMark
                         , cmpTest
                         , emptyEnv
                         , emptyMeta
                         , filterKeys
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
                         , sortKey
                         , substringKey
                         , columnsKey
                         , storeEnv
                         , tagsKey
                         , todayMeta
                         , unspaced    -- SPEC PIN, see 'halfShift'
                         , viewAddedIn
                         ) where

import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time (Day)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (hrActive, hrId, hrLinks, hrSearch)
                    , RefVia (..), refTarget, refVia
                    , Meta (..), activeMeta, archiveTag, cellSep, dayOf, filterKeys
                    , groupOn, idPropertyOf, inactiveMeta, isoDay, metaWord
                    , priorityLetter, refSpellings, shiftDay, shiftUnits, tagRunEntries )


dateKeys :: [Text]
dateKeys = ["scheduled", "deadline"]

-- | @ref:ROWID@ — rows whose subtree points at the row named.  Producer-only, and the one predicate value that is NOT folded.
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

-- | THE SIGN A TOKEN OPENS WITH, and a token wears one: the scanner reads the
-- FIRST CHARACTER alone, so a second sign is body text.
data Sign
  = Unsigned  -- ^ the token opened with neither sign.
  | Neg       -- ^ the token opened with @-@.
  | Add       -- ^ the token opened with @+@.
  deriving (Eq, Show)

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

-- | The sign C opens a token with, or 'Nothing' where C is body text.
signOf :: Char -> Maybe Sign
signOf '-' = Just Neg
signOf '+' = Just Add
signOf _   = Nothing

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

archiveMeta :: Text
archiveMeta = metaWord MArchive

emptyMeta :: Text
emptyMeta = metaWord MEmpty

-- | @*today*@ — the starred family's DATE VALUE, legal wherever a date literal
-- stands: bare, behind any operator, at either end of a range.
todayMeta :: Text
todayMeta = metaWord MToday

metaOf :: Text -> Maybe Text
metaOf value = do
  inner <- T.stripSuffix "*" =<< T.stripPrefix "*" value
  if T.null inner then Nothing else Just inner

-- | Does Q name 'archiveMeta' through the @tag@ column?  Any spelling counts, alternatives included, and the STARRED spelling alone.
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
-- @>@, @..@, @2026-08..@, @..2026-08@, @*today*+@.  Those are the HALF-TYPED
-- tokens, which narrow nothing; every other value is an atom, a literal naming
-- no date included (that one matches no row, the way @state:TOD@ matches none).
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
-- @BASE(+|-)N UNIT@ — the base a day literal, @*today*@ or nothing at all, N a
-- positive decimal run and UNIT org's own @d@\/@w@\/@m@\/@y@.  A SHIFTED VALUE
-- IS ONE MORE SPELLING OF A DAY LITERAL and no new atom kind: 'Stamp' gains no
-- constructor, the shift being read below the forms, at the literal.

-- | L as its BASE and the SIGNED COUNT and UNIT it moves that base by, or
-- 'Nothing' where L carries no shift.  READ FROM THE END, which is what keeps
-- ISO's own separator out of the sign's reach: @2026-08-03@ ends in a digit
-- where a shift ends in one of org's unit letters.  AN EMPTY BASE IS THE BARE
-- SHIFT, which 'dayIn' reads today-relative.
shiftIn :: Text -> Maybe (Text, Integer, Char)
shiftIn l = case T.unsnoc l of
  Just (run, unit)
    | unit `elem` shiftUnits
    , digits            <- T.takeWhileEnd isDigit run
    , not (T.null digits)
    , Just (base, mark) <- T.unsnoc (T.dropWhileEnd isDigit run)
    , Just way <- signOf mark
    -> Just (base, shiftWay way * decimalIn digits, unit)
  _noShift -> Nothing

-- | HOW FAR A SIGN CARRIES a shifted day.  THE SHIFT'S SIGN IS THE VALUE'S AND
-- NEVER THE TOKEN'S: 'scanQuery' reads a token's sign off its FIRST character
-- and stops there, so in @+scheduled:+30d@ the token's reader never sees the
-- value's sign and this one never sees the token's — one charset ('signOf'),
-- two readers.  ONE EQUATION PER CONSTRUCTOR and no wildcard, the discipline
-- 'valueFor' states; 'signOf' spells no character for 'Unsigned', whose day
-- stands still.
shiftWay :: Sign -> Integer
shiftWay Unsigned = 0
shiftWay Add      = 1
shiftWay Neg      = -1

-- | A decimal run as the number it spells; every character is a digit by
-- construction ('shiftIn' spans on 'isDigit' before calling this).
decimalIn :: Text -> Integer
decimalIn = T.foldl' (\n c -> n * 10 + toInteger (digitToInt c)) 0

-- | Does L END MID-SHIFT — a @+@ with nothing but digits behind it?  THE PLUS
-- FAMILY ALONE, because @+@ appears in no date a cell carries where @-@ is
-- ISO's own separator: a rule that read the incomplete minus would read
-- @2026-08-03@ as @2026-08@ moved @03@ of no unit.  An incomplete minus stays
-- the literal it always was, and @*today*-7@ matches no row rather than
-- narrowing none.
halfShift :: Text -> Bool
halfShift l = case T.unsnoc (T.dropWhileEnd isDigit l) of
  Just (_base, mark) -> signOf mark == Just Add
  Nothing            -> False

-- | THE LONG UNIT WORDS the quoted value form admits, each folded onto org's
-- own letter, LONGEST FIRST so @days@ is read before @day@.  The quoted form is
-- the one that may carry spaces (the pinned law, @tag:"two words"@), so it is
-- the one that may spell a unit out.
unitWords :: [(Text, Char)]
unitWords = [ (stem <> plural, letter)
            | (stem, letter) <- [("day", 'd'), ("week", 'w'), ("month", 'm'), ("year", 'y')]
            , plural          <- ["s", ""] ]

-- | V with every space dropped BUT THE ONE BETWEEN TWO DIGITS, which is the
-- timed stamp's own: @2026-08-01 09:30@ keeps its space where @\<= 2026-08-01@
-- loses one it never meant.  ONE PARSER, TWO SPELLINGS: the quoted form is the
-- one that may carry spaces (@scheduled:"\<= *today* + 30 days"@) and folds
-- here onto the space-free one (@scheduled:\<=*today*+30d@) ABOVE every form
-- read.  An unquoted value carries no space at all — the scanner cuts a token
-- on one — so the fold is invisible to the compact spelling.
unspaced :: Text -> Text
unspaced = T.pack . go . T.unpack
  where go (a : ' ' : b : rest) | isDigit a, isDigit b = a : ' ' : go (b : rest)
                                | otherwise            = go (a : b : rest)
        go (' ' : rest)         = go rest
        go (c : rest)           = c : go rest
        go []                   = []

-- | LITERAL L's long unit word cut to org's letter, AND ONLY WHERE A SHIFT
-- COMES OUT OF IT: @today@ ends in a unit word and is the starred word's
-- near-miss, never a @to@ moved one day.
unitFolded :: Text -> Text
unitFolded l = fromMaybe l (listToMaybe
  [ short | (word, letter) <- unitWords
          , Just base      <- [T.stripSuffix word l]
          , let short = base <> T.singleton letter
          , isJust (shiftIn short) ])


-- | What a predicate may ask OUTSIDE the row it is matching: the store, which
-- @ref:@ resolves an id against, and the request's own DAY, which @*today*@
-- names and a shift moves.  ONE CLOCK READ PER REQUEST, taken before any row:
-- the day arrives here already read, so a query asked across midnight cannot
-- mean two days.
data FilterEnv = FilterEnv
  { feRef   :: Text -> Maybe HeadlineRecord  -- ^ a row id resolved, or 'Nothing' where no row claims it.
  , feToday :: Maybe Day                     -- ^ the request's own day; 'Nothing' where no clock was read, and @*today*@ then names no day — with a shift behind it or without.
  }

emptyEnv :: FilterEnv
emptyEnv = FilterEnv (const Nothing) Nothing

storeEnv :: [HeadlineRecord] -> FilterEnv
storeEnv rows = FilterEnv (\rid -> find ((== rid) . hrId) rows) Nothing

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

data Field = Col !Int | Planned | Ref | Order | Whole deriving Eq

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

-- | Which fields narrow at all.  A VIEW TOKEN narrows nothing in either
-- polarity, which is why 'compile' drops it above the inverter.
narrows :: Field -> Bool
narrows Order   = False
narrows (Col _) = True
narrows Planned = True
narrows Ref     = True
narrows Whole   = True

-- | Do KEY's cells hold ISO stamps?  THE COMPARISON FORMS ARE READ ON THESE
-- KEYS AND NOWHERE ELSE, so @title:>x@ is the substring it always was.
stamped :: Text -> Bool
stamped = maybe False stampedField . fieldOf

-- | Does FIELD name date cells, and ONLY date cells?  Read off the cells the
-- field carries rather than off a key list — the renderer's own reading, which
-- samples the column — so a field that grows a second date cell takes the
-- operator with it instead of silently missing it.
stampedField :: Field -> Bool
stampedField field = not (null cells) && all (`elem` dateColumns) cells
  where cells = fieldCells field

-- | TERMS as the tests a row must all pass, ONE PER AXIS.  A view token is
-- dropped HERE, above the inverter: a match-all under it would make @-sort:x@
-- empty the table.  A vacuous term is dropped beside it ('vacuous').
-- Grouping is by KEY and never by adjacency, so token order carries nothing.
compile :: FilterEnv -> [Term] -> [HeadlineRecord -> Bool]
compile env terms = map (axisTest . snd) (groupOn axisOf narrowing)
  where
    narrowing = [ t | t <- terms, narrows (axisOf t), not (vacuous t) ]
    -- WITHIN ONE AXIS the plain and negated terms AND and the added ones OR
    -- against that conjunction; an axis of added terms alone is the
    -- disjunction, so a lone @+tag:work@ is @tag:work@.
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
-- NAMING NO ATOM — @state:@, @+state:@, @+state:|@, a lone @+@ — is dropped
-- ahead of the grouping: left standing it is a match-all in the conjunction
-- half and saturates its axis's disjunction, so @state: +state:DONE@ would
-- serve every row where it must serve the DONE rows.  A NEGATED one keeps its
-- own law, and a lone @-@ or @-state:@ still empties the table.
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
-- HALF-TYPED COMPARISON IS NO ATOM — an operator or a range end with no literal
-- behind it, and a shift with no unit behind it — so @scheduled:>@ and
-- @scheduled:*today*+@ ride 'vacuous' and narrow nothing, and their negations
-- empty the table exactly as @-state:@ does.  THE QUOTED SPELLING'S SPACES GO
-- HERE, above every form read ('unspaced'), so one parser answers both
-- spellings.  ONE LAW, SPELLED HERE ALONE: 'vacuous' asks it of the whole term
-- and 'predTest' tests what it leaves, each calling this, so neither can hold a
-- rule the other does not.
atomsUnder :: Text -> Text -> [Text]
atomsUnder key value | stamped key = filter (isJust . stampOf) (map unspaced (alternatives value))
                     | otherwise   = alternatives value

-- ONE EQUATION PER CONSTRUCTOR and no wildcard, so a fifth key is named HERE by the compiler.
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
-- An unresolvable id matches nothing; a row is not its own reference.
keyTest env _key Ref value = case feRef env value of
  Nothing  -> const False
  -- Over the RECORD's references, so the kind beside each one is in reach.  A
  -- link matches in ITS OWN namespace: the row's spellings for 'ViaRow', the
  -- @:ID:@ property alone for org-id's 'ViaOrgId'.
  Just row ->
    let targets = refSpellings row
        oid = idPropertyOf row
        names l = case refVia l of
          ViaRow   -> refTarget l `elem` targets
          ViaOrgId -> maybe False (refTarget l ==) oid
    in \r -> hrId r /= hrId row && any names (hrLinks r)
keyTest _env _key Order _value = const True
keyTest _env _key Whole value = freeTest value
-- The two that read a row's CELLS, spelled out: a fifth key falling in here would read an empty cell list and match nothing, with no warning.
keyTest env key field@(Col _) value = cellsTest env key field value
keyTest env key field@Planned value = cellsTest env key field value

-- * A 'Stamp' over a CELL, which is where the clock and the row arrive

-- | The DATE LITERAL L names.  @*today*@ is the request's own day and a SHIFT
-- moves whichever day its base names; both are resolved HERE — once per
-- predicate, off the one clock read the request already took, never per row.
-- THE SHIFT RESOLVES TO A PLAIN DAY LITERAL and every law below then applies
-- untouched, the granularity cuts and the empty cell's exclusion included.  ONE
-- FORMATTER SPELLS BOTH SIDES of a date comparison: 'isoDay' writes the literal
-- and @isoStamp@ writes the cells it is compared against, so the two cannot
-- drift into two shapes of one day.
literalIn :: FilterEnv -> Text -> Maybe Text
literalIn env l = case shiftIn l of
  Just (base, n, unit) -> isoDay <$> (shiftDay unit n =<< dayIn env base)
  Nothing | l == todayMeta -> isoDay <$> feToday env
          | otherwise      -> Just l

-- | The DAY a shift's BASE names.  @*today*@ and THE EMPTY BASE are both the
-- request's own day: THE BARE SHIFT IS TODAY-RELATIVE, decided off the planning
-- grammar's own precedent, which already reads a bare @+3d@ that way
-- (@set-planning@'s date), consistency being the tiebreaker.  Any other base is
-- the day it spells, so a base naming none — a month, a timed stamp — leaves
-- the whole value naming none, and it then matches no row the way @state:TOD@
-- matches none.
dayIn :: FilterEnv -> Text -> Maybe Day
dayIn env base | T.null base       = feToday env
               | base == todayMeta = feToday env
               | otherwise         = dayOf base

-- | L as a literal BYTE ORDER may be asked about, which owes an opening digit.
-- The prefix reading is total over any text; @<@ over @banana@ would serve every
-- ISO cell there is, so the guard sits on the COMPARED forms alone and the bare
-- form stays byte for byte what it was.
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

-- | THE EMPTY CELL SITS OUTSIDE EVERY COMPARISON AND EVERY RANGE: @""@ is below
-- every literal in byte order, so an unguarded @<@ would serve every undated
-- row.  @*empty*@ stays the one name for that cell, which is why @-k:\<D@ and
-- @k:>=D@ differ and NEGATION IS NO MIRROR.
dated :: (Text -> Bool) -> Text -> Bool
dated p c = not (T.null c) && p c

-- | THE GRANULARITY LAW, one equation per constructor: @<@ and @>=@ cut at the
-- literal's FIRST instant, @<=@ and @>@ at its LAST.  The last instant is
-- spelled as "everything the prefix reaches", which is the prefix test the bare
-- form already runs — so NO DATE ARITHMETIC is owed anywhere, and @k:D@ is
-- exactly @k:>=D@ and @k:\<=D@ together.
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
      -- and @*today*@ resolved at compile time, never per row.  WHERE A KEY
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
