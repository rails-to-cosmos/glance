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
-- when KEY names a column ('Glance.Query.filterKeys') or one of the producer's virtual keys,
-- which is what keeps org cell text — @:work:@, @=code=@ — from turning into
-- one by accident; @=@ is an alias for @:@, a leading @-@ negates either form,
-- and a token that /opens/ with a quote is free text whatever it spells.
-- Everything else is free text: a case-insensitive substring of the row as it
-- displays.
--
-- The virtual keys are this producer's org tags — every distinct tag in the
-- @tag@ column is a key of its own, so @contact:tanik@ is "tagged @contact@
-- and matching @tanik@", the facet-then-search shape a tag tree gives an org
-- user.  Membership is whole-tag, so @web:@ is not @website:@; an empty value
-- asks for the tag alone.  A column shadows a tag of the same name
-- (@title:@ stays the column), and a key that is neither is free text as
-- before.
--
-- Same-key predicates combine by the field's arity: a single-valued one ORs
-- (@state:TODO state:DONE tanik@ is either state and the text — ANDing a badge
-- with itself is always empty), a multi-valued one ANDs (@tag:a tag:b@ is a
-- row carrying both, and @contact:x contact:y@ is tagged @contact@ and matching
-- both texts).  Distinct keys and free text AND; negations AND regardless.
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
module Glance.Web.Filter ( Term (..)
                         , Token (..)
                         , archiveKey
                         , cellAt
                         , filterKeys
                         , matchesFilter
                         , namesArchive
                         , parseFilter
                         , scanQuery
                         ) where

import Data.List (elemIndex, nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (hrActive, hrSearch), archiveTag, cellSep
                    , filterKeys, tagsOfCell )

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

-- | Q's tokens resolved against the view's columns and VOCABULARY: a field
-- predicate where the token names one of them, free text everywhere else.
-- Columns are looked up first, so a tag sharing a column's name is shadowed by
-- it rather than the other way round (SCHEMA.md).
parseFilter :: [Text] -> Text -> [Term]
parseFilter vocabulary = map resolve . scanQuery
  where
    resolve t
      | tkQuoted t = free t
      | otherwise  = case splitKey (tkBody t) of
          Just (key, value) | known key -> Term (tkNegated t) (Just key) value
          _notAPredicate                -> free t
    known key = key `elem` filterKeys || key `elem` vocabulary
    free t = Term (tkNegated t) Nothing (tkBody t)

-- | The virtual key an archived row answers to: 'Glance.Query.archiveTag'
-- folded, the way every tag reaches the vocabulary ('tagsOfCell').  It is an
-- ordinary tag key in every respect — @archive:@, @-archive:@ and
-- @archive:draft@ all parse and match as they would for @:work:@ — and the one
-- thing that is not ordinary about it is who names it: @\/headlines@ hides
-- archived rows unless the query does ('namesArchive'), so this is the key that
-- turns the default view off.
archiveKey :: Text
archiveKey = T.toLower archiveTag

-- | Does Q name 'archiveKey' as a predicate, given VOCABULARY?  Any spelling
-- counts — @archive:@, a negated one, one carrying a value — because all of
-- them are a reader who has said something about archived rows, and a default
-- exclusion layered under any of them would answer a different question than
-- the one asked.
--
-- Resolved through 'parseFilter' rather than by scanning the string, so the
-- word only counts where it is a key: with no archived row loaded, @archive@ is
-- not in the vocabulary, @archive:x@ is free text, and this is 'False' — which
-- is sound, since there is nothing to hide either.
namesArchive :: [Text] -> Text -> Bool
namesArchive vocabulary q =
  any ((== Just archiveKey) . tmKey) (parseFilter vocabulary q)

-- | BODY at its first @:@ or @=@, when the separator has a key ahead of it and
-- is there at all.  A body opening with the separator has none, which is what
-- leaves @:work:@ and @=code=@ as the org text they are.
splitKey :: Text -> Maybe (Text, Text)
splitKey text'
  | T.null key || T.null rest = Nothing
  | otherwise                 = Just (key, T.drop 1 rest)
  where (key, rest) = T.break (\c -> c == ':' || c == '=') text'

-- Matching

-- | Does a row match Q, given the producer's VOCABULARY?  Q is parsed and
-- compiled once, so @filter (matchesFilter tags q)@ pays for the query per
-- request rather than per row — the same reason
-- 'Glance.Query.matchesSearch' takes its needle first.
matchesFilter :: [Text] -> Text -> HeadlineRecord -> Bool
matchesFilter vocabulary q = case compile (parseFilter vocabulary q) of
  []      -> const True
  [test]  -> test
  tests   -> \r -> all ($ r) tests

-- | What a predicate's key turned out to name: a column, at its field of the
-- search text, or one of the producer's virtual keys, which is a tag.  Resolved
-- once per term, so the arity and the test read one answer rather than looking
-- the key up again for each.
data Field = Col !Int | Tag
  deriving (Eq)

-- | KEY as a field.  A key that is not a column reached 'Term' by being in the
-- vocabulary, which is to say by being a tag.
fieldOf :: Text -> Field
fieldOf key = maybe Tag Col (elemIndex key filterKeys)

-- | Does FIELD hold a list of values rather than one?  The @tag@ column does,
-- and so does every virtual key; the rest of this view holds one value per
-- cell.  This is the split SCHEMA.md makes: @state:TODO state:DONE@ has to be
-- either state, since a row with both does not exist, while @tag:a tag:b@ is a
-- row carrying both, the way a label filter reads.
multiValued :: Field -> Bool
multiValued Tag     = True
multiValued (Col i) = i == tagsColumn

-- | TERMS as the tests a row must all pass.  Positive predicates sharing a key
-- collapse into one test, and which one depends on the field's arity
-- ('multiValued'): a cell holding one value can only be one of them, so they
-- OR, while a cell holding a list can hold all of them, so they AND.  A
-- negation and a free-text token each stand on their own, so
-- @-state:TODO -state:DONE@ is neither rather than either.
compile :: [Term] -> [HeadlineRecord -> Bool]
compile terms = singles <> groups
  where
    singles = [ inverted t | t <- terms, tmNegated t || isNothing (tmKey t) ]
    inverted t | tmNegated t = not . termTest t
               | otherwise   = termTest t
    keyed   = [ (key, field, keyTest key field (folded t))
              | t <- terms, not (tmNegated t), Just key <- [tmKey t]
              , let field = fieldOf key ]
    groups  = [ joining field [ test | (k, _field, test) <- keyed, k == key ]
              | (key, field) <- nub [ (k, f) | (k, f, _test) <- keyed ] ]
    joining field | multiValued field = \tests r -> all ($ r) tests
                  | otherwise         = \tests r -> any ($ r) tests

-- | T's value folded the way the haystack was folded at load, so only the value
-- ever needs folding.
folded :: Term -> Text
folded = T.toLower . tmValue

-- | T as a row test, its negation aside — 'compile' applies that, since where a
-- term lands in the AND\/OR shape depends on it.  Kept for the one list that
-- mixes the two kinds: the negations and the free text, which stand alone.
termTest :: Term -> HeadlineRecord -> Bool
termTest t = maybe (freeTest value) (\key -> keyTest key (fieldOf key) value) (tmKey t)
  where value = folded t

-- | VALUE as free text: a substring of the row as it displays, an empty value
-- matching every row.
freeTest :: Text -> HeadlineRecord -> Bool
freeTest value | T.null value = const True
               | otherwise    = T.isInfixOf value . hrSearch

-- | @KEY:VALUE@ as a row test, FIELD being what KEY resolved to.  A virtual key
-- is a facet: the tag has to be on the row, and the value then searches the row
-- the way a bare token would.  With no value it is the facet alone, which is
-- the one place an empty value narrows anything.
keyTest :: Text -> Field -> Text -> HeadlineRecord -> Bool
keyTest key Tag value =
  \r -> key `elem` tagsOfCell (cellOf tagsColumn r) && freeTest value r
keyTest key (Col i) value
  | T.null value        = const True                    -- half-typed: narrows nothing
  | value == "none"     = T.null . cell
  | key == "state"      = state
  | key == "priority"   = (== value) . cell             -- one letter, so exact
  | key `elem` dateKeys = T.isPrefixOf value . cell
  | otherwise           = T.isInfixOf value . cell
  where
    cell = cellOf i
    -- The two meta-values SCHEMA.md lets a producer add.  Group membership is
    -- resolved at LOAD, per row, by the nearest scope that classifies the
    -- keyword — the row's file, then its tags' configs, then the system layer,
    -- then org's TODO/DONE ('Data.Org.Config.classify') — and arrives here as
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
tagsColumn = length (takeWhile (/= "tag") filterKeys)

-- | Field N of HAY, which is 'Glance.Query.hrSearch' — the display cells,
-- lowercased and joined by @\\x1f@ in 'filterKeys' order.  Cut rather than
-- split: a predicate reads one field and a row is not worth a list for it.
cellAt :: Int -> Text -> Text
cellAt n hay = T.takeWhile (/= cellSep) (skip n hay)
  where skip k t | k <= 0    = t
                 | otherwise = skip (k - 1) (T.drop 1 (T.dropWhile (/= cellSep) t))
