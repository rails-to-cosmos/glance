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
-- when KEY names a column ('filterKeys') or one of the producer's virtual keys,
-- which is what keeps org cell text — @:work:@, @=code=@ — from turning into
-- one by accident; @=@ is an alias for @:@, a leading @-@ negates either form,
-- and a token that /opens/ with a quote is free text whatever it spells.
-- Everything else is free text: a case-insensitive substring of the row as it
-- displays.
--
-- The virtual keys are this producer's org tags — every distinct tag in the
-- @tags@ column is a key of its own, so @contact:tanik@ is "tagged @contact@
-- and matching @tanik@", the facet-then-search shape a tag tree gives an org
-- user.  Membership is whole-tag, so @web:@ is not @website:@; an empty value
-- asks for the tag alone.  A column shadows a tag of the same name
-- (@title:@ stays the column), and a key that is neither is free text as
-- before.
--
-- Same-key predicates OR, distinct keys and free text AND, negations AND
-- regardless: @state:TODO state:DONE tanik@ is either state, and the text.
--
-- Three rules are uniform across the column types: @key:none@ matches the empty
-- cell (so a literal cell reading @none@ is unreachable by predicate — the
-- accepted cost of one spelling for "unset"), @key:@ with nothing after it
-- narrows nothing, and a predicate's value may be quoted (@tags:"two words"@).
--
-- The haystack is 'Glance.Query.hrSearch', built at load: the cells as they
-- display, lowercased and @\\x1f@-joined in column order.  Free text searches
-- the whole string and a predicate searches one field of it ('cellAt'), which
-- is the renderer's own @search@ and @cells@ — so the two agree by construction
-- rather than by two implementations of @displayText@ staying in step.
module Glance.Web.Filter ( Term (..)
                         , Token (..)
                         , cellAt
                         , filterKeys
                         , matchesFilter
                         , parseFilter
                         , scanQuery
                         ) where

import Data.List (elemIndex, nub)
import Data.Maybe (isNothing)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (hrKeywords, hrSearch, hrState)
                    , TodoKeywords (tkActive, tkInactive), tagsOfCell )

-- Grammar

-- | The column keys a predicate may name, in the order
-- 'Glance.Query.viewJSON' declares the columns — which is also the order
-- 'Glance.Query.hrSearch' joins the cells in, so a key's position in this list
-- is its field's position in the haystack.  Matched case-sensitively, the way
-- the renderer matches its own @columns()@ keys.
filterKeys :: [Text]
filterKeys = ["state", "priority", "title", "tags", "scheduled", "deadline"]

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

-- | TERMS as the tests a row must all pass.  Positive predicates sharing a key
-- collapse into one test any of them satisfies; a negation and a free-text
-- token each stand on their own, so @-state:TODO -state:DONE@ is neither rather
-- than either.
compile :: [Term] -> [HeadlineRecord -> Bool]
compile terms = singles <> groups
  where
    singles = [ inverted t | t <- terms, tmNegated t || isNothing (tmKey t) ]
    keyed   = [ (key, termTest t) | t <- terms, not (tmNegated t), Just key <- [tmKey t] ]
    groups  = [ anyOf [ test | (k, test) <- keyed, k == key ]
              | key <- nub (map fst keyed) ]
    inverted t | tmNegated t = not . termTest t
               | otherwise   = termTest t
    anyOf tests r = any ($ r) tests

-- | T as a row test, its negation aside — 'compile' applies that, since where a
-- term lands in the AND\/OR shape depends on it.
termTest :: Term -> HeadlineRecord -> Bool
termTest t = case tmKey t of
    Nothing  -> freeText
    -- A key that is not a column reached 'Term' by being in the vocabulary,
    -- which is to say by being a tag.
    Just key -> maybe (tagged key) (predicate key) (elemIndex key filterKeys)
  where
    value = T.toLower (tmValue t)
    -- The haystack is lowercased at load, so only the value needs folding.
    freeText | T.null value = const True
             | otherwise    = T.isInfixOf value . hrSearch
    -- A virtual key is a facet: the tag has to be on the row, and the value
    -- then searches the row the way a bare token would.  With no value it is
    -- the facet alone, which is the one place an empty value narrows anything.
    tagged key r = key `elem` tagsOfCell (cellOf tagsColumn r) && freeText r
    predicate key i
      | T.null value        = const True                    -- half-typed: narrows nothing
      | value == "none"     = T.null . cell
      | key == "state"      = state
      | key == "priority"   = (== value) . cell             -- one letter, so exact
      | key `elem` dateKeys = T.isPrefixOf value . cell
      | otherwise           = T.isInfixOf value . cell
      where
        cell = cellOf i
        -- The two meta-values SCHEMA.md lets a producer add: membership in the
        -- record's own keyword sets, which are its file's `#+TODO:' line.  A
        -- headline with no keyword is in neither.
        state r | value == "active"   = grouped tkActive r
                | value == "inactive" = grouped tkInactive r
                | otherwise           = cell r == value     -- badge: whole value
        grouped set r = maybe False (`elem` set (hrKeywords r)) (hrState r)

-- | Field N of R's search text.
cellOf :: Int -> HeadlineRecord -> Text
cellOf n = cellAt n . hrSearch

-- | Where the tags column sits in 'filterKeys', which is where its field sits
-- in the search text.
tagsColumn :: Int
tagsColumn = length (takeWhile (/= "tags") filterKeys)

-- | Field N of HAY, which is 'Glance.Query.hrSearch' — the display cells,
-- lowercased and joined by @\\x1f@ in 'filterKeys' order.  Cut rather than
-- split: a predicate reads one field and a row is not worth a list for it.
cellAt :: Int -> Text -> Text
cellAt n hay = T.takeWhile (/= cellSep) (skip n hay)
  where skip k t | k <= 0    = t
                 | otherwise = skip (k - 1) (T.drop 1 (T.dropWhile (/= cellSep) t))

-- | What 'Glance.Query.hrSearch' joins its cells with, and the one character
-- 'Glance.Query.displayText' guarantees a cell cannot hold.
cellSep :: Char
cellSep = '\US'
