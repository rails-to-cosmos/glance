-- | The ORDER a query states: @?q=@'s @sort:@ tokens as a
-- 'Glance.Query.SortChain'.
--
-- @table-view\/SCHEMA.md@ ("Filter query") is the contract and the renderer
-- implements the same grammar locally, so this is a port term for term, the way
-- 'Glance.Web.Filter' is — the two modules split one query between them: which
-- tokens NARROW is that one's, which tokens ORDER is this one's, and both read
-- ONE parse ('Glance.Web.Filter.parseFilter'), so a token cannot be a predicate
-- for one and an ordering for the other.
--
-- A sort token names ONE column in ONE direction: @sort:COL@ ascends,
-- @sort:COL:desc@ descends, @sort:COL:asc@ spells the default.  Written order is
-- PRECEDENCE and repeats compose, so @sort:state sort:deadline@ is state with
-- deadline settling its ties.  A query naming any sort key REPLACES the chain it
-- was asked under; one naming none leaves that chain standing, which is what
-- keeps the default order invisible until a reader diverges from it.
--
-- The refusals are the other half of "one column, one direction".  A negation, an
-- alternation, a column no view carries, a direction that is neither word, and a
-- column named twice are each a query this producer answers as an ERROR naming
-- the token — where a renderer, having nobody to refuse to, drops the key and
-- leaves the token narrowing nothing.  That divergence is deliberate and is the
-- loud half: the rows a refused query would have served are the rows it asked
-- for in an order nobody can give, and answering it quietly in another order is
-- the one thing worse than saying so.
--
-- @sort:@ with nothing after it is the @key:@ rule: it orders nothing and
-- narrows nothing, which is the half-typed token every commit passes through.
module Glance.Web.Sort (sortChainIn) where

import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (SortChain)
import Glance.Web.Filter ( Term (tmKey, tmNegated, tmValue), filterKeys
                         , parseFilter, sortKey )

-- | The direction words a token may spell, and what each means.  An unspelled
-- one ascends, so a column alone and a column with a trailing colon are the
-- same key — which is what the renderer's own table says.
directions :: [(Text, Bool)]
directions = [("", True), ("asc", True), ("desc", False)]

-- | The chain Q states, over the chain BASE it was asked under: BASE where Q
-- names no sort token, Q's own keys in written order where it names any, and
-- 'Left' naming the token where one is not a chain key at all.
--
-- BASE is what @?order=@ picked — 'Glance.Query.defaultSortChain', or the empty
-- chain for document order — so the query overrides a default rather than being
-- laid on top of it: a reader who states an order states the whole of it.
sortChainIn :: SortChain -> Text -> Either Text SortChain
sortChainIn base q = case filter ((== Just sortKey) . tmKey) (parseFilter q) of
  []     -> Right base
  tokens -> foldl (\chain t -> chain >>= extend t) (Right []) tokens
  where
    extend t chain = do
      key <- keyOf t
      case key of
        Nothing                               -> Right chain
        Just (column, _) | named column chain -> Left (twice t column)
        Just k                                -> Right (chain <> [k])
    named column = any ((== column) . fst)

-- | T as a chain key, 'Nothing' for the half-typed @sort:@ that names no
-- column, or 'Left' with what is wrong with it.
keyOf :: Term -> Either Text (Maybe (Text, Bool))
keyOf t
  | tmNegated t                 = Left (refused t "a sort key cannot be negated")
  | T.isInfixOf "|" (tmValue t) = Left (refused t "a sort token names one column")
  | T.null column               = Right Nothing            -- `sort:', half typed
  | column `notElem` filterKeys = Left (refused t ("no column is called "
                                                     <> quoted column))
  | otherwise = maybe (Left (refused t ("a sort direction is "
                                          <> spelled (map fst (drop 1 directions)))))
                      (Right . Just . (,) column)
                      (lookup dir directions)
  where
    (column, rest) = T.break (== ':') (tmValue t)
    dir            = T.toLower (T.drop 1 rest)

-- | WHY, with the token as the reader wrote it.  The parse has taken the quotes
-- out and normalized an @=@ separator to a @:@, which is as close to what was
-- typed as a refusal needs to be.
refused :: Term -> Text -> Text
refused t why = why <> ": " <> quoted (spelling t)

-- | COLUMN named a second time, which SCHEMA.md calls a producer error.
twice :: Term -> Text -> Text
twice t column = refused t ("the chain already sorts by " <> quoted column)

-- | T as the reader wrote it, negation and all.
spelling :: Term -> Text
spelling t = (if tmNegated t then "-" else "") <> sortKey <> ":" <> tmValue t

-- | WORDS as a sentence lists them: @'asc' or 'desc'@.
spelled :: [Text] -> Text
spelled = T.intercalate " or " . map quoted

quoted :: Text -> Text
quoted t = "'" <> t <> "'"
