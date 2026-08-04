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
-- A sort SEGMENT names ONE column in ONE direction: @sort:COL@ ascends,
-- @sort:COL:desc@ descends, @sort:COL:asc@ spells the default.  Written order is
-- PRECEDENCE and repeats compose, so @sort:state sort:deadline@ is state with
-- deadline settling its ties.  A query naming any sort key REPLACES the chain;
-- one naming none leaves the view's declared chain standing, which is what
-- keeps the default order invisible until a reader diverges from it.
--
-- A token may CHAIN its columns with @->@: @sort:state->deadline:desc@ is
-- @sort:state sort:deadline:desc@ written once.  Sugar, and ONE semantics — each
-- segment is read where it is written, exactly as a token's whole value is — so
-- the two spellings are one query and every rule here reaches across an arrow
-- unchanged.  NEGATION is the exception, and only because the @-@ is written
-- before the key: it covers every segment of the token rather than the first.
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
-- @sort:COL->@ is a SEGMENT half typed and is that same nothing, being the state
-- a reader passes through on the way to the next column.
--
-- @sort:*none*@ is the EMPTY CHAIN, and it is the query's whole vocabulary for
-- document order.  It wears the stars because it is a reserved meta rather than
-- a column — the family @*active*@, @*inactive*@, @*archive*@ and @*clear*@ are
-- already in — so no column may be called it and no cell may hold it.  The
-- empty chain ADMITS NO COMPANIONS: a token beside it names a key the answer is
-- not to have, and a reader who wrote both meant one of them, so the request is
-- refused naming the meta rather than resolved by a precedence rule nobody
-- would remember.  A SEGMENT is a companion like any other, so
-- @sort:*none*->title@ is that same pair written once.  The half-typed @sort:@
-- is no companion, since it names nothing either way.
module Glance.Web.Sort (sortChainIn) where

import Control.Monad (foldM)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (SortChain, defaultSortChain)
import Glance.Web.Filter ( Term (tmKey, tmNegated, tmValue), filterKeys
                         , parseFilter, sortKey )

-- | The direction words a token may spell, and what each means.  An unspelled
-- one ascends, so a column alone and a column with a trailing colon are the
-- same key — which is what the renderer's own table says.
directions :: [(Text, Bool)]
directions = [("", True), ("asc", True), ("desc", False)]

-- | The meta that spells the empty chain: document order, and no @sort@ field
-- on the wire.  A starred word, so it can never be a column and never a cell.
noOrder :: Text
noOrder = "*none*"

-- | The separator that CHAINS one token's columns.  @sort:a->b@ is @sort:a
-- sort:b@ said once, so this is where the sugar begins and ends.
arrow :: Text
arrow = "->"

-- | The chain Q states: 'defaultSortChain' where Q names no sort token, Q's own
-- keys in written order where it names any — segments and tokens read as one
-- sequence — the EMPTY chain where it names 'noOrder', and 'Left' naming the
-- token where one is not a chain key at all.
--
-- A query naming any sort key REPLACES the chain rather than being laid on top
-- of it: a reader who states an order states the whole of it, and the default is
-- what a query with nothing to say about the order leaves standing.
sortChainIn :: Text -> Either Text SortChain
sortChainIn q = case filter ((== Just sortKey) . tmKey) (parseFilter q) of
  []     -> Right defaultSortChain
  tokens -> do
    named <- concat <$> traverse segmentsOf tokens
    -- The half-typed segment drops out first, so what is left is the segments
    -- that have something to say about the order — which is what "no companions"
    -- counts.
    let ordering = [ pair | pair@(_t, n) <- named, orders n ]
    case [ t | (t, NoOrder) <- ordering ] of
      []      -> foldM extend [] [ (t, c, a) | (t, Column c a) <- ordering ]
      empty : _
        | length ordering > 1 -> Left (alone empty)
        | otherwise           -> Right []
  where
    orders Silent = False
    orders _named = True
    -- 'foldM' over 'Either' stops at the first refusal, so a query naming two
    -- bad keys is answered by the one written first.
    extend keys (t, column, ascending)
      | any ((== column) . fst) keys = Left (twice t column)
      | otherwise                    = Right (keys <> [(column, ascending)])

-- | What one segment of a @sort:@ token names.
data Named
  = Silent               -- ^ @sort:@ half typed: it names no column.
  | NoOrder              -- ^ @sort:*none*@: the empty chain.
  | Column !Text !Bool   -- ^ a column and whether it ascends.

-- | Every segment T chains, each paired with T for a refusal to name.  A NEGATED
-- token chains none: the @-@ stands before the key, so it covers all of them.
segmentsOf :: Term -> Either Text [(Term, Named)]
segmentsOf t
  | tmNegated t = Left (refused t "a sort key cannot be negated")
  | otherwise   = map ((,) t) <$> traverse (nameOf t) (T.splitOn arrow (tmValue t))

-- | What SEG names, or 'Left' with what is wrong with it — named on the token T
-- it was written in, which is the string the reader can go and look at.
nameOf :: Term -> Text -> Either Text Named
nameOf t seg
  | T.isInfixOf "|" seg         = Left (refused t "a sort token names one column")
  | T.null column               = Right Silent             -- `sort:', half typed
  -- The empty chain has no direction to spell: there is no key in it to reverse.
  | column == noOrder           =
      if T.null rest then Right NoOrder
      else Left (refused t (quoted noOrder <> " has no key to reverse"))
  | column `notElem` filterKeys = Left (refused t ("no column is called "
                                                     <> quoted column))
  | otherwise = maybe (Left (refused t ("a sort direction is "
                                          <> spelled (map fst (drop 1 directions)))))
                      (Right . Column column)
                      (lookup dir directions)
  where
    (column, rest) = T.break (== ':') seg
    dir            = T.toLower (T.drop 1 rest)

-- | The empty chain was asked for beside a key, which is two orders in one
-- query.  Named on the meta rather than on the key it stands beside, since the
-- meta is the token that admits nothing else.
alone :: Term -> Text
alone t = refused t (quoted noOrder <> " is the whole order and stands alone")

-- | WHY, with the token as the reader wrote it.  The parse has taken the quotes
-- out and normalized an @=@ separator to a @:@, which is as close to what was
-- typed as a refusal needs to be.
refused :: Term -> Text -> Text
refused t why = why <> ": " <> quoted (spelling t)

-- | COLUMN named a second time, in this token or an earlier one — the dedup
-- spans the segments and the token boundaries alike.
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
