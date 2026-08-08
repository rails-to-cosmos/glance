-- | The ORDER a query states: @?q=@'s @sort:@ tokens as a
-- 'Glance.Query.SortChain'.
--
-- 'Glance.Web.Filter''s twin: the two split one query — which tokens NARROW is
-- that one's, which ORDER is this one's — and both read ONE parse, so a token
-- cannot be a predicate for one and an ordering for the other.
--
-- A SEGMENT names ONE column in ONE direction (@sort:COL@, @:desc@, @:asc@).
-- Written order is PRECEDENCE and repeats compose.  A query naming any sort key
-- REPLACES the chain; one naming none leaves the view's standing, which keeps
-- the default order invisible until a reader diverges from it.
--
-- @->@ CHAINS a token's columns and is SUGAR: each segment is read where it is
-- written, exactly as a whole token's value is, so no rule below knows which
-- spelling it came from.  NEGATION is the exception — the @-@ stands before the
-- key, so it covers every segment.
--
-- THE REFUSALS are the other half of "one column, one direction": a negation,
-- an alternation, an unknown column and a direction that is neither word are
-- each an ERROR naming the token, where the renderer has nobody to refuse to
-- and drops the key.  Deliberate, and the loud half — answering quietly in
-- another order is the one thing worse than saying so.
--
-- @sort:@ and @sort:COL->@ are half typed: they order nothing and narrow
-- nothing, the @key:@ rule.
--
-- @sort:*none*@ is the EMPTY CHAIN and the query's whole vocabulary for
-- document order.  It wears the stars because it is a meta rather than a
-- column, and it ADMITS NO COMPANIONS: a reader who wrote both meant one of
-- them, so the request is refused rather than resolved by a precedence rule
-- nobody would remember.  A SEGMENT is a companion like any other.
module Glance.Web.Sort (sortChainIn) where

import Control.Monad (foldM)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (SortChain, defaultSortChain)
import Glance.Web.Filter ( Term (tmKey, tmNegated, tmValue), filterKeys
                         , parseFilter, refusedOn, sortKey )

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
      []      -> foldM extend [] [ (c, a) | (_t, Column c a) <- ordering ]
      empty : _
        | length ordering > 1 -> Left (alone empty)
        | otherwise           -> Right []
  where
    orders Silent = False
    orders _named = True
    -- EVERY REFUSAL IS ALREADY SPENT by the time this runs: 'segmentsOf' and
    -- 'nameOf' answer in 'Either' and the 'traverse' above stops at the first
    -- of them, so a query naming two bad keys is answered by the one written
    -- first.  What is left here is the DEDUP, which refuses nothing on either
    -- side: the first spelling of a column wins and the later key drops, which
    -- is the renderer's own rule and what SCHEMA.md records — a duplicate names
    -- an order the chain already has, so nothing a reader could have meant is
    -- lost.
    extend keys (column, ascending)
      | any ((== column) . fst) keys = Right keys
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

-- | 'Glance.Web.Filter.refusedOn' under this reader's own key.
refused :: Term -> Text -> Text
refused = refusedOn sortKey

-- | WORDS as a sentence lists them: @'asc' or 'desc'@.
spelled :: [Text] -> Text
spelled = T.intercalate " or " . map quoted

quoted :: Text -> Text
quoted t = "'" <> t <> "'"
