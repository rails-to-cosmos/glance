-- | The ORDER a query states: @?q=@'s @sort:@ tokens as a
-- 'Glance.Query.SortChain'.  'Glance.Web.Filter''s twin, reading the same one
-- parse.  THE REFUSALS are this side's alone — the renderer drops the key
-- instead.  Grammar and the starred family in AGENTS.hs.
module Glance.Web.Sort (noOrder, sortChainIn) where

import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (Meta (..), SortChain, defaultSortChain, firstBy, metaWord)
import Glance.Web.Filter ( Term (tmKey, tmNegated, tmValue), filterKeys
                         , parseFilter, refusedOn, sortKey )

directions :: [(Text, Bool)]
directions = [("", True), ("asc", True), ("desc", False)]

noOrder :: Text
noOrder = metaWord MNone

arrow :: Text
arrow = "->"

sortChainIn :: Text -> Either Text SortChain
sortChainIn q = case filter ((== Just sortKey) . tmKey) (parseFilter q) of
  []     -> Right defaultSortChain
  tokens -> do
    named <- concat <$> traverse segmentsOf tokens
    -- Half-typed segments drop FIRST, so "no companions" counts orders alone.
    let ordering = [ pair | pair@(_t, n) <- named, orders n ]
    case [ t | (t, NoOrder) <- ordering ] of
      -- The DEDUP refuses nothing: a duplicate names an order the chain has.
      []      -> Right (firstBy fst [ (c, a) | (_t, Column c a) <- ordering ])
      empty : _
        | length ordering > 1 -> Left (alone empty)
        | otherwise           -> Right []
  where
    orders Silent = False
    orders _named = True

data Named
  = Silent               -- ^ @sort:@ half typed: it names no column.
  | NoOrder              -- ^ @sort:*none*@: the empty chain.
  | Column !Text !Bool   -- ^ a column and whether it ascends.

segmentsOf :: Term -> Either Text [(Term, Named)]
segmentsOf t
  | tmNegated t = Left (refused t "a sort key cannot be negated")
  | otherwise   = map ((,) t) <$> traverse (nameOf t) (T.splitOn arrow (tmValue t))

nameOf :: Term -> Text -> Either Text Named
nameOf t seg
  | T.isInfixOf "|" seg         = Left (refused t "a sort token names one column")
  | T.null column               = Right Silent             -- `sort:', half typed
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

alone :: Term -> Text
alone t = refused t (quoted noOrder <> " is the whole order and stands alone")

refused :: Term -> Text -> Text
refused = refusedOn sortKey

spelled :: [Text] -> Text
spelled = T.intercalate " or " . map quoted

quoted :: Text -> Text
quoted t = "'" <> t <> "'"
