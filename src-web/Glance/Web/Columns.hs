-- | The COLUMN SET a query states: @?q=@'s @columns:@ tokens as the names a
-- view shows, in written order.  'Glance.Web.Sort''s twin and the third reader
-- of the ONE parse ('Glance.Web.Filter.parseFilter'); rules in AGENTS.hs.
module Glance.Web.Columns (columnNamesIn) where

import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (firstBy)
import Glance.Web.Filter ( Term (tmKey, tmNegated, tmValue), columnsKey
                         , parseFilter, refusedOn )

columnNamesIn :: Text -> Either Text (Maybe [Text])
columnNamesIn q = case filter ((== Just columnsKey) . tmKey) (parseFilter q) of
  []     -> Right Nothing
  tokens -> do
    named <- concat <$> traverse namesOf tokens
    pure $ case firstBy T.toCaseFold named of
      []    -> Nothing
      names -> Just names

namesOf :: Term -> Either Text [Text]
namesOf t
  | tmNegated t                 = Left (refused t "a columns key cannot be negated")
  | T.isInfixOf "|" (tmValue t) = Left (refused t "a columns list is commas, \
                                                  \and takes no alternatives")
  | otherwise = Right (filter (not . T.null) (T.splitOn "," (tmValue t)))

refused :: Term -> Text -> Text
refused = refusedOn columnsKey
