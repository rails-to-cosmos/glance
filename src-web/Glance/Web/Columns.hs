-- | The COLUMN SET a query states: @?q=@'s @columns:@ tokens as the names a
-- view shows, in written order.
--
-- 'Glance.Web.Sort''s twin, and the third reader of the ONE parse
-- ('Glance.Web.Filter.parseFilter'): which tokens NARROW is Filter's, which
-- ORDER is Sort's, which columns SHOW is this one's, so a token cannot be two
-- of those at once.  @columns:State,Title,Tags@ shows those three and nothing
-- else; the names are resolved case-insensitively downstream
-- ('Glance.Query.resolveColumns'), where one the view does not carry becomes a
-- CUSTOM column reading the headline's own property drawer — so no name is
-- refused HERE for being unknown, unknown being the feature.
--
-- The grammar is the sort token's: repeats COMPOSE in written order
-- (@columns:state columns:title@ is @columns:state,title@), a duplicate name
-- keeps its first place — case-insensitively, since resolution folds — and a
-- negation or an alternation is the whole request's refusal naming the token,
-- there being no set a reader could mean by either.  @columns:@ with nothing
-- after it is the @key:@ rule — it shows nothing and narrows nothing — and an
-- empty name between commas drops the way an empty alternative does.  A query
-- naming NO columns token keeps the default view, which is what keeps the
-- default set invisible until a reader diverges from it.
module Glance.Web.Columns (columnNamesIn) where

import Data.Text (Text)

import qualified Data.Text as T

import Glance.Web.Filter ( Term (tmKey, tmNegated, tmValue), columnsKey
                         , parseFilter, refusedOn )

-- | The column names Q states: 'Nothing' where Q names no columns token — the
-- default view stands — the names in written order where it names any, and
-- 'Left' naming the token where one cannot be read.  All-empty tokens
-- (@columns:@, @columns:,@) collapse to 'Nothing' too: they name no set, and
-- the default is what a query with nothing to say about the columns leaves
-- standing.
columnNamesIn :: Text -> Either Text (Maybe [Text])
columnNamesIn q = case filter ((== Just columnsKey) . tmKey) (parseFilter q) of
  []     -> Right Nothing
  tokens -> do
    named <- concat <$> traverse namesOf tokens
    pure $ case foldl extend [] named of
      []    -> Nothing
      names -> Just names
  where
    -- The dedup is the sort chain's: the first spelling of a name wins and the
    -- later one drops, refusing nothing — a duplicate names a column the set
    -- already shows, so nothing a reader could have meant is lost.
    extend names name
      | any (same name) names = names
      | otherwise             = names <> [name]
    same a b = T.toCaseFold a == T.toCaseFold b

-- | Every name T lists, or 'Left' with what is wrong with the token.
namesOf :: Term -> Either Text [Text]
namesOf t
  | tmNegated t                 = Left (refused t "a columns key cannot be negated")
  | T.isInfixOf "|" (tmValue t) = Left (refused t "a columns list is commas, \
                                                  \and takes no alternatives")
  | otherwise = Right (filter (not . T.null) (T.splitOn "," (tmValue t)))

-- | 'Glance.Web.Filter.refusedOn' under this reader's own key.
refused :: Term -> Text -> Text
refused = refusedOn columnsKey
