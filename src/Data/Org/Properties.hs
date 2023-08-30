module Data.Org.Properties (OrgProperties (..)) where
import Data.Org.Property

newtype OrgProperties = OrgProperties [OrgProperty]
  deriving (Show, Eq)

instance Semigroup OrgProperties where
  (<>) (OrgProperties lhs) (OrgProperties rhs) = OrgProperties (lhs <> rhs)

instance Monoid OrgProperties where
  mempty = OrgProperties []
