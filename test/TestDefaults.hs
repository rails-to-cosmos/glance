module TestDefaults (defaultContext, defaultHeadline) where
import Data.Org (Headline, OrgContext)

defaultHeadline :: Headline
defaultHeadline = mempty :: Headline

defaultContext :: OrgContext
defaultContext = mempty :: OrgContext
