module TestDefaults (defaultContext, defaultHeadline) where
import Data.Org (OrgHeadline, OrgContext)

defaultHeadline = mempty :: OrgHeadline
defaultContext = mempty :: OrgContext
