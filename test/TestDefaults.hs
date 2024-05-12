module TestDefaults (defaultContext, defaultHeadline) where
import Data.Org (OrgHeadline, OrgContext)

defaultHeadline :: OrgHeadline
defaultHeadline = mempty :: OrgHeadline

defaultContext :: OrgContext
defaultContext = mempty :: OrgContext
