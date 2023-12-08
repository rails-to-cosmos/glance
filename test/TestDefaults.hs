module TestDefaults (defaultContext, defaultHeadline) where
import Data.Org (OrgHeadline, OrgContext)

defaultHeadline :: OrgHeadline
defaultHeadline = mempty

defaultContext :: OrgContext
defaultContext = mempty
