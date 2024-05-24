module TestDefaults (defaultContext, defaultHeadline) where
import Data.Org qualified as Org

defaultHeadline :: Org.Headline
defaultHeadline = mempty :: Org.Headline

defaultContext :: Org.Context
defaultContext = mempty :: Org.Context
