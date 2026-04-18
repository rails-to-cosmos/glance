{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Org.Base ( Identity (..)
                     , Display (..)
                     , Parse (..)
                     , StatelessParser
                     , StatefulParser
                     ) where

import Data.Text (Text)
import Data.Void (Void)
import Control.Monad.State.Lazy (StateT)
import Text.Megaparsec (Parsec)

class Identity a where
  identity :: a -> Maybe Text

class Display a where
  display :: a -> Text

-- | " | a -> s" means: "Given element 'a', the state 's' is known."
class Parse s a | a -> s where
    parse :: StatefulParser s a

-- type OrgParser = Context -> Text -> OrgParserResult
-- type OrgParserResult = ([Element], Context, Maybe (ParseErrorBundle Text Void))
type StatelessParser = Parsec Void Text
type StatefulParser s = StateT s StatelessParser
