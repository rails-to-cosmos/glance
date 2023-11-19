{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Org.Base (Parser, StatefulParser, OrgElement (..)) where

import Data.Text (Text)
import Data.Void (Void)
import Data.Kind (Type)
import Text.Megaparsec (Parsec, MonadParsec(try))

import Control.Monad.State qualified as State

type Parser = Parsec Void Text
type StatefulParser s a = State.StateT s Parser a

class OrgElement a where
  type StateType a :: Type

  parser :: StateType a -> Parser a
  modifyState :: a -> StateType a -> StateType a

  apply :: StatefulParser (StateType a) a
  apply = do
    ctx <- State.get
    result <- State.lift $ try $ parser ctx
    State.modify $ modifyState result
    return result
