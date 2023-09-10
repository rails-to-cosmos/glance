{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Org.Base (Parser, StatefulParser, OrgElement (..)) where

import Data.Text (Text)
import Data.Void ( Void )
import Data.Kind (Type)
import Text.Megaparsec ( Parsec, MonadParsec(try) )

import Control.Monad.State qualified as S

type Parser = Parsec Void Text
type StatefulParser s a = S.StateT s Parser a

class OrgElement a where
  type StateType a :: Type

  parser :: StateType a -> Parser a
  modifier :: a -> StateType a -> StateType a

  apply :: StatefulParser (StateType a) a
  apply = do
    ctx <- S.get
    result <- S.lift $ try $ parser ctx
    S.modify $ modifier result
    return result
