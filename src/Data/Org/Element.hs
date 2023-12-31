{-# LANGUAGE ImportQualifiedPost #-}

module Data.Org.Element (Parser, StatefulParser, OrgElement (..)) where

import Data.Text (Text)
import Data.Void (Void)

import Data.Org.Context

import Text.Megaparsec (Parsec, MonadParsec(try))

import Control.Monad.State qualified as State

type Parser = Parsec Void Text
type StatefulParser s a = State.StateT s Parser a

class OrgElement a where
  parser :: OrgContext -> Parser a
  modifyState :: a -> OrgContext -> OrgContext

  apply :: StatefulParser OrgContext a
  apply = do
    ctx <- State.get
    result <- State.lift $ try $ parser ctx
    State.modify $ modifyState result
    return result
