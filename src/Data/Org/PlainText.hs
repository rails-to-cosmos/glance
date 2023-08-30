{-# LANGUAGE TypeFamilies #-}

module Data.Org.PlainText (PlainText (..)) where

import Data.Text (Text, pack)
import Data.Org.Base
import Data.Org.Context
import Data.Org.Timestamp (timestampCtrl)
import Data.Org.Tags (tagCtrl)
import TextShow (TextShow, fromText, showb)

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad (void)

newtype PlainText = PlainText Text
  deriving (Show, Eq)

instance Semigroup PlainText where
  (<>) (PlainText lhs) (PlainText rhs) = PlainText (lhs <> rhs)

instance Monoid PlainText where
  mempty = PlainText (mempty :: Text)

instance TextShow PlainText where
  showb (PlainText t) = fromText t

instance OrgElement PlainText where
  type StateType PlainText = OrgContext

  parser _ = do
    let stop = lookAhead $ void tagCtrl <|> void timestampCtrl <|> void eol <|> eof
    PlainText <$> pack <$> manyTill anySingle stop

  modifier _ ctx = ctx
