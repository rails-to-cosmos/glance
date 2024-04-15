module Data.Org.PlainText (PlainText(..)) where

import           Data.Text (Text, pack)
import           Data.Org.Element
import           Data.Org.Timestamp

import           TextShow (TextShow, fromText, showb)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Control.Monad (void)

newtype PlainText = PlainText Text
  deriving (Show, Eq)

instance Semigroup PlainText where
  (<>) (PlainText lhs) (PlainText rhs) = PlainText (lhs <> rhs)

instance Monoid PlainText where
  mempty = PlainText (mempty :: Text)

instance TextShow PlainText where
  showb (PlainText t) = fromText t

instance OrgElement PlainText where
  parser = do
    let stop = lookAhead (void (parser :: OrgParser OrgTimestamp)) <|> void eol <|> eof

    PlainText <$> fmap pack (manyTill anySingle stop)
