module Data.Org.Elements.Base (Element (..)) where

import Data.Org.Elements.Headline (Headline)
import Data.Org.Elements.Pragma (Pragma)
import Data.Org.Elements.Separator (Separator)
import Data.Org.Elements.Timestamp (Timestamp)
import Data.Org.Elements.Token (Token)
import Data.Org.Identity (Identity(..))
import Data.Org.Parse (Parse, StatefulParser, parse)

import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable

import Text.Megaparsec qualified as MP
import TextShow (TextShow)
import TextShow qualified

data Element where
  Element :: ( Show a
             , TextShow a
             , Typeable a
             , Eq a
             , Parse a
             , Identity a) => a -> Element

instance Identity Element where
  identity (Element a) = identity a

instance Show Element where
  show (Element a) = show a

instance Eq Element where
  (Element x) == (Element y) = case Typeable.cast y of
    Just y' -> x == y'
    Nothing -> False

instance Parse Element where
  parse = MP.choice [ MP.try (Element <$> (parse :: StatefulParser Separator))
                    , MP.try (Element <$> (parse :: StatefulParser Headline))
                    , MP.try (Element <$> (parse :: StatefulParser Pragma))
                    , MP.try (Element <$> (parse :: StatefulParser Timestamp))
                    , Element <$> (parse :: StatefulParser Token) ]

instance TextShow Element where
  showb (Element a) = TextShow.showb a
