{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Elements.Base (Element (..)) where

import Data.Typeable (Typeable)
import Data.Org.Identifiable (Identifiable)
import Data.Org.Elements.Headline (Headline)
import Data.Org.Elements.Pragma (Pragma)
import Data.Org.Elements.Separator (Separator)
import Data.Org.Elements.Timestamp (Timestamp)
import Data.Org.Elements.Token (Token)
import Data.Org.Parser (Parseable, StatefulParser, parser)
import Data.Typeable qualified as Typeable

import Text.Megaparsec (try, choice)

import TextShow (TextShow)
import TextShow qualified

data Element where
  Element :: ( Show a
             , TextShow a
             , Typeable a
             , Eq a
             , Parseable a
             , Identifiable a ) => a -> Element

instance Show Element where
  show (Element a) = show a

instance Eq Element where
  (Element x) == (Element y) = case Typeable.cast y of
    Just y' -> x == y'
    Nothing -> False

instance Parseable Element where
  parser = choice [ try (Element <$> (parser :: StatefulParser Separator))
                  , try (Element <$> (parser :: StatefulParser Headline))
                  , try (Element <$> (parser :: StatefulParser Pragma))
                  , try (Element <$> (parser :: StatefulParser Timestamp))
                  , Element <$> (parser :: StatefulParser Token) ]

instance TextShow Element where
  showb (Element a) = TextShow.showb a
