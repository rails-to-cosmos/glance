{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Element (Element (..)) where

import Data.Typeable
import Data.Org.Parse
import Data.Org.Headline
import Data.Org.Token
import Data.Org.Pragma
import Data.Org.Timestamp
import Data.Org.Separator
import Text.Megaparsec (try, choice)

import TextShow (TextShow)
import TextShow qualified as TS

data Element where
  Element :: Parse a => a -> Element

instance Show Element where
  show (Element a) = show a

instance Eq Element where
    (Element x) == (Element y) = case cast y of
        Just y' -> x == y'
        Nothing -> False

instance Parse Element where
  parser = choice [ try (Element <$> (parser :: StatefulParser Sep))
                  , try (Element <$> (parser :: StatefulParser Headline))
                  , try (Element <$> (parser :: StatefulParser Pragma))
                  , try (Element <$> (parser :: StatefulParser Ts))
                  , Element <$> (parser :: StatefulParser Token) ]

instance TextShow Element where
  showb (Element a) = TS.showb a
