{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Element (Element (..)) where

import Data.Typeable
import Data.Org.Base qualified as Org
import Data.Org.Headline
import Data.Org.Token
import Data.Org.Pragma
import Data.Org.Timestamp
import Data.Org.Separator
import Text.Megaparsec (try, choice)

import TextShow (TextShow)
import TextShow qualified as TS

data Element where
  Element :: Org.Parse a => a -> Element

instance Show Element where
  show (Element a) = show a

instance Eq Element where
    (Element x) == (Element y) = case cast y of
        Just y' -> x == y'
        Nothing -> False

instance Org.Parse Element where
  parser = choice [ try (Element <$> (Org.parser :: Org.StatefulParser Sep))
                  , try (Element <$> (Org.parser :: Org.StatefulParser Headline))
                  , try (Element <$> (Org.parser :: Org.StatefulParser Pragma))
                  , try (Element <$> (Org.parser :: Org.StatefulParser Ts))
                  , Element <$> (Org.parser :: Org.StatefulParser Tk) ]

instance TextShow Element where
  showb (Element a) = TS.showb a
