{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Element (OrgElement (..)) where

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

data OrgElement where
  OrgElement :: Org.Base a => a -> OrgElement

instance Show OrgElement where
  show (OrgElement a) = show a

instance Eq OrgElement where
    (OrgElement x) == (OrgElement y) = case cast y of
        Just y' -> x == y'
        Nothing -> False

instance Org.Base OrgElement where
  parser = choice [ try (OrgElement <$> (Org.parser :: Org.StatefulParser Sep))
                  , try (OrgElement <$> (Org.parser :: Org.StatefulParser Headline))
                  , try (OrgElement <$> (Org.parser :: Org.StatefulParser Pragma))
                  , try (OrgElement <$> (Org.parser :: Org.StatefulParser Ts))
                  , OrgElement <$> (Org.parser :: Org.StatefulParser Tk) ]

instance TextShow OrgElement where
  showb (OrgElement a) = TS.showb a
