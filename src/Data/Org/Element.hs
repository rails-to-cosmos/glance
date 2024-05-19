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
import TextShow (TextShow, showb)

data OrgElement where
  OrgElement :: Org.Base a => a -> OrgElement

instance Show OrgElement where
  show (OrgElement a) = show a

instance Eq OrgElement where
    (OrgElement x) == (OrgElement y) = case cast y of
        Just y' -> x == y'
        Nothing -> False

instance Org.Base OrgElement where
  parser = choice [ try (OrgElement <$> (Org.parser :: Org.OrgParser Sep))
                  , try (OrgElement <$> (Org.parser :: Org.OrgParser Headline))
                  , try (OrgElement <$> (Org.parser :: Org.OrgParser Pragma))
                  , try (OrgElement <$> (Org.parser :: Org.OrgParser Ts))
                  , OrgElement <$> (Org.parser :: Org.OrgParser Tk) ]

instance TextShow OrgElement where
  showb (OrgElement a) = showb a
