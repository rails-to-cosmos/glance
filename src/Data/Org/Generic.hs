{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Org.Generic (OrgElement (..)) where

import Data.Typeable
import Data.Org.Element
import Data.Org.Headline
import Data.Org.Token
import Data.Org.Pragma
import Data.Org.Properties
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Separator
import Text.Megaparsec (try, choice)
import TextShow (TextShow, showb)

data OrgElement where
  OrgElement :: Org a => a -> OrgElement

instance Show OrgElement where
  show (OrgElement a) = show a

instance Eq OrgElement where
    (OrgElement x) == (OrgElement y) = case cast y of
        Just y' -> x == y'
        Nothing -> False

instance Org OrgElement where
  parser = choice [ try (OrgElement <$> (parser :: OrgParser Sep))
                  , try (OrgElement <$> (parser :: OrgParser Headline))
                  , try (OrgElement <$> (parser :: OrgParser Pragma))
                  , try (OrgElement <$> (parser :: OrgParser Ts))
                  , OrgElement <$> (parser :: OrgParser Tk) ]

instance TextShow OrgElement where
  showb (OrgElement a) = showb a
