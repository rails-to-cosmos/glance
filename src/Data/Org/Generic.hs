{-# LANGUAGE LambdaCase #-}

module Data.Org.Generic (GElem (..)) where

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

data GElem = GHeadline   !Headline
           | GPragma     !Pragma
           | GProperties !Properties
           | GTags       !Tags
           | GTs         !Ts
           | GTk         !Tk
           | GSep        !Sep
           deriving (Show, Eq)

instance TextShow GElem where
  showb (GTags       t) = showb t
  showb (GTs         t) = showb t
  showb (GTk         t) = showb t
  showb (GPragma     t) = showb t
  showb (GProperties t) = showb t
  showb (GHeadline   t) = showb t
  showb (GSep        t) = showb t

instance OrgElement GElem where
  parser = choice [ GSep      <$> try parser
                  , GHeadline <$> try parser
                  , GPragma   <$> try parser
                  , GTs       <$> try parser
                  , GTk       <$>     parser ]
