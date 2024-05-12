{-# LANGUAGE LambdaCase #-}

module Data.Org.Generic (GElement (..)) where

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

data GElement = GHeadline   !Headline
              | GPragma     !Pragma
              | GProperties !Properties
              | GTags       !Tags
              | GTs  !Ts
              | GText       !Tk
              | GSep  !Sep
              deriving (Show, Eq)

instance TextShow GElement where
  showb = \case
    GTags       t -> showb t
    GTs  t -> showb t
    GText       t -> showb t
    GPragma     t -> showb t
    GProperties t -> showb t
    GHeadline   t -> showb t
    GSep  t -> showb t

instance OrgElement GElement where
  parser = choice [ GSep <$> try parser
                  , GHeadline  <$> try parser
                  , GPragma    <$> try parser
                  , GTs <$> try parser
                  , GText      <$>     parser ]
