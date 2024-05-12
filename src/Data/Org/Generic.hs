{-# LANGUAGE LambdaCase #-}

module Data.Org.Generic (GElement (..)) where

import Data.Org.Element
import Data.Org.Headline
import Data.Org.Lexeme
import Data.Org.Pragma
import Data.Org.Properties
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Separator
import Text.Megaparsec
import TextShow (TextShow, showb)

data GElement = GHeadline      !Headline
                       | GPragma        !Pragma
                       | GProperties !Properties
                       | GTags          !Tags
                       | GTimestamp     !Timestamp
                       | GText          !Lexeme
                       | GSeparator     !Separator
                       deriving (Show, Eq)

instance TextShow GElement where
  showb = \case
    GTags          t -> showb t
    GTimestamp     t -> showb t
    GText          t -> showb t
    GPragma        t -> showb t
    GProperties t -> showb t
    GHeadline      t -> showb t
    GSeparator     t -> showb t

instance OrgElement GElement where
  parser = choice [ GSeparator <$> try parser
                  , GHeadline  <$> try parser
                  , GPragma    <$> try parser
                  , GTimestamp <$> try parser
                  , GText      <$>     parser
                  ]
