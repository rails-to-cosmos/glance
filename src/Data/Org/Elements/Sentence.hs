module Data.Org.Elements.Sentence (Sentence(..), Element(..)) where

import Control.Monad (void)

import Data.Org.Parser
import Data.Org.Elements.Separator
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Token

import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable

import TextShow (TextShow)
import TextShow qualified

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

data Element where
  Element :: (Show a, TextShow a, Typeable a, Eq a, Parse a) => a -> Element

instance Show Element where
  show (Element a) = show a

instance TextShow Element where
  showb (Element a) = TextShow.showb a

instance Eq Element where
    (Element a) == (Element b) = case Typeable.cast b of
        Just b' -> a == b'
        Nothing -> False

instance Parse Element where
  parse = MP.choice [ MP.try (Element <$> (parse :: StatefulParser Separator))
                    , MP.try (Element <$> (parse :: StatefulParser Timestamp))
                    , Element <$> (parse :: StatefulParser Token) ]

newtype Sentence = Sentence [Element]
  deriving (Show, Eq)

instance Monoid Sentence where
  mempty = Sentence []

instance Semigroup Sentence where
  (<>) (Sentence a) (Sentence b) = Sentence (a <> b)

instance TextShow Sentence where
  showb (Sentence []) = ""
  showb (Sentence (x:xs)) = TextShow.showb x <> TextShow.showb (Sentence xs)

instance Parse Sentence where
  parse = do
    let stop = MP.choice [ void MPC.eol, MP.eof ]
    elems <- MP.manyTill (parse :: StatefulParser Element) (MP.lookAhead stop)
    return (Sentence elems)
