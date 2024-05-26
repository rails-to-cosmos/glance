module Data.Org.Elements.Pragma (Pragma (..)) where

import Control.Monad
import Control.Monad.State qualified as State

import Data.Org.State
import Data.Org.Parse
import Data.Org.Elements.Keyword
import Data.Org.Elements.Token
import Data.Org.Elements.Sentence
import Data.Text (Text, pack, unwords)
import Data.Set qualified as Set

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

import Prelude hiding (unwords, concat, replicate, concatMap)

data Pragma = Pragma !Keyword !Text
            | PTodo !(Set.Set Text) !(Set.Set Text)
            | PCategory !Sentence
  deriving (Show, Eq)

instance Parse Pragma where
  parser = do
    let keyword = parser :: StatefulParser Keyword
        todoList = some (todo <* space)
        doneList = option [] (char '|' *> space *> todoList)
        todoShort = pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          Keyword result <- keyword <* skipMany todoShort
          return result

    key <- string "#+" *> keyword <* string ":" <* space
    case key of
      Keyword "CATEGORY" -> do
        category <- parser :: StatefulParser Sentence
        State.modify (categoryUpdate (TS.showt category))
        return $ PCategory category
      Keyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (todoUpdate pragmaActive pragmaInactive)

        return $ PTodo pragmaActive pragmaInactive
      _keyword -> do
        Token value <- parser :: StatefulParser Token
        return $ Pragma key value

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TS.showb k <> ": " <> TS.fromText v
  showb (PTodo active inactive) = "#+TODO:" <> TS.showbSpace <> TS.fromText (unwords (Set.toList active)) <> " | " <> TS.fromText (unwords (Set.toList inactive))
  showb (PCategory category) = "#+CATEGORY:" <> TS.showbSpace <> TS.showb category
