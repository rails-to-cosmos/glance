module Data.Org.Elements.Pragma (Pragma (..)) where

import Control.Monad
import Control.Monad.State qualified as State

import Data.Org.Identity (Identity)
import Data.Org.Identity qualified as Identity

import Data.Org.State
import Data.Org.Parse
import Data.Org.Elements.Keyword
import Data.Org.Elements.Token
import Data.Org.Elements.Sentence

import Data.List qualified as List

import Data.Text (Text)
import Data.Text qualified as Text

import Data.Set qualified as Set

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified

import Prelude hiding (unwords, concat, replicate, concatMap)

data Pragma = Pragma !Keyword !Text
            | PTodo !(Set.Set Text) !(Set.Set Text)
            | PCategory !Sentence
  deriving (Show, Eq)

instance Identity Pragma where
  id (Pragma keyword text) = Text.intercalate "-" [TextShow.showt keyword, text]
  id (PTodo active inactive) = Text.intercalate "-" (List.sort (Set.toList (active <> inactive)))
  id (PCategory category) = TextShow.showt category

instance Parse Pragma where
  parser = do
    let keyword = parser :: StatefulParser Keyword
        todoList = some (todo <* space)
        doneList = option [] (char '|' *> space *> todoList)
        todoShort = Text.pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          Keyword result <- keyword <* skipMany todoShort
          return result

    key <- string "#+" *> keyword <* string ":" <* space
    case key of
      Keyword "CATEGORY" -> do
        category <- parser :: StatefulParser Sentence
        State.modify $ setCategory (TextShow.showt category)
        return $ PCategory category
      Keyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (setTodo pragmaActive pragmaInactive)

        return $ PTodo pragmaActive pragmaInactive
      _keyword -> do
        Token value <- parser :: StatefulParser Token
        return $ Pragma key value

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TextShow.showb k <> ": " <> TextShow.fromText v
  showb (PTodo active inactive) = "#+TODO:" <> TextShow.showbSpace <> TextShow.fromText (Text.unwords (Set.toList active)) <> " | " <> TextShow.fromText (Text.unwords (Set.toList inactive))
  showb (PCategory category) = "#+CATEGORY:" <> TextShow.showbSpace <> TextShow.showb category
