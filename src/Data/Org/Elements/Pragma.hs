module Data.Org.Elements.Pragma (Pragma (..)) where

import Control.Monad
import Control.Monad.State qualified as State

import Data.Org.Identity (Identity)
import Data.Org.Identity qualified as Identity

import Data.Org.Context
import Data.Org.Parser
import Data.Org.Elements.Keyword
import Data.Org.Elements.Token
import Data.Org.Elements.Sentence

import Data.List qualified as List

import Data.Text (Text)
import Data.Text qualified as Text

import Data.Set qualified as Set

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import TextShow (TextShow)
import TextShow qualified

import Prelude hiding (unwords, concat, replicate, concatMap)

data Pragma = Pragma !Keyword !Text
            | PTodo !(Set.Set Text) !(Set.Set Text)
            | PCategory !Sentence
  deriving (Show, Eq)

instance Identity Pragma where
  identity (Pragma keyword text) = Text.intercalate "-" [TextShow.showt keyword, text]
  identity (PTodo active inactive) = Text.intercalate "-" (List.sort (Set.toList (active <> inactive)))
  identity (PCategory category) = TextShow.showt category

instance Parse Pragma where
  parse = do
    let keyword = parse :: StatefulParser Keyword
        todoList = MP.some (todo <* MPC.space)
        doneList = MP.option [] (MPC.char '|' *> MPC.space *> todoList)
        todoShort = Text.pack <$> MP.between (MPC.char '(') (MPC.char ')') (MP.many (MP.noneOf ['(', ')', '\n']))
        todo = do
          Keyword result <- keyword <* MP.skipMany todoShort
          return result

    key <- MPC.string "#+" *> keyword <* MPC.string ":" <* MPC.space
    case key of
      Keyword "CATEGORY" -> do
        category <- parse :: StatefulParser Sentence
        State.modify $ setCategory (TextShow.showt category)
        return $ PCategory category
      Keyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (setTodo pragmaActive pragmaInactive)

        return $ PTodo pragmaActive pragmaInactive
      _keyword -> do
        Token value <- parse :: StatefulParser Token
        return $ Pragma key value

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TextShow.showb k <> ": " <> TextShow.fromText v
  showb (PTodo active inactive) = "#+TODO:" <> TextShow.showbSpace <> TextShow.fromText (Text.unwords (Set.toList active)) <> " | " <> TextShow.fromText (Text.unwords (Set.toList inactive))
  showb (PCategory category) = "#+CATEGORY:" <> TextShow.showbSpace <> TextShow.showb category
