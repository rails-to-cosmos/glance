module Data.Org.Pragma (Pragma (..)) where

import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.Lexeme
import Data.Text (Text, pack, unwords)
import Data.Set qualified as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

import Control.Monad
import Control.Monad.State qualified as State
import Prelude hiding (unwords, concat, replicate, concatMap)

data Pragma = Pragma !Keyword !Text
            | TodoPragma !(Set.Set Text) !(Set.Set Text)
            | OrgCategoryPragma !Text
  deriving (Show, Eq)

instance OrgElement Pragma where
  parser = do
    let keyword = parser :: OrgParser Keyword
        todoList = some (todo <* space)
        doneList = option [] (char '|' *> space *> todoList)
        todoShort = pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          Keyword result <- keyword <* skipMany todoShort
          return result

    key <- string "#+" *> keyword <* string ":" <* space
    case key of
      Keyword "CATEGORY" -> do
        Lexeme category <- parser :: OrgParser Lexeme
        State.modify (\ctx -> ctx {metaCategory = category})
        return $ OrgCategoryPragma category
      Keyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (\ctx -> ctx { metaTodoActive = metaTodoActive ctx <> pragmaActive
                                  , metaTodoInactive = metaTodoInactive ctx <> pragmaInactive })

        return $ TodoPragma pragmaActive pragmaInactive
      _keyword -> do
        Lexeme value <- parser :: OrgParser Lexeme
        return $ Pragma key value

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> showb k <> ": " <> fromText v
  showb (TodoPragma active inactive) = "#+TODO:" <> showbSpace <> fromText (unwords (Set.toList active)) <> " | " <> fromText (unwords (Set.toList inactive))
  showb (OrgCategoryPragma category) = "#+CATEGORY:" <> showbSpace <> fromText category
