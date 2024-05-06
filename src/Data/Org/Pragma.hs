module Data.Org.Pragma (OrgPragma (..)) where

import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.PlainText
import Data.Text (Text, pack, unwords)
import qualified Data.Set as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

import Control.Monad
import qualified Control.Monad.State as State
import Prelude hiding (unwords, concat, replicate, concatMap)

data OrgPragma = OrgPragma !OrgKeyword !Text
               | OrgTodoPragma !(Set.Set Text) !(Set.Set Text)
               | OrgCategoryPragma !Text
  deriving (Show, Eq)

instance OrgElement OrgPragma where
  parser = do
    let keyword = parser :: OrgParser OrgKeyword
        todoList = some (todo <* space)
        doneList = option [] (char '|' *> space *> todoList)
        todoShort = pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          OrgKeyword result <- keyword <* skipMany todoShort
          return result

    key <- string "#+" *> keyword <* string ":" <* space
    case key of
      OrgKeyword "CATEGORY" -> do
        PlainText category <- parser :: OrgParser PlainText
        State.modify (\ctx -> ctx {metaCategory = category})
        return $ OrgCategoryPragma category
      OrgKeyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (\ctx -> ctx { metaTodoActive = metaTodoActive ctx <> pragmaActive
                                  , metaTodoInactive = metaTodoInactive ctx <> pragmaInactive })

        return $ OrgTodoPragma pragmaActive pragmaInactive
      _keyword -> do
        PlainText value <- parser :: OrgParser PlainText
        return $ OrgPragma key value

instance TextShow OrgPragma where
  showb (OrgPragma k v) = "#+" <> showb k <> ": " <> fromText v
  showb (OrgTodoPragma active inactive) = "#+TODO:" <> showbSpace <> fromText (unwords (Set.toList active)) <> " | " <> fromText (unwords (Set.toList inactive))
  showb (OrgCategoryPragma category) = "#+CATEGORY:" <> showbSpace <> fromText category
