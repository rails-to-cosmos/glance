module Data.Org.Pragma (Pragma (..)) where

import Data.Org.Base qualified as Org
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.Token
import Data.Org.Sentence
import Data.Text (Text, pack, unwords)
import Data.Set qualified as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

import Control.Monad
import Control.Monad.State qualified as State
import Prelude hiding (unwords, concat, replicate, concatMap)

data Pragma = Pragma !Keyword !Text
            | PTodo !(Set.Set Text) !(Set.Set Text)
            | PCategory !Sentence
  deriving (Show, Eq)

instance Org.Base Pragma where
  parser = do
    let keyword = Org.parser :: Org.OrgParser Keyword
        todoList = some (todo <* space)
        doneList = option [] (char '|' *> space *> todoList)
        todoShort = pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          Keyword result <- keyword <* skipMany todoShort
          return result

    key <- string "#+" *> keyword <* string ":" <* space
    case key of
      Keyword "CATEGORY" -> do
        category <- Org.parser :: Org.OrgParser Sentence
        State.modify (\ctx -> ctx {metaCategory = showt category})
        return $ PCategory category
      Keyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (\ctx -> ctx { metaTodoActive = metaTodoActive ctx <> pragmaActive
                                  , metaTodoInactive = metaTodoInactive ctx <> pragmaInactive })

        return $ PTodo pragmaActive pragmaInactive
      _keyword -> do
        Tk value <- Org.parser :: Org.OrgParser Tk
        return $ Pragma key value

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> showb k <> ": " <> fromText v
  showb (PTodo active inactive) = "#+TODO:" <> showbSpace <> fromText (unwords (Set.toList active)) <> " | " <> fromText (unwords (Set.toList inactive))
  showb (PCategory category) = "#+CATEGORY:" <> showbSpace <> showb category
