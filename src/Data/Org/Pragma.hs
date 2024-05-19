module Data.Org.Pragma (Pragma (..)) where

import Control.Monad
import Control.Monad.State qualified as State

import Data.Org.Base qualified as Org
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.Token
import Data.Org.Sentence
import Data.Text (Text, pack, unwords)
import Data.Set qualified as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

import Prelude hiding (unwords, concat, replicate, concatMap)

data Pragma = Pragma !Keyword !Text
            | PTodo !(Set.Set Text) !(Set.Set Text)
            | PCategory !Sentence
  deriving (Show, Eq)

instance Org.Base Pragma where
  parser = do
    let keyword = Org.parser :: Org.StatefulParser Keyword
        todoList = some (todo <* space)
        doneList = option [] (char '|' *> space *> todoList)
        todoShort = pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          Keyword result <- keyword <* skipMany todoShort
          return result

    key <- string "#+" *> keyword <* string ":" <* space
    case key of
      Keyword "CATEGORY" -> do
        category <- Org.parser :: Org.StatefulParser Sentence
        State.modify (\ctx -> ctx {metaCategory = TS.showt category})
        return $ PCategory category
      Keyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (\ctx -> ctx { metaTodoActive = metaTodoActive ctx <> pragmaActive
                                  , metaTodoInactive = metaTodoInactive ctx <> pragmaInactive })

        return $ PTodo pragmaActive pragmaInactive
      _keyword -> do
        Tk value <- Org.parser :: Org.StatefulParser Tk
        return $ Pragma key value

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TS.showb k <> ": " <> TS.fromText v
  showb (PTodo active inactive) = "#+TODO:" <> TS.showbSpace <> TS.fromText (unwords (Set.toList active)) <> " | " <> TS.fromText (unwords (Set.toList inactive))
  showb (PCategory category) = "#+CATEGORY:" <> TS.showbSpace <> TS.showb category
