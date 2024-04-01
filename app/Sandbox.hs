{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Sandbox () where

import Data.Org.Element

import Data.Void
import Data.Either
import Data.Text qualified as T

import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Char.Lexer (decimal)

import Control.Monad.State
import Control.Monad
import TextShow
import Data.Org
-- import Data.Org.Element
-- import Data.Org.Context
-- import Data.Org.Generic
-- import Data.Org.Pragma
-- import Data.Org.PlainText
-- import Data.Org.Property
-- import Data.Org.Headline
-- import Data.Org.PropertyBlock
-- import Data.Org.Elements

import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Lazy (toStrict)
import Data.Text (Text)

import Control.Monad.State qualified as S

import           Data.Text (Text, pack)
import           Data.Void (Void)
import           Data.Org.Context (OrgContext)
import           Text.Megaparsec (Parsec, MonadParsec(try), ParsecT)
import qualified Control.Monad.State as State
import Data.Time
import Data.Time.Format
import Text.Megaparsec (Parsec, MonadParsec(try), ParsecT, runParserT, getParserState)

type OrgModeParser = ParsecT Void Text (State.StateT OrgContext Parser)

-- p :: OrgModeParser Text
-- p  = do
--   text <- pack <$> manyTill anySingle eof
--   modify (\ctx -> ctx { metaCategory = "Hello" })
--   return text

-- pt what = parseTest (runStateT (runParserT p "" (pack what)) ctx) (pack what)

-- parseCategory :: Text -> Either (ParseErrorBundle Text Void) (OrgContext, OrgCategory)
-- parseCategory = runParser (runStateT apply defaultContext) ""

-- ctx = mempty :: OrgContext

-- showcase :: String -> IO ()
-- showcase what = do
--   let ctx = mempty :: OrgContext
--       parser = runStateT apply ctx :: Parser (OrgGenericElement, OrgContext)
--       input = T.pack what

--   parseTest (manyTill parser eof) input

  -- case  of
  --   Left err -> putStrLn $ errorBundlePretty err
  --   Right x -> print x

-- testParse :: IO OrgElement
-- testParse = do
--   let elems_context = parse (runStateT elementsParser defaultContext) "" (Text.pack ":TODO: A B C | D E F")

  -- case elem_context of
  --   Right (elem, context) -> return elem
  --   Left _ -> return (EPlainText "")

-- testTimestampParsing :: IO ()
-- testTimestampParsing = do
--   let
--     tss = parse timestampParser "" (Text.pack "[2023-07-24 20:10:01 +1w] hey ololo :a:b:c: hey :this:is:tag:")
--     ts = rights [tss] !! 0
--   printT ts

-- testHeadlineParsing :: IO OrgElement
-- testHeadlineParsing = do
--   let
--     hls = parse (headlineParser defaultContext) "" (Text.pack "* TODO Hey [2023-07-24 20:10:01 +1w] hey ololo :a:b:c: hey :this:is:tag:")
--   return (EHeadline (rights [hls] !! 0))

-- testElementsParsing :: IO [OrgElement]
-- testElementsParsing = do
--   let
--     hls = parse (elementsParser defaultContext) "" (Text.pack "TODO Hey [2023-07-24 20:10:01 +1w] hey ololo :a:b:c: hey :this:is:tag:")
--     hl = rights [hls] !! 0
--   return hl
