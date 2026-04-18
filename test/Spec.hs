module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified TestLexer as Lexer
import qualified TestParser as Parser
-- import           TestRepresentation

tests :: TestTree
tests = testGroup "Tests"
  [ Lexer.spec
  -- , Parser.spec
  -- , orgModeParserUnitTests
  -- , orgElementReprUnitTests
  ]

main :: IO ()
main = defaultMain tests
