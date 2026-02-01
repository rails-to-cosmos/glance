module Main (main) where

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           TestParser
import           TestRepresentation

tests :: TestTree
tests = testGroup "Tests" [orgModeParserUnitTests, orgElementReprUnitTests]

main :: IO ()
main = defaultMain tests
