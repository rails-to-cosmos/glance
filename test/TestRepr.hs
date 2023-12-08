{-# LANGUAGE OverloadedStrings #-}

module TestRepr (orgElementReprUnitTests) where

import           Data.Org
import           Data.Text (Text)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)
import           TextShow (TextShow(showb), fromText)
import           TestDefaults

data TestCase = TestCase { description :: String
                         , element :: OrgGenericElement
                         , representation :: Text
                         }

testCases :: [TestCase]
testCases = [ TestCase { description = "Headline string representation"
                       , element = OrgGenericHeadline
                           defaultHeadline { indent = OrgIndent 1
                                           , todo = mempty :: OrgTodo
                                           , title = OrgTitle "Hello, world!"
                                           , tags = OrgTags ["greetings"]
                                           }
                       , representation = "* Hello, world! :greetings:"
                       }]

assertOne :: TestCase -> TestTree
assertOne tc = testCase descr $ assertEqual [] (showb el) (fromText repr)
  where
    descr = description tc
    repr = representation tc
    el = element tc

assertAll :: [TestTree]
assertAll = map assertOne testCases

orgElementReprUnitTests :: TestTree
orgElementReprUnitTests = testGroup "Org-mode element representation" assertAll
