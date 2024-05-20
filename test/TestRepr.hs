module TestRepr (orgElementReprUnitTests) where

import Data.Org
import Data.Org qualified as Org

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TextShow qualified as TS
import TestDefaults

data TestCase = TestCase { description :: !String
                         , element :: !Org.Element
                         , representation :: !Text }

testCases :: [TestCase]
testCases = [ TestCase { description = "Org-mode headline representation"
                       , element = Org.Element defaultHeadline { indent = Indent 1
                                                               , todo = mempty :: Todo
                                                               , title = Title [ TText (Tk "Hello,")
                                                                               , TSep SPC
                                                                               , TText (Tk "world!")
                                                                               , TSep SPC
                                                                               , TTags (Tags ["greetings"])]}
                       , representation = "* Hello, world! :greetings:" }

            , TestCase { description = "Todo state representation"
                       , element = Org.Element defaultHeadline { indent = Indent 1
                                                               , todo = Todo (Just "TODO")
                                                               , title = Title [ TText (Tk "foo") ]}
                       , representation = "* TODO foo" } ]

assertOne :: TestCase -> TestTree
assertOne tc = testCase (description tc) $ assertEqual [] (TS.fromText (representation tc)) (TS.showb (element tc))

assertAll :: [TestTree]
assertAll = map assertOne testCases

orgElementReprUnitTests :: TestTree
orgElementReprUnitTests = testGroup "Org-mode elements representations" assertAll
