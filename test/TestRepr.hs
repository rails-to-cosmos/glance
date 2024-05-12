module TestRepr (orgElementReprUnitTests) where

import           Data.Org
import           Data.Text (Text)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)
import           TextShow (TextShow(showb), fromText)
import           TestDefaults

data TestCase = TestCase { description :: !String
                         , element :: !GElement
                         , representation :: !Text }

testCases :: [TestCase]
testCases = [ TestCase { description = "Org-mode headline representation"
                       , element = GHeadline defaultHeadline { indent = Indent 1
                                                                      , todo = mempty :: Todo
                                                                      , title = Title [ TText (Token "Hello,")
                                                                                      , TSep SPC
                                                                                      , TText (Token "world!")
                                                                                      , TSep SPC
                                                                                      , TitleTags (Tags ["greetings"])]}
                       , representation = "* Hello, world! :greetings:" }]

assertOne :: TestCase -> TestTree
assertOne tc = testCase (description tc) $ assertEqual [] (fromText (representation tc)) (showb (element tc))

assertAll :: [TestTree]
assertAll = map assertOne testCases

orgElementReprUnitTests :: TestTree
orgElementReprUnitTests =
  testGroup "Org-mode elements representations" assertAll
