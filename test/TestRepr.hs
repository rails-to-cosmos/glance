module TestRepr (orgElementReprUnitTests) where

import Data.Org
import Data.Org qualified as Org
import Data.Org.Elements.Title qualified as Org.Title
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestDefaults
import TextShow qualified as TS

data TestCase = TestCase { description :: !String
                         , element :: !Org.Element
                         , representation :: !Text }

testCases :: [TestCase]
testCases = [ TestCase { description = "Org-mode headline representation"
                       , element = Org.Element defaultHeadline { indent = Indent 1
                                                               , todo = Nothing
                                                               , title = Title [ Org.Title.Element (Token "Hello,")
                                                                               , Org.Title.Element SPC
                                                                               , Org.Title.Element (Token "world!")
                                                                               , Org.Title.Element SPC
                                                                               , Org.Title.Element (Tags ["greetings"])]}
                       , representation = "* Hello, world! :greetings:" }

            , TestCase { description = "Todo state representation"
                       , element = Org.Element defaultHeadline { indent = Indent 1
                                                               , todo = Just (Todo {name = "TODO", active = True})
                                                               , title = Title [ Org.Title.Element (Token "foo") ]}
                       , representation = "* TODO foo" } ]

assertOne :: TestCase -> TestTree
assertOne tc = testCase (description tc) $ assertEqual [] (TS.fromText (representation tc)) (TS.showb (element tc))

assertAll :: [TestTree]
assertAll = map assertOne testCases

orgElementReprUnitTests :: TestTree
orgElementReprUnitTests = testGroup "Org-mode elements representations" assertAll
