module TestRepresentation (orgElementReprUnitTests) where

import Data.Org
import qualified Data.Org as Org
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestDefaults
import qualified TextShow as TS

data TestCase = TestCase { description :: !String
                         , element :: !Org.Element
                         , representation :: !Text }

testCases :: [TestCase]
testCases = [ TestCase { description = "Headline"
                       , element = Org.Element defaultHeadline { indent = Indent 1
                                                               , todo = Nothing
                                                               , title = Title [ OrgLineToken "Hello,"
                                                                               , OrgLineSeparator SPC
                                                                               , OrgLineToken "world!"
                                                                               , OrgLineSeparator SPC
                                                                               ]
                                                               , tags = Tags ["greetings"]
                                                               }
                       , representation = "* Hello, world! :greetings:" }

            , TestCase { description = "TODO"
                       , element = Org.Element defaultHeadline { indent = Indent 1
                                                               , todo = Just (Todo {name = "TODO", active = True})
                                                               , title = Title [ OrgLineToken "foo" ]}
                       , representation = "* TODO foo" } ]

assertOne :: TestCase -> TestTree
assertOne tc = testCase (description tc) $ assertEqual [] (TS.fromText (representation tc)) (TS.showb (element tc))

assertAll :: [TestTree]
assertAll = map assertOne testCases

orgElementReprUnitTests :: TestTree
orgElementReprUnitTests = testGroup "Representation" assertAll
