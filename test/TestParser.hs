module TestParser (spec) where

import Data.Org hiding (defaultHeadline)
import qualified Data.Org as Org

import Data.Text (Text, intercalate, unpack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestDefaults
import qualified Data.Set as Set
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)

strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack t) :: UTCTime

data Result = Result { elements :: ![Org.Element]
                     , context :: !Org.Context
                     } deriving (Eq, Show)

data TestCase = TestCase { description :: !String
                         , inputs :: ![Text]
                         , expected :: !Result }

testCases :: [TestCase]
testCases = [ TestCase { description = "Headline"
                       , inputs = ["** TODO [#A] Hello :a:b:c:"]
                       , expected = Result { elements = [ Org.EHeadline defaultHeadline { indent = Indent 2
                                                                                      , todo = Just (Todo { name = "TODO", active = True})
                                                                                      , priority = Just (Priority 'A')
                                                                                      , title = Title [OrgLineToken "Hello"]
                                                                                      , tags = Tags ["a", "b", "c"]
                                                                                      }]
                                           , context = initialState }}


            , TestCase { description = "Corrupted tags"
                       , inputs = ["* Hello world :a:b:c"]
                       , expected = Result { elements = [ Org.EHeadline defaultHeadline { title = Title [ OrgLineToken "Hello"
                                                                                                      , OrgLineToken "world"
                                                                                                      , OrgLineToken ":a:b:c" ]}]
                                           , context = initialState }}

            , TestCase { description = "Property block"
                       , inputs = [ "* Hello"
                                  , ":PROPERTIES:"
                                  , ":TITLE: New title"
                                  , ":END:" ]
                       , expected = Result { elements = [ Org.EHeadline defaultHeadline { title = Title [OrgLineToken "Hello"]
                                                                                      , properties = Properties [Property (
                                                                                                                    Keyword "TITLE") (
                                                                                                                    OrgLine [ OrgLineToken "New"
                                                                                                                            , OrgLineToken "title"
                                                                                                                            ])]}]
                                           , context = initialState }}

            , TestCase { description = "Drawer"
                       , inputs = [":DRAWER:"]
                       , expected = Result { elements = [Org.EToken (Token ":DRAWER:")]
                                           , context = initialState }}

            , TestCase { description = "Category pragma"
                       , inputs = ["#+CATEGORY: foo bar"]
                       , expected = Result { elements = [Org.EPragma (Pragma (Keyword "CATEGORY") (OrgLine [ OrgLineToken "foo"
                                                                                                          , OrgLineToken "bar" ]))]
                                           , context = initialState `withCategory` "foo bar" }}

            , TestCase { description = "Category property"
                       , inputs = [ "* Hello"
                                  , ":PROPERTIES:"
                                  , ":CATEGORY: Updated category"
                                  , ":END:" ]
                       , expected = Result { elements = [ Org.EHeadline defaultHeadline { title = Title [OrgLineToken "Hello"]
                                                                                      , properties = Properties [Property (Keyword "CATEGORY") (OrgLine [ OrgLineToken "Updated"
                                                                                                                                                        , OrgLineToken "category"])]}]
                                           , context = initialState `withCategory` "Updated category"}}

            , TestCase { description = "TODO pragma"
                       , inputs = [ "#+TODO: TODO | CANCELLED"
                                  , "* CANCELLED Mess" ]
                       , expected = Result { elements = [ Org.EPragma (PTodo (Set.fromList ["TODO"]) (Set.fromList ["CANCELLED"]))
                                                        , Org.EHeadline (defaultHeadline { todo = Just (Todo { name = "CANCELLED", active = False })
                                                                                       , title = Title [OrgLineToken "Mess"] })]
                                           , context = initialState `withTodo` (["TODO"], ["DONE", "CANCELLED"]) }}

            , TestCase { description = "TODO pragma (active only)"
                       , inputs = ["#+TODO: foo"]
                       , expected = Result { elements = [ Org.EPragma (PTodo (Set.fromList ["foo"]) (Set.fromList [])) ]
                                           , context = initialState `withTodo` (["TODO", "foo"], ["DONE"])}}

            , TestCase { description = "Multiline"
                       , inputs = [ "* foo"
                                  , "* bar" ]
                       , expected = Result { elements = [ Org.EHeadline (defaultHeadline {title = Title [OrgLineToken "foo"]})
                                                        , Org.EHeadline (defaultHeadline {title = Title [OrgLineToken "bar"]}) ]
                                           , context = initialState }}

            , TestCase { description = "Empty text"
                       , inputs = [""]
                       , expected = Result { elements = []
                                           , context = initialState }}

            , TestCase { description = "Timestamp"
                       , inputs = [ "<2024-01-01>"
                                  , "<2024-01-01 Mon>" ]
                       , expected = Result { elements = [ Org.ETimestamp day2024
                                                        , Org.ETimestamp day2024 ]
                                           , context = initialState }}

            , TestCase { description = "Timestamp range"
                       , inputs = ["[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"]
                       , expected = Result { elements = [ Org.ETimestamp Timestamp { tsStatus = TimestampInactive
                                                                                   , tsInterval = Nothing
                                                                                   , tsStart = TsMoment (strptime "2023-07-15 15:54:00") True
                                                                                   , tsEnd = Just (TsMoment (strptime "2023-07-15 17:10:00") True) }]
                                           , context = initialState }}
            ]

-- | 2024-01-01, date only.
day2024 :: Org.Timestamp
day2024 = Timestamp { tsStatus = TimestampActive
                    , tsInterval = Nothing
                    , tsStart = TsMoment (strptime "2024-01-01 00:00:00") False
                    , tsEnd = Nothing }

spec :: TestTree
spec = testGroup "Parser" assertMany
  where assert tc = testCase (description tc) $ assertEqual [] (expected tc) (result tc)
        result tc = case orgParse defaultContext (intercalate "\n" (inputs tc)) of
          (headlines, context, maybeError) -> Result (map (stripSpans . valueOf) headlines) context
        assertMany = map assert testCases
