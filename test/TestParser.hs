module TestParser (orgModeParserUnitTests) where

import Data.Org
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
testCases = [ TestCase { description = "Parse headline with tags"
                       , inputs = ["* Hello world :a:b:c:"]
                       , expected = Result { elements = [ Org.Element defaultHeadline { title = Title [ TitleElement (Token "Hello")
                                                                                                      , TitleElement SPC
                                                                                                      , TitleElement (Token "world")
                                                                                                      , TitleElement SPC
                                                                                                      , TitleElement (Tags ["a", "b", "c"]) ]}]
                                           , context = initialState }}

            , TestCase { description = "Parse headline with corrupted tag string"
                       , inputs = ["* Hello world :a:b:c"]
                       , expected = Result { elements = [ Org.Element defaultHeadline { title = Title [ TitleElement (Token "Hello")
                                                                                                      , TitleElement SPC
                                                                                                      , TitleElement (Token "world")
                                                                                                      , TitleElement SPC
                                                                                                      , TitleElement (Token ":a:b:c") ]}]
                                           , context = initialState }}

            , TestCase { description = "Parse property block"
                       , inputs = [ "* Hello"
                                  , ":PROPERTIES:"
                                  , ":TITLE: New title"
                                  , ":END:" ]
                       , expected = Result { elements = [ Org.Element defaultHeadline { title = Title [ TitleElement (Token "Hello") ]
                                                                                      , properties = Properties [Property (Keyword "TITLE") (Sentence [ ElToken "New"
                                                                                                                                                      , ElSeparator SPC
                                                                                                                                                      , ElToken "title" ])]}]
                                           , context = initialState }}

            , TestCase { description = "Parse drawer"
                       , inputs = [":DRAWER:"]
                       , expected = Result { elements = [Org.Element (Token ":DRAWER:")]
                                           , context = initialState }}

            , TestCase { description = "Category pragma affects context"
                       , inputs = ["#+CATEGORY: foo bar"]
                       , expected = Result { elements = [Org.Element (PCategory (Sentence [ ElToken "foo"
                                                                                          , ElSeparator SPC
                                                                                          , ElToken "bar" ]))]
                                           , context = initialState `withCategory` "foo bar" }}

            , TestCase { description = "Category property affects context"
                       , inputs = [ "* Hello"
                                  , ":PROPERTIES:"
                                  , ":CATEGORY: Updated category"
                                  , ":END:" ]
                       , expected = Result { elements = [ Org.Element defaultHeadline { title = Title [TitleElement (Token "Hello")]
                                                                                      , properties = Properties [Property (Keyword "CATEGORY") (Sentence [ ElToken "Updated"
                                                                                                                                                         , ElSeparator SPC
                                                                                                                                                         , ElToken "category"])]}]
                                           , context = initialState `withCategory` "Updated category"}}

            , TestCase { description = "Parse complete headline"
                       , inputs = ["** TODO [#A] Hello :a:b:c:"]
                       , expected = Result { elements = [ Org.Element defaultHeadline { indent = Indent 2
                                                                                      , todo = Just (Todo { name = "TODO", active = True})
                                                                                      , priority = Just (Priority 'A')
                                                                                      , title = Title [ TitleElement (Token "Hello")
                                                                                                      , TitleElement SPC
                                                                                                      , TitleElement (Tags ["a", "b", "c"])]}]
                                           , context = initialState }}

            , TestCase { description = "Parse headline with custom todo state"
                       , inputs = [ "#+TODO: TODO | CANCELLED"
                                  , "* CANCELLED Mess" ]
                       , expected = Result { elements = [ Org.Element (PTodo (Set.fromList ["TODO"]) (Set.fromList ["CANCELLED"]))
                                                        , Org.Element (defaultHeadline { todo = Just (Todo { name = "CANCELLED", active = False })
                                                                                       , title = Title [TitleElement (Token "Mess")] })]
                                           , context = initialState `withTodo` (["TODO"], ["DONE", "CANCELLED"]) }}

            , TestCase { description = "No inactive todo states"
                       , inputs = ["#+TODO: foo"]
                       , expected = Result { elements = [ Org.Element (PTodo (Set.fromList ["FOO"]) (Set.fromList [])) ]
                                           , context = initialState `withTodo` (["TODO", "FOO"], ["DONE"])}}

            -- , TestCase { description = "Messed active/inactive todo states"
            --            , inputs = [ "#+TODO: CANCELLED | CANCELLED"
            --                       , "* CANCELLED Mess" ]
            --            , expected = Result { elements = [ GPragma (PTodo (Set.fromList ["CANCELLED"]) (Set.fromList ["CANCELLED"]))
            --                                                    , Org.Element (defaultHeadline { todo = Todo Nothing
            --                                                                                 , title = Title [ TText (Token "CANCELLED")
            --                                                                                                 , TSeparator SPC
            --                                                                                                 , TText (Token "Mess")]})]
            --                                       , context = initialState { todoActive = Set.fromList ["TODO"]
            --                                                                  , todoInactive = Set.fromList ["DONE"] }}}

            , TestCase { description = "Multiline parsing"
                       , inputs = [ "* foo"
                                  , "* bar" ]
                       , expected = Result { elements = [ Org.Element (defaultHeadline {title = Title [TitleElement (Token "foo")]})
                                                        , Org.Element (defaultHeadline {title = Title [TitleElement (Token "bar")]}) ]
                                           , context = initialState }}

            , TestCase { description = "Empty text parsing"
                       , inputs = [""]
                       , expected = Result { elements = []
                                           , context = initialState }}

            , TestCase { description = "Restrict infinite parsing of eol / eof"
                       , inputs = ["", "", ""]
                       , expected = Result { elements = [ Org.Element EOL
                                                        , Org.Element EOL ]
                                           , context = initialState }}

            , TestCase { description = "Parse timestamps"
                       , inputs = [ "<2024-01-01>"
                                  , "<2024-01-01 Mon>" ]
                       , expected = Result { elements = [ Org.Element Timestamp {tsStatus = TimestampActive, tsInterval = Nothing, tsTime = strptime "2024-01-01 00:00:00"}
                                                        , Org.Element EOL
                                                        , Org.Element Timestamp {tsStatus = TimestampActive, tsInterval = Nothing, tsTime = strptime "2024-01-01 00:00:00"}]
                                           , context = initialState }}

            -- , TestCase { description = "Parse schedule property"
            --            , inputs = [ "* foo"
            --                       , "SCHEDULED: <2024-04-28 Sun>"
            --                       , ":PROPERTIES:"
            --                       , ":CATEGORY: bar"
            --                       , ":END:" ]
            --            , expected = Result { elements = [ Org.Element (defaultHeadline { title = Title [TText (Token "foo")]
            --                                                                           , schedule = Just Timestamp { tsStatus = TimestampActive
            --                                                                                                , tsInterval = Nothing
            --                                                                                                , tsTime = strptime "2024-04-28 00:00:00" }
            --                                                                           , properties = Properties [Property (Keyword "CATEGORY") (Sentence [(SToken (Token "bar"))])]})]
            --                                , context = initialState { metaCategory = "bar" }}}

            -- , TestCase { description = "Parse links"
            --            , inputs = ["[[file:/home/foo/bar.org::*NN Pipeline][NN Pipeline]]"]
            --            , expected = Result { elements = [Org.Element (Token "[[file:/home/foo/bar.org::*NN Pipeline][NN Pipeline]]")]
            --                                       , context = initialState}}

            ]

            -- , TestCase
            --     { description = "Parse custom todo state",
            --       inputs =
            --         [ "#+TODO: TODO | CANCELLED",
            --           "* CANCELLED Mess"
            --         ],
            --       expected =
            --         Org.Context
            --           { headline =
            --               Headline
            --                 { indent = EIndent 1
            --                 , todo = ETodo "CANCELLED"
            --                 , priority = EPriority Nothing
            --                 , title = "Mess"
            --                 , tags = []
            --                 , properties = defaultProperties
            --                 , meta = defaultMeta { todo = (["TODO"], ["DONE", "CANCELLED"])
            --                                      }
            --                 }
            --           }
            --     }
            --   TestCase
            --     { description = "Timestamp affects context",
            --       inputs =
            --         [ "[2021-08-22 Sun 10:18]"
            --         ],
            --       expected =
            --         (initialState)
            --           { metaTime =
            --               [ strptime "2021-08-22 10:18:00"
            --               ]
            --           }
            --     },
            --   TestCase
            --     { description = "Headline title affects context",
            --       inputs =
            --         [ "* Headline with a ts in it [2021-08-22 Sun 11:37]"
            --         ],
            --       expected =
            --         (initialState)
            --           { metaTime =
            --               [ strptime "2021-08-22 11:37:00"
            --               ]
            --           }
            --     }
            -- , TestCase
            --     { description = "Parse minimal headline",
            --       inputs = ["* Hello"],
            --       expected =
            --         initialState
            --           { headline =
            --               Headline
            --                 { indent = EIndent 1,
            --                   todo = ETodo "",
            --                   priority = EPriority Nothing,
            --                   title = "Hello",
            --                   tags = [],
            --                   properties = defaultProperties,
            --                   meta = defaultMeta
            --                 }
            --           }
            --     }
            -- , TestCase
            --     { description = "Parse corrupted headline",
            --       inputs = ["* TOO [#AD] Hey :a:b:c"],
            --       expected =
            --         initialState
            --           { headline =
            --               Headline
            --                 { indent = EIndent 1,
            --                   todo = ETodo "",
            --                   priority = EPriority Nothing,
            --                   title = "TOO [#AD] Hey :a:b:c",
            --                   tags = [],
            --                   properties = defaultProperties,
            --                   meta = defaultMeta
            --                 }
            --           }
            --     }
            -- , TestCase
            --     { description = "Multiple todo headers",
            --       inputs =
            --         [ "#+TODO: TODO | DONE",
            --           "#+TODO: PENDING | CANCELLED",
            --           "#+TODO: STARTED(s!) | CANCELLED(c!)"
            --         ],
            --       expected =
            --         Org.Context
            --           { headline =
            --               defaultHeadline
            --                 { meta = defaultMeta {todo = (["TODO", "PENDING", "STARTED"], ["DONE", "CANCELLED"])}
            --                 }
            --           }
            --     }
            -- , TestCase
            --     { description = "Parse missing todo state",
            --       inputs =
            --         [ "* CANCELLED Mess"
            --         ],
            --       expected =
            --         Org.Context
            --           { headline =
            --               Headline
            --                 { indent = EIndent 1
            --                 , todo = ETodo ""
            --                 , priority = EPriority Nothing
            --                 , title = "CANCELLED Mess"
            --                 , tags = []
            --                 , properties = defaultProperties
            --                 , meta = defaultMeta
            --                 }
            --           }
            --     }
            -- , TestCase
            --     { description = "Parse category pragma",
            --       inputs =
            --         [ "#+CATEGORY: New category"
            --         ],
            --       expected =
            --         initialState
            --           { headline =
            --               defaultHeadline
            --                 { meta = defaultMeta {metaCategory = "New category"}
            --                 }
            --           }
            --     }
            -- , TestCase
            --     { description = "Parse category property",
            --       inputs =
            --         [ ":CATEGORY: New category"
            --         ],
            --       expected =
            --         initialState
            --           { headline =
            --               defaultHeadline
            --                 { properties = makeProperties [("CATEGORY", "New category")]
            --                 , meta = defaultMeta {metaCategory = "New category"}
            --                 }
            --           }
            --     }
            -- , TestCase
            --     { description = "Parse custom underscored property",
            --       inputs =
            --         [ ":GLANCE_ID: MyAwesomeNote"
            --         ],
            --       expected = initialState {headline = defaultHeadline `withProperties` [("GLANCE_ID", "MyAwesomeNote")]}
            --     }
            -- , TestCase
            --     { description = "Parse todo pragma",
            --       inputs =
            --         [ "#+TODO: TODO(t) STARTED(s!) DELEGATED(e@/!) PENDING(p!) | DONE(d!) CANCELLED(c!)"
            --         ],
            --       expected =
            --         initialState
            --           { headline =
            --               defaultHeadline
            --                 { properties = defaultProperties,
            --                   meta = defaultMeta {todo = (["TODO", "STARTED", "DELEGATED", "PENDING"], ["DONE", "CANCELLED"])}
            --                 }
            --           }
            --     }
            -- , TestCase
            --     { description = "Meta inheritance",
            --       inputs =
            --         [ "* My first headline"
            --         , "[2021-08-22 Sun 10:18]"
            --         , "** My second headline"
            --         ],
            --       expected =
            --         initialState
            --           { headline = defaultHeadline
            --             { indent = EIndent 2
            --             , title = "My second headline"
            --             , meta = defaultMeta
            --             }
            --           }
            --     }

orgModeParserUnitTests :: TestTree
orgModeParserUnitTests = testGroup "Org-mode parser spec" assertMany
  where assert tc = testCase (description tc) $ assertEqual [] (expected tc) (result tc)
        result tc = case orgParseM (intercalate "\n" (inputs tc)) of
          (headlines, _context) -> Result headlines _context
        assertMany = map assert testCases
