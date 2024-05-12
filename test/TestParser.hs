module TestParser (orgModeParserUnitTests) where

import Data.Org
import Data.Text (Text, intercalate, unpack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestDefaults
import Data.Set qualified as Set
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)

strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack t) :: UTCTime

data ParsingResult = ParsingResult { elements :: ![OrgGenericElement]
                                   , context :: !OrgContext
                                   } deriving (Eq, Show)

data TestCase = TestCase { description :: !String
                         , inputs :: ![Text]
                         , expected :: !ParsingResult }

testCases :: [TestCase]
testCases = [ TestCase { description = "Parse headline with tags"
                       , inputs = ["* Hello world :a:b:c:"]
                       , expected = ParsingResult { elements = [ OrgGenericHeadline defaultHeadline { title = OrgTitle [ OrgTitleText (PlainText "Hello")
                                                                                                                       , OrgTitleSeparator SPC
                                                                                                                       , OrgTitleText (PlainText "world")
                                                                                                                       , OrgTitleSeparator SPC
                                                                                                                       , OrgTitleTags (OrgTags ["a", "b", "c"]) ]}]
                                                  , context = defaultContext }}

  , TestCase { description = "Parse headline with corrupted tag string"
             , inputs = ["* Hello world :a:b:c"]
             , expected = ParsingResult { elements = [ OrgGenericHeadline defaultHeadline { title = OrgTitle [ OrgTitleText (PlainText "Hello")
                                                                                                             , OrgTitleSeparator SPC
                                                                                                             , OrgTitleText (PlainText "world")
                                                                                                             , OrgTitleSeparator SPC
                                                                                                             , OrgTitleText (PlainText ":a:b:c") ]}]
                                        , context = defaultContext }}

  , TestCase { description = "Parse property block"
             , inputs = [ "* Hello"
                        , ":PROPERTIES:"
                        , ":TITLE: New title"
                        , ":END:" ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { title = OrgTitle [ OrgTitleText (PlainText "Hello") ]
                                                                                           , properties = OrgPropertyBlock [OrgProperty (OrgKeyword "TITLE") "New title"] })]
                                        , context = defaultContext }}

  , TestCase { description = "Parse drawer"
             , inputs = [ ":DRAWER:" ]
             , expected = ParsingResult { elements = [OrgGenericText (PlainText ":DRAWER:")]
                                        , context = defaultContext }}

  , TestCase { description = "Category pragma affects context"
             , inputs = [ "#+CATEGORY: Category 1" ]
             , expected = ParsingResult { elements = [OrgGenericPragma (OrgCategoryPragma "Category 1")]
                                        , context = defaultContext { metaCategory = "Category 1" }}}

  , TestCase { description = "Category property affects context"
             , inputs = [ "* Hello"
                        , ":PROPERTIES:"
                        , ":CATEGORY: Updated category"
                        , ":END:" ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { title = OrgTitle [ OrgTitleText (PlainText "Hello")
                                                                                                              , OrgTitleSeparator SPC]
                                                                                           , properties = OrgPropertyBlock [OrgProperty (OrgKeyword "CATEGORY") "Updated category"]})]
                                        , context = defaultContext { metaCategory = "Updated category" }}}

  , TestCase { description = "Parse complete headline"
             , inputs = ["** TODO [#A] Hello :a:b:c:"]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { indent = OrgIndent 2
                                                                                           , todo = OrgTodo (Just "TODO")
                                                                                           , priority = OrgPriority (Just 'A')
                                                                                           , title = OrgTitle [ OrgTitleText (PlainText "Hello")
                                                                                                              , OrgTitleSeparator SPC
                                                                                                              , OrgTitleTags (OrgTags ["a", "b", "c"])]})]
                                        , context = defaultContext }}

  , TestCase { description = "Parse headline with custom todo state"
             , inputs = [ "#+TODO: TODO | CANCELLED"
                        , "* CANCELLED Mess" ]
             , expected = ParsingResult { elements = [ OrgGenericPragma (OrgTodoPragma (Set.fromList ["TODO"]) (Set.fromList ["CANCELLED"]))
                                                     , OrgGenericHeadline (defaultHeadline { todo = OrgTodo (Just "CANCELLED")
                                                                                           , title = OrgTitle [ OrgTitleText (PlainText "Mess")
                                                                                                              , OrgTitleSeparator SPC]})]
                                        , context = defaultContext { metaTodoActive = Set.fromList ["TODO"]
                                                                   , metaTodoInactive = Set.fromList ["DONE", "CANCELLED"]}}}

  , TestCase { description = "No inactive todo states"
             , inputs = ["#+TODO: foo"]
             , expected = ParsingResult { elements = [OrgGenericPragma (OrgTodoPragma (Set.fromList ["FOO"]) (Set.fromList []))]
                                        , context = defaultContext { metaTodoActive = Set.fromList ["TODO", "FOO"]
                                                                   , metaTodoInactive = Set.fromList ["DONE"] }}}

  -- , TestCase { description = "Messed active/inactive todo states"
  --            , inputs = [ "#+TODO: CANCELLED | CANCELLED"
  --                       , "* CANCELLED Mess" ]
  --            , expected = ParsingResult { elements = [ OrgGenericPragma (OrgTodoPragma (Set.fromList ["CANCELLED"]) (Set.fromList ["CANCELLED"]))
  --                                                    , OrgGenericHeadline (defaultHeadline { todo = OrgTodo Nothing
  --                                                                                          , title = OrgTitle "CANCELLED Mess" })]
  --                                       , context = defaultContext { metaTodoActive = Set.fromList ["TODO"]
  --                                                                  , metaTodoInactive = Set.fromList ["DONE"] }}}

  , TestCase { description = "Parse several headlines (multiline parsing)"
             , inputs = [ "* foo"
                        , "* bar" ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline {title = OrgTitle [OrgTitleText (PlainText "foo")]})
                                                     , OrgGenericHeadline (defaultHeadline {title = OrgTitle [OrgTitleText (PlainText "bar")]})]
                                        , context = defaultContext }}

  , TestCase { description = "Empty text parsing"
             , inputs = [""]
             , expected = ParsingResult { elements = []
                                        , context = defaultContext }}

  , TestCase { description = "Restrict infinite parsing of eol / eof"
             , inputs = ["", "", ""]
             , expected = ParsingResult { elements = [ OrgGenericText (PlainText "") ]
                                        , context = defaultContext }}

  , TestCase { description = "Parse timestamps"
             , inputs = [ "<2024-01-01>"
                        , "<2024-01-01 Mon>" ]
             , expected = ParsingResult { elements = [ OrgGenericTimestamp OrgTimestamp {tsStatus = TsActive, tsRep = Nothing, tsTime = strptime "2024-01-01 00:00:00"}
                                                     , OrgGenericTimestamp OrgTimestamp {tsStatus = TsActive, tsRep = Nothing, tsTime = strptime "2024-01-01 00:00:00"}]
                                        , context = defaultContext }}

  , TestCase { description = "Parse schedule property"
             , inputs = [ "* foo"
                        , "SCHEDULED: <2024-04-28 Sun>"
                        , ":PROPERTIES:"
                        , ":CATEGORY: bar"
                        , ":END:" ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { title = OrgTitle [OrgTitleText (PlainText "foo")]
                                                                                           , schedule = Just OrgTimestamp { tsStatus = TsActive
                                                                                                                          , tsRep = Nothing
                                                                                                                          , tsTime = strptime "2024-04-28 00:00:00" }
                                                                                           , properties = OrgPropertyBlock [OrgProperty (OrgKeyword "CATEGORY") "bar"]})]
                                        , context = defaultContext { metaCategory = "bar" }}}

  , TestCase { description = "Parse links"
             , inputs = ["[[file:/home/foo/bar.org::*NN Pipeline][NN Pipeline]]"]
             , expected = ParsingResult { elements = [OrgGenericText (PlainText "[[file:/home/foo/bar.org::*NN Pipeline][NN Pipeline]]")]
                                        , context = defaultContext}}

  ]

    -- , TestCase
    --     { description = "Parse custom todo state",
    --       inputs =
    --         [ "#+TODO: TODO | CANCELLED",
    --           "* CANCELLED Mess"
    --         ],
    --       expected =
    --         OrgContext
    --           { headline =
    --               OrgHeadline
    --                 { indent = EIndent 1
    --                 , todo = ETodo "CANCELLED"
    --                 , priority = EPriority Nothing
    --                 , title = "Mess"
    --                 , tags = []
    --                 , properties = defaultProperties
    --                 , meta = defaultMeta { metaTodo = (["TODO"], ["DONE", "CANCELLED"])
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
    --         (defaultContext)
    --           { metaTime =
    --               [ strptime "2021-08-22 10:18:00"
    --               ]
    --           }
    --     },
    --   TestCase
    --     { description = "Headline title affects context",
    --       inputs =
    --         [ "* Headline with a timestamp in it [2021-08-22 Sun 11:37]"
    --         ],
    --       expected =
    --         (defaultContext)
    --           { metaTime =
    --               [ strptime "2021-08-22 11:37:00"
    --               ]
    --           }
    --     }
    -- , TestCase
    --     { description = "Parse minimal headline",
    --       inputs = ["* Hello"],
    --       expected =
    --         defaultContext
    --           { headline =
    --               OrgHeadline
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
    --         defaultContext
    --           { headline =
    --               OrgHeadline
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
    --         OrgContext
    --           { headline =
    --               defaultHeadline
    --                 { meta = defaultMeta {metaTodo = (["TODO", "PENDING", "STARTED"], ["DONE", "CANCELLED"])}
    --                 }
    --           }
    --     }
    -- , TestCase
    --     { description = "Parse missing todo state",
    --       inputs =
    --         [ "* CANCELLED Mess"
    --         ],
    --       expected =
    --         OrgContext
    --           { headline =
    --               OrgHeadline
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
    --         defaultContext
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
    --         defaultContext
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
    --       expected = defaultContext {headline = defaultHeadline `withProperties` [("GLANCE_ID", "MyAwesomeNote")]}
    --     }
    -- , TestCase
    --     { description = "Parse todo pragma",
    --       inputs =
    --         [ "#+TODO: TODO(t) STARTED(s!) DELEGATED(e@/!) PENDING(p!) | DONE(d!) CANCELLED(c!)"
    --         ],
    --       expected =
    --         defaultContext
    --           { headline =
    --               defaultHeadline
    --                 { properties = defaultProperties,
    --                   meta = defaultMeta {metaTodo = (["TODO", "STARTED", "DELEGATED", "PENDING"], ["DONE", "CANCELLED"])}
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
    --         defaultContext
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
        result tc = case mparse (intercalate "\n" (inputs tc)) of
          (headlines, context) -> ParsingResult headlines context
        assertMany = map assert testCases
