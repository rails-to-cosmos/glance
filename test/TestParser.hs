{-# LANGUAGE OverloadedStrings #-}

module TestParser (orgModeParserUnitTests) where

import           Data.Org
import           Data.Text (Text, intercalate)
import           Repl.State (parseOrgElements)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)
import           TestDefaults

data ParsingResult = ParsingResult { elements :: [OrgGenericElement]
                                   , context :: OrgContext
                                   } deriving (Eq, Show)

data TestCase = TestCase { description :: String
                         , inputs :: [Text]
                         , expected :: ParsingResult
                         }

testCases :: [TestCase]
testCases = [ TestCase { description = "Parse single tagged headline"
                       , inputs = [ "* Hello world :a:b:c:"
                                  ]
                       , expected = ParsingResult { elements = [ OrgGenericHeadline defaultHeadline { title = OrgTitle "Hello world"
                                                                                                    , tags = OrgTags ["a", "b", "c"]
                                                                                                    }
                                                               ]
                                                  , context = defaultContext}
             }

  , TestCase { description = "Parse single headline with corrupted tag string"
             , inputs = [ "* Hello world :a:b:c"
                        ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline defaultHeadline {title = OrgTitle "Hello world :a:b:c"}
                                                     ]
                                        , context = defaultContext
                                        }
             }

  , TestCase { description = "Parse property block"
             , inputs = [ "* Hello"
                        , ":PROPERTIES:"
                        , ":TITLE: New title"
                        , ":END:"
                        ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { title = OrgTitle "Hello"
                                                                                           , properties = OrgPropertyBlock [OrgProperty (OrgKeyword "TITLE") "New title"]
                                                                                           })
                                                     ]
                                        , context = defaultContext
                                        }
             }

  , TestCase { description = "Parse drawer"
             , inputs = [ ":DRAWER:"
                        ]
             , expected = ParsingResult { elements = [OrgGenericText (PlainText ":DRAWER:")]
                                        , context = defaultContext
                                        }
             }

  , TestCase { description = "Category pragma affects context"
             , inputs = [ "#+CATEGORY: Category 1"
                        ]
             , expected = ParsingResult { elements = [OrgGenericPragma (OrgCategoryPragma "Category 1")]
                                        , context = defaultContext { metaCategory = "Category 1" }
                                        }
             }

  , TestCase { description = "Category property affects context"
             , inputs = [ "* Hello"
                        , ":PROPERTIES:"
                        , ":CATEGORY: Updated category"
                        , ":END:"
                        ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { title = OrgTitle "Hello"
                                                                                           , properties = OrgPropertyBlock [ OrgProperty (OrgKeyword "CATEGORY") "Updated category"
                                                                                                                           ]
                                                                                           })
                                                     ]
                                        , context = defaultContext { metaCategory = "Updated category" }
                                        }
             }

  , TestCase { description = "Parse complete headline"
             , inputs = [ "** TODO [#A] This is a simple headline :a:b:c:"
                        ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { indent = OrgIndent 2
                                                                                           , todo = OrgTodo (Just "TODO")
                                                                                           , priority = OrgPriority (Just 'A')
                                                                                           , title = OrgTitle "This is a simple headline"
                                                                                           , tags = OrgTags ["a", "b", "c"]
                                                                                           })
                                                     ]
                                        , context = defaultContext
                                        }
             }

  , TestCase { description = "Parse headline with custom todo state"
             , inputs = [ "#+TODO: TODO | CANCELLED"
                        , "* CANCELLED Mess"
                        ]
             , expected = ParsingResult { elements = [ OrgGenericPragma (OrgTodoPragma ["TODO"] ["CANCELLED"])
                                                     , OrgGenericHeadline (defaultHeadline { todo = OrgTodo (Just "CANCELLED")
                                                                                           , title = OrgTitle "Mess"
                                                                                           })
                                                     ]
                                        , context = defaultContext {metaTodo = (["TODO"], ["DONE", "CANCELLED"])}}
             }

  , TestCase { description = "Parse several headlines (multiline parsing)"
             , inputs = [ "* foo"
                        , "* bar"
                        ]
             , expected = ParsingResult { elements = [ OrgGenericHeadline (defaultHeadline { title = OrgTitle "foo" })
                                                     , OrgGenericHeadline (defaultHeadline { title = OrgTitle "bar" })]
                                        , context = defaultContext
                                        }
             }
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
        result tc = case parseOrgElements defaultContext (intercalate "\n" (inputs tc)) of
          (headlines, context) -> ParsingResult headlines context
        assertMany = map assert testCases
