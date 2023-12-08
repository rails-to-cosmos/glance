{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Org
import           Data.Text (Text, intercalate)
import           Repl.State
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

data TestCase = TestCase { description :: String
                         , inputs :: [Text]
                         , expected :: (OrgGenericElement, OrgContext)
                         }

-- strptime :: Text -> UTCTime
-- strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack t) :: UTCTime
defaultHeadline :: OrgHeadline
defaultHeadline = mempty

defaultContext :: OrgContext
defaultContext = mempty

testCases :: [TestCase]
testCases =
  [ TestCase { description = "Parse simple headline with tags"
             , inputs = ["* Hello world :a:b:c:"]
             , expected = ( OrgGenericHeadline
                              defaultHeadline { title = OrgTitle "Hello world"
                                              , tags = OrgTags ["a", "b", "c"]
                                              }
                          , defaultContext)
             }
  , TestCase { description = "Corrupted tag string"
             , inputs = ["* Hello world :a:b:c"]
             , expected =
                 ( OrgGenericHeadline
                     defaultHeadline { title = OrgTitle "Hello world :a:b:c" }
                 , defaultContext)
             }]

    -- TestCase
    --   { description = "Parse headline with properties",
    --     inputs =
    --       [ "* Hello",
    --         ":PROPERTIES:",
    --         ":CATEGORY: New category",
    --         ":END:"
    --       ],
    --     expected =
    --       ( OrgGenericHeadline
    --           ( defaultHeadline
    --               { title = OrgTitle [OrgTitleText (PlainText "Hello")],
    --                 properties = OrgPropertyBlock [OrgProperty (OrgKeyword "CATEGORY") "New category"]
    --               }
    --           ),
    --         defaultContext {metaCategory = "New category"}
    --       )
    --   }
    -- ( TestCase
    --       "Category property affects context"
    --       [ ":PROPERTIES:"
    --       , ":CATEGORY: New category"
    --       , ":END:"
    --       ]
    --       (defaultContext) { metaCategory = "New category" }
    --   )
    -- , TestCase
    --     { description = "Category pragma affects context",
    --       inputs =
    --         [ "#+CATEGORY: Category 1"
    --         , "#+CATEGORY: Category 2"
    --         ],
    --       expected = (defaultContext) { metaCategory = "Category 2" }
    --     }
    -- , TestCase
    --     { description = "Todo pragma affects context",
    --       inputs =
    --         [ "#+TODO: PENDING | CANCELLED",
    --           "#+TODO: STARTED(s!) | CANCELLED(c!)"
    --         ],
    --       expected = (defaultContext) { metaTodo = (["TODO", "PENDING", "STARTED"], ["DONE", "CANCELLED"]) }
    --     }
    -- , TestCase
    --     { description = "Parse basic headline",
    --       inputs = ["** TODO [#A] This is a simple headline :a:b:c:"],
    --       expected = defaultContext
    --     },
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
main :: IO ()
main = defaultMain (testGroup "Org-mode parsers" assertAll)

assertOne :: TestCase -> TestTree
assertOne tc =
  testCase (description tc) (assertEqual [] (expected tc) (actual tc))

assertAll :: [TestTree]
assertAll = map assertOne testCases

actual :: TestCase -> (OrgGenericElement, OrgContext)
actual tc = applyCommand defaultContext (intercalate "\n" (inputs tc))
