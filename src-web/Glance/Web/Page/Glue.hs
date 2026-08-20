-- | The shell's configuration blob: every server value @assets\/glue.js@ reads, as ONE JSON object the page emits ahead of it.
module Glance.Web.Page.Glue (glueConfig) where

import Data.Aeson (object, (.=))
import Data.Text (Text)

import Glance.Query ( archiveTag, followableTypes, materialTypes, linkColumns
                    , planningKeywords, tagColumns )
import Glance.Web.Base (codeList, docCells, jsonValue, logLinesDefault, logLinesMax, logLinesMin)

-- | The blob for VIEWS, the tree's saved views in registry order.  Member names are the script's @CFG.*@ reads.
glueConfig :: [(Text, Text)] -> Text
glueConfig views = jsonValue $ object
  [ "views"        .= [ object ["id" .= i, "query" .= q] | (i, q) <- views ]
  , "dcells"       .= map fst docCells
  , "planning"     .= planningKeywords
  , "archiveTag"   .= archiveTag
  , "followable"   .= followableTypes
  , "material"     .= materialTypes
  , "codes"        .= codeList
  , "lcols"        .= linkColumns
  , "tcols"        .= tagColumns
  , "log"          .= object [ "key" .= ("glance-log" :: Text)
                             , "def" .= logLinesDefault
                             , "min" .= logLinesMin
                             , "max" .= logLinesMax ]
  ]

