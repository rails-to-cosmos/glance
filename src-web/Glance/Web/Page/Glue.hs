-- | The shell's configuration blob: every server value @assets\/glue.js@ reads, as ONE JSON object the page emits ahead of it.
module Glance.Web.Page.Glue (glueConfig) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import Glance.Query ( archiveTag, captureCodes, followableTypes, linkColumns
                    , planningKeywords, tagColumns )
import Glance.Web.Base (docCells, jsonValue, logLinesDefault, logLinesMax, logLinesMin)

-- | The blob for VIEWS, the tree's saved views in registry order.  Member names are the script's @CFG.*@ reads.
glueConfig :: [(Text, Text)] -> Text
glueConfig views = jsonValue $ object
  [ "views"        .= [ object ["id" .= i, "query" .= q] | (i, q) <- views ]
  , "dcells"       .= map fst docCells
  , "planning"     .= planningKeywords
  , "archiveTag"   .= archiveTag
  , "followable"   .= followableTypes
  , "codes"        .= codeList
  , "lcols"        .= linkColumns
  , "tcols"        .= tagColumns
  , "log"          .= object [ "key" .= ("glance-log" :: Text)
                             , "def" .= logLinesDefault
                             , "min" .= logLinesMin
                             , "max" .= logLinesMax ]
  ]

-- | 'captureCodes' as objects: the code and the one line saying what it does, the shape @GET \/capture@ serves.
codeList :: [Value]
codeList = [ object ["code" .= code, "means" .= means] | (code, means) <- captureCodes ]
