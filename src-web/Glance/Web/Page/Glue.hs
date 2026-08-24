-- | The shell's configuration blob: every server value @assets\/glue.js@ reads, as ONE JSON object the page emits ahead of it.
module Glance.Web.Page.Glue (glueConfig) where

import Data.Aeson (object, (.=))
import Data.Text (Text)

import Glance.Query ( archiveTag, followableTypes, materialTypes, linkColumns
                    , planningKeywords, settableKeywords, tagColumns )
import Glance.Web.Base ( codeList, docCells, jsonValue
                       , logLinesDefault, logLinesMax, logLinesMin
                       , zoomDefault, zoomMax, zoomMin, zoomStep )

-- | The blob for VIEWS, the tree's saved views in registry order.  Member names are the script's @CFG.*@ reads.
glueConfig :: [(Text, Text)] -> Text
glueConfig views = jsonValue $ object
  [ "views"        .= [ object ["id" .= i, "query" .= q] | (i, q) <- views ]
  , "dcells"       .= map fst docCells
  , "planning"     .= planningKeywords
  -- The two of the three whose value this server COMPOSES, which is the two that
  -- OWE A DATE: CLOSED is written verbatim or refused, so no field over it offers
  -- a phrase.  CARRIED RATHER THAN RESPELLED, so the widget's walls are the
  -- server's own list and cannot drift from it.
  , "settable"     .= settableKeywords
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
  -- The zoom band is the SERVER's, so the page's own clamp and the window's are
  -- one figure read twice rather than two figures written twice.
  , "zoom"         .= object [ "key" .= ("glance-zoom" :: Text)
                             , "def" .= zoomDefault
                             , "min" .= zoomMin
                             , "max" .= zoomMax
                             , "step" .= zoomStep ]
  ]

