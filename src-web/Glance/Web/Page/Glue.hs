-- | The shell's configuration blob — every server value the script reads,
-- as ONE JSON object the page emits ahead of it.
--
-- The script itself is @assets\/glue.js@, a real JavaScript file compiled
-- into the binary the way the renderer is ('Glance.Web.Routes'); this module
-- is what is left of the 5.2k-line Haskell string list it used to be
-- (docs\/proposal-glue-extraction.md).  Eight members are per-build
-- constants off the library; @defaultQuery@ is the one per-request value and
-- the reason the blob is the PAGE's to emit.  The blob rides a
-- @\<script type="application\/json"\>@ element — the keymap blob's own
-- pattern — so the page still inlines no executable JS beyond the file it
-- names.
module Glance.Web.Page.Glue (glueConfig) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import Glance.Query (captureCodes, followableTypes, linkColumns, planningKeywords, tagColumns)
import Glance.Web.Base (jsonValue, logLinesDefault, logLinesMax, logLinesMin)

-- | The blob for WANTED, the tree's default view.  Member names are the
-- script's @CFG.*@ reads; a member added here without its read is inert, one
-- read without its member is an undefined the suite's boot catches.
glueConfig :: Text -> Text
glueConfig wanted = jsonValue $ object
  [ "defaultQuery" .= wanted
  , "dcells"       .= (["state", "priority", "title", "tags"] :: [Text])
  , "planning"     .= planningKeywords
  , "followable"   .= followableTypes
  , "codes"        .= codeList
  , "lcols"        .= linkColumns
  , "tcols"        .= tagColumns
  , "log"          .= object [ "key" .= ("glance-log" :: Text)
                             , "def" .= logLinesDefault
                             , "min" .= logLinesMin
                             , "max" .= logLinesMax ]
  ]

-- | 'captureCodes' as the objects the page's own completion reads: the code and
-- the one line saying what it does, in the order the list declares them.  The
-- same shape @GET \/capture@ serves, so a client that reads it there and this
-- blob's copy cannot come to describe two different subsets.
codeList :: [Value]
codeList = [ object ["code" .= code, "means" .= means] | (code, means) <- captureCodes ]
