-- | The floor the web layer stands on: what more than one module above needs.
module Glance.Web.Base ( ServeOptions (..)
                       , walkFor
                         -- * The one clock read
                       , Day
                       , today
                       , defaultPort
                       , logLinesDefault
                       , logLinesMin
                       , logLinesMax
                       , glueAsset
                       , elmAsset
                       , gluePartFiles
                       , rendererAsset
                       , viewTitleFor
                       , tenths
                       , codeList
                       , docCells
                         -- * Bodies
                       , withBody
                       , bodyObject
                         -- * What a write route answers
                       , noSuchRow
                       , conflict
                       , plannedValue
                       , unreadable
                       , reparsed
                       , rewritten
                       , configMoved
                       , captureMoved
                       , answerWrite
                         -- * Responses
                       , jsonType
                       , sized
                       , jsonResponse
                       , jsonError
                       , html
                       , plain
                       , escape
                       , jsonValue
                       ) where

import Data.Aeson (Object, ToJSON, Value, eitherDecode', encode, object, toJSON, withObject, (.=))
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Network.HTTP.Types ( Header, Status, hContentType, status200, status409
                          , status413, status500 )
import Network.HTTP.Types.Header (hContentLength)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Glance.Query ( HeadlineRecord, WalkOptions (..), WriteFailure (..)
                    , captureCodes, hrPriority, hrState, hrTags, hrTitle
                    , planningTimestamp, readsAsTimestamp, settableKeywords )


-- | What one server serves.
data ServeOptions = ServeOptions
  { soDir     :: !FilePath          -- ^ org root, walked once at startup and watched after.
  , soPort    :: !Int               -- ^ loopback port to listen on.
  , soAssets  :: !(Maybe FilePath)  -- ^ @--assets@ directory; 'Nothing' serves 'Glance.Web.Routes.embeddedRenderer'.
  , soDerived :: !Bool              -- ^ serve org-glance's mirror directories too; see 'Data.Org.Walk'.
  } deriving (Eq, Show)

-- | A file the walk passed over must not come back through an inotify event.
walkFor :: ServeOptions -> WalkOptions
walkFor opts = WalkOptions { woIncludeDerived = soDerived opts }

-- | The server's own day, off the local clock.  ONE CLOCK READ PER REQUEST,
-- taken before any row: a request that spans midnight must mean ONE day, so
-- every reader takes the day from here and none reads the clock a second time.
today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime

defaultPort :: Int
defaultPort = 7777

-- | The event strip's height; the stylesheet spells the same cap.
logLinesDefault, logLinesMin, logLinesMax :: Int
logLinesDefault = 7
logLinesMin = 1
logLinesMax = 50


rendererAsset :: FilePath
rendererAsset = "table-view.js"

glueAsset :: FilePath
glueAsset = "glue.js"

elmAsset :: FilePath
elmAsset = "elm.js"

-- | THE SHELL, ONE WIDGET PER FILE.  ORDER IS DATA, stated here alone.
gluePartFiles :: [FilePath]
gluePartFiles =
  [ "00-core.js"       -- the config blob, the log strip, the wash, fetching, the query, the crumbs
  , "05-keys.js"       -- key naming and the echo pill, behind an argument list
  , "20-sheet.js"      -- the materialize sheet: both panes, the ladder, the opening
  , "30-capture.js"    -- the capture form and the value palette
  , "40-popups.js"     -- the link popup and the tags popup
  , "50-settings.js"   -- the settings sheet: tabs, saved views, the states table, the theme
  , "60-refer.js"      -- `@' in the sheet: the reference picker, a table-view over /refer
  , "70-shell.js"      -- the modal surfaces, the dispatch and the boot
  ]

tenths :: Double -> Double
tenths s = fromIntegral (round (s * 10) :: Int) / 10

-- | 'captureCodes' as objects: the code and the one line saying what it does,
-- the shape @GET \/capture@ serves and the boot blob carries.
codeList :: [Value]
codeList = [ object ["code" .= code, "means" .= means] | (code, means) <- captureCodes ]

-- | The doc pane's cells, ONE list: the route builds the values off it and the
-- shell's config blob takes its keys from it, and the pane indexes the one by
-- the other.  Spelled apart, a key added to the route was never drawn and a key
-- renamed drew an empty cell, both in silence.
docCells :: [(Text, HeadlineRecord -> Value)]
docCells = [ ("state",    toJSON . hrState)
           , ("priority", toJSON . hrPriority)
           , ("title",    toJSON . hrTitle)
           , ("tags",     toJSON . hrTags) ]

bodyLimit :: Int
bodyLimit = 1024 * 1024

tooBig :: Text
tooBig = "body over " <> T.pack (show bodyLimit) <> " bytes"

noSuchRow :: Text -> Text
noSuchRow rid = "no headline with id " <> rid

-- | A 409 spelling REASON, the digest on disk now, and WHY as a whole sentence.
conflict :: Text -> Text -> Text -> Response
conflict reason current why = jsonResponse status409
  [ "error"  .= why
  , "reason" .= reason
  , "digest" .= current
  ]

-- | KEY's planning value, read the way ITS OWN KEY is written, or the refusal in
-- that reader's own words.  A value no timestamp parser reads back may not land:
-- the line silently stops being a planning line on the next load.
--
-- ONE ARM PER KEY, AND BOTH WRITE DOORS READ IT HERE.  @SCHEDULED@ and
-- @DEADLINE@ take 'planningTimestamp' — the wall is also the TRANSFORM, so a
-- pane may type any spelling that grammar owns and gets back the bytes org
-- itself would write.  @CLOSED@ is org's own bookkeeping and takes REPARSE
-- alone: verbatim or refused, no English widened to it, @timestamp@ being what
-- that reading actually wants.  @set-planning@ and @POST \/headline@ asking the
-- same function is what makes the two doors agree BY CONSTRUCTION rather than by
-- two spellings that must be kept level.
plannedValue :: Day -> Text -> Text -> Either Text Text
plannedValue day key value
  | key `elem` settableKeywords = planningTimestamp day value
  | readsAsTimestamp value      = Right value
  | otherwise                   = Left (unreadable key)

unreadable :: Text -> Text
unreadable key = key <> " is not a timestamp org would read back"
  <> "; spell it <2026-08-01 Sat> or clear the row"

reparsed, rewritten, configMoved :: Text
reparsed  = "the file was re-read since this subtree was materialized" <> again
rewritten = "the file changed on disk since this subtree was materialized" <> again
configMoved = "the config file changed on disk since it was read; open settings again"

captureMoved :: FilePath -> Text
captureMoved path =
  T.pack path <> " changed on disk while the entry was being written; capture it again"

-- | A drift-locked write's answer: OK's fields, the 409 MOVED spells, or a 500.
answerWrite :: Text -> (Text -> [Pair]) -> Either WriteFailure Text -> Response
answerWrite moved ok written = case written of
  Right fresh              -> jsonResponse status200 (ok fresh)
  Left (WriteDrift onDisk) -> conflict "drift" onDisk moved
  Left (WriteRefused why)  -> jsonError status500 why

again :: Text
again = "; materialize it again and re-apply the edit"

-- | Chunk by chunk: 'Network.Wai.strictRequestBody' pays before it can refuse.
takeBody :: Int -> Request -> IO (Maybe BL.ByteString)
takeBody limit request = go 0 []
  where
    go seen chunks = do
      chunk <- getRequestBodyChunk request
      let taken = seen + BS.length chunk
      if BS.null chunk        then pure (Just (BL.fromChunks (reverse chunks)))
        else if taken > limit then pure Nothing
        else go taken (chunk : chunks)

-- | One door, so the 413 OUTRANKS every other refusal on all three routes.
withBody :: Request -> (BL.ByteString -> IO Response) -> IO Response
withBody request k =
  maybe (pure (jsonError status413 tooBig)) k =<< takeBody bodyLimit request

-- | NAME is what @aeson@ calls the object; ONE door, so @body:@ is spelled once.
bodyObject :: String -> (Object -> Parser a) -> BL.ByteString -> Either Text a
bodyObject name shape raw = first (("body: " <>) . T.pack) $
  parseEither (withObject name shape) =<< eitherDecode' raw

viewTitleFor :: FilePath -> Text
viewTitleFor dir = T.pack dir <> " — glance"

-- | Angle brackets escaped: inside @\<script\>@, @\<\/@ closes it whatever JSON says.
jsonValue :: ToJSON a => a -> Text
jsonValue = T.replace "<" "\\u003c" . T.replace ">" "\\u003e"
          . TE.decodeUtf8 . BL.toStrict . encode

escape :: Text -> Text
escape = T.concatMap esc
  where esc '&'  = "&amp;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '"'  = "&quot;"
        esc '\'' = "&#39;"
        esc c    = T.singleton c


jsonType :: Header
jsonType = (hContentType, "application/json; charset=utf-8")

-- | The gzip threshold reads the length header, so warp's own comes too late.
sized :: Status -> [Header] -> BL.ByteString -> Response
sized status headers body =
  responseLBS status ((hContentLength, BSC.pack (show (BL.length body))) : headers) body

jsonResponse :: Status -> [Pair] -> Response
jsonResponse status fields = sized status [jsonType] (encode (object fields))

jsonError :: Status -> Text -> Response
jsonError status msg = jsonResponse status ["error" .= msg]

html :: Text -> Response
html body = sized status200 [(hContentType, "text/html; charset=utf-8")] (utf8 body)

plain :: Status -> Text -> Response
plain status msg =
  sized status [(hContentType, "text/plain; charset=utf-8")] (utf8 (msg <> "\n"))

utf8 :: Text -> BL.ByteString
utf8 = BL.fromStrict . TE.encodeUtf8
