-- | The floor the rest of the web layer stands on: what one server serves,
-- and how it answers.
--
-- 'ServeOptions' is here because every module above takes one.  The response
-- constructors and the body reader are here because the route table and the
-- command table both answer through them, and the write-refusal vocabulary
-- ('answerWrite' and the sentences it chooses between) because three routes
-- write files and a client's handling of a refusal must not turn on which one
-- it asked.
module Glance.Web.Base ( ServeOptions (..)
                       , walkFor
                       , defaultPort
                       , logLinesDefault
                       , logLinesMin
                       , logLinesMax
                       , logLinesBand
                       , rendererAsset
                       , viewTitleFor
                       , tenths
                         -- * Bodies
                       , withBody
                       , bodyObject
                         -- * What a write route answers
                       , noSuchRow
                       , conflict
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

import Data.Aeson (Object, ToJSON, eitherDecode', encode, object, withObject, (.=))
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import Network.HTTP.Types ( Header, Status, hContentType, status200, status409
                          , status413, status500 )
import Network.HTTP.Types.Header (hContentLength)
import Network.Wai (Request, Response, getRequestBodyChunk, responseLBS)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Glance.Query (WalkOptions (..), WriteFailure (..))

-- Options

-- | What one server serves.
data ServeOptions = ServeOptions
  { soDir     :: !FilePath          -- ^ org root, walked once at startup and watched after.
  , soPort    :: !Int               -- ^ loopback port to listen on.
  , soAssets  :: !(Maybe FilePath)  -- ^ @--assets@ directory; 'Nothing' serves 'Glance.Web.Routes.embeddedRenderer'.
  , soDerived :: !Bool              -- ^ serve org-glance's mirror directories too; see 'Data.Org.Walk'.
  } deriving (Eq, Show)

-- | How OPTS wants the tree walked, for the load and for the watch alike: a
-- file the walk passed over must not come back through an inotify event.
walkFor :: ServeOptions -> WalkOptions
walkFor opts = WalkOptions { woIncludeDerived = soDerived opts }

defaultPort :: Int
defaultPort = 7777

-- | How many of its own line boxes the event strip grows to before it stops and
-- scrolls inside itself, and the band a reader may move that to from the
-- settings sheet.  Seven fills the strip a working page produces without giving
-- half the window to a log nobody is reading; one shows the last line, and past
-- fifty the table has nothing left to be.  The number is a @localStorage@
-- preference (@glance-log@), so these three are the DEFAULT and the two walls
-- the glue validates against — spelled here because the stylesheet's own cap is
-- the same figure and the two must not drift.
logLinesDefault, logLinesMin, logLinesMax :: Int
logLinesDefault = 7
logLinesMin = 1
logLinesMax = 50

-- | The band as the settings field's placeholder shows it.
logLinesBand :: Text
logLinesBand = T.pack (show logLinesMin <> "–" <> show logLinesMax)

-- | The asset the demo shell loads.  Served from 'Glance.Web.Routes.embeddedRenderer' by default;
-- under @--assets@ its presence in that directory decides which page @\/@ serves.
rendererAsset :: FilePath
rendererAsset = "table-view.js"

-- | S rounded the way a banner and an indexing body both want it.
tenths :: Double -> Double
tenths s = fromIntegral (round (s * 10) :: Int) / 10

-- | The largest commit body this server reads, in bytes.  A subtree is org
-- text and a megabyte of it is an enormous one; past that the request is a
-- mistake or an attack, and either way the answer is a 413.
bodyLimit :: Int
bodyLimit = 1024 * 1024

-- | What a body past 'bodyLimit' is told, on every route that reads one.
tooBig :: Text
tooBig = "body over " <> T.pack (show bodyLimit) <> " bytes"

-- | What a route says about an id the store holds no row for.
noSuchRow :: Text -> Text
noSuchRow rid = "no headline with id " <> rid

-- | A 409 spelling REASON, the digest the file carries now, and WHY.  The two
-- ways a materialized subtree goes stale are told apart for a client that has
-- to decide what to do next; both mean the same thing to one that does not.
-- Every write route answers a moved file this way, so WHY carries the whole
-- sentence rather than half of one this appends to.
conflict :: Text -> Text -> Text -> Response
conflict reason current why = jsonResponse status409
  [ "error"  .= why
  , "reason" .= reason
  , "digest" .= current
  ]

-- | Why KEY's planning entry was refused.
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

-- | What a drift-locked write answers: the file's new digest with whatever else
-- OK wants beside it, the 409 MOVED spells, or the 500 the engine's own refusal
-- is.  All three write routes answer this way, so the sentence a moved file
-- gets is the only thing any of them chooses.
answerWrite :: Text -> (Text -> [Pair]) -> Either WriteFailure Text -> Response
answerWrite moved ok written = case written of
  Right fresh              -> jsonResponse status200 (ok fresh)
  Left (WriteDrift onDisk) -> conflict "drift" onDisk moved
  Left (WriteRefused why)  -> jsonError status500 why

again :: Text
again = "; materialize it again and re-apply the edit"

-- | At most LIMIT bytes of REQUEST's body, or 'Nothing' when there are more of
-- them.  Chunk by chunk rather than through 'Network.Wai.strictRequestBody': a
-- cap that measures the body once it is all in memory has already paid for
-- what it means to refuse.
takeBody :: Int -> Request -> IO (Maybe BL.ByteString)
takeBody limit request = go 0 []
  where
    go seen chunks = do
      chunk <- getRequestBodyChunk request
      let taken = seen + BS.length chunk
      if BS.null chunk        then pure (Just (BL.fromChunks (reverse chunks)))
        else if taken > limit then pure Nothing
        else go taken (chunk : chunks)

-- | REQUEST's body handed to K, or the 413.  The three routes that read a body
-- share the door, so the cap OUTRANKS every other refusal on all of them alike:
-- this server declines to read a megabyte to find out what it says.
withBody :: Request -> (BL.ByteString -> IO Response) -> IO Response
withBody request k =
  maybe (pure (jsonError status413 tooBig)) k =<< takeBody bodyLimit request

-- | RAW read through SHAPE, or what is wrong with it — NAME being what @aeson@
-- calls the object in its own message.  ONE door for the three routes that take
-- a JSON body, so the wire's @body:@ prefix is written once and a malformed
-- request reads the same whichever of them was asked.  Both refusals come
-- through it: a body that is not JSON at all, and one whose shape the parser
-- declines.
bodyObject :: String -> (Object -> Parser a) -> BL.ByteString -> Either Text a
bodyObject name shape raw = first (("body: " <>) . T.pack) $
  parseEither (withObject name shape) =<< eitherDecode' raw

-- | The view title for DIR: what the browser tab and the table heading show.
-- Exported so the suite renders the same document the server does.
viewTitleFor :: FilePath -> Text
viewTitleFor dir = T.pack dir <> " — glance"

-- | X as a JavaScript literal — a string, an array, an object — escaped through
-- the JSON encoder so the glue can carry a value the tree supplies without a
-- quoting rule of its own.  The angle brackets go the way 'Glance.Web.Keymap.keyBindingsJSON'
-- sends them: the literal sits inside a @\<script\>@ element, where @\<\/@
-- closes it whatever the JSON says.
jsonValue :: ToJSON a => a -> Text
jsonValue = T.replace "<" "\\u003c" . T.replace ">" "\\u003e"
          . TE.decodeUtf8 . BL.toStrict . encode

-- | T with the five characters that would leave text mode escaped.
escape :: Text -> Text
escape = T.concatMap esc
  where esc '&'  = "&amp;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '"'  = "&quot;"
        esc '\'' = "&#39;"
        esc c    = T.singleton c

-- Responses

jsonType :: Header
jsonType = (hContentType, "application/json; charset=utf-8")

-- | STATUS with HEADERS and BODY, the body's length among them.  Warp writes a
-- @Content-Length@ too, downstream of every middleware; the gzip threshold
-- reads that header off the response, so a body too small to be worth
-- compressing is recognisable as one only when the length is written here, and
-- 'Network.Wai.Middleware.Gzip.gzip' drops it again where it compresses.
sized :: Status -> [Header] -> BL.ByteString -> Response
sized status headers body =
  responseLBS status ((hContentLength, BSC.pack (show (BL.length body))) : headers) body

-- | STATUS with FIELDS as its JSON body.  Hand-built like the view document:
-- these objects are a contract with the shell rather than a type's projection.
jsonResponse :: Status -> [Pair] -> Response
jsonResponse status fields = sized status [jsonType] (encode (object fields))

-- | STATUS carrying MSG as @{"error": …}@, so a refusal parses the way the
-- success it replaces does.
jsonError :: Status -> Text -> Response
jsonError status msg = jsonResponse status ["error" .= msg]

html :: Text -> Response
html body = sized status200 [(hContentType, "text/html; charset=utf-8")] (utf8 body)

-- | STATUS with MSG as its whole body — errors read in a terminal as well as
-- in a browser.
plain :: Status -> Text -> Response
plain status msg =
  sized status [(hContentType, "text/plain; charset=utf-8")] (utf8 (msg <> "\n"))

utf8 :: Text -> BL.ByteString
utf8 = BL.fromStrict . TE.encodeUtf8
