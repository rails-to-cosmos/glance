-- | @POST \/mcp@: the command engine and the read views as an MCP tool
-- catalog, over JSON-RPC 2.0.  ONE CORE, injected transports: the door is
-- handed the very handlers @\/command@ and @\/headline@ run
-- ('McpTools'), so a tool is that handler's answer re-wrapped, never a
-- second write path.  The write tools ARE 'commandNames', checked by the suite.
module Glance.Web.Mcp
  ( McpTools (..)
  , mcpRoute
  , mcpHandle
  , runMcpStdio
  , mcpWriteToolNames
  ) where

import Control.Monad (unless)
import Data.Aeson (Value (Number, Object, String), encode, eitherDecode', object, (.=))
import Data.Char (isSpace)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Types (Status, status200, status202, statusCode)
import Network.Wai (Request, Response, responseToStream)
import System.IO (BufferMode (LineBuffering), hSetBuffering, isEOF, stdin, stdout)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL

import Glance.Web.Base (jsonType, jsonValue, sized, withBody)


-- | The handlers the door dispatches to, wired to the live Hub by the caller.
-- The write path is @\/command@'s own core over an already-read body; the two
-- reads are @GET \/headline@ and @GET \/headlines@.
data McpTools = McpTools
  { mtWrite     :: BL.ByteString -> IO Response         -- ^ a @\/command@ body.
  , mtHeadline  :: Text -> IO Response                  -- ^ one subtree by row id.
  , mtHeadlines :: Maybe Text -> Maybe Int -> IO Response  -- ^ a query and a cap.
  }

-- | @POST \/mcp@: one JSON-RPC message in, one response out (a notification
-- gets an empty 202).  Streamable-HTTP's single-JSON subset.
mcpRoute :: McpTools -> Request -> IO Response
mcpRoute tools request = withBody request $ \raw ->
  maybe (sized status202 [jsonType] "") (jsonRpc status200) <$> mcpHandle tools raw

-- | ONE JSON-RPC message to its response, the core both transports share:
-- 'Nothing' for a notification (no reply owed), 'Just' the response Value
-- otherwise, a parse or shape error included.
mcpHandle :: McpTools -> BL.ByteString -> IO (Maybe Value)
mcpHandle tools raw = case eitherDecode' raw of
  Left _ -> pure (Just (rpcError Nothing (-32700) "parse error: expected a JSON-RPC message"))
  Right v -> case message v of
    Left why -> pure (Just (rpcError Nothing (-32600) why))
    -- ABSENT id is a notification: acknowledged, never answered.
    Right (Nothing, _method, _params) -> pure Nothing
    Right (Just rid, method, params) -> Just <$> answer tools rid method params

-- | @glance mcp@'s transport: newline-delimited JSON-RPC over stdin\/stdout, the
-- MCP stdio contract.  A message a line, its response a line; a notification is
-- read and answered with nothing; a blank line is skipped.  Ends at EOF.
runMcpStdio :: McpTools -> IO ()
runMcpStdio tools = hSetBuffering stdout LineBuffering >> loop
  where
    loop = do
      eof <- isEOF
      unless eof $ do
        line <- BSC.hGetLine stdin
        unless (BSC.all isSpace line) $ do
          resp <- mcpHandle tools (BL.fromStrict line)
          mapM_ (\v -> BL.hPut stdout (encode v) >> BS.hPut stdout "\n") resp
        loop

jsonRpc :: Status -> Value -> Response
jsonRpc status v = sized status [jsonType] (encode v)

-- | A JSON-RPC message as (id, method, params); 'Nothing' id is a notification.
message :: Value -> Either Text (Maybe Value, Text, Value)
message (Object o) = case KM.lookup "method" o of
  Just (String m) -> Right (KM.lookup "id" o, m, fromMaybe (object []) (KM.lookup "params" o))
  _               -> Left "a JSON-RPC message needs a string \"method\""
message _ = Left "a JSON-RPC message is an object"

answer :: McpTools -> Value -> Text -> Value -> IO Value
answer tools rid method params = case method of
  "initialize" -> pure (rpcResult rid (initialized params))
  "ping"       -> pure (rpcResult rid (object []))
  "tools/list" -> pure (rpcResult rid (object ["tools" .= map info toolTable]))
  "tools/call" -> callTool tools rid params
  _            -> pure (rpcError (Just rid) (-32601) ("method not found: " <> method))

rpcResult :: Value -> Value -> Value
rpcResult rid result = object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result]

-- | An absent id ('Nothing') echoes as JSON @null@, which is what a parse or a
-- shape error owes when the id could not be read.
rpcError :: Maybe Value -> Int -> Text -> Value
rpcError rid code msg = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= rid
  , "error" .= object ["code" .= code, "message" .= msg]
  ]

-- | The @initialize@ result: echo the client's protocol version when it names
-- one, else the version glance speaks.
initialized :: Value -> Value
initialized params = object
  [ "protocolVersion" .= fromMaybe protocolVersion (argText "protocolVersion" params)
  , "capabilities" .= object ["tools" .= object []]
  , "serverInfo" .= object ["name" .= ("glance" :: Text), "version" .= serverVersion]
  ]
  where
    protocolVersion = "2024-11-05" :: Text
    serverVersion = "0.1" :: Text

callTool :: McpTools -> Value -> Value -> IO Value
callTool tools rid params = case argText "name" params of
  Nothing   -> pure (rpcError (Just rid) (-32602) "tools/call wants params {\"name\": …}")
  Just name -> case find ((== name) . toolName) toolTable of
    Nothing -> pure (rpcError (Just rid) (-32602) ("no such tool: " <> name))
    Just t  -> do
      (status, val) <- toolRun t tools (argValue "arguments" params)
      pure (rpcResult rid (content status val))

-- | A tool answer wrapped as MCP content: the handler's JSON as text, and
-- @isError@ off its HTTP status (a per-row refusal is a 200 the agent reads).
content :: Status -> Value -> Value
content status val = object
  [ "content" .= [ object ["type" .= ("text" :: Text), "text" .= jsonValue val] ]
  , "isError" .= (statusCode status >= 400)
  ]


-- The catalog

data Tool = Tool
  { toolName   :: !Text
  , toolDesc   :: !Text
  , toolSchema :: !Value
  , toolRun    :: McpTools -> Value -> IO (Status, Value)
  }

info :: Tool -> Value
info t = object
  ["name" .= toolName t, "description" .= toolDesc t, "inputSchema" .= toolSchema t]

toolTable :: [Tool]
toolTable = writeTools <> readTools

-- | The write tools, one per command verb; the suite pins these to
-- 'Glance.Web.Commands.commandNames'.
mcpWriteToolNames :: [Text]
mcpWriteToolNames = map toolName writeTools

writeTools :: [Tool]
writeTools =
  [ writeTool "capture"
      "Create a headline. Either {text: \"TODO Buy milk :errands:\"} for a raw line, or a\
      \ draft {title, …}. With a tag it files a blob under org-glance; without, into the inbox."
      [ ("text", str "a raw org headline line, e.g. \"TODO Buy milk :errands:\"")
      , ("title", str "the draft's title (use instead of text)")
      , ("state", str "a TODO keyword for the draft")
      , ("priority", str "a priority letter, e.g. A")
      , ("tags", arrStr "the draft's tags")
      , ("planning", pairs "planning entries, [[\"SCHEDULED\", \"+3d\"], …]")
      , ("properties", pairs "drawer properties, [[KEY, VALUE], …]")
      , ("body", str "the draft's body text")
      , ("tag", str "file under this org-glance kind rather than the inbox")
      ] []
  , writeTool "set-title" "Rename a headline."
      [idProp, ("title", str "the new title")] ["id", "title"]
  , writeTool "set-state" "Set (or clear) a headline's TODO keyword."
      [idProp, ("keyword", strOrNull "a TODO keyword, e.g. DONE; null clears it")] ["id"]
  , writeTool "set-priority" "Set (or clear) a headline's priority."
      [idProp, ("priority", strOrNull "a priority letter, e.g. A; null clears it")] ["id"]
  , writeTool "set-planning" "Set (or clear) a SCHEDULED, DEADLINE, or CLOSED date."
      [idProp, ("keyword", str "SCHEDULED, DEADLINE, or CLOSED")
      , ("date", strOrNull "a date like 2026-09-08 or +3d; null takes the entry off")]
      ["id", "keyword"]
  , writeTool "add-tag" "Add a tag to a headline."
      [idProp, ("tag", str "the tag, e.g. work")] ["id", "tag"]
  , writeTool "remove-tag" "Remove a tag from a headline."
      [idProp, ("tag", str "the tag to remove")] ["id", "tag"]
  , writeTool "rename-tag" "Rename a tag on a headline."
      [idProp, ("from", str "the current tag"), ("to", str "the new tag")]
      ["id", "from", "to"]
  , writeTool "edit-link" "Edit a link in a headline's title, by its character span."
      [idProp, ("span", span'), ("target", str "where the link points")
      , ("desc", strOrNull "the link's description; null for a bare link")]
      ["id", "span", "target"]
  , writeTool "archive" "Add the archive tag to a headline (a soft delete)."
      [idProp] ["id"]
  , writeTool "delete" "Move an archived headline's blob to the trash (only after archive)."
      [idProp] ["id"]
  ]
  where idProp = ("id", str "the row id, e.g. FILE.org#3")

readTools :: [Tool]
readTools =
  [ Tool "get-headline"
      "Read one headline subtree as it stands on disk, by row id."
      (schema [("id", str "the row id, e.g. FILE.org#3")] ["id"])
      (\tools args -> value =<< mtHeadline tools (fromMaybe "" (argText "id" args)))
  , Tool "list-headlines"
      "List headlines matching a query (the filter language the UI table uses)."
      (schema [ ("query", str "a filter query like state:*active* or tag:work; empty lists all")
              , ("limit", int "cap on the number of rows returned") ] [])
      (\tools args -> value =<< mtHeadlines tools (argText "query" args) (argInt "limit" args))
  ]

-- | A write tool: its arguments become a @\/command@ body, run through the
-- engine, its JSON answer wrapped back.
writeTool :: Text -> Text -> [(Text, Value)] -> [Text] -> Tool
writeTool name desc props required = Tool name desc (schema props required) run
  where run tools args = value =<< mtWrite tools (encode (asCommand name args))

-- | Arguments become @{name, id\/ids, args}@: @id@ and @ids@ ride the top
-- level the command engine names them at, everything else is the @args@ object.
asCommand :: Text -> Value -> Value
asCommand name (Object o) = object $
  ["name" .= name, "args" .= Object (KM.delete "id" (KM.delete "ids" o))]
    <> ["id"  .= v | Just v <- [KM.lookup "id" o]]
    <> ["ids" .= v | Just v <- [KM.lookup "ids" o]]
asCommand name _ = object ["name" .= name, "args" .= object []]


-- JSON-Schema builders and argument readers

schema :: [(Text, Value)] -> [Text] -> Value
schema props required = object
  [ "type" .= ("object" :: Text)
  , "properties" .= object [Key.fromText k .= v | (k, v) <- props]
  , "required" .= required
  ]

str, int, arrStr, pairs :: Text -> Value
str d = object ["type" .= ("string" :: Text), "description" .= d]
int d = object ["type" .= ("integer" :: Text), "description" .= d]
arrStr d = object
  ["type" .= ("array" :: Text), "items" .= str "", "description" .= d]
pairs d = object
  [ "type" .= ("array" :: Text), "description" .= d
  , "items" .= object ["type" .= ("array" :: Text), "items" .= str ""] ]

strOrNull :: Text -> Value
strOrNull d = object ["type" .= (["string", "null"] :: [Text]), "description" .= d]

span' :: Value
span' = object
  [ "type" .= ("array" :: Text), "description" .= ("[START, END] character offsets" :: Text)
  , "items" .= int "" ]

argText :: Text -> Value -> Maybe Text
argText k (Object o) = case KM.lookup (Key.fromText k) o of
  Just (String s) -> Just s
  _               -> Nothing
argText _ _ = Nothing

argInt :: Text -> Value -> Maybe Int
argInt k (Object o) = case KM.lookup (Key.fromText k) o of
  Just (Number n) -> Just (round n)
  _               -> Nothing
argInt _ _ = Nothing

argValue :: Text -> Value -> Value
argValue k (Object o) = fromMaybe (object []) (KM.lookup (Key.fromText k) o)
argValue _ _ = object []


-- | A handler's 'Response' as its status and its JSON body: MCP re-wraps what
-- @\/command@ and @\/headline@ already answer.  A non-JSON body is a bug the
-- error surfaces rather than hides.
value :: Response -> IO (Status, Value)
value res = do
  let (status, _headers, runStream) = responseToStream res
  ref <- newIORef mempty
  runStream $ \send -> send (\b -> modifyIORef' ref (<> b)) (pure ())
  raw <- B.toLazyByteString <$> readIORef ref
  pure (status, either (const (object ["error" .= ("non-JSON response" :: Text)])) id
                       (eitherDecode' raw))
