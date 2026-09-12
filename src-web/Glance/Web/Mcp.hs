-- | @POST \/mcp@: the command engine and read views as an MCP tool catalog over
-- JSON-RPC 2.0.  ONE CORE, injected transports — a tool re-wraps the live
-- @\/command@ and @\/headline@ handlers ('McpTools'), never a second write path.
-- The write tools ARE 'commandNames', pinned by the suite.
module Glance.Web.Mcp
  ( Arg
  , McpTools (..)
  , mcpRoute
  , runMcpStdio
  , runMcpStdioWith
  , mcpDaemonAt
  , mcpWriteToolNames
  ) where

import Control.Exception (try)
import Control.Monad (unless)
import Data.Aeson ( FromJSON (parseJSON), Result (Error, Success)
                  , Value (Bool, Null, Number, Object, String), decode, encode
                  , eitherDecode', fromJSON, object, withObject, (.:), (.=) )
import Data.Char (isSpace)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Types (Status, hContentType, status200, status202, status400, statusCode)
import Network.Wai (Request, Response, responseToStream)
import System.Directory (canonicalizePath)
import System.IO (BufferMode (LineBuffering), hSetBuffering, isEOF, stdin, stdout)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC

import Glance.Query (neighborDepth, neighborDepthCap)
import Glance.Web.Base (jsonType, jsonValue, sized, withBody)


-- | One tool argument as the query bytes it was rendered to, 'Nothing' for one
-- that was not given.  'argBytes' is the rendering, and the route's own readers
-- ('Glance.Web.Routes.wholeNumber' and its kin) are the walls it then meets.
type Arg = Maybe BS.ByteString

-- | The handlers the door dispatches to, wired to the live Hub by the caller.
data McpTools = McpTools
  { mtWrite     :: BL.ByteString -> IO Response         -- ^ a @\/command@ body.
  , mtHeadline  :: Text -> Arg -> IO Response           -- ^ one subtree by row id, @edges@ as given.
  , mtHeadlines :: Arg -> Arg -> Arg -> IO Response     -- ^ a query, a cap and @edges@.
  , mtNeighbors :: Text -> Arg -> Arg -> Arg -> IO Response
      -- ^ the subgraph around a row: id, then depth, cap and the kind it walks.
  , mtDoctor    :: IO Response                          -- ^ the startup health verdict.
  }

-- | @POST \/mcp@: one JSON-RPC message in, one out; a notification gets an empty 202.
mcpRoute :: McpTools -> Request -> IO Response
mcpRoute tools request = withBody request $ \raw ->
  maybe (sized status202 [jsonType] "") (jsonRpc status200) <$> mcpHandle tools raw

-- | One JSON-RPC message to its response, the core both transports share:
-- 'Nothing' for a notification (no reply owed), 'Just' otherwise.
mcpHandle :: McpTools -> BL.ByteString -> IO (Maybe Value)
mcpHandle tools raw = case eitherDecode' raw of
  Left _ -> pure (Just (rpcError Nothing (-32700) "parse error: expected a JSON-RPC message"))
  Right v -> case message v of
    Left why -> pure (Just (rpcError Nothing (-32600) why))
    Right (Nothing, _method, _params) -> pure Nothing
    Right (Just rid, method, params) -> Just <$> answer tools rid method params

-- | @glance mcp@'s transport: the MCP stdio contract (newline-delimited JSON-RPC over stdin\/stdout), dispatched locally.
runMcpStdio :: McpTools -> IO ()
runMcpStdio tools = runMcpStdioWith (fmap (fmap encode) . mcpHandle tools)

-- | The stdio transport over ANY per-message handler: a message a line, its
-- response a line ('Nothing' writes nothing), blank lines skipped, ends at EOF.
runMcpStdioWith :: (BL.ByteString -> IO (Maybe BL.ByteString)) -> IO ()
runMcpStdioWith handle = hSetBuffering stdout LineBuffering >> loop
  where
    loop = do
      eof <- isEOF
      unless eof $ do
        line <- BSC.hGetLine stdin
        unless (BSC.all isSpace line) $ do
          resp <- handle (BL.fromStrict line)
          mapM_ (\b -> BL.hPut stdout b >> BS.hPut stdout "\n") resp
        loop

data StatusInfo = StatusInfo { siReady :: !Bool, siDir :: !FilePath }

instance FromJSON StatusInfo where
  parseJSON = withObject "status" $ \o -> StatusInfo <$> o .: "ready" <*> o .: "dir"

-- | A READY glance daemon on PORT already owning DIR (per its @\/status@) yields
-- a forwarder to that daemon's @\/mcp@ — one live store, no second walk, no stale
-- view; 'Nothing' when there's none, and the caller boots offline.  Loopback only.
mcpDaemonAt :: Int -> FilePath -> IO (Maybe (BL.ByteString -> IO (Maybe BL.ByteString)))
mcpDaemonAt port dir = do
  want <- canonicalizePath dir
  mgr <- HC.newManager HC.defaultManagerSettings
  probe <- try (HC.parseRequest (base <> "/status") >>= flip HC.httpLbs mgr)
             :: IO (Either HC.HttpException (HC.Response BL.ByteString))
  pure $ case probe of
    Right resp
      | statusCode (HC.responseStatus resp) == 200
      , Just si <- decode (HC.responseBody resp)
      , siReady si, siDir si == want -> Just (forward mgr)
    _noOwner -> Nothing
  where
    base = "http://127.0.0.1:" <> show port
    -- An empty body is the daemon's empty-202 to a notification: the stdio loop's "no reply".
    forward mgr raw = do
      req0 <- HC.parseRequest (base <> "/mcp")
      let req = req0 { HC.method = "POST"
                     , HC.requestHeaders = [(hContentType, "application/json")]
                     , HC.requestBody = HC.RequestBodyLBS raw }
      body <- HC.responseBody <$> HC.httpLbs req mgr
      pure (if BL.null body then Nothing else Just body)

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

-- | An absent id ('Nothing') echoes as JSON @null@, per the spec when the id could not be read.
rpcError :: Maybe Value -> Int -> Text -> Value
rpcError rid code msg = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= rid
  , "error" .= object ["code" .= code, "message" .= msg]
  ]

-- | The @initialize@ result: echo the client's protocol version, else glance's own.
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

-- | A tool answer as MCP content: the handler's JSON as text, @isError@ off its
-- HTTP status (a per-row refusal is a 200, not flagged).
content :: Status -> Value -> Value
content status val = object
  [ "content" .= [ object ["type" .= ("text" :: Text), "text" .= jsonValue val] ]
  , "isError" .= (statusCode status >= 400)
  ]


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

-- | The write tools, one per command verb; the suite pins these to 'Glance.Web.Commands.commandNames'.
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
  , writeTool "add-link"
      "Link one headline to another: append [[glance:TARGET?kind=KIND][DESC]] under it.\
      \ The reverse edge needs no write -- list-headlines with edges reads it back."
      [idProp, ("target", str "the row id this one points at")
      , ("kind", str "the edge's own kind, e.g. blocked-by; omit for a plain mention")
      , ("desc", str "the link's description; omit for a bare link")
      , ("where", str "body (the default) or title")]
      ["id", "target"]
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
      (schema [("id", str "the row id, e.g. FILE.org#3"), ("edges", edgesProp)] ["id"])
      (\tools args -> reading (mtHeadline tools (fromMaybe "" (argText "id" args))
                                 <$> argBytes "edges" args))
  , Tool "list-headlines"
      "List headlines matching a query (the filter language the UI table uses). Answers\
      \ {total, clean, rows}: total is the uncapped match count, clean the index's\
      \ health flag, and each row {id, title, state, priority, scheduled, deadline, tags}."
      (schema [ ("query", str "a filter query like state:*active* or tag:work; empty lists all")
              , ("limit", int "cap on the number of rows returned")
              , ("edges", edgesProp) ] [])
      (\tools args -> reading (mtHeadlines tools <$> argBytes "query" args
                                                 <*> argBytes "limit" args
                                                 <*> argBytes "edges" args))
  , Tool "neighbors"
      "Walk the reference graph around one headline: the rows within depth hops of it,\
      \ either direction, as {total, nodes: [a row each], edges: [{from, to, kind}]}.\
      \ One call instead of a ref:/from: query per hop."
      (schema [ ("id", str "the row to walk out from")
              , ("depth", int ("hops to take, " <> count neighborDepth <> " (the default) to "
                                 <> count neighborDepthCap))
              , ("limit", int "cap on the number of nodes returned")
              , ("kind", str "walk only edges carrying this kind, e.g. blocked-by") ]
              ["id"])
      (\tools args -> reading (mtNeighbors tools (fromMaybe "" (argText "id" args))
                                 <$> argBytes "depth" args
                                 <*> argBytes "limit" args
                                 <*> argBytes "kind" args))
  , Tool "doctor"
      "The index's health as measured at startup: the clean flag, one sentence per\
      \ finding, and the counts (parse and decode and read failures, span violations,\
      \ id collisions, org-glance drift, unindexed and recordless rows)."
      (schema [] [])
      (\tools _args -> value =<< mtDoctor tools)
  ]

-- | A write tool: its arguments become a @\/command@ body, run and wrapped back.
writeTool :: Text -> Text -> [(Text, Value)] -> [Text] -> Tool
writeTool name desc props required = Tool name desc (schema props required) run
  where run tools args = value =<< mtWrite tools (encode (asCommand name args))

-- | Arguments become @{name, id\/ids, args}@: @id@ and @ids@ ride the top level the command engine names them at, the rest is @args@.
asCommand :: Text -> Value -> Value
asCommand name (Object o) = object $
  ["name" .= name, "args" .= Object (KM.delete "id" (KM.delete "ids" o))]
    <> ["id"  .= v | Just v <- [KM.lookup "id" o]]
    <> ["ids" .= v | Just v <- [KM.lookup "ids" o]]
asCommand name _ = object ["name" .= name, "args" .= object []]


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

-- | The @edges@ argument both read tools take.  WITHOUT IT the answer is the one
-- they always gave, byte for byte.
edgesProp :: Value
edgesProp = object
  [ "type" .= ("boolean" :: Text)
  , "description" .= ("also answer each row's refs (what it points at) and referrers\
                      \ (what points at it)" :: Text) ]

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

-- | An argument as the query bytes the door reads it from: a string as itself, a
-- number in its own decimal spelling, @true@ for a flag that is set.  A flag that
-- is off, a @null@ and an absent argument are alike NO PARAMETER, the absence the
-- query string spells with; anything else is refused naming the argument.  THE
-- TOOL RENDERS AND THE ROUTE READS, so neither door grows a second vocabulary.
argBytes :: Text -> Value -> Either Text Arg
argBytes k (Object o) = case KM.lookup (Key.fromText k) o of
  Nothing         -> Right Nothing
  Just Null       -> Right Nothing
  Just (Bool b)   -> Right (if b then Just "true" else Nothing)
  Just (String s) -> Right (Just (TE.encodeUtf8 s))
  Just n@Number{} -> Right (Just (spelled n))
  Just _other     -> Left (k <> " is a string, a number, or true or false")
argBytes _ _ = Right Nothing

-- | A number as the query string spells it: a whole one without the @.0@ JSON
-- would carry, any other as written.
spelled :: Value -> BS.ByteString
spelled v = case fromJSON v :: Result Integer of
  Success n -> BSC.pack (show n)
  Error _   -> BL.toStrict (encode v)

-- | A read tool's answer, or the 400 an argument the door refuses earns.
reading :: Either Text (IO Response) -> IO (Status, Value)
reading = either refused (value =<<)

-- | An argument the door refuses, answered as the 400 a query string would get.
refused :: Text -> IO (Status, Value)
refused why = pure (status400, object ["error" .= why])

-- | A wall's own number, spelled into the schema text rather than beside it.
count :: Int -> Text
count = T.pack . show

argValue :: Text -> Value -> Value
argValue k (Object o) = fromMaybe (object []) (KM.lookup (Key.fromText k) o)
argValue _ _ = object []


-- | A handler's 'Response' as its status and JSON body; a non-JSON body becomes an error value.
value :: Response -> IO (Status, Value)
value res = do
  let (status, _headers, runStream) = responseToStream res
  ref <- newIORef mempty
  runStream $ \send -> send (\b -> modifyIORef' ref (<> b)) (pure ())
  raw <- B.toLazyByteString <$> readIORef ref
  pure (status, either (const (object ["error" .= ("non-JSON response" :: Text)])) id
                       (eitherDecode' raw))
