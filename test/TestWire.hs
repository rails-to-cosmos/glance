-- | One door onto the HTTP surface, for the suites that drive it: the apps a
-- case boots, the two verbs, and the readings every wire assertion goes through.
module TestWire ( app, appOf, assertOk, assetsDir, body, builtIn, capture
                , command, commitBody, decoded, drainNow, etagOf, getFrom
                , getWith, header, headlinePath, keywordArg, loadingApp, ok
                , postTo, served, serverAt, serverWith, status, withCommittedAt
                , withTreeOf ) where

import Control.Monad (void)
import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( HeaderName, RequestHeaders, methodPost, renderQuery
                          , statusCode )
import Network.Wai ( Application, defaultRequest, requestHeaders, requestMethod )
import Network.Wai.Test ( SRequest (SRequest)
                        , SResponse (simpleBody, simpleHeaders, simpleStatus)
                        , request, runSession, setPath, srequest )
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

import Glance.Query (defaultWalk)
import Glance.Web (ServeOptions (..), application, defaultPort, soDir)
import Glance.Web.Store (Hub, loadStore, newHub, newLoadingHub)
import Glance.Web.Watch (drain)
import TestDefaults (committable, orgFile, viewDir, withTempDir)

serverWith :: ServeOptions -> IO (Application, Hub)
serverWith opts = do
  hub <- newHub =<< loadStore (soDir opts)
  pure (application opts hub, hub)

postTo :: Application -> ByteString -> BL.ByteString -> IO SResponse
postTo app' path payload = runSession (srequest (SRequest req payload)) app'
  where req = (setPath defaultRequest path)
                { requestMethod  = methodPost
                , requestHeaders = [("Content-Type", "application/json")] }

status :: SResponse -> Int
status = statusCode . simpleStatus

-- | A server over DIR with ASSETS ('Nothing' for the compiled-in renderer).
serverAt :: Maybe FilePath -> FilePath -> IO (Application, Hub)
serverAt assets dir = serverWith (ServeOptions dir defaultPort assets False)

ok :: SResponse -> IO SResponse
ok r = r <$ assertEqual "status" 200 (status r)

assertOk :: SResponse -> Assertion
assertOk = void . ok

-- | The renderer the fixture tree is served with.
assetsDir :: FilePath
assetsDir = "test/fixtures/assets"

-- | The options a server over the suite's fixture tree runs, compiled-in renderer.
builtIn :: ServeOptions
builtIn = ServeOptions { soDir = viewDir, soPort = defaultPort, soAssets = Nothing
                       , soDerived = False }

-- | The same, serving ASSETS instead.
served :: FilePath -> ServeOptions
served assets = builtIn { soAssets = Just assets }

-- | The app OPTS runs, over a store loaded from the directory OPTS names.
appOf :: ServeOptions -> IO Application
appOf opts = application opts <$> (newHub =<< loadStore (soDir opts))

app :: FilePath -> IO Application
app assets = appOf (served assets)

-- | The same app with the startup walk still running.
loadingApp :: IO Application
loadingApp = application (served assetsDir) <$> (newLoadingHub =<< getMonotonicTime)

getFrom :: Application -> ByteString -> IO SResponse
getFrom app' path = getWith app' path []

getWith :: Application -> ByteString -> RequestHeaders -> IO SResponse
getWith app' path headers =
  runSession (request (setPath defaultRequest path) { requestHeaders = headers }) app'

header :: HeaderName -> SResponse -> Maybe ByteString
header name r = lookup name (simpleHeaders r)

body :: SResponse -> Text
body = TE.decodeUtf8 . BL.toStrict . simpleBody

etagOf :: SResponse -> IO ByteString
etagOf r = maybe (assertFailure "no ETag on the response") pure (header "ETag" r)

-- | @\/headline?id=…@ percent-encoded: a row id is @FILE#K@, slashes and hash included.
headlinePath :: Text -> ByteString
headlinePath rid = "/headline" <> renderQuery True [("id", Just (TE.encodeUtf8 rid))]

commitBody :: Text -> Text -> BL.ByteString
commitBody org digest = encode (object ["org" .= org, "digest" .= digest])

decoded :: SResponse -> IO Value
decoded r = either (\e -> assertFailure ("response JSON: " <> e)) pure
                   (eitherDecode (simpleBody r))

-- | A server over a fresh tree holding FILES, handed to K with its hub and the
-- directory.  The one opener the on-disk cases boot through; a case wanting a
-- file's path spells it @dir \<\/\> name@.
withTreeOf :: [(FilePath, Text)] -> (Application -> Hub -> FilePath -> IO a) -> IO a
withTreeOf files k = withTempDir $ \dir -> do
  mapM_ (uncurry (orgFile dir)) files
  (a, hub) <- serverAt (Just assetsDir) dir
  k a hub dir

-- | A server over 'committable' with its first headline materialized, handed to
-- K with the hub, the file on disk and the answer.
withCommittedAt :: (Application -> Hub -> FilePath -> Value -> Assertion) -> Assertion
withCommittedAt k = withTempDir $ \dir -> do
  path <- orgFile dir "notes.org" committable
  (a, hub) <- serverAt (Just assetsDir) dir
  v <- decoded =<< getFrom a (headlinePath "first")
  k a hub path v

command :: Text -> [Text] -> Value -> BL.ByteString
command name ids args = encode (object ["name" .= name, "ids" .= ids, "args" .= args])

keywordArg :: Maybe Text -> Value
keywordArg keyword = object ["keyword" .= keyword]

capture :: Text -> BL.ByteString
capture text = encode (object [ "name" .= ("capture" :: Text)
                              , "args" .= object ["text" .= text] ])

-- | One turn of the drain loop over DIR's HUB, debounce zeroed so all is ripe.
drainNow :: FilePath -> Hub -> IO ()
drainNow = drain defaultWalk (0 :: Double)
