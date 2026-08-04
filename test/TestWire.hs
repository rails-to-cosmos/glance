-- | One door onto the HTTP surface, for the two suites that drive it.
--
-- TestServe drives every route and TestExternal drives the ones that write, so
-- both stand a server up and both POST JSON at it.  Each spelled its own
-- version of these three; here they are once, and a request built here is the
-- request both suites send.  No socket is bound: the 'Application' is handed
-- requests directly, so the suite stays free of ports.
module TestWire ( assertOk, capture, command, drainNow, keywordArg, ok, postTo
                , serverAt, serverWith, status ) where

import Control.Monad (void)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Types (methodPost, statusCode)
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SRequest (SRequest), SResponse (simpleStatus)
                        , runSession, setPath, srequest )
import Test.Tasty.HUnit (Assertion, assertEqual)

import qualified Data.ByteString.Lazy as BL

import Glance.Query (defaultWalk)
import Glance.Web (ServeOptions (..), application, defaultPort, soDir)
import Glance.Web.Store (Hub, loadStore, newHub)
import Glance.Web.Watch (drain)

-- | The app OPTS describes over a store loaded from its own directory, and the
-- hub that store lives in.  A caller that writes looks at the hub afterwards to
-- show the route left the store alone.
serverWith :: ServeOptions -> IO (Application, Hub)
serverWith opts = do
  hub <- newHub =<< loadStore (soDir opts)
  pure (application opts hub, hub)

-- | POST PAYLOAD to PATH on APP, as JSON.
postTo :: Application -> ByteString -> BL.ByteString -> IO SResponse
postTo app path payload = runSession (srequest (SRequest req payload)) app
  where req = (setPath defaultRequest path)
                { requestMethod  = methodPost
                , requestHeaders = [("Content-Type", "application/json")] }

-- | R's status code.
status :: SResponse -> Int
status = statusCode . simpleStatus

-- | A server over DIR with ASSETS ('Nothing' for a build serving the renderer
-- it was compiled with), and the hub its store lives in.  'serverWith' with the
-- three fields nothing here varies filled in.
serverAt :: Maybe FilePath -> FilePath -> IO (Application, Hub)
serverAt assets dir = serverWith (ServeOptions dir defaultPort assets False)

-- | R once its status is 200.  Written @r \<- ok =\<\< getFrom …@, which is the
-- two lines every reader of a good answer opened with as one.
ok :: SResponse -> IO SResponse
ok r = r <$ assertEqual "status" 200 (status r)

-- | 'ok' where the answer itself is never read: the request landed, and what it
-- wrote is asserted off the file or the next read.
assertOk :: SResponse -> Assertion
assertOk = void . ok

-- | A command as the shell sends one.
command :: Text -> [Text] -> Value -> BL.ByteString
command name ids args = encode (object ["name" .= name, "ids" .= ids, "args" .= args])

-- | @set-state@'s argument, a keyword or the null that clears one.
keywordArg :: Maybe Text -> Value
keywordArg keyword = object ["keyword" .= keyword]

-- | A capture as the shell sends one: no ids at all, and one line of org.
capture :: Text -> BL.ByteString
capture text = encode (object [ "name" .= ("capture" :: Text)
                              , "args" .= object ["text" .= text] ])

-- | One turn of the drain loop over DIR's HUB with the debounce out of the way:
-- everything queued is ripe, which is what a test wants and what the 25 ms poll
-- would otherwise make it sleep for.
drainNow :: FilePath -> Hub -> IO ()
drainNow = drain defaultWalk (0 :: Double)
