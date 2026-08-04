-- | One door onto the HTTP surface, for the two suites that drive it.
--
-- TestServe drives every route and TestExternal drives the ones that write, so
-- both stand a server up and both POST JSON at it.  Each spelled its own
-- version of these three; here they are once, and a request built here is the
-- request both suites send.  No socket is bound: the 'Application' is handed
-- requests directly, so the suite stays free of ports.
module TestWire (postTo, serverWith, status) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (methodPost, statusCode)
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SRequest (SRequest), SResponse (simpleStatus)
                        , runSession, setPath, srequest )

import qualified Data.ByteString.Lazy as BL

import Glance.Web (ServeOptions, application, soDir)
import Glance.Web.Store (Hub, loadStore, newHub)

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
