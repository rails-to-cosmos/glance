-- | One door onto the HTTP surface, for the two suites that drive it.
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

serverWith :: ServeOptions -> IO (Application, Hub)
serverWith opts = do
  hub <- newHub =<< loadStore (soDir opts)
  pure (application opts hub, hub)

postTo :: Application -> ByteString -> BL.ByteString -> IO SResponse
postTo app path payload = runSession (srequest (SRequest req payload)) app
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
