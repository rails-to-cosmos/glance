-- | The server, driven as a WAI 'Application'.  No socket is bound: every case
-- here is a request handed straight to the app, so the suite stays free of
-- ports and of the races that come with them.  The websocket route is the one
-- thing an upgrade-less request cannot reach, and the frames it would carry
-- are TestStore's subject.
module TestServe (spec) where

import Data.Aeson (Value (Object), eitherDecode)
import Data.ByteString (ByteString)
import Data.List (sort)
import Network.HTTP.Types (HeaderName, methodPost, statusCode)
import Network.Wai (Application, defaultRequest, requestMethod)
import Network.Wai.Test ( SResponse (simpleBody, simpleHeaders, simpleStatus)
                        , request, runSession, setPath )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Glance.Query (QueryResult (qrRecords), loadDir, viewJSON)
import Glance.Web (ServeOptions (..), application, defaultPort, viewTitleFor)
import Glance.Web.Store (loadStore, newHub)

-- Fixtures

-- | The directory TestQuery loads: one sample document and one that is not
-- UTF-8, six headlines between them.
viewDir :: FilePath
viewDir = "test/fixtures/view"

-- | An assets directory holding a stub renderer.
assetsDir :: FilePath
assetsDir = "test/fixtures/assets"

-- | A path with no renderer under it, and no directory either.
missingAssetsDir :: FilePath
missingAssetsDir = "test/fixtures/assets-not-here"

served :: FilePath -> ServeOptions
served assets = ServeOptions { soDir = viewDir, soPort = defaultPort, soAssets = assets }

-- | The app a server with ASSETS runs, over a store loaded the way 'serve'
-- loads one.  A fresh store per request is the suite's convenience; the server
-- keeps one for its lifetime.
app :: FilePath -> IO Application
app assets = application (served assets) <$> (newHub =<< loadStore viewDir)

-- | GET PATH from a server configured with ASSETS.
get :: FilePath -> ByteString -> IO SResponse
get assets path = do
  application' <- app assets
  runSession (request (setPath defaultRequest path)) application'

-- Assertions

status :: SResponse -> Int
status = statusCode . simpleStatus

header :: HeaderName -> SResponse -> Maybe ByteString
header name r = lookup name (simpleHeaders r)

body :: SResponse -> T.Text
body = TE.decodeUtf8 . BL.toStrict . simpleBody

assertContains :: String -> T.Text -> T.Text -> Assertion
assertContains what needle haystack =
  assertBool (what <> ": no " <> show needle <> " in " <> show (T.take 400 haystack))
             (needle `T.isInfixOf` haystack)

-- | R's body as a JSON 'Value', or the decode error as a test failure.
decoded :: SResponse -> IO Value
decoded r = either (\e -> assertFailure ("response JSON: " <> e)) pure
                   (eitherDecode (simpleBody r))

-- Spec

spec :: TestTree
spec = testGroup "Serve" [headlineSpec, statsSpec, pageSpec, assetSpec, errorSpec]

-- | @\/headlines@ is the facade's view document — the same 'Value' 'viewJSON'
-- builds from the same directory, so the server adds nothing to the wire.
headlineSpec :: TestTree
headlineSpec = testGroup "GET /headlines"
  [ testCase "is the view JSON for the served directory, rendered from the store" $ do
      r <- get assetsDir "/headlines"
      expected <- viewJSON (viewTitleFor viewDir) . qrRecords <$> loadDir viewDir
      assertEqual "status" 200 (status r)
      got <- decoded r
      assertEqual "view" expected got

  , testCase "is UTF-8 JSON, and says so" $ do
      r <- get assetsDir "/headlines"
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      assertContains "unicode cell" "Привет мир" (body r)

  , testCase "carries one row per headline" $ do
      r <- get assetsDir "/headlines"
      rows <- length . qrRecords <$> loadDir viewDir
      assertEqual "fixture rows" 6 rows
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)

  , testCase "serves JSON with the assets directory missing" $ do
      r <- get missingAssetsDir "/headlines"
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)
  ]

-- | The load counts ride in headers; the body stays SCHEMA.md's field set.
statsSpec :: TestTree
statsSpec = testGroup "Load stats"
  [ testCase "report what the walk covered" $ do
      r <- get assetsDir "/headlines"
      assertEqual "files" (Just "2") (header "X-Glance-Files" r)
      assertEqual "parse failures" (Just "0") (header "X-Glance-Parse-Failures" r)
      assertEqual "decode failures" (Just "1") (header "X-Glance-Decode-Failures" r)
      assertEqual "read failures" (Just "0") (header "X-Glance-Read-Failures" r)

  , testCase "leave the view document's field set alone" $ do
      v <- get assetsDir "/headlines" >>= decoded
      case v of
        Object o -> assertEqual "top-level keys"
                                ["columns", "rows", "sort", "title"]
                                (sort (map Key.toText (KM.keys o)))
        _        -> assertFailure ("expected an object, got " <> show v)
  ]

-- | @\/@ in both modes: a shell that mounts the renderer, and a page that
-- explains where the renderer went.
pageSpec :: TestTree
pageSpec = testGroup "GET /"
  [ testCase "with assets, is a shell that fetches and mounts" $ do
      r <- get assetsDir "/"
      assertEqual "status" 200 (status r)
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "renderer" "src=\"table-view.js\"" (body r)
      assertContains "fetch glue" "fetch(\"/headlines\")" (body r)
      assertContains "mount" "TableView.mount(" (body r)

  , testCase "with assets, opens a socket and applies the streaming ops" $ do
      r <- get assetsDir "/"
      mapM_ (\needle -> assertContains "live glue" needle (body r))
            [ "new WebSocket(", "/ws", "\"set-rows\"", "table.setRows("
            , "\"upsert-row\"", "table.upsertRow(", "\"delete-row\"", "table.deleteRow(" ]

  , testCase "with assets, re-fetches and remounts after a close" $ do
      r <- get assetsDir "/"
      mapM_ (\needle -> assertContains "reconnect glue" needle (body r))
            [ "socket.onclose", "setTimeout(start,", "Math.min(backoff * 2, 30000)" ]

  , testCase "without assets, explains JSON-only mode" $ do
      r <- get missingAssetsDir "/"
      assertEqual "status" 200 (status r)
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "mode" "JSON-only" (body r)
      assertContains "flag" "--assets" (body r)
      assertContains "endpoint" "/headlines" (body r)
      assertBool "mounts a renderer it has not got"
                 (not ("TableView.mount(" `T.isInfixOf` body r))
  ]

-- | Assets come out of the configured directory, and only from there.
assetSpec :: TestTree
assetSpec = testGroup "Assets"
  [ testCase "the renderer is served as JavaScript" $ do
      r <- get assetsDir "/table-view.js"
      assertEqual "status" 200 (status r)
      assertEqual "content type"
                  (Just "text/javascript; charset=utf-8") (header "Content-Type" r)

  , testCase "a file the assets directory lacks is a 404" $ do
      r <- get assetsDir "/table-view.css"
      assertEqual "status" 404 (status r)

  , testCase "a traversal segment is not a file name" $ do
      r <- get assetsDir "/.."
      assertEqual "status" 404 (status r)
  ]

errorSpec :: TestTree
errorSpec = testGroup "Errors"
  [ testCase "an unknown path is a 404" $ do
      r <- get assetsDir "/graph"
      assertEqual "status" 404 (status r)
      assertContains "hint" "/headlines" (body r)

  , testCase "a write method is refused until the command tier exists" $ do
      let req = (setPath defaultRequest "/headlines") { requestMethod = methodPost }
      application' <- app assetsDir
      r <- runSession (request req) application'
      assertEqual "status" 405 (status r)

  , testCase "/ws without an upgrade says what it wants" $ do
      r <- get assetsDir "/ws"
      assertEqual "status" 400 (status r)
      assertContains "hint" "websocket" (body r)
  ]
