-- | The server, driven as a WAI 'Application'.  No socket is bound: every case
-- here is a request handed straight to the app, so the suite stays free of
-- ports and of the races that come with them.  The websocket route is the one
-- thing an upgrade-less request cannot reach, and the frames it would carry
-- are TestStore's subject.
module TestServe (spec) where

import Data.Aeson (Value (Number, Object, String), eitherDecode, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.List (sort)
import Network.HTTP.Types (HeaderName, methodDelete, methodPost, renderQuery, statusCode)
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SRequest (SRequest)
                        , SResponse (simpleBody, simpleHeaders, simpleStatus)
                        , request, runSession, setPath, srequest )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (orgFile, withTempDir)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Glance.Query ( HeadlineRecord (hrDigest, hrId), QueryResult (qrRecords)
                    , loadDir, loadFile, viewJSON )
import Glance.Web (ServeOptions (..), application, defaultPort, viewTitleFor)
import Glance.Web.Store (Hub, loadStore, newHub)

-- Fixtures

-- | The directory TestQuery loads: one sample document and one that is not
-- UTF-8, six headlines between them.
viewDir :: FilePath
viewDir = "test/fixtures/view"

-- | The document those six headlines come from.
sampleFile :: FilePath
sampleFile = viewDir <> "/sample.org"

-- | @sha256sum test\/fixtures\/view\/sample.org@.  Written down rather than
-- computed here: the digest the server hands out is what a client pins its
-- edit to, and an oracle that runs the same code as the server proves nothing
-- about it.
sampleDigest :: T.Text
sampleDigest = "0de46a0cceb1b1b30364c0bba0107e63bbd2c9b504d1e2bf31f29321f1ff2493"

-- | A file whose first headline is materialized, edited and written back.  The
-- id is in the drawer, so it stays the same across the temp directory's name
-- and across the edit.
committable :: T.Text
committable = T.unlines
  [ "#+CATEGORY: notes"
  , "* TODO First :one:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: first"
  , ":END:"
  , "body of first"
  , "* TODO Second"
  , "tail"
  ]

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

-- | A server over DIR: the app, and the hub whose store it answers from — the
-- write cases look at that store afterwards to show the route left it alone.
serverOver :: FilePath -> IO (Application, Hub)
serverOver dir = do
  hub <- newHub =<< loadStore dir
  pure (application (served assetsDir) { soDir = dir } hub, hub)

-- | GET PATH from a server configured with ASSETS.
get :: FilePath -> ByteString -> IO SResponse
get assets path = do
  application' <- app assets
  getFrom application' path

-- | GET PATH from APPLICATION'.
getFrom :: Application -> ByteString -> IO SResponse
getFrom application' path = runSession (request (setPath defaultRequest path)) application'

-- | POST PAYLOAD to PATH on APPLICATION', as JSON.
postTo :: Application -> ByteString -> BL.ByteString -> IO SResponse
postTo application' path payload = runSession (srequest (SRequest req payload)) application'
  where req = (setPath defaultRequest path)
                { requestMethod  = methodPost
                , requestHeaders = [("Content-Type", "application/json")] }

-- | @\/headline?id=…@ with ID percent-encoded, the way a client builds it: a
-- row id is @FILE:START@ and carries both separators the path would fight over.
headlinePath :: T.Text -> ByteString
headlinePath rid = "/headline" <> renderQuery True [("id", Just (TE.encodeUtf8 rid))]

-- | A commit body: the subtree text and the digest it was materialized with.
commitBody :: T.Text -> T.Text -> BL.ByteString
commitBody org digest = encode (object ["org" .= org, "digest" .= digest])

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

-- | V's value at KEY.
field :: T.Text -> Value -> IO Value
field k (Object o) = maybe (assertFailure ("missing key " <> show k))
                           pure (KM.lookup (Key.fromText k) o)
field k v = assertFailure ("expected an object with " <> show k <> ", got " <> show v)

-- | The string at KEY of V.
textAt :: T.Text -> Value -> IO T.Text
textAt k v = field k v >>= string
  where string (String t) = pure t
        string other = assertFailure ("expected a string at " <> show k
                                        <> ", got " <> show other)

-- | The number at KEY of V, as an offset.
intAt :: T.Text -> Value -> IO Int
intAt k v = field k v >>= number
  where number (Number n) = pure (round n)
        number other = assertFailure ("expected a number at " <> show k
                                        <> ", got " <> show other)

-- | PATH's text, decoded the way the loader decodes it.
document :: FilePath -> IO T.Text
document path = TE.decodeUtf8 <$> BS.readFile path

-- | PATH's digest as a fresh load computes it — what the next materialize of
-- that file would hand out.
digestOf :: FilePath -> IO T.Text
digestOf path = loadFile path >>= first'
  where first' (Right (r : _)) = pure (hrDigest r)
        first' other = assertFailure ("expected " <> path <> " to load with rows, got "
                                        <> show (fmap (map hrId) other))

-- Spec

spec :: TestTree
spec = testGroup "Serve"
  [ headlineSpec, statsSpec, materializeSpec, commitSpec, pageSpec, assetSpec, errorSpec ]

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
                                ["actions", "columns", "rows", "sort", "title"]
                                (sort (map Key.toText (KM.keys o)))
        _        -> assertFailure ("expected an object, got " <> show v)
  ]

-- | @GET \/headline@: one subtree out of the read model, with the coordinates
-- a write back to it needs.
materializeSpec :: TestTree
materializeSpec = testGroup "GET /headline"
  [ testCase "is the raw subtree, the file it came from and its digest" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a (headlinePath "ship-table-view")
      assertEqual "status" 200 (status r)
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      v <- decoded r
      org <- textAt "org" v
      rid <- textAt "id" v
      file <- textAt "file" v
      digest <- textAt "digest" v
      assertEqual "id" "ship-table-view" rid
      assertEqual "file" (T.pack sampleFile) file
      assertEqual "digest" sampleDigest digest
      assertEqual "org" (T.unlines [ "* NEXT [#A] Ship the table view :web:glance:"
                                   , "SCHEDULED: <2026-08-01 Sat 09:30> DEADLINE: <2026-08-05 Wed>"
                                   , ":PROPERTIES:"
                                   , ":ORG_GLANCE_ID: ship-table-view"
                                   , ":END:" ])
                  org

  , testCase "the org text is exactly the span the response reports" $ do
      (a, _hub) <- serverOver viewDir
      v <- getFrom a (headlinePath "ship-table-view") >>= decoded
      org <- textAt "org" v
      extent <- field "span" v
      start <- intAt "start" extent
      end <- intAt "end" extent
      doc <- document sampleFile
      assertEqual "slice" (T.take (end - start) (T.drop start doc)) org
      -- The extent starts at the stars, so the file's own header belongs to no
      -- subtree and a commit cannot take it with the headline.
      assertEqual "the preamble sits ahead of the first subtree"
                  "#+CATEGORY: sample\n#+TODO: NEXT WAITING | CANCELLED\n\n" (T.take start doc)

  , testCase "an id carrying a colon and slashes round-trips" $ do
      (a, _hub) <- serverOver viewDir
      let rid = T.pack sampleFile <> ":210"
      r <- getFrom a (headlinePath rid)
      assertEqual "status" 200 (status r)
      v <- decoded r
      back <- textAt "id" v
      org <- textAt "org" v
      assertEqual "id" rid back
      assertContains "subtree" "Привет мир" org

  , testCase "an id no row carries is a 404" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a (headlinePath "no-such-headline")
      assertEqual "status" 404 (status r)
      assertContains "hint" "no headline with id" (body r)

  , testCase "no id at all says what the route wants" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a "/headline"
      assertEqual "status" 400 (status r)
      assertContains "hint" "id=" (body r)
  ]

-- | @POST \/headline@: the subtree written back, and every way that is refused.
commitSpec :: TestTree
commitSpec = testGroup "POST /headline"
  [ testCase "writes the edited subtree and leaves the rest of the file alone" $
      withTempDir $ \dir -> do
        path <- orgFile dir "notes.org" committable
        (a, _hub) <- serverOver dir
        v <- getFrom a (headlinePath "first") >>= decoded
        org <- textAt "org" v
        digest <- textAt "digest" v
        before <- document path
        extent <- field "span" v
        start <- intAt "start" extent
        end <- intAt "end" extent
        let edited = T.replace "TODO First" "DONE First" org <> "an added line\n"
        r <- postTo a (headlinePath "first") (commitBody edited digest)
        assertEqual "status" 200 (status r)
        after <- document path
        assertEqual "the file is prefix + new subtree + suffix"
                    (T.take start before <> edited <> T.drop end before) after
        assertContains "the edit landed" "* DONE First" after
        assertContains "the next headline is untouched" "* TODO Second\ntail\n" after
        fresh <- textAt "digest" =<< decoded r
        expected <- digestOf path
        assertEqual "the reported digest is the file's" expected fresh

  , testCase "leaves the store alone — the watch is what updates rows" $
      withTempDir $ \dir -> do
        path <- orgFile dir "notes.org" committable
        (a, _hub) <- serverOver dir
        before <- decoded =<< getFrom a (headlinePath "first")
        org <- textAt "org" before
        digest <- textAt "digest" before
        r <- postTo a (headlinePath "first") (commitBody (org <> "a line\n") digest)
        assertEqual "status" 200 (status r)
        -- No watcher runs in this suite, so the store still holds the load it
        -- started with: the route wrote to the file and to nothing else.
        after <- decoded =<< getFrom a (headlinePath "first")
        assertEqual "the store's subtree" (Just org) . Just =<< textAt "org" after
        assertEqual "the store's digest" (Just digest) . Just =<< textAt "digest" after
        onDisk <- digestOf path
        assertBool "the file did not move" (onDisk /= digest)

  , testCase "a file rewritten behind the client is a conflict, and stays as it is" $
      withTempDir $ \dir -> do
        path <- orgFile dir "notes.org" committable
        (a, _hub) <- serverOver dir
        v <- getFrom a (headlinePath "first") >>= decoded
        org <- textAt "org" v
        digest <- textAt "digest" v
        let meddled = committable <> "* TODO Someone else\n"
        _ <- orgFile dir "notes.org" meddled
        r <- postTo a (headlinePath "first") (commitBody (org <> "mine\n") digest)
        assertEqual "status" 409 (status r)
        conflict <- decoded r
        reason <- textAt "reason" conflict
        assertEqual "reason" "drift" reason
        assertContains "the message says to materialize again" "materialize" (body r)
        after <- document path
        assertEqual "the file is the meddler's" meddled after

  , testCase "a digest the store no longer holds is a conflict too" $
      withTempDir $ \dir -> do
        path <- orgFile dir "notes.org" committable
        (a, _hub) <- serverOver dir
        v <- getFrom a (headlinePath "first") >>= decoded
        org <- textAt "org" v
        let stale = T.replicate 64 "0"
        r <- postTo a (headlinePath "first") (commitBody org stale)
        assertEqual "status" 409 (status r)
        conflict <- decoded r
        reason <- textAt "reason" conflict
        current <- textAt "digest" conflict
        assertEqual "reason" "stale" reason
        assertEqual "the digest to re-materialize with" (Just current) . Just
          =<< textAt "digest" v
        after <- document path
        assertEqual "untouched" committable after

  , testCase "a body that is not the two fields is a 400" $ withTempDir $ \dir -> do
      _ <- orgFile dir "notes.org" committable
      (a, _hub) <- serverOver dir
      broken <- postTo a (headlinePath "first") "{not json"
      missing <- postTo a (headlinePath "first") (encode (object ["org" .= ("x" :: T.Text)]))
      assertEqual "malformed" 400 (status broken)
      assertEqual "incomplete" 400 (status missing)
      assertContains "says which" "digest" (body missing)

  , testCase "a body over the cap is refused before it is read" $ withTempDir $ \dir -> do
      _ <- orgFile dir "notes.org" committable
      (a, _hub) <- serverOver dir
      let huge = BL.fromStrict (BS.replicate (1024 * 1024 + 1) 0x78)
      r <- postTo a (headlinePath "first") huge
      assertEqual "status" 413 (status r)
      assertContains "the cap" "body over" (body r)

  , testCase "an id no row carries is a 404, and no id a 400" $ withTempDir $ \dir -> do
      _ <- orgFile dir "notes.org" committable
      (a, _hub) <- serverOver dir
      unknown <- postTo a (headlinePath "no-such-headline") (commitBody "* x\n" "d")
      anonymous <- postTo a "/headline" (commitBody "* x\n" "d")
      assertEqual "unknown id" 404 (status unknown)
      assertEqual "no id" 400 (status anonymous)
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

  , testCase "with assets, materializes a row and posts it back" $ do
      r <- get assetsDir "/"
      mapM_ (\needle -> assertContains "materialize glue" needle (body r))
            [ "\"materialize\"", "/headline?id=${encodeURIComponent(", "<textarea id=\"mtext\""
            , "method: \"POST\"", "digest: editing.digest", "a.status === 409"
            , "File changed since materialize", "Re-materialize", "\"Escape\"" ]

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

  , testCase "a write to a read route is refused" $ do
      let req = (setPath defaultRequest "/headlines") { requestMethod = methodPost }
      application' <- app assetsDir
      r <- runSession (request req) application'
      assertEqual "status" 405 (status r)
      assertContains "hint" "/headline" (body r)

  , testCase "/headline takes GET and POST, and nothing else" $ do
      let req = (setPath defaultRequest "/headline") { requestMethod = methodDelete }
      application' <- app assetsDir
      r <- runSession (request req) application'
      assertEqual "status" 405 (status r)

  , testCase "/ws without an upgrade says what it wants" $ do
      r <- get assetsDir "/ws"
      assertEqual "status" 400 (status r)
      assertContains "hint" "websocket" (body r)
  ]
