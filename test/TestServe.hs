-- | The server, driven as a WAI 'Application'.  No socket is bound: every case
-- here is a request handed straight to the app, so the suite stays free of
-- ports and of the races that come with them.  The websocket route is the one
-- thing an upgrade-less request cannot reach, and the frames it would carry
-- are TestStore's subject.
module TestServe (spec) where

import Data.Aeson ( Value (Null, Number, Object, String)
                  , eitherDecode, encode, object, parseJSON, (.=) )
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.List (find, nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types ( HeaderName, RequestHeaders, methodDelete, methodPost
                          , renderQuery, statusCode )
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SRequest (SRequest)
                        , SResponse (simpleBody, simpleHeaders, simpleStatus)
                        , request, runSession, setPath, srequest )
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (orgFile, withTempDir)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Glance.Query ( HeadlineRecord (hrDigest, hrId), QueryResult (qrRecords)
                    , loadDir, loadFile, viewJSON )
import Glance.Web ( ServeOptions (..), application, bootstrapWanted, defaultPort
                  , viewTitleFor )
import Glance.Web.Store (Hub, applyFile, loadStore, newHub, publish)

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
getFrom application' path = getWith application' path []

-- | GET PATH from APPLICATION', sending HEADERS — the conditional and the
-- content-negotiation cases are all one request header apart.
getWith :: Application -> ByteString -> RequestHeaders -> IO SResponse
getWith application' path headers =
  runSession (request (setPath defaultRequest path) { requestHeaders = headers }) application'

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

-- | The string at KEY of V, where the key is there and its value may be null.
maybeTextAt :: T.Text -> Value -> IO (Maybe T.Text)
maybeTextAt k v = field k v >>= string
  where string Null = pure Nothing
        string (String t) = pure (Just t)
        string other = assertFailure ("expected a string or null at " <> show k
                                        <> ", got " <> show other)

-- | The array at KEY of V.
listAt :: T.Text -> Value -> IO [Value]
listAt k v = field k v >>= read'
  where read' x = either (\e -> assertFailure ("array at " <> show k <> ": " <> e)) pure
                         (parseEither parseJSON x)

-- | The array of strings at KEY of V.
textsAt :: T.Text -> Value -> IO [T.Text]
textsAt k v = field k v >>= read'
  where read' x = either (\e -> assertFailure ("strings at " <> show k <> ": " <> e)) pure
                         (parseEither parseJSON x)

-- | The object at KEY of V, as its members by name.
membersAt :: T.Text -> Value -> IO [(T.Text, Value)]
membersAt k v = field k v >>= members
  where members (Object o) = pure [ (Key.toText name, x) | (name, x) <- KM.toList o ]
        members other = assertFailure ("expected an object at " <> show k
                                         <> ", got " <> show other)

-- | R's @rows@ array.
rowsOf :: SResponse -> IO [Value]
rowsOf r = listAt "rows" =<< decoded r

-- | ROW's @id@, or the whole row when it has none — a failure that reads.
rowId :: Value -> T.Text
rowId row = case row of
  Object o -> case KM.lookup "id" o of
    Just (String i) -> i
    _noId           -> T.pack (show row)
  _notARow -> T.pack (show row)

-- | ROW's @scheduled@ cell, empty when it has none.  The key the view declares
-- its sort on, so a page has to come out of this order.
scheduledOf :: Value -> T.Text
scheduledOf row = case row of
  Object o -> case KM.lookup "cells" o of
    Just (Object cells) -> case KM.lookup "scheduled" cells of
      Just (String s) -> s
      _unscheduled    -> ""
    _noCells -> ""
  _notARow -> ""

-- | The state column's badge values, in palette order.
badgeValues :: Value -> IO [T.Text]
badgeValues view = do
  cols <- listAt "columns" view
  state <- maybe (assertFailure "no state column") pure
                 (find (keyIs "state") cols)
  traverse (textAt "value") =<< listAt "badges" state
  where keyIs k (Object o) = KM.lookup "key" o == Just (String k)
        keyIs _ _notAColumn = False

-- | What sits between OPEN and CLOSE in HAYSTACK, when both are in it.
between :: T.Text -> T.Text -> T.Text -> Maybe T.Text
between open close haystack
  | T.null after = Nothing
  | T.null rest  = Nothing
  | otherwise    = Just inner
  where (_before, after) = T.breakOn open haystack
        (inner, rest)    = T.breakOn close (T.drop (T.length open) after)

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
  [ headlineSpec, statsSpec, cacheSpec, gzipSpec, querySpec, bootstrapSpec
  , materializeSpec, commitSpec, pageSpec, keymapSpec
  , shellFontSpec, assetSpec, errorSpec ]

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

-- | The @ETag@ is the store's generation, and the store's generation is what
-- the watcher moves.  Every query variant shares it — the parameters are in
-- the URL, and an HTTP cache is keyed by URL, so each variant revalidates
-- against the tag it was itself given.
cacheSpec :: TestTree
cacheSpec = testGroup "GET /headlines cache validation"
  [ testCase "carries a generation tag, and says to revalidate every time" $ do
      r <- get assetsDir "/headlines"
      assertEqual "ETag" (Just "\"g0\"") (header "ETag" r)
      assertEqual "Cache-Control" (Just "no-cache") (header "Cache-Control" r)

  , testCase "the tag it just gave out is a 304 with no body" $ do
      a <- app assetsDir
      first' <- getFrom a "/headlines"
      let tag = fromMaybe "" (header "ETag" first')
      again <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 304 (status again)
      assertEqual "body" "" (simpleBody again)
      assertEqual "the tag comes back" (Just tag) (header "ETag" again)
      -- Nothing else is owed on a 304, and Content-Type least of all.
      assertEqual "no content type" Nothing (header "Content-Type" again)

  , testCase "a weak tag, or one in a list, still matches" $ do
      a <- app assetsDir
      weak <- getWith a "/headlines" [("If-None-Match", "W/\"g0\"")]
      listed <- getWith a "/headlines" [("If-None-Match", "\"g9\", \"g0\"")]
      assertEqual "weak" 304 (status weak)
      assertEqual "listed" 304 (status listed)

  , testCase "a tag from another generation is the whole document again" $ do
      a <- app assetsDir
      r <- getWith a "/headlines" [("If-None-Match", "\"g7\"")]
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)

  , testCase "a store the watch moved is a fresh tag" $ withTempDir $ \dir -> do
      path <- orgFile dir "notes.org" committable
      (a, hub) <- serverOver dir
      before <- getFrom a "/headlines"
      let tag = fromMaybe "" (header "ETag" before)
      -- The watch's own step, taken here without a watcher: re-load the file
      -- and publish it, which is the one path that updates the store.
      _ <- orgFile dir "notes.org" (committable <> "* TODO Third\n")
      outcome <- loadFile path
      _ <- publish hub (applyFile path outcome)
      after <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 200 (status after)
      assertBool "the tag moved with the store"
                 (header "ETag" after /= Just tag)
      assertEqual "and the new row is in it" (Just "3") (header "X-Glance-Rows" after)

  , testCase "a re-load that changes nothing leaves the tag where it was" $
      withTempDir $ \dir -> do
        path <- orgFile dir "notes.org" committable
        (a, hub) <- serverOver dir
        before <- getFrom a "/headlines"
        outcome <- loadFile path
        _ <- publish hub (applyFile path outcome)
        after <- getFrom a "/headlines"
        assertEqual "the tag" (header "ETag" before) (header "ETag" after)

  , testCase "one tag serves every variant, which the URL keeps apart" $ do
      a <- app assetsDir
      full <- getFrom a "/headlines"
      paged <- getFrom a "/headlines?limit=2"
      filtered <- getFrom a "/headlines?q=table"
      assertEqual "the paged tag" (header "ETag" full) (header "ETag" paged)
      assertEqual "the filtered tag" (header "ETag" full) (header "ETag" filtered)
      -- The bodies differ, which is what the distinct URLs are for.
      assertBool "one URL's answer served for another"
                 (simpleBody full /= simpleBody paged)
  ]

-- | Compression: on for the text this server sends, off for a body too small
-- to gain by it, and always with the @Vary@ that keeps the two encodings from
-- being confused for each other.
gzipSpec :: TestTree
gzipSpec = testGroup "Compression"
  [ testCase "the view JSON is gzipped for a client that asks" $ do
      a <- app assetsDir
      plain' <- getFrom a "/headlines"
      zipped <- getWith a "/headlines" [("Accept-Encoding", "gzip")]
      assertEqual "status" 200 (status zipped)
      assertEqual "Content-Encoding" (Just "gzip") (header "Content-Encoding" zipped)
      assertBool "compressed to no less than it was"
                 (BL.length (simpleBody zipped) < BL.length (simpleBody plain'))

  , testCase "and left alone for a client that does not" $ do
      r <- get assetsDir "/headlines"
      assertEqual "Content-Encoding" Nothing (header "Content-Encoding" r)
      assertContains "the body is JSON" "\"rows\"" (body r)

  , testCase "every answer varies on the encoding, 304s included" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines"
      notModified <- getWith a "/headlines"
        [("If-None-Match", fromMaybe "" (header "ETag" r))]
      assertEqual "on the 200" (Just "Accept-Encoding") (header "Vary" r)
      assertEqual "on the 304" (Just "Accept-Encoding") (header "Vary" notModified)

  , testCase "a body under the threshold is not worth compressing" $ do
      a <- app assetsDir
      r <- getWith a "/headline" [("Accept-Encoding", "gzip")]
      assertEqual "status" 400 (status r)
      assertBool "the error JSON is small" (BL.length (simpleBody r) < 860)
      assertEqual "Content-Encoding" Nothing (header "Content-Encoding" r)

  , testCase "the renderer is compressed too, though it is a file" $ do
      a <- app assetsDir
      r <- getWith a "/table-view.js" [("Accept-Encoding", "gzip")]
      assertEqual "status" 200 (status r)
      assertEqual "Content-Encoding" (Just "gzip") (header "Content-Encoding" r)
  ]

-- | @q@, @limit@ and @offset@: filter first, then page, and report what the
-- page covers in the header family the load counts already use.
querySpec :: TestTree
querySpec = testGroup "GET /headlines filter and paging"
  [ testCase "no parameters is the whole set, as it always was" $ do
      r <- get assetsDir "/headlines"
      assertEqual "X-Glance-Total" (Just "6") (header "X-Glance-Total" r)
      assertEqual "X-Glance-Has-Next" (Just "false") (header "X-Glance-Has-Next" r)
      assertEqual "rows" 6 . length =<< rowsOf r

  , testCase "q narrows on the row as it displays, case-insensitively" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=SHIP%20THE%20TABLE"
      assertEqual "X-Glance-Total" (Just "1") (header "X-Glance-Total" r)
      ids <- map rowId <$> rowsOf r
      assertEqual "the matching row" ["ship-table-view"] ids

  , testCase "q matches a bracket link by its description, not its target" $
      withTempDir $ \dir -> do
        -- What the row shows is what a filter searches, the way the renderer
        -- searches its own cached display text (table-view.js `displayText').
        _ <- orgFile dir "links.org"
               "* TODO Read [[file:table-view/SCHEMA.md][the schema]]\n"
        (a, _hub) <- serverOver dir
        shown <- getFrom a "/headlines?q=the%20schema"
        target <- getFrom a "/headlines?q=SCHEMA.md"
        assertEqual "the description matches" (Just "1") (header "X-Glance-Total" shown)
        assertEqual "the target does not" (Just "0") (header "X-Glance-Total" target)

  , testCase "q matching nothing is an empty page under a 200" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=no-such-headline-anywhere"
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Total" (Just "0") (header "X-Glance-Total" r)
      assertEqual "rows" 0 . length =<< rowsOf r

  , testCase "limit cuts a page out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines"
      page <- rowsOf =<< getFrom a "/headlines?limit=3"
      assertEqual "page size" 3 (length page)
      -- The page is the first three by scheduled ascending, which is what the
      -- view declares — not the first three the walk found.
      assertEqual "the sort the view declares"
                  (take 3 (map rowId (sortOn scheduledOf whole)))
                  (map rowId page)

  , testCase "offset walks the pages, and has-next says when to stop" $ do
      a <- app assetsDir
      whole <- map rowId . sortOn scheduledOf <$> (rowsOf =<< getFrom a "/headlines")
      one <- getFrom a "/headlines?limit=4&offset=0"
      two <- getFrom a "/headlines?limit=4&offset=4"
      past <- getFrom a "/headlines?limit=4&offset=6"
      assertEqual "page one" (take 4 whole) . map rowId =<< rowsOf one
      assertEqual "page two" (drop 4 whole) . map rowId =<< rowsOf two
      assertEqual "more follows page one" (Just "true") (header "X-Glance-Has-Next" one)
      assertEqual "nothing follows page two" (Just "false") (header "X-Glance-Has-Next" two)
      assertEqual "past the end is empty" 0 . length =<< rowsOf past
      assertEqual "and says so" (Just "false") (header "X-Glance-Has-Next" past)

  , testCase "the filter runs before the page, so the total is the match count" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=e&limit=2&offset=1"
      matched <- length <$> (rowsOf =<< getFrom a "/headlines?q=e")
      assertEqual "the total is what matched" (Just (T.pack (show matched)))
                  (fmap TE.decodeUtf8 (header "X-Glance-Total" r))
      assertBool "the fixture would not exercise the arithmetic" (matched > 3)
      page <- rowsOf r
      assertEqual "the page is a slice of it" 2 (length page)
      assertEqual "and more follows" (Just "true") (header "X-Glance-Has-Next" r)

  , testCase "the state palette is the store's, whatever the page holds" $ do
      a <- app assetsDir
      whole <- badgeValues =<< decoded =<< getFrom a "/headlines"
      page <- badgeValues =<< decoded =<< getFrom a "/headlines?limit=1"
      none <- badgeValues =<< decoded =<< getFrom a "/headlines?q=no-such-headline"
      assertEqual "the paged palette" whole page
      assertEqual "the empty page's palette" whole none
      assertBool "the fixture declares keywords" (length whole > 2)

  , testCase "a limit past the cap is refused, and named" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?limit=20001"
      ok <- getFrom a "/headlines?limit=20000"
      assertEqual "over" 400 (status r)
      assertContains "the cap" "20000" (body r)
      assertEqual "at the cap" 200 (status ok)

  , testCase "a parameter that is not a number is a 400 saying which" $ do
      a <- app assetsDir
      mapM_ (\(path, named) -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the parameter" named (body r))
            [ ("/headlines?limit=lots", "limit")
            , ("/headlines?limit=-1", "limit")
            , ("/headlines?offset=x", "offset")
            , ("/headlines?offset=-3", "offset") ]

  , testCase "a bare parameter reads as an absent one" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?limit&q"
      assertEqual "status" 200 (status r)
      assertEqual "rows" 6 . length =<< rowsOf r
  ]

-- | @\/ws?bootstrap=off@: the opening @set-rows@ dropped for a client that
-- fetched the rows over HTTP.  Checked on the parser, since the suite binds no
-- socket — the decision is the whole of what the query controls.
bootstrapSpec :: TestTree
bootstrapSpec = testGroup "Socket bootstrap control"
  [ testCase "is wanted by default, and by every query but the one" $
      mapM_ (\path -> assertBool (show path <> " skipped the bootstrap")
                                 (bootstrapWanted path))
            ["/ws", "/ws?", "/ws?keys=vim", "/ws?bootstrap=on", "/ws?bootstrap="]

  , testCase "bootstrap=off drops it, wherever it sits in the query" $
      mapM_ (\path -> assertBool (show path <> " still sent the bootstrap")
                                 (not (bootstrapWanted path)))
            ["/ws?bootstrap=off", "/ws?keys=vim&bootstrap=off", "/ws?bootstrap=off&x=1"]
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
      assertContains "fetch glue" "fetch(`/headlines${params}`" (body r)
      assertContains "mount" "TableView.mount(" (body r)

  , testCase "with assets, paints a page and loads the rest behind it" $ do
      b <- body <$> get assetsDir "/"
      mapM_ (\needle -> assertContains "paging glue" needle b)
            [ "const PAGE = 1000;", "load(`?limit=${PAGE}`)"
            , "r.headers.get(\"X-Glance-Total\")", "a.total > (a.view.rows || []).length"
            , "load(\"\").then(" ]

  , testCase "with assets, hands the filter to the server and aborts stale fetches" $ do
      b <- body <$> get assetsDir "/"
      mapM_ (\needle -> assertContains "filter glue" needle b)
            [ "onFilter: filter", "new AbortController()", "inflight.abort()"
            , "signal: inflight.signal", "?q=${encodeURIComponent(query)}"
            , "e.name !== \"AbortError\"" ]

  , testCase "with assets, opens a socket and applies the streaming ops" $ do
      r <- get assetsDir "/"
      mapM_ (\needle -> assertContains "live glue" needle (body r))
            [ "new WebSocket(", "/ws?bootstrap=off", "table.setRows("
            , "\"upsert-row\"", "table.upsertRow(", "\"delete-row\"", "table.deleteRow("
            -- Under a filter the rows are the server's answer to a query, so a
            -- row frame is re-asked for rather than spliced into them.
            , "setTimeout(() => filter(query), 250)" ]
      -- With `bootstrap=off' no `set-rows' frame can arrive, so the branch that
      -- would have applied one is gone rather than left unreachable.
      assertBool "a branch for a frame this shell cannot receive"
                 (not ("\"set-rows\"" `T.isInfixOf` body r))

  , testCase "with assets, re-fetches and remounts after a close" $ do
      r <- get assetsDir "/"
      mapM_ (\needle -> assertContains "reconnect glue" needle (body r))
            [ "socket.onclose", "setTimeout(start,", "Math.min(backoff * 2, 30000)" ]

  , testCase "with assets, materializes a row and posts it back" $ do
      r <- get assetsDir "/"
      mapM_ (\needle -> assertContains "materialize glue" needle (body r))
            [ "\"materialize\"", "/headline?id=${encodeURIComponent(", "<textarea id=\"mtext\""
            , "method: \"POST\"", "digest: editing.digest", "a.status === 409"
            -- The sheet's two exits are keymap rows now: ESC cancels it,
            -- C-x C-s saves it from inside the textarea.
            , "File changed since materialize", "Re-materialize"
            , "keyboard-quit", "C-x C-s" ]

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

-- | A keymap row as the suite spells it: the keys the dispatch matches, the
-- notation the echo widget shows, the command name, the handler behind it (or
-- none, for a binding no daemon command backs yet) and the scope it is live in.
type Row = ([T.Text], T.Text, T.Text, Maybe T.Text, T.Text)

-- | The shell's keymap, checked as the data it is.  The page carries the map
-- as a JSON blob and its own dispatch parses that blob, so reading it here
-- reads what the browser reads rather than what a grep for handler names would
-- find.
--
-- The expected map is written down rather than imported: the point of the
-- assertion is that the sequences and the org-glance command names are the
-- ones @org-glance-overview-mode-map@ spells, and an oracle taken from the
-- code under test would agree with any of them.
--
-- What both profiles carry — every command that is not movement, plus the
-- arrows and org-glance's own buffer-ends keys.
expectedShared :: [Row]
expectedShared =
  [ (["<down>"],     "<down>",  "next-row",                        Just "nextRow",        "table")
  , (["<up>"],       "<up>",    "previous-row",                    Just "previousRow",    "table")
  , ([","],          ",",       "first-row",                       Just "firstRow",       "table")
  , (["<"],          "<",       "first-row",                       Just "firstRow",       "table")
  , (["."],          ".",       "last-row",                        Just "lastRow",        "table")
  , ([">"],          ">",       "last-row",                        Just "lastRow",        "table")
  , (["RET"],        "RET",     "org-glance-overview:materialize", Just "materializeRow", "table")
  , (["/"],          "/",       "filter-rows",                     Just "focusFilter",    "table")
  , (["q"],          "q",       "quit-window",                     Just "quitWindow",     "table")
  , (["TAB"],        "TAB",     "org-cycle",                       Nothing,               "table")
  , (["!"],          "!",       "org-glance-overview:open",        Nothing,               "table")
  , (["a"],          "a",       "org-glance-agenda",               Nothing,               "table")
  , (["@"],          "@",       "org-glance-overview:relations",   Nothing,               "table")
  , (["+"],          "+",       "org-glance-overview:capture",     Nothing,               "table")
  , (["D"],          "D",       "org-glance-overview:delete",      Nothing,               "table")
  , (["C-c", "C-t"], "C-c C-t", "org-glance-overview:todo",        Nothing,               "table")
  , (["C-c", "C-s"], "C-c C-s", "org-glance-overview:schedule",    Nothing,               "table")
  , (["C-c", "C-d"], "C-c C-d", "org-glance-overview:deadline",    Nothing,               "table")
  , (["C-x", "C-s"], "C-x C-s", "save-buffer",                     Just "save",           "modal")
  , (["ESC"],        "ESC",     "keyboard-quit",                   Just "cancel",         "any")
  ]

-- | The movement each profile adds, and what it displaces.  @j@ is the
-- overview's open-stub under emacs and down under vim; @g@ is refresh under
-- emacs and the opening of @gg@ under vim, which sends refresh to @R@.
expectedProfiles :: [(T.Text, [Row])]
expectedProfiles =
  [ ("emacs",
      [ (["n"], "n", "next-row",                    Just "nextRow",     "table")
      , (["p"], "p", "previous-row",                Just "previousRow", "table")
      , (["g"], "g", "org-glance-overview:refresh", Just "refresh",     "table")
      , (["j"], "j", "org-glance-overview:open",    Nothing,            "table")
      ])
  , ("vim",
      [ (["j"],      "j",  "next-row",                    Just "nextRow",     "table")
      , (["k"],      "k",  "previous-row",                Just "previousRow", "table")
      , (["g", "g"], "gg", "first-row",                   Just "firstRow",    "table")
      , (["G"],      "G",  "last-row",                    Just "lastRow",     "table")
      , (["R"],      "R",  "org-glance-overview:refresh", Just "refresh",     "table")
      ])
  ]

-- | The keymap blob out of SHELL: the shared rows, and the profiles by name.
keymapOf :: T.Text -> IO ([Row], [(T.Text, [Row])])
keymapOf shell = do
  raw <- maybe (assertFailure "no keymap blob in the shell") pure
               (between "<script id=\"keys\" type=\"application/json\">" "</script>" shell)
  blob <- either (\e -> assertFailure ("keymap JSON: " <> e)) pure
                 (eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)))
  shared <- traverse row =<< listAt "shared" blob
  named <- traverse profile =<< membersAt "profiles" blob
  pure (shared, sortOn fst named)
  where
    profile (name, v) = do
      rows <- either (\e -> assertFailure (T.unpack name <> " profile: " <> e)) pure
                     (parseEither parseJSON v)
      (,) name <$> traverse row rows
    row v = (,,,,) <$> textsAt "keys" v <*> textAt "seq" v <*> textAt "command" v
                   <*> maybeTextAt "handler" v <*> textAt "scope" v

-- | The shell's inline glue, on its own — what a syntax check is run over.
glueOf :: T.Text -> IO T.Text
glueOf shell = maybe (assertFailure "no inline script in the shell") pure
                     (between "\n  <script>\n" "  </script>" shell)

keymapSpec :: TestTree
keymapSpec = testGroup "Shell keymap"
  [ testCase "is one JSON blob, in org-glance's own command names" $ do
      (shared, _profiles) <- keymapOf . body =<< get assetsDir "/"
      assertEqual "the rows both profiles carry" expectedShared shared

  , testCase "carries a movement profile per editor, emacs by default" $ do
      (_shared, profiles) <- keymapOf . body =<< get assetsDir "/"
      assertEqual "profiles" expectedProfiles profiles
      assertContains "the default is named in the blob" "\"default\":\"emacs\""
        . body =<< get assetsDir "/"

  , testCase "no profile shadows a shared binding, or hides its own longer one" $ do
      (shared, profiles) <- keymapOf . body =<< get assetsDir "/"
      let keysOf rows = [ k | (k, _, _, _, _) <- rows ]
      mapM_ (\(name, rows) -> do
               let bound = keysOf (shared <> rows)
                   twice = [ k | k <- nub bound, length (filter (== k) bound) > 1 ]
                   -- A complete sequence that also opens a longer one would
                   -- match first and leave the longer one unreachable.
                   eaten = [ (k, l) | k <- bound, l <- bound
                                    , k /= l, k == take (length k) l ]
               assertEqual (T.unpack name <> ": bound twice") [] twice
               assertEqual (T.unpack name <> ": swallows a longer sequence") [] eaten)
            profiles

  , testCase "the dispatch and the echo widget read that blob and no other map" $ do
      b <- body <$> get assetsDir "/"
      mapM_ (\needle -> assertContains "keymap glue" needle b)
        [ "<script id=\"keys\" type=\"application/json\">"
        , "JSON.parse(el(\"keys\").textContent)"
        , "MAPS.shared.concat(MAPS.profiles[name])"
        , "KEYS.filter(live)", "HANDLERS[b.handler]" ]

  , testCase "the profile is remembered, askable, and switchable in place" $ do
      b <- body <$> get assetsDir "/"
      mapM_ (\needle -> assertContains "profile glue" needle b)
        [ "localStorage.getItem(\"glance-keys\")", "localStorage.setItem(\"glance-keys\""
        , "new URLSearchParams(location.search).get(\"keys\")"
        , "<button id=\"keyset\"", "#keyset{", "keys: ${name}", "movement: ${profile}" ]

  , testCase "a binding with no handler names what it is waiting for" $ do
      b <- body <$> get assetsDir "/"
      assertContains "staged toast" "arrives with daemon commands (M4)" b

  , testCase "the echo widget is mounted, in Emacs wording" $ do
      b <- body <$> get assetsDir "/"
      mapM_ (\needle -> assertContains "echo widget" needle b)
        [ "<div id=\"echo\"", "#echo{position:fixed", "is undefined", "timed out"
        , "Enter: \"RET\"", "Escape: \"ESC\"", "ArrowUp: \"<up>\"" ]

  , testCase "the prefix keys are claimed only where they are ours" $ do
      b <- body <$> get assetsDir "/"
      mapM_ (\needle -> assertContains "chord policy" needle b)
        -- A selection keeps C-c and C-x as copy and cut; the reserved chords
        -- reach the browser even as the continuation of a claimed prefix, which
        -- is why neither profile moves on C-n or C-p.
        [ "if (!selecting()) { e.preventDefault();"
        , "const RESERVED = [\"C-l\", \"C-r\", \"C-t\", \"C-w\", \"C-n\", \"C-p\", \"<f5>\"];"
        , "if (RESERVED.indexOf(k) === -1) e.preventDefault();" ]

  , testCase "row movement drives the renderer's own selection" $ do
      b <- body <$> get assetsDir "/"
      -- The renderer virtualizes, so a row outside the window has no element:
      -- movement is ids out of `getVisible()' handed back to `select(id)'.
      mapM_ (\needle -> assertContains "row focus" needle b)
        [ "tbody tr.tv-sel", "table.getVisible()", "table.select(id)", ".tv-filter" ]
      mapM_ (\gone -> assertBool ("the DOM movement path survives: " <> show gone)
                                 (not (gone `T.isInfixOf` b)))
        [ "tr.click()", "scrollIntoView", "rowEls(" ]

  , testCase "the inline glue is JavaScript, where there is a node to say so" $ do
      node <- findExecutable "node"
      case node of
        -- No node on this machine: the syntax of the glue is checked wherever
        -- there is one, and the rest of this group still reads it as text.
        Nothing  -> pure ()
        Just exe -> withTempDir $ \dir -> do
          glue <- glueOf . body =<< get assetsDir "/"
          let path = dir </> "shell.js"
          TIO.writeFile path glue
          (code, _out, err) <- readProcessWithExitCode exe ["--check", path] ""
          assertEqual ("node --check said: " <> err) ExitSuccess code
  ]

-- | The shell is monospace, and gets there without asking the network for it.
shellFontSpec :: TestTree
shellFontSpec = testGroup "Shell type"
  [ testCase "asks for one font stack, everywhere in the page" $ do
      b <- body <$> get assetsDir "/"
      assertContains "stack"
        "--glance-mono:\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"
        b
      -- The renderer injects `.tv-root{font:…}' from its own script, which
      -- lands after this page's style element; the extra selector step wins.
      assertContains "the renderer's font, overridden"
                     "#app .tv-root{font-family:var(--glance-mono)}" b
      mapM_ (\needle -> assertContains "monospace widget" needle b)
            ["font:14px/1.5 var(--glance-mono)", "font:12px/1.5 var(--glance-mono)"]

  , testCase "with no font file to serve, says nothing about one" $ do
      b <- body <$> get assetsDir "/"
      assertBool "an @font-face with no file behind it"
                 (not ("@font-face" `T.isInfixOf` b))

  , testCase "a font in the assets directory is declared and served" $
      withTempDir $ \dir -> do
        TIO.writeFile (dir </> "table-view.js") ""
        BS.writeFile (dir </> "JetBrainsMono-Regular.woff2") "wOF2"
        b <- body <$> get dir "/"
        assertContains "declared" "@font-face{font-family:\"JetBrains Mono\"" b
        assertContains "from this server, by name"
                       "src:url(\"JetBrainsMono-Regular.woff2\") format(\"woff2\")" b
        r <- get dir "/JetBrainsMono-Regular.woff2"
        assertEqual "status" 200 (status r)
        assertEqual "content type" (Just "font/woff2") (header "Content-Type" r)

  , testCase "no page this server serves reaches off it" $ do
      shell <- body <$> get assetsDir "/"
      bare <- body <$> get missingAssetsDir "/"
      mapM_ (\(what, page') ->
               mapM_ (\scheme -> assertBool (what <> " fetches " <> show scheme)
                                            (not (scheme `T.isInfixOf` page')))
                     ["http://", "https://", "@import"])
            [("the shell", shell), ("the JSON-only page", bare)]
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
