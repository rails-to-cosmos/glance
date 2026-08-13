{-# LANGUAGE OverloadedStrings #-}

-- | The spec asked of the tree.  @AGENTS.hs@ is glance's domain written as
-- types and registries; every case below compares one of those registries with
-- the code's own -- the route table, the command table, the columns, the cabal
-- stanzas -- so the model and the tree cannot drift apart in silence.  What the
-- spec once checked against itself is asked here instead, where one case
-- reaches both.
module TestSpec (spec) where

import qualified AGENTS as Spec
import Control.Monad (filterM)
import Data.List (sort)
import Data.Org (Element (EPragma, ETimestamp), Headline (spans), HeadlineSpans (..), Keyword (..), OrgLine (..), OrgLineElement (OrgLineToken), Pragma (..), Span (..), Spanned (..), Timestamp (..), TimestampRepeaterInterval (..), TimestampRepeaterSign (TRSPlus), TimestampRepeaterType (Restart), TimestampStatus (TimestampActive), TimestampUnit (Days, Weeks), TimestampWarningInterval (..), defaultContext, defaultHeadline, headlineSpanParts, hsFull, orgParse, todoActive)
import Data.Text (Text)
import Glance.Query (subtreeText)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults (at, bare, bareParse, compactTs, on, plainTs, propertyKeys, withDoc, withHeadline)
import TextShow (showt)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (forM, forM_)
import Data.Aeson (object, (.=))
import Data.List (nub, sortOn)
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Tasty.HUnit (assertFailure)
import TestDefaults (orgFile, withTempDirNamed)
import TestWire (command, postTo, serverAt, status)
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import Data.Org.Edit (Edit (..), applyEdits)
import Data.Org.External (completionsFile, completionsPathOf, externalFile, externalPathOf)
import Data.Org.Index (IndexFold (..), IndexRecord (..), foldSegments, metaDir)
import Data.Org.Walk (Found (..), WalkOptions (..), defaultWalk, findOrgFilesWith, mapFilesConcurrently)
import Glance.Query (HeadlineRecord (..), Repeat (..), blobPathIn, loadFile, noConfig, repeatOn, shiftRepeat, storeRootIn)
import Glance.Web.Commands (commandNames)
import System.Posix.Files (createSymbolicLink)
import Data.Org.Walk (blobFile, claimById, configDir, isConfig, isDerived, isDocument, isWalked, orgGlanceDir, storeDir, trashDir)
import Glance.Query (IdCollision (..), QueryResult (..), configPath, derivedPath, documentPath, loadDir, rowIdIn)
import Data.Org.Config (ConfigLayerFile (..), SavedView (..), TodoKeywords (..), TreeSettings (..), noTreeSettings, savedViews, stateColorsOf, todoPragmas, treeSettings, viewQueryIn)
import Glance.Query (ConfigSetting (..), SettingScope (..), configSettings)
import Control.Concurrent.STM (readTVarIO)
import Data.Aeson (Value, decode, eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.String (fromString)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types (HeaderName, RequestHeaders, methodDelete, methodHead, renderQuery)
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders), request, runSession, setPath)
import System.Directory (removeFile)
import Test.Tasty.HUnit (Assertion)
import TestDefaults (assertContains, document, textAt, viewDir, withTempDir)
import TestWire (assertOk)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import AGENTS (Method (GET), RefusalBody (JsonRefusal), notFoundText, pageHeaders, rMethods, rNeeds, rPath, rRefusal, routeAt, routes, statsHeaders, takesText)
import Glance.Web (ServeOptions (..), application, defaultPort)
import Glance.Web.Store (CloseReason, Frame (Op), Hub (hubPending, hubStore), RowOp (DeleteRow, UpsertRow), Store (stGen, stPrint, stTags), applyFile, closeReason, loadStore, newHub, newLoadingHub, recordsUnder, storeRecords)
import Glance.Web.Watch (settle)
import Data.Aeson (Value (Object))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Lazy.Encoding as TLE
import AGENTS (ColKind (KBadge), Column (cCellKind, cHead, cKey), SortDir (Asc), defaultSortChain, keyOf, viewColumns, viewKeys)
import Glance.Web.Columns (columnNamesIn)
import Glance.Web.Filter (Term (tmKey), Token (..), columnsKey, emptyEnv, filterKeys, matchesFilter, parseFilter, plannedKey, refKey, scanQuery, sortKey, substringKey)
import Glance.Web.Sort (sortChainIn)
import TestDefaults (listAt, withDocDir)
import qualified Glance.Query as Q
import Data.Aeson (Value (Null), toJSON)
import Data.Either (isLeft)
import Data.Maybe (isNothing, mapMaybe)
import Data.Time.Calendar (fromGregorian)
import Data.Org (isTagChar)
import Glance.Query (prioritySlots, stateSlots, tagText)
import Glance.Web.Theme (Mode (Dark, Light), Theme (thId, thMode), themeCSS, themeIds, themes)
import Glance.Web.Base (logLinesDefault, logLinesMax, logLinesMin)
import Glance.Web.Page.Glue (glueConfig)
import Glance.Web.Page.Style (page)
import qualified Data.Text.Read as T.Read
import Glance.Desktop (browserCandidates)
import Glance.Query (OrgLink (olSpan), cellSep, orgLinks, planningKeywords, subtreeLinks)
import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.List (isPrefixOf, (\\))
import Data.Maybe (fromMaybe)
import qualified Glance.Web.Base as WB (gluePartFiles)
import AGENTS (BuildAsset (baSplice), CabalFlag (flCpp, flManual, flName, flOn, flStanza), Component (coName, coVis), Proj (DefaultProj, NativeProj), Status, Vis (Public), WMod (WBase, WDesktop, WNative, WRoutes, WWeb), authorEmail, buildAssets, compDeps, components, flags, giSpellings, gluePartFiles, negationReveal, projBuildDir, projFlags, projGir, projPackages, sdistExtras, statusWord, vendoredGirs, versionSites, webExposed, webTargets, wimports, wmods, wname)

-- | Parse: the headline's sub-spans, the extent they fold to, and what the
-- parser keeps, drops and folds on its way in.
--
-- The ORDER is the spec's registry ("AGENTS") and the parts are
-- 'Data.Org.Types.headlineSpanParts'', so the two cannot drift; everything
-- below it is the parser's own answer, read off one source apiece.
specGroup03 :: TestTree
specGroup03 = testGroup "Parse"
  [ testCase "sub-span order: todo < priority < title < tags < planning < properties" $
      -- 'headlineSpanParts' lists every part with its 'Maybe Span', so the
      -- absent ones are still named and the ORDER is readable off a headline
      -- carrying no span at all.
      assertEqual "the spec's sub-span order and headlineSpanParts have drifted"
                  (concatMap subLabels Spec.subOrder) (partLabels defaultHeadline)

    -- The stars are 'hsFull''s fold SEED and the one unconditional span, so
    -- they are no entry of the part list.
  , testCase "Stars is the seed, subOrder the other six" $ do
      assertBool "the fold's seed became a part: hsStars is among the sub-spans"
                 ("hsStars" `notElem` partLabels defaultHeadline)
      assertEqual "the spec's Sub no longer spells Stars plus subOrder"
                  [minBound .. maxBound] (sort (Spec.Stars : Spec.subOrder))
      assertEqual "a Sub the headline record carries no sub-span for"
                  (length (concatMap subLabels Spec.subOrder))
                  (length (partLabels defaultHeadline))

    -- The title runs PAST the tags here, which is the one shape that tells a
    -- left fold from a maximum: a maximum over ends would answer Span 0 30.
  , testCase "hsFull is a fold, never a maximum over ends" $
      assertEqual "hsFull answered the widest end rather than the LAST part's"
                  (Span 0 23)
                  (hsFull ((spans defaultHeadline) { hsStars = Span 0 1
                                                   , hsTitle = Just (Span 12 30)
                                                   , hsTags  = Just (Span 18 23) }))

    -- A subtree runs from a headline's stars, so the pragmas above the first
    -- one are in nobody's extent.  No fixture carries a preamble, which leaves
    -- this half of the geometry resting on the rule alone.
  , testCase "the #+ preamble sits ahead of the first extent" $
      withDoc "spec-preamble" "pre.org" "#+TODO: A | B\n* one\nbody\n" $ \recs ->
        assertEqual "the pragma line landed inside a subtree extent"
                    ["* one\nbody\n"] (map subtreeText recs)

    -- 'tsBodyParser' reads a LIST of cookies and takes the first of each kind;
    -- no other case doubles one, so the rule is unasserted without this.
  , testCase "at most one repeater and one warning, first of each winning" $ do
      assertEqual "a second repeater outranked the first"
                  (Just (Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus)))
                  (tsInterval <$> timestampIn "<2024-01-15 Mon +1w ++2d>")
      assertEqual "a second warning outranked the first"
                  (Just (Just (TimestampWarningInterval False 3 Days)))
                  (tsWarning <$> timestampIn "<2024-01-15 Mon -3d --7d>")

    -- The flag alone is what the suite exercises; these are its other two
    -- conditions, each of which would render a range's end DATE away.
  , testCase "compactly wants the flag, both ends timed, and one day" $ do
      assertEqual "a range spanning two days rendered compact"
                  "<2024-01-15 Mon 10:30>--<2024-01-16 Tue 11:30>"
                  (showt (compactTs TimestampActive (at "2024-01-15 10:30:00")
                                                    (at "2024-01-16 11:30:00")))
      assertEqual "a range whose end spells no time rendered compact"
                  "<2024-01-15 Mon 10:30>--<2024-01-15 Mon>"
                  (showt (compactTs TimestampActive (at "2024-01-15 10:30:00")
                                                    (on "2024-01-15 00:00:00")))

  , testCase "a range with no end never renders compact" $
      assertEqual "a flag with no end to compact reached the range render"
                  "<2024-01-15 Mon 10:30>"
                  (showt ((plainTs TimestampActive (at "2024-01-15 10:30:00"))
                            { tsCompactRange = True }))

    -- Three lexemes, three folds, each read off one lowercase spelling: org
    -- matches keywords case-sensitively, and uppercasing a property key is what
    -- lets the reserved-name guard terminate the drawer.
  , testCase "TODO keywords are verbatim; pragma and property keys uppercase" $ do
      assertEqual "a pragma key is no longer uppercased"
                  ["TITLE"]
                  [ k | EPragma (Pragma (Keyword k) _)
                          <- bareParse defaultContext "#+title: My Doc" ]
      assertEqual "the folded key no longer names the cycle"
                  [EPragma (Pragma (Keyword "TITLE")
                                   (OrgLine [OrgLineToken "My", OrgLineToken "Doc"]))]
                  (bareParse defaultContext "#+title: My Doc")
      assertEqual "a keyword is no longer stored as the tree wrote it"
                  [["next"], ["gone"]]
                  (concat [ [active, done]
                          | EPragma (PTodo active done)
                              <- bareParse defaultContext "#+todo: next | gone" ])
      withHeadline "* one\n:PROPERTIES:\n:my_key: v\n:END:\n" $ \h ->
        assertEqual "a property key is no longer uppercased" ["MY_KEY"] (propertyKeys h)
      assertEqual "the spec's casing table no longer describes those three folds"
                  [Spec.Verbatim, Spec.Uppercased, Spec.Uppercased]
                  (map Spec.casing [minBound .. maxBound])

    -- The parser's own drop.  'Data.Org.Config.todoPragmas' is pinned for this
    -- elsewhere, which is a different reader over the same line.
  , testCase "a fast-access selector is dropped" $
      case orgParse defaultContext "#+TODO: TODO(t!) NEXT(n) | DONE(d!)" of
        (elems, ctx, _err) -> do
          assertEqual "a selector survived into the cycle"
                      [EPragma (PTodo ["TODO", "NEXT"] ["DONE"])] (bare elems)
          assertEqual "a selector survived into the recognition set"
                      (Set.fromList ["TODO", "NEXT"]) (todoActive ctx)
          assertEqual "the spec cuts a selector off a word somewhere else"
                      [Spec.Kw "TODO", Spec.Kw "NEXT", Spec.Kw "DONE"]
                      (map Spec.ptodoWord ["TODO(t!)", "NEXT(n)", "DONE(d!)"])

    -- Spans are the lossless channel, so the WRITE engine and the WIRE reach
    -- for the re-serializer nowhere.  Both regions are clean today and nothing
    -- but this says they stay so.  Comments are exempt, as they are in
    -- 'TestSelfContained''s own sweeps: naming the rule is no use of it.
  , testCase "TextShow is never a write-back channel" $ do
      files <- ("src/Data/Org/Edit.hs" :) <$> sourcesUnder "src-web"
      assertBool ("too few sources swept: " <> show (length files)) (length files >= 13)
      hits <- concat <$> mapM reserializes files
      assertEqual "a write or wire module naming the re-serializer" [] hits
  ]
  where
    partLabels :: Headline -> [Text]
    partLabels h = [ l | (l, _sp, _ok) <- headlineSpanParts h ]

    -- The spec's `Sub' spelled as the record's own field names: `Planning'
    -- stands for the three that permute on one line, and the seed for none.
    subLabels :: Spec.Sub -> [Text]
    subLabels Spec.Stars      = []
    subLabels Spec.Todo       = ["hsTodo"]
    subLabels Spec.Priority   = ["hsPriority"]
    subLabels Spec.Title      = ["hsTitle"]
    subLabels Spec.Tags       = ["hsTags"]
    subLabels Spec.Planning   = ["hsSchedule", "hsDeadline", "hsClosed"]
    subLabels Spec.Properties = ["hsProperties"]

    timestampIn :: Text -> Maybe Timestamp
    timestampIn input = case orgParse defaultContext input of
      (Spanned _ (ETimestamp ts) : _, _, _) -> Just ts
      _                                     -> Nothing

    sourcesUnder :: FilePath -> IO [FilePath]
    sourcesUnder dir = do
      entries <- map (dir </>) <$> listDirectory dir
      files <- filterM doesFileExist entries
      nested <- mapM sourcesUnder =<< filterM doesDirectoryExist entries
      pure (filter ((== ".hs") . takeExtension) files <> concat nested)

    reserializes :: FilePath -> IO [String]
    reserializes path = report . T.lines <$> TIO.readFile path
      where report ls = [ path <> ":" <> show n <> ": " <> T.unpack stripped
                        | (n, l) <- zip [(1 :: Int) ..] ls
                        , let stripped = T.strip l
                        , any (`T.isInfixOf` stripped) ["TextShow", "showt", "showb"]
                        , not ("--" `T.isPrefixOf` stripped) ]

-- | THE SCAN AND THE TWO LEDGERS: the read pool both loaders share, the store
-- the walk finds by DECLINING it, the WAL fold's two truth rules, org's repeat,
-- and where each ledger line is keyed.
--
-- The registries are the SPEC's — @AGENTS.rtsOpts@, @AGENTS.planStamps@,
-- @AGENTS.cmds@ and @AGENTS.ledgerFile@ — asked of the real code rather than
-- restated beside it, so a rule that moves in one of the two places fails the
-- build instead of going quietly out of step.
specGroup04 :: TestTree
specGroup04 = testGroup "Scan and the ledgers"
  [ -- ONE POOL, TWO CALLERS.  'Data.Org.Walk.mapFilesConcurrently' is the read
    -- pool 'Glance.Query.loadDirFilesWith' and the scan share; a third
    -- implementation would be a second answer about worker width and about the
    -- order results reassemble in.  Nothing in the types says so, which is why
    -- it is swept for.
    testCase "the pool is one implementation with two callers" $ do
      files <- sweptSources
      assertBool ("too few sources swept: " <> show (length files)) (length files >= 12)
      assertBool "the sweep missed the pool's own module" (poolModule `elem` files)
      callers <- concat <$> mapM (callsIn "mapFilesConcurrently")
                                 [ f | f <- files, f /= poolModule ]
      assertEqual "callers of the one pool"
                  ["app/Scan.hs", "src-query/Glance/Query.hs"] (sort (nub callers))

    -- A load of one file forks nothing: the pool costs a worker per capability
    -- and the walk hands it single files often enough for that to matter.
  , testCase "a path list of one skips the pool" $ do
      me <- myThreadId
      ts <- mapFilesConcurrently (const myThreadId) ["only.org"]
      assertEqual "a one-path load runs in the caller's thread" [me] ts
      none <- mapFilesConcurrently (const myThreadId) []
      assertEqual "and an empty one forks nothing" ([] :: [ThreadId]) none

    -- AND THE POOL IS WORTH NOTHING WITHOUT THE RUNTIME UNDER IT: with one
    -- capability 'mapFilesConcurrently' falls back to 'mapM'.  The DAEMON's
    -- stanza is the half no running test can see — the suite can only ask about
    -- the runtime it is itself in.
  , testCase "both stanzas ask for the threaded runtime" $ do
      cabalText <- TIO.readFile "glance.cabal"
      assertBool "the spec names no RTS options" (not (null Spec.rtsOpts))
      forM_ ["executable glance", "test-suite glance-test"] $ \stanza -> do
        let opts = T.words (ghcOptionsOf stanza cabalText)
        assertBool (T.unpack stanza <> " has no ghc-options line") (not (null opts))
        forM_ Spec.rtsOpts $ \o ->
          assertBool (T.unpack stanza <> " lost " <> o) (T.pack o `elem` opts)

    -- A STORE IS FOUND BY BEING DECLINED.  The scan folds each root's own
    -- @.org-glance/meta@ plus every @meta@ the walk refused to enter, so a
    -- nested store costs nothing to find — and @--include-derived@ walks into it
    -- instead, which is exactly how it goes missing.
  , testCase "a declined store is found, and missed under --include-derived" $
      withTempDirNamed "derived" $ \dir -> do
        let store = dir </> ".org-glance" </> metaDir
        createDirectoryIfMissing True store
        found <- findOrgFilesWith defaultWalk [dir]
        opened <- findOrgFilesWith (WalkOptions True) [dir]
        assertEqual "the declined store the fold reads for free" [store] (foundDerived found)
        assertEqual "--include-derived enters it, so nothing reports it as a store"
                    [] (foundDerived opened)

    -- TWO FILES, TWO READERS, AND THEY DIVERGE.  'Data.Org.Index.recordOf' tests
    -- @tombstone@ with elisp's non-nil rule, so the STRING drops a record;
    -- @archived@ is elisp's own @(eq t VALUE)@, so the same string is false.
    -- Writers here spell @true@ as a literal because that is the one spelling
    -- both readers agree on.
  , testCase "the WAL reader is loose where elisp's flag is strict" $ do
      let folded = foldSegments
            [(True, "{\"id\":\"a\",\"tombstone\":\"true\"}\n\
                    \{\"id\":\"b\",\"archived\":\"true\"}\n")]
      assertEqual "the STRING drops the record here (truthy)"
                  ["b"] (Map.keys (ifRecords folded))
      assertEqual "and the same string is no archive flag (flagOf)"
                  (Just (Just False)) (irArchived <$> Map.lookup "b" (ifRecords folded))

    -- @{}@ is how org-glance's writer spells nil, and it is the one object both
    -- readers call false.
  , testCase "elisp's nil, written {}, is false under both" $ do
      let folded = foldSegments [(True, "{\"id\":\"a\",\"tombstone\":{},\"archived\":{}}\n")]
      assertEqual "nil is no tombstone" ["a"] (Map.keys (ifRecords folded))
      assertEqual "and none is counted" 0 (ifTombstones folded)
      assertEqual "nil is no archive flag either"
                  (Just (Just False)) (irArchived <$> Map.lookup "a" (ifRecords folded))

    -- The guard 'repeatDay' opens with, and the whole of what keeps the @++@
    -- until-loop terminating: a zero-width interval takes the @+N@ arm.
  , testCase "a zero-width interval takes the +N arm" $
      assertEqual "a zero interval stands still rather than looping"
        (Just "<2026-08-08 Sat ++0w>")
        (shiftRepeat (Time.fromGregorian 2026 8 8) "<2026-08-08 Sat ++0w>")

    -- ORG REPEATS A PLAN RATHER THAN A RECORD OF ONE.  The bracket is the
    -- ACTIVE one on all three so the timestamp kind cannot be what spares
    -- @CLOSED:@; what spares it is which planning slots a repeat reads.
  , testCase "CLOSED: is never shifted" $
      withTempDirNamed "closed" $ \dir -> do
        assertEqual "the spec names org's three planning keywords" 3 (length Spec.planStamps)
        forM_ Spec.planStamps $ \(kw, moves) -> do
          r <- rowOf dir (kw <> ".org")
                 ("* TODO t\n" <> T.pack kw <> ": <2020-01-06 Mon +1w>\n")
          assertEqual (kw <> ": does a repeater on it make a repeat?")
                      moves (isJust (repeatOn noConfig today "DONE" r))
        both <- rowOf dir "both.org"
                  "* TODO t\nSCHEDULED: <2020-01-06 Mon +1w> CLOSED: <2020-01-06 Mon +1w>\n"
        assertEqual "the plan alone moves, beside the keyword's own reset"
                    2 (length (maybe [] rpEdits (repeatOn noConfig today "DONE" both)))

    -- ORG'S OWN CONDITION IS BOTH HALVES: an inactive keyword, and a planning
    -- stamp carrying a repeater.  Either alone is a plain state change.
  , testCase "a repeat needs BOTH halves" $
      withTempDirNamed "halves" $ \dir -> do
        row <- rowOf dir "r.org" repeatingRow
        assertBool "an inactive keyword over a repeater repeats"
                   (isJust (repeatOn noConfig today "DONE" row))
        assertEqual "an ACTIVE keyword over the same row is a plain state change"
                    Nothing (repeatOn noConfig today "TODO" row)
        unrepeating <- rowOf dir "p.org" "* TODO t\nSCHEDULED: <2020-01-06 Mon>\n"
        assertEqual "and an inactive keyword with no repeater is one too"
                    Nothing (repeatOn noConfig today "DONE" unrepeating)

    -- ONE WRITE, ONE DIGEST, ONE EVENT: the shifted stamp and the reset keyword
    -- are one edit set, and 'applyEdits' takes it because the spans are
    -- disjoint.  Two writes would be two digests over one act.
  , testCase "the shift and the reset are ONE set of disjoint spans" $
      withTempDirNamed "oneset" $ \dir -> do
        r <- rowOf dir "r.org" repeatingRow
        rp <- theRepeat (repeatOn noConfig today "DONE" r)
        let sorted = sortOn spanStart (map fst (rpEdits rp))
        assertEqual "one shift and one reset" 2 (length (rpEdits rp))
        assertBool ("the two spans overlap: " <> show sorted)
                   (and [ spanEnd a <= spanStart b | (a, b) <- zip sorted (drop 1 sorted) ])
        assertEqual "and applyEdits takes them as ONE write"
                    (Right "* TODO t\nSCHEDULED: <2020-01-13 Mon +1w>\n")
                    (applyEdits (hrDoc r) [ Edit sp t | (sp, t) <- rpEdits rp ])

    -- A ROW'S ANSWER CARRIES BOTH — the spans a command moves and the ledger
    -- line riding their success — and exactly one command ever fills the second
    -- half.  Every command is fired at ONE repeating blob, which is the row that
    -- would record if any of them could; the 200 is what keeps a request refused
    -- for its shape from reading as a command that records nothing.
  , testCase "nine commands answer through plain and one records" $ do
      assertEqual "the command table the spec carries and the one that runs"
                  (sort (map T.pack Spec.commandNames)) (sort commandNames)
      let asked = [ c | c <- Spec.cmds, Spec.cKind c /= Spec.Makes ]
      assertEqual "the one kind owing no ids" 1 (length Spec.cmds - length asked)
      recorded <- forM asked $ \c -> do
        let name = T.pack (Spec.cWire c)
        wrote <- withTempDirNamed "ledger" $ \dir -> do
          _ <- blobIn dir "abcdef" repeatingEntry
          (app, _hub) <- serverAt Nothing dir
          r <- postTo app "/command" (command name ["abcdef"] (argsFor name))
          assertEqual (Spec.cWire c <> " was refused before it could write")
                      200 (status r)
          doesFileExist (dir </> ".org-glance" </> metaDir </> completionsFile)
        pure (Spec.cWire c, wrote)
      assertEqual "which commands leave a ledger line"
                  [ (Spec.cWire c, Spec.cRecords c) | c <- asked ] recorded
      assertEqual "the spec's count of commands that record"
                  Spec.recordingCommands (length (filter snd recorded))

    -- TWO NOTES, TWO KEYS.  The write note is keyed off the PATH the bytes moved
    -- at, so a blob inside a nested store is noted in THAT store; the completion
    -- is keyed off the SERVED root, which no write door carries.
  , testCase "they sit at different layers and are keyed differently" $
      withTempDirNamed "served" $ \served -> do
        createDirectoryIfMissing True (served </> ".org-glance" </> metaDir)
        assertEqual "the spec's name for the write ledger"
                    (Spec.ledgerFile Spec.ExternalL) (metaDir </> externalFile)
        assertEqual "the spec's name for the completions ledger"
                    (Spec.ledgerFile Spec.CompletionsL) (metaDir </> completionsFile)
        assertEqual "the write note follows the BYTES, into the nested store"
          (Just ("/a/.org-glance/data/x/y/.org-glance" </> metaDir </> externalFile))
          (externalPathOf "/a/.org-glance/data/x/y/.org-glance/data/ab/cd/data.org")
        assertEqual "the completion follows the SERVED root, whatever store the blob sat in"
          (Just (served </> ".org-glance" </> metaDir </> completionsFile))
          =<< completionsPathOf served
  ]
  where
    -- The pool's own module, left out of the sweep: it is the implementation and
    -- its export list, so it names the symbol without calling it.
    poolModule = "src/Data/Org/Walk.hs"

    -- One day past the fixture's stamp, so a @+1w@ lands a week on and the
    -- shifted text is worth pinning.
    today = Time.fromGregorian 2020 1 8
    repeatingRow = "* TODO t\nSCHEDULED: <2020-01-06 Mon +1w>\n"

    -- A blob as org-glance stores one: repeating, id-bearing, and carrying a
    -- link, so every command in the sweep has something of its own to move.
    -- Spelled out rather than composed, the planning line being the ONE line
    -- after the title and before any drawer.
    repeatingEntry = "* TODO Water " <> linkLiteral <> "\n\
                     \SCHEDULED: <2020-01-06 Mon +1w>\n\
                     \  :PROPERTIES:\n\
                     \  :ORG_GLANCE_ID: abcdef\n\
                     \  :END:\n"
    linkLiteral = "[[https://x.example][the plants]]"
    linkFrom = T.length (fst (T.breakOn linkLiteral repeatingEntry))

    argsFor name = case name of
      "add-tag"      -> object ["tag" .= ("work" :: Text)]
      "remove-tag"   -> object ["tag" .= ("work" :: Text)]
      "rename-tag"   -> object ["from" .= ("work" :: Text), "to" .= ("projects" :: Text)]
      "set-state"    -> object ["keyword" .= ("DONE" :: Text)]
      "set-planning" -> object ["keyword" .= ("SCHEDULED" :: Text), "date" .= ("+1d" :: Text)]
      "set-title"    -> object ["title" .= ("Water" :: Text)]
      "set-priority" -> object ["priority" .= ("A" :: Text)]
      "edit-link"    -> object [ "span" .= [linkFrom, linkFrom + T.length linkLiteral]
                               , "target" .= ("https://y.example" :: Text) ]
      _takesNoArgs   -> object []

    -- Write ID's blob under DIR's store and answer its path.  The layout is the
    -- LIBRARY's, so the fixture shards an id the way the writer does.
    blobIn dir ident text = do
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path text
      pure path
      where path = blobPathIn (storeRootIn dir) ident

    rowOf dir name text = do
      loaded <- loadFile =<< orgFile dir name text
      case loaded of
        Right [r] -> pure r
        other -> assertFailure (name <> " is no longer one row: " <> show (fmap length other))

    theRepeat = maybe (assertFailure "the fixture no longer repeats") pure

    -- Every Haskell file this package builds from, the vendored GTK bindings
    -- out: they are upstream's and are not built unless @-f native-window@ is.
    sweptSources =
      concat <$> mapM under ["src", "src-query", "src-web", "src-desktop-native", "app"]
      where
        under dir = do
          entries <- map (dir </>) <$> listDirectory dir
          files <- filterM doesFileExist entries
          nested <- mapM under =<< filterM doesDirectoryExist entries
          pure (filter ((== ".hs") . takeExtension) files <> concat nested)

    -- TestSelfContained's own @calls@ idiom: PATH once per non-comment line
    -- naming SYMBOL, so a caller is NAMED rather than counted.
    callsIn symbol path = report . T.lines <$> TIO.readFile path
      where
        report ls = [ path | l <- ls, let stripped = T.strip l
                           , symbol `T.isInfixOf` stripped
                           , not ("--" `T.isPrefixOf` stripped) ]

    -- The @ghc-options:@ line under STANZA's own header.  By whole stripped
    -- lines rather than by 'T.breakOn': @executable glance@ is a prefix of
    -- @executable glance-wasm-probe@, which declares no RTS options at all.
    ghcOptionsOf stanza body = case dropWhile (/= stanza) (map T.strip (T.lines body)) of
      _header : rest -> firstOf rest
      [] -> ""
      where
        key = "ghc-options:"
        firstOf ls = case [ T.drop (T.length key) l | l <- ls, key `T.isPrefixOf` l ] of
          (opts : _) -> opts
          _noneUnderIt -> ""

-- | Walk: what the denylist names, where each name is asked for, and what the
-- walk records when it declines a directory.
--
-- The registries are the SPEC's ("AGENTS") and the predicates are the code's,
-- so each case is a comparison rather than a second copy: a name added to
-- 'Spec.everyDenied' and not to 'Data.Org.Walk' fails here, and so does the
-- reverse.
specGroup05 :: TestTree
specGroup05 = testGroup "Walk"
  [ testCase "the denylist names five, and data is not one of them" $ do
      assertEqual "the spec's denylist and the walk's own names have drifted"
                  ["overviews", metaDir, trashDir, "occurrences", configDir]
                  (map Spec.deniedName Spec.everyDenied)
      assertEqual "a mirror name the walk no longer declines" []
                  [ n | n <- ["overviews", metaDir, trashDir]
                      , not (isDerived (orgGlanceDir </> n </> "x.org")) ]
      assertBool "the config area is no longer declined"
                 (isConfig (orgGlanceDir </> configDir </> "x.org"))
      assertBool "a blob's history is no longer declined"
                 (isDerived (blobStore </> "occurrences" </> "s.org"))
      assertBool "the blob itself is declined — data is on the denylist"
                 (not (isDerived (blobStore </> blobFile))
                    && not (isConfig (blobStore </> blobFile)))

    -- @occurrences@ is asked for ANYWHERE under @data@, a two-character id
    -- being unsharded; the other four are asked for directly under
    -- @.org-glance@ and mean nothing further in.
  , testCase "the blob's history is the one name asked for anywhere under data" $ do
      assertEqual "an occurrence directory the walk no longer declines" []
                  [ p | p <- [ blobStore </> "occurrences" </> "s.org"
                             , orgGlanceDir </> storeDir </> "ab" </> "occurrences" </> "s.org" ]
                      , not (isDerived p) ]
      assertBool "`overviews' under data is declined — the mirror names are asked \
                 \for directly under .org-glance alone"
                 (not (isDerived (orgGlanceDir </> storeDir </> "ab" </> "overviews" </> "x.org")))
      assertBool "`config' under data is declined, where the name is asked for \
                 \directly under .org-glance alone"
                 (not (isConfig (orgGlanceDir </> storeDir </> "ab" </> configDir </> "x.org")))

  , testCase "--include-derived lifts every denial but the config one" $ do
      sweptSample
      assertEqual "a path the spec's denial rule and the walk's predicates read \
                  \differently" []
                  [ p | p <- Spec.wSample
                      , isJust (Spec.deniedBy Spec.walkKept p) /= (isDerived p || isConfig p) ]
      assertEqual "--include-derived no longer leaves the config denial standing \
                  \alone" []
                  [ p | p <- Spec.wSample
                      , isJust (Spec.deniedBy Spec.walkAll p) /= isConfig p ]

    -- The occurrence carries the LIVE entry's own @ORG_GLANCE_ID@, so ranking
    -- it canonical made the pair a tie and walk order decided which document a
    -- command wrote to.
  , testCase "an occurrence loses the id rather than tying with its blob" $ do
      let blob = blobStore </> blobFile
          occurrence = blobStore </> "occurrences" </> "2026-08-02.org"
      assertEqual "the blob no longer takes the id off its own history"
                  (True, (blob, occurrence)) (claimById blob occurrence)
      assertEqual "the history takes the id when it is challenged first"
                  (False, (blob, occurrence)) (claimById occurrence blob)

    -- Nothing canonicalizes the root, so the rule is over the path AS TYPED.
  , testCase "the exclusion is TEXTUAL: a root inside the tree defeats it" $ do
      assertBool "a mirror under a spelled .org-glance is walked"
                 (isDerived (orgGlanceDir </> "overviews" </> "x.org"))
      assertEqual "a path that never spells .org-glance is declined" []
                  [ p | p <- ["overviews" </> "x.org", "." </> "overviews" </> "x.org"]
                      , isDerived p ]
      assertBool "a config directory nothing roots in .org-glance is declined"
                 (not (isConfig (configDir </> "tags" </> "x.org")))

  , testCase "isWalked is visit's own conjunction" $ do
      sweptSample
      assertEqual "a path the spec's walk and the library's isWalked read \
                  \differently" []
                  [ p | p <- Spec.wSample, Spec.isWalked p /= isWalked p ]

    -- One predicate serves the walk and the watch, so a file the walk never
    -- loaded cannot arrive by inotify.
  , testCase "the watch reaches these rules through the facade, never a second copy" $ do
      sweptSample
      assertEqual "a path the facade and the walk answer differently" []
                  [ p | p <- Spec.wSample
                      , (derivedPath p, documentPath p, configPath p)
                          /= (isDerived p, isDocument p, isConfig p) ]

    -- 'foundDerived' and 'foundConfig' hold DIRECTORIES: a file under a
    -- declined one is dropped with no record of its own, the directory never
    -- being entered.
  , testCase "a declined directory is recorded, a file dropped on its path is not" $
      withTempDirNamed "walk-denied" $ \root -> do
        let store  = root </> orgGlanceDir
            mirror = store </> "overviews"
            config = store </> configDir
        mapM_ (createDirectoryIfMissing True) [mirror </> "deep", config]
        _ <- orgFile root "notes.org" "* TODO one\n"
        _ <- orgFile (mirror </> "deep") "m.org" "* DONE the mirror\n"
        _ <- orgFile config "system.org" "#+TODO: TODO | DONE\n"
        found <- findOrgFilesWith defaultWalk [root]
        assertEqual "the declined mirror DIRECTORY went unrecorded"
                    [mirror] (foundDerived found)
        assertEqual "the declined config DIRECTORY went unrecorded"
                    [config] (foundConfig found)
        assertEqual "a file under a declined directory reached the walk's files"
                    [root </> "notes.org"] (foundFiles found)

    -- A root the walk cannot read is reported through 'foundDirErrs'; a
    -- symlinked directory is never followed and raises nothing, whether its
    -- name would have kept it or not.
  , testCase "a root the walk cannot read IS reported, a symlinked directory vanishes" $
      withTempDirNamed "walk-roots" $ \base -> do
        let tree = base </> "tree"
            away = base </> "away"
        mapM_ (createDirectoryIfMissing True) [tree, away]
        _ <- orgFile tree "notes.org" "* TODO one\n"
        _ <- orgFile away "unreachable.org" "* TODO two\n"
        createSymbolicLink away (tree </> "dirlink")
        createSymbolicLink away (tree </> "dirlink.org")
        gone <- findOrgFilesWith defaultWalk [base </> "nowhere"]
        assertEqual "a root that is not there went unreported"
                    [base </> "nowhere"] (map fst (foundDirErrs gone))
        linked <- findOrgFilesWith defaultWalk [tree]
        assertEqual "a symlinked directory raised an error" []
                    (map fst (foundDirErrs linked))
        assertEqual "a symlinked directory was followed"
                    [tree </> "notes.org"] (foundFiles linked)

    -- The ordinal numbers EMITTED rows, so an entry going blank spends none;
    -- an id names its row whatever stands above it.
  , testCase "an ORG_GLANCE_ID is the only immunity" $ do
      let entry = "* one\n:PROPERTIES:\n:ORG_GLANCE_ID: x\n:END:\n"
      withDoc "walk-id" "notes.org" entry $ \alone ->
        withDoc "walk-id" "notes.org" ("* \n" <> entry) $ \withBlank -> do
          assertEqual "the id-carrying row is no longer named by its id"
                      ["x"] (map hrId alone)
          assertEqual "an entry going blank ahead of it renamed the row"
                      (map hrId alone) (map hrId withBlank)

    -- A walked path always ends in its @.org@ extension, so the separator needs
    -- no rule of its own.
  , testCase "FILE#K is recoverable at its LAST hash" $ do
      assertEqual "the ordinal id is spelled another way"
                  "a#b/n.org#3" (rowIdIn "a#b/n.org" 3)
      assertEqual "a file name carrying a hash no longer splits at the last one"
                  ("a#b/n.org#", "3") (T.breakOnEnd "#" (rowIdIn "a#b/n.org" 3))

    -- Nothing parses a row id apart, so an ORG_GLANCE_ID spelling one is an
    -- ordinary duplicate and is reported as one.
  , testCase "an id spelling another row's FILE#K is an ordinary collision" $
      withTempDirNamed "walk-collide" $ \dir -> do
        let ordinal = rowIdIn (dir </> "a.org") 0
        _ <- orgFile dir "a.org" "* TODO one\n"
        _ <- orgFile dir "b.org"
               ("* TODO two\n:PROPERTIES:\n:ORG_GLANCE_ID: " <> ordinal <> "\n:END:\n")
        qr <- loadDir dir
        assertEqual "the ordinal spelling bought itself a special case"
                    1 (length (qrIdCollisions qr))
        assertEqual "and the collision is reported under something other than the \
                    \ordinal"
                    [ordinal] (map icId (qrIdCollisions qr))

    -- The name is asked for anywhere under @data@, so an id whose REMAINDER
    -- spells it takes its own blob out of the walk.
  , testCase "the depth is left open: a remainder spelling occurrences is declined" $ do
      let store = storeRootIn "/t"
      assertBool "a blob whose id remainder spells occurrences is walked"
                 (isDerived (blobPathIn store "xyoccurrences"))
      assertBool "an ordinary blob is declined"
                 (not (isDerived (blobPathIn store "xy1234")))
  ]
  where
    blobStore = orgGlanceDir </> storeDir </> "ab" </> "cd"
    -- A sweep over nothing passes, so each one says what it swept.
    sweptSample = assertBool ("too few paths swept: " <> show (length Spec.wSample))
                             (length Spec.wSample >= 10)

-- Keyword configuration
--
-- The registries a tree's configuration is read through, pinned as DATA: the
-- saved views, the tree-wide settings, and the two folds that read a
-- @system.org@ line.  Each case is an equality against the real registry rather
-- than a property over it, so a member added, renamed or rescoped fails here
-- and names itself.

-- | A @system.org@ layer at PATH holding TEXT.  The digest is the path's own —
-- 'treeSettings' reads neither, and two layers still have to be told apart.
systemLayer :: FilePath -> Text -> ConfigLayerFile
systemLayer path = ConfigLayerFile path Nothing (T.pack path)

specGroup06 :: TestTree
specGroup06 = testGroup "Keyword configuration"
  [ testCase "a keyword token is letters and underscores" $
      -- The POSITIVE half of 'Data.Org.Parser.keywordTextP''s charset; the
      -- suite's other cases are the starred refusals, which say what it lacks.
      assertEqual "an underscored keyword no longer parses as one"
        (TodoKeywords ["IN_PROGRESS"] ["DONE"])
        (todoPragmas "#+TODO: IN_PROGRESS | DONE\n")

  , testCase "three saved views, each with its id, pragma and built-in" $
      assertEqual "the saved view registry moved"
        [ ("default", "GLANCE_DEFAULT_FILTER", "state:*active*")
        , ("agenda",  "GLANCE_AGENDA_FILTER",  "state:*active* -planned:*empty* sort:scheduled")
        , ("archive", "GLANCE_ARCHIVE_FILTER", "tag:*archive*") ]
        [ (svId v, svPragma v, svBuiltin v) | v <- savedViews ]

  , testCase "every saved view is a GLANCE_ pragma" $
      -- Quantified over the registry, so a fourth view is covered with no case
      -- of its own.
      mapM_ (\v -> assertBool (T.unpack (svId v) <> " names a pragma outside the GLANCE_ namespace: "
                                 <> T.unpack (svPragma v))
                              ("GLANCE_" `T.isPrefixOf` svPragma v))
            savedViews

  , testCase "a view no build carries is the empty query" $
      -- The READ fallback for an unknown id; the WRITE refuses it by name.
      assertEqual "an id no build carries read back as something other than the empty query"
        "" (viewQueryIn "notes" noTreeSettings)

  , testCase "views take the first system layer that names one" $
      assertEqual "a second config directory outranked the first"
        [("default", "tag:x")]
        (tsViews (treeSettings
          [ systemLayer "a/system.org" "#+GLANCE_DEFAULT_FILTER: tag:x\n"
          , systemLayer "b/system.org" "#+GLANCE_DEFAULT_FILTER: tag:y\n" ]))

  , testCase "the colours take every system layer's lines" $
      -- The asymmetry inside the one fold: a view is the first layer's, a hue
      -- every layer's, since a tree names one line per theme.
      assertEqual "a system layer's hues were dropped"
        [("light", [("TODO", "#111")]), ("dark", [("TODO", "#eee")])]
        (tsColors (treeSettings
          [ systemLayer "a/system.org" "#+GLANCE_STATE_COLORS: light TODO=#111\n"
          , systemLayer "b/system.org" "#+GLANCE_STATE_COLORS: dark TODO=#eee\n" ]))

  , testCase "a keyword named twice takes its last spelling" $
      assertEqual "a repeated keyword kept a hue other than its last"
        [("light", [("TODO", "#2")])]
        (stateColorsOf "#+GLANCE_STATE_COLORS: light TODO=#1\n\
                       \#+GLANCE_STATE_COLORS: light TODO=#2\n")

  , testCase "three settings, named and scoped as written" $
      -- Names and scopes together: the list is non-empty and its names unique
      -- by being this list, and a member rescoped fails the equality rather
      -- than slipping past a mask nobody wrote a case for.
      assertEqual "the config setting registry moved"
        [("views", TreeWide), ("colors", TreeWide), ("template", PerLayer)]
        [ (csName s, csScope s) | s <- configSettings ]
  ]

-- | The store as the wire sees it, the watch step that keeps it current, and
-- the HTTP surface over both.  The route table is compared against the model's
-- own registry ('AGENTS.routes') rather than restated here, so a route added on
-- either side fails the build.
specGroup07 :: TestTree
specGroup07 = testGroup "Store, watch, HTTP surface"
  [ testCase "stTags counts files, so two rows of one file are one vote" $
      withStoreOf [("a.org", "* TODO one :web:\n* TODO two :web:\n")] $ \_dir _path st ->
      assertEqual "one vote per file" (Just 1) (Map.lookup "web" (stTags st))

    -- The fingerprint stamps each file with its FIRST row's digest, so a file
    -- that contributes none contributes its path and an empty stamp.  Rewriting
    -- it moves no tag, and a client's 304 survives an edit no answer carries.
  , testCase "a rowless file stands in the fingerprint as its path alone" $
      withTempDir $ \dir -> do
      _ <- orgFile dir "empty.org" "#+TITLE: one\n"
      before <- stPrint <$> loadStore dir
      _ <- orgFile dir "empty.org" "#+TITLE: two\n"
      after <- stPrint <$> loadStore dir
      assertEqual "a file contributing no rows is its path alone" before after

  , testCase "the ETag is the fingerprint's first sixteen and the generation" $ do
      st <- loadStore viewDir
      a <- app assetsDir
      r <- getFrom a "/headlines"
      assertEqual "the tag is stPrint's first sixteen and stGen"
                  (Just ("\"" <> TE.encodeUtf8 (T.take 16 (stPrint st))
                           <> "-g" <> BSC.pack (show (stGen st)) <> "\""))
                  (header "ETag" r)

    -- b.org keeps the palette standing, so what the step produces is rows
    -- rather than the `view-changed' a moved keyword set would replace them
    -- with.  Blanking the first entry renumbers the second onto its ordinal:
    -- one upsert, one delete, and the upsert goes first.
  , testCase "upserts lead deletes in a step" $
      withStoreOf [ ("a.org", "* TODO one\n* TODO two\n")
                  , ("b.org", "* TODO keeps the palette\n") ] $ \_dir path store -> do
      (_next, frames) <- rewrite path "* \n* TODO two\n" store
      assertEqual "upserts lead deletes"
                  ([f | f@(Op (UpsertRow _)) <- frames] <> [f | f@(Op (DeleteRow _)) <- frames])
                  frames
      assertBool ("the step produced both, got " <> show frames) (length frames == 2)

  , testCase "the close vocabulary is two words, and they are distinct" $ do
      let spoken = map closeReason [minBound .. maxBound :: CloseReason]
      assertEqual "the whole vocabulary of a server-initiated close"
                  ["view-changed", "resync"] spoken
      assertEqual "each reason spells a word of its own" (nub spoken) spoken

    -- POST is a method @/headlines@ never takes, so a loaded store answers it
    -- 405.  While the walk runs the gate answers first.
  , testCase "the load gate runs ahead of the method check" $ do
      a <- loadingApp
      r <- postTo a "/headlines" ""
      assertEqual "the gate answers a method the route refuses" 503 (status r)
      assertEqual "Retry-After" (Just "1") (header "Retry-After" r)

  , testCase "HEAD is GET's, once" $ do
      a <- app assetsDir
      g <- getFrom a "/headlines"
      h <- runSession (request (setPath defaultRequest "/headlines")
                         { requestMethod = methodHead }) a
      assertEqual "status" (status g) (status h)
      assertEqual "ETag" (header "ETag" g) (header "ETag" h)
      assertEqual "X-Glance-Rows" (header "X-Glance-Rows" g) (header "X-Glance-Rows" h)

  , testCase "only / serves while the walk runs" $ do
      a <- loadingApp
      mapM_ (\r -> do
               resp <- getFrom a (BSC.pack (rPath r))
               assertEqual (rPath r <> ": while the store loads")
                           (if rNeeds r then 503 else 200) (status resp))
            routes

  , testCase "a route spells its 405 in JSON exactly where it takes a POST" $ do
      a <- app assetsDir
      mapM_ (\r -> do
               resp <- runSession (request (setPath defaultRequest (BSC.pack (rPath r)))
                                     { requestMethod = methodDelete }) a
               assertEqual (rPath r <> ": status") 405 (status resp)
               assertEqual (rPath r <> ": refusal kind")
                           (rRefusal r == JsonRefusal)
                           (isJust (decode (simpleBody resp) :: Maybe Value)))
            routes

  , testCase "every route takes GET but /command" $ do
      a <- app assetsDir
      mapM_ (\r -> do
               resp <- getFrom a (BSC.pack (rPath r))
               assertEqual (rPath r <> ": takes GET")
                           (GET `elem` rMethods r) (status resp /= 405))
            routes

  , testCase "the route table names each path once" $ do
      a <- app assetsDir
      r <- getFrom a "/no/such/route"
      let listed = T.splitOn ", " (T.drop (T.length "not found: ")
                     (T.replace ", or an asset name" "" (T.stripEnd (body r))))
      assertEqual "the spec's paths are the served table's"
                  (map (T.pack . rPath) routes) listed
      assertEqual "and each path once" (nub listed) listed

  , testCase "the 405 sentence and the 404 list are derived from the table" $ do
      a <- app assetsDir
      miss <- getFrom a "/no/such/route"
      assertEqual "the 404 names the table" (T.pack notFoundText) (T.stripEnd (body miss))
      bad <- getFrom a "/command"
      assertContains "the 405 sentence is the entry's own"
                     (T.pack (maybe "" takesText (routeAt "/command"))) (body bad)

  , testCase "the asset route refuses the traversal pair and a separator" $ do
      a <- app assetsDir
      assertEqual "a real asset" 200 . status =<< getFrom a "/table-view.js"
      mapM_ (\p -> assertEqual (BSC.unpack p <> ": is no asset name") 404 . status
                     =<< getFrom a p)
            ["/.", "/..", "/%2Fetc%2Fpasswd", "/a%5Cb"]

  , testCase "a 304 carries the route's ETag and Cache-Control and nothing else" $ do
      a <- app assetsDir
      tag <- etagOf =<< getFrom a "/headlines"
      nm <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 304 (status nm)
      assertEqual "ETag" (Just tag) (header "ETag" nm)
      assertEqual "Cache-Control" (Just "no-cache") (header "Cache-Control" nm)
      mapM_ (\h -> assertEqual (h <> " rides the 200 alone")
                               Nothing (header (fromString h) nm))
            (statsHeaders <> pageHeaders)

  , testCase "warp lengths the 304 and the asset; everything else lengths itself" $ do
      a <- app assetsDir
      mapM_ (\p -> do
               r <- getFrom a p
               assertEqual (BSC.unpack p <> ": Content-Length")
                           (Just (BSC.pack (show (BL.length (simpleBody r)))))
                           (header "Content-Length" r))
            ["/headlines", "/", "/no/such/route"]
      tag <- etagOf =<< getFrom a "/headlines"
      nm <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "the 304 leaves its length to warp" Nothing (header "Content-Length" nm)
      file <- getFrom a "/table-view.js"
      assertEqual "and so does a file off the assets directory"
                  Nothing (header "Content-Length" file)
      idx <- loadingApp
      s <- getFrom idx "/headlines"
      assertEqual "the 503 lengths itself too"
                  (Just (BSC.pack (show (BL.length (simpleBody s)))))
                  (header "Content-Length" s)

  , testCase "413 outranks 404" $
      withCommitted $ \a _path _v -> do
      r <- postTo a (headlinePath "no-such-headline")
                    (BL.fromStrict (BS.replicate (bodyCap + 1) 0x78))
      assertEqual "the cap outranks the id lookup" 413 (status r)

  , testCase "a refused write nudges nothing" $
      withCommittedHub $ \a hub _path v -> do
      org <- textAt "org" v
      r <- postTo a (headlinePath "first") (commitBody org (T.replicate 64 "0"))
      assertEqual "status" 409 (status r)
      assertEqual "a refused write queues no path" [] . Map.keys
        =<< readTVarIO (hubPending hub)

    -- The composed shapes are trimmed where they are composed; the raw @{org}@
    -- door hands a whole document back, so the trim is owed at the route.
  , testCase "the raw {org} door trims the trailing run like the composers" $
      withCommitted $ \a path v -> do
      org <- textAt "org" v
      digest <- textAt "digest" v
      assertOk =<< postTo a (headlinePath "first")
                            (commitBody (org <> "typed over  \t\n") digest)
      after <- document path
      assertBool ("a line kept its trailing run: " <> show after)
                 (not (any (\l -> l /= T.stripEnd l) (T.lines after)))

  , testCase "existence decides a deletion, whatever the event was" $
      withStoreOf [("a.org", "* TODO one\n"), ("b.org", "* TODO two\n")]
      $ \dir path store -> do
      hub <- newHub store
      removeFile path
      settle defaultWalk dir hub [path]
      next <- readTVarIO (hubStore hub)
      assertEqual "the rows under the missing path are gone" []
                  (map hrId (recordsUnder path next))
      assertEqual "and the other file stands" 1 (length (storeRecords next))
  ]
  where
    assetsDir :: FilePath
    assetsDir = "test/fixtures/assets"

    -- | The options a server over the suite's fixture directory runs.
    served :: FilePath -> ServeOptions
    served assets = ServeOptions { soDir = viewDir, soPort = defaultPort
                                 , soAssets = Just assets, soDerived = False }

    -- | The app a server with ASSETS runs over a store already loaded.
    app :: FilePath -> IO Application
    app assets = application (served assets) <$> (newHub =<< loadStore viewDir)

    -- | The same app with the startup walk still running.
    loadingApp :: IO Application
    loadingApp = application (served assetsDir) <$> (newLoadingHub =<< getMonotonicTime)

    getFrom :: Application -> ByteString -> IO SResponse
    getFrom a path = getWith a path []

    getWith :: Application -> ByteString -> RequestHeaders -> IO SResponse
    getWith a path headers =
      runSession (request (setPath defaultRequest path) { requestHeaders = headers }) a

    header :: HeaderName -> SResponse -> Maybe ByteString
    header name r = lookup name (simpleHeaders r)

    body :: SResponse -> T.Text
    body = TE.decodeUtf8 . BL.toStrict . simpleBody

    etagOf :: SResponse -> IO ByteString
    etagOf r = maybe (assertFailure "no ETag on the response") pure (header "ETag" r)

    -- | @\/headline?id=…@ with ID percent-encoded, the way a client builds it.
    headlinePath :: T.Text -> ByteString
    headlinePath rid = "/headline" <> renderQuery True [("id", Just (TE.encodeUtf8 rid))]

    commitBody :: T.Text -> T.Text -> BL.ByteString
    commitBody org digest = encode (object ["org" .= org, "digest" .= digest])

    bodyCap :: Int
    bodyCap = 1024 * 1024

    -- | Run K over a store loaded from a directory holding FILES, handing it
    -- the directory, the FIRST file's path and the store.
    withStoreOf :: [(FilePath, T.Text)] -> (FilePath -> FilePath -> Store -> IO a) -> IO a
    withStoreOf files k = withTempDir $ \dir -> do
      paths <- mapM (uncurry (orgFile dir)) files
      case paths of
        path : _ -> k dir path =<< loadStore dir
        []       -> assertFailure "withStoreOf: a store of no files says nothing"

    -- | Rewrite PATH with TEXT and fold the re-read into STORE: one watch step.
    rewrite :: FilePath -> T.Text -> Store -> IO (Store, [Frame])
    rewrite path text store = do
      TIO.writeFile path text
      applyFile path <$> loadFile path <*> pure store

    -- | A file whose first headline is materialized, edited and written back.
    committable :: T.Text
    committable = T.unlines
      [ "#+CATEGORY: notes"
      , "* TODO First :one:"
      , ":PROPERTIES:"
      , ":ORG_GLANCE_ID: first"
      , ":END:"
      , "body of first"
      , "* TODO Second"
      , "tail" ]

    -- | A server holding 'committable' with its first headline materialized:
    -- the app, the hub behind it, the file on disk, and the answer a commit has
    -- to present back.
    withCommittedHub :: (Application -> Hub -> FilePath -> Value -> Assertion) -> Assertion
    withCommittedHub k = withTempDir $ \dir -> do
      path <- orgFile dir "notes.org" committable
      (a, hub) <- serverAt (Just assetsDir) dir
      v <- decoded =<< getFrom a (headlinePath "first")
      k a hub path v

    withCommitted :: (Application -> FilePath -> Value -> Assertion) -> Assertion
    withCommitted k = withCommittedHub (\a _hub path v -> k a path v)

    decoded :: SResponse -> IO Value
    decoded r = either (\e -> assertFailure ("response JSON: " <> e)) pure
                       (eitherDecode (simpleBody r))

-- | The query language's four registries — the view's columns, the tokens that
-- state a VIEW rather than narrow one, the default order, and the column set a
-- @columns:@ token picks — each read against @AGENTS@'s model of it.
--
-- The spec's tables are plain data, so a case here compares the two lists
-- rather than restating either: a column, a view token or a chain key that
-- moves on one side alone fails the build.  What the spec cannot hold — a
-- record, a loaded fixture, the encoder's own JSON — is asked of the real code
-- directly.
specGroup08 :: TestTree
specGroup08 = testGroup "Query language"
  [ testCase "viewColumns names every column once, in order" $
      assertEqual "the spec's column table and the served view's have drifted"
        [ (key, header, kind) | (key, header, kind, _cell) <- Q.viewColumns ]
        [ (T.pack (cKey c), T.pack (cHead c), kindWord c) | c <- viewColumns ]

    -- The whole-tag meta is keyed by the CELL's index rather than by the key
    -- name, which is what puts it out of `planned''s reach: `planned' names the
    -- two date cells, so a starred value there is an ordinary prefix and
    -- matches nothing.
  , testCase "the whole-tag meta is keyed by the cell, so planned cannot reach it" $
      withDocDir "spec-query" "a.org"
                 "* filed :archive:\nSCHEDULED: <2026-08-01 Sat>\n" $ \recs ->
        case recs of
          []      -> assertFailure "the one-entry fixture loaded no rows"
          (r : _) -> do
            assertBool "tag:*archive* no longer reads the whole tag"
                       (matchesFilter emptyEnv "tag:*archive*" r)
            assertBool "planned:*archive* reached the tags cell's meta"
                       (not (matchesFilter emptyEnv "planned:*archive*" r))
            assertBool "planned: no longer prefix-matches a date cell"
                       (matchesFilter emptyEnv "planned:2026-08" r)

    -- Both halves over the SAME rows and the same absurd value, so the two
    -- lists differ in nothing but the key: a view token leaves the set whole
    -- and every predicate key empties it.
  , testCase "the three view tokens narrow nothing, and everything else narrows" $ do
      recs <- Q.qrRecords <$> Q.loadDir viewDir
      assertBool ("too few fixture rows swept: " <> show (length recs))
                 (length recs >= 6)
      assertEqual "a view token that narrowed the answer" []
        [ k | k <- [sortKey, columnsKey, "view"]
            , length (filter (matchesFilter emptyEnv (k <> ":zzzz")) recs)
                /= length recs ]
      assertEqual "a predicate key that narrowed nothing" []
        [ k | k <- filterKeys <> [plannedKey, substringKey, refKey]
            , not (null (filter (matchesFilter emptyEnv (k <> ":zzzznosuchvalue"))
                                recs)) ]

  , testCase "the view tokens are sort, columns and view" $ do
      assertEqual "the keys the parser resolves a view token to"
        (map (Just . T.pack . fst) viewKeys)
        [ tmKey t | t <- parseFilter "sort:x columns:y view:z" ]
      assertEqual "the spec's first two view keys and the reader's"
        [sortKey, columnsKey] (take 2 (map (T.pack . fst) viewKeys))

  , testCase "separators are & space tab newline" $
      assertEqual "the token separators moved" ["a", "b", "c", "d", "e"]
                  (map tkBody (scanQuery "a&b c\td\ne"))

  , testCase "and a carriage return is not one" $
      assertEqual "\\r became a separator" [Token False False "a\rb"]
                  (scanQuery "a\rb")

  , testCase "the default chain is state, title, deadline, scheduled, all ascending" $
      assertEqual "the spec's default chain and the served view's have drifted"
        Q.defaultSortChain
        [ (T.pack (keyOf c), d == Asc) | (c, d) <- defaultSortChain ]

    -- The palette names two of the four keywords, so `WAIT' and `HOLD' rank one
    -- past its end and TIE — and the sort being stable, the tie keeps walk
    -- order rather than falling to the text.
  , testCase "an unlisted keyword ties at the back of the palette" $
      withRows (T.unlines [ "#+TODO: TODO WAIT HOLD | DONE"
                          , "* TODO a", "* WAIT b", "* DONE c", "* HOLD d" ]) $ \recs -> do
        assertEqual "the fixture no longer loads four rows" 4 (length recs)
        assertEqual "an unlisted keyword no longer ties at the back"
          [Just "TODO", Just "DONE", Just "WAIT", Just "HOLD"]
          (map Q.hrState
               (Q.sortedForViewWith palette [("state", True)] recs))

  , testCase "empty cells sort last, outside the direction" $
      withRows (T.unlines ["* plain", "* TODO a", "* DONE b"]) $ \recs -> do
        assertEqual "the fixture no longer loads three rows" 3 (length recs)
        assertEqual "an empty cell followed the direction" [Nothing, Nothing]
          [ last (map Q.hrState (Q.sortedForViewWith palette [("state", asc)] recs))
          | asc <- [True, False] ]

    -- The dedup folds the name as WRITTEN, and `Tags' is the tag column's
    -- header where `tag' is its key: two spellings that fold apart, so both
    -- survive the grammar and both resolve to one column.
  , testCase "the dedup reads the name as written, so key and header pick twice" $ do
      assertEqual "columns:Tags,tag" (Right (Just ["Tags", "tag"]))
                  (columnNamesIn "columns:Tags,tag")
      assertEqual "the header and the key no longer pick the same column twice"
        ["title", "tag", "tag"]
        [ key | (key, _h, _kind, _cell) <- Q.resolveColumns ["Tags", "tag"] ]

  , testCase "an unknown name is a custom column: key folded, header as written, text" $
      assertEqual "a custom column's key, header or kind moved"
        [("title", "Title", "text"), ("effort", "Effort", "text")]
        [ (key, header, kind)
        | (key, header, kind, _cell) <- Q.resolveColumns ["Effort"] ]

  , testCase "a custom column is no filter key and no chain key" $ do
      assertEqual "columns:Effort" (Right (Just ["Effort"]))
                  (columnNamesIn "columns:Effort")
      assertBool "sort:effort was accepted, so a custom column orders"
                 (either (const True) (const False) (sortChainIn "sort:effort"))
      assertEqual "effort:x resolved to a filter key" [Nothing]
                  [ tmKey t | t <- parseFilter "effort:x" ]

    -- The title column is INJECTED ahead of a set that names it nowhere, so the
    -- four rows read are title, state, tag, effort.
  , testCase "extras ride the key and a custom column has none" $ do
      view <- decoded (Q.viewJSONTextFor (Q.resolveColumns ["state", "tag", "Effort"])
                                         [] Q.defaultSortChain "t" palette [])
      keys <- mapM keysOf =<< listAt "columns" view
      assertEqual "a column's extras no longer ride its key"
        [ ["header", "key", "sortable", "type"]
        , ["badges", "header", "key", "sortable", "type", "values"]
        , ["header", "key", "multi", "sortable", "type"]
        , ["header", "key", "sortable", "type"] ]
        keys
  ]
  where
    -- The spec's column kind as SCHEMA.md's word for it.
    kindWord :: Column -> Text
    kindWord c = if cCellKind c == KBadge then "badge" else "text"

    -- The palette the badge column is ranked against: two keywords, so a third
    -- a fixture declares is one this does not name.
    palette :: Q.TodoKeywords
    palette = Q.TodoKeywords ["TODO"] ["DONE"]

    withRows doc k = withDoc "spec-query" "a.org" doc k

    -- The wire's own encoder read back, so what is asserted is the JSON a
    -- client receives rather than the 'Data.Aeson.Value' behind it.
    decoded lazy = either (assertFailure . ("the view JSON did not parse: " <>)) pure
                          (eitherDecode (TLE.encodeUtf8 lazy))

    -- V's field names, sorted: field order is the encoder's business and no
    -- part of the contract.
    keysOf (Object o) = pure (sort (map Key.toText (KM.keys o)))
    keysOf other = assertFailure ("expected a column object, got " <> show other)

-- | The one command route's table, the request shapes it refuses before a byte
-- moves, and the palette every answer is drawn in.
--
-- The registries are the SPEC's (@AGENTS@) and the answers are the code's, so a
-- case compares the two rather than restating either: a command, a nullable
-- field, a role or a theme that moves on one side alone fails the build.  What
-- the spec cannot hold — a refusal's own words, a loaded record, the CSS the
-- emitters produce — is asked of the real code directly.
specGroup09 :: TestTree
specGroup09 = testGroup "Commands and writes"
  [ testCase "eleven commands, each spelled once" $ do
      assertEqual "the spec's registry and the route's table have drifted"
        (sort (map (T.pack . Spec.cWire) Spec.cmds)) (sort commandNames)
      assertEqual "the command count moved" 11 (length commandNames)

    -- The ids wall runs off the KIND, ahead of the entry's own @csArgs@, so a
    -- request naming nothing but a name reaches it for every command that owes
    -- rows and for none that does not.
  , testCase "capture is the one id-less command" $ do
      assertEqual "the spec no longer names capture the one id-less command"
        ["capture"] idless
      withCommandServer $ \app ->
        forM_ commandNames $ \n -> do
          r <- postTo app "/command" (encode (object ["name" .= n]))
          assertEqual (T.unpack n <> ": the ids wall and the spec's Ids disagree")
            (n `elem` idless) (not ("a command names rows" `T.isInfixOf` bodyText r))

    -- @edit-link@'s args name a row's own TEXT, so a span means nothing to a
    -- second row and the ROW COUNT is the coarsest thing wrong with the
    -- request — which is why its shape is the one handed the ids.
  , testCase "edit-link is the one naming ONE row" $ do
      assertEqual "the spec no longer names edit-link the one single-row command"
        ["edit-link"] single
      withCommandServer $ \app ->
        forM_ commandNames $ \n -> do
          r <- postTo app "/command"
                 (encode (object ["name" .= n, "ids" .= (["a", "b"] :: [T.Text])]))
          assertEqual (T.unpack n <> ": the row-count wall and the spec's Ids disagree")
            (n `elem` single) ("names one row" `T.isInfixOf` bodyText r)

    -- 'Glance.Query.repeatOn' is the ledger line's whole trigger, and org's
    -- condition is both halves: an INACTIVE keyword and a planning stamp
    -- carrying a repeater.  Either alone is a plain state change.
  , testCase "only set-state's answer records" $ do
      assertEqual "the spec no longer names set-state the one recording command"
        [Spec.SetState] [Spec.cName c | c <- Spec.cmds, Spec.cRecords c]
      withDoc "spec-repeat" "repeat.org" (repeating "<2026-08-01 Sat +1w>") $ \recs ->
        withFirst recs $ \r -> do
          assertBool "an inactive keyword over a repeating stamp no longer records"
            (isJust (repeatOn noConfig today "CANCELLED" r))
          assertEqual "an active keyword records where org calls it a state change"
            noRepeat (repeatOn noConfig today "NEXT" r)
      withDoc "spec-repeat" "plain.org" (repeating "<2026-08-01 Sat>") $ \recs ->
        withFirst recs $ \r ->
          assertEqual "a stamp with no repeater records"
            noRepeat (repeatOn noConfig today "CANCELLED" r)

    -- The @.:!@ \/ @.:?@ split asked of the ROUTE: every arg nulled in turn with
    -- the rest of its request well formed, so a fifth nullable field cannot
    -- arrive unnamed and a Req one cannot quietly start clearing.  The ids name
    -- no row on purpose — what is under test is decided before one is looked up.
  , testCase "one nullable field per clearing command" $ do
      assertEqual "the spec's nullable fields moved"
        [ (Spec.SetState, "keyword"), (Spec.SetPlanning, "date")
        , (Spec.SetPriority, "priority"), (Spec.EditLink, "desc") ]
        [ (Spec.cName c, f) | c <- Spec.cmds, Spec.Arg f Spec.Nul <- Spec.cArgs c ]
      withCommandServer $ \app ->
        forM_ [ (c, f, a) | c <- Spec.cmds, Spec.Arg f a <- Spec.cArgs c ] $ \(c, f, arity) -> do
          r <- postTo app "/command" (encode (object
                 [ "name" .= T.pack (Spec.cWire c)
                 , "ids"  .= (["nosuchrow"] :: [T.Text])
                 , "args" .= object [ fromString k .= v | (k, v) <- nulling f (Spec.cArgs c) ] ]))
          assertEqual (Spec.cWire c <> "." <> f <> ": a null and the field's arity disagree")
            (arity == Spec.Req)
            (status r == 400 && T.pack f `T.isInfixOf` bodyText r)

    -- 'Data.Org.isTagChar' has no star, which is what makes a starred word
    -- undeclarable as a tag and keeps the meta family total.
  , testCase "no starred meta is a legal tag" $ do
      assertBool "the spec's meta family is empty" (not (null Spec.metas))
      assertEqual "a starred meta the tag wall now accepts" []
        [ w | m <- Spec.metas, let w = Spec.metaWord m, not (isLeft (tagText (T.pack w))) ]
      assertBool "the star became a tag character" (not (isTagChar '*'))

    -- The emitters ('pageTokens', 'tableTokens') are private, so the spec's role
    -- table is bound to the CSS they produce rather than to a second list.
  , testCase "every role reaches a namespace" $ do
      assertBool "the spec's role table is empty" (not (null Spec.roles))
      assertEqual "a role reaching neither namespace" []
        [ show r | r <- Spec.roles
                 , isNothing (Spec.pageToken r), isNothing (Spec.tableToken r) ]
      assertEqual "a token the spec names and no theme declares" []
        [ t | t <- mapMaybe Spec.pageToken Spec.roles
                    <> mapMaybe Spec.tableToken Spec.roles
            , not (declares (T.pack t)) ]

  , testCase "no theme moves geometry" $ do
      assertBool "the spec names no geometry token" (not (null Spec.geometryTokens))
      assertEqual "a geometry token a theme now declares" []
        [ t | t <- Spec.geometryTokens, T.pack t `T.isInfixOf` themeCSS ]

  , testCase "one option per theme beside auto" $
      assertEqual "the spec's theme ids and the build's have drifted"
        (map T.pack Spec.themeIds) themeIds

    -- 'Glance.Web.Theme.defaultFor' errors on a mode no theme answers, so the
    -- claim is spelled out here rather than crashed into at request time.
  , testCase "a build carries a theme per mode" $ do
      assertEqual "the build no longer carries exactly one light theme"
        1 (length [ t | t <- themes, thMode t == Light ])
      assertEqual "the build no longer carries exactly one dark theme"
        1 (length [ t | t <- themes, thMode t == Dark ])
      assertEqual "the spec's modes and the build's have drifted"
        [Spec.TLight, Spec.TDark] [ m | Spec.Theme _ _ m <- Spec.themes ]
      assertEqual "a theme the spec and the build spell differently"
        [ T.pack i | Spec.Theme i _ _ <- Spec.themes ] (map thId themes)

    -- A theme fills exactly the slots the WIRE names, however many hues it
    -- declares: fewer repeat, more are never reached.
  , testCase "a slot is its place modulo the wire's count" $ do
      assertEqual "the wire's state slot count moved" Spec.stateSlots stateSlots
      assertEqual "org's priority cycle length moved" Spec.prioritySlots prioritySlots
      assertEqual "a slot past the count no longer wraps"
        (Spec.slotToken "a" 1) (Spec.slotToken "a" (1 + Spec.stateSlots))
      assertEqual "a slot the wire names and no theme declares" []
        [ t | t <- slotTokens, not (declares t) ]
      assertBool "a slot past the wire's count is declared"
        (not (declares ("--g-state-a" <> T.pack (show stateSlots))))

  , testCase "a keyword named twice takes its last spelling" $ do
      assertEqual "the spec's pragma is no longer the one the reader accepts"
        [("light", [("TODO", "#a")])]
        (stateColorsOf (T.pack Spec.stateColorsPragma <> " light TODO=#a\n"))
      assertEqual "a keyword named twice across lines no longer takes the last"
        [("light", [("DONE", "#b"), ("TODO", "#c")])]
        (stateColorsOf "#+GLANCE_STATE_COLORS: light TODO=#a DONE=#b\n\
                       \#+GLANCE_STATE_COLORS: light TODO=#c\n")
      assertEqual "one LINE now dedups its own pairs"
        [("light", [("TODO", "#a"), ("TODO", "#c")])]
        (stateColorsOf "#+GLANCE_STATE_COLORS: light TODO=#a TODO=#c\n")
      -- KNOWN DIVERGENCE: the spec folds last-wins INSIDE a line where
      -- 'Data.Org.Config.stateColorsOf' folds it across lines alone.  Pinned
      -- from both sides so closing it names the decision.
      assertEqual "the spec's per-line fold stopped deduping"
        (Just ("light", [("TODO", "#c")])) (Spec.stateColorsOf "light TODO=#a TODO=#c")

  , testCase "a line naming no theme is no line" $ do
      assertEqual "a valueless line now names a theme" []
        (stateColorsOf "#+GLANCE_STATE_COLORS:\n")
      assertEqual "an empty document now names a theme" [] (stateColorsOf "")
      assertEqual "the spec's reader now takes a line naming nothing"
        Nothing (Spec.stateColorsOf "")
  ]
  where
    idless = [ T.pack (Spec.cWire c) | c <- Spec.cmds, Spec.cIds c == Spec.IdsNone ]
    single = [ T.pack (Spec.cWire c) | c <- Spec.cmds, Spec.cIds c == Spec.IdsOne ]
    noRepeat = Nothing :: Maybe Repeat
    today = fromGregorian 2026 8 8
    slotTokens =
      [ "--g-state-a" <> T.pack (show i) | i <- [0 .. stateSlots - 1] ]
        <> [ "--g-state-i" <> T.pack (show i) | i <- [0 .. stateSlots - 1] ]
        <> [ "--g-priority-" <> T.pack (show i) | i <- [0 .. prioritySlots - 1] ]

-- | Does some theme declare TOKEN?  The emitted block spells @name:value;@, so
-- the colon is what tells @--tv-col@ from @--tv-col-wash@.
declares :: T.Text -> Bool
declares token = (token <> ":") `T.isInfixOf` themeCSS

-- | R's body as the refusals spell it.
bodyText :: SResponse -> T.Text
bodyText = TE.decodeUtf8 . BL.toStrict . simpleBody

-- | A server over a tree of one row, for the cases that ask the ROUTE what it
-- refuses.  A capture that lands writes into the temp directory and goes with it.
withCommandServer :: (Application -> Assertion) -> Assertion
withCommandServer k = withTempDirNamed "spec-command" $ \dir -> do
  _ <- orgFile dir "notes.org" "* TODO first\n"
  (app, _hub) <- serverAt Nothing dir
  k app

-- | K over the first of ROWS, or the failure that says the fixture loaded none.
withFirst :: [a] -> (a -> Assertion) -> Assertion
withFirst [] _ = assertFailure "the fixture loaded no rows"
withFirst (x : _) k = k x

-- | One entry over a cycle naming an active and an inactive word, carrying
-- STAMP: the file's own @#+TODO:@ is the chain 'repeatOn' resets from.
repeating :: T.Text -> T.Text
repeating stamp = T.unlines
  [ "#+TODO: NEXT | CANCELLED", "* NEXT ship", "SCHEDULED: " <> stamp ]

-- | ARGS with FIELD nulled and every other one legal, so a refusal is about the
-- null rather than about what the request left out.
nulling :: String -> [Spec.Arg] -> [(String, Value)]
nulling field args = [ (k, if k == field then Null else argValue k) | Spec.Arg k _ <- args ]

-- | A value each arg name is legal with.  A name the spec grows lands here.
argValue :: String -> Value
argValue "keyword"  = "DONE"
argValue "date"     = "+3d"
argValue "text"     = "TODO buy milk"
argValue "title"    = "buy milk"
argValue "priority" = "A"
argValue "tag"      = "work"
argValue "fields"   = object []
argValue "from"     = "work"
argValue "to"       = "projects"
argValue "span"     = toJSON ([0, 1] :: [Int])
argValue "target"   = "https://example.org"
argValue "desc"     = "the link"
argValue other      = error ("no fixture value for the arg " <> other)

-- | Shell: keys, table gestures, surfaces.
--
-- Both rules here are figures written down TWICE — the log knob in
-- 'Glance.Web.Base' and again in the spec's 'Spec.logKnob', the z band in the
-- stylesheet and again in the spec's 'Spec.zOf' — with the renderer's own
-- levels the third party to the second.  Each case compares the copies rather
-- than restating one of them, so a figure moved on either side fails here.
specGroup10 :: TestTree
specGroup10 = testGroup "Shell"
  [ testCase "the default sits in the band" $ do
      assertEqual "the spec's log knob and Glance.Web.Base's own three have drifted"
                  (logLinesDefault, logLinesMin, logLinesMax)
                  ( Spec.kDef Spec.logKnob
                  , Spec.kMin Spec.logKnob
                  , Spec.kMax Spec.logKnob )
      assertBool "the log-height default sits inside its own band"
                 (logLinesMin <= logLinesDefault && logLinesDefault <= logLinesMax)
      -- The band is validated in the shell against the key the blob names, so
      -- the three figures are only reachable under that one spelling.
      assertBool ("the storage key the spec names is not the one the blob carries: "
                    <> Spec.kKey Spec.logKnob)
                 (T.pack (show (Spec.kKey Spec.logKnob)) `T.isInfixOf` glueConfig [])

    -- The cross-repo constraint: the backdrop pair has to clear the renderer's
    -- sticky header and completion list, which are the asset's own values and
    -- move without this repo being told.
  , testCase "the backdrop clears the renderer's chrome" $ do
      rend <- TIO.readFile "assets/table-view.js"
      let asset = levels rend
          shell = sort (nub (levels (page "" [] "t" "")))
          band = map Spec.zOf [minBound .. maxBound :: Spec.Z]
      -- A sweep over nothing passes, so it says what it swept first.
      assertBool ("too few z levels read out of the renderer: " <> show asset)
                 (length asset >= 4)
      assertEqual "a renderer level reaching the shell's band" []
                  (filter (>= Spec.zOf Spec.ZBackdrop) asset)
      assertEqual "a chrome level the spec names and the asset no longer declares" []
                  [ n | n <- [Spec.tvHeader, Spec.tvCompletion], n `notElem` asset ]
      assertEqual "the spec's z band and the page's own values have drifted"
                  band shell
      assertEqual "the shell's own three" [2, 100, 101] shell
      assertBool "the status corner's retired level is back on the page"
                 (Spec.zRetired `notElem` shell)
  ]
  where
    zKey = "z-index:" :: T.Text
    -- Every level a stylesheet declares, in source order and with repeats.
    levels :: T.Text -> [Int]
    levels t = [ n | (_, r) <- T.breakOnAll zKey t
                   , Right (n, _) <- [T.Read.decimal (T.strip (T.drop (T.length zKey) r))] ]

-- | Sheets, the document pane, and the small lists Elm draws.
--
-- Three of these are a figure written down TWICE — the narrow's separator, the
-- panel's three planning rows, the browser ladder's candidates — so each case
-- compares the copies rather than restating one of them, and a figure moved on
-- either side fails here.  The fourth is the guarantee @Doc.elm@'s @drawText@
-- rests on and nothing checked: it is asked of the real scanner.
specGroup11 :: TestTree
specGroup11 = testGroup "Sheets, document pane, Elm"
  [ -- @drawText@ walks the segments in order and SILENTLY drops a link opening
    -- inside the one before it, so what it rests on is the producer's: spans
    -- ascending and pairwise disjoint.  The nested shape is the one that could
    -- break it — the outer bracket fails to close and the rescan picks the
    -- inner link up one bracket late.
    testCase "a link opening inside the previous one is dropped" $ do
      let nested = orgLinks "see [[https://a][x [[https://b][y]] z]] and https://c end"
      assertEqual "the nested shape stopped yielding the links drawText walks"
                  3 (length nested)
      assertEqual "orgLinks answered a link opening inside the one before it"
                  [] (overlaps nested)
      withDocDir "spec-links" "links.org" linkDoc $ \recs -> do
        let scanned = map subtreeLinks recs
        -- A sweep over nothing passes, so it says what it swept first.
        assertEqual "the link fixture stopped yielding both rows' links"
                    [4, 4] (map length scanned)
        assertEqual "subtreeLinks answered a link opening inside the one before it"
                    [] (concatMap overlaps scanned)

    -- @Listing.elm@ joins the cells it DREW with U+001F and 'searchTextOf'
    -- joins 'hrSearch' with 'cellSep'; matching ACROSS the join is what pins
    -- the spec's own separator to the producer's character.
  , testCase "the narrow folds case and reads the cells the list draws" $ do
      assertBool ("the narrow's separator and Glance.Query.cellSep " <> show cellSep
                    <> " have drifted")
                 (Spec.narrowMatch ['a', cellSep, 'b'] ["a", "b"])
      assertBool "the narrow joined two drawn cells with nothing between them"
                 (not (Spec.narrowMatch "ab" ["a", "b"]))
      assertBool "the narrow stopped folding case"
                 (Spec.narrowMatch "ODO" ["TODO", "x"])

  , testCase "the three planning rows are org's, in org's order" $
      assertEqual "the panel's fixed rows and Glance.Query.planningKeywords have drifted"
                  (map T.unpack planningKeywords) Spec.planningRows

  , testCase "environment leads the flag, and the PATH ladder is chromium-family" $ do
      assertEqual "the spec's browser candidates and Glance.Desktop's own have drifted"
                  browserCandidates Spec.chromiumFamily
      assertEqual "the ladder no longer opens with the environment and the flag"
                  [Spec.FromEnv, Spec.FromFlag] (take 2 Spec.launchLadder)
      assertEqual "the ladder no longer ends with the URL printed"
                  Spec.PrintUrl (last Spec.launchLadder)
      assertEqual "the ladder's PATH rungs and its own candidate list have drifted"
                  (map Spec.OnPath Spec.chromiumFamily) (filter onPath Spec.launchLadder)
  ]
  where
    -- The pairs a segment walk would draw twice, or draw out of order.
    overlaps :: [OrgLink] -> [(Span, Span)]
    overlaps ls = [ (olSpan p, olSpan q)
                  | (p, q) <- zip ls (drop 1 ls)
                  , spanEnd (olSpan p) > spanStart (olSpan q) ]

    -- Both bracket shapes, a bare URL, one target respelled under another
    -- description, and the nested shape, spread over two rows.
    linkDoc :: Text
    linkDoc = T.unlines
      [ "* one [[https://a][alpha]] and [[https://b]]"
      , "body https://c and [[mailto:x@y][mail]]"
      , "* two [[https://d][x [[https://e][y]] z]]"
      , "trailing https://f and [[https://a][beta]]" ]

    onPath :: Spec.Launch -> Bool
    onPath (Spec.OnPath _) = True
    onPath _other          = False

-- Build and discipline
--
-- The registries the build is described by, bound to the files that carry it:
-- @glance.cabal@'s stanzas, the web layer's own import graph, the two project
-- files, the proposals directory and the changelog.  Each case reads the real
-- file and compares it to the spec's list, so a stanza added, a dependency
-- widened, a module renamed or a status invented fails here and names itself.

-- | A line opening a stanza, in the spelling 'coName' uses: the unqualified
-- @library@ is the package's own name, an executable wears an @exe:@ prefix.
stanzaHeader :: Text -> Maybe String
stanzaHeader l
  | indented l = Nothing
  | otherwise = case T.words l of
      ["library"]       -> Just "glance"
      ["library", n]    -> Just (T.unpack n)
      ["executable", n] -> Just ("exe:" <> T.unpack n)
      ["test-suite", n] -> Just (T.unpack n)
      _                 -> Nothing

indented :: Text -> Bool
indented l = maybe False (isSpace . fst) (T.uncons l)

stanzaNames :: Text -> [String]
stanzaNames = mapMaybe stanzaHeader . T.lines

-- | The body of NAME's stanza: everything under its header up to the next line
-- at column 1, which is where the next stanza or its comment begins.
cabalStanza :: String -> Text -> [Text]
cabalStanza name =
  takeWhile (\l -> T.null (T.strip l) || indented l)
    . drop 1 . dropWhile ((/= Just name) . stanzaHeader) . T.lines

-- | The indented values under FIELD.  Blank and comment lines are stepped over
-- rather than ending the run — @extra-source-files@ carries three comments
-- between its entries — and the run ends at the next field or the next stanza.
fieldBlock :: Text -> [Text] -> [String]
fieldBlock field ls = case break ((field `T.isPrefixOf`) . T.strip) ls of
  (_, _ : rest) -> go rest
  (_, [])       -> []
  where
    go [] = []
    go (l : more)
      | T.null s || "--" `T.isPrefixOf` s       = go more
      | not (indented l) || ":" `T.isInfixOf` s = []
      | otherwise                               = T.unpack s : go more
      where s = T.strip l

-- | The FIRST @build-depends:@ of a stanza's body — the unconditional one, so
-- @glance-desktop-native@ answers with what it needs whatever the flag says.
buildDepends :: [Text] -> [Text]
buildDepends ls = case break (("build-depends:" `T.isPrefixOf`) . T.strip) ls of
  (_, _ : rest) -> run True [ s | l <- rest, let s = T.strip l
                                , not (T.null s), not ("--" `T.isPrefixOf` s) ]
  (_, [])       -> []
  where
    run _ [] = []
    run first (x : xs)
      | first                = x : run False xs
      | "," `T.isPrefixOf` x = T.strip (T.drop 1 x) : run False xs
      | otherwise            = []

-- | The components a dependency entry names inside this package: @glance:X@,
-- @glance:{A, B}@, and the bare package name, which is the public library.
sublibsOf :: Text -> [String]
sublibsOf entry = case T.stripPrefix "glance:" entry of
  Just r -> case T.stripPrefix "{" (T.strip r) of
    Just inner -> map (T.unpack . T.strip) (T.splitOn "," (T.takeWhile (/= '}') inner))
    Nothing    -> [T.unpack (T.takeWhile (not . isSpace) r)]
  Nothing
    | T.takeWhile (not . isSpace) entry == "glance" -> ["glance"]
    | otherwise                                     -> []

intraDeps :: String -> Text -> [String]
intraDeps name cab = concatMap sublibsOf (buildDepends (cabalStanza name cab))

-- | @Nothing@ is the unqualified @library@, which is the public one.
exposedModulesOf :: Maybe String -> Text -> [String]
exposedModulesOf who cab = fieldBlock "exposed-modules:" (cabalStanza (fromMaybe "glance" who) cab)

extraSourceFiles :: Text -> [String]
extraSourceFiles = fieldBlock "extra-source-files:" . T.lines

-- | Every @flag@ block with the two words that decide whether the solver may
-- turn it on by itself.
flagStanzas :: Text -> [(String, Bool, Bool)]
flagStanzas cab =
  [ (T.unpack n, says "manual:" body, says "default:" body)
  | (i, l) <- zip [0 :: Int ..] ls, not (indented l), ["flag", n] <- [T.words l]
  , let body = takeWhile (\b -> T.null (T.strip b) || indented b) (drop (i + 1) ls) ]
  where
    ls = T.lines cab
    says field body =
      or [ T.strip v == "True" | b <- body, Just v <- [T.stripPrefix field (T.strip b)] ]

flagNames :: Text -> [String]
flagNames cab = [ n | (n, _, _) <- flagStanzas cab ]

stanzasNaming :: String -> Text -> [String]
stanzasNaming needle cab =
  [ n | n <- stanzaNames cab, any (T.pack needle `T.isInfixOf`) (cabalStanza n cab) ]

cabalField :: Text -> Text -> Text
cabalField field t =
  case [ T.strip v | l <- T.lines t, Just v <- [T.stripPrefix field (T.strip l)] ] of
    (v : _) -> v
    []      -> ""

-- | A project file's packages, the value's own line and the indented lines
-- continuing it.
packagesOf :: Text -> [Text]
packagesOf = go . T.lines
  where
    go [] = []
    go (l : more)
      | Just rest <- T.stripPrefix "packages:" (T.strip l) = T.words rest <> cont more <> go more
      | otherwise = go more
    cont (l : more)
      | indented l, s <- T.strip l
      , not (T.null s), not ("--" `T.isPrefixOf` s), not (":" `T.isInfixOf` s) = T.words s <> cont more
    cont _ = []

changelogHeadings :: Text -> [Text]
changelogHeadings t = [ T.strip (T.drop 3 l) | l <- T.lines t, "## " `T.isPrefixOf` l ]

-- | A released section's heading, cut in two: @VERSION - YYYY-MM-DD@.
releaseOf :: Text -> Maybe (Text, Text)
releaseOf h = case T.splitOn " - " h of
  [v, d] | T.length d == 10, T.all (\c -> isDigit c || c == '-') d -> Just (T.strip v, d)
  _ -> Nothing

-- | The status a proposal's NAME wears: the segment before its extension.
statusPart :: FilePath -> Maybe String
statusPart n = case reverse (T.splitOn "." (T.pack n)) of
  (_ : st : _ : _) -> Just (T.unpack st)
  _                -> Nothing

-- | Emacs's editor state declares nothing, which is 'Data.Org.Walk.isSidecar''s
-- rule one directory over.  The sweep still reads the DIRECTORY.
proposalSidecar :: FilePath -> Bool
proposalSidecar n = (".#" `isPrefixOf` n) || ("#" `isPrefixOf` n && "#" `isPrefixOf` reverse n)

-- | The @Glance.Web.*@ and @Glance.Desktop*@ modules a source reads.
-- 'Glance.Query' is another component's business and is dropped.
webImportsOf :: Text -> [String]
webImportsOf src =
  [ T.unpack m
  | l <- T.lines src, Just r <- [T.stripPrefix "import " l]
  , let named = fromMaybe (T.strip r) (T.stripPrefix "qualified " (T.strip r))
  , let m = T.takeWhile (\c -> isAlphaNum c || c == '.' || c == '_') (T.strip named)
  , "Glance.Web" `T.isPrefixOf` m || "Glance.Desktop" `T.isPrefixOf` m ]

modPath :: WMod -> FilePath
modPath m = "src-web" </> map (\c -> if c == '.' then '/' else c) (wname m) <> ".hs"

-- | Every Haskell file this package builds from.  'TestSelfContained' sweeps
-- the same set for its own reasons and exports only its 'TestTree', so the
-- sweep is written twice.
buildSources :: IO [FilePath]
buildSources = concat <$> mapM under ["src", "src-query", "src-web", "src-desktop-native", "app"]
  where
    under dir = do
      entries <- map (dir </>) <$> listDirectory dir
      files <- filterM doesFileExist entries
      nested <- mapM under =<< filterM doesDirectoryExist entries
      pure (filter ((== ".hs") . takeExtension) files <> concat nested)

-- | The lines of PATH carrying NEEDLE, each with its file and number, so a
-- failure says where to look.
namesIn :: Text -> FilePath -> IO [String]
namesIn needle path = report . T.lines <$> TIO.readFile path
  where
    report ls = [ path <> ":" <> show n <> ": " <> T.unpack (T.strip l)
                | (n, l) <- zip [1 :: Int ..] ls, needle `T.isInfixOf` l ]

-- | The COMMENT lines of PATH written as a negation reveal, by the spec's own
-- detector, so the rule and what enforces it are one function.
revealingComments :: FilePath -> IO [String]
revealingComments path = report . T.lines <$> TIO.readFile path
  where
    report ls = [ path <> ":" <> show n <> ": " <> T.unpack s
                | (n, l) <- zip [1 :: Int ..] ls, let s = T.stripStart l
                , "--" `T.isPrefixOf` s, negationReveal (T.unpack s) ]

specGroup12 :: TestTree
specGroup12 = testGroup "Build and discipline"
  [ testCase "six components, each named once" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "the stanzas glance.cabal declares"
        (sort (map coName components)) (sort (stanzaNames cab))

  , testCase "one public component, and it is the facade" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "the public library's exposed-modules"
        ["Glance.Query"] (exposedModulesOf Nothing cab)
      assertEqual "the component the spec calls public"
        ["glance"] [ coName c | c <- components, coVis c == Public ]

  , testCase "a web or daemon target depends on the public library alone" $ do
      cab <- TIO.readFile "glance.cabal"
      assertBool "the spec names no web target" (not (null webTargets))
      forM_ webTargets $ \n ->
        assertEqual (n <> "'s intra-package build-depends") (compDeps n) (intraDeps n cab)

  , testCase "the parser is reachable from no web target" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "a web target naming the parser"
        [] [ n | n <- webTargets, "glance-internal" `elem` intraDeps n cab ]

  , testCase "the CLI dispatches to three sublibraries" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "exe:glance's sublibraries"
        (sort (compDeps "exe:glance")) (sort (intraDeps "exe:glance" cab))

  , testCase "the suite names the three components carrying testable code" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "glance-test's sublibraries"
        (sort (compDeps "glance-test")) (sort (intraDeps "glance-test" cab))
      assertBool "the suite now reaches the native stanza, which needs GTK to build"
        ("glance-desktop-native" `notElem` intraDeps "glance-test" cab)

  , testCase "the floor imports nothing above it" $ do
      src <- TIO.readFile (modPath WBase)
      assertEqual "Glance.Web.Base imports a module above it" [] (webImportsOf src)
      assertEqual "and the spec's floor gained an edge" [] (wimports WBase)

    -- The door half falls out of the whole table: `Glance.Web' is read by
    -- `Glance.Desktop' and `Glance.Desktop.Native' and by nothing else.
  , testCase "the door is read by the desktop pair alone" $ do
      real <- forM wmods $ \m -> (,) (wname m) . sort . nub . webImportsOf <$> TIO.readFile (modPath m)
      assertEqual "the web layer's import graph"
        [ (wname m, sort (map wname (wimports m))) | m <- wmods ] real
      assertEqual "the modules reading the door"
        [wname WDesktop, wname WNative] [ wname m | m <- wmods, WWeb `elem` wimports m ]

  , testCase "the exposed list and the import graph name one set of modules" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "glance-web's exposed-modules"
        (sort (map wname wmods)) (sort (exposedModulesOf (Just "glance-web") cab))
      assertEqual "the spec's two spellings of the web module set"
        (sort webExposed) (sort (map wname wmods))

  , testCase "Routes alone carries TemplateHaskell, and every splice sits in it" $ do
      files <- buildSources
      assertBool ("too few sources swept: " <> show (length files)) (length files >= 12)
      hits <- sort <$> filterM (fmap ("TemplateHaskell" `T.isInfixOf`) . TIO.readFile) files
      assertEqual "sources carrying TemplateHaskell" [modPath WRoutes] hits
      assertEqual "the modules the spec's assets are spliced into"
        [wname WRoutes] (map wname (nub (mapMaybe baSplice buildAssets)))

  , testCase "and rides in extra-source-files, so a splice recompiles and sdist carries it" $ do
      cab <- TIO.readFile "glance.cabal"
      let extras = extraSourceFiles cab
      assertBool ("too few extra-source-files read: " <> show extras) (length extras >= 4)
      assertEqual "a splice input outside extra-source-files"
        [] [ p | p <- sdistExtras, p `notElem` extras ]

  , testCase "the shell is seven parts, folded in their numbered order" $
      -- ORDER INCLUDED: a part added, renamed or reordered in `Glance.Web.Base'
      -- fails here, and the on-disk half is `TestSelfContained''s.
      assertEqual "the shell's parts, as the spec and the build fold them"
        gluePartFiles WB.gluePartFiles

  , testCase "the flag is manual and off, so the solver never turns it on" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "the flags glance.cabal declares" (map flName flags) (flagNames cab)
      assertEqual "a flag that is not manual-and-off"
        [] [ n | (n, m, d) <- flagStanzas cab, not m || d ]
      assertEqual "a flag the spec and the cabal describe differently"
        [] [ n | f <- flags, (n, m, d) <- flagStanzas cab, n == flName f
               , (m, d) /= (flManual f, flOn f) ]

    -- Per flag rather than over the list, so the case says the same thing about
    -- however many flags the spec grows.
  , testCase "one stanza sees it, and all it does there is a define and its dependencies" $ do
      cab <- TIO.readFile "glance.cabal"
      assertBool "the spec declares no flag" (not (null flags))
      forM_ flags $ \f -> do
        forM_ (flCpp f) $ \d ->
          assertEqual ("the stanzas naming " <> d) [flStanza f] (stanzasNaming d cab)
        assertEqual (flStanza f <> "'s unconditional intra-package deps")
          (compDeps (flStanza f)) (intraDeps (flStanza f) cab)

  , testCase "the default project carries no vendored package" $ do
      def <- TIO.readFile "cabal.project"
      nat <- TIO.readFile "cabal.project.native"
      assertEqual "the default project's packages" ["."] (packagesOf def)
      assertEqual "the vendored packages the native project names"
        (length (projPackages NativeProj))
        (length [ p | p <- packagesOf nat, "vendored/" `T.isPrefixOf` p ])
      forM_ (projPackages NativeProj) $ \p ->
        assertBool (p <> " is named by no line of cabal.project.native")
          (any (T.isPrefixOf (T.pack p)) (packagesOf nat))

  , testCase "and no flag, so `-f native-window' against it still fails in the solver" $ do
      def <- TIO.readFile "cabal.project"
      nat <- TIO.readFile "cabal.project.native"
      assertBool "cabal.project names a flag" (not ("flags:" `T.isInfixOf` def))
      assertEqual "the flags the spec says the default project sets" [] (projFlags DefaultProj)
      forM_ (projFlags NativeProj) $ \(n, set) ->
        assertBool ("cabal.project.native no longer sets " <> n)
          (T.pack ((if set then "+" else "-") <> n) `T.isInfixOf` nat)

  , testCase "the GIR search path is the flagged build's alone, and names three foreign types" $ do
      mk <- TIO.readFile "Makefile"
      girs <- filter ((== ".gir") . takeExtension) <$> listDirectory "vendored/gir"
      assertBool "the native target no longer sets the GIR path"
        ("HASKELL_GI_GIR_SEARCH_PATH" `T.isInfixOf` mk)
      assertEqual "the default build gained a GIR path" Nothing (projGir DefaultProj)
      forM_ (projGir NativeProj) $ \dir ->
        assertBool ("the Makefile no longer points the GIR path at " <> dir)
          (T.pack dir `T.isInfixOf` mk)
      assertEqual "the hand-written GIRs on disk"
        (sort (map (<> ".gir") vendoredGirs)) (sort girs)

  , testCase "each build has its own directory, so neither overwrites the other's binary" $ do
      mk <- TIO.readFile "Makefile"
      assertBool ("the native build no longer has a builddir of its own: " <> projBuildDir NativeProj)
        (T.pack ("--builddir=" <> projBuildDir NativeProj) `T.isInfixOf` mk)
      assertBool "the two build directories collided"
        (projBuildDir DefaultProj /= projBuildDir NativeProj)

  , testCase "each gi spelling is the ambiguous name plus its major version" $ do
      cab <- TIO.readFile "glance.cabal"
      forM_ giSpellings $ \(chosen, ambiguous) -> do
        assertBool (chosen <> " is no longer the dependency") (T.pack chosen `T.isInfixOf` cab)
        assertEqual ("the ambiguous " <> ambiguous <> " is back")
          [] [ T.unpack (T.strip l) | l <- T.lines cab
             , T.pack (", " <> ambiguous <> " ") `T.isPrefixOf` T.strip l ]

  , testCase "the five status words are distinct and lowercase" $ do
      names <- filter (not . proposalSidecar) <$> listDirectory "docs/proposals"
      assertBool ("too few proposals swept: " <> show (length names)) (length names >= 35)
      let spelled = map statusWord [minBound .. maxBound :: Status]
          worn = nub (mapMaybe statusPart names)
      assertEqual "a status word the spec spells twice" spelled (nub spelled)
      assertEqual "a status word that is not lowercase"
        [] [ s | s <- spelled, s /= map toLower s ]
      assertEqual "a proposal wearing a status the spec does not spell" [] (worn \\ spelled)
      assertEqual "a status word no proposal wears" [] (spelled \\ worn)

  , testCase "a cut takes every entry and leaves Unreleased empty" $ do
      ch <- TIO.readFile "CHANGELOG.md"
      let heads = changelogHeadings ch
      assertEqual "the section a cut promotes" ["Unreleased"] (take 1 heads)
      assertEqual "Unreleased headings" 1 (length (filter (== "Unreleased") heads))
      assertEqual "a released heading that is not `VERSION - YYYY-MM-DD'"
        [] [ T.unpack h | h <- drop 1 heads, isNothing (releaseOf h) ]

  , testCase "and a dated release cannot be cut again" $ do
      ch <- TIO.readFile "CHANGELOG.md"
      let rs = mapMaybe releaseOf (changelogHeadings ch)
      assertBool ("too few releases read: " <> show (length rs)) (length rs >= 4)
      assertEqual "a version cut twice" (nub (map fst rs)) (map fst rs)
      assertBool "a release dated after the one above it"
        (and (zipWith (>=) (map snd rs) (drop 1 (map snd rs))))

  , testCase "a version is spelled in three files, the changelog among them" $ do
      cab <- TIO.readFile "glance.cabal"
      rd <- TIO.readFile "README.org"
      ch <- TIO.readFile "CHANGELOG.md"
      let v = cabalField "version:" cab
      assertEqual "the files a cut bumps"
        (sort ["CHANGELOG.md", "glance.cabal", "README.org"]) (sort versionSites)
      assertBool ("README.org does not spell " <> T.unpack v) (("=" <> v <> "=") `T.isInfixOf` rd)
      assertBool ("CHANGELOG.md has no `## " <> T.unpack v <> " - ' section")
        (("## " <> v <> " - ") `T.isInfixOf` ch)

  , testCase "the author is one address, and it is the protonmail one" $ do
      cab <- TIO.readFile "glance.cabal"
      assertEqual "glance.cabal's maintainer" (T.pack authorEmail) (cabalField "maintainer:" cab)
      files <- buildSources
      assertBool ("too few sources swept: " <> show (length files)) (length files >= 12)
      hits <- concat <$> mapM (namesIn "akatovda@gmail.com")
                              ("glance.cabal" : "README.org" : "CHANGELOG.md" : files)
      assertEqual "a tracked file naming the other address" [] hits

  , testCase "the negation-reveal detector fires on its own shape" $ do
      assertBool "the spec's detector no longer reads the banned shape"
        (negationReveal "it is not X, but Y" && negationReveal "it's not just A, it's B")
      assertBool "the spec's detector reads a direct statement as the banned shape"
        (not (negationReveal "X holds; Y follows"))
      files <- buildSources
      assertBool ("too few sources swept: " <> show (length files)) (length files >= 12)
      hits <- concat <$> mapM revealingComments files
      assertEqual "a comment written as a negation reveal" [] hits
  ]

-- | Every section, in the spec's own order.
spec :: TestTree
spec = testGroup "Spec"
  [ specGroup03, specGroup04, specGroup05, specGroup06, specGroup07
  , specGroup08, specGroup09, specGroup10, specGroup11, specGroup12 ]
