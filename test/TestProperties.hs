-- | The laws @AGENTS.hs@ writes as UNIVERSALS, asked as universals.
--
-- Every group here restates a rule the repo already pins with a hand-chosen
-- example list and hands it a generator instead.  Three kinds, and the kind
-- decides which alphabet 'TestGen' draws from:
--
-- * ANSWER — the generator computed the offsets, so the assertion is an equality
--   against a value the parser never saw.  Plain alphabet.
-- * ALGEBRA — self-consistency and round-trips, which need no answer, so they
--   take the adversarial alphabet too.
-- * NEGATIVE — the documented refusals.
--
-- WHAT THE GROUPS DO NOT BUY IS STATED PLAINLY: the nesting and well-formedness
-- properties ask the corpus's own questions of a hundred generated headlines
-- where @glance scan@ asks them of 12630 real ones, and what they add over it is
-- OFFLINE and SHRINKING.  The reparse, the lens, @applyEdits@' algebra, the
-- timestamps and the level sequences are unreachable from a read-only scan.
--
-- The seed is fixed ('TestDefaults.testProperty'), so a red run replays from the
-- commit alone; @GLANCE_QC_SEED@ unfixes it.
module TestProperties (spec) where

import Data.Char (isAlpha, isDigit)
import Data.List (nub, sortOn)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.QuickCheck ( (===), (.&&.), (==>), Property, arbitrary, conjoin
                       , counterexample, ioProperty, property, variant )
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import TextShow (showt)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time

import Data.Org ( Context, Element (EHeadline, ETimestamp), Headline (spans)
                , HeadlineSpans (..), Span (..), Spanned (spanOf, valueOf)
                , Timestamp (tsCompactRange, tsEnd, tsStart), TsMoment (tsmHasTime, tsmTime)
                , defaultContext, hsFull, orgParse, sliceSpan, stripSpans )
import Data.Org.Edit (Edit (..), EditError, applyEdits)
import Glance.Query ( HeadlineParts (..), HeadlineRecord (hrSubtree)
                    , headlineParts, loadFile, recomposedSubtree, subtreeText )
import TestDefaults (bare, headlinesOf, testProperty, testPropertyWith, withTempDirNamed)
import TestGen ( Broken (Broken), DocSpec (dsEntries, dsFinalEol), EntrySpec (..)
               , Expected (exBlank, exLevel, exSpans)
               , Rendered (rdEntries, rdText), TsAny (TsAny), TsImage (TsImage)
               , Wild (Wild), brokenRange, docSample, expectedExtents, render
               , shapeFloors, shapesOf )

import System.FilePath ((</>))

spec :: TestTree
spec = testGroup "Properties"
  [ generatorSpec
  , answerSpec
  , algebraSpec
  , editSpec
  , lensSpec
  , timestampSpec
  , negativeSpec
  ]

-- The instrument, asserted before anything is read through it

-- | A property that generates nothing interesting passes forever.  This group
-- lands FIRST and says what the image holds; the counts are over ONE
-- deterministic sample, so the census is a fact about the generator rather than
-- about the run that read it — @groundSweep@\'s idiom, which asserts what it
-- swept before reading through it.
generatorSpec :: TestTree
generatorSpec = testGroup "Generator"
  [ testCase "the image is wide" $ do
      let sample = docSample sampleSize
          census = concatMap shapesOf sample
          seen name = length (filter (== name) census)
          short = [ (name, floor', seen name)
                  | (name, floor') <- shapeFloors, seen name < floor' ]
      assertBool "nothing sampled" (length sample == sampleSize)
      assertBool ("short of the floor over " <> show sampleSize <> " documents: "
                    <> show short <> "\nthe whole census: "
                    <> show [ (name, seen name) | (name, _floor) <- shapeFloors ])
                 (null short)

    -- A generated document the PARSER refuses, or one whose entries do not each
    -- come back as a headline, would make every property below vacuous in the
    -- quiet direction.
  , testCase "every document it spells parses, and yields the entries it spelled" $ do
      let bad = [ (why, rdText r) | ds <- docSample sampleSize
                                  , let r = render ds
                                  , Just why <- [wrong r] ]
          wrong r = case orgParse defaultContext (rdText r) of
            (_elems, _ctx, Just _err) -> Just "parse error"
            (elems, _ctx, Nothing)
              | length (headlinesOf elems) /= length (rdEntries r) -> Just "headline count"
              | otherwise -> Nothing
      assertBool (show (take 2 bad) <> " (" <> show (length bad) <> " of "
                    <> show sampleSize <> ")")
                 (null bad)

    -- The acceptance property below is an EQUALITY and cannot go vacuous, but
    -- the three guarded by 'legal' can, so both sides of the boundary are
    -- counted here off the very generator they read through.
  , testCase "the edit sets reach both sides of the acceptance boundary" $ do
      let sets = editSample sampleSize
          legals = length [ () | (doc, es) <- sets, legal doc es ]
      assertBool ("legal edit sets: " <> show legals <> " of " <> show sampleSize)
                 (legals >= 60 && sampleSize - legals >= 60)
  ]
  where sampleSize = 400

-- ANSWER properties

-- | Equalities against offsets the generator counted.  The first assertions in
-- this repo that are not self-consistent: every span-slice assertion in
-- @TestSpans@ checks a slice against the component the SAME parse produced.
answerSpec :: TestTree
answerSpec = testGroup "Spans, against the offsets they were written at"
  [ testProperty "every sub-span is where the generator spelled it" $ \ds ->
      let r = render ds
      in  withParse r $ \_ctx elems ->
            map spans (headlinesOf elems) === map exSpans (rdEntries r)

    -- 'hsFull' is a LEFT FOLD over 'spanParts' seeded with the stars, ending at
    -- the LAST present part in SOURCE order — never a maximum over ends.  The
    -- two agree exactly while that list is in source order and diverge the
    -- moment it is not, which is the failure mode AGENTS.hs describes and
    -- one fixture samples.
  , testProperty "hsFull is the fold, and the fold is the maximum" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \_ctx elems -> conjoin
            [ counterexample (T.unpack (sliceSpan (rdText r) full))
                (spanStart full === spanStart (hsStars hs)
                   .&&. spanEnd full === maximum (spanEnd (hsStars hs) : map spanEnd present))
            | h <- headlinesOf elems
            , let hs = spans h
                  full = hsFull hs
                  present = mapMaybe ($ hs) (map snd namedSubSpans) ]

    -- Subtree extents TILE, computed over EVERY headline though only top
    -- entries keep records.  A BLANK top entry is left out of this ON PURPOSE:
    -- it keeps its extent and loses its record, and the survivors then do NOT
    -- meet — see the delivery note in
    -- docs/proposals/2026-08-11-property-tests.done.md.
  , testPropertyWith 60 "top entries tile the document, and meet exactly" $ \ds ->
      let r = render ds
          want = [ sp | (e, sp) <- zip (rdEntries r)
                                       (expectedExtents (T.length (rdText r)) (rdEntries r))
                      , exLevel e == 1, not (exBlank e) ]
      in  noBlankTopEntry r ==> ioProperty (withLoaded r (\doc recs ->
            let got = map hrSubtree recs
            in  got === want
                  .&&. counterexample "the extents do not tile"
                         (T.concat (map (sliceSpan doc) got)
                            === maybe "" (\sp -> T.drop (spanStart sp) doc) (firstOf got))))
  ]

-- ALGEBRA properties

algebraSpec :: TestTree
algebraSpec = testGroup "Spans, as an algebra"
  [ -- 'assertInvariants' (TestSpans) with its 24 documents replaced by a
    -- generator: containment, source order, non-overlap, and the drawer closing
    -- the headline.
    testProperty "sub-spans nest inside hsFull, in order, never overlapping" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \_ctx elems -> conjoin (map nested (headlinesOf elems))

  , testProperty "hsFull never covers trailing whitespace" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \_ctx elems -> conjoin
            [ T.stripEnd slice === slice
            | h <- headlinesOf elems
            , let slice = sliceSpan (rdText r) (hsFull (spans h)) ]

    -- Threaded with the parse's OWN final context, which is what a generated
    -- '#+TODO:' line makes load-bearing: a slice carrying a custom keyword
    -- reparsed under 'defaultContext' reads that keyword as title text.  One
    -- fixture wide today.
  , testProperty "hsFull reparses to the headline it came from" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \ctx elems -> conjoin
            [ reparses ctx (sliceSpan (rdText r) (hsFull (spans h))) (EHeadline h)
            | h <- headlinesOf elems ]

  , testProperty "an element span reparses to the element it came from" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \ctx elems -> conjoin
            [ reparses ctx (sliceSpan (rdText r) (spanOf e)) (valueOf e) | e <- elems ]

    -- The SEMANTIC half of 'stripSpans', which the compiler cannot state: move
    -- the document by a preamble and the stripped elements must not move.  A
    -- constructor that leaks a span fails this and nothing else — and ~150
    -- span-insensitive assertions read elements through 'bare'.
  , testProperty "stripSpans leaves no offset behind" $ \(Wild ds) pad ->
      let doc = rdText (render ds)
          ahead = preamble pad
      in  parses (ahead <> doc) ==>
            bareOf doc === drop (length (bareOf ahead)) (bareOf (ahead <> doc))
  ]

-- | Every rule 'assertInvariants' states, over one headline.
nested :: Headline -> Property
nested h = conjoin (inside <> ordered <> drawerClosesIt)
  where
    hs = spans h
    full = hsFull hs
    present = [ (name, sp) | (name, get) <- namedSubSpans, Just sp <- [get hs] ]
    inside = [ counterexample (name <> " is outside hsFull")
                 (property (spanStart sp >= spanStart full && spanEnd sp <= spanEnd full))
             | (name, sp) <- present ]
    ordered = [ counterexample (a <> " must end before " <> b <> " starts")
                  (property (spanEnd x <= spanStart y))
              | ((a, x), (b, y)) <- zip inOrder (drop 1 inOrder) ]
    inOrder = sourceOrder hs
    drawerClosesIt = [ counterexample "the drawer must end where hsFull ends"
                         (spanEnd sp === spanEnd full)
                     | Just sp <- [hsProperties hs] ]

-- | HS's present sub-spans in SOURCE order: five of them positional, and the
-- three PLANNING entries sorted by offset, since org lets one line permute them
-- freely.  The rule 'spanParts' answers, written out rather than read off it.
sourceOrder :: HeadlineSpans -> [(String, Span)]
sourceOrder hs = there before <> sortOn (spanStart . snd) (there planning) <> there after
  where
    there names = [ (name, sp) | (name, get) <- names, Just sp <- [get hs] ]
    (before, rest) = splitAt 4 namedSubSpans
    (planning, after) = splitAt 3 rest

-- applyEdits

-- | 'applyEdits' sorts and checks NEIGHBOURS, which is a reduction from the
-- quadratic pairwise rule its docstring states.  'legal' below is that rule
-- written out — the specification — and the acceptance property is what asks the
-- reduction to justify itself.
--
-- The three properties guarded by @legal@ are their own coverage oracle: a
-- generator that stopped producing legal sets would exceed QuickCheck's discard
-- ratio and the shim would fail them.
editSpec :: TestTree
editSpec = testGroup "applyEdits"
  [ testProperty "accepts exactly the disjoint sets" $ \ds plan texts ->
      let doc = rdText (render ds)
          es = editsOver doc plan texts
      in  counterexample (show es) (isRight (applyEdits doc es) === legal doc es)

  , -- The two below take sets that are DISJOINT BY CONSTRUCTION rather than by a
    -- guard: over the adversarial sets above only a fifth are legal, and a
    -- precondition that discards four cases in five is a property mostly not
    -- being run.  Legality is asserted rather than assumed, so a construction
    -- that stopped producing legal sets says so.
    testProperty "the answer does not depend on the order they were named" $ \ds plan texts k ->
      let doc = rdText (render ds)
          es = disjointEdits doc plan texts
      in  distinctKeys es ==>
            counterexample (show es) (property (legal doc es))
              .&&. applyEdits doc es === applyEdits doc (rotate k es)

  , testProperty "length is the document's plus what each edit adds" $ \ds plan texts ->
      let doc = rdText (render ds)
          es = disjointEdits doc plan texts
      in  counterexample (show es) (property (legal doc es))
            .&&. (T.length <$> applyEdits doc es)
                   === Right (T.length doc + sum (map delta es))

    -- "untouched bytes stay byte-identical" (AGENTS.hs) as a
    -- universal.  TestEdit's ten splice cases are four points of this algebra.
  , testProperty "replacing a span with its own text is the document" $ \ds a b ->
      let doc = rdText (render ds)
          sp = spanIn doc a b
      in  applyEdits doc [Edit sp (sliceSpan doc sp)] === Right doc

    -- The sort is STABLE, so two INSERTIONS at one offset land in LIST order.
  , testProperty "two insertions at one offset land in list order" $ \ds a ->
      let doc = rdText (render ds)
          at = clamp 0 (T.length doc) a
          sp = Span at at
      in  applyEdits doc [Edit sp "<1>", Edit sp "<2>"]
            === Right (T.take at doc <> "<1><2>" <> T.drop at doc)
  ]

-- The subtree lens

-- | Decompose → recompose is byte-identical UP TO THE LINE END, over region
-- PRESENCE (three independent bits), region STYLE (indentation × line ending ×
-- ordering) and a body the client has changed — the last being the half that
-- exercises the subtraction the region indices are computed by.  Twelve
-- fixtures under one @testCase@ is the whole of it today.
--
-- The law used to be plain identity and the trailing space the generator draws
-- ('esTrail', 'esHeadTrail') is what narrowed it: a write spells no trailing
-- space, so a subtree carrying one comes back one run shorter and one carrying
-- none comes back byte for byte, which is the same statement.
lensSpec :: TestTree
lensSpec = testGroup "Subtree lens"
  [ testPropertyWith 60 "decompose then recompose is the subtree, its line ends trimmed" $ \ds ->
      let r = render ds
      in  lensRepresentable ds ==> ioProperty (withLoaded r (\_doc recs -> conjoin
            [ recomposedSubtree q (headlineParts q) === asWritten (subtreeText q) | q <- recs ]))

  , testPropertyWith 60 "the body's lines are the subtree's, minus the regions" $ \ds ->
      let r = render ds
      in  ioProperty (withLoaded r (\_doc recs -> conjoin (map ownsEachByte recs)))

    -- Re-decomposing a body the client changed must answer the parts it was
    -- given: a region goes back on the line it sat on less the lines every
    -- region ahead of it took out, and the failure mode is a region landing one
    -- line late.
  , testPropertyWith 20 "re-decomposing an edited body answers the parts it was given" $ \ds ->
      let r = render ds
      in  lensRepresentable ds ==> ioProperty (withLoaded r
            (\doc recs -> conjoin (map (reDecomposes doc) recs)))
  ]

-- Timestamps

-- | The value round-trip is the direction that is TOTAL: render → parse → equal.
-- Text → render is lossy by design, the weekday being recomputed, which is why
-- @TestRoundtrip@ spells canonical stamps in all 26 of its rows.
timestampSpec :: TestTree
timestampSpec = testGroup "Timestamps"
  [ testProperty "a value survives render then parse" $ \(TsImage ts) ->
      counterexample (T.unpack (showt ts)) (readTimestamp (showt ts) === Just ts)

  , testProperty "a render is a fixed point of parse then render" $ \(TsAny ts) ->
      counterexample (T.unpack (showt ts))
        ((showt <$> readTimestamp (showt ts)) === Just (showt ts))

    -- The weekday slot takes a run of LETTERS in any script, any length, and
    -- drops it: display-only, so a locale's word is as good as org's.  One Dutch
    -- fixture and a seven-word list are the whole of it today.
  , testProperty "any weekday spelling reads to the same value" $ \(TsImage ts) word ->
      let source = showt ts
          reworded = respellWeekday (weekdayWord word) source
      in  counterexample (T.unpack source <> " -> " <> T.unpack reworded)
            (readTimestamp reworded === Just ts)

    -- 'compactly' guards the compact render with three conditions — the flag,
    -- both ends timed, one day — of which AGENTS.hs records that ONLY THE
    -- FLAG is exercised.  A flag the ends cannot hold takes the '--' arm and the
    -- value comes back with the flag cleared and nothing else moved.
  , testProperty "a compact flag the ends cannot hold comes back cleared" $ \(TsAny ts) ->
      tsCompactRange ts && not (compactible ts) ==>
        counterexample (T.unpack (showt ts))
          (readTimestamp (showt ts) === Just ts { tsCompactRange = False })
  ]

-- Negative

-- | A sub-parser stopping mid-word fails the WHOLE file, and @orgParse@ on error
-- returns zero elements AND the caller's context untouched — the second half
-- being what a document declaring a @#+TODO:@ cycle makes worth asking, since a
-- successful parse of it would have moved the context.
negativeSpec :: TestTree
negativeSpec = testGroup "Refusals"
  [ testProperty "a mismatched range fails the whole file and leaves the context" $
      \(Broken ds k) ->
        let doc = rdText (render (breakAt k ds))
        in  counterexample (T.unpack doc) $ case orgParse defaultContext doc of
              (elems, ctx, Just _err) -> bare elems === [] .&&. ctx === defaultContext
              (_elems, _ctx, Nothing) ->
                counterexample "expected a refusal" (property False)
  ]

-- | DS with the mismatched range spliced into entry K's body.
breakAt :: Int -> DocSpec -> DocSpec
breakAt k ds = ds { dsEntries = zipWith at [0 ..] (dsEntries ds) }
  where n = max 1 (length (dsEntries ds))
        at i e | i == (k :: Int) `mod` n = e { esBody = esBody e <> [brokenRange] }
               | otherwise = e

-- Helpers

-- | The eight sub-spans in the order 'spanParts' spells them for a headline
-- whose planning entries are written in org's own order.  Named here rather than
-- read off 'headlineSpanParts': an oracle derived from the library would agree
-- with any change to it.
namedSubSpans :: [(String, HeadlineSpans -> Maybe Span)]
namedSubSpans =
  [ ("hsTodo", hsTodo), ("hsPriority", hsPriority), ("hsTitle", hsTitle), ("hsTags", hsTags)
  , ("hsSchedule", hsSchedule), ("hsDeadline", hsDeadline), ("hsClosed", hsClosed)
  , ("hsProperties", hsProperties) ]

-- | Run K over R's parse, failing the property on a parse error and showing the
-- text either way — which is what a reader pastes into a file.
withParse :: Rendered -> (Context -> [Spanned Element] -> Property) -> Property
withParse r k = counterexample (T.unpack (rdText r)) $
  case orgParse defaultContext (rdText r) of
    (elems, ctx, Nothing) -> k ctx elems
    (_elems, _ctx, Just _err) ->
      counterexample "the generator spelled a document the parser refuses" (property False)

parses :: Text -> Bool
parses t = case orgParse defaultContext t of
  (_elems, _ctx, err) -> isNothing err

bareOf :: Text -> [Element]
bareOf t = case orgParse defaultContext t of
  (elems, _ctx, _err) -> bare elems

-- | SLICE parsed in CTX is exactly the one element it was cut from.
reparses :: Context -> Text -> Element -> Property
reparses ctx slice want = counterexample (T.unpack slice) $
  case orgParse ctx slice of
    (elems, _ctx, Nothing) -> bare elems === [stripSpans want]
    (_elems, _ctx, Just _err) -> counterexample "parse error" (property False)

-- | R written to a file and loaded the way the store loads one, so the records
-- under test are the ones the daemon would hold.  Bytes rather than
-- 'TestDefaults.orgFile': the documents carry unicode and the decode on the way
-- back in is UTF-8 whatever the locale is.
withLoaded :: Rendered -> (Text -> [HeadlineRecord] -> Property) -> IO Property
withLoaded r k = withTempDirNamed "prop" $ \dir -> do
  let doc = rdText r
      path = dir </> "prop.org"
  BS.writeFile path (TE.encodeUtf8 doc)
  loaded <- loadFile path
  pure $ counterexample (T.unpack doc) $ case loaded of
    Right recs -> k doc recs
    Left why -> counterexample ("load failed: " <> show why) (property False)

-- | The body's lines are the subtree's own, in order, with the three regions
-- taken out — the whole-line cutting rule, which is what one-owner-per-byte buys
-- on the side the lens exposes.  The region SPANS are module-private, so this is
-- the subsequence rather than the tiling equation.
ownsEachByte :: HeadlineRecord -> Property
ownsEachByte r = counterexample (show (subtreeText r, hpBody parts)) $
  property (subsequence (T.lines (hpBody parts)) (T.lines (subtreeText r)))
    .&&. counterexample "the logbook is in the body too"
           (property (T.null (hpLogbook parts)
                      || not (T.strip (hpLogbook parts) `T.isInfixOf` hpBody parts)))
  where parts = headlineParts r

-- | PARTS as a write leaves them: line ends trimmed, and the logbook's
-- terminator off.  A region that was the file's LAST line and no longer is must
-- GAIN one, which is the splice being right rather than wrong; a trailing run a
-- write took is the rule rather than a move; nothing else may shift.
--
-- The two pair lists need neither: a planning span is the timestamp alone and a
-- property's value is stripped as it is read, so no trailing run reaches them.
settled :: HeadlineParts -> HeadlineParts
settled parts = parts { hpBody    = asWritten (hpBody parts)
                      , hpLogbook = T.stripEnd (asWritten (hpLogbook parts)) }

-- | TEXT as a write spells it: each line's trailing horizontal run off, its
-- terminator kept.
--
-- An INDEPENDENT spelling of the rule 'recomposedSubtree' enforces rather than
-- the export of it: a trim that took the carriage return along with the spaces
-- passes over there and fails here, and the generator draws CRLF documents.
asWritten :: Text -> Text
asWritten = T.intercalate "\n" . map line . T.splitOn "\n"
  where line l = case T.stripSuffix "\r" l of
          Just body -> T.dropWhileEnd horizontal body <> "\r"
          Nothing   -> T.dropWhileEnd horizontal l
        horizontal c = c == ' ' || c == '\t'

subsequence :: Eq a => [a] -> [a] -> Bool
subsequence [] _ = True
subsequence _ [] = False
subsequence (x : xs) (y : ys) | x == y = subsequence xs ys
                              | otherwise = subsequence (x : xs) ys

-- | R's parts with one line appended to the body, recomposed, spliced back into
-- DOC and re-read: the parts must come back exactly as they were handed over.
reDecomposes :: Text -> HeadlineRecord -> Property
reDecomposes doc r = ioProperty $ withTempDirNamed "prop-lens" $ \dir -> do
  let parts = headlineParts r
      ending | T.null (hpBody parts) || "\n" `T.isSuffixOf` hpBody parts = ""
             | otherwise = "\n"
      parts' = parts { hpBody = hpBody parts <> ending <> "extra\n" }
      sp = hrSubtree r
      doc' = T.take (spanStart sp) doc
               <> recomposedSubtree r parts'
               <> T.drop (spanEnd sp) doc
      path = dir </> "lens.org"
  BS.writeFile path (TE.encodeUtf8 doc')
  loaded <- loadFile path
  pure $ counterexample (T.unpack doc') $ case loaded of
    Left why -> counterexample ("load failed: " <> show why) (property False)
    Right recs -> case [ q | q <- recs, spanStart (hrSubtree q) == spanStart sp ] of
      (q : _) -> settled (headlineParts q) === settled parts'
      [] -> counterexample "the edited row is gone" (property False)

-- | The QUADRATIC rule 'applyEdits' reduces to a neighbour check: every span
-- inside the document, and no two of them overlapping.
legal :: Text -> [Edit] -> Bool
legal doc es = all inBounds es && and [ apart a b | (a, b) <- pairs (map editSpan es) ]
  where
    inBounds (Edit (Span s e) _) = s >= 0 && s <= e && e <= T.length doc
    apart (Span s1 e1) (Span s2 e2) = e1 <= s2 || e2 <= s1
    pairs xs = [ (a, b) | a : rest <- tailsOf xs, b <- rest ]
    tailsOf [] = []
    tailsOf xs@(_ : rest) = xs : tailsOf rest

-- | Are the sort keys distinct?  Two edits sharing one, and only those, are the
-- pair the STABLE sort keeps in list order.
distinctKeys :: [Edit] -> Bool
distinctKeys es = length (dedup keys) == length keys
  where keys = [ (spanStart sp, spanEnd sp) | Edit sp _ <- es ]
        dedup = foldr (\x acc -> x : filter (/= x) acc) []

delta :: Edit -> Int
delta (Edit (Span s e) new) = T.length new - (e - s)

-- | Up to four edits over DOC: an offset near the document and a WIDTH that may
-- be negative, so out-of-bounds, backwards and overlapping sets are all
-- reachable and legal ones stay common ('editSample' counts both sides).
editsOver :: Text -> [(Int, Int)] -> [String] -> [Edit]
editsOver doc plan texts =
  [ Edit (Span at (at + width b)) (T.pack (take 6 t))
  | ((a, b), t) <- zip (take 4 plan) (cycle (if null texts then ["x"] else texts))
  , let at = (a `mod` (len + 5)) - 2 ]
  where len = T.length doc
        width b = (b `mod` 12) - 1

-- | A DETERMINISTIC sample of the very edit sets the properties draw, taken
-- through QuickCheck's own generator so the census describes the distribution
-- they see rather than one spelled a second time here.
editSample :: Int -> [(Text, [Edit])]
editSample n = [ (doc, editsOver doc plan texts)
               | (i, ds) <- zip [1 ..] (docSample n)
               , let doc = rdText (render ds)
                     (plan, texts) = drawn i ]
  where drawn i = unGen (variant i arbitrary) (mkQCGen 7) (4 + i `mod` 24)

-- | Up to four edits over DOC that are DISJOINT AND IN BOUNDS by construction:
-- each starts where the last one ended or later, and none runs past the
-- document.  Zero-width inserts are reachable, since a width may be 0.
disjointEdits :: Text -> [(Int, Int)] -> [String] -> [Edit]
disjointEdits doc plan texts = go 0 (zip (take 4 plan) (cycle (texts <> ["x"])))
  where
    len = T.length doc
    go _at [] = []
    go at (((a, b), t) : rest)
      | start > len = []
      | otherwise   = Edit (Span start end) (T.pack (take 6 t)) : go end rest
      where start = at + (a `mod` 7)
            end = min len (start + (b `mod` 6))

spanIn :: Text -> Int -> Int -> Span
spanIn doc a b = Span (min x y) (max x y)
  where x = clamp 0 (T.length doc) a
        y = clamp 0 (T.length doc) b

clamp :: Int -> Int -> Int -> Int
clamp lo hi n = lo + (abs n `mod` (hi - lo + 1))

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate k xs = drop n xs <> take n xs
  where n = abs k `mod` length xs

isRight :: Either EditError Text -> Bool
isRight = either (const False) (const True)

firstOf :: [a] -> Maybe a
firstOf (x : _rest) = Just x
firstOf [] = Nothing

-- | Can the lens hold every byte DS spells?  TWO shapes it cannot, each found by
-- the byte-identity property above and each reported rather than weakened away:
--
-- * an EMPTY property drawer — @HeadlineParts@ carries no bit for a drawer's
--   PRESENCE, so a drawer holding no pair at all reads as a client that emptied
--   one, and the recompose takes it off;
--
-- * a planning line closed by HORIZONTAL SPACE — the region is the whole line,
--   but it is rebuilt from each entry's raw text plus the line ending, and
--   whatever trails the LAST entry is in neither;
--
-- * a planning line spelling one KEYWORD TWICE — the parse keeps the last stamp
--   alone, the region is the whole line, and the recompose writes the one entry
--   that survived;
--
-- * a file whose LAST line is an unterminated planning line — the recompose
--   closes it with the line ending, so the subtree comes back one byte longer.
lensRepresentable :: DocSpec -> Bool
lensRepresentable ds = all holdable (dsEntries ds) && terminated
  where
    holdable e = esProperties e /= Just []
                 && not (repeatedKey e)
                 && not (esTrail e > 0 && planningLast e)
    terminated = dsFinalEol ds
                 || not (any (\e -> esGap e == 0 && planningLast e)
                             (take 1 (reverse (dsEntries ds))))
    planningLast e = not (null (esPlanning e)) && null (esBody e)
                     && esLogbook e == Nothing && esProperties e == Nothing
    repeatedKey e = length (nub (map fst (esPlanning e))) /= length (esPlanning e)

-- | Has R a BLANK top entry — one carrying none of the six column sub-spans?
-- Such an entry keeps its outline extent and loses its record, so the surviving
-- extents no longer meet.  Excluded from the tiling property, and reported.
noBlankTopEntry :: Rendered -> Bool
noBlankTopEntry r = not (any blank (rdEntries r))
  where blank e = exLevel e == 1 && exBlank e

-- Timestamp helpers

-- | T read as the one timestamp it spells, 'Nothing' where it spells anything
-- else.  The only door in: the parser exposes no timestamp entry point of its
-- own, and 'orgParse' over the bracketed text is what every fixture uses.
readTimestamp :: Text -> Maybe Timestamp
readTimestamp t = case orgParse defaultContext t of
  (elems, _ctx, Nothing) -> case bare elems of
    [ETimestamp ts] -> Just ts
    _other -> Nothing
  (_elems, _ctx, Just _err) -> Nothing

-- | Can TS's compact flag be SPELLED?  Both ends timed and on one day — the two
-- guards beside the flag itself.
compactible :: Timestamp -> Bool
compactible ts = maybe False both (tsEnd ts)
  where both e = tsmHasTime (tsStart ts) && tsmHasTime e
                 && Time.utctDay (tsmTime (tsStart ts)) == Time.utctDay (tsmTime e)

-- | A word for the weekday slot: letters in any script, any length.
weekdayWord :: Int -> Text
weekdayWord n = words' !! (abs n `mod` length words')
  where words' = ["M", "Mon", "Monday", "do", "понедельник", "月曜日", "Xyzzy"]

-- | SOURCE with every weekday slot respelled WORD.  The slot is the letter run
-- behind a digit and a space, which in a stamp is the weekday and nothing else:
-- a repeater's unit sits behind a DIGIT with no space, and its cookie opens with
-- @.@, @+@, @-@ or a digit.
respellWeekday :: Text -> Text -> Text
respellWeekday word source = T.pack (go (T.unpack source))
  where
    go (d : ' ' : rest)
      | isDigit d, not (null run) = d : ' ' : T.unpack word <> go tail'
      where (run, tail') = span isAlpha rest
    go (c : cs) = c : go cs
    go [] = []

-- | A preamble ahead of a generated document, ending in whitespace so the
-- element behind it cannot merge with the document's first.
preamble :: Int -> Text
preamble n = pads !! (abs n `mod` length pads)
  where pads = ["", "pad\n", "#+TITLE: t\n", "* Pad\n\n", "<2024-01-15 Mon>\n\n"]
