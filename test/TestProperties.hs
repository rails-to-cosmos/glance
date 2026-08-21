-- | @AGENTS.hs@'s laws asked as universals; @GLANCE_QC_SEED@ unfixes the seed.
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

-- | A generator with a narrow image passes forever, so this group lands FIRST.
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

    -- A document the parser refuses makes every property below vacuous.
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

  , testCase "the edit sets reach both sides of the acceptance boundary" $ do
      let sets = editSample sampleSize
          legals = length [ () | (doc, es) <- sets, legal doc es ]
      assertBool ("legal edit sets: " <> show legals <> " of " <> show sampleSize)
                 (legals >= 60 && sampleSize - legals >= 60)
  ]
  where sampleSize = 400

-- | Equalities against offsets the generator counted: NOT self-consistent.
answerSpec :: TestTree
answerSpec = testGroup "Spans, against the offsets they were written at"
  [ testProperty "every sub-span is where the generator spelled it" $ \ds ->
      let r = render ds
      in  withParse r $ \_ctx elems ->
            map spans (headlinesOf elems) === map exSpans (rdEntries r)

    -- Fold and maximum agree exactly while 'spanParts' is in source order.
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

    -- A BLANK top entry is left out ON PURPOSE: it keeps its extent, loses its record.
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

algebraSpec :: TestTree
algebraSpec = testGroup "Spans, as an algebra"
  [ -- 'assertInvariants' (TestSpans) with its 24 documents replaced by a
    -- generator.
    testProperty "sub-spans nest inside hsFull, in order, never overlapping" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \_ctx elems -> conjoin (map nested (headlinesOf elems))

  , testProperty "hsFull never covers trailing whitespace" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \_ctx elems -> conjoin
            [ T.stripEnd slice === slice
            | h <- headlinesOf elems
            , let slice = sliceSpan (rdText r) (hsFull (spans h)) ]

    -- The parse's OWN context: under 'defaultContext' a '#+TODO:' keyword reads as title.
  , testProperty "hsFull reparses to the headline it came from" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \ctx elems -> conjoin
            [ reparses ctx (sliceSpan (rdText r) (hsFull (spans h))) (EHeadline h)
            | h <- headlinesOf elems ]

  , testProperty "an element span reparses to the element it came from" $ \(Wild ds) ->
      let r = render ds
      in  withParse r $ \ctx elems -> conjoin
            [ reparses ctx (sliceSpan (rdText r) (spanOf e)) (valueOf e) | e <- elems ]

    -- The SEMANTIC half of 'stripSpans': a leaked span fails this and nothing else.
  , testProperty "stripSpans leaves no offset behind" $ \(Wild ds) pad ->
      let doc = rdText (render ds)
          ahead = preamble pad
      in  parses (ahead <> doc) ==>
            bareOf doc === drop (length (bareOf ahead)) (bareOf (ahead <> doc))
  ]

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

-- | Written out rather than read off 'spanParts', which it is the oracle for.
sourceOrder :: HeadlineSpans -> [(String, Span)]
sourceOrder hs = there before <> sortOn (spanStart . snd) (there planning) <> there after
  where
    there names = [ (name, sp) | (name, get) <- names, Just sp <- [get hs] ]
    (before, rest) = splitAt 4 namedSubSpans
    (planning, after) = splitAt 3 rest

-- | 'applyEdits' checks NEIGHBOURS; 'legal' is the quadratic rule it reduces.
editSpec :: TestTree
editSpec = testGroup "applyEdits"
  [ testProperty "accepts exactly the disjoint sets" $ \ds plan texts ->
      let doc = rdText (render ds)
          es = editsOver doc plan texts
      in  counterexample (show es) (isRight (applyEdits doc es) === legal doc es)

  , -- The two below take sets that are DISJOINT BY CONSTRUCTION rather than by a
    -- guard: a precondition discarding four cases in five is barely run.
    testProperty "the answer does not depend on the order they were named" $ \ds plan texts k ->
      let doc = rdText (render ds)
          es = disjointEdits doc plan texts
      in  distinctKeys es ==>
            counterexample (show es) (applyEdits doc es === applyEdits doc (rotate k es))

  , testProperty "length is the document's plus what each edit adds" $ \ds plan texts ->
      let doc = rdText (render ds)
          es = disjointEdits doc plan texts
      in  counterexample (show es) (property (legal doc es))
            .&&. (T.length <$> applyEdits doc es)
                   === Right (T.length doc + sum (map delta es))

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

-- | Byte-identical UP TO THE LINE END: a write spells no trailing space.
lensSpec :: TestTree
lensSpec = testGroup "Subtree lens"
  [ testPropertyWith 60 "decompose then recompose is the subtree, its line ends trimmed" $ \ds ->
      let r = render ds
      in  lensRepresentable ds ==> ioProperty (withLoaded r (\_doc recs -> conjoin
            [ recomposedSubtree q (headlineParts q) === asWritten (subtreeText q) | q <- recs ]))

  , testPropertyWith 60 "the body's lines are the subtree's, minus the regions" $ \ds ->
      let r = render ds
      in  ioProperty (withLoaded r (\_doc recs -> conjoin (map ownsEachByte recs)))

  , testPropertyWith 20 "re-decomposing an edited body answers the parts it was given" $ \ds ->
      let r = render ds
      in  lensRepresentable ds ==> ioProperty (withLoaded r
            (\doc recs -> conjoin (map (reDecomposes doc) recs)))
  ]

-- | The TOTAL direction is render → parse → equal; text → render is lossy.
timestampSpec :: TestTree
timestampSpec = testGroup "Timestamps"
  [ testProperty "a value survives render then parse" $ \(TsImage ts) ->
      counterexample (T.unpack (showt ts)) (readTimestamp (showt ts) === Just ts)

  , testProperty "a render is a fixed point of parse then render" $ \(TsAny ts) ->
      counterexample (T.unpack (showt ts))
        ((showt <$> readTimestamp (showt ts)) === Just (showt ts))

  , testProperty "any weekday spelling reads to the same value" $ \(TsImage ts) word ->
      let source = showt ts
          reworded = respellWeekday (weekdayWord word) source
      in  counterexample (T.unpack source <> " -> " <> T.unpack reworded)
            (readTimestamp reworded === Just ts)

    -- 'compactly' guards on three conditions and only the FLAG is exercised.
  , testProperty "a compact flag the ends cannot hold comes back cleared" $ \(TsAny ts) ->
      tsCompactRange ts && not (compactible ts) ==>
        counterexample (T.unpack (showt ts))
          (readTimestamp (showt ts) === Just ts { tsCompactRange = False })
  ]

-- | A refusal returns zero elements AND the caller's context untouched.
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

breakAt :: Int -> DocSpec -> DocSpec
breakAt k ds = ds { dsEntries = zipWith at [0 ..] (dsEntries ds) }
  where n = max 1 (length (dsEntries ds))
        at i e | i == (k :: Int) `mod` n = e { esBody = esBody e <> [brokenRange] }
               | otherwise = e

-- | Named here rather than read off 'headlineSpanParts', which it is the oracle for.
namedSubSpans :: [(String, HeadlineSpans -> Maybe Span)]
namedSubSpans =
  [ ("hsTodo", hsTodo), ("hsPriority", hsPriority), ("hsTitle", hsTitle), ("hsTags", hsTags)
  , ("hsSchedule", hsSchedule), ("hsDeadline", hsDeadline), ("hsClosed", hsClosed)
  , ("hsProperties", hsProperties) ]

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

reparses :: Context -> Text -> Element -> Property
reparses ctx slice want = counterexample (T.unpack slice) $
  case orgParse ctx slice of
    (elems, _ctx, Nothing) -> bare elems === [stripSpans want]
    (_elems, _ctx, Just _err) -> counterexample "parse error" (property False)

-- | Bytes rather than 'TestDefaults.orgFile': the decode must be UTF-8.
withLoaded :: Rendered -> (Text -> [HeadlineRecord] -> Property) -> IO Property
withLoaded r k = withTempDirNamed "prop" $ \dir -> do
  let doc = rdText r
      path = dir </> "prop.org"
  BS.writeFile path (TE.encodeUtf8 doc)
  loaded <- loadFile path
  pure $ counterexample (T.unpack doc) $ case loaded of
    Right recs -> k doc recs
    Left why -> counterexample ("load failed: " <> show why) (property False)

-- | The region SPANS are module-private, so this is the subsequence alone.
ownsEachByte :: HeadlineRecord -> Property
ownsEachByte r = counterexample (show (subtreeText r, hpBody parts)) $
  property (subsequence (T.lines (hpBody parts)) (T.lines (subtreeText r)))
    .&&. counterexample "the logbook is in the body too"
           (property (T.null (hpLogbook parts)
                      || not (T.strip (hpLogbook parts) `T.isInfixOf` hpBody parts)))
  where parts = headlineParts r

settled :: HeadlineParts -> HeadlineParts
settled parts = parts { hpBody    = asWritten (hpBody parts)
                      , hpLogbook = T.stripEnd (asWritten (hpLogbook parts)) }

-- | An INDEPENDENT spelling of what 'recomposedSubtree' enforces, CRLF included.
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

-- | The QUADRATIC rule 'applyEdits' reduces to a neighbour check.
legal :: Text -> [Edit] -> Bool
legal doc es = all inBounds es && and [ apart a b | (a, b) <- pairs (map editSpan es) ]
  where
    inBounds (Edit (Span s e) _) = s >= 0 && s <= e && e <= T.length doc
    apart (Span s1 e1) (Span s2 e2) = e1 <= s2 || e2 <= s1
    pairs xs = [ (a, b) | a : rest <- tailsOf xs, b <- rest ]
    tailsOf [] = []
    tailsOf xs@(_ : rest) = xs : tailsOf rest

distinctKeys :: [Edit] -> Bool
distinctKeys es = length (dedup keys) == length keys
  where keys = [ (spanStart sp, spanEnd sp) | Edit sp _ <- es ]
        dedup = foldr (\x acc -> x : filter (/= x) acc) []

delta :: Edit -> Int
delta (Edit (Span s e) new) = T.length new - (e - s)

editsOver :: Text -> [(Int, Int)] -> [String] -> [Edit]
editsOver doc plan texts =
  [ Edit (Span at (at + width b)) (T.pack (take 6 t))
  | ((a, b), t) <- zip (take 4 plan) (cycle (if null texts then ["x"] else texts))
  , let at = (a `mod` (len + 5)) - 2 ]
  where len = T.length doc
        width b = (b `mod` 12) - 1

-- | Drawn through QuickCheck's own generator: the distribution properties see.
editSample :: Int -> [(Text, [Edit])]
editSample n = [ (doc, editsOver doc plan texts)
               | (i, ds) <- zip [1 ..] (docSample n)
               , let doc = rdText (render ds)
                     (plan, texts) = drawn i ]
  where drawn i = unGen (variant i arbitrary) (mkQCGen 7) (4 + i `mod` 24)

-- | DISJOINT AND IN BOUNDS by construction; zero-width inserts are reachable.
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

-- | The shapes the lens cannot hold, reported rather than weakened away.
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

-- | A BLANK entry keeps its extent and loses its record, so survivors do not meet.
noBlankTopEntry :: Rendered -> Bool
noBlankTopEntry r = not (any blank (rdEntries r))
  where blank e = exLevel e == 1 && exBlank e

-- | The only door in: the parser exposes no timestamp entry point of its own.
readTimestamp :: Text -> Maybe Timestamp
readTimestamp t = case orgParse defaultContext t of
  (elems, _ctx, Nothing) -> case bare elems of
    [ETimestamp ts] -> Just ts
    _other -> Nothing
  (_elems, _ctx, Just _err) -> Nothing

compactible :: Timestamp -> Bool
compactible ts = maybe False both (tsEnd ts)
  where both e = tsmHasTime (tsStart ts) && tsmHasTime e
                 && Time.utctDay (tsmTime (tsStart ts)) == Time.utctDay (tsmTime e)

weekdayWord :: Int -> Text
weekdayWord n = words' !! (abs n `mod` length words')
  where words' = ["M", "Mon", "Monday", "do", "понедельник", "月曜日", "Xyzzy"]

-- | The slot is the letter run behind a digit AND A SPACE; a repeater's unit has none.
respellWeekday :: Text -> Text -> Text
respellWeekday word source = T.pack (go (T.unpack source))
  where
    go (d : ' ' : rest)
      | isDigit d, not (null run) = d : ' ' : T.unpack word <> go tail'
      where (run, tail') = span isAlpha rest
    go (c : cs) = c : go cs
    go [] = []

preamble :: Int -> Text
preamble n = pads !! (abs n `mod` length pads)
  where pads = ["", "pad\n", "#+TITLE: t\n", "* Pad\n\n", "<2024-01-15 Mon>\n\n"]
