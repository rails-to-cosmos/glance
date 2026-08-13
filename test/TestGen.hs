-- | The document generator: a 'DocSpec' says WHAT to spell, 'render' spells it
-- out and RECORDS where every component landed.  So a property can compare the
-- parser's spans against offsets counted by code the parser never ran — the
-- suite's independent-oracle idiom ('TestDefaults.headlinesOf',
-- @TestFilter@'s layout list) carried down to the span layer.
--
-- A RENDERER IS A SPELLING, A PARSER IS A RECOGNITION.  Org's spelling is fixed
-- and small — stars, a space, a keyword, @[#A]@, words, @:a:b:@, a planning
-- line, a drawer.  Its recognition is where every subtlety in @AGENTS.hs@'s
-- Parser section lives, and that is the half a generated document is worth
-- asking about.  NEVER 'TextShow': that is the lossy REPL re-serializer, and
-- rendering through it would test the parser against its own inverse and hide
-- every bug the two share.
--
-- OFFSETS ARE COUNTED IN CHARACTERS, which is what a 'Span' is — so a generator
-- that spells @Привет@ and a parser that answers 6 rather than 12 agree by
-- construction.
--
-- TWO ALPHABETS, and the property kind picks one.  'arbitrary' draws words whose
-- parse the generator can PREDICT; 'Wild' draws words that CHANGE the parse and
-- is for the properties needing no predicted answer.
--
-- WHAT THE GENERATOR WILL NOT SPELL ('normEntry'), each because the parser reads
-- it in a way an oracle would have to MIRROR rather than predict:
--
-- * a headline with no title but with tags — @* :a:b:@ hands the colons to the
--   TITLE, org spelling tags only after one (@blankEntry@'s own note);
--
-- A HEADLINE WITH NO TITLE BUT WITH A KEYWORD OR A PRIORITY IS DRAWN, and was
-- not until the parser stopped letting one eat the line under it: 'lexemeP'
-- takes horizontal space now, so @* TODO@ ends where its line does.  The
-- exclusion that used to sit here is what this case is the guard for.
--
-- Trailing horizontal space likewise goes on ANY line, a title's included: it
-- detached the planning line under it until @planningP@ skipped it.
module TestGen ( Broken (..)
               , DocSpec (..)
               , Eol (..)
               , EntrySpec (..)
               , Expected (..)
               , PlanKey (..)
               , Rendered (..)
               , TsAny (..)
               , TsImage (..)
               , TsRange (..)
               , TsSpec (..)
               , Wild (..)
               , brokenRange
               , docSample
               , eolText
               , expectedExtents
               , keywordsOf
               , planWord
               , plainWords
               , render
               , shapeFloors
               , shapesOf
               , wildWords
               ) where

import Control.Monad (forM, forM_, when)
import Data.List (nub, sort, tails)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Test.QuickCheck ( Arbitrary (arbitrary, shrink), Gen, choose, elements
                       , frequency, listOf, listOf1, resize, shrinkList
                       , sized, variant, vectorOf )
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

import qualified Data.Text as T
import qualified Data.Time as Time

import Data.Org ( HeadlineSpans (..), Span (..), Timestamp (..), TimestampRepeaterInterval (..)
                , TimestampRepeaterSign (..), TimestampRepeaterType (..)
                , TimestampStatus (..), TimestampUnit (..), TimestampWarningInterval (..)
                , TsMoment (..) )

-- The spec

data Eol = LF | CRLF
  deriving (Eq, Show)

eolText :: Eol -> Text
eolText LF = "\n"
eolText CRLF = "\r\n"

data PlanKey = KScheduled | KDeadline | KClosed
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The word org opens KEY's planning entry with.
planWord :: PlanKey -> Text
planWord KScheduled = "SCHEDULED"
planWord KDeadline  = "DEADLINE"
planWord KClosed    = "CLOSED"

-- | A document as a STRUCTURE — what to spell, never where it lands.
data DocSpec = DocSpec
  { dsKeywords :: !(Maybe ([Text], [Text]))  -- ^ the @#+TODO:@ cycle this document declares.
  , dsEol      :: !Eol                       -- ^ LF or CRLF, the DOCUMENT's own.
  , dsFinalEol :: !Bool                      -- ^ does the last line end, or does the file.
  , dsEntries  :: ![EntrySpec]
  } deriving (Eq, Show)

data EntrySpec = EntrySpec
  { esLevel      :: !Int                       -- ^ 1..4.
  , esTodo       :: !(Maybe Text)              -- ^ drawn from 'keywordsOf', so recognition is reachable.
  , esPriority   :: !(Maybe Char)
  , esTitle      :: ![Text]                    -- ^ possibly empty: the blank entry is a case.
  , esTags       :: ![Text]
  , esPlanning   :: ![(PlanKey, TsSpec)]       -- ^ IN THE ORDER THEY ARE WRITTEN — the permutation.
  , esProperties :: !(Maybe [(Text, Text)])    -- ^ 'Nothing' is no drawer, @Just []@ an empty one.
  , esLogbook    :: !(Maybe [Text])
  , esBody       :: ![Text]                    -- ^ whole lines.
  , esIndent     :: !Int                       -- ^ the drawer's own indentation.
  , esHeadTrail  :: !Int                       -- ^ horizontal space closing the TITLE line.
  , esTrail      :: !Int                       -- ^ horizontal space closing the entry's LAST line.
  , esGap        :: !Int                       -- ^ blank lines before the next entry.
  } deriving (Eq, Show)

-- | A timestamp as SOURCE TEXT to spell.  The generator predicts only WHERE it
-- lands, so nothing here has to name the value the parser will read.
data TsSpec = TsSpec
  { tspActive  :: !Bool
  , tspDate    :: !(Integer, Int, Int)
  , tspWeekday :: !Text                 -- ^ letters in any script; the parser drops them.
  , tspTime    :: !(Maybe (Int, Int))
  , tspRange   :: !(Maybe TsRange)
  , tspCookie  :: !(Maybe Text)         -- ^ spelled verbatim, e.g. @+1w@ or @+1m -3d@.
  } deriving (Eq, Show)

-- | A range half: org's compact same-day spelling, or the @--@ one.
data TsRange = Compact (Int, Int)
             | Dashed (Integer, Int, Int) Text (Maybe (Int, Int))
  deriving (Eq, Show)

-- | The recognized keywords a document's entries may spell: org's own pair plus
-- whatever its @#+TODO:@ line declares ('setTodo' unions).
keywordsOf :: DocSpec -> [Text]
keywordsOf ds = ["TODO", "DONE"] <> maybe [] (uncurry (<>)) (dsKeywords ds)

-- The renderer that knows the answer

-- | A rendered document and, per entry, the offsets it was spelled at.
data Rendered = Rendered { rdText :: !Text, rdEntries :: ![Expected] }
  deriving (Eq, Show)

-- | Where one entry landed: the spans the parser owes for it, its level, and
-- whether it is a blank entry (no column sub-span, so no row).
data Expected = Expected
  { exSpans :: !HeadlineSpans
  , exLevel :: !Int
  , exStart :: !Int
  , exBlank :: !Bool
  } deriving (Eq, Show)

-- | A writer over a character counter.  Hand-rolled rather than @mtl@'s: the
-- suite's build-depends grow by QuickCheck alone.
newtype Emit a = Emit { runEmit :: (Int, [Text]) -> (a, (Int, [Text])) }

instance Functor Emit where
  fmap f (Emit g) = Emit (\s -> case g s of (a, s') -> (f a, s'))

instance Applicative Emit where
  pure a = Emit (\s -> (a, s))
  Emit f <*> Emit g = Emit (\s -> case f s of
                                    (h, s1) -> case g s1 of (a, s2) -> (h a, s2))

instance Monad Emit where
  Emit g >>= k = Emit (\s -> case g s of (a, s1) -> runEmit (k a) s1)

-- | Write T and answer the span it occupies.
emit :: Text -> Emit Span
emit t = Emit (\(at, out) -> (Span at (at + T.length t), (at + T.length t, t : out)))

-- | 'emit', for text whose offsets nobody records — the whitespace BETWEEN
-- components, which is what makes "sub-spans are tight" checkable.
emit_ :: Text -> Emit ()
emit_ t = () <$ emit t

-- | SPEC spelled out, and where each entry's components landed.
render :: DocSpec -> Rendered
render ds = Rendered text entries
  where
    (entries, (_at, out)) = runEmit (docE ds) (0, [])
    whole = T.concat (reverse out)
    text | dsFinalEol ds = whole
         | otherwise     = fromMaybe whole (T.stripSuffix (eolText (dsEol ds)) whole)

docE :: DocSpec -> Emit [Expected]
docE ds = do
  case dsKeywords ds of
    Nothing -> pure ()
    Just (active, inactive) ->
      emit_ ("#+TODO: " <> T.unwords active <> " |"
             <> T.concat [" " <> k | k <- inactive] <> eol)
  mapM (entryE eol) (dsEntries ds)
  where eol = eolText (dsEol ds)

entryE :: Text -> EntrySpec -> Emit Expected
entryE eol e = do
  stars <- emit (T.replicate (esLevel e) "*")
  emit_ " "
  todoSp <- optionalE (esTodo e) (\k -> emit k <* emit_ " ")
  prioSp <- optionalE (esPriority e) (\c -> emit ("[#" <> T.singleton c <> "]") <* emit_ " ")
  titleSp <- emitWords (esTitle e)
  tagsSp <- case esTags e of
    []   -> pure Nothing
    tags -> do emit_ " "
               Just <$> emit (":" <> T.intercalate ":" tags <> ":")
  -- CLOSING THE TITLE LINE, which detached the planning line under it until
  -- `planningP' skipped it.  After the tags, so neither span covers it.
  emit_ (T.replicate (esHeadTrail e) " ")
  plan <- planningE eol (esPlanning e)
  drawSp <- drawerE eol ind (esProperties e)
  logbookE eol ind (esLogbook e)
  forM_ (esBody e) (\l -> emit_ (eol <> l))
  emit_ (T.replicate (esTrail e) " ")
  emit_ eol
  emit_ (T.replicate (esGap e) eol)
  let hs = HeadlineSpans { hsStars      = stars
                         , hsTodo       = todoSp
                         , hsPriority   = prioSp
                         , hsTitle      = titleSp
                         , hsTags       = tagsSp
                         , hsSchedule   = lastFor KScheduled plan
                         , hsDeadline   = lastFor KDeadline plan
                         , hsClosed     = lastFor KClosed plan
                         , hsProperties = drawSp
                         }
  pure Expected { exSpans = hs
                , exLevel = esLevel e
                , exStart = spanStart stars
                  -- A repeated planning keyword keeps its LAST timestamp, so a
                  -- span the line spelled twice is the second one's.
                , exBlank = all (== Nothing) [ todoSp, prioSp, titleSp, tagsSp
                                             , lastFor KScheduled plan
                                             , lastFor KDeadline plan ]
                }
  where
    ind = T.replicate (esIndent e) " "
    lastFor k plan = case [sp | (k', sp) <- plan, k' == k] of
      [] -> Nothing
      sps -> Just (last sps)

optionalE :: Maybe a -> (a -> Emit Span) -> Emit (Maybe Span)
optionalE Nothing _ = pure Nothing
optionalE (Just a) k = Just <$> k a

-- | WORDS separated by one space, spanned first to last.
emitWords :: [Text] -> Emit (Maybe Span)
emitWords = go Nothing
  where
    go acc [] = pure acc
    go acc (w : ws) = do
      when (isJust acc) (emit_ " ")
      sp <- emit w
      go (Just (maybe sp (<> sp) acc)) ws

-- | The planning LINE: every entry on one line, each span the bracketed stamp
-- alone — the keyword is not part of it.
planningE :: Text -> [(PlanKey, TsSpec)] -> Emit [(PlanKey, Span)]
planningE _eol [] = pure []
planningE eol entries = do
  emit_ eol
  forM (zip [0 :: Int ..] entries) $ \(i, (k, ts)) -> do
    when (i > 0) (emit_ " ")
    emit_ (planWord k <> ": ")
    sp <- emit (renderTs ts)
    pure (k, sp)

-- | The drawer, spanned from the LINE START of @:PROPERTIES:@ — indentation
-- included — through @:END:@ and no further.
drawerE :: Text -> Text -> Maybe [(Text, Text)] -> Emit (Maybe Span)
drawerE _eol _ind Nothing = pure Nothing
drawerE eol ind (Just pairs) = do
  emit_ eol
  opened <- emit (ind <> ":PROPERTIES:")
  forM_ pairs (\(k, v) -> emit_ (eol <> ind <> ":" <> k <> ": " <> v))
  emit_ eol
  closed <- emit (ind <> ":END:")
  pure (Just (Span (spanStart opened) (spanEnd closed)))

-- | The logbook, which the PARSER never reads: it is body text here, and only
-- the subtree lens locates it, textually.
logbookE :: Text -> Text -> Maybe [Text] -> Emit ()
logbookE _eol _ind Nothing = pure ()
logbookE eol ind (Just ls) = do
  emit_ (eol <> ind <> ":LOGBOOK:")
  forM_ ls (\l -> emit_ (eol <> ind <> l))
  emit_ (eol <> ind <> ":END:")

renderTs :: TsSpec -> Text
renderTs t = case tspRange t of
  Nothing -> bracket (head' <> cookie)
  Just (Compact (h, m)) -> bracket (head' <> "-" <> clock h m <> cookie)
  Just (Dashed d wd tm) -> bracket (head' <> cookie) <> "--" <> bracket (stamp d wd tm)
  where
    (open, close) = if tspActive t then ('<', '>') else ('[', ']')
    bracket b = T.cons open (T.snoc b close)
    head' = stamp (tspDate t) (tspWeekday t) (tspTime t)
    cookie = maybe "" (" " <>) (tspCookie t)
    stamp (y, mo, d) wd tm =
      pad 4 y <> "-" <> pad 2 mo <> "-" <> pad 2 d <> " " <> wd
        <> maybe "" (\(h, m) -> " " <> clock h m) tm
    clock h m = pad 2 h <> ":" <> pad 2 m
    pad n v = T.justifyRight n '0' (T.pack (show v))

-- | Where each entry's outline extent runs over a document of LEN characters:
-- from its own stars to the next entry at its level or shallower, else to EOF.
--
-- ORG'S RULE SPELLED A SECOND TIME, deliberately: a quadratic scan forward
-- rather than @subtreeSpans@' right-to-left stack fold, so the two agree only
-- if the rule is right.
expectedExtents :: Int -> [Expected] -> [Span]
expectedExtents len entries =
  [ Span (exStart e) (endOf (exLevel e) rest)
  | e : rest <- init (tails entries) ]
  where
    endOf lvl rest = case dropWhile ((> lvl) . exLevel) rest of
      (next : _) -> exStart next
      []         -> len

-- The alphabets

-- | Words whose parse the generator can predict.
plainWords :: [Text]
plainWords = ["a", "task", "Привет", "проверка", "note"]

-- | Words that CHANGE the parse.  Every one of them still PARSES: a document
-- that fails the parser makes the algebra properties vacuous rather than sharp,
-- and refusal is 'Broken'\'s subject.
wildWords :: [Text]
wildWords = plainWords <>
  [ "TODO", "DONE", "*bold*", "[[https://x][y]]", ":a:", "<2026-01-01 Thu>"
  , "key::value", "#+TITLE:", "-", "|", "%?", "[#A]", "a:b" ]

plainTags :: [Text]
plainTags = ["a", "b", "work", "тег"]

wildTags :: [Text]
wildTags = plainTags <> ["@home", "a-b", "x%y", "TODO", "ARCHIVE"]

propertyKeys :: [Text]
propertyKeys = ["K", "OWNER", "ORG_GLANCE_ID", "TELE2", "ЖКХ"]

weekdays :: [Text]
weekdays = ["Mon", "Thu", "do", "zo", "понедельник", "月曜日"]

cookies :: [Text]
cookies = ["+1w", "++2d", ".+3m", "-3d", "--7d", "+1m -3d"]

customKeywords :: [Text]
customKeywords = ["NEXT", "STARTED", "WAITING", "SKIP", "CANCELLED"]

-- Generators

-- | The PLAIN generator: every answer property reads through this one.
instance Arbitrary DocSpec where
  arbitrary = genDoc plainWords plainTags
  shrink = shrinkDoc

-- | The ADVERSARIAL generator, for the properties needing no predicted answer.
newtype Wild = Wild DocSpec
  deriving (Eq, Show)

instance Arbitrary Wild where
  arbitrary = Wild <$> genDoc wildWords wildTags
  shrink (Wild ds) = map Wild (shrinkDoc ds)

-- | A document with a MISMATCHED RANGE spliced into one entry's body: org's
-- documented mid-word refusal, which fails the WHOLE file.
data Broken = Broken DocSpec Int
  deriving (Eq, Show)

instance Arbitrary Broken where
  arbitrary = Broken <$> genDoc plainWords plainTags <*> choose (0, 8)
  shrink (Broken ds k) = [Broken ds' k | ds' <- shrinkDoc ds]

-- | The text org refuses: a range whose halves disagree about the bracket kind,
-- so the timestamp ends mid-word and the top loop finds no whitespace after it.
brokenRange :: Text
brokenRange = "[2023-07-15 Sat 15:54]--<2023-07-15 Sat 17:10>"

genDoc :: [Text] -> [Text] -> Gen DocSpec
genDoc words' tags = sized $ \n -> do
  keywords <- frequency [(2, pure Nothing), (1, Just <$> genCycle)]
  eol <- elements [LF, LF, CRLF]
  finalEol <- frequency [(4, pure True), (1, pure False)]
  count <- choose (1, max 1 (min 4 (1 + n `div` 6)))
  entries <- vectorOf count (genEntry words' tags (keywordsOf (DocSpec keywords eol finalEol [])))
  pure DocSpec { dsKeywords = keywords
               , dsEol = eol
               , dsFinalEol = finalEol
               , dsEntries = entries }

genCycle :: Gen ([Text], [Text])
genCycle = do
  active <- sublist 1 2 customKeywords
  inactive <- sublist 0 2 (filter (`notElem` active) customKeywords)
  pure (active, inactive)

genEntry :: [Text] -> [Text] -> [Text] -> Gen EntrySpec
genEntry words' tags keywords = do
  level <- frequency [(6, pure 1), (2, pure 2), (1, pure 3), (1, pure 4)]
  todo <- maybeGen 1 3 (elements keywords)
  priority <- maybeGen 1 4 (elements "ABCD")
  title <- frequency [(1, pure []), (9, resize 3 (listOf1 (elements words')))]
  tags' <- frequency [(2, pure []), (1, sublist 1 3 tags)]
  planning <- genPlanning
  properties <- maybeGen 1 2 (frequency [ (1, pure [])
                                        , (4, resize 3 (listOf1 (genPair words'))) ])
  logbook <- maybeGen 1 6 (resize 2 (listOf1 (genLogLine words')))
  body <- resize 3 (listOf (T.unwords <$> resize 3 (listOf1 (elements words'))))
  indentN <- frequency [(3, pure 0), (1, pure 2)]
  trail <- frequency [(4, pure 0), (1, choose (1, 2))]
  headTrail <- frequency [(4, pure 0), (1, choose (1, 2))]
  gap <- frequency [(3, pure 0), (2, choose (1, 2))]
  pure (normEntry EntrySpec { esLevel = level
                            , esTodo = todo
                            , esPriority = priority
                            , esTitle = title
                            , esTags = tags'
                            , esPlanning = planning
                            , esProperties = properties
                            , esLogbook = logbook
                            , esBody = body
                            , esIndent = indentN
                            , esHeadTrail = headTrail
                            , esTrail = trail
                            , esGap = gap })

-- | A LOGBOOK line, which is body text to the parser.  Never a bare @:END:@ and
-- never opening a drawer of its own, either of which would close the region
-- early or open a second one.
genLogLine :: [Text] -> Gen Text
genLogLine words' = do
  ws <- resize 3 (listOf1 (elements words'))
  pure ("CLOCK: " <> T.unwords ws)

genPair :: [Text] -> Gen (Text, Text)
genPair words' = (,) <$> elements propertyKeys <*> elements words'

genPlanning :: Gen [(PlanKey, TsSpec)]
genPlanning = frequency
  [ (3, pure [])
  , (4, do keys <- sublist 1 3 [minBound .. maxBound]
           order <- shuffled keys
           mapM (\k -> (,) k <$> genTsSpec) order)
    -- A keyword the line spells twice: the LAST stamp wins, and only a
    -- generated document reaches it.
  , (1, do k <- elements [minBound .. maxBound]
           a <- genTsSpec
           b <- genTsSpec
           pure [(k, a), (k, b)])
  ]

genTsSpec :: Gen TsSpec
genTsSpec = do
  active <- arbitrary
  date <- genDate
  weekday <- elements weekdays
  time <- maybeGen 1 2 genClock
  range <- case time of
    Nothing -> maybeGen 1 4 (Dashed <$> genDate <*> elements weekdays <*> pure Nothing)
    Just _  -> frequency [ (4, pure Nothing)
                         , (1, Just . Compact <$> genClock)
                         , (1, Just <$> (Dashed <$> genDate <*> elements weekdays
                                                <*> (Just <$> genClock))) ]
  cookie <- maybeGen 1 3 (elements cookies)
  pure TsSpec { tspActive = active
              , tspDate = date
              , tspWeekday = weekday
              , tspTime = time
              , tspRange = range
              , tspCookie = cookie }

genDate :: Gen (Integer, Int, Int)
genDate = (,,) <$> choose (2020, 2030) <*> choose (1, 12) <*> choose (1, 28)

genClock :: Gen (Int, Int)
genClock = (,) <$> choose (0, 23) <*> choose (0, 59)

-- | 'Just' with probability WEIGHT in OF.
maybeGen :: Int -> Int -> Gen a -> Gen (Maybe a)
maybeGen weight from g = frequency [(from - weight, pure Nothing), (weight, Just <$> g)]

-- | LO to HI distinct members of XS, in the order XS spells them.
sublist :: Eq a => Int -> Int -> [a] -> Gen [a]
sublist lo hi xs = do
  n <- choose (lo, min hi (length xs))
  pick n xs
  where
    pick 0 _ = pure []
    pick _ [] = pure []
    pick n ys = do
      i <- choose (0, length ys - 1)
      let y = ys !! i
      (y :) <$> pick (n - 1 :: Int) (filter (/= y) ys)

shuffled :: [a] -> Gen [a]
shuffled [] = pure []
shuffled xs = do
  i <- choose (0, length xs - 1)
  let (before, after) = splitAt i xs
  case after of
    (y : ys) -> (y :) <$> shuffled (before <> ys)
    []       -> pure before

-- | What the generator refuses to spell, applied to every entry it makes and
-- every entry a shrink proposes.  Both clauses are the module header's.
normEntry :: EntrySpec -> EntrySpec
normEntry e
  | null (esTitle e) = e { esTags = [] }
  | otherwise        = e

-- Shrinking
--
-- Shrink the SPEC, never the text: a text-level shrinker produces bytes that
-- need not be a legal org document, and a minimal counterexample that does not
-- parse teaches nothing.

shrinkDoc :: DocSpec -> [DocSpec]
shrinkDoc ds = concat
  [ [ ds { dsEntries = es } | es <- shrinkList shrinkEntry (dsEntries ds), not (null es) ]
    -- The cycle and the keywords that name it move TOGETHER: a custom keyword
    -- left standing over a dropped '#+TODO:' line is title text.
  , [ ds { dsKeywords = Nothing, dsEntries = map plainTodo (dsEntries ds) }
    | isJust (dsKeywords ds) ]
  , [ ds { dsEol = LF } | case dsEol ds of { CRLF -> True; LF -> False } ]
  , [ ds { dsFinalEol = True } | not (dsFinalEol ds) ]
  ]
  where plainTodo e | maybe False (`notElem` ["TODO", "DONE"]) (esTodo e) = e { esTodo = Nothing }
                    | otherwise = e

shrinkEntry :: EntrySpec -> [EntrySpec]
shrinkEntry e = map normEntry . filter (/= e) $ concat
  [ [ e { esPlanning = ps } | ps <- shrinkList (const []) (esPlanning e) ]
  , [ e { esProperties = Nothing } | isJust (esProperties e) ]
  , [ e { esProperties = Just ps } | Just pairs <- [esProperties e]
                                   , ps <- shrinkList (const []) pairs ]
  , [ e { esLogbook = Nothing } | isJust (esLogbook e) ]
  , [ e { esTags = [] } | not (null (esTags e)) ]
  , [ e { esTodo = Nothing } | isJust (esTodo e) ]
  , [ e { esPriority = Nothing } | isJust (esPriority e) ]
  , [ e { esBody = bs } | bs <- shrinkList (const []) (esBody e) ]
  , [ e { esTitle = ts } | ts <- shrinkList (const []) (esTitle e) ]
  , [ e { esTitle = map (const "a") (esTitle e) } | any (/= "a") (esTitle e) ]
  , [ e { esLevel = l } | l <- [1 .. esLevel e - 1] ]
  , [ e { esIndent = 0 } | esIndent e > 0 ]
  , [ e { esHeadTrail = 0 } | esHeadTrail e > 0 ]
  , [ e { esTrail = 0 } | esTrail e > 0 ]
  , [ e { esGap = 0 } | esGap e > 0 ]
  ]

-- Timestamp VALUES
--
-- A different question from the text above: these are the values 'TextShow'
-- renders and the parser reads back.

-- | A timestamp inside the PARSER'S OWN IMAGE — one some org text could have
-- produced.  The value space is wider than the image in three places, each of
-- which the render is DOCUMENTED to normalise rather than preserve:
--
-- * an untimed moment holding anything but midnight (the render drops the time);
-- * 'tsCompactRange' over ends the compact spelling cannot hold ('TsAny');
-- * a 'Restart' repeater signed 'TRSMinus', which renders @-3d@ — org's WARNING
--   cookie, which the parser tries first.
newtype TsImage = TsImage Timestamp
  deriving (Eq, Show)

instance Arbitrary TsImage where
  arbitrary = TsImage <$> genTimestamp True

-- | A timestamp whose 'tsCompactRange' may be one the compact spelling cannot
-- hold, which is what reaches @compactly@'s other two guards.
newtype TsAny = TsAny Timestamp
  deriving (Eq, Show)

instance Arbitrary TsAny where
  arbitrary = TsAny <$> genTimestamp False

genTimestamp :: Bool -> Gen Timestamp
genTimestamp consistent = do
  status <- elements [TimestampActive, TimestampInactive]
  start <- genMoment
  end <- maybeGen 1 2 genMoment
  compact <- case end of
    Nothing -> pure False
    Just e | consistent -> if tsmHasTime start && tsmHasTime e
                              && Time.utctDay (tsmTime start) == Time.utctDay (tsmTime e)
                             then arbitrary
                             else pure False
           | otherwise -> arbitrary
  interval <- maybeGen 1 3 genRepeater
  warning <- maybeGen 1 4 genWarning
  pure Timestamp { tsStatus = status
                 , tsInterval = interval
                 , tsWarning = warning
                 , tsStart = start
                 , tsEnd = end
                 , tsCompactRange = compact }

genMoment :: Gen TsMoment
genMoment = do
  (y, mo, d) <- genDate
  hasTime <- arbitrary
  (h, mi) <- genClock
  s <- frequency [(3, pure 0), (1, choose (1, 59))]
  let day = Time.fromGregorian y mo d
      tod | hasTime   = Time.TimeOfDay h mi (fromIntegral (s :: Int))
          | otherwise = Time.TimeOfDay 0 0 0
  pure TsMoment { tsmTime = Time.UTCTime day (Time.timeOfDayToTime tod)
                , tsmHasTime = hasTime }

-- | A repeater the parser could have READ.  @Restart@ is spelled by the absence
-- of a type character, so a minus sign on one is org's warning cookie and never
-- comes back as a repeater at all.
genRepeater :: Gen TimestampRepeaterInterval
genRepeater = do
  (kind, sign) <- elements [ (Restart, TRSPlus)
                           , (CatchUp, TRSPlus), (CatchUp, TRSMinus)
                           , (Cumulative, TRSPlus), (Cumulative, TRSMinus) ]
  value <- choose (0, 12)
  unit <- elements [Days, Weeks, Months, Years]
  pure TimestampRepeaterInterval { repeaterType = kind
                                 , repeaterValue = value
                                 , repeaterUnit = unit
                                 , repeaterSign = sign }

genWarning :: Gen TimestampWarningInterval
genWarning = do
  firstOnly <- arbitrary
  value <- choose (0, 12)
  unit <- elements [Days, Weeks, Months, Years]
  pure TimestampWarningInterval { warningFirstOnly = firstOnly
                                , warningValue = value
                                , warningUnit = unit }

-- The generator's own oracle

-- | The shapes SPEC carries, so a census can say what the image holds.
shapesOf :: DocSpec -> [String]
shapesOf ds = concat
  [ [ "custom cycle"    | isJust (dsKeywords ds) ]
  , [ "crlf"            | case dsEol ds of { CRLF -> True; LF -> False } ]
  , [ "no final eol"    | not (dsFinalEol ds) ]
  , [ "several entries" | length (dsEntries ds) >= 3 ]
  , concatMap entryShapes (dsEntries ds)
  ]

entryShapes :: EntrySpec -> [String]
entryShapes e = concat
  [ [ "todo"                  | isJust (esTodo e) ]
  , [ "priority"              | isJust (esPriority e) ]
  , [ "tags"                  | not (null (esTags e)) ]
  , [ "planning"              | not (null keys) ]
  , [ "permuted planning"     | keys /= sort keys ]
  , [ "repeated planning key" | length (nub keys) < length keys ]
  , [ "range"                 | any (isJust . tspRange . snd) (esPlanning e) ]
  , [ "cookie"                | any (isJust . tspCookie . snd) (esPlanning e) ]
  , [ "drawer"                | isJust (esProperties e) ]
  , [ "empty drawer"          | esProperties e == Just [] ]
  , [ "indented drawer"       | esIndent e > 0 && isJust (esProperties e) ]
  , [ "logbook"               | isJust (esLogbook e) ]
  , [ "blank title"           | null (esTitle e) ]
  , [ "deep"                  | esLevel e >= 3 ]
  , [ "body"                  | not (null (esBody e)) ]
  , [ "gap"                   | esGap e > 0 ]
  , [ "trailing space"        | esTrail e > 0 ]
  , [ "title line trailing"   | esHeadTrail e > 0 ]
  ]
  where keys = map fst (esPlanning e)

-- | What a sample of 400 must hold at least this many of.  A property that
-- generates nothing interesting passes forever, which is the failure mode
-- @groundSweep@ and @paletteSweep@ answer in their own way; this is the answer
-- here, and it lands ahead of everything read through the generator.
shapeFloors :: [(String, Int)]
shapeFloors =
  [ ("custom cycle", 80), ("crlf", 80), ("no final eol", 40), ("several entries", 40)
  , ("todo", 80), ("priority", 60), ("tags", 80), ("planning", 150)
  , ("permuted planning", 40), ("repeated planning key", 30), ("range", 60)
  , ("cookie", 60), ("drawer", 80), ("empty drawer", 10), ("indented drawer", 20)
  , ("logbook", 30), ("blank title", 20), ("deep", 40), ("body", 150)
  , ("gap", 80), ("trailing space", 50)
  ]

-- | A DETERMINISTIC sample of N documents, so the census is a fact about the
-- generator rather than about the run that read it.
docSample :: Int -> [DocSpec]
docSample n = [ unGen (variant i (arbitrary :: Gen DocSpec)) (mkQCGen 1) (size i)
              | i <- [1 .. n] ]
  where size i = 4 + i `mod` 24
