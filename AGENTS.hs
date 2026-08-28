{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns -Wno-unused-top-binds -Wno-missing-signatures -Wno-missing-export-lists #-}

-- | glance's domain, modelled.  This file replaces CLAUDE.md and
-- docs/invariants.md: the vocabulary is types, the registries are data, and
-- `runghc AGENTS.hs' prints what the model still rests on.
--
-- THREE TIERS.  A rule is TYPED (the illegal state does not compile), CHECKED
-- (a property over the registries below) or NOTED (a `Why' beside the construct
-- it constrains, carrying what proves it).  Tier three is debt; `main' counts
-- it by `Proof'.
--
-- TIER TWO IS THE SUITE'S.  Every check this file once ran has moved: a rule an
-- existing case already covered was dropped, and one nothing asked became a
-- case in `test/TestSpec.hs', which reads THESE registries beside the real
-- code's, so a spec that drifts from the tree fails `cabal test' rather than a
-- separate script.  What could not be asked at all stayed here as a `Note'.
-- The `Check' machinery below is what the next spec edit writes into before it
-- earns a case.
--
-- The module is compiled by the test suite (`other-modules: AGENTS') and still
-- runs standalone: `runghc AGENTS.hs' prints the notes by `Proof'.
module AGENTS where

import Control.Monad (unless)
import Data.Char (isAlpha, isDigit, isSpace, toLower, toUpper)
import Data.List (foldl', intercalate, isPrefixOf, isSuffixOf, nub, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- * Ubiquitous language
--
-- Every section below is written in these.  A rule about a field nobody has
-- does not compile.

newtype Chars = Chars Int deriving (Eq, Ord, Show)
-- ^ Offsets are CHARACTERS into the `orgParse' input.  No `Bytes', no
-- `LineCol': the types the codebase must never use are the types this file
-- does not have.

data Span = Span Chars Chars deriving (Eq, Show)
-- ^ Half-open @[start, end)@.

instance Semigroup Span where Span a _ <> Span _ b = Span a b
-- ^ Left start, right end.  `hsFull' is a fold of this and nothing else.

newtype Digest = Digest String deriving (Eq, Show)     -- ^ the optimistic lock
newtype Id     = Id String     deriving (Eq, Ord, Show) -- ^ ORG_GLANCE_ID, opaque wherever READ
newtype Kw     = Kw String     deriving (Eq, Ord, Show) -- ^ a TODO keyword, verbatim
newtype Tag    = Tag String    deriving (Eq, Ord, Show)
newtype Query  = Query String  deriving (Eq, Show)
type Path = FilePath

data RowId = Named Id | Nth Path Int deriving (Eq, Show)
-- ^ `FILE#K', K the place among the file's EMITTED rows.  `Named' is the only
-- immunity to renumbering.

type Why = String
-- ^ What the types cannot say.  Every occurrence is debt.

data Proof = Typed | Test | Corpus | Interop | Browser | Elm | Comment | Docs | Unguarded
  deriving (Eq, Ord, Show, Enum, Bounded)
-- ^ What catches a violation.  `Unguarded' is silently relied on.

data Note = Note Why [Proof]
-- ^ A tier-three rule: prose, and what would notice it going wrong.

-- ** Words this file reserves
--
-- REGION is the scanner's structural run alone (`RegionKind'). A config write
-- moves PARTS (`ConfigParts'); the lens lifts a title, a body and properties.
-- NOTE is this file's rule-with-proof alone; a ledger writes a LINE
-- (`LedgerLine') at a SITE (`LineSite').
-- A KIND is a closed set this program mints and is a sum; a NAME is open text
-- an author wrote and is a String. `Stop.name' carries both, which is a shape
-- worth knowing and has cost nothing measured
-- (expired/2026-08-14-stop-kind-vs-org-name.md).

-- * Checks
--
-- A tier-two rule.  The `Bool' is computed from the registries, so a spec edit
-- that contradicts another section turns a line red.  `checks' is empty: every
-- one of them is a case in `test/TestSpec.hs' now, asked of the tree as well as
-- of this model.  A new rule starts here and leaves for the suite.

data Check = Check String Bool
check :: String -> Bool -> Check
check = Check

ordered :: Ord a => [a] -> Bool
ordered xs = and (zipWith (<=) xs (drop 1 xs))

unique :: Ord a => [a] -> Bool
unique xs = length (nub xs) == length xs
-- * Parse
--
-- Offsets into the document, the headline's sub-spans, what the parser refuses,
-- and what the re-serializer loses.

-- ** The headline's sub-spans

data Sub = Stars | Todo | Priority | Title | Tags | Planning | Properties
  deriving (Eq, Ord, Show, Enum, Bounded)

subOrder :: [Sub]
-- ^ SOURCE order.  `Stars' is the fold's seed rather than a part, so it is out.
subOrder = [Todo, Priority, Title, Tags, Planning, Properties]

data PlanKw = PSched | PDead | PClosed deriving (Eq, Ord, Show, Enum, Bounded)
-- ^ The planning line's whole vocabulary.  @CLOCK:@ is no constructor here.
planKwText :: PlanKw -> String
planKwText PSched  = "SCHEDULED:"
planKwText PDead   = "DEADLINE:"
planKwText PClosed = "CLOSED:"

data HeadlineSpans = HeadlineSpans
  { hsStars :: Span              -- ^ the stars alone; the one unconditional span
  , hsFixed :: [(Sub, Span)]     -- ^ todo/priority/title/tags/properties, positional
  , hsPlan  :: [(PlanKw, Span)]  -- ^ the three, permuting freely on their line
  } deriving (Eq, Show)
-- ^ There is no @hsFull@ field: the extent is `hsFull', a fold over `spanParts'.

spanParts :: HeadlineSpans -> [(Sub, Span)]
-- ^ The keyed sub-spans in source order, the planning triple sorted by offset.
spanParts hs = [ (s, sp) | s <- subOrder, sp <- at s ]
  where at Planning = map snd (sortOn (\(_, Span a _) -> a) (hsPlan hs))
        at s        = [ sp | (k, sp) <- hsFixed hs, k == s ]

hsFull :: HeadlineSpans -> Span
-- ^ Stars through the end of the LAST part present in `spanParts' order.
hsFull hs = foldl' (<>) (hsStars hs) (map snd (spanParts hs))

planningOf :: HeadlineSpans -> PlanKw -> Maybe Span
-- ^ A keyword repeated on the line keeps its LAST timestamp, as org reads one.
planningOf hs k = listToMaybe (reverse [ sp | (k', sp) <- hsPlan hs, k' == k ])

subtreeSpan :: Chars -> [(Int, Span)] -> [Span]
-- ^ EVERY headline's extent, from its stars to the next headline at its level or
-- shallower, else to the document's end.  Argument: (level, `hsFull') in order.
subtreeSpan eof = go
  where go []                       = []
        go ((l, Span a _) : rest)   = Span a (nxt l rest) : go rest
        nxt _ []                    = eof
        nxt l ((l', Span b _) : r)  | l' <= l   = b
                                    | otherwise = nxt l r

-- ** Elements

data Prag = PTodo [Kw] [Kw] | POther String String deriving (Eq, Show)
-- ^ `PTodo' carries both halves as LISTS in line order: a @#+TODO:@ line is a
-- cycle, and its spelling is the tree's whole say over state order and palette.

todoPragmaNames :: [String]
-- ^ All three land in one `PTodo'; a re-render says the head.
todoPragmaNames = ["TODO", "SEQ_TODO", "TYP_TODO"]

ptodoWord :: String -> Kw
-- ^ The fast-access selector in @TODO(t!)@ is dropped.
ptodoWord = Kw . takeWhile (/= '(')

data Element = EHeadline HeadlineSpans | EPragma Prag | ETimestamp Ts | EToken String
  deriving (Eq, Show)
-- ^ A closed sum; the element's own span rides beside the value, and is only
-- well-formed + reparse-checked where a sub-span slices its component exactly.

stripSpans :: Element -> Element
-- ^ NO catch-all: a fifth span-carrying constructor fails the build.
stripSpans (EHeadline hs)   = EHeadline hs { hsStars = Span (Chars 0) (Chars 0)
                                           , hsFixed = [], hsPlan = [] }
stripSpans e@(EPragma _)    = e
stripSpans e@(ETimestamp _) = e
stripSpans e@(EToken _)     = e

-- ** Timestamps

data Bracket = TsActive | TsInactive deriving (Eq, Show)
bracketChars :: Bracket -> (Char, Char)
bracketChars TsActive   = ('<', '>')
bracketChars TsInactive = ('[', ']')

data TsUnit = Dy | Wk | Mo | Yr deriving (Eq, Ord, Show, Enum, Bounded)
tsUnitChar :: TsUnit -> Char
tsUnitChar Dy = 'd'
tsUnitChar Wk = 'w'
tsUnitChar Mo = 'm'
tsUnitChar Yr = 'y'

data TsRepeat = CatchUp | Restart | Cumulative deriving (Eq, Ord, Show, Enum, Bounded)
tsTypeChars :: TsRepeat -> String
-- ^ THE NAMING TRAP, pinned to characters: two of three read as org's opposites.
tsTypeChars CatchUp    = "+"   -- ^ @++@, org's catch-up
tsTypeChars Restart    = ""    -- ^ the UNPREFIXED @+@, which org calls cumulate
tsTypeChars Cumulative = "."   -- ^ @.+@, which org calls restart

data TsSign = TRSPlus | TRSMinus deriving (Eq, Show, Enum, Bounded)
tsSignChar :: TsSign -> Char
tsSignChar TRSPlus  = '+'
tsSignChar TRSMinus = '-'      -- ^ unreached: a lone @-3d@ is the WARNING cookie

data TsRep = TsRep TsRepeat TsSign Int TsUnit deriving (Eq, Show)
tsRepText :: TsRep -> String
tsRepText (TsRep t s n u) = tsTypeChars t ++ [tsSignChar s] ++ show n ++ [tsUnitChar u]

data TsWarning = TsWarning Bool Int TsUnit deriving (Eq, Show)
-- ^ org's warning\/delay cookie; the `Bool' is the first-only @--@ spelling.
tsWarnText :: TsWarning -> String
tsWarnText (TsWarning first n u) =
  (if first then "--" else "-") ++ show n ++ [tsUnitChar u]

data TsMoment = TsMoment { tsmDay :: Int, tsmSecs :: Int, tsmHasTime :: Bool }
  deriving (Eq, Show)
-- ^ No WEEKDAY field (recomputed from the date on render, so a locale's word
-- costs nothing) and no REPEATER field (an end half's cookie is discarded, the
-- start's winning).  `tsmHasTime' alone decides whether a time renders: a
-- date-only stamp holds midnight.

data Ts = Ts { tsBracket :: Bracket         -- ^ ONE kind; both halves of a range share it
             , tsRep     :: Maybe TsRep     -- ^ at most one repeater
             , tsWarn    :: Maybe TsWarning -- ^ at most one warning
             , tsStart   :: TsMoment
             , tsEnd     :: Maybe TsMoment
             , tsCompact :: Bool            -- ^ which range spelling the source wrote
             } deriving (Eq, Show)

tsCookies :: [Either TsRep TsWarning] -> (Maybe TsRep, Maybe TsWarning)
-- ^ Either order, FIRST of each kind winning.
tsCookies cs = (listToMaybe [ r | Left r <- cs ], listToMaybe [ w | Right w <- cs ])

tsCookieText :: Ts -> String
-- ^ Repeater THEN warning, whatever order the source spelled them.
tsCookieText ts = maybe "" ((' ' :) . tsRepText) (tsRep ts)
               ++ maybe "" ((' ' :) . tsWarnText) (tsWarn ts)

compactly :: Ts -> Bool
-- ^ The compact render's three conditions: the flag, both ends timed, one day.
compactly (Ts _ _ _ _ Nothing _)      = False
compactly (Ts _ _ _ s (Just e) c) = c && tsmHasTime s && tsmHasTime e
                                      && tsmDay s == tsmDay e

data Dash = RangeEnd | WarnCookie deriving (Eq, Show)
dashOf :: String -> Dash
-- ^ What a @-@ opens.  The end time is TRIED FIRST and the colon is the whole
-- difference: @-1d@ gets through the decimal, fails at the missing @:@, and
-- backtracks whole, leaving the cookie its text.
dashOf s = case dropWhile isDigit s of { (':' : _) -> RangeEnd; _ -> WarnCookie }

weekdayOf :: String -> Maybe String
-- ^ A run of LETTERS in any script, any length, read and DROPPED.  One letter is
-- what keeps @.+3d@ out of the slot, a repeater opening with @.@, @+@, @-@ or a
-- digit.
weekdayOf s = case takeWhile isAlpha s of { "" -> Nothing; w -> Just w }

-- ** What the parser accepts

data Boundary = AtHspace | AtEol | AtEof | MidWord deriving (Eq, Ord, Show, Enum, Bounded)
accepts :: Boundary -> Bool
-- ^ A top-level element ends at whitespace or EOF; stopping MID-WORD fails the
-- WHOLE document.
accepts AtHspace = True
accepts AtEol    = True
accepts AtEof    = True
accepts MidWord  = False

consumed :: Boundary -> Bool
consumed AtHspace = True
consumed AtEol    = False   -- ^ looked at; the stars never take the NEWLINE
consumed AtEof    = False
consumed MidWord  = False

headlineAt :: Bool -> Boundary -> Bool
-- ^ Both halves are required: column 1 (a threaded begin-of-line `Bool'), and a
-- star run that ENDS.  A bare star run is the empty headline it always was.
headlineAt col1 b = col1 && accepts b

data Lexeme = TodoWord | PragmaKey | PropertyKey deriving (Eq, Ord, Show, Enum, Bounded)
data Cased = Verbatim | Uppercased deriving (Eq, Show)
casing :: Lexeme -> Cased
-- ^ Swapping the two breaks either keyword matching or drawer termination.
casing TodoWord      = Verbatim
casing PragmaKey   = Uppercased
casing PropertyKey = Uppercased

reservedProperties :: [String]
-- ^ Rejecting these as property keys is what terminates the drawer.
reservedProperties = ["PROPERTIES", "END"]

-- ** Context

data Ctx = Ctx { ctxActive :: [Kw]   -- ^ recognition alone; ORDER is `Config''s question
               , ctxInactive :: [Kw]
               , ctxCat :: String
               , ctxIas :: [(Id, HeadlineSpans)]
               } deriving (Eq, Show)
-- ^ No `Semigroup'\/`Monoid': `mempty' re-seeded TODO\/DONE and @<>@ concatenated
-- categories, so there is no Ctx merge to be unlawful.

ctxDefault :: Ctx
ctxDefault = Ctx [Kw "TODO"] [Kw "DONE"] "" []

ctxSetTodo :: [Kw] -> [Kw] -> Ctx -> Ctx
-- ^ APPEND-ONLY: a @#+TODO:@ line grows the sets and never replaces them.
ctxSetTodo a i c = c { ctxActive   = nub (ctxActive c ++ a)
                     , ctxInactive = nub (ctxInactive c ++ i) }

ctxKnows :: Kw -> Ctx -> Bool
ctxKnows k c = k `elem` (ctxActive c ++ ctxInactive c)

iasRegister :: Maybe Id -> HeadlineSpans -> Ctx -> Ctx
-- ^ Opt-in on `ORG_GLANCE_ID', LAST WRITER WINS, so a re-parse is idempotent.
iasRegister Nothing  _  c = c
iasRegister (Just i) hs c = c { ctxIas = (i, hs) : [ e | e@(j, _) <- ctxIas c, j /= i ] }

data Parsed = Failed Ctx | Parsed [Element] Ctx deriving (Eq, Show)
-- ^ `Failed' has no element list: an error yields ZERO elements.
orgParse :: Ctx -> Maybe ([Element], Ctx) -> Parsed
orgParse c Nothing          = Failed c        -- the caller's context, untouched
orgParse _ (Just (es, c'))  = Parsed es c'

-- ** Render

data Roundtrip = Stable | Exact deriving (Eq, Ord, Show)
data Loss = WhitespaceCollapsed | PragmaKeyUppercased | PlanningDropped
          | WeekdayRespelled | CookieOrderNormalized
  deriving (Eq, Ord, Show, Enum, Bounded)

textShowLosses :: [Loss]
-- ^ `TextShow' is a REPL re-serializer.  Spans are the lossless channel.
textShowLosses = [minBound ..]

roundtripCases :: [(String, Roundtrip)]
-- ^ The lossiness BUDGET: a `Stable' row asserts fidelity the renderer lacks, so
-- a promotion has to be measured first.  The budget is empty.
roundtripCases =
  [ ("hello",                                          Exact)
  , ("hello world",                                    Exact)
  , ("* Hello",                                        Exact)
  , ("* TODO Hello",                                   Exact)
  , ("** TODO [#A] Hello",                             Exact)
  , ("* Hello :tag1:tag2:",                            Exact)
  , ("** TODO [#B] My task :work:urgent:",             Exact)
  , ("**** Deep headline",                             Exact)
  , ("#+CATEGORY: mycat",                              Exact)
  , ("#+TODO: TODO STARTED | DONE CANCELLED",          Exact)
  , ("#+TITLE: My Document",                           Exact)
  , ("<2024-01-15 Mon 10:30>",                         Exact)
  , ("[2024-06-01 Sat 09:00]",                         Exact)
  , ("<2024-01-01 Mon 00:00>",                         Exact)
  , ("[2024-01-01 Mon 00:00]",                         Exact)
  , ("<2026-07-08 Wed>",                               Exact)
  , ("[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]", Exact)
  , ("<2024-01-15 Mon>--<2024-01-19 Fri>",             Exact)
  , ("<2024-01-15 Mon 10:30-11:30>",                   Exact)
  , ("[2021-11-09 Tue 17:30-18:30]",                   Exact)
  , ("<2024-01-15 Mon 10:30-11:30 +1w>",               Exact)
  , ("<2024-01-15 Mon 10:30:15-11:45:30>",             Exact)
  , ("<2024-01-15 Mon +1m -3d>",                       Exact)
  , ("[2024-01-15 Mon .+2d --7d]",                     Exact)
  , ("<2024-01-15 Mon -3d>",                           Exact)
  , ("* Due <2026-07-08 Wed>",                         Exact)
  ]

-- ** Measurements the prose carried

residual :: (Int, Int)      -- ^ files lost whole to a mid-word stop, of files walked (2026-07-31)
residual = (11, 6290)
starRunRows :: (Int, Int)   -- ^ body lines read as headlines before the star run had to end, of them at level one
starRunRows = (251, 29)
headlineCount :: (Int, Int) -- ^ headlines before, after
headlineCount = (12884, 12606)
weekdayLoss :: (Int, Int)   -- ^ blobs losing drawer and id to a Dutch weekday, of blobs walked
weekdayLoss = (28, 6063)
unicodeCanary :: (Int, Int) -- ^ TestSubtree's fixture: characters, bytes, one document
unicodeCanary = (61, 105)

parseNotes :: [Note]
parseNotes =
  [ Note "A sub-parser stopping mid-word fails the WHOLE document; a `withRecovery' changes orgParse's all-or-nothing contract and every caller." [Test, Docs]
  , Note "The residual-failure taxonomy (`::' in titles, `:)', a timestamp glued to punctuation, a hyphen in a commented #+TODO:) predates the derived-mirror exclusion and needs re-measuring before it is quoted." [Unguarded]
  , Note "Headlines parse at column 1 off a threaded Bool; getSourcePos re-scans from the last checkpoint per call, quadratic on failure-heavy input — 13.6 s, 464 files/s, 19 MB, with zero test failures." [Docs]
  , Note "Offsets are CHARACTERS: a byte consumer splices mid-codepoint on the first unicode title. TestSpans' `Привет мир' headline is a shape fixture and asserts no counts." [Test, Corpus]
  , Note "A part appended to spanParts out of source order silently shortens every extent past it; capture/refile insertion points derive from hsFull, and an append-note writing at its end would then write inside the drawer." [Test]
  , Note "Element spans may carry consumed trailing whitespace and are only bounds- and reparse-checked, where a sub-span slices its component exactly and a planning slice reparses to the same ETimestamp." [Test, Corpus]
  , Note "-Werror=incomplete-patterns is on in all seven stanzas, so no policy over a closed sum carries a catch-all: valueFor spells its five and keyTest's Col/Planned arms share one named cellsTest." [Typed]
  , Note "~150 span-insensitive assertions read a parse through `bare = map (stripSpans . valueOf)', so they go span-sensitive the moment stripSpans stops being total." [Test]
  , Note "The planning line is the one line after the title and ahead of any drawer; the whole line backtracks when it is not one, so a SCHEDULED: further down stays body. The `try' around each entry must roll back the leading hspace it skipped — the top loop separates elements by whitespace, a timestamp excepted below — and a required hspace1 holds keyword and timestamp apart, which is what makes a failed entry recoverable." [Test]
  , Note "A timestamp closes on its OWN bracket, so prose abuts it with nothing between: `[2026-08-03 Mon].' is a timestamp and the token `.', and the file parses. ONLY a timestamp continues the element loop abutted — a token already ran to the next space, and a headline stopping mid-line stays corrupt org — and NEVER at BOL, so no `*' behind a timestamp opens a headline. Before the exception, one full stop behind a bracket failed the whole file and the one-file law hid the loss (docs/bugs/fixed/2026-08-24-a-full-stop-behind-a-timestamp-fails-the-whole-file.md)." [Test]
  , Note "In spannedContainerUntil the end-parser branch precedes the hspace-eol branch: tags open with hspace1 and the eol branch would eat that space without backtracking." [Comment, Test]
  , Note "Trailing hspace terminates a container and stays unconsumed, so elements do not tile the input; the #+TODO: pragma's element span is the one that swallows it, reparse-safe." [Test]
  , Note "Without the reserved-key guard `:END:' parses as a property, manyTill runs to EOF and hsProperties swallows the rest of the file." [Unguarded]
  , Note "No space may sit around a timestamp's `-', or ` -1d' reads as an end time." [Test]
  , Note "A trailing dot is refused in the weekday slot, so French `lun.' still fails; admitting it needs a guard `.+3d' would otherwise trip." [Corpus]
  , Note "The renderer never canonicalizes a range: emacs writes CLOCK ranges as `--' though both halves share a date." [Test]
  , Note "compactly's two date conditions are asserted by the comment alone — only the flag is exercised." [Unguarded]
  , Note "Ord Ts compares start moments where Eq reads every field, so a Set or Map key deduplicates distinct timestamps sharing a start." [Unguarded]
  , Note "spanRange forces at every step (foldl' + $!): a lazy accumulator is a thunk chain holding Spans that reference the document." [Comment]
  , Note "TextShow Headline emits the title line alone, so a planning line survives the span channel only." [Unguarded]
  , Note "A #+TODO: line affects the headlines below it — one left-to-right pass, no retroactive application." [Unguarded]
  , Note "The file-watch parses each file from ctxDefault, never a shared long-lived context." [Test]
  , Note "resolveHeadline keeps h1 where both are scheduled and h1 is strictly later and yields h2 otherwise; registerHeadline reaches the IAS by a plain insert, so the suite is its one caller and it describes no production behaviour." [Test, Unguarded]
  , Note "Trailing blank lines belong to the subtree above, and the extents are computed in one right-to-left pass with a stack." [Test, Corpus]
  , Note "A dated observation against one private tree, reproduced by nothing here: 4661 planning lines carrying 7220 entries, 7161 attached, ~70 stragglers, and 2642 planning lines inside files that fail to parse outright." [Unguarded]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "Every roundtrip input is pinned once." [Unguarded]
  , Note "The residual failure class is a minority of the corpus." [Unguarded]
  , Note "The star-run rule dropped rows and headlines together." [Unguarded]
  , Note "A property's key and its value are held apart by HORIZONTAL space alone. `MPC.space' there eats the newline after an EMPTY value, so `:KEY:' with nothing on it takes the `:END:' under it as its value and the WHOLE drawer stops parsing — and an empty value is a pair this repo writes itself, whenever one is cleared and wherever a capture template's ask opens." [Test]
  , Note "A Dutch weekday cost whole drawers." [Unguarded]
  ]
-- * Scan and the ledgers

-- ** The pool
--
-- One implementation, two callers.  The walk ahead of it is serial and most of
-- the wall; every fold behind it is serial too.

poolCallers :: [String]
poolCallers = ["Glance.Query.loadDirFilesWith", "app/Scan.hs"]

data Held = Sliced | Copied
newtype Txt (h :: Held) = Txt String
-- ^ A slice shares the document's array, so a cell a row KEEPS is a copy.
detach :: Txt 'Sliced -> Txt 'Copied
detach (Txt t) = Txt t
data Kept = Kept [Txt 'Copied]
-- ^ the cells, and NOTHING ELSE.  A RECORD RETAINS NO DOCUMENT BYTES: `hrDoc'
-- is gone and the parsed headline with it, so the text a reader wants is read
-- by path and pinned against `hrDigest'.

newtype Forced a = Forced a
-- ^ A worker forces before returning: in-flight memory is pool width × one document.
data Taken = Taken Int (Forced Kept) | Untaken Int Path
takeIx :: Taken -> Int
takeIx (Taken i _) = i
takeIx (Untaken i _) = i
reassemble :: [Taken] -> [Taken]
reassemble = sortOn takeIx
-- ^ A worker tags what it took with its INPUT INDEX, so a parallel load is the
-- serial one record for record whatever order the reads finished in.

data OrderReader = ResolveIds | StoreElems | CappedFailures deriving (Eq, Show, Enum, Bounded)
-- ^ the three readers of that sequence, each answering differently off completion order.
cappedFailures :: Int
cappedFailures = 20
serialAt :: Int -> Bool
serialAt paths = paths <= 1
-- ^ A path list of one skips the pool: the watch's `reload' calls `loadFile'.

residency :: [(Int, Double)]
-- ^ MB max residency over ~/sync (6290 files, @+RTS -s@, 2026-07-31) by pool width.
residency = [(1, 21.9), (2, 23.4), (4, 28.9), (8, 37.8)]
perWorker :: Double
perWorker = 2.3
firstWalkMB :: Double
firstWalkMB = 1400
rtsOpts :: [String]
rtsOpts = ["-threaded", "-rtsopts", "-with-rtsopts=-N"]

-- ** The index fold
--
-- `Data.Org.Index', read-only, faithful to `org-glance-graph--latest-records'.

data StoreSource = OwnRoot | FromDeclined deriving (Eq, Show, Enum, Bounded)
storeFound :: Bool -> StoreSource -> Bool
storeFound _ OwnRoot = True
storeFound includeDerived FromDeclined = not includeDerived
-- ^ A store the walk DECLINED is found for free, and missed under @--include-derived@.

data Seg = Sealed Int | OpenSeg deriving (Eq, Show)
segRank :: Seg -> Int
segRank (Sealed n) = n
segRank OpenSeg = maxBound
foldOrder :: [Seg] -> [Seg]
foldOrder = sortOn segRank
-- ^ MANIFEST's sealed segments oldest-first, the open `headlines.jsonl' LAST.
opensAs :: String -> Maybe Seg
opensAs n
  | n == "headlines.jsonl" = Just OpenSeg
  | "seg-" `isPrefixOf` n, ".jsonl" `isSuffixOf` n
  , ds <- take (length n - 10) (drop 4 n), not (null ds), all isDigit ds = Just (Sealed (read ds))
  | otherwise = Nothing

data ArchKey = Unkeyed | HasFlag Bool deriving (Eq, Show)
-- ^ `archived' joined the schema late, so ABSENT is a third answer.
data WalRec = LiveRec Id Kw ArchKey | TombRec Id deriving (Eq, Show)
walId :: WalRec -> Id
walId (LiveRec i _ _) = i
walId (TombRec i) = i
latestPerId :: [WalRec] -> [WalRec]
latestPerId rs = [ r | i <- nub (map walId rs), r <- take 1 (reverse (byId i)), alive r ]
  where byId i = [ x | x <- rs, walId x == i ]
        alive (TombRec _) = False
        alive LiveRec{}   = True

data BlobEntry = BlobEntry Id Kw Bool deriving (Eq, Show)
-- ^ a blob's FIRST headline: its id, its keyword, whether it wears the archive tag.
data Disagree = StateDiff | ArchivedDiff deriving (Eq, Show, Enum, Bounded)
disagree :: WalRec -> BlobEntry -> [Disagree]
disagree (TombRec _) _ = []
disagree (LiveRec _ k a) (BlobEntry _ k' arch) =
  [ StateDiff | k /= k' ] ++ [ ArchivedDiff | HasFlag f <- [a], f /= arch ]

data JsonV = JBool Bool | JStr String | JEmpty deriving (Eq, Show)
-- ^ elisp writes @nil@ as @{}@.
truthy :: JsonV -> Bool
truthy (JBool b) = b
truthy (JStr s)  = not (null s)
truthy JEmpty    = False
flagOf :: JsonV -> Bool
flagOf (JBool b) = b
flagOf (JStr _)  = False
flagOf JEmpty    = False

data Census = Census
  { cnRun :: String
  , cnRecords, cnLive, cnMalformed, cnBlobs, cnFiles :: Int
  , cnState, cnArchived, cnUnindexed, cnOrphans :: Int
  , cnTombs, cnIdless :: Maybe Int }
censuses :: [Census]
censuses =
  [ Census "2026-08-02" 6502 6071 0 6063 6071 20 1 0 59 (Just 0) Nothing
  , Census "2026-08-03" 6503 6071 0 6063 6071 38 1 0 29 Nothing  (Just 21) ]
disagreements :: Census -> Int
disagreements c = cnState c + cnArchived c
indexReport :: Census -> String
indexReport c = "org-glance index: " ++ show (disagreements c) ++ " rows disagree ("
             ++ show (cnState c) ++ " state, " ++ show (cnArchived c) ++ " archived)"
topDisagreeing :: Int
topDisagreeing = 10
weekdayFix :: [(String, Int, Int)]
-- ^ the weekday-slot fix over ~/sync\/views, 2026-08-02: before, after.
weekdayFix = [ ("idless blobs", 49, 21), ("records without blobs", 57, 29) ]
lostWeekdays :: Int
lostWeekdays = 28
corpusScan :: (Int, Int, Int)
-- ^ @glance doctor ~/sync@: span violations, headlines, seconds of walk.
corpusScan = (0, 12600, 10)

-- ** Repeat
--
-- A repeat is a `set-state', and org's condition is both halves.

data Cookie = Plus | PlusPlus | DotPlus deriving (Eq, Show, Enum, Bounded)
cookies :: [Cookie]
cookies = [minBound .. maxBound]
cookieChar :: Cookie -> Maybe Char
cookieChar Plus     = Nothing
cookieChar PlusPlus = Just '+'
cookieChar DotPlus  = Just '.'
cookieSpelling :: Cookie -> String
cookieSpelling c = maybe "" (: []) (cookieChar c) ++ "+N"
ctorName :: Cookie -> String
-- ^ KNOWN NAMING TRAP: `TimestampRepeaterType' reads as org's opposite for two of three.
ctorName Plus     = "Restart"
ctorName PlusPlus = "CatchUp"
ctorName DotPlus  = "Cumulative"
orgWord :: Cookie -> String
orgWord Plus     = "cumulate"
orgWord PlusPlus = "catch-up"
orgWord DotPlus  = "restart"

newtype Dayno = Dayno Int deriving (Eq, Ord, Show)
data RepUnit = RHour | RDay | RWeek | RMonth | RYear deriving (Eq, Show, Enum, Bounded)
data Every = Every Int RepUnit deriving (Eq, Show)
unitDays :: RepUnit -> Int
unitDays RHour  = 0
unitDays RDay   = 1
unitDays RWeek  = 7
unitDays RMonth = 30
unitDays RYear  = 365
everyDays :: Every -> Int
everyDays (Every n u) = n * unitDays u
addUnit :: Every -> Dayno -> Dayno
addUnit e (Dayno d) = Dayno (d + everyDays e)
repeatDay :: Cookie -> Every -> Dayno -> Dayno -> Dayno
-- ^ COOKIE, its interval, TODAY, the stamp: where the stamp lands.  A
-- zero-width interval takes the @+N@ arm, the @++@ loop over one never ending.
repeatDay _        e _     stamp | everyDays e <= 0 = addUnit e stamp
repeatDay Plus     e _     stamp = addUnit e stamp
repeatDay DotPlus  e today _     = addUnit e today
repeatDay PlusPlus e today stamp = until (> today) (addUnit e) stamp

planStamps :: [(String, Bool)]
-- ^ which planning stamps a repeat moves; org repeats a plan, and `CLOSED:' is
-- the record of one.
planStamps = [("SCHEDULED", True), ("DEADLINE", True), ("CLOSED", False)]

data StampPart = SpDate | SpWeekday | SpTime | SpWarning | SpRepeater | SpRangeEnd
  deriving (Eq, Show, Enum, Bounded)
data Touch = Rewritten | Respelled | Authors deriving (Eq, Show)
stampTouch :: StampPart -> Touch
-- ^ THE SHIFT IS TEXTUAL: `rewriteDates' moves the @YYYY-MM-DD@ runs and
-- respells each weekday behind its own date.
stampTouch SpDate     = Rewritten
stampTouch SpRangeEnd = Rewritten
stampTouch SpWeekday  = Respelled
stampTouch SpTime     = Authors
stampTouch SpWarning  = Authors
stampTouch SpRepeater = Authors

data Repeat = Repeat (Maybe Kw) Dayno [Span] deriving (Eq, Show)
-- ^ the keyword it lands on (none takes the keyword OFF), its next occurrence,
-- and the shift and the reset as ONE set of disjoint spans: one write, one
-- digest, one event.
repeatOn :: Asked -> Bool -> Span -> [(Span, Cookie, Every, Dayno)] -> [Kw] -> Maybe Repeat
repeatOn (Asked today _) inactive kwAt stamps actives
  | inactive, (_, c, e, d) : _ <- stamps =
      Just (Repeat (listToMaybe actives) (repeatDay c e today d) (kwAt : map stampAt stamps))
  | otherwise = Nothing
  where stampAt (s, _, _, _) = s
edStart :: Span -> Chars
edStart (Span a _) = a

-- ** What a request resolved, and what a row answers

data Asked = Asked Dayno (Maybe String)
-- ^ read from ONE clock before any row is touched: the day, and
-- `set-planning''s rendered stamp.  The next request-level value joins here and
-- ten row signatures stay put (`ConfigParts''s reason).

data Completion = Completion Id Kw String deriving (Eq, Show)
-- ^ one repeat as the ledger records it: the entry, the state it landed on, its
-- next occurrence cookie and all.  The @at@ is the append's own clock read.
data RowWrite = RowWrite [Span] (Maybe Completion)
-- ^ ONE answer: the spans a command moves, and the ledger line riding their success.
plain :: [Span] -> RowWrite
plain es = RowWrite es Nothing
plainCommands, recordingCommands :: Int
plainCommands = 9
recordingCommands = 1
setStateWrite :: Id -> [Span] -> Maybe Repeat -> RowWrite
-- ^ ONE `repeatOn': the spans and the line it records come off one answer.
setStateWrite _ es Nothing = plain es
setStateWrite i _ (Just (Repeat st (Dayno d) es)) =
  RowWrite es (Just (Completion i (fromMaybe (Kw "") st) (show d)))

-- ** The two ledgers

data Ledger = ExternalL | CompletionsL deriving (Eq, Show, Enum, Bounded)
ledgerFile :: Ledger -> Path
ledgerFile ExternalL    = "meta/EXTERNAL.jsonl"
ledgerFile CompletionsL = "meta/COMPLETIONS.jsonl"
data Keyed = ByPath | ByServedRoot deriving (Eq, Show)
keyedBy :: Ledger -> Keyed
keyedBy ExternalL    = ByPath
keyedBy CompletionsL = ByServedRoot
data LineSite = AtReplaceSpans | AtWriteOne deriving (Eq, Show)
lineSite :: Ledger -> LineSite
lineSite ExternalL    = AtReplaceSpans
lineSite CompletionsL = AtWriteOne

data LedgerLine = Written | Tombstoned deriving (Eq, Show, Enum, Bounded)
jsonStr :: String -> String
jsonStr s = "\"" ++ s ++ "\""
stampWidth :: Int
stampWidth = length "2026-08-13T09:41:07Z"
noteLine :: LedgerLine -> Id -> String -> String
-- ^ Keys hand-assembled so the field order is frozen; only VALUES go through
-- the encoder, and @true@ is a LITERAL spliced in.  Absence IS the plain line.
noteLine k (Id i) at = "{" ++ jsonStr "id" ++ ":" ++ jsonStr i
                    ++ "," ++ jsonStr "at" ++ ":" ++ jsonStr at ++ extra k ++ "}"
  where extra Written    = ""
        extra Tombstoned = "," ++ jsonStr "tombstone" ++ ":true"
completionLine :: Completion -> String -> String
completionLine (Completion (Id i) (Kw st) sh) at =
  "{" ++ jsonStr "id" ++ ":" ++ jsonStr i ++ "," ++ jsonStr "at" ++ ":" ++ jsonStr at
      ++ "," ++ jsonStr "state" ++ ":" ++ jsonStr st
      ++ "," ++ jsonStr "shifted" ++ ":" ++ jsonStr sh ++ "}"

data Blobbed = Blobbed Path Id
-- ^ TWO GATES, ONE PLACE: a blob under a store, and an id in its first headline.
noteBlob :: Bool -> Path -> Maybe Id -> Maybe Blobbed
noteBlob underStore p (Just i) | underStore, "data.org" `isSuffixOf` p = Just (Blobbed p i)
noteBlob _ _ _ = Nothing
data ByteDoor = DoorSplice | DoorTrash deriving (Eq, Show, Enum, Bounded)
-- ^ bytes move two ways: `replaceSpans' splices, `Data.Org.Trash.trashBlob' moves.
doorNote :: ByteDoor -> LedgerLine
doorNote DoorSplice = Written
doorNote DoorTrash  = Tombstoned
noteFor :: ByteDoor -> Blobbed -> String -> String
noteFor d (Blobbed _ i) = noteLine (doorNote d) i

-- ** The cursor, and rotation

newtype Sha = Sha String deriving (Eq, Show)
data Cursor = Cursor Chars Sha Sha
-- ^ @meta\/EXTERNAL.cursor@: one byte offset PLUS two sha1s of the bytes it
-- names, written temp-then-rename.
data Bound = BExact | BBounded
newtype Fold (b :: Bound) = Fold Chars
foldFrom :: Cursor -> (Sha, Sha) -> Chars -> Either (Fold 'BBounded) (Fold 'BExact)
-- ^ The digests hold, or bytes a union merge inserted AHEAD of the offset
-- re-fold and what the offset named is no longer exact.
foldFrom (Cursor _ a b) (a', b') end
  | (a, b) == (a', b') = Right (Fold end)
  | otherwise          = Left (Fold end)
newtype RotGen = RotGen Int deriving (Eq, Show)
rotatedName :: RotGen -> Path
rotatedName (RotGen n) = "meta/EXTERNAL-" ++ show n ++ ".jsonl"
retire :: Chars -> RotGen -> Fold 'BExact -> Maybe Path
-- ^ ROTATION: an EXACT fold past the cap renames the live file, and the
-- daemon's next append creates a fresh one — `appendLine' opens the path per line.
retire (Chars cap) g (Fold (Chars n)) | n > cap   = Just (rotatedName g)
                                      | otherwise = Nothing
drainOrder :: [RotGen] -> [Path]
-- ^ a rotated file drains AHEAD of the live one and is never deleted on the
-- pass that made it.
drainOrder gs = map rotatedName gs ++ [ledgerFile ExternalL]

data Seen = SawWrite | SawDelete deriving (Eq, Show)
foldWindow :: [(Id, Seen)] -> [(Id, Seen)]
-- ^ one @(ID . KIND)@ per id, each at its FIRST sighting carrying its LAST
-- sighting's kind, so a write and a delete of one id inside one window fold as
-- the delete.
foldWindow seen = [ (i, k) | i <- nub (map fst seen), k <- take 1 (reverse (kindsOf i)) ]
  where kindsOf i = [ x | (j, x) <- seen, j == i ]

-- ** The interop harness
--
-- `make interop' is the only check that runs the peer.

interopCases :: [(String, String)]
interopCases =
  [ ("sidecars-are-not-rows",                        "CLAIM 20")
  , ("blob-path-agrees",                             "CLAIM 4")
  , ("external-bytes",                               "CLAIM 2 + CLAIM 3")
  , ("meta-untouched",                               "CLAIM 21")
  , ("emacs-adopts",                                 "CLAIM 5")
  , ("cursor-advances-and-the-bytes-stay",           "CLAIM 8")
  , ("tag-cycle-survives",                           "CLAIM 7")
  , ("bytes-move-under-a-live-cursor",               "CLAIM 22")
  , ("browser-sees-emacs",                           "CLAIM 19")
  , ("archive-flag-round-trips",                     "CLAIM 14")
  , ("scan-agrees-with-the-writer",                  "CLAIM 15")
  , ("HOLE: a tagged capture never reaches the WAL", "CLAIM 17")
  , ("delete-tombstones-the-record",                 "CLAIM 18") ]
breaks :: [(String, String)]
-- ^ @BREAK=name@ takes ONE harness step out and names the case that must go red.
breaks =
  [ ("no-write",       "external-bytes")
  , ("no-refresh",     "emacs-adopts")
  , ("no-put",         "browser-sees-emacs")
  , ("wrong-id",       "blob-path-agrees")
  , ("meta-moved",     "meta-untouched")
  , ("no-delete-fold", "delete-tombstones-the-record")
  , ("no-owed-write",  "bytes-move-under-a-live-cursor") ]
emacsRun :: [String]
emacsRun = ["host", "podman"]
interopSkips :: [String]
interopSkips = ["node", "Emacs", "the org-glance checkout", "its deps"]
digestCase :: String
digestCase = "bytes-move-under-a-live-cursor"
dragged :: [String]
-- ^ what a digest-free reader takes down after `digestCase'.
dragged = ["browser-sees-emacs", "archive-flag-round-trips", "scan-agrees-with-the-writer"]
drainedAsks :: [String]
-- ^ the fold and the READ PATH ask different things of one cursor, so `drained'
-- asks both: a peer that polls by SIZE passes the first and fails the second.
drainedAsks = ["the cursor against the file", "what the peer's read path says is owed"]
holeCase :: String
holeCase = "HOLE: a tagged capture never reaches the WAL"
unmatchedLine :: (Int, Int)
-- ^ glance's own instrument on that hole: unindexed blobs, records without blobs.
unmatchedLine = (1, 0)
watchCallSites :: (Int, Int)
-- ^ `watchOrgTree' in @src@, and in @test@.
watchCallSites = (1, 0)
suiteSizes :: (Int, Int)
-- ^ the Haskell suite when the severing was run, and today: green either way.
suiteSizes = (1857, 1867)

scanNotes :: [Note]
scanNotes =
  [ Note "`forceResult' runs inside `evaluate' + `try', so one pathological file cannot abort \
         \the run and no thunk retains a document." [Comment, Docs]
  , Note "Only `glance doctor ~/sync' exposes a strictness regression; `cabal test' cannot see one." [Docs]
  , Note "The pool is sound because there is no shared parse state: every file parses from \
         \`defaultContext'." [Test]
  , Note "`loadDirFilesSerially' is exported for the assertion — TestQuery compares the two \
         \record for record over a forty-document fixture carrying one failure of each kind, \
         \five runs deep to pin the id-resolution winner." [Test]
  , Note "Under a non-threaded runtime `getNumCapabilities' is 1 whatever -N says and every \
         \assertion still passes, so TestQuery asserts `rtsSupportsBoundThreads'." [Test]
  , Note "A RECORD RETAINS NO DOCUMENT BYTES: `hrDoc' is gone and the parsed headline with \
         \it — what a row kept off that parse is cut into fields (`hrSpans', `hrLevel', \
         \`hrOrgId', `hrIdProperty', `hrRepeats') — so a loaded store is its cells and no \
         \file's text. Residency is rows times cells rather than the tree's bytes." [Typed]
  , Note "A REQUEST-TIME READER TAKES THE DOCUMENT AS AN ARGUMENT — `subtreeText', \
         \`headlineParts', `subtreeEntries', `subtreeLinks' and every edit's span math — and \
         \the route reads it ONCE per request, PINNED against `hrDigest' by `pinnedDocument', \
         \which is the read `replaceSpans' itself opens with. A READ RELOADS ON DRIFT — the \
         \store lagging its own tree is this server's signal, so `Watch.reload' runs and the \
         \id is addressed again under the fresh digest — where a WRITE takes the 409, a write \
         \never re-targeting bytes the client did not see." [Test]
  , Note "NO QUERY TOUCHES THE DISK: a custom column is asked per row per request, so \
         \`closed' and the drawer pairs are cut at parse into `hrClosed' and `hrDrawer' and \
         \`/properties' walks those." [Test]
  , Note "`Cursor' assumes non-decreasing span starts; an out-of-order visit degrades to \
         \O(start) per slice." [Comment]
  , Note "`Data.Org.Index' is the only reader of `.org-glance/meta' in this repo, and \
         \`Data.Org.External' the only writer under it." [Comment]
  , Note "A segment on disk the MANIFEST does not name is invisible: the MANIFEST rename is \
         \the format's sole commit point." [Test]
  , Note "Only the OPEN segment's final line may be torn; the elisp re-signals on any other \
         \parse failure where this read-only instrument counts it (`ifMalformed') and carries on." [Test]
  , Note "A blob's entry is its file's FIRST headline — six corpus blobs open at level two, \
         \and a CHILD's id names a different record." [Test, Corpus]
  , Note "`dfIdless' is the instrument on itself: blobs this parser read and found no \
         \ORG_GLANCE_ID in, which is what kept `records without blobs' from reading as index lag." [Corpus]
  , Note "It named the largest cause: 28 blobs carried a non-English weekday in the planning \
         \line, so `planningP' failed, the drawer was no longer next and the headline lost its \
         \properties whole." [Corpus]
  , Note "What is left of the 21 idless blobs has not been attributed." [Corpus]
  , Note "The report spells the store, the fold's counts, the blob counts, the unmatched pair, \
         \then up to ten disagreeing ids with both values." [Test]
  , Note "Re-rendering a shifted stamp from `Timestamp' would spell it this library's way, \
         \which is `TextShow''s lossy job." [Comment]
  , Note "Month and year steps are the calendar's own; the widths modelled here are nominal." [Docs]
  , Note "THE LEDGER IS DERIVED, NEVER TRUTH: delete COMPLETIONS.jsonl and every entry is \
         \byte-identical with only the history gone." [Docs]
  , Note "The completion ledger is STORE-LEVEL and off the SERVED root, so a tree with no \
         \`.org-glance' repeats org-natively and records nothing — no daemon makes a store \
         \directory it was not given." [Comment]
  , Note "INCOMPLETE BY CONSTRUCTION: Emacs's `org-todo' writes org's own LOGBOOK and no line \
         \here, and there is no join." [Docs]
  , Note "`appendLine' is `openFd' append plus one `fdWriteBuf'; `BS.appendFile' measurably \
         \loses lines under concurrency." [Comment]
  , Note "Every IO failure in an append is swallowed: the org file is renamed into place by \
         \the time it runs." [Comment]
  , Note "The daemon appends only, never truncates, and touches no `meta/' file but its own two." [Interop]
  , Note "`tombstone' is org-glance's own WAL spelling (`:tombstone t'), which `Data.Org.Index' \
         \already reads, so there is no second vocabulary." [Docs]
  , Note "The peer's EXTERNAL.jsonl reader takes JSON `true' alone, pinned by its own \
         \`external-refresh-deletes-on-json-true-alone'; write for the stricter reader." [Interop]
  , Note "`trashBlob' reads the id BEFORE the move, the document being what the move takes away." [Test]
  , Note "KNOWN LIMIT: one blob, one tombstone, however many entries it holds — a hand-written \
         \blob's SECOND top-level entry loses its bytes and keeps its record." [Docs]
  , Note "THE READER MUTATES THE FILE NEVER, which lets two Emacsen fold one store with no \
         \lock: a repeated fold appends equal records, and a crash between the append and the \
         \cursor is that same repeated fold." [Interop]
  , Note "The peer's git conflict resolver names the WAL's own files positively (its invariant \
         \8), so a third file here would be out of its reach." [Docs]
  , Note "THE DIGESTS ARE WHY AN OFFSET IS SAFE: the store is git-synced and the cursor is \
         \tracked beside the file, so a union merge can insert another machine's lines ahead of \
         \it." [Interop]
  , Note "`refresh-external' re-derives each written id via `graph:insert' — blobs are read, \
         \never rewritten — and appends a tombstone for each deleted one under `graph:delete''s \
         \own guard." [Interop]
  , Note "A rename between an open and its write lands that line in the ROTATED file, which is \
         \never deleted on the pass that made it." [Docs]
  , Note "A NEW glance against an OLD org-glance degrades exactly: `--read-external' reads `id' \
         \alone, ignores keys it does not know, skips the line as no stored blob, and leaves it \
         \on disk spent by the cursor." [Docs]
  , Note "An OLD glance against a NEW org-glance writes the third field never, so nothing \
         \changes." [Docs]
  , Note "The GLANCE_CORPUS groups pass when the variable is unset and print `SKIPPED — \
         \GLANCE_CORPUS is unset' on stderr; a green run without those lines is unverified on \
         \the corpus half." [Unguarded]
  , Note "A GLANCE_CORPUS naming a missing directory fails loudly, as does a run that sampled \
         \nothing." [Test]
  , Note "The browser harness settles on the PAGE's schedule: every `setTimeout' is tracked \
         \with when it is due, so `wait:900' is 900 ms of that schedule and a 30 s reconnect \
         \backoff is never owed to it." [Browser]
  , Note "Where the page's own backoff decides the moment, `until:stale=off' polls for the \
         \CONDITION with a cap." [Browser]
  , Note "THE ELISP ONLY REPORTS: it prints what Emacs says and the DRIVER compares, so a step \
         \cannot agree with itself." [Interop]
  , Note "`test/interop/og.el' loads org-glance's LIVE `src/' under `emacs -Q -batch' with \
         \`load-prefer-newer' on, never `.eask/elpa''s installed copy; `drive.mjs' is the daemon \
         \plus one `?bootstrap=off' socket, and ONE temp store carries both." [Interop]
  , Note "EMACS_RUN=podman reuses org-glance's OWN Containerfile through its OWN `podman-build', \
         \the store bind-mounted at ITS OWN PATH so a compared path string means one thing on \
         \both sides of the mount." [Interop]
  , Note "`make interop' is out of `cabal test' for browser-check's reason and skips loudly." [Interop]
  , Note "The cases share ONE store and each asks about the state the one before it left; a \
         \failure is reported and the run continues, so the FIRST red line is the one to read." [Interop]
  , Note "Each side pins the EXTERNAL.jsonl format twice by hand — a golden string here, \
         \`test-external.el''s spelled-out `format' there — so renaming a field leaves both \
         \suites green." [Interop]
  , Note "Every other interop case only ever GROWS the file, the one event an offset survives, \
         \so `bytes-move-under-a-live-cursor' re-lays it with an owed line ahead of the folded \
         \prefix at the same length." [Interop]
  , Note "Severing `watchOrgTree''s inotify callback leaves the whole Haskell suite green; \
         \`browser-sees-emacs' is the case that goes red." [Interop]
  , Note "`Data.Org.Index' is otherwise read only over MANIFESTs TestIndex hand-wrote; here it \
         \folds one org-glance produced." [Interop]
  , Note "The hole is a TAGGED CAPTURE: it mints an id `refresh-external' skips as unknown, so \
         \the line is spent on nothing, and closing it turns that case red." [Interop]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "Three readers depend on that order." [Unguarded]
  , Note "Residency grows with the pool and by ~2.3 MB a worker." [Unguarded]
  , Note "The ~19 MB budget was the -N1 figure." [Unguarded]
  , Note "The first walk retained orders more." [Unguarded]
  , Note "A census reads more records than it keeps live." [Unguarded]
  , Note "A day of ordinary browser use moved the disagreements alone." [Unguarded]
  , Note "The weekday fix moved both counts by the blobs the instrument named." [Unguarded]
  , Note "The corpus scan is clean." [Unguarded]
  , Note "Two of the three constructors read as org's opposite." [Unguarded]
  , Note "A chain declaring no active word takes the keyword off." [Unguarded]
  , Note "A cursor carries an offset and TWO sha1s." [Interop]
  , Note "A cursor whose digests hold folds EXACT." [Interop]
  , Note "Bytes that no longer hash re-fold." [Interop]
  , Note "Only a fold past the cap retires the file." [Interop]
  , Note "A rotated file drains ahead of the live one." [Interop]
  , Note "One id per window, first sighting, last kind." [Interop]
  , Note "Every BREAK names a case the harness runs." [Interop]
  , Note "No break and no case is named twice." [Interop]
  , Note "The order is the story: the digest case runs before the three it drags." [Interop]
  , Note "Thirteen cases, each pinning a claim." [Interop]
  , Note "Drained reads BOTH answers." [Interop]
  , Note "Host Emacs is the default." [Interop]
  , Note "The skip names which of four is missing." [Interop]
  , Note "The hole is asserted as it is today." [Interop]
  , Note "`watchOrgTree' is covered by nothing in test/." [Interop]
  -- The doctor at startup: the same scan, cached for the wire.
  , Note "THE DAEMON RUNS THE DOCTOR AT STARTUP: after the store loads and BEFORE \
         \`finishLoading' opens the routes, `diagnose' caches a `Doctor' on the hub — \
         \failures and id collisions off the store's own QR, span violations and index \
         \drift off a fresh scan. It rides the view JSON — `/headlines' and the \
         \`set-rows' frame — as `doctor', never the banner." [Test]
  , Note "The CLI report and the daemon's `Doctor' fold off ONE `corpusDoctor' over the \
         \same scan, so `glance doctor' and the boot log cannot disagree on a count." [Test]
  , Note "The boot log shows one `doctor:' warn line per nonzero finding \
         \(`doctorWarnings'), or one `doctor: clean' info line, ONCE per boot off the \
         \first view the boot processes; a refetch says nothing." [Browser]
  -- The one-shot migration: the same walk, honest about the evidence.
  , Note "BACKFILL-CREATED IS HONEST ABOUT PRECISION: `Glance.Backfill' stamps every \
         \headline lacking `creationProperty' from the earliest `:LOGBOOK:' time in its \
         \subtree, else the file's mtime, else the run's day — and the report COUNTS each \
         \tier, so a wall of run-day stamps is never mistaken for real creation times." [Test]
  , Note "The backfill writes through `replaceSpans', ONE drift-locked splice per file that \
         \adds one drawer line and leaves every other byte alone, so it is idempotent and \
         \org-glance adopts it on the `EXTERNAL.jsonl' note; `--dry-run' reports and writes \
         \nothing." [Test]
  ]
-- * Walk
--
-- What the traversal collects, and the two filters between a headline and a row.

-- ** Paths as components.  One split, so no rule below spells a path twice.
wComps :: Path -> [String]
wComps = filter (not . null) . foldr step [""]
  where step '/' acc      = "" : acc
        step c   (h : t)  = (c : h) : t
        step _   []       = []
wJoin :: [String] -> Path
wJoin = intercalate "/"
wName :: Path -> String
wName p = case wComps p of { [] -> "" ; cs -> last cs }
wExt :: Path -> String
wExt p = case break (== '.') (reverse (wName p)) of
  (r, '.' : _) -> '.' : reverse r
  _noDot       -> ""
wSufs :: [a] -> [[a]]
wSufs []           = [[]]
wSufs l@(_ : rest) = l : wSufs rest

wOrgGlance, wData, wOccurrences, wBlobFile :: String
wOrgGlance   = ".org-glance"
wData        = "data"
wOccurrences = "occurrences"
wBlobFile    = "data.org"

-- ** The denylist
data Denied = DOverviews | DMeta | DTrash | DOccurrences | DConfig
  deriving (Eq, Ord, Show, Enum, Bounded)
-- ^ Names declined under a @.org-glance@ component.  A DENYLIST: @data@ is not
-- privileged in the walk, it survives by not being here.
everyDenied :: [Denied]
everyDenied = [minBound .. maxBound]
deniedName :: Denied -> String
deniedName DOverviews   = "overviews"
deniedName DMeta        = "meta"
deniedName DTrash       = "trash"
deniedName DOccurrences = wOccurrences
deniedName DConfig      = "config"
data Site = Directly | UnderData deriving (Eq, Show)
-- ^ Where the name is asked for.  A blob's history is asked for ANYWHERE under
-- @data@, a two-character id being unsharded.
siteOf :: Denied -> Site
siteOf DOverviews   = Directly
siteOf DMeta        = Directly
siteOf DTrash       = Directly
siteOf DOccurrences = UnderData
siteOf DConfig      = Directly
liftedByFlag :: Denied -> Bool
liftedByFlag DOverviews   = True
liftedByFlag DMeta        = True
liftedByFlag DTrash       = True
liftedByFlag DOccurrences = True
liftedByFlag DConfig      = False
-- ^ @--include-derived@ lifts a denial.  Config is INPUT to a parse, so it is
-- declined unconditionally, with its own accumulator and scan row.

newtype IncludeDerived = IncludeDerived Bool deriving (Eq, Show)
walkAll, walkKept :: IncludeDerived
walkAll  = IncludeDerived True
walkKept = IncludeDerived False

-- | Does PATH spell the component at all?  The allocation-free guard ahead of
-- the split, worth 1.7 s of the corpus walk.
namesOrgGlance :: Path -> Bool
namesOrgGlance p = any (wOrgGlance `isPrefixOf`) (wSufs p)
orgGlanceTails :: Path -> [[String]]
orgGlanceTails p | namesOrgGlance p = wTailsSlow p
                 | otherwise        = []
wTailsSlow :: Path -> [[String]]
wTailsSlow p = [ rest | d : rest <- wSufs (wComps p), d == wOrgGlance ]

deniedTail :: Denied -> [String] -> Bool
deniedTail d t = case (siteOf d, t) of
  (Directly,  c : _)    -> c == deniedName d
  (UnderData, c : rest) -> c == wData && deniedName d `elem` rest
  (_, [])               -> False
deniedIn :: [Denied] -> Path -> Bool
deniedIn ds p = or [ deniedTail d t | d <- ds, t <- orgGlanceTails p ]
-- | Which denial takes PATH, the flag having its say.  ONE rule for the walk
-- and the watch alike.
deniedBy :: IncludeDerived -> Path -> Maybe Denied
deniedBy (IncludeDerived incl) p =
  listToMaybe [ d | d <- everyDenied, not (incl && liftedByFlag d), deniedIn [d] p ]
isDerived, isConfig :: Path -> Bool
isDerived = deniedIn (filter liftedByFlag everyDenied)
isConfig  = deniedIn (filter (not . liftedByFlag) everyDenied)

-- | Is PATH in the canonical store, a blob's history excluded?  An occurrence
-- carries the LIVE entry's id, so ranking it canonical made the pair a tie.
isCanonical :: Path -> Bool
isCanonical p = not (deniedIn [DOccurrences] p) && any shard (orgGlanceTails p)
  where shard (c : _ : _) = c == wData
        shard _shallow    = False
isBlob :: Path -> Bool
isBlob p = wName p == wBlobFile && isCanonical p
isOrg :: Path -> Bool
isOrg p = map toLower (wExt p) == ".org"
-- | Emacs's sidecars, BOTH SHAPES EXACT — the auto-save on its closing @#@ too.
isSidecar :: Path -> Bool
isSidecar p = take 2 n == ".#" || (take 1 n == "#" && "#" `isSuffixOf` n) where n = wName p
isDocument :: Path -> Bool
isDocument p = isOrg p && not (isSidecar p)
-- | Would the walk COLLECT PATH?  All three predicates, which is what a capture
-- target is refused by.
isWalked :: Path -> Bool
isWalked p = isDocument p && isNothing (deniedBy walkKept p)
-- | The watch asks the walk's own rules through the facade, never a second copy.
facadeExports :: [(String, String)]
facadeExports = [ ("Glance.Query.derivedPath",  "isDerived")
                , ("Glance.Query.documentPath", "isDocument")
                , ("Glance.Query.configPath",   "isConfig") ]

-- ** One entry, one lstat
data Entry = EDir | EReg | ELink deriving (Eq, Show)
-- ^ @getSymbolicLinkStatus@, which never follows.  A FAILED stat answers 'EReg'
-- — the keep-on-name branch, silently.
data Points = AtDir | Elsewhere deriving (Eq, Show)
-- ^ The SECOND stat (@getFileStatus@, which follows).  'Elsewhere' is a file or
-- a missing target alike.
data Visit = VEnter | VKeep | VDecline Denied | VDrop | VErr deriving (Eq, Show)
-- | A link pays a second stat only where the answer could change what is
-- collected, so @.#name.org@ is refused by NAME ahead of both.
paysSecond :: IncludeDerived -> Entry -> Path -> Bool
paysSecond incl ELink p = isDocument p || isJust (deniedBy incl p)
paysSecond _    EDir  _ = False
paysSecond _    EReg  _ = False
visit :: IncludeDerived -> Entry -> Maybe Points -> Path -> Visit
visit incl EDir  _ p = maybe VEnter VDecline (deniedBy incl p)
visit incl EReg  _ p = keepByName incl p
visit incl ELink t p
  | not (paysSecond incl ELink p) = VDrop
  | t == Just AtDir               = maybe VDrop VDecline (deniedBy incl p)
  | otherwise                     = keepByName incl p
keepByName :: IncludeDerived -> Path -> Visit
keepByName incl p | isDocument p && isNothing (deniedBy incl p) = VKeep
                  | otherwise                                   = VDrop
data Given = GDir | GFile | GGone deriving (Eq, Show)
-- | A NAMED ROOT is never itself tested against the denylist, and it is the one
-- path probed for existence.
collectRoot :: Given -> Path -> Visit
collectRoot GDir  _ = VEnter
collectRoot GFile p = if isDocument p then VKeep else VDrop
collectRoot GGone _ = VErr

data Found = Found { fFiles :: [Path], fDirErrs :: [Path], fDerived :: [Path], fConfig :: [Path] }
  deriving (Eq, Show)
-- ^ The walk's four accumulators.  'fDerived' and 'fConfig' hold DIRECTORIES.
emptyFound :: Found
emptyFound = Found [] [] [] []
intoFound :: Visit -> Path -> Found -> Found
intoFound VKeep              p f = f { fFiles   = p : fFiles f }
intoFound VErr               p f = f { fDirErrs = p : fDirErrs f }
intoFound (VDecline DConfig) p f = f { fConfig  = p : fConfig f }
intoFound (VDecline _)       p f = f { fDerived = p : fDerived f }
intoFound VEnter             _ f = f
intoFound VDrop              _ f = f
walkReports :: Visit -> Bool
walkReports VErr         = True
walkReports VEnter       = False
walkReports VKeep        = False
walkReports (VDecline _) = False
walkReports VDrop        = False

data ScanRow = DirsScanned | DerivedSkipped deriving (Eq, Show, Enum, Bounded)
data Counted = Roots | Directories | FilesWalked deriving (Eq, Show)
scanCounts :: ScanRow -> Counted
scanCounts DirsScanned    = Roots
scanCounts DerivedSkipped = Directories
-- | The commands, each asked for BY NAME: a bare @glance@ prints the usage.
data Cli = CliScan | CliServe | CliDesktop | CliBackfill | CliRepl
  deriving (Eq, Show, Enum, Bounded)
permissiveArgs :: Cli -> Bool
permissiveArgs CliScan     = True
permissiveArgs CliServe    = False
permissiveArgs CliDesktop  = False
-- @backfill-created [DIR...] [--dry-run]@: roots and one flag, like @doctor@.
permissiveArgs CliBackfill = True
permissiveArgs CliRepl     = True
data ScanArg = ArgFlag | ArgRoot Path deriving (Eq, Show)
scanArg :: String -> ScanArg
scanArg "--include-derived" = ArgFlag
scanArg s                   = ArgRoot s

-- ** A row is a top entry, and it has something to show
data ColSpan = CsTodo | CsPriority | CsTitle | CsTags | CsScheduled | CsDeadline
  deriving (Eq, Ord, Show, Enum, Bounded)
-- ^ The six the table has a column for.  There is no @CsClosed@: a @CLOSED:@
-- stamp, a drawer, a body and children rescue no entry.
data Hl = Hl { hlLevel :: Int, hlCols :: [ColSpan], hlId :: Maybe Id } deriving (Eq, Show)
topLevel, blankEntry :: Hl -> Bool
topLevel h = hlLevel h == 1
blankEntry = null . hlCols
-- | The extents are cut over the WHOLE sequence and BOTH filters run before the
-- numbering, so a child and a blank entry each spend no ordinal.
recordsOf :: Path -> [Hl] -> [(RowId, Hl)]
recordsOf p hs = zipWith row [0 ..] [ h | h <- hs, topLevel h, not (blankEntry h) ]
  where row k h = (maybe (rowIdIn p k) Named (hlId h), h)
rowIdIn :: Path -> Int -> RowId
rowIdIn = Nth
rowIdText :: RowId -> String
rowIdText (Named (Id i)) = i
rowIdText (Nth p k)      = p ++ "#" ++ show k
-- | @#@ rather than @:@: a walked path always ends in its @.org@ extension, so
-- @FILE#K@ is recoverable at its LAST @#@.  Nothing in the library parses one.
splitRowId :: String -> Maybe (Path, Int)
splitRowId s = case break (== '#') (reverse s) of
  (k, '#' : p) | not (null k), all isDigit k -> Just (reverse p, read (reverse k))
  _notAnOrdinal                              -> Nothing

-- ** One row per id
data Claim = Claim Path Id deriving (Eq, Show)
beatsForId :: Path -> Path -> Bool
beatsForId a b = isCanonical a && not (isCanonical b)
-- | Kept in walk order, and every loser reported with what it lost to.
resolveIds :: [Claim] -> ([Claim], [(Claim, Claim)])
resolveIds = foldl' step ([], [])
  where step (kept, lost) c@(Claim a i) = case break (\(Claim _ j) -> j == i) kept of
          (_, [])                                 -> (kept ++ [c], lost)
          (before, held@(Claim b _) : after)
            | beatsForId a b -> (before ++ [c] ++ after, lost ++ [(held, c)])
            | otherwise      -> (kept, lost ++ [(c, held)])
resolveSites :: [String]
resolveSites = [ "Query.loadDir.summarise", "Store.storeRecords"
               , "Store.storeResult",       "Store.resolvedRows" ]
idCollisionHeader :: String
idCollisionHeader = "X-Glance-Id-Collisions"
idCollisionCap :: Int
idCollisionCap = 20   -- ^ pairs the scan lists

-- ** A blob's path, and where a deleted one goes
-- | @data\/\<2\>\/\<rest\>\/data.org@, the shard verbatim and UNFOLDED; an id of
-- two characters or fewer is not sharded at all.
blobPathIn :: Path -> Id -> Path
blobPathIn store (Id i)
  | length i > 2 = wJoin [store, wData, take 2 i, drop 2 i, wBlobFile]
  | otherwise    = wJoin [store, wData, i, wBlobFile]
trashDirIn :: Path -> Path
trashDirIn root = wJoin [root, wOrgGlance, deniedName DTrash]
-- | Where the blob at PATH is kept, the SHARD carried over so the id a restore
-- names is still spelled by the path.  ONLY A BLOB: a row in a shared org file
-- is many rows' document.
trashPathFor :: Path -> Path -> Maybe Path
trashPathFor root p
  | not (isBlob p) = Nothing
  | otherwise      = fmap under (afterData (wComps p))
  where under rest = trashDirIn root ++ "/" ++ wJoin rest ++ ".gz"
        afterData cs = case break (== wData) cs of
          (_, _d : rest) | not (null rest) -> Just rest
          _noStore                         -> Nothing
data Trashed = TKept Path | TNotABlob | TAlreadyThere Path | TMoveFailed
  deriving (Eq, Show)
data TrashStep = TReadId | TCopy | TRemove | TNote deriving (Eq, Ord, Show, Enum, Bounded)
-- | THE WHOLE BLOB DIRECTORY GOES, each file under the mirror of its own path.
trashOrder :: [TrashStep]
trashOrder = [TReadId, TCopy, TRemove, TNote]
data Sweep = SRecurse | SCopy | SSkip deriving (Eq, Show)
-- | @filesUnder@ takes the walk's OWN reading, so a link to a directory is
-- declined in each: following it copied a foreign tree in while the removal
-- took only the link.
underTrash :: Entry -> Maybe Points -> Sweep
underTrash EDir  _              = SRecurse
underTrash EReg  _              = SCopy
underTrash ELink (Just AtDir)   = SSkip
underTrash ELink _elsewhere     = SCopy

-- ** Measured
data WalkCensus = WalkCensus { cFiles :: Int, cHeadlines :: Int, cFails :: Int, cClashes :: Int }
  deriving (Eq, Show)
withMirrors, mirrorsDenied :: WalkCensus   -- ^ ~/sync, 2026-07-31
withMirrors   = WalkCensus 6313 13384 14 522
mirrorsDenied = WalkCensus 6290 12870 11 9
data Tally = Tally { tRows :: Int, tClashes :: Int } deriving (Eq, Show)
everyHeadline, topEntries, scanTally :: Tally   -- ^ ~/sync, 2026-08-01
everyHeadline = Tally 12875 9
topEntries    = Tally 10685 7
scanTally     = Tally 12884 9   -- ^ the parser oracle, which neither filter moves
topEntriesWalked, blankOnDisk, occurrencesOnDisk :: Int
topEntriesWalked  = 10441   -- ^ 2026-08-01, over 6287 files
blankOnDisk       = 0
occurrencesOnDisk = 0       -- ^ 2026-08-02, so the rule closed the hazard first
blobsOnDisk, blobGenerations, modernUuids :: Int
blobsOnDisk     = 6073
blobGenerations = 4         -- ^ superseded id schemes, beside the UUIDs
modernUuids     = 45
corpusDocs, corpusDirs, corpusEntries :: Int
corpusDocs    = 6287
corpusDirs    = 89691
corpusEntries = 702296
walkSecs, poolSecs, findSecs, strLoopSecs, rawLoopSecs :: Double
walkSecs    = 10.4   -- ^ the serial walk: most of a @glance doctor@
poolSecs    = 1.2    -- ^ the parallel read of every file, inside it
findSecs    = 2.0    -- ^ @find .@ — the syscall floor
strLoopSecs = 7.6    -- ^ @listDirectory@ + @lstat@ in `String'
rawLoopSecs = 3.3    -- ^ the same loop on @RawFilePath@
twoStatSecs, oneStatSecs, sharedTailsSecs, infixCostSecs :: Double
twoStatSecs     = 12.9    -- ^ before ONE @lstat@ an entry
oneStatSecs     = 12.1    -- ^ before 'namesOrgGlance' guarded the split
sharedTailsSecs = 0.130   -- ^ sharing one tails scan between the two rules; DECLINED
infixCostSecs   = 0.45    -- ^ what @isInfixOf@ costs over the hand-written scan
parallelWalk :: [(Int, Double)]
parallelWalk = [(1, 11.9), (8, 13.5)]   -- ^ @-N@, seconds; GC 1.0 s elapsed either way

wStore, wBlob, wOccur, wOverview, wCfg, wDoc :: Path
wStore    = "t/.org-glance"
wBlob     = "t/.org-glance/data/a7/92f0/data.org"
wOccur    = "t/.org-glance/data/a7/92f0/occurrences/2026-08-02.org"
wOverview = "t/.org-glance/overviews/c1f3/overview.org"
wCfg      = "t/.org-glance/config/tags/x.org"
wDoc      = "notes/a.org"
wSample :: [Path]
wSample = [ wDoc, "notes/.#a.org", "notes/#a.org#", "notes/#inbox.org", "notes/sub"
          , wBlob, wOccur, wOverview, wCfg, "t/.org-glance/meta/headlines.jsonl"
          , "t/.org-glance/trash/a7/92f0/data.org", "./overviews/x.org" ]
wTops :: [Hl]
wTops = [ Hl 1 [CsTitle] Nothing, Hl 2 [CsTitle] (Just (Id "deep"))
        , Hl 1 [] Nothing, Hl 1 [CsTodo] Nothing ]

walkNotes :: [Note]
walkNotes =
  [ Note "Nothing canonicalizes the root, so a symlink or bind mount renaming the component away walks every mirror as truth." [Unguarded]
  , Note "A mirror named as a root is entered and yields zero files, no error and no derived-skipped row; Walk.hs's comment says otherwise." [Unguarded]
  , Note "No test reaches foundDerived or foundConfig on a symlink; both implementations were run by hand over one tree (2 derived, 1 config, 2 config keywords, identical)." [Unguarded]
  , Note "A failed lstat falls to the keep-on-name branch silently, the way doesDirectoryExist used to swallow one into a False." [Unguarded]
  , Note "A dangling .org symlink is a permanent ReadFailed: the watch is filtered by the same rule, so no event that would clear it is ever delivered." [Test]
  , Note "A blob whose sharded remainder spells occurrences is indistinguishable by path from a two-character id's history, so no rule separates them." [Docs]
  , Note "The 2-5 s band needs a RawFilePath walk, which owes byte-level twins of isOrg, isSidecar, isDerived and isConfig — two encodings of the one rule the single definitions prevent." [Docs]
  , Note "Serving a mirror put 514 extra headlines in the table and rendered one headline twice under a tag filter, once from its blob and once from an overview." [Corpus]
  , Note "The extents are cut before the filter because subtreeSpans is org's outline rule over a DOCUMENT; for topLevel the two orders agree, checked exhaustively over every level shape up to five headlines, and on a predicate keeping levels 1 and 3 they disagree." [Test]
  , Note "Top-entry extents TILE rather than nest: consecutive ones meet exactly, the nesting having moved inside one extent." [Test]
  , Note "A word only a child carries matches nothing; materializing the entry is how the child is reached." [Test]
  , Note "blankEntry's tags clause never fires alone: org spells tags after a title and the parser hands `* :tag:' its colons as the TITLE." [Test]
  , Note "The filter is the RECORD's rule at the HEADLINE's layer because the ordinal numbers emitted rows; the layers agree since each span is Nothing exactly where recordOf cuts an empty cell." [Test]
  , Note "The one reachable path to a blank entry is set-state with a null keyword over a title-less row, which leaves `* ' in the file and deletes the row; a blank parent takes its whole subtree out of the view." [Test]
  , Note "A file whose last row goes takes its keyword contribution with it, so where it alone declared TODO the step is a moved palette and the socket closes instead of streaming the delete." [Test]
  , Note "An edit under a child moves the document, the digest and the extent and no cell: the entry is refreshed so materialize is drift-free, with no frame and no generation bump; `linked' rides in that JSON, so the child edit that does stream is the one giving the subtree its first link or taking its last." [Test]
  , Note "FILE#K replaced FILE:START, the offset that moved on any edit above the headline: measured live, one upsert where the offset id shipped three deletes and three inserts." [Test]
  , Note "The id carries a #, which a raw URL reads as a fragment: it rides the query string percent-encoded on both sides, and POST /command carries ids in a JSON body." [Test]
  , Note "The nine surviving collisions are genuine duplicates — an elpa working copy of a checkout, and documents whose data.org repeats the source document's id." [Corpus]
  , Note "ONE BLOB, ONE TOMBSTONE: the move takes the whole directory and the note is keyed off the FIRST headline's id, so a hand-written blob's second top entry loses its bytes and keeps its record." [Comment]
  , Note "A destination that already exists is a second deletion of one id and is refused, the first one's bytes being what is kept; a refused delete notes nothing, having moved nothing." [Comment]
  , Note "The trash is compressed because a trash that costs nothing is one nobody empties." [Comment]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "No scan row counts files." [Unguarded]
  , Note "Scan alone is permissive, and reads an unknown flag as a root." [Unguarded]
  , Note "`resolveIds' has four call sites." [Unguarded]
  , Note "Denying the mirrors dropped 23 files and 514 headlines." [Unguarded]
  , Note "The top-entry filter moved the store, and the parser oracle sees more." [Unguarded]
  , Note "The rule costs a real tree nothing." [Unguarded]
  , Note "The occurrence rule closed the hazard before it was reachable." [Unguarded]
  , Note "An ORG_GLANCE_ID is an opaque string: four schemes precede the UUIDs." [Unguarded]
  , Note "A corpus's cost here is its DIRECTORY count." [Unguarded]
  , Note "The walk is most of the wall and the pool cannot touch that half." [Unguarded]
  , Note "Marshalling a FilePath is the whole gap to a 2-5 s walk." [Unguarded]
  , Note "One lstat and the fast guard took 2.5 s off." [Unguarded]
  , Note "Sharing the tails scan is 1.2% and is declined." [Unguarded]
  , Note "The walk got SLOWER as -N rose." [Unguarded]
  ]
-- * Keyword configuration
--
-- Recognition is a SUPERSET and classification is WIDEST-SCOPE.  One chain,
-- two readers, three answers: `classify' folds it, `keywordSources' reports it,
-- `settableStates' is that flattened.

-- | A @#+TODO:@ line is a CYCLE, and a LIST: the org file's own left-to-right
-- spelling is the tree's whole say over how states sort and how a palette draws.
data TodoKw = TodoKw { tkActive :: [Kw], tkInactive :: [Kw] } deriving (Eq, Show)
noKw :: TodoKw
noKw = TodoKw [] []
kwsOf :: TodoKw -> [Kw]
kwsOf (TodoKw a i) = a ++ i
-- | Layers as one: first-seen order, a word declared both ways anywhere ACTIVE.
mergeKw :: [TodoKw] -> TodoKw
mergeKw ks = TodoKw as (filter (`notElem` as) (nub (concatMap tkInactive ks)))
  where as = nub (concatMap tkActive ks)
-- | Org's own cycle, read off the parser's default context and answering under
-- the name @default@ at the head of the chain.
builtinKeywords :: TodoKw
builtinKeywords = TodoKw [Kw "TODO"] [Kw "DONE"]
-- | A keyword token: letters and underscores, so a starred meta is undeclarable
-- and needs no guard of its own at either wall.
keywordText :: String -> Maybe Kw
keywordText s | not (null s), all (\c -> isAlpha c || c == '_') s = Just (Kw s)
              | otherwise                                        = Nothing

-- | FOUR scopes and no fifth.  The rank travels beside the name because a tree
-- may configure a tag called @system@.
data Scope = Default | System | TagScope Tag | FileScope deriving (Eq, Show)
scopeRank :: Scope -> Int
scopeRank Default      = 0
scopeRank System       = 1
scopeRank (TagScope _) = 2
scopeRank FileScope    = 3
scopeName :: Scope -> String
scopeName Default            = "default"
scopeName System             = "system"
scopeName (TagScope (Tag t)) = t
scopeName FileScope          = "file"

-- | What CLASSIFIES: @system.org@'s cycle and the tag cycles.  The recognition
-- union has no field here, so nothing below can classify or authorize by it.
data Chain = Chain TodoKw [(Tag, TodoKw)]
-- | The scopes answering for a headline carrying TAGS in a file declaring
-- FILEKW, WIDEST FIRST.
keywordScopes :: Chain -> TodoKw -> [Tag] -> [(Scope, TodoKw)]
keywordScopes (Chain sys tags) fileKw worn =
  [(Default, builtinKeywords), (System, sys)]
    ++ [ (TagScope t, kw) | t <- worn, Just kw <- [lookup t tags] ]
    ++ [(FileScope, fileKw)]
-- | Is KW active there?  The first scope with an opinion answers; a keyword no
-- scope of this headline claims falls through as ACTIVE — unfinished work.
classify :: Chain -> TodoKw -> [Tag] -> Kw -> Bool
classify ch fileKw tags kw =
  fromMaybe True (listToMaybe (mapMaybe (says . snd) (keywordScopes ch fileKw tags)))
  where says (TodoKw a i) | kw `elem` a = Just True
                          | kw `elem` i = Just False
                          | otherwise   = Nothing
-- | The chain turned inside out — @GET \/keywords@: each keyword under the
-- WIDEST source declaring it and nowhere below, an emptied source dropped.
-- Several rows merge by source NAME, the @file@ entry being their union.
keywordSources :: Chain -> [(TodoKw, [Tag])] -> [(Scope, TodoKw)]
keywordSources ch rows = widest [] (sortOn (scopeRank . fst) chain)
  where filed = mergeKw (map fst rows)
        chain = [ e | (_, tags) <- rows, e <- keywordScopes ch filed tags ]
        widest _ [] = []
        widest seen ((s, kw) : rest)
          | null as && null is = widest seen rest
          | otherwise          = (s, TodoKw as is) : widest (seen ++ as ++ is) rest
          where as = filter (`notElem` seen) (tkActive kw)
                is = filter (`notElem` seen) (tkInactive kw)
-- | The states a row may be set to: that one row's sources FLATTENED, so the
-- offer and the wall cannot come apart.
settableStates :: Chain -> (TodoKw, [Tag]) -> [Kw]
settableStates ch row = concatMap (kwsOf . snd) (keywordSources ch [row])

-- ** Layers

-- | One config file: @config\/system.org@, or @config\/tags\/TAG.org@ whose
-- NAME is the tag it configures.
data Layer = Layer
  { lPath     :: Path
  , lTag      :: Maybe Tag          -- ^ 'Nothing' is the tree's own system layer.
  , lDigest   :: Digest             -- ^ of the bytes it was SHOWN as; the lock a write presents back.
  , lCycle    :: [CfgLine]
  , lTemplate :: Maybe String
  , lViews    :: [(String, Query)]
  , lColors   :: StateColors
  }
isSystemLayer :: Layer -> Bool
isSystemLayer = isNothing . lTag
-- | A layer's lines, as far as a write may read them.
data CfgLine = Blank | TodoLine [Kw] [Kw] | OtherLine deriving (Eq, Show)
lineKws :: CfgLine -> [Kw]
lineKws Blank          = []
lineKws (TodoLine a i) = a ++ i
lineKws OtherLine      = []
cycleOf :: [CfgLine] -> TodoKw
cycleOf ls = mergeKw [ TodoKw a i | TodoLine a i <- ls ]

-- | One root's config.  'lySeed' is stored rather than derived: it unions every
-- entry read, shadowed ones included, in walk order, where the chain's tags keep
-- the FIRST config of each tag across directories.
data Layers = Layers { lyChain :: Chain, lySeed :: TodoKw, lyTree :: TreeSettings, lyDirs :: [Path] }
-- | Every keyword a file under LY declaring FILEKW recognizes, IN PALETTE
-- ORDER; the seed's only other consumer is the parse context.
recognizedKeywords :: Layers -> TodoKw -> TodoKw
recognizedKeywords ly fileKw = mergeKw [builtinKeywords, lySeed ly, fileKw]
configDirIn :: Path -> Path
configDirIn root = root ++ "/.org-glance/config"
configPathsIn :: Path -> (Path, Path)
configPathsIn dir = (dir ++ "/system.org", dir ++ "/tags")
-- | The directories the walk MET, else the one a writer would put there.
configDirsIn :: Path -> Layers -> [Path]
configDirsIn root ly | null (lyDirs ly) = [configDirIn root]
                     | otherwise        = lyDirs ly

-- ** Tree-wide settings

-- | Every TREE-WIDE value as one record: the load caches it and @GET \/config@
-- folds it over files it has JUST READ, so a member owes ONE line and both
-- paths have it.
data TreeSettings = TreeSettings { tsViews :: [(String, Query)], tsColors :: StateColors }
noTreeSettings :: TreeSettings
noTreeSettings = TreeSettings [] (StateColors [])
-- | Views take the FIRST system layer that names one; the colours take EVERY
-- system layer's lines.
treeSettingsOf :: [Layer] -> TreeSettings
treeSettingsOf ls = TreeSettings views colors
  where sys    = filter isSystemLayer ls
        views  = [ (svId v, q) | v <- savedViews
                 , Just q <- [listToMaybe (mapMaybe (lookup (svId v) . lViews) sys)] ]
        colors = StateColors (concat [ c | StateColors c <- map lColors sys ])

-- | A saved view: an id, its @system.org@ pragma, and the query where no layer
-- names one.  A fourth view is one entry.
data SavedView = SavedView { svId :: String, svPragma :: String, svBuiltin :: Query }
savedViews :: [SavedView]
savedViews =
  [ SavedView "default" "GLANCE_DEFAULT_FILTER" (Query "state:*active*")
  , SavedView "agenda"  "GLANCE_AGENDA_FILTER"  (Query "state:*active* -planned:*empty* sort:scheduled")
  , SavedView "archive" "GLANCE_ARCHIVE_FILTER" (Query "tag:archive")
  ]
savedView :: String -> Maybe SavedView
savedView vid = listToMaybe [ v | v <- savedViews, svId v == vid ]
-- | Absent means the built-in; a line naming nothing means the EMPTY query; a
-- view no build carries is the empty query too.
viewQueryIn :: String -> TreeSettings -> Query
viewQueryIn vid ts = fromMaybe (maybe (Query "") svBuiltin (savedView vid)) (lookup vid (tsViews ts))
-- | The ANSWER: an ordered array in registry order, each view's query NOW, so a
-- client reads the order without iterating keys.
viewsAnswer :: TreeSettings -> [(String, Query)]
viewsAnswer ts = [ (svId v, viewQueryIn (svId v) ts) | v <- savedViews ]

-- | The @#+GLANCE_STATE_COLORS:@ lines: theme first, then @KEYWORD=VALUE@ pairs.
-- SHAPE alone is validated — an unknown theme declares tokens nothing reads and
-- a non-colour is a value CSS ignores, both the author's business.
newtype StateColors = StateColors [(String, [(Kw, String)])] deriving (Eq, Show)
-- | Folded: EVERY line is read, one per theme being the shape, and a keyword
-- named twice takes its LAST spelling.
colorsOf :: StateColors -> [(String, [(Kw, String)])]
colorsOf (StateColors ls) = [ (th, [ (k, last (hues th k)) | k <- keys th ]) | th <- nub (map fst ls) ]
  where keys th = nub [ k | (t, ps) <- ls, t == th, (k, _) <- ps ]
        hues th k = [ h | (t, ps) <- ls, t == th, (k', h) <- ps, k' == k ]
-- | FLAT on the wire in both directions: one @{theme, keyword, hue}@ each.
colorsWire :: StateColors -> [(String, Kw, String)]
colorsWire sc = [ (th, k, h) | (th, ps) <- colorsOf sc, (k, h) <- ps ]

-- ** Writing a layer

-- | WHOSE FILE a setting is: a 'TreeWide' one is @system.org@'s alone.
data SettingScope = TreeWide | PerLayer deriving (Eq, Show)
-- | A setting is a REGISTRY ROW carrying how a write reads it, so a new member
-- cannot join without saying so.
data ConfigSetting = ConfigSetting { csName :: String, csScope :: SettingScope, csMoved :: ConfigParts -> Bool }
configSettings :: [ConfigSetting]
configSettings =
  [ ConfigSetting "views"    TreeWide (any (triMoved . snd) . cpViews)
  , ConfigSetting "colors"   TreeWide (triMoved . cpColors)
  , ConfigSetting "template" PerLayer (triMoved . cpTemplate)
  ]
-- | The registry masked by scope: a tag layer's write cannot reach a tree-wide
-- line whatever it named.
settingsFor :: Layer -> [ConfigSetting]
settingsFor l | isSystemLayer l = configSettings
              | otherwise       = filter ((== PerLayer) . csScope) configSettings
settingsWritten :: Layer -> ConfigParts -> [String]
settingsWritten l p = [ csName s | s <- settingsFor l, csMoved s p ]

-- | Three-valued: absent leaves the region, empty takes it away, a value writes it.
data Tri a = Untouched | Emptied | Given a deriving (Eq, Show)
triMoved :: Tri a -> Bool
triMoved Untouched = False
triMoved Emptied   = True
triMoved (Given _) = True
-- | What a write names.  A RECORD, each part its own type: positional
-- @Maybe Text@s let a caller swap two and still compile.
data ConfigParts = ConfigParts
  { cpLines    :: Tri [CfgLine]
  , cpViews    :: [(String, Tri Query)]
  , cpColors   :: Tri StateColors
  , cpTemplate :: Tri String
  }
noParts, everyPart :: ConfigParts
noParts   = ConfigParts Untouched [] Untouched Untouched
everyPart = ConfigParts (Given [TodoLine [Kw "TODO"] [Kw "DONE"]])
                        [ (svId v, Given (svBuiltin v)) | v <- savedViews ]
                        (Given (StateColors [])) (Given bareTemplate)
-- | ONE layer, ONE digest, ONE splice: four regions of one file ride one write,
-- since four writes would be four digests.
data LayerWrite = LayerWrite Layer Digest ConfigParts
data ConfigRefusal = NotATodoLine | DeclaresNothing | UnknownLayer | UnknownView | MalformedBody | Drift | TooLarge
  deriving (Eq, Show)
configStatus :: ConfigRefusal -> Int
configStatus NotATodoLine    = 400
configStatus DeclaresNothing = 400
configStatus UnknownLayer    = 400
configStatus UnknownView     = 400
configStatus MalformedBody         = 400
configStatus Drift           = 409
configStatus TooLarge        = 413
-- | The parts a write to KNOWN layers carries, or why it wrote nothing.  The
-- served layer list is the allowlist and the whole of the traversal defence.
writeLayer :: [Layer] -> LayerWrite -> Either ConfigRefusal [String]
writeLayer known (LayerWrite l pin p)
  | lPath l `notElem` map lPath known = Left UnknownLayer
  | pin /= lDigest l                  = Left Drift
  | otherwise = case cpLines p of
      Untouched -> Right (settingsWritten l p)
      Emptied   -> Right (settingsWritten l p ++ ["lines"])
      Given ls  -> blockEdits ls >> Right (settingsWritten l p ++ ["lines"])
-- | What a layer may say, checked ahead of the write: blanks drop, every line
-- left is a @#+TODO:@ pragma, the block declares at least one keyword, and an
-- EMPTY block is the deletion.
blockEdits :: [CfgLine] -> Either ConfigRefusal [CfgLine]
blockEdits ls
  | null kept                     = Right []
  | OtherLine `elem` kept         = Left NotATodoLine
  | null (concatMap lineKws kept) = Left DeclaresNothing
  | otherwise                     = Right kept
  where kept = filter (/= Blank) ls
-- | The whole-line splice every tree-wide pragma and the @#+TODO:@ block share.
data PragmaEdit = Rewrite | InsertUnderHeader | DeleteLine deriving (Eq, Show)
pragmaLineEdits :: Bool -> String -> PragmaEdit
pragmaLineEdits _     "" = DeleteLine
pragmaLineEdits True  _  = Rewrite
pragmaLineEdits False _  = InsertUnderHeader
-- | LAST line wins, the way a reader scrolling the file reads it.  One NAME per
-- setting, folded for the read and rendered for the write.
settingOf :: String -> [(String, String)] -> Maybe String
settingOf key ls = listToMaybe (reverse [ v | (k, v) <- ls, k == key ])
-- | The pin for a file that is not there: creation is that one lock rather than
-- a second write path.
absentPin :: Digest
absentPin = Digest ""

-- | The template a tagged capture expands when no layer names one.  A CONSTANT
-- at the end of the chain, so every case takes ONE path through the expansion.
bareTemplate :: String
bareTemplate = "* %?"
-- | The tag's own layer (the FIRST file configuring it), then the system
-- layer's, then 'bareTemplate'.
captureTemplate :: Tag -> [Layer] -> String
captureTemplate t ls = fromMaybe bareTemplate (listToMaybe (mapMaybe lTemplate (mine ++ sys)))
  where mine = take 1 [ l | l <- ls, lTag l == Just t ]
        sys  = filter isSystemLayer ls

-- ** Fixtures: the sample a rule about this section is stated over

cfgSample :: Chain
cfgSample = Chain (TodoKw [Kw "STARTED"] [Kw "CANCELLED"])
                  [ (Tag "book",   TodoKw [Kw "READING"] [Kw "READ"])
                  , (Tag "system", TodoKw [Kw "PINNED"] []) ]
lySample :: Layers
lySample = Layers cfgSample seed noTreeSettings []
  where seed = mergeKw [ TodoKw [Kw "STARTED"] [Kw "CANCELLED"]
                       , TodoKw [Kw "READING"] [Kw "READ"], TodoKw [Kw "PINNED"] [] ]
sysA, sysB, bookLayer :: Layer
sysA = Layer "/r/.org-glance/config/system.org" Nothing (Digest "d1")
             [TodoLine [Kw "STARTED"] [Kw "CANCELLED"]] (Just "* SYSTEM %?")
             [("default", Query "tag:x")] (StateColors [("light", [(Kw "TODO", "#111")])])
sysB = Layer "/r/v/.org-glance/config/system.org" Nothing (Digest "d2") [] Nothing
             [("agenda", Query "tag:y")] (StateColors [("dark", [(Kw "TODO", "#eee")])])
bookLayer = Layer "/r/.org-glance/config/tags/book.org" (Just (Tag "book")) (Digest "d3")
                  [TodoLine [Kw "READING"] [Kw "READ"]] (Just "* READING %?") [] (StateColors [])
bookRow, bareRow :: (TodoKw, [Tag])
bookRow = (noKw, [Tag "book"])
bareRow = (TodoKw [Kw "READING"] [], [])

configNotes :: [Note]
configNotes =
  [ Note "Config lives at <root>/.org-glance/config/{system.org,tags/*.org}, the tag being the file NAME." [Test]
  , Note "Config files are inputs, never rows: the walk declines config/ and the reader reaches it by path." [Test]
  , Note "A config change reseeds and reloads the world — recognition changed means every file's parse may change — debounced, with view-changed following the keyword-union move." [Test]
  , Note "Reordering a #+TODO: line is a palette move: the state column sorts by the cycle, the palette's which-key letters are assigned over the declared order, and the socket closes view-changed." [Test, Corpus]
  , Note "Sets answer RECOGNITION alone: seedContext builds Context's two Sets from the ordered lists, the one boundary where a keyword becomes a Set." [Test]
  , Note "default drawing first gives TODO the letter t and DONE the letter d in every tree; an empty store's palette is org's own pair." [Test]
  , Note "hrDeclared is stored beside the union and FORCED: a TodoKeywords field is strict to WHNF alone, so a thunk would pin the whole parse for the process's life." [Test, Corpus]
  , Note "set-state legality is the row's own chain: a keyword any named row's chain lacks refuses the WHOLE request, naming the keyword and the row." [Test]
  , Note "`+' IN THE STATE PALETTE DECLARES BEFORE IT SETS: the state is written\
         \ into a config layer, the store's reread is WAITED FOR, and only then is\
         \ `set-state' fired — the chain wall above is walked THROUGH rather than\
         \ around.  A word the store has not reread yet is reported as declared." [Test]
  , Note "THE NAMESPACE IS WHERE THE DECLARATION GOES: `system' the tree, `tag:X'\
         \ the rows carrying X.  The select is `system', the tags the applied query\
         \ names — the rows on screen are the rows that filter chose — then the tag\
         \ layers the tree already has.  `default' is org's builtin pair, which is\
         \ CODE and has no file to write into, so it is no namespace here." [Test]
  , Note "A TAG LAYER IS MINTED BY BEING WRITTEN TO, the rule `system.org' already\
         \ had: `filesIn' can only list what exists, so a state could never be added\
         \ to a tag with no file.  Only under the FIRST config dir's own `tags/',\
         \ and only where org can read the basename back as a tag." [Test]
  , Note "A STATE'S SPELLING IS CHECKED ON THE WAY IN, before the count: a word\
         \ org cannot read makes `todoPragmas' yield nothing, so without the wall\
         \ the writer is told the block declares nothing rather than which word\
         \ did it.  The charset is the PARSER's own `isKeywordChar'." [Test]
  , Note "A HUE IS ASKED FOR ONCE PER THEME.  The colour config is keyed by theme\
         \ end to end, so a form with one field edits whichever theme is on and\
         \ leaves the other on a palette slot.  A colour is the SYSTEM layer's, so\
         \ a state minted under a tag moves two files." [Test]
  , Note "GET /keywords reads every ids/id occurrence, so ?ids=a&ids=b says what ?ids=a,b says — the repeated form is what an id containing a comma owes." [Test]
  , Note "GET /tags answers PER ROW — {rows, vocabulary, counts, unknown} — the vocabulary the whole store's and counts ROWS per tag, counted per request because stTags counts FILES." [Test]
  , Note "GET /config reads the FILES rather than the loaded layers, the digest it hands out being the lock; it serves each layer's lines, keywords, template and digest beside the tree's settings and the build's theme names." [Test]
  , Note "A MISSING file pinned with a real digest stays ReadFailed; the probe sits at the start of the write and rename(2) has no exclusive form, so a file created inside that window is replaced." [Test]
  , Note "ORDER IS DATA: two absent pragmas insert at ONE offset and applyEdits resolves them in list order rather than refusing." [Test]
  , Note "The daemon embeds the tree's default view into the served page as DEFAULT_QUERY, read off the STORE at request time." [Test]
  , Note "P (set-saved-view) writes ONE view under the digest GET /config just served and with no lines key, so the #+TODO: block stands." [Test]
  , Note "- over that palette arms RESET: a letter then writes the EMPTY query, which takes the line off, and the write re-reads /config for the built-in it now lives with." [Browser]
  , Note "The states table's STATE rides its layer's write and its COLOUR rides system.org's, so one row moves two files in one flush; a keyword no layer declares is listed under file, colourable and immovable." [Browser]
  , Note "Two editors, one cycle: the states table and the keywords box are both views of the layer's text, so takeLayer reads the box only while its own panel shows." [Browser]
  , Note "The client names a part only where it MOVED — always sending the template puts every layer's first heading through the one-top-entry wall." [Browser]
  , Note "One drift-locked POST /config per FILE that moved, each awaited, each under its own digest; a refusal SELECTS its layer." [Browser]
  , Note "Creating the FIRST .org-glance/config in a tree is two directories at once, which fsnotify arms and never enters, so writeLayer reseeds through Watch.writeSpans." [Test]
  , Note "A capture template is a layer's FIRST ^\\*+ line to EOF, right-trimmed; everything above it is the pragma region, so the two regions cannot overlap." [Test]
  , Note "The template write's one wall is ONE TOP ENTRY: a template opening at a deeper level writes a blob with no entry in it." [Test]
  ]
-- * Store, watch, HTTP surface

-- ** The projection
--
-- One entry per @.org@ file, keyed by PATH, so the elements are walk order and
-- a served answer equals a fresh `loadDir' — rows and counts both.

data LoadFailure = ReadFailed | DecodeFailed | ParseFailed deriving (Eq, Show)

data FileEntry = FileEntry
  { feRows    :: [RowId]             -- ^ the last GOOD parse; a failure keeps them
  , feFailure :: Maybe LoadFailure
  , feDigest  :: Digest              -- ^ of the bytes `loadFile' decoded, taken there
  , feTags    :: [Tag]               -- ^ one file's SET, which is why `stTags' counts FILES
  , feKws     :: [Kw]                -- ^ the FILE's, since every row of one shares them
  }

newtype Gen   = Gen Int      deriving (Eq, Ord, Show)  -- ^ per process, never persisted
newtype Print = Print String deriving (Eq, Show)       -- ^ what survives a restart

data Store = Store
  { stFiles   :: [(Path, FileEntry)]  -- ^ path order, which is walk order
  , stDirErrs :: Int                  -- ^ written by the LOAD alone, so it moves on a reseed
  , stGen     :: Gen
  , stPrint   :: Print
  }
-- ^ No field by id: resolution answers which file WINS, where a count said only
-- how many claim it.

stRows :: Store -> [RowId]
stRows = concatMap (feRows . snd) . stFiles

-- | Tag to how many FILES carry it: the per-file projection is a set, so forty
-- rows of one file contribute 1.
stTags :: Store -> [(Tag, Int)]
stTags st = [ (t, length [ () | (_, fe) <- stFiles st, t `elem` feTags fe ]) | t <- vocab ]
  where vocab = nub (concatMap (feTags . snd) (stFiles st))

-- | ONE record per file merges the keyword sets: an N-file fold, and there is
-- no per-row list for it to truncate.
storeKeywords :: Store -> [Kw]
storeKeywords = nub . concatMap (feKws . snd) . stFiles

-- | Path plus the digest of the bytes it was parsed from, folded in path order.
-- A file contributing no rows stands as its PATH alone.
fingerprint :: Store -> Print
fingerprint st = Print (concatMap stamp (sortOn fst (stFiles st)))
  where stamp (p, fe) = p ++ (if null (feRows fe) then "" else unwrap (feDigest fe))
        unwrap (Digest d) = d

-- | The tree's fingerprint, the store's generation, and THE DAY the request was
-- answered on.  THE DAY RIDES ALONG WHATEVER THE QUERY SPELLS and no reader
-- tests for a clock word: a day word resolves per request (`dayWords'), so a
-- store nothing touched across midnight must revalidate rather than answer 304
-- with yesterday's rows -- and a rename of the word cannot leave a detector
-- behind.  One extra revalidation a day is the whole cost.
data ETag = ETag String Gen String deriving (Eq, Show)
etagOf :: String -> Store -> ETag
etagOf day st = ETag (take 16 p) (stGen st) day where Print p = stPrint st
etagText :: ETag -> String
etagText (ETag p (Gen n) day) = "\"" ++ p ++ "-g" ++ show n ++ "-d" ++ day ++ "\""
cacheControl :: String
cacheControl = "no-cache"

-- | A file's load folded in.  `orgParse' is all-or-nothing, so a failure KEEPS
-- the rows the last good parse left and marks the entry.
putFile :: Path -> Digest -> Either LoadFailure ([RowId], [Tag], [Kw]) -> Store -> Store
putFile p d outcome st = st { stFiles = sortOn fst ((p, entry) : others) }
  where others = [ e | e <- stFiles st, fst e /= p ]
        was    = listToMaybe [ fe | (q, fe) <- stFiles st, q == p ]
        entry  = case outcome of
          Left f                 -> FileEntry (maybe [] feRows was) (Just f) d
                                              (maybe [] feTags was) (maybe [] feKws was)
          Right (rows, tgs, kws) -> FileEntry rows Nothing d tgs kws

removeFile :: Path -> Store -> Store
removeFile p st = st { stFiles = [ e | e <- stFiles st, fst e /= p ] }

-- ** The generation
--
-- `installed' is the ONE rule and has TWO callers, so they cannot disagree
-- about it; the counter comes off the OLD store, which is what makes a reseed
-- carry it over, where a fresh counter would start at zero.

data Installer = PerFileEvent | ConfigReseed deriving (Eq, Show, Enum, Bounded)
genWriters :: [Installer]
genWriters = [minBound .. maxBound]

installed :: Store -> Store -> Bool -> [Frame] -> Store
installed old next outcomeMoved out = next { stGen = bumped (stGen old) }
  where bumped (Gen n) = Gen (if null out && not outcomeMoved then n else n + 1)

-- | A step's frames, and the CLOSE that REPLACES them where the palette moved:
-- a client never receives rows built against a palette that is already gone.
guarded :: Bool -> [Frame] -> [Frame]
guarded True  _  = [Close ViewChanged]
guarded False fs = fs

data StoreWriter = FinishLoading | Publish deriving (Eq, Show, Enum, Bounded)
-- ^ The two and only writers of the store TVar.  `FinishLoading' bypasses
-- `guarded': nothing can have subscribed behind a 503.

-- ** Frames
--
-- A message and a close are told apart by the COMPILER.

data Frame = Op RowOp | Close CloseReason deriving (Eq, Show)
data RowOp = SetRows [RowId] | UpsertRow RowId | DeleteRow RowId deriving (Eq, Show)
data CloseReason = ViewChanged | Resync deriving (Eq, Show, Enum, Bounded)

closeReason :: CloseReason -> String
closeReason ViewChanged = "view-changed"
closeReason Resync      = "resync"

frameOp :: Frame -> Maybe RowOp
frameOp (Op o)    = Just o
frameOp (Close _) = Nothing

-- | The ops IDS owe between two views, both sides read through the store's own
-- id RESOLUTION so a streamed row is a served row.  UPSERTS LEAD, so a client
-- applying the batch in order never shows fewer rows than the store has.
rowFrames :: [RowId] -> [(RowId, String)] -> [(RowId, String)] -> [Frame]
rowFrames ids before after =
  [ Op (UpsertRow i) | i <- ids, Just r <- [lookup i after], lookup i before /= Just r ]
  ++ [ Op (DeleteRow i) | i <- ids, isNothing (lookup i after) ]

streamed :: (Store -> [(RowId, String)]) -> [RowId] -> Store -> Store -> [Frame]
streamed resolve touched before after = rowFrames touched (resolve before) (resolve after)

mailboxCap :: Int
mailboxCap = 1024

-- | The frame a socket opens with, snapshotted INSIDE the subscribing
-- transaction; @?bootstrap=off@ drops it and trades the gap for it.
subscribeFrames :: Bool -> Store -> [Frame]
subscribeFrames off st = [ Op (SetRows (stRows st)) | not off ]

-- ** The load gate

data LoadState = Loading Double | Loaded deriving (Eq, Show)
data Gate = HttpGate Double | UpgradeGate deriving (Eq, Show)
-- ^ Per ROUTE, and the upgrade constructor carries no elapsed: a rejected
-- upgrade has no reader that would use it.

tenths :: Double -> Double
tenths s = fromIntegral (round (s * 10) :: Int) / 10

body503 :: Gate -> String
body503 (HttpGate s) = "{\"loading\":true,\"elapsed\":" ++ show (tenths s) ++ "}"
body503 UpgradeGate  = "{\"loading\":true}"

retryAfter :: Int
retryAfter = 1

-- ** The route table

data Method  = GET | POST deriving (Eq, Ord, Show)   -- ^ HEAD is GET's; no entry names it
data RefusalBody = JsonRefusal | TextRefusal deriving (Eq, Show)
data HttpCode = S200 | S304 | S400 | S404 | S405 | S409 | S413 | S503 deriving (Eq, Ord, Show)

data Route = Route
  { rPath    :: String
  , rNeeds   :: Bool      -- ^ answers out of the store, so a 503 while the walk runs
  , rRefusal :: RefusalBody
  , rMethods :: [Method]  -- ^ with a handler each; the 405 sentence reads off this
  }

routes :: [Route]
routes =
  [ Route "/"           False TextRefusal [GET]
  , Route "/headlines"  True  TextRefusal [GET]
  , Route "/refer"      True  TextRefusal [GET]
  , Route "/headline"   True  JsonRefusal [GET, POST]
  , Route "/command"    True  JsonRefusal [POST]
  , Route "/config"     True  JsonRefusal [GET, POST]
  , Route "/capture"    True  TextRefusal [GET]
  , Route "/keywords"   True  TextRefusal [GET]
  , Route "/links"      True  TextRefusal [GET]
  , Route "/tags"       True  TextRefusal [GET]
  , Route "/properties" True  TextRefusal [GET]
  , Route "/ws"         True  TextRefusal [GET]
  , Route "/status"     False JsonRefusal [GET]
  ]

data Verb = VGet | VHead | VPost | VOther deriving (Eq, Show)
wanted :: Verb -> Maybe Method
wanted VGet   = Just GET
wanted VHead  = Just GET
wanted VPost  = Just POST
wanted VOther = Nothing

-- | The load gate runs AHEAD of the method check.
routeAnswer :: Route -> LoadState -> Verb -> HttpCode
routeAnswer r (Loading _) _ | rNeeds r = S503
routeAnswer r _ v = case wanted v of
  Just m | m `elem` rMethods r -> S200
  _notTaken                    -> S405

routeAt :: String -> Maybe Route
routeAt p = listToMaybe [ r | r <- routes, rPath r == p ]

takesText :: Route -> String
takesText r = rPath r ++ " takes " ++ intercalate " and " (map show (rMethods r))
notFoundText :: String
notFoundText = "not found: " ++ intercalate ", " (map rPath routes) ++ ", or an asset name"

-- | @\/status@ answers whether or not the tree has loaded (`rNeeds' False), so
-- the 200 is LIVENESS and the `ready' flag READINESS: `{ok, ready, loading}',
-- plus `elapsed' while loading or `rows' once loaded.
statusFields :: [String]
statusFields = ["ok", "ready", "loading", "elapsed", "rows"]
writeHint :: String                               -- ^ DERIVED, like `notFoundText'; spelled by hand it had missed @/config@
writeHint = "method not allowed; "
         ++ intercalate " and " [ "POST " ++ rPath r | r <- routes, POST `elem` rMethods r ]
         ++ " write"
wsHint :: String
wsHint = "/ws is a websocket endpoint; connect with Upgrade: websocket"

-- | ONE path segment, and what is left to reject is the traversal pair and the
-- empty name.
safeName :: String -> Bool
safeName n = not (null n) && n `notElem` [".", ".."] && not (any (`elem` "/\\") n)

-- ** What rides a response

statsHeaders :: [String]
statsHeaders = [ "X-Glance-Rows", "X-Glance-Files", "X-Glance-Parse-Failures"
               , "X-Glance-Decode-Failures", "X-Glance-Read-Failures", "X-Glance-Id-Collisions" ]
pageHeaders :: [String]
pageHeaders = ["X-Glance-Total", "X-Glance-Has-Next", "X-Glance-Archived"]

-- | The startup doctor's summary, a GLOBAL field on every view JSON envelope
-- (`/headlines' and the `set-rows' frame): the eight counts, the `clean' flag
-- and the `warnings' the boot log shows.  `TestSpec' pins it against `doctorJSON'.
doctorFields :: [String]
doctorFields = [ "clean", "warnings", "parseFailures", "decodeFailures"
               , "readFailures", "spanViolations", "idCollisions", "drift"
               , "unindexed", "recordless" ]

data Answered = A200 | A304 deriving (Eq, Show)
headersOn :: Answered -> [String]
headersOn A200 = ["ETag", "Cache-Control"] ++ statsHeaders ++ pageHeaders
headersOn A304 = ["ETag", "Cache-Control"]

data BodyKind = JsonBody | HtmlBody | PlainBody | NotModified | AssetFile
  deriving (Eq, Show, Enum, Bounded)
data Sizer = Sized | Warp deriving (Eq, Show)
sizedBy :: BodyKind -> Sizer
sizedBy JsonBody    = Sized
sizedBy HtmlBody    = Sized
sizedBy PlainBody   = Sized
sizedBy NotModified = Warp
sizedBy AssetFile   = Warp
-- ^ The 503 is a `JsonBody', so it carries its own length like every other.

varyHeader :: String
varyHeader = "Vary: Accept-Encoding"
varies :: [BodyKind]
varies = [minBound .. maxBound]
-- ^ The gzip middleware writes it on every HTTP response, 304s included.  A
-- websocket rejection is no `BodyKind' and carries none.

bodyCap :: Int
bodyCap = 1024 * 1024
limitCap :: Int
limitCap = 20000

data Paging = WholeSet | PageOf Int deriving (Eq, Show)
pageOf :: Maybe Int -> Either HttpCode Paging
pageOf Nothing  = Right WholeSet
pageOf (Just n) | n > limitCap = Left S400
                | otherwise    = Right (PageOf n)

-- | Which refusal a request gets where several apply, coarsest first.
refusalOrder :: [HttpCode]
refusalOrder = [S413, S400, S404, S409]
rankOf :: HttpCode -> Int
rankOf s = length (takeWhile (/= s) refusalOrder)

data WriteRoute = PostHeadline | PostCommand | PostConfig deriving (Eq, Show, Enum, Bounded)
writesStore :: WriteRoute -> Bool
writesStore PostHeadline = False
writesStore PostCommand  = False
writesStore PostConfig   = False
-- ^ A write route reads the store for the row, the spans and the digest, writes
-- the FILE, and the watch re-reads what was written.

-- ** The watch

data Edge  = Leading | Trailing deriving (Eq, Show)
data Clock = Monotonic | Wall deriving (Eq, Show)
data Loop  = Serial | Concurrent deriving (Eq, Show)

data Watch = Watch
  { wDelayMs :: Int
  , wTickMs  :: Int
  , wEdge    :: Edge
  , wClock   :: Clock
  , wCeiling :: Maybe Int   -- ^ every event OVERWRITES the path's timestamp
  , wLoop    :: Loop
  }

watch :: Watch
watch = Watch 100 25 Trailing Monotonic Nothing Serial

-- | The paths last touched at least DELAY before NOW, and what is left pending.
-- Pure, and the whole of the debounce.
due :: Double -> Double -> [(Path, Double)] -> ([Path], [(Path, Double)])
due delay now pend = (map fst (ripe True), ripe False)
  where ripe want = [ e | e <- pend, (now - snd e >= delay) == want ]

-- | Ripe paths taken OUT in the transaction before they are settled, so a nudge
-- arriving mid-parse waits a turn.  A turn with nothing ripe writes NOTHING.
data Turn = Turn { tRipe :: [Path], tDirties :: Bool } deriving (Eq, Show)
drain :: Double -> Double -> [(Path, Double)] -> Turn
drain delay now pend = Turn ripe (not (null ripe))
  where (ripe, _rest) = due delay now pend

data Step = Reload Path | Reseed deriving (Eq, Show)
-- | The two do not mix: a reseed re-walks the whole tree, so it already covers
-- every ordinary path that ripened beside it.
settleOf :: (Path -> Bool) -> [Path] -> [Step]
settleOf cfg ps | any cfg ps = [Reseed]
                | otherwise  = map Reload ps

data Update = ApplyFile Path | DropFile Path deriving (Eq, Show)
-- | Deletion is decided by EXISTENCE at reload time; the event kind is never
-- consulted, which is what keeps the behaviour one across backends.
reloadOf :: Bool -> Path -> Update
reloadOf True  = ApplyFile
reloadOf False = DropFile

data Source = Inotify | Daemon deriving (Eq, Show)
-- | `nudge' is the ONE door into the queue, so the SOURCE cannot widen the
-- filter: a nudge is filtered exactly as an event is.
nudgeAccepts :: (Path -> Bool) -> Source -> Path -> Bool
nudgeAccepts watchable _ = watchable

data WSite = SiteInbox | SiteBlob | SiteWriteOne | SiteCommit | SiteLayer
  deriving (Eq, Show, Enum, Bounded)
data WOutcome = Wrote | Refused deriving (Eq, Show)

-- | THE ONE DOOR EVERY WRITE ROUTE LEAVES THROUGH: a splice under a DIGEST, and
-- a nudge of the path just written on the SUCCESS branch alone.
writeSpans :: WSite -> Path -> Digest -> [(Span, String)] -> WOutcome -> [Path]
writeSpans _ path _ _ Wrote   = [path]
writeSpans _ _    _ _ Refused = []

-- ** Materialize, and the subtree lens

data Addr = AtRow RowId | AtChild RowId Int deriving (Eq, Show)
-- | The digest and the id stay the ROW's: one file, one lock.
lockOf :: Addr -> RowId
lockOf (AtRow r)     = r
lockOf (AtChild r _) = r

data ChildAsk = Numbered Int | Unparsable deriving (Eq, Show)
childStatus :: Int -> ChildAsk -> HttpCode
childStatus _ Unparsable   = S400
childStatus n (Numbered k) = if k >= 0 && k < n then S200 else S404

headlineFields :: [String]
headlineFields = [ "id", "path", "digest", "org", "body", "properties", "planning", "logbook"
                 , "cells", "links", "child", "parent", "children", "ownLines" ]

data Refused409 = R409Stale | R409Drift | R409Planning String deriving (Eq, Show)
reason409 :: Refused409 -> String
reason409 R409Stale             = "stale"
reason409 R409Drift             = "drift"
reason409 (R409Planning _) = "planning"

data LensRegion = RPlanning | RProps | RLog deriving (Eq, Show, Enum, Bounded)
-- ^ THREE regions lifted out of a subtree; every byte left is the BODY's, so a
-- child's drawer is body text.  Every cut is by WHOLE lines.
lensOrder :: [LensRegion]
lensOrder = [minBound .. maxBound]
rankIn :: LensRegion -> Int
rankIn r = length (takeWhile (/= r) lensOrder)

data LensWay = RoundTrip | ServeOnly deriving (Eq, Show)
carried :: LensRegion -> LensWay
carried RPlanning = RoundTrip
carried RProps    = RoundTrip
carried RLog      = ServeOnly

data LensProp = PropLine String | PropSet String String deriving (Eq, Show)
-- ^ An untouched pair carries no key to re-render, so it cannot be canonicalized.
renderProp :: String -> LensProp -> String
renderProp _   (PropLine l)      = l
renderProp ind (PropSet k v) = ind ++ ":" ++ k ++ ": " ++ v

data PlanEntry = PlanLine String | PlanSet Kw String deriving (Eq, Show)
planKeywords :: [Kw]
planKeywords = [Kw "SCHEDULED", Kw "DEADLINE", Kw "CLOSED"]

-- | Untouched entries where they were, moved ones canonical BEHIND them.  An
-- empty list drops the line; a value is checked by REPARSE before a byte moves.
planningLine :: [PlanEntry] -> Maybe String
planningLine [] = Nothing
planningLine es = Just (unwords (was ++ moved))
  where was   = [ t | PlanLine t <- es ]
        moved = [ k ++ ": " ++ v | Kw k <- planKeywords, PlanSet (Kw k') v <- es, k == k' ]

hiddenProperties :: [String]
hiddenProperties = ["ORG_GLANCE_ID", "ORG_GLANCE_CREATION_TIME"]
shownPairs :: [(String, String)] -> [(String, String)]
shownPairs = filter ((`notElem` hiddenProperties) . fst)
preservedPairs :: [(String, String)] -> [(Int, String)]
preservedPairs ps = [ (i, l) | (i, (k, l)) <- zip [0 ..] ps, k `elem` hiddenProperties ]

-- | Hidden lines woven back at the INDEX they sat at.
weave :: [(Int, String)] -> [String] -> [String]
weave hs = go 0
  where go i rest = [ l | (j, l) <- hs, j == i ] ++ case rest of
          []       -> [ l | (j, l) <- hs, j > i ]
          (x : xs) -> x : go (i + 1) xs

-- | The drawer goes only when nothing hidden is in it.
drawerOf :: [(Int, String)] -> [LensProp] -> String -> Maybe [String]
drawerOf [] [] _   = Nothing
drawerOf hs ps ind = Just ([":PROPERTIES:"] ++ weave hs (map (renderProp ind) ps) ++ [":END:"])

-- | Regions go back at BODY indices — the subtree line less what every region
-- ahead took out — so two naming one line land in `lensOrder', and one past the
-- body's length lands at the end.
spliceRegions :: [String] -> [(LensRegion, Int, [String])] -> [String]
spliceRegions body regs = go 0 body (sortOn (\(r, i, _) -> (i, rankIn r)) regs)
  where
    go _ ls []                            = ls
    go n ls owed@((_, i, rs) : rest)
      | n >= i    = rs ++ go n ls rest
      | otherwise = case ls of
          []       -> concat [ x | (_, _, x) <- owed ]
          (l : ls') -> l : go (n + 1) ls' owed

data LensPost
  = PostRaw   String Digest                     -- ^ @{org, digest}@
  | PostSplit String [LensProp] [PlanEntry] Digest  -- ^ @{body, properties, planning, digest}@
-- ^ No constructor names both shapes (a request that does is a 400) and none
-- carries a body without both lists beside it.

-- | The horizontal run off the END of each line, stepping OVER the terminator:
-- a `stripEnd' would take a CRLF line's @\r@ with the spaces.
untrailed :: String -> String
untrailed s = intercalate "\n" (map trim (split s))
  where split t = case break (== '\n') t of
          (a, [])        -> [a]
          (a, _nl : end) -> a : split end
        trim l | "\r" `isSuffixOf` l = hz (init l) ++ "\r"
               | otherwise           = hz l
        hz = reverse . dropWhile (`elem` " \t") . reverse

data TrailDoor = ViaRecompose | ViaBlob | ViaCapture | ViaRawOrg
  deriving (Eq, Show, Enum, Bounded)
composes :: TrailDoor -> Bool
composes ViaRecompose = True
composes ViaBlob      = True
composes ViaCapture   = True
composes ViaRawOrg    = False
-- ^ The raw @{org}@ shape composes nothing — the client hands a whole document
-- back — and is trimmed on the way in all the same.

-- ** Fixtures: the sample a rule about this section is stated over

fxFiles :: [(Path, FileEntry)]
fxFiles =
  [ ("a.org", FileEntry [Named (Id "x")]               Nothing            (Digest "d1") [Tag "work"]               [Kw "TODO"])
  , ("b.org", FileEntry [Nth "b.org" 0, Nth "b.org" 1] Nothing            (Digest "d2") [Tag "work", Tag "home"]    [Kw "TODO", Kw "DONE"])
  , ("c.org", FileEntry []                             (Just ParseFailed) (Digest "d3") []                         [])
  ]
fxStore :: Store
fxStore = Store fxFiles 0 (Gen 0) (Print "0123456789abcdefXX")

fxPending :: [(Path, Double)]
fxPending = [("a.org", 100.0), ("b.org", 100.95)]

storeNotes :: [Note]
storeNotes =
  [ Note "The server binds before it walks; the walk runs on its own thread and the watch starts after finishLoading, installing the store and opening the routes in one transaction." [Test]
  , Note "The websocket upgrade is REFUSED with 503 and never accepted onto an empty store: a set-rows of one claims the tree has no headlines." [Test]
  , Note "A plain non-upgrade request to /ws is routed as HTTP and gets the long body; once loaded, GET answers 400 with wsHint and every other method 405." [Test]
  , Note "The short 503 body is asserted by nothing." [Unguarded]
  , Note "An upgrade aimed at any path but /ws is rejected." [Test]
  , Note "The assets are no table entry: every one-segment path falls through to the assets directory, so the miss below the table doubles as the route list." [Test]
  , Note "With no limit the whole set is served, which is the mode the shell settles into." [Test]
  , Note "watched is a document by the walk's own rule minus what the walk declined to enter; the config is the one deliberate exception, watched though never a row because a change to one changes the files that are." [Test]
  , Note "The watch re-parses ONE file per event, seeded from defaultContext, so no file's #+TODO: reaches another's headlines." [Test, Docs]
  , Note "A failed load streams nothing; dropping the rows instead empties the table between two keystrokes." [Test]
  , Note "There is no ceiling and no leading edge: a generator writing in a tight loop is invisible until it stops, which is right for an autosave and wrong for a log." [Unguarded]
  , Note "A config reseed BLOCKS the drain loop, so the 100 ms debounce means 100 ms or a full re-walk." [Docs]
  , Note "reseed builds the fresh store OUTSIDE the transaction and installs it wholesale; make the loop concurrent and any edit that landed during the walk is silently reverted." [Comment]
  , Note "fsnotify arms a newly created directory and does not traverse into it, so a blob under a fresh shard raises no event ever — which is what every write's nudge buys." [Test]
  , Note "KNOWN GAP: an EXTERNAL create into a fresh shard is invisible until a restart." [Unguarded]
  , Note "Nothing loads or publishes at the nudge door; settle stays the sole store updater." [Comment]
  , Note "stDirErrs and stPrint are written by the load alone, so a directory that becomes readable is invisible until a reseed or a restart." [Unguarded]
  , Note "Two headlines of ONE file sharing an id keep the FIRST on both sides — a file does not outrank itself — the per-file tag projection never sees the duplicate, and X-Glance-Id-Collisions reports one whose kept and dropped paths are the same file." [Test]
  , Note "storeKeywords merging one record per file is sound because every row of a file shares that file's sets; the day a record carries its own it becomes a silent truncation." [Unguarded]
  , Note "Editing the LOSER of a shared id streams nothing; a winner that goes away re-points the id at the row behind it with an upsert." [Test]
  , Note "A full mailbox abandons that client's backlog and its registration where retrying the transaction would hold the watcher up: it waits for no browser, and the close is named resync." [Test]
  , Note "The backlog is never drained on the way out, so the cut is O(1) inside the transaction." [Test]
  , Note "publish coalesces WITHIN a step, not across them; what overruns the mailbox is a burst of steps." [Test]
  , Note "The socket is NOT filtered: a client with an empty query splices in an archived row /headlines would not have served." [Unguarded]
  , Note "The store is the SERVED view's own resolution; a per-client projection would be the second authoritative structure this design does not have." [Docs]
  , Note "One ETag covers every query variant: an HTTP cache is keyed by URL, so no Vary is owed beyond the gzip middleware's own Accept-Encoding." [Test]
  , Note "The websocket rejection carries no Vary." [Test]
  , Note "The fingerprint is deliberately not recomputed per edit; the generation already says how far the tree has moved since the load." [Test]
  , Note "The path is in the fingerprint because an id-less row id is FILE#K: the same bytes under another name are a different document." [Test]
  , Note "Materialize pins the digest taken at LOAD, beside offsets measured in that same text; re-reading the file at GET time would pin bytes the extent was never measured against." [Test]
  , Note "A 409 stale is the store's digest and a 409 drift the file's own re-digest; both leave the target byte-identical and both mean materialize again." [Test]
  , Note "A byte-identical commit still rewrites the file, so it costs an inotify event and a re-parse; guarded then finds nothing moved." [Test]
  , Note "A malformed ?child= is a 400 raised BEFORE the id lookup, and the 1 MiB cap answers 413 before it too." [Test]
  , Note "GET /headline's answer carries `parent`, null being the row itself, and `ownLines` so the same bytes are never both a paragraph and the child that owns them." [Test]
  , Note "Decompose then recompose is byte-identical up to trailing space; on a subtree already carrying none the two statements are one." [Test]
  , Note "A child's property drawer and a child's logbook are BODY text: they belong to the child's own lens." [Test]
  , Note "Property pairs are read by splitting lines, never through the parser's Properties, which uppercases keys and re-tokenises values; a line that is somehow not a property comes back keyless and the client drops the row." [Test]
  , Note "Raw property lines are consumed one per pair, never looked up, so one pair spelled twice keeps both spellings." [Test]
  , Note "The planning region is the whole LINE the outermost timestamp sits on, keywords and spacing included, so an untouched line goes back byte for byte and a round trip tidies no permutation." [Test]
  , Note "Every planning value must READ BACK as the line the write would produce, a newline is refused outright, and the 409 names the field AND CARRIES THE READER'S OWN SENTENCE -- the date grammar's for a key this server SETS, naming every form it would have taken, and reparse's for CLOSED, where `timestamp' is the right word. THE WALL IS ALSO THE TRANSFORM for the two keywords a key SETS: SCHEDULED and DEADLINE take the planning grammar whole and come back the bytes org itself would write, where CLOSED -- org's own bookkeeping, settable by nothing -- still takes reparse alone. THE RAW HALF TRANSFORMS NOTHING: a WholeSubtree is a document the client typed, and rewriting bytes inside it would be the server editing a buffer." [Test]
  , Note "The logbook is located textually — past the title line, ahead of the first child's stars — and the scan steps OVER the property drawer's extent, so a :LOGBOOK: line inside one stays the properties'." [Test]
  , Note "An unterminated drawer owns every line it may own; a headline with no logbook does not grow one." [Test]
  , Note "The sheet's logbook strip shows the drawer's INTERIOR lines alone, which is a display cut: what re-splices is the whole original drawer." [Test]
  , Note "A body with fewer lines than an index takes the region at the end, where a client that deleted the lines above it has left room; a region the headline never had goes on the line under the title." [Test]
  , Note "Deliberately out of the trailing-space rule: Data.Org.Edit, content-agnostic by law, and the config layer's #+TODO: and template regions, a file being edited as its own lines." [Docs]
  , Note "Display and TextShow stay off the wire: cells are sliced from spans and the view value is hand-built, since SCHEMA.md is the contract." [Docs]
  , Note "The daemon binds 127.0.0.1 until privilege tiers land." [Docs]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "The generation has exactly two writers." [Unguarded]
  , Note "The store TVar has exactly two writers." [Unguarded]
  , Note "The two 503 bodies differ, and the upgrade one spells no elapsed." [Unguarded]
  , Note "The drain loop is serial." [Unguarded]
  , Note "A turn with nothing ripe writes nothing." [Unguarded]
  , Note "Five write sites leave through the one door." [Unguarded]
  ]
-- * Query language
--
-- @?q=@ is SCHEMA.md's micro-syntax, ported term for term from
-- @table-view.js@.  ONE parse, THREE readers: which tokens NARROW, which state
-- the ORDER, which state the COLUMNS — so a token cannot be two of them at once.

-- ** The view's six columns

data Col = CState | CPrio | CTitle | CSched | CDead | CTag
  deriving (Eq, Ord, Show, Enum, Bounded)
data ColKind  = KBadge | KText deriving (Eq, Show)
data Extra = Badges | Values | Multi deriving (Eq, Show)
-- ^ What a KEY carries past sorting: the state palette, its two metas, the tags list.
data Column = Column { cCol :: Col, cKey :: String, cHead :: String, cCellKind :: ColKind }

-- | ONE table, so the four that must agree cannot drift: the wire's @columns@,
-- `rowJSON''s cells, `filterKeys' and the search haystack's field order.
viewColumns :: [Column]
viewColumns =
  [ Column CState "state"     "State"     KBadge
  , Column CPrio  "priority"  "#"         KBadge
  , Column CTitle "title"     "Title"     KText
  , Column CSched "scheduled" "Scheduled" KText
  , Column CDead  "deadline"  "Deadline"  KText
  , Column CTag   "tag"       "Tags"      KText
  ]

cols :: [Col]
cols = [minBound .. maxBound]
keyOf, headOf :: Col -> String
keyOf  c = fromMaybe "" (lookup c [(cCol x, cKey  x) | x <- viewColumns])
headOf c = fromMaybe "" (lookup c [(cCol x, cHead x) | x <- viewColumns])
kindOf :: Col -> ColKind
kindOf c = fromMaybe KText (lookup c [(cCol x, cCellKind x) | x <- viewColumns])
filterKeys :: [String]
filterKeys = map cKey viewColumns
-- | `hrSearch''s @\x1f@ fields, in column order — the same list, so no drift.
searchOrder :: [Col]
searchOrder = map cCol viewColumns
tagsColumn :: Int
tagsColumn = length (takeWhile (/= CTag) searchOrder)

extras :: Col -> [Extra]
extras CState = [Badges, Values]
extras CPrio  = [Badges]
extras CTitle = []
extras CSched = []
extras CDead  = []
extras CTag   = [Multi]

-- | The tags CELL is the one sorted for drawing; every other cell is the file's.
cellSorted :: Col -> Bool
cellSorted CTag   = True
cellSorted CState = False
cellSorted CPrio  = False
cellSorted CTitle = False
cellSorted CSched = False
cellSorted CDead  = False

type Cell = Maybe String
-- | An absent cell and an empty one are one empty field: the row JSON's @null@
-- and what @key:*empty*@ reads.
blank :: Cell -> Bool
blank = maybe True null

-- ** Field resolution
--
-- ONE resolution answers both "is this a key" and "what does it read", so the
-- grammar and the matcher cannot disagree about a token.

data VK    = VSort | VColumns | VView deriving (Eq, Show, Enum, Bounded)
-- | THE AXIS A KEY JOINS, and `ref' and `from' STAND AS TWO.  They are one edge
-- read from its two ends and never one predicate: `ref:T' asks what the ROW's
-- own subtree points at and `from:T' what T's does, and no row's answer to one
-- decides its answer to the other.  On one axis `+ref:T' would widen `from:'
-- too and put two unrelated questions inside one conjunction; as two, every
-- axis law reads them exactly as it reads `tag' beside `state'.
data Field = FCol Col | FPlanned | FRef | FFrom | FView VK | FFree deriving (Eq, Show)

viewKeys :: [(String, VK)]
viewKeys = [("sort", VSort), ("columns", VColumns), ("view", VView)]
dateCols :: [Col]
dateCols = [CSched, CDead]

fieldOf :: String -> Maybe Field
fieldOf k | k == "planned"   = Just FPlanned
          | k == "ref"       = Just FRef
          | k == "from"      = Just FFrom
          | k == "substring" = Just FFree
          | Just v <- lookup k viewKeys = Just (FView v)
          | otherwise        = FCol <$> lookup k [(cKey x, cCol x) | x <- viewColumns]

-- | The cells a key names; a predicate reads them and nothing else.
fieldCells :: Field -> [Col]
fieldCells (FCol c)  = [c]
fieldCells FPlanned  = dateCols
fieldCells FRef      = []
fieldCells FFrom     = []
fieldCells (FView _) = []
fieldCells FFree     = []

-- | Every predicate value is case-folded but the two references': a row id is
-- exact, and the kind half behind the `?' is SLUGGED rather than folded.
folds :: Field -> Bool
folds FRef      = False
folds FFrom     = False
folds (FCol _)  = True
folds FPlanned  = True
folds (FView _) = True
folds FFree     = True

-- | A view token is dropped ABOVE the negation inverter, so it narrows nothing
-- in either polarity — under it, @-sort:x@ would empty the table.
narrows :: Field -> Bool
narrows (FView _) = False
narrows (FCol _)  = True
narrows FPlanned  = True
narrows FRef      = True
narrows FFrom     = True
narrows FFree     = True

allFields :: [Field]
allFields = map FCol cols <> [FPlanned, FRef, FFrom, FFree] <> map (FView . snd) viewKeys

-- ** The scanner and its terms

-- | THE SIGN A TOKEN OPENS WITH: @-@ negates, @+@ widens its own axis, and a
-- token opening with neither is `Unsigned'.
data Sign = Unsigned | Neg | Add deriving (Eq, Show)
data Tok  = Tok Sign Bool String deriving (Eq, Show)  -- sign, opened with @"@, body
data Term = Term Sign (Maybe String) String deriving (Eq, Show)  -- sign, key, value
data Sc = Sc
  { body     :: String
  , sign     :: Sign
  , quoted   :: Bool
  , seen     :: Bool
  , hasBody  :: Bool
  , inQuotes :: Bool
  }

-- | Which sign a term wears.  A SIGN IS THE TOKEN'S FIRST CHARACTER, so a term
-- wears exactly one: a second sign lands in the body.
tmSign :: Term -> Sign
tmSign (Term s _ _) = s

-- | The sign C opens a token with, or `Nothing' where C is body text.
signOf :: Char -> Maybe Sign
signOf '-' = Just Neg
signOf '+' = Just Add
signOf _   = Nothing

-- | How a sign is written; one equation per constructor, no wildcard.
signMark :: Sign -> String
signMark Unsigned = ""
signMark Neg      = "-"
signMark Add      = "+"

isSep :: Char -> Bool
isSep c = c == '&' || c == ' ' || c == '\t' || c == '\n'

-- | Q cut into tokens; an unclosed quote runs to the end of Q.
scanQ :: String -> [Tok]
scanQ q = reverse (flush st out)
  where
    (st, out) = foldl' step (fresh, []) q
    fresh = Sc "" Unsigned False False False False
    step (s, acc) c
      | c == '"'                  = (s { seen     = True
                                       , hasBody  = True
                                       , quoted   = quoted s || not (hasBody s)
                                       , inQuotes = not (inQuotes s) }, acc)
      | not (inQuotes s), isSep c = (fresh, flush s acc)
      -- SEEN GUARDS THE SIGN, so a second one lands in the body.
      | not (seen s), Just sg <- signOf c = (s { seen = True, sign = sg }, acc)
      | otherwise                 = (s { body = c : body s, seen = True, hasBody = True }, acc)
    flush s acc
      | seen s    = Tok (sign s) (quoted s) (reverse (body s)) : acc
      | otherwise = acc

-- | @key:value@ splits on the FIRST @:@ or @=@; a body opening with one has no key.
splitKey :: String -> Maybe (String, String)
splitKey s | null k || null rest = Nothing
           | otherwise           = Just (k, drop 1 rest)
  where (k, rest) = break (\c -> c == ':' || c == '=') s

termOf :: Tok -> Term
termOf (Tok sg qt b)
  | qt = Term sg Nothing b
  | Just (k, v) <- splitKey b, isJust (fieldOf k) = Term sg (Just k) v
  | otherwise = Term sg Nothing b

parseQ :: String -> [Term]
parseQ = map termOf . scanQ
parse1 :: String -> Term
parse1 s = case parseQ s of { [t] -> t ; _ -> Term Unsigned Nothing s }

-- | A predicate's VALUE splits on @|@ and empty alternatives are DROPPED; a
-- value left with none narrows nothing, which is the @key:@ rule.  The bar is a
-- PREDICATE's, so a literal one is free text's alone.
alts :: String -> [String]
alts = filter (not . null) . splitOnStr "|"

-- | AXES AND, ALTERNATIVES OR, and an empty query matches every row; the plain
-- tokens AND within one axis too, and `axisTest' is where the added ones join.
tokensAnd :: [Bool] -> Bool
tokensAnd = and
-- | A predicate with no alternative left narrows nothing.
predOf :: [Bool] -> Bool
predOf [] = True
predOf ts = or ts

-- | What a term READS, which is the AXIS it joins: a key nobody resolves left
-- it free text, key and all, and @substring:@ shares FFree with free text, so
-- @milk +bread@ widens ONE axis rather than two.
readsAs :: Term -> Field
readsAs (Term _ k _) = fromMaybe FFree (k >>= fieldOf)

-- | (negated, what it reads, the value as the matcher sees it).
evalOf :: Term -> (Bool, Field, String)
evalOf t@(Term _ _ v) = (tmSign t == Neg, f, if folds f then qFold v else v)
  where f = readsAs t

-- | The ATOMS a term contributes: a predicate's alternatives, free text's one
-- word.  The bar is a predicate's, so free text's own stays literal.
atomsOf :: Term -> [String]
atomsOf (Term _ k v) | isJust k  = alts v
                     | otherwise = [v | not (null v)]

-- | AN UNSIGNED OR ADDED TERM CARRYING NO ATOM NARROWS NOTHING and establishes
-- no axis, so it is dropped ahead of the grouping: @state:@, @+state:@,
-- @+state:|@ and a lone @+@ each leave Q meaning what it meant, which is the
-- half-typed token's own law.  Left in BASE an empty-valued term is a match-all
-- and floods its axis's disjunction.  A lone @-@ is untouched and still empties
-- the table.
vacuous :: Term -> Bool
vacuous t = tmSign t /= Neg && null (atomsOf t)

-- | ONE AXIS as its group G, ATOM reading one term's own predicate: BASE is the
-- plain and negated tokens' readings, inverted where negated, and WIDE the added
-- tokens'.  With the empty conjunction true and the empty disjunction false,
-- @(base nonempty AND its conjunction) OR the disjunction of WIDE@ — today's law
-- where no @+@ joined, @base OR wide@ beside one, and WIDE alone on an axis
-- holding only @+@ tokens, so a lone @+tag:work@ is @tag:work@.
axisTest :: (Term -> Bool) -> [Term] -> Bool
axisTest atom g = (not (null base) && tokensAnd base) || or wide
  where base = [if tmSign t == Neg then not (atom t) else atom t
               | t <- g, tmSign t /= Add]
        wide = [atom t | t <- g, tmSign t == Add]

-- | Q's narrowing law: the narrowing terms group by the axis they read, each
-- group folds by `axisTest', and the axes AND.  Grouping is by KEY rather than
-- by adjacency, so token order carries nothing.
queryTest :: (Term -> Bool) -> [Term] -> Bool
queryTest atom ts =
  tokensAnd [axisTest atom [t | t <- kept, readsAs t == f] | f <- nub (map readsAs kept)]
  where kept = [t | t <- ts, narrows (readsAs t), not (vacuous t)]

-- | A term as the chips spell it: free text wears its key, a value carrying a
-- separator wears quotes.  The signs are exclusive, the scanner reading the
-- first character alone.
spell :: Term -> String
spell t@(Term _ k v) = signMark (tmSign t) <> fromMaybe "substring" k <> ":" <> quote v
  where quote x | any isSep x = "\"" <> x <> "\"" | otherwise = x

-- ** Matching, by KEY NAME and never by the declared kind

data Match = MWhole | MExact | MPrefix | MInfix deriving (Eq, Show)
matchOf :: Col -> Match
matchOf CState = MWhole   -- whole-value case-insensitive, plus the two group metas
matchOf CPrio  = MExact   -- read THROUGH org's brackets
matchOf CSched = MPrefix
matchOf CDead  = MPrefix
matchOf CTitle = MInfix
matchOf CTag   = MInfix
plannedMatch :: Match
plannedMatch = MPrefix

-- ** The comparison atom, read off the VALUE
--
-- `Match' gains no constructor: a timestamp key's value may open with a
-- COMPARISON or spell a RANGE, and both are read at the head of the VALUE on
-- the keys whose cells are ISO stamps -- which keeps matching by KEY NAME and
-- never by the declared kind.  The ground was DEAD: `isoStamp' opens with a
-- digit and never with a bracket, so before this every comparison-shaped token
-- served zero rows and its negation served every row.

-- | The keys the operator is read on, and nowhere else: on every other key `<'
-- and `>' are body text, so `title:>x' is the substring it always was.
stampedCols :: [Col]
stampedCols = [c | c <- cols, matchOf c == MPrefix]

-- | DECLARED LONGEST FIRST, so `>=' is never `>' before a literal `='.
data Cmp = CGe | CLe | CGt | CLt deriving (Eq, Show, Enum, Bounded)
cmpMark :: Cmp -> String
cmpMark CGe = ">="
cmpMark CLe = "<="
cmpMark CGt = ">"
cmpMark CLt = "<"
cmps :: [Cmp]
cmps = [minBound ..]
rangeMark :: String
rangeMark = ".."

-- | What a timestamp key's VALUE spells, read at COMPILE TIME rather than per
-- row.  `SRange' is the one form two tokens cannot say: on a MULTI-CELL key it
-- asks ONE CELL to lie inside the interval where two tokens ask the axis twice
-- -- `stampMatch' asked of each cell and ORed, and the Note carries the law.
data Stamp = SPrefix String | SCmp Cmp String | SRange String String
  deriving (Eq, Show)

-- | V as a timestamp atom, `Nothing' where a literal is owed and missing: the
-- HALF-TYPED token, which narrows nothing and joins `vacuous' rather than
-- matching none.  A literal naming NO date is still an atom and matches no row,
-- the way `state:TOD' matches none.
stampOf :: String -> Maybe Stamp
stampOf v = case listToMaybe [(o, l) | o <- cmps, Just l <- [stripPrefix (cmpMark o) v]] of
  Just (o, l) -> SCmp o <$> typed l
  Nothing     -> case breakOnStr rangeMark v of
    (lo, rest) | Just hi <- stripPrefix rangeMark rest -> SRange <$> typed lo <*> typed hi
    _noRange                                           -> SPrefix <$> typed v
  -- The long unit word is folded AT THE LITERAL, the one place a literal's end
  -- is known: a range carries two of them.
  where typed t = if null w || halfShift w then Nothing else Just w
          where w = unitFolded t

-- | THE SHIFT a date literal may carry: @BASE(+|-)N UNIT@, the base a day
-- literal, @*today*@ or NOTHING AT ALL, N a decimal run and UNIT org's own
-- charset (`unitChars').  A SHIFTED VALUE IS ONE MORE SPELLING OF A DAY LITERAL
-- and no new atom kind -- `Stamp' gains no constructor, the shift being read
-- below the forms, at the literal, and resolved by `litOf' rather than matched.
--
-- READ FROM THE END, which is what keeps ISO's own separator out of the sign's
-- reach: @2026-08-03@ ends in a digit where a shift ends in a unit letter.
shiftIn :: String -> Maybe (String, Integer, Char)
shiftIn l = case reverse l of
  unit : back | unit `elem` unitChars
              , (ds, signed) <- span isDigit back
              , not (null ds)
              , mark : base <- signed
              , Just way <- signOf mark
              -> Just (reverse base, shiftWay way * read (reverse ds), unit)
  _noShift    -> Nothing

-- | ORG'S OWN SHIFT CHARSET, the units a query moves a day by.
unitChars :: String
unitChars = "dwmy"

-- | HOW FAR A SIGN CARRIES a shifted day.  THE SHIFT'S SIGN IS THE VALUE'S AND
-- NEVER THE TOKEN'S: the scanner reads a token's sign off its FIRST CHARACTER
-- and stops there, so @+scheduled:+30d@ wears one of each and neither reader
-- sees the other's -- one charset (`signOf'), two readers.
shiftWay :: Sign -> Integer
shiftWay Unsigned = 0
shiftWay Add      = 1
shiftWay Neg      = -1

-- | Does L END MID-SHIFT -- a @+@ with nothing but digits behind it?  THE PLUS
-- FAMILY ALONE, since @+@ appears in no date a cell carries where @-@ is ISO's
-- own separator: a rule reading the incomplete minus would read @2026-08-03@ as
-- @2026-08@ moved @03@ of no unit.  An incomplete minus stays the literal it
-- was, so @*today*-7@ matches no row rather than narrowing none.
halfShift :: String -> Bool
halfShift l = case dropWhile isDigit (reverse l) of
  mark : _base -> signOf mark == Just Add
  []           -> False

-- | THE LONG UNIT WORDS the QUOTED value form admits, folded onto org's letter,
-- LONGEST FIRST so @days@ is read before @day@.  The quoted form is the one
-- that may carry spaces (@tag:"two words"@), so it is the one that may spell a
-- unit out: @scheduled:"\<= *today* + 30 days"@.
unitWords :: [(String, Char)]
unitWords = [ (stem <> plural, letter)
            | (stem, letter) <- [("day", 'd'), ("week", 'w'), ("month", 'm'), ("year", 'y')]
            , plural         <- ["s", ""] ]

-- | LITERAL L's long unit word cut to org's letter, AND ONLY WHERE A SHIFT
-- COMES OUT OF IT: @today@ ends in a unit word and is the starred word's
-- near-miss, never a @to@ moved one day.
unitFolded :: String -> String
unitFolded l = fromMaybe l (listToMaybe
  [ short | (word, letter) <- unitWords
          , Just base      <- [stripSuffixStr word l]
          , let short = base <> [letter]
          , isJust (shiftIn short) ])

-- | V with every space dropped BUT THE ONE BETWEEN TWO DIGITS, which is the
-- timed stamp's own: @2026-08-01 09:30@ keeps its space where @\<= 2026-08-01@
-- loses one it never meant.  ONE PARSER, TWO SPELLINGS -- the quoted form folds
-- onto the space-free one ABOVE every form read.
unspaced :: String -> String
unspaced (a : ' ' : b : rest) | isDigit a, isDigit b = a : ' ' : unspaced (b : rest)
                              | otherwise            = unspaced (a : b : rest)
unspaced (' ' : rest)                                = unspaced rest
unspaced (c : rest)                                  = c : unspaced rest
unspaced []                                          = []

-- | THE CALENDAR a date value resolves against: the request's OWN day, read
-- once per request (`FilterEnv.feToday') and never off a row, and the MOVE a
-- shift makes.  BOTH ARE GIVEN TO THE MODEL rather than spelled in it -- this
-- file has no clock and no calendar -- and the real pair is the request's day
-- beside `Data.Org.Types.addUnit' spelled by `Glance.Query.isoDay'
-- (`shiftResolution').
data Calendar = Calendar
  { calToday :: Maybe String
    -- ^ `Nothing' where no clock was read, and no day word then names a date.
  , calMove  :: Integer -> Char -> String -> Maybe String
    -- ^ a day moved N of UNIT and spelled back; `Nothing' where the day is no
    -- day (a month, a timed stamp) or the unit no unit.
  }

-- | No clock and no calendar, which is `emptyEnv': the starred word names no
-- day and neither does a shift, with or without a base.
noCalendar :: Calendar
noCalendar = Calendar Nothing (\_n _unit _day -> Nothing)

-- | THE DAY WORDS, each with how far off the request's own day it stands: the
-- spellings read off the CLOCK rather than off ISO's digits.  @today@ and
-- @tomorrow@ are the canonical pair, offered and displayed; @*today*@ is
-- @today@'s OLD spelling, READ AND NEVER OFFERED so a stored view and a typed
-- habit survive the rename.  ONE ROSTER for the filter's literals and the
-- planning wall's dates alike (`Glance.Query.dayWords', one base reader
-- `dayNamed'), so a word one surface takes is a word the other takes.
dayWords :: [(String, Integer)]
dayWords = [("today", 0), ("tomorrow", 1), (metaWord MToday, 0)]

-- | Does L spell a day WORD rather than a date?  The clock words alone go
-- through `dayWordIn'; every other literal is left byte for byte what it was,
-- so @2026-08@ stays the month prefix it always was.  THE PREDICATE IS OWED
-- SEPARATELY from the resolution: with no clock read a day word still IS one,
-- and answering no day rather than the bytes back is what leaves @today@
-- matching no row.
namesDay :: String -> Bool
namesDay l = isJust (lookup l dayWords)

-- | The DAY W names against the calendar's own; `Nothing' where W is no day
-- word, and `Nothing' where no clock was read, which is what makes a clock word
-- name no date under `noCalendar'.
dayWordIn :: Calendar -> String -> Maybe String
dayWordIn cal w = do
  n   <- lookup w dayWords
  day <- calToday cal
  if n == 0 then Just day else calMove cal n 'd' day

-- | A DAY WORD is a DATE VALUE: it stands wherever a literal stands and
-- resolves off the request's OWN day.  A SHIFT RESOLVES THROUGH THIS SAME DOOR
-- and at the same altitude -- its base's day moved, at COMPILE, to a plain day
-- literal -- so every law below reads one kind of literal and gains no case
-- (`shiftResolution').
litOf :: Calendar -> String -> Maybe String
litOf cal l = case shiftIn l of
  Just (base, n, unit) -> calMove cal n unit =<< dayIn cal base
  Nothing | namesDay l -> dayWordIn cal l
          | otherwise  -> Just l

-- | The DAY a shift's BASE names.  A DAY WORD and THE EMPTY BASE are both read
-- off the clock: THE BARE SHIFT IS TODAY-RELATIVE, decided off the planning
-- grammar's own precedent, which reads a bare @+3d@ that way (`set-planning''s
-- date, docs/commands.md), consistency being the tiebreaker.  Any other base is
-- the day it SPELLS, and one spelling no day is refused by the move -- which is
-- where the calendar is -- so the whole value then names none and matches no
-- row, the way @state:TOD@ matches none.
dayIn :: Calendar -> String -> Maybe String
dayIn cal base | null base     = calToday cal
               | namesDay base = dayWordIn cal base
               | otherwise     = Just base

-- | A literal BYTE ORDER may be asked about owes an opening digit: the prefix
-- reading is total over any text where `<banana' would serve every ISO cell
-- there is.  So the guard sits on the COMPARED forms alone, and the bare form
-- stays byte for byte the prefix arm it was.  A RESOLVED SHIFT PASSES IT by
-- construction, the one formatter opening every day it writes with a digit.
dateOf :: Calendar -> String -> Maybe String
dateOf cal l = do
  d <- litOf cal l
  c <- listToMaybe d
  if isDigit c then Just d else Nothing

-- | S over ONE cell, `Nothing' being the empty one.  THE EMPTY CELL SITS
-- OUTSIDE EVERY COMPARISON AND EVERY RANGE -- `"" < "2026-09"' holds in byte
-- order, so an unguarded `<' would serve every undated row -- and NEGATION IS
-- NO MIRROR because of it: `-k:<D' serves the undated rows and `k:>=D' does
-- not, so no surface may rewrite one into the other.
stampMatch :: Calendar -> Stamp -> Maybe String -> Bool
stampMatch _   _              Nothing  = False
stampMatch cal (SPrefix l)    (Just c) = maybe False (`isPrefixOf` c) (litOf cal l)
stampMatch cal (SCmp o l)     (Just c) = maybe False (\d -> cmpTest o d c) (dateOf cal l)
stampMatch cal (SRange lo hi) (Just c) = case (dateOf cal lo, dateOf cal hi) of
  (Just a, Just b) -> cmpTest CGe a c && cmpTest CLe b c
  _noDate          -> False

-- | THE GRANULARITY LAW, one equation per constructor: `<' and `>=' cut at the
-- literal's FIRST instant, `<=' and `>' at its LAST.  The last instant is
-- spelled as "everything the prefix reaches", which is the prefix test the bare
-- form already runs, so NO DATE ARITHMETIC is owed -- and `k:D' is exactly
-- `k:>=D k:<=D' on a single-cell key.
cmpTest :: Cmp -> String -> String -> Bool
cmpTest CLt d c = c < d
cmpTest CGe d c = c >= d
cmpTest CLe d c = c < d || d `isPrefixOf` c
cmpTest CGt d c = c > d && not (d `isPrefixOf` c)

-- | A REFERENCE IS A LINK WITH A KIND, and the kind is the EDGE's rather than
-- either row's: org-glance writes it after the id as `?kind=SLUG'.  Nothing is
-- a plain mention.
data RefVia = ViaRow | ViaOrgId deriving (Eq, Show)
data Ref = Ref { refTarget :: String, refKind :: Maybe String, refVia :: RefVia }
  deriving (Eq, Show)

-- | The reference protocols, EACH BOUND TO ITS NAMESPACE -- a prefix cannot
--   land without declaring where it resolves.  `id:' is org-id's and names
--   the `:ID:' property; ORG_GLANCE_ID never resolves it.
refVias :: [(String, RefVia)]
refVias = [ ("glance:", ViaRow), ("org-glance-material:", ViaRow)
          , ("org-glance-visit:", ViaRow), ("org-glance-open:", ViaRow)
          , ("id:", ViaOrgId) ]

-- | A ROW AS BOTH ENDS OF AN EDGE: the id it is known by, the NAMES it answers
-- to and the references its subtree makes.  Each name is bound to the namespace
-- it answers in — SPELLINGS (`ORG_GLANCE_ID' and title, what `[[Title]]' and
-- `[[*Title]]' resolve against) answer `ViaRow', and the `:ID:' property alone
-- answers `ViaOrgId', `id:' being org-id's protocol and `ORG_GLANCE_ID' never
-- resolving it.  ONE LIST FOR BOTH DIRECTIONS, so `ref:' and `from:' cannot
-- drift into two namespace rules.
data Node = Node { nodeId :: RowId, nodeNames :: [(RefVia, String)], nodeLinks :: [Ref] }
  deriving (Eq, Show)

node :: RowId -> [String] -> Maybe String -> [Ref] -> Node
node i spelling orgId =
  Node i ([(ViaRow, s) | s <- spelling] <> [(ViaOrgId, o) | Just o <- [orgId]])

-- | Does L name N?  The link's own namespace decides, `nodeNames' spelling both.
namesRow :: Node -> Ref -> Bool
namesRow n l = (refVia l, refTarget l) `elem` nodeNames n

-- | Does an edge carrying E answer a token asking for KIND?  `Nothing' IS THE
-- KIND-BLIND READING and every edge answers it, which is what keeps a bare
-- `ref:' the test it always was.  BOTH SIDES ARRIVE SLUGGED — the edge's kind
-- by `refTargetOf', the token's by `anchorIn' — so this compares canon to canon.
carriesKind :: Maybe String -> Maybe String -> Bool
carriesKind Nothing  _ = True
carriesKind (Just k) e = e == Just k

-- | N's references narrowed to the ones carrying KIND.
refsCarrying :: Maybe String -> Node -> [Ref]
refsCarrying kind n = [ l | l <- nodeLinks n, carriesKind kind (refKind l) ]

-- | @ref:T?kind=K@ over a target the store resolved: no row claiming the id
-- matches nothing, and a row is never its own reference.  ROW is the candidate
-- and T the anchor, so the CANDIDATE's own references are walked and matched
-- against the ANCHOR's names.
refTest :: Maybe Node -> Maybe String -> Node -> Bool
refTest Nothing  _    _   = False
refTest (Just t) kind row = nodeId row /= nodeId t && any (namesRow t) (refsCarrying kind row)

-- | @from:T?kind=K@ — THE REVERSE, and the same edge read from its other end:
-- the ANCHOR's references are walked and matched against the CANDIDATE's names.
-- Every law `refTest' carries rides along unchanged — an unresolvable anchor
-- serves nothing, a row is not its own target, the kind narrows the edge — and
-- the end that varies is the whole of what differs.  THE DUALITY that ties the
-- two: `from:T' serves R exactly where `ref:R' serves T.
fromTest :: Maybe Node -> Maybe String -> Node -> Bool
fromTest Nothing  _    _   = False
fromTest (Just t) kind row = nodeId row /= nodeId t && any (namesRow row) (refsCarrying kind t)

-- | @ref:*any*@ and @from:*any*@ — THE UNION OVER THE ANCHOR SLOT, spelled as
-- the union: over the store's ROWS, the starred word serves exactly the rows
-- some named anchor serves.  So a self-link alone answers neither (a row is not
-- its own reference at either end) and a link naming no row answers neither (an
-- unresolvable anchor serves nothing, and this is that law's union).
anyTest :: (Maybe Node -> Maybe String -> Node -> Bool)
        -> [Node] -> Maybe String -> Node -> Bool
anyTest test store kind row = any (\t -> test (Just t) kind row) store

-- | A `ref:'\/`from:' VALUE as the ANCHOR it names and the KIND it tests for.
-- THE CUT IS THE LINK TARGET'S OWN — at the first `?', the peer's `kind=' key
-- read behind it — AND IS TAKEN ONLY WHERE A KIND COMES OUT OF IT: a link's
-- target is the peer's URL, whose `?' always opens a query, where a ROW ID is
-- no URL.  So a `?' declaring no kind stays in the id, a title's own question
-- mark is text, and every value that resolved before resolves to the same row.
anchorIn :: String -> (String, Maybe String)
anchorIn v = case kindIn (drop 1 query) of
  Nothing   -> (v, Nothing)
  Just kind -> (row, Just kind)
  where (row, query) = breakOnStr "?" v

-- | The `kind' a `?' query declares; an EMPTY one and a whitespace-only one
-- declare none, and only the peer's own key is read — anything else it writes
-- is not a kind.
kindIn :: String -> Maybe String
kindIn query = listToMaybe [ slug | part <- splitOnStr "&" query
                                  , Just v <- [stripPrefix "kind=" part]
                                  , let slug = kindSlug v, not (null slug) ]

-- | A kind CANONICALIZED, and the rule is the PEER's: downcased, trimmed, runs
-- of whitespace folded to one `-' (org-glance's `org-glance--kind-slug').  It
-- slugs on encode AND on read, so a hand-typed `Roasted By' and a written
-- `roasted-by' are ONE kind.
kindSlug :: String -> String
kindSlug = intercalate "-" . words . qFold

-- | `priorityLetter': org's brackets off, folded — the MATCHER's rule and the
-- priority column's sort key.
qLetter :: String -> String
qLetter v = qFold (fromMaybe v (unbracket v))
  where unbracket x | "[#" `isPrefixOf` x, "]" `isSuffixOf` x, length x >= 3
                        = Just (drop 2 (take (length x - 1) x))
                    | otherwise = Nothing

-- ** The starred family, and it is total

data Meta = MActive | MInactive | MEmpty | MNone | MToday | MAny
  deriving (Eq, Show, Enum, Bounded)
metas :: [Meta]
metas = [minBound .. maxBound]
starred :: String -> String
starred w = "*" <> w <> "*"
metaWord :: Meta -> String
metaWord MActive   = starred "active"
metaWord MInactive = starred "inactive"
metaWord MEmpty    = starred "empty"
metaWord MNone     = starred "none"
metaWord MToday    = starred "today"     -- READ-COMPAT: `today' is the word now
metaWord MAny      = starred "any"       -- an ANCHOR, and the union over that slot
-- | The stars read backwards; a bare word is never a meta and @**@ is no word.
metaOf :: String -> Maybe String
metaOf v | "*" `isPrefixOf` v, "*" `isSuffixOf` v, length v > 2 = Just (drop 1 (init v))
         | otherwise = Nothing
isMeta :: String -> Bool
isMeta v = v `elem` map metaWord metas

data MetaHome = EveryCell | StateCell | OrderToken | DateValue | RefAnchor
  deriving (Eq, Show)
-- | Where each meta is answered.
metaHome :: Meta -> MetaHome
metaHome MEmpty    = EveryCell    -- every column key, and `planned'
metaHome MActive   = StateCell
metaHome MInactive = StateCell
metaHome MNone     = OrderToken   -- the ORDER token's own word, and it reads no cell
-- The one meta that is a VALUE rather than a predicate: it names no cell and
-- reads no row, standing where a date literal stands ('stampOf', 'litOf').  It
-- is the OLD spelling of the bare `today' (`dayWords') and stays on the roster,
-- every starred word the code spells being on it.
metaHome MToday    = DateValue
-- A PREDICATE that reads no cell either: it stands where a `ref:'/`from:' row
-- id stands ('anchorIn') and is the UNION over that slot ('anyTest').
metaHome MAny      = RefAnchor

data StateOf = SActive | SInactive | SNone deriving (Eq, Show, Enum, Bounded)
states :: [StateOf]
states = [minBound .. maxBound]
-- | @*active*@ is the file's active keywords PLUS the empty cell — a stateless
-- entry is live work — where @*inactive*@ is stated keywords alone.
groupTest :: Meta -> StateOf -> Bool
groupTest MActive   s = s /= SInactive
groupTest MInactive s = s == SInactive
groupTest MEmpty    s = s == SNone
groupTest MNone     _ = False
groupTest MToday    _ = False
groupTest MAny      _ = False
-- | The state column's meta VALUES, beside its badges: filter vocabulary, no cell.
stateValues :: [String]
stateValues = map metaWord [MActive, MInactive]

-- | Does Q name the archive through the @tag@ column?  The PLAIN WORD alone --
-- `tag:archive'; any spelling of the token, alternatives read too, both
-- signs and `+'.  The starred form is the generic whole-tag match and
-- names nothing.
namesArchive :: String -> Bool
namesArchive = any named . parseQ
  where named (Term _ k v) = k == Just "tag" && "archive" `elem` alts (qFold v)
-- | What @\/headlines@ lays under a query that does not name the archive:
-- the WHOLE tag, org's ARCHIVE alone -- `archived' in the code.
archiveExclusion :: Term
archiveExclusion = parse1 ("-tag:" <> starred "archive")

qnSocket :: Note
qnSocket = Note "The socket is not filtered, so an unfiltered client splices in an \
                \archived row /headlines would not have served." [Unguarded]

-- | The law `Calendar' is handed instead of spelling: named, because the date
-- block's doc comments cite it where the model has no calendar to show it in.
shiftResolution :: Note
shiftResolution =
  Note "A DATE VALUE MAY CARRY A SHIFT — BASE+N UNIT or BASE-N UNIT, the base a \
       \day literal, a DAY WORD or nothing at all, N a decimal run and UNIT \
       \org's own d/w/m/y — and IT RESOLVES AT COMPILE TO A PLAIN DAY LITERAL: \
       \the day the word names, the day it spells otherwise, moved by ORG'S OWN \
       \ARITHMETIC \
       \(addUnit), where a week is seven days and a month or a year is CLIPPED \
       \to the target month's last — Jan 31 moved one month is February's last \
       \day, and Feb 29 moved one year is the 28th.  The moved day is spelled by \
       \the ONE FORMATTER that writes the cells it is compared against (isoDay \
       \beside isoStamp), so the literal and the cell cannot drift into two \
       \shapes of one day.  AFTER THE RESOLUTION EVERY LAW ABOVE APPLIES \
       \UNTOUCHED — the granularity cuts, the empty cell outside, negation no \
       \mirror, both range ends (today..today+30d is the 30-day agenda in \
       \one token), the alternatives and the signs — a shifted value being one \
       \more SPELLING of a day literal and never a new atom kind.  With no clock \
       \behind it a day word names no day, a shift behind it or none; a base naming \
       \no day (a month, a timed stamp) leaves the value naming none, and it \
       \then matches no row the way state:TOD matches none." [Test]

-- ** The order

data SortDir = Asc | Desc deriving (Eq, Show)
-- | The EMPTY chain is walk order and no @sort@ field on the wire, one answer
-- for both.
type SortChain = [(Col, SortDir)]
-- | What a query naming no sort key opens on and is served in; `declaredSort'
-- spells the EFFECTIVE chain onto the wire and one arranger reads it.
defaultSortChain :: SortChain
defaultSortChain = [(CState, Asc), (CTitle, Asc), (CDead, Asc), (CSched, Asc)]

data By = ByPalette | ByLetter | ByFolded deriving (Eq, Show)
by :: Col -> By
by CState = ByPalette
by CPrio  = ByLetter
by CTitle = ByFolded
by CSched = ByFolded
by CDead  = ByFolded
by CTag   = ByFolded

-- | Empty cells last, OUTSIDE the key's direction.
cmpBy :: Ord a => SortDir -> Maybe a -> Maybe a -> Ordering
cmpBy _    Nothing  Nothing  = EQ
cmpBy _    Nothing  (Just _) = GT
cmpBy _    (Just _) Nothing  = LT
cmpBy Asc  (Just x) (Just y) = compare x y
cmpBy Desc (Just x) (Just y) = compare y x
-- | A badge column orders by palette position; an unlisted keyword ties at the back.
rank :: [Kw] -> Kw -> Int
rank pal k = fromMaybe (length pal) (qIdx k pal)

directions :: [(String, SortDir)]
directions = [("", Asc), ("asc", Asc), ("desc", Desc)]
-- | ONE token's columns, chained.  Sugar: a segment is read where a whole
-- token's value is, so no rule below knows which spelling it came from.
arrow :: String
arrow = "->"

data SortSeg = Silent | SNoneSeg | SCol Col SortDir deriving (Eq, Show)
-- | Each refuses the WHOLE request, naming the token as the reader wrote it.
data QueryRefusal = RNeg | RAdd | RAlt | RUnknown | RDirection | RNoneDir | RCompanion
  deriving (Eq, Show)

-- | The third reader beside `sortChainIn' and `columnNamesIn': `pageParams'
-- refuses an added view token, Q being where the other two are read too.
viewAddedIn :: String -> Either QueryRefusal ()
viewAddedIn q | any added (parseQ q) = Left RAdd
              | otherwise            = Right ()
  where added t = tmSign t == Add && readsAs t == FView VView

readSeg :: String -> Either QueryRefusal SortSeg
readSeg seg
  | '|' `elem` seg      = Left RAlt
  | null col            = Right Silent            -- @sort:@, half typed
  | col == metaWord MNone = if null rest then Right SNoneSeg else Left RNoneDir
  | otherwise = case lookup col [(cKey x, cCol x) | x <- viewColumns] of
      Nothing -> Left RUnknown
      Just c  -> maybe (Left RDirection) (Right . SCol c) (lookup dir directions)
  where (col, rest) = break (== ':') seg
        dir         = qFold (drop 1 rest)

-- | The chain Q states: the default where it names no sort key, its own keys in
-- written order where it names any, the EMPTY chain for the meta.
sortChainIn :: String -> Either QueryRefusal SortChain
sortChainIn q = case [t | t@(Term _ (Just "sort") _) <- parseQ q] of
  [] -> Right defaultSortChain
  ts -> do
    named <- concat <$> mapM segsOf ts
    let ordering = filter (/= Silent) named
    if SNoneSeg `elem` ordering
      then if length ordering > 1 then Left RCompanion else Right []
      else Right (foldl' extend [] [(c, d) | SCol c d <- ordering])
  where
    segsOf t@(Term _ _ v) = case tmSign t of
      Neg      -> Left RNeg
      Add      -> Left RAdd
      Unsigned -> mapM readSeg (splitOnStr arrow v)
    extend ks (c, d) | any ((== c) . fst) ks = ks
                     | otherwise             = ks <> [(c, d)]

-- ** The column set

-- | The names Q shows, in written order; 'Nothing' leaves the default view.
columnNamesIn :: String -> Either QueryRefusal (Maybe [String])
columnNamesIn q = case [t | t@(Term _ (Just "columns") _) <- parseQ q] of
  [] -> Right Nothing
  ts -> do
    named <- concat <$> mapM namesOf ts
    pure (case foldl' add [] named of { [] -> Nothing ; ns -> Just ns })
  where
    namesOf t@(Term _ _ v) = case tmSign t of
      Neg -> Left RNeg
      Add -> Left RAdd
      Unsigned | '|' `elem` v -> Left RAlt
               | otherwise    -> Right (filter (not . null) (splitOnStr "," v))
    add ns n | any ((== qFold n) . qFold) ns = ns
             | otherwise                     = ns <> [n]

data Pick = Builtin Col | Custom String String deriving (Eq, Show)  -- folded key, header as written
-- | A name resolves case-insensitively against the default view's KEYS and
-- HEADERS alike; anything else is a custom column.  THE MINIMAL SET IS TITLE.
resolveColumns :: [String] -> [Pick]
resolveColumns names = withTitle (map pick names)
  where
    pick n = maybe (Custom (qFold n) n) Builtin (lookup (qFold n) builtins)
    builtins = concat [ [(qFold (cKey x), cCol x), (qFold (cHead x), cCol x)]
                      | x <- viewColumns ]
    withTitle ps | Builtin CTitle `elem` ps = ps
                 | otherwise                = Builtin CTitle : ps

-- | Extras ride the KEY, so a picked column keeps them wherever it lands.
extrasOf :: Pick -> [Extra]
extrasOf (Builtin c)  = extras c
extrasOf (Custom _ _) = []
-- | Sorting stays the BUILTIN columns': a custom name is no chain key.
sortableOf :: Pick -> Bool
sortableOf (Builtin _)  = True
sortableOf (Custom _ _) = False
kindOfPick :: Pick -> ColKind
kindOfPick (Builtin c)  = kindOf c
kindOfPick (Custom _ _) = KText

data CustomSrc = PlanClosed | FromDrawer deriving (Eq, Show)
-- | Where a custom cell is read out of the row's own subtree.
customSrc :: String -> CustomSrc
customSrc n | qFold n == "closed" = PlanClosed
            | otherwise           = FromDrawer

-- ** Parity: the enumerated gaps, each with a direction

data Gap = SortRefusals | PriorityFold | MultiColumn | DateNess | RefKey | StateGroups
  deriving (Eq, Show, Enum, Bounded)
-- | Which side answers with FEWER rows; 'Neither' is undecided per page.
data Narrower = Producer | Renderer | Neither deriving (Eq, Show, Enum, Bounded)

gaps :: [(Gap, Narrower)]
gaps =
  [ (SortRefusals, Producer)   -- it refuses; the renderer drops the key
  , (PriorityFold, Renderer)   -- `tokenTest' does not fold, so `priority:A' finds nothing there
  , (MultiColumn,  Neither)    -- declared here by NAME, sampled there
  , (DateNess,     Neither)    -- two hardcoded keys here, sampled date-shape there; the comparison forms answer alike on both
    -- THE WHOLE REFERENCE AXIS under one row: `ref', `from', the `?kind=' cut
    -- and `*any*' are all undecidable off a page, the edge map being the
    -- store's, so each reads there as the free text it always was.
  , (RefKey,       Renderer)
  , (StateGroups,  Renderer)   -- literal badge text there, but for @*active*@'s empty term
  ]
-- | The renderer's sampling for a multi-valued column: cells read, tag-shaped
-- cells needed, contrary cells tolerated.
sampling :: (Int, Int, Int)
sampling = (40, 2, 0)

-- ** Helpers

qFold :: String -> String
qFold = map toLower
qIdx :: Eq a => a -> [a] -> Maybe Int
qIdx x xs = listToMaybe [i | (i, y) <- zip [0 ..] xs, y == x]
splitOnStr :: String -> String -> [String]
splitOnStr sep = go ""
  where go acc [] = [reverse acc]
        go acc s@(c : cs) | sep `isPrefixOf` s = reverse acc : go "" (drop (length sep) s)
                          | otherwise          = go (c : acc) cs
-- | S cut at the FIRST SEP: what stands before it, and the rest with SEP still
-- on the front.  `Data.Text.breakOn', which the matcher reads a range with.
breakOnStr :: String -> String -> (String, String)
breakOnStr sep = go ""
  where go acc s | sep `isPrefixOf` s = (reverse acc, s)
        go acc (c : cs)               = go (c : acc) cs
        go acc []                     = (reverse acc, "")
-- | S with SUF cut off its end, or 'Nothing' where S does not end in one.
-- `Data.Text.stripSuffix', which the matcher folds a long unit word with.
stripSuffixStr :: String -> String -> Maybe String
stripSuffixStr suf s = reverse <$> stripPrefix (reverse suf) (reverse s)

-- ** Notes

queryNotes :: [Note]
queryNotes =
  [ qnSocket
  , Note "Nothing versions the agreement with table-view.js — no handshake, no \
         \schema version — so every gap above is silent by construction and both \
         \sides are kept term for term by hand." [Unguarded]
  , Note "The tripwire fires only where the server returned zero, drops the key, \
         \tests the value against the whole joined row text, consults column keys \
         \alone, reports a suspicion and corrects nothing; its baseline is a \
         \remembered unfiltered paint, armed once per page." [Test]
  , Note "hrSearch is the view cells lowercased and \\x1f-joined at load, a mirror \
         \of the renderer's displayText: [[T][D]] shows D, [[T]] and [[T][]] show \
         \T, an unclosed link is left as written, control runs collapse to one \
         \space." [Test]
  , Note "The tags CELL sorts case-folded and stably; the file, hrTags and /tags \
         \keep the author's and first-seen orders, since classify is first-wins \
         \over a row's tags and whichKeys is order-dependent." [Test]
  , Note "Filtering runs before paging and a page is cut out of the EFFECTIVE \
         \chain's order, so X-Glance-Total is the match count and page two is the \
         \rows the table would show after page one." [Test]
  , Note "The badge palette a view declares is the STORE's, never the page's: \
         \deriving it from a page would move the badge list every time the page \
         \did." [Test]
  , Note "sortBy is stable, so rows equal on every key keep walk order, and text \
         \compares case-folded — the nearest this side gets to localeCompare." [Test]
  , Note "^ composes onto the chain IN FORCE and writes it into the applied query \
         \as one arrow-form sort: token with :asc unwritten, so this page \
         \remembers no order and DEL takes the chip whole." [Test]
  , Note "Under two dated rows the renderer finds no date column, so scheduled: \
         \substring-matches there and planned reads no columns at all: the \
         \predicate is term for term and the column set under it is not." [Unguarded]
  , Note "Each badge names its group, active or inactive: palette order cannot say \
         \where a #+TODO: bar fell and the hues are no contract." [Test]
  , Note "The exclusion is asked in two halves, each once — whether the TREE \
         \carries the tag (storeTags) and whether the QUERY named it \
         \(namesArchive) — and X-Glance-Archived counts what was withheld, zero \
         \whenever the query named it." [Test]
  , Note "A reference is matched against the candidate's hrLinks IN ITS OWN \
         \NAMESPACE: refSpellings — ORG_GLANCE_ID plus title, what [[Title]] and \
         \[[*Title]] resolve against — answers ViaRow, and the target's `:ID:' \
         \property alone answers ViaOrgId; FilterEnv carries the store for this key \
         \and carries nothing else, and no locally-filtered path applies one." [Test]
  , Note "A KIND RIDES THE EDGE, not the row: the peer writes `?kind=SLUG' after \
         \the id, so `refTargetOf' cuts the row at the first `?' and keeps the kind \
         \BESIDE it.  A title's own `?' is text and stays, the strip being guarded \
         \to the protocol branch." [Test]
  , Note "`from:ID' IS `ref:ID' READ FROM THE OTHER END: `ref:' serves the rows \
         \whose subtree points AT the row the id names, `from:' the rows that row \
         \POINTS AT — its own links resolved through the same two namespaces, \
         \ViaRow against every row's spellings and ViaOrgId against the `:ID:' \
         \property.  Every law `ref:' carries rides along: an id no row claims \
         \serves nothing and does NOT 400, a row is never its own target at either \
         \end (the materialize footer's self-link included), the value keeps its \
         \case alone among the predicates, and the alternatives OR.  THE TWO ARE \
         \TWO AXES rather than one, being two relations: no row's answer to one \
         \decides its answer to the other, so they AND the way `tag' and `state' \
         \do, `+ref:' widens the ref axis alone, and the grouping being by KEY, \
         \token order carries nothing across them.  THE DUALITY is the whole of \
         \what ties them — `from:T' serves R exactly where `ref:R' serves T." [Test]
  , Note "A KIND TEST RHYMES WITH THE EDGE'S OWN FILE SPELLING: the peer writes \
         \`glance:ID?kind=SLUG' and the token spells `ref:ID?kind=SLUG' and \
         \`from:ID?kind=SLUG', each narrowing to the edges CARRYING that kind.  ONE \
         \READING under both, the cut at the first `?' with the peer's `kind=' key \
         \behind it, and ONE SLUG under both, so a hand-typed `Blocked By' and a \
         \written `blocked-by' are one kind.  THE BARE FORMS STAY KIND-BLIND, \
         \today's law untouched.  THE CUT IS TAKEN ONLY WHERE A KIND COMES OUT OF \
         \IT, `unitFolded''s own discipline: a link's target is the peer's URL and \
         \its `?' always opens a query, where a ROW ID is no URL — so a `?' \
         \declaring no kind stays in the id and a title's question mark is text." [Test]
  , Note "`ref:*any*' AND `from:*any*' ARE THE UNION OVER THE ANCHOR SLOT: each \
         \serves exactly the rows some named anchor on its own key serves, so a \
         \self-link alone answers neither and a link naming no row answers neither, \
         \the unresolvable anchor's law being what this is the union of.  THE WORD \
         \JOINS THE ROSTER rather than being read privately by two keys, on \
         \`*today*''s own precedent: a starred word standing where a VALUE stands \
         \took a `MetaHome' of its own with it, and a word the code spells that \
         \`metas' does not list would falsify the roster's own law — every starred \
         \word the code spells is in it — and leave the census blind to it.  The \
         \value is NOT folded on these keys, so `ref:*ANY*' names no row and \
         \matches none, exactly as `ref:ALPHA' does." [Test]
  , Note "NEITHER DIRECTION WANTS AN INDEX: each fixes its anchor at COMPILE and \
         \walks one small link list per row — `ref:' the row's own against the \
         \anchor's names, `from:' the anchor's own against the row's names — which \
         \is one cost with the ends swapped.  THE STARRED ANCHOR fixes neither end \
         \and is the one reading that does: a name to the rows CLAIMING it, and a \
         \name to the edges NAMING it.  Both maps are built LAZILY inside the \
         \per-request `storeEnv', off the SAME id-resolved row list the filter runs \
         \over, so a query naming no `*any*' forces neither.  AT THE STORE THEY \
         \COULD NOT BE RIGHT: an edge resolves through every file's rows and \
         \through `resolveIds', where `stTags' is per-row and per-file, so a \
         \`putFile' maintaining them would be maintaining a whole-store function \
         \one file at a time.  The two-writers law is left standing." [Test]
  , Note "THE COMPILE IS ONCE PER REQUEST AND A BINDING IS ALL THAT HOLDS IT \
         \THERE: `matchesFilter' hands back a closure, but applied inside \
         \`viewPage''s row lambda the partial application is rebuilt per row and \
         \the parse, the anchor's resolution through a LINEAR `feRef' and the \
         \fixed end's own half run per row with it.  Measured over 10452 rows, \
         \`ref:' on an anchor at the store's end cost 830 ms unbound against 11 \
         \ms bound, six times past a keystroke's budget.  NOTHING CATCHES A \
         \REGRESSION: the shape type-checks either way and no timing assertion \
         \guards it; a `Matcher' newtype the route must hold would." [Unguarded]
  , Note "THE PRICE OF THE NEW KEY, STATED: `from' resolved to no field before, so \
         \`from:X' was a FREE-TEXT needle searched in hrSearch and a row whose \
         \cells spell it matched; as a key it is a predicate and that row no longer \
         \does.  That is the closed list's own cost, paid once per key added, and \
         \the no-tree-can-take-the-key-away pin moves with the law.  THE KIND HALF \
         \COSTS NOTHING: `?kind=' inside a `ref:' value made the whole string an id \
         \no row claims, so it matched nothing and the ground was DEAD; the cut \
         \being taken only where a kind comes out of it, every other `?' in a value \
         \is left exactly where it was.  EVERY QUERY NAMING NEITHER ANSWERS BYTE \
         \FOR BYTE." [Test]
  , Note "DEDUP IS ON THE PAIR, which is the peer's own rule: two typed edges to \
         \one row are two references where two plain mentions are one.  `nub' over \
         \the whole `Ref' is that rule." [Test]
  , Note "ONE SLUG ACROSS TWO PROGRAMS: a kind is downcased, trimmed and its \
         \whitespace runs folded to one `-', which is org-glance's own \
         \`org-glance--kind-slug' applied on ITS encode and ITS read.  Read as \
         \written instead, a hand-typed `Roasted By' and a written `roasted-by' \
         \would be two kinds, forking the dedup rule and counting one vocabulary \
         \twice." [Test]
  , Note "?order= is gone and present at all is a 400 naming its replacement: a \
         \parameter silently ignored would look like a working request." [Test]
  , Note "view:NAME is a MACRO the shell expands before the fetch; it never \
         \survives into the applied query, and one reaching this side is answered \
         \with every row." [Test]
  , Note "A `+' TOKEN JOINS ITS KEY'S OWN AXIS as an alternative: within one axis \
         \the plain tokens AND and a negated one inverts inside that conjunction, \
         \the `+' tokens OR against it, and the axes still AND — `tag:work \
         \priority:[#A] +priority:[#B]' serves the work rows at A or B, and \
         \`+tag:work' alone is `tag:work'.  Grouping is by KEY rather than by \
         \adjacency, so token order carries nothing." [Test]
  , Note "An UNSIGNED OR ADDED token whose value yields no atom — `state:', \
         \`+state:', `+state:|', a lone `+' — is dropped ahead of the grouping \
         \and establishes no axis, since a half-typed token never empties the \
         \table and never floods one either: read off the formula, `+state:' on a \
         \fresh axis would be false and serve nothing, and `state:' left in BASE \
         \is a match-all that saturates the axis's OR, so `state: +state:DONE' \
         \would serve every row where it must serve the DONE rows.  The negated \
         \sign keeps its inversion law, so a lone `-' and `-state:' still empty \
         \the table." [Test]
  , Note "The three shaping keys refuse the added sign where they refuse the \
         \negated one, each a 400 naming the token: `a sort key cannot be added', \
         \`a columns key cannot be added', `a view key cannot be added' — the \
         \first two in their own readers and `view' in pageParams, above the drop \
         \that leaves a stray view token harmless.  `-view:NAME' keeps that silent \
         \drop, a query carrying no `+' meaning exactly what it means today." [Test]
  , Note "THE RENDERER CARRIES THE ADDED SIGN TERM FOR TERM, so the divergence \
         \table names no added key: its scanner takes `+' as a first-character \
         \sign beside `-', a chip spells the sign back out, the completion \
         \stages offer and restore it, the value stage behind an added token \
         \drops what its axis already carries under the column's own value \
         \reading — re-adding a carried value is the idempotent no-op and a \
         \dead offer — and its axis matcher folds the added \
         \terms as one OR against the plain conjunction — this model's own law, \
         \read locally off a page.  `+sort:' rides SortRefusals as `-sort:' \
         \does: the renderer drops the key, the producer refuses the token." [Unguarded]
  , Note "A TIMESTAMP KEY TAKES A COMPARISON IN THE VALUE POSITION — >=, <=, >, \
         \< read longest first at the value's head, or the range A..B — and the \
         \GRANULARITY LAW is that < and >= cut at the literal's FIRST instant \
         \where <= and > cut at its LAST, the last instant spelled as \
         \everything the prefix reaches: no date arithmetic anywhere, and k:D is \
         \exactly k:>=D k:<=D on a single-cell key.  The operator is read on the \
         \timestamp keys alone, per alternative, below the sign and below the \
         \axis grouping, so the per-axis law is quoted rather than amended." [Test]
  , Note "THE EMPTY CELL SITS OUTSIDE EVERY COMPARISON AND EVERY RANGE, since \
         \\"\" is below every literal in byte order and an unguarded < would serve \
         \every undated row; *empty* stays its one name.  NEGATION IS NO MIRROR \
         \because of it: -k:<D serves the undated rows and k:>=D does not, so the \
         \four operators do not pair off under the sign and no surface may \
         \rewrite one into another." [Test]
  , Note "The comparison spellings land on DEAD GROUND: isoStamp opens with a \
         \digit and never with a bracket, so before this every comparison-shaped \
         \token served exactly zero rows and its negation served every row — and \
         \the renderer agreed for its own reason, a substring search for \
         \'>=2026-09' no ISO cell carries.  A literal that does not open with a \
         \digit matches no row, and the bare prefix reading is byte for byte what \
         \it was." [Test]
  , Note "A RANGE ON A MULTI-CELL KEY IS ONE CELL INSIDE THE INTERVAL, which no \
         \pair of tokens says: planned:>=A planned:<=B asks the axis twice, so a \
         \row scheduled 2027 with a deadline of 2020 passes both tokens and lies \
         \in neither range.  That is the whole reason `..' exists; on a \
         \single-cell key A..B is exactly >=A and <=B." [Test]
  , Note "A DAY WORD IS A DATE VALUE and reads no cell: today and tomorrow are \
         \the roster (dayWords), legal bare, behind any operator, at either \
         \range end and as a shift's base.  ONE READER answers them for the \
         \filter's literals and for every date-owed field (Query.dayNamed), so a \
         \word one surface types is a word the other serves.  A word resolves \
         \ONCE PER REQUEST at filter compile off the server's local day, which \
         \is the one-clock-read law, so a query asked across midnight cannot \
         \mean two days; with no clock behind it the word names no date and \
         \matches no row.  The renderer answers the same laws off the BROWSER's \
         \local day, one machine over loopback, and the skew is accepted." [Test]
  , Note "*today* IS today's OLD SPELLING and stays a member of the starred \
         \family: READ AND NEVER OFFERED.  Every reader takes it where today \
         \stands — bare, behind an operator, at either range end, mixed with the \
         \bare word inside one range — so a stored view and a typed habit \
         \survive the rename, while completion proposes today alone and a chip \
         \shows what was typed.  The one place completion canonicalises it is a \
         \shift's base, where *today*+3 is offered as today+3d; the chip strip's \
         \own annihilation folds the two together already, twins being matched \
         \on what a value MEANS." [Test]
  , shiftResolution
  , Note "THE BARE SHIFT SHIPS AND IS TODAY-RELATIVE: `scheduled:+30d' is \
         \`scheduled:today+30d', decided off the planning grammar's own \
         \precedent — `set-planning' already reads a bare `+3d' against the \
         \server's day (docs/commands.md) — with consistency as the tiebreaker. \
         \THE TWO SIGNS ARE READ APART: the scanner takes a token's sign off its \
         \FIRST character and stops there, so in `+scheduled:+30d' the token \
         \wears the added sign and the value wears the shift's plus, and neither \
         \reader sees the other's." [Test]
  , Note "A SHIFT WITH NO UNIT BEHIND IT IS HALF-TYPED and joins that family: \
         \`today+' and `today+30' narrow nothing unsigned and empty the \
         \table negated.  THE PLUS FAMILY ALONE is read that way, since `+' \
         \appears in no date a cell carries where `-' is ISO's own separator — a \
         \rule reading the incomplete minus would read `2026-08-03' as `2026-08' \
         \moved `03' of no unit — so an incomplete minus stays the literal it \
         \was and `today-7' matches no row rather than narrowing none.  The \
         \ground the shift lands on was DEAD: a `+', and a base a shift's letter \
         \follows, prefix-match no ISO cell, so every value that composed before \
         \answers byte for byte." [Test]
  , Note "THE QUOTED VALUE FORM IS THE SPACED ONE and folds onto the compact \
         \spelling ABOVE every form read, so one parser answers both: every \
         \space goes BUT THE ONE BETWEEN TWO DIGITS, which is the timed stamp's \
         \own, and a long unit word is cut to org's letter where a shift comes \
         \out of it — `today' ends in one and IS THE DAY WORD.  `scheduled:\"<= \
         \today + 30 days\"' and `scheduled:<=today+30d' are one query, and \
         \an unquoted value carries no space at all, the scanner cutting a token \
         \on one." [Test]
  , Note "ANNIHILATION IS THE STRIP'S RULE AND NOT THE GRAMMAR'S: committing a \
         \token whose opposite-signed twin already stands removes both, the pair \
         \being the tautology the producer answers as every row.  Twins are \
         \matched on the key and value `parseQuery' resolves, so a quote that \
         \OPENS a token makes it free text and no twin; it runs on the \
         \interactive commit alone, never on the seed a producer's whole query \
         \arrives as." [Browser]
  , Note "The view tokens are one list on each side (Filter.viewKeys, tv's \
         \VIEW_KEYS); the renderer's half is chip dress and keeping them out of \
         \free text, and the shell remounts when a fetched answer's columns differ \
         \from the mounted ones." [Docs]
  , Note "A custom column's cells are read-only, so the hidden properties are not \
         \hidden there." [Test]
  , Note "THE `created' ALIAS IS SORTABLE, NOT A DEFAULT COLUMN: `aliasColumns' names \
         \`created'/`Created' over `creationProperty', absent from `viewColumns' so no \
         \default table shows it, yet `resolveColumns' picks it and `sortKeys'/`sortCell' \
         \order by it — the bracketed inactive stamp stripped to its ISO prefix, \
         \chronological like `scheduled'/`deadline', empties last." [Test]
  , Note "TestFilter's hardcoded six-cell layout list is an INDEPENDENT oracle and \
         \moves by hand, as do Filter.dateKeys and keyTest's name switch, neither \
         \positional." [Test]
  , Note "fixtures/parity/filter-query.json and sort-tokens.json run the shared \
         \half of the grammar over the browser renderer." [Browser]
  , Note "The two vocabularies once had different scopes — the store's tags here, \
         \the loaded rows' there — which is the divergence the tripwire was built \
         \for; tagVocab survives as the tags column's value domain." [Test]
  , Note "The price of the one spelling: tag: is a SUBSTRING of the cell where a \
         \tag key was whole-tag, and org writes a tags cell :web:, so the free \
         \text `web:' is inside every row carrying the tag." [Test]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "A keyless token spells its key, quoting a separator." [Browser]
  , Note "Naming the archive serves the rows an unnamed query withholds; the plain\
         \ word alone names it, and the whole-tag `*archive*' form matches only rows\
         \ the exclusion still withholds." [Unguarded]
  , Note "Every gap is named once, with a direction." [Unguarded]
  , Note "The sort refusals are the one gap where this side is stricter." [Unguarded]
  , Note "Every other gap leaves the renderer narrower or undecided." [Unguarded]
  ]
-- * Commands and writes
--
-- ONE ROUTE, @POST \/command {name, id | ids, args, digests?}@, over ONE table.
-- A command is its entry: the dispatch is a total case over 'CmdKind', so the
-- door, the walls and whether ids are owed are all read off the kind rather
-- than off the name.

data CmdName = SetState | SetPlanning | SetTitle | SetPriority | Archive | Capture
             | AddTag | RemoveTag | RenameTag | EditLink | Delete
  deriving (Eq, Ord, Show, Enum, Bounded)

data CmdKind = Splices | Makes | Moves deriving (Eq, Show)
-- ^ how bytes move.  'Makes' is the one kind owing no ids.

data Ids = IdsNone | IdsMany | IdsOne deriving (Eq, Show)
data Arity = Req | Nul | Opt deriving (Eq, Show)
-- ^ absent is a 400 naming the field | null is the CLEAR | absent is legal.
-- @.:!@ rather than @.:?@ is what tells an absent field from a null one.
data Arg = Arg String Arity deriving (Eq, Show)

data Cmd = Cmd
  { cName    :: CmdName
  , cWire    :: String
  , cKind    :: CmdKind
  , cArgs    :: [Arg]   -- ^ the entry's own request-shape guard (@csArgs@)
  , cIds     :: Ids
  , cDated   :: Bool    -- ^ resolves 'Asked'''s day before any row is touched
  , cRecords :: Bool    -- ^ its 'RowWrite' may carry a ledger line
  }

cmds :: [Cmd]
cmds =
  [ Cmd SetState    "set-state"    Splices [Arg "keyword" Nul]                                IdsMany False True
  , Cmd SetPlanning "set-planning" Splices [Arg "keyword" Req, Arg "date" Nul]                IdsMany True  False
  , Cmd SetTitle    "set-title"    Splices [Arg "title" Req]                                  IdsMany False False
  , Cmd SetPriority "set-priority" Splices [Arg "priority" Nul]                               IdsMany False False
  , Cmd Archive     "archive"      Splices []                                                 IdsMany False False
  , Cmd Capture     "capture"      Makes   captureArgs                                       IdsNone False False
  , Cmd AddTag      "add-tag"      Splices [Arg "tag" Req]                                    IdsMany False False
  , Cmd RemoveTag   "remove-tag"   Splices [Arg "tag" Req]                                    IdsMany False False
  , Cmd RenameTag   "rename-tag"   Splices [Arg "from" Req, Arg "to" Req]                     IdsMany False False
  , Cmd EditLink    "edit-link"    Splices [Arg "span" Req, Arg "target" Req, Arg "desc" Nul] IdsOne  False False
  , Cmd Delete      "delete"       Moves   []                                                 IdsMany False False
  ]

-- | @capture@'s TWO ROADS in ONE arg list: @text@ (with @fields@, through the
-- tag's template) is the older wire, @title@ and the six under it are the
-- capture sheet's own cargo.  EXACTLY ONE ROAD IS TAKEN — naming both is
-- refused rather than resolved — which is why neither opener is 'Req' and the
-- either-or is a wall rather than an arity.  A NULL is an absence at every one
-- of them: a capture makes a row, so there is nothing to clear.
captureArgs :: [Arg]
captureArgs = [ Arg "text" Opt, Arg "fields" Opt, Arg "tag" Opt ]
                <> [ Arg (cargoWire k) Opt | k <- [minBound .. maxBound] ]

-- | The widened cargo, key by key: the doc pane's standing shape, which is the
-- same header-and-body @POST \/headline@ already speaks.
data CargoKey = KTitle | KState | KPriority | KTags | KPlanning | KProperties | KBody
  deriving (Bounded, Enum, Eq, Show)

cargoWire :: CargoKey -> String
cargoWire KTitle      = "title"
cargoWire KState      = "state"
cargoWire KPriority   = "priority"
cargoWire KTags       = "tags"
cargoWire KPlanning   = "planning"
cargoWire KProperties = "properties"
cargoWire KBody       = "body"

data CargoWall = TitleReparse | StateInCycle | PriorityLetter | TagCharset
               | PlanningKey | PlanningValue | NoWall | OneTopEntry
  deriving (Eq, Show)

-- | WHAT EACH KEY MEETS, and every one of them is the wall a ROW EDIT meets:
-- the draft commits through the same sentences, so a spelling one door takes is
-- a spelling the other takes.  @properties@ has none, exactly as the commit
-- door's drawer list has none.
cargoWall :: CargoKey -> [CargoWall]
cargoWall KTitle      = [TitleReparse]
cargoWall KState      = [StateInCycle]
cargoWall KPriority   = [PriorityLetter]
cargoWall KTags       = [TagCharset]
cargoWall KPlanning   = [PlanningKey, PlanningValue]  -- THE KEY OUTRANKS THE VALUE
cargoWall KProperties = [NoWall]
cargoWall KBody       = [OneTopEntry]

commandNames :: [String]
commandNames = map cWire cmds

namesRows :: CmdKind -> Bool
namesRows Splices = True
namesRows Moves   = True
namesRows Makes   = False
rowFn :: CmdKind -> Bool                 -- ^ has a row function at all
rowFn Splices = True
rowFn Makes   = False
rowFn Moves   = False

data Door = ReplaceSpans | TrashBlob deriving (Eq, Show)
-- ^ ONE door per way bytes move; both share @noteBlob@'s two gates.
doorOf :: CmdKind -> Door
doorOf Splices = ReplaceSpans
doorOf Makes   = ReplaceSpans
doorOf Moves   = TrashBlob

spliceSites :: [String]
spliceSites = ["captureInbox", "captureBlob", "writeOne", "commit", "writeLayer"]
nudgeSites :: [String]                   -- ^ delete's is the sixth, and splices no spans
nudgeSites = spliceSites <> ["deleteRows"]

-- ** Walls
--
-- A per-id refusal, each checked on the SERVER as well as in the shell.

data Wall = NoSuchRow | StalePin | NotArchived | NotABlob deriving (Eq, Show)
walls :: CmdKind -> [Wall]
walls Makes   = []
walls Splices = [NoSuchRow, StalePin]
walls Moves   = [NoSuchRow, NotArchived, NotABlob]

data Result = Result RowId (Either Wall Digest)
resultsFor :: (RowId -> Either Wall Digest) -> [RowId] -> [Result]
resultsFor f = map (\r -> Result r (f r))
-- ^ resolved ONCE, in the order NAMED, so a caller zips the answer to the request.
resultIds :: [Result] -> [RowId]
resultIds rs = [r | Result r _ <- rs]

-- ** Whole-request refusals
--
-- 400 with nothing written, each refusing the WHOLE request; 413 outranks
-- everything.

data Refusal = Oversize | BadBody | NoSuchName | NoIds | UndeclaredKw | UnreadableDate
             | NotATag | LinkRefusal LinkWall
  deriving (Eq, Show)
refusals :: [Refusal]
refusals = [Oversize, BadBody, NoSuchName, NoIds, UndeclaredKw, UnreadableDate, NotATag]
        <> map LinkRefusal linkWalls

-- ** The write
--
-- Surgical span replacement, optimistic lock, atomic temp+rename; untouched
-- bytes stay byte-identical.  'Data.Org.Edit' is content-agnostic and spells no
-- 'TextShow'.

data Edit = Edit Span String
spanOf :: Edit -> Span
spanOf (Edit s _) = s
newtype Edits = Edits [Edit]

overlapping :: Span -> Span -> Bool
overlapping (Span a b) (Span c d) = a < d && c < b
-- ^ an edit may START where the previous one ENDED; only overlap is refused.
applyEdits :: Edits -> Maybe Edits
applyEdits es@(Edits xs)
  | any (uncurry overlapping) (pairsOf (map spanOf xs)) = Nothing
  | otherwise                                           = Just es
pairsOf :: [a] -> [(a, a)]
pairsOf xs = [(x, y) | (i, x) <- zip [(0 :: Int) ..] xs, (j, y) <- zip [0 ..] xs, i < j]

data Write = Write Path Digest Edits
-- ^ a write cannot be BUILT without its pin, so "every write is drift-locked"
-- is not a sentence.
writesFor :: [(Path, Digest, Edit)] -> [Write]
writesFor xs = mapMaybe one (nub [p | (p, _, _) <- xs])
  where one p = fmap (\d -> Write p d (Edits [e | (q, _, e) <- xs, q == p]))
                     (listToMaybe [d | (q, d, _) <- xs, q == p])
-- ^ ids group by FILE and each file is ONE call: two rows of one file MUST land
-- in one write, the second otherwise pinned to the digest the first invalidated.

-- | A row's answer is `RowWrite' and the nine plain ones are `plain', both in
-- Scan beside the ledger line they carry; `untrailed' is in Store beside the
-- recomposer.  One statement each, so the two cannot come to disagree.
rstrip :: String -> String
rstrip = reverse . dropWhile (`elem` " \t") . reverse
composers :: [String]                   -- ^ every text this repo COMPOSES for a write
composers = ["recomposedSubtree", "draftEntry", "stampedEntry", "captureEdits"]
-- ^ @draftEntry@ spells the headline and hands the two lists to
-- @recomposedSubtree@; @stampedEntry@ joins the properties a capture owes and
-- is what @blobDocument@ became when the inbox path wanted the same splice.

-- ** Span math
--
-- 'Glance.Query'''s, because @HeadlineSpans@ is @glance-internal@'s.  Each
-- command is one site when the part is there, a chain of fallbacks when it is
-- not, and a cut when the value is null.

data EditSite = OwnSpan | AfterStars | AfterKeyword | AfterPriority | PastStarsHSpace
              | TagRunEnd | TitleLineEnd | PlanningLineEnd | UnderTitleLine
              | EntryTextNoColon | GivenSpan | FileEnd
  deriving (Eq, Show)
data EditCut = TokenAndHSpaceBehind | EntryAndColon | LastEntryRunAndHSpaceBefore
             | EntryAndTrailingHSpace | EntryAndLeadingHSpace | WholeLine | FurtherEntries
  deriving (Eq, Show)
data Math = Math EditSite [EditSite] [EditCut] deriving (Eq, Show)

setStateEdits, setPriorityEdits, setTitleEdits, setPlanningEdits :: Math
setStateEdits    = Math OwnSpan [AfterStars] [TokenAndHSpaceBehind]
setPriorityEdits = Math OwnSpan [AfterKeyword, AfterStars] [TokenAndHSpaceBehind]
setTitleEdits    = Math OwnSpan [AfterPriority, AfterKeyword, PastStarsHSpace] []
setPlanningEdits = Math OwnSpan [PlanningLineEnd, UnderTitleLine]
                        [EntryAndTrailingHSpace, EntryAndLeadingHSpace, WholeLine]
addTagEdits, archiveEdits, removeTagEdits, renameTagEdits, editLinkEdits :: Math
addTagEdits    = Math TagRunEnd [TitleLineEnd] []
archiveEdits   = addTagEdits            -- ^ ONE insertion rule, not two that must agree
removeTagEdits = Math OwnSpan [] [EntryAndColon, LastEntryRunAndHSpaceBefore]
renameTagEdits = Math EntryTextNoColon [] [FurtherEntries]
editLinkEdits  = Math GivenSpan [] []   -- ^ the range @\/links@ measured and handed out
captureAt :: EditSite                   -- ^ the END of the target, a missing newline written first
captureAt = FileEnd

mathOf :: CmdName -> Maybe Math
mathOf SetState    = Just setStateEdits
mathOf SetPlanning = Just setPlanningEdits
mathOf SetTitle    = Just setTitleEdits
mathOf SetPriority = Just setPriorityEdits
mathOf Archive     = Just archiveEdits
mathOf AddTag      = Just addTagEdits
mathOf RemoveTag   = Just removeTagEdits
mathOf RenameTag   = Just renameTagEdits
mathOf EditLink    = Just editLinkEdits
mathOf Capture     = Nothing
mathOf Delete      = Nothing

costsNoEdit :: CmdName -> Bool          -- ^ answers @[]@ where the row already says it
costsNoEdit AddTag      = True
costsNoEdit RemoveTag   = True
costsNoEdit RenameTag   = True
costsNoEdit Archive     = True
costsNoEdit SetPriority = True
costsNoEdit SetPlanning = True
costsNoEdit SetState    = False
costsNoEdit SetTitle    = False
costsNoEdit EditLink    = False
costsNoEdit Capture     = False
costsNoEdit Delete      = False

-- ** The walls a value takes

titleText :: String -> Maybe String     -- ^ at least one character, ONE line
titleText t = let s = rstrip t in if null s || '\n' `elem` s then Nothing else Just s
priorityText :: String -> Maybe Char    -- ^ ONE ASCII letter, uppercased; @[#D]@ is writable
priorityText [c] | isAlpha c && c < '\128' = Just (toUpper c)
priorityText _   = Nothing
isTagChar :: Char -> Bool               -- ^ org's set plus @-@; no @*@, so no starred meta is a tag
isTagChar c = isAlpha c || isDigit c || c `elem` "_@#%-"
tagText :: String -> Maybe Tag          -- ^ what this server writes has to reparse HERE; both ends of rename-tag take it
tagText t = if not (null t) && all isTagChar t then Just (Tag t) else Nothing
orgStampForms :: [String]               -- ^ ONE renderer, the brackets the difference, the weekday COMPUTED
orgStampForms = ["<YYYY-MM-DD Day[ HH:MM]>", "[YYYY-MM-DD Day[ HH:MM]]"]

data Plan = Scheduled | Deadline | Closed deriving (Eq, Show)
data PlanWall = Composed | Reparsed deriving (Eq, Show)
-- | WHICH WALL A PLANNING VALUE MEETS.  ALL THREE ARE SET at @set-planning@ and
--   at @POST \/headline@; what differs is the READING, and the KEYWORD picks it.
--   @SCHEDULED@ and @DEADLINE@ are COMPOSED for — the whole date grammar in
--   ('DateForm'), the bytes org itself would write out — while @CLOSED@ is org's
--   own bookkeeping and is never resolved for: it REPARSES, verbatim or refused,
--   and no English is widened to it.  BOTH WRITE DOORS ASK ONE FUNCTION, so a
--   value one takes is a value the other takes; the KEYWORD is refused ahead of
--   the value, an unknown key outranking every date.  It is also what a client
--   asks whether a field OWES a date, which is why a widget over @CLOSED@ offers
--   no vocabulary and draws no ghost.
planWall :: Plan -> PlanWall
planWall Scheduled = Composed
planWall Deadline  = Composed
planWall Closed    = Reparsed

data DateForm = Bracketed | Wrapped | Today | Tomorrow | Relative | TodayMeta
              | IsoDate | English | EnglishSpan
  deriving (Eq, Show)
-- ^ @+N@ in ANY unit org spells; an ISO date takes an optional @HH:MM@;
-- @TodayMeta@ is the filter's own @*today*@ and its shift, read here too so ONE
-- FIELD SPELLS ONE GRAMMAR; the two English forms are the day-and-month phrase
-- and the interval between two.  @Wrapped@ is any of the rest INSIDE org's own
-- brackets — @[today]@, @<18 aug>@, @[today+30d]@ — read only where @Bracketed@
-- read nothing, since a value org itself reads back is org's own spelling.
verbatimDate :: DateForm -> Bool        -- ^ the rest render with the weekday COMPUTED
verbatimDate Bracketed   = True
verbatimDate Wrapped     = False
verbatimDate Today       = False
verbatimDate Tomorrow    = False
verbatimDate Relative    = False
verbatimDate TodayMeta   = False
verbatimDate IsoDate     = False
verbatimDate English     = False
verbatimDate EnglishSpan = False

stampActivity :: Maybe Bracket -> Bracket
-- ^ WHICH BRACKET A RESOLVED ANSWER WEARS.  Bare is ACTIVE — org's convention
-- for a planning value, and the origin's *"ACTIVE by default"* — while a bracket
-- the reader typed picks its own, which is the whole of what @Wrapped@ adds:
-- @[today]@ is the clock day inactive and @<today>@ is @today@'s own bytes.  An
-- interval takes ONE kind for both halves ('Ts'), so the choice is made once.
stampActivity = fromMaybe TsActive

-- *** The English a date-owed field reads
--
-- THE FIELD'S WHOLE CONTENT IS THE PHRASE: this reading fires only where the
-- write already owes a date and nothing else can be meant, never over prose.

monthTable :: [(String, Int)]
-- ^ Org's three-letter form and the full one, lower case, matched case-folded.
-- @may@ is ONE entry, its two forms coinciding; four-letter @sept@ and any form
-- carrying a full stop are outside it.  THE ONLY LANGUAGE-BEARING DATUM in the
-- grammar: a second language is a second table and a selector, and nothing else
-- in the parser moves.
monthTable =
  [ ("jan", 1),  ("january", 1),  ("feb", 2),  ("february", 2)
  , ("mar", 3),  ("march", 3),    ("apr", 4),  ("april", 4)
  , ("may", 5)
  , ("jun", 6),  ("june", 6),     ("jul", 7),  ("july", 7)
  , ("aug", 8),  ("august", 8),   ("sep", 9),  ("september", 9)
  , ("oct", 10), ("october", 10), ("nov", 11), ("november", 11)
  , ("dec", 12), ("december", 12) ]

data DateWord = MonthName Int | DayNum Int | YearNum Integer | NoWord deriving (Eq, Show)
dateWord :: String -> DateWord
-- ^ What ONE word of a phrase says.  A day is one digit or two naming 1..31 and
-- a year is FOUR DIGITS AND NEVER TWO, which is what leaves @18 aug 18@ text
-- where a fuzzy reader answers 2018.  Everything else says nothing: an ordinal
-- suffix, a full stop, a separator, and A WEEKDAY WORD, which is computed on
-- render and so never read — @Tue 18 Aug@ is text even when Tuesday is right.
dateWord w
  | Just m <- lookup (map toLower w) monthTable  = MonthName m
  | all isDigit w, length w >= 1, length w <= 2
  , d <- read w, d >= 1, d <= 31                 = DayNum d
  | all isDigit w, length w == 4                 = YearNum (read w)
  | otherwise                                    = NoWord

englishForms :: [String]
-- ^ Every arrangement the phrase takes, the separator a RUN of spaces and tabs
-- and nothing else.  A bare day and a bare month are each no date; only an
-- INTERVAL'S LEFT END may elide, and what it elides it INHERITS FROM THE RIGHT —
-- the English idiom says one month once, and inheritance is what keeps
-- @from 18 to 19 august 2027@ two days in 2027 rather than a twelve-month span.
englishForms =
  [ "D MONTH", "MONTH D", "D MONTH YYYY", "MONTH D YYYY"
  , "[from] D to D MONTH [YYYY]"
  , "[from] D MONTH [YYYY] to D MONTH [YYYY]" ]

data SpanEnds = Ordered | SameDay | Inverted deriving (Eq, Show)
spanAnswer :: SpanEnds -> String
-- ^ What an interval's two ends earn.  ONE LAW, "refuse end before start": the
-- equal case is no second clause but a COLLAPSE to the single stamp, which is
-- what makes @from 18 to 18 aug@ and @18 aug@ one answer and one fixed point.
-- Tested on the two MOMENTS, so a timed range sharing a day stays a range.
spanAnswer Ordered  = "<A>--<B>"
spanAnswer SameDay  = "<A>"
spanAnswer Inverted = "refused: the end falls before the start, and a typed year at each end is the remedy"

data YearRule = Flat | PreferFuture deriving (Eq, Show)
yearRule :: YearRule
-- ^ SETTLED: the year is the clock's, flat, so @18 aug@ typed in December means
-- that August and a typist meaning next year writes it.  Org's
-- @org-read-date-prefer-future@ is the rejected reading — a calendar-dependent
-- clause where the flat rule is statable without one.
yearRule = Flat

-- ** Capture
--
-- The ONE id-less command: it MAKES a row.  The answer is its own shape,
-- @{ok, file, digest, id}@, and @id@ is what the cursor lands on.  THE CAPTURE
-- DOC IS THE MATERIAL DOC: @GET \/capture@ answers a DRAFT in the very shape
-- @\/headline@ serves, the sheet opens over it, and @C-c C-c@ commits the
-- pane's cargo back through this one command.

data CaptureTo = ToInbox | ToBlob Tag deriving (Eq, Show)
captureInto :: Maybe Tag -> CaptureTo   -- ^ ABSENT is the config's inbox, PRESENT a blob
captureInto Nothing  = ToInbox
captureInto (Just t) = ToBlob t
captureAnswer :: [String]
captureAnswer = ["ok", "file", "digest", "id"]
capturedId :: Path -> Int -> RowId      -- ^ K is the store's rows for that FILE at the last load
capturedId = Nth

captureText :: String -> Maybe String   -- ^ the OLDER ROAD's wall, and every @fields@ answer's
captureText t = let s = rstrip (dropWhile (`elem` " \t") t)
                in if null s || '\n' `elem` s then Nothing else Just s

data CaptureRefusal = NoStore | OneHeadlineWall | NoPlaceholder | UnansweredAsk | TemplateNoHeadline
  deriving (Eq, Show)
captureOrder :: [CaptureRefusal]        -- ^ coarsest first, every one of them ahead of a byte
captureOrder = [NoStore, OneHeadlineWall, NoPlaceholder, UnansweredAsk, TemplateNoHeadline]

data CaptureDoor = DraftDoor | CommitDoor deriving (Eq, Show)
-- | WHICH DOOR EACH REFUSAL IS SPOKEN AT.  The two a TEMPLATE can be wrong in
-- are the draft door's as well, so a broken layer is named when @+@ OPENS
-- rather than after a reader has composed a whole entry over it.
refusalAt :: CaptureRefusal -> [CaptureDoor]
refusalAt NoStore            = [CommitDoor]
refusalAt OneHeadlineWall    = [CommitDoor]
refusalAt NoPlaceholder      = [DraftDoor, CommitDoor]
refusalAt UnansweredAsk      = [CommitDoor]
refusalAt TemplateNoHeadline = [DraftDoor, CommitDoor]

data Code = Code String String
captureCodes :: [Code]                  -- ^ the CONTRACT's window: the settings box completes over this
captureCodes =
  [ Code "%?"         "where point opens"
  , Code "%U"         "an inactive stamp, this request's one clock read"
  , Code "%T"         "an active stamp"
  , Code "%^{PROMPT}" "its EMPTY value: a drawer pair with none, a slot in the body"
  ]
scanCodes :: [String]                   -- ^ @templateParts@ spells the same four as a CASE
scanCodes = ["%?", "%U", "%T", "%^{PROMPT}"]

-- | @GET \/capture[?tag=NAME]@: the DRAFT.  @\/headline@'s own members off bytes
-- with no file behind them, and the three a doc that is not one yet owes.  NO
-- FILE IS CREATED; @id@ is null, @file@ empty and @digest@ @""@, which is the
-- CREATE PIN the commit that follows writes under.
captureRead :: [String]
captureRead = headlineFields <> ["point", "cycle", "tags"]

data DraftPoint = AtHeadline | AtBodyLine Int deriving (Eq, Show)
-- ^ where @%?@ stood, in the coordinates @body@ is ALREADY read in — the ones
-- @ownLines@ and a child's @line@ use — so the pane lands by a reading it makes
-- anyway.  Line 0 IS the headline, so the body form never names it.
draftPoint :: Maybe Int -> DraftPoint   -- ^ the wire is @point: <int> | null@
draftPoint Nothing  = AtHeadline
draftPoint (Just n) = AtBodyLine n

data Lent = LentTag | LentState | LentPriority | LentTags | LentPlanning
  deriving (Eq, Show)
-- | WHAT THE STANDING FILTER LENDS A DRAFT, and the gap each one fills.
-- TEMPLATE-FIRST: where the template speaks the filter is silent.  Only a fact
-- the query pins to ONE ordinary positive value is lent — never a negation,
-- never an alternation, never a meta — and a fact the draft's own walls turn
-- down is simply not inherited, so INHERITANCE NEVER REFUSES A CAPTURE.
lentInto :: Lent -> String
lentInto LentTag      = "the destination, which is also the template and the cycle"
lentInto LentState    = "the keyword, where the template spells none AND the draft's cycle declares it"
lentInto LentPriority = "the priority, where the template spells none"
lentInto LentTags     = "JOINS the run rather than filling a gap"
lentInto LentPlanning = "one settable key pinned to one day, where the line has no such entry"

data TplSrc = TplTag Tag | TplSystem | TplBare deriving (Eq, Show)
templateChain :: Tag -> [TplSrc]        -- ^ ending at Config's `bareTemplate', so every case takes one path
templateChain t = [TplTag t, TplSystem, TplBare]
headingStars :: String -> Bool          -- ^ @^\*+ @: a star run then HORIZONTAL space
headingStars s = let (st, r) = span (== '*') s in not (null st) && take 1 r `elem` [" ", "\t"]
topEntry :: String -> Bool              -- ^ the one-star wall, the WRITER's alone
topEntry s = headingStars s && takeWhile (== '*') s == "*"

creationProperty :: String              -- ^ org's INACTIVE form, server clock, column 1
creationProperty = "ORG_GLANCE_CREATION_TIME"
inboxDefault :: Path                    -- ^ the tree's ONE entry point; no layer names it
inboxDefault = "inbox.org"

-- ** Blob
--
-- org-glance's own layout.  Reading an id is a different question: an
-- @ORG_GLANCE_ID@ is an OPAQUE STRING everywhere it is read.

-- The PATH is `blobPathIn', in Walk: one path rule, one definition.
uuidGroups :: [Int]                     -- ^ @org-id-uuid@'s form: random v4, lowercase
uuidGroups = [8, 4, 4, 4, 12]
blobIdLen :: Int
blobIdLen = sum uuidGroups + length uuidGroups - 1
emptyPin :: Digest                      -- ^ NO RESERVATION: a path already holding a file DRIFTS
emptyPin = Digest ""

-- ** edit-link
--
-- The ONE command whose args name a row's own TEXT, so it names ONE row.

data LinkForm = PlainUrl | TargetOnly | Described deriving (Eq, Show)
data DescArg = DescKeep | DescClear | DescSet String deriving (Eq, Show)
normDesc :: DescArg -> DescArg          -- ^ a description that SHOWS nothing is the null respelled
normDesc (DescSet d) | null (rstrip d) = DescClear
normDesc a = a
editLinkForm :: LinkForm -> DescArg -> LinkForm
editLinkForm f a = case normDesc a of
  DescKeep  -> f
  DescSet _ -> Described
  DescClear -> case f of PlainUrl -> PlainUrl; TargetOnly -> TargetOnly; Described -> TargetOnly

data LinkWall = LinkNewline | InSubtree | EdgeToEdge | Reparses deriving (Eq, Show)
linkWalls :: [LinkWall]                 -- ^ the newline ahead of both walls; each a 400
linkWalls = [LinkNewline, InSubtree, EdgeToEdge, Reparses]

-- ** Theme and palette
--
-- ONE source: a 'Role' per theme, emitted into BOTH namespaces, so a role the
-- page and the renderer both spell has ONE value.

data Role = RBg | RFg | RSurface | RMuted | RBorder | RAccent | RSel | RPoint | RPointDim | RPointOff | RHover | RLink
          | RFrost | RCol | ROk | RWarn | RBad | RVeil | RShadow | RChipWash | RChipEdge
          | RMarkWash | RFlagWash | RColWash | RCellWash | RSortWash | RColsWash
  deriving (Eq, Ord, Show, Enum, Bounded)
roles :: [Role]
roles = [minBound .. maxBound]

pageToken :: Role -> Maybe String
pageToken RBg = Just "--g-bg"
pageToken RFg = Just "--g-fg"
pageToken RSurface = Just "--g-surface"
pageToken RMuted = Just "--g-mute"
pageToken RBorder = Just "--g-border"
pageToken RAccent = Just "--g-accent"
pageToken RSel = Just "--g-sel"
pageToken RPoint = Just "--g-point"
pageToken RPointDim = Just "--g-point-dim"
pageToken RPointOff = Just "--g-point-off"
pageToken RLink = Just "--g-link"
pageToken RCol = Just "--g-col"
pageToken RCellWash = Just "--g-cell-wash"
pageToken RFlagWash = Just "--g-flag-wash"
pageToken ROk = Just "--g-ok"
pageToken RWarn = Just "--g-warn"
pageToken RBad = Just "--g-bad"
pageToken RVeil = Just "--g-veil"
pageToken RShadow = Just "--g-shadow"
pageToken RHover = Nothing
pageToken RFrost = Nothing
pageToken RChipWash = Nothing
pageToken RChipEdge = Nothing
pageToken RMarkWash = Nothing
pageToken RColWash = Nothing
pageToken RSortWash = Nothing
pageToken RColsWash = Nothing

tableToken :: Role -> Maybe String
tableToken RBg = Just "--tv-bg"
tableToken RFg = Just "--tv-fg"
tableToken RSurface = Just "--tv-alt"
tableToken RMuted = Just "--tv-muted"
tableToken RBorder = Just "--tv-border"
tableToken RAccent = Just "--tv-accent"
tableToken RSel = Just "--tv-sel"
-- THE MARK IS THE DOCUMENT'S ALONE: the renderer grounds its cursor row.
tableToken RPoint = Nothing
tableToken RPointDim = Nothing
tableToken RPointOff = Nothing
tableToken RHover = Just "--tv-hover"
tableToken RLink = Just "--tv-link"
tableToken RFrost = Just "--tv-frost"
tableToken RBad = Just "--tv-flag"      -- ^ an error and the archive flag are one red
tableToken RCol = Just "--tv-col"
tableToken RVeil = Just "--tv-veil"
tableToken RShadow = Just "--tv-shadow"
tableToken RChipWash = Just "--tv-chip-wash"
tableToken RChipEdge = Just "--tv-chip-edge"
tableToken RMarkWash = Just "--tv-mark-wash"
tableToken RFlagWash = Just "--tv-flag-wash"
tableToken RColWash = Just "--tv-col-wash"
tableToken RCellWash = Just "--tv-cell-wash"
tableToken RSortWash = Just "--tv-sort-wash"
tableToken RColsWash = Just "--tv-cols-wash"
tableToken ROk = Nothing
tableToken RWarn = Nothing

data ThemeMode = TLight | TDark deriving (Eq, Show)
data Theme = Theme String String ThemeMode   -- ^ id, label, which system preference it answers
themes :: [Theme]                            -- ^ in the order the sheet offers them
themes = [Theme "light" "light" TLight, Theme "dark" "dark" TDark]
themeIds :: [String]                         -- ^ the boot script's test and @#themesel@'s options beside `auto'
themeIds = [i | Theme i _ _ <- themes]

stateSlots, prioritySlots :: Int             -- ^ the WIRE's counts, the same for every theme
stateSlots = 4
prioritySlots = 3
slotToken :: String -> Int -> String         -- ^ a `var()' rather than a hex: a theme switches with no refetch
slotToken grp i = "var(--g-state-" <> grp <> show (i `mod` stateSlots) <> ")"
overridable :: String -> String -> String -> String
overridable prefix value fallback = "var(--g-" <> prefix <> "-" <> value <> ", " <> fallback <> ")"
-- ^ a CSS FALLBACK CHAIN, so a per-keyword hue costs the wire nothing.
geometryTokens :: [String]                   -- ^ what stays OUT of a theme
geometryTokens = ["--g-doc-", "--g-pop-"]

stateColorsPragma :: String
stateColorsPragma = "#+GLANCE_STATE_COLORS:"
stateColorsOf :: String -> Maybe (String, [(String, String)])
stateColorsOf line = case words line of
  (theme : rest) -> Just (theme, foldl' lastWins [] (mapMaybe pair rest))
  []             -> Nothing
  where pair w = case break (== '=') w of (k, '=' : v) | not (null k) -> Just (k, v); _ -> Nothing
        lastWins acc (k, v) = filter ((/= k) . fst) acc <> [(k, v)]
-- ^ the theme first, @KEYWORD=VALUE@ after; a keyword named twice takes its LAST
-- spelling; the SHAPE is all that is validated.

-- ** Notes

cmdNotes :: [Note]
cmdNotes =
  [ Note "A write is temp+rename, so untouched bytes stay byte-identical; the rename replaces the destination NAME, leaving a regular file where a symlinked .org was." [Comment, Unguarded]
  , Note "The rename is atomic and NOT durable: the containing directory is fsynced after it, and a write that had to create directories syncs each new one's parent, else a crash takes back a write that answered 200." [Comment, Test]
  , Note "`Data.Org.Edit' is content-agnostic BY LAW, which is why the edit-link layer owes all three of its checks." [Typed, Test]
  , Note "No rollback ACROSS files and none is possible: a 200 means the command RAN, never that every row moved." [Test]
  , Note "The route never writes the store; the watch re-reads what was written, so a second command against a file the first wrote drifts under a suite that runs no watch." [Test]
  , Note "`parseCommand' resolves the NAME before anything else, so a Command cannot be built without the entry it resolved to." [Typed]
  , Note "`csArgs' is handed the IDS beside the args because a shape refusal is about the REQUEST; only edit-link reads them, and `wantsLink' names the row COUNT first." [Test]
  , Note "Keyword legality is the ROW's own chain (`settableStates', the palette's own fold), and a word any named row's chain lacks refuses the WHOLE request naming keyword and row." [Test]
  , Note "The date is parsed ONCE per request against the server's today and passed DOWN, so a set crossing midnight cannot land on two days." [Test]
  , Note "ONE WALL PER KEY, AND BOTH WRITE DOORS ASK IT: `plannedValue' is what set-planning's date argument and POST /headline's planning entries each read (`planWall') — SCHEDULED and DEADLINE take `planningTimestamp', CLOSED reparses verbatim — so a spelling one door takes is a spelling the other takes and a refusal is one refusal. THE KEYWORD IS REFUSED AHEAD OF THE VALUE at both, an unknown key outranking every date, and the client's own wall over CLOSED wears this one's words, which is what makes the widget's optimistic shut safe on that door." [Test]
  , Note "THE FIELD'S WHOLE CONTENT IS THE PHRASE, which is what makes the English reading safe: it fires only where a write already owes a date and never over prose. dateutil's fuzzy mode is the measurement it is built to refuse — `18 August was rainy', `Chapter 18 Aug summary' and `read pages 3 to 4' all come back dates, and `from 18 to 19 august' comes back ONE instant in 2018, wrong about the year, the day AND the arity." [Test]
  , Note "The month word folds TOTALLY (August = august = AUGUST), a year is FOUR digits and never two, a bare day and a bare month are each no date, and THE WEEKDAY IS NEVER READ since it is computed on render." [Test]
  , Note "An interval's LEFT END INHERITS every field it elides from the right; one whose end falls before its start is REFUSED IN ITS OWN WORDS, `not a date' reading oddly of a phrase naming two perfectly good ones; and the DEGENERATE pair COLLAPSES, which is what keeps the grammar's two spellings of one day a single fixed point." [Test]
  , Note "A BRACKET IS THE ASK FOR AN ACTIVITY, read BEHIND org's own spelling and nowhere else: a bracketed value that REPARSES is kept verbatim (`verbatimDate Bracketed', wrong weekday and all), and only a bracket that reparses nothing is read as the whole grammar WRAPPED — the body by the very reader a bare field uses, the answer wearing the pair that was typed (`stampActivity'). So `[today]' is the clock day inactive, `<today>' is `today''s own bytes character for character, and an interval rides in with the rest, both ends wearing the one bracket kind the format allows. A MISMATCHED or EMPTY pair, and a body outside the grammar, keep the bracket's own refusal — the arm widens what is TAKEN and never what is said about the rest. CLOSED is untouched by it: that wall REPARSES, so `[today]' meets its sentence like any other phrase." [Test]
  , Note "The interval costs NO SECOND STAMP RENDERER: the `--' pair joins two stamps the wall's own single-stamp renderer writes, each recomputing its weekday off its date, so one spelling of a day serves both ends and the bare form. TextShow is the lossy REPL re-serializer and never a write-back channel, so no byte the wall lands comes from it. `..' is the FILTER's range, compiled to a predicate per request and never written anywhere; `--' is the file format's and the only spelling this parser produces." [Test]
  , Note "`rename-tag' is a command rather than a remove plus an add: those two edit sets APPLY and write wrong bytes two independent ways, and would be two writes under two digests." [Test]
  , Note "ONE TAG ONCE: the first `from' entry becomes `to' and further ones are cut; a row already carrying `to' has every `from' cut instead." [Test]
  , Note "Presence is FOLDED through `tagsOfCell', so a removal takes EVERY entry spelling the tag and rename is a change of spelling like any other." [Test]
  , Note "A written line takes its ending from `eolOf', so a write into a CRLF file leaves a CRLF file." [Test]
  , Note "The captured id is `rowIdIn path K' with K the store's rows for that FILE — a race, honestly, since /command never writes the store." [Comment]
  , Note "`blobDocument' ends the text FIRST and measures afterwards; the drawer joins an existing :PROPERTIES: under its own indentation, else is written whole under the PLANNING line." [Test]
  , Note "Both properties are written whatever the template said, a template spelling ORG_GLANCE_ID claiming an identity the store hands out." [Test]
  , Note "KNOWN DIVERGENCE from org-glance: its renderer also rewrites the template heading's TITLE from the capture's title." [Corpus]
  , Note "A tagged capture's blob shard is unwatched for the daemon's life; it reaches the table because every write nudges its own path." [Test]
  , Note "ONE clock read covers both stamps a capture writes, so a template naming the moment and the creation time it is filed under can never name two." [Test]
  , Note "`tags' rides GET /capture rather than /tags, that route answering about ROWS a caller names and a capture naming none." [Test]
  , Note "THE CAPTURE DOC IS THE MATERIAL DOC: GET /capture answers a DRAFT in `headlineFields'' own shape off bytes with no file behind them, so the pane draws a capture with the doc code it already has rather than a second editor with rules of its own. `id' is null, `file' is empty and `digest' is \"\" — THE CREATE PIN, the very lock the write path already spells for a target that is not there, which is what makes the commit that follows an ordinary drift-locked write." [Test]
  , Note "The draft is composed by ONE PARSE the commit reads back through (`draftRecord'), and a BLANK ENTRY IS KEPT where `recordsOf' drops one: `* ' with an empty title is exactly what the bare template opens as, so the row the table would refuse to show is the row capture starts from." [Test]
  , Note "`point' is a LINE OF `body', the coordinates `ownLines' and a child's `line' are already in, and null is the headline itself — so the pane lands by a reading it makes anyway rather than by a second addressing scheme. It is measured on the EXPANDED doc before the filter's seeds are spliced, which is safe because a seed edits the headline line and the planning line and neither is a line `body' carries." [Test]
  , Note "`cycle' rides the draft answer because /keywords is ROW-KEYED and a draft has no row: `draftKeywords' walks the same `keywordScopes' chain with an empty FILE scope, so the cycle the state door OFFERS is the list the commit door WALLS with, one fold (`flatKeywords') serving both." [Test]
  , Note "The prompting escapes DISSOLVED: `%^{PROMPT}' expands to its EMPTY VALUE — a drawer pair with none, a slot in the body — because a pre-form field existed only where the form could not edit structure. The stamping escapes still take the server's clock, the page spelling no org, and `%?' writes nothing at all: it is where point opens." [Test]
  , Note "TWO ROADS, EXACTLY ONE TAKEN: `text' (with `fields', through the tag's template) is the older wire and stays, the door being public and org-glance able to drive it; `title' opens the sheet's cargo. NAMING BOTH IS REFUSED rather than resolved, and both roads hand the SAME org to the same minting, which is why the shard path, the id, the creation drawer, the ledger note and the inbox split are untouched by the widening." [Test]
  , Note "ONE WALL PER KEY AND EVERY ONE OF THEM THE ROW EDIT'S (`cargoWall'): planning through `plannedValue' with the KEY OUTRANKING THE VALUE, tags and the state through their charsets, the title through the headline reparse, and the state through the very cycle the draft door offered. A capture is ONE TOP ENTRY, so a body line opening a single star is refused rather than written — the one-headline wall reaching the widened road the way `captureText' reaches the older one." [Test]
  , Note "A BLOB IS A NEW FILE AND HAS NO LINE ENDING OF ITS OWN, so it takes the TEMPLATE'S — the bytes it is composed out of. The older road got this for nothing, `expandTemplate' copying the template verbatim; the widened road joins its head line with the same ending and RE-ENDS the pane's body into it, the page speaking `\\n' and knowing no other. A CRLF layer lands a CRLF blob rather than an entry ending one way over a body ending the other." [Test]
  , Note "The header is composed and READ BACK before it is written (`draftEntry'): a title carrying a tag run or a star reparses as something else, and is refused naming the part rather than written and misread on the next load." [Test]
  , Note "THE INBOX PATH GAINED THE BLOB PATH'S SPLICE AND NOTHING ELSE: `stampedEntry' joins the creation stamp to whatever drawer the entry has, `blobDocument' being that function with an id and a tag handed to it. No id, no tag and no ledger line for the inbox, and the old jot's bytes are unmoved." [Test]
  , Note "WHAT THE FILTER LENDS IS TEMPLATE-FIRST (`lentInto'), and the tag seed is that rule's first case: only a fact the query pins to ONE ordinary positive value is lent, tags JOIN rather than fill, and a lent fact the draft's own walls turn down is simply not inherited — the draft door never 400s over one. The CLIENT extracts the facts and the SERVER merges them, so one composer owns precedence and a day word resolves under the door's one clock read." [Test]
  , Note "THE DRAFT SAYS WHERE IT LANDS. Its tag cell is CONSTRUCTED (`draftCells') and owes no round trip through the org line: the DESTINATION leads it, the template's own run and the lent tags follow, and the cell is what the pane draws and what the commit carries out as the capture's `tags'. The minting joins the destination IDEMPOTENTLY (`addTagEditsIn' folds), so the blob wears each tag once. A lent tag the charset turns down is not worn, the never-refuse rule as everywhere; the destination is no lent fact and rides as the reader settled it, the file line already naming it." [Test]
  , Note "KNOWN LIMIT, and it is the ORG LINE'S alone: a tag run needs a TITLE to stand after, this parser reading `* :work:' as the title itself, so the draft's own bytes carry no run until one is typed. The DISPLAY cell says the destination all the same, and the commit composes the header out of the cell rather than out of that line." [Test]
  , Note "DELETION IS A MOVE: the whole blob DIRECTORY is gzipped under the trash's mirror of its path, the copy landing before the original goes, a destination that already exists refused." [Test]
  , Note "KNOWN LIMIT: one blob, one tombstone — a hand-written blob's SECOND top-level entry loses its bytes and keeps its record." [Test]
  , Note "delete's three walls are checked on the SERVER as well as in the shell, because a request is a request whoever wrote it." [Test]
  , Note "The renderer ships its palette at ZERO specificity (:where(.tv-root)), which is what lets the page's ordinary rules win whatever order the stylesheets land in." [Docs]
  , Note "A pill draws its hue as INK over a 15% wash of itself over the ROW's ground, so a theme picks hues readable over its own pBg AND pSelection." [Docs]
  , Note "A COLUMN'S TEXT IS `ch' AND ITS GROUNDS ARE `px', each in the unit the stylesheet spends it in: a pill's 16px of padding allowed for as 2 characters is exact at one font size and short at every other. The allowance carries a further px because a column width lands DOWN on the engine's 1/64 grid, and a pill is an inline-block `text-overflow' cannot cut — so a hair short draws the whole badge with an ellipsis behind it." [Browser]
  , Note "`paletteSweep' is the DERIVED oracle: it reads the served page and compares the two namespaces role by role, and counts the slots the served rows name." [Test]
  , Note "A tree's state hues are the SYSTEM layer's alone and are emitted per REQUEST after `themeCSS', coming off the store's config rather than out of the build." [Test]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "Every CmdName has an entry." [Unguarded]
  , Note "Ids owed exactly where the kind names rows." [Unguarded]
  , Note "One Makes, one Moves." [Unguarded]
  , Note "Only set-planning is dated." [Unguarded]
  , Note "Capture and delete are the two with no row function." [Unguarded]
  , Note "Nine row functions answer through plain." [Unguarded]
  , Note "A Makes owes no walls." [Unguarded]
  , Note "One command's answer records, which is what the ledger counts." [Unguarded]
  , Note "Span math is exactly the row functions' commands." [Unguarded]
  , Note "Tokens are unique in both namespaces." [Unguarded]
  ]
-- * Shell: keys, table gestures, surfaces

-- ** A press becomes a NAME at ONE function, which every listener goes through.

data Press = Press { pKey :: String, pCode :: Maybe String, pCtrl, pAlt, pShift :: Bool }
data KeyBase = KSpecial String | KChar Char deriving (Eq, Show)
-- ^ `S-' is reachable from a `KSpecial' alone: a letter's shift IS the uppercase
-- binding, so `d' and `D' are two rows no layout can collapse into each other.

namedKeys :: [(String, String)]                  -- ^ browser key -> binding name
namedKeys =
  [ ("Enter", "RET"), ("Tab", "TAB"), (" ", "SPC"), ("Escape", "ESC")
  , ("Backspace", "DEL"), ("Delete", "<delete>")
  , ("ArrowUp", "<up>"), ("ArrowDown", "<down>")
  , ("ArrowLeft", "<left>"), ("ArrowRight", "<right>")
  , ("Home", "<home>"), ("End", "<end>"), ("PageUp", "<prior>"), ("PageDown", "<next>") ]

codeLetter :: String -> Maybe Char
codeLetter c = case c of { ['K','e','y',l] | isAlpha l -> Just (toLower l); _ -> Nothing }

fkey :: String -> Maybe String
fkey k = case k of
  ('F':ds) | not (null ds), all isDigit ds, length ds <= 2 -> Just ("<" ++ map toLower k ++ ">")
  _ -> Nothing

baseOf :: Press -> Maybe KeyBase
baseOf p | Just n <- lookup (pKey p) namedKeys = Just (KSpecial n)
         | Just n <- fkey (pKey p)           = Just (KSpecial n)
         | Just l <- pCode p >>= codeLetter  = Just (KChar (if pShift p then toUpper l else l))
         | [c] <- pKey p                     = Just (KChar c)
         | otherwise                         = Nothing

keyName :: Press -> Maybe String
keyName p = fmap named (baseOf p)
  where named b = concat ["C-" | pCtrl p] ++ concat ["M-" | pAlt p]
                  ++ concat ["S-" | pShift p, isSpecial b] ++ word b
        word (KSpecial n) = n
        word (KChar c)    = [c]
isSpecial :: KeyBase -> Bool
isSpecial (KSpecial _) = True
isSpecial (KChar _)    = False
press :: String -> Press                     -- ^ a bare press, no code and no modifier
press k = Press k Nothing False False False

-- ** The keymap: ONE table, carried as @{rows, hints, reserved, once}@.

-- | @SWindow@ is the NATIVE WINDOW'S OWN: live only where a window stands behind
--   the page, which is what leaves the browser's zoom chords to the browser.
data KeyScope = STable | SModal | SWindow | SAny deriving (Eq, Ord, Show, Enum, Bounded)
newtype Elisp = Elisp String deriving (Eq, Ord, Show)
-- ^ commands are elisp function names and the echo speaks them verbatim.
data Binding = Binding [String] Elisp KeyScope
bkeys  :: Binding -> [String] ; bkeys  (Binding k _ _) = k
bcmd   :: Binding -> Elisp    ; bcmd   (Binding _ c _) = c
bscope :: Binding -> KeyScope    ; bscope (Binding _ _ s) = s
seqOf :: Binding -> String                   -- ^ derived: the keys, one space between
seqOf = unwords . bkeys
helpOf :: Binding -> Maybe String            -- ^ derived, and optional by construction
helpOf b = listToMaybe ([ t | ((s, ss), t) <- scopedHelps, s == bscope b, seqOf b `elem` ss ]
                     <> [ t | (ss, t) <- keyHelps, seqOf b `elem` ss ])

bindings :: [Binding]
bindings =
  [ Binding ["n"]           (Elisp "next-row")                        STable
  , Binding ["p"]           (Elisp "previous-row")                    STable
  , Binding ["j"]           (Elisp "next-row")                        STable
  , Binding ["k"]           (Elisp "previous-row")                    STable
  , Binding ["<down>"]      (Elisp "next-row")                        STable
  , Binding ["<up>"]        (Elisp "previous-row")                    STable
  , Binding ["f"]           (Elisp "next-column")                     STable
  , Binding ["b"]           (Elisp "previous-column")                 STable
  , Binding ["l"]           (Elisp "next-column")                     STable
  , Binding ["h"]           (Elisp "previous-column")                 STable
  , Binding ["<right>"]     (Elisp "next-column")                     STable
  , Binding ["<left>"]      (Elisp "previous-column")                 STable
  , Binding ["<"]           (Elisp "first-row")                       STable
  , Binding [">"]           (Elisp "last-row")                        STable
  , Binding ["G"]           (Elisp "last-row")                        STable
  , Binding ["]"]           (Elisp "next-page")                       STable
  , Binding ["["]           (Elisp "previous-page")                   STable
  , Binding ["^"]           (Elisp "toggle-sort")                     STable
  , Binding ["RET"]         (Elisp "org-glance-overview:materialize")  STable
  , Binding ["/"]           (Elisp "filter-rows")                     STable
  , Binding ["."]           (Elisp "compose-query")                   STable
  , Binding ["DEL"]         (Elisp "filter-drop-token")               STable
  , Binding ["g"]           (Elisp "apply-default-filter")            STable
  , Binding ["P"]           (Elisp "set-saved-view")                  STable
  , Binding ["m"]           (Elisp "mark-toggle")                     STable
  , Binding ["SPC"]         (Elisp "mark-toggle")                     STable
  , Binding ["u"]           (Elisp "unmark")                          STable
  , Binding ["U"]           (Elisp "unmark-all")                      STable
  , Binding ["M"]           (Elisp "mark-all")                        STable
  , Binding ["q"]           (Elisp "quit-window")                     STable
  , Binding ["TAB"]         (Elisp "org-cycle")                       STable
  , Binding ["o"]           (Elisp "org-glance-overview:open")        STable
  , Binding ["!"]           (Elisp "org-glance-overview:open")        STable
  , Binding ["a"]           (Elisp "org-glance-agenda")               STable
  , Binding ["@"]           (Elisp "org-glance-overview:relations")   STable
  , Binding ["+"]           (Elisp "org-glance-overview:capture")     STable
  , Binding ["d"]           (Elisp "archive-flag")                    STable
  , Binding ["D"]           (Elisp "org-glance-overview:delete")      STable
  , Binding ["x"]           (Elisp "dired-do-flagged-delete")         STable
  , Binding ["S-<up>"]      (Elisp "priority-up")                     STable
  , Binding ["S-<down>"]    (Elisp "priority-down")                   STable
  , Binding ["t"]           (Elisp "org-glance-overview:todo")        STable
  , Binding ["C-c", "C-t"]  (Elisp "org-glance-overview:todo")        STable
  , Binding [":"]           (Elisp "org-agenda-set-tags")             STable
  , Binding ["C-c", "C-s"]  (Elisp "org-glance-overview:schedule")    STable
  , Binding ["C-c", "C-d"]  (Elisp "org-glance-overview:deadline")    STable
  , Binding [","]           (Elisp "customize")                       STable
  -- ONE KEY, TWO SURFACES: `@' READS the edges from the table and WRITES one
  -- from the sheet, so the scope is what tells the two apart.
  , Binding ["@"]           (Elisp "org-glance-material:refer")       SModal
  , Binding ["C-x", "C-s"]  (Elisp "save-buffer")                     SModal
  , Binding ["C-c", "C-c"]  (Elisp "org-ctrl-c-ctrl-c")               SModal
  , Binding ["C-c", "'"]    (Elisp "org-edit-special")                SModal
  -- `+' WANTS THE SHIFT on most layouts, and a browser reads the unshifted key
  -- as zoom-in for that reason, so both spellings reach the one command.
  , Binding ["C-+"]         (Elisp "text-scale-increase")             SWindow
  , Binding ["C-="]         (Elisp "text-scale-increase")             SWindow
  , Binding ["C--"]         (Elisp "text-scale-decrease")             SWindow
  , Binding ["C-0"]         (Elisp "text-scale-set")                  SWindow
  , Binding ["ESC"]         (Elisp "keyboard-quit")                   SAny
  ]

-- | Where ONE SEQUENCE MEANS TWO THINGS, the scope decides which help it carries.
scopedHelps :: [((KeyScope, [String]), String)]
scopedHelps =
  [ ((SModal, ["@"]), "link a headline into the prose; at a word boundary, so an\
                      \ address stays text") ]

keyHelps :: [([String], String)]                -- ^ where the command name does not say enough
keyHelps =
  [ (["f", "l", "<right>"], "the cell to the right; row movement keeps the column")
  , (["b", "h", "<left>"],  "the cell to the left; from a whole row, the first column")
  , (["<"],                 "first row, again = page up")
  , ([">", "G"],            "last row, again = page down")
  , (["^"],                 "put this column at the head of the order; again reverses it")
  , (["/"],                 "summon the filter box onto the chip strip")
  , (["."],                 "the whole expression: filters, sort: and columns: together")
  , (["DEL"],               "unmark all, else drop the filter's last token")
  , (["g"],                 "the view this tree opens on")
  , (["P"],                 "pin the applied view, into whichever saved view answers")
  , (["m", "SPC"],          "toggle this row's mark, then step down")
  , (["u"],                 "take this row's archive flag off, else its mark, then step down")
  , (["U"],                 "every mark and every archive flag off")
  , (["M"],                 "mark every row loaded")
  , (["o", "!"],            "open links: the row here, the element in the sheet; several list them")
  , (["a"],                 "the active rows carrying a date, earliest first")
  , (["@"],                 "the rows referring to this one; DEL walks back")
  , (["+"],                 "a headline for the inbox, typed as org")
  , (["d"],                 "flag for archive; d again archives all flagged")
  , (["D"],                 "archive the flagged; an already-archived row deletes, on a typed word")
  , (["x"],                 "act on the flagged rows, after asking; d flags, D is the quick one")
  , (["S-<up>", "S-<down>"], "cycle the priority of the marked rows, or the row at point")
  , (["t"],                 "set the state of the marked rows, or the row at point")
  , (["C-c C-t"],           "the org spelling, where the browser lets it through")
  , (["C-c C-s", "C-c C-d"], "a date over the marked rows, or the row at point; empty clears it")
  , ([","],                 "the settings sheet: theme, keyword cycles")
  , (["C-x C-s"],           "sync the sheet now; again to overwrite a conflict")
  , (["C-c C-c"],           "commit the element being edited")
  , (["C-c '"],             "the sheet as raw org, or as body and properties; sync an edited one first")
  , (["C-+", "C-=", "C--"], "the window's own zoom, a tenth of itself at a time")
  , (["C-0"],               "back to 100%")
  , (["ESC"],               "close the sheet, syncing an edited one; again to discard")
  ]

once :: [Elisp]                              -- ^ auto-repeat off, named by COMMAND
once = map Elisp
  [ "filter-drop-token", "unmark-all", "mark-all", "archive-flag"
  , "org-glance-overview:delete", "dired-do-flagged-delete", "set-saved-view"
  , "org-glance-overview:open", "org-glance-agenda", "org-glance-overview:relations"
  , "priority-up", "priority-down", "toggle-sort" ]

reserved :: [String]                         -- ^ left to the browser unless a sequence completes
reserved = ["C-l", "C-r", "C-t", "C-u", "C-w", "C-n", "C-p", "<f5>"]

-- ** The modal surfaces are ONE list, and four readers take everything off it.

data Surface = Surface { sName :: String, sMomentary, sOff, sOpens, sRowed :: Bool
                       , sEdit, sNarrow, sPanelled :: Bool }
surfaces :: [Surface]
surfaces =
  --      name        mom   off   opens rowed edit  narr  panel
  [ Surface "mint"    True  True  False False False False False
  , Surface "prompt"  True  True  False False False False False
  , Surface "refer"   True  True  False False False False False
  , Surface "capture" True  True  True  False False False False
  , Surface "links"   True  True  True  True  True  True  False
  , Surface "tags"    True  True  True  True  True  True  False
  , Surface "sheet"   False False True  True  True  False False
  , Surface "config"  False False True  False True  True  True
  ]
momentaryUp :: [Surface] -> Maybe Surface    -- ^ the list ORDER breaks the one tie
momentaryUp = listToMaybe . filter sMomentary
sole :: [Surface] -> [Surface]               -- ^ a raise closes every momentary one
sole = filter (not . sMomentary)
live :: Bool -> [Surface] -> KeyScope -> Bool -- ^ WINDOWED, the surfaces up, the row's scope
live _   _  SAny    = True
live _   up SModal  = any (not . sMomentary) up
live _   up STable  = null up                 -- plus a focused control: `typing()'
-- THE ONE SEAM THAT LEAVES A KEY TO THE BROWSER: `run' is reached only past a
-- `preventDefault', so a handler cannot decline a press.  A row that is never
-- LIVE is never matched, and the dispatch claims nothing.
live win _  SWindow = win

data Rung = REdit | RNarrow | RSurf deriving (Eq, Show)
escAt :: Bool -> Bool -> Rung                -- ^ ESC: three rungs per surface, innermost first
escAt True _    = REdit
escAt _    True = RNarrow
escAt _    _    = RSurf

-- ** Mark and Flag: two id-keyed renderer sets, read by different commands.

data Sel = Mark | Flag deriving (Eq, Ord, Show, Enum, Bounded)
data Take = Marked | Flagged | FlaggedOnly | AtPoint | NoRows deriving (Eq, Show)
-- ^ `Marked'/`Flagged' are marked-else-point and flagged-else-point; `x' is the
-- flags ALONE, which is what makes it the deliberate half of the pair.
takesRows :: [(Elisp, Take)]
takesRows =
  [ (Elisp "org-glance-overview:todo",      Marked)
  , (Elisp "org-agenda-set-tags",           Marked)
  , (Elisp "org-glance-overview:schedule",  Marked)
  , (Elisp "org-glance-overview:deadline",  Marked)
  , (Elisp "priority-up",                   Marked)
  , (Elisp "priority-down",                 Marked)
  , (Elisp "archive-flag",                  Flagged)
  , (Elisp "org-glance-overview:delete",    Flagged)
  , (Elisp "dired-do-flagged-delete",       FlaggedOnly)
  , (Elisp "org-glance-overview:relations", AtPoint)
  , (Elisp "org-glance-overview:open",      AtPoint)
  , (Elisp "org-glance-overview:capture",   NoRows)
  ]
readsSel :: Take -> Maybe Sel
readsSel Marked      = Just Mark
readsSel Flagged     = Just Flag
readsSel FlaggedOnly = Just Flag
readsSel AtPoint     = Nothing
readsSel NoRows      = Nothing
unmarkAt :: Bool -> Sel                      -- ^ `u' takes the flag off before the mark
unmarkAt True  = Flag
unmarkAt False = Mark
clears :: Elisp -> [Sel]                     -- ^ `U' clears both; DEL's rung the marks alone
clears c | c == Elisp "unmark-all"        = [Mark, Flag]
         | c == Elisp "filter-drop-token" = [Mark]
         | otherwise                      = []

-- ** ONE d/D/x/u gesture over FOUR surfaces, each declaring a shape.  A property
-- pair is a DOC row now, so its flags ride the doc's own shape.

data FlagS = FlagS { fName, fVerb, fNone :: String, fWalled, fLogs, fRendered :: Bool }
flagSurfaces :: [FlagS]
flagSurfaces =
  --      name     verb      an empty cursor                   wall  logs  rendered
  [ FlagS "table"  "archive" "no row"                          True  True  True
  , FlagS "tags"   "remove"  "org-toggle-tag (no tag)"         False False True
  , FlagS "doc"    "delete"  "org-delete-element (no element)" False False False
  , FlagS "states" "remove"  "org-todo-remove-state (no row)"  False False True
  ]
data FKey = Fd | FD | Fx | Fu deriving (Eq, Show, Enum, Bounded)
data Act = FlagIt | Unflag | Take' Bool | Ask | Idle | NoCursor | NoFlags deriving (Eq, Show)
-- ^ @Take' True@ is the flagged set, @False@ the row at point.
flagAct :: FKey -> FlagS -> Bool -> Bool -> Maybe RowId -> [RowId] -> Act
--          key    shape    flagsOn walled cursor         flags laid down
flagAct _  _ _     _ Nothing  _  = NoCursor
flagAct FD _ _     _ _        fs = Take' (not (null fs))
flagAct Fd _ _     _ (Just a) fs | a `elem` fs = Take' True   -- the second press IS `D'
flagAct _  _ False _ _        _  = NoFlags
flagAct Fx s _     w _        fs | null fs        = Idle      -- no deletions requested
                                 | fWalled s && w = Take' True -- a wall of its own is not asked twice
                                 | otherwise      = Ask
flagAct Fu _ _     _ _        _  = Unflag
flagAct Fd _ _     _ _        _  = FlagIt

-- ** DEL in the table is a ladder: erase the last structure standing.

data Del = ClearMarks | NoFilter | PopCrumb | DropToken deriving (Eq, Show)
delRung :: Int -> Int -> Int -> Del           -- ^ marks, query tokens, trail depth
delRung marks toks trail
  | marks > 0              = ClearMarks
  | toks == 0              = NoFilter
  | toks == 1, trail > 0   = PopCrumb
  | otherwise              = DropToken

-- ** DEL in the PICKER is its own ladder, and the box is a rung of it.

-- | The picker's rungs, outermost last.  The first two are the RENDERER's, taken
-- while the summoned editor holds the keys; the two below are the picker's own
-- listener, which stands aside until the box has gone.
data ReferDel = EraseChar | ShutBox | DropChip | DropMark deriving (Eq, Show)
referRung :: Bool -> Bool -> Int -> ReferDel
-- ^ the box has the keys, the box is empty, chips applied
referRung True False _      = EraseChar
referRung True True  _      = ShutBox
referRung False _    chips
  | chips > 0               = DropChip
  | otherwise               = DropMark

-- ** Drill-down: one semantic at two grains, and DEL is the single undo.

data Crumb = Crumb Query String              -- ^ the renderer's, and it keeps nothing else
data Trail = Trail [Crumb] [RowId]           -- ^ `crumbSels' rides BESIDE the stack
type Labels = [(String, String)]             -- ^ token -> label; no lookup recovers it
selsFit :: Trail -> Bool                     -- ^ a side table out of step is dropped whole
selsFit (Trail cs ss) = length cs == length ss
crumbMax :: Int
crumbMax = 4                                 -- then a "... +N" fold
data Drill = NoView | Applied Bool deriving (Eq, Show)
drill :: Query -> Int -> Drill               -- ^ probed under @limit=1@ before it applies
drill _ 0           = NoView
drill (Query q) _   = Applied (not (null q)) -- a drill out of the EMPTY query pushes no crumb

-- ** Where the cursor lands: three rules at one door.

data Landing = Landing (Maybe RowId) (Maybe Int)
land :: Landing -> [RowId] -> Maybe RowId
land (Landing want back) rows = case want of
  Just r | r `elem` rows -> Just r
  _ -> back >>= \i -> if i < length rows then Just (rows !! i) else Nothing

data Anchor = Anchor { aFrom :: RowId, aId :: RowId, aAt, aOn :: Int }
anchorFor :: RowId -> [RowId] -> [RowId] -> Int -> Maybe Anchor
anchorFor from rows going page =                 -- taken at FIRE time, worked out from POINT
  fmap (\r -> Anchor from r (place r) page) (listToMaybe (down ++ reverse up))
  where i = length (takeWhile (/= from) rows)
        stays r = r `notElem` going
        down = filter stays (drop (i + 1) rows)  -- first row not leaving, below point
        up   = filter stays (take i rows)        -- failing that, the nearest above
        place r = length (takeWhile (/= r) (filter stays rows))
settle :: Anchor -> [RowId] -> Int -> Maybe Landing
settle a rows page | aFrom a `elem` rows = Nothing   -- the unfiltered client: nothing moved
                   | page /= aOn a       = Nothing   -- `visible()' is ONE page
                   | otherwise           = Just (Landing (Just (aId a)) (Just (aAt a)))

data LandDoor = DApply | DBoot | DHome | DPop | DArchive | DRefetch
  deriving (Eq, Show, Enum, Bounded)
landingAt :: LandDoor -> Maybe Anchor -> Maybe RowId -> Landing
landingAt DApply   _ _ = Landing Nothing (Just 0)    -- `a' and a commit are new questions
landingAt DBoot    _ _ = Landing Nothing (Just 0)    -- a boot IS an applied view
landingAt DHome    _ r = Landing r (Just 0)          -- `g' is save-excursion
landingAt DPop     _ r = Landing r (Just 0)
landingAt DRefetch _ _ = Landing Nothing Nothing     -- a watch step is no new question
landingAt DArchive a _ =
  maybe (Landing Nothing Nothing) (\x -> Landing (Just (aId x)) (Just (aAt x))) a

-- ** The log strip: append-only, and `append(scope, severity, message)' is all of it.

data Sev = Info | Warn | Error deriving (Eq, Ord, Show, Enum, Bounded)
data LogScope = LWs | LSync | LCmd | LFilter | LConfig | LBoot deriving (Eq, Show, Enum, Bounded)
-- | @HH:MM:SS SEV scope message@, plus the @xN@ a repeat bumps.
data Line = Line LogScope Sev String Int deriving (Eq, Show)
sevWord :: Sev -> String                     -- ^ SPELLED uppercase, what a screenful is scanned for
sevWord Info = "INFO" ; sevWord Warn = "WARN" ; sevWord Error = "ERROR"
sevClass :: Sev -> String                    -- ^ WORN lowercase: one value, two cases
sevClass = map toLower . sevWord
scopeWord :: LogScope -> String
scopeWord s = case s of
  LWs -> "ws" ; LSync -> "sync" ; LCmd -> "cmd"
  LFilter -> "filter" ; LConfig -> "config" ; LBoot -> "boot"
logCap :: Int
logCap = 500                                 -- ^ lines KEPT; the OLDEST goes
logAppend :: [Line] -> Line -> [Line]           -- ^ newest first
logAppend ls l@(Line s v m _) = case ls of
  Line s' v' m' n : rest | (s', v', m') == (s, v, m)
    -> Line s' v' m' (n + 1) : rest          -- the ONE mutation an append-only strip allows
  _ -> take logCap (l : ls)

data Knob = Knob { kKey :: String, kDef, kMin, kMax :: Int }
logKnob :: Knob
logKnob = Knob "glance-log" 7 1 50           -- mirrored in Haskell as logLinesDefault/Min/Max
-- | The window's zoom, AS WHOLE PERCENTAGES, and the second knob of this shape: the
--   keys move it, the settings row reads it back, and the window wears it as a level.
--   CLAMPED rather than declined -- a press at the ceiling is a press, where a typed
--   number out of band is a typo.
zoomKnob :: Knob
zoomKnob = Knob "glance-zoom" 100 50 300     -- mirrored in Haskell as zoomDefault/Min/Max
zoomStep :: Double                           -- ^ one press: a tenth of itself, rounded whole
zoomStep = 1.1
zoomPress :: Int -> Int -> Int               -- ^ LEVEL, then the direction; 0 is back to the default
zoomPress level step
  | step == 0 = kDef zoomKnob
  | otherwise = held (round (if step > 0 then fromIntegral level * zoomStep
                                         else fromIntegral level / zoomStep))
  where held n = max (kMin zoomKnob) (min (kMax zoomKnob) n)
data LogPref = ToDefault | Lines Int | Declined deriving (Eq, Show)
logLines :: String -> LogPref                   -- ^ DECLINED rather than clamped; blank REMOVES the key
logLines t | null t = ToDefault
           | all isDigit t
           , n <- read t :: Int
           , n >= kMin logKnob, n <= kMax logKnob = Lines n
           | otherwise = Declined

-- ** The stale wash: one mechanism, two triggers, one class.

data Wash = WView | WSocket deriving (Eq, Show, Enum, Bounded)
grace :: Wash -> Int                         -- ^ ms before a reason arms
grace WView = 300 ; grace WSocket = 400
data Counting = Stepped | Held deriving (Eq, Show)
counting :: Wash -> Counting                 -- ^ an abort overlaps its replacement; a refusal never opened
counting WView = Stepped ; counting WSocket = Held
washOpacity :: Double ; washOpacity = 0.55
washEase :: Int ; washEase = 180             -- ms
washCovers, washExempt :: [String]
washCovers = ["#app", "#modal", "#prompt", "#config"]
washExempt = ["#log", "#keys"]                   -- where a reader finds out why

-- ** Four z-indexes, and a fifth overlay costs no band.

-- | `ZRefer' stands OVER the sheet because it is drawn INTO it: the picker hangs
-- at the caret of the box it is about to write to, so a level under the sheet
-- would put it behind the prose it is completing.
data Z = ZSpine | ZEcho | ZBackdrop | ZSheet | ZRefer deriving (Eq, Show, Enum, Bounded)
zOf :: Z -> Int
zOf ZSpine = 1 ; zOf ZEcho = 2 ; zOf ZBackdrop = 100 ; zOf ZSheet = 101 ; zOf ZRefer = 102
zRetired :: Int ; zRetired = 3               -- ^ the status corner's, forbidden coming back
tvHeader, tvCompletion :: Int
tvHeader = 1 ; tvCompletion = 5              -- the renderer's, which the backdrop must clear

-- ** Every popup has a URL, and one writer holds it.

data UrlState = UrlState { uQ :: Query, uPage :: Maybe String, uRow :: Maybe RowId
                         , uCrumbs :: Maybe Trail, uPanel :: Maybe String
                         , uKeys :: Maybe String }
remembered :: UrlState -> Query -> Maybe Trail -> Maybe Surface -> Maybe RowId
           -> Maybe String -> UrlState
remembered u q tr s row at = u { uQ = q, uCrumbs = tr, uPage = pg, uRow = rw, uPanel = pn }
  where place = s >>= \x -> if sOpens x then Just x else Nothing
        pg = fmap sName place
        rw = if maybe False sRowed place then row else Nothing
        pn = if maybe False sPanelled place then at else Nothing
bootQuery :: Maybe Query -> Query -> Query   -- ^ absent gets the default; present-and-empty is intent
bootQuery Nothing  def = def
bootQuery (Just q) _   = q

-- ** Notes

shellNotes :: [Note]
shellNotes =
  [ Note "MOVEMENT NEVER CHANGES CONTEXT: n/p, f/b and the grain relocate attention alone, RET goes deeper and DEL comes back out, which is why movement is what ONCE leaves out." [Docs]
  , Note "The echo speaks SEQ then the command verbatim, anything else in brackets after it; the resident key line is curated prose naming a group." [Test]
  , Note "Chromium handles Ctrl+T/N/W above the document, so C-c C-t is dead in the browser however correctly it is dispatched; C-x C-s works because Ctrl+S is a page default action." [Unguarded]
  , Note "A reserved chord reaches the browser unless it completes a bound sequence; what the list buys is the abandoned prefix." [Test]
  , Note "THE LIST IS ALSO THE REFUSAL A DESIGN READS: a chord the browser owns is\
         \ one this page does not offer, whatever the elisp original binds.  `C-u' is\
         \ org's universal argument and the browser's view-source, so a prefixed\
         \ gesture spells itself `C-c' — the prefix this map already carries." [Test]
  , Note "BINDING A CHORD IS WHAT TAKES IT, and A PREFIX TAKES IT EVERYWHERE: the\
         \ dispatch calls `preventDefault' on a matched binding AND on any press that\
         \ merely OPENS one, so binding `C-u @' would swallow every `C-u' in the app\
         \ rather than only the gesture — the reader loses view-source at the prefix,\
         \ before the second key decides anything.  `C-c' has already paid that." [Test]
  , Note "Prefix opening is guarded by selecting(), one predicate over the focused field's range and the document selection, so C-c and C-x stay copy and cut." [Test]
  , Note "The map is QWERTY's POSITIONS, so a Latin layout that moves its letters reads its own a as this map's q; a layout spelling no < or [ cannot reach the punctuation half." [Test]
  , Note "The modal surfaces' keys live outside keyBindings, so each guards e.repeat by hand, hand-spells the SEQ arrow shape, and writes no strip line." [Unguarded]
  , Note "SURFACES order is load-bearing for exactly one pair: + over the tags popup leaves prompt and tags both up, and momentary() resolves that tie by list position." [Test]
  , Note "typing() goes false when a click blurs non-focusable sheet chrome, so table rows are live under an open sheet; the guard is spelled per surface rather than held by the map." [Unguarded]
  , Note "THE TAGS CELL IS A SECOND DOOR ONTO ONE COMMAND: a click on the values runs\
         \ the map's own `org-agenda-set-tags' row, so its marked-else-point rule and its\
         \ echo have one source and only the seq is respelled `click'.  The renderer marks\
         \ that cell `tv-multi' and its own click has selected the row under it before this\
         \ page's listener runs." [Test]
  , Note "q is quit-window one window in rather than a rung, so it closes a narrowed popup outright, and it is dead inside an open edit." [Test]
  , Note "Marks and flags are the renderer's session state keyed by id, so both survive a setRows, a filter hiding their row, and a page they are not on; this page keeps no set." [Test]
  , Note "d and D SPEND the flags they fire over, or the refetch's surviving flags are archived again by the next press." [Test]
  , Note "Flags come back oldest first, never in row order, so a caller firing one command per flag runs them the way the reader pressed them." [Test]
  , Note "m and u stay off ONCE because both advance: a held one walks a column laying marks down." [Test]
  , Note "The flag pair is feature-detected: an asset predating flagRow/getFlagged echoes and writes nothing." [Test]
  , Note "The document pane's mount is a Set of ids rather than a renderer, so its `missing' phrase is unreachable." [Unguarded]
  , Note "A property pair is DROPPED and the planning line CLEARED, both leaving through the lists the write carries; an empty value is already how an entry is absent." [Test]
  , Note "Cell movement walks OFF the cells: an out-of-range index goes to select and the renderer answers with the whole-row look, so the glue may name no at-first or at-last wall." [Test]
  , Note "The crumb stack is the renderer's; popCrumb pops and returns without applying, because whoever owns the fetching owns what a query means." [Test]
  , Note "crumbLabels is token to label because no lookup recovers it: the title belongs to the row referred TO, which is rarely among its own referrers." [Test]
  , Note "stash and restore say nothing about crumbs: what they carry is work the reader has not committed." [Test]
  , Note "A ?crumbs= that does not parse is one boot without a trail, and setCrumbs drops whatever is not a crumb." [Test]
  , Note "The drill is feature-detected on the four crumb calls; an asset without them is told so and nothing is applied." [Test]
  , Note "settled ALWAYS spends the anchor, which is what keeps it describing ONE watch step; a commit and a remount drop it outright." [Test]
  , Note "spent(mine) is keyed to its own anchor and decides before unmark, which can throw, so an earlier answer cannot disarm a later archive's." [Test]
  , Note "What the anchor buys over the renderer's keepSelection is rows going from ABOVE point, which keepSelection skips one for." [Test]
  , Note "A mount has no cursor until something selects in it, so a boot that landed nothing left d, D and RET answering `no row'." [Test]
  , Note "Nothing clears the log strip, and the end is scrolled to unless the reader has scrolled up." [Test]
  , Note "A message's control characters collapse to spaces, so an entry is one line whatever it was handed." [Test]
  , Note "The severity and scope are COLUMNS as wide as their own longest word, and TestServe derives both widths off the page's own append calls." [Test]
  , Note "LOGCAP and the height knob are two limits easily confused, so the suite forbids the ring being spelled off the knob's constants." [Test]
  , Note "The knob writes a NUMBER onto the element, so the arithmetic is the stylesheet's and a page whose glue never ran is capped at the same figure." [Test]
  , Note "The wash is opacity and NEVER a filter: a filter makes its element the containing block for position:fixed descendants, and the renderer's palette backdrop is one." [Browser]
  , Note "The page never reads the stale class back; the look is entirely the stylesheet's." [Test]
  , Note "Only fetches whose answer replaces the rows hold the wash, so the parity baseline and @'s probe dim nothing." [Test]
  , Note "There is no status corner: the wash and the strip's ws lines carry the socket twice over, and the absence of #corner and #dot is what is asserted." [Test]
  , Note "With no popup open the table holds the keys, and the page's own column holds no select, input, textarea, button or anchor at all." [Test]
  , Note "A popup hands the keys back ONCE, on close, so no control on this page blurs on its own change." [Test]
  , Note "Every mutation of the crumb stack is followed by a remember, so the address bar is current whenever a view-changed remount re-reads it." [Test]
  , Note "A boot raises what the URL names once the rows are in hand; a re-application is a view the reader asked for here." [Test]
  , Note "A rowed surface lands on its row first and says so where the view no longer holds it." [Test]
  , Note "Keyboard-first: every feature ships with a key path mirroring the Emacs org-glance maps, buttons only where keys cannot reach, and the echo knows every binding." [Docs]
  , Note "ONE map and no profiles: the page carries the blob and its own dispatch parses it, so a binding cannot exist in the handler and not in the map." [Test]
  , Note "Sequences and command names are org-glance's own where org-glance has one, and a row with no handler is recognized in full and says what backs it later." [Test]
  , Note "TWO DOORS ONTO ONE QUERY: `/' (filter-rows) opens the box on the FILTER\
         \ half — the narrowing keys alone in completion, and a shaping token\
         \ (sort:, columns:, view:, either sign) refused on commit with a spoken\
         \ line naming the other door, left standing in the box and never chipped\
         \ — while `.' (compose-query) opens the same box on the whole expression.\
         \ The standing shaping chips ride a `/' commit untouched, so narrowing\
         \ never loses the shape, and both doors read and write the one ?q=: the\
         \ narrow one is a restricted VIEW over it, never a second query." [Test, Browser]
  , Note "THE BOX DOCKS ON THE STRIP: `filterDock: \"strip\"' summons the one box\
         \ onto the CHIP STRIP's own row — chips in their column, the box taking\
         \ the slack, revealed while the mount is typing — over a table that keeps\
         \ its full height, its sort arrows and its hint line.  The veil and the\
         \ centred panel belong to the overlay dock alone, so the main table draws\
         \ neither and the page's glue may name neither.  The SUMMONED ladder\
         \ follows the summoning rather than the veil: two-step ESC (the typed text\
         \ first, the box second), a dead DEL over an empty box, and filtering on\
         \ COMMIT alone hold in whichever dock THIS page's box lands in.  The\
         \ picker's inline box is summoned by the same `openFilter' and keeps its\
         \ own rungs — one ESC, a DEL that shuts it (`referRung'), and rows that\
         \ narrow as it is typed.  The narrow/whole door is the session's and\
         \ moves with neither dock." [Test, Browser]
  , Note "DEL over the LINK and TAG popups steps out where ESC does, the popup being the last structure standing; inside an open rename, link edit or narrow it stays the field's character erase." [Test]
  , Note "Over the value palette DEL is the ENTRIES' rule: a palette nothing claims the key in steps out, and the state palette keeps it because *empty* claims it and commits a null keyword." [Test]
  , Note "THE TYPED VALUE IS ALWAYS AN OFFER where a field's VOCABULARY IS OPEN:\
         \ the add-a-tag palette, the kind picker and both halves of the sheet's\
         \ pair box draw what was typed as their own LEADING entry, hinted `new'\
         \ in the slot the pair box's `planning' rides — so RET commits the word\
         \ the reader spelled and a match is one `C-n' away.  An EMPTY field\
         \ offers no literal, and a typed value folding to an entry COINCIDES\
         \ with it: one entry drawn, never two.  A commit therefore has ONE\
         \ source, the entry point rests on, and a field with a door of its own\
         \ past the drawn list committed a value nothing ever showed.  OPENNESS\
         \ IS SPELLED AT THE CALL rather than read off the list, so the CLOSED\
         \ fields — the state palette and the capture template's code list — keep\
         \ no free-text door." [Test, Browser]
  , Note "A rung with nothing under it falls through in SILENCE, and the pill says the command that RAN." [Test]
  , Note "M is markAll and it TOGGLES: the renderer only adds, so a count that did not move takes them all off, the marks a filter is hiding included." [Test]
  , Note "m and u take the renderer's word for where a mark landed and then step down, dired's rule, and this page keeps no set of its own." [Test]
  , Note "Rows are virtualized a page at a time, so a row step is selectStep; < and > take the page's end row and, pressed again, turn a page and land on the same end." [Test]
  , Note "A page that booted blind re-reads /config on the first load that lands and re-applies the default, but only where the reader has not made the query theirs." [Test]
  , Note "Which rows a key writes over is asked for AT command time, and every set is the renderer's." [Test]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "A surface is momentary exactly where it declares an off." [Unguarded]
  , Note "The value palette is the one surface that is no place." [Unguarded]
  , Note "Three surfaces owe a row." [Unguarded]
  , Note "Modal reaches the sheet under a momentary surface." [Unguarded]
  , Note "No command reads both selections." [Unguarded]
  , Note "The table is the only surface that logs a flag." [Unguarded]
  , Note "The document pane is the one mount that is a Set rather than a renderer." [Unguarded]
  , Note "A side table out of step is dropped whole." [Unguarded]
  , Note "The value palette is a keystroke's answer rather than a place." [Unguarded]
  ]
-- * Sheets, document pane, Elm

-- ** ONE BUTTONLESS SHEET, and there are two of them
--
-- Both run the ladder written once (`saveSheet'/`leaveSheet'/`note') over a sheet
-- object; what differs stays in the verbs.

data SheetName  = ShMaterialize | ShSettings deriving (Eq, Ord, Show)
data FlushShape = OneHeadlinePost | PostPerMovedLayer deriving (Eq, Show)
data SheetState = Synced | Syncing | Conflict | Errored deriving (Eq, Ord, Show, Enum, Bounded)

-- | `{dirty, flush, refresh, shut, scope}' plus its own state word; the name, the log
--   scope its `note' writes under, and what ONE flush is.
data Sheet = Sh SheetName String FlushShape
shName  (Sh n _ _) = n
shScope (Sh _ s _) = s
shFlush (Sh _ _ f) = f

sheets :: [Sheet]
sheets = [ Sh ShMaterialize "sync"   OneHeadlinePost
         , Sh ShSettings    "config" PostPerMovedLayer ]

-- | `activeSheet' is total because neither sheet opens over the other.
activeSheet :: Bool -> Bool -> Maybe Sheet
activeSheet True  _    = Just (head sheets)
activeSheet False True = Just (last sheets)
activeSheet False False = Nothing

-- | The header word; the two that WAIT for a keystroke each spell the key that clears it.
headerWord :: SheetState -> String
headerWord Synced   = "synced"
headerWord Syncing  = "syncing..."
headerWord Conflict = "conflict - C-x C-s overwrite / ESC discard"
headerWord Errored  = "error - C-x C-s retry / ESC discard"

waitsForAKey :: SheetState -> Bool
waitsForAKey s = s == Conflict || s == Errored

-- | Dirty = the header's lists against the materialized original, and RAW mode's text:
--   the document pane commits per element.
sheetDirty :: Bool -> Bool -> Bool
sheetDirty listsMoved rawMoved = listsMoved || rawMoved

data Leave = ShutQuiet | FlushThenShut | ShutDiscarding | LeaveWaits deriving (Eq, Show)
-- | ESC or the backdrop, over state and dirt.  A conflict or an error discards.
leaveSheet :: SheetState -> Bool -> Leave
leaveSheet Conflict _    = ShutDiscarding
leaveSheet Errored  _    = ShutDiscarding
leaveSheet _       False = ShutQuiet
leaveSheet Syncing True  = LeaveWaits
leaveSheet Synced  True  = FlushThenShut

data Save = CommitElement | FlushIt | RefreshThenFlush | SaveWaits deriving (Eq, Show)
-- | `C-x C-s', over an open element and the state: mid-edit it commits, on a conflict it
--   overwrites, otherwise it flushes the sheet.
saveSheet :: Bool -> SheetState -> Save
saveSheet True  _        = CommitElement
saveSheet False Syncing  = SaveWaits
saveSheet False Conflict = RefreshThenFlush
saveSheet False _        = FlushIt

data CommitKey = SaveBuffer | OrgCommit deriving (Eq, Show)
-- | `C-c C-c' stops where the ELEMENT does; `C-x C-s' is the BUFFER's and reaches the
--   sheet with nothing open.
reaches :: CommitKey -> Bool -> Bool
reaches OrgCommit  open = open
reaches SaveBuffer _    = True

data SheetShape = TwoPane | RawOrg deriving (Eq, Show)
-- | `C-c '' swaps the two by RE-MATERIALIZING; a dirty sheet is refused (`sync first -
--   C-x C-s'), a local conversion needing the org parser this page keeps out.
swapShape :: SheetShape -> Bool -> Maybe SheetShape
swapShape _       True  = Nothing
swapShape TwoPane False = Just RawOrg
swapShape RawOrg  False = Just TwoPane

-- | Stash and restore, for a DIRTY sheet alone.
stashed :: [String]
stashed = ["both panes", "the shape", "where the cursor stood", "what an open edit held"]

data Pane = DocPane | PropPane deriving (Eq, Show)
data PaneHolds = HoldsBody | HoldsPropsAndPlanning deriving (Eq, Show)
-- | The cut is the SERVER's and a flush posts both back.
paneHolds :: Pane -> PaneHolds
paneHolds DocPane  = HoldsBody
paneHolds PropPane = HoldsPropsAndPlanning

data PopTier = PopBand | PopSheet deriving (Eq, Show)
-- | `.pop-band' grows with its content to the cap; `.pop-sheet' is FIXED on both axes.
popTiers :: [(String, PopTier)]
popTiers = [ ("state palette",    PopBand)
           , ("tag manager",      PopBand)
           , ("materialize sheet", PopSheet)
           , ("link popup",       PopSheet)
           , ("capture form",     PopSheet)
           , ("settings sheet",   PopSheet) ]

-- | `--g-pop-max' in vh: the foot margin is the HEAD's, derived from the anchor.
popMax :: Double -> Double
popMax top = min 90 (100 - 2 * top)

-- ** The document pane, and its Elm half

data Grain   = Element | Composite | Leaf deriving (Eq, Show)
data RowKind = DHead | DPara | DChild | DMetaRow deriving (Eq, Show)
-- ^ `DMetaRow' is Elm's `Meta': a planning or drawer row (`DMeta' names a denied dir).

-- | A STOP: the pane's row.  `was' is what it arrived as, so a draft that has not moved
--   off it writes nothing.
data Stop = Stop
  { stId :: String, stKind :: RowKind, stGrain :: Grain
  , stName :: Maybe String, stOwner :: Maybe String
  , stFrom :: Int, stTo :: Int, stText :: String, stWas :: String, stAlone :: Bool }

-- | The headline line's cells; a part the headline has not got renders NOTHING.
--   THE HEADLINE IS ONE STOP: each part has its own key and no rung walks into them.
docCells :: [String]
docCells = ["state", "priority", "title", "tags"]

-- | The markup the stylesheet and the harness read: a stop wearing its kind as `d-*',
--   point, a flag, a cell with its key, the marker org wrote with its steppable
--   bullet inside it, a checkbox, text, a link's shown half,
--   what no rung claims, and the strip that names the way back.
docClasses :: [String]
docClasses = [ ".de", ".dat", ".dfl", ".dc", ".dm", ".dbul", ".dbx", ".dt", ".dk", ".dl"
             , ".dg", ".dpath", ".dpunc", ".dlead", ".blk" ]

-- | Geometry written onto `#mdoc' as NUMBERS, the arithmetic staying in the stylesheet.
docVars :: [String]
docVars = ["--g-doc-indent", "--g-doc-rows", "--g-doc-off", "--g-doc-fs", "--g-doc-lh"]

-- | `org-hide-leading-stars' + `org-startup-indented': every star but the LAST is a
--   space.  Depth is RELATIVE to the entry the sheet stands on, and this is chrome ahead
--   of the state cell rather than a cell.
drawStars :: Int -> String
drawStars n = replicate (n - 1) ' ' ++ "*"

scrollOff :: Int          -- ^ `--g-doc-off', in the pane's OWN lines
scrollOff = 3
docRowsCap :: Int         -- ^ `DOCROWS', spelled in the shell and nowhere else
docRowsCap = 10
-- | The four doors that move `--g-doc-rows'; `shutEdit' gives the room back.
docRowDoors :: [String]
docRowDoors = ["the fill", "M-RET's splice at the caret", "the field's own input", "shutEdit"]

-- THE STEP HAS THREE COHORTS.  From a CHILD headline, `n'/`p' walk every visible
-- headline in document order -- org's next-visible-heading, a folded subtree skipped
-- whole.  From anything else they walk the rows owned by what owns point -- a leaf
-- its item run, an element its shelf -- clamped at the run's ends.  `p' OFF A SHELF'S
-- EDGE IS HEADLINE-SIZED: an element steps back over its own shelf while the shelf
-- has something above and lands on the NEAREST VISIBLE HEADLINE where it does not --
-- org's previous-visible-heading, so a body's first element climbs to its own
-- headline and the TAIL past every subtree is ONE press from the document's last
-- one.  A run's leaves are the composite's, so their edge still clamps.  THE ROOT IS
-- THE READER'S EXCEPTION: the entry's own line shares its contents' cohort, so `n'
-- steps into the body, while headlines walking up still land on it.  Contents are
-- otherwise behind `f'/`b'.

data Finer = IntoLeaves | Finest deriving (Eq, Show)
-- | `f' descends ONE rung; `Finest' refuses with an echo.  `l' and the right arrow are
--   aliases: three dialects, one axis.  THE GRAIN IS ASKED SECOND: a synthesized
--   header row answers by ID first (`planFiner'), the planning line being an
--   `Element' whose finer grain is no row at all.
finer :: Grain -> Finer
finer Composite = IntoLeaves
finer Element   = Finest
finer Leaf      = Finest

data Wider = ToOwner | ToEntryLine deriving (Eq, Show)
-- | `b' is the WIDEST rung, reversed expand-region, and NEVER a close: out of a leaf to
--   its IMMEDIATE owner by id, out of an element to the entry's own line.  ASKED
--   SECOND for `finer''s own reason: point standing IN a planning entry steps back
--   through the entries and off the FIRST one to the whole line, and only then does
--   the row grain answer.
wider :: Grain -> Wider
wider Leaf      = ToOwner
wider Composite = ToEntryLine
wider Element   = ToEntryLine

-- | ONE GRAIN SPEAKS FOR A RANGE: a leaf is left OUT of the splice whenever its owner
--   moved or is going, so flagging a list and one of its items is one deletion.  A
--   composite is drawn once with its leaves inside it and what no leaf claims is inert.
silenced :: Stop -> [Stop] -> Bool
silenced s moved = stOwner s `elem` map (Just . stId) moved

data Locator = Ground | InsetShadow | Underline | Border | Outline | DropShadow
  deriving (Eq, Show, Enum, Bounded)
-- | EVERY SELECTION IS A GROUND, and the ROW is the language.  A LOCATOR MUST NOT MOVE
--   THE TEXT, so an inset shadow is permitted and the FLAG is the one thing that draws
--   one.
locatorOk :: Locator -> Bool
locatorOk Ground      = True
locatorOk InsetShadow = True
locatorOk Underline   = False
locatorOk Border      = False
locatorOk Outline     = False
locatorOk DropShadow  = False

data DocKey = Ret | SRet | MRet | KPlus deriving (Eq, Show)
data DocPress = Commits | CommitsAndAsks | Newline | TypedChar | Inserts | OpensByKind
  deriving (Eq, Show)
-- | Over an OPEN box `RET' commits (org's `C-c C-c' under another name), `S-RET' commits
--   and asks for another stop, `M-RET' is the newline; outside one all three of `+',
--   `S-RET' and `M-RET' are the insert.
pressed :: DocKey -> Bool -> DocPress
pressed Ret  True  = Commits
pressed SRet True  = CommitsAndAsks
pressed MRet True  = Newline
pressed KPlus True  = TypedChar
pressed Ret  False = OpensByKind
pressed _    False = Inserts

data DocTarget = TChild | TPara | THeadline | TTitle | TState | TTags | TPriority
  deriving (Eq, Show, Enum, Bounded)
data Opens = Rematerialize | Textarea | TitleField | ValuePalette | TagsPopup | NamesTwoKeys
  deriving (Eq, Show)
-- | `RET' is BY KIND, and the headline LINE's edit is its title, so no `f' is spent
--   picking the cell.  A CHILD's cells are read-only: no row id, so no `/command'
--   addresses it.
retOn :: DocTarget -> Opens
retOn TChild    = Rematerialize
retOn TPara     = Textarea
retOn THeadline = TitleField
retOn TTitle    = TitleField
retOn TState    = ValuePalette
retOn TTags     = TagsPopup
retOn TPriority = NamesTwoKeys

data DelOut = ToParent | SheetDoor deriving (Eq, Show)
-- | `DEL' is UP, and the dispatch stands aside for a key this listener claimed or the
--   table's own `DEL' would strip a filter token on the same press.
delOn :: Bool -> DelOut
delOn True  = ToParent
delOn False = SheetDoor

data CheckBox = BoxEmpty | BoxX | BoxLower | BoxPartial deriving (Eq, Show, Enum, Bounded)
-- | `SPC' at the stop under point, and `C-c C-c' with nothing open is org's second
--   meaning of the key.  The box is the item's FIRST line and the write is the paragraph
--   splice over that item's lines alone.
flipBox :: CheckBox -> CheckBox
flipBox BoxEmpty   = BoxX
flipBox BoxX       = BoxEmpty
flipBox BoxLower   = BoxEmpty
flipBox BoxPartial = BoxX

boxes :: [String]
boxes = ["[ ]", "[X]", "[x]", "[-]"]

-- | `X' (Shift-x) HIDES DONE CHECKBOXES in the material doc: a display-only,
--   ephemeral UI mode (never an org edit, lost on reload) INDEPENDENT of the
--   `.focus' dim.  A LEAF -- a checkbox item owning no checkbox sublist -- is
--   hidden iff its box is ticked; an INTERIM item -- one that does own such a
--   sublist -- iff EVERY descendant leaf is, so one empty box anywhere keeps it
--   and every ancestor up to the run's root visible.  Non-checkbox content is
--   never hidden.  The set of hidden rows is a pure post-order over the owner
--   tree, so a box ticked while the mode is on hides its subtree live and
--   unticking re-shows it; point snaps off a swallowed row (next, else prev)
--   and `n'/`p' step past the set.
data Item = ILeaf CheckBox | IInterim [Item]

boxDone :: CheckBox -> Bool
boxDone BoxX       = True
boxDone BoxLower   = True
boxDone BoxEmpty   = False
boxDone BoxPartial = False

subtreeDone :: Item -> Bool
subtreeDone (ILeaf b)     = boxDone b
subtreeDone (IInterim ks) = all subtreeDone ks

data HideScope = ThisRun | AllRuns deriving (Eq, Show)
-- | `X' keys on the run's OUTERMOST list composite: point inside a list scopes
--   to that run alone (a nested sublist carries no composite of its own, so the
--   owner walk meets exactly one).  Point off every list is the MASTER toggle --
--   any run on turns them all off, none on turns them all on.
hideScope :: Bool -> HideScope
hideScope True  = ThisRun
hideScope False = AllRuns

-- | `drawText' walks the segments in order and SILENTLY DROPS a link opening inside the
--   previous one.  SPAN-driven, never search-driven; the shown text is the server's
--   `desc' verbatim and the range its `span'.
drawText :: [Span] -> [Span]
drawText = foldl' step []
  where step acc s@(Span a _) = case reverse acc of
          Span _ b : _ | a < b -> acc
          _                    -> acc ++ [s]

retryMs :: Int            -- ^ the one retry a lagging store gets
retryMs = 300
-- | THE STORE LAGS THE WRITE IT ANSWERS FOR: a re-read whose digest is not the write's
--   own receipt is DROPPED, the model the write was built from standing.
takesReread :: Digest -> Digest -> Bool
takesReread receipt got = receipt == got

-- | `d'/`D'/`u' over the document take PARAGRAPHS; a headline refuses with a log line.
docFlagged :: RowKind -> Bool
docFlagged DPara  = True
docFlagged DHead  = False
docFlagged DChild = False
docFlagged DMetaRow = False

data DocPort = PEdit | PDelete | PGrainKey deriving (Eq, Show)
-- | THE BODY A WRITE SENDS IS ELM'S ANSWER: a splice cannot be rebuilt out of the model
--   it just changed, so the rule lives once, where the model is.
docAnswers :: DocPort -> String
docAnswers PEdit     = "docBody"
docAnswers PDelete   = "docTook"
docAnswers PGrainKey = "docSaid"

-- ** ONE RECOGNIZER, AND THE TOTAL CASES OVER ITS SUM

data RegionKind = Plain | Item | Table | Block | Drawer
  deriving (Eq, Ord, Show, Enum, Bounded)
regionKinds :: [RegionKind]
regionKinds = [minBound .. maxBound]

-- | A region at a level: its kind, a block's own NAME, its lines, and the regions inside
--   it.  A REGION NEED NOT BE A STOP.
data Region = Region { rgKind :: RegionKind, rgName :: String, rgFrom :: Int, rgTo :: Int
                     , rgIn :: [Region] }

-- | ORG'S GREATER/LESSER SPLIT decides RE-ENTRY.  A table is greater in ORG and a leaf
--   here, the one name this walk departs on: org's `table' contains `table-row' alone.
greater :: RegionKind -> String -> Bool
greater Item   _ = True
greater Drawer _ = True
greater Block  n = notElem n verbatimBlocks
greater Table  _ = False
greater Plain  _ = False

-- | The five names `org-element-greater-elements' leaves out; every other `#+begin_X' is
--   a SPECIAL block and greater.  `org-list-forbidden-blocks' is a different variable and
--   a shorter list — four, sparing `comment' — and it answers about LISTS.
verbatimBlocks :: [String]
verbatimBlocks = ["comment", "example", "export", "src", "verse"]

-- | WHICH KINDS CARRY A CLOSER, the one reader of that set: the interior, whether a line
--   is the closing one, and whether an opener is one a run steps over whole.
closes :: RegionKind -> Bool
closes Block  = True
closes Drawer = True
closes Plain  = False
closes Item   = False
closes Table  = False

data Marker = NoMarker | ItemBullet | TableRow | BareIndent deriving (Eq, Show)
-- | ONE ARM PER KIND, org's own in each: the item's own indent and bullet with the number
--   continued and an EMPTY box after one, an empty ROW aligned to the table's own widths,
--   the block's or drawer's indent, and a paragraph's own emptiness.
markerFor :: RegionKind -> Marker
markerFor Plain  = NoMarker
markerFor Item   = ItemBullet
markerFor Table  = TableRow
markerFor Block  = BareIndent
markerFor Drawer = BareIndent

data Caret = AtMarkerEnd | InFirstCell deriving (Eq, Show)
-- | Point goes at the marker's END, a marker being a LEAD; a TABLE ROW closes with a pipe
--   typing past which opens a column, so point goes one space into the first cell.
caretIn :: Marker -> Caret
caretIn TableRow   = InFirstCell
caretIn NoMarker   = AtMarkerEnd
caretIn ItemBullet = AtMarkerEnd
caretIn BareIndent = AtMarkerEnd

-- | ONE PAST A REGION'S LAST INTERIOR LINE: a kind that closes keeps its closer out.
interiorEnd :: Region -> Int
interiorEnd r = if closes (rgKind r) then rgTo r - 1 else rgTo r

-- | A region's CLOSING line, where the kind has one.  A TABLE has none, so a caret on its
--   last row keeps the new row inside it — which is how a table is built.
closerAt :: Region -> Int -> Bool
closerAt r line = closes (rgKind r) && line == rgTo r - 1

-- | ONE WALK, TWO CONSUMERS: the region holding LINE.  A greater region is RE-ENTERED;
--   what no nested region claims is the greater one's, and so is a nested one's closing
--   line.  A line no region claims is prose of its own.
regionAt :: [Region] -> Int -> Region
regionAt rs line = case filter holds rs of
    r : _ | greater (rgKind r) (rgName r) -> inside r
          | otherwise                     -> r
    []                                    -> Region Plain "" line (line + 1) []
  where holds r  = rgFrom r <= line && line < rgTo r
        inside r = let inner = regionAt (rgIn r) line
                   in if rgKind inner == Plain || closerAt inner line then r else inner

-- | ITEMS TILE THE RUN they sit in, so a stop cut from a region is SNUG: it ends at its
--   last line with something on it.
snug :: [String] -> [String]
snug = reverse . dropWhile shBlank . reverse

-- | WHO IS OWED A BLANK is the REGION's, carried on the row: a paragraph stands between
--   blanks and every other region's line sits against its neighbours.
aloneIn :: RegionKind -> Bool
aloneIn Plain = True
aloneIn _     = False

-- | THE SEPARATOR IS THE SPLICE'S: a zero-width range ADDS lines, so both neighbours are
--   read at splice time.  `Nothing' is the entry's own line, the one place no blank is
--   owed above.
apart :: Bool -> Maybe String -> Bool
apart alone nb = alone && maybe False (not . shBlank) nb

-- | AND THE WORD FOR WHERE IT LANDS IS THE MODEL'S: read on the page, a table row inside
--   a list item was called "an item at this level".
regionWord :: RegionKind -> String
regionWord Item   = "an item at this level"
regionWord Table  = "a row in this table"
regionWord Block  = "a line in this block"
regionWord Drawer = "a line in this drawer"
regionWord Plain  = "a line here"

-- | What a paragraph riding PAST a stop is called.
pastWord :: Stop -> String
pastWord t = case stName t of
  Just n  -> "after the " ++ n
  Nothing -> if stGrain t == Composite then "after the block" else "after this paragraph"

-- ** THE INSERT

draftId :: String         -- ^ the row a paragraph waits in before it says anything
draftId = "D"

-- | The DRAWN row is ZERO-WIDTH and nothing is owed until it is filled; a box still
--   holding only its own marker writes nothing, and `draftRow' prepends NOTHING.
writesNothing :: Stop -> Bool
writesNothing s = stText s == stWas s

data Join = Join { jUnder :: String, jLine :: Int, jMarker :: Marker
                 , jOwner :: Maybe String, jAlone :: Bool, jWord :: String }

-- | WHERE `+' JOINS, the two questions told apart by whether a LINE was named.  `Nothing'
--   for a CHILD, whose bytes are outside this window, and for an id no row wears; the
--   HEADLINE's leads the body at line 1.
joinAt :: [Stop] -> Stop -> Stop -> Maybe (Int, Region) -> Maybe Join
joinAt stops top s caret = case stKind s of
  DChild -> Nothing
  DMetaRow -> Nothing
  DHead  -> Just (Join (stId s) 1 NoMarker Nothing True "at the top")
  DPara  -> Just (maybe (joinSibling top s) (uncurry (joinInside stops top)) caret)

-- | AN ABSENT index is `+' with no box open, so THE GRAIN SELECTS: a LIST LEAF joins
--   strictly below the stop wearing the stop's own opener (org's `M-RET'), and its `to'
--   already covers the run nested inside it; everything else rides past the whole
--   structure — the only way to a paragraph after a list.
joinSibling :: Stop -> Stop -> Join
joinSibling top s
  | stName top == Just "list" && stGrain s == Leaf =
      Join (stId s) (stTo s) ItemBullet (stOwner s) False (regionWord Item)
  | otherwise = Join (stId top) (stTo top) NoMarker Nothing True (pastWord top)

-- | THE REGION HOLDING THE CARET'S LINE ANSWERS BOTH HALVES — the marker, and the
--   interior IMMEDIATELY UNDER that line, so a run splits where the reader stands.  On a
--   CLOSING line it lands PAST the region wearing the continuation of whatever holds it.
joinInside :: [Stop] -> Stop -> Int -> Region -> Join
joinInside stops top line reg
  | rgKind reg == Plain || closerAt reg line =
      Join (stId top) (rgTo reg) NoMarker Nothing True (pastWord top)
  | otherwise = Join (stId top) (line + 1) (markerFor (rgKind reg))
                     (anchorOwner stops (line + 1)) (aloneIn (rgKind reg))
                     (regionWord (rgKind reg))

-- | THE OWNER IS THE ANCHORED LINE'S: the deepest stop holding the line above it, which
--   keeps the row order and the splice one answer.
anchorOwner :: [Stop] -> Int -> Maybe String
anchorOwner stops line =
  listToMaybe [ stId t | t <- reverse stops, stFrom t <= line - 1, line - 1 < stTo t ]

-- | A cursor is owed a LINE rather than an id across the write, block ids being
--   positional: the rescan mints the row.
landsOn :: Join -> Int
landsOn = jLine

-- | `Scan.at' READS AN `Array': ms over a 2,178-line corpus body, `List.drop i' being
--   O(i) and costing nine million cons cells.
lineReadMs :: [(String, Int)]
lineReadMs = [("List.drop", 75), ("Array.get", 11)]

-- ** THE SMALL LISTS ARE ONE ELM PROGRAM

-- | `listing(host, cols, hint, pane)'.  Each is a list of RECORDS under declared columns
--   with a cursor, optional delete flags, a click that selects and a `/' narrow.
data Mount = Mount { mHost :: String, mCols :: [String], mHint :: String, mPane :: String }

-- | The THREE mounts; what a row MEANS stays with the surface, so each keeps its own rows.
mounts :: [Mount]
mounts =
  [ Mount "ltable"  ["title", "url"]                     ""                      "lpane"
  , Mount "ttable"  ["title", "on", "rows"]              "d/D remove · u unflag" "tpane"
  , Mount "cstates" ["tag", "state", "group", "colour"]  "d/D remove · u unflag" "cstates" ]

-- | THE ONE LIST THAT IS NOT ELM'S is the table at this host: the renderer's own job.
tableHost :: String
tableHost = "app"

-- | The five calls a flag surface owes, over whichever program holds its rows.
flagPortCalls :: [String]
flagPortCalls = ["flagRow", "unflagRow", "getFlagged", "clearFlags", "selectStep"]

-- | The shape `flagKey', `stepIn' and `selectedId' already asked for, so none of them
--   learned that a mount became a program.
listingHandle :: [String]
listingHandle = flagPortCalls ++
  [ "getSelection", "setRows", "el"
  , "openNarrow", "shutNarrow", "narrowing", "narrowBox", "counted" ]

-- | FLAGS COME BACK IN THE ORDER THEY WERE LAID DOWN, oldest first, never in row order.
flaggedOrder :: [Id] -> [Id] -> [Id]
flaggedOrder laid _rows = laid

data CellIn = CellText String | CellInt Int | CellFloat Double
-- | A CELL IS DRAWN AS TEXT whatever it arrives as — a count is a NUMBER.
drawCell :: CellIn -> String
drawCell (CellText s)  = s
drawCell (CellInt n)   = show n
drawCell (CellFloat x) = show x

-- | `/' narrows ANY of them: `substring:''s rule verbatim, case-FOLDED, over the cells
--   the list DRAWS, joined the way `hrSearch' joins them, and NO GRAMMAR — a bar, a colon
--   and a leading `-' are the characters they spell.
narrowMatch :: String -> [String] -> Bool
narrowMatch q cells = shInfix (map toLower q) (map toLower (intercalate "\x1f" cells))

-- | The keys the field claims while it holds them; the surface's own bindings are
--   suspended and `DEL' is the field's own erase.
narrowKeys :: [String]
narrowKeys = ["RET", "C-n", "C-p", "the vertical arrows"]

data Narrowed = KeepsItsRow | FirstMatch | NoRow deriving (Eq, Show)
-- | The cursor KEEPS its row where the narrow spares it, lands on the first match where
--   it does not, and an empty answer leaves no row.
narrowed :: Bool -> Bool -> Narrowed
narrowed True  _     = KeepsItsRow
narrowed False True  = FirstMatch
narrowed False False = NoRow

-- ** The header in the pane: planning and the properties drawer

-- | The server LIFTS planning, the drawer and the logbook out of the body and sends the
--   first two as LISTS; the pane draws them back as SYNTHESIZED rows -- no span, no part
--   in the splice, edited as lists and carried by every write.  Ids: the planning line
--   `PLN', the drawer composite `PR', pair N `PR<n>'.
data PRow = PlanRow | DrawerRow | Pair Int
prowId :: PRow -> String
prowId PlanRow   = "PLN"
prowId DrawerRow = "PR"
prowId (Pair n)  = "PR" ++ show n

data PFiner = IntoEntries | ByGrain deriving (Eq, Show)
-- | What `f' reaches inside a synthesized header row: ONE DEVIATION, the rest deferring
--   to the grain law (`finer').  THE PLANNING LINE'S FINER GRAIN IS THE ENTRIES IT
--   DRAWS: it is one `Element' holding one text and no rows, so there is nothing for
--   the grain to descend into and the walk runs over the line's own list in
--   `planningRows' order -- `f' from the whole line takes the first entry, `f' from an
--   entry the next, and the LAST clamps, spoken and standing still.  POINT NEVER LEAVES
--   THE ROW: the entry is a SUB-ROW grain, so a row step drops it.  The drawer and its
--   pairs are ordinary rows and `finer' answers for them.
planFiner :: PRow -> PFiner
planFiner PlanRow   = IntoEntries
planFiner DrawerRow = ByGrain
planFiner (Pair _)  = ByGrain

-- | The planning line's keys, org's order, and the order `f' walks its entries in.
--   THE LINE IS ONE ROW WITH A GRAIN INSIDE IT (`planFiner'), drawn only when a pair
--   exists, and an edit that leaves a keyword valueless clears it.
planningRows :: [String]
planningRows = ["SCHEDULED", "DEADLINE", "CLOSED"]

data TakenAs = Dropped | Cleared deriving (Eq, Show)
-- | `d d' on a pair DROPS it through the lists; on the planning line it CLEARS the line;
--   on the drawer row it takes every pair.  All ride the doc's own flag shape.
taken :: PRow -> TakenAs
taken PlanRow = Cleared
taken _       = Dropped

data HeaderThing = APlanning | AProperty | AHidden | ALogbook deriving (Eq, Show, Enum, Bounded)
-- | The hidden properties are never drawn and never sent back reworded; the logbook is a
--   read-only strip under the pane, out of `dirty()' and never sent.
rowed :: HeaderThing -> Bool
rowed APlanning = True
rowed AProperty = True
rowed AHidden   = False
rowed ALogbook  = False

-- `RET' on a pair opens its LINE, org's own spelling `:KEY: value', and a line that
-- opens no key is refused.  `+' TYPES THE PAIR INLINE: a row is drawn at the drawer's
-- end and a two-field box covers it, dressed as the line it will become -- `:', TAB or
-- RET hands the key over to its value, TAB or RET from the value writes at once, and
-- ESC takes the box and the drawn row together.
-- A KEY THAT FOLDS TO ONE OF `planningRows' NEVER REACHES THE DRAWER: it is a planning
-- entry wearing a property's clothes, and the write ROUTES it to the planning line --
-- upcased, placed by the composer -- typed fresh in the box or standing in the drawer
-- already, where the commit MIGRATES it and both lists move on the ONE write the cargo
-- carries.  The KEY half offers the three beside the tree's own vocabulary, hinted:
-- `GET /properties' walks DRAWERS and no tree spells them there.

data PairGoes = ToDrawer | ToPlanning String deriving (Eq, Show)
-- | Where a pair LANDS.  The case is the typist's and the write's is org's, so a key
--   case-folds to the word and lands upcased.
pairGoes :: String -> PairGoes
pairGoes key | up `elem` planningRows = ToPlanning up
             | otherwise              = ToDrawer
  where up = map toUpper key

data PairWall = NoKey | KeyPunctuated | KeyReserved | KeyHidden | NoValue
              | PlanningUnreadable
  deriving (Eq, Show, Enum, Bounded)
-- | Why a typed pair is refused, or nothing.  EVERY wall is the BOX'S OWN: it
--   shuts before the model answers, so a wall only the model knew would leave a
--   drawn row with nothing left to reach it.  The two key lists are the parser's
--   and the store's -- `reservedProperties' is what terminates a drawer, and
--   `hiddenProperties' is the identity a row is found and linked by.  The last is
--   the SERVER'S OWN planning wall, spelled here because the box has to ask it.
pairRefused :: String -> String -> Maybe PairWall
pairRefused key value
  | null key                     = Just NoKey
  | any bad key                  = Just KeyPunctuated
  | up `elem` reservedProperties = Just KeyReserved
  | up `elem` hiddenProperties   = Just KeyHidden
  | null value                   = Just NoValue
  | ToPlanning _ <- pairGoes key
  , not (stampShaped value)      = Just PlanningUnreadable
  | otherwise                    = Nothing
  where up    = map toUpper key
        bad c = isSpace c || c == ':'

-- | Is VALUE a timestamp org reads back?  ONE bracketed stamp, or two joined by org's
--   `--' AND WEARING THE SAME BRACKET: the server's range parser takes the pair's
--   OPENING bracket again after the join, so `<..>--[..]' is a range in neither
--   direction.  THE BOX'S READING of the server's own wall, which reparses, and kept
--   NO LOOSER than it: a value this let through would meet the refusal with the box
--   already shut, which is the one shape the pair's walls exist to prevent.  Space
--   around the value is GONE BY HERE -- the box trims each half and a drawer line is
--   read trimmed -- and the server strips it too.  The English date phrases proposed
--   for the value widen this and nothing else.
stampShaped :: String -> Bool
stampShaped value = case afterStamp (dropWhile isSpace value) of
  Just (open, rest)
    | all isSpace rest        -> True
    | '-' : '-' : end <- rest -> case afterStamp end of
        Just (open', left) -> open' == open && all isSpace left
        Nothing            -> False
  _                           -> False

-- | The bracket ONE stamp off the front of S opened with, and what is left behind it,
--   or nothing.  A stamp ends at its own closing bracket, which is why a range needs
--   no lookahead and the `--7d' warning cookie inside one is never read as the range
--   join.  THE OPENING BRACKET RIDES OUT so a range can be asked to wear one kind.
afterStamp :: String -> Maybe (Char, String)
afterStamp []           = Nothing
afterStamp (open : ends) = case lookup open (map bracketChars [TsActive, TsInactive]) of
  Nothing    -> Nothing
  Just close -> case break (== close) ends of
    (inside, _ : after) | dateOpens inside -> Just (open, after)
    _                                      -> Nothing

-- | Does S open with org's @YYYY-MM-DD@?  THE SERVER READS EACH FIELD AS A DECIMAL
--   RUN and RANGE-CHECKS the month and the day, so `2026-8-1' opens a date, so does
--   `2026-08-01Sat' with no space before its weekday, and `2026-13-45' opens none.
--   Whatever weekday, time and cookies follow it are org's.
dateOpens :: String -> Bool
dateOpens s = case decimalAt s of
  Just (_year, '-' : s1)
    | Just (month, '-' : s2) <- decimalAt s1
    , Just (day, _rest)      <- decimalAt s2
      -> month >= 1 && month <= 12 && day >= 1 && day <= 31
  _   -> False

-- | The run of digits S opens with, read as a number, and what follows it.
decimalAt :: String -> Maybe (Integer, String)
decimalAt s = case span isDigit s of
  ([], _)    -> Nothing
  (ds, rest) -> Just (read ds, rest)

-- ** The settings sheet

-- | `SECTIONS' owns the names and the ORDER: the tab order, the URL FRAGMENT, and the
--   strip of buttons over one pane at a time.  A panel fills itself from the model on
--   arrival through its `enter' hook, and NO caller indexes this list by number.
data Sec = Sec { secTitle :: String, secParts :: [String], secEnter :: Bool }

secs :: [Sec]
secs = [ Sec "ui"       ["ctheme"]                   True
       , Sec "keywords" ["clayers", "ceff", "cfoot"] True ]

-- | The fields each panel draws.
secFields :: [(String, [String])]
secFields = [ ("ui",       ["#themesel", "#readsel", "#czoom", "the tree's own state hues"])
            , ("keywords", ["the layer select", "#ctext", "#ctpl", "#ceff", "#clab", "#clerr"]) ]

data SCol = SColTag | SColState | SColGroup | SColColour deriving (Eq, Show, Enum, Bounded)
data WriteTo = ItsLayer | SystemOrg deriving (Eq, Show)
-- | THE STATES TABLE, the fourth `listing' mount and the second MUTABLE one.  A STATE
--   rides its layer's write and a COLOUR rides `system.org''s, so one row can move two
--   files and both leave in the one flush; the tag is the layer and is read-only, which
--   is why `#sedit' has three fields.
statesWrite :: SCol -> Maybe WriteTo
statesWrite SColTag    = Nothing
statesWrite SColState  = Just ItsLayer
statesWrite SColGroup  = Just ItsLayer
statesWrite SColColour = Just SystemOrg

-- | One row per keyword the tree knows, BY LAYER then cycle order (system first, then the
--   tags alphabetically), actives before the done-like.  A word two layers declare is TWO
--   rows: a state belongs to a FILE.
data StateRow = StateRow { srLayer :: String, srKw :: Kw, srGroup :: String, srHue :: Maybe String }

-- | A keyword no config layer declares is listed under the tag `file': the tree
--   recognizes it and this sheet cannot move it, so it is there to be COLOURED and `d'
--   says so and leaves it standing.
movable :: StateRow -> Bool
movable r = srLayer r /= "file"

-- | ONE THEME CONTROL and the hues follow it: which theme they describe is DERIVED from
--   the reader's choice, `auto' resolving through the media query the boot line reads.
--   Storage stays per theme because READABILITY is.
hueTheme :: String -> String -> String
hueTheme "auto" sys = sys
hueTheme t     _    = t

-- | The wire carries a hue FLAT in both directions, so nothing iterates keys to read back
--   what it wrote; the model is `{theme: {keyword: hue}}' on the SYSTEM layer.
data Hue = Hue { hTheme :: String, hKw :: Kw, hHue :: String }

-- | `cmoved': the comparable NOW against what was SERVED, both read BEFORE the await, so
--   a keystroke landing mid-write leaves the sheet dirty.  The flush names a part only
--   where it moved.
cmoved :: String -> String -> Bool
cmoved now base = now /= base

-- ** Desktop

-- | Browser resolution is a fixed ladder, ENVIRONMENT FIRST — a machine set up once in a
--   shell profile is obeyed by every launcher — then the flag, then `PATH', each run as
--   `CMD --app=URL'; failing all of it `xdg-open URL', failing that the URL printed.
data Launch = FromEnv | FromFlag | OnPath String | XdgOpen | PrintUrl deriving (Eq, Show)

launchLadder :: [Launch]
launchLadder = [FromEnv, FromFlag] ++ map OnPath chromiumFamily ++ [XdgOpen, PrintUrl]

-- | Chromium-family, since `--app' is the flag that drops the chrome and Firefox no
--   longer has it.
chromiumFamily :: [String]
chromiumFamily =
  ["chromium", "chromium-browser", "google-chrome-stable", "google-chrome", "brave", "vivaldi"]

-- | The flag AND neither `$GLANCE_BROWSER' nor `--browser': an operator who wrote one
--   down meant it, and a build that grew a window since must not quietly ignore it.
prefersNative :: Bool -> Bool -> Bool -> Bool
prefersNative built env flag = built && not env && not flag

data DeskStage = Bind | Window | Walk | WatchStage deriving (Eq, Show)
-- | THE WINDOW OPENS AT THE SOCKET: the page is served immediately and `/headlines'
--   answers 503 while the walk runs, where waiting on the store is a blank screen for its
--   length.
deskOrder :: [DeskStage]
deskOrder = [Bind, Window, Walk, WatchStage]

windowFailureFatal :: Bool    -- ^ the server is the product; every step degrades
windowFailureFatal = False
exitNoSocket :: Int           -- ^ a daemon that stops before it listens has already said why
exitNoSocket = 1

data AfterWindow = StopDaemon | KeepServing deriving (Eq, Show)
-- | THE WINDOW IS THE APP: closing it stops the daemon, `--keep-serving' puts stage 1
--   back, and a window that never OPENED leaves the daemon serving — which
--   `gtk_init_check' is what makes reachable.
afterWindow :: Bool -> Bool -> AfterWindow
afterWindow False _     = KeepServing
afterWindow True  True  = KeepServing
afterWindow True  False = StopDaemon

-- | The reading pane, as a share of the main window: centred, transient, ESC or the
--   manager's close ending it, its own new-window navigating IN PLACE.
popupShare :: (Double, Double)
popupShare = (0.80, 0.90)

data Opening = ReadingPane | ShowUri deriving (Eq, Show)
-- | An `http(s)' target alone earns the pane; everything else goes to
--   `gtk_show_uri_on_window', and a URI that will not open is printed and dropped.
openTarget :: String -> Opening
openTarget u
  | isPrefixOf "http://" u || isPrefixOf "https://" u = ReadingPane
  | otherwise                                        = ShowUri

data OpenKind   = AnchorBlank | ScriptedOpen deriving (Eq, Show)
data WebKitDoor = NewWindowAction | CreateSignal deriving (Eq, Show, Enum, Bounded)
-- | TWO DOORS BECAUSE WEBKIT HAS TWO: a real `target="_blank"' anchor arrives as a policy
--   decision, a scripted `window.open' fires `create' INSTEAD.
webkitDoor :: OpenKind -> WebKitDoor
webkitDoor AnchorBlank  = NewWindowAction
webkitDoor ScriptedOpen = CreateSignal

-- | And only one may be ANSWERED with a window: connected, `create' dereferences the
--   scripted open's `WindowFeatures' optional, which `"noopener"' leaves disengaged — a
--   SIGABRT of the whole daemon.  Unconnected it drops the open silently.
answerable :: WebKitDoor -> Bool
answerable NewWindowAction = True
answerable CreateSignal    = False

-- | So the scripted half is intercepted ABOVE WebKit's window machinery: a document-start
--   user script in the TOP frame alone replaces `window.open' with a post to the `popup'
--   script-message handler and answers null, and the handler opens the pane.  A page read
--   INSIDE a popup keeps the real `window.open', inert there.
openOverrideFrames :: String
openOverrideFrames = "top frame alone"

-- ** helpers

shInfix :: String -> String -> Bool
shInfix n h = any (isPrefixOf n) (foldr (\c t -> (c : head t) : t) [[]] h)

shBlank :: String -> Bool
shBlank = all (`elem` " \t")

-- ** Notes

sheetNotes :: [Note]
sheetNotes =
  [ Note "`beforeunload' flushes with `keepalive' only when the sheet is dirty." [Test]
  , Note "The page holds no org parser and must not grow one." [Docs]
  , Note "TWO RESOLVERS, ONE TRUTH. Because the page spells no org, the date field's client reading is for THE GHOST'S PREVIEW ALONE: the commit sends the RAW TYPED TEXT, the server transforms at the planning wall, and the pane redraws off that answer. The two are DRIFT-PINNED over one shared corpus, `test/fixtures/english-dates.json', which both suites read -- a vector added there is owed an answer by both halves, so neither can grow a reading the other lacks." [Test]
  , Note "A DRAFT POSTS NOTHING, so a planning value it takes never comes back\
         \ TRANSFORMED the way a row's does — the reader's phrase used to stand in the\
         \ planning line until the capture landed.  `settleDraftPlan' is the draft's own\
         \ stand-in for that round trip: the entry is redrawn as the GHOST'S OWN READER\
         \ reads it, so the pane says what the file will hold.  WHAT TRAVELS IS STILL WHAT\
         \ WAS TYPED (`typedPlan'): the wall transforms ONCE against the server's clock, and\
         \ this reading is for ink, exactly as the ghost's is.  A phrase the resolver\
         \ refuses stays raw and meets the wall's own sentence; CLOSED is not settable and\
         \ keeps its verbatim value untouched." [Test]
  , Note "Movement relocates attention alone and `RET'/`DEL' are the context axis, which\
         \ is why the movement keys are the ones left out of `ONCE'." [Docs]
  , Note "The materialize sheet is ONE file — both panes, the ladder and the opening —\
         \ and it owns the open entry, the shape, and the two baselines dirt is measured\
         \ against." [Test]
  , Note "A CAPTURE IS THE SHEET OVER A SUBTREE THAT DOES NOT EXIST YET, and `capturing()'\
         \ is a fact about the HANDLE (`editing.capture') rather than a second mutable, so\
         \ every door below reads one flag.  NOTHING ABOUT A DRAFT IS OWED TO A FILE: it is\
         \ never dirty, so the leave-flush, the `beforeunload' keepalive and the socket's\
         \ reload all read it clean — which is the whole of what makes `ESC' free." [Test]
  , Note "THE BARE-DRAFT LAW: where the draft is star-space and nothing else the title box\
         \ IS the capture — `RET' on the typed title commits, so the inbox jot stays `+',\
         \ the line, `RET', and `ESC' there drops the capture whole rather than coming back\
         \ to a sheet standing over no file.  `bareCapture' is asked in ONE place, so the\
         \ commit and the escape cannot part.  A template with more than a bare headline\
         \ commits on `C-c C-c' alone, where `RET' just closes the title.  THE DESTINATION\
         \ TAG IS NOT ONE OF THE THINGS THAT MAKE A DRAFT RICH (`tagsBeyond'): it is the\
         \ address `+' already asked for, so a bare template under a tag is the bare draft\
         \ and the tagged jot is four keys too." [Test]
  , Note "EVERY DRAFT OPENS EDITING, at the place `%?' named (`showDraft', `openLanding'):\
         \ a reader who asked for a capture is composing one, and a pane that made them\
         \ press `RET' first asked a question `+' had answered.  Point on the HEADLINE is\
         \ the title edit, opened at once off the handle's own cells; a BODY line is the\
         \ paragraph editor over that line, opened when the fill settles, the row `%?'\
         \ named landing a macrotask behind the send.  The caret rests at the line's end,\
         \ `point' naming a LINE and no offset to aim at.  On the RICH draft the box is an\
         \ ordinary sheet edit and the STANDING LADDER holds: `RET' closes it, `C-c C-c'\
         \ behind it takes the capture, ESC closes it and the next ESC drops the draft." [Test]
  , Note "A HEADLINE ALWAYS DRAWS ITS TITLE CELL, empty or not (`drawnCells', Doc.elm):\
         \ that cell is the SLOT the title edit anchors in (`dTitleAt'), and a row that drew\
         \ none left the box anchored on the whole line — swallowing the star, the state,\
         \ the priority and the tag run it stands among.  Every other cell is drawn only\
         \ when it says something.  The capture surfaced this and never owned it: any\
         \ headline with an empty title wore it." [Browser]
  , Note "AN EMPTY TITLE IS NO COMMIT AND THE BOX STAYS UP behind the word, the wall every\
         \ other sheet edit's is asked at: above the shut, while what was typed is still on\
         \ screen to be fixed.  A REFUSED capture leaves the SHEET standing for the same\
         \ reason, and `C-c C-c' retries it." [Test]
  , Note "Over a bare draft that box FINALIZES A CAPTURE, so the pill names\
         \ `org-capture-finalize' under whichever key reached it rather than the title-write\
         \ it is not." [Test]
  , Note "ONE `note' SHORTHAND PER SHEET, each bound to the sheet it speaks for (`sync',\
         \ `capnote', `cnote'): `note' writes the sheet's own `state', so a verb reaching\
         \ for another sheet's shorthand moves a state its caller never owned." [Test]
  , Note "A CAPTURE SHEET IS A GHOST in `SURFACES': it stands over a draft no row id in a\
         \ URL could bring back, so it is remembered as NOTHING and the address stays the\
         \ view's own." [Test]
  , Note "A ROW-ADDRESSING COMMAND OVER A DRAFT LANDS IN THE HANDLE (`draftWrote'), which\
         \ answers in the wire's own shape: the state palette, the tags popup and the title\
         \ and priority doors are the doors they always were, and the empty digest rides\
         \ back untouched, being the create pin." [Test]
  , Note "The browser fixture tree carries a REAL `.org-glance' store and one tag layer\
         \ with a `#+TODO:' cycle and a template, so the capture cases read the org a\
         \ reader's own keystrokes wrote rather than a stub's answer — template expanded,\
         \ pair filled, date summoned, blob minted with its id, its shard path, its\
         \ creation drawer and its ledger line." [Browser]
  , Note "The sheet is one `SURFACES' entry, the fourth `flagKey' surface and the fourth\
         \ `openEdit' surface -- four of the seven shapes are its own, the title, the\
         \ paragraph, the drawer's pair and the planning date -- whose `anchor' is the\
         \ one thing a shape declares that a mount's does not." [Test]
  , Note "No popup box declares a width or a height of its own; `#mpanes' hides its\
         \ overflow and no pane carries a floor, `#mdoc' owning its scroll and the mounts\
         \ inside the other panes owning theirs." [Test]
  , Note "`@' IN THE SHEET IS THE WRITE HALF of the key the table READS with: the\
         \ overview drills into the rows referring to this one, the sheet links one\
         \ INTO the prose.  Two scopes, one sequence, and `live' keeps them apart." [Test]
  , Note "THE PICKER IS THE TABLE, SHRUNK: a table-view mount in the renderer's own\
         \ `inline' mode over `GET /refer', which is `/headlines'' own pipeline behind\
         \ `viewPage'.  The columns, the badge hues, the cursor, the filter grammar,\
         \ its suggestions and DEL arrive built, so no second table lives in the shell." [Browser]
  , Note "`inline' IS THE RENDERER'S OWN MODE: chips resident and the filter SUMMONED\
         \ onto their line, no title, no hint line, no sort marks, a capped window.  A\
         \ compact table drawn beside the renderer would fork the grammar at the first\
         \ fix." [Docs]
  , Note "ESC IN `inline' IS ONE STEP: the half-typed filter is dropped AND the cursor\
         \ lands on a row.  A compact table is a thing to pick FROM, so stopping at an\
         \ emptied box leaves the reader in an editor they were already done with." [Browser]
  , Note "TWO CUTS ARE THE PICKER'S, and both are cuts rather than refusals met\
         \ after choosing: a row with no `ORG_GLANCE_ID' cannot be linked to, and a\
         \ row is not its own reference." [Test]
  , Note "`@' IS A CHARACTER FIRST, so it is WRITTEN the moment it is typed and the\
         \ picker is what happens on top of it.  The binding claimed the key, so the\
         \ literal is the handler's to write; at a word boundary the picker rides\
         \ over it, mid-word an address typed into prose stays an address." [Browser]
  , Note "`K' DECLARES THE KIND, and `k' is the previous row in the vim dialect —\
         \ the three movement dialects outrank a mnemonic, so the kind takes the\
         \ shift.  It takes NO CHORD to reach: a prefixed `@' cannot be pressed over\
         \ a selected region, the dispatch leaving `C-c' to copy there, and a region\
         \ becoming the link is what layer 1 is FOR." [Browser]
  , Note "`K' AND `/ kind:' ARE ONE STATE: the chip is the control — DEL takes it\
         \ off like any other — and the badge is the readout.  `kind:' comes OUT of\
         \ the row query before it is sent, the kind saying what the link WILL BE\
         \ rather than narrowing the rows it is written from." [Browser]
  , Note "THE CURSOR GROUNDS THE ITEM, NOT WHAT HANGS OFF IT: a nested list item\
         \ is drawn INSIDE its parent, so the parent's element is as tall as its\
         \ whole subtree and a ground on it runs the lot.  The nested rows take the\
         \ pane's own ground back; a flagged one keeps its wash." [Browser]
  , Note "THE PLATFORM PAINTS THE FORM CONTROLS, and `color-scheme' is the only\
         \ thing that tells it which way: a `<select>' on a dark page that never\
         \ declares its scheme is drawn from the UA's LIGHT palette, and the page's\
         \ own inherited `color' over that is white on white — which is what the\
         \ native WebKitGTK window drew.  It rides the PALETTE BLOCKS, so a theme\
         \ cannot carry tokens without carrying the scheme beside them." [Test, Browser]
  , Note "THE BADGE IS AN OUTLINE where every other badge is a washed ground: a\
         \ state or a tag describes the ROW it sits on, and this describes the EDGE\
         \ about to be written.  Reading alike would say they were one kind of\
         \ thing." [Browser]
  , Note "THE SLUG IS THE SERVER'S, said once: the picker sends a kind as it was\
         \ typed and writes back what comes home, so `kindSlug' is not spelled a\
         \ second time on the page and the file holds what org-glance would have\
         \ written.  `GET /refer' echoes it beside the `kinds' it offers." [Test]
  , Note "AN EMPTIED SUMMONED BOX IS DEL'S FIRST RUNG: `inline''s editor was the last\
         \ thing the reader put there, so it is the first thing taken back.  The chips\
         \ under it are the PICKER's own rung — its listener, not the table's\
         \ `filter-drop-token', which is dead while a surface is up — reached once the\
         \ box has gone.  A RESIDENT box has no such rung and walks the chips from the\
         \ first press." [Browser]
  , Note "ESC ABANDONS, DEL DELETES: dismissing the picker leaves the `@' standing —\
         \ the reader typed a character and keeps it — where DEL's last rung takes it\
         \ away, the rung after the typed text, the summoned box and the chips.  A REGION IS THE\
         \ EXCEPTION to both: no `@' is written over the words the link is to read\
         \ as, so neither key has one to leave or take." [Browser]
  , Note "A SELECTED REGION BECOMES THE LINK and its own words are what the link\
         \ READS AS.  They are no query: seeding the filter with them narrows the\
         \ store by an accident of phrasing and puts the reader's prose on the chip\
         \ strip.  A region is as explicit as a prefix, so no boundary gates it." [Browser]
  , Note "ONE BOX TAKES THE WRITE, WHICHEVER IS OPEN: a title edit takes the link into\
         \ the TITLE, a paragraph at its caret, and with neither open `insertHere' —\
         \ `+''s own path — draws the row and opens one.  The sheet's existing commit\
         \ is the only write route." [Browser]
  , Note "The picker CLAIMS its keys with `stopPropagation', not `preventDefault'\
         \ alone: taking a row SHUTS it, so a listener further along would find no\
         \ momentary surface up and read the same RET as its own — the sheet would\
         \ commit the paragraph the link had just been written into." [Browser]
  , Note "SCROLLING IS SILENT: the surfaces that scroll — `.tv-scroll' and `#kbd' —\
         \ hide the bar in both spellings, `scrollbar-width' for Firefox and the\
         \ `::-webkit-scrollbar' pseudo for Chromium.  A classic bar takes LAYOUT width,\
         \ which the fill column then loses and the sideways scroll begins a bar early." [Browser]
  , Note "The left pane is a list of KINDS, so it is no table-view mount." [Docs]
  , Note "The openers are the corpus's — `-', `1.'/`1)', `+' and an INDENTED `*' — and a\
         \ block is any `#+begin_X' with a matching `#+end_X' BY NAME." [Elm]
  , Note "ONE blank line stays inside a list; two, or a blank with something else under\
         \ it, close it.  An item deeper than the first rides INSIDE the item above." [Elm]
  , Note "An opener with no closer is ordinary text, and a paragraph ends at the next\
         \ STRUCTURE as readily as at a blank line." [Elm]
  , Note "`listRun' steps over a block or a drawer WHOLE, org's own `org-list-struct'\
         \ rule, so no item boundary is cut through one." [Elm]
  , Note "THE DISCIPLINE IS NON-WIDENING: the walk reads the same predicates everything\
         \ else does, so text org declines — a bullet with no space, an unclosed\
         \ `#+begin_', a pipe row under a blank line, `:a:b:' — takes the prose answer." [Elm]
  , Note "The marker is SEEDED ONCE from the drawn row, so it is on screen while the line\
         \ is typed and a second seeding cannot overwrite the typing." [Elm]
  , Note "A table's leaf is a LINE, cut inline, rules included: no cell grain and no\
         \ column awareness." [Elm]
  , Note "THE PRICE IS PINNED: a caret on an item's own wrapped prose answers with that\
         \ item's bullet, so `S-RET' there cuts the paragraph and org reads one more\
         \ item; which of the two it should be is undecided." [Elm]
  , Note "Closing the top level's drawer arm means minting a stop there, which re-cuts\
         \ the pane over every drawer in the corpus." [Elm]
  , Note "`drawText' rests on a non-overlap guarantee only `subtreeLinks' can give." [Unguarded]
  , Note "Links ride the materialize — `GET /headline' carries the row's whole scan — so\
         \ the display is compact from the FIRST frame; links are no stops and bind no\
         \ mouse, `o' sharing the same held answer." [Test]
  , Note "`scrollIntoView' is forbidden over the TABLE's rows and ordinary here; the\
         \ suite keeps the distinction by COUNTING call sites -- ONE, which is what\
         \ a page with no layout degrades to." [Test]
  , Note "THE READING LINE: after a move, point's row whose BOTTOM has fallen below a\
         \ line drawn `glance-reading-line' percent down the pane comes to rest ON\
         \ that line, so there is always text under what is being read.\
         \ DIRECTION-FREE, and if possible -- the pane clamps at its end.  A row\
         \ TALLER than the band above the line, and one standing ABOVE the pane's\
         \ top, keep the `nearest' ask, which is what the three-line scroll-margin\
         \ already covers.  THE SCROLLER IS ASKED FOR RATHER THAN NAMED: `#mdoc'\
         \ stretches to `#mpanes' where the document fits its box and grows past it\
         \ -- `#mpanes' clipping -- where it does not.  The percent is browser-local\
         \ like the theme and banded 20-90, outside which no band stands above the\
         \ line for a row to rest in." [Browser, Test]
  , Note "`C-l' IS `recenter-top-bottom', org's own cycle: point's row to the pane's\
         \ MIDDLE, then its TOP under the three-line band, then its BOTTOM above it,\
         \ and round again.  ANY OTHER KEY STARTS THE CYCLE OVER, which is the whole\
         \ of what makes a run of presses one gesture.  ONE PLACEMENT LAW SERVES IT\
         \ AND THE READING LINE BOTH (`placeRow'): an edge of the row on a line\
         \ measured down the band.  The key is CLAIMED, the browser owning `C-l' for\
         \ its address bar; it stays reserved on the table, where no pane stands." [Browser, Test]
  , Note "`M-<left>'/`M-<right>' OVER A NESTED HEADLINE are org's own\
         \ `org-promote-subtree' and `org-demote-subtree': the child's headline line\
         \ and every headline line inside its EXTENT gain or lose ONE star, the\
         \ contents untouched.  A LINE REWRITE the `Para' splice cannot express, so\
         \ the rewritten lines ride the ordinary commit cargo -- `bodyText' walks\
         \ `Para' alone and passes clean rows through, which is what lets one door\
         \ write both.  THE ENTRY'S OWN LINE REFUSES, its level being the table's\
         \ row, and so does a row that is no headline.  THE NARROWING WALL IS A\
         \ FLOOR: a direct child promoted would leave the subtree, so nothing\
         \ shallower than a child of the entry is reachable; demoting has no\
         \ ceiling.  POINT DOES NOT MOVE -- a child's id is its POSITION among the\
         \ file's headlines, which no shift changes." [Elm, Test, Browser]
  , Note "Content sits under the head's own TITLE column, by PADDING: a margin would take\
         \ the selection wash off the left of the line and a `text-indent' would indent a\
         \ block's first line alone." [Test]
  , Note "A headline line is a flex row where a paragraph is flowing text, the tags\
         \ flushed to the far edge by `margin-left:auto' (`org-tags-column')." [Test]
  , Note "An edit box IS the block it covers, so the lines under it move down; the field\
         \ renders in the PANE's own metrics, a count in the popup fields' reserving\
         \ 19.5px a line for 20.8px of text." [Test]
  , Note "`placeEdit' sizes the box on all four edges off the row's own rect and computed\
         \ padding; the stylesheet's span is the fallback for a page that measured\
         \ nothing, which stands one line tall." [Test]
  , Note "TAB WALKS THE RUNGS AN ITEM MAY SIT ON while its edit is open: its own,\
         \ one deeper -- which needs a PREVIOUS SIBLING to hang under -- and one\
         \ shallower, which needs a parent.  Illegal rungs are skipped and the walk\
         \ WRAPS, so `+' then TAB makes a child and TAB again undoes it from the\
         \ keyboard alone." [Test]
  , Note "THE SUBTREE RIDES ALONG.  A row's own line is spliced over its own extent,\
         \ so re-indenting it without moving what hangs off it reparents the lot to\
         \ whatever stands above; `bodyText' shifts every line from the own line to\
         \ `r.to' by the same delta." [Test]
  , Note "TAB FOLDS, as it does in org: on or in a drawer it toggles the fold, in an open\
         \ item edit it walks the rungs, and the model answers `nothing folds here'\
         \ everywhere else.  The browser would move focus, so the key is claimed." [Test]
  , Note "A DRAWER IS A STOP the reader points at and folds: its composite wears the\
         \ drawer's own name, an inner line is a leaf and a nested closed region ONE\
         \ leaf -- a finer take would strand its closer.  FOLDED IS THE DEFAULT, org's\
         \ own ellipsis; `f' into a folded drawer opens it, RET on the frame refuses\
         \ -- reserved -- POINT IS NEVER HIDDEN, and what the reader folded or opened\
         \ stays so across the rescan." [Elm, Browser]
  , Note "A CHILD HEADLINE FOLDS TOO, org's own cycle: TAB hides its subtree WHOLE --\
         \ ownership settles the hidden set in one ordered pass -- and the folded line\
         \ wears the ellipsis.  A child arrives OPEN; only drawers start folded.  `f'\
         \ on the headline enters the body, everything being under it." [Elm, Browser]
  , Note "A CHILD IS A HEADLINE AT EVERY DEPTH: it wears the headline's own face, the\
         \ path strip names it by its TITLE and walks through it -- a rung of the way\
         \ back, whatever its grain -- and `f' enters its body the way the root's\
         \ does.  The fold class stays the drawer's own: a child folds without\
         \ wearing `d-drawer'." [Elm, Browser]
  , Note "A SHELF INDENTS UNDER ITS OWN FIRST LETTER -- the cleaned stars' width, the\
         \ root's own geometry -- so stars and contents step together and never cross.\
         \ A BLOCK IS AN ELEMENT and its spine its own edge: one unbroken bar the\
         \ block's whole height, margins and deeper blocks included, so the root's\
         \ runs past every nested headline.  Its ink is F'S RAMP, the spike's winner:\
         \ rank 0 the block point is in, a step dimmer per shelf out, other branches\
         \ resting, a flag over all.  A headline draws no mark and, SELECTED, lights\
         \ the block it carries; the `up'/`sib' tiers stay flat for the text." [Elm, Browser]
  , Note "A LIST RUN WEARS A SPINE, the blocks' own grammar one storey down: every\
         \ item bars its whole extent at its run's rail, siblings stack into one\
         \ unbroken bar, and a nested run adds its column inside the parent item's.\
         \ ORG'S BULLET ALWAYS PAINTS -- the marker is content, never hidden -- and\
         \ the retired `glance-bullets' look stamps nothing." [Test, Browser]
  , Note "THE PANE IS A NARROWING: what is written stays INSIDE the materialized\
         \ subtree.  A typed headline at the root's level or above is DEMOTED to the\
         \ first child level -- org's narrowed buffer -- and nothing outside the\
         \ subtree is ever touched." [Elm, Browser]
  , Note "A RESERVED TOKEN ALIGNS ON ITS LETTER: the colons are punctuation, dimmed,\
         \ the leading one hanging into the gutter.  THE RAIL DOES NOT BREAK AT THE\
         \ DRAWER: the composite wears the paragraph's bar, and a pair's gutter bar\
         \ sits at the same x -- `rail + 1.5ch' is the indent at every shelf." [Browser]
  , Note "THE HEADER THE SERVER LIFTS IS DRAWN BACK: planning and the properties drawer\
         \ arrive as LISTS beside the body and their rows are SYNTHESIZED -- no span,\
         \ no part in the splice, `bodyText' walking `Para' alone -- and are edited as\
         \ lists.  A pair edits as its own `:KEY: value' line; `d d' drops a pair\
         \ through the lists; the frame and the keys wear point's ink, org's\
         \ `org-special-keyword'.  `+' TYPES A FRESH PAIR INLINE: a row drawn at the\
         \ drawer's end, which opens for it, under a two-field box dressed as the\
         \ `:KEY: value' line it will become -- `:', TAB or RET hands the key over to\
         \ its value, TAB or RET from the value writes at once, both halves complete\
         \ from the tree's own vocabulary (`GET /properties'), and ESC takes the box\
         \ and the row together, the drawer the bytes it was.  THE DRAWN ROW JOINS NO\
         \ LIST: `props' is what a flush writes, so a half-typed key in it would land\
         \ on disk the moment the sheet is left." [Elm, Test, Browser]
  , Note "EVERY WALL A TYPED PAIR MEETS IS THE BOX'S OWN (`pairRefused'), and the box\
         \ stays up behind each so what was typed is there to be fixed: an empty half,\
         \ a key holding a space or a colon, `reservedProperties' -- writing `:END:'\
         \ as a key would close the drawer over everything under it -- and\
         \ `hiddenProperties', which would forge the identity a row is linked by.\
         \ Neither list guarded a typed key before: the ask took `END' and wrote it.\
         \ A PLANNING KEY'S VALUE MEETS ONE MORE, the server's own reparse spelled\
         \ where the box can ask it (`stampShaped'): a value that does not read back\
         \ stops being a planning entry on the next load, and the write's 409 would\
         \ carry these very words to a box already shut." [Test, Browser]
  , Note "A KEY THAT CASE-FOLDS TO SCHEDULED, DEADLINE OR CLOSED IS NO PROPERTY\
         \ (`pairGoes'): it is a planning entry wearing a property's clothes, so the\
         \ commit routes it to the PLANNING LINE -- upcased, set or replaced where the\
         \ composer puts it -- and the drawer is left the bytes it was.  BOTH DOORS\
         \ ROUTE: the inline pair box writes one there instead of appending to the\
         \ drawer, and `RET' on a pair another writer minted into a drawer MIGRATES it\
         \ -- the drawer entry off and the planning entry set on the ONE write, since\
         \ the cargo carries both lists.  BOTH DOORS WEAR THE VALUE'S WALL as well\
         \ (`stampShaped'), asked ABOVE the shut so a value org would not read back is\
         \ refused with the line still on screen.  An emptied value CLEARS the entry,\
         \ org's own way, and the DRAWER door alone can spell one: the pair box owes\
         \ both halves.  The three ride the key half's offers out of `CFG.planning',\
         \ hinted\
         \ and upcased, since `GET /properties' walks drawers and the parser lifts\
         \ planning off the headline before one is read." [Elm, Test, Browser]
  , Note "THE COMMIT CARRIES ITS OWN CARGO: `docBody' and `docTook' hand the shell\
         \ body, properties and planning TOGETHER, since a flush reading the shell's\
         \ mirrors would race the state push for them.  THE WORD FOR A REROUTED EDIT\
         \ RIDES THE SAME CARGO: two ports carry no order between them, so a second\
         \ one would race this and the caller's wording would win the race.  THE\
         \ BASELINE COMES OFF THE FILL for the same reason: the mirrors land a\
         \ macrotask behind it, and a baseline read off them called every fresh sheet\
         \ dirty." [Elm, Test]
  , Note "A RELOAD NEVER LANDS OVER AN OPEN EDIT, checked when the frame arrives AND\
         \ when the fetch returns: the reader can open one while it flies, and the\
         \ refill would shut the box over their caret.  Staleness is the drift lock's\
         \ to catch at the commit." [Browser]
  , Note "A DRIVER THAT WATCHED THE DRAW MUST SEE THE MIRROR AGREE BEFORE A KEY: one\
         \ update schedules the redraw on rAF and the port push a macrotask apart, and\
         \ a key in that gap acts on the row the reader just left.  Rows carry\
         \ `data-id', the mirror's cursor is `docAtNow', and the harness's walks\
         \ require the two to agree -- docs/bugs/2026-08-19." [Browser]
  , Note "THE PANE IS THE SUBTREE: every descendant is drawn WHOLE -- its headline row,\
         \ its blocks under it -- each segment cut at the next child's line, the line\
         \ arriving with the child since only the server knows what it lifted.  A\
         \ child's blocks are spliced by the same door the entry's own are.  A SHELF\
         \ INDENTS UNDER ITS OWN STAR: a row carries its headline's level, and the\
         \ indent and the bar are that level's -- org's own geometry, the root's being\
         \ the stylesheet's default." [Test, Browser]
  , Note "THE STEP HAS THREE COHORTS: from a CHILD headline `n'/`p' walk every visible\
         \ headline in document order -- org's next-visible-heading, a folded subtree\
         \ skipped whole; from anything else, the ROOT's own line included, they walk\
         \ the rows owned by what owns point -- a leaf its item run, an element its\
         \ shelf -- so `n' from the top reads into the entry's contents; and `p' off a\
         \ SHELF'S EDGE is headline-sized -- an element steps back over its own shelf\
         \ while the shelf has something above and lands on the nearest visible\
         \ headline where it does not, so a body's first element climbs to its own\
         \ headline and the TAIL past every subtree is one press from the document's\
         \ last one, a folded subtree counting as its line alone.  A run's leaves\
         \ answer to the composite, so their edge still clamps.  `f' steps into what\
         \ point owns, `b' climbs to the owner.  THE VERTICAL ARROWS, `j'/`k' AND\
         \ `C-n'/`C-p' ALIAS `n'/`p' wherever this page walks rows -- one `rowStep',\
         \ four dialects -- and the chord pair is the half that survives a field\
         \ beside it, which is all a narrowed list is left with." [Elm, Test, Browser]
  , Note "THE PLANNING LINE HAS A GRAIN INSIDE IT (`planFiner'), and it is the only\
         \ row that does: one `Element' holding one text and no rows, so `f' walks the\
         \ ENTRIES it draws rather than descending into anything.  `f' from the whole\
         \ line takes the first, `f' from an entry the next, and the LAST clamps in the\
         \ house's own word (`grain-finer (at the finest)'); the echo counts them the\
         \ way a run's leaves are counted, `grain-finer (SCHEDULED 1/3)'.  `b' steps\
         \ back along the entries and off the FIRST one to `grain-broader (the planning\
         \ line)', where the row grain answers again.  POINT NEVER LEAVES THE ROW: the\
         \ entry is a SUB-ROW grain, so `n'/`p', a named landing and a summon's draft\
         \ each drop it -- it is this line's own axis and does not ride to another\
         \ row." [Elm, Test]
  , Note "`planKey' IS THE MIRROR OF THAT AXIS, and the shell reads no index of its\
         \ own: the model sends the KEYWORD of the entry point stands in, NULL for the\
         \ whole line, so the page cannot hold an entry the model has dropped.  It is\
         \ the one thing the shell needs off the walk -- which widget to raise -- and\
         \ spelling it as a keyword rather than a position is what keeps the two from\
         \ disagreeing about a line that was rewritten under them." [Elm, Test]
  , Note "`RET' ON THE PLANNING LINE SPLITS BY THE SAME AXIS, the frame's own rule one\
         \ grain finer: over an ENTRY it raises the very widget `C-c C-s' raises, keyed\
         \ by that entry, so the two doors are one box and one wall and there is no key\
         \ of its own to learn; over the WHOLE LINE it is INERT and names the way in\
         \ (`f' reaches the entries).  A line of entries is not one value, so nothing\
         \ edits it whole -- and the door that could have cleared every entry at a\
         \ stroke went with that; the widget clears them one at a time." [Test, Browser]
  , Note "THE TAIL IS THE PANE'S LAST ROW: one empty line past everything,\
         \ hidden until the walk reaches it (`.dat' shows it), zero-width and\
         \ `alone', so an edit splices a fresh paragraph\
         \ at the document's end -- the door `+' cannot be when the body ends in\
         \ something owned, a properties drawer above all.  It has no span, so the\
         \ flag and delete doors refuse it like a planning row, and the headline\
         \ walk ends on it." [Elm, Browser]
  , Note "`o' OPENS A ROW'S REACH: an element its own span, a HEADLINE the whole\
         \ subtree under it -- the root's reach the entry -- so the links door\
         \ answers from where the reader stands." [Elm, Test]
  , Note "ONE LIST OF POPUP SURFACES, `Glance.Web.Page.Popups': the veil, the `.on'\
         \ rule, the box sizing, the stale wash and the tier sweep all join it, so a\
         \ surface added there joins them by itself.  Six readers spelled the\
         \ membership by hand until the mint commit edited six of the seven and left\
         \ `#mint' neither fading nor dimming." [Test]
  , Note "THE WAY BACK IS NAMED AS WELL AS DRAWN: `.dpath' rides the pane's top,\
         \ sticky inside the scroller, and names the same chain the connectors draw --\
         \ a composite by its NAME, anything else by its own line with the marker taken\
         \ off.  The last crumb is point, in the ink point's own connector takes, so a\
         \ crumb and its rail agree." [Browser]
  , Note "THE MARKS HAVE A COLUMN OF THEIR OWN: the pane keeps a character the text\
         \ never enters, and every bar sits at its shelf's rail, half a cell left of\
         \ the tab stop." [Test, Browser]
  , Note "ONE INK PER ROW: a tier sets `--ink' on the row and the run's bar SPENDS it.\
         \ A case reads the tier rather than the shape, so one assertion covers the\
         \ mark however it is drawn." [Browser]
  , Note "A WHOLE NUMBER OF PIXELS PER LINE: `--g-doc-lh' is a LENGTH, since a 1px\
         \ hairline and a hinted glyph land on one device row only when every row\
         \ starts at the same sub-pixel offset, and 13 x 1.6 is 20.8.  The gaps around\
         \ a paragraph are whole pixels for the same reason." [Browser]
  , Note "ONE CURSOR, ONE GROUND: the stop under point wears `--g-sel', the ground the\
         \ table's cursor wears, whatever kind it is.  A NESTED ROW IS DRAWN INSIDE ITS\
         \ PARENT, so the page's own ground is given back to it or the cursor would run\
         \ the whole subtree; a COMPOSITE is the exception, the list itself being the\
         \ stop, so what it grounds is its rows." [Test, Browser]
  , Note "A CONTINUATION LANDS UNDER THE ITEM'S OWN TEXT, the checkbox counted with\
         \ the bullet: org reads a continuation by its INDENT.  Setting `value' fires\
         \ no `input', so the newline places the box itself -- the listener that\
         \ re-lays it after typing never runs for `M-RET'.  THE BOX MAKES ROOM BY THE\
         \ ROWS THE TEXT OCCUPIES, wrapping counted: org's newlines alone left it a\
         \ line short over a wrapped item and the continuation was typed out of sight.\
         \ `scrollHeight' never reads under the height the box already stands at, so\
         \ the field is collapsed for the measure and put back." [Test, Browser]
  , Note "A DELETION LANDS ON THE NEXT SIBLING, and on the PARENT only when the branch\
         \ is emptied: the row point stood on is about to stop existing, and the reader\
         \ was working among its siblings.  A LINE rather than an id -- the rescan\
         \ mints new ones -- and a line BELOW the cut has moved up by what the splice\
         \ actually dropped, counted rather than guessed." [Test]
  , Note "EVERYTHING IS UNDER THE HEADLINE, so the strip's way back starts there:\
         \ `headline -> list -> item'.  The entry's own line is the root the list and\
         \ the prose alike hang off." [Browser]
  , Note "A FLAG TAKES THE BRANCH: what hangs off a flagged row goes with it wherever\
         \ the flag leads -- a delete takes the subtree -- so the mark runs the whole\
         \ way down." [Browser]
  , Note "A SIBLING IS THE CHOICE THE READER IS STANDING IN, so it stays readable and\
         \ ITS OWN BRANCH COMES WITH IT -- a branch whose contents are dimmed is one\
         \ they cannot weigh.  `sib' is the row sharing point's owner." [Browser]
  , Note "FULL INK UNTIL THE READER GOES INTO A BLOCK -- a list run, a drawer's pairs,\
         \ a child's contents.  Dimming answers WHICH BRANCH AM I IN, so it engages\
         \ when point's row has an owner: `focus' rides the program's own root and\
         \ every row off the path drops to `--g-point-off', text and marks alike." [Test, Browser]
  , Note "`--g-point-off' is the ink nobody is looking at, PICKED per theme -- dark's\
         \ a deep blue, light's a pale one -- since a mix toward the ground lands on\
         \ brown in one and on nothing in the other." [Test]
  , Note "THE PATH, NOT THE LEVEL: what lights is point's OWNERS (`up'), not every\
         \ sibling of every owner -- that lit whole levels and said nothing about the\
         \ way back.  THE BARS STEP: an enclosing run's bar rides the accent ramp by\
         \ its distance out (`up-K'), while the TEXT of the path stays flat -- dimming\
         \ the rest is what makes it read.  What point CARRIES takes the page's ink,\
         \ and a COMPOSITE at point GROUNDS the rows it opens." [Browser]
  , Note "THE MARKS SIT OUTSIDE THE GROUND: a bar stands one tab stop LEFT of the\
         \ row's own text, so point's ground and its marks never cover each other." [Test, Browser]
  , Note "A TICKED BOX WEARS THE DONE FACE, `--g-state-i0': a settled keyword's hue,\
         \ said in one glyph.  An EMPTY box wears its line's ink, saying nothing yet,\
         \ and a dimmed line dims both -- `> .dp' scopes the exceptions, since rows\
         \ NEST and `.up .dbx' reaches every box under an owner of point rather than\
         \ the one on its own line." [Browser]
  , Note "THE OUTERMOST RUNG STANDS UNDER THE HEADLINE'S STARS: a connector is drawn\
         \ under the MARKER of the line it hangs off, the stars for the outermost and\
         \ the parent's bullet below that -- the COLUMN, whether or not the bullet\
         \ paints." [Test]
  , Note "A MARKER TAKES COLOUR ALONE.  A bolder bullet sits taller than the line it\
         \ opens and reads as a different FACE; the pane's business is which line.\
         \ A link carries its own ink, which outranks what it inherits, so a dimmed\
         \ line keeps a lit link inside it until the link is named too." [Browser]
  , Note "WHAT STANDS ON THE GROUND READS OVER IT: a marker on point's own line takes\
         \ the PAGE's ink, since point's hue is the ground's hue in the light theme and\
         \ a marker painted in it went missing -- the ordinals with it." [Browser]
  , Note "THE HEADLINE IS THE ROOT OF THE PATH: the way back runs headline, list,\
         \ owner, point, so it keeps its ink whichever list the reader stands in." [Browser]
  , Note "THE CHECKBOX IS PART OF THE MARKER: `- [X]' is one thing the reader points\
         \ at, so the marker runs the indent, the bullet and the box with its gap --\
         \ `.dm' over the first two and `.dbx' over the box.  What point CARRIES wears\
         \ its marker in the ink its connector takes." [Browser]
  , Note "`X' HIDES DONE CHECKBOXES ('subtreeDone', 'hideScope'): a ticked leaf and\
         \ any interim item all of whose leaves are ticked wear `.d-hidden'\
         \ (display:none), the walk and point stepping past them; a WHOLLY-DONE run\
         \ hides its own composite too ('subtreeDone' reads it like an interim), so\
         \ the container vanishes rather than leaving an empty box.  The mode is a\
         \ client-side Set of run-root ids that survives a rescan -- so ticking the\
         \ last box hides its branch live -- but never the org text, and is gone on\
         \ reload.  Point inside a list scopes to that run; off every list is the\
         \ master toggle.  INDEPENDENT of `.focus'." [Browser]
  , Note "THE MARKER ORG WROTE IS THE MARKER: a headline under point draws no connector,\
         \ its stars sitting in the column one would use, and an item keeps whatever org\
         \ wrote -- `-', `+', `*', `1.', `1)'.  Every one of them reads over point's\
         \ ground in the page's ink; `> .dp >' keeps the rule on the row's OWN line." [Browser]
  , Note "EVERY MARK IS OPAQUE: bars and spines overlap at shared columns, and a\
         \ translucent one composites darker where two meet, the same resting mark\
         \ reading as two styles." [Test]
  , Note "THE RUNG AND THE STEP TRAVEL AS CLASSES -- `lvl-top', `sp-N', `up-K' -- since\
         \ `Html.Attributes.style' assigns `style[key]' and browsers ignore that for a\
         \ custom property.  NOT `d-top': the harness reads a row's KIND off its `d-'\
         \ classes." [Test]
  , Note "`--g-point' is the cursor's INK and `--g-sel' the ground both surfaces wear: a\
         \ GROUND HUE IS NOT AN INK, and dark's selection is a slate that vanishes as a\
         \ hairline.  LIGHT SPENDS NO GOLD ON TEXT -- gold is its ground, so its point\
         \ ink is a deep blue that reads on the page AND on the ground, while dark keeps\
         \ an amber.  `--g-point-dim' is PICKED per theme for the same reason." [Test]
  , Note "THE HEADLINE IS ONE STOP: `f' does not walk into its parts, since each part\
         \ already has a key -- `t' the state, `:' the tags, S-<up>/S-<down> the priority\
         \ and RET the title.  A cursor over a part would be a second way to say the same\
         \ thing, and the one that goes stale when the part is absent." [Test]
  , Note "A CURSOR IS ONLY DRAWN WHERE THE KEYS ARE; the POSITION is not gated, being the\
         \ model's, and a FLAG keeps its mark either way." [Test]
  , Note "Elm pushes a port BEFORE it paints, so `keepInView' and `placeEdit' run a turn\
         \ later and the cursor anchor is read off the DRAW." [Browser]
  , Note "`Doc.elm' owns the scanner, the regions, the parse, the splice, the two-axis\
         \ cursor, the grain ladder and the flags, and draws them; the shell keeps the\
         \ keys, the two edit overlays, the writes and the actions." [Elm]
  , Note "`Scan.elm' is the pure half, split so it can be ASKED — functions over lines\
         \ rather than the Model, extensible records keeping every call site as it was." [Elm]
  , Note "`dmount' is a port shape where it was a `Set', and the gesture never knew the\
         \ difference." [Test]
  , Note "The harness reads the pane off what it DREW, taking its stops off a SELECTOR, so\
         \ the pane is free to wear the wrapper an Elm mount adds." [Test]
  , Note "A CELL EDIT RE-PINS the same way: the digest comes off the `/command' answer's\
         \ own per-id one, the frame that would re-read being guarded off under an open\
         \ edit; what it wrote comes back through the WATCH." [Test]
  , Note "The sheet's keys register AHEAD of the dispatch and fall through on every key\
         \ they do not claim; `preventDefault' fires only where a binding does, and only\
         \ over an open subtree sheet." [Test]
  , Note "The sheet keeps exactly one variable of its own, `--dk-mono' (Hack first)." [Test]
  , Note "The settings sheet's TAB walks the panels and wraps, `S-TAB' back, the newly\
         \ shown panel's first control taking the focus; a hidden panel is out of the\
         \ flow, so its fields leave the tab order with it.  It opens on the theme\
         \ panel's first field and blurs on the way out." [Test]
  , Note "The horizontal arrows walk the tab strip while a tab button holds the focus, and\
         \ the sheet's listener claims nothing while the sheet is shut or a momentary\
         \ popup stands over it." [Test]
  , Note "A `parts' id the markup lacks throws at boot: the panels are markup wrapped at\
         \ boot rather than built from the list, laid out by class." [Test]
  , Note "TWO EDITORS, ONE CYCLE: `takeLayer' reads the keywords box only while its own\
         \ panel is showing, and the page renders the one `#+TODO:' line." [Test]
  , Note "`%' in the template box raises the value palette over the SERVER's code list, so\
         \ the completion cannot offer a code the expansion does not know: the call declares\
         \ that vocabulary CLOSED, which is what keeps the typed line out of the list." [Test]
  , Note "A refusal SELECTS its layer, so the box shows the file the message describes." [Test]
  , Note "KNOWN GAP: the gear was the coarse pointer's only settings door and went with\
         \ the corner; `,' cannot be typed there." [Unguarded]
  , Note "`assets/elm.js' is a committed BUILD INPUT carrying both programs, embedded by\
         \ its own splice and named as the page's THIRD script; `make elm' reproduces the\
         \ committed bytes over an ephemeral `npx --yes elm', and `elm.json' must say\
         \ 0.19.2, 0.19.1 being a hard refusal." [Typed]
  , Note "Nothing in the Haskell suite rebuilds the Elm, so what is asserted offline is\
         \ that the bundle carries every program the target NAMES and that each source is\
         \ on disk." [Test]
  , Note "`make elm-test' is out of `cabal test': elm-test fetches its dependency at run\
         \ time and the Haskell suite stays offline.  Its oracles say only what they\
         \ check — `pairsUp' is a line count and `tableRuns' a second line reading." [Elm]
  , Note "Bottom-up ordering keeps most composite-and-leaf cases right on its own; what\
         \ it cannot survive is a leaf splice that CHANGES THE LINE COUNT under it." [Elm]
  , Note "The harness reads every small list off what it DREW, the counters asking WHICH\
         \ list an init was for, off the host element." [Test]
  , Note "The wire carries 38 KB gzipped of Elm runtime (182 KB raw), under the renderer's\
         \ 78 KB and the shell's 41 KB; minifying would take it to 13 KB and nothing\
         \ does." [Docs]
  , Note "`--dry-run' resolves and exits BEFORE binding, the native path replacing one\
         \ line of the same output rather than writing its own." [Test]
  , Note "GTK owns the main thread, so `runNative' forks the daemon and hands this thread\
         \ to the window; `Ctrl-C' stops both through a handler that asks the GTK loop to\
         \ quit and puts the previous handler back." [Unguarded]
  , Note "The downcast to the navigation policy decision is CHECKED, and every other\
         \ decision type is left to WebKit so ordinary navigation and the socket upgrade\
         \ are untouched." [Unguarded]
  , Note "The script-message shape is WKWebView's own, so the iOS/Android ports inherit\
         \ the design instead of a GTK-ism." [Docs]
  , Note "The bindings are vendored — six lines across two packages: `pkgconfig-depends'\
         \ to the 4.1 spelling, `Setup.hs''s version, and soup2 to soup3 — and both keep\
         \ upstream's name and version so a local package shadows Hackage's." [Typed]
  , Note "`vendored/gir/' carries the hand-written GIR XML for cairo, xlib and freetype,\
         \ which haskell-gi searches BEFORE the system path." [Typed]
  , Note "KNOWN GAP: the native window has been compiled, never opened — the chords in a\
         \ chrome-less view, the first paint, the close rules, the SIGINT handler and\
         \ `gtk_init_check' over a missing display are all the eyeball list." [Unguarded]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "The array reading is the one the module took." [Unguarded]
  , Note "The narrow claims exactly four keys." [Unguarded]
  , Note "Panel row ids cannot collide." [Unguarded]
  , Note "An http(s) target alone earns the pane." [Unguarded]
  , Note "Two doors, and only the policy one may be answered with a window." [Unguarded]
  ]
-- * Build and discipline

-- ** Components -- seven stanzas, one direction

data Vis = Public | Private | Exe | Suite deriving (Eq, Show)
data Component = Component { coName :: String, coDir :: Path, coVis :: Vis, coDeps :: [String] }
-- ^ `coDeps' is the INTRA-PACKAGE half of `build-depends'; the rest is Hackage.

components :: [Component]
components =
  [ Component "glance-internal"       "src/"                Private []
  , Component "glance"                "src-query/"          Public  ["glance-internal"]
  , Component "glance-web"            "src-web/"            Private ["glance"]
  , Component "glance-migrate"        "src-migrate/"        Private ["glance","glance-internal"]
  , Component "glance-desktop-native" "src-desktop-native/" Private []
  , Component "exe:glance"            "app/"                Exe     ["glance-desktop-native","glance-internal","glance-migrate","glance-web"]
  , Component "exe:glance-wasm-probe" "app/"                Exe     ["glance"]
  , Component "glance-test"           "test/"               Suite   ["glance","glance-desktop-native","glance-internal","glance-migrate","glance-web"]
  ]
-- ^ The wasm probe is buildable under `pure-crypto' alone (@buildable: False@
-- otherwise), so it costs the ordinary build nothing and is a stanza like any
-- other here.  THE SUITE REACHES THE NATIVE STANZA and costs nothing for it:
-- that stanza is base-only while `native-window' is off, and the flagged build
-- needs GTK for the executable anyway.

compDeps :: String -> [String]
compDeps n = concat [ coDeps c | c <- components, coName c == n ]
webTargets :: [String]
webTargets = [ coName c | c <- components, coDir c `elem` ["src-web/","src-desktop-native/"] ]

acyclic :: (a -> [a]) -> [a] -> Bool
acyclic edge xs = all (go (length xs)) xs
  where go n x = n > (0 :: Int) && all (go (n - 1)) (edge x)

-- ** Inside glance-web: `Base' the floor, `Glance.Web' the door

data WMod = WBase | WKeymap | WThemeTypes | WThemeDefault | WTheme | WPopups | WStyle
          | WGlue | WPage
          | WFilter | WSort | WColumns | WStore | WWatch | WCommands | WRoutes | WWeb
          | WDesktop | WNative deriving (Eq, Show, Enum, Bounded)
wmods :: [WMod]
wmods = [minBound .. maxBound]

wname :: WMod -> String
wname WBase         = "Glance.Web.Base"
wname WKeymap       = "Glance.Web.Keymap"
wname WThemeTypes   = "Glance.Web.Theme.Types"
wname WThemeDefault = "Glance.Web.Theme.Default"
wname WTheme        = "Glance.Web.Theme"
wname WPopups       = "Glance.Web.Page.Popups"
wname WStyle        = "Glance.Web.Page.Style"
wname WGlue         = "Glance.Web.Page.Glue"
wname WPage         = "Glance.Web.Page"
wname WFilter       = "Glance.Web.Filter"
wname WSort         = "Glance.Web.Sort"
wname WColumns      = "Glance.Web.Columns"
wname WStore        = "Glance.Web.Store"
wname WWatch        = "Glance.Web.Watch"
wname WCommands     = "Glance.Web.Commands"
wname WRoutes       = "Glance.Web.Routes"
wname WWeb          = "Glance.Web"
wname WDesktop      = "Glance.Desktop"
wname WNative       = "Glance.Desktop.Native"

-- | What a module reads INSIDE the component; `Glance.Query' is outside it.
wimports :: WMod -> [WMod]
wimports WBase         = []
wimports WKeymap       = [WBase]
wimports WThemeTypes   = []
wimports WThemeDefault = [WThemeTypes]
wimports WTheme        = [WThemeDefault, WThemeTypes]
wimports WPopups       = []
wimports WStyle        = [WBase, WPopups, WTheme]
wimports WGlue         = [WBase]
wimports WPage         = [WBase, WKeymap, WTheme, WGlue, WPopups, WStyle]
wimports WFilter       = []
wimports WSort         = [WFilter]
wimports WColumns      = [WFilter]
wimports WStore        = []
wimports WWatch        = [WStore]
wimports WCommands     = [WBase, WStore, WWatch]
wimports WRoutes       = [WBase, WCommands, WFilter, WSort, WColumns, WPage, WStyle, WTheme, WStore, WWatch]
wimports WWeb          = [WBase, WRoutes, WStore, WWatch]
wimports WDesktop      = [WWeb, WWatch]
wimports WNative       = [WDesktop, WWeb, WWatch]

-- | `Base' is exactly what more than one module above needs.
webFloor :: [String]
webFloor = ["ServeOptions", "response constructors", "body reader", "write-refusal vocabulary"]
webTH :: [WMod]
webTH = [WRoutes, WStyle]         -- ^ the modules carrying `TemplateHaskell'
webExposed :: [String]
webExposed =
  [ "Glance.Desktop", "Glance.Desktop.Native", "Glance.Web", "Glance.Web.Base"
  , "Glance.Web.Columns", "Glance.Web.Commands", "Glance.Web.Filter", "Glance.Web.Keymap"
  , "Glance.Web.Page", "Glance.Web.Page.Glue", "Glance.Web.Page.Popups"
  , "Glance.Web.Page.Style", "Glance.Web.Routes"
  , "Glance.Web.Sort", "Glance.Web.Store", "Glance.Web.Theme", "Glance.Web.Theme.Default"
  , "Glance.Web.Theme.Types", "Glance.Web.Watch" ]

-- ** Assets: what the binary embeds, and who may touch it

-- | @assets/@ holds the BYTES a build embeds and nothing else; front-end SOURCE
-- lives under @frontend/@.  The glue is both at once — its source bytes are the
-- embedded bytes, no build step between them — and it is filed as source.
data Origin = Hand | Sibling | Built deriving (Eq, Show)
data BuildAsset = BuildAsset
  { baPath :: Path, baOrigin :: Origin, baRefresh :: Maybe String, baSplice :: Maybe WMod }

buildAssets :: [BuildAsset]
buildAssets =
  [ BuildAsset "assets/table-view.js"   Sibling (Just "make sync-renderer") (Just WRoutes)
  , BuildAsset "assets/elm.js"          Built   (Just "make elm")           (Just WRoutes)
  , BuildAsset "assets/page.css"        Hand    Nothing                     (Just WStyle)
  , BuildAsset "frontend/glue/*.js"     Hand    Nothing                     (Just WRoutes)
  , BuildAsset "frontend/jsconfig.json" Hand    Nothing                     Nothing
  ]

-- | ORDER IS DATA: the splice folds this, `tsc' checks the same files, and a
-- name with no file fails the build.
gluePartFiles :: [Path]
gluePartFiles =
  [ "00-core.js"      -- the config blob, the log strip, the wash, fetching, the query, the crumbs
  , "05-keys.js"      -- key naming and the echo pill
  , "20-sheet.js"     -- the materialize sheet: both panes, the ladder, the opening
  , "30-capture.js"   -- the capture form and the value palette
  , "40-popups.js"    -- the link popup and the tags popup
  , "50-settings.js"  -- tabs, saved views, the states table, the theme
  , "60-refer.js"     -- `@' in the sheet: the reference picker over /refer
  , "70-shell.js"     -- the modal surfaces, the dispatch and the boot
  ]
-- | The same list as `tsc' reads it: a part named once cannot drift into two.
jsconfigFiles :: [Path]
jsconfigFiles = map ("glue/" <>) gluePartFiles <> ["glue.d.ts"]

sdistExtras :: [Path]
sdistExtras = ["assets/table-view.js", "assets/elm.js", "assets/page.css"]

-- | `openBinaryTempFile' splits at the LAST dot, so the suffix IS the
-- leftover's extension and a half-written document is out of the walk's reach.
tempSuffix :: String
tempSuffix = ".glance-tmp"
tempName :: Path -> String -> Path
tempName p rnd = p ++ rnd ++ tempSuffix

-- ** The native-window flag, and the project file that satisfies it

data CabalFlag = CabalFlag
  { flName :: String, flManual :: Bool, flOn :: Bool, flStanza :: String
  , flCpp :: [String], flPkgs :: Int, flGiPkgs :: Int }

flags :: [CabalFlag]
-- ^ Both MANUAL and both OFF: the solver turns neither on, so the ordinary
-- build is byte-identical whatever they permit.  `pure-crypto' swaps crypton
-- for a pure digest and entropy (SHA, random), which is what lets the core
-- build on wasm32-wasi at all.
flags = [ CabalFlag "native-window" True False "glance-desktop-native" ["-DNATIVE_WINDOW"] 28 25
        , CabalFlag "pure-crypto"   True False "glance-internal"       ["-DPURE_CRYPTO"]    2  0 ]

data Proj = DefaultProj | NativeProj deriving (Eq, Show, Enum, Bounded)
projFile :: Proj -> Path
projFile DefaultProj = "cabal.project"
projFile NativeProj  = "cabal.project.native"
projPackages :: Proj -> [Path]
projPackages DefaultProj = []
projPackages NativeProj  = ["vendored/gi-webkit2", "vendored/gi-javascriptcore4"]
projFlags :: Proj -> [(String, Bool)]
projFlags DefaultProj = []
projFlags NativeProj  = [("native-window", True)]
projBuildDir :: Proj -> Path
projBuildDir DefaultProj = "dist-newstyle"
projBuildDir NativeProj  = "dist-newstyle-native"
projGir :: Proj -> Maybe Path
projGir DefaultProj = Nothing
projGir NativeProj  = Just "vendored/gir"      -- ^ HASKELL_GI_GIR_SEARCH_PATH, searched FIRST
vendoredGirs :: [String]
vendoredGirs = ["cairo-1.0", "xlib-2.0", "freetype2-2.0"]

-- | The chosen spelling and the ambiguous one it replaces: `gi-webkit2' names
-- the former, so the pair keeps one `GI.Gtk' in the plan.
giSpellings :: [(String, String)]
giSpellings = [("gi-gtk3", "gi-gtk"), ("gi-gdk3", "gi-gdk")]

data Rekey = Rekey { rkLib :: String, rkFrom :: String, rkTo :: String }
rekeys :: [Rekey]
rekeys = [ Rekey "glib2" "2.88.1" "2.88.3", Rekey "webkit2gtk-4.1" "2.52.4" "2.52.5" ]
rekeyedEntries, untouchedHelpers :: Int
rekeyedEntries   = 17      -- ^ store entries built from a `.pc' that moved
untouchedHelpers = 10      -- ^ pure-Haskell packages beside them

-- ** Proposals: the directory tells the status, the name the date

data Status = Proposed | Partial | Done | Expired | Draft deriving (Eq, Ord, Show, Enum, Bounded)
statusWord :: Status -> String
statusWord Proposed = "proposed"
statusWord Partial  = "partial"
statusWord Done     = "done"
statusWord Expired  = "expired"
statusWord Draft    = "draft"

data Ymd = Ymd Int Int Int deriving (Eq, Ord)
ymdText :: Ymd -> String
ymdText (Ymd y m d) = show y ++ "-" ++ pad m ++ "-" ++ pad d
  where pad n = (if n < 10 then "0" else "") ++ show n

data Proposal = Proposal Ymd String Status
-- ^ There is NO path field: the filename, the `**Date:**' line and the
-- `**Status:**' line are three readings of one record, so no two can drift.
proposalDir :: Path
proposalDir = "docs/proposals/"
proposalPath :: Proposal -> Path
proposalPath (Proposal d s st) = proposalDir ++ statusWord st ++ "/" ++ ymdText d ++ "-" ++ s ++ ".md"
dateLine, statusLine :: Proposal -> String
dateLine   (Proposal d _ _)  = "**Date:** " ++ ymdText d
statusLine (Proposal _ _ st) = "**Status:** " ++ statusWord st
-- | The ONE move a proposal ever owes: a date never changes, its directory does.
retitle :: Status -> Proposal -> Proposal
retitle st (Proposal d s _) = Proposal d s st
siblingPath :: Proposal -> Path                  -- ^ org-glance spells one shape, in org
siblingPath p = take (length s - 3) s ++ ".org" where s = proposalPath p
proposalsMoved :: Int
proposalsMoved = 37                              -- ^ renamed on one day, the cost paid in full once

-- ** CHANGELOG: a feature earns a line, a cut promotes it

data Change = Added | Changed | Fixed deriving (Eq, Show, Enum, Bounded)
data ChangeEntry = ChangeEntry Change String     -- ^ user-visible behaviour, ONE line per feature
newtype Ver = Ver String deriving (Eq, Show)
data Release = Unreleased [ChangeEntry] | Released Ymd Ver [ChangeEntry]
-- | Promote `Unreleased' whole; what is left behind is empty.
cutRelease :: Ymd -> Ver -> Release -> (Release, Release)
cutRelease d v (Unreleased es)  = (Released d v es, Unreleased [])
cutRelease _ _ r@(Released {})  = (r, Unreleased [])
versionSites :: [Path]                           -- ^ a cut bumps every one of them
versionSites = ["CHANGELOG.md", "glance.cabal", "README.org"]
entriesOf :: Release -> [ChangeEntry]
entriesOf (Unreleased es)    = es
entriesOf (Released _ _ es)  = es

-- ** Prose budget: comment:code, target 10%

data LocBucket = BSource | BTests | BDocs | BFixtures | BGenerated | BVendored
  deriving (Eq, Ord, Show, Enum, Bounded)
locOf :: Path -> LocBucket
locOf p
  | p == "assets/table-view.js"                            = BVendored
  | p == "assets/elm.js"                                   = BGenerated
  | "test/fixtures/" `isPrefixOf` p                        = BFixtures
  | "docs/" `isPrefixOf` p                                 = BDocs
  | "README" `isPrefixOf` p || "CHANGELOG" `isPrefixOf` p  = BDocs
  | ".md" `isSuffixOf` p || ".org" `isSuffixOf` p          = BDocs
  | "test/" `isPrefixOf` p                                 = BTests
  | otherwise                                              = BSource
authoredBuckets :: [LocBucket]                   -- ^ other people's lines are counted apart
authoredBuckets = [ b | b <- [minBound .. maxBound], b /= BGenerated, b /= BVendored ]

data Loc = Loc { lcBucket :: LocBucket, lcFiles :: Int, lcLines :: Int, lcComment :: Int, lcBlank :: Int }
lcCode :: Loc -> Int                             -- ^ derived; the tool has no code column of its own
lcCode l = lcLines l - lcComment l - lcBlank l
locTable :: [Loc]
locTable =
  [ Loc BSource    253 40016 11444 4283
  , Loc BTests      27 26028  6582 2539
  , Loc BDocs       51 19348     5 1880
  , Loc BFixtures   12  2754  1088   20
  , Loc BGenerated   1  9481   259  902
  , Loc BVendored    1  5173  2561  228
  ]
authoredLoc :: [Loc]
authoredLoc = [ l | l <- locTable, lcBucket l `elem` authoredBuckets ]
authoredFiles, authoredLines, authoredCode, authoredComment :: Int
authoredFiles   = sum (map lcFiles authoredLoc)
authoredLines   = sum (map lcLines authoredLoc)
authoredCode    = sum (map lcCode authoredLoc)
authoredComment = sum (map lcComment authoredLoc)
reportedAuthored :: (Int, Int, Int, Int)         -- ^ what `make loc' prints, banner-side
reportedAuthored = (343, 88146, 60305, 19119)
proseTarget :: Int
proseTarget = 10                                 -- ^ percent of CODE, blanks excluded
commentBudget, overBy, proseTenths :: Int
commentBudget = authoredCode * proseTarget `div` 100
overBy        = authoredComment - commentBudget
proseTenths   = 1000 * authoredComment `div` authoredCode
shellNow, shellWas :: Int                        -- ^ the shell's own density, then and now
shellNow = 6
shellWas = 49

data LineKind = BlankL | CodeL | CommentL deriving (Eq, Show)
-- | A line carrying code AND a trailing comment is CODE -- the cut a compaction
-- pass moves.
commentMarker :: Path -> Maybe String
commentMarker p
  | any (`isSuffixOf` p) [".hs", ".elm", ".cabal"]           = Just "--"
  | any (`isSuffixOf` p) [".js", ".mjs", ".ts"]              = Just "//"
  | ".el" `isSuffixOf` p                                     = Just ";"
  | any (`isSuffixOf` p) [".sh", ".yml", ".yaml", ".json5"]  = Just "#"
  | any (`isSuffixOf` p) ["Makefile", "Eask", ".mk"]         = Just "#"
  | "tools/" `isPrefixOf` p                                  = Just "#"
  | otherwise                                                = Nothing
classifyLine :: Maybe String -> String -> LineKind
classifyLine m s
  | null t                                  = BlankL
  | "{-" `isPrefixOf` t || "/*" `isPrefixOf` t = CommentL
  | Just k <- m, k `isPrefixOf` t           = CommentL
  | otherwise                               = CodeL
  where t = dropWhile (`elem` " \t") s

-- ** Identity, and the prose rules

newtype Email = Email String deriving (Eq, Show)
data Ident = Ident String Email
author :: Ident
author = Ident "Dmitry Akatov" (Email "dmitry.akatov@protonmail.com")
authorEmail :: String
authorEmail = e where Ident _ (Email e) = author
identitySites :: [String]
identitySites = ["commit message", "Author: header", "Maintainer: header", "code comment", "generated text"]

data Earns = EOrder | EQuirk | EHazard | EXref | ETsc deriving (Eq, Show, Enum, Bounded)
-- ^ The five grounds a comment survives on: an ordering constraint, a browser
-- quirk, a hazard, a cross-reference, and what `tsc' reads.
data Cut = CBloat | CRedundant | CRestated | CQualifier deriving (Eq, Show, Enum, Bounded)
-- ^ What a compaction takes out: over-explanation, redundancy, a clause
-- restating a prior one, a qualifier that does not change meaning.
data CommentRule = CommentRule { crEarns :: [Earns], crCuts :: [Cut], crHome :: [Path] }
commentRule :: CommentRule
commentRule = CommentRule [minBound .. maxBound] [minBound .. maxBound] ["AGENTS.hs"]
-- ^ Every rule a comment used to restate lives at `crHome', where one copy can
-- be kept true.

within :: String -> String -> Bool
within n h = any (n `isPrefixOf`) (drops h) where drops s = s : if null s then [] else drops (tail s)
-- | The banned shape, in any spelling: state the point directly instead.
negationReveal :: String -> Bool
negationReveal s = any (`within` t) ["not just", "isn't about", "is not about"]
                   || ("not " `within` t && ", but " `within` t)
  where t = map toLower s

data Docstring = Docstring { dsFirst :: String, dsArgs :: [String], dsRest :: [String] }
-- | checkdoc's shape: a complete first line, arg names in CAPS, facts intact.
checkdoc :: Docstring -> Bool
checkdoc d = complete (dsFirst d) && all caps (dsArgs d)
             && not (any negationReveal (dsFirst d : dsRest d))
  where complete l = not (null l) && last l == '.' && isAlpha (head l) && head l == toUpper (head l)
        caps a = not (null a) && a == map toUpper a

-- ** Notes

buildNotes :: [Note]
buildNotes =
  [ Note "`glance.cabal' is hand-maintained; hpack and package.yaml are gone -- regeneration dropped OverloadedRecordDot and dependencies and broke the build." [Typed]
  , Note "`Data.Org.*' in a web or daemon target's build-depends is impossible from outside the package: the facade constraint is the solver's, not review's." [Typed]
  , Note "`cabal build -f native-window all' against the default project still fails in the solver -- the flag and the bindings satisfying it are in one file." [Typed]
  , Note "The vendored packages keep upstream's name and version, so a local package shadows every Hackage version of it and `cabal get NAME-VERSION && diff -r' is the whole diff." [Typed]
  , Note "`vendored/gir/' holds the hand-written GIRs this machine has only -runtime of; haskell-gi searches the env path FIRST, so a distribution copy makes the directory dead weight." [Typed]
  , Note "`make native' rather than a cabal line, because a project file cannot supply the GIR search path." [Docs]
  , Note "`make sync-renderer' copies from the sibling checkout and prints `git diff --stat --no-index'; with no sibling it copies nothing, which keeps a bare clone buildable. Editing the vendored copy by hand is a fork." [Test]
  , Note "`Data.Org.Edit.tempSuffix' is asserted against `isDocument', the string being interesting only for what the walk does with it." [Test]
  , Note "The glue parts are the only source: no whole `glue.js' in the repo, and `--assets DIR' takes either shape." [Test]
  , Note "`TestSelfContained' compares `jsconfig.json' against `gluePartFiles': tsc reports clean over whatever it was handed, so a part named in one and not the other is checked by nothing." [Test]
  , Note "The suite shells out where a claim needs a real interpreter -- `node --check' over the extracted glue and `test/fixtures/shell-harness.js' -- and both answer `pure ()' with no node on PATH." [Test]
  , Note "WATCH: a `cabal test' run hangs occasionally and has never reproduced on a retry; nothing waits on a socket, the node cases are bounded by the child, `TestDesktop.waitUntil' gives up after 200 x 10 ms. Run with --test-options=--timeout=120s so a hang names the test." [Unguarded]
  , Note "`TestSelfContained' reads a proposal's status and date off the FIRST line carrying each marker and compares by string equality; the sweep LISTS the directory, so a document cannot escape by being named wrong." [Test]
  , Note "A status change is a `git mv' plus the line, and the suite says so when only one of the two moves; a rename breaks every link to the old path and past commit messages keep citing it." [Test]
  , Note "Every implemented feature earns a CHANGELOG entry under Unreleased, written as user-visible behaviour, one line per feature." [Docs]
  , Note "The author's address is substituted even where the repo's git config or an existing file header says otherwise." [Unguarded]
  , Note "APACHE-2.0, the SPDX id spelled in `glance.cabal'.  No NOTICE: the vendored\
         \ `assets/table-view.js' is MIT under the same author, who chose to carry no\
         \ separate notice for his own bytes." [Docs]
  -- Tier two until the checks moved into the suite: what no gate asks.
  , Note "Every intra-package dependency names a component." [Unguarded]
  , Note "The component graph is acyclic." [Unguarded]
  , Note "The web module graph is acyclic." [Unguarded]
  , Note "The floor holds exactly what more than one module above needs -- four members." [Unguarded]
  , Note "No module imports itself." [Unguarded]
  , Note "Most of what it pulls is the gi-gtk 3 tree." [Unguarded]
  , Note "An upgrade re-keys the generated bindings and spares the pure helpers." [Unguarded]
  , Note "The sibling repo reads one shape." [Unguarded]
  , Note "The whole set was renamed on one day." [Unguarded]
  , Note "The generated and the vendored are counted apart from what this repo wrote." [Unguarded]
  , Note "Every path lands in exactly one bucket." [Unguarded]
  , Note "The table folds to the banner it printed." [Unguarded]
  , Note "The budget is a tenth of the authored code." [Unguarded]
  , Note "The sweep stands at 31.7%, over by 13089." [Unguarded]
  , Note "The shell is already inside the target the repo is not." [Unguarded]
  , Note "A trailing comment counts as code." [Unguarded]
  , Note "A block opener is a comment whatever the file's marker." [Unguarded]
  , Note "Every commented language has a marker, and prose has none." [Unguarded]
  , Note "A docstring is complete, capitalised and CAPS-argued." [Unguarded]
  , Note "A comment earns its line on five grounds and is cut on four." [Unguarded]
  ]

-- * Running the model
--
-- Tier two is a line each; tier three is counted by what would notice it.

checks :: [Check]
-- ^ Empty, and the machinery under it is what a new rule is written into: tier
-- two runs in `test/TestSpec.hs', where a case asks the model AND the code.
checks = []

-- | Every note list under the section that owns it.  ONE REGISTRY: `notes' is
-- its fold and the report locates debt by it, so a new section is added HERE
-- once and every line below counts it.
sections :: [(String, [Note])]
sections =
  [ ("parse",    parseNotes)
  , ("scan",     scanNotes)
  , ("walk",     walkNotes)
  , ("config",   configNotes)
  , ("store",    storeNotes)
  , ("query",    queryNotes)
  , ("commands", cmdNotes)
  , ("shell",    shellNotes)
  , ("sheets",   sheetNotes)
  , ("build",    buildNotes)
  ]

notes :: [Note]
notes = concatMap snd sections

say :: Check -> String
say (Check w ok) = (if ok then "  ok  " else "FAIL  ") ++ w

-- | What a `Proof' costs to break: the run that goes red, in a breath.
gloss :: Proof -> String
gloss Typed     = "the illegal state does not compile"
gloss Test      = "`cabal test' goes red"
gloss Corpus    = "the corpus groups say so, and only where GLANCE_CORPUS is set"
gloss Interop   = "`make interop' goes red, emacs present"
gloss Browser   = "`make browser-check' goes red, a browser present"
gloss Elm       = "elm's own compiler, or `make elm-test'"
gloss Comment   = "a comment beside the code, and nothing runs"
gloss Docs      = "a document says so, and nothing runs"
gloss Unguarded = "nothing catches these"

-- | The debt, counted by `Proof'.  A `Note' carrying several is counted under each.
summary :: [Note] -> String
summary ns = intercalate "\n"
  ( (show (length ns) ++ " notes, " ++ show (length checks) ++ " checks")
  : [ "  " ++ show p ++ " " ++ show n ++ " — " ++ gloss p
    | p <- [minBound .. maxBound], let n = tally p, n > 0 ] )
  where tally p = length [ () | Note _ ps <- ns, p `elem` ps ]

-- | The direction, said as the ledger says it.
narrowerWord :: Narrower -> String
narrowerWord Producer = "producer narrower"
narrowerWord Renderer = "renderer narrower"
narrowerWord Neither  = "undecided"

-- | THE DIVERGENCE LEDGER, grouped by which side answers with fewer rows.  The
-- groups are the `Narrower' sum's own, so a gap added to `gaps' prints itself.
divergence :: [String]
divergence =
  [ "  " ++ narrowerWord n ++ ": " ++ intercalate ", " gs
  | n <- [minBound .. maxBound], let gs = [ show g | (g, n') <- gaps, n' == n ], not (null gs) ]

unguardedIn :: [Note] -> [Why]
unguardedIn ns = [ w | Note w ps <- ns, Unguarded `elem` ps ]

-- | THE DEBT, LOCATED: the unguarded notes under each section that has any,
-- silent sections omitted.  BOTH READINGS COME OFF THIS ONE, so the count a
-- line prints and the notes a report lists cannot disagree.
debtBySection :: [(String, [Why])]
debtBySection = [ (s, ws) | (s, ns) <- sections, let ws = unguardedIn ns, not (null ws) ]

-- | The debt as ONE LINE: where it sits, by size.
debtLine :: String
debtLine = "unguarded — " ++ intercalate " · "
  [ s ++ " " ++ show (length ws) | (s, ws) <- debtBySection ]

-- | What the model enumerates, by size alone: a chosen six, by hand.
registryLine :: String
registryLine = "registries — " ++ intercalate " · "
  [ "routes " ++ show (length routes)
  , "commands " ++ show (length cmds)
  , "keys " ++ show (length bindings)
  , "popups " ++ show (length popTiers)
  , "columns " ++ show (length viewColumns)
  , "metas " ++ show (length metas) ]

-- | The tier-three backlog, readable: every unguarded note under its section.
debtReport :: String
debtReport = intercalate "\n"
  [ l
  | (s, ws) <- debtBySection
  , l <- (s ++ " (" ++ show (length ws) ++ ")") : map ("  " ++) ws ]

report :: IO ()
report = do
  putStrLn (summary notes)
  putStrLn "divergence"
  mapM_ putStrLn divergence
  putStrLn debtLine
  putStrLn registryLine

main :: IO ()
main = do
  args <- getArgs
  mapM_ (putStrLn . say) checks
  if "debt" `elem` args then putStrLn debtReport else report
  unless (and [ b | Check _ b <- checks ]) exitFailure
