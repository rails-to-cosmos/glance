-- | @POST \/command@: the structured writes, as ONE table ('commands'); the
-- edits are 'Glance.Query''s.  `commandNames' rides out for the SUITE alone.
module Glance.Web.Commands (commandNames, runCommand, runCommandRaw) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Aeson (Object, Value, object, (.:), (.:!), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser)
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Network.HTTP.Types (status200, status400)
import Network.Wai (Request, Response)
import System.Directory (doesDirectoryExist)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as Time

import Glance.Query ( Completion (..), Repeat (..), noteCompletion, repeatOn, writeRefusalText
                    , BlobSeed (..), ConfigLayers
                    , DraftCargo (..), draftEntry, draftStates
                    , HeadlineRecord (hrDigest, hrFile, hrId, hrLevel, hrOrgId, hrSubtree)
                    , Span (Span), WriteFailure (..)
                    , addTagEdits, archiveEdits, archived, bareTemplate
                    , blobDocument
                    , blobPathIn, captureEdits, captureStamp, captureText
                    , captureTargetIn, captureTemplateIn, currentDocument
                    , pinnedDocument, rowSnapshot
                    , addLinkEdits, editLinkEdits, editedEntry, eolOf, expandTemplate, glanceLink
                    , groupOn, LinkPlace (InBody), linkPlaceOf, linkPlaceWord, linkPlaces
                    , linkTargetIn, mintBlobId
                    , plannedEntry, plannedValue
                    , priorityText
                    , removeTagEdits
                    , renameTagEdits, rowIdIn, setPlanningEdits
                    , setPriorityEdits, setStateEdits, setTitleEdits
                    , storeRootIn, tagText, titleText, trashBlob, unplanned )
import Glance.Web.Base ( ServeOptions (soDir), answerWrite, bodyObject, captureMoved
                       , dayAt, jsonError, jsonResponse, noSuchRow, now
                       , walkFor, withBody )
import Glance.Web.Store ( Hub, Store (stConfig), headlinesIn, hubStore, layersFor
                        , recordsUnder, storeRecords )
import Glance.Web.Watch (nudge, writeSpans)


data Command = Command
  { cmdSpec    :: !CommandSpec      -- ^ its entry in 'commands'.
  , cmdIds     :: ![Text]           -- ^ in the order named, deduplicated; empty for @capture@.
  , cmdArgs    :: !Args             -- ^ whatever @args@ carried.
  , cmdDigests :: !(Map Text Text)  -- ^ id to the digest the client holds for its file.
  }

-- | THE NESTED 'Maybe's: ABSENT said nothing, NULL asked for the value off.
data Args = Args
  { agKeyword :: !(Maybe (Maybe Text))
  , agDate    :: !(Maybe (Maybe Text))
  , agText    :: !(Maybe Text)
  , agTitle    :: !(Maybe Text)
  , agPriority :: !(Maybe (Maybe Text))
  , agTag     :: !(Maybe Text)
  , agFields  :: !(Maybe (Map Text Text))
    -- THE WIDENED CAPTURE CARGO: the doc pane's own standing shape, the same
    -- @body@ / @properties@ / @planning@ the commit door speaks.
  , agState   :: !(Maybe Text)
  , agTags    :: !(Maybe [Text])
  , agPlanning :: !(Maybe [(Text, Text)])
  , agProps   :: !(Maybe [(Text, Text)])
  , agBody    :: !(Maybe Text)
  , agFrom    :: !(Maybe Text)
  , agTo      :: !(Maybe Text)
  , agSpan    :: !(Maybe Span)
  , agTarget  :: !(Maybe Text)
  , agDesc    :: !(Maybe (Maybe Text))
  , agKind    :: !(Maybe Text)
  , agWhere   :: !(Maybe Text)
  }

-- | The planning keyword ARGS names, absent and null alike reading as @""@ — a
-- word naming no entry, which 'unplanned' refuses.  ONE SPELLING, so the shape
-- check and the write read the same key.
keyOf :: Args -> Text
keyOf = fromMaybe "" . join . agKeyword

data FilePlan = FilePlan
  { fpPath   :: !FilePath
  , fpDigest :: !Text
  , fpRows   :: ![(Text, RowWrite, Maybe Completion, Maybe Sprout)]
      -- ^ row id, what it writes, the ledger line riding its success, and the
      -- blob a row leaving the inbox lands as before the write.
  }

-- | ONE ANSWER rather than two fields that must agree: asking the spans and the
-- ledger line apart ran `repeatOn' -- and so `keywordSources' -- twice a row.
data RowWrite = RowWrite
  { rwEdits :: ![(Span, Text)]
  , rwNote  :: !(Maybe (Text, Text))  -- ^ the state landed on, and its next occurrence.
  }

plain :: [(Span, Text)] -> Either Text RowWrite
plain edits = Right (RowWrite edits Nothing)

-- | A row's edits over the DOCUMENT its file holds now, read once per request
-- ('documentsFor').
type RowEdits = ConfigLayers -> Asked -> Args -> Text -> HeadlineRecord
              -> Either Text RowWrite

-- | WHAT A REQUEST RESOLVES before any row is touched: the day off ONE clock
-- read, and whichever request-level value the command owes.
data Asked = Asked
  { askNow   :: !Time.ZonedTime -- ^ the request's ONE clock read, taken before any row.
  , askStamp :: !(Maybe Text)  -- ^ @set-planning@'s date, already rendered.
  , askLink  :: !(Maybe Text)  -- ^ @add-link@'s org link, its target resolved to a row.
  }

-- | The day the request stands in, read off its one clock reading.
askToday :: Asked -> Time.Day
askToday = dayAt . askNow

-- | WHAT THE DOOR RESOLVES FOR A COMMAND, the whole vocabulary.  A closed word
-- rather than a flag apiece, so a fourth request-level value is named by the
-- compiler at 'resolveAsked' rather than defaulted there.
data Asks = AsksNothing | AsksDate | AsksLink

data CommandSpec = CommandSpec
  { csArgs  :: [Text] -> Args -> Maybe Text
      -- ^ why the request's shape is refused, where it is.
  , csAsks  :: Asks                -- ^ what the door resolves for it, once per request.
  , csKind  :: CommandKind         -- ^ what it does to the rows it names.
  }

data CommandKind
  = Splices Reads RowEdits
    -- ^ edits each named row; the ten that write spans.
  | Makes
    -- ^ MAKES a row rather than naming one: @capture@, the one that owes no ids.
  | Moves
    -- ^ moves a file out of the tree: @delete@.

-- | A row an inbox write carries out of the inbox: what it lands as, and the
-- span it leaves behind.  The id it left is its key in the file plan.
data Sprout = Sprout
  { spId   :: !Text      -- ^ the @ORG_GLANCE_ID@ minted for it.
  , spPath :: !FilePath  -- ^ where the blob lands.
  , spDoc  :: !Text      -- ^ the document it lands as.
  , spCut  :: !Span      -- ^ its subtree in the inbox, spliced to nothing.
  }

-- | Does an edit set READ the file it lands in?  Most cut their spans out of
-- the text on disk and are handed a pinned read per file; @add-tag@ and
-- @archive@ compute off the ROW alone, so their request opens no file at all.
data Reads = ReadsFile | ReadsNothing

namesRows :: CommandKind -> Bool
namesRows Makes = False
namesRows (Splices _reads _edits) = True
namesRows Moves = True

commands :: [(Text, CommandSpec)]
commands =
    -- THE EDGE NOTHING COULD WRITE: `edit-link' takes a span that is already a
    -- link, so linking two rows meant rewriting the file outside the daemon.
  [ ("add-link", CommandSpec (overIds wantsAddLink) AsksLink
      (Splices ReadsFile (\_cfg asked args doc r ->
               plain =<< addLinkEdits (placeOf args) (word askLink asked) doc r)))
  , ("add-tag", CommandSpec (overIds (wantsTag "add-tag")) AsksNothing
      (Splices ReadsNothing (\_cfg _asked args _doc r -> plain (addTagEdits (tagOf args) r))))
  , ("archive", CommandSpec (overIds (const Nothing)) AsksNothing
      (Splices ReadsNothing (\_cfg _asked _args _doc r -> plain (archiveEdits r))))
  , ("capture", CommandSpec (overIds wantsCapture) AsksNothing Makes)
    -- THE ONE DESTRUCTIVE COMMAND: it moves a FILE rather than splicing spans,
    -- and every wall it has is per row and checked HERE as well as in the shell.
  , ("delete", CommandSpec (overIds (const Nothing)) AsksNothing Moves)
  , ("edit-link", CommandSpec wantsLink AsksNothing
      (Splices ReadsFile (\_cfg _asked args doc r ->
               plain =<< editLinkEdits (fromMaybe (Span 0 0) (agSpan args))
                                       (word agTarget args) (agDesc args) doc r)))
  , ("remove-tag", CommandSpec (overIds (wantsTag "remove-tag")) AsksNothing
      (Splices ReadsFile (\_cfg _asked args doc r -> plain (removeTagEdits (tagOf args) doc r))))
  , ("rename-tag", CommandSpec (overIds wantsRename) AsksNothing
      (Splices ReadsFile (\_cfg _asked args doc r ->
               plain (renameTagEdits (word agFrom args) (word agTo args) doc r))))
  , ("set-planning", CommandSpec (overIds wantsPlanning) AsksDate
      (Splices ReadsFile (\_cfg asked args doc r ->
               plain =<< setPlanningEdits (keyOf args) (askStamp asked) doc r)))
    -- A REPEAT IS A `set-state', and the one command that RECORDS anything.
  , ("set-state", CommandSpec (overIds wantsState) AsksNothing
      (Splices ReadsFile stateEdits))
  , ("set-priority", CommandSpec (overIds wantsPriority) AsksNothing
      (Splices ReadsFile
        (\_cfg _asked args doc r -> plain =<< setPriorityEdits (join (agPriority args)) doc r)))
  , ("set-title", CommandSpec (overIds wantsTitle) AsksNothing
      (Splices ReadsFile
        (\_cfg _asked args doc r -> plain =<< setTitleEdits (word agTitle args) doc r)))
  ]
  where
    overIds = const
    stateEdits cfg asked args doc r = case repeating cfg asked args doc r of
      -- ONE `repeatOn': the spans and the line recorded come off one answer.
      Just rp -> Right (RowWrite (rpEdits rp) (Just (rpState rp, rpShifted rp)))
      Nothing -> plain =<< setStateEdits cfg (join (agKeyword args)) doc r
    repeating cfg asked args doc r =
      join (agKeyword args) >>= \keyword -> repeatOn cfg (askToday asked) keyword doc r
    word field = fromMaybe "" . field
    tagOf = word agTag
    -- The one command whose keyword may be NULL: that is how a state comes off.
    wantsState args
      | Nothing <- agKeyword args =
          Just "set-state wants args {\"keyword\": \"DONE\"}, or a null keyword to clear it"
      | otherwise = Nothing
    -- AN UNKNOWN KEY OUTRANKS EVERY VALUE, the commit door's own order: the
    -- keyword picks which wall the date meets, so a word naming no entry is
    -- refused before anything reads one.
    wantsPlanning args = case join (agKeyword args) of
      Nothing -> Just "set-planning wants args {\"keyword\": \"SCHEDULED\", \"date\": \"+3d\"}"
      Just k
        | Just why <- unplanned k -> Just why
        | Nothing <- agDate args  ->
            Just "set-planning wants a date, or a null one to take the entry off"
        | otherwise -> Nothing
    -- TWO ROADS, EXACTLY ONE TAKEN: @text@ is the raw line the old wire carries
    -- and @title@ opens the draft's own cargo.  NAMING BOTH IS REFUSED rather
    -- than resolved — the wire is public, and a caller that means both means one
    -- of them differently than this server would read it.
    wantsCapture args
      | Just _ <- agText args, Just _ <- agTitle args =
          Just "capture takes either args {\"text\": …} or the draft's args\
               \ {\"title\": …}, and not both"
      | Nothing <- agText args, Nothing <- agTitle args =
          Just "capture wants args {\"text\": \"TODO Buy milk :errands:\"},\
               \ or a draft's own {\"title\": \"Buy milk\"}"
      | Just given <- agTag args = either Just (const Nothing) (tagText given)
      | otherwise = Nothing
    -- PADDING is refused here, so the string tested is the string written.
    -- THE ROW COUNT IS FIRST: the coarsest thing wrong.
    wantsLink ids args
      | length ids > 1 =
          Just "edit-link names one row: its args describe that row's own text"
      | Nothing <- agSpan args =
          Just ("edit-link wants args {\"span\": [START, END],"
                  <> " \"target\": \"https://example.org\"}")
      | maybe True (T.null . T.strip) (agTarget args) =
          Just "edit-link wants a target: a link points somewhere"
      | Just given <- agTarget args, T.strip given /= given =
          Just ("edit-link wants a target with no leading or trailing space: "
                  <> T.strip given)
      | otherwise = Nothing
    -- WHERE IS A CLOSED WORD and TARGET IS A ROW: the row itself is resolved
    -- per id the way every command resolves one, so only the args wall here.
    wantsAddLink args
      | maybe True (T.null . T.strip) (agTarget args) =
          Just "add-link wants args {\"target\": \"<a row id>\"}: a link points at a row"
      | Just given <- agWhere args, Nothing <- linkPlaceOf given =
          Just (given <> " is no place for a link; add-link writes into "
                  <> T.intercalate " or " (map linkPlaceWord linkPlaces))
      | otherwise = Nothing
    -- THE BODY IS THE DEFAULT: a link appended where no byte of the headline moves.
    placeOf args = fromMaybe InBody (linkPlaceOf =<< agWhere args)
    wantsTag name args = case agTag args of
      Nothing    -> Just (name <> " wants args {\"tag\": \"work\"}")
      Just given -> either Just (const Nothing) (tagText given)
    wantsRename args = case (agFrom args, agTo args) of
      (Just from, Just to) -> either Just (const Nothing) (tagText from >> tagText to)
      _absent -> Just "rename-tag wants args {\"from\": \"work\", \"to\": \"projects\"}"
    wantsPriority args = case agPriority args of
      Nothing        -> Just "set-priority wants args {\"priority\": \"A\"},\
                             \ or a null one to take it off"
      Just Nothing   -> Nothing
      Just (Just given) -> either Just (const Nothing) (priorityText given)
    wantsTitle args = case agTitle args of
      Nothing    -> Just "set-title wants args {\"title\": \"Buy milk\"}"
      Just given -> either Just (const Nothing) (titleText given)

commandNames :: [Text]
commandNames = map fst commands

-- | @POST \/command@ over the rows the client names.  BATCHING IS PER FILE: one
-- drift-locked write per file, no rollback across files, so the answer is per
-- id; a shape or keyword refusal is the WHOLE request's.  The store is untouched.
runCommand :: ServeOptions -> Hub -> Request -> IO Response
runCommand opts hub request = withBody request (runCommandRaw opts hub)

-- | The command engine over an already-read body: @\/command@ past its 413
-- gate, and the MCP door's write path speak the same core.  The store is
-- untouched.
runCommandRaw :: ServeOptions -> Hub -> BL.ByteString -> IO Response
runCommandRaw opts hub raw = do
  st <- readTVarIO (hubStore hub)
  case parseCommand raw of
    Left why -> pure (jsonError status400 why)
    -- ONE TOTAL CASE over the kinds there are; the kind is destructured ONCE.
    Right cmd -> case csKind (cmdSpec cmd) of
      Moves -> deleteRows opts hub st cmd
      Makes -> captureInto opts hub st cmd
      Splices reads' edits -> do
        -- The target a row points at is named among the same rows the ids are.
        asked <- resolveAsked (storeRecords st) cmd
        either (pure . jsonError status400)
               (\at -> overRows opts hub st at reads' edits cmd) asked

-- | CMD's rows moved into the store's trash, answered per id in the order named.
-- THREE WALLS PER ROW, checked HERE as well as in the shell.  Splicing no spans,
-- the tombstone rides 'trashBlob''s own branch and this door nudges the path.
deleteRows :: ServeOptions -> Hub -> Store -> Command -> IO Response
deleteRows opts hub st cmd =
  jsonResponse status200 . pure . ("results" .=) <$> mapM (either pure taken) (namedRows st cmd)
  where
    taken r
      | not (archived r) =
          pure (refused (hrId r) (hrId r <> " is not archived: archive it first"))
      | otherwise = do
          put <- trashBlob (soDir opts) (hrFile r)
          case put of
            Left why   -> pure (refused (hrId r) why)
            Right dest -> do
              nudge (walkFor opts) hub (hrFile r)
              pure (object [ "id" .= hrId r, "ok" .= True, "trash" .= T.pack dest ])

-- | CMD's ids in the order NAMED, which is the wire's order and is kept HERE.
namedRows :: Store -> Command -> [Either Value HeadlineRecord]
namedRows st cmd =
  [ maybe (Left (refused rid (noSuchRow rid))) Right (lookup rid found)
  | rid <- cmdIds cmd ]
  where found = [ (hrId r, r) | r <- fst (headlinesIn (storeRecords st) (cmdIds cmd)) ]

overRows :: ServeOptions -> Hub -> Store -> Asked -> Reads -> RowEdits -> Command
         -> IO Response
overRows opts hub st asked reads' edits cmd = do
  -- RESOLVED AT THE DOOR, once: 'storeRecords' is a full resolution per call.
  let named = headlinesIn (storeRecords st) (cmdIds cmd)
  docs <- case reads' of
    ReadsFile    -> documentsFor (fst named)
    ReadsNothing -> pure Map.empty
  planned <- planCommand opts docs named st asked edits cmd
  case planned of
    Left why -> pure (jsonError status400 why)
    Right (plans, said) -> do
      written <- mapM (writeOne opts hub) plans
      let outcomes = said <> concat written
      pure (jsonResponse status200
              ["results" .= [ v | rid <- cmdIds cmd, Just v <- [lookup rid outcomes] ]])

-- | WHICH OF ROWS THE WRITE CARRIES OUT OF THE INBOX, and the blob each one
-- lands as.  THE SPROUT IS A RULE OF THE INBOX WRITE, decided once a row's edits
-- are known: a top-level, id-less inbox row whose EDITED title line wears a tag
-- run has a home now, so the subtree goes to it — @add-tag@, a @set-title@
-- spelling a run and a @rename-tag@ alike.  One minted id apiece and the
-- creation stamp off the request's ONE clock read ('askNow'), the way
-- 'captureBlob' takes its own; NOTHING IS WRITTEN HERE, so a row the write goes
-- on to refuse costs a uuid and no bytes.
--
-- ONE READ of the inbox per request, and only where a candidate stands: an
-- ordinary tag write never opens a file at all.
sprouted :: ServeOptions -> Asked -> Map FilePath (Either WriteFailure Text)
         -> [(HeadlineRecord, RowWrite)] -> IO (Map Text Sprout)
sprouted opts asked docs rows = case candidates of
  []              -> pure Map.empty
  ((r, _w) : _st) -> do
    doc <- maybe (pinnedDocument (rowSnapshot r)) pure (Map.lookup inbox docs)
    case doc of
      Left _moved -> pure Map.empty
      Right text  -> Map.fromList . concat <$> mapM (grow text) candidates
  where
    inbox = captureTargetIn (soDir opts)
    -- WHAT MAY LEAVE: a TOP-LEVEL row of the store's own inbox carrying no id.
    -- A row that already carries one has a home; a child is no row of its own
    -- ('recordsOf' keeps top entries) and rides the subtree that leaves; and
    -- every row outside the inbox stays where it is.
    candidates = [ row | row@(r, _w) <- rows
                 , hrFile r == inbox, hrLevel r == 1, isNothing (hrOrgId r) ]
    -- The blob is composed the way `captureBlob' composes one, off the EDITED
    -- subtree: 'blobDocument' joins the destination ('addTagEditsIn', @add-tag@'s
    -- very rule, which folds a tag the run already wears) and the two drawer
    -- pairs, so capture and the command cannot disagree.
    grow text (r, w) = case editedEntry text (rwEdits w) r of
      Just (entry, tag : _rest) -> do
        minted <- mintedBlob (soDir opts) tag (askNow asked) entry
        pure [ (hrId r, Sprout ident path blob (hrSubtree r))
             | Right (ident, path, blob) <- [minted] ]
      _staying -> pure []

-- | A blob minted for ENTRY under TAG in ROOT's store: the id, where it lands,
-- and the document it lands as.  THE STORE IS THE WALL and it is asked HERE, a
-- tree that keeps none having nowhere to mint into.  Nothing is written.
mintedBlob :: FilePath -> Text -> Time.ZonedTime -> Text
           -> IO (Either Text (Text, FilePath, Text))
mintedBlob root tag at entry = do
  there <- doesDirectoryExist store
  if not there then pure (Left noStore) else do
    ident <- mintBlobId
    pure ((,,) ident (blobPathIn store ident)
            <$> blobDocument (BlobSeed tag ident (captureStamp at)) entry)
  where
    store = storeRootIn root
    noStore = T.pack store <> " is not there, so this tree keeps no org-glance store;\
                              \ capture with no tag to file into the inbox instead"

-- | PATH's blob, written under the EMPTY DIGEST that is the create pin, so an
-- occupied path DRIFTS.  BOTH MINTING ROADS' one write, so the id, the creation
-- drawer, the ledger line ('replaceSpans') and the nudge ride one road.
writeBlob :: ServeOptions -> Hub -> FilePath -> Text -> IO (Either WriteFailure Text)
writeBlob opts hub path doc = writeSpans (walkFor opts) hub path "" [(Span 0 0, doc)]

-- | The FILES ROWS sit in, each read ONCE per request and PINNED against the
-- digest their parse took ('rowSnapshot').  Rows of one file share that digest,
-- so which of them supplies the pin does not matter; a file that has moved
-- comes back as the refusal its rows are answered with.
documentsFor :: [HeadlineRecord] -> IO (Map FilePath (Either WriteFailure Text))
documentsFor rows =
  traverse pinnedDocument (Map.fromList [ (hrFile r, rowSnapshot r) | r <- rows ])

-- | ONE clock read, before any row: a marked set must not cross midnight.  What
-- ELSE is resolved here is the entry's own ('csAsks'), and a refusal is the whole
-- REQUEST's — the value is the same for every id it names.
--
-- THE KEYWORD PICKS THE WALL, 'plannedValue' being the one place that choice is
-- made: the two this server composes for read the whole date grammar, @CLOSED@
-- reparses org's own bracket and nothing else.  A word naming no planning entry
-- never reaches here — `wantsPlanning' has refused the request already.
resolveAsked :: [HeadlineRecord] -> Command -> IO (Either Text Asked)
resolveAsked rows cmd = do
  at <- now
  let day = dayAt at
  pure $ case csAsks (cmdSpec cmd) of
    AsksNothing -> Right (Asked at Nothing Nothing)
    AsksDate    -> (\v -> Asked at v Nothing) <$> traverse (plannedValue day key)
                                                           (join (agDate args))
    AsksLink    -> Asked at Nothing . Just <$> link
  where
    args = cmdArgs cmd
    key  = keyOf args
    want = fromMaybe "" (agTarget args)
    -- ONE LINK FOR THE WHOLE REQUEST: the same bytes land in every id named, so
    -- the target is resolved HERE, among the rows the ids resolve among, and a
    -- target naming no row refuses the request rather than each row in turn.
    link = do
      r <- maybe (Left (want <> " is no row in this tree; add-link points at a row's own id"))
                 Right (listToMaybe (fst (headlinesIn rows [want])))
      target <- linkTargetIn r
      glanceLink target (agKind args) (join (agDesc args))

captureInto :: ServeOptions -> Hub -> Store -> Command -> IO Response
captureInto opts hub st cmd =
  maybe (captureInbox opts hub st args) (captureBlob opts hub st args) (agTag args)
  where args = cmdArgs cmd

captureInbox :: ServeOptions -> Hub -> Store -> Args -> IO Response
captureInbox opts hub st args = do
    (doc, digest) <- currentDocument inbox
    at <- now
    let composed = do
          entry <- capturedEntry (stConfig st) at (eolOf doc) Nothing args
          captureEdits doc (captureStamp at) entry
    case composed of
      Left why    -> pure (jsonError status400 why)
      Right edits -> answerWrite (captureMoved inbox) (landed inbox)
                       <$> writeSpans (walkFor opts) hub inbox digest edits
  where
    inbox = captureTargetIn (soDir opts)
    -- A RACE, honestly: @\/command@ never writes the store, so K is the last
    -- load's count.  'recordsUnder', since 'storeRecords' drops a collision loser.
    landed path fresh = captured path fresh (rowIdIn path (length (recordsUnder path st)))

-- | A capture down the tag's own road: composed, minted and written, the very
-- three steps a moved inbox row takes ('sprouted', 'mintedBlob', 'writeBlob').
captureBlob :: ServeOptions -> Hub -> Store -> Args -> Text -> IO Response
captureBlob opts hub st args tag = do
  layers <- layersFor (soDir opts) st
  at <- now
  -- THE BLOB IS A NEW FILE, so it has no line ending of its own to keep: it
  -- takes the TEMPLATE'S, the bytes it is composed out of.  The older road
  -- already did — `expandTemplate' copies the template verbatim, and the drawer
  -- splice under it reads `eolOf' off what came back — and the widened road's
  -- head line must join the same way, or a CRLF layer lands a blob whose
  -- headline ends one way and whose body ends the other.
  let template = fromMaybe bareTemplate (captureTemplateIn tag layers)
  case capturedEntry (stConfig st) at (eolOf template) (Just template) args of
    Left why    -> pure (jsonError status400 why)
    Right entry -> do
      minted <- mintedBlob (soDir opts) tag at entry
      case minted of
        Left why -> pure (jsonError status400 why)
        Right (ident, path, doc) ->
          answerWrite (captureMoved path) (\fresh -> captured path fresh ident)
            <$> writeBlob opts hub path doc

-- | The ONE entry a capture writes, off EITHER arg shape.  The widened cargo is
-- composed the way a materialize commit composes a subtree; the old
-- @{text, fields}@ goes through the tag's TEMPLATE, or straight onto a star
-- where there is none (the inbox's jot).  Both hand the same org on to the
-- minting below, which is why the blob path, the id, the creation drawer, the
-- ledger note and the inbox split are untouched by the widening.
--
-- TEMPLATE is 'Nothing' on the inbox path, which expands nothing.
capturedEntry :: ConfigLayers -> Time.ZonedTime -> Text -> Maybe Text -> Args
              -> Either Text Text
capturedEntry cfg at eol template args = case agTitle args of
  Just title -> do
    stated cfg args
    -- THE WALL IS 'plannedEntry''s, whose refusal names the key the door drops here.
    plan <- traverse (first snd . plannedEntry (dayAt at))
                     (fromMaybe [] (agPlanning args))
    draftEntry cfg eol DraftCargo
      { dcTitle      = title
      , dcState      = agState args
      , dcPriority   = join (agPriority args)
      , dcTags       = fromMaybe [] (agTags args)
      , dcPlanning   = plan
      , dcProperties = fromMaybe [] (agProps args)
      , dcBody       = fromMaybe "" (agBody args)
      }
  Nothing -> case template of
    Nothing  -> ("* " <>) <$> captureText (capturedText args)
    Just tpl -> do
      (text, answers) <- capturedParts args
      expandTemplate at answers text tpl

-- | A DRAFT HAS NO ROW, so the cycle is its DESTINATION'S: the tag it is filed
-- under and whatever its own run wears ('draftStates'), which is the very list
-- @GET \/capture@ offered the state door.
stated :: ConfigLayers -> Args -> Either Text ()
stated cfg args = case agState args of
  Just want | want `notElem` settable ->
    Left (want <> " is not a TODO keyword for a capture" <> under
            <> "; this one may be set to " <> T.intercalate ", " settable)
  _spelled -> Right ()
  where
    settable = draftStates cfg (captureScopes args)
    under = maybe "" (\t -> " under :" <> t <> ":") (agTag args)

-- | The tag scopes a capture's own keyword chain is drawn from, folded the way
-- @config\/tags\/TAG.org@ is named.
captureScopes :: Args -> [Text]
captureScopes args =
  map T.toLower (maybe [] pure (agTag args) <> fromMaybe [] (agTags args))

capturedText :: Args -> Text
capturedText = fromMaybe "" . agText

captured :: FilePath -> Text -> Text -> [Pair]
captured path fresh ident = okPairs ident fresh <> ["file" .= path]

capturedParts :: Args -> Either Text (Text, [(Text, Text)])
capturedParts args =
  (,) <$> captureText (capturedText args)
      <*> traverse answered (Map.toList (fromMaybe Map.empty (agFields args)))
  where
    answered (want, value) =
      (,) want <$> first (\why -> "the answer to " <> want <> ": " <> why)
                         (captureText value)

-- | PLAN's one write, and the blobs its SPROUTING rows land as before it: a
-- failure between the two leaves a DUPLICATE and never a loss.  A blob that did
-- not land keeps its row's cut out of the write, so that row stands where it was.
writeOne :: ServeOptions -> Hub -> FilePlan -> IO [(Text, Value)]
writeOne opts hub plan = do
  (lost, standing) <- partitionEithers <$> mapM land (fpRows plan)
  written <- writeSpans (walkFor opts) hub (fpPath plan) (fpDigest plan)
                        (concat [ rwEdits w | (_rid, w, _note, _sp) <- standing ])
  -- THE LEDGER RIDES THE SUCCESS BRANCH.  HERE rather than in `replaceSpans'
  -- beside `noteExternalWrite': a completion is keyed off the SERVED ROOT.
  case written of
    Right _digest -> mapM_ record standing
    Left _refused -> pure ()
  pure (lost <> [ (rid, answered rid sp written) | (rid, _w, _note, sp) <- standing ])
  where
    land row@(_rid, _w, _note, Nothing) = pure (Right row)
    land row@(rid, _w, _note, Just sp)  = do
      put <- writeBlob opts hub (spPath sp) (spDoc sp)
      pure $ case put of
        Left bad -> Left (rid, refused rid (writeRefusalText (spPath sp) bad))
        Right _d -> Right row
    record (_rid, _write, note, _sp) = mapM_ (noteCompletion (soDir opts)) note
    answered rid sp written = case (sp, written) of
      (Nothing, Right d)   -> object (okPairs rid d)
      (Nothing, Left bad)  -> refused rid (writeRefusalText (fpPath plan) bad)
      (Just s, Right d)    -> moved rid s d
      (Just s, Left bad)   -> orphaned (fpPath plan) rid s bad

-- | Why PATH wrote nothing.  ONE SENTENCE, TWO ASKS: the plan refuses a row
-- whose file moved before the parse's spans are cut, and 'writeSpans' refuses
-- one that moved after.  THE SPROUT IS DECIDED HERE, once a row's edits are
-- known and once its file group has passed the client's pin, so no uuid is drawn
-- for a row this refuses.
planCommand :: ServeOptions -> Map FilePath (Either WriteFailure Text)
            -> ([HeadlineRecord], [Text]) -> Store -> Asked -> RowEdits -> Command
            -> IO (Either Text ([FilePlan], [(Text, Value)]))
planCommand opts docs (held, absent) st asked rowEdits cmd = case mapM withEdits standing of
  Left why   -> pure (Left why)
  Right rows -> do
    let groups = groupOn (hrFile . fst) rows
        living = [ g | g@(_path, rs) <- groups, not (stale rs) ]
    grown <- sprouted opts asked docs (concatMap snd living)
    -- A SPROUTING ROW WRITES NO TAG: its subtree is spliced to nothing, and that
    -- cut rides its file's ONE write beside whatever the rows staying there edit.
    let planned r w = case Map.lookup (hrId r) grown of
          Just sp -> (hrId r, RowWrite [(spCut sp, "")] Nothing, Nothing, Just sp)
          Nothing -> (hrId r, w, noted r w, Nothing)
    pure (Right
      ( [ FilePlan path (hrDigest r0) [ planned r w | (r, w) <- rs ]
        | (path, rs@((r0, _) : _)) <- living ]
      , missing <> drifted <> [ (hrId r, refused (hrId r) (staleWhy path))
                              | (path, rs) <- groups, stale rs, (r, _w) <- rs ] ))
  where
    -- A ROW WHOSE PINNED READ WAS REFUSED IS REFUSED HERE, in the write door's
    -- own words: its spans were cut from bytes the file no longer holds, so
    -- nothing computes edits over it.  A command whose edits ignore the
    -- document reads no file, so its map is empty and every row stands.
    (drifted, standing) = partitionEithers (map textFor held)
    textFor r = case fromMaybe (Right "") (Map.lookup (hrFile r) docs) of
      Left failed -> Left (hrId r, refused (hrId r) (writeRefusalText (hrFile r) failed))
      Right doc   -> Right (r, doc)
    withEdits (r, doc) = (,) r <$> rowEdits (stConfig st) asked (cmdArgs cmd) doc r
    -- Keyed by `ORG_GLANCE_ID': an ordinal names a different row a week on.
    noted r w = do
      (state, shifted) <- rwNote w
      ident <- hrOrgId r
      pure (Completion ident state shifted)
    missing = [ (rid, refused rid (noSuchRow rid)) | rid <- absent ]
    stale rs = any (stalePin cmd . fst) rs
    staleWhy path = T.pack path
                      <> " has been re-read since these rows were fetched; ask for them again"

-- | Does CMD's pin for R disagree with the digest R's parse took?  THE CLIENT'S
-- OWN LOCK, and 'planCommand' is the one door that reads it: a group it refuses
-- computes no edits, mints no uuid and writes no byte.
stalePin :: Command -> HeadlineRecord -> Bool
stalePin cmd r = maybe False (/= hrDigest r) (Map.lookup (hrId r) (cmdDigests cmd))

-- | What every landed row says: the id it is addressed by now, and its file's
-- fresh digest.  A capture's answer and a move's are this plus the file.
okPairs :: Text -> Text -> [Pair]
okPairs rid digest = [ "id" .= rid, "ok" .= True, "digest" .= digest ]

-- | A ROW THAT LEFT: the answer names the id it ARRIVES under, so the sheet's
-- point follows it, and @from@ names the id the request spelled.  The digest is
-- the INBOX's, the file this write pinned; the blob arrives with its own.
moved :: Text -> Sprout -> Text -> Value
moved from sp digest =
  object (okPairs (spId sp) digest <> [ "from" .= from, "file" .= spPath sp ])

-- | THE BLOB LANDED AND THE INBOX WOULD NOT GIVE THE SUBTREE UP, so it stands in
-- both: said plainly, naming the file that refused and the id already written.
orphaned :: FilePath -> Text -> Sprout -> WriteFailure -> Value
orphaned path from sp bad = object
  [ "id" .= spId sp, "ok" .= False
  , "from" .= from, "file" .= spPath sp
  , "error" .= (writeRefusalText path bad <> "; " <> spId sp
                  <> " was written before it, so the subtree stands in both") ]

refused :: Text -> Text -> Value
refused rid why = object [ "id" .= rid, "ok" .= False, "error" .= why ]

parseCommand :: BL.ByteString -> Either Text Command
parseCommand raw = bodyObject "command" command raw >>= checked
  where
    command o = do
      name <- o .: "name"
      one <- o .:? "id"
      several <- o .:? "ids"
      digests <- o .:? "digests"
      -- @.:!@ rather than @.:?@, which folds a NULL into an absence.
      a <- fromMaybe mempty <$> (o .:? "args" :: Parser (Maybe Object))
      sp <- fmap (uncurry Span) <$> (a .:? "span" :: Parser (Maybe (Int, Int)))
      parsed <- Args <$> a .:! "keyword" <*> a .:! "date" <*> a .:? "text"
                     <*> a .:? "title" <*> a .:! "priority" <*> a .:? "tag"
                     <*> a .:? "fields"
                     <*> a .:? "state" <*> a .:? "tags"
                     <*> cargoPairs a "planning" <*> cargoPairs a "properties"
                     <*> a .:? "body"
                     <*> a .:? "from" <*> a .:? "to"
                     <*> pure sp <*> a .:? "target" <*> a .:! "desc"
                     <*> a .:? "kind" <*> a .:? "where"
      pure ( name :: Text, nub (maybe [] pure one <> fromMaybe [] several)
           , parsed, fromMaybe Map.empty digests )
    -- @[[KEY, VALUE], …]@, the shape @POST \/headline@'s own cargo carries: one
    -- spelling of the doc pane's two lists, so a draft and a row edit agree.
    cargoPairs a key = traverse (traverse pair) =<< (a .:? key :: Parser (Maybe [[Text]]))
    pair [k, v] = pure (k, v)
    pair _other = fail "each planning entry and property is a [key, value] pair"
    checked (name, ids, args, digests) = case lookup name commands of
      Nothing -> Left ("no such command: " <> name <> "; this server runs "
                         <> T.intercalate " and " commandNames)
      Just spec
        | namesRows (csKind spec), null ids ->
            Left "a command names rows: {\"ids\": [\"…\"]}, or {\"id\": \"…\"} for one"
        | Just why <- csArgs spec ids args -> Left why
        | otherwise -> Right (Command spec ids args digests)
