-- | @POST \/command@: the structured writes, as ONE table ('commands'); the
-- edits are 'Glance.Query''s.  `commandNames' rides out for the SUITE alone.
module Glance.Web.Commands (commandNames, runCommand) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Aeson (Object, Value, object, (.:), (.:!), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser)
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Types (status200, status400)
import Network.Wai (Request, Response)
import System.Directory (doesDirectoryExist)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as Time

import Glance.Query ( Completion (..), Repeat (..), noteCompletion, repeatOn
                    , BlobSeed (..), ConfigLayers
                    , DraftCargo (..), draftEntry, draftStates
                    , HeadlineRecord (hrDigest, hrFile, hrId, hrOrgId)
                    , Span (Span), WriteFailure (..)
                    , addTagEdits, archiveEdits, archived, bareTemplate
                    , blobDocument
                    , blobPathIn, captureEdits, captureStamp, captureText
                    , captureTargetIn, captureTemplateIn, currentDocument
                    , pinnedDocument, rowSnapshot
                    , editLinkEdits, eolOf, expandTemplate, groupOn, mintBlobId
                    , plannedValue
                    , priorityText
                    , removeTagEdits
                    , renameTagEdits, rowIdIn, setPlanningEdits
                    , setPriorityEdits, setStateEdits, setTitleEdits
                    , storeRootIn, tagText, titleText, trashBlob, unplanned )
import Glance.Web.Base ( ServeOptions (soDir), answerWrite, bodyObject, captureMoved
                       , jsonError, jsonResponse, noSuchRow, today
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
  }

-- | The planning keyword ARGS names, absent and null alike reading as @""@ — a
-- word naming no entry, which 'unplanned' refuses.  ONE SPELLING, so the shape
-- check and the write read the same key.
keyOf :: Args -> Text
keyOf = fromMaybe "" . join . agKeyword

data FilePlan = FilePlan
  { fpPath   :: !FilePath
  , fpDigest :: !Text
  , fpRows   :: ![(Text, RowWrite, Maybe Completion)]
      -- ^ row id, what it writes, and the ledger line riding its success.
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

data Asked = Asked
  { askToday :: !Time.Day      -- ^ the day every date is worked out against.
  , askStamp :: !(Maybe Text)  -- ^ @set-planning@'s date, already rendered.
  }

data CommandSpec = CommandSpec
  { csArgs  :: [Text] -> Args -> Maybe Text
      -- ^ why the request's shape is refused, where it is.
  , csDated :: Bool                -- ^ its @date@ is read against today, once per request.
  , csKind  :: CommandKind         -- ^ what it does to the rows it names.
  }

data CommandKind
  = Splices Reads RowEdits
    -- ^ edits each named row in place; the nine that write spans.
  | Makes
    -- ^ MAKES a row rather than naming one: @capture@, the one that owes no ids.
  | Moves
    -- ^ moves a file out of the tree: @delete@.

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
  [ ("add-tag", CommandSpec (overIds (wantsTag "add-tag")) False
      (Splices ReadsNothing (\_cfg _asked args _doc r -> plain (addTagEdits (tagOf args) r))))
  , ("archive", CommandSpec (overIds (const Nothing)) False
      (Splices ReadsNothing (\_cfg _asked _args _doc r -> plain (archiveEdits r))))
  , ("capture", CommandSpec (overIds wantsCapture) False Makes)
    -- THE ONE DESTRUCTIVE COMMAND: it moves a FILE rather than splicing spans,
    -- and every wall it has is per row and checked HERE as well as in the shell.
  , ("delete", CommandSpec (overIds (const Nothing)) False Moves)
  , ("edit-link", CommandSpec wantsLink False
      (Splices ReadsFile (\_cfg _asked args doc r ->
               plain =<< editLinkEdits (fromMaybe (Span 0 0) (agSpan args))
                                       (word agTarget args) (agDesc args) doc r)))
  , ("remove-tag", CommandSpec (overIds (wantsTag "remove-tag")) False
      (Splices ReadsFile (\_cfg _asked args doc r -> plain (removeTagEdits (tagOf args) doc r))))
  , ("rename-tag", CommandSpec (overIds wantsRename) False
      (Splices ReadsFile (\_cfg _asked args doc r ->
               plain (renameTagEdits (word agFrom args) (word agTo args) doc r))))
  , ("set-planning", CommandSpec (overIds wantsPlanning) True
      (Splices ReadsFile (\_cfg asked args doc r ->
               plain =<< setPlanningEdits (keyOf args) (askStamp asked) doc r)))
    -- A REPEAT IS A `set-state', and the one command that RECORDS anything.
  , ("set-state", CommandSpec (overIds wantsState) False
      (Splices ReadsFile stateEdits))
  , ("set-priority", CommandSpec (overIds wantsPriority) False
      (Splices ReadsFile
        (\_cfg _asked args doc r -> plain =<< setPriorityEdits (join (agPriority args)) doc r)))
  , ("set-title", CommandSpec (overIds wantsTitle) False
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
runCommand opts hub request = withBody request $ \raw -> do
  st <- readTVarIO (hubStore hub)
  case parseCommand raw of
    Left why -> pure (jsonError status400 why)
    -- ONE TOTAL CASE over the kinds there are; the kind is destructured ONCE.
    Right cmd -> case csKind (cmdSpec cmd) of
      Moves -> deleteRows opts hub st cmd
      Makes -> captureInto opts hub st cmd
      Splices reads' edits -> do
        asked <- resolveAsked cmd
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
  case planCommand docs named st asked edits cmd of
    Left why -> pure (jsonError status400 why)
    Right (plans, said) -> do
      written <- mapM (writeOne opts hub) plans
      let outcomes = said <> concat written
      pure (jsonResponse status200
              ["results" .= [ v | rid <- cmdIds cmd, Just v <- [lookup rid outcomes] ]])

-- | The FILES ROWS sit in, each read ONCE per request and PINNED against the
-- digest their parse took ('rowSnapshot').  Rows of one file share that digest,
-- so which of them supplies the pin does not matter; a file that has moved
-- comes back as the refusal its rows are answered with.
documentsFor :: [HeadlineRecord] -> IO (Map FilePath (Either WriteFailure Text))
documentsFor rows =
  traverse pinnedDocument (Map.fromList [ (hrFile r, rowSnapshot r) | r <- rows ])

-- | ONE clock read, before any row: a marked set must not cross midnight.
--
-- THE KEYWORD PICKS THE WALL, 'plannedValue' being the one place that choice is
-- made: the two this server composes for read the whole date grammar, @CLOSED@
-- reparses org's own bracket and nothing else.  A word naming no planning entry
-- never reaches here — `wantsPlanning' has refused the request already.
resolveAsked :: Command -> IO (Either Text Asked)
resolveAsked cmd = do
  day <- today
  pure $ case join (agDate (cmdArgs cmd)) of
    Just text | csDated (cmdSpec cmd) -> Asked day . Just <$> plannedValue day key text
    _nothingToResolve                 -> Right (Asked day Nothing)
  where key = keyOf (cmdArgs cmd)

captureInto :: ServeOptions -> Hub -> Store -> Command -> IO Response
captureInto opts hub st cmd =
  maybe (captureInbox opts hub st args) (captureBlob opts hub st args) (agTag args)
  where args = cmdArgs cmd

captureInbox :: ServeOptions -> Hub -> Store -> Args -> IO Response
captureInbox opts hub st args = do
    (doc, digest) <- currentDocument inbox
    now <- Time.getZonedTime
    let composed = do
          entry <- capturedEntry (stConfig st) now (eolOf doc) Nothing args
          captureEdits doc (captureStamp now) entry
    case composed of
      Left why    -> pure (jsonError status400 why)
      Right edits -> answerWrite (captureMoved inbox) (landed inbox)
                       <$> writeSpans (walkFor opts) hub inbox digest edits
  where
    inbox = captureTargetIn (soDir opts)
    -- A RACE, honestly: @\/command@ never writes the store, so K is the last
    -- load's count.  'recordsUnder', since 'storeRecords' drops a collision loser.
    landed path fresh = captured path fresh (rowIdIn path (length (recordsUnder path st)))

-- | The write goes out under the EMPTY digest, so an occupied path DRIFTS.
captureBlob :: ServeOptions -> Hub -> Store -> Args -> Text -> IO Response
captureBlob opts hub st args tag = do
  there <- doesDirectoryExist store
  if not there then pure (jsonError status400 noStore) else do
    layers <- layersFor (soDir opts) st
    now <- Time.getZonedTime
    ident <- mintBlobId
    let template = fromMaybe bareTemplate (captureTemplateIn tag layers)
        path = blobPathIn store ident
        -- THE BLOB IS A NEW FILE, so it has no line ending of its own to keep:
        -- it takes the TEMPLATE'S, the bytes it is composed out of.  The older
        -- road already did — `expandTemplate' copies the template verbatim, and
        -- the drawer splice under it reads `eolOf' off what came back — and the
        -- widened road's head line must join the same way, or a CRLF layer lands
        -- a blob whose headline ends one way and whose body ends the other.
        composed = do
          entry <- capturedEntry (stConfig st) now (eolOf template) (Just template) args
          blobDocument (BlobSeed tag ident (captureStamp now)) entry
    case composed of
      Left why  -> pure (jsonError status400 why)
      Right doc -> answerWrite (captureMoved path) (landed path ident)
                     <$> writeSpans (walkFor opts) hub path "" [(Span 0 0, doc)]
  where
    store = storeRootIn (soDir opts)
    landed path ident fresh = captured path fresh ident
    noStore = T.pack store <> " is not there, so this tree keeps no org-glance store;\
                               \ capture with no tag to file into the inbox instead"

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
capturedEntry cfg now eol template args = case agTitle args of
  Just title -> do
    stated cfg args
    plan <- traverse (plannedPair (Time.localDay (Time.zonedTimeToLocalTime now)))
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
      expandTemplate now answers text tpl

-- | ONE PLANNING ENTRY through 'plannedValue', THE WALL'S OWN SENTENCE kept: the
-- draft meets what @set-planning@ and @POST \/headline@ meet, and an unknown key
-- outranks every value at all three.
plannedPair :: Time.Day -> (Text, Text) -> Either Text (Text, Text)
plannedPair day (key, value) = case unplanned key of
  Just why -> Left why
  Nothing  -> (,) key <$> plannedValue day key value

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
captured path fresh ident =
  ["ok" .= True, "file" .= path, "digest" .= fresh, "id" .= ident]

capturedParts :: Args -> Either Text (Text, [(Text, Text)])
capturedParts args =
  (,) <$> captureText (capturedText args)
      <*> traverse answered (Map.toList (fromMaybe Map.empty (agFields args)))
  where
    answered (want, value) =
      (,) want <$> first (\why -> "the answer to " <> want <> ": " <> why)
                         (captureText value)

writeOne :: ServeOptions -> Hub -> FilePlan -> IO [(Text, Value)]
writeOne opts hub plan = do
  written <- writeSpans (walkFor opts) hub (fpPath plan) (fpDigest plan) spliced
  -- THE LEDGER RIDES THE SUCCESS BRANCH.  HERE rather than in `replaceSpans'
  -- beside `noteExternalWrite': a completion is keyed off the SERVED ROOT.
  case written of
    Right _digest -> mapM_ record (fpRows plan)
    Left _refused -> pure ()
  pure (report written)
  where
    spliced = concat [ rwEdits w | (_rid, w, _note) <- fpRows plan ]
    record (_rid, _write, note) = mapM_ (noteCompletion (soDir opts)) note
    report written = [ (rid, either (refused rid . why) (done rid) written)
                     | (rid, _write, _note) <- fpRows plan ]
    why = writeWhy (fpPath plan)

-- | Why PATH wrote nothing.  ONE SENTENCE, TWO ASKS: the plan refuses a row
-- whose file moved before the parse's spans are cut, and 'writeSpans' refuses
-- one that moved after.
writeWhy :: FilePath -> WriteFailure -> Text
writeWhy path (WriteDrift found) =
  T.pack path <> " changed on disk (it digests to " <> T.take 12 found
    <> "… now); nothing was written to it"
writeWhy _path (WriteRefused spelled) = spelled

planCommand :: Map FilePath (Either WriteFailure Text) -> ([HeadlineRecord], [Text])
            -> Store -> Asked -> RowEdits -> Command
            -> Either Text ([FilePlan], [(Text, Value)])
planCommand docs (held, absent) st asked rowEdits cmd = do
  rows <- mapM withEdits standing
  let groups = groupOn (hrFile . fst) rows
  pure ( [ FilePlan path (hrDigest r0) [ (hrId r, w, noted r w) | (r, w) <- rs ]
         | (path, rs@((r0, _) : _)) <- groups, not (stale rs) ]
       , missing <> moved <> [ (hrId r, refused (hrId r) (staleWhy path))
                             | (path, rs) <- groups, stale rs, (r, _w) <- rs ] )
  where
    -- A ROW WHOSE PINNED READ WAS REFUSED IS REFUSED HERE, in the write door's
    -- own words: its spans were cut from bytes the file no longer holds, so
    -- nothing computes edits over it.  A command whose edits ignore the
    -- document reads no file, so its map is empty and every row stands.
    (moved, standing) = partitionEithers (map textFor held)
    textFor r = case fromMaybe (Right "") (Map.lookup (hrFile r) docs) of
      Left failed -> Left (hrId r, refused (hrId r) (writeWhy (hrFile r) failed))
      Right doc   -> Right (r, doc)
    withEdits (r, doc) = (,) r <$> rowEdits (stConfig st) asked (cmdArgs cmd) doc r
    -- Keyed by `ORG_GLANCE_ID': an ordinal names a different row a week on.
    noted r w = do
      (state, shifted) <- rwNote w
      ident <- hrOrgId r
      pure (Completion ident state shifted)
    missing = [ (rid, refused rid (noSuchRow rid)) | rid <- absent ]
    stale rs = or [ pinned /= hrDigest r
                  | (r, _w) <- rs, Just pinned <- [Map.lookup (hrId r) (cmdDigests cmd)] ]
    staleWhy path = T.pack path
                      <> " has been re-read since these rows were fetched; ask for them again"

done :: Text -> Text -> Value
done rid digest = object [ "id" .= rid, "ok" .= True, "digest" .= digest ]

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
