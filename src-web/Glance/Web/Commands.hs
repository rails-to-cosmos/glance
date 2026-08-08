-- | @POST \/command@: the structured writes, as ONE table.
--
-- A command is one entry in 'commands' — what its request's shape owes, whether
-- its date is resolved against today, and what it does to a row — so adding one
-- is adding a row rather than a name in a list, two guards and a case arm.  The
-- edits themselves are 'Glance.Query''s: a headline's spans belong to the
-- private sublibrary and this layer cannot see them.
module Glance.Web.Commands (runCommand) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Aeson (Object, Value, object, (.:), (.:!), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Network.HTTP.Types (status200, status400)
import Network.Wai (Request, Response)
import System.Directory (doesDirectoryExist)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as Time

import Glance.Query ( Repeat (..), noteCompletion, repeatOn, rowOrgId
                    , BlobSeed (..), ConfigLayers
                    , HeadlineRecord (hrDigest, hrFile, hrId)
                    , Span (Span), WalkOptions, WriteFailure (..)
                    , addTagEdits, archiveEdits, bareTemplate, blobDocument
                    , blobPathIn, captureEdits, captureStamp, captureText
                    , captureTargetIn, captureTemplateIn, currentDocument
                    , editLinkEdits, expandTemplate, mintBlobId
                    , planningTimestamp, priorityText
                    , removeTagEdits
                    , renameTagEdits, rowIdIn, setPlanningEdits
                    , setPriorityEdits, setStateEdits, setTitleEdits
                    , storeRootIn, tagText, titleText )
import Glance.Web.Base ( ServeOptions (soDir), answerWrite, bodyObject, captureMoved
                       , jsonError, jsonResponse, noSuchRow, walkFor, withBody )
import Glance.Web.Store ( Hub, Store (stConfig), headlinesIn, hubStore, layersFor
                        , recordsUnder, storeDocument, storeRecords )
import Glance.Web.Watch (writeSpans)

-- Commands

-- | What a command request asks for.  A 'Command' cannot be built without its
-- 'CommandSpec' ('parseCommand'), which puts the unknown-name refusal ahead of
-- everything else: what reaches the planner is a command this server
-- implements, and the NAME has no reader left past the lookup that resolved it.
data Command = Command
  { cmdSpec    :: !CommandSpec      -- ^ its entry in 'commands'.
  , cmdIds     :: ![Text]           -- ^ in the order named, deduplicated; empty for @capture@.
  , cmdArgs    :: !Args             -- ^ whatever @args@ carried.
  , cmdDigests :: !(Map Text Text)  -- ^ id to the digest the client holds for its file.
  }

-- | The @args@ object, read once for every command that takes one.  Twelve
-- fields between them and a request naming one command leaves the rest absent:
-- @keyword@ (@set-state@'s state and @set-planning@'s keyword, one field the
-- wire spells both ways), @date@, @text@, @tag@, @fields@, @from@\/@to@, and
-- @edit-link@'s @span@\/@target@\/@desc@.
--
-- THE NESTED 'Maybe's are the distinction the command layer turns on: ABSENT
-- said nothing, NULL asked for the value to come off.  An absent @keyword@ or
-- @date@ is a 400; an absent @desc@ is @edit-link@'s ordinary case.  The flat
-- fields belong to commands with no value to clear.
--
-- @rename-tag@ spells @from@\/@to@ rather than reusing @tag@ for one half, and
-- @set-title@ spells @title@ rather than @text@: a capture's line is a whole
-- headline as org, a title one component of one.
data Args = Args
  { agKeyword :: !(Maybe (Maybe Text))
  , agDate    :: !(Maybe (Maybe Text))
  , agText    :: !(Maybe Text)
  , agTitle    :: !(Maybe Text)
  , agPriority :: !(Maybe (Maybe Text))
  , agTag     :: !(Maybe Text)
  , agFields  :: !(Maybe (Map Text Text))
  , agFrom    :: !(Maybe Text)
  , agTo      :: !(Maybe Text)
  , agSpan    :: !(Maybe Span)
  , agTarget  :: !(Maybe Text)
  , agDesc    :: !(Maybe (Maybe Text))
  }

-- | One file's share of a command: the write it costs, and the ids it answers
-- for.  Every row of a file shares the file's digest, so the plan carries one.
data FilePlan = FilePlan
  { fpPath   :: !FilePath
  , fpDigest :: !Text
  , fpRows   :: ![(Text, [(Span, Text)], Maybe (Text, Text, Text))]
      -- ^ row id, the spans it moves, and what to record on success.
  }

-- | The span edits a command asks for on ONE row, under the store's config and
-- the timestamp the request resolved to ('resolveDate').  A refusal here is the
-- WHOLE request's, since half a write over a marked set is worse than none of
-- one.
type RowEdits = ConfigLayers -> Asked -> Args -> HeadlineRecord
              -> Either Text [(Span, Text)]

-- | What a request resolved BEFORE any row was touched: the clock, read once.
-- A record rather than a widening argument list, `ConfigParts`' reason — the
-- next request-level value joins here and no row function changes shape.
data Asked = Asked
  { askToday :: !Time.Day      -- ^ the day every date is worked out against.
  , askStamp :: !(Maybe Text)  -- ^ @set-planning@'s date, already rendered.
  }

-- | What a command RECORDS about a row it wrote, beyond the file itself: the
-- state it landed on and its next occurrence. Only a repeat has one, and the
-- line rides the write's success branch.
type RowNote = ConfigLayers -> Asked -> Args -> HeadlineRecord -> Maybe (Text, Text)

-- | One command, as its three fields.  'Nothing' for the edits is the command
-- that names no rows — it MAKES one, so there is no row function to hold and no
-- ids rule to apply.
--
-- 'csArgs' is handed the IDS beside the @args@ because a shape refusal is about
-- the request rather than the @args@ object alone: @edit-link@ carries a span,
-- which means nothing to a second row and a different range in each file, so
-- "one row" is part of what its SHAPE owes.  Nine of the ten entries ignore the
-- list — a rule only one command has, kept out of a flag every entry would
-- answer.
data CommandSpec = CommandSpec
  { csArgs  :: [Text] -> Args -> Maybe Text
      -- ^ why the request's shape is refused, where it is.
  , csDated :: Bool                -- ^ its @date@ is read against today, once per request.
  , csEdits :: Maybe RowEdits      -- ^ what it does to each named row.
  , csNotes :: Maybe RowNote       -- ^ what it records about one, where it records anything.
  }

-- | The commands this route implements, which is also the whole of what @args@
-- can mean: @set-state@ @{"keyword": "DONE"}@ or @{"keyword": null}@,
-- @set-planning@ @{"keyword": "SCHEDULED", "date": "+3d"}@ or a null date,
-- @capture@ @{"text": "TODO Buy milk :errands:"}@ with an optional @tag@ and
-- the @fields@ its template asks for, @add-tag@ and @remove-tag@
-- @{"tag": "work"}@, @rename-tag@ @{"from": "work", "to": "projects"}@,
-- @edit-link@ @{"span": [S, E], "target": "https:\/\/x", "desc": "…"}@, and
-- @archive@ nothing.
--
-- ONE table: 'commandNames' is its keys, 'parseCommand' refuses a name it does
-- not carry and asks the entry's own 'csArgs' for the rest, and 'runCommand'
-- reads the edits off the entry the name resolved to.  A lookup past the parse
-- cannot miss, so there is no arm for a command that is not there.
commands :: [(Text, CommandSpec)]
commands =
  [ ("add-tag", CommandSpec (overIds (wantsTag "add-tag")) False
      (Just (\_cfg _asked args r -> Right (addTagEdits (tagOf args) r))) Nothing)
    -- @archive@ is 'addTagEdits' at a fixed name, and it takes no @tag@ of its
    -- own: the name is org's and a client cannot aim it elsewhere.  Idempotent,
    -- so a row already tagged costs no edit and still answers @ok@.
  , ("archive", CommandSpec (overIds (const Nothing)) False
      (Just (\_cfg _asked _args r -> Right (archiveEdits r))) Nothing)
  , ("capture", CommandSpec (overIds wantsText) False Nothing Nothing)
    -- The ONE command whose args describe a row's own TEXT rather than a
    -- property of it, so its shape includes the ROW COUNT ('wantsLink', which
    -- refuses a missing span or target too, so neither can refuse per row).
  , ("edit-link", CommandSpec wantsLink False
      (Just (\_cfg _asked args r ->
               editLinkEdits (fromMaybe (Span 0 0) (agSpan args))
                             (word agTarget args) (agDesc args) r)) Nothing)
  , ("remove-tag", CommandSpec (overIds (wantsTag "remove-tag")) False
      (Just (\_cfg _asked args r -> Right (removeTagEdits (tagOf args) r))) Nothing)
    -- One command rather than a @remove-tag@ and an @add-tag@ fired in turn:
    -- the rename is ONE drift-locked write per file, and the pair it would
    -- compose from spells the tag onto the title ('Glance.Query.renameTagEdits').
  , ("rename-tag", CommandSpec (overIds wantsRename) False
      (Just (\_cfg _asked args r ->
               Right (renameTagEdits (word agFrom args) (word agTo args) r))) Nothing)
    -- The keyword is there and the date has been resolved: 'csArgs' refuses a
    -- @set-planning@ without either, so neither can refuse per row.
  , ("set-planning", CommandSpec (overIds wantsPlanning) True
      (Just (\_cfg asked args r ->
               setPlanningEdits (fromMaybe "" (join (agKeyword args))) (askStamp asked) r))
      Nothing)
    -- A REPEAT IS A `set-state', and the one command that RECORDS anything: a
    -- row completed into an inactive keyword while a planning stamp carries a
    -- repeater shifts and resets in ONE write, and the ledger line rides its
    -- success ('Glance.Query.repeatOn').
  , ("set-state", CommandSpec (overIds wantsState) False
      (Just stateEdits) (Just completed))
    -- The title is the one CELL a reader edits as text, so it is a command
    -- rather than a subtree write: 'Glance.Query.setTitleEdits' replaces the
    -- title's own span and leaves the keyword, the priority and the tags around
    -- it their bytes, where a recomposed subtree would rewrite the line.  The
    -- priority is org's own token rather than a cell a reader types, so the key
    -- CYCLES it and this takes the letter that cycle landed on — nullable for
    -- the reason @set-state@ is, the wrap going through NONE.
  , ("set-priority", CommandSpec (overIds wantsPriority) False
      (Just (\_cfg _asked args r -> setPriorityEdits (join (agPriority args)) r)) Nothing)
  , ("set-title", CommandSpec (overIds wantsTitle) False
      (Just (\_cfg _asked args r -> setTitleEdits (word agTitle args) r)) Nothing)
  ]
  where
    -- A shape that has nothing to say about HOW MANY rows were named, which is
    -- nine of the ten.
    overIds = const
    -- `set-state', with org's repeat folded in: a row that repeats shifts its
    -- stamp and resets its keyword in one set, and anything else takes the
    -- plain path with the refusal surface unchanged.
    stateEdits cfg asked args r = case repeating cfg asked args r of
      Just rp -> Right (rpEdits rp)
      Nothing -> setStateEdits cfg (join (agKeyword args)) r
    completed cfg asked args r =
      (\rp -> (rpState rp, rpShifted rp)) <$> repeating cfg asked args r
    repeating cfg asked args r =
      join (agKeyword args) >>= \keyword -> repeatOn cfg (askToday asked) keyword r
    word field = fromMaybe "" . field
    tagOf = word agTag
    -- The one command whose keyword may be NULL: that is how a state comes off,
    -- so what it owes is the FIELD rather than a value ('Args').
    wantsState args
      | Nothing <- agKeyword args =
          Just "set-state wants args {\"keyword\": \"DONE\"}, or a null keyword to clear it"
      | otherwise = Nothing
    wantsPlanning args
      | Nothing <- join (agKeyword args) =
          Just "set-planning wants args {\"keyword\": \"SCHEDULED\", \"date\": \"+3d\"}"
      | Nothing <- agDate args =
          Just "set-planning wants a date, or a null one to take the entry off"
      | otherwise = Nothing
    -- @capture@'s @tag@ is optional and its charset is the ordinary tag wall:
    -- absent files into the tree's inbox, present files a BLOB into the store.
    -- A word that is not a tag names no directory to write and no vocabulary to
    -- join, so it is refused with the rest of the request's shape.
    wantsText args
      | Nothing <- agText args =
          Just "capture wants args {\"text\": \"TODO Buy milk :errands:\"}"
      | Just given <- agTag args = either Just (const Nothing) (tagText given)
      | otherwise = Nothing
    -- A link points SOMEWHERE, so an empty target is refused with the rest of
    -- the request's shape: it is no more a link for one row than for another.
    -- PADDING is refused beside it, so the string this tests and the string
    -- 'Glance.Query.editLinkEdits' writes are one — a target is written
    -- verbatim, and a bracketed link would otherwise take the spaces a bare one
    -- is refused for.  The description is not asked for: absent is the ordinary
    -- request and leaves the link the one it has.  THE ROW COUNT IS FIRST as the
    -- coarsest thing wrong — a caller naming three rows has misunderstood the
    -- command, and a missing span would answer the smaller question.
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
    -- The charset is a property of the STRING, so it is refused with the rest
    -- of the request's shape: a word that is not a tag is not a tag for any row.
    wantsTag name args = case agTag args of
      Nothing    -> Just (name <> " wants args {\"tag\": \"work\"}")
      Just given -> either Just (const Nothing) (tagText given)
    -- BOTH ends take the charset wall: a @from@ org could not have written
    -- names nothing, and a @to@ it could not read would take the whole run down
    -- into title text on the next load.
    wantsRename args = case (agFrom args, agTo args) of
      (Just from, Just to) -> either Just (const Nothing) (tagText from >> tagText to)
      _absent -> Just "rename-tag wants args {\"from\": \"work\", \"to\": \"projects\"}"
    -- Charset, emptiness and the one-line rule are properties of the STRING
    -- too, so they are refused with the shape rather than once per row.
    wantsPriority args = case agPriority args of
      Nothing        -> Just "set-priority wants args {\"priority\": \"A\"},\
                             \ or a null one to take it off"
      Just Nothing   -> Nothing
      Just (Just given) -> either Just (const Nothing) (priorityText given)
    wantsTitle args = case agTitle args of
      Nothing    -> Just "set-title wants args {\"title\": \"Buy milk\"}"
      Just given -> either Just (const Nothing) (titleText given)

-- | The names 'commands' carries, in the order it carries them — which is the
-- order a refusal lists them in.
commandNames :: [Text]
commandNames = map fst commands

-- | @POST \/command@ with body @{"name", "ids", "args"}@: a structured write
-- over the rows the client names.  @"id"@ is an @"ids"@ of one.  The edits are
-- 'Glance.Query''s; this route owns which rows, which file, and what the answer
-- says.
--
-- BATCHING IS PER FILE: ids group by their row's file and each file is written
-- ONCE under its own digest, so a marked set spanning three files is three
-- atomic writes, each all-or-nothing.  Rollback ACROSS files is impossible, so
-- the answer reports per id.
--
-- A bad body, an unimplemented name, no ids, and a keyword some named row's
-- chain does not declare are 400 with nothing written — the keyword refuses the
-- WHOLE request deliberately, half a state change over a marked set being worse
-- than none.  Per id: an unknown id, and a file whose digest moved.  So a 200
-- is "the command ran" rather than "every row moved".
--
-- Nothing here touches the store: the write goes to the file and the watch
-- streams the rows, so a browser command reaches every tab by an editor's path.
runCommand :: ServeOptions -> Hub -> Request -> IO Response
runCommand opts hub request = withBody request $ \raw -> do
  st <- readTVarIO (hubStore hub)
  case parseCommand raw of
    Left why -> pure (jsonError status400 why)
    -- The one command with no row function MAKES a row, and the 'Maybe' is
    -- destructured HERE, once: what goes down to the planner is the edits
    -- themselves, so nothing below has an arm for a command that edits nothing.
    Right cmd -> case csEdits (cmdSpec cmd) of
      Nothing -> captureInto opts hub st cmd
      -- The clock is read ONCE per request, ahead of any row ('resolveDate'),
      -- and everything that can refuse is decided before a file is opened,
      -- leaving the 400 or the IO that writes.
      Just edits -> do
        asked <- resolveAsked cmd
        either (pure . jsonError status400) id
               (asked >>= \at -> overRows opts hub st at edits cmd)

-- | CMD's rows written, as the IO that writes them or the 400 that stops it.
-- STAMP is @set-planning@'s date already worked out, and is nothing to every
-- other name.
overRows :: ServeOptions -> Hub -> Store -> Asked -> RowEdits -> Command
         -> Either Text (IO Response)
overRows opts hub st asked edits cmd = do
  (plans, said) <- planCommand st asked edits cmd
  pure $ do
    written <- mapM (writeOne opts hub) plans
    -- Answered in the order the client named the ids, so a caller can zip the
    -- results against what it asked for.
    let outcomes = said <> concat written
    pure (jsonResponse status200
            ["results" .= [ v | rid <- cmdIds cmd, Just v <- [lookup rid outcomes] ]])

-- | The timestamp CMD's rows are to carry: its date text rendered against the
-- server's today ('Glance.Query.planningTimestamp'), 'Nothing' where the
-- request asked for the entry to come off or where the command has no date.
--
-- Worked out ONCE, before any row is planned: a marked set crossing midnight
-- would otherwise land on two days, and a text no date parser reads is the
-- WHOLE request's 400 the way an undeclared keyword is ('RowEdits').  The
-- answer is handed on as a value rather than written back into 'Args', so
-- @agDate@ means the text the client typed at every point in the request.
resolveAsked :: Command -> IO (Either Text Asked)
resolveAsked cmd = do
  today <- Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime
  pure $ case join (agDate (cmdArgs cmd)) of
    Just text | csDated (cmdSpec cmd) -> Asked today . Just <$> planningTimestamp today text
    _nothingToResolve                 -> Right (Asked today Nothing)

-- | @capture@: CMD's line written as a new top entry, and WHERE decides which
-- of two shapes it takes.
--
-- NO TAG is the tree's inbox, deliberately bare: the target comes off the
-- config, which also refuses one this daemon will not write to, so a misspelled
-- pragma is a 400 naming itself rather than a file written outside the tree.
-- The entry is @* TEXT@ under a creation stamp, no template consulted.
--
-- A TAG is a BLOB in the store ('captureBlob').
--
-- The document and digest come off the STORE where it holds the file and off a
-- fresh read where it does not — a missing target reads as the empty document
-- under the empty digest, which is what 'editFile' creates under.  Either way
-- the offset and the lock describe one text.
--
-- Both leave through 'Glance.Web.Watch.writeSpans', so the path each wrote is
-- queued for the drain loop — a nudge rather than a store update, and what
-- makes a captured row arrive live rather than at the next restart.
captureInto :: ServeOptions -> Hub -> Store -> Command -> IO Response
captureInto opts hub st cmd =
  -- 'wantsText' has already put every @tag@ past 'tagText', which refuses the
  -- empty string and every character a tag may not hold, so ABSENT is the whole
  -- of what "no tag" means here and there is nothing left to strip or test.
  -- The 'Command' is read ONCE, here: neither path below asks anything of the
  -- name or the ids, a capture naming no row.
  maybe (captureInbox opts hub st args) (captureBlob opts hub st args) (agTag args)
  where args = cmdArgs cmd

-- | @capture@ with no tag: ARGS' line appended to the tree's capture target.
captureInbox :: ServeOptions -> Hub -> Store -> Args -> IO Response
captureInbox opts hub st args = case captureTargetIn (soDir opts) (stConfig st) of
  Left why   -> pure (jsonError status400 why)
  Right path -> do
    (doc, digest) <- maybe (currentDocument path) pure (storeDocument path st)
    now <- Time.getZonedTime
    case captureEdits doc (captureStamp now) (capturedText args) of
      Left why    -> pure (jsonError status400 why)
      Right edits -> answerWrite (captureMoved path) (landed path)
                       <$> writeSpans (walkFor opts) hub path digest edits
  where
    -- The row the entry BECOMES, so the page can land its cursor on it when the
    -- watch delivers it.  A captured entry joins the END of the file and every
    -- ordinal ahead of it is already spent, so K is how many rows the store
    -- holds for this file ('Glance.Query.rowIdIn', one spelling of the
    -- separator).  A RACE, and an honest one: @\/command@ never writes the
    -- store, so this counts what the last load saw, and an entry appended by
    -- another writer between that load and this one shifts the answer by one.
    -- The cost is a cursor that does not move, the shell landing nothing for a
    -- row its view has not got.
    --
    -- 'recordsUnder' rather than a filter over 'storeRecords': that one is
    -- 'Glance.Query.resolveIds' over the WHOLE store, which is a pass over ten
    -- thousand rows per capture AND the wrong list — a row that lost an id
    -- collision is dropped from it, where the ordinal was handed out before any
    -- resolution ran.
    landed path fresh = captured path fresh (rowIdIn path (length (recordsUnder path st)))

-- | @capture@ under a TAG: a new blob in the served root's own store.
--
-- FOUR REFUSALS, all decided before a byte is written, coarsest first.  The
-- store root must be there — a capture that MADE one would be this daemon
-- deciding a tree is an org-glance store.  The tag's template comes off the
-- config layers read HERE rather than off the loaded config, so what a settings
-- sheet shows is what a capture expands; a tag no layer configures takes
-- 'bareTemplate'.  The expansion refuses a template with no @%?@ and an ask
-- nobody answered, and the composition one that expands to no headline.
--
-- The id is minted and the path is org-glance's sharded one; the write goes out
-- under the EMPTY digest, which creates the file and DRIFTS rather than
-- overwrites should anything sit there.  Minting ahead of the last refusal
-- costs nothing — nothing is reserved, so an unwritten id is one nobody sees.
--
-- The @EXTERNAL.jsonl@ note costs nothing either: @data.org@ under a store's
-- @data\/@ is a blob, so 'replaceSpans' appends the line on its way out.  Blob
-- first, line second, the order the contract asks for.
captureBlob :: ServeOptions -> Hub -> Store -> Args -> Text -> IO Response
captureBlob opts hub st args tag = do
  there <- doesDirectoryExist store
  if not there then pure (jsonError status400 noStore) else do
    layers <- layersFor (soDir opts) st
    now <- Time.getZonedTime
    ident <- mintBlobId
    let template = fromMaybe bareTemplate (captureTemplateIn tag layers)
        path = blobPathIn store ident
        composed = do
          (text, answers) <- capturedParts args
          expanded <- expandTemplate now answers text template
          blobDocument (BlobSeed tag ident (captureStamp now)) expanded
    case composed of
      Left why  -> pure (jsonError status400 why)
      -- Every write leaves through 'writeSpans', which is what delivers this
      -- one: a blob's @\<shard>\/\<rest>\/@ pair is one
      -- @createDirectoryIfMissing True@, so it lands under a directory fsnotify
      -- armed and never entered, and no event is coming for it now or later.
      Right doc -> answerWrite (captureMoved path) (landed path ident)
                     <$> writeSpans (walkFor opts) hub path "" [(Span 0 0, doc)]
  where
    store = storeRootIn (soDir opts)
    landed path ident fresh = captured path fresh ident
    noStore = T.pack store <> " is not there, so this tree keeps no org-glance store;\
                               \ capture with no tag to file into the inbox instead"

-- | The line ARGS captures, which every capture path writes and no path may be
-- without ('wantsText').
capturedText :: Args -> Text
capturedText = fromMaybe "" . agText

-- | What a capture answers with: the FILE it wrote, that file's FRESH digest,
-- and the id of the row it made.  One spelling for both paths, since a client
-- reads one shape whichever it asked for; the id itself is each path's own —
-- the blob's is minted, the inbox row's is an ordinal.
captured :: FilePath -> Text -> Text -> [Pair]
captured path fresh ident =
  ["ok" .= True, "file" .= path, "digest" .= fresh, "id" .= ident]

-- | ARGS' captured line and its @fields@ answers, each past the one-headline
-- wall, or the whole request's refusal naming the field that failed it.
--
-- THE INBOX WALL REACHES THE TAGGED PATH TOO.  A template's @%?@ and each of
-- its @%^{PROMPT}@ holes are spliced into the same document, so a newline in any
-- of them lands a column-1 star the parser reads as a second entry — and a blob
-- holds ONE entry, which is the headline org-glance keys it by.  An empty
-- answer writes a template with a hole in it.
--
-- Answers come back in 'Data.Map.Strict' key order, which is what
-- 'Glance.Query.expandTemplate' looks a prompt up in; order means nothing to a
-- lookup.
capturedParts :: Args -> Either Text (Text, [(Text, Text)])
capturedParts args =
  (,) <$> captureText (capturedText args)
      <*> traverse answered (Map.toList (fromMaybe Map.empty (agFields args)))
  where
    answered (want, value) =
      (,) want <$> first (\why -> "the answer to " <> want <> ": " <> why)
                         (captureText value)

-- | PLAN's file written once, and what that came to for each of its ids.  Both
-- outcomes are shared by the whole group, because the write is: the batch lands
-- or the file is untouched.
writeOne :: ServeOptions -> Hub -> FilePlan -> IO [(Text, Value)]
writeOne opts hub plan = do
  written <- writeSpans (walkFor opts) hub (fpPath plan) (fpDigest plan) spliced
  -- THE LEDGER RIDES THE SUCCESS BRANCH, where `noteExternalWrite' already
  -- sits: the file is renamed into place by now, so a note that cannot be
  -- written changes nothing about the write that landed.
  case written of
    Right _digest -> mapM_ record (fpRows plan)
    Left _refused -> pure ()
  pure (report written)
  where
    spliced = concat [ edits | (_rid, edits, _note) <- fpRows plan ]
    record (_rid, _edits, note) = case note of
      Just (ident, state, shifted) ->
        noteCompletion (soDir opts) (Just ident) state shifted
      Nothing -> pure ()
    report written = [ (rid, either (refused rid . why) (done rid) written)
                     | (rid, _edits, _note) <- fpRows plan ]
    why (WriteDrift found) = T.pack (fpPath plan) <> " changed on disk (it digests to "
                               <> T.take 12 found <> "… now); nothing was written to it"
    why (WriteRefused spelled) = spelled

-- | CMD as the files to write and the ids refused without opening one, or as
-- the 400 that stops it.  Two refusals are decided here rather than in the IO
-- above: an unknown id, and a digest the client pinned that the store no longer
-- holds — @POST \/headline@'s own @stale@ check, per file because a digest is.
planCommand :: Store -> Asked -> RowEdits -> Command
            -> Either Text ([FilePlan], [(Text, Value)])
planCommand st asked rowEdits cmd = do
  rows <- mapM withEdits held
  let groups = groupOn (hrFile . fst) rows
  pure ( [ FilePlan path (hrDigest r0) [ (hrId r, edits, noted r) | (r, edits) <- rs ]
         | (path, rs@((r0, _) : _)) <- groups, not (stale rs) ]
       , missing <> [ (hrId r, refused (hrId r) (staleWhy path))
                    | (path, rs) <- groups, stale rs, (r, _edits) <- rs ] )
  where
    -- One resolution for the whole set rather than one per id, which is what
    -- keeps a marked set of a hundred rows off a hundred passes of the store.
    (held, absent) = headlinesIn (storeRecords st) (cmdIds cmd)
    withEdits r = (,) r <$> rowEdits (stConfig st) asked (cmdArgs cmd) r
    -- What this command records about a row, where it records anything: the
    -- entry's own id beside it, since the ledger is keyed by `ORG_GLANCE_ID'
    -- and an ordinal names a different row a week on.
    noted r = do
      note <- csNotes (cmdSpec cmd)
      (state, shifted) <- note (stConfig st) asked (cmdArgs cmd) r
      ident <- rowOrgId r
      pure (ident, state, shifted)
    missing = [ (rid, refused rid (noSuchRow rid)) | rid <- absent ]
    stale rs = or [ pinned /= hrDigest r
                  | (r, _edits) <- rs, Just pinned <- [Map.lookup (hrId r) (cmdDigests cmd)] ]
    staleWhy path = T.pack path
                      <> " has been re-read since these rows were fetched; ask for them again"

-- | One row's outcome: the file's new digest, so a caller can pin its next
-- write without re-reading.
done :: Text -> Text -> Value
done rid digest = object [ "id" .= rid, "ok" .= True, "digest" .= digest ]

-- | One row's refusal, shaped like the success it replaces.
refused :: Text -> Text -> Value
refused rid why = object [ "id" .= rid, "ok" .= False, "error" .= why ]

-- | XS grouped by KEY: each group in arrival order, the groups in first-seen
-- order.  Quadratic in the number of distinct files, which is the marked set's
-- size and not the store's.
groupOn :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOn key xs = [ (k, [ x | x <- xs, key x == k ]) | k <- nub (map key xs) ]

-- | RAW as a command, or what is wrong with it.  Every refusal that is the
-- request's shape rather than the tree's state is decided here, so what reaches
-- 'planCommand' is a name it implements with rows to run it over.  The name
-- resolves to its entry in 'commands' FIRST and the 'Command' is built out of
-- that entry, so an unimplemented name never reaches a rule that would have to
-- guess what it takes; what is left is the entry's own two — whether it names
-- rows, and what the request's shape owes it ('csArgs', handed the ids beside
-- the @args@).
parseCommand :: BL.ByteString -> Either Text Command
parseCommand raw = bodyObject "command" command raw >>= checked
  where
    command o = do
      name <- o .: "name"
      one <- o .:? "id"
      several <- o .:? "ids"
      digests <- o .:? "digests"
      -- @.:!@ rather than @.:?@ for the nullable fields, and that is the whole
      -- of how ABSENT is told from NULL: @.:?@ folds a null into an absence,
      -- which would make @{"args": {}}@ an instruction to clear.  A request
      -- with no @args@ at all reads as an empty one — no second shape to carry.
      a <- fromMaybe mempty <$> (o .:? "args" :: Parser (Maybe Object))
      -- The span arrives as the half-open pair it is, @[START, END]@, and is a
      -- 'Span' from here on: the offsets are the parser's own currency and
      -- nothing below this line reads them as a tuple.
      sp <- fmap (uncurry Span) <$> (a .:? "span" :: Parser (Maybe (Int, Int)))
      parsed <- Args <$> a .:! "keyword" <*> a .:! "date" <*> a .:? "text"
                     <*> a .:? "title" <*> a .:! "priority" <*> a .:? "tag"
                     <*> a .:? "fields" <*> a .:? "from" <*> a .:? "to"
                     <*> pure sp <*> a .:? "target" <*> a .:! "desc"
      pure ( name :: Text, nub (maybe [] pure one <> fromMaybe [] several)
           , parsed, fromMaybe Map.empty digests )
    checked (name, ids, args, digests) = case lookup name commands of
      Nothing -> Left ("no such command: " <> name <> "; this server runs "
                         <> T.intercalate " and " commandNames)
      Just spec
        | isJust (csEdits spec), null ids ->
            Left "a command names rows: {\"ids\": [\"…\"]}, or {\"id\": \"…\"} for one"
        | Just why <- csArgs spec ids args -> Left why
        | otherwise -> Right (Command spec ids args digests)
