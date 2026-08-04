-- | @POST \/command@: the structured writes, as ONE table.
--
-- A command is one entry in 'commands' — what its request's shape owes, whether
-- its date is resolved against today, and what it does to a row — so adding one
-- is adding a row rather than a name in a list, two guards and a case arm.  The
-- edits themselves are 'Glance.Query''s: a headline's spans belong to the
-- private sublibrary and this layer cannot see them.
module Glance.Web.Commands (commands, commandNames, runCommand) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (join)
import Data.Aeson (Object, Value, object, (.:), (.:!), (.:?), (.=))
import Data.Aeson.Types (Parser)
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

import Glance.Query ( ConfigLayers, HeadlineRecord (hrDigest, hrFile, hrId)
                    , Span (Span), WalkOptions, WriteFailure (..)
                    , addTagEdits, archiveEdits, bareTemplate, blobDocument
                    , blobPathIn, captureEdits, captureStamp
                    , captureTargetIn, captureTemplateIn, currentDocument
                    , editLinkEdits, expandTemplate, mintBlobId
                    , planningTimestamp, priorityText, readConfigLayers
                    , removeTagEdits
                    , renameTagEdits, rowIdIn, setPlanningEdits
                    , setPriorityEdits, setStateEdits, setTitleEdits
                    , storeRootIn, tagText, titleText )
import Glance.Web.Base ( ServeOptions (soDir), answerWrite, bodyObject, captureMoved
                       , jsonError, jsonResponse, noSuchRow, walkFor, withBody )
import Glance.Web.Store ( Hub, Store (stConfig), configDirsIn, headlinesIn, hubStore
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
-- fields between them, and a request naming one command leaves the rest absent:
-- @keyword@ is @set-state@'s state and @set-planning@'s planning keyword (one
-- field, the wire spelling both that way), @date@ the timestamp text, @text@
-- the line @capture@ writes, @tag@ the one @add-tag@ and @remove-tag@ move AND
-- the one @capture@ files under, @fields@ the answers @capture@ carries for its
-- template's @%^{PROMPT}@ asks, @from@\/@to@ @rename-tag@'s pair, and @span@,
-- @target@ and @desc@ @edit-link@'s three.
--
-- The nested 'Maybe's are the distinction the whole command layer turns on:
-- ABSENT said nothing, NULL asked for the value to come off.  An absent
-- @keyword@ or @date@ is a 400, neither command having anything to do without
-- it; an absent @desc@ is @edit-link@'s ordinary case and leaves the link the
-- description it has.  The flat fields belong to commands with no value to
-- clear — a tag comes off through @remove-tag@ rather than through a null.
--
-- @rename-tag@ spells its pair @from@\/@to@ rather than reusing @tag@ for one
-- half, the two being symmetric and @tag@ leaving a reader guessing which end.
-- @set-title@ spells its own @title@ rather than reusing @text@: @capture@'s
-- line is a whole headline as org, and a title is one component of one.
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
  , fpRows   :: ![(Text, [(Span, Text)])]  -- ^ row id, and the spans it moves.
  }

-- | The span edits a command asks for on ONE row, under the store's config and
-- the timestamp the request resolved to ('resolveDate').  A refusal here is the
-- WHOLE request's, since half a write over a marked set is worse than none of
-- one.
type RowEdits = ConfigLayers -> Maybe Text -> Args -> HeadlineRecord
              -> Either Text [(Span, Text)]

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
      (Just (\_cfg _stamp args r -> Right (addTagEdits (tagOf args) r))))
    -- @archive@ is 'addTagEdits' at a fixed name, and it takes no @tag@ of its
    -- own: the name is org's and a client cannot aim it elsewhere.  Idempotent,
    -- so a row already tagged costs no edit and still answers @ok@.
  , ("archive", CommandSpec (overIds (const Nothing)) False
      (Just (\_cfg _stamp _args r -> Right (archiveEdits r))))
  , ("capture", CommandSpec (overIds wantsText) False Nothing)
    -- The ONE command whose args describe a row's own TEXT rather than a
    -- property of it, so its shape includes the ROW COUNT ('wantsLink', which
    -- refuses a missing span or target too, so neither can refuse per row).
  , ("edit-link", CommandSpec wantsLink False
      (Just (\_cfg _stamp args r ->
               editLinkEdits (fromMaybe (Span 0 0) (agSpan args))
                             (word agTarget args) (agDesc args) r)))
  , ("remove-tag", CommandSpec (overIds (wantsTag "remove-tag")) False
      (Just (\_cfg _stamp args r -> Right (removeTagEdits (tagOf args) r))))
    -- One command rather than a @remove-tag@ and an @add-tag@ fired in turn:
    -- the rename is ONE drift-locked write per file, and the pair it would
    -- compose from spells the tag onto the title ('Glance.Query.renameTagEdits').
  , ("rename-tag", CommandSpec (overIds wantsRename) False
      (Just (\_cfg _stamp args r ->
               Right (renameTagEdits (word agFrom args) (word agTo args) r))))
    -- The keyword is there and the date has been resolved: 'csArgs' refuses a
    -- @set-planning@ without either, so neither can refuse per row.
  , ("set-planning", CommandSpec (overIds wantsPlanning) True
      (Just (\_cfg stamp args r ->
               setPlanningEdits (fromMaybe "" (join (agKeyword args))) stamp r)))
  , ("set-state", CommandSpec (overIds wantsState) False
      (Just (\cfg _stamp args r -> setStateEdits cfg (join (agKeyword args)) r)))
    -- The title is the one CELL a reader edits as text, so it is a command
    -- rather than a subtree write: 'Glance.Query.setTitleEdits' replaces the
    -- title's own span and leaves the keyword, the priority and the tags around
    -- it their bytes, where a recomposed subtree would rewrite the line.  The
    -- priority is org's own token rather than a cell a reader types, so the key
    -- CYCLES it and this takes the letter that cycle landed on — nullable for
    -- the reason @set-state@ is, the wrap going through NONE.
  , ("set-priority", CommandSpec (overIds wantsPriority) False
      (Just (\_cfg _stamp args r -> setPriorityEdits (join (agPriority args)) r)))
  , ("set-title", CommandSpec (overIds wantsTitle) False
      (Just (\_cfg _stamp args r -> setTitleEdits (word agTitle args) r)))
  ]
  where
    -- A shape that has nothing to say about HOW MANY rows were named, which is
    -- nine of the ten.
    overIds = const
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

-- | @POST \/command@ with body @{"name": …, "ids": […], "args": {…}}@: a
-- structured write over the rows the client names.  @"id"@ is an @"ids"@ of
-- one, since a command over the row at point is the common one.  The edits are
-- 'Glance.Query''s ('Glance.Query.setStateEdits', 'Glance.Query.archiveEdits');
-- this route owns which rows, which file, and what the answer says.
--
-- Batching is per FILE: ids group by the file their rows came from and each
-- file is written ONCE ('Glance.Query.replaceSpans') under its own digest, so a
-- marked set spanning three files is three atomic writes, each all-or-nothing —
-- a rejected batch writes nothing.  Rollback ACROSS files is impossible, since
-- a rename that has happened cannot be undone by a later failure, so the answer
-- reports per id, which is what a client showing @archived (5)@ and a line per
-- refusal needs.
--
-- A body that is not a command, a name nothing implements, no ids, and a
-- keyword some named row's own classification chain does not declare are all
-- 400 with nothing written — the keyword refuses the WHOLE request
-- deliberately, half a state change over a marked set being worse than none of
-- one.  Per id: an id the store has no row for, and a file whose digest moved.
-- A 200 is therefore "the command ran" rather than "every row moved"; the
-- results say which did.
--
-- Nothing here touches the store, as with @POST \/headline@: the write goes to
-- the file, the watch re-reads it and streams the rows, so a browser command
-- reaches every open tab by the path an editor's save takes.
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
        stamp <- resolveDate cmd
        either (pure . jsonError status400) id
               (stamp >>= \at -> overRows opts hub st at edits cmd)

-- | CMD's rows written, as the IO that writes them or the 400 that stops it.
-- STAMP is @set-planning@'s date already worked out, and is nothing to every
-- other name.
overRows :: ServeOptions -> Hub -> Store -> Maybe Text -> RowEdits -> Command
         -> Either Text (IO Response)
overRows opts hub st stamp edits cmd = do
  (plans, said) <- planCommand st stamp edits cmd
  pure $ do
    written <- mapM (writeOne (walkFor opts) hub) plans
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
resolveDate :: Command -> IO (Either Text (Maybe Text))
resolveDate cmd = case join (agDate (cmdArgs cmd)) of
  Just text | csDated (cmdSpec cmd) -> do
    today <- Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime
    pure (Just <$> planningTimestamp today text)
  _nothingToResolve -> pure (Right Nothing)

-- | @capture@: CMD's line written as a new top entry, and WHERE decides which
-- of the two shapes it takes.
--
-- NO TAG is the tree's inbox, unchanged and deliberately bare: the target comes
-- out of the config ('Glance.Query.captureTargetIn'), which also refuses a
-- target this daemon will not write to, so a misspelled pragma is a 400 naming
-- itself rather than a file written outside the tree; the entry is @* TEXT@
-- under a creation stamp, no template consulted, which is what keeps the
-- quick-jot path one keystroke and one line.
--
-- A TAG is a BLOB in the store, org-glance's own citizen ('captureBlob').
--
-- The document and the digest come off the STORE where it holds the file, that
-- being the text this server last read, and off a fresh read where it does not:
-- a target that is not there yet reads as the empty document under the empty
-- digest, which is what 'Data.Org.Edit.editFile' creates under.  Either way the
-- offset and the lock describe one text.  Nothing here touches the store — the
-- watch re-reads the file and streams the new row, as for a capture out of
-- Emacs.
--
-- Both leave through 'Glance.Web.Watch.writeSpans', like every other write
-- here, so the path each wrote is queued for the drain loop.  That is a nudge
-- into the watch's own queue rather than a store update — the loop still does
-- the loading — and it is what makes a captured row arrive live rather than at
-- the next restart.
captureInto :: ServeOptions -> Hub -> Store -> Command -> IO Response
captureInto opts hub st cmd =
  -- 'wantsText' has already put every @tag@ past 'tagText', which refuses the
  -- empty string and every character a tag may not hold, so ABSENT is the whole
  -- of what "no tag" means here and there is nothing left to strip or test.
  maybe (captureInbox opts hub st cmd) (captureBlob opts hub st cmd) (agTag (cmdArgs cmd))

-- | @capture@ with no tag: CMD's line appended to the tree's capture target.
captureInbox :: ServeOptions -> Hub -> Store -> Command -> IO Response
captureInbox opts hub st cmd = case captureTargetIn (soDir opts) (stConfig st) of
  Left why   -> pure (jsonError status400 why)
  Right path -> do
    (doc, digest) <- maybe (currentDocument path) pure (storeDocument path st)
    now <- Time.getZonedTime
    case captureEdits doc (captureStamp now) (capturedText cmd) of
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
    landed path fresh = [ "ok" .= True, "file" .= path, "digest" .= fresh
                        , "id" .= rowIdIn path (length (recordsUnder path st)) ]

-- | @capture@ under a TAG: a new blob in the served root's own store.
--
-- FOUR REFUSALS, all of them decided before a byte is written.  The store root
-- is the SERVED root's own @.org-glance@ and has to be there — a capture that
-- MADE one would be this daemon deciding a tree is an org-glance store — and it
-- is asked first, being the coarsest thing that can be wrong and the one answer
-- that does not depend on what the reader typed.  The tag's template comes off
-- the config layers, read HERE rather than off the loaded config, so what a
-- settings sheet shows is what a capture expands
-- ('Glance.Query.captureTemplateIn'); a tag no layer configures takes
-- 'Glance.Query.bareTemplate', which is @* %?@ and goes through the same
-- expansion as any other.  That expansion refuses a template with no @%?@ and an
-- ask nobody answered ('Glance.Query.expandTemplate'), and the composition
-- refuses a template that expands to no headline at all.
--
-- The id is minted and the path is org-glance's sharded one; the write goes out
-- under the EMPTY digest, which creates the file and the directories over it and
-- DRIFTS rather than overwrites should anything already sit there.  Minting
-- ahead of the last refusal costs nothing — this side reserves no directory, so
-- an id that is not written is an id nobody ever sees.
--
-- The note to org-glance costs nothing either: @data.org@ under a store's
-- @data\/@ is a blob, so 'Glance.Query.replaceSpans' appends the
-- @EXTERNAL.jsonl@ line on its way out as it does for any other blob write.
-- Blob first, line second, which is the order the contract asks for.
captureBlob :: ServeOptions -> Hub -> Store -> Command -> Text -> IO Response
captureBlob opts hub st cmd tag = do
  there <- doesDirectoryExist store
  if not there then pure (jsonError status400 noStore) else do
    layers <- readConfigLayers (configDirsIn (soDir opts) st)
    now <- Time.getZonedTime
    ident <- mintBlobId
    let template = fromMaybe bareTemplate (captureTemplateIn tag layers)
        answers = Map.toList (fromMaybe Map.empty (agFields (cmdArgs cmd)))
        path = blobPathIn store ident
        composed = expandTemplate now answers (capturedText cmd) template
                     >>= blobDocument tag ident (captureStamp now)
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
    landed path ident fresh =
      ["ok" .= True, "file" .= path, "digest" .= fresh, "id" .= ident]
    noStore = T.pack store <> " is not there, so this tree keeps no org-glance store;\
                               \ capture with no tag to file into the inbox instead"

-- | The line CMD captures, which every capture path writes and no path may be
-- without ('wantsText').
capturedText :: Command -> Text
capturedText = fromMaybe "" . agText . cmdArgs

-- | PLAN's file written once, and what that came to for each of its ids.  Both
-- outcomes are shared by the whole group, because the write is: the batch lands
-- or the file is untouched.
writeOne :: WalkOptions -> Hub -> FilePlan -> IO [(Text, Value)]
writeOne opts hub plan =
  report <$> writeSpans opts hub (fpPath plan) (fpDigest plan) spliced
  where
    spliced = concatMap snd (fpRows plan)
    report written = [ (rid, either (refused rid . why) (done rid) written)
                     | (rid, _edits) <- fpRows plan ]
    why (WriteDrift found) = T.pack (fpPath plan) <> " changed on disk (it digests to "
                               <> T.take 12 found <> "… now); nothing was written to it"
    why (WriteRefused spelled) = spelled

-- | CMD as the files to write and the ids refused without opening one, or as
-- the 400 that stops it.  Two refusals are decided here rather than in the IO
-- above: an unknown id, and a digest the client pinned that the store no longer
-- holds — @POST \/headline@'s own @stale@ check, per file because a digest is.
planCommand :: Store -> Maybe Text -> RowEdits -> Command
            -> Either Text ([FilePlan], [(Text, Value)])
planCommand st stamp rowEdits cmd = do
  rows <- mapM withEdits held
  let groups = groupOn (hrFile . fst) rows
  pure ( [ FilePlan path (hrDigest r0) [ (hrId r, edits) | (r, edits) <- rs ]
         | (path, rs@((r0, _) : _)) <- groups, not (stale rs) ]
       , missing <> [ (hrId r, refused (hrId r) (staleWhy path))
                    | (path, rs) <- groups, stale rs, (r, _edits) <- rs ] )
  where
    -- One resolution for the whole set rather than one per id, which is what
    -- keeps a marked set of a hundred rows off a hundred passes of the store.
    (held, absent) = headlinesIn (storeRecords st) (cmdIds cmd)
    withEdits r = (,) r <$> rowEdits (stConfig st) stamp (cmdArgs cmd) r
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
