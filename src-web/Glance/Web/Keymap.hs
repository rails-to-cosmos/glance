-- | The shell's keymap and the blob the page carries it in.
--
-- ONE map.  'keyBindings' is org-glance's @overview-mode@ under org-glance's
-- own command names, and 'keyBindingsJSON' is what the page parses for its own
-- dispatch, so a key cannot be bound and undocumented and a hint cannot name a
-- command this map does not carry.
module Glance.Web.Keymap (keyBindingsJSON) where

import Data.Aeson (object, (.=))
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Web.Base (jsonValue)

-- Keymap

-- | One row of the shell's keymap.
data KeyBinding = KeyBinding
  { kbKeys    :: ![Text]        -- ^ the keys in order; what the dispatch matches.
  , kbCommand :: !Text          -- ^ the command name the echo widget shows.
  , kbHandler :: !(Maybe Text)  -- ^ the shell function running it; 'Nothing' is staged.
  , kbScope   :: !Text          -- ^ @table@, @modal@ or @any@ — where it is live.
  , kbHelp    :: !(Maybe Text)  -- ^ what it does, when the command name does not say; see 'helps'.
  }

-- | KEYS bound to a command.  The notation the echo widget shows is derived
-- rather than stored ('keyBindingsJSON').
bind :: [Text] -> Text -> Maybe Text -> Text -> KeyBinding
bind keys command handler scope = KeyBinding keys command handler scope Nothing

-- | B with the one line the echo widget shows past its command name.  A row
-- earns one where the name is the Emacs name for a key whose behaviour here is
-- narrower than the name — @save-buffer@ on a sheet that syncs itself, and the
-- @keyboard-quit@ that flushes on the way out.
helps :: KeyBinding -> Text -> KeyBinding
helps b text' = b { kbHelp = Just text' }

-- | The map, whole, and every row in it live wherever its scope is: the
-- movement profiles are gone, and with them the question of which keys a reader
-- has.  @n@\/@p@ and @j@\/@k@ both step a row, @f@\/@b@ and @l@\/@h@ both step
-- a cell — the spellings cost a row each, where a profile cost a selector, a
-- stored choice, a URL parameter and a key line rewritten whenever it moved.
--
-- The names are org-glance's (@org-glance-overview-mode-map@, plus Emacs's
-- @C-x C-s@ for the sheet) wherever org-glance has one, and descriptive where
-- it does not.  A row with no handler is recognized in full and says what it is
-- waiting for, so the map is complete ahead of the daemon commands that will
-- back it (M4) rather than answering a key with silence.
--
-- Claimed chords, and only these.  @C-c@ becomes a prefix while no text field
-- has focus and the selection is collapsed, so a copy is still a copy; @C-x@
-- likewise, and only while the sheet is open, the one place @C-x C-s@ means
-- anything.  @RET@, @TAB@, @\/@ and @DEL@ are taken while the table has focus —
-- @DEL@ is the filter's own undo, and a field with focus keeps its backspace.
-- Nothing here moves on @C-n@ or @C-p@: they are 'reservedChords', which are
-- never bound on their own.
keyBindings :: [KeyBinding]
keyBindings =
  -- Movement.  Two spellings of each, and the arrows over both.  Order matters
  -- once: the key line shows a command's FIRST row ('keyHints'), so the letters
  -- lead and the line reads @n\/p rows@ rather than @\<down\>\/\<up\> rows@.
  [ bind ["n"]          "next-row"                        (Just "nextRow")        "table"
  , bind ["p"]          "previous-row"                    (Just "previousRow")    "table"
  , bind ["j"]          "next-row"                        (Just "nextRow")        "table"
  , bind ["k"]          "previous-row"                    (Just "previousRow")    "table"
  , bind ["<down>"]     "next-row"                        (Just "nextRow")        "table"
  , bind ["<up>"]       "previous-row"                    (Just "previousRow")    "table"
  , bind ["f"]          "next-column"                     (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["b"]          "previous-column"                 (Just "previousColumn") "table"
      `helps` previousColumnHelp
  , bind ["l"]          "next-column"                     (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["h"]          "previous-column"                 (Just "previousColumn") "table"
      `helps` previousColumnHelp
  -- The arrows ride with the letters on BOTH axes and silently, behind `f' and
  -- `b' as `<down>' sits behind `n'.  Same handler, so walking off either end
  -- is the landing it is for the letters rather than a wall.
  , bind ["<right>"]    "next-column"                     (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["<left>"]     "previous-column"                 (Just "previousColumn") "table"
      `helps` previousColumnHelp
  -- The ends of the buffer, org-glance's own pair, plus vi's @G@ beside @>@.
  -- Progressive: the page's end row, then the previous or next page's, so the
  -- pair reaches the ends of the whole set without reaching for the brackets.
  , bind ["<"]          "first-row"                       (Just "firstRow")       "table"
      `helps` firstRowHelp
  , bind [">"]          "last-row"                        (Just "lastRow")        "table"
      `helps` lastRowHelp
  , bind ["G"]          "last-row"                        (Just "lastRow")        "table"
      `helps` lastRowHelp
  , bind ["]"]          "next-page"                       (Just "nextPage")       "table"
  , bind ["["]          "previous-page"                   (Just "previousPage")   "table"
  -- The order the rows are in, over the column the cell keys are standing in:
  -- table-view's own @^@ (@table-view-sort-cycle@ there).  It REVERSES rather
  -- than cycling off — the handle states an order with no way back to none —
  -- and refuses a whole-row selection rather than guessing which column.
  , bind ["^"]          "toggle-sort"                     (Just "toggleSort")     "table"
      `helps` "put this column at the head of the order; again reverses it"
  , bind ["RET"]        "org-glance-overview:materialize" (Just "materializeRow") "table"
  , bind ["/"]          "filter-rows"                     (Just "focusFilter")    "table"
      `helps` "summon the filter palette"
    -- ERASE THE LAST STRUCTURE STANDING, which is the backspace's own rhyme: a
    -- marked set first, then the query's last token, then the drill it was made
    -- in.  The marks rung runs @unmark-all@ and the pill says so.
  , bind ["DEL"]        "filter-drop-token"               (Just "filterDrop")     "table"
      `helps` "unmark all, else drop the filter's last token"
  -- The default view, as the tree configures it (@#+GLANCE_DEFAULT_FILTER:@).
  , bind ["g"]          "apply-default-filter"            (Just "applyDefault")   "table"
      `helps` "the view this tree opens on"
    -- The pin: the applied query — sort tokens and all — becomes the tree's
    -- `#+GLANCE_DEFAULT_FILTER:' line, through the settings write the sheet
    -- already rides.  Composing stays the table's widget; the sheet's field
    -- shows what is pinned.
  , bind ["P"]          "set-default-view"                (Just "pinView")        "table"
      `helps` "pin the applied view as the tree's default"
  , bind ["m"]          "mark-toggle"                     (Just "markToggle")     "table"
      `helps` "toggle this row's mark, then step down"
  , bind ["u"]          "unmark"                          (Just "unmarkRow")      "table"
      `helps` "take this row's archive flag off, else its mark, then step down"
  , bind ["U"]          "unmark-all"                      (Just "unmarkAll")      "table"
      `helps` "every mark and every archive flag off"
  , bind ["M"]          "mark-all"                        (Just "markAll")        "table"
      `helps` "mark every row loaded"
  , bind ["q"]          "quit-window"                     (Just "quitWindow")     "table"
  , bind ["TAB"]        "org-cycle"                       Nothing                 "table"
    -- Where the row points, out of its own subtree: one link opens, several
    -- raise the popup that lists them, none says so.  Two org-glance spellings.
  , bind ["o"]          "org-glance-overview:open"        (Just "openLinks")      "table"
      `helps` openHelp
  , bind ["!"]          "org-glance-overview:open"        (Just "openLinks")      "table"
      `helps` openHelp
    -- A canned VIEW rather than a mode: the active rows carrying a date,
    -- earliest first.  `g' is the way back.
  , bind ["a"]          "org-glance-agenda"               (Just "applyAgenda")    "table"
      `helps` "the active rows carrying a date, earliest first"
    -- The drill: the rows pointing AT the one at point, applied as a `ref:'
    -- view with a crumb left behind.  A look rather than a bulk act, so it
    -- takes the row at point and never the marked set.
  , bind ["@"]          "org-glance-overview:relations"   (Just "relations")      "table"
      `helps` "the rows referring to this one; DEL walks back"
  -- The one command that names no row: it writes a new entry into the tree's
  -- capture target, which is a line of the system config.
  , bind ["+"]          "org-glance-overview:capture"     (Just "capture")        "table"
      `helps` "a headline for the inbox, typed as org"
  -- dired's flag, and dired's @dd@: the first press flags the row, the second
  -- archives every flagged row through @D@'s own handler, so a lone flag is a
  -- set of one and the single-row flow is the general one.  The flag IS the
  -- confirmation — no prompt, and no undo to build since @u@ takes it off — and
  -- plain @d@ is never a write on its own, so a mis-key costs a keystroke.
  , bind ["d"]          "archive-flag"                    (Just "archiveFlag")    "table"
      `helps` "flag for archive; d again archives all flagged"
  , bind ["D"]          "org-glance-overview:delete"      (Just "archiveRows")    "table"
      `helps` "archive the flagged rows, or the row at point — never a delete"
      -- The user's own spelling; Chromium owns Ctrl+T above the document, so
      -- the org chord stays as the secondary for browsers that deliver it.
    -- ORG'S OWN PRIORITY KEYS, and they CYCLE rather than ask: a priority is
    -- one of three letters and a reader knows which way to go, so a palette
    -- would be three lines to read where a press is the answer.  Up runs
    -- @none → C → B → A → none@ and down the reverse — org's own wrap THROUGH
    -- none, making the token removable without a second key.  Each row cycles
    -- from ITS OWN value, so a mixed marked set stays mixed and moves together.
  , bind ["S-<up>"]     "priority-up"                     (Just "priorityUp")     "table"
      `helps` priorityHelp
  , bind ["S-<down>"]   "priority-down"                   (Just "priorityDown")   "table"
      `helps` priorityHelp
  , bind ["t"]          "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "set the state of the marked rows, or the row at point"
  , bind ["C-c", "C-t"] "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "the org spelling, where the browser lets it through"
      -- The agenda's own key for the same question over there.  One palette, and
      -- it STAYS UP: managing tags is several ops, where setting a state is one.
  , bind [":"]          "org-agenda-set-tags"             (Just "manageTags")     "table"
      `helps` "add or drop tags over the marked rows, or the row at point"
      -- Both chords survive the browser, where @C-c C-t@ does not: @Ctrl+S@ and
      -- @Ctrl+D@ are page default actions rather than chrome shortcuts, so
      -- @preventDefault@ on the completing chord is the whole of what they need.
  , bind ["C-c", "C-s"] "org-glance-overview:schedule"    (Just "schedulePlan")   "table"
      `helps` planningHelp
  , bind ["C-c", "C-d"] "org-glance-overview:deadline"    (Just "deadlinePlan")   "table"
      `helps` planningHelp
  , bind [","]          "customize"                       (Just "openSettings")   "table"
      `helps` "the settings sheet: general, theme, keyword cycles"
  , bind ["C-x", "C-s"] "save-buffer"                     (Just "save")           "modal"
      `helps` "sync the sheet now; again to overwrite a conflict"
      -- ORG'S OWN "DO THE THING HERE", the thing being the element that is
      -- open: the paragraph textarea and the two-field overlay alike.  It is
      -- `C-x C-s' minus the sheet — that key commits an open element and then
      -- flushes or overwrites a conflict, a BUFFER's act rather than an
      -- element's, so this one stops where the element does.  `Ctrl+C' is a page
      -- default action rather than a chrome shortcut, so `preventDefault' on the
      -- completing press is all the chord needs, and COPY is untouched because
      -- prefix opening is guarded by `selecting()': with anything selected the
      -- first `C-c' is the browser's, which is when a reader means to copy.
  , bind ["C-c", "C-c"] "org-ctrl-c-ctrl-c"               (Just "commitEdit")     "modal"
      `helps` "commit the element being edited"
  , bind ["C-c", "'"]   "org-edit-special"                (Just "toggleRaw")      "modal"
      `helps` "the sheet as raw org, or as body and properties; sync an edited one first"
  , bind ["ESC"]        "keyboard-quit"                   (Just "cancel")         "any"
      `helps` "close the sheet, syncing an edited one; again to discard"
  ]

-- | The cell-movement help lines, one pair for the two spellings of each: the
-- keys differ, what they do does not.  Between them they say the whole rule —
-- the column rides along with row movement, and a whole-row selection starts at
-- the first column whichever direction asks for one.
nextColumnHelp, previousColumnHelp :: Text
nextColumnHelp     = "the cell to the right; row movement keeps the column"
previousColumnHelp = "the cell to the left; from a whole row, the first column"

-- | The buffer-end help lines: each key takes the page's end row, and taking it
-- again turns the page onto the SAME end of the next one, which is what makes
-- the pair walk the whole set.  The two spellings of @last-row@ share a line.
firstRowHelp, lastRowHelp :: Text
firstRowHelp = "first row, again = page up"
lastRowHelp  = "last row, again = page down"

-- | The reschedule help line, shared by the two keys: what they take differs by
-- one word and what a reader has to know does not.
planningHelp :: Text
planningHelp = "a date over the marked rows, or the row at point; empty clears it"

-- | The priority help line, shared by the two keys: what they take differs by a
-- direction and what a reader has to know does not.
priorityHelp :: Text
priorityHelp = "cycle the priority of the marked rows, or the row at point"

-- | The open help line, shared by the two spellings of the one command, saying
-- WHICH links each surface follows: the table's @o@ takes the row's whole
-- subtree, the document's the element the cursor is on.  The line says both
-- grains rather than leaving a reader to find the second by pressing it.
openHelp :: Text
openHelp = "open links: the row here, the element in the sheet; several list them"

-- | Chords the browser needs more than this page does: never claimed as the key
-- that abandons a prefix this map had entered, which is what leaves @C-x C-l@
-- to the browser.  One completing a bound sequence is still claimed — that is
-- what makes @C-c C-t@ work — and none of them is bound on its own.
reservedChords :: [Text]
reservedChords = ["C-l", "C-r", "C-t", "C-w", "C-n", "C-p", "<f5>"]

-- | The commands auto-repeat is taken off: one press, one token.  Movement
-- wants the repeat — a held @n@ is how you cross a table, and the renderer
-- coalesces those to a frame — where a held @DEL@ would walk the whole query
-- away between one glance at the chips and the next.  By command NAME, so a
-- command two keys spell is off under both.
--
-- @m@ and @u@ stay off it: both advance, so a held one walks a column rather
-- than working one row twice (docs\/invariants.md).  The writes are on it so a
-- held key is not a hundred @\/command@ requests, @d@ most of all — a repeat
-- surviving here would flag a row and archive it from ONE press, the
-- confirmation the two-press shape exists to be.
onceCommands :: [Text]
onceCommands = [ "filter-drop-token", "unmark-all", "mark-all"
               , "archive-flag", "org-glance-overview:delete"
                 -- One write per press: a held pin is a config write, a reseed
                 -- and a view-changed remount per repeat.
               , "set-default-view"
                 -- Neither writes a file, and both are ruinous held down: a
                 -- leaned-on `o' is a browser tab per repeat, a leaned-on `a' a
                 -- remount per repeat.  `@' remounts and leaves a crumb, so a
                 -- held key builds a trail DEL walks back one step at a time.
               , "org-glance-overview:open", "org-glance-agenda"
               , "org-glance-overview:relations"
                 -- A held priority key would walk the cycle round and land
                 -- wherever the repeat count left it, which is the reversing
                 -- key's problem one ring wider.
               , "priority-up", "priority-down"
                 -- A reversing key is the one kind a repeat cannot help: a held
                 -- `^' re-sorts the whole set per repeat and lands on whichever
                 -- direction the parity of the count leaves it, so what a reader
                 -- gets is the order they asked for or its opposite, at random.
               , "toggle-sort" ]

-- | The resident key line, in the order it reads: the commands worth naming
-- ahead of the echo pill, each with the word the line shows for it — the rest
-- is the echo pill's to name as it runs.  Commands rather than keys, so the
-- page looks each one up in the map and the line cannot advertise a key nothing
-- is bound to.  The page pair is listed backwards on purpose: a bracket pair
-- reads open then close, so the line says @[\/]@ where the row and cell pairs
-- say forward first.
keyHints :: [([Text], Text)]
keyHints =
  [ (["next-row", "previous-row"],         "rows")
  , (["next-column", "previous-column"],   "cells")
  , (["previous-page", "next-page"],       "pages")
  -- The one row whose label carries a second sentence: without it a reader
  -- takes `<' for a within-page key and never finds out that it climbs.
  , (["first-row", "last-row"],            "first/last row, again = page up/down")
  -- Beside the cell keys in spirit: what it sorts by is the column they picked.
  , (["toggle-sort"],                      "sort")
  , (["org-glance-overview:materialize"],  "materialize")
  , (["org-glance-overview:open"],         "open link")
  , (["mark-toggle", "unmark", "unmark-all", "mark-all"], "mark")
  -- The two structured commands, beside the keys that pick what they run over.
  -- `state' runs over the MARKED set; archiving runs over the FLAGGED one and is
  -- named as its two steps — `d' puts a flag on, either key takes them off.
  , (["org-glance-overview:todo"],         "state")
  , (["priority-up", "priority-down"],     "priority")
  , (["org-agenda-set-tags"],              "tags")
  , (["org-glance-overview:schedule", "org-glance-overview:deadline"], "schedule/deadline")
  , (["org-glance-overview:capture"],      "capture")
  , (["archive-flag"],                     "flag for archive")
  , (["archive-flag", "org-glance-overview:delete"], "archive flagged")
  , (["filter-rows"],                      "filter")
  , (["apply-default-filter"],             "default view")
  , (["org-glance-agenda"],                "agenda")
  -- Named beside the key that walks back out of it: the drill and its undo are
  -- one gesture, and a reader who sees only the way in has no way home.
  , (["org-glance-overview:relations"],    "references")
  , (["filter-drop-token"],                "unmark/drop token/back")
  , (["customize"],                        "settings")
  , (["quit-window"],                      "quit")
  ]

-- | The keymap as the page carries it: the one row list, and the three tables
-- the dispatch reads off the same blob — the key line's hints, the chords never
-- claimed, and the commands auto-repeat is off for.  The angle brackets are
-- escaped because five of these sequences are angle brackets: a blob that
-- cannot spell a tag cannot open one, whatever element it sits in, and
-- @JSON.parse@ undoes them.  @seq@ is derived here rather than carried by a
-- row — the keys with one space between them, how Emacs spells a sequence and
-- the only notation left now that no row runs two keys together.
keyBindingsJSON :: Text
keyBindingsJSON = jsonValue $ object
  [ "rows"     .= map row keyBindings
  , "hints"    .= [ object [ "commands" .= cs, "label" .= label ] | (cs, label) <- keyHints ]
  , "reserved" .= reservedChords
  , "once"     .= onceCommands
  ]
  where row b = object [ "keys"    .= kbKeys b
                       , "seq"     .= T.unwords (kbKeys b)
                       , "command" .= kbCommand b
                       , "handler" .= kbHandler b
                       , "scope"   .= kbScope b
                       , "help"    .= kbHelp b ]
