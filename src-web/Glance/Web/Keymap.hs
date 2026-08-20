-- | The shell's keymap and the JSON blob the page carries it in.  ONE map:
-- the page's own dispatch parses that blob, so no key is bound undocumented.
module Glance.Web.Keymap (keyBindingsJSON) where

import Data.Aeson (object, (.=))
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Web.Base (jsonValue)


-- | One row of the shell's keymap.
data KeyBinding = KeyBinding
  { kbKeys    :: ![Text]        -- ^ the keys in order; what the dispatch matches.
  , kbCommand :: !Text          -- ^ the command name the echo widget shows.
  , kbHandler :: !(Maybe Text)  -- ^ the shell function running it; 'Nothing' is staged.
  , kbScope   :: !Text          -- ^ @table@, @modal@ or @any@ — where it is live.
  , kbHelp    :: !(Maybe Text)  -- ^ what it does, when the command name does not say; see 'helps'.
  }

bind :: [Text] -> Text -> Maybe Text -> Text -> KeyBinding
bind keys command handler scope = KeyBinding keys command handler scope Nothing

helps :: KeyBinding -> Text -> KeyBinding
helps b text' = b { kbHelp = Just text' }

-- | The map, whole.  Command names are org-glance's where org-glance has one;
-- a row with no handler is recognized and says what it is waiting for.
keyBindings :: [KeyBinding]
keyBindings =
  -- Order matters once: the key line shows a command's FIRST row ('keyHints').
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
  , bind ["<right>"]    "next-column"                     (Just "nextColumn")     "table"
      `helps` nextColumnHelp
  , bind ["<left>"]     "previous-column"                 (Just "previousColumn") "table"
      `helps` previousColumnHelp
  , bind ["<"]          "first-row"                       (Just "firstRow")       "table"
      `helps` firstRowHelp
  , bind [">"]          "last-row"                        (Just "lastRow")        "table"
      `helps` lastRowHelp
  , bind ["G"]          "last-row"                        (Just "lastRow")        "table"
      `helps` lastRowHelp
  , bind ["]"]          "next-page"                       (Just "nextPage")       "table"
  , bind ["["]          "previous-page"                   (Just "previousPage")   "table"
  , bind ["^"]          "toggle-sort"                     (Just "toggleSort")     "table"
      `helps` "put this column at the head of the order; again reverses it"
  , bind ["RET"]        "org-glance-overview:materialize" (Just "materializeRow") "table"
  , bind ["/"]          "filter-rows"                     (Just "focusFilter")    "table"
      `helps` "summon the filter palette"
  -- TWO DOORS, ONE QUERY: `/' edits the filter half, `.' the whole expression.
  , bind ["."]          "compose-query"                   (Just "focusQuery")     "table"
      `helps` "the whole expression: filters, sort: and columns: together"
  , bind ["DEL"]        "filter-drop-token"               (Just "filterDrop")     "table"
      `helps` "unmark all, else drop the filter's last token"
  , bind ["g"]          "apply-default-filter"            (Just "applyDefault")   "table"
      `helps` "the view this tree opens on"
  , bind ["P"]          "set-saved-view"                  (Just "pinView")        "table"
      `helps` "pin the applied view, into whichever saved view answers"
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
  , bind ["o"]          "org-glance-overview:open"        (Just "openLinks")      "table"
      `helps` openHelp
  , bind ["!"]          "org-glance-overview:open"        (Just "openLinks")      "table"
      `helps` openHelp
  , bind ["A"]          "org-glance-agenda"               (Just "applyAgenda")    "table"
      `helps` "the active rows carrying a date, earliest first"
  , bind ["@"]          "org-glance-overview:relations"   (Just "relations")      "table"
      `helps` "the rows referring to this one; DEL walks back"
  , bind ["+"]          "org-glance-overview:capture"     (Just "capture")        "table"
      `helps` "a headline for the inbox, typed as org"
  , bind ["d"]          "archive-flag"                    (Just "archiveFlag")    "table"
      `helps` "flag for archive; d again archives all flagged"
  , bind ["D"]          "org-glance-overview:delete"      (Just "archiveRows")    "table"
      `helps` "archive the flagged; an already-archived row deletes, on a typed word"
  , bind ["x"]          "dired-do-flagged-delete"         (Just "flaggedDelete")  "table"
      `helps` "act on the flagged rows, after asking; d flags, D is the quick one"
  , bind ["S-<up>"]     "priority-up"                     (Just "priorityUp")     "table"
      `helps` priorityHelp
  , bind ["S-<down>"]   "priority-down"                   (Just "priorityDown")   "table"
      `helps` priorityHelp
  , bind ["t"]          "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "set the state of the marked rows, or the row at point"
  , bind ["C-c", "C-t"] "org-glance-overview:todo"        (Just "setState")       "table"
      `helps` "the org spelling, where the browser lets it through"
  , bind [":"]          "org-agenda-set-tags"             (Just "manageTags")     "table"
      `helps` "add or drop tags over the marked rows, or the row at point"
  , bind ["C-c", "C-s"] "org-glance-overview:schedule"    (Just "schedulePlan")   "table"
      `helps` planningHelp
  , bind ["C-c", "C-d"] "org-glance-overview:deadline"    (Just "deadlinePlan")   "table"
      `helps` planningHelp
  , bind [","]          "customize"                       (Just "openSettings")   "table"
      `helps` "the settings sheet: general, theme, keyword cycles"
  -- ONE KEY, TWO SURFACES, which is the peer's own split: `@' READS the edges
  -- from the table and WRITES one from the sheet.
  , bind ["@"]          "org-glance-material:refer"       (Just "refer")          "modal"
      `helps` "link a headline into the prose; at a word boundary, so an address stays text"
  , bind ["C-x", "C-s"] "save-buffer"                     (Just "save")           "modal"
      `helps` "sync the sheet now; again to overwrite a conflict"
  , bind ["C-c", "C-c"] "org-ctrl-c-ctrl-c"               (Just "commitEdit")     "modal"
      `helps` "commit the element being edited"
  , bind ["C-c", "'"]   "org-edit-special"                (Just "toggleRaw")      "modal"
      `helps` "the sheet as raw org, or as body and properties; sync an edited one first"
  , bind ["ESC"]        "keyboard-quit"                   (Just "cancel")         "any"
      `helps` "close the sheet, syncing an edited one; again to discard"
  ]

nextColumnHelp, previousColumnHelp :: Text
nextColumnHelp     = "the cell to the right; row movement keeps the column"
previousColumnHelp = "the cell to the left; a whole row has none"

firstRowHelp, lastRowHelp :: Text
firstRowHelp = "first row, again = page up"
lastRowHelp  = "last row, again = page down"

planningHelp :: Text
planningHelp = "a date over the marked rows, or the row at point; empty clears it"

priorityHelp :: Text
priorityHelp = "cycle the priority of the marked rows, or the row at point"

openHelp :: Text
openHelp = "open links: the row here, the element in the sheet; several list them"

-- | Chords left to the browser unless they complete a bound sequence.  None
-- is bound on its own, which is what leaves an abandoned prefix to it.
reservedChords :: [Text]
reservedChords = ["C-l", "C-r", "C-t", "C-u", "C-w", "C-n", "C-p", "<f5>"]

-- | The commands auto-repeat is off for, by NAME.  Movement keeps its repeat;
-- a surviving one on @d@ would flag a row and archive it from ONE press.
onceCommands :: [Text]
onceCommands = [ "filter-drop-token", "unmark-all", "mark-all"
               , "archive-flag", "org-glance-overview:delete"
               , "dired-do-flagged-delete"
               , "set-saved-view"
               , "org-glance-overview:open", "org-glance-agenda"
               , "org-glance-overview:relations"
               , "priority-up", "priority-down"
               , "toggle-sort" ]

-- | The resident key line, in reading order.  Commands rather than keys, so
-- the line cannot advertise a key nothing is bound to.
keyHints :: [([Text], Text)]
keyHints =
  [ (["next-row", "previous-row"],         "rows")
  , (["next-column", "previous-column"],   "cells")
  , (["previous-page", "next-page"],       "pages")
  , (["first-row", "last-row"],            "first/last row, again = page up/down")
  , (["toggle-sort"],                      "sort")
  , (["org-glance-overview:materialize"],  "materialize")
  , (["org-glance-overview:open"],         "open link")
  , (["mark-toggle", "unmark", "unmark-all", "mark-all"], "mark")
  , (["org-glance-overview:todo"],         "state")
  , (["priority-up", "priority-down"],     "priority")
  , (["org-agenda-set-tags"],              "tags")
  , (["org-glance-overview:schedule", "org-glance-overview:deadline"], "schedule/deadline")
  , (["org-glance-overview:capture"],      "capture")
  , (["archive-flag"],                     "flag for archive")
  , (["archive-flag", "org-glance-overview:delete"], "archive flagged")
  , (["filter-rows"],                      "filter")
  , (["compose-query"],                    "whole query")
  , (["apply-default-filter"],             "default view")
  , (["org-glance-agenda"],                "agenda")
  , (["org-glance-overview:relations"],    "references")
  , (["filter-drop-token"],                "unmark/drop token/back")
  , (["customize"],                        "settings")
  , (["quit-window"],                      "quit")
  ]

-- | The keymap as the page carries it.  Angle brackets are escaped because
-- five sequences spell them: a blob that cannot spell a tag cannot open one.
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
