-- | The two documents this server serves: the shell, and the page that says
-- there is no renderer to mount.
module Glance.Web.Page (demoShell, assetsMissing) where

import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (defaultCaptureFile)
import Glance.Web.Base ( ServeOptions (..), escape, glueAsset, logLinesBand, logLinesDefault
                       , rendererAsset, viewTitleFor )
import Glance.Web.Keymap (keyBindingsJSON)
import Glance.Web.Theme (Theme (..), themes)
import Glance.Web.Page.Glue (glueConfig)
import Glance.Web.Page.Style (fontFace, page)

-- | The page a browser gets: load the renderer, fetch a page of the view, mount
-- it, then hold a socket open and apply what it sends.  WANTED is the tree's
-- default view, which the glue boots on when the URL names no query.
--
-- The markup is the DOM the glue addresses, and nothing else: one mount point,
-- the event strip, the key line, and the overlays — sheet, palettes, settings —
-- that stand over them.  The page is one column the height of the viewport and
-- does not scroll; the table keeps the height it asks for, the log takes what
-- is left under the settings sheet's cap, and both scroll inside themselves, so
-- the key line holds its place whatever arrives.
--
-- The log is an event strip: connection, sync outcomes, the parity warning,
-- errors.  What is loaded is the renderer's hint line and what the keys are is
-- the resident key line's, so neither is repeated there.  The frame is
-- resident, so the first event to arrive does not shift the key line under it.
--
-- There is no status corner: the socket's state is the stale wash and the
-- strip's own @ws@ lines, and every preference is a panel of the settings sheet
-- @,@ raises.

demoShell :: ServeOptions -> Maybe FilePath -> [(Text, [(Text, Text)])]
          -> [(Text, Text)] -> Text
demoShell opts font colours views =
  page (fontFace font) colours (viewTitleFor (soDir opts)) $ T.unlines $
  -- No heading: the view title is already the tab's, and printing it a second
  -- time here put it on screen twice.  In palette mode the renderer carries no
  -- bar either, so the page opens on the table itself.
  -- There is NO status corner.  The stale wash — the whole page fading back the
  -- moment a change can no longer reach it — and the `ws' lines still there to
  -- scroll to already say it, so a dot would be a third spelling costing a fixed
  -- box, a z-level and a top padding to keep clear of.
  [ "  <div id=\"app\"></div>"
  , "  <div id=\"log\"></div>"
  , "  <div id=\"kbd\"></div>"
  , "  <div id=\"modal\">"
  , "    <div id=\"sheet\" class=\"pop-sheet\">"
  , "      <div id=\"mhead\"><span id=\"mfile\"></span><span id=\"mnote\"></span></div>"
  -- Where in the outline the sheet is standing: the titles from the ROW down to
  -- the headline on screen, the server's own `path'.  One line, empty over a
  -- row, so a sheet that never left the row it was opened on looks as it did.
  , "      <div id=\"mwhere\"></div>"
  -- TWO PANES over one subtree: the STRUCTURED DOCUMENT on the left, the
  -- property panel on the right.  The cut between them is the server's
  -- (`GET /headline' hands both), so neither pane is derived here.
  --
  -- The document stands where the textarea did and draws the subtree's TEXT as
  -- the elements it is made of: the headline line with its cells, the body's
  -- paragraphs, the children under it.  The textarea is still there as raw mode
  -- and the escape hatch, shown by `C-c '', with `#sheet.raw' deciding which is
  -- up.  The document is hand-drawn rather than a table-view mount — the
  -- doctrine line, since the renderer's list widget draws a list of RECORDS, one
  -- shape per row, where this is a list of KINDS.  The panel beside it IS a
  -- mount, the same reason read the other way: a drawer is a list of records.
  -- Each pane carries its own edit overlay, since the mount rewrites its rows as
  -- it scrolls and the document redraws whole on every move — an edit living
  -- inside a row would be thrown away by the next frame.
  , "      <div id=\"mpanes\">"
  , "        <textarea id=\"mtext\" spellcheck=\"false\"></textarea>"
  , "        <div id=\"mdoc\"><div id=\"dlist\"></div>"
      <> "<div id=\"dtitle\"><input id=\"dtin\" spellcheck=\"false\"></div>"
      <> "<div id=\"dpara\"><textarea id=\"dtext\" spellcheck=\"false\"></textarea></div>"
      <> "</div>"
  , "        <div id=\"mprops\"><div id=\"mptable\"></div>"
      <> "<div id=\"pedit\"><input id=\"pkey\" spellcheck=\"false\">"
      <> "<input id=\"pval\" spellcheck=\"false\"></div></div>"
  , "      </div>"
  -- The logbook, read-only and full width under both panes, and the server's:
  -- shown so a reader can see what the row has been through, never sent back,
  -- and out of Tab and out of `dirty()' because nothing here can move it.
  , "      <pre id=\"mlog\"></pre>"
  , "    </div>"
  , "  </div>"
  -- The tags popup, hosting the page's FOURTH table-view mount and the only
  -- MUTABLE one.  What a set of rows is tagged with is a list of RECORDS — a
  -- name, a coverage over the set, a weight in the tree — so it takes the link
  -- popup's shape rather than the value palette's, and carries what a read-only
  -- mount does not: the rename overlay, the property panel's edit model over
  -- one cell.  `#tpane' positions it, as `#mdoc' does the document's.
  ]
  -- The tag manager is three SHORT columns — the tag, its coverage, its
  -- weight — so it wears the band's width: a sheet-wide box drew 80vw of
  -- ground for a list narrower than the palette.
  <> popupFrame "tags" "t" "pop-band" ("<div id=\"tedit\">" <> field "tname" <> "</div>")
  -- The value palette.  Letter mode is the resident one and its field is
  -- hidden, so the box carries the mode: `#pbox.narrow' is the completing-read
  -- `/' falls back to, and `+' over the tags popup is its other door.  The foot
  -- names the keys the list itself cannot draw.
  <>
  [ "  <div id=\"prompt\">"
  , "    <div id=\"pbox\" class=\"pop-band\">"
  , "      <div id=\"phead\"></div>"
  , "      <input id=\"pinput\" spellcheck=\"false\" autocomplete=\"off\">"
  , "      <div id=\"plist\"></div>"
  , "      <div id=\"pfoot\"></div>"
  , "    </div>"
  , "  </div>"
  -- The link popup, hosting the page's THIRD table-view mount.  A list of links
  -- is a TABLE — what each one is, what the entry calls it, where it points —
  -- and this page has one list widget, so it draws that table rather than a
  -- hand-made list under hand-assigned letters.  A sibling of `#app' sharing the
  -- two z levels with the sheets and the value palette, so the three values
  -- still stand.  `#lpane' is the edit overlay's positioning parent, as `#tpane'
  -- is the rename's: `RET' lays two fields over the row's title and url cells,
  -- and the mount rewrites its rows as it scrolls, so an edit living inside one
  -- would be thrown away by the next frame.
  ]
  <> popupFrame "links" "l" "pop-sheet"
       ("<div id=\"ledit\">" <> field "ltitle" <> field "lurl" <> "</div>")
  -- The capture form, one popup where a chain of palettes used to blink: the
  -- tag field with the vocabulary narrowing under it, the fields a template
  -- asks grown into `#kfields' when the tag settles, and the line last.  RET
  -- moves the focus forward and captures at the line; ESC leaves whole.
  <>
  [ "  <div id=\"capture\">"
  , "    <div id=\"kbox\" class=\"pop-sheet\">"
  , "      <div id=\"khead\"></div>"
  , "      <input id=\"ktag\" spellcheck=\"false\" autocomplete=\"off\""
      <> " placeholder=\"tag — empty is the inbox\">"
  , "      <div id=\"klist\"></div>"
  , "      <div id=\"kfields\"></div>"
  , "      <textarea id=\"ktext\" spellcheck=\"false\""
      <> " placeholder=\"a headline, as org\"></textarea>"
  , "      <div id=\"kfoot\"></div>"
  , "    </div>"
  , "  </div>"
  ]
  -- The settings sheet: the page's ONE place for a preference, in TABS.
  -- General, theme, keywords — one panel on screen at a time, named by a button
  -- in `#ctabs' and drawn from the `SECTIONS' list below, so a fourth panel is
  -- one entry there and the markup it wraps.  A sibling of `#app' like the
  -- other two overlays, so a remount leaves it standing — which this one needs
  -- most, writing a layer being itself what moves the columns.  The panel
  -- BODIES are declared here, each wearing `cpart', and `SECTIONS' wraps them
  -- at boot: the list owns the names and the order, the markup owns what is
  -- under them, and the stylesheet reads the class rather than a roll of ids.
  <>
  [ "  <div id=\"config\">"
  , "    <div id=\"cbox\" class=\"pop-sheet\">"
  , "      <div id=\"chead\"><span id=\"ctitle\">settings</span>"
      <> "<span id=\"cnote\"></span></div>"
  , "      <div id=\"ctabs\"></div>"
  , "      <div id=\"csecs\"></div>"
  -- GENERAL: the two tree-wide lines of `system.org'.  They are that layer's
  -- own bytes and ride in that layer's own write, so which panel shows them is
  -- a matter of reading rather than of where the splice goes.  Each field's
  -- fallback when emptied is its placeholder here, off the same Haskell
  -- constants the server answers with — the value cannot change while the page
  -- is up, so nothing carries it into the glue and back out per sheet-open.
  , "      <div id=\"cgen\" class=\"cpart\">"
  -- A SAVED VIEW is edited in the SAME widget the main page filters with: a
  -- table-view COMPOSER mount — the omnibox bar and the chips, no table behind
  -- them — so badges, completion and the whole query grammar are the form
  -- control.  WHICH view is the label's own selector, off the registry, so a
  -- third view is an entry in 'savedViews' and nothing here.  `P' on the table
  -- is still the pin; this row is the sheet's door to the same lines.
  , crow "<select id=\"cwhich\" class=\"cview\" title=\"which saved view\"></select>"
         "<div id=\"cfbox\"></div>"
  , crow (clab "capture target")
         (cinput "ctarget" ""
                 ("where + captures; empty is " <> T.pack defaultCaptureFile))
  -- The log's height, and the one field on this sheet that asks no server: it
  -- is a `localStorage' preference like the theme, and the panel says where a
  -- reader READS a preference rather than what writes it.  `cmoved' never sees
  -- it, so it costs no request and cannot make a pristine sheet dirty.
  , crow (clab "log lines")
         (cinput "clog" " inputmode=\"numeric\""
                 ("how tall the log grows, " <> logLinesBand <> "; empty is "
                    <> T.pack (show logLinesDefault)))
  , "      </div>"
  -- THEME: three values, one `localStorage' key, and the pre-paint boot in the
  -- head that reads it — a preference like every other one on this sheet, which
  -- is why it is here.  It wears `cview' with the fields above, taking their
  -- border, radius and font and the coarse-pointer rule that stops iOS zooming
  -- in on a focused control.
  , "      <div id=\"ctheme\" class=\"cpart\">"
  -- THE OPTIONS ARE THE REGISTRY'S, so a theme is a record in `Glance.Web.Theme`
  -- and nothing else: the same list the CSS blocks and the boot script's id test
  -- read.  `auto' leads and is the one entry naming no theme — the media query
  -- rather than a palette — which is why it is spelled here and the rest are not.
  -- The page is built per request, so a theme a TREE comes to name joins this
  -- select by joining that list.
  , crow (clab "theme")
         ("<select id=\"themesel\" class=\"cview\" title=\"theme\">"
            <> "<option value=\"auto\">auto</option>"
            <> T.concat [ "<option value=\"" <> thId t <> "\">" <> escape (thLabel t)
                            <> "</option>" | t <- themes ]
            <> "</select>")
  -- AND THE TREE'S OWN STATE HUES, UNDER THE THEME ON SCREEN.  There is ONE
  -- theme control: the fields describe whatever theme the reader is looking at,
  -- so picking one moves the whole page and these rows with it.  The STORAGE
  -- stays per theme — a hue that reads on white is unreadable on black, which
  -- is what `#+GLANCE_STATE_COLORS:' is keyed by theme for — and which theme is
  -- being written is DERIVED from the applied one rather than picked a second
  -- time.  The rows are filled by the glue off the tree's own cycle.
  -- THE STATES TABLE, the page's FIFTH table-view mount and the second MUTABLE
  -- one: `tag | state | group | colour', one row per keyword the tree knows.
  -- A state belongs to a config LAYER and rides that layer's own write; a
  -- COLOUR is tree-wide and rides `system.org''s, so one row can move two files
  -- and both leave in the one flush.  `#sedit' is its edit overlay, the
  -- property panel's own mechanism one surface over.
  , "      <div id=\"chues\" class=\"cpart\"><div id=\"cstates\"></div>"
      <> "<div id=\"sedit\"><input id=\"sname\" spellcheck=\"false\">"
      <> "<input id=\"sgroup\" spellcheck=\"false\">"
      <> "<input id=\"shue\" spellcheck=\"false\"></div></div>"
  , "      </div>"
  -- KEYWORDS: ONE LAYER AT A TIME.  A tree has as many config files as it has
  -- tags, and a stack of boxes made the panel as tall as that number — the
  -- reader scrolled past every layer they were not editing to reach the one they
  -- were.  So the layers are a `select' with one box under it, holding the
  -- SELECTED layer's `#+TODO:' lines verbatim; a switch must not cost an edit,
  -- so every layer's text is kept in memory whether or not it is on screen and
  -- the sync writes all of them.  The select is the sheet's own chrome and takes
  -- the sheet's focus rules: inside a popup, so it keeps the focus it is given
  -- and native tabbing walks it in DOM order with the rest.
  , "      <div id=\"clayers\" class=\"cpart\">"
  , crow (clab "layer")
         "<select id=\"clayer\" class=\"cview\" title=\"config layer\"></select>"
  -- The label is the SELECTED layer's — where the file is, and whether it is
  -- there at all.  A layer with no digest is not a file yet; saying so is what
  -- makes creating the first one an edit rather than a mystery.  It is the one
  -- label the server writes, so it is the one that carries an id.
  , crow "<div id=\"clab\" class=\"clab\"></div>"
         ("<textarea id=\"ctext\" class=\"ctext\" spellcheck=\"false\""
            <> " placeholder=\"#+TODO: TODO STARTED | DONE\"></textarea>"
            <> "<div id=\"clerr\" class=\"cerr\"></div>")
  -- The same layer's CAPTURE TEMPLATE, which is a region of the same file and
  -- rides in the same write.  Verbatim, because the page has no org parser: the
  -- server slices the first heading's extent and splices what comes back over
  -- it.  `%' in the box raises the code list the server serves, so what the
  -- completion offers is what a capture expands.
  , crow (clab "capture template")
         ("<textarea id=\"ctpl\" class=\"ctext\" spellcheck=\"false\""
            <> " placeholder=\"* %?\"></textarea>"
            <> "<div id=\"ctplf\">% offers the codes a capture expands</div>")
  , "      </div>"
  , "      <div id=\"ceff\"></div>"
  , "      <div id=\"cfoot\">read-only: the union every file is parsed with."
      <> " A file's own #+TODO: line adds to it and outranks these for that"
      <> " file's own headlines.</div>"
  , "    </div>"
  , "  </div>"
  , "  <div id=\"echo\" role=\"status\" aria-live=\"polite\"></div>"
  , "  <script id=\"keys\" type=\"application/json\">" <> keyBindingsJSON <> "</script>"
  -- The shell's configuration, the keymap blob's own pattern: eight per-build
  -- constants and the per-request default view, read by glue.js as `CFG'.
  -- The script itself is a compiled-in asset like the renderer, so the page
  -- inlines no executable JS at all — the theme boot line in <head> aside.
  , "  <script id=\"cfg\" type=\"application/json\">" <> glueConfig views <> "</script>"
  , "  <script src=\"" <> T.pack rendererAsset <> "\"></script>"
  , "  <script src=\"" <> T.pack glueAsset <> "\"></script>"
  ]

-- | A table popup's frame: NAME the wrapper the backdrop wears, P the letter
-- every part of it is prefixed with, OVERLAY the edit box its pane positions.
--
-- ONE FRAME for the link popup and the tags popup — a head, a positioning pane
-- holding a mount, a foot — since the overlay is the whole of what they differ
-- in and the size is the tier's.  A third table popup is a call rather than
-- eight lines nothing compares.
popupFrame :: Text -> Text -> Text -> Text -> [Text]
popupFrame name p tier overlay =
  [ "  <div id=\"" <> name <> "\">"
  , "    <div id=\"" <> p <> "box\" class=\"" <> tier <> "\">"
  , "      <div id=\"" <> p <> "head\"></div>"
  , "      <div id=\"" <> p <> "pane\"><div id=\"" <> p <> "table\"></div>"
      <> overlay <> "</div>"
  , "      <div id=\"" <> p <> "foot\"></div>"
  , "    </div>"
  , "  </div>"
  ]

-- | A bare text field, which is what every edit overlay on this page lays over
-- the cell it opens.
field :: Text -> Text
field name = "<input id=\"" <> name <> "\" spellcheck=\"false\">"

-- | One row of the settings sheet: LABEL beside CONTROL.
crow :: Text -> Text -> Text
crow label control = "        <div class=\"crow\">" <> label <> control <> "</div>"

-- | A settings row's label.  The layer box's is the exception and is written
-- out where it stands: it is empty and carries an id, the sheet filling it with
-- where that layer's file is.
clab :: Text -> Text
clab word = "<div class=\"clab\">" <> word <> "</div>"

-- | A settings field: ID under the sheet's one control class, EXTRA whatever
-- attributes that one field owes, HINT the placeholder — which is also the
-- value the field falls back to when a reader empties it, so it is spelled from
-- the same constant the server answers with.
cinput :: Text -> Text -> Text -> Text
cinput name extra hint =
  "<input id=\"" <> name <> "\" class=\"cview\" spellcheck=\"false\"" <> extra
    <> " placeholder=\"" <> escape hint <> "\">"

-- | The page a browser gets when DIR — the @--assets@ directory — holds no
-- renderer: what still works, and the two ways out.  Reachable under that flag
-- alone; drop it and 'Glance.Web.Routes.embeddedRenderer' serves, so a default run never sees
-- this page.
assetsMissing :: ServeOptions -> FilePath -> Text
assetsMissing opts dir = page "" [] "glance — JSON only" $ T.unlines
  [ "  <h1>glance — JSON-only mode</h1>"
  , "  <p>No <code>" <> T.pack rendererAsset <> "</code> under <code>"
      <> escape (T.pack dir) <> "</code>, and <code>--assets</code> replaces the"
      <> " renderer this binary carries, so there is no table to render here."
      <> " The server is otherwise complete:</p>"
  , "  <p><code>curl -s localhost:" <> T.pack (show (soPort opts))
      <> "/headlines | jq '.rows | length'</code></p>"
  , "  <p>Drop <code>--assets</code> to get the built-in renderer back, or point"
      <> " it at a directory holding <code>" <> T.pack rendererAsset
      <> "</code> (the <code>web/</code> directory of a table-view checkout):</p>"
  , "  <p><code>glance serve --dir " <> escape (T.pack (soDir opts))
      <> " --assets /path/to/table-view/web</code></p>"
  ]

