-- | The two documents this server serves: the shell, and the no-renderer page.
module Glance.Web.Page (demoShell, assetsMissing) where

import Data.Text (Text)

import qualified Data.Text as T

import Glance.Web.Base ( ServeOptions (..), escape, glueAsset
                       , elmAsset, rendererAsset, viewTitleFor )
import Glance.Web.Keymap (keyBindingsJSON)
import Glance.Web.Theme (Theme (..), themes)
import Glance.Web.Page.Glue (glueConfig)
import Glance.Web.Page.Style (fontFace, page)


demoShell :: ServeOptions -> Maybe FilePath -> [(Text, [(Text, Text)])]
          -> [(Text, Text)] -> Text
demoShell opts font colours views =
  page (fontFace font) colours (viewTitleFor (soDir opts)) $ T.unlines $
  [ "  <div id=\"app\"></div>"
  , "  <div id=\"log\"></div>"
  , "  <div id=\"kbd\"></div>"
  , "  <div id=\"modal\">"
  , "    <div id=\"sheet\" class=\"pop-sheet\">"
  , "      <div id=\"mhead\"><span id=\"mfile\"></span><span id=\"mnote\"></span></div>"
  , "      <div id=\"mwhere\"></div>"
  -- Each pane positions its own edit overlay: a redraw would throw one away.
  , "      <div id=\"mpanes\">"
  , "        <textarea id=\"mtext\" spellcheck=\"false\"></textarea>"
  , "        <div id=\"mdoc\"><div id=\"dlist\"></div>"
      -- Autocorrect and friends OFF: these boxes write ORG into the user's files.
      <> "<div id=\"dtitle\"><input id=\"dtin\" spellcheck=\"false\" autocomplete=\"off\""
      <> " autocapitalize=\"off\" autocorrect=\"off\"></div>"
      <> "<div id=\"dpara\"><textarea id=\"dtext\" spellcheck=\"false\" autocomplete=\"off\""
      <> " autocapitalize=\"off\" autocorrect=\"off\"></textarea></div>"
      <> "</div>"
  , "        <div id=\"mprops\"><div id=\"mptable\"></div>"
      <> "<div id=\"pedit\"><input id=\"pkey\" spellcheck=\"false\">"
      <> "<input id=\"pval\" spellcheck=\"false\"></div></div>"
  , "      </div>"
  , "      <pre id=\"mlog\"></pre>"
  , "    </div>"
  , "  </div>"
  ]
  <> popupFrame "tags" "t" "pop-band" ("<div id=\"tedit\">" <> field "tname" <> "</div>")
  <>
  [ "  <div id=\"prompt\">"
  , "    <div id=\"pbox\" class=\"pop-band\">"
  , "      <div id=\"phead\"></div>"
  , "      <input id=\"pinput\" spellcheck=\"false\" autocomplete=\"off\">"
  , "      <div id=\"plist\"></div>"
  , "      <div id=\"pfoot\"></div>"
  , "    </div>"
  , "  </div>"
  ]
  -- RAISED OVER THE PALETTE, which stands beneath: ESC hands it back.
  <>
  [ "  <div id=\"mint\">"
  , "    <div id=\"nbox\" class=\"pop-band\">"
  , "      <div id=\"nhead\">new TODO state</div>"
  , nrow "nspace" "namespace" "<select id=\"nspace\" class=\"cview\"></select>"
  , nrow "nname" "state" ("<input id=\"nname\" spellcheck=\"false\" autocomplete=\"off\""
                            <> " autocapitalize=\"off\" placeholder=\"letters and _\">")
  , nrow "ngroup" "group" ("<select id=\"ngroup\" class=\"cview\">"
                             <> "<option value=\"active\">active</option>"
                             <> "<option value=\"inactive\">inactive</option></select>")
  -- ONE HUE PER THEME: the colour config is keyed by theme, so two fields.
  , nrow "nlight" "light hue" (hueField "nlight")
  , nrow "ndark" "dark hue" (hueField "ndark")
  , "      <div id=\"nfoot\">TAB walks · RET adds it · ESC leaves</div>"
  , "    </div>"
  , "  </div>"
  ]
  <> popupFrame "links" "l" "pop-sheet"
       ("<div id=\"ledit\">" <> field "ltitle" <> field "lurl" <> "</div>")
  -- The picker hangs at the CARET rather than centring, so it takes no tier.
  <>
  [ "  <div id=\"refer\">"
  , "    <div id=\"rbox\">"
  , "      <div id=\"rmount\"></div>"
  -- THE KIND IS THE EDGE'S: drawn APART from the row's badges and filter chips.
  , "      <div id=\"rkind\"></div>"
  , "      <div id=\"rfoot\">n/p move · / filter or kind: · K kind · DEL drop"
      <> " · RET link · ESC dismiss</div>"
  , "    </div>"
  , "  </div>"
  ]
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
  -- Panel bodies wear `cpart'; glue.js's `SECTIONS' wraps them at boot.
  <>
  [ "  <div id=\"config\">"
  , "    <div id=\"cbox\" class=\"pop-sheet\">"
  , "      <div id=\"chead\"><span id=\"ctitle\">settings</span>"
      <> "<span id=\"cnote\"></span></div>"
  , "      <div id=\"ctabs\"></div>"
  , "      <div id=\"csecs\"></div>"
  , "      <div id=\"ctheme\" class=\"cpart\">"
  , crow (clab "theme")
         ("<select id=\"themesel\" class=\"cview\" title=\"theme\">"
            <> "<option value=\"auto\">auto</option>"
            <> T.concat [ "<option value=\"" <> thId t <> "\">" <> escape (thLabel t)
                            <> "</option>" | t <- themes ]
            <> "</select>")
  , "      <div id=\"chues\" class=\"cpart\"><div id=\"cstates\"></div>"
      <> "<div id=\"sedit\"><input id=\"sname\" spellcheck=\"false\">"
      <> "<input id=\"sgroup\" spellcheck=\"false\">"
      <> "<input id=\"shue\" spellcheck=\"false\" title=\"light hue\">"
      <> "<input id=\"sdark\" spellcheck=\"false\" title=\"dark hue\"></div></div>"
  , "      </div>"
  , "      <div id=\"clayers\" class=\"cpart\">"
  , crow (clab "layer")
         "<select id=\"clayer\" class=\"cview\" title=\"config layer\"></select>"
  , crow "<div id=\"clab\" class=\"clab\"></div>"
         ("<textarea id=\"ctext\" class=\"ctext\" spellcheck=\"false\""
            <> " placeholder=\"#+TODO: TODO STARTED | DONE\"></textarea>"
            <> "<div id=\"clerr\" class=\"cerr\"></div>")
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
  , "  <script id=\"cfg\" type=\"application/json\">" <> glueConfig views <> "</script>"
  , "  <script src=\"" <> T.pack rendererAsset <> "\"></script>"
  , "  <script src=\"" <> T.pack elmAsset <> "\"></script>"
  , "  <script src=\"" <> T.pack glueAsset <> "\"></script>"
  ]

-- | A table popup's frame: NAME the wrapper, P the prefix every part id wears,
-- TIER the size class, OVERLAY the edit box.
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

field :: Text -> Text
field name = "<input id=\"" <> name <> "\" spellcheck=\"false\">"

-- | A labelled row of the mint form, in the capture form's two classes.  The
-- `nrow-NAME' class is the browser suite's handle for taking one field away.
nrow :: Text -> Text -> Text -> Text
nrow name label control =
  "      <div class=\"krow nrow-" <> name <> "\"><label class=\"klab\">"
    <> label <> "</label>" <> control <> "</div>"

hueField :: Text -> Text
hueField name =
  "<input id=\"" <> name <> "\" spellcheck=\"false\" autocomplete=\"off\""
    <> " placeholder=\"#RRGGBB — empty keeps the palette's own\">"

crow :: Text -> Text -> Text
crow label control = "        <div class=\"crow\">" <> label <> control <> "</div>"

clab :: Text -> Text
clab word = "<div class=\"clab\">" <> word <> "</div>"


-- | The page served when DIR — the @--assets@ directory — holds no renderer.
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

