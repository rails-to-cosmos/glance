-- | The document every served page wears: the wrapper, the stylesheet, the
-- type stack.
--
-- Styles are inline and the only asset a page names is the renderer, so a
-- served document reaches nothing off this server (docs\/invariants.md).
module Glance.Web.Page.Style ( page
                             , fontAssets
                             , fontFace
                             ) where

import Data.Text (Text)
import System.FilePath (takeExtension)

import qualified Data.Text as T

import Glance.Web.Base (escape, logLinesDefault)

-- Type

-- | The type stack the shell asks for, in the table, the sheet and the widgets
-- alike.  Nothing is fetched for it: these are names looked up on the machine,
-- and 'fontFace' adds an @\@font-face@ only when an @--assets@ directory holds
-- a file.  A page that reaches the network for a font renders differently
-- offline, and this one serves over loopback to a machine that may have none
-- (docs\/invariants.md).
monoStack :: Text
monoStack = "\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"

-- | The font files the shell will use when the assets directory holds one,
-- best first.  With neither there, 'monoStack' falls through to whatever is
-- installed and the page says nothing about it.
fontAssets :: [FilePath]
fontAssets = ["JetBrainsMono-Regular.woff2", "JetBrainsMono-Regular.ttf"]

-- | An @\@font-face@ for NAME, which the asset route serves out of the same
-- directory the renderer comes from.  The @src@ is a bare file name, resolved
-- against this server the way the renderer's own @src@ is.
fontFace :: Maybe FilePath -> Text
fontFace Nothing     = ""
fontFace (Just name) = T.concat
  [ "  @font-face{font-family:\"JetBrains Mono\";font-display:swap;"
  , "src:url(\"", T.pack name, "\") format(\"", format, "\")}" ]
  where format | takeExtension name == ".woff2" = "woff2"
               | otherwise                      = "truetype"

-- | BODY wrapped in a document titled TITLE, with HEAD opening the style block.
-- Styles inline, no asset but the renderer and whatever HEAD names on this same
-- server, dark and light both.
page :: Text -> Text -> Text -> Text
page head' title body = T.unlines
  [ "<!doctype html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>" <> escape title <> "</title>"
  , "<style>" <> (if T.null head' then "" else "\n" <> head')
  -- The palette is danneskjold, this author's Emacs theme, in one
  -- custom-property set the whole page reads.  From danneskjold-theme.el
  -- (../danneskjold-theme, github.com/rails-to-cosmos/danneskjold-theme).
  -- Dark: `default' #FFFFFF on #000000, `region' #373D4F,
  -- `font-lock-comment-face' #A4C2EB, `company-tooltip' #21252B,
  -- `org-done'/success #B6E63E, `error' #E74C3C, `accent' #4CB5F5.  Light:
  -- #000000 on #FFFFFF, `light-comment' #7F8C8D, `light-surface' #F8F8FF,
  -- `light-golden' #FFD600 for selection, `green-dark' #27AE60 where white
  -- needs the darker green.
  --
  -- @--g-border@ is the one value off palette, and it is the renderer's:
  -- @table-view.js@ draws its own rules in @--tv-border@, #E3E6EA light and
  -- #2a2d3d dark, so the page's hairlines weigh the table's rather than frame
  -- it in heavier chrome.  danneskjold's own `vertical-border' #223959 and
  -- `light-dim' #BDC3C7 frame at 1.8:1 against their ground, these at 1.25:1
  -- and 1.5:1; text contrast is untouched, only the rules recede.
  --
  -- Three ways, the renderer's own pattern: the media query is the default and
  -- @data-theme@ on the root pins it — the attribute the @theme:@ selector
  -- writes and the renderer's overrides key off.  `--g-col' and `--g-cell-wash'
  -- alias the RENDERER's crosshair: its column band `--tv-col', a pale amber,
  -- taken at `--tv-cell-wash' over the cursor row's own ground where the two
  -- cross.  The structured document's horizontal point wears that same stack,
  -- so vertical is the ROW language and horizontal the COLUMN language on both
  -- surfaces.  Hand-copied literals like `--g-border' and `--g-sel', and for
  -- their reason: these properties live on `.tv-root', unreadable outside a
  -- mount, so each hex is spelled ONCE here, every rule that wants it reads the
  -- alias, and a renderer change to any of them needs a matching edit nothing
  -- detects.
  , "  :root{--glance-mono:" <> monoStack <> ";"
  , "    --g-bg:#FFFFFF;--g-fg:#000000;--g-border:#E3E6EA;--g-mute:#7F8C8D;"
  , "    --g-surface:#F8F8FF;--g-sel:#FFD600;--g-accent:#4CB5F5;"
    -- THE LINK INK, hand-copied from the renderer's `--tv-link' the way
    -- `--g-border' and `--g-sel' are: it is declared on `.tv-root' rather than
    -- the document element, so a live `var()' read resolves to nothing outside
    -- the mount.  ALIASED — one name per theme, every use reading
    -- `var(--g-link)' — so a renderer change is one edit here rather than one
    -- per link.
  , "    --g-link:#30739B;"
  , "    --g-col:#FFF3D0;--g-cell-wash:60%;--g-doc-pad:6px;"
      -- The document pane's own inset, named because the overlays laid over it
      -- have to answer for it: an absolutely positioned box is placed against
      -- the PADDING box and the text it covers sits inside the content box.
  , "    --g-doc-padx:10px;--g-doc-pady:8px;"
  , "    --g-doc-fs:13px;--g-doc-lh:1.6;"
    -- THE SCROLLOFF BAND, three of the document pane's OWN lines, off the two
    -- numbers the pane is set in, so a type-scale change moves the band rather
    -- than leaving a pixel figure that used to be three lines.  One variable,
    -- one arithmetic — the grid's own discipline on the other axis.
  , "    --g-doc-off:calc(3 * var(--g-doc-fs) * var(--g-doc-lh));"
      -- ONE TOP LINE FOR EVERY POPUP: a shallow palette and a tall sheet open
      -- with their top borders on one rule, so raising one after another does
      -- not move the eye down the window and back.  Growth is DOWNWARD from it
      -- and `--g-pop-max' is what is left under it, so a tier's bounds cannot
      -- put a box off the screen.  Three properties, one site, read by every
      -- backdrop and tier.  SYMMETRIC: the foot margin is the head's, DERIVED
      -- from the anchor rather than a second literal — a tall box stopping
      -- short by a different figure read as one out of room.  At the shipped
      -- anchor the two agree and the 90vh cap never binds; the `min' keeps a
      -- RAISED anchor capped all the same.
  , "    --g-pop-top:5vh;--g-pop-pad:24px;"
  , "    --g-pop-max:min(90vh,"
  , "      calc(100vh - 2 * var(--g-pop-top)));"
  , "    --g-ok:#27AE60;--g-warn:#FFA500;--g-bad:#E74C3C}"
  , "  @media (prefers-color-scheme:dark){:root{--g-bg:#000000;--g-fg:#FFFFFF;"
  , "    --g-border:#2A2D3D;--g-mute:#A4C2EB;--g-surface:#21252B;--g-sel:#373D4F;"
  , "    --g-link:#7CC9F8;"
      -- The band's hue does not move between themes; what moves is how far it
      -- washes, since dark's ink has less room over a lit ground.
  , "    --g-cell-wash:9%;--g-ok:#B6E63E}}"
  , "  :root[data-theme=\"light\"]{--g-bg:#FFFFFF;--g-fg:#000000;--g-border:#E3E6EA;"
  , "    --g-mute:#7F8C8D;--g-surface:#F8F8FF;--g-sel:#FFD600;--g-cell-wash:60%;"
  , "    --g-link:#30739B;--g-ok:#27AE60}"
  , "  :root[data-theme=\"dark\"]{--g-bg:#000000;--g-fg:#FFFFFF;--g-border:#2A2D3D;"
  , "    --g-mute:#A4C2EB;--g-surface:#21252B;--g-sel:#373D4F;--g-cell-wash:9%;"
  , "    --g-link:#7CC9F8;--g-ok:#B6E63E}"
  , "  body{margin:0;font:14px/1.5 var(--glance-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);"
  -- One column, exactly the viewport tall: the table at the height it asks for,
  -- the log taking whatever that leaves, the key line last.  The page itself
  -- never scrolls — the two boxes that can outgrow their room scroll inside
  -- themselves — so the key line stays on screen with no scrollbar under it.
  , "    height:100vh;box-sizing:border-box;overflow:hidden;"
  -- One padding on all four sides.  The extra 10px on top was the fixed status
  -- corner's room, and the corner is gone: nothing floats over the table's top
  -- edge, so the table starts where the page does.
  , "    padding:24px;display:flex;flex-direction:column;gap:14px}"
  , "  h1{font-size:16px;margin:0}"
  , "  p{margin:0;max-width:70ch}"
  , "  code{font-size:12px;color:var(--g-mute)}"
  -- The height that is LEFT, and none it cannot give back: the table takes the
  -- room the log gives up under its cap, and a window shorter than the column's
  -- parts takes the difference out of the table — which scrolls inside itself —
  -- rather than off the key line, which does not.
  , "  #app{flex:1 1 auto;min-height:0}"
  -- The renderer injects its own `.tv-root' font, and injects it from a script,
  -- so its rule lands after this element and ties on specificity.  One more
  -- selector step settles it, and leaves the size and the leading it set.
  , "  #app .tv-root{font-family:var(--glance-mono)}"
  -- The selected row is marked once, by the renderer's own secondary-highlight
  -- background; the accent stripe this page drew was a second mark that
  -- disagreed where the row began.  The log repeats the table's container under
  -- it: same width, this rule being the one place either width is set, and the
  -- same hairline, radius and surface tint as @.tv-root@.  Its height is FIXED
  -- (below) and scrolls inside that, and the frame is resident with nothing to
  -- say, so a long message moves nothing and an arriving event never moves the
  -- key line under it or the table above it.
  , "  #app,#log{width:100%;box-sizing:border-box}"
  -- N LINES, ALWAYS: the strip is exactly N of its own line boxes tall whatever
  -- it holds, the messages scrolling inside a fixed frame.  A strip that GREW
  -- resized the table under a reader's cursor on every logged line — the one
  -- thing a keyboard surface must not do — and a quiet page now reads like a
  -- busy one.  The height is computed rather than eyeballed: a line box is the
  -- body's unitless 1.5 over this rule's OWN font size, which is exactly what
  -- `em' reads for a length here, so the size is declared once and the
  -- arithmetic follows it rather than restating `12px'.
  -- `box-sizing:border-box' counts the 6px padding and the 1px border twice
  -- each inside the figure, so those two stay spelled out and it is a sum
  -- rather than `140px'.  N IS A CUSTOM PROPERTY, which is what makes it a
  -- preference: the arithmetic lives HERE, once, and the settings sheet writes
  -- a NUMBER onto the element.  The declared value is 'logLinesDefault', so a
  -- page whose glue has not run — or a reader who never touched the field —
  -- gets the same figure the sheet would put back.  `flex:none' is the other
  -- half: the strip takes its declared height and `#app', the flexible one, the
  -- whole of the rest.
  , "  #log{font-size:12px;color:var(--g-mute);padding:6px 10px;"
  , "    border:1px solid var(--g-border);border-radius:8px;"
  , "    --g-logn:" <> T.pack (show logLinesDefault) <> ";"
  , "    height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px);"
  , "    background:var(--g-surface);flex:none;overflow-y:auto}"
  -- A line is spans so its parts can be told apart: stamp and scope recede into
  -- the strip's colour, the message takes the page's text colour, and the
  -- severity alone changes colour, making a warning findable.  The counter is
  -- empty until a line repeats, an empty span occupying nothing.
  , "  #log div>span{margin-right:6px}"
    -- THE SEVERITY AND THE SCOPE ARE COLUMNS, so every message starts at one x
    -- and the strip reads down rather than ragged.  Each width is its own
    -- longest WORD, the words untouched: `error' sets the severity at 5ch and
    -- `config' the scope at 6ch, so a longer scope moves this number, and
    -- `TestServe' derives both off the page's `append' calls to say so.
  , "  #log .lv,#log .lc{display:inline-block}"
  , "  #log .lv{width:5ch}"
  , "  #log .lc{width:6ch}"
  , "  #log .lt{opacity:.65}"
  , "  #log .lm{color:var(--g-fg)}"
  , "  #log .warn .lv{color:var(--g-warn)}"
  , "  #log .error .lv{color:var(--g-bad)}"
  -- The resident key line, and the page's last: what can run, where the echo
  -- pill says what just did.  Slim and muted, so it reads as chrome rather
  -- than as content; one line that scrolls sideways instead of wrapping, so a
  -- narrow window cannot grow it into the table's room.
  , "  #kbd{flex:none;font-size:11px;color:var(--g-mute);white-space:nowrap;"
  , "    overflow-x:auto;padding:0 2px}"
  -- The sheet is the one place the author's Emacs font is asked for by name:
  -- the subtree reads there as it reads in the buffer it came out of, in the
  -- page's own colours, which are already danneskjold's.  The backdrop is a
  -- direct child of the body, which is neither transformed nor positioned, so
  -- these two levels are the root stacking context's and clear the renderer's
  -- chrome outright — its sticky @th@ carries @z-index:1@ and its completion
  -- list @5@, and an unnumbered backdrop paints under both.  The sheet is a
  -- flex item, so its own level would apply anyway; @position@ says so without
  -- relying on that.  The two overlays share the backdrop and the two levels —
  -- the sheet is the subtree's, the prompt a command's, and a reader has one
  -- open at a time — and the prompt sits high rather than centred, since a list
  -- that grows downward should not move the line above it.
  , "  #modal,#prompt,#config,#links,#tags{--dk-mono:\"Hack\", var(--glance-mono);"
  , "    display:none;position:fixed;inset:0;z-index:100;background:#0009;"
  , "    padding:var(--g-pop-pad);padding-top:var(--g-pop-top);"
  , "    align-items:flex-start;justify-content:center}"
  , "  #modal.on,#prompt.on,#config.on,#links.on,#tags.on{display:flex}"
  -- Four fifths of the window, in both directions: two panes of monospace want
  -- the room, and the fifth left over is what says there is a table under this
  -- rather than a page of its own.  The `min' keeps it inside the backdrop's
  -- padding on a window too narrow for the share to fit.
  , "  #sheet{display:flex;flex-direction:column;gap:8px;padding:14px;border-radius:6px;"
  , "    position:relative;z-index:101;font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
    -- THE TWO SHEETS WEAR ONE HEADER: name left, state word right.  Declared
    -- once, the same widget over different files — the state RULES below cannot
    -- join them, `#mnote''s spelling being pinned by the suite.
  , "  #mhead,#chead{display:flex;justify-content:space-between;gap:12px;"
  , "    font-size:12px}"
  , "  #mfile,#ctitle{color:var(--g-mute)}"
  -- The crumb strip: one line, standing, and never wrapping — a trail that
  -- reflowed would move the panes under a reader who had just arrived.  It
  -- scrolls sideways instead, the way the key line does.
  , "  #mwhere{display:flex;gap:5px;align-items:center;font-size:12px;"
  , "    white-space:nowrap;overflow-x:auto;flex:none}"
  -- A CRUMB, in the renderer's own silhouette: the pill `.tv-chip' draws, at
  -- `.tv-chip-muted''s ink and ground — hand-copied for `--g-border''s reason,
  -- those rules living inside `.tv-root' — so a renderer chip change needs a
  -- matching edit here and nothing detects the drift.  A long title is cut
  -- rather than pushing the trail off the bar.
  , "  .wc{display:inline-flex;align-items:center;flex:0 1 auto;min-width:0;"
  , "    padding:1px 8px;border-radius:999px;border:1px solid var(--g-border);"
  , "    color:var(--g-mute);background:transparent;"
  , "    overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
  -- WHERE THE READER STANDS takes the full ink, which the table's own crumbs
  -- need not: there every crumb is somewhere left behind and the live view is
  -- the chips beside them; here the last crumb IS the view.
  , "  .wc.wat{color:var(--g-fg)}"
  , "  #mnote{text-align:right;color:var(--g-ok)}"
  , "  #mnote.syncing{color:var(--g-mute)}"
  , "  #mnote.conflict,#mnote.error{color:var(--g-bad)}"
  -- Two panes, wrapping rather than querying: a window too narrow for both side
  -- by side puts the panel under the text, the answer a width breakpoint would
  -- give at no second place to keep it.  C-c ' takes the panel off the sheet
  -- and the text has the width.  THE PANES ROW CLAMPS, and `overflow' makes it:
  -- `flex:1;min-height:0' lets the row be SIZED by the bounded sheet above it
  -- but does not stop its content painting past that size, and with `flex-wrap'
  -- the flex LINE is content-sized while `align-items:stretch' stretches the
  -- panes to the LINE rather than the box — so a tall subtree grew the line,
  -- which painted outside the sheet, the panes' unbounded heights never
  -- engaging `overflow'.  `overflow:hidden' bounds the line, handing each pane
  -- a definite height to scroll inside.
  , "  #mpanes{flex:1;min-height:0;overflow:hidden;"
  , "    display:flex;flex-wrap:wrap;gap:10px}"
      -- The panes are one frame language, so they wear ONE radius: the panel's
      -- comes from `.tv-root' and is 8px, which is the page's own — the log
      -- strip and the sheet's logbook already wear it — so the textarea takes
      -- it too rather than being the one corner cut tighter than its neighbour.
  -- NO FLOOR ON THE PANE.  A `min-height' on a flex child is a refusal to
  -- shrink, so raw mode's textarea held the panes row open at its figure,
  -- pushing the sheet past its bound — this file's own bug, from when the box
  -- grew and a two-row textarea would have made raw mode a slot rather than an
  -- escape hatch.  THE BOX IS FIXED now (`.pop-sheet'), so nothing is left for
  -- a pane to hold open: it stretches into the box and scrolls inside.
  , "  #mtext{min-height:0;flex:2 1 320px;min-width:0;font:12px/1.5 var(--dk-mono);padding:8px;"
  , "    border-radius:8px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit;resize:none}"
  , "  #mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- ONE FOCUS LANGUAGE ACROSS THE SHEET: whichever pane holds the keys says so
  -- on its own FRAME, in the accent; neither wears it otherwise.  The panes
  -- differ in kind — a textarea takes a real focus, the panel holds the keys
  -- with nothing focused — so the browser's ring dresses only one, and a reader
  -- crossing with TAB would watch the mark disappear.  Declared here rather
  -- than left to the UA: one treatment rather than two resembling each other,
  -- and the ring goes with it, the border is the mark.
  , "  #mtext:focus{outline:none;border-color:var(--g-accent)}"
  -- THE STRUCTURED DOCUMENT, the sheet's one pane.  Its whole look is FLOWING
  -- TEXT — no rules, no boxes, no labels — so a reader who is not editing sees
  -- the entry as org spells it, and what marks structure is the CURSOR alone:
  -- the element under point wearing the page's own selection, the language the
  -- table draws a selected row in.  It is also the overlay's positioning
  -- parent, as `#tpane' is the rename's and `#lpane' the link edit's: the list
  -- is redrawn whole on every move, so an edit living inside an element would
  -- be thrown away by the next draw.
  , "  #mdoc{flex:2 1 320px;min-width:0;min-height:0;position:relative;"
  , "    overflow:auto;padding:var(--g-doc-pady) var(--g-doc-padx);"
  , "    font:var(--g-doc-fs)/var(--g-doc-lh) var(--dk-mono);"
  , "    border:1px solid var(--g-border);border-radius:8px}"
  , "  #sheet.raw #mdoc{display:none}"
  , "  #sheet:not(.raw) #mtext{display:none}"
  -- THE SAME MARK, in the accent: whichever pane holds the keys says so on its
  -- own FRAME, the other none.  NEITHER focuses anything in the structured
  -- shape, so the browser's ring dresses neither and a TAB crossing has nothing
  -- to read it off.  `#mprops.on' is the panel holding them, the same state
  -- `pnav' reads; the document has them when it does not.
  , "  #mprops.on .tv-root,#mdoc.on{border-color:var(--g-accent)}"
  -- The panel's cursor takes the same guard.  Its wash is the RENDERER's
  -- (`tr.tv-sel'), so gating it from here costs TWO rules: the suppression and
  -- the STRIPE put back under it, since the renderer's `tr.tv-alt' is of equal
  -- specificity and this one would outrank it, so an unfocused cursor row on an
  -- even row would lose stripe as well as wash.  `--tv-alt' resolves because
  -- these selectors sit inside `.tv-root'.  The clean retirement is a `cursor:'
  -- gate in table-view: a sibling-repo change plus `make sync-renderer'.
  , "  #mprops:not(.on) .tv-table tbody tr.tv-sel{background:transparent}"
  , "  #mprops:not(.on) .tv-table tbody tr.tv-sel.tv-alt{background:var(--tv-alt)}"
  -- The property panel: the mount's host, the pane that positions its overlay,
  -- and raw mode taking it off the sheet.  No frame of its own — `.tv-root'
  -- brings the hairline and the shared radius.
  , "  #mprops{flex:1 1 240px;min-width:0;min-height:0;position:relative;"
  , "    overflow:hidden;display:flex;flex-direction:column}"
  , "  #mptable{flex:1;min-height:0;display:flex}"
  , "  #sheet.raw #mprops{display:none}"
  -- The sheet's own face for the panel and the two popups' mounts, which is the
  -- one rule they share with the pane above: the sheet's monospace rather than
  -- the page's.
  , "  #mptable .tv-root,#ltable .tv-root,#ttable .tv-root{flex:1;min-width:0;"
  , "    font-family:var(--dk-mono)}"
  -- AN ELEMENT: a row of the document, wearing the cursor and nothing else —
  -- the page's selection under point, the warn tint of a flag beside it.  The
  -- rhythm is the reading rhythm: a headline line, then its planning and its
  -- drawer indented under it the way org writes them, then the paragraphs, then
  -- the children one level in.  EVERY SELECTION HERE IS A GROUND — the element
  -- under point, the cell under point and a flagged element are all a
  -- background, and no rule of them draws an underline, a border or an outline
  -- — which is the TABLE's own language (`.tv-sel' the cursor row's ground,
  -- `.tv-cell-sel' a further wash where the column band crosses it), for the
  -- same reason: a locator that adds a LINE to a row of text moves the text,
  -- and a document is read as text.  The `de' radius below is the element's own
  -- shape rather than a selection's, which is why it sits on the base rule.
  -- ONE GRID, ONE BASE: every element of the document is inset by the same
  -- `--g-doc-pad', so the headline line and the content under it count from one
  -- place — a second literal here and in the paragraph's own rule is exactly
  -- the drift a reader sees as a star column out of line.
  , "  .de{scroll-margin-block:var(--g-doc-off);"
  , "    padding:1px var(--g-doc-pad);border-radius:4px;white-space:pre-wrap;"
  , "    overflow-wrap:anywhere}"
    -- A CURSOR IS ONLY DRAWN WHERE THE KEYS ARE.  The pane holding them says so
    -- on its frame already (`#mdoc.on', `#mprops.on'), and the CURSOR INSIDE it
    -- takes the same guard, so the sheet never shows two of them and a reader
    -- looking at either pane is looking at the one that will move.  The
    -- POSITION does NOT go with the focus: it is the model's, so crossing away
    -- and back finds the cursor where it was left and the wash simply returns.
    -- The FLAG is a queue a reader has laid down rather than a cursor, so it
    -- keeps its ground either way and has to be readable from the other pane.
  , "  #mdoc.on .de.dat{background:var(--g-sel);color:var(--g-fg)}"
  , "  .de.dfl{background:color-mix(in srgb, var(--g-warn) 22%, transparent)}"
  , "  #mdoc.on .de.dat.dfl{background:color-mix(in srgb, var(--g-warn) 38%, var(--g-sel))}"
  -- The paragraphs take back the blank line between them, which is the rhythm
  -- org writes them in.  A paragraph is CONTENT, so it starts at the title's
  -- column rather than at the stars': `org-startup-indented' laid over the
  -- org-cleaned stars above.  PADDING rather than a margin or a `text-indent',
  -- each ruled out by something this page needs — a margin would shrink the
  -- element's box and take the selection wash off the left of the line with it,
  -- a `text-indent' would indent the block's FIRST line alone where a paragraph
  -- is a run of them — and padding keeps the box full width, so the ground
  -- still covers the whole line and the text still starts where org would.  The
  -- file bytes are untouched: this is chrome, and `bodyText' never reads it.
  , "  .d-para,.d-comp{margin:.5em 0;"
  , "    padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}"
  -- A COMPOSITE IS A CONTAINER, so the inset is its and its parts sit flush
  -- inside it: an item carries its own leading spaces in the file already and
  -- `pre-wrap' keeps them, so a second indent here would say the depth twice.
  , "  .d-comp{padding-top:0;padding-bottom:0}"
  , "  .d-item{padding-left:0;padding-right:0}"
  -- THE INERT RUNS: what the composite covers and no leaf claims — the
  -- `#+begin_'/`#+end_' delimiters, the blank line org lets stand between two
  -- items, a lead-in the list opener did not take.  Drawn muted because they
  -- are not stops: nothing lands on them, so nothing should look like it might.
  , "  .dg{padding:0;white-space:pre-wrap;overflow-wrap:anywhere;color:var(--g-mute)}"
  -- A LINK, in the renderer's own link language: its ink and an underline, the
  -- two marks the table already spends on one.  No href, no cursor, no handler
  -- — `o' is the opener here and a link is not a stop — so what the style says
  -- is "there is a reference in this text", the whole of what the display owes.
  , "  .dl{color:var(--g-link);text-decoration:underline}"
  -- A HEADLINE LINE IS A LINE, and org lays one out with the tags FLUSHED RIGHT
  -- (`org-tags-column'): stars, then the state and the priority, then the title
  -- taking whatever room is left, and the tags against the far edge.  So the
  -- two headline kinds are flex rows where a paragraph is flowing text — the
  -- title is the flexible middle and `margin-left:auto' puts the tags at the
  -- edge whether or not a title is in front of them to push them there.  The
  -- selection is unaffected: the element's ground is the whole line, gap
  -- included, and the cell wash lands on the tags cell where it sits.
  , "  .d-head,.d-child{display:flex;align-items:baseline}"
  , "  .d-child{color:var(--g-fg)}"
  , "  .d-head{font-weight:600}"
  -- The stars, org-cleaned.  Every star but the last is a SPACE, so the run
  -- carries no ink of its own and the last one takes the page's — which is what
  -- `org-hide-leading-stars' comes to once its hide face has done its work.  The
  -- run has to keep its spaces, hence `pre'.
  , "  .ds{white-space:pre;color:var(--g-fg);font-weight:400;flex:none}"
  -- THE STAR HANGS IN A GUTTER THE BODY RESPECTS, the two being the SAME
  -- arithmetic rather than the same glyph count: the head's prefix takes the
  -- gutter's width outright, what a paragraph is padded by, so the title and
  -- every content line below start at one x whatever the font does — a bold or
  -- fallback face of different advance cannot move one without the other.  A
  -- CHILD's prefix is off this grid and sizes to its own text: its star hangs
  -- one deeper, at the column its content will use when materialized.
  , "  .d-head .ds{width:calc(var(--g-doc-indent, 2) * 1ch)}"
  -- A cell is a word in a line, spaced like one.  The TITLE gives: it takes the
  -- room the line has left, leaving the tags at the edge and letting a long one
  -- wrap inside its cell rather than pushing them off.
  , "  .dc{margin-right:.6em;flex:none}"
  , "  .dc-title{flex:1 1 auto;min-width:0}"
  -- The cell under point: the table's own CROSSHAIR, `.tv-cell-sel' over
  -- `.tv-sel' exactly — the column band's hue at the cell step, over the
  -- element's own ground.
  , "  #mdoc.on .dc.don{background:color-mix(in srgb, var(--g-col) var(--g-cell-wash), transparent)}"
  , "  .dc-tags{color:var(--g-mute);font-size:11px;margin-left:auto;margin-right:0}"
  -- What is not there yet: shown only under point, naming the part a reader
  -- stands on, and invisible elsewhere.  The open row's two fields lie over the
  -- row they belong to, ABSOLUTE because the row is virtualized — the mount
  -- rewrites its own rows as it scrolls, so an edit living inside one would be
  -- thrown away by the next frame — with `top' and `height' the glue's, off the
  -- row's own box.  The rhythm is the table's, so the fields land on the text
  -- they replace: `5px 12px' per cell is `.tv-table td' exactly, the column
  -- split the renderer's two columns.  No z-index: a positioned LATER sibling
  -- of the mount, so paint order puts it over the rows already and the page's
  -- four bands stay four.  The two popups' overlays are the same thing over a
  -- RANGE of cells, so all four share every declaration but geometry: `#dedit'
  -- spans its element, `#tedit' the tag cell alone, `#ledit' the title and url
  -- cells together, whose left and width the glue reads off those cells.  One
  -- edit vocabulary across the page means one look.
  , "  #dedit,#dpara,#pedit,#tedit,#ledit{display:none;position:absolute;"
  , "    background:var(--g-sel)}"
    -- THE GROUND IS THE ONLY SIGNAL, so it has to be one the block is not
    -- wearing.  `#dpara' opens over the DOCUMENT CURSOR's block, already
    -- `--g-sel', so the shared ground would say nothing; the page's input
    -- surface is what every other field sits on.  The three CELL overlays keep
    -- `--g-sel', opening over table rows where it reads as the edit it is.
  , "  #dpara{background:var(--g-surface)}"
  , "  #pedit{left:0;right:0}"
      -- THE DOCUMENT'S OVERLAYS SPAN THE PANE'S CONTENT BOX.  `left:0' is the
      -- PADDING box, so over a pane with a horizontal padding the box lands that
      -- far left of the text it covers and every line of the block jumps when
      -- the edit opens.  Read off the pane's own inset rather than respelled.
      -- `#pedit' keeps the bare zero: `#mprops' has no padding to answer for.
  , "  #dedit,#dpara{left:var(--g-doc-padx);right:var(--g-doc-padx)}"
  , "  #dedit.on,#pedit.on,#tedit.on,#ledit.on{display:flex;align-items:center}"
  , "  #dpara.on{display:flex}"
      -- The mount's own cell metrics, so the fields land on the text they
      -- replace: `.tv-table td' is `5px 12px' at the root's 13px/1.5, and a
      -- coarse pointer stretches the row rather than the padding.
  , "  #dedit input,#pedit input,#tedit input,#ledit input,#dpara textarea{"
  , "    font:13px/1.5 var(--dk-mono);"
  , "    padding:5px 12px;border:none;border-bottom:1px solid transparent;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
      -- IT IS THE BLOCK, WEARING A DIFFERENT GROUND.  `RET' over a paragraph
      -- puts the textarea where the paragraph was, so ENTERING THE EDIT MOVES
      -- NOTHING: the box takes the block's font, its line height, all four of
      -- its paddings — the grid inset and the title-column indent among them —
      -- its full width and no margin of its own, and it draws no border and no
      -- outline.  Only the GROUND and the caret change.  `font:inherit' rather
      -- than a figure, and the paddings are the block's own expressions: a
      -- literal `13px/1.5' here against the pane's `--g-doc-fs'/`--g-doc-lh' is
      -- what put every line after the first on a different rhythm, and a
      -- literal `6px' against `--g-doc-pad' is the same drift one axis over.
      -- Read the block's declarations, never copy them.
  , "  #dpara textarea{flex:1;resize:none;border:none;margin:0;font:inherit;"
  , "    width:100%;overflow-wrap:anywhere;padding:1px var(--g-doc-pad);"
  , "    padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}"
  , "  #dedit input:focus,#pedit input:focus,#tedit input:focus,"
  , "  #ledit input:focus{outline:none;border-bottom-color:var(--g-border)}"
      -- No line on focus either: a border under the box is a second signal, and
      -- a LINE is the one thing a document read as text must not grow.
  , "  #dpara textarea:focus{outline:none;border:none}"
  , "  #dedit input::selection,#pedit input::selection,#tedit input::selection,"
  , "  #ledit input::selection,#dpara textarea::selection{"
  , "    background:var(--g-sel);color:var(--g-fg)}"
  , "  #tname{flex:1 1 auto}"
  -- The link overlay's two fields split the width the two cells give them: the
  -- description reads as the row's name and the target is the longer string, so
  -- the split is the property panel's, key against value.
  , "  #ltitle{flex:1 1 40%}"
  , "  #lurl{flex:2 1 50%}"
  -- A cell's key is the column's name rather than the author's, so its field is
  -- muted and takes no typing — a label with a caret in it.
  , "  #dkey,#pkey{flex:1 1 40%}"
  , "  #dkey[readonly],#pkey[readonly]{color:var(--g-mute)}"
  , "  #dval,#pval{flex:2 1 50%}"
  -- The logbook: full width under both panes, muted, read-only and out of the
  -- tab order — it is the server's, and there is nothing here to press.
      -- The same dress as the page's log strip: 12px muted on the surface
      -- tint, one hairline, the shared 8px radius, 6px 10px padding.
  , "  #mlog{display:none;flex:0 0 auto;max-height:22vh;overflow:auto;margin:0;"
  , "    font-size:12px;font-family:var(--dk-mono);color:var(--g-mute);"
  , "    white-space:pre-wrap;padding:6px 10px;background:var(--g-surface);"
  , "    border:1px solid var(--g-border);border-radius:8px}"
  , "  #mlog.on{display:block}"
  , "  #sheet.raw #mlog{display:none}"
  -- The value palette and the link popup are ONE BOX: a heading naming what is
  -- being decided, a body, and a foot naming the keys the body cannot draw for
  -- itself.  Declared once so a restyle is one edit and a fifth overlay joins by
  -- adding a selector, which is what `#modal,#prompt,#config,#links' above does
  -- for the band.  What they no longer differ in is SIZE: that is a TIER now,
  -- worn as a class, and no box declares one of its own.
  , "  #pbox,#lbox,#tbox{display:flex;flex-direction:column;gap:6px;padding:10px;"
  , "    border-radius:6px;position:relative;z-index:101;"
  , "    font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #phead,#lhead,#thead{font-size:12px;color:var(--g-mute)}"
  , "  #pfoot,#lfoot,#tfoot,#cfoot,#ctplf{font-size:11px;color:var(--g-mute)}"
  , "  #pinput{font:12px/1.5 var(--dk-mono);padding:5px 7px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  , "  #pbox:not(.narrow) #pinput{display:none}"
  , "  #plist{max-height:40vh;overflow-y:auto;font-size:12px}"
  -- The resolution table: source, active, inactive.  A row is a source, the
  -- hairline between two being that row's own top border — the table's border
  -- language, where the old flat list needed a divider.  The source column is
  -- the muted small lowercase a tag wears elsewhere here, tag or reserved
  -- label; a long tag breaks rather than widening the box.  `*empty*' spans.
  , "  .pr{display:grid;grid-template-columns:6.5em 1fr 1fr;gap:4px 8px;padding:4px 7px}"
  , "  .pr+.pr{border-top:1px solid var(--g-border)}"
  , "  .ph,.ps{font-size:11px;color:var(--g-mute)}"
  , "  .ps{overflow-wrap:anywhere}"
  , "  .pc{display:flex;flex-wrap:wrap;gap:2px 10px}"
  , "  .pr.pm{grid-template-columns:1fr}"
  , "  .pnone{padding:4px 7px;color:var(--g-mute)}"
  -- An entry is its key token and its word: the token boxed in the accent so
  -- the letters read as a column, the word in its badge colour so a keyword
  -- looks the same as in the table, and the claimed letter marked INSIDE the
  -- word — bold and underlined in that state's badge hue — which says why
  -- DELEGATED answers to `e'.  There is no key-token column: an entry IS its
  -- keyword, and a boxed letter beside it said that twice while pushing every
  -- word rightwards.  An entry that claimed nothing is drawn bare and reachable
  -- through `/' alone, "unmarked means untyped" being learned from the marked
  -- ones beside it.  The padding is the FLAT list's, where an entry is a row of
  -- its own; inside a cell the gaps do it.
  , "  .pe{display:flex;align-items:center;gap:6px;border-radius:4px}"
  , "  #plist>.pe{padding:3px 7px}"
      -- The one entry that keeps a token: `*empty*' answers to DEL, which has
      -- no position inside a word to be marked at.
  , "  .pk{flex:none;min-width:1.6em;text-align:center;padding:1px 5px;border-radius:3px;"
  , "    font:11px/1.4 var(--dk-mono);"
  , "    border:1px solid var(--g-accent);color:var(--g-accent)}"
      -- Weight AND a rule under it in the keyword's badge hue (inline per
      -- entry, since only the entry knows it): two marks rather than one, the
      -- token column that carried the letter being gone and this the whole of
      -- what says which key commits.  Thick enough to read at 11px and offset
      -- clear of the descenders, what made a rule chrome the first time.
  , "  .pw b{font-weight:700;text-decoration:underline;"
  , "    text-decoration-thickness:2px;text-underline-offset:2px}"
  , "  .pm .pw{font-style:italic;color:var(--g-mute)}"
  -- The muted aside beside an entry, truncated: the tag palette's partial
  -- count, and the only thing that goes in it.  It is a note ABOUT the entry
  -- rather than part of it, so it recedes and gives the word the left edge.
  , "  .pt{flex:1 1 0;min-width:0;overflow:hidden;text-overflow:ellipsis;"
  , "    white-space:nowrap;text-align:right;font-size:11px;color:var(--g-mute)}"
  -- The fallback's cursor row wears the page's selection, which in the light
  -- theme is a bright yellow — a badge hue written inline reads badly on it,
  -- and this is the one place a declaration has to beat one.
  , "  #plist .pat{background:var(--g-sel);color:var(--g-fg)}"
  , "  #plist .pat .pw{color:var(--g-fg)!important}"
  -- The settings sheet, third in the same two bands: panels down a column, each
  -- a header over its rows, its top line the shared anchor's.
  , "  #cbox{display:flex;flex-direction:column;gap:10px;padding:14px;border-radius:6px;"
  , "    position:relative;z-index:101;overflow-y:auto;font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #cnote{text-align:right;color:var(--g-ok)}"
  , "  #cnote.syncing{color:var(--g-mute)}"
  , "  #cnote.conflict,#cnote.error{color:var(--g-bad)}"
  -- A panel: a header naming it, then a stack of rows.  The hairline is the
  -- header's own top border, the value palette's border language, and the first
  -- panel sits under the sheet's head so it wears none.  Panel bodies match by
  -- CLASS — a panel added to `SECTIONS' is laid out with no entry here.
  , "  #csecs{display:flex;flex-direction:column;gap:14px}"
  , "  .csec,.cpart{display:flex;flex-direction:column;gap:8px}"
  , "  .chdr{font-size:11px;letter-spacing:.08em;text-transform:uppercase;"
  , "    color:var(--g-mute);border-top:1px solid var(--g-border);padding-top:8px}"
  , "  .csec:first-child .chdr{border-top:none;padding-top:0}"
  , "  .crow{display:flex;flex-direction:column;gap:4px}"
  -- A path is long and has nowhere to wrap, so it is told it may break
  -- anywhere rather than widening the sheet past the viewport.
  , "  .clab{font-size:11px;color:var(--g-mute);overflow-wrap:anywhere}"
  -- Every control the sheet edits with, in ONE rule: the layer boxes, the two
  -- general fields and the theme select, which wears `cview' for exactly this.
  -- The coarse-pointer block names the same two classes, so a control that
  -- takes its look here takes its 16px there and iOS never zooms in on it.
  , "  .ctext,.cview{font:12px/1.5 var(--dk-mono);padding:6px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  -- ONE box, so it gets the room the stack used to share out: a `#+TODO:' block
  -- is a handful of lines and a reader editing one wants it whole.
  , "  .ctext{height:7em;resize:vertical}"
  , "  .ctext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- The two selects are the controls with a popup of their own, which has to
  -- paint on something, and the ones that would stretch to the sheet's width.
  , "  #themesel,#clayer{background:var(--g-bg);align-self:flex-start;"
  , "    max-width:100%;min-width:10em}"
  , "  #themesel option,#clayer option{background:var(--g-bg);color:var(--g-fg)}"
  -- What the server said about the last write to that layer, and nothing when
  -- it said nothing.
  , "  .cerr{font-size:11px;color:var(--g-bad)}"
  , "  .cerr:empty{display:none}"
  , "  #ceff{font-size:12px;padding-top:8px;border-top:1px solid var(--g-border)}"
  -- The link popup, fourth in the same two bands and sharing the palette's box
  -- above.  High rather than centred, for the palette's reason, and the table
  -- is capped and scrolls inside itself, since a row can point at a great many
  -- places.  The mount brings its own frame (`.tv-root''s hairline and radius),
  -- so the box declares none of the table's look past the sheet's monospace —
  -- the one rule `#mdoc' takes too.
      -- The tier gives the box its height, so the table inside it GROWS to what
      -- is left rather than carrying a cap of its own — a second size rule
      -- about one popup is the thing the tiers exist to have none of.
  , "  #ltable,#ttable{flex:1;min-height:0;display:flex;overflow:hidden}"
  -- The tags popup, fifth in the same two bands and box.  Each popup's PANE is
  -- its edit overlay's positioning parent — `#mdoc''s job in the sheet, for the
  -- same reason: the mount rewrites its rows as it scrolls, so an edit living
  -- inside one would be thrown away by the next frame.
  , "  #tpane,#lpane{flex:1;position:relative;min-height:0;display:flex;"
  , "    flex-direction:column;overflow:hidden}"
  -- POPUP SIZE IS A TIER, every popup wears exactly ONE, and no box declares a
  -- width or height of its own.  A size is a KIND OF SURFACE rather than a fact
  -- about one popup, so a new overlay names its kind and inherits the measure
  -- the others of its kind agree on.  Defined here, once, and worn in the
  -- markup — the whole of what a reviewer has to check, since a box that grew a
  -- size rule instead is what the sweep looks for.  A TIER NAMES AN INTENT and
  -- the number is its DEFINITION, so the measure can move without the name
  -- going stale.
  --
  --   `pop-band'   a list committed from memory, no wider than its entries
  --                need: the state palette, whose rows are single words.
  --   `pop-sheet'  a WORKING SURFACE, fixed on both axes: the materialize
  --                sheet, the link and tag popups, and the settings sheet.  A
  --                box that does not reflow under an edit is the point — the
  --                panes in it have a measure to lay out against, and a reader's
  --                eye keeps its place while the content under it changes.
  --
  -- `pop-wide' WAS THE THIRD, and it is gone: it grew with what it held between
  -- a floor and a ceiling, and fixing its height at the bound made its
  -- definition character for character `pop-sheet''s — two names for one box, a
  -- tier system saying something it does not mean.  The fixed tier gives
  -- outright what the growing one was reaching for, the ceiling being the same
  -- number, and that floor existed only because a sparse entry made a GROWING
  -- box a strip rather than a surface, which a fixed box cannot become.  A
  -- SURFACE THAT NEEDS A MEASURE NEITHER GIVES ADDS A TIER HERE and wears it,
  -- rather than declaring a width on its own box; that is the rule a third one
  -- arrives under, and there is no third today.
  , "  .pop-band{width:min(560px,100%);max-height:var(--g-pop-max)}"
  , "  .pop-sheet{width:min(80vw,100%);height:var(--g-pop-max)}"
  -- THE WASH.  One class on the document element, over the table and the whole
  -- modal band: a sheet open on rows that have gone stale is stale with them,
  -- so the overlays go under the same wash rather than floating over it looking
  -- current.  Eased, so the state arrives as a change rather than a flicker,
  -- and the transition sits on the elements themselves so coming back is eased
  -- too.  ONE PROPERTY, and it is `opacity'.  Never BLUR — a reader has to be
  -- able to read a stale row while its replacement is on the way.  Never
  -- `filter' either, which is the cross-repo constraint here: ANY filter makes
  -- its element the containing block for `position:fixed' descendants, and the
  -- renderer's summoned filter palette is one — a `.tv-veil' inside `#app',
  -- which would stop covering the viewport and start being clipped by
  -- `.tv-root''s own `overflow:hidden' the moment a fetch went past its grace.
  -- `opacity' creates a stacking context and no containing block, so it is the
  -- one declaration that dims everything and re-anchors nothing; over the
  -- page's own ground it takes the colour out of a badge as it goes.
  --
  -- The event strip and the key line are EXEMPT by omission: they are where a
  -- reader finds out what the page is waiting on — a lost socket is a `ws' line
  -- there and nowhere else now — and dimming the explanation with the thing it
  -- explains leaves the page saying nothing.
  , "  #app,#modal,#prompt,#config,#links,#tags{transition:opacity .18s ease}"
  , "  html.stale #app,html.stale #modal,html.stale #prompt,html.stale #config,"
  , "  html.stale #links,html.stale #tags{opacity:.55}"
  -- The echo area is the page's, and the backdrop dims the page: it sits under
  -- it (2 against the modal's 100) and greys out with everything else while the
  -- sheet is open.  It stays above the table.
  , "  #echo{position:fixed;right:14px;bottom:12px;z-index:2;padding:4px 10px;"
  , "    border-radius:999px;border:1px solid var(--g-border);font-size:12px;"
  , "    white-space:pre;background:var(--g-surface);color:var(--g-fg);opacity:0;"
  , "    transition:opacity .35s;pointer-events:none}"
  -- Touch.  Keyboard-first holds wherever there are keys; a coarse pointer is
  -- where they cannot reach, so the filter earns the one tap target on the
  -- page.  The chip row is what a palette-mode page carries of the filter, and
  -- it becomes that target: 44px of it, and a word saying so while no chip has
  -- filled it.  The renderer hides an empty row with an inline @display:none@,
  -- which only @!important@ outranks from a stylesheet.  Every rule is inside
  -- the query, so a mouse sees exactly what it saw before.  THE SETTINGS SHEET
  -- IS NOW UNREACHABLE HERE, a known gap rather than a decision this block
  -- records: the gear was the coarse pointer's only door to `,', and it went
  -- with the corner it sat in.  A touch reader can filter and read but cannot
  -- open the settings, and the page has no other affordance to offer.  Whatever
  -- answers it — a chord surface, a long press, a control inside the sheet band
  -- — is one place, and this is where the query for it lives.
  --
  -- iOS zooms the page in on a focused field under 16px and does not zoom back
  -- out; the sheet's textarea and its property fields are the shell's own, and
  -- the renderer's input is the renderer's.  The panes stack here whatever the
  -- width: a thumb wants the text full-width and the drawer under it, where the
  -- wrap rule would keep them side by side on a wide tablet.
  , "  @media (pointer:coarse){"
  , "    #app .tv-chips{min-height:44px;cursor:pointer}"
  , "    #app .tv-chips:empty{display:flex!important;align-items:center}"
  , "    #app .tv-chips:empty::after{content:\"filter …\";color:var(--g-mute);"
  , "      font-size:12px}"
  , "    #mpanes{flex-direction:column}"
  , "    #mtext,#pinput,#dedit input,#pedit input,#tedit input,#ledit input,"
  , "    #dpara textarea,"
  , "    .ctext,.cview{font-size:16px}}"
  , "</style>"
  -- The stored theme, applied before anything paints: a page that renders in
  -- the wrong one and corrects itself a frame later is a flash the selector
  -- exists to avoid.  One line, so the suite's glue extractor still finds the
  -- one inline script it checks.
  , "<script>" <> themeBoot <> "</script>"
  , "</head>"
  , "<body>"
  , body <> "</body>"
  , "</html>"
  ]

-- | The head script: the remembered theme pinned on the root element ahead of
-- the first paint.  @auto@ and anything unrecognised leave the attribute off,
-- which is the media query's business.
themeBoot :: Text
themeBoot = T.concat
  [ "try{var t=localStorage.getItem(\"glance-theme\");"
  , "if(t===\"light\"||t===\"dark\")document.documentElement.dataset.theme=t}"
  , "catch(e){}" ]
