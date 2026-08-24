-- | The document every served page wears; styles inline, the renderer its one asset.
module Glance.Web.Page.Style ( page
                             , fontAssets
                             , fontFace
                             ) where

import Glance.Web.Page.Popups (chromeBoxes, chromeFeet, chromeHeads, boxes, veiled, washed)
import Data.Text (Text)
import System.FilePath (takeExtension)

import qualified Data.Text as T

import Glance.Web.Base (escape, logLinesDefault)
import Glance.Web.Theme (themeCSS, themeIds, themeOverrides)


-- | @#a.on,#b.on,…@ — the same list, each wearing the class that shows it.
onEach :: Text -> Text
onEach = T.intercalate "," . map (<> ".on") . T.splitOn ","

-- | @html.stale #a,…@ — the wash names each surface under the stale root.
staleEach :: Text -> Text
staleEach = T.intercalate "," . map ("html.stale " <>) . T.splitOn ","

monoStack :: Text
monoStack = "\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"

fontAssets :: [FilePath]
fontAssets = ["JetBrainsMono-Regular.woff2", "JetBrainsMono-Regular.ttf"]

fontFace :: Maybe FilePath -> Text
fontFace Nothing     = ""
fontFace (Just name) = T.concat
  [ "  @font-face{font-family:\"JetBrains Mono\";font-display:swap;"
  , "src:url(\"", T.pack name, "\") format(\"", format, "\")}" ]
  where format | takeExtension name == ".woff2" = "woff2"
               | otherwise                      = "truetype"

-- | BODY wrapped in a document titled TITLE, with HEAD opening the style block.
page :: Text -> [(Text, [(Text, Text)])] -> Text -> Text -> Text
page head' colours title body = T.unlines
  [ "<!doctype html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>" <> escape title <> "</title>"
  , "<style>" <> (if T.null head' then "" else "\n" <> head')
  -- Colours live in 'Glance.Web.Theme'; this block is GEOMETRY.
  , "  :root{--glance-mono:" <> monoStack <> ";"
  , "    --g-doc-pad:6px;"
  , "    --g-doc-padx:10px;--g-doc-pady:8px;"
  -- A WHOLE NUMBER OF PIXELS PER LINE: a 1px hairline and a hinted glyph land on
  -- one device row only when the row itself starts on the grid, and 13 x 1.6 is
  -- 20.8.  The line height is a LENGTH for it, and every reader multiplies no more.
  , "    --g-doc-fs:13px;--g-doc-lh:21px;"
  , "    --g-doc-off:calc(3 * var(--g-doc-lh));"
  , "    --g-edit-fs:13px;--g-edit-lh:1.5;"
  , "    --g-pop-top:5vh;--g-pop-pad:24px;"
  , "    --g-pop-max:min(90vh,"
  , "      calc(100vh - 2 * var(--g-pop-top)))}"
  , T.stripEnd (themeCSS <> themeOverrides colours)
  , "  body{margin:0;font:14px/1.5 var(--glance-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);"
  , "    height:100vh;box-sizing:border-box;overflow:hidden;"
  , "    padding:24px;display:flex;flex-direction:column;gap:14px}"
  , "  h1{font-size:16px;margin:0}"
  , "  p{margin:0;max-width:70ch}"
  , "  code{font-size:12px;color:var(--g-mute)}"
  , "  #app{flex:1 1 auto;min-height:0}"
  , "  #app .tv-root{font-family:var(--glance-mono)}"
  , "  #app,#log{width:100%;box-sizing:border-box}"
  , "  #log{font-size:12px;color:var(--g-mute);padding:6px 10px;"
  , "    border:1px solid var(--g-border);border-radius:8px;"
  , "    --g-logn:" <> T.pack (show logLinesDefault) <> ";"
  , "    height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px);"
  , "    background:var(--g-surface);flex:none;overflow-y:auto}"
  , "  #log div>span{margin-right:6px}"
  -- Each width is its own longest WORD, which `TestServe' derives and asserts.
  , "  #log .lv,#log .lc{display:inline-block}"
  , "  #log .lv{width:5ch}"
  , "  #log .lc{width:6ch}"
  , "  #log .lt{opacity:.65}"
  , "  #log .lm{color:var(--g-fg)}"
  , "  #log .warn .lv{color:var(--g-warn)}"
  , "  #log .error .lv{color:var(--g-bad)}"
  , "  #kbd{flex:none;font-size:11px;color:var(--g-mute);white-space:nowrap;"
  , "    overflow-x:auto;scrollbar-width:none;padding:0 2px}"
  , "  #kbd::-webkit-scrollbar{width:0;height:0}"
  -- These two z-levels clear the renderer's sticky header (1) and list (5).
  , "  " <> veiled <> "{--dk-mono:\"Hack\", var(--glance-mono);"
  , "    display:none;position:fixed;inset:0;z-index:100;background:var(--g-veil);"
  , "    padding:var(--g-pop-pad);padding-top:var(--g-pop-top);"
  , "    align-items:flex-start;justify-content:center}"
  , "  " <> onEach veiled <> "{display:flex}"
  , "  " <> boxes <> "{display:flex;flex-direction:column;"
  , "    border-radius:6px;position:relative;z-index:101;"
  , "    font-family:var(--dk-mono);"
  , "    background:var(--g-bg);color:var(--g-fg);border:1px solid var(--g-border)}"
  , "  #sheet{gap:8px;padding:14px}"
  , "  #mhead,#chead{display:flex;justify-content:space-between;gap:12px;"
  , "    font-size:12px}"
  , "  #mfile,#ctitle{color:var(--g-mute)}"
  , "  #mwhere{display:flex;gap:5px;align-items:center;font-size:12px;"
  , "    white-space:nowrap;overflow-x:auto;flex:none}"
  -- Hand-copied from `.tv-chip'; nothing detects drift from the renderer.
  , "  .wc{display:inline-flex;align-items:center;flex:0 1 auto;min-width:0;"
  , "    padding:1px 8px;border-radius:999px;border:1px solid var(--g-border);"
  , "    color:var(--g-mute);background:transparent;"
  , "    overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
  , "  .wc.wat{color:var(--g-fg)}"
  , "  #mnote{text-align:right;color:var(--g-ok)}"
  , "  #mnote.syncing{color:var(--g-mute)}"
  , "  #mnote.conflict,#mnote.error{color:var(--g-bad)}"
  , "  #mpanes{flex:1;min-height:0;overflow:hidden;"
  , "    display:flex;flex-wrap:wrap;gap:10px}"
  -- NO FLOOR: a `min-height' on a flex child is a refusal to shrink.
  , "  #mtext{min-height:0;flex:2 1 320px;min-width:0;font:12px/1.5 var(--dk-mono);padding:8px;"
  , "    border-radius:8px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit;resize:none}"
  , "  #mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
  , "  #mtext:focus{outline:none;border-color:var(--g-accent)}"
  -- THE LIT MARK'S INK: a step short of the page's, so a bar leads the eye
  -- without competing with the words beside it.  Bars only; text stays full.
  , "  #mdoc{--g-mark:color-mix(in srgb, var(--g-fg) 68%, var(--g-bg));"
  , "    flex:2 1 320px;min-width:0;min-height:0;position:relative;"
  , "    overflow:auto;padding:var(--g-doc-pady) var(--g-doc-padx);"
    -- THE TREE HAS A COLUMN OF ITS OWN, and the text never enters it.
  , "    padding-left:calc(var(--g-doc-padx) + 1ch);"
  , "    font:var(--g-doc-fs)/var(--g-doc-lh) var(--dk-mono);"
  , "    border:1px solid var(--g-border);border-radius:8px}"
  , "  #sheet.raw #mdoc{display:none}"
  , "  #sheet:not(.raw) #mtext{display:none}"
  , "  #mdoc.on{border-color:var(--g-accent)}"
  , "  #ltable .tv-root,#ttable .tv-root{flex:1;min-width:0;"
  , "    font-family:var(--dk-mono)}"
  -- POINT IS A MARK BESIDE THE LINE: a nested item is drawn INSIDE its parent, so a
  -- ground would run the whole subtree.  The mark sits one tab stop left of the text.
  -- EVERY TOKEN INK RIDES THE ROW: `--fg' the stars and lit text, `--tok' the
  -- reserved words, `--punc' the colons, `--lnk' links, `--box' a ticked box.
  -- One dim rule and one lit rule move them all; no token is named twice.
  , "  .de{scroll-margin-block:var(--g-doc-off);position:relative;"
  , "    --ink:var(--g-point-off);--fg:var(--g-fg);--tok:var(--g-point);"
  , "    --punc:var(--g-mute);--lnk:var(--g-link);--box:var(--g-state-i0);"
  , "    padding:1px var(--g-doc-pad);border-radius:4px;white-space:pre-wrap;"
  , "    overflow-wrap:anywhere}"
  , "  .dp{position:relative}"
  , "  .de::before{content:\"\";position:absolute;left:var(--rail);"
  , "    pointer-events:none}"
  -- THE CURSOR IS THE GROUND THE TABLE'S CURSOR WEARS, `--g-sel' on both surfaces.
  -- A NESTED ROW IS DRAWN INSIDE ITS PARENT, so the ground would run the whole
  -- subtree; the page's own ground takes it back.  A COMPOSITE is the exception --
  -- the list itself is the stop, so its rows are what it grounds.
  , "  #mdoc.on .de.dat{background-color:var(--g-sel);color:var(--g-fg)}"
  , "  #mdoc.on .de.dat:not(.d-comp) .de{background-color:var(--g-bg)}"
  , "  .de.dat{min-height:calc(var(--g-doc-rows, 0) * var(--g-doc-lh))}"
  -- ONE TAB STOP LEFT OF THE ROW'S TEXT: a top-level row is indented by PADDING and
  -- a nested one by SPACES.  Elm writes `--rail' per row; this is the fallback.
  , "  .de{--rail:calc(var(--g-doc-pad) + 0.5ch)}"
  -- THE TAIL HIDES until the walk reaches it: an empty last line is noise
  -- until it is the door in use.  Reached, it keeps a LINE's height.
  , "  .de.d-tail{display:none}"
  , "  .de.d-tail.dat{display:block;min-height:var(--g-doc-lh)}"
  -- STICKY INSIDE THE SCROLLER, so the strip holds while the rows move under it; the
  -- negative margins reach the pane's edges.  It always names something, so no floor.
  , "  .dpath{position:sticky;z-index:2;font-size:11px;line-height:1.5;"
  , "    top:calc(-1 * var(--g-doc-pady));"
  , "    margin:calc(-1 * var(--g-doc-pady)) calc(-1 * var(--g-doc-padx)) 6px"
  , "      calc(-1 * (var(--g-doc-padx) + 1ch));"
  , "    padding:4px var(--g-doc-padx);color:var(--g-mute);"
  , "    background:var(--g-surface);border-bottom:1px solid var(--g-border);"
  , "    white-space:nowrap;overflow:hidden;text-overflow:ellipsis}"
  , "  .dsep{padding:0 5px;opacity:.6}"
  -- WHAT THE CONNECTORS SAY, said again: point in its own ink, the way back in the
  -- page's.  The strip is never dimmed -- it is the answer to where you are.
  , "  #mdoc.on .cr-0{color:var(--g-point);font-weight:600}"
  , "  #mdoc.on .cr-1,#mdoc.on .cr-2,#mdoc.on .cr-3{color:var(--g-fg)}"
  -- A LIST RUN WEARS A SPINE, the blocks' own grammar one storey down: every
  -- item bars its whole extent at its run's rail, siblings stack seamlessly
  -- (a composite's rows carry no margin), and a nested run adds its deeper
  -- column inside the parent item's.  Tiers set `--ink'; org's bullet paints.
  -- OVER THE SUBTREE'S GROUND: a dat item's nested rows win the page ground
  -- back, and an element's background paints over its parent's pseudo-edge --
  -- the run's bar lifts a level, the way the block spine and the flag do.
  , "  .d-list .d-item::before{top:0;bottom:0;width:1px;border-radius:1px;"
  , "    z-index:1;background:var(--ink)}"
  -- A BLOCK IS AN ELEMENT AND ITS SPINE IS ITS OWN `::before': one unbroken
  -- bar the block's whole height, margins and deeper blocks included, at the
  -- shelf's rail.
  , "  .blk{position:relative;--spine:var(--g-point-off)}"
  -- OVER THE GROUND: the rows are the spine's siblings' children and their
  -- backgrounds paint above the wrapper's edge, so the cursor's ground was
  -- erasing the bar beside it -- the spine lifts one stacking level instead.
  , "  .blk::before{content:\"\";position:absolute;left:var(--rail);"
  , "    top:0;bottom:0;width:1px;border-radius:1px;z-index:1;"
  , "    background:var(--spine);pointer-events:none}"
  -- F'S RAMP, the spike's winner: the block point is IN takes the page's ink,
  -- each enclosing block a step dimmer in the accent, the rest resting.  A
  -- FLAG OUTRANKS THE RAMP.  The `up'/`sib' tiers below stay FLAT — they light
  -- the list's connectors and the text, where a ramp cost more than it said.
  , "  .blk.sp-0{--spine:var(--g-mark)}"
  , "  .blk.sp-1{--spine:color-mix(in srgb, var(--g-accent) 67%, var(--g-bg))}"
  , "  .blk.sp-2{--spine:color-mix(in srgb, var(--g-accent) 45%, var(--g-bg))}"
  , "  .blk.sp-3{--spine:color-mix(in srgb, var(--g-accent) 25%, var(--g-bg))}"
  -- A HEADLINE DRAWS NO MARK: its stars sit in the connector's own column.
  , "  .d-head::before,.d-child::before{display:none}"
  -- A FLAG'S MARK RIDES THE SPINE: the flagged row's own stretch turns red,
  -- and a flagged headline reddens the whole block it carries.  ONE LEVEL
  -- OVER THE SPINE, which paints over row grounds and covered the flag too.
  , "  .lvl-top.dfl:not(.d-head):not(.d-child)::before{"
  , "    top:-7px;bottom:0;width:1px;z-index:2;"
  , "    border:0;border-radius:1px;background:var(--g-bad)}"
  , "  .de.dfl.d-child + .blk{--spine:var(--g-bad)}"
  -- A HEADLINE AT POINT LIGHTS ITS BLOCK ONE LEVEL DEEP: the block's spine
  -- and the shelf's own TOP runs light; a nested run keeps its resting bar,
  -- and a child headline's block keeps every bar.  THE CHILD COMBINATOR HOLDS
  -- THE LIGHT TO ONE SHELF: a descendant one lit lists through child blocks
  -- where paragraphs stayed resting, a depth the picture never claimed.
  -- AND SO DOES ANY STOP ON THE BLOCK'S OWN SHELF: point on the drawer, the
  -- planning line or a paragraph reads the same level-1 picture as point on
  -- the headline -- the shelf, not the row, is what is being stood on.
  , "  #mdoc.on .de.dat.d-head + .blk > .d-comp > .de,"
  , "  #mdoc.on .de.dat.d-child + .blk > .d-comp > .de,"
  , "  #mdoc.on .blk:has(> .de.dat) > .d-comp > .de{--ink:var(--g-mark)}"
  -- A SIBLING'S OWN SUBTREE COMES WITH IT: the reader is choosing between BRANCHES,
  -- and a branch whose contents are dimmed is a branch they cannot weigh.
  , "  #mdoc.on .up,#mdoc.on .sib,#mdoc.on .sib .de{--ink:var(--g-mark)}"
  -- AND THE RUNS RIDE THE RAMP: an enclosing run's bar steps down the accent
  -- by its distance out -- `up-0' the nearest -- and only point's own run
  -- (dat and sib segments) bars in the page's ink.
  , "  #mdoc.on .up.up-0{--ink:color-mix(in srgb, var(--g-accent) 67%, var(--g-bg))}"
  , "  #mdoc.on .up.up-1{--ink:color-mix(in srgb, var(--g-accent) 45%, var(--g-bg))}"
  , "  #mdoc.on .up.up-2{--ink:color-mix(in srgb, var(--g-accent) 25%, var(--g-bg))}"
  -- THE GROUND SAYS WHERE POINT IS; a bar never wears gold for it, and
  -- what point carries -- OWNERSHIP IS IN THE NESTING -- takes the same ink.
  , "  #mdoc.on .de.dat,#mdoc.on .de.dat .de{--ink:var(--g-mark)}"
  -- FULL INK UNTIL THE READER GOES INTO A BLOCK -- a list, a drawer, a
  -- child's contents.  `focus' rides the program's own root, so dimming is a
  -- MODE rather than the resting look.  ONE RULE MOVES EVERY INK: it matches
  -- each row DIRECTLY, so a dim cousin nested inside a lit owner still dims.
  , "  #mdoc.on .focus .de{color:var(--g-point-off);--fg:var(--g-point-off);"
  , "    --tok:var(--g-point-off);--punc:var(--g-point-off);"
  , "    --lnk:var(--g-point-off);--box:var(--g-point-off)}"
  -- THE PATH NAMES ITSELF BACK OUT: point, what it carries, its owners, the
  -- choice beside it, the root's line, and the block a lit headline stands
  -- over -- every ink at once, stars and tokens and links riding along.  A
  -- child headline off the chain is matched by neither list and stays dim.
  , "  #mdoc.on .focus .de.dat,#mdoc.on .focus .de.dat .de,"
  , "  #mdoc.on .focus .up,#mdoc.on .focus .sib,#mdoc.on .focus .sib .de,"
  , "  #mdoc.on .focus .d-head,"
  , "  #mdoc.on .focus .de.dat.d-head + .blk .de,"
  , "  #mdoc.on .focus .de.dat.d-child + .blk .de,"
  , "  #mdoc.on .focus .de.sib.d-child + .blk .de{"
  , "    color:var(--g-fg);--fg:var(--g-fg);--tok:var(--g-point);"
  , "    --punc:var(--g-mute);--lnk:var(--g-link);--box:var(--g-state-i0)}"
  -- A BARE DRAWER DIMS: a frame holding no pairs is furniture, and it drops
  -- to the ink nobody is looking at -- word and colons alike, whatever else
  -- the row wears.  The lit selectors are spelled again at their own weight,
  -- so a bare drawer inside a lit block still dims.
  , "  #mdoc.on .de.d-drawer.bare,"
  , "  #mdoc.on .focus .de.d-drawer.bare,"
  , "  #mdoc.on .focus .de.dat.d-head + .blk .de.d-drawer.bare,"
  , "  #mdoc.on .focus .de.dat.d-child + .blk .de.d-drawer.bare,"
  , "  #mdoc.on .focus .de.sib.d-child + .blk .de.d-drawer.bare{"
  , "    --tok:var(--g-point-off);--punc:var(--g-point-off)}"
  -- AND SO DOES ITS BAR: a pair's gutter bar rides the block's spine ink,
  -- which stays lit while the drawer itself dims -- off the path it drops
  -- with its frame, the flag's red still outranking.
  , "  #mdoc.on .focus .d-drawer:not(.up):not(.sib):not(.dat)"
  , "    .d-meta:not(.dfl)::before{background:var(--g-point-off)}"
  -- WHAT STANDS ON THE GROUND TAKES THE PAGE'S INK: the ground says which line, and a
  -- marker in point's own hue vanished into it -- gold on gold, and the ordinals with
  -- it.  THE SPINE KEEPS ITS HUE, its column being outside the ground.
  , "  #mdoc.on .de.dat>.dp>.dm,#mdoc.on .de.dat .de>.dp>.dm{color:var(--g-fg)}"
  -- A FLAG TAKES THE BRANCH.  What hangs off a flagged row goes with it wherever
  -- the flag leads -- a delete takes the subtree, so the mark says so; the second
  -- selector outweighs the rule carrying what point holds.
  , "  #mdoc.on .de.dfl,#mdoc.on .de.dfl .de,"
  , "  #mdoc.on .de.dat .de.dfl,#mdoc.on .de.dat .de.dfl .de{--ink:var(--g-bad)}"
  , "  .de.dfl.d-head .ds,.de.dfl.d-child .ds,.de.dfl>.dp>.dm,"
  , "  .de.dfl .de>.dp>.dm{color:var(--g-bad)}"
  -- PADDING: a margin would take the selection wash off the left of the line.
  -- A PAIR IS NOT NESTED: the drawer's lines read as paragraphs, flush under the
  -- frame, wearing the paragraph's own thin bar IN THE GUTTER, left of the flush
  -- text -- the bar spends `--ink', so a flag reddens a pair marked for the drop.
  -- AT THE RAIL'S OWN X: the composite's indent is `rail + 1.5ch' at every
  -- shelf, so the pair's bar continues the drawer's line rather than a second one.
  -- IN THE SPINE'S OWN INK: the block's bar runs behind the drawer, and a pair
  -- segment in another tier read as a break in it.  A flag reddens its stretch.
  , "  .d-drawer .d-meta::before{top:0;bottom:0;left:-1.5ch;width:1px;border:0;"
  , "    border-radius:1px;background:var(--spine)}"
  , "  .d-drawer .d-meta.dfl::before{background:var(--g-bad)}"
  -- THE DRAWER IS A RESERVED TOKEN, frame and keys alike, in point's own ink;
  -- ITS COLONS ARE PUNCTUATION -- dimmed, the leading one HANGING into the
  -- gutter so the token lines up on its letter.
  , "  .d-drawer .dg,.dk{color:var(--tok)}"
  -- ONLY A LEADING COLON HANGS: `:first-child' counts elements, so a trailing
  -- colon after a text node matched it and overlapped the key's last letter.
  , "  .dpunc{color:var(--punc)}"
  , "  .dpunc.dlead{margin-left:-1ch}"
  , "  .d-para,.d-comp,.d-meta{margin:7px 0;"
  , "    padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}"
  , "  .d-comp,.d-comp .de{padding-top:0;padding-bottom:0}"
  -- The draft row holds nothing and `:empty' misses it — Elm emits a text node.
  , "  .d-draft{min-height:var(--g-doc-lh)}"
  , "  .d-item,.d-drawer .d-meta{padding-left:0;padding-right:0}"
  , "  .dg{padding:0;white-space:pre-wrap;overflow-wrap:anywhere;color:var(--punc)}"
  , "  .dl{color:var(--lnk);text-decoration:underline}"
  -- A TICKED BOX WEARS THE DONE FACE: the first inactive slot is the hue an org
  -- keyword takes when it is settled, and a box is the same statement in one glyph.
  -- An EMPTY box wears the line's ink, since it says nothing yet.
  , "  .dbx.on{color:var(--box)}"
  -- A CHILD HEADLINE IS A HEADLINE: the same face as the sheet's own, indented.
  , "  .d-head,.d-child{display:flex;align-items:baseline;font-weight:600}"
  , "  .ds{white-space:pre;color:var(--fg);font-weight:400;flex:none}"
  , "  .d-head .ds{width:calc(var(--g-doc-indent, 2) * 1ch)}"
  , "  .dc{margin-right:.6em;flex:none}"
  , "  .dc-title{flex:1 1 auto;min-width:0}"
  , "  .dc-tags{color:var(--punc);font-size:11px;margin-left:auto;margin-right:0}"
  , "  #dtitle,#dpara,#dpair,#ddate,#sedit,#tedit,#ledit{display:none;"
      <> "position:absolute;background:var(--g-sel)}"
  , "  #dpara,#dpair,#ddate,#dtitle{background:var(--g-surface)}"
  , "  #dtitle{min-width:8em}"
  , "  #sedit{left:0;right:0}"
  , "  #chues{position:relative}"
  , "  #cstates{overflow:auto;max-height:40vh}"
  -- Fallback only; `placeEdit' places from the ROW's own box.
  , "  #dpara,#dpair{left:var(--g-doc-padx);right:var(--g-doc-padx)}"
  , "  #dtitle.on,#dpair.on,#ddate.on,#sedit.on,#tedit.on,#ledit.on{"
      <> "display:flex;align-items:center}"
  , "  #dpara.on{display:flex}"
  -- THE TWO GOLDS, AND WHY ONE OF THEM HAS TO GO WHILE A TIGHT BOX STANDS.
  -- `--g-sel' is spent on both the cursor row's own wash (`:154') and every
  -- field's TEXT SELECTION (below): a box standing INSIDE its row over one slot
  -- would select its entry in the gold already behind it -- SET, FOCUSED AND
  -- INVISIBLE.  The box's own ground says where the edit is, so the row lifts
  -- its.  THE SHAPE DECLARES ITSELF TIGHT (`openEdit' stamps the pane), so this
  -- names no one box.
  , "  #mdoc.on.tight .de.dat{background-color:transparent}"
  -- THE PAIR WEARS THE DRAWER'S OWN COLONS, since it is drawn where the pair
  -- will stand; `--punc' is a ROW's variable and no row holds this box.
  , "  #dpair{--punc:var(--g-mute)}"
  -- THE OFFERS HANG UNDER THE FIELD, out of the box's own flex row and over the
  -- rows beneath: small and mute, since what is being read is the field.
  -- NO STACKING NUMBER: the box is positioned and stands after `#dlist', so it
  -- paints over the rows already — and the page's numbers are the backdrop's.
  , "  #doffer,#dwoffer{display:none;position:absolute;top:100%;left:0;"
  , "    min-width:12em;max-height:calc(6 * var(--g-doc-lh));overflow-y:auto;"
  , "    background:var(--g-surface);border:1px solid var(--g-border);"
  , "    border-radius:4px;font-size:11px}"
  , "  #doffer.on,#dwoffer.on{display:block}"
  , "  .dof{display:flex;align-items:baseline;gap:8px;"
  , "    padding:1px var(--g-doc-pad);color:var(--g-mute);white-space:pre}"
  , "  .dof.dat{background:var(--g-sel);color:var(--g-fg)}"
  -- THE HINT NAMES WHERE THE OFFER LANDS, off to the right and mute under the
  -- selection too: what is being read is the word.
  , "  .dot{flex:1 1 0;text-align:right;font-size:10px;color:var(--g-mute)}"
  , "  #sedit input,#tedit input,#ledit input{"
  , "    font:var(--g-edit-fs)/var(--g-edit-lh) var(--dk-mono);"
  , "    padding:5px 12px;border:none;border-bottom:1px solid transparent;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
  -- ONE DRESS FOR THE PANE'S OWN FIELDS: the document's face, since each one is
  -- laid over the row it writes.  The FLEX is each field's own.
  , "  #dtin,#dpair input,#ddate input{font:inherit;padding:0;border:none;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
  , "  #dtin{flex:1}"
  -- NO WIDTH RULE HERE: a ghosted field is exactly as wide as what it holds, and
  -- `drawGhost' writes the `ch' width AND takes the `flex' off per keystroke, so
  -- a rule here would only restate what an inline style already wins.
  , "  #dwhen::placeholder{color:var(--g-mute);opacity:1}"
  -- THE GHOST: the resolution riding after what was typed, on the field's own
  -- line -- mute where it resolves, the refusal's own ink where the grammar
  -- refuses outright.  A SPAN AND NEVER THE FIELD'S VALUE: nothing selects it.
  , "  .dgh{flex:none;white-space:pre;color:var(--g-mute);"
  , "    user-select:none;pointer-events:none}"
  , "  .dgh.bad{color:var(--g-bad)}"
  -- THE VALUE'S OWN SLOT, which the widget is laid over: an inline-BLOCK, so its
  -- box is the pane's own line whatever it holds, and a zero-width space where a
  -- summoned keyword has no value yet -- an empty INLINE box has no height for
  -- `placeEdit' to measure.
  , "  .dpv{display:inline-block;line-height:var(--g-doc-lh)}"
  , "  .dpv:empty::before{content:\"\\200B\"}"
  -- THE ENTRY AT POINT IS THE CURSOR, one grain finer than the row: it wears the
  -- same `--g-sel' ground the row wears (`:154') and the line lifts its while one
  -- does -- "THE TWO GOLDS" above, one grain finer, since gold inside gold is a
  -- selection nobody can see.  WHAT LIGHTS IS THE VALUE: the keyword beside it is
  -- the line's own token, not a stop the walk makes.
  , "  #mdoc.on .de.dat:has(.dpv.dat){background-color:transparent}"
  , "  #mdoc.on .dpv.dat{background-color:var(--g-sel);color:var(--g-fg)}"
  -- THE KEY IS EXACTLY AS WIDE AS WHAT IT HOLDS, so the closing colon stands
  -- flush against the text the way it does on the drawer line the box is laid
  -- over.  `pairMoved' writes the `ch' width per keystroke -- monospace does the
  -- arithmetic -- and the `1ch' here is the floor an empty key keeps for its
  -- caret.  The value takes the rest, and the margin is the SPACE org writes
  -- after the closing colon.
  , "  #dkey{flex:0 0 auto;width:1ch}"
  , "  #dval{flex:2 1 60%;margin-left:1ch}"
  -- Hide the scrollbar: one taking layout width wraps the field narrower than its row.
  , "  #dpara textarea{flex:1;resize:none;border:none;margin:0;font:inherit;"
  , "    background:transparent;color:var(--g-fg);min-width:0;"
  , "    width:100%;overflow-wrap:anywhere;padding:1px var(--g-doc-pad);"
  , "    scrollbar-width:none;"
  , "    padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}"
  , "  #dpara textarea::-webkit-scrollbar{width:0;height:0}"
  , "  #sedit input:focus,#tedit input:focus,"
  , "  #ledit input:focus{outline:none;border-bottom-color:var(--g-border)}"
  , "  #dpara textarea:focus,#dtin:focus,#dpair input:focus,"
      <> "#ddate input:focus{outline:none;border:none}"
  , "  #dtin::selection,#dpair input::selection,#ddate input::selection,"
  , "  #sedit input::selection,#tedit input::selection,"
  , "  #ledit input::selection,#dpara textarea::selection{"
  , "    background:var(--g-sel);color:var(--g-fg)}"
  , "  #tname{flex:1 1 auto}"
  , "  #ltitle{flex:1 1 40%}"
  , "  #lurl{flex:2 1 50%}"
  , "  #mlog{display:none;flex:0 0 auto;max-height:22vh;overflow:auto;margin:0;"
  , "    font-size:12px;font-family:var(--dk-mono);color:var(--g-mute);"
  , "    white-space:pre-wrap;padding:6px 10px;background:var(--g-surface);"
  , "    border:1px solid var(--g-border);border-radius:8px}"
  , "  #mlog.on{display:block}"
  , "  #sheet.raw #mlog{display:none}"
  , "  " <> chromeBoxes <> "{gap:6px;padding:10px}"
  , "  " <> chromeHeads <> "{font-size:12px;color:var(--g-mute)}"
    -- `#ctplf' is the capture-template hint inside config, and no popup's part.
  , "  " <> chromeFeet <> ",#ctplf{font-size:11px;color:var(--g-mute)}"
  , "  #pinput,#ktag,#kfields input,#nbox .krow input,#nbox .krow select{"
  , "    font:12px/1.5 var(--dk-mono);padding:5px 7px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  , "  #pbox:not(.narrow) #pinput{display:none}"
  , "  #plist{max-height:40vh;overflow-y:auto;font-size:12px}"
  , "  #klist{overflow-y:auto;font-size:12px;flex:0 1 auto}"
  , "  .ke{padding:2px 7px}"
  , "  .ke.kh{background:var(--g-sel)}"
  , "  .krow{display:flex;gap:8px;align-items:center;margin-top:4px}"
  , "  .klab{font-size:11px;color:var(--g-mute);min-width:8em}"
  , "  .krow input{flex:1 1 auto}"
  , "  #ktext{flex:1 1 auto;min-height:0;font:12px/1.5 var(--dk-mono);"
  , "    padding:5px 7px;border-radius:4px;border:1px solid var(--g-border);"
  , "    background:transparent;color:inherit;resize:none}"
  , "  #plist.ptable{display:grid;"
  , "    grid-template-columns:6.5em minmax(0,1fr) minmax(0,1fr)}"
  , "  .ptable>.pr{display:grid;grid-template-columns:subgrid;grid-column:1/-1}"
  , "  .pr{display:grid;grid-template-columns:6.5em minmax(0,1fr) minmax(0,1fr);"
  , "    gap:4px 8px;padding:4px 7px}"
  , "  .pr+.pr{border-top:1px solid var(--g-border)}"
  , "  .ph,.ps{font-size:11px;color:var(--g-mute)}"
  , "  .ps{overflow-wrap:anywhere}"
  , "  .pc{display:flex;flex-wrap:wrap;gap:2px 10px}"
  , "  .pr.pm{grid-template-columns:1fr}"
  , "  .ptable>.pr.pm>*{grid-column:1/-1}"
  , "  .pnone{padding:4px 7px;color:var(--g-mute)}"
  , "  .pe{display:flex;align-items:center;gap:6px;border-radius:4px}"
  , "  #plist>.pe{padding:3px 7px}"
  , "  .pk{flex:none;min-width:1.6em;text-align:center;padding:1px 5px;border-radius:3px;"
  , "    font:11px/1.4 var(--dk-mono);"
  , "    border:1px solid var(--g-accent);color:var(--g-accent)}"
  , "  .pw b{font-weight:700;text-decoration:underline;"
  , "    text-decoration-thickness:2px;text-underline-offset:2px}"
  , "  .pm .pw{font-style:italic;color:var(--g-mute)}"
  , "  .pt{flex:1 1 0;min-width:0;overflow:hidden;text-overflow:ellipsis;"
  , "    white-space:nowrap;text-align:right;font-size:11px;color:var(--g-mute)}"
  , "  #plist .pat{background:var(--g-sel);color:var(--g-fg)}"
  , "  #plist .pat .pw{color:var(--g-fg)!important}"
  , "  #cbox{gap:10px;padding:14px;overflow-y:auto}"
  , "  #cnote{text-align:right;color:var(--g-ok)}"
  , "  #cnote.syncing{color:var(--g-mute)}"
  , "  #cnote.conflict,#cnote.error{color:var(--g-bad)}"
  -- Panel bodies match by CLASS; a panel added to `SECTIONS' needs none here.
  , "  #csecs{display:flex;flex-direction:column;gap:14px}"
  , "  #ctabs{display:flex;gap:4px;border-bottom:1px solid var(--g-border);"
      <> "margin-bottom:4px}"
  , "  .ctab{font:11px/1.5 var(--dk-mono);letter-spacing:.08em;"
      <> "text-transform:uppercase;background:none;border:none;cursor:pointer;"
      <> "color:var(--g-mute);padding:6px 10px;border-bottom:2px solid transparent}"
  , "  .ctab.on{color:var(--g-fg);border-bottom-color:var(--g-accent)}"
  , "  .csec{display:none}"
  , "  .csec.on,.cpart{display:flex;flex-direction:column;gap:8px}"
  , "  .crow{display:flex;flex-direction:column;gap:4px}"
  , "  .clab{font-size:11px;color:var(--g-mute);overflow-wrap:anywhere}"
  , "  .ctext,.cview{font:12px/1.5 var(--dk-mono);padding:6px;border-radius:4px;"
  , "    border:1px solid var(--g-border);background:transparent;color:inherit}"
  , "  .ctext{height:7em;resize:vertical}"
  , "  .ctext::selection{background:var(--g-sel);color:var(--g-fg)}"
  -- Every <select> says its ground; a transparent one keeps the UA's control paint.
  , "  #themesel,#clayer,#nspace,#ngroup{background:var(--g-bg);align-self:flex-start;"
  , "    max-width:100%;min-width:10em}"
  , "  #themesel option,#clayer option,#nspace option,#ngroup option{"
  , "    background:var(--g-bg);color:var(--g-fg)}"
  , "  .cerr{font-size:11px;color:var(--g-bad)}"
  , "  .cerr:empty{display:none}"
  , "  #ceff{font-size:12px;padding-top:8px;border-top:1px solid var(--g-border)}"
  , "  #ltable,#ttable{flex:1;min-height:0;display:flex;overflow:hidden}"
  , "  #tpane,#lpane{flex:1;position:relative;min-height:0;display:flex;"
  , "    flex-direction:column;overflow:hidden}"
  , "  #refer{display:none;position:fixed;inset:0;z-index:102;pointer-events:none;"
  , "    --dk-mono:\"Hack\", var(--glance-mono)}"
  , "  #refer.on{display:block}"
  , "  #rbox{position:fixed;pointer-events:auto;box-sizing:border-box;"
  , "    width:min(1020px,calc(100vw - 24px));"
  , "    box-shadow:0 4px 14px var(--g-shadow);overflow:hidden}"
  , "  #rkind{display:none;align-self:flex-start;margin:4px 8px 0;"
  , "    padding:0 7px;border-radius:999px;font-size:10px;"
  , "    border:1px dashed var(--g-accent);color:var(--g-accent);"
  , "    background:transparent}"
  , "  #rkind.on{display:block}"
  , "  #rfoot{border-top:1px solid var(--g-border);padding:2px 8px;"
  , "    font-size:10px;color:var(--g-mute)}"
  , "  .pop-band,.pop-sheet{box-sizing:border-box}"
  , "  .pop-band{width:min(560px,100%);max-height:var(--g-pop-max)}"
  , "  .pop-sheet{width:min(80vw,100%);height:var(--g-pop-max)}"
  -- `opacity' and never a filter: a filter contains the renderer's fixed backdrop.
  , "  #app," <> washed <> "{transition:opacity .18s ease}"
  , "  html.stale #app," <> staleEach washed <> "{opacity:.55}"
  , "  #echo{position:fixed;right:14px;bottom:12px;z-index:2;padding:4px 10px;"
  , "    border-radius:999px;border:1px solid var(--g-border);font-size:12px;"
  , "    white-space:pre;background:var(--g-surface);color:var(--g-fg);opacity:0;"
  , "    transition:opacity .35s;pointer-events:none}"
  -- Every touch rule is in this query; iOS zooms a field under 16px and stays there.
  , "  @media (pointer:coarse){"
  , "    #app .tv-chips{min-height:44px;cursor:pointer}"
  , "    #app .tv-pin{font-size:20px;padding:8px}"
  , "    #app .tv-chips:empty{display:flex!important;align-items:center}"
  , "    #app .tv-chips:empty::after{content:\"filter …\";color:var(--g-mute);"
  , "      font-size:12px}"
  , "    #mpanes{flex-direction:column}"
  , "    #mtext,#pinput,#dtin,#dpair input,#ddate input,"
  , "    #sedit input,#tedit input,#ledit input,"
  , "    #dpara textarea,#ktag,#kfields input,#ktext,#app .tv-filter,"
  , "    .ctext,.cview{font-size:16px}}"
  , "</style>"
  -- One line, so the suite's glue extractor still finds the one inline script.
  , "<script>" <> themeBoot <> "</script>"
  , "</head>"
  , "<body>"
  , body <> "</body>"
  , "</html>"
  ]

-- | The head script: the remembered LOOK pinned before the first paint -- the
--   theme.  A value the page does not know is ignored, so the default look
--   survives a hand-edited store.
themeBoot :: Text
themeBoot = T.concat
  [ "try{", stamp "theme" "glance-theme" themeIds, "}catch(e){}" ]
  where
    stamp prop key ids = T.concat
      [ "var v=localStorage.getItem(\"", key, "\");"
      , "if(", T.intercalate "||" [ "v===\"" <> name <> "\"" | name <- ids ]
      , ")document.documentElement.dataset.", prop, "=v;" ]

