-- | The document every served page wears; styles inline, the renderer its one asset.
module Glance.Web.Page.Style ( page
                             , fontAssets
                             , fontFace
                             ) where

import Glance.Web.Page.Popups (boxes, veiled, washed)
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
  , "    --g-doc-fs:13px;--g-doc-lh:1.6;"
  , "    --g-doc-off:calc(3 * var(--g-doc-fs) * var(--g-doc-lh));"
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
  , "  #mdoc{flex:2 1 320px;min-width:0;min-height:0;position:relative;"
  , "    overflow:auto;padding:var(--g-doc-pady) var(--g-doc-padx);"
    -- THE TREE HAS A COLUMN OF ITS OWN, and the text never enters it.
  , "    padding-left:calc(var(--g-doc-padx) + 1ch);"
  , "    font:var(--g-doc-fs)/var(--g-doc-lh) var(--dk-mono);"
  , "    border:1px solid var(--g-border);border-radius:8px}"
  , "  #sheet.raw #mdoc{display:none}"
  , "  #sheet:not(.raw) #mtext{display:none}"
  , "  #mprops.on .tv-root,#mdoc.on{border-color:var(--g-accent)}"
  -- Gating the renderer's `tr.tv-sel' costs TWO rules: the stripe goes back.
  , "  #mprops:not(.on) .tv-table tbody tr.tv-sel{background:transparent}"
  , "  #mprops:not(.on) .tv-table tbody tr.tv-sel.tv-alt{background:var(--tv-alt)}"
  , "  #mprops{flex:1 1 240px;min-width:0;min-height:0;position:relative;"
  , "    overflow:hidden;display:flex;flex-direction:column}"
  , "  #mptable{flex:1;min-height:0;display:flex}"
  , "  #sheet.raw #mprops{display:none}"
  , "  #mptable .tv-root,#ltable .tv-root,#ttable .tv-root{flex:1;min-width:0;"
  , "    font-family:var(--dk-mono)}"
  -- POINT IS A MARK BESIDE THE LINE: a nested item is drawn INSIDE its parent, so a
  -- ground would run the whole subtree.  The mark sits one tab stop left of the text.
  , "  .de{scroll-margin-block:var(--g-doc-off);position:relative;"
  , "    --ink:var(--g-point-off);"
  , "    padding:1px var(--g-doc-pad);border-radius:4px;white-space:pre-wrap;"
  , "    overflow-wrap:anywhere}"
  , "  .dp{position:relative}"
  , "  .de::before,.de::after{content:\"\";position:absolute;left:var(--rail);"
  , "    pointer-events:none}"
  , "  .de.dat{min-height:calc(var(--g-doc-rows, 0) * var(--g-doc-fs)"
  , "    * var(--g-doc-lh))}"
  -- ONE TAB STOP LEFT OF THE ROW'S TEXT: a top-level row is indented by PADDING and
  -- a nested one by SPACES.  Elm writes `--rail' per row; this is the fallback.
  , "  .de{--rail:calc(var(--g-doc-pad) + 0.5ch)}"
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
  -- A CONNECTOR PER ROW, the way `tree' draws one, HALF A CELL LEFT OF THE TAB STOP:
  -- one reaching the text reads as one dashed run with org's bullet.  Tiers set `--ink'.
  -- THE TURN SITS ON THE DASH'S OWN INK, not on the middle of the line box.  In Hack
  -- at 13px the hyphen's ink centres 1px below the half-line and the border's own
  -- half is another 0.5px, so the height carries both -- `0.115em'.
  , "  .d-list .d-item::before{top:0;width:0.8ch;background:none;"
  , "    height:calc(var(--g-doc-fs) * var(--g-doc-lh) / 2 + 0.115em);"
  , "    border-left:1px solid var(--ink);border-bottom:1px solid var(--ink);"
  , "    border-radius:0 0 0 3px}"
  , "  .d-list .d-item.kin::after{bottom:0;width:1px;border-radius:1px;"
  , "    top:calc(var(--g-doc-fs) * var(--g-doc-lh) / 2 + 0.115em);"
  , "    background:var(--ink)}"
  -- NOTHING TO ELBOW INTO, so a paragraph wears a bar; a COMPOSITE is drawn as its tree.
  , "  .lvl-top:not(.d-head):not(.d-comp)::before{top:0;bottom:0;width:1px;"
  , "    border:0;border-radius:1px;background:var(--ink)}"
  -- WHAT LIGHTS IS POINT'S OWNERS, flat: dimming the rest is what makes the path
  -- read, and a ramp said WHICH ancestor at the cost of saying THAT.
  -- A SIBLING'S OWN SUBTREE COMES WITH IT: the reader is choosing between BRANCHES,
  -- and a branch whose contents are dimmed is a branch they cannot weigh.
  , "  #mdoc.on .up,#mdoc.on .sib,#mdoc.on .sib .de{--ink:var(--g-fg)}"
  -- OWNERSHIP IS IN THE NESTING: a row drawn inside point is what point carries;
  -- a composite's own children are the roots it opens.
  , "  #mdoc.on .de.dat .de{--ink:var(--g-fg)}"
  , "  #mdoc.on .de.dat{--ink:var(--g-point)}"
  , "  #mdoc.on .de.dat.d-comp>.de{--ink:var(--g-point)}"
  -- FULL INK UNTIL THE READER GOES INTO A LIST.  `focus' rides the program's own
  -- root, so dimming is a MODE rather than the resting look; colour inherits, so the
  -- lit rows name themselves back out of it.
  , "  #mdoc.on .focus .de{color:var(--g-point-off)}"
  -- THE HEADLINE IS THE ROOT OF THE PATH: the way back runs headline, list, owner,
  -- point, so it keeps its ink whichever list the reader is standing in.
  , "  #mdoc.on .focus .de.dat,#mdoc.on .focus .de.dat .de,"
  -- A SIBLING IS THE CHOICE THE READER IS STANDING IN, so it is readable too.
  , "  #mdoc.on .focus .up,#mdoc.on .focus .sib,#mdoc.on .focus .sib .de,"
  , "  #mdoc.on .focus .d-head{color:var(--g-fg)}"
  , "  #mdoc.on .focus .de.dat .dg,#mdoc.on .focus .up .dg{color:var(--g-mute)}"
  -- A LINK CARRIES ITS OWN INK, which outranks what it inherits, so a dimmed line
  -- kept a lit link inside it until the link was named too.
  , "  #mdoc.on .focus .dl,#mdoc.on .focus .dbx.on{color:var(--g-point-off)}"
  -- `> .dp' OR AN ANCESTOR LIGHTS ITS WHOLE SUBTREE: rows nest, so `.up .dl' reaches
  -- every link under an owner of point, not the one on the owner's own line.  What
  -- point CARRIES is the one place the subtree is meant.
  , "  #mdoc.on .focus .de.dat>.dp .dl,#mdoc.on .focus .de.dat .de>.dp .dl,"
  , "  #mdoc.on .focus .up>.dp .dl,#mdoc.on .focus .sib>.dp .dl,"
  , "  #mdoc.on .focus .sib .de>.dp .dl,"
  , "  #mdoc.on .focus .d-head .dl{color:var(--g-link)}"
  , "  #mdoc.on .focus .de.dat>.dp .dbx.on,#mdoc.on .focus .up>.dp .dbx.on,"
  , "  #mdoc.on .focus .sib>.dp .dbx.on,#mdoc.on .focus .sib .de>.dp .dbx.on,"
  , "  #mdoc.on .focus .de.dat .de>.dp .dbx.on{color:var(--g-state-i0)}"
  -- A HEADLINE'S STARS SIT IN THE CONNECTOR'S COLUMN: they take the ink, none is
  -- drawn.  COLOUR ALONE -- a bolder marker sits taller than the line it opens, and
  -- the pane's business is which line, never which face.
  , "  #mdoc.on .de.dat.d-head::before{display:none}"
  , "  #mdoc.on .de.dat.d-head .ds,#mdoc.on .de.dat>.dp>.dm,"
  , "  #mdoc.on .de.dat.d-comp>.de>.dp>.dm{color:var(--g-point)}"
  , "  #mdoc.on .de.dat .de>.dp>.dm{color:var(--g-fg)}"
  -- SPELLED TWICE ON PURPOSE: a flag outranks point and keeps its mark after point has
  -- moved on, so the gated copy outweighs the heavier rule carrying what point holds.
  -- A FLAG TAKES THE BRANCH.  What hangs off a flagged row goes with it wherever the
  -- flag leads -- a delete takes the subtree, so the mark says so.
  , "  .de.dfl,.de.dfl .de{--ink:var(--g-bad)}"
  , "  #mdoc.on .de.dfl,#mdoc.on .de.dfl .de,"
  , "  #mdoc.on .de.dat .de.dfl,#mdoc.on .de.dat .de.dfl .de{--ink:var(--g-bad)}"
  , "  .de.dfl.d-head .ds,.de.dfl>.dp>.dm,"
  , "  .de.dfl .de>.dp>.dm{color:var(--g-bad)}"
  -- PADDING: a margin would take the selection wash off the left of the line.
  , "  .d-para,.d-comp{margin:.5em 0;"
  , "    padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}"
  , "  .d-comp,.d-comp .de{padding-top:0;padding-bottom:0}"
  -- The draft row holds nothing and `:empty' misses it — Elm emits a text node.
  , "  .d-draft{min-height:calc(var(--g-doc-fs) * var(--g-doc-lh))}"
  , "  .d-item{padding-left:0;padding-right:0}"
  , "  .dg{padding:0;white-space:pre-wrap;overflow-wrap:anywhere;color:var(--g-mute)}"
  , "  .dl{color:var(--g-link);text-decoration:underline}"
  -- A TICKED BOX WEARS THE DONE FACE: the first inactive slot is the hue an org
  -- keyword takes when it is settled, and a box is the same statement in one glyph.
  -- An EMPTY box wears the line's ink, since it says nothing yet.
  , "  .dbx.on{color:var(--g-state-i0)}"
  , "  .d-head,.d-child{display:flex;align-items:baseline}"
  , "  .d-child{color:var(--g-fg)}"
  , "  .d-head{font-weight:600}"
  , "  .ds{white-space:pre;color:var(--g-fg);font-weight:400;flex:none}"
  , "  .d-head .ds{width:calc(var(--g-doc-indent, 2) * 1ch)}"
  , "  .dc{margin-right:.6em;flex:none}"
  , "  .dc-title{flex:1 1 auto;min-width:0}"
  , "  .dc-tags{color:var(--g-mute);font-size:11px;margin-left:auto;margin-right:0}"
  , "  #dtitle,#dpara,#pedit,#sedit,#tedit,#ledit{display:none;position:absolute;"
  , "    background:var(--g-sel)}"
  , "  #dpara,#dtitle{background:var(--g-surface)}"
  , "  #dtitle{min-width:8em}"
  , "  #pedit,#sedit{left:0;right:0}"
  , "  #chues{position:relative}"
  , "  #cstates{overflow:auto;max-height:40vh}"
  -- Fallback only; `placeEdit' places from the ROW's own box.
  , "  #dpara{left:var(--g-doc-padx);right:var(--g-doc-padx)}"
  , "  #dtitle.on,#pedit.on,#sedit.on,#tedit.on,#ledit.on{display:flex;align-items:center}"
  , "  #dpara.on{display:flex}"
  , "  #pedit input,#sedit input,#tedit input,#ledit input{"
  , "    font:var(--g-edit-fs)/var(--g-edit-lh) var(--dk-mono);"
  , "    padding:5px 12px;border:none;border-bottom:1px solid transparent;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
  , "  #dtin{flex:1;font:inherit;padding:0;border:none;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
  -- Hide the scrollbar: one taking layout width wraps the field narrower than its row.
  , "  #dpara textarea{flex:1;resize:none;border:none;margin:0;font:inherit;"
  , "    background:transparent;color:var(--g-fg);min-width:0;"
  , "    width:100%;overflow-wrap:anywhere;padding:1px var(--g-doc-pad);"
  , "    scrollbar-width:none;"
  , "    padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}"
  , "  #dpara textarea::-webkit-scrollbar{width:0;height:0}"
  , "  #pedit input:focus,#sedit input:focus,#tedit input:focus,"
  , "  #ledit input:focus{outline:none;border-bottom-color:var(--g-border)}"
  , "  #dpara textarea:focus,#dtin:focus{outline:none;border:none}"
  , "  #dtin::selection,#pedit input::selection,#sedit input::selection,\n      #tedit input::selection,"
  , "  #ledit input::selection,#dpara textarea::selection{"
  , "    background:var(--g-sel);color:var(--g-fg)}"
  , "  #tname{flex:1 1 auto}"
  , "  #ltitle{flex:1 1 40%}"
  , "  #lurl{flex:2 1 50%}"
  , "  #pkey{flex:1 1 40%}"
  , "  #pkey[readonly]{color:var(--g-mute)}"
  , "  #pval{flex:2 1 50%}"
  , "  #mlog{display:none;flex:0 0 auto;max-height:22vh;overflow:auto;margin:0;"
  , "    font-size:12px;font-family:var(--dk-mono);color:var(--g-mute);"
  , "    white-space:pre-wrap;padding:6px 10px;background:var(--g-surface);"
  , "    border:1px solid var(--g-border);border-radius:8px}"
  , "  #mlog.on{display:block}"
  , "  #sheet.raw #mlog{display:none}"
  , "  #pbox,#lbox,#tbox,#kbox,#nbox{gap:6px;padding:10px}"
  , "  #phead,#lhead,#thead,#khead,#nhead{font-size:12px;color:var(--g-mute)}"
  , "  #pfoot,#lfoot,#tfoot,#cfoot,#ctplf,#kfoot,#nfoot{font-size:11px;color:var(--g-mute)}"
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
  , "    #mtext,#pinput,#dtin,#pedit input,#sedit input,#tedit input,#ledit input,"
  , "    #dpara textarea,#ktag,#kfields input,#ktext,"
  , "    .ctext,.cview{font-size:16px}}"
  , "</style>"
  -- One line, so the suite's glue extractor still finds the one inline script.
  , "<script>" <> themeBoot <> "</script>"
  , "</head>"
  , "<body>"
  , body <> "</body>"
  , "</html>"
  ]

-- | The head script: the remembered theme pinned before the first paint.
themeBoot :: Text
themeBoot = T.concat
  [ "try{var t=localStorage.getItem(\"glance-theme\");"
  , "if(", T.intercalate "||" [ "t===\"" <> name <> "\"" | name <- themeIds ]
  , ")document.documentElement.dataset.theme=t}"
  , "catch(e){}" ]

