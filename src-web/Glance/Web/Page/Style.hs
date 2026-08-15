-- | The document every served page wears; styles inline, the renderer its one asset.
module Glance.Web.Page.Style ( page
                             , fontAssets
                             , fontFace
                             ) where

import Data.Text (Text)
import System.FilePath (takeExtension)

import qualified Data.Text as T

import Glance.Web.Base (escape, logLinesDefault)
import Glance.Web.Theme (themeCSS, themeIds, themeOverrides)


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
  -- THE PALETTE IS NOT HERE ('Glance.Web.Theme'); what stays here is GEOMETRY.
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
  -- The renderer injects its own rule from a script and ties on specificity.
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
  -- The strip is one nowrap line and is WIDER THAN THE WINDOW AT EVERY SIZE, so
  -- `overflow-x:auto' drew a bar under it always — 15px of chrome under a hint
  -- nobody drags. It still scrolls; it draws nothing. Both spellings, as
  -- `#dpara textarea' below.
  , "  #kbd{flex:none;font-size:11px;color:var(--g-mute);white-space:nowrap;"
  , "    overflow-x:auto;scrollbar-width:none;padding:0 2px}"
  , "  #kbd::-webkit-scrollbar{width:0;height:0}"
  -- These two z-levels clear the renderer's sticky header (1) and list (5).
  , "  #modal,#prompt,#config,#links,#tags,#capture{--dk-mono:\"Hack\", var(--glance-mono);"
  , "    display:none;position:fixed;inset:0;z-index:100;background:var(--g-veil);"
  , "    padding:var(--g-pop-pad);padding-top:var(--g-pop-top);"
  , "    align-items:flex-start;justify-content:center}"
  , "  #modal.on,#prompt.on,#config.on,#links.on,#tags.on,#capture.on{display:flex}"
  , "  #sheet,#cbox,#pbox,#lbox,#tbox,#kbox{display:flex;flex-direction:column;"
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
  -- `overflow:hidden' bounds the flex LINE, or a tall subtree paints outside.
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
  -- EVERY SELECTION IS A GROUND: no underline, border or outline ('TestServe').
  , "  .de{scroll-margin-block:var(--g-doc-off);"
  , "    padding:1px var(--g-doc-pad);border-radius:4px;white-space:pre-wrap;"
  , "    overflow-wrap:anywhere}"
  , "  #mdoc.on .de.dat{background:var(--g-sel);color:var(--g-fg)}"
      -- AN EDIT IS INLINE: the block grows with the typing, in the PANE's own line box.
  , "  .de.dat{min-height:calc(var(--g-doc-rows, 0) * var(--g-doc-fs)"
  , "    * var(--g-doc-lh))}"
  -- The background is ONE SLOT and the cursor wins it, so a flag says it INSET.
  , "  .de.dfl{background:color-mix(in srgb, var(--g-bad) var(--g-flag-wash), transparent);"
  , "    box-shadow:inset 3px 0 0 var(--g-bad)}"
  -- PADDING: a margin would take the selection wash off the left of the line.
  , "  .d-para,.d-comp{margin:.5em 0;"
  , "    padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}"
  -- Zeroed: a leaf would spend the stop padding AGAIN inside its composite and drift.
  , "  .d-comp,.d-comp .de{padding-top:0;padding-bottom:0}"
  -- The draft row holds nothing and `:empty' misses it — Elm emits a text node.
  , "  .d-draft{min-height:calc(var(--g-doc-fs) * var(--g-doc-lh))}"
  , "  .d-item{padding-left:0;padding-right:0}"
  , "  .dg{padding:0;white-space:pre-wrap;overflow-wrap:anywhere;color:var(--g-mute)}"
  , "  .dl{color:var(--g-link);text-decoration:underline}"
  , "  .d-head,.d-child{display:flex;align-items:baseline}"
  , "  .d-child{color:var(--g-fg)}"
  , "  .d-head{font-weight:600}"
  , "  .ds{white-space:pre;color:var(--g-fg);font-weight:400;flex:none}"
  , "  .d-head .ds{width:calc(var(--g-doc-indent, 2) * 1ch)}"
  , "  .dc{margin-right:.6em;flex:none}"
  , "  .dc-title{flex:1 1 auto;min-width:0}"
  , "  #mdoc.on .dc.don{background:color-mix(in srgb, var(--g-col) var(--g-cell-wash), transparent)}"
  , "  .dc-tags{color:var(--g-mute);font-size:11px;margin-left:auto;margin-right:0}"
  -- Absolute, over the row: the mount rewrites its rows as it scrolls.
  , "  #dtitle,#dpara,#pedit,#sedit,#tedit,#ledit{display:none;position:absolute;"
  , "    background:var(--g-sel)}"
  , "  #dpara,#dtitle{background:var(--g-surface)}"
  , "  #dtitle{min-width:8em}"
  , "  #pedit,#sedit{left:0;right:0}"
  , "  #chues{position:relative}"
  , "  #cstates{overflow:auto;max-height:40vh}"
      -- The fallback for a page that measured nothing; `placeEdit' uses the ROW's box.
  , "  #dpara{left:var(--g-doc-padx);right:var(--g-doc-padx)}"
  , "  #dtitle.on,#pedit.on,#sedit.on,#tedit.on,#ledit.on{display:flex;align-items:center}"
  , "  #dpara.on{display:flex}"
  , "  #pedit input,#sedit input,#tedit input,#ledit input{"
  , "    font:var(--g-edit-fs)/var(--g-edit-lh) var(--dk-mono);"
  , "    padding:5px 12px;border:none;border-bottom:1px solid transparent;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
  , "  #dtin{flex:1;font:inherit;padding:0;border:none;"
  , "    background:transparent;color:var(--g-fg);min-width:0}"
      -- Naming the popup metrics here too tied specificity: this box inherits the PANE's.
  -- `scrollbar-width:none': a scrollbar taking layout width wraps the field
  -- narrower than the row under it, and a one-line row overflows by a thousandth.
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
  , "  #pbox,#lbox,#tbox,#kbox{gap:6px;padding:10px}"
  , "  #phead,#lhead,#thead,#khead{font-size:12px;color:var(--g-mute)}"
  , "  #pfoot,#lfoot,#tfoot,#cfoot,#ctplf,#kfoot{font-size:11px;color:var(--g-mute)}"
  , "  #pinput,#ktag,#kfields input{font:12px/1.5 var(--dk-mono);padding:5px 7px;border-radius:4px;"
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
    -- `subgrid': every row borrows the LIST's tracks, so header and values agree.
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
  , "  #themesel,#clayer{background:var(--g-bg);align-self:flex-start;"
  , "    max-width:100%;min-width:10em}"
  , "  #themesel option,#clayer option{background:var(--g-bg);color:var(--g-fg)}"
  , "  .cerr{font-size:11px;color:var(--g-bad)}"
  , "  .cerr:empty{display:none}"
  , "  #ceff{font-size:12px;padding-top:8px;border-top:1px solid var(--g-border)}"
  , "  #ltable,#ttable{flex:1;min-height:0;display:flex;overflow:hidden}"
  , "  #tpane,#lpane{flex:1;position:relative;min-height:0;display:flex;"
  , "    flex-direction:column;overflow:hidden}"
  -- BORDER-BOX, or the padding and border fall OUTSIDE the `--g-pop-max' cap.
  , "  .pop-band,.pop-sheet{box-sizing:border-box}"
  , "  .pop-band{width:min(560px,100%);max-height:var(--g-pop-max)}"
  , "  .pop-sheet{width:min(80vw,100%);height:var(--g-pop-max)}"
  -- `opacity' and never a filter: a filter contains the renderer's fixed backdrop.
  , "  #app,#modal,#prompt,#config,#links,#tags,#capture{transition:opacity .18s ease}"
  , "  html.stale #app,html.stale #modal,html.stale #prompt,html.stale #config,"
  , "  html.stale #links,html.stale #tags,html.stale #capture{opacity:.55}"
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

