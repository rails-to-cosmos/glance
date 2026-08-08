-- | THE PALETTE IS ONE SOURCE, and a theme is one file.
--
-- Every colour the page and the table it mounts are drawn in comes from a
-- 'Palette' in a theme module and an entry in 'themes'.  Nothing else spells a
-- hex; every rule reads a token through @var()@.
--
-- TWO NAMESPACES, ONE ROLE SET.  The page draws in @--g-*@ and the renderer in
-- @--tv-*@ — its own theming API.  A role both need is declared once and
-- emitted into both ('pageTokens', 'tableTokens').
--
-- WHOSE VALUE WINS: the renderer ships its palette at zero specificity
-- (@:where(.tv-root)@), so these rules override it whatever order the two
-- stylesheets land in — the renderer injects its own at mount time, after the
-- served page's.
--
-- A theme is PICKED by @data-theme@; absent, the media query chooses between
-- the first theme of each 'Mode'.  Adding one is a record plus a 'themes'
-- entry: the CSS, the boot script and the selector all read that list.
module Glance.Web.Theme
  ( module Glance.Web.Theme.Types
  , Theme (..)
  , themes
  , themeIds
  , defaultFor
  , themeCSS
  , themeOverrides
  ) where

import Data.List (find)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (prioritySlots, stateSlots)
import Glance.Web.Theme.Default (defaultDark, defaultLight)
import Glance.Web.Theme.Types (Mode (..), Palette (..))

-- | What @data-theme@ spells, what the settings sheet shows, which preference
-- it answers, and its colours.
data Theme = Theme
  { thId      :: !Text
  , thLabel   :: !Text
  , thMode    :: !Mode
  , thPalette :: !Palette
  }

-- | Every theme this build carries, in the order the sheet offers them.
themes :: [Theme]
themes =
  [ Theme "light" "light" Light defaultLight
  , Theme "dark"  "dark"  Dark  defaultDark
  ]

-- | The names @data-theme@ may spell — the boot script's test and the
-- selector's options beside @auto@.
themeIds :: [Text]
themeIds = map thId themes

-- | The theme MODE opens on with nothing pinned.  A build missing one fails
-- here rather than serving a page with no palette.
defaultFor :: Mode -> Theme
defaultFor mode = case find ((== mode) . thMode) themes of
  Just t  -> t
  Nothing -> error ("Glance.Web.Theme: no " <> show mode <> " theme")

-- | The palette as the PAGE spells it.  Layout tokens (@--g-doc-*@,
-- @--g-pop-*@) stay in the stylesheet: no theme moves geometry.
pageTokens :: Palette -> [(Text, Text)]
pageTokens p =
  [ ("--g-bg",        pBg p)
  , ("--g-fg",        pFg p)
  , ("--g-surface",   pSurface p)
  , ("--g-mute",      pMuted p)
  , ("--g-border",    pBorder p)
  , ("--g-accent",    pAccent p)
  , ("--g-sel",       pSelection p)
  , ("--g-link",      pLink p)
  , ("--g-col",       pColumn p)
  , ("--g-cell-wash", pCellWash p)
  , ("--g-ok",        pOk p)
  , ("--g-warn",      pWarn p)
  , ("--g-bad",       pBad p)
  , ("--g-veil",      pVeil p)
  , ("--g-shadow",    pShadow p)
  ]
 <> slots p "--g-state-a"   stateSlots    (pActive p)
 <> slots p "--g-state-i"   stateSlots    (pInactive p)
 <> slots p "--g-priority-" prioritySlots (pPriority p)

-- | N slot tokens under PREFIX, P's HUES cycled to fill them.
--
-- THE COUNT IS THE WIRE'S ('Glance.Query.stateSlots', 'prioritySlots') and the
-- same for every theme, so a slot the badges name is always declared.  A theme
-- spells as MANY OR AS FEW hues as it likes: fewer repeat, and more than the
-- wire names are never reached — a hue nothing can ask for.  An EMPTY list
-- falls back to the theme's own ink, which keeps a badge readable rather than
-- leaving its token undefined and its pill unpainted.
slots :: Palette -> Text -> Int -> [Text] -> [(Text, Text)]
slots p prefix n hues =
  [ (prefix <> T.pack (show i), cycle filled !! i) | i <- [0 .. n - 1] ]
  where filled = if null hues then [pFg p] else hues

-- | And as the RENDERER spells it.  Its FLAG is 'pBad': the archive flag and
-- an error are one red.
tableTokens :: Palette -> [(Text, Text)]
tableTokens p =
  [ ("--tv-bg",        pBg p)
  , ("--tv-fg",        pFg p)
  , ("--tv-alt",       pSurface p)
  , ("--tv-muted",     pMuted p)
  , ("--tv-border",    pBorder p)
  , ("--tv-accent",    pAccent p)
  , ("--tv-sel",       pSelection p)
  , ("--tv-hover",     pHover p)
  , ("--tv-link",      pLink p)
  , ("--tv-frost",     pFrost p)
  , ("--tv-flag",      pBad p)
  , ("--tv-col",       pColumn p)
  , ("--tv-veil",      pVeil p)
  , ("--tv-shadow",    pShadow p)
  , ("--tv-chip-wash", pChipWash p)
  , ("--tv-chip-edge", pChipEdge p)
  , ("--tv-mark-wash", pMarkWash p)
  , ("--tv-flag-wash", pFlagWash p)
  , ("--tv-col-wash",  pColWash p)
  , ("--tv-cell-wash", pCellWash p)
  , ("--tv-sort-wash", pSortWash p)
  , ("--tv-cols-wash", pColsWash p)
  ]

-- | Both namespaces, for the two system defaults and for every theme by name.
-- Folded over 'themes', so a theme is served without a rule spelled for it.
themeCSS :: Text
themeCSS = T.concat
  ( [ rules "  " ":root" ".tv-root" (defaultFor Light)
    , media (rules "    " ":root" ".tv-root" (defaultFor Dark))
    ]
 <> [ rules "  " (pinned t) (pinned t <> " .tv-root") t | t <- themes ]
  )
  where
    pinned t = ":root[data-theme=\"" <> thId t <> "\"]"
    media css = "  @media (prefers-color-scheme:dark){\n" <> css <> "  }\n"
    rules pad page table t = block pad page (pageTokens (thPalette t))
                          <> block pad table (tableTokens (thPalette t))

-- | A TREE'S OWN state colours, per theme, as the blocks that override the
-- slots — `Data.Org.Config.stateColorsOf`'s answer made CSS.
--
-- Emitted AFTER 'themeCSS' and at the same specificity, so a later rule wins;
-- and per REQUEST, since it comes off the config the store holds rather than
-- out of this build.  A theme name no build carries still emits its block: the
-- attribute is simply never stamped, so nothing reads it — which is the honest
-- answer for a config naming a theme this binary does not have.
themeOverrides :: [(Text, [(Text, Text)])] -> Text
themeOverrides settings = T.concat
  [ block "  " (":root[data-theme=\"" <> theme <> "\"]")
          [ ("--g-state-" <> value, hue) | (value, hue) <- pairs ]
  | (theme, pairs) <- settings, not (null pairs) ]

-- | SELECTOR carrying TOKENS at PAD, one to a line: a palette is read by eye.
block :: Text -> Text -> [(Text, Text)] -> Text
block pad selector tokens = T.concat
  ( [pad, selector, "{\n"]
 <> [ T.concat [pad, "  ", name, ":", value, ";\n"] | (name, value) <- tokens ]
 <> [pad, "}\n"] )
