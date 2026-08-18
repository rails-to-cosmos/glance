-- | THE PALETTE IS ONE SOURCE — each 'Palette' role emitted into both the page's
-- @--g-*@ and the renderer's @--tv-*@.  Precedence and adding one: AGENTS.hs.
module Glance.Web.Theme
  ( module Glance.Web.Theme.Types
  , Theme (..)
  , themes
  , themeIds
  , bulletsKey
  , bulletsShown
  , themeCSS
  , themeOverrides
  ) where

import Data.List (find)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (prioritySlots, stateSlots)
import Glance.Web.Theme.Default (defaultDark, defaultLight)
import Glance.Web.Theme.Types (Mode (..), Palette (..))

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

-- | The names @data-theme@ may spell: the boot script's test and the selector.
themeIds :: [Text]
themeIds = map thId themes

-- | THE SECOND LOOK THE PAGE REMEMBERS: whether the doc pane draws org's own
--   unordered bullets.  HIDDEN is the default and is the ATTRIBUTE'S ABSENCE, the way
--   @auto@ is for the theme, so a page with no stored choice paints before any script.
bulletsKey :: Text
bulletsKey = "glance-bullets"

-- | The one value @data-bullets@ may spell, named once for the boot script's test and
--   for the selector that spends it.
bulletsShown :: Text
bulletsShown = "shown"

defaultFor :: Mode -> Theme
defaultFor mode = case find ((== mode) . thMode) themes of
  Just t  -> t
  Nothing -> error ("Glance.Web.Theme: no " <> show mode <> " theme")

-- | Geometry tokens (@--g-doc-*@, @--g-pop-*@) stay in the stylesheet.
pageTokens :: Palette -> [(Text, Text)]
pageTokens p =
  [ ("--g-bg",        pBg p)
  , ("--g-fg",        pFg p)
  , ("--g-surface",   pSurface p)
  , ("--g-mute",      pMuted p)
  , ("--g-border",    pBorder p)
  , ("--g-accent",    pAccent p)
  , ("--g-sel",       pSelection p)
  , ("--g-point",     pPoint p)
  , ("--g-point-dim", pPointDim p)
  , ("--g-point-off", pPointOff p)
  , ("--g-link",      pLink p)
  , ("--g-col",       pColumn p)
  , ("--g-cell-wash", pCellWash p)
  , ("--g-flag-wash", pFlagWash p)
  , ("--g-ok",        pOk p)
  , ("--g-warn",      pWarn p)
  , ("--g-bad",       pBad p)
  , ("--g-veil",      pVeil p)
  , ("--g-shadow",    pShadow p)
  ]
 <> slots p "--g-state-a"   stateSlots    (pActive p)
 <> slots p "--g-state-i"   stateSlots    (pInactive p)
 <> slots p "--g-priority-" prioritySlots (pPriority p)

-- | THE COUNT IS THE WIRE'S, so a slot the badges name is always declared;
-- fewer hues repeat, and an empty list falls back to the theme's own ink.
slots :: Palette -> Text -> Int -> [Text] -> [(Text, Text)]
slots p prefix n hues =
  [ (prefix <> T.pack (show i), cycle filled !! i) | i <- [0 .. n - 1] ]
  where filled = if null hues then [pFg p] else hues

-- | The renderer's FLAG is 'pBad': the archive flag and an error are one red.
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
    rules pad page table t = block pad page (scheme t : pageTokens (thPalette t))
                          <> block pad table (tableTokens (thPalette t))
    -- A `<select>' is drawn by the UA: undeclared, a dark page gets the LIGHT
    -- control palette and the page's own `color' over it — white on white.
    -- Rides the palette blocks so the scheme cannot drift from its tokens.
    scheme t = ("color-scheme", case thMode t of Dark -> "dark"; Light -> "light")

-- | Emitted AFTER 'themeCSS' at the same specificity, so a later rule wins, and
-- per REQUEST — these come off the store's config, never out of the build.
themeOverrides :: [(Text, [(Text, Text)])] -> Text
themeOverrides settings = T.concat
  [ block "  " (":root[data-theme=\"" <> theme <> "\"]")
          [ ("--g-state-" <> value, hue) | (value, hue) <- pairs ]
  | (theme, pairs) <- settings, not (null pairs) ]

block :: Text -> Text -> [(Text, Text)] -> Text
block pad selector tokens = T.concat
  ( [pad, selector, "{\n"]
 <> [ T.concat [pad, "  ", name, ":", value, ";\n"] | (name, value) <- tokens ]
 <> [pad, "}\n"] )
