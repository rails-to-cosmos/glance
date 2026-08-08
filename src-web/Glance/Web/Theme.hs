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
  ) where

import Data.List (find)
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (stateSlots)
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
 <> slots "--g-state-a" stateSlots (pActive p)
 <> slots "--g-state-i" stateSlots (pInactive p)
 <> slots "--g-priority-" (length (pPriority p)) (pPriority p)

-- | N slot tokens under PREFIX, HUES cycled to fill them.  The COUNT is the
-- wire's (`Glance.Query.stateSlots`) and the same for every theme, so a slot
-- the badges name is always declared however many hues a theme spells.
slots :: Text -> Int -> [Text] -> [(Text, Text)]
slots prefix n hues =
  [ (prefix <> T.pack (show i), cycle hues !! i) | i <- [0 .. n - 1] ]

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

-- | SELECTOR carrying TOKENS at PAD, one to a line: a palette is read by eye.
block :: Text -> Text -> [(Text, Text)] -> Text
block pad selector tokens = T.concat
  ( [pad, selector, "{\n"]
 <> [ T.concat [pad, "  ", name, ":", value, ";\n"] | (name, value) <- tokens ]
 <> [pad, "}\n"] )
