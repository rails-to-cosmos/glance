-- | The default theme, light and dark.
--
-- Its faces are the author's Emacs theme (../danneskjold-theme), which is why
-- a tree reads the same in both places; nothing else here depends on that.
--
-- Four values are deliberately off those faces:
--
--   * light 'pMuted' #7F8C8D -> #667071 and light 'pAccent' #4CB5F5 ->
--     #31769F, lightness only, hue held: 3.5:1 -> 5.1:1 and 2.3:1 -> 5.0:1 on
--     white.  Muted text is read; a focus frame is looked for.
--   * 'pLink' is that same correction on the accent, measured against the
--     grounds a ROW can wear.
--   * 'pBorder' goes the other way, QUIETER than the theme's own (1.25:1 and
--     1.5:1 against 1.8:1): a hairline carries nothing, so a visible rule is
--     noise.
module Glance.Web.Theme.Default (defaultLight, defaultDark) where

import Glance.Web.Theme.Types (Palette (..))

-- | Black on white.
defaultLight :: Palette
defaultLight = Palette
  { pBg        = "#FFFFFF"
  , pFg        = "#000000"
  , pSurface   = "#F8F8FF"
  , pMuted     = "#667071"
  , pBorder    = "#E3E6EA"
  , pAccent    = "#31769F"
  , pSelection = "#FFD600"
  , pHover     = "#FAFAFA"
  , pLink      = "#30739B"
  , pFrost     = "#D0E1F9"
  , pColumn    = "#FFF3D0"
  , pOk        = "#27AE60"
  , pWarn      = "#FFA500"
  , pBad       = "#E74C3C"
  , pVeil      = "#00000066"
  , pShadow    = "#00000033"
  -- A lit ground takes a hue further before it reads as tinted.
  , pChipWash  = "45%"
  , pChipEdge  = "95%"
  , pMarkWash  = "8%"
  , pFlagWash  = "8%"
  , pColWash   = "35%"
  , pCellWash  = "60%"
  , pSortWash  = "52%"
  , pColsWash  = "52%"
  }

-- | White on true black, which is the face rather than a charcoal.
defaultDark :: Palette
defaultDark = Palette
  { pBg        = "#000000"
  , pFg        = "#FFFFFF"
  , pSurface   = "#21252B"
  , pMuted     = "#A4C2EB"
  , pBorder    = "#2A2D3D"
  , pAccent    = "#4CB5F5"
  , pSelection = "#373D4F"
  , pHover     = "#1F1F1F"
  , pLink      = "#7CC9F8"
  , pFrost     = "#D0E1F9"
  , pColumn    = "#FFF3D0"
  , pOk        = "#B6E63E"
  , pWarn      = "#FFA500"
  , pBad       = "#E74C3C"
  , pVeil      = "#00000099"
  , pShadow    = "#00000077"
  -- And a dark one has less room, so every wash is the shorter journey.
  , pChipWash  = "18%"
  , pChipEdge  = "34%"
  , pMarkWash  = "30%"
  , pFlagWash  = "30%"
  , pColWash   = "8%"
  , pCellWash  = "9%"
  , pSortWash  = "18%"
  , pColsWash  = "18%"
  }
