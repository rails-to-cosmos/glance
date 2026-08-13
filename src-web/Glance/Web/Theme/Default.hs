-- | The default theme, light and dark.  Its faces are the author's Emacs theme
-- (../danneskjold-theme); four values are corrected for contrast, hue held.
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
  , pChipWash  = "45%"
  , pChipEdge  = "95%"
  , pMarkWash  = "8%"
  , pFlagWash  = "8%"
  , pColWash   = "35%"
  , pCellWash  = "60%"
  , pSortWash  = "52%"
  , pColsWash  = "52%"
    -- DARK ENOUGH TO BE INK.  A pill draws its hue as text over a 15% wash of
    -- itself, and on a SELECTED row that wash sits on golden.
  , pActive    = ["#9A3412", "#B45309", "#B91C1C", "#7C2D12"]
  , pInactive  = ["#166534", "#0F766E", "#1D4ED8", "#4B5563"]
  , pPriority  = ["#B91C1C", "#A16207", "#15803D"]
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
  , pChipWash  = "18%"
  , pChipEdge  = "34%"
  , pMarkWash  = "30%"
  , pFlagWash  = "30%"
  , pColWash   = "8%"
  , pCellWash  = "9%"
  , pSortWash  = "18%"
  , pColsWash  = "18%"
  , pActive    = ["#E0AF68", "#FF9E64", "#F7768E", "#FFC777"]
  , pInactive  = ["#9ECE6A", "#73DACA", "#41A6B5", "#7C86BF"]
  , pPriority  = ["#E74C3C", "#FFCC00", "#27AE60"]
  }
