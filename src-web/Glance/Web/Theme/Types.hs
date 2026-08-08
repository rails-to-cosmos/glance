-- | What a theme IS, apart from which themes there are.
--
-- Its own module so a theme file names the type without importing the registry
-- that lists it ('Glance.Web.Theme'), which would be a cycle.
module Glance.Web.Theme.Types (Mode (..), Palette (..)) where

import Data.Text (Text)

-- | Which system preference a theme answers by default.
data Mode = Light | Dark deriving (Eq, Show)

-- | One theme's colours, by ROLE rather than by the token a consumer spells
-- them with.  Both emitters read this, so a role has one value per theme.
--
-- Washes are percentages: a hue is mixed into its ground at these strengths,
-- and how far it travels depends on whether the ground is lit or dark.
data Palette = Palette
  { pBg        :: !Text  -- ^ page and table ground.
  , pFg        :: !Text  -- ^ ordinary ink.
  , pSurface   :: !Text  -- ^ raised ground: popups, log strip, striped row.
  , pMuted     :: !Text  -- ^ ink for what is read past.
  , pBorder    :: !Text  -- ^ every hairline, one weight.
  , pAccent    :: !Text  -- ^ the focus frame: which pane holds the keys.
  , pSelection :: !Text  -- ^ the cursor row, table and document alike.
  , pHover     :: !Text  -- ^ the row under the pointer.
  , pLink      :: !Text  -- ^ org link ink, measured against a ROW's grounds.
  , pFrost     :: !Text  -- ^ the applied filter's chips.
  , pColumn    :: !Text  -- ^ the crosshair's column band.
  , pOk        :: !Text  -- ^ a write that landed.
  , pWarn      :: !Text  -- ^ a warning line.
  , pBad       :: !Text  -- ^ an error, and the archive FLAG — one red.
  , pVeil      :: !Text  -- ^ what a modal lays over what it covers.
  , pShadow    :: !Text  -- ^ what a raised box drops.
  , pChipWash  :: !Text  -- ^ how far 'pFrost' tints a chip.
  , pChipEdge  :: !Text  -- ^ and its hairline.
  , pMarkWash  :: !Text  -- ^ a marked row's ground.
  , pFlagWash  :: !Text  -- ^ a flagged row's.
  , pColWash   :: !Text  -- ^ the crosshair's column band.
  , pCellWash  :: !Text  -- ^ and the cell the two bands cross in.
  , pSortWash  :: !Text  -- ^ the chip stating the ORDER.
  , pColsWash  :: !Text  -- ^ the chip stating the COLUMN SET.
  }
