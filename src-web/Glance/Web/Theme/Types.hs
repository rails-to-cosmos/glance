-- | What a theme IS.  Its own module: naming the registry would be a cycle.
module Glance.Web.Theme.Types (Mode (..), Palette (..)) where

import Data.Text (Text)

data Mode = Light | Dark deriving (Eq, Show)

-- | One theme's colours by ROLE; both emitters read it, so a role has one value.
data Palette = Palette
  { pBg        :: !Text  -- ^ page and table ground.
  , pFg        :: !Text  -- ^ ordinary ink.
  , pSurface   :: !Text  -- ^ raised ground: popups, log strip, striped row.
  , pMuted     :: !Text  -- ^ ink for what is read past.
  , pBorder    :: !Text  -- ^ every hairline, one weight.
  , pAccent    :: !Text  -- ^ the focus frame: which pane holds the keys.
  , pSelection :: !Text  -- ^ the cursor row's ground, in the table and the listings.
  , pPoint     :: !Text  -- ^ the RESERVED-TOKEN ink: a drawer's frame and keys, the
                         --   planning keywords, the strip's last crumb.  A ground hue
                         --   is no ink, so a theme cannot reuse 'pSelection'.
  , pPointDim  :: !Text  -- ^ what point CARRIES: subordinate to 'pPoint' and nearer
                         --   grey than gold, PICKED rather than mixed -- a darker
                         --   yellow is brown.
  , pPointOff :: !Text   -- ^ a connector, and a line, nobody is looking at.  PICKED
                         --   per theme: dark's is a deep blue, light's a pale one.
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
    -- A hue is INK over a 15% wash of itself: it must read over 'pBg' AND 'pSelection'.
  , pActive    :: ![Text]  -- ^ keywords that still want work.
  , pInactive  :: ![Text]  -- ^ the done-like ones.
  , pPriority  :: ![Text]  -- ^ org's @[#A]@ @[#B]@ @[#C]@, in that order.
  }
