-- | THE POPUP SURFACES, ONE LIST, because six hand-spelled copies drifted —
--   docs\/proposals\/partial/2026-08-18-generalize-popup-surface-registry.md.
module Glance.Web.Page.Popups
  ( Popup (..)
  , Tier (..)
  , popups
  , veiled
  , washed
  , boxes
  , chromeBoxes
  , chromeFeet
  , chromeHeads
  , tierClass
  ) where

import Data.Text (Text)

import qualified Data.Text as T

-- | The size a surface's box takes.  @refer@ hangs at the caret and takes none.
data Tier = Band | Sheet | Untiered
  deriving (Eq, Show)

data Popup = Popup
  { puWrap   :: !Text   -- ^ the wrapper id, which the veil and the wash name
  , puId     :: !Text   -- ^ the prefix every part id wears: @p@ spells @#phead@
  , puBox    :: !Text   -- ^ the box inside it, which the tier sizes
  , puTier   :: !Tier
  , puVeiled :: !Bool   -- ^ takes the backdrop
  , puWashed :: !Bool   -- ^ dims while the store is stale
  , puChrome :: ![Text] -- ^ the shared-chrome parts it wears: box, head, foot
  }

-- | Every popup the shell raises; @sheet@ is the materialize surface, whose wrapper
--   is @modal@.
popups :: [Popup]
popups =
  [ Popup "modal"   "m" "sheet" Sheet     True  True  []
  , Popup "prompt"  "p" "pbox"  Band      True  True  everyPart
  , Popup "config"  "c" "cbox"  Sheet     True  True  ["foot"]
  , Popup "links"   "l" "lbox"  Sheet     True  True  everyPart
  , Popup "tags"    "t" "tbox"  Band      True  True  everyPart
    -- ONE FIELD AND A SHORT LIST since the sheet took the document: the band is
    -- the palette's own tier and the shape this form now has.
  , Popup "capture" "k" "kbox"  Band      True  True  everyPart
  , Popup "mint"    "n" "nbox"  Band      True  True  everyPart
  , Popup "refer"   "r" "rbox"  Untiered  False False []
  ]

-- | The three shared-chrome parts, which most surfaces wear together.
everyPart :: [Text]
everyPart = ["box", "head", "foot"]

-- | @#a,#b,…@ over the surfaces PICK holds for, in the order 'popups' names them.
sel :: (Popup -> Text) -> (Popup -> Bool) -> Text
sel part pick = T.intercalate "," [ "#" <> part p | p <- popups, pick p ]

veiled, washed, boxes :: Text
veiled = sel puWrap puVeiled
washed = sel puWrap puWashed
boxes = sel puBox (const True)

-- | @#pbox,#lbox,…@ — PART on every surface wearing it.
chrome :: Text -> Text
chrome part = sel ((<> part) . puId) (elem part . puChrome)

-- | The three selectors, bound so the shell renders them rather than rebuilds them.
chromeBoxes, chromeHeads, chromeFeet :: Text
chromeBoxes = chrome "box"
chromeHeads = chrome "head"
chromeFeet  = chrome "foot"

-- | The class 'Glance.Web.Page' hangs on a box, which the stylesheet sizes.
tierClass :: Tier -> Text
tierClass Band = "pop-band"
tierClass Sheet = "pop-sheet"
tierClass Untiered = ""
