-- | THE POPUP SURFACES, ONE LIST.  Six readers spelled the membership by hand and
--   the copies drifted: the mint commit hand-edited six sibling id lists and missed
--   the seventh, which is why @#mint@ neither faded nor dimmed.
module Glance.Web.Page.Popups
  ( Popup (..)
  , Tier (..)
  , popups
  , veiled
  , washed
  , boxes
  , tierClass
  ) where

import Data.Text (Text)

-- | The size a surface's box takes.  @refer@ hangs at the caret and takes none.
data Tier = Band | Sheet | Untiered
  deriving (Eq, Show)

data Popup = Popup
  { puWrap   :: !Text   -- ^ the wrapper id, which the veil and the wash name
  , puBox    :: !Text   -- ^ the box inside it, which the tier sizes
  , puTier   :: !Tier
  , puVeiled :: !Bool   -- ^ takes the backdrop
  , puWashed :: !Bool   -- ^ dims while the store is stale
  }

-- | Every popup the shell raises; @sheet@ is the materialize surface, whose wrapper
--   is @modal@.
popups :: [Popup]
popups =
  [ Popup "modal"   "sheet" Sheet     True  True
  , Popup "prompt"  "pbox"  Band      True  True
  , Popup "config"  "cbox"  Sheet     True  True
  , Popup "links"   "lbox"  Sheet     True  True
  , Popup "tags"    "tbox"  Band      True  True
  , Popup "capture" "kbox"  Sheet     True  True
  , Popup "mint"    "nbox"  Band      True  True
  , Popup "refer"   "rbox"  Untiered  False False
  ]

-- | @#a,#b,…@ over the surfaces PICK holds for, in the order 'popups' names them.
sel :: (Popup -> Text) -> (Popup -> Bool) -> Text
sel part pick = joinWith "," [ "#" <> part p | p <- popups, pick p ]

veiled, washed, boxes :: Text
veiled = sel puWrap puVeiled
washed = sel puWrap puWashed
boxes = sel puBox (const True)

-- | The class 'Glance.Web.Page' hangs on a box, which the stylesheet sizes.
tierClass :: Tier -> Text
tierClass Band = "pop-band"
tierClass Sheet = "pop-sheet"
tierClass Untiered = ""

joinWith :: Text -> [Text] -> Text
joinWith _ [] = ""
joinWith s (x:xs) = foldl (\acc y -> acc <> s <> y) x xs
