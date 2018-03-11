module Vis.Utils where

import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Rectangle)
import Math (pi)
import Prelude (($), (*))
import Vis.Drawable (class Drawable)
import Vis.Types (AnimationOperation, MetaData(..), Shape(..), StateTree(..))

circle :: forall a. Number -> Number -> Number -> Shape a
circle x y r = Circle {x, y, r, start: 0.0, end: 2.0 * pi}

rectangle :: forall a. Number -> Number -> Number -> Number -> Shape a
rectangle x y w h = Rect {x, y, w, h}

defaultMetaData :: Array AnimationOperation -> MetaData
defaultMetaData animOps = MetaData {groupName: "default", animOps, globalClear: false}

defaultProps :: Map String String
defaultProps = fromFoldable [ Tuple "id" "i", Tuple "stroke" "#eee123" ]

subTree :: forall a. Array AnimationOperation -> StateTree (Shape a) MetaData
subTree animOps = Parent meta (Draw defaultProps meta $ circle 300.0 250.0 20.0) (Draw defaultProps meta $ rectangle 200.0 200.0 50.0 50.0)
  where
    meta = defaultMetaData animOps

tree :: forall a. Array AnimationOperation -> StateTree (Shape a) MetaData
tree animOps = Parent meta (subTree animOps) (Draw defaultProps meta (circle 50.0 250.0 20.0))
  where
    meta = defaultMetaData animOps

-- TODO: Add lens and remove this
getAnimOps :: forall a. StateTree a MetaData -> Array AnimationOperation
getAnimOps (Parent (MetaData m) _ _ ) = m.animOps
getAnimOps (Draw p (MetaData m) _) = m.animOps

-- used to clear the screen
-- TODO: make it dynamic
globalBound :: Rectangle
globalBound = {x: 0.0, y: 0.0, w: 800.0, h: 800.0}

globalClear :: forall a. Drawable a => StateTree a MetaData -> Boolean
globalClear (Draw _ (MetaData m) _) = m.globalClear
globalClear (Parent (MetaData m) _ _) = m.globalClear
