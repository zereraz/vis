module Utils where

import Prelude
import Types

import Math (pi)

circle :: forall a. Number -> Number -> Number -> Shape a
circle x y r = Circle {x, y, r, start: 0.0, end: 2.0 * pi}

rectangle :: forall a. Number -> Number -> Number -> Number -> Shape a
rectangle x y w h = Rect {x, y, w, h}

defaultMetaData :: Array AnimationOperation -> MetaData
defaultMetaData animOps = MetaData {groupName: "default", animOps}

subTree :: forall a. Array AnimationOperation -> StateTree (Shape a) MetaData
subTree animOps = Parent meta (Draw meta $ circle 300.0 250.0 20.0) (Draw meta $ rectangle 200.0 200.0 50.0 50.0)
  where
    meta = defaultMetaData animOps

tree :: forall a. Array AnimationOperation -> StateTree (Shape a) MetaData
tree animOps = Parent meta (subTree animOps) (Draw meta (circle 50.0 250.0 20.0))
  where
    meta = defaultMetaData animOps

-- TODO: Add lens and remove this
getAnimOps :: forall a. StateTree a MetaData -> Array AnimationOperation
getAnimOps (Parent (MetaData m) _ _ ) = m.animOps
getAnimOps (Draw (MetaData m) _) = m.animOps
