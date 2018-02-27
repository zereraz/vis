module Utils where

import Prelude
import Math (pi)
import Types

circle :: forall a. Number -> Number -> Number -> Shape a
circle x y r = Circle {x, y, r, start: 0.0, end: 2.0 * pi}

rectangle :: forall a. Number -> Number -> Number -> Number -> Shape a
rectangle x y w h = Rect {x, y, w, h}

defaultMetaData :: MetaData
defaultMetaData = MetaData {groupName: "default"}

subTree :: forall a. StateTree (Shape a) MetaData
subTree = Parent defaultMetaData (Draw $ circle 300.0 250.0 20.0) (Draw $ rectangle 200.0 200.0 50.0 50.0)

tree :: forall a. StateTree (Shape a) MetaData
tree = Parent defaultMetaData subTree (Draw $ circle 50.0 100.0 40.0)
