module Vis.Types ( AllEffs
             , CanvasEff
             , Effs
             , StateTree(..)
             , Shape(..)
             , MetaData(..)
             , AnimationOperation(..)
             , AnimationOperations(..)
             , Graphic(..)
             , Gref
             , class IsProperty
             , toProperty
             , applyProperty
             , Property(..)
             , PropVal(..)) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import FRP (FRP)
import Graphics.Canvas (Arc, CANVAS, Context2D, Rectangle, setFillStyle, setStrokeStyle)

type AllEffs eff = (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | eff)
type CanvasEff eff = (canvas :: CANVAS | eff)
type Effs = (console :: CONSOLE, canvas :: CANVAS, frp :: FRP)

-- TODO: change map string -> string to more generic?
-- use things similar to smolder for attributes
data StateTree a b = Draw (Map String String) b (Shape a) | Parent b (StateTree a b) (StateTree a b)

data Shape a = Circle Arc | Rect Rectangle

newtype MetaData = MetaData { groupName :: String, animOps :: Array AnimationOperation }

data AnimationOperation = Translate Number Number | Rotate Number

type AnimationOperations = Array AnimationOperation

data Property = Stroke PropVal |  Id PropVal | Fill PropVal | Shadow PropVal | NoSet

data PropVal = StrokeVal String | IdVal String | FillVal String | ShadowVal String

class IsProperty a where
  toProperty :: Tuple String String -> a
  applyProperty :: forall eff. Context2D -> a -> Eff (canvas :: CANVAS | eff) Context2D

instance propertyIsProperty :: IsProperty Property where
  toProperty (Tuple k v)
    | k == "stroke" = Stroke (StrokeVal v)
    | k == "fill" = Fill (FillVal v)
    | otherwise = NoSet

  applyProperty ctx (Stroke (StrokeVal s)) = setStrokeStyle s ctx
  applyProperty ctx (Fill (FillVal f)) = setFillStyle f ctx
  applyProperty ctx _ = pure ctx

-- | Each graphic contains it's animation canceller
-- | update loop canceller
type Gref a = {
  updateCanceller :: Eff Effs Unit
  , animCanceller :: Eff Effs Unit
  , state :: StateTree a MetaData
}
newtype Graphic a = Graphic a

instance shapeEq :: Eq (Shape a) where
  eq (Circle c1) (Circle c2) = c1.x == c2.x && c1.y == c2.y && c1.r == c2.r
  eq (Rect r1) (Rect r2) = r1.x == r2.x && r1.y == r2.y && r1.w == r2.w && r1.h == r2.h
  eq _ _ = false

instance stateTreeEq :: Eq a => Eq (StateTree a MetaData) where
  eq (Draw p1 m1 s1) (Draw p2 m2 s2) = eq s1 s2 && m1 == m2 && p1 == p2
  eq (Parent v1 lTree1 rTree1) (Parent v2 lTree2 rTree2) = eq lTree1 lTree2 && eq rTree1 rTree2 && eq v1 v2
  eq _ _ = false

instance metaDataEq :: Eq MetaData where
  eq (MetaData m1) (MetaData m2) = m1.groupName == m2.groupName && m1.animOps == m2.animOps

instance animOpsEq :: Eq AnimationOperation where
  eq (Translate x1 y1) (Translate x2 y2) = x1 == x2 && y1 == y2
  eq (Rotate r1) (Rotate r2) = r1 == r2
  eq _ _ = false
