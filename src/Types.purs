module Types ( AllEffs
             , CanvasEff
             , StateTree(..)
             , Shape(..)
             , MetaData(..)
             , AnimationOperation(..)
             , AnimationOperations(..)) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import FRP (FRP)
import Graphics.Canvas (Arc, CANVAS, CanvasElement, Context2D, Rectangle, arc, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, rect, setStrokeStyle, stroke)

type AllEffs eff = (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | eff)
type CanvasEff eff = (canvas :: CANVAS | eff)

data StateTree a b = Draw (Shape a) | Parent b (StateTree a b) (StateTree a b)

data Shape a = Circle Arc | Rect Rectangle

newtype MetaData = MetaData { groupName :: String }

data AnimationOperation = Translate Number Number | Rotate Number

type AnimationOperations = Array AnimationOperation

instance shapeEq :: Eq (Shape a) where
  eq (Circle c1) (Circle c2) = c1.x == c2.x && c1.y == c2.y && c1.r == c2.r
  eq (Rect r1) (Rect r2) = r1.x == r2.x && r1.y == r2.y && r1.w == r2.w && r1.h == r2.h
  eq _ _ = false

instance stateTreeEq :: Eq a => Eq (StateTree a MetaData) where
  eq (Draw s1) (Draw s2) = eq s1 s2
  eq (Parent v1 lTree1 rTree1) (Parent v2 lTree2 rTree2) = eq lTree1 lTree2 && eq rTree1 rTree2 && eq v1 v2
  eq _ _ = false

instance metaDataEq :: Eq MetaData where
  eq (MetaData m1) (MetaData m2) = m1.groupName == m2.groupName
