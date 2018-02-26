module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Maybe (Maybe(..))
import FRP (FRP)
import FRP.Behavior (Behavior, sample_, step)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Graphics.Canvas (Arc, CANVAS, CanvasElement, Context2D, Rectangle, arc, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, rect, setStrokeStyle, stroke)
import Math (pi)
import Unsafe.Coerce (unsafeCoerce)

type AllEffs eff = (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | eff)
type CanvasEff eff = (canvas :: CANVAS | eff)

data StateTree a b = Draw (Shape a) | Parent b (StateTree a b) (StateTree a b)

data Shape a = Circle Arc | Rect Rectangle

newtype MetaData = MetaData { groupName :: String }

instance drawShape :: Drawable (Shape a) where
  draw ctx (Circle a) = beginPath ctx *> arc ctx a *> stroke ctx *> closePath ctx
  draw ctx (Rect a) = beginPath ctx *> rect ctx a *> stroke ctx *> closePath ctx

  getBound (Circle a) = {x:a.x - a.r, y:a.y - a.r, w: 2.0 * a.r, h: 2.0 * a.r}
  getBound (Rect a) = a

  translate x y (Circle c) = Circle (c {x = c.x + x, y = c.y + y})
  translate x y (Rect r) = Rect (r {x = r.x + x, y = r.y + y})

addBounds :: forall a. Rectangle -> Rectangle -> Rectangle
addBounds {x:x1,y:y1,w:w1,h:h1}
          {x:x2,y:y2,w:w2,h:h2} = let minX = min x1 x2
                                      maxX = max (x1 + w1) (x2 + w2)
                                      minY = min y1 y2
                                      maxY = max (y1 + h1) (y2 + h2)
                                   in  {x: minX, y: minY, w: maxX - minX, h: maxY - minY}

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


instance drawStateTree :: Drawable a => Drawable (StateTree a MetaData) where
  draw ctx (Draw s) = draw ctx s
  draw ctx (Parent _ leftTree rightTree) = draw ctx leftTree *> draw ctx rightTree

  getBound (Draw s) = getBound s
  getBound (Parent _ leftTree rightTree) = addBounds (getBound leftTree) (getBound rightTree)

  translate x y (Draw s) = Draw $ translate x y s
  translate x y (Parent val leftTree rightTree) = Parent val (translate x y leftTree) (translate x y rightTree)

class Drawable a where
  draw :: forall eff. Context2D -> a -> Eff (canvas :: CANVAS | eff) Context2D
  getBound :: a -> Rectangle
  translate :: Number -> Number -> a -> a

setupUpdate :: forall e a. Behavior (StateTree (Shape a) MetaData) -> Event (StateTree (Shape a) MetaData) -> Context2D -> Eff (AllEffs e) Unit
setupUpdate treeBeh evStream ctx = let sampler = sample_ treeBeh evStream
                                       in subscribe sampler (\s -> let {x,y,w,h} = getBound s in (clearRect ctx {x: x - 2.0,y: y - 2.0,w: w + 4.0,h: h + 4.0} *> draw ctx s)) *> pure unit

defaultMetaData :: MetaData
defaultMetaData = MetaData {groupName: "default"}

circle :: forall a. Number -> Number -> Number -> Shape a
circle x y r = Circle {x, y, r, start: 0.0, end: 2.0 * pi}

rectangle :: forall a. Number -> Number -> Number -> Number -> Shape a
rectangle x y w h = Rect {x, y, w, h}

subTree :: forall a. StateTree (Shape a) MetaData
subTree = Parent defaultMetaData (Draw $ circle 300.0 250.0 20.0) (Draw $ rectangle 200.0 200.0 50.0 50.0)

tree :: forall a. StateTree (Shape a) MetaData
tree = Parent defaultMetaData subTree (Draw $ circle 50.0 100.0 40.0)

animate :: forall a b eff. Drawable a => Eq a => (Behavior (StateTree a MetaData)) -> Event Int -> ((StateTree a MetaData) -> Eff _ Unit) -> Eff _ Unit
animate stateBeh frameStream push = let sampler = sample_ stateBeh frameStream
                                     in subscribe sampler (\s -> let newState = translate 0.0 1.0 s in when (not $ newState == s) (push newState)) *> pure unit

initCanvas :: forall e. CanvasElement -> Context2D -> Eff (AllEffs e) Unit
initCanvas c ctx = do
  {event, push} <- create
  let stateBeh = step tree event
      frameInterval = 1000 / 60
      frameStream = interval frameInterval
  setStrokeStyle "#000000" ctx *>
  setupUpdate stateBeh event ctx *>
  animate stateBeh frameStream push
  push tree

drawBound :: forall a e. Drawable a => Context2D -> a -> Eff (AllEffs e) Unit
drawBound ctx s = let bound = getBound s
                in draw ctx (Rect bound) *> pure unit

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | e) Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
       Just c -> getContext2D c >>= initCanvas c
       Nothing -> log "no canvas found"
