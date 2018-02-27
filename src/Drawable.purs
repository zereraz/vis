module Drawable where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import FRP (FRP)
import FRP.Behavior (Behavior, sample_, step)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Graphics.Canvas (Arc, CANVAS, CanvasElement, Context2D, Rectangle, arc, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, rect, setStrokeStyle, stroke)
import Math (pi)
import Types

circle :: forall a. Number -> Number -> Number -> Shape a
circle x y r = Circle {x, y, r, start: 0.0, end: 2.0 * pi}

rectangle :: forall a. Number -> Number -> Number -> Number -> Shape a
rectangle x y w h = Rect {x, y, w, h}

class Drawable a where
  draw :: forall eff. Context2D -> a -> Eff (canvas :: CANVAS | eff) Context2D
  getBound :: a -> Rectangle
  translate :: Number -> Number -> a -> a

instance drawShape :: Drawable (Shape a) where
  draw ctx (Circle a) = beginPath ctx *> arc ctx a *> stroke ctx *> closePath ctx
  draw ctx (Rect a) = beginPath ctx *> rect ctx a *> stroke ctx *> closePath ctx

  getBound (Circle a) = {x:a.x - a.r, y:a.y - a.r, w: 2.0 * a.r, h: 2.0 * a.r}
  getBound (Rect a) = a

  translate x y (Circle c) = Circle (c {x = c.x + x, y = c.y + y})
  translate x y (Rect r) = Rect (r {x = r.x + x, y = r.y + y})

instance drawStateTree :: Drawable a => Drawable (StateTree a MetaData) where
  draw ctx (Draw s) = draw ctx s
  draw ctx (Parent _ leftTree rightTree) = draw ctx leftTree *> draw ctx rightTree

  getBound (Draw s) = getBound s
  getBound (Parent _ leftTree rightTree) = addBounds (getBound leftTree) (getBound rightTree)

  translate x y (Draw s) = Draw $ translate x y s
  translate x y (Parent val leftTree rightTree) = Parent val (translate x y leftTree) (translate x y rightTree)

addBounds :: forall a. Rectangle -> Rectangle -> Rectangle
addBounds {x:x1,y:y1,w:w1,h:h1}
          {x:x2,y:y2,w:w2,h:h2} = let minX = min x1 x2
                                      maxX = max (x1 + w1) (x2 + w2)
                                      minY = min y1 y2
                                      maxY = max (y1 + h1) (y2 + h2)
                                   in  {x: minX, y: minY, w: maxX - minX, h: maxY - minY}

drawBound :: forall a e. Drawable a => Context2D -> a -> Eff (AllEffs e) Unit
drawBound ctx s = let bound = getBound s
                in draw ctx (Rect bound) *> pure unit