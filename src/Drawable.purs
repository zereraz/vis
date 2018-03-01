module Drawable (
    class Drawable
  , bound
  , translate
  , rotate
  , scale
  , draw
  , setProperties
  ) where

import Prelude
import Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (foldl, head, tail)
import Data.Map (Map, empty, toUnfoldable)
import Data.Maybe (fromMaybe)
import Data.Newtype (overF)
import Data.Traversable (sequence, traverse)
import FRP (FRP)
import FRP.Behavior (Behavior, sample_, step)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Global (infinity)
import Graphics.Canvas (Arc, CANVAS, CanvasElement, Context2D, Rectangle, arc, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setStrokeStyle, stroke)
import Math (Radians, cos, pi, pow, sin, sqrt)


path' :: forall e. Context2D -> Array Point -> Eff (CanvasEff e) Unit
path' ctx p = do
  let first = fromMaybe {x: 0.0, y: 0.0} (head p)
      rest  = fromMaybe [] (tail p)
  moveTo ctx first.x first.y *> sequence ((\t -> lineTo ctx t.x t.y) <$> rest) *> pure unit

class Drawable a where
  draw :: forall eff. Context2D -> Map String String -> a -> Eff (CanvasEff eff) Context2D
  bound :: a -> Rectangle
  translate :: Number -> Number -> a -> a
  rotate :: Number -> Number -> Radians -> a -> a
  scale :: Number -> Number -> Number -> a -> a
  setProperties :: forall eff. Context2D -> Map String String -> a -> Eff (CanvasEff eff) (Array Context2D)

instance drawShape :: Drawable (Shape a) where
  draw ctx props s@(Circle a) = setProperties ctx props s *> beginPath ctx *> arc ctx a *> stroke ctx *> closePath ctx
  draw ctx props s@(Rect a) = beginPath ctx *> rect ctx a *> stroke ctx *> closePath ctx
  draw ctx props s@(Path p) = beginPath ctx *> path' ctx p *> stroke ctx *> closePath ctx

  bound (Circle {x,y,r}) = {x:x - r, y:y - r, w: 2.0 * r, h: 2.0 * r}
  bound (Rect a) = a
  bound (Path p) = let minX = foldl (\a b -> min a b) infinity $ map _.x p
                       minY = foldl (\a b -> min a b) infinity $ map _.y p
                       maxX = foldl (\a b -> max a b) (-1.0 * infinity) $ map _.x p
                       maxY = foldl (\a b -> max a b) (-1.0 * infinity) $ map _.y p
                   in {x: minX, y: minY, w: maxX - minX, h: maxY - minY}

  translate x y (Circle c) = Circle (c {x = c.x + x, y = c.y + y})
  translate x y (Rect r) = Rect (r {x = r.x + x, y = r.y + y})
  translate x y (Path p) = Path $ (\t -> {x: t.x + x, y: t.y + y}) <$> p

  rotate x y ang (Circle c) = let o = {x: c.x - x, y: c.y - y}
                              in Circle $ c {x = ((o.x * cos ang) - (o.y * sin ang)) + x, y = ((o.y * cos ang) + (o.x * sin ang)) + y}
  rotate x y ang (Rect r) = let o t = {x: t.x - x, y: t.y - y}
                            in case (toPoints $ Rect r) of
                                Path p -> Path $ (\t -> t {x = ((o t).x * cos ang) - ((o t).y * sin ang) + x, y = ((o t).y * cos ang) + ((o t).x * sin ang) + y}) <$> p
                                _ -> Rect r
  rotate x y ang (Path p) = let o t = {x: t.x - x, y: t.y - y}
                            in Path $ (\t -> t {x = ((o t).x * cos ang) - ((o t).y * sin ang) + x, y = ((o t).y * cos ang) + ((o t).x * sin ang) + y}) <$> p
  
  scale x y t (Circle c) = let o = {x: c.x - x, y: c.y - y}
                              in Circle $ c {x = (o.x * (t + 1.0)) + x, y = (o.y * (t + 1.0)) + y, r = t * c.r}
  scale x y t (Rect r) = let o k = {x: k.x - x, y: k.y - y}
                         in case (toPoints $ Rect r) of
                              Path p -> Path $ (\k -> k {x = ((o k).x * (t + 1.0)) + x, y = ((o k).y * (t + 1.0)) + y}) <$> p
                              _ -> Rect r
  scale x y t (Path p) = let o k = {x: k.x - x, y: k.y - y}
                         in Path $ (\k -> k {x = ((o k).x * (t + 1.0)) + x, y = ((o k).y * (t + 1.0)) + y}) <$> p

  setProperties ctx props _ = traverse (applyProperty ctx) $ (toProperty <$> toUnfoldable props) :: Array Property


instance drawStateTree :: Drawable a => Drawable (StateTree a MetaData) where
  draw ctx p (Draw props _ s) = draw ctx props s
  draw ctx p (Parent _ leftTree rightTree) = draw ctx p leftTree *> draw ctx p rightTree

  bound (Draw _ _ s) = bound s
  bound (Parent _ leftTree rightTree) = addBounds (bound leftTree) (bound rightTree)

  translate x y (Draw p m s) = Draw p m (translate x y s)
  translate x y (Parent m leftTree rightTree) = Parent m (translate x y leftTree) (translate x y rightTree)

  setProperties ctx props (Draw p m s) = setProperties ctx p s
  setProperties ctx props (Parent m leftTree rightTree) = setProperties ctx props leftTree *> setProperties ctx props rightTree

  rotate x y ang (Draw p m s) = Draw p m $ rotate x y ang s
  rotate x y ang (Parent val leftTree rightTree) = Parent val (rotate x y ang leftTree) (rotate x y ang rightTree)

  scale x y r (Draw p m s) = Draw p m $ scale x y r s
  scale x y r (Parent val lefttree righttree) = Parent val (scale x y r lefttree) (scale x y r righttree)

class Pathable a where
  toPoints :: Drawable a => a -> a
  fromPoints :: Drawable a => a -> a

instance pathShape :: Pathable (Shape a) where
  toPoints (Circle c) = Circle c
  toPoints (Rect {x, y, w, h}) = Path [{x, y}, {x: x + w, y}, {x: x + w, y: y + w}, {x, y: y + h}, {x, y}]
  toPoints (Path p) = Path p

  fromPoints (Circle c) = Circle c
  fromPoints (Rect r) = Rect r
  fromPoints (Path p) = Path p

addBounds :: Rectangle -> Rectangle -> Rectangle
addBounds {x:x1,y:y1,w:w1,h:h1}
          {x:x2,y:y2,w:w2,h:h2} = let minX = min x1 x2
                                      maxX = max (x1 + w1) (x2 + w2)
                                      minY = min y1 y2
                                      maxY = max (y1 + h1) (y2 + h2)
                                  in  {x: minX, y: minY, w: maxX - minX, h: maxY - minY}

drawBound :: forall a e. Drawable a => Context2D -> a -> Eff (AllEffs e) Unit
drawBound ctx s = draw ctx empty (Rect $ bound s) *> pure unit
