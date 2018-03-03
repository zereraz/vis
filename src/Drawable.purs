module Drawable (
    class Drawable
  , getBound
  , translate
  , draw
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Graphics.Canvas (Context2D, CANVAS, Rectangle, arc, beginPath, closePath, rect, stroke)
import Types (Shape(..), StateTree(..), MetaData, AllEffs)


class Drawable a where
  draw :: forall eff. Context2D -> a -> Eff (canvas :: CANVAS | eff) Context2D
  getBound :: a -> Rectangle
  translate :: Number -> Number -> a -> a

instance drawShape :: Drawable (Shape a) where
  draw ctx (Circle a) = beginPath ctx *> arc ctx a *> stroke ctx *> closePath ctx
  draw ctx (Rect a) = beginPath ctx *> rect ctx a *> stroke ctx *> closePath ctx

  getBound (Circle {x,y,r}) = {x:x - r, y:y - r, w: 2.0 * r, h: 2.0 * r}
  getBound (Rect a) = a

  translate x y (Circle c) = Circle (c {x = c.x + x, y = c.y + y})
  translate x y (Rect r) = Rect (r {x = r.x + x, y = r.y + y})

instance drawStateTree :: Drawable a => Drawable (StateTree a MetaData) where
  draw ctx (Draw _ _ s) = draw ctx s
  draw ctx (Parent _ leftTree rightTree) = draw ctx leftTree *> draw ctx rightTree

  getBound (Draw _ _ s) = getBound s
  getBound (Parent _ leftTree rightTree) = addBounds (getBound leftTree) (getBound rightTree)

  translate x y (Draw p m s) = Draw p m (translate x y s)
  translate x y (Parent m leftTree rightTree) = Parent m (translate x y leftTree) (translate x y rightTree)

addBounds :: Rectangle -> Rectangle -> Rectangle
addBounds {x:x1,y:y1,w:w1,h:h1}
          {x:x2,y:y2,w:w2,h:h2} = let minX = min x1 x2
                                      maxX = max (x1 + w1) (x2 + w2)
                                      minY = min y1 y2
                                      maxY = max (y1 + h1) (y2 + h2)
                                   in  {x: minX, y: minY, w: maxX - minX, h: maxY - minY}

drawBound :: forall a e. Drawable a => Context2D -> a -> Eff (AllEffs e) Unit
drawBound ctx s = let bound = getBound s in draw ctx (Rect bound) *> pure unit
