module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Maybe (Maybe(..))
import FRP (FRP)
import FRP.Behavior (sample_, step)
import FRP.Event (Event, subscribe)
import FRP.Event.Time (interval)
import Graphics.Canvas (Arc, CANVAS, CanvasElement, Context2D, Rectangle, arc, beginPath, closePath, getCanvasElementById, getContext2D, rect, setStrokeStyle, stroke)
import Math (pi)

type AllEffs eff = (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | eff)
type CanvasEff eff = (canvas :: CANVAS | eff)

data StateTree a b = Draw (Shape a) | Parent b (StateTree a b) (StateTree a b)

data Shape a = Circle Arc | Rect Rectangle

newtype MetaData = MetaData { groupName :: String }

instance drawShape :: Drawable (Shape a) where
  draw ctx (Circle a) = beginPath ctx *> arc ctx a *> stroke ctx *> closePath ctx
  draw ctx (Rect a) = beginPath ctx *> rect ctx a *> stroke ctx *> closePath ctx

instance drawStateTree :: Drawable a => Drawable (StateTree a MetaData) where
  draw ctx (Draw s) = draw ctx s
  draw ctx (Parent val leftTree rightTree) = draw ctx leftTree *> draw ctx rightTree


class Drawable a where
  draw :: forall eff. Context2D -> a -> Eff (canvas :: CANVAS | eff) Context2D

setupBehaviors :: forall e. Event Int -> Eff (AllEffs e) Unit
setupBehaviors evStream = let sampler = sample_ (step 0 evStream) evStream
                           in subscribe sampler (\s -> logShow s) *> pure unit

defaultMetaData :: MetaData
defaultMetaData = MetaData {groupName: "default"}

circle :: forall a. Number -> Number -> Shape a
circle x y = Circle {x, y, r: 50.0, start: 0.0, end: 2.0 * pi}

rectangle :: forall a. Number -> Number -> Number -> Number -> Shape a
rectangle x y w h = Rect {x, y, w, h}

subTree :: forall a. StateTree (Shape a) MetaData
subTree = Parent defaultMetaData (Draw $ circle 0.0 0.0) (Draw $ rectangle 200.0 200.0 50.0 50.0)

tree :: forall a. StateTree (Shape a) MetaData
tree = Parent defaultMetaData subTree (Draw $ circle 50.0 100.0)

initCanvas :: forall e. CanvasElement -> Context2D -> Eff (AllEffs e) Unit
initCanvas c ctx = do
  _ <- setStrokeStyle "#000000" ctx *>
  draw ctx tree
  setupBehaviors (interval  100)

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | e) Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
       Just c -> getContext2D c >>= initCanvas c
       Nothing -> log "no canvas found"
  log "Hello!"
