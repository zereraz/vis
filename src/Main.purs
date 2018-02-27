module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (foldM)
import Data.Maybe (Maybe(..))
import FRP (FRP)
import FRP.Behavior (Behavior, sample_, step)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Graphics.Canvas (Arc, CANVAS, CanvasElement, Context2D, Rectangle, arc, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, rect, setStrokeStyle, stroke)
import Math (pi)
import Unsafe.Coerce (unsafeCoerce)
import Types
import Drawable

setupUpdate :: forall e a. Behavior (StateTree (Shape a) MetaData) -> Event (StateTree (Shape a) MetaData) -> Context2D -> Eff (AllEffs e) Unit
setupUpdate treeBeh evStream ctx = let sampler = sample_ treeBeh evStream
                                       in subscribe sampler (\s -> let {x,y,w,h} = getBound s in (clearRect ctx {x: x - 2.0,y: y - 2.0,w: w + 4.0,h: h + 4.0} *> draw ctx s)) *> pure unit

defaultMetaData :: MetaData
defaultMetaData = MetaData {groupName: "default"}

subTree :: forall a. StateTree (Shape a) MetaData
subTree = Parent defaultMetaData (Draw $ circle 300.0 250.0 20.0) (Draw $ rectangle 200.0 200.0 50.0 50.0)

tree :: forall a. StateTree (Shape a) MetaData
tree = Parent defaultMetaData subTree (Draw $ circle 50.0 100.0 40.0)

runOp :: forall a. Drawable a => StateTree a MetaData -> AnimationOperation -> StateTree a MetaData
runOp (Draw s) (Translate x y) = Draw (translate x y s)
runOp (Parent v lTree rTree) (Translate x y) = Parent v (translate x y lTree) (translate x y rTree)
runOp s _ = s


animate :: forall a b eff. Drawable a => Eq a => (Behavior (StateTree a MetaData)) -> Event Int -> ((StateTree a MetaData) -> Eff _ Unit) -> Eff _ Unit
animate stateBeh frameStream push = let sampler = sample_ stateBeh frameStream
                                     in subscribe sampler (\s -> (foldM (\state op -> pure $ runOp state op) s [Translate 0.0 1.0, Translate 1.0 0.0]) >>= \newState -> when (not $ newState == s) (push newState)) *> pure unit

initCanvas :: forall e. CanvasElement -> Context2D -> Eff (AllEffs e) Unit
initCanvas c ctx = do
  {event, push} <- create
  let stateBeh = step tree event
      frameInterval = 1000 / 60
      frameStream = interval frameInterval
  setStrokeStyle "#000000" ctx *>
  setupUpdate stateBeh event ctx *>
  animate stateBeh frameStream push

  -- draw first time
  -- first event in the stateBeh
  push tree

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | e) Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
       Just c -> getContext2D c >>= initCanvas c
       Nothing -> log "no canvas found"
