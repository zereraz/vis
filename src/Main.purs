module Main where

import Drawable
import Prelude
import Types

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
import Utils (tree, subTree, defaultMetaData)

-- | Find bound to redraw and clear the screen with that bound
-- | then draw the stateTree given to it via state behavior
-- TODO: add drawable constraint on a and typecheck
setupUpdate
  :: forall eff a.
   Behavior (StateTree (Shape a) MetaData)
   -> Event (StateTree (Shape a) MetaData)
   -> Context2D
   -> Eff (AllEffs eff) (Eff (AllEffs eff) Unit)
setupUpdate stateB evStream ctx =
  subscribe sampler
    \s -> clearRect ctx (rectBoundToClear (getBound s)) *> draw ctx s
      where
        rectBoundToClear {x, y, w, h} = {x: x - 2.0, y: y - 2.0, w: w + 4.0, h: h + 4.0}
        sampler = sample_ stateB evStream

-- | Run Animation operation on any StateTree
-- | and return a new updated StateTree
runOp
  :: forall a.
   Drawable a
   => StateTree a MetaData
   -> AnimationOperation
   -> StateTree a MetaData
runOp (Draw s) (Translate x y) = Draw (translate x y s)
runOp (Parent v lTree rTree) (Translate x y) = Parent v (translate x y lTree) (translate x y rTree)
runOp s _ = s

-- | At each event/tick of frameStream perform all animOperations
-- | Check if state changed, if changed push the new state to stateStream
animate
  :: forall a eff
   . Drawable a
   => Eq a
   => (Behavior (StateTree a MetaData))
   -> Event Int
   -> (StateTree a MetaData -> Eff (AllEffs eff) Unit)
   -> Eff (AllEffs eff) (Eff (AllEffs eff) Unit)
animate stateB frameStream push =
  let sampler = sample_ stateB frameStream
      subscriber = subscribe sampler
  in subscribe sampler onSubscribe
    where
      -- | Send new state event when state changed
      updateIfChanged newState oldState = when (not $ newState == oldState) (push newState) *> pure newState
      -- | Perform each operation and update
      onSubscribe = \s -> let animOperations = [Translate 0.0 1.0, Translate 1.0 0.0]
                          in foldM (\state op -> updateIfChanged (runOp state op) state) s animOperations

-- | Create stateStream - stream of events for any new state
-- | Setup update loop and animation operations loop
-- TODO: use cancellers or register them somewhere
initCanvas :: forall e. CanvasElement -> Context2D -> Eff (AllEffs e) Unit
initCanvas c ctx = do
  {event, push} <- create
  let stateBeh = step tree event
      frameInterval = 1000 / 60
      frameStream = interval frameInterval
  setStrokeStyle "#000000" ctx *>
  animate stateBeh frameStream push *>
  setupUpdate stateBeh event ctx *>

  -- draw first time
  -- first event in the stateBeh
  push tree

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS, frp :: FRP | e) Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
       Just c -> getContext2D c >>= initCanvas c
       Nothing -> log "no canvas found"
