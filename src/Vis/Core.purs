module Vis.Core where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Array (foldM, (!!))
import Data.Map (empty)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import FRP.Behavior (Behavior, sample_, step)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Graphics.Canvas (Context2D, Rectangle, clearRect)
import Math (round)
import Vis.Drawable (class Drawable, bound, draw, rotate, scale, translate)
import Vis.Types (AllEffs, AnimationOperation(..), Effs, EndOperation(..), Graphic(..), Gref, MetaData, Shape(..), StateTree(..))
import Vis.Utils (getAnimOps, globalBound, globalClear)

-- | Find bound to redraw and clear the screen with that bound
-- | then draw the stateTree given to it via state behavior
-- TODO: add drawable constraint on a and typecheck
setupUpdate
  :: forall eff a
   . Drawable a
   => Context2D
   -> Behavior (StateTree a MetaData)
   -> Event (StateTree a MetaData)
   -> Eff (AllEffs eff) (Eff (AllEffs eff) Unit)
setupUpdate ctx stateB evStream =
  subscribe sampler
    \s -> (if globalClear s then clearRect ctx globalBound else clearRect ctx (rectBoundToClear $ bound s)) *> draw ctx empty s
      where
        rectBoundToClear {x, y, w, h} = {x: x - 2.0, y: y - 2.0, w: w + 4.0, h: h + 4.0}
        sampler = sample_ stateB evStream

-- | Run Animation operation on any StateTree
-- | and return a new updated StateTree
runOp
  :: forall a
   . Drawable a
   => StateTree a MetaData
   -> AnimationOperation
   -> StateTree a MetaData
runOp (Draw p m s) (Translate x y) = Draw p m (translate x y s)
runOp (Parent v lTree rTree) (Translate x y) = Parent v (translate x y lTree) (translate x y rTree)

runOp (Draw p m s) (Rotate x y ang) = Draw p m (rotate x y ang s)
runOp (Parent v lTree rTree) (Rotate x y ang) = Parent v (rotate x y ang lTree) (rotate x y ang rTree)

runOp (Draw p m s) (Scale x y r) = Draw p m (scale x y r s)
runOp (Parent v lTree rTree) (Scale x y r) = Parent v (scale x y r lTree) (scale x y r rTree)

runOp d@(Draw p m s) op@(Complete (Tuple mainOp (EndPoint x1 y1))) = if ( (round st.x) == x1 && (round st.y) == y1) then d else runOp d mainOp
  where
    st = getShapeState s
    getShapeState (Circle a) = {x: a.x, y: a.y}
    getShapeState (Rect a) = {x: a.x, y: a.y}
    getShapeState (Path arr) = let firstPt = fromMaybe {x:(-1.0), y:(-1.0)} (arr !! 0) in {x: firstPt.x, y: firstPt.y}
runOp d@(Parent v lTree rTree) op@(Complete (Tuple _ (EndPoint x1 y1))) = Parent v (runOp lTree op) (runOp rTree op)

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
      onSubscribe = \s -> foldM (\state -> runOp state >>> pure) s (getAnimOps s) >>= push

-- | Every Animation contains a state
-- | has own update loop where it actually get's drawn
-- | Animation loop where animation operations are done to state
-- | so it has animation pipe line
createAnim
  :: forall a
   . Drawable a
   => Eq a
   => Context2D
   -> StateTree a MetaData
   -> Int
   -> Eff (Effs) (Graphic (Gref a))
createAnim ctx state frameRate = do
  { event, push } <- create
  let stateBeh = step state event
      frameInterval = 1000 / frameRate
      frameStream = interval frameInterval
  animCanceller <- animate stateBeh frameStream push
  updateCanceller <- setupUpdate ctx stateBeh event
  push state -- first time
  pure $ Graphic {animCanceller, updateCanceller, state}
