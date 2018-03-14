module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import Data.Foldable (foldM)
import Data.Map (empty, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CanvasElement, Context2D, Arc, getCanvasElementById, getContext2D)
import Math (atan, cos, pow, sin, sqrt)
import Vis.Core (createAnim)
import Vis.Drawable (class Drawable)
import Vis.Types (AnimationOperation(..), Effs, EndOperation(..), Graphic(..), Gref, MetaData(..), Shape(..), StateTree, StateTree(..), Point)
import Vis.Utils (circle, defaultMetaData, defaultProps, tree)

init :: Eff Effs Unit
init = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
       Just c -> getContext2D c >>= initCanvas c
       Nothing -> log "no canvas found"

mkCircle :: forall a. Array AnimationOperation -> Number -> Number -> Number -> StateTree (Shape a) MetaData
mkCircle animOps x y =  Draw defaultProps (defaultMetaData animOps) <<<  circle x y

getPoint :: forall a. Graphic (Gref a) -> Point
getPoint (Graphic gref) = getState gref.state
  where
    getState (Draw p b s) = unwrapShape s
    getState (Parent v lTree rTree) = {x: (lpt.x + rpt.x) / 2.0, y: (lpt.y + rpt.y) / 2.0}
      where
        lpt = getState lTree
        rpt = getState rTree
    unwrapShape (Circle s) = {x: s.x, y: s.y}
    unwrapShape (Rect r) = {x: r.x, y: r.y}
    unwrapShape _ = {x: (-1.0), y: (-1.0)}

getSlope :: Point -> Point -> Number
getSlope to from = (to.y - from.y) / (to.x - from.x)

-- | Create stateStream - stream of events for any new state
-- | Setup update loop and animation operations loop
initCanvas :: CanvasElement -> Context2D -> Eff Effs Unit
initCanvas c ctx = do
-- | graphic variables containing their own animation cycle and update cycle
  let arrPoints = [{x: 50.0, y: 350.0, r: 10.0, id: "1"}, { x: 50.0, y: 250.0, r: 10.0, id: "2"}, {x: 140.0, y: 250.0, r: 10.0, id: "3"}, { x: 210.0, y: 50.0, r: 10.0, id: "4"}]
      animatePointsFrom = [{to: "1", from: "2"}, {to:"4", from:"3"}]
  refMap <- foldM (\pointMap p -> createAnim ctx (mkCircle [] p.x p.y p.r) 60 >>= (flip (insert p.id) pointMap) >>> pure) empty arrPoints
  _ <- traverse (\p -> let maybeG1 = lookup p.to refMap
                           maybeG2 = lookup p.from refMap
                       in case [maybeG1, maybeG2] of
                            [Just g1, Just g2] ->
                              let to = getPoint g1
                                  from = getPoint g2
                                  slope = getSlope to from
                                  theta = atan slope
                               in createAnim ctx (mkCircle [Complete (Tuple (Translate (cos theta) (sin theta)) (EndPoint to.x to.y))] from.x from.y 3.0) 60 *> pure unit

                            _                 -> pure unit) animatePointsFrom
  pure unit


main :: Eff (Effs) Unit
main = do
  init
