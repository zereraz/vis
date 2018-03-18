module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Foldable (foldM)
import Data.Map (empty, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import FRP (FRP)
import FRP.Behavior (Behavior, step)
import FRP.Event (Event)
import Graphics.Canvas (Arc, CANVAS, CanvasElement, Context2D, arc, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setStrokeStyle, stroke)
import Math (atan, cos, pi, pow, sin, sqrt)
import Vis.Core (createAnim)
import Vis.Drawable (class Drawable)
import Vis.Event (setOnClick, setOnLoad, setOnResize)
import Vis.Types (AnimationOperation(..), Effs, EndOperation(..), Graphic(..), Gref, MetaData(..), Shape(..), StateTree, StateTree(..), Point)
import Vis.Utils (circle, defaultMetaData, defaultProps, tree)

init :: Eff Effs Unit
init = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
       Just c -> getContext2D c >>= initCanvas c
       Nothing -> log "no canvas found"

type Dynamic a = {
    beh :: Behavior a
  , ev :: Event a
}

type GlobalStreams = {
    click :: Dynamic {x :: Number, y :: Number}
  , resize :: Dynamic {width :: Number, height :: Number}
  {-- , drag :: Dynamic {x :: Number, y :: Number} --}
}

initGlobalListeners :: CanvasElement -> Eff Effs GlobalStreams
initGlobalListeners c = do
  _ <- setOnLoad onLoad
  let resizeEv = setOnResize
      resizeBeh = step {width: -1.0, height: -1.0} resizeEv
      clickEv = setOnClick c
      clickBeh = step {x: -1.0, y: -1.0} clickEv
  pure {click: {beh: clickBeh, ev: clickEv}, resize: {beh: resizeBeh, ev: resizeEv}}


onLoad :: Eff Effs Unit
onLoad = do
  log "on load!"

initCanvas :: CanvasElement -> Context2D ->  Eff Effs Unit
initCanvas c ctx = do
  globalStream <- initGlobalListeners c
  width <- getCanvasWidth c
  height <- getCanvasHeight c
  _ <- arc ctx {x:width/2.0,y:height/2.0,r:40.0,start:0.0,end:2.0 * pi}
  _ <- setStrokeStyle "black" ctx
  _ <- stroke ctx
  pure unit


main :: Eff Effs Unit
main = do
  init

