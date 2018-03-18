module Vis.Event where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import FRP.Event (Event)
import Graphics.Canvas (CanvasElement)
import Vis.Types (Effs)

foreign import setOnLoad :: forall e. Eff e Unit -> Eff Effs Unit
foreign import setOnResize :: Event { width :: Number, height :: Number }
foreign import setOnClick :: CanvasElement -> Event {x :: Number, y :: Number}

