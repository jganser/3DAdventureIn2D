module Player where

import Actor
import Data.Maybe
import Graphics.Proc

data Player = P {
        standsOnActor :: Maybe Actor
      , drawP :: P3 -> Draw
      , pos :: P3
      , isTalking :: Bool
    }
    

