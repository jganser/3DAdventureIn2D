module Player where

import Actor
import Data.Maybe
import Graphics.Proc
import Constants
import ObjectUtils

data Player = P {
        standsOnActor :: Maybe Actor
      , drawP :: P3 -> Draw
      , pos :: P3
      , isTalking :: Bool
      , isFalling :: Bool
    }
    
newPlayer :: P3 -> Player
newPlayer pos = P Nothing drawPlayer pos False False

drawPlayer :: P3 -> Draw
drawPlayer pos = do
        strokeFill $ blue
        strokeWeight 1
        ellipse (centerOf $ pos) (playerSize, playerSize)

updatePos :: P3 -> Player -> Player
updatePos npos player = player { pos = npos }

