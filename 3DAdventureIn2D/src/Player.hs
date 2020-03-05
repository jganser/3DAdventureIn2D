module Player where

import Actor
import GameState
import Data.Maybe
import Graphics.Proc
import Constants
import ObjectUtils

data Player = P {
        standsOnActor :: Maybe String
      , drawP :: P3 -> Draw
      , pos :: P3
      , isTalking :: Bool
      , isFalling :: Bool
      , dialog :: [String]
    }
    
newPlayer :: P3 -> Player
newPlayer pos = P Nothing drawPlayer pos False False []

drawPlayer :: P3 -> Draw
drawPlayer pos = do
  strokeFill $ blue
  strokeWeight 1
  ellipse (centerOf $ pos) (playerSize, playerSize)

updatePos :: P3 -> Player -> Player
updatePos npos player = player { pos = npos }

movePlayer :: P3 -> Player -> Player
movePlayer dir p = updatePos (pos p + dir) p

updatePlayerOnActor :: Player -> EventActors -> Player
updatePlayerOnActor p@(P (Just n) _ xyz _ _ _) ea = 
  p { standsOnActor = onEventActorCheck n xyz ea }
updatePlayerOnActor p@(P Nothing _ xyz _ _ _) ea = 
  p { standsOnActor = onAnyEventActorCheck xyz ea}



