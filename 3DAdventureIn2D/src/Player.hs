module Player where

import Actor
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

movePlayer :: P3 -> Player -> Player
movePlayer dir p = updatePos (pos p + dir) p

updateStandsOnActor :: Player -> (String,Actor) -> Player
updateStandsOnActor p@(P (Just _) _ _ _ _)  _ = p -- player already stands on an actor | will mostlikely never be called...
updateStandsOnActor p@(P Nothing _ xyz _ _) (n,a) = 
  if onActorCheck xyz a  then p {standsOnActor = Just n}
  else p

updatePlayerOnActor :: Player -> [(String,Actor)] -> Player
updatePlayerOnActor p [] = p
updatePlayerOnActor p@(P (Just n) _ xyz _ _) l = 
  if onActorCheck xyz (lookupActor n l) then p 
  else p { standsOnActor = Nothing }
updatePlayerOnActor p@(P Nothing _ xyz _ _) ((n,a):as) = 
  if onActorCheck xyz a then p {standsOnActor = Just n}
  else updatePlayerOnActor p as



