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

instance Show Player where
  show p = 
    "P { standsOnActor: " ++ show (standsOnActor p) ++
    ", pos: " ++ show (pos p) ++
    ", isTalking: " ++ show (isTalking p) ++
    ", isFalling: " ++ show (isFalling p) ++
    ", dialog: " ++ show (dialog p) ++ "}"
    
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

stopTalking :: Player -> Player
stopTalking p = p { isTalking = False }

hasNextLine :: Player -> Bool
hasNextLine = (/=) [] . dialog

nextTextLine :: Player -> (String,Player)
nextTextLine p = (nextLine, p_2)
  where
    text = dialog p
    nextLine = if text == [] then "---> No Dialog remains <---" else head text
    remainingText = drop 1 text
    p_1 = p { dialog = remainingText}
    p_2 = if hasNextLine p_1 then p_1 else stopTalking p_1
  
updatePlayerOnActor :: Player -> EventActors -> Player
updatePlayerOnActor p@(P (Just n) _ xyz _ _ _) ea = 
  p { standsOnActor = onEventActorCheck n xyz ea }
updatePlayerOnActor p@(P Nothing _ xyz _ _ _) ea = 
  p { standsOnActor = onAnyEventActorCheck xyz ea}



