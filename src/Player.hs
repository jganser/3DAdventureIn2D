module Player where

import Actor
import GameState
import Data.Maybe
import Graphics.Proc
import Constants
import Colors
import ObjectUtils
import qualified Characters.LastLift as LastLift

data Player = P {
        standsOnActor :: Maybe String
      , drawP :: P3 -> Draw
      , pos :: P3
      , isTalking :: Bool
      , isFalling :: Bool
      , dialog :: [String]
      , target :: P3
    }

instance Show Player where
  show p = 
    "P { standsOnActor: " ++ show (standsOnActor p) ++
    ", pos: " ++ show (pos p) ++
    ", isTalking: " ++ show (isTalking p) ++
    ", isFalling: " ++ show (isFalling p) ++
    ", dialog: " ++ show (dialog p) ++ "}"
    
newPlayer :: P3 -> Player
newPlayer pos = P Nothing drawPlayer pos False False [] pos

drawPlayer :: P3 -> Draw
drawPlayer = drawPlayerWithSize playerSize

drawPlayerWithSize :: Float -> P3 -> Draw
drawPlayerWithSize size pos = do  
  stroke $ azure
  fill $ blue
  strokeWeight 1
  ellipse (centerOf $ pos) (size, size)


updatePos :: P3 -> Player -> Player
updatePos npos player = player { pos = npos }

movePlayer :: P3 -> Player -> Player
movePlayer dir p = updatePos (pos p + dir) p

stopTalking :: Player -> Player
stopTalking p = p { isTalking = False }

startTalkingText :: [String] -> Player -> Player
startTalkingText text p =  p {isTalking = True, dialog = text}

startTalking :: Actor -> Player -> Player
startTalking a = startTalkingText $ textToSay a

hasNextLine :: Player -> Bool
hasNextLine = (/=) [] . dialog

nextTextLine :: Player -> (String,Player)
nextTextLine p = if hasNextLine p then (nextLine, p_1) else (nextLine, stopTalking p)
  where
    text = dialog p
    nextLine = if text == [] then "---> No Dialog remains <---" else head text
    remainingText = drop 1 text
    p_1 = p { dialog = remainingText}
  
updatePlayerOnActor :: Player -> EventActors -> Player
updatePlayerOnActor p@(P (Just n) _ xyz _ _ _ _) ea = 
  p { standsOnActor = onEventActorCheck n xyz ea }
updatePlayerOnActor p@(P Nothing _ xyz _ _ _ _) ea = 
  p { standsOnActor = onAnyEventActorCheck xyz ea}

isPlayerOnHengeLift :: Player -> Bool
isPlayerOnHengeLift = 
  (==) (Just LastLift.hengeLiftName) . standsOnActor

isPlayerOnLastLiftUp :: Player -> Bool
isPlayerOnLastLiftUp p = 
  ((==) (Just LastLift.lastLiftName) . standsOnActor) p &&
  ((==) (trd LastLift.lastLiftUp) . trd . pos) p

isPlayerOnLastLiftDown :: Player -> Bool
isPlayerOnLastLiftDown p = 
  ((==) (Just LastLift.lastLiftName) . standsOnActor) p &&
  ((==) (trd LastLift.lastLiftDown) . trd . pos) p

getKicked :: Player -> Player
getKicked p = 
  p { isFalling = True, 
      target = pos p + (0,5,-20), 
      pos = pos p + (0,5,0) }

getVomitted :: Player -> Player
getVomitted p = 
  p { isFalling = True, 
      target = playerVomitTarget }

playerVomitTarget :: P3
playerVomitTarget = (846,297,0)

-- Monologs
sacrifice :: [String]
sacrifice = [
    "Hero      : I coudn\'t stop her. Why does live challenge us this way?"
  , "Hero      : I won\'t accept this I need to find a solution!"
  , "Hero      : Wasn\'t the monster chased away once in the history of village?"
  , "Hero      : Maybe one of the elders knows about it."
  , "Hero      : They live in the round natural huts. Left of the big plaza."
  , "Hero      : Behind the main platform."
  , "Hero      : Or maybe I could just take the straight line."
  , "Hero      : And fight the monster head on for her..."
  ]

onPlatformWithoutFlowers :: [String]
onPlatformWithoutFlowers = [
    "Hero      : This is the place where the old shaman learned to vanish!"
  , "Hero      : It is said that this henge has a deep connection to the other world."
  , "Hero      : I wonder how he did it... "
  , "Hero      : This place feels really old. "
  , "Hero      : Like on can see the the history of rituals."
  , "Hero      : The looks like the old rituals included some flowers..."
  , "Hero      : Most likely as sacrifices."
  , "Hero      : I wonder if the old flower lady knows more about this."
  , "Hero      : She is living right under the monster, behind the shamans hut."
  ]

