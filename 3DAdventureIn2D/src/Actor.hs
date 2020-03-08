module Actor where

import Drawable
import Geometry
import ObjectUtils
import Graphics.Proc (P3)


type Tick = Float -> Actor -> Actor

class Tickable a where
  tick :: Float -> a -> a

data Actor = A {
    geo :: Geometry
  , actorTick :: Tick
  , moving :: Bool
  , talking :: Bool
  , finishedTalking :: Bool
  , textToSay :: [String]
  , blocksPlayer :: Bool
}

instance Tickable Actor where
  tick dt a = (actorTick a) dt a

instance Show Actor where
  show a = "Actor { "
    ++ show (geo a) 
    ++ ", moving: " ++ show (moving a) 
    ++ ", talking: " ++ show (talking a)
    ++ ", finishedTalking: " ++ show (finishedTalking a)
    ++ ", nextLine: " ++ show (take 1 $ textToSay a)
    ++ ", blocksPlayer: " ++ show (blocksPlayer a)
    ++ " }"

emptyActor :: Actor
emptyActor = A emptyGeo idle False False False [] False 

talkingCharacter :: Geometry -> [String] -> Actor
talkingCharacter geometry text = 
  let a = emptyActor
  in a { geo = geometry, talking = True, textToSay = text, blocksPlayer = True}
    
pIsOnActor :: Actor -> (Float,Float,Float) -> Bool
pIsOnActor = flip onActorCheck

onActorCheck :: P3 -> Actor -> Bool
onActorCheck (x,y,z) a = 
  x >= x_low b && x <= x_high b && -- lazy evaluation may stop before calculating the real path
  y >= y_low b && y <= y_high b &&
  z >= z_low b && z <= z_high b &&
  (x,y) `elem` path z a -- is only checked if the player is in the bounding box of the actor
    where
      b = boundingBox a

onActorsCheck :: P3 -> [Actor] -> (Actor,Bool)
onActorsCheck pos [] = (emptyActor, False)
onActorsCheck pos (a:as) = 
  if onActorCheck pos a then (a,True) 
  else onActorsCheck pos as

aPos :: Actor -> P3
aPos = transition . transform . geo

azPos :: Actor -> Float
azPos = trd . aPos

aSize :: Actor -> P3
aSize = scaling . transform . geo

idle :: Tick
idle = const id

newTick :: Tick -> Actor -> Actor
newTick f a = a {actorTick = f}

moveTo :: Tick -> P3 -> Float -> Tick
moveTo finishedTick target speed deltaT a = a {geo = newGeo, actorTick = newTick}
  where
    pos = aPos a
    dir@(dx,dy,dz) = norm3 $ target - pos
    v = speed * deltaT
    --newPos = pos + roundP3 (dir * (v,v,v/10))
    newPos = pos + roundXY (dir * (v,v,v/10))
    --newPos = pos + dir * (v,v,v/10)
    newDir@(ndx,ndy,ndz) = norm3 $ target - newPos
    beyondGoal = (dx * ndx + dy * ndy + dz * ndz) <= 0
    newGeo = if beyondGoal then newGeoPos target (geo a) else newGeoPos newPos (geo a)
    newTick = if beyondGoal 
              then finishedTick
              else moveTo finishedTick target speed

addText :: [String] -> Actor -> Actor
addText [] a = a
addText text a = a { textToSay = textToSay a ++ text, finishedTalking = False}

talk :: Actor -> (Maybe String, Actor)
talk a | not (finishedTalking a) = 
  if tail (textToSay a) == [] 
  then (Just (head (textToSay a)), a {textToSay = [], finishedTalking = True})
  else (Just (head (textToSay a)), a {textToSay = tail (textToSay a)})
  | otherwise = (Nothing, a)

moveToAndIdle :: P3 -> Float -> Tick
moveToAndIdle = moveTo idle

-- idleFor x seconds
idleFor :: Float -> Tick -> Tick
idleFor x nextTick deltaT a = a { actorTick = newTick}
  where
    newTick = if x - deltaT < 0 then nextTick else idleFor (x-deltaT) nextTick

-- idleFor x seconds at the end of each movement
moveBetweenAndIdleFor :: Float -> [P3] -> Float -> Tick
moveBetweenAndIdleFor x points speed = idleFor x $ moveToNext points
  where
    moveToNext points = moveTo (idleFor x (moveToNext newOrder)) (head newOrder) speed
      where
        newOrder = tail points ++ [head points]

lookupActor :: String -> [(String,Actor)] -> Actor
lookupActor n  [] = emptyActor
lookupActor n ((tn,a):as) 
  | n == tn = a
  | otherwise = lookupActor n as

replaceActor :: (String,Actor) -> [(String,Actor)] -> [(String,Actor)]
replaceActor (n,a) = map (\(tn,an) -> if tn == n then (n,a) else (tn,an))
    
instance Drawable Actor where
    drawAt daytime playerPos a = drawAt daytime playerPos (geo a)
    path z a = path z (geo a)
    boundingBox = boundingBox . geo

allActorPath :: Float -> [Actor] -> Path
allActorPath z = concatMap $ path z

-- Actors

-- An Actor that moves between two points and idles for a set amount of secs
-- between these movements
-- @points list of stations for this actor
-- @speed speed of the movement
-- @geo   Geometry that is moving
-- @x     seconds to idle after reaching start or end point
movingActor :: [P3] -> Float -> Geometry -> Float-> Actor
movingActor points speed geo x = A geo (moveBetweenAndIdleFor x points speed) True False False [] False

