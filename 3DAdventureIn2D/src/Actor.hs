module Actor where

import Drawable
import Geometry
import ObjectUtils
import Graphics.Proc (P3)
type Tick = Float -> Actor -> Actor

data Actor = A {
        geo :: Geometry
      , tick :: Tick
      , moving :: Bool
      , talking :: Bool
      , finishedTalking :: Bool
      , textToSay :: [String]
      , blocksPlayer :: Bool
    }

    
pIsOnActor :: Actor -> (Float,Float,Float) -> Bool
pIsOnActor a p@(x,y,z) = path z a /= []

aPos a = transition $ transform $ geo a
azPos = trd . aPos

idle :: Tick
idle = const id

newTick :: Tick -> Actor -> Actor
newTick f a = a {tick = f}

moveTo :: Tick -> P3 -> Float -> Tick
moveTo finishedTick target speed deltaT a = a {geo = newGeo, tick = newTick}
  where
    pos = aPos a
    dir@(dx,dy,dz) = norm3 $ target - pos
    v = speed * deltaT
    newPos = pos + dir * (v,v,v)
    newDir@(ndx,ndy,ndz) = norm3 $ target - newPos
    beyondGoal = (dx * ndx + dy * ndy + dz * ndz) < 0
    newGeo = if beyondGoal then newGeoPos target (geo a) else newGeoPos newPos (geo a)
    newTick = if beyondGoal 
              then finishedTick
              else moveTo finishedTick target speed

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
idleFor x nextTick deltaT a = a { tick = newTick}
  where
    newTick = if x - deltaT < 0 then nextTick else idleFor (x-deltaT) nextTick

-- idleFor x seconds at the end of each movement
moveBetweenAndIdleFor :: Float -> P3 -> P3 -> Float -> Tick
moveBetweenAndIdleFor x start end speed = idleFor x moveToEnd
  where
    moveToEnd = moveTo (idleFor x moveToStart) end speed
    moveToStart = moveTo (idleFor x moveToEnd) start speed


instance Drawable Actor where
    drawAt daytime playerPos a = drawAt daytime playerPos (geo a)
    path z a = if blocksPlayer a then [] else path z (geo a)