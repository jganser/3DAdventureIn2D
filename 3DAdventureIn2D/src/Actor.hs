module Actor where

import Drawable
import Geometry
import ObjectUtils
import Graphics.Proc (P3)


data Actor = A {
        geo :: Geometry
      , tick :: Actor -> Actor
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

idle :: Actor -> Actor
idle = id

newTick :: (Actor -> Actor) -> Actor -> Actor
newTick f a = a {tick = f}

moveTo :: (Actor -> Actor) -> P3 -> Float -> Actor -> Actor
moveTo finishedTick target speed a = a {geo = newGeo, tick = newTick}
  where
    pos = aPos a
    dir@(dx,dy,dz) = norm3 $ target - pos
    newPos = pos + dir * (speed,speed,speed)
    newDir@(ndx,ndy,ndz) = norm3 $ target - newPos
    beyondGoal = (dx * ndx + dy * ndy + dz * ndz) < 0
    newGeo = if beyondGoal then newGeoPos target (geo a) else newGeoPos newPos (geo a)
    newTick = if beyondGoal 
              then finishedTick
              else moveTo finishedTick target speed


moveToAndIdle :: P3 -> Float -> Actor -> Actor
moveToAndIdle = moveTo idle

instance Drawable Actor where
    drawAt daytime playerPos a = drawAt daytime playerPos (geo a)
    path z a = path z (geo a)