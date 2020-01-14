module Characters.LastLift (lastLift, sendUp, sendDown) where

import Actor
import Geometry
import Graphics.Proc
import Objects hiding (moveTo)
import ObjectUtils
import Constants
import DayTime

lastLift :: Actor
lastLift = A (fullCube (T lastLiftUp (0,0,0) (50,30,5)) green) idle True False False []

idle :: Actor -> Actor
idle = id

sendUp :: Actor -> Actor
sendUp a = a { tick = moveTo lastLiftUp 1}

sendDown :: Actor -> Actor
sendDown a = a { tick = moveTo lastLiftDown 1}

moveTo :: P3 -> Float -> Actor -> Actor
moveTo target speed a = a {geo = newGeo, tick = newTick}
  where
    pos = aPos a
    dir@(dx,dy,dz) = norm3 $ target - pos
    newPos = pos + dir * (speed,speed,speed)
    newDir@(ndx,ndy,ndz) = norm3 $ target - newPos
    beyondGoal = (dx * ndx + dy * ndy + dz * ndz) < 0
    newGeo = if beyondGoal then newGeoPos target (geo a) else newGeoPos newPos (geo a)
    newTick = if beyondGoal 
              then idle
              else moveTo target speed
