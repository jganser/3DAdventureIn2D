module Characters.LastLift (lastLift, hengeLift, sendHengeLiftUp, sendUp, sendDown) where

import Actor
import Geometry
import Graphics.Proc
import Objects hiding (moveTo)
import ObjectUtils
import Colors
import DayTime
import Constants

lastLift :: (String,Actor)
lastLift = (,) "lastLift" $ A (fullCube (T lastLiftUp (0,0,0) lastLiftSize) green) idle True False False [] False

sendUp :: Actor -> Actor
sendUp = sendToWithIdle lastLiftUp

sendDown :: Actor -> Actor
sendDown = sendToWithIdle lastLiftDown

sendToWithIdle :: P3 -> Actor -> Actor
sendToWithIdle = sendTo idle liftSpeed

sendTo :: Tick -> Float -> P3 -> Actor -> Actor
sendTo newTick speed target a = a { actorTick = moveTo newTick target speed}

hengeLift :: (String,Actor)
hengeLift = (,) "hengeLift" $ A (cylinder (T hengeLiftLow (0,0,0) hengeLiftSize) darkSlateGray 1) idle True False False [] False

sendHengeLiftUp :: Actor -> Actor
sendHengeLiftUp = sendToWithIdle hengeLiftUp


-- constants

liftSpeed :: Float
liftSpeed = 40

lastLiftSize :: (Float,Float,Float)
lastLiftSize = (60,30,3)

lastLiftUp :: (Float,Float,Float)
lastLiftUp = (820,300,20)

lastLiftDown :: (Float,Float,Float)
lastLiftDown = (255,300,10)

hengeLiftSize :: (Float,Float,Float)
hengeLiftSize = (26,26,6)

hengeLiftLow :: (Float,Float,Float)
hengeLiftLow = hengePos

hengeLiftUp :: (Float,Float,Float)
hengeLiftUp = (100,600,20)