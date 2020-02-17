module Characters.LastLift (lastLift, hengeLift, sendHengeLiftUp, sendUp, sendDown) where

import Actor
import Geometry
import Graphics.Proc
import Objects hiding (moveTo)
import ObjectUtils
import Constants
import Colors
import DayTime

lastLift :: Actor
lastLift = A (fullCube (T lastLiftUp (0,0,0) lastLiftSize) green) idle True False False [] False

sendUp :: Actor -> Actor
sendUp = sendToWithIdle lastLiftUp

sendDown :: Actor -> Actor
sendDown = sendToWithIdle lastLiftDown

sendToWithIdle :: P3 -> Actor -> Actor
sendToWithIdle = sendTo idle liftSpeed

sendTo :: Tick -> Float -> P3 -> Actor -> Actor
sendTo newTick speed target a = a { tick = moveTo newTick target speed}

hengeLift :: Actor
hengeLift = A (cylinder (T hengeLiftLow (0,0,0) hengeLiftSize) darkSlateGray 1) idle True False False [] False

sendHengeLiftUp :: Actor -> Actor
sendHengeLiftUp = sendToWithIdle hengeLiftUp