module Characters.LastLift (lastLift, sendUp, sendDown) where

import Actor
import Geometry
import Graphics.Proc
import Objects hiding (moveTo)
import ObjectUtils
import Constants
import DayTime

lastLift :: Actor
lastLift = A (fullCube (T lastLiftUp (0,0,0) (50,30,5)) green) idle True False False [] False

sendUp :: Actor -> Actor
sendUp = sendTo lastLiftUp

sendDown :: Actor -> Actor
sendDown = sendTo lastLiftDown

sendTo :: P3 -> Actor -> Actor
sendTo target a = a { tick = moveTo idle target liftSpeed}

