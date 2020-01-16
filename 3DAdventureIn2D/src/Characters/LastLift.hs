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

sendUp :: Actor -> Actor
sendUp a = a { tick = moveTo lastLiftUp 1}

sendDown :: Actor -> Actor
sendDown a = a { tick = moveTo lastLiftDown 1}

