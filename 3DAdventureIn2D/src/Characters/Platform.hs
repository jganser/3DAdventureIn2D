module Characters.Platform where

import Actor
import Constants
import Colors
import Objects
import Graphics.Proc
import Geometry

hengePlatform = ("hengePlatform", movingActor [hengePlatformStart, hengePlatformEnd] platformSpeed (fullCube (T hengePlatformStart (0,0,0) hengePlatformSize) green) 2) -- platform to henge

plazaPlatform = ("plazaPlatform", movingActor [plazaPlatformStart, plazaPlatformEnd] platformSpeed (fullCube (T plazaPlatformStart (0,0,0) plazaPlatformSize) green) 3) -- main platform

roundTripPlatform = ("roundTripPlatform", movingActor [(100,205,20), (80,50,24), (220,30,16), (200,225,20)] platformSpeed (ellipsoide (T (100,205,20) (0,0,0) (20,40,4)) seaGreen) 2) -- 3rd level round trip platform