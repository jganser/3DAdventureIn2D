module Characters.Female (
  female, sacrificed, moveFemale, moveFemaleOutOfHouse, name,
  getVomitted, vomitTarget
  ) where

import Actor
import Graphics.Proc
import Constants
import Colors
import Objects
import Drawable

female :: (String,Actor)
female = (name, femaleActor)

name = "female"

femaleActor = talkingCharacter femaleGeo femaleDialog

femaleGeo = gSCirc (112,334) 8 0 (rgb 255 192 203) ghostWhite

drawFemale p@(x,y,z) size player@(p1,p2,p3) 
  | p3 /= z = return ()
  | otherwise = do
    strokeWeight 1
    stroke $ ghostWhite
    fill $  rgb 255 192 203
    triangle leftUp rightUp bottom
      where
        bottom = (x, y - size)
        ditsUp = ((sqrt 2)/2) * (size)
        leftUp = (x - ditsUp, y + size)
        rightUp = (x + ditsUp, y + size)

moveFemaleOutOfHouse a = a {actorTick = moveToAndIdle [(100,326,0),(100,300,0)] 40} 

moveFemale :: Actor -> Actor
moveFemale a = a {actorTick = moveToAndIdle [(885,300,0)] 80}

sacrificed :: Actor -> Bool
sacrificed = (==) bossStart . aPos 

getVomitted :: Actor -> Actor
getVomitted a = a { actorTick = moveToAndIdle [vomitTarget] 300}

vomitTarget :: P3
vomitTarget = (840,302,0)

femaleDialog = [
    "Beloved  : I can\'t believe you saved!"
  , "Beloved  : You just came out of nowhere!"
  , "Beloved  : I thought it was all over..."
  , "Beloved  : Waiting to die inside of this thing and all..."
  , "Beloved  : Thank you! I don\'t care how you did!"
  , "Hero     : True love, my dear."
  , "Beloved  : Oh, I love you! Smoothtalker..."
  , "Hero     : I love you, too."
  --todo EPILOG
  ]