module Characters.Female where

import Actor
import Graphics.Proc
import Constants

female :: (String,Actor)
female = ("female", emptyActor)--todo

drawFemale p@(x,y,z) size player@(p1,p2,p3) 
  | p3 /= z = return ()
  | otherwise = do
    strokeFill $ rgb 255 192 203 
    triangle leftUp rightUp bottom
      where
        bottom = (x, y - size)
        ditsUp = ((sqrt 2)/2) * (size)
        leftUp = (x - ditsUp, y + size)
        rightUp = (x + ditsUp, y + size)

goToMonster :: Actor -> Actor
goToMonster = newTick $ moveToAndIdle bossStart 3

sacrificed :: Actor -> Bool
sacrificed = (==) bossStart . aPos 