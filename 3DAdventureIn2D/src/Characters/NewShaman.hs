module Characters.NewShaman (newShaman, moveAside, name, addShamansText) where

import Actor
import Geometry
import Graphics.Proc
import ObjectUtils
import Constants
import DayTime
import Objects
import Colors
import Drawable


newShaman :: (String, Actor)
newShaman = (,) name newShamanActor 

newShamanActor = talkingCharacter newShamanGeo shamansText

newShamanGeo = Geo (T shamanStart (0,0,0) townManSize3) drawShaman pathShaman

name = "newShaman"

drawShaman ::  Transform -> DayTime -> P3 -> Draw
drawShaman t daytime playerPos 
 | (trd playerPos) /=  (trd $ transition t) = return ()
 | otherwise = do
    stroke deepSkyBlue
    fill skyBlue
    strokeWeight 1
    ellipse (centerOf $ transition t) (centerOf $ scaling t)

pathShaman = ellipsePath

moveAside :: Actor -> Actor
moveAside a = a {actorTick = moveToAndIdle [shamanEnd] shamanSpeed}

-- constants

shamanStart :: (Float,Float,Float)
shamanStart = (460,460,0)

shamanEnd :: (Float,Float,Float)
shamanEnd = (477,465,0)

shamanSpeed :: Float
shamanSpeed = 8

-- Text To Say
addShamansText :: Actor -> Actor
addShamansText = addText shamansText2

shamansText :: [String]
shamansText = [
    "Shaman    : My sincerest apologies..."
  , "Shaman    : Your beloved was chosen to satisfy the monster."
  , "Shaman    : If there is anyway I can help you with..."
  ]

-- Text ones the hero has talked to the elder
shamansText2 = [
    "Hero      : I want to follow the steps of the old shaman."
  , "Shaman    : Ofcourse you do... I don\'t want to crush your hopes but..."
  , "Shaman    : I wasn\'t able to find the path of the old shaman."
  , "Shaman    : I never found out how to vanish in the stonehenge."
  , "Shaman    : Good luck, maybe you will find the grace of the totems."
  , "Shaman    : Who knows, it may be your fate to learn the art of vanishing!"
  , "Shaman    : To walk the otherworld!"
  , "Shaman    : I really hope it is. For you and for her..."
  ] -- shaman walks away



