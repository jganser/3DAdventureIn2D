module Levels where

import Objects
import Constants
import Geometry
import Graphics.Proc


type Level = [Geometry]



level1  :: Level
level1 = [
    sphere (200,200,50) 200 red,
    sphere (100,200,0) 450 blue,
    ellipsoide (T (300,100,0) (0,0,0) (50,100,20)) green,
    cube (T (300,275,50) (0,0,0) (100,150,100))  5 green,
    cube (T (50,225,50) (0,0,0) (200, 50,120)) 12 blue,
    fullCube (T (200,200,50) (0,0,0) (100,  150, 120)) (rgb 128 240 240),
    fullCube (T (200,300,50) (0,0,0) (400, 50, 120)) (rgb 240 128 128),
    cylinder (T (250,250,250) (0,0,0) (100,100,100)) olive,
    henge (T hengePos (0,0,0) (200,200,200)),
    --lines 
    gLine (px,py) (bx,by) 0 5 black,
    gLine ( 50, 50) (100,200)  0 12 black,
    gLine (150,150) (400,200)  0  5 red,
    
    ]
    
level2 :: Level
level2 = [
      gLine ( 50, 50) (200,100) 10 20 yellow
    , gLine (150,150) (300,200) 10  4 lime
    ]

ps = playerStart
bs = bossStart

level1Actors = [
    --boss
    boss (T bossStart (0,0,0) (bossSize,bossSize,bossSize))
    ]
