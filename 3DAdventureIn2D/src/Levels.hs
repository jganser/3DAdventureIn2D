module Levels where

import Objects
import Constants
import Geometry
import Graphics.Proc
import Characters.LastLift
import Characters.OldShaman
import Characters.Female
import Colors

type Level = [Geometry]


level1  :: Level
level1 = [
    --lines 
      gLine ( 80,300) (860,300) 0 14 darkSaddleBrown -- main street to boss
    , gLine (100,300) (100,320) 0 8 darkSaddleBrown -- street to the house of the hero    
    , gLine (160,150) (160,100) 0 8 darkSaddleBrown -- street behind main platform
    , gLine (120,100) (180,100) 0 8 darkSaddleBrown -- T Street after main platform
    , gLine (260,100) (360,100) 0 8 darkSaddleBrown -- Street to forum
    , gLine (410,120) (435,120) 0 6 firebrick -- bridge to plaza on Forum -> left
    , gLine (460, 70) (460, 95) 0 6 firebrick -- bridge to plaza on Forum -> up
    , gLine (460,145) (460,170) 0 6 firebrick -- bridge to plaza on Forum -> down
    , gLine (485,120) (510,120) 0 6 firebrick -- bridge to plaza on Forum -> right
    , gLine (460,300) (460,600) 0 8 darkSaddleBrown -- street down leading to henge
    , gLine (400,600) (460,600) 0 8 darkSaddleBrown -- street to the left leading to henge
    , gLine (110,600) (200,600) 0 8 darkSaddleBrown -- street left leading onto the henge lift
    , gLine (540,460) (640,460) 0 8 darkSaddleBrown -- street behind shaman hud
    , gLine (640,460) (640,580) 0 8 darkSaddleBrown -- street down behind shaman hud
    , gLine (640,500) (800,500) 0 8 darkSaddleBrown -- street to the house under the boss
    , gLine (580,600) (615,600) 0 8 darkSaddleBrown -- street at the right low left
    , gLine (665,600) (700,600) 0 8 darkSaddleBrown -- street at the right low right
    -- 2D Objects
    , gFullRect (100,340) (40,40) 0 1 firebrick -- house of the hero
    , gFullRect (385,120) (50,200) 0 1 royalBlue -- Forum part => left   x = 360 - 410, y =  20 - 220
    , gFullRect (460,45) (100,50) 0 1 royalBlue  -- Form part  => up     x = 410 - 510, y =  20 -  70
    , gFullRect (460,195) (100,50) 0 1 royalBlue -- Form part  => down   x = 410 - 510, y = 170 - 220
    , gFullRect (535,120) (50,200) 0 1 royalBlue -- Form part  => right  x = 510 - 560, y =  20 - 220
    , gCirc (460,120) 50 0 gold -- Forum Plaza                           x = 435 - 485, y =  95 - 145
    , gEllipse (40,100) (60,120) 0 cadetBlue -- upper left hud
    , gCirc (500, 460) 80 0 firebrick -- house of the shaman
    , gFullRect (840,520) (80,60) 0 1 firebrick -- fullrect under boss
    , gCirc (570,600) 20 0 cadetBlue -- low left hud
    , gEllipse (730,600) (60,40) 0 cadetBlue -- upper left hud           x = 700 - 760, y =  580 - 620
    ]
    
threeDs :: Level
threeDs = [
    fullCube (T (220,100,0) (0,0,0) (80,  40, 22)) goldenRod -- house after main platform in dircetion of forum
  , cylinder (T (105,100,0) (0,0,0) (30,30,25)) olive 1 -- first round cylindric hud after main platform left
  , cylinder (T (80,100,0) (0,0,0) (30,30,30)) seaGreen 1 -- second round cylindric hud after main platform left
  , henge (T hengePos (0,0,0) (200,200,100))
  , fullCube (T (240,275,18) (0,0,0) (10,50,36)) darkSaddleBrown -- street to main platform
  , fullCube (T (640,600,10) (0,0,0) (50,40,36)) seaGreen -- cube at the bottom right
  , ellipsoide (T (400,100,10) (0,0,0) (40,40,6)) seaGreen -- sphere
  , cube (T (300,510,18) (0,0,0) (300,12,6)) 6 paleGoldenRod -- open cube in 3rd level, that shall be visible whle going up
  ]

level2 :: Level
level2 = [
    --lines 
      gLine (255,230) (400,230) 10 8 darkSaddleBrown -- street to the right from  mainplatform
    , gLine (400,230) (400,110) 10 8 darkSaddleBrown -- street up
    , gLine (390,100) (260,100) 10 8 darkSaddleBrown -- street left form ellipsoide
    , gLine (120,100) (180,100) 10 8 darkSaddleBrown -- street to the huds
    , gLine ( 80,115) (80,200) 10 8 darkSaddleBrown -- street to old Shaman hud
    -- 2d objects
    , gCirc (80,250) 100 10 darkSlateGray -- old shaman hud
    ]

level3 :: Level
level3 = [
    gLine (450,570) (450,520) 20 10 sienna
  , gLine (50,500) (150,500) 20 10 sienna
  , gLine (50,500) (50,350) 20 8 sienna
  , gLine (50,250) (100,250) 20 12 sienna
  , gLine (100,250) (100,220) 20 12 sienna
  , gLine (200,220) (240,220) 20 10 seaGreen
  , gLine (240,220) (240,250) 20 10 seaGreen
  , gLine (243,260) (300,260) 20  8 seaGreen
  , gLine (300,260) (300,320) 20  8 seaGreen
  , gLine (300,320) (330,320) 20 10 seaGreen
  , gLine (330,315) (330,290) 20 10 seaGreen
  , gLine (330,290) (790,290) 20 10 seaGreen
  , gLine (848,300) (925,300) 20 6 seaGreen -- street over the boss, with spare space for oldShaman
  , gLine (110,600) (200,600) 20 8 sienna -- street left leading onto the henge lift
  -- 2d objects
  , gEllipse (450, 600) (100,60) 20 saddleBrown
  , gEllipse (50, 300) (20,100) 20 paleGoldenRod
  , gFullRect (280,250) (40,20) 20 1 seaGreen 
  ]

ps@(px,py,pz) = playerStart
bs@(bx,by,bz) = bossStart

allObjects = level1 ++ level2 ++ level3 ++ threeDs

normalTownFolkPos :: [P3]
normalTownFolkPos = [
    (520,25,0)
  , (525,45,0)
  , (540,75,0)
  , (550,115,0)
  , (555,125,0)
  , (520,205,0)
  , (530,210,0)
  , (420,25,0)
  , (430,45,0)
  , (440,23,0)
  , (480,55,0)
  , (470,60,0)
  , (500,25,0)
  , (420,178,0)
  , (420,205,0)
  , (444,185,0)
  , (440,215,0)
  , (450,136,0)
  , (460,146,0)
  , (745,610,0)
  , (850,530,0)
  ]


 