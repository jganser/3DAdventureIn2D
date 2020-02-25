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
    --sphere (200,200,50) 200 red,
    --sphere (100,200,0) 450 blue,
    --ellipsoide (T (300,100,0) (0,0,0) (50,100,20)) green,
    --cube (T (300,275,50) (0,0,0) (100,150,100))  5 green,
    --cube (T (50,225,50) (0,0,0) (200, 50,120)) 12 blue,
    --fullCube (T (200,200,50) (0,0,0) (100,  150, 120)) (rgb 128 240 240),
    --fullCube (T (200,300,50) (0,0,0) (400, 50, 120)) (rgb 240 128 128),
    --cylinder (T (250,250,250) (0,0,0) (100,100,100)) olive 1,
    --henge (T hengePos (0,0,0) (200,200,200)),
    --lines 
      gLine ( 80,300) (860,300) 0 8 darkSaddleBrown -- main street to boss
    , gLine (100,300) (100,320) 0 4 darkSaddleBrown -- street to the house of the hero    
    , gLine (160,150) (160,100) 0 4 darkSaddleBrown -- street behind main platform
    , gLine (120,100) (180,100) 0 4 darkSaddleBrown -- T Street after main platform
    , gLine (260,100) (360,100) 0 4 darkSaddleBrown -- Street to forum
    , gLine (410,120) (435,120) 0 3 firebrick -- bridge to plaza on Forum -> left
    , gLine (460,70) (460,95) 0 3 firebrick -- bridge to plaza on Forum -> up
    , gLine (460,145) (460,170) 0 3 firebrick -- bridge to plaza on Forum -> down
    , gLine (485,120) (510,120) 0 3 firebrick -- bridge to plaza on Forum -> right
    , gLine (460,300) (460,600) 0 4 darkSaddleBrown -- street down leading to henge
    , gLine (400,600) (460,600) 0 4 darkSaddleBrown -- street to the left leading to henge
    , gLine (120,600) (200,600) 0 4 darkSaddleBrown -- street left leading onto the henge lift
    , gLine (540,460) (640,460) 0 4 darkSaddleBrown -- street behind shaman hud
    , gLine (640,460) (640,580) 0 4 darkSaddleBrown -- street down behind shaman hud
    , gLine (640,500) (780,500) 0 4 darkSaddleBrown -- street to the house under the boss
    , gLine (580,600) (615,600) 0 4 darkSaddleBrown -- street at the right low left
    , gLine (665,600) (700,600) 0 4 darkSaddleBrown -- street at the right low right
    -- 2D Objects
    , gFullRect (100,340) (40,40) 0 1 firebrick -- house of the hero
    , gFullRect (385,120) (50,200) 0 1 royalBlue -- Forum part => left
    , gFullRect (460,45) (100,50) 0 1 royalBlue  -- Form part  => up
    , gFullRect (460,195) (100,50) 0 1 royalBlue -- Form part  => down
    , gFullRect (535,120) (50,200) 0 1 royalBlue -- Form part  => right
    , gCirc (460,120) 50 0 gold -- Forum Plaza
    , gEllipse (40,100) (60,120) 0 cadetBlue -- upper left hud
    , gCirc (500, 460) 80 0 firebrick -- house of the shaman
    , gFullRect (840,520) (80,60) 0 1 firebrick -- fullrect under boss
    , gCirc (570,600) 20 0 cadetBlue -- low left hud
    , gEllipse (730,600) (60,40) 0 cadetBlue -- upper left hud
    ]
    
threeDs :: Level
threeDs = [
    fullCube (T (220,100,0) (0,0,0) (80,  40, 22)) goldenRod -- house after main platform in dircetion of forum
  , cylinder (T (105,100,0) (0,0,0) (30,30,25)) olive 1 -- first round cylindric hud after main platform left
  , cylinder (T (80,100,0) (0,0,0) (30,30,30)) seaGreen 1 -- second round cylindric hud after main platform left
  , henge (T hengePos (0,0,0) (200,200,100))
  , fullCube (T (240,275,18) (0,0,0) (6,50,36)) darkSaddleBrown -- street to main platform
  , fullCube (T (640,600,10) (0,0,0) (50,40,36)) seaGreen -- cube at the bottom right
  , ellipsoide (T (400,100,10) (0,0,0) (40,40,6)) seaGreen -- sphere
  , cube (T (300,510,18) (0,0,0) (300,8,6)) 8 paleGoldenRod -- open cube in 3rd level, that shall be visible whle going up
  ]

level2 :: Level
level2 = [
    --  gLine ( 50, 50) (200,100) 10 20 yellow
    --, gLine (150,150) (300,200) 10  4 lime
    --lines 
      gLine (255,230) (400,230) 10 8 darkSaddleBrown -- street to the right from  mainplatform
    , gLine (400,230) (400,120) 10 4 darkSaddleBrown -- street up
    , gLine (380,100) (260,100) 10 4 darkSaddleBrown -- street left form ellipsoide
    , gLine (120,100) (180,100) 10 4 darkSaddleBrown -- street to the huds
    , gLine ( 80,115) (80,200) 10 4 darkSaddleBrown -- street to old Shaman hud
    -- 2d objects
    , gCirc (80,250) 100 10 darkSlateGray -- old shaman hud
    ]

level3 :: Level
level3 = [
    gLine (450,570) (450,520) 20 6 sienna
  , gLine (50,500) (150,500) 20 6 sienna
  , gLine (50,500) (50,350) 20 4 sienna
  , gLine (50,250) (100,220) 20 8 sienna
  , gLine (200,220) (240,240) 20 6 seaGreen
  , gLine (300,260) (305,320) 20 6 seaGreen
  , gLine (305,320) (335,315) 20 6 seaGreen
  , gLine (335,315) (330,290) 20 6 seaGreen
  , gLine (332,290) (790,280) 20 6 seaGreen
  , gLine (848,300) (905,300) 20 3 seaGreen
  -- 2d objects
  , gEllipse (450, 600) (100,60) 20 saddleBrown
  , gEllipse (50, 300) (20,100) 20 paleGoldenRod
  , gFullRect (280,250) (40,20) 20 1 seaGreen 
  ]

ps@(px,py,pz) = playerStart
bs@(bx,by,bz) = bossStart

allObjects = level1 ++ level2 ++ level3 ++ threeDs

platforms = [    
    movingActor [hengePlatformStart, hengePlatformEnd] platformSpeed (fullCube (T hengePlatformStart (0,0,0) hengePlatformSize) green) 2 -- platform to henge
  , movingActor [plazaPlatformStart, plazaPlatformEnd] platformSpeed (fullCube (T plazaPlatformStart (0,0,0) plazaPlatformSize) green) 3 -- main platform
  , movingActor [(100,205,20), (80,50,24), (220,30,16), (200,225,20)] platformSpeed (ellipsoide (T (100,205,20) (0,0,0) (20,40,4)) seaGreen) 1 -- 3rd level round trip platform
  ]

lifts = [
    ("lastLift", lastLift)
  , ("hengeLift", hengeLift)
  ]

townfolk = [
  -- guards       -- blue with grey border
  -- female       -- corale
  --, female
  -- old lady     -- deep purple
  -- old man      -- crimson
  -- shaman       -- white with purple border
  ]

normalTownFolkPos :: [P3]
normalTownFolkPos = [
  --todo
  ]

bossActor = ("boss", boss (T bs (0,0,0) (bossSize,bossSize,bossSize)))
 