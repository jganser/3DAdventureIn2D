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
  ]

level2 :: Level
level2 = [
      gLine ( 50, 50) (200,100) 10 20 yellow
    , gLine (150,150) (300,200) 10  4 lime
    ]

level3 :: Level
level3 = []

ps@(px,py,pz) = playerStart
bs@(bx,by,bz) = bossStart

allObjects = level1 ++ level2 ++ level3 ++ threeDs

platforms = [
    lastLift
  , movingActor hengePlatformStart hengePlatformEnd platformSpeed (fullCube (T hengePlatformStart (0,0,0) hengePlatformSize) green) 3 -- platform to henge
  , movingActor plazaPlatformStart plazaPlatformEnd platformSpeed (fullCube (T plazaPlatformStart (0,0,0) plazaPlatformSize) green) 5 -- main platform
  --, 
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

allActors = [
  --boss
    boss (T bs (0,0,0) (bossSize,bossSize,bossSize))
  -- oldShaman    -- azure with purple border
  --, oldShaman
  ] ++ platforms ++ townfolk