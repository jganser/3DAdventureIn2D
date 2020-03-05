module Constants where


width :: Float
width = 1240.0

height :: Float
height = 1060.0

playerStart :: (Float,Float,Float)
playerStart = (100,330,0)
--playerStart = hengePos
--playerStart = lastLiftUp

playerSize :: Float
playerSize = 10

townManSize :: Float
townManSize = playerSize

townManSize3 :: (Float,Float,Float)
townManSize3 = (townManSize,townManSize,townManSize)

femaleStart :: (Float,Float,Float)
femaleStart = (100,300,0)

bossStart :: (Float,Float,Float)
bossStart = (900,300,0)

hengePos :: (Float,Float,Float)
hengePos = (100,600,0)

bossSize :: (Float,Float,Float)
bossSize = (70,70,6)

layers :: [Float]
layers = [0,10,20]

hengePlatformSize :: (Float,Float,Float)
hengePlatformSize = (30,20,30)

hengePlatformStart :: (Float,Float,Float)
hengePlatformStart = (385,600,10)

hengePlatformEnd :: (Float,Float,Float)
hengePlatformEnd = (215,600,10)

platformSpeed :: Float
platformSpeed = 100

plazaPlatformSize :: (Float,Float,Float)
plazaPlatformSize = (36,40,12)

plazaPlatformStart :: (Float,Float,Float)
plazaPlatformStart = (240,230,6)

plazaPlatformEnd :: (Float,Float,Float)
plazaPlatformEnd = (160,170,6)



