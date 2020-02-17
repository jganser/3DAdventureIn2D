module Constants where


width :: Float
width = 1240.0

height :: Float
height = 1060.0

playerStart :: (Float,Float,Float)
playerStart = (100,330,0)

playerSize :: Float
playerSize = 5

femaleStart :: (Float,Float,Float)
femaleStart = (100,300,0)

bossStart :: (Float,Float,Float)
bossStart = (900,300,0)

hengePos :: (Float,Float,Float)
hengePos = (100,600,0)

bossSize :: Float
bossSize = 70

lastLiftUp :: (Float,Float,Float)
lastLiftUp = (820,300,20)

lastLiftDown :: (Float,Float,Float)
lastLiftDown = (520,300,10)

layers :: [Float]
layers = [0,10,20]

hengePlatformSize :: (Float,Float,Float)
hengePlatformSize = (30,10,30)

hengePlatformStart :: (Float,Float,Float)
hengePlatformStart = (385,600,10)

hengePlatformEnd :: (Float,Float,Float)
hengePlatformEnd = (215,600,10)

platformSpeed :: Float
platformSpeed = 50

plazaPlatformSize :: (Float,Float,Float)
plazaPlatformSize = (30,40,12)

plazaPlatformStart :: (Float,Float,Float)
plazaPlatformStart = (240,230,6)

plazaPlatformEnd :: (Float,Float,Float)
plazaPlatformEnd = (160,170,6)

liftSpeed :: Float
liftSpeed = 30