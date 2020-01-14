module Colors where

import Graphics.Proc
-- import Data.Tuple
-- import Data.Tuple.Extra

------------
-- colors --
------------

crgb (a,b,c) = rgb a b c

crimson :: Col
crimson = crgb (220, 20, 60)

goldenRod :: Col
goldenRod = crgb (218,165,32)

paleGoldenRod :: Col
paleGoldenRod = crgb (238,232,170)

olive :: Col
olive = crgb (128,128,0)

darkSlateGray :: Col
darkSlateGray = crgb (47,79,79)

saddleBrown :: Col
saddleBrown = crgb (139,69,19)

darkSaddleBrown :: Col
darkSaddleBrown = crgb (70,30,10)

azure :: Col
azure = crgb (240,255,255)