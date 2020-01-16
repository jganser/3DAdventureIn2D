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

darkSlateGray :: Col
darkSlateGray = crgb (47,79,79)

seaGreen :: Col
seaGreen = crgb (46,139,87)

cadetBlue :: Col
cadetBlue = crgb (95,158,160)

firebrick :: Col
firebrick = crgb (178,34,34)

gold :: Col
gold = crgb (255,215,0)

royalBlue :: Col
royalBlue = crgb (65,105,225)

saddleBrown :: Col
saddleBrown = crgb (139,69,19)

darkSaddleBrown :: Col
darkSaddleBrown = crgb (70,30,10)

azure :: Col
azure = crgb (240,255,255)