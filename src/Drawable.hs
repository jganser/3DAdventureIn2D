module Drawable where

import Graphics.Proc
import DayTime
import Data.Vector (Vector)
import qualified Data.Vector as V


data Transform = T {
    transition :: P3, 
    rotation :: P3, 
    scaling :: P3
  } deriving (Eq, Show)

data BoundingBox = BB {
    x_low :: Float, -- x low value
    x_high :: Float,-- x high value
    y_low :: Float,   -- y low value
    y_high :: Float, -- y high value
    z_low :: Float, -- z low value
    z_high :: Float  -- z high value
  } deriving (Eq, Show)

emptyBoundingBox :: BoundingBox
emptyBoundingBox = BB 0 0 0 0 0 0

class Drawable a where
    drawAt :: DayTime -> P3 -> a -> Draw
    path :: Float -> a -> Path
    boundingBox :: a -> BoundingBox
    newDraw :: (Transform -> DayTime -> P3 -> Draw) -> a -> a

type Path = [Field]
type Field = P2
type Layer = Vector (Vector Bool)

updateLayer :: Layer -> Bool -> Path -> Layer
updateLayer l add path =  V.accum (\v t -> v V.// t) l p
  where
    p :: [(Int,[(Int,Bool)])]
    p = Prelude.map (\(x,y) -> (round x,[(round y,add)])) path
