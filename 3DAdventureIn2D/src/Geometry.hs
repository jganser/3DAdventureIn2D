module Geometry where

import Drawable
import Graphics.Proc

data Transform = T {transition :: P3, rotation :: P3, scaling :: P3} deriving (Eq, Show)

data Geometry = Geo {
        transform :: Transform, 
        drawGeo :: Transform -> P3 -> Draw,
        geoPath :: Transform -> Float -> Path
    }

instance Drawable Geometry where
    drawAt pPos (Geo t drawIt _) = drawIt t pPos
    path height (Geo t _ pathOfIt) = pathOfIt t height
