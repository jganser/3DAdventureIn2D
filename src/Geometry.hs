module Geometry where

import Drawable
import DayTime
import Graphics.Proc


data Geometry = Geo {
        transform :: Transform, 
        drawGeo :: Transform -> DayTime -> P3 -> Draw,
        geoPath :: Transform -> Float -> Path
    }

instance Show Geometry where
    show g = "Geometry { transform: " ++ show (transform g) ++ " }"

instance Drawable Geometry where
    drawAt daytime pPos (Geo t drawIt _) = drawIt t daytime pPos
    path height (Geo t _ pathOfIt) = pathOfIt t height
    boundingBox (Geo (T (x,y,z) _ (sx,sy,sz)) _ _) = BB (x-sx/2) (x+sx/2) (y-sy/2) (y+sy/2) (z-sz/2) (z+sz/2)
    newDraw f g = g { drawGeo = f}

newGeoPos :: P3 -> Geometry -> Geometry
newGeoPos pos g = g { transform = newTPos pos $ transform g}

newTPos :: P3 -> Transform -> Transform
newTPos pos t = t { transition = pos}

emptyGeo :: Geometry
emptyGeo = Geo zeroTransform (\ _ _ _ -> return ()) (\_ _ -> [])

zeroTransform :: Transform
zeroTransform = T (0,0,0) (0,0,0) (0,0,0)


