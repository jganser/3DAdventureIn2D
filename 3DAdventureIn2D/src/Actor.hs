module Actor where

import Drawable
import Geometry

data Actor = A {
        geo :: Geometry,
        tick :: Actor -> Actor
    }

instance Drawable Actor where
    drawAt daytime z (A geo _) = drawAt daytime z geo
    path z (A geo _) = path z geo
