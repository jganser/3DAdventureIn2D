module Actor where

import Drawable
import Geometry

data Actor = A {
        geo :: Geometry
      , tick :: Actor -> Actor
      , moving :: Bool
      , talking :: Bool
      , finishedTalking :: Bool
      , textToSay :: [String]
    }
    
pIsOnActor :: Actor -> (Float,Float,Float) -> Bool
pIsOnActor a p@(x,y,z) = path z a /= []



instance Drawable Actor where
    drawAt daytime z a = drawAt daytime z (geo a)
    path z a = path z (geo a)
