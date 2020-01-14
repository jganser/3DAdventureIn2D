module Actor where

import Drawable
import Geometry
import ObjectUtils


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

aPos a = transition $ transform $ geo a
azPos = trd . aPos


instance Drawable Actor where
    drawAt daytime playerPos a = drawAt daytime playerPos (geo a)
    path z a = path z (geo a)