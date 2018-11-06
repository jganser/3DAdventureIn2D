module Lib
    ( 
      main
    ) where

import Graphics.Proc
import Constants
import State
import Levels
import Drawable
import CharacterSet hiding (size)
import qualified CharacterSet as CS
import DayTime
import Data.List (union)
import Prelude hiding (lines)


main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update, procKeyPressed = movement }

setup :: Pio State
setup = do
        size (width, height)
        let st = emptyState
        let st' = st { objects = level1 }
        let st'' = st' { actors = level1Actors }
        return st''

draw :: State -> Pio ()
draw st = do     
        -- provide easy access for pure values
        let time = daytime st        
        let geos = objects st
        let acts = actors st 
        let playerPos = dotPos st
        -- draw the scene
            -- draw the background for the current daytime
        bgForDayTime time
            -- draw simple gemoetries if they are visible, with regards to daytime
        mapM_ (drawAt time playerPos) geos
            -- draw actors if they are visible, with regards to daytime
        mapM_ (drawAt time playerPos) acts
            -- draw the player, regardless of daytime 
        strokeFill aqua
        circle playerSize (xy (dotPos st))
            -- draw hostage, if visible
        --TODO
        -- Text Test !! 
        let pText = PText "Hello World!" (100,100) 4 white black
        drawPText pText time

    where xy (px,py,pz) = (px,py)      
        

update :: State -> Pio State
update st = return st


bgForDayTime :: DayTime -> Pio ()
bgForDayTime daytime | daytime == Night = background black 
                     | otherwise = background white

movement :: State -> Pio State
movement st = do
    --strokeFill orange
    --mapM_ point allPath
    arrow <- key
    case arrow of
        SpecialKey KeyUp -> return (st {dotPos = onPath st ((0,-2,0) + dotPos st)})
        SpecialKey KeyDown -> return (st {dotPos = onPath st ((0,2,0) + dotPos st)})
        SpecialKey KeyRight -> return (st {dotPos = onPath st ((2,0,0) + dotPos st)})
        SpecialKey KeyLeft -> return (st {dotPos = onPath st ((-2,0,0) + dotPos st)})
        _ -> return st

        where
            (_,_,z) = dotPos st
            onPath st p = if elem (p1,p2) allPath then p else dotPos st
                where
                    (p1,p2,_) = p
            allObjectPath = foldr (\o ps -> ((++ ps) . path z) o) [] $ objects st
            allActorPath = foldr (\a ps -> ((++ ps) . path z) a) [] $ actors st
            allPath = (++) allObjectPath allActorPath