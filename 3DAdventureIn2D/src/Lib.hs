module Lib
    ( 
      main
    ) where

import Graphics.Proc
import Constants
import State
import Levels
import Drawable
import Actor
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
        let st' = st { objects = allObjects }
        let st'' = st' { actors = allActors }
        return st''

draw :: State -> Pio ()
draw st = do     
        -- provide easy access for pure values
        let time = daytime st        
        let geos = objects st
        let acts = actors st 
        let playerPos = dotpos st
        -- draw the scene
            -- draw the background for the current daytime
        bgForDayTime time
            -- draw simple gemoetries if they are visible, with regards to daytime
        mapM_ (drawAt time playerPos) geos
            -- draw actors if they are visible, with regards to daytime
        mapM_ (drawAt time playerPos) acts
            -- draw the player, regardless of daytime 
        strokeFill aqua
        strokeWeight 1
        circle playerSize (xy (dotpos st))

        
        --TODO
        -- Text Test !! 
        let pText = PText "Hello World!" (100,900) 2 white black
        drawPText pText time

    where xy (px,py,pz) = (px,py)      
        

update :: State -> Pio State
update st = do    
    let updatedActors = map (\a -> (tick a) a) $ actors st
    return $ st { actors = updatedActors }


bgForDayTime :: DayTime -> Pio ()
bgForDayTime daytime | daytime == Night = background black 
                     | otherwise = background white

movement :: State -> Pio State
movement st = do
    --strokeFill orange
    --mapM_ point allPath
    arrow <- key
    case arrow of
        SpecialKey KeyUp    -> return (newPos (onPath st ((0,-2,0) + dotpos st)) st)
        Char 'w'            -> return (newPos (onPath st ((0,-2,0) + dotpos st)) st)
        SpecialKey KeyDown  -> return (newPos (onPath st ((0,2,0) + dotpos st)) st)
        Char 's'            -> return (newPos (onPath st ((0,2,0) + dotpos st)) st)
        SpecialKey KeyRight -> return (newPos (onPath st ((2,0,0) + dotpos st)) st)
        Char 'd'            -> return (newPos (onPath st ((2,0,0) + dotpos st)) st)
        SpecialKey KeyLeft  -> return (newPos (onPath st ((-2,0,0) + dotpos st)) st)
        Char 'a'            -> return (newPos (onPath st ((-2,0,0) + dotpos st)) st)
        _ -> return st

        where
            (_,_,z) = dotpos st
            onPath st p = if elem (p1,p2) allPath then p else dotpos st
                where
                    (p1,p2,_) = p
            allObjectPath = foldr (\o ps -> ((++ ps) . path z) o) [] $ objects st
            allActorPath = foldr (\a ps -> ((++ ps) . path z) a) [] $ actors st
            allPath = (++) allObjectPath allActorPath