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
import Characters.Townpeople
import Player
import qualified Data.Vector as V


main = runProc $ def { procSetup = setup, procDraw = draw, procUpdateTime = update, procKeyPressed = movement }

setup :: Pio State
setup = do
        size (width, height)
        let st = emptyState
        let st' = foldl (\state obj -> addObject obj state) st allObjects
        normaltownfolk <- createAllTownPeople normalTownFolkPos
        let st'' = st' { eventActors = bossActor : townfolk ++ lifts}        
        let st_3 = st'' { movingActors = platforms, staticActors = normaltownfolk }
        let st_4 = reduceLayerByBlockingStaticActors st_3
        return st_4

draw :: State -> Pio ()
draw st = do     
        -- provide easy access for pure values
        let time = daytime st        
        let geos = objects st
        let sActs = staticActors st 
        let playerPos = dotpos st
        -- draw the scene
            -- draw the background for the current daytime
        bgForDayTime time
            -- draw simple gemoetries if they are visible, with regards to daytime
        mapM_ (drawAt time playerPos) geos
            -- draw actors if they are visible, with regards to daytime
        mapM_ (drawAt time playerPos) sActs
        mapM_ (drawAt time playerPos) $ movingActors st
        mapM_ (drawAt time playerPos) $ map snd $ eventActors st
            -- draw the player, regardless of daytime 
        drawP (player st) playerPos

        
        --TODO
        -- Text Test !! 
        let pText = PText "Hello World!" (100,900) 2 white black
        drawPText pText time

    where xy (px,py,pz) = (px,py)      
        

update :: TimeInterval -> State -> Pio State
update deltaT st = do    
    let updatedActors = map (\a -> (tick a) deltaT a) $ movingActors st
    return $ st { movingActors = updatedActors }


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
          onPath st p@(p1,p2,z) = if (reduceByBlockers V.! round p1) V.! round p2 then p else dotpos st
            where 
              curLayer = staticPathAt z $ staticPath st 
              allActorPath = foldr (\a ps -> ((++ ps) . path z) a) [] 
              curLayerPlusMovers = updateLayer curLayer True $ allActorPath $ movingActors st
              addEventActors = updateLayer curLayer True $ allActorPath $ filter (not . blocksPlayer) $ map snd $ eventActors st
              reduceByBlockers = updateLayer curLayer False $ allActorPath $ filter (blocksPlayer) $ map snd $ eventActors st
            