module Lib
    ( 
      main
    ) where

import Graphics.Proc
import Graphics.UI.GLUT.Callbacks.Window (KeyState(..))
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
import Characters.LastLift
import qualified Data.Vector as V


main = runProc $ def { procSetup = setup, procDraw = draw, procUpdateTime = update, procKeyPressed = movement }

setup :: Pio State
setup = do
        size (width, height)
        let st = emptyState
        let st' = foldl (\state obj -> addObject obj state) st allObjects
        normaltownfolk <- createAllTownPeople normalTownFolkPos
        let st'' = st' { eventActors = bossActor : townfolk ++ lifts ++ platforms}        
        let st_3 = st'' { staticActors = normaltownfolk }
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
    st_1 <- updateActorMovement deltaT st
    st_2 <- updatePlayerStandsOn st_1
    let a = standsOnActor (player st_2)
    --liftIO $ putStrLn $ "player stand on: " ++ show a
    --let hengeL = lookupActor "hengeLift" $ eventActors st_2
    --liftIO $ putStrLn $ "BoundingBox of hengeLift: " ++ (show $ boundingBox hengeL)
    -- todo: process game state...
    return st_2
    -- liftIO $ putStrLn $ "updating with deltaT: " ++ show deltaT -- debugline

updatePlayerStandsOn :: State -> Pio State
updatePlayerStandsOn st = do
    let p = player st
    let p_1 = updatePlayerOnActor p $ eventActors st
    return $ st { player = p_1}

updateActorMovement :: TimeInterval -> State -> Pio State
updateActorMovement deltaT st = do
    let p = player st
    case standsOnActor p of
        Nothing -> return $ st { eventActors = updateActors deltaT st}
        Just n -> do
            let curActorPos = aPos (lookupActor n (eventActors st))
            let actors = updateActors deltaT st
            let dir = aPos (lookupActor n actors) - curActorPos
            return $ st { eventActors = actors, player = movePlayer dir p}

updateActors :: TimeInterval -> State -> [(String,Actor)]
updateActors deltaT st = map (\(n,a) -> (n, (tick a) deltaT a)) $ eventActors st


bgForDayTime :: DayTime -> Pio ()
bgForDayTime daytime | daytime == Night = background black 
                     | otherwise = background white

movement :: State -> Pio State
movement st = do
    m <- modifiers
    let playerSpeed = case shift m of {Up -> 2; Down -> 10} -- modifiers seems not to work...
    arrow <- key
    case arrow of
        SpecialKey KeyUp    -> return (newPos (onPath st ((0,(-1) * playerSpeed,0) + dotpos st)) st)
        Char 'w'            -> return (newPos (onPath st ((0,(-1) * playerSpeed,0) + dotpos st)) st)
        SpecialKey KeyDown  -> return (newPos (onPath st ((0,1 * playerSpeed,0) + dotpos st)) st)
        Char 's'            -> return (newPos (onPath st ((0,1 * playerSpeed,0) + dotpos st)) st)
        SpecialKey KeyRight -> return (newPos (onPath st ((1 * playerSpeed,0,0) + dotpos st)) st)
        Char 'd'            -> return (newPos (onPath st ((1 * playerSpeed,0,0) + dotpos st)) st)
        SpecialKey KeyLeft  -> return (newPos (onPath st (((-1) * playerSpeed,0,0) + dotpos st)) st)
        Char 'a'            -> return (newPos (onPath st (((-1) * playerSpeed,0,0) + dotpos st)) st)
        Char 'h'            -> do
            let a = lookupActor "hengeLift" $ eventActors st
            let a_1 = sendHengeLiftUp a
            let eas = replaceActor ("hengeLift", a_1) $ eventActors st
            -- mapM_ (liftIO . putStrLn . show) $ eas
            return $ st {eventActors = eas}
        Char 'l'            -> do
            let a = lookupActor "lastLift" $ eventActors st
            let a_1 = sendUp a
            let eas = replaceActor ("lastLift", a_1) $ eventActors st
            return $ st {eventActors = eas}
        Char 'k'            -> do
            let a = lookupActor "lastLift" $ eventActors st
            let a_1 = sendDown a
            let eas = replaceActor ("lastLift", a_1) $ eventActors st
            return $ st {eventActors = eas}
        _ -> return st


onPath :: State -> P3 -> P3
onPath st p@(p1,p2,z) =
--  | standsOnActor (player st) == Nothing = 
    let curLayer = staticPathAt z $ staticPath st 
        allActorPath = concatMap $ path z 
        curLayerPlusMovers = updateLayer curLayer True $ allActorPath $ movingActors st
        addEventActors = updateLayer curLayerPlusMovers True $ allActorPath $ filter (not . blocksPlayer) $ map snd $ eventActors st
        reduceByBlockers = updateLayer addEventActors False $ allActorPath $ filter (blocksPlayer) $ map snd $ eventActors st
    in  if (reduceByBlockers V.! round p1) V.! round p2 then p else dotpos st
--  | otherwise = 
--    let Just n = standsOnActor $ player st
--        a = lookupActor n $ eventActors st
--        b = boundingBox a
--    in  if p1 >= x_low b && p1 <= x_high b &&
--           p2 >= y_low b && p2 <= y_high b && 
--           (p1,p2) `elem` path z a then p else dotpos st
              
                