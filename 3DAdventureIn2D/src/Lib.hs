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
import Prelude hiding (lines, Either(..))
import Characters.Townpeople
import Player
import Characters.LastLift
import qualified Data.Vector as V
import GameState as GS


main = runProc $ def { procSetup = setup, procDraw = draw, procUpdateTime = update, procKeyPressed = movement }

setup :: Pio State
setup = do
        size (width, height)
        let st = startState
        normaltownfolk <- createAllTownPeople normalTownFolkPos
        let st_3 = st { staticActors = normaltownfolk }
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
        drawAt time playerPos $ eventActors st
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
    return st_2

updatePlayerStandsOn :: State -> Pio State
updatePlayerStandsOn st = do
    let p = player st
    let p_1 = updatePlayerOnActor p $ eventActors st
    return $ st { player = p_1}

updateActorMovement :: TimeInterval -> State -> Pio State
updateActorMovement deltaT st = do
    let p = player st
    case standsOnActor p of
        Nothing -> return $ tick deltaT st
        Just n -> do
            let curActorPos = aPos (GS.lookupActor n (eventActors st))
            let actors = tick deltaT $ eventActors st
            let dir = aPos (GS.lookupActor n actors) - curActorPos
            return $ st { eventActors = actors, player = movePlayer dir p}

bgForDayTime :: DayTime -> Pio ()
bgForDayTime daytime | daytime == Night = background black 
                     | otherwise = background white

data Dir = Up | Down | Left | Right deriving (Eq,Show)

movement :: State -> Pio State
movement st = do
    let playerSpeed = 2
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
        Char 'h'            -> return $ mapActor "hengeLift" sendHengeLiftUp st
        Char 'l'            -> return $ mapActor "lastLift" sendUp st
        Char 'k'            -> return $ mapActor "lastLift" sendDown st
        _ -> return st

moveOrStartTalking :: Float -> Dir -> State -> State
moveOrStartTalking speed dir st = 
    let nextPos = posOf speed dir st
        (st' , b) = startTalking nextPos st 
    in  if b then st'
        else newPos (onPath st nextPos) st

startTalking :: P3 -> State -> (State, Bool)
startTalking pos st = 
  (st { player = p_2, eventActors = ea_1}, b || nb)
    where
      sActs = staticActors st
      (a, b) = onActorsCheck pos sActs
      p = player st
      p_1 = if b then p {isTalking = True, dialog = textToSay a} else p
      ea = eventActors st
      (n,na,nb) = onAnyTalkingEventActor pos ea 
      p_2 = if nb then p_1 {isTalking = True, dialog = textToSay na} else p_1
      na_1 = if nb then na {textToSay = [], finishedTalking = True} else na
      ea_1 = if nb then replace (n,na_1) ea else ea
  

posOf :: Float -> Dir -> State -> P3
posOf speed Up    st = (0,(-1) * speed,0) + dotpos st
posOf speed Down  st = (0,  1  * speed,0) + dotpos st
posOf speed Right st = (  1  * speed,0,0) + dotpos st
posOf speed Left  st = ((-1) * speed,0,0) + dotpos st

onPath :: State -> P3 -> P3
onPath st p@(p1,p2,z) =
    let l = staticPathAt z $ staticPath st
        layer = addEventActors z l st                 
    in  if (layer V.! round p1) V.! round p2 then p else dotpos st

              
                