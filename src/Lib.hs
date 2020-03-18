module Lib
    ( 
      main
    ) where

import Graphics.Proc
import Control.Concurrent
import Constants
import State
import StateMachine
import StateHelper
import Levels
import Drawable
import Actor
import ObjectUtils (norm3, roundXY)
import CharacterSet hiding (size, pos)
import qualified CharacterSet as CS
import DayTime
import Data.List (union)
import Prelude hiding (lines, Either(..))
import Characters.Townpeople
import Player hiding (startTalking)
import Characters.LastLift
import qualified Characters.OldShaman as OldShaman
import qualified Characters.Boss as Boss
import Characters.Female (drawFemaleWithSize)
import qualified Data.Vector as V
import GameState as GS hiding (eventStateSum)


main = runProc $ def { procSetup = setup, procDraw = draw, procUpdateTime = update, procKeyPressed = processKeys }

setup :: Pio State
setup = do
    size (width, height)
    return startState

gameSetup :: Pio State
gameSetup = do
    let st = freshRunningState
    normaltownfolk <- createAllTownPeople normalTownFolkPos
    let st_3 = st { staticActors = normaltownfolk }
    let st_4 = reduceLayerByBlockingStaticActors st_3
    let st_5 = startGuardDialog st_4
    return st_5

draw :: State -> Pio ()
draw st = 
    case gameState st of 
        GameWon -> drawWinningScreen
        Start -> drawStartingScreen
        GameOver -> drawGameLost
        _ -> drawRunning st
    
drawWinningScreen :: Pio ()
drawWinningScreen = do
    let pText = PText "Congrats to winning!" (100,400) 6 black white
    let pText2 = PText "Press E To Restart" (150,600) 4 black white
    bgForDayTime Night
    drawPText pText Day
    drawPText pText2 Day
    drawPlayerInfo


drawPlayerInfo :: Draw
drawPlayerInfo = do
    let playerText  = PText "Hero    --> " (900,400) 3 black white
    drawPText playerText Day
    drawPlayerWithSize 40 (1200,400,0)
    let belovedText = PText "Beloved --> " (900,500) 3 black white
    drawPText belovedText Day
    drawFemaleWithSize 36 (1200,500)

drawStartingScreen :: Pio ()
drawStartingScreen = do
    let pText = PText "A Flatlanders" (160,300) 6 black white
    let pText2 = PText "Adventure" (200,450) 6 black white
    let pText3 = PText "Press E To Start" (150,600) 4 black white
    bgForDayTime Night
    drawPText pText Day
    drawPText pText2 Day
    drawPText pText3 Day
    drawPlayerInfo

drawGameLost ::  Pio ()
drawGameLost = do
    let pText = PText "Game Lost" (100,400) 6 black white
    let pText2 = PText "Press E To Restart" (150,600) 4 black white
    bgForDayTime Night
    drawPText pText Day
    drawPText pText2 Day
    drawPlayerInfo

drawRunning :: State -> Pio ()
drawRunning st = do            
    -- provide easy access for pure values
    let time = daytime st        
    let geos = objects st
    let sActs = staticActors st 
    let playerPos = dotpos st
    -- draw the scene
        -- draw the background for the current daytime
    bgForDayTime Night
        -- draw simple gemoetries if they are visible, with regards to daytime
    mapM_ (drawAt time playerPos) geos
        -- draw actors if they are visible, with regards to daytime
    mapM_ (drawAt time playerPos) sActs
    drawAt time playerPos $ eventActors st
        -- draw the player, regardless of daytime 
    drawP (player st) playerPos     
    -- draw TextField
    writeStandard (currentText st) time
    when (playerTalks st && timeToNextLine st < 0) $ 
        writePressE time
    when (not (playerTalks st) && hengeIsActive st && 
        isPlayerOnHengeLift (player st)) $ writePressH time
    when (not (playerTalks st) && isPlayerOnLastLiftUp (player st)) $ 
        writePressK time
    when (not (playerTalks st) && isPlayerOnLastLiftDown (player st)) $ 
        writePressL time

update :: TimeInterval -> State -> Pio State
update deltaT st = case gameState st of
    Running _ -> updateRunningGame deltaT st
    _ -> return st

updateRunningGame :: TimeInterval -> State -> Pio State
updateRunningGame dt st = 
    if Just Boss.name == standsOnActor (player st) && State.eventStateSum st < 8
    then return $ st { gameState = GameOver}
    else updateRunning dt st

updateRunning :: TimeInterval -> State -> Pio State
updateRunning deltaT st = do    
    let t = timeToNextLine st
    let t_1 = if t >= 0 then  t - deltaT else t
    st_1 <- updateActorMovement deltaT st
    st_2 <- updatePlayerStandsOn st_1
    let st_3 = st_2 { timeToNextLine = t_1 }    
    let st_5 = updateState st_3
    let esum = State.eventStateSum st
    let newEsum = State.eventStateSum st_5
    when (esum < newEsum) $ liftIO $ putStrLn $ "newEventSum: " ++ show newEsum
    st_6 <- updateFalling deltaT st_5
    return st_6

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
            return $ st { eventActors = actors, player = Player.movePlayer dir p}

updateFalling :: TimeInterval -> State -> Pio State
updateFalling deltaT st = 
    if not $ playerFalls st
    then return st
    else 
        let p = player st
            dir@(dx,dy,dz) = norm3 $ target p - pos p
            v = 200 * deltaT
            dist = dir * (v,v,v/10)
            newPos = pos p + roundXY dist
            newDir@(ndx,ndy,ndz) = norm3 $ target p - newPos
            beyondGoal = (dx * ndx + dy * ndy + dz * ndz) <= 0
            newP = if beyondGoal 
                   then p { isFalling = False, pos = target p} 
                   else Player.movePlayer dist p
        in  return $ st {player = newP }

bgForDayTime :: DayTime -> Pio ()
bgForDayTime daytime | daytime == Night = background black 
                     | otherwise = background white

data Dir = Up | Down | Left | Right deriving (Eq,Show)

processKeys :: State -> Pio State
processKeys st = case gameState st of
    Running _ -> processRunningKeys st
    _ -> startGame st

startGame :: State -> Pio State
startGame st = do
    k <- key
    case k of 
        Char 'e' -> gameSetup
        _ -> return st

processRunningKeys :: State -> Pio State
processRunningKeys st = 
    if playerFalls st
    then return st
    else processPlayerInput st

processPlayerInput :: State -> Pio State
processPlayerInput st =
    if playerTalks st 
    then keepTalking st
    else movement st

keepTalking :: State -> Pio State
keepTalking st = 
  if timeToNextLine st > 0 then return st
  else do
    k <- key
    case k of 
        Char 'e' -> do
            return $ curTextUpdate st            
        _  -> return st

writeStandard :: String -> DayTime -> Draw 
writeStandard text time = 
    let pText = PText text (40,900) 2 black white
    in  drawPText pText time

writePress :: Char -> DayTime -> Draw
writePress char time = 
    drawPText (PText ("--- Press "++ [char] ++ "---") (650,930) 2 black white) time


writePressE :: DayTime -> Draw
writePressE = writePress 'E'

writePressH :: DayTime -> Draw
writePressH = writePress 'H'

writePressK :: DayTime -> Draw
writePressK = writePress 'K' 
    
writePressL :: DayTime -> Draw
writePressL = writePress 'L'

movement :: State -> Pio State
movement st = do
    let playerSpeed = 2
    arrow <- key
    case arrow of
        SpecialKey KeyUp    -> return $ moveOrStartTalking playerSpeed Up st
        Char 'w'            -> return $ moveOrStartTalking playerSpeed Up st
        SpecialKey KeyDown  -> return $ moveOrStartTalking playerSpeed Down st
        Char 's'            -> return $ moveOrStartTalking playerSpeed Down st
        SpecialKey KeyRight -> return $ moveOrStartTalking playerSpeed Right st
        Char 'd'            -> return $ moveOrStartTalking playerSpeed Right st
        SpecialKey KeyLeft  -> return $ moveOrStartTalking playerSpeed Left st
        Char 'a'            -> return $ moveOrStartTalking playerSpeed Left st
        Char 'h'            -> return $ mapActor "hengeLift" sendHengeLiftUp st
        Char 'l'            -> do
            let st_1 = if (isInOldShaman3 st) then mapActor OldShaman.name OldShaman.moveWithPlatform st else st
            return $ mapActor "lastLift" sendUp st_1
        Char 'k'            -> do
            if eventStateSum st == 8 || eventStateSum st == 9
            then return st -- don't let the player fiddle with the platform when the oldshaman is walking
            else return $ mapActor "lastLift" sendDown st
        _ -> return st

moveOrStartTalking :: Float -> Dir -> State -> State
moveOrStartTalking speed dir st = 
    let nextPos = posOf speed dir st
        (st' , b) = startTalking nextPos st 
    in  if b then curTextUpdate st'
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
      ea_1 = if nb then GS.replace (n,na_1) ea else ea
  

posOf :: Float -> Dir -> State -> P3
posOf speed Up    st = (0,(-1) * speed,0) + dotpos st
posOf speed Down  st = (0,  1  * speed,0) + dotpos st
posOf speed Right st = (  1  * speed,0,0) + dotpos st
posOf speed Left  st = ((-1) * speed,0,0) + dotpos st

onPath :: State -> P3 -> P3
onPath st p@(p1,p2,z) =
    let l = staticPathAt z $ staticPath st
        lea = addEventActors z l st
        layer = reduceByBlockers z lea st
    in  if (layer V.! round p1) V.! round p2 then p else dotpos st

              
                