{-# LANGUAGE FlexibleInstances #-}

module State where

import Objects
import Levels
import Geometry
import Drawable
import Actor hiding (lookupActor,replaceActor)
import Constants
import DayTime
import Player hiding (movePlayer)
import qualified Player (movePlayer)
import Graphics.Proc
import qualified Data.Vector as V
import GameState hiding (replace,lookupActor)
import qualified GameState as GS
import Data.Maybe


-- the actors shall be disjoint distributed over the 3 actor lists (unique occurence)
data State = ST {
    player :: Player, 
    objects :: [Geometry],
    staticActors :: [Actor], -- static actors can be used to make the path function faster
    gameState :: GameState,
    daytime :: DayTime,
    staticPath :: StaticPath,
    eventActors :: EventActors,
    currentText :: String,
    timeToNextLine :: Float
}

eventActorList :: State -> [(String, Actor)] -- this list has a name to actor reference included to enable situational updates
eventActorList = (asList . eventActors)

instance EventHolder State where
  eventState = eventState . gameState
  isEventSensitive = isEventSensitive . gameState
  updateEventState st es = st { gameState = (updateEventState (gameState st) es)}
  
instance Tickable State where
  tick dt st = st { eventActors = tick dt $ eventActors st}

-- this structure shall be able to speed up the calculations necessary in each
-- time step
data StaticPath = SP Layer Layer Layer

emptyState :: State
emptyState = ST 
  (newPlayer playerStart) 
  [] 
  [] 
  (Running newEventState) 
  Day 
  (SP emptyLayer emptyLayer emptyLayer) startEventActors
  standardText
  0

startState :: State
startState = let st = emptyState in st {gameState = Start}

emptyLayer :: Layer
emptyLayer = V.replicate (round width) (V.replicate (round height) False)

freshRunningState :: State
freshRunningState = 
  let st = emptyState
      st' = foldl (\state obj -> addObject obj state) st allObjects
  in  st'

standardText :: String
standardText = "    --- Move with W A S D or the Arrow keys ---"

-- helper
mapActor :: String -> (Actor -> Actor) -> State -> State
mapActor n f st = 
            let a = GS.lookupActor n $ eventActors st
                a_1 = f a
                eas = GS.replace (n, a_1) $ eventActors st
            in st {eventActors = eas}

dotpos :: State -> P3
dotpos = pos . player 

newPos :: P3 -> State -> State
newPos pos st = st { player = updatePos pos $ player st }

addObject :: Geometry -> State -> State
addObject geo st = st {objects = addedObj, staticPath = extendedPath}
  where
    addedObj = geo : objects st    
    extendedPath =  SP 
      (updateLayer zero True (path 0 geo))
      (updateLayer ten True (path 10 geo))
      (updateLayer twenty True (path 20 geo))
        where (SP zero ten twenty) = staticPath st

playerTalks :: State -> Bool
playerTalks = isTalking . player

playerFalls :: State -> Bool
playerFalls = isFalling . player

playerFinishedTalking :: State -> State
playerFinishedTalking st  = 
  let p = stopTalking $ player st in st { player = p }

lookupActor :: String -> State -> Actor
lookupActor name st = GS.lookupActor name $ eventActors st

replace :: (String,Actor) -> State -> State
replace na st = 
  let ea = GS.replace na $ eventActors st 
  in  st { eventActors = ea}

dialogUpdate :: [String] -> State -> State
dialogUpdate text st =
  let p = startTalkingText text $ player st 
  in  curTextUpdate $ st { player = p }

dialogUpdateA :: Actor -> State -> State
dialogUpdateA a = dialogUpdate (textToSay a) 

dialogUpdateN :: String -> State -> State
dialogUpdateN name st = 
  let a    = lookupActor name st
      a_1  = a { textToSay = [], finishedTalking = True}      
      st_1 = replace (name, a_1) st
  in  dialogUpdateA a st_1
  
curTextUpdate :: State -> State
curTextUpdate st = 
    let (text, p) = nextTextLine $ player st
    -- liftIO $ putStrLn $ "player after next Line: " ++ show p      
    in  st { 
        player = p, 
        timeToNextLine = 0.4,
        currentText = 
            if isTalking p
            then text 
            else standardText
        }

movePlayer :: P3 -> State -> State
movePlayer dir st = 
  let p = Player.movePlayer dir $ player st
  in st { player = p}

eventStateSum st 
  | isEventSensitive st = GS.eventStateSum $ fromJust $ eventState st
  | otherwise = 0

-- Layering
addEventActors :: Float -> Layer -> State -> Layer
addEventActors z l st = updateLayer l True $ allActorPath z $ filter (not . blocksPlayer) $ Prelude.map snd $ eventActorList st

reduceByBlockers :: Float -> Layer -> State -> Layer
reduceByBlockers z l st = updateLayer l False $ allActorPath z $ filter (blocksPlayer) $ Prelude.map snd $ eventActorList st

staticPathAt :: Float -> StaticPath -> Layer
staticPathAt z (SP zero ten twenty) = case z of
    0 -> zero
    10 -> ten
    20 -> twenty
    _ -> emptyLayer

reduceLayerByBlockingStaticActors :: State -> State
reduceLayerByBlockingStaticActors st = reduceLayerByBlockers (staticActors st) st

reduceLayerByBlockers :: [Actor] -> State -> State
reduceLayerByBlockers as st = st {staticPath = newLayers}
  where
    blockers = filter (blocksPlayer) as    
    zeroP = concatMap (path 0) blockers
    tenP = concatMap (path 10) blockers
    twentyP = concatMap (path 20) blockers
    (SP zero ten twenty) = staticPath st
    newLayers = SP 
      (updateLayer zero False zeroP)
      (updateLayer ten False tenP)
      (updateLayer twenty False twentyP)

