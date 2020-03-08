{-# LANGUAGE FlexibleInstances #-}

module State where

import Objects
import Levels
import Geometry
import Drawable
import Actor
import qualified Characters.Female as F
import Constants
import DayTime
import Player
import Graphics.Proc
import qualified Data.Vector as V
import GameState as GS


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

emptyLayer :: Layer
emptyLayer = V.replicate (round width) (V.replicate (round height) False)

startState :: State
startState = 
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

playerFinishedTalking :: State -> State
playerFinishedTalking st  = let p = stopTalking $ player st in st { player = p }

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


-- State Machine

updateState :: State -> State
updateState = checkProlog

phaseCheck :: (State -> Bool) -> (State -> Bool) -> (State -> State) -> (State -> State) -> State -> State
phaseCheck phasePred phaseDonePred phaseDone nextPhaseToCheck st = 
  if phasePred st 
  then if phaseDonePred st then phaseDone st else st
  else nextPhaseToCheck st

checkProlog :: State -> State
checkProlog = phaseCheck isInProlog guardCheck setPrologDone checkElder

checkElder :: State -> State
checkElder = phaseCheck isInElderCheck oldManCheck setElderDone checkShaman

checkShaman :: State -> State
checkShaman = phaseCheck isInNewShaman shamanCheck setShamanMovedDone checkFlowers

checkFlowers :: State -> State
checkFlowers = phaseCheck isInFlowers flowersCheck setFlowerDone checkOld1

checkOld1 = phaseCheck isInOldShaman1 oldCheck1 setOld1 checkOld2
checkOld2 = phaseCheck isInOldShaman2 oldCheck2 setOld2 checkOld3
checkOld3 = phaseCheck isInOldShaman3 oldCheck3 setOld3 checkOld4
checkOld4 = phaseCheck isInOldShaman4 oldCheck4 setOld4 checkInMonster

checkInMonster = phaseCheck isInPlayerInMonster inMonsterCheck setInMonster checkVomit

checkVomit = phaseCheck isInVomitted vomitCheck setVomitted checkEpilog

checkEpilog = phaseCheck isInEpilog epilogCheck setEpilog id

isInElderCheck :: State -> Bool
isInElderCheck = isInPhaseX $ inElder 

isInProlog :: State -> Bool
isInProlog = isInPhaseX inProlog

isInNewShaman = isInPhaseX inNewShaman
isInFlowers = isInPhaseX inOldLady
isInOldShaman1 = isInPhaseX inOldShaman1
isInOldShaman2 = isInPhaseX inOldShaman2
isInOldShaman3 = isInPhaseX inOldShaman3
isInOldShaman4 = isInPhaseX inOldShaman4
isInPlayerInMonster = isInPhaseX inPlayerInMonster
isInVomitted = isInPhaseX inVomitted
isInEpilog = isInPhaseX inEpilog


isInPhaseX :: (Maybe EventState -> Bool) -> State -> Bool
isInPhaseX f st = isEventSensitive st && (not . f . eventState) st

inPhase :: (EventState -> Bool) -> Maybe EventState -> Bool
inPhase f (Just es) = f es
inPhase f Nothing = False

inProlog = inPhase prolog
inElder = inPhase oldManInfo
inOldLady = inPhase hasFlowers
inNewShaman = inPhase shamanMovedAside
inOldShaman1 = inPhase oldShamanPartOne
inOldShaman2 = inPhase oldShamanOnPlatform
inOldShaman3 = inPhase oldShamanOnPlatformAtTop
inOldShaman4 = inPhase oldShamanOverMonster
inPlayerInMonster = inPhase playerInMonster
inVomitted = inPhase playerAndFemaleOutOfMonster
inEpilog = inPhase epilog


eaCheck :: (Actor -> Bool) -> (EventActors -> (String,Actor)) -> State -> Bool
eaCheck f g = f . snd . g . eventActors

eaTalkCheck :: (EventActors -> (String,Actor)) -> State -> Bool
eaTalkCheck = eaCheck finishedTalking

guardCheck :: State -> Bool
guardCheck = eaTalkCheck GS.guard

oldManCheck = eaTalkCheck GS.elder

shamanCheck = eaTalkCheck GS.newShaman

flowerCheck = eaTalkCheck GS.oldLady



     

setPhaseDone :: (EventState -> EventState) -> State -> State
setPhaseDone f st 
  | isEventSensitive st = 
    let Just es = eventState st
    in updateEventState st $ f es
  | otherwise = st


setPrologDone :: State -> State
setPrologDone = setPhaseDone (\es -> es { prolog = True})
 
setElderDone = setPhaseDone (\es -> es { oldManInfo = True})
setFlowerDone = setPhaseDone (\es -> es { hasFlowers = True})
setShamanMovedDone = setPhaseDone (\es -> es { shamanMovedAside = True})
setOld1 = setPhaseDone (\es -> es { oldShamanPartOne = True})
setOld2 = setPhaseDone (\es -> es { oldShamanOnPlatform = True})
setOld3 = setPhaseDone (\es -> es { oldShamanOnPlatformAtTop = True})
setOld4 = setPhaseDone (\es -> es { oldShamanOverMonster = True})
setInMonster = setPhaseDone (\es -> es { playerInMonster = True})
setVomitted = setPhaseDone (\es -> es { playerAndFemaleOutOfMonster = True})
setEpilog = setPhaseDone (\es -> es { epilog = True})




-- Monologs
sacrifice :: [String]
sacrifice = [
    "Hero      : I coudn\'t stop her. Why does live challenge us this way?"
  , "Hero      : I won\'t accept this I need to find a solution!"
  , "Hero      : Wasn\'t the monster chased away once in the history of village?"
  , "Hero      : Maybe one of the elders knows about it."
  , "Hero      : They live in the round natural huts. Left of the big plaza."
  , "Hero      : Behind the main platform."
  ]

onPlatformWithoutFlowers :: [String]
onPlatformWithoutFlowers = [
    "Hero      : This is the place where the old shaman learned to vanish!"
  , "Hero      : It is said that this henge has a deep connection to the other world."
  , "Hero      : I wonder how he did it... "
  , "Hero      : This place feels really old. "
  , "Hero      : Like on can see the the history of rituals."
  , "Hero      : The looks like the old rituals included some flowers..."
  , "Hero      : Most likely as sacrifices."
  , "Hero      : I wonder if the old flower lady knows more about this."
  , "Hero      : She is living right under the monster, behind the shamans hut."
  ]