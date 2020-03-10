{-# LANGUAGE FlexibleInstances #-}

module StateMachine  where

import Actor
import State
import StateHelper
import Constants
import Graphics.Proc
import GameState as GS
import qualified Player




updateState :: State -> State
updateState = checkProlog1

phaseCheck :: (State -> Bool) -> (State -> Bool) -> (State -> State) -> (State -> State) -> State -> State
phaseCheck phasePred phaseDonePred phaseDone nextPhaseToCheck st = 
  if phasePred st 
  then if phaseDonePred st then phaseDone st else st
  else nextPhaseToCheck st

checkProlog1 :: State -> State
checkProlog1 = phaseCheck isInProlog guardCheck setPrologDone1 checkProlog2

checkProlog2 :: State -> State
checkProlog2 = phaseCheck isInProlog2 guardCheck setPrologDone2 checkElder

checkElder :: State -> State
checkElder = phaseCheck isInElderCheck oldManCheck setElderDone checkShaman

checkShaman :: State -> State
checkShaman = phaseCheck isInNewShaman shamanCheck setShamanMovedDone checkPlatform

checkPlatform :: State -> State
checkPlatform = phaseCheck isInStoodOnPlatform platformCheck setStoodOnPlatformDone checkFlowers

checkFlowers :: State -> State
checkFlowers = phaseCheck isInFlowers flowerCheck setFlowerDone checkOld1

checkOld1 = phaseCheck isInOldShaman1 oldShamanCheck1 setOld1 checkOld2
checkOld2 = phaseCheck isInOldShaman2 oldShamanCheck2 setOld2 checkOld3
checkOld3 = phaseCheck isInOldShaman3 oldShamanCheck3 setOld3 checkOld4
checkOld4 = phaseCheck isInOldShaman4 oldShamanCheck4 setOld4 checkKicked

checkKicked = phaseCheck isInPlayerKicked kickedCheck setPlayerKicked checkInMonster

checkInMonster = phaseCheck isInPlayerInMonster inMonsterCheck setInMonster checkVomit

checkVomit = phaseCheck isInVomitted vomitCheck setVomitted checkEpilog

checkEpilog = phaseCheck isInEpilog epilogCheck setEpilog id

-- Phase Helper
isInPhaseX :: (Maybe EventState -> Bool) -> State -> Bool
isInPhaseX f st = isEventSensitive st && (not . f . eventState) st

isInProlog :: State -> Bool
isInProlog = isInPhaseX inProlog
isInProlog2 = isInPhaseX inProlog2
isInElderCheck = isInPhaseX $ inElder 
isInNewShaman = isInPhaseX inNewShaman
isInFlowers = isInPhaseX inOldLady
isInOldShaman1 = isInPhaseX inOldShaman1
isInOldShaman2 = isInPhaseX inOldShaman2
isInOldShaman3 = isInPhaseX inOldShaman3
isInOldShaman4 = isInPhaseX inOldShaman4
isInPlayerInMonster = isInPhaseX inPlayerInMonster
isInVomitted = isInPhaseX inVomitted
isInEpilog = isInPhaseX inEpilog
isInStoodOnPlatform = isInPhaseX inStoodOnPlatform
isInPlayerKicked = isInPhaseX inPlayerKicked


inPhase :: (EventState -> Bool) -> Maybe EventState -> Bool
inPhase f (Just es) = f es
inPhase f Nothing = False

inProlog = inPhase prolog
inProlog2 = inPhase prolog2
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
inStoodOnPlatform = inPhase stoodOnPlatform
inPlayerKicked = inPhase playerKicked


     
--Phase Done functions, including state transition
setPhaseDone :: (EventState -> EventState) -> (State -> State) -> State -> State
setPhaseDone f g st 
  | isEventSensitive st = 
    let Just es = eventState st
    in g $ updateEventState st $ f es
  | otherwise = st


setPrologDone1 :: State -> State
setPrologDone1 = 
  setPhaseDone (\es -> es { prolog = True}) 
    (startGuardDialog . moveFemaleOutOfHouse . unlockGuardDialog2)

setPrologDone2 :: State -> State
setPrologDone2 = 
  setPhaseDone (\es -> es { prolog2 = True}) 
    (dialogUpdate Player.sacrifice . moveGuard . moveFemale)

setElderDone = 
  setPhaseDone (\es -> es { oldManInfo = True}) 
    (closeMonsterMouth . unlockShamanDialog)
setStoodOnPlatformDone = 
  setPhaseDone (\es -> es { stoodOnPlatform = True}) 
    (dialogUpdate Player.onPlatformWithoutFlowers . unlockFlowerDialog)
setFlowerDone = 
  setPhaseDone (\es -> es { hasFlowers = True}) id
setShamanMovedDone = 
  setPhaseDone (\es -> es { shamanMovedAside = True}) 
    updateNewShamanTick
setOld1 = 
  setPhaseDone (\es -> es { oldShamanPartOne = True}) 
    (sendOldShamanToPlatform . deactivateMainPlatform)
setOld2 = 
  setPhaseDone (\es -> es { oldShamanOnPlatform = True}) 
    reactivateMainPlatform
setOld3 = 
  setPhaseDone (\es -> es { oldShamanOnPlatformAtTop = True}) 
  sendOldShamanToEdge
setOld4 = 
  setPhaseDone (\es -> es { oldShamanOverMonster = True})
  unlockLastDialogOfOldShaman
setPlayerKicked = 
  setPhaseDone (\es -> es { playerKicked = True})
  kickPlayer
setInMonster = 
  setPhaseDone (\es -> es { playerInMonster = True})
  vomit
setVomitted = 
  setPhaseDone (\es -> es { playerAndFemaleOutOfMonster = True})
  (startLastDialog . sendMonsterAway)
setEpilog = 
  setPhaseDone (\es -> es { epilog = True})
  winGame


--
hengeIsActive :: State -> Bool
hengeIsActive st = not (
  isInProlog st || isInProlog2 st || isInElderCheck st ||
  isInNewShaman st || isInStoodOnPlatform st || isInFlowers st )