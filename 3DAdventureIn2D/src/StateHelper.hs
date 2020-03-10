{-# LANGUAGE FlexibleInstances #-}

module StateHelper where

import Actor
import State
import Player
import Constants
import qualified GameState as GS
import qualified Characters.Elder as Elder
import qualified Characters.Female as Female
import qualified Characters.Guard as Guard
import qualified Characters.LastLift as LastLift
import qualified Characters.NewShaman as NewShaman
import qualified Characters.OldShaman as OldShaman
import qualified Characters.OldLady as OldLady
import qualified Characters.Boss as Boss
import qualified Characters.Platform as Platform

-- State Transitions
moveFemaleOutOfHouse :: State -> State
moveFemaleOutOfHouse = 
  mapActor Female.name Female.moveFemaleOutOfHouse

startGuardDialog :: State -> State
startGuardDialog = dialogUpdateN Guard.name

unlockGuardDialog2 :: State -> State
unlockGuardDialog2 = 
  mapActor Guard.name $ addText Guard.guardDialog2

moveGuard :: State -> State
moveGuard = 
  mapActor Guard.name Guard.moveGuard

moveFemale :: State -> State
moveFemale = 
  mapActor Female.name Female.moveFemale

unlockShamanDialog :: State -> State
unlockShamanDialog = 
  mapActor NewShaman.name $ NewShaman.addShamansText

unlockFlowerDialog :: State -> State
unlockFlowerDialog = 
  mapActor OldLady.name $ addText OldLady.oldLadyDialog2

updateNewShamanTick :: State -> State
updateNewShamanTick = 
  mapActor NewShaman.name NewShaman.moveAside

sendOldShamanToPlatform :: State -> State
sendOldShamanToPlatform = 
  mapActor OldShaman.name OldShaman.goToPlatform

deactivateMainPlatform :: State -> State
deactivateMainPlatform = 
  mapActor Platform.plazaPlatformName $ 
    newTick $ moveToAndIdle [plazaPlatformStart] platformSpeed

reactivateMainPlatform :: State -> State
reactivateMainPlatform = 
  mapActor Platform.plazaPlatformName $ 
    newTick $ 
      moveBetweenAndIdleFor 3 
        [plazaPlatformStart, plazaPlatformEnd] platformSpeed

sendOldShamanToEdge :: State -> State
sendOldShamanToEdge = 
  mapActor OldShaman.name OldShaman.goToEdge

unlockLastDialogOfOldShaman :: State -> State
unlockLastDialogOfOldShaman =
  mapActor OldShaman.name $ addText OldShaman.lastDialog

kickPlayer :: State -> State
kickPlayer st = 
  st { player = getKicked $ player st}

vomit :: State -> State
vomit = vomitPlayer . vomitFemale . vomitBoss

vomitBoss :: State -> State
vomitBoss = 
  mapActor Boss.name $ Boss.vomit

vomitPlayer :: State -> State
vomitPlayer st =
  st { player = getVomitted $ player st} 

vomitFemale :: State -> State
vomitFemale = mapActor Female.name Female.getVomitted

startLastDialog :: State -> State
startLastDialog = dialogUpdateN Female.name

sendMonsterAway :: State -> State
sendMonsterAway = mapActor Boss.name Boss.flee

winGame :: State -> State
winGame st = st { gameState = GS.GameWon}

closeMonsterMouth :: State -> State
closeMonsterMouth = mapActor Boss.name Boss.closeMouth

-- Phase State Check Helper
eaCheck :: (Actor -> Bool) -> (GS.EventActors -> (String,Actor)) ->
           State -> Bool
eaCheck f g = f . snd . g . eventActors

eaTalkCheck :: (GS.EventActors -> (String,Actor)) -> State -> Bool
eaTalkCheck f st = eaCheck finishedTalking f st &&
  (not . playerTalks) st 

guardCheck :: State -> Bool
guardCheck = eaTalkCheck GS.guard

platformCheck :: State -> Bool
platformCheck = stoodOnPlatformCheck

stoodOnPlatformCheck :: State -> Bool
stoodOnPlatformCheck = isPlayerOnHengeLift . player

oldManCheck :: State -> Bool
oldManCheck = eaTalkCheck GS.elder

shamanCheck :: State -> Bool
shamanCheck = eaTalkCheck GS.newShaman

flowerCheck :: State -> Bool
flowerCheck = eaTalkCheck GS.oldLady

oldShamanCheck1 :: State -> Bool
oldShamanCheck1 = eaTalkCheck GS.oldShaman

oldShamanCheck2 :: State -> Bool
oldShamanCheck2 = oldShamanInFirstTarget
oldShamanInFirstTarget = 
  eaCheck OldShaman.isInFirstTarget GS.oldShaman

oldShamanCheck3 :: State -> Bool
oldShamanCheck3 = oldShamanInSecondTarget
oldShamanInSecondTarget = 
  eaCheck OldShaman.isInSecondTarget GS.oldShaman

oldShamanCheck4 :: State -> Bool
oldShamanCheck4 = eaTalkCheck GS.oldShaman

inMonsterCheck :: State -> Bool
inMonsterCheck = (==) playerVomitTarget . pos . player

vomitCheck :: State -> Bool
vomitCheck = eaCheck ((==) Female.vomitTarget . aPos) GS.female

epilogCheck :: State -> Bool
epilogCheck = eaTalkCheck GS.female

