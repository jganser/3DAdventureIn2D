{-# LANGUAGE FlexibleInstances #-}

module GameState where

import qualified Characters.Elder as Elder
import qualified Characters.Female as Female
import qualified Characters.Guard as Guard
import qualified Characters.LastLift as LastLift
import qualified Characters.NewShaman as NewShaman
import qualified Characters.OldShaman as OldShaman
import qualified Characters.OldLady as OldLady
import qualified Characters.Boss as Boss
import qualified Characters.Platform as Platform
import Actor hiding (lookupActor)
import Drawable
import Graphics.Proc (P3)
import Prelude hiding (lookup,map)
import qualified Prelude as Pre (map)

class EventHolder a where
  eventState :: a -> Maybe EventState
  isEventSensitive :: a -> Bool
  updateEventState :: a -> EventState -> a 

data GameState = Running EventState | GameOver | GameWon | Start deriving (Eq, Ord, Show)

data EventState = ES {
    prolog :: Bool,                         --   1
    oldManInfo :: Bool,                     --   2
    hasFlowers :: Bool,                     --   3
    shamanMovedAside :: Bool,               --   4    
    oldShamanPartOne :: Bool,               --   5
    oldShamanOnPlatform :: Bool,            --   6
    oldShamanOnPlatformAtTop :: Bool,       --   7
    oldShamanOverMonster :: Bool,           --   8
    playerInMonster :: Bool,                --   9
    playerAndFemaleOutOfMonster :: Bool,    --  10
    epilog :: Bool,                         --  11
    stoodOnPlatform :: Bool,                --  12
    prolog2 :: Bool,                        --  13
    playerKicked :: Bool                    --  14
} deriving (Eq, Ord, Show)


instance EventHolder GameState where
  eventState GameOver = Nothing
  eventState GameWon = Nothing
  eventState Start = Nothing
  eventState (Running es) = Just es
  isEventSensitive GameOver = False
  isEventSensitive GameWon = False
  isEventSensitive Start = False
  isEventSensitive (Running _) = True
  updateEventState GameOver _ = GameOver
  updateEventState GameWon  _ = GameWon
  updateEventState Start _ = Start
  updateEventState (Running _) es = Running es

--     1     2    3     4      5    6     7      8    
--       9    10    11    12    13    14
newEventState = 
  ES False False False False False False False False 
      False False False False False False


eventStateAsList (ES p1 omi hf sm o1 o2 o3 o4 pim pafom ep som p2 pk) = [p1,omi,hf,sm,o1,o2,o3,o4,pim,pafom,ep,som,p2,pk]
eventStateSum = sum . Pre.map (\b -> case b of {True -> 1; False -> 0}) . eventStateAsList

type EventActors = EventActor (String,Actor) 

data EventActor a = EA {
  elder :: a,
  female :: a,
  guard :: a,
  lastLift :: a,
  hengeLift :: a,
  newShaman :: a,
  oldShaman :: a,
  oldLady :: a,
  boss :: a,
  hengePlatform :: a,
  plazaPlatform :: a,
  roundTripPlatform :: a
}

instance Tickable EventActors where
  tick dt = mapA $ Actor.tick dt

instance Drawable EventActors where
  drawAt daytime pos = mapM_ (\(n,a) -> drawAt daytime pos a) . asList
  path z = foldr (\(n,a) p -> p ++ path z a) []
  boundingBox = const emptyBoundingBox
  newDraw = const id

instance Foldable EventActor where
  foldr f z = Prelude.foldr f z . asList

  

startEventActors = EA 
  Elder.elder 
  Female.female 
  Guard.guard 
  LastLift.lastLift 
  LastLift.hengeLift
  NewShaman.newShaman
  OldShaman.oldShaman
  OldLady.oldLady
  Boss.boss
  Platform.hengePlatform
  Platform.plazaPlatform
  Platform.roundTripPlatform


instance Functor EventActor where
  fmap f ea = ea {
    elder = f $ elder ea,
    female = f $ female ea,
    guard = f $ guard ea,
    lastLift = f $ lastLift ea,
    hengeLift = f $ hengeLift ea,
    newShaman = f $ newShaman ea,
    oldShaman = f $ oldShaman ea,
    oldLady = f $ oldLady ea,
    boss = f $ boss ea,
    hengePlatform = f $ hengePlatform ea,
    plazaPlatform = f $ plazaPlatform ea,
    roundTripPlatform = f $ roundTripPlatform ea
  }

mapA :: ((Actor) -> (Actor)) -> EventActors -> EventActors
mapA f ea = ea {
    elder = let (n,a) = elder ea in (,) n $ f a,
    female = let (n,a) = female ea in (,) n $ f a,
    guard = let (n,a) = guard ea in (,) n $ f a,
    lastLift = let (n,a) = lastLift ea in (,) n $ f a,
    hengeLift = let (n,a) = hengeLift ea in (,) n $ f a,
    newShaman = let (n,a) = newShaman ea in (,) n $ f a,
    oldShaman = let (n,a) = oldShaman ea in (,) n $ f a,
    oldLady = let (n,a) = oldLady ea in (,) n $ f a,
    boss = let (n,a) = boss ea in (,) n $ f a,
    hengePlatform = let (n,a) = hengePlatform ea in (,) n $ f a,
    plazaPlatform = let (n,a) = plazaPlatform ea in (,) n $ f a,
    roundTripPlatform = let (n,a) = roundTripPlatform ea in (,) n $ f a
  }

asList :: EventActor a -> [a]
asList (EA e f g l h n os ol b hp pp rp) = [e, f, g, l, h, n, os, ol, b, hp, pp, rp]

lookup :: String -> EventActors -> Maybe (String,Actor)
lookup n ea = case n of
  "elder"             -> Just $ elder ea
  "female"            -> Just $ female ea
  "guard"             -> Just $ guard ea
  "lastLift"          -> Just $ lastLift ea
  "hengeLift"         -> Just $ hengeLift ea
  "newShaman"         -> Just $ newShaman ea
  "oldShaman"         -> Just $ oldShaman ea
  "oldLady"           -> Just $ oldLady ea
  "boss"              -> Just $ boss ea
  "hengePlatform"     -> Just $ hengePlatform ea
  "plazaPlatform"     -> Just $ plazaPlatform ea
  "roundTripPlatform" -> Just $ roundTripPlatform ea
  _                   -> Nothing

lookupActor :: String -> EventActors -> Actor
lookupActor n ea = case lookup n ea of
    Just (n,a) -> a
    Nothing    -> emptyActor

replace :: (String,Actor) -> EventActors -> EventActors
replace (n,a) ea = case n of
  "elder"             -> ea {elder = (n,a)}
  "female"            -> ea {female = (n,a)}
  "guard"             -> ea {guard = (n,a)}
  "lastLift"          -> ea {lastLift = (n,a)}
  "hengeLift"         -> ea {hengeLift = (n,a)}
  "newShaman"         -> ea {newShaman = (n,a)}
  "oldShaman"         -> ea {oldShaman = (n,a)}
  "oldLady"           -> ea {oldLady = (n,a)}
  "boss"              -> ea {boss = (n,a)}
  "hengePlatform"     -> ea {hengePlatform = (n,a)}
  "plazaPlatform"     -> ea {plazaPlatform = (n,a)}
  "roundTripPlatform" -> ea {roundTripPlatform = (n,a)}
  _                   -> ea

onEventActorCheck :: String -> P3 -> EventActors -> Maybe String
onEventActorCheck n xyz ea = do
    (_,a) <- lookup n ea
    if onActorCheck xyz a then Just n
    else Nothing

onAnyEventActorCheck :: P3 -> EventActors -> Maybe String
onAnyEventActorCheck xyz = foldr (
    \(n,a) m -> case m of {
        Just mn -> Just mn;
        _ -> if onActorCheck xyz a 
             then Just n
             else Nothing }) Nothing

onAnyEventActor :: P3 -> EventActors -> (String, Actor, Bool)
onAnyEventActor p ea = 
  case onAnyEventActorCheck p ea of
    Just n -> (n, lookupActor n ea, True)
    Nothing -> ("",emptyActor, False)

onAnyTalkingEventActor :: P3 -> EventActors -> (String, Actor, Bool)
onAnyTalkingEventActor p ea = 
  let (n,na,b) = onAnyEventActor p ea
  in  if b 
      then 
        if talking na && (not $ finishedTalking na) 
        then (n,na,b) 
        else ("",emptyActor, False)
      else
        (n,na,b)