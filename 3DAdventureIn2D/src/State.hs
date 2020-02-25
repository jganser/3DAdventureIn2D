module State where

import Objects
import Levels
import Geometry
import Drawable
import Actor
import Constants
import DayTime
import Player
import Graphics.Proc
import qualified Data.Vector as V



data GameState = Running | GameOver | GameWon

-- the actors shall be disjoint distributed over the 3 actor lists (unique occurence)
data State = ST {
    player :: Player, 
    objects :: [Geometry],
    staticActors :: [Actor], -- static actors can be used to make the path function faster
    movingActors :: [Actor], -- may be able to influence the player position
    eventActors :: [(String, Actor)], -- this list has a name to actor reference included to enable situational updates
    gameState :: GameState,
    daytime :: DayTime,
    staticPath :: StaticPath    
}

-- this structure shall be able to speed up the calculations necessary in each
-- time step
data StaticPath = SP Layer Layer Layer

emptyState = ST (newPlayer playerStart) [] [] [] [] Running Day (SP emptyLayer emptyLayer emptyLayer)
emptyLayer = V.replicate (round width) (V.replicate (round height) False)


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

staticPathAt :: Float -> StaticPath -> Layer
staticPathAt z (SP zero ten twenty) = case z of
    0 -> zero
    10 -> ten
    20 -> twenty
    _ -> emptyLayer

updateLayer :: Layer -> Bool -> Path -> Layer
updateLayer l add path =  V.accum (\v t -> v V.// t) l p
  where
    p :: [(Int,[(Int,Bool)])]
    p = Prelude.map (\(x,y) -> (round x,[(round y,add)])) path

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

