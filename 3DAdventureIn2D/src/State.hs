module State where

import Objects
import Levels
import Geometry
import Actor
import Constants
import DayTime
import Player
import Graphics.Proc


data GameState = Running | GameOver | GameWon


data State = ST {
    player :: Player, 
    objects :: [Geometry],
    actors :: [Actor],
    gameState :: GameState,
    daytime :: DayTime
}

emptyState = ST (newPlayer playerStart) [] [] Running Day

dotpos :: State -> P3
dotpos = pos . player 

newPos :: P3 -> State -> State
newPos pos st = st { player = updatePos pos $ player st }