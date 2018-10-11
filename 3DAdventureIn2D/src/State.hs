module State where

import Objects
import Levels
import Geometry
import Actor
import Constants
--import Graphics.Proc


data GameState = Running | GameOver | GameWon

data DayTime = Night | Day

data State = ST {
    dotPos :: (Float,Float,Float), 
    objects :: [Geometry],
    actors :: [Actor],
    gameState :: GameState,
    daytime :: DayTime
}

emptyState = ST playerStart [] [] Running Day

