module Drawable where

import Graphics.Proc
import DayTime

class Drawable a where
    drawAt :: DayTime -> P3 -> a -> Draw
    path :: Float -> a -> Path

type Path = [Field]
type Field = P2
