module Drawable where

import Graphics.Proc

class Drawable a where
    drawAt :: P3 -> a -> Draw
    path :: Float -> a -> Path

type Path = [Field]
type Field = P2
