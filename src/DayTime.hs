module DayTime where

import Graphics.Proc

data DayTime = Night | Day deriving (Eq, Show)

toggleDayTime :: DayTime -> DayTime
toggleDayTime Day = Night
toggleDayTime Night = Day


colorForDayTime :: Col -> DayTime -> Col
colorForDayTime c Day = c
colorForDayTime (Col r g b a) Night = grey (((r + g + b)/3)*255)