module DayTime where

data DayTime = Night | Day

toggleDayTime :: DayTime -> DayTime
toggleDayTime Day = Night
toggleDayTime Night = Day