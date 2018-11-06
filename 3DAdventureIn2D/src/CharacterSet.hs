module CharacterSet(
    drawPText
  , drawPTextRaw 
  , PText(..)
  ) where

import Graphics.Proc hiding (e, size)
import ObjectUtils (timesF)
import Data.Foldable (foldrM)
import Data.Char (toLower)
import DayTime

type CharacterBitMap = [[Integer]]

type Size = Int

-- text := string that shall be displayed
-- pos  := left upper position of the text
-- size := how big the letters shall get (1 means 7x5 px; 2 means 14x10 px; ...; size < 0 means 7x5 px)
data PText = PText { text :: String, pos :: P2,  size :: Size, bgc :: Col, tc :: Col }

drawPTextRaw :: PText -> Draw 
drawPTextRaw ptext = drawText (pos ptext) (size ptext) (text ptext) (bgc ptext) (tc ptext)

drawPText :: PText -> DayTime -> Draw 
drawPText ptext dt = drawText (pos ptext) (size ptext) (text ptext) (colorForDayTime (bgc ptext) dt) (colorForDayTime (tc ptext) dt)

drawText :: P2 -> Size -> String -> Col -> Col -> Draw
drawText pos size string bgc tc = do 
    foldrM (drawCBitMapPlus size bgc tc) pos $ reverse $ map characterToBitMap $ map toLower string
    return ()

drawCBitMapPlus :: Size -> Col-> Col -> CharacterBitMap -> P2 -> Pio P2
drawCBitMapPlus size bgc ct cbmap pos = do
    let rcbm = resizeCBitMap size cbmap
    let evbm = generateVerticalEmptyBitMap size
    drawCBitMap bgc ct pos evbm -- little space left
    let pos' = (pos + (timesF (1,0) $ fromIntegral size))
    drawCBitMap bgc ct pos' rcbm -- the character
    let pos'' = (pos' + (timesF (5,0) $ fromIntegral size))
    drawCBitMap bgc ct pos'' evbm -- little space right 
    return $ pos'' + (timesF (1,0) $ fromIntegral size)

drawCBitMap :: Col -> Col -> P2 -> CharacterBitMap -> Draw
drawCBitMap bgc tc pos cbm = do
    foldrM (\row p -> drawRow bgc tc row p) pos cbm
    return ()

drawRow :: Col -> Col -> [Integer] -> P2 -> Pio P2
drawRow bgc tc row pos@(px,py) = do
    foldrM (\cell p -> drawCell bgc tc cell p) pos row
    return (px, py-1)

drawCell :: Col -> Col -> Integer -> P2 -> Pio P2
drawCell bgc tc cell pos@(px,py) = do
    let color = if cell == 0 then bgc else tc
    strokeFill color
    point pos
    return (px+1,py)



generateVerticalEmptyBitMap :: Size -> CharacterBitMap
generateVerticalEmptyBitMap i = resizeCBitMap i emptyVerticalLine

emptyVerticalLine :: [[Integer]]
emptyVerticalLine = [
        [0]
      , [0]
      , [0]
      , [0]
      , [0]
      , [0]
      , [0]
    ]

resizeCBitMap :: Size -> CharacterBitMap -> CharacterBitMap
resizeCBitMap i cbm = duplicateRows i $ map (duplicateEntries i) cbm

duplicateRows :: Size -> [[Integer]] -> [[Integer]]
duplicateRows i = foldr (\row outs -> (take i (repeat row )) ++ outs) []

duplicateEntries :: Size -> [Integer] -> [Integer]
duplicateEntries i = foldr (\e outs -> outs ++ (take i (repeat e)) ) []

characterToBitMap character = case character of
    'a' -> a
    'b' -> b
    'c' -> c
    'd' -> d
    'e' -> e
    'f' -> f
    'g' -> g
    'h' -> h
    'i' -> i
    'j' -> j
    'k' -> k
    'l' -> l
    'm' -> m
    'n' -> n
    'o' -> o
    'p' -> p
    'q' -> q
    'r' -> r
    's' -> s
    't' -> t
    'u' -> u
    'v' -> v
    'w' -> w
    'x' -> x
    'y' -> y
    'z' -> z
    ' ' -> space
    _   -> unknown


a = [
        [ 0, 0, 1, 0, 0]
      , [ 0, 1, 0, 1, 0] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
    ]

b = [
        [ 1, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 0]
    ]

c = [
        [ 0, 0, 1, 1, 0]
      , [ 0, 1, 0, 0, 1] 
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 0, 1, 0, 0, 1]
      , [ 0, 0, 1, 1, 0]
    ]

d = [
        [ 1, 1, 1, 0, 0]
      , [ 1, 0, 0, 1, 0] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 1, 0]
      , [ 1, 1, 1, 0, 0]
    ]

e = [
        [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 0] 
      , [ 1, 0, 0, 0, 0]
      , [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 1, 1, 1, 1]
    ]

f = [
        [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 0] 
      , [ 1, 0, 0, 0, 0]
      , [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
    ]

g = [
        [ 0, 0, 1, 1, 0]
      , [ 0, 1, 0, 0, 1] 
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 1, 1]
      , [ 0, 1, 0, 0, 1]
      , [ 0, 0, 1, 1, 0]
    ]

h = [ 
        [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
    ]

i = [ 
        [ 1, 1, 1, 1, 1]
      , [ 0, 0, 1, 0, 0] 
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 1, 1, 1, 1, 1]
    ]

j = [ 
        [ 1, 1, 1, 1, 1]
      , [ 0, 0, 0, 0, 1] 
      , [ 0, 0, 0, 0, 1]
      , [ 0, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 1, 0]
      , [ 0, 1, 1, 0, 0]
    ]

k = [ 
        [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 1, 0] 
      , [ 1, 0, 1, 0, 0]
      , [ 1, 1, 0, 0, 0]
      , [ 1, 0, 1, 0, 0]
      , [ 1, 0, 0, 1, 0]
      , [ 1, 0, 0, 0, 1]
    ]

l = [ 
        [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0] 
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 1, 1, 1, 1]
    ]

m = [ 
        [ 1, 0, 0, 0, 1]
      , [ 1, 1, 0, 1, 1] 
      , [ 1, 0, 1, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
    ]

n = [ 
        [ 1, 1, 0, 0, 1]
      , [ 1, 1, 0, 0, 1] 
      , [ 1, 0, 1, 0, 1]
      , [ 1, 0, 1, 0, 1]
      , [ 1, 0, 1, 0, 1]
      , [ 1, 0, 0, 1, 1]
      , [ 1, 0, 0, 1, 1]
    ]

o = [ 
        [ 0, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 0, 1, 1, 1, 0]
    ]

p = [ 
        [ 1, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
    ]

q = [ 
        [ 0, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 1, 0, 1]
      , [ 1, 0, 0, 1, 0]
      , [ 0, 1, 1, 0, 1]
    ]

r = [ 
        [ 1, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 0]
      , [ 1, 0, 1, 0, 0]
      , [ 1, 0, 0, 1, 0]
      , [ 1, 0, 0, 0, 1]
    ]

s = [ 
        [ 0, 1, 1, 1, 0]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 0]
      , [ 0, 1, 1, 1, 0]
      , [ 0, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 0, 1, 1, 1, 0]
    ]

t = [ 
        [ 1, 1, 1, 1, 1]
      , [ 0, 0, 1, 0, 0] 
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
    ]

u = [ 
        [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 0, 1, 1, 1, 0]
    ]

v = [ 
        [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 0, 1, 0, 1, 0]
      , [ 0, 1, 0, 1, 0]
      , [ 0, 1, 0, 1, 0]
      , [ 0, 0, 1, 0, 0]
    ]

w = [ 
        [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 0, 1, 0, 1]
      , [ 1, 1, 0, 1, 1]
      , [ 1, 0, 0, 0, 1]
    ]

x = [ 
        [ 1, 0, 0, 0, 1]
      , [ 0, 1, 0, 1, 0] 
      , [ 0, 1, 0, 1, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 1, 0, 1, 0]
      , [ 0, 1, 0, 1, 0]
      , [ 1, 0, 0, 0, 1]
    ]

y = [ 
        [ 1, 0, 0, 0, 1]
      , [ 1, 0, 0, 0, 1] 
      , [ 0, 1, 0, 1, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 0, 1, 0, 0]
    ]

z = [ 
        [ 1, 1, 1, 1, 1]
      , [ 0, 0, 0, 0, 1] 
      , [ 0, 0, 0, 1, 0]
      , [ 0, 0, 1, 0, 0]
      , [ 0, 1, 0, 0, 0]
      , [ 1, 0, 0, 0, 0]
      , [ 1, 1, 1, 1, 1]
    ]

space = [ 
        [ 0, 0, 0, 0, 0]
      , [ 0, 0, 0, 0, 0] 
      , [ 0, 0, 0, 0, 0]
      , [ 0, 0, 0, 0, 0]
      , [ 0, 0, 0, 0, 0]
      , [ 0, 0, 0, 0, 0]
      , [ 0, 0, 0, 0, 0]
    ]

unknown = [ 
        [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 1] 
      , [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 1]
      , [ 1, 0, 0, 0, 1]
      , [ 1, 1, 1, 1, 1]
    ]