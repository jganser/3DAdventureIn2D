module ObjectUtils where


type Z = Float
type StrokeWeight = Float
type Diameter = Float

-- helper functions

outOf3DRange :: Transform -> Z -> Bool
outOf3DRange (T (_,_,tz) _ (_,_,sz)) z = z < tz - sz/2 || z > tz + sz/2 

outOf2DRange :: Transform -> Z -> Bool
outOf2DRange (T (_,_,tz) _ _) z = tz /= z 

insideEllipse :: P2 -> P2 -> Float -> P2 -> Bool
insideEllipse f1 f2 a p = 
    distf2 + distf1 < 2*a 
    where
        distf1 = veclength2 (f1 - p)
        distf2 = veclength2 (f2 - p)


norm2 :: (Float,Float) -> (Float,Float)
norm2 (0,0) = (0,0)
norm2 vec = vec `divF` veclength2 vec

veclength2 :: (Float,Float) -> Float
veclength2 (0,0) = 0
veclength2 (x,y) = sqrt (x^2 + y^2)

timesF :: (Float,Float) -> Float -> (Float,Float)
timesF (x,y) t = (t*x, t*y)

tupleAdd :: (a,a) -> b -> (a,a,b)
tupleAdd (x,y) z = (x,y,z)

divF :: (Float,Float) -> Float -> (Float,Float)
divF (x,y) t = (x/t,y/t)

centerOf :: P3 -> P2
centerOf (x,y,z) = (x,y)

whAt :: Transform -> Float -> P2
whAt (T _ _ (x,y,_)) d = (sliceR x, sliceR y)
    where sliceR axis = sqrt $ (axis/2) ^ 2 - d ^ 2

distE :: Transform -> Float -> Float
distE t z = abs $ (-) z $ trd $ transition t

trd :: (a,b,c) -> c
trd (_,_,x) = x

isOnLine :: Float -> P2 -> P2 -> P2 -> Bool
isOnLine sw beg end poi = (sw/2) > lineDistance beg end poi

lineDistance :: P2 -> P2 -> P2 -> Float 
lineDistance (bx,by) (ex,ey) (px,py) = 
        foldr (\x out -> if x < out then x else out) 10000 [distToLine, ditstToEnd, distToBeg]
    where        
        -- https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
        distToLine = (abs ((esy-bsy)*px - (esx-bsx)*py + esx*bsy - esy*bsx)) / (sqrt ((esy-bsy)^2 + (esx-bsx)^2)) 
        distToBeg = veclength2 $ (bsx,bsy) - (px,py)
        ditstToEnd = veclength2 $ (esx,esy) - (px,py) 
        bsx = if bx < ex then bx  else ex 
        esx = if bx < ex then ex  else bx 
        bsy = if by < ey then by  else ey 
        esy = if by < ey then ey  else by 
