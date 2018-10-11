{-# LANGUAGE DuplicateRecordFields #-}


module Objects(
    sphere 
    ,ellipsoide
    ,cube
    ,fullCube
    ,gLine
    ,boss
    ,trd
    ,cylinder
    ,henge
    --,lineDistance
    ,rectPath
    ,
    ) where

import Graphics.Proc
import Drawable
import Geometry
import Actor



drawCube col strkW t (_,_,z) | outOfRange t z = return ()
      | otherwise = do 
        stroke col
        strokeWeight strkW
        linePath [leftLower, leftUpper, rightUpper, rightLower, leftLower]
        where 
            (sx,sy,sz) = scaling t
            (cx,cy) = centerOf $ transition t
            leftLower = (cx - sx/2, cy + sy/2)
            leftUpper = (cx - sx/2, cy - sy/2)
            rightLower = (cx + sx/2, cy + sy/2)
            rightUpper = (cx + sx/2, cy - sy/2)

cubePath sw t z | outOfRange t z = []
      | otherwise = linePathing sw leftLower leftUpper ++ 
                    linePathing sw leftUpper rightUpper ++
                    linePathing sw rightUpper rightLower ++
                    linePathing sw rightLower leftLower 
        where
            (sx,sy,sz) = scaling t
            (cx,cy) = centerOf $ transition t
            leftLower = (cx - sx/2, cy + sy/2)
            leftUpper = (cx - sx/2, cy - sy/2)
            rightLower = (cx + sx/2, cy + sy/2)
            rightUpper = (cx + sx/2, cy - sy/2)

drawFullCube col t (_,_,z) | outOfRange t z = return ()
      | otherwise = do
        strokeFill col
        strokeWeight 1
        rectMode Corners
        rect leftLower rightUpper         
        where 
            (sx,sy,sz) = scaling t
            (cx,cy) = centerOf $ transition t
            leftLower = (cx - sx/2, cy + sy/2)
            rightUpper = (cx + sx/2, cy - sy/2)


outOfRange :: Transform -> Float -> Bool
outOfRange (T (_,_,tz) _ (_,_,sz)) z = z < tz - sz/2 || z > tz + sz/2 

lineOutOfRange :: Transform -> Float -> Bool
lineOutOfRange (T (_,_,tz) _ _) z = tz /= z 

drawLine sw col t@(T (tx,ty,_) _ (sx,sy,_)) (_,_,z) | lineOutOfRange t z = return ()
      | otherwise = do
        strokeWeight sw
        stroke col
        line begin end
        where
            begin = (tx,ty)
            end = (sx,sy)
    
gLinePath sw t z | lineOutOfRange t z = []
      | otherwise = linePathing sw beg end
        where            
            (bx,by,_) = transition t
            beg = (bx,by)
            (ex,ey,_) = scaling t
            end = (ex,ey)

linePathing sw beg@(bx,by) end@(ex,ey) = [p | p <- rect, isOnLine sw beg end p]
    where
        rect = [(x,y) | x <- [bsx..esx], y <- [bsy..esy]]
        sr = sw
        bsx = if bx < ex then bx - sr else ex - sr
        esx = if bx < ex then ex + sr else bx + sr
        bsy = if by < ey then by - sr else ey - sr
        esy = if by < ey then ey + sr else by + sr


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
    

drawEllipsoide col t (_,_,z) | outOfRange t z = return ()
      | otherwise = do
        strokeFill col
        strokeWeight 1
        ellipse (centerOf $ transition t) $ whAt t $ distE t z


ellipsoidePath t z 
  | outOfRange t z = []
  | otherwise = [p | p <- rectPath t z, insideEllipse f1 f2 a p]    
    where
        (wx,wy) = whAt t $ distE t z
        (rx,ry) = (wx/2, wy/2)
        (cx,cy) = centerOf $ transition t
        a = if wx > wy then rx else ry
        focalOffset = if wx > wy then sqrt (rx^2 - ry^2) else sqrt (ry^2 - rx^2)
        (f1,f2) = if wx > wy 
                  then ((cx,cy) + (-focalOffset,0), (cx,cy) + (focalOffset,0))
                  else ((cx,cy) + (0,-focalOffset), (cx,cy) + (0,focalOffset))


centerOf :: P3 -> P2
centerOf (x,y,z) = (x,y)

whAt :: Transform -> Float -> P2
whAt (T _ _ (x,y,_)) d = (sliceR x, sliceR y)
    where sliceR axis = sqrt $ (axis/2) ^ 2 - d ^ 2

distE :: Transform -> Float -> Float
distE t z = abs $ (-) z $ trd $ transition t

trd :: (a,b,c) -> c
trd (_,_,x) = x


sphere :: P3 -> Float -> Col -> Geometry
sphere transition width col = Geo (T transition (0,0,0) (width, width, width)) (drawEllipsoide col) ellipsoidePath

cube :: Transform -> Float -> Col -> Geometry
cube t strkW col = Geo t (drawCube col strkW) (cubePath strkW)

fullCube :: Transform -> Col -> Geometry
fullCube t col = Geo t (drawFullCube col) rectPath

gLine :: P2 -> P2 -> Float -> Float -> Col -> Geometry
gLine (bx,by) (ex,ey) z strkW col = Geo (T (bx,by,z) (0,0,0) (ex,ey,z)) (drawLine strkW col) (gLinePath strkW)

ellipsoide :: Transform -> Col -> Geometry
ellipsoide t col = Geo t (drawEllipsoide col) ellipsoidePath

boss :: Transform -> Actor
boss t = A (Geo t drawBoss rectPath) bossTick  --TODO maybe rectpath is not suited that well


drawBoss :: Transform -> P3 -> Draw
drawBoss t (px,py,pz) 
  | outOfRange t pz = return ()
  | otherwise = do 
        stroke black
        strokeWeight 6
        -- corpus
        line leftUpper rightUpper
        line rightUpper rightLower
        line rightLower leftLower

        -- mouth --TODO
        line (fst leftLower, leftFstThirdUp) (fst leftLower, leftSndThirdUp)

        -- round stuff
        strokeWeight 1
        -- eyes
        fill white
        circle eyeRadius lowerEyeMid
        circle eyeRadius upperEyeMid
        -- pupils
        strokeFill black
        circle pupilRadius lowerPupilMid
        circle pupilRadius upperPupilMid
        where 
            (sx,sy,sz) = scaling t
            (cx,cy) = centerOf $ transition t
            leftLower = (cx - sx/2, cy + sy/2)
            leftUpper = (cx - sx/2, cy - sy/2)
            rightLower = (cx + sx/2, cy + sy/2)
            rightUpper = (cx + sx/2, cy - sy/2)
            third = (snd leftUpper - snd leftLower)/3
            leftFstThirdUp :: Float
            leftFstThirdUp = third + snd leftLower
            leftSndThirdUp :: Float
            leftSndThirdUp = 2*third + snd leftLower
            eyeRadius :: Float
            eyeRadius = (leftFstThirdUp - snd leftLower)/2
            --lowerEyeMid = (fst leftLower, eyeRadius + snd leftLower)
            --upperEyeMid = (fst leftUpper, snd leftUpper - eyeRadius)
            lowerEyeMid = leftLower
            upperEyeMid = leftUpper
            lowerEyeVec = (px,py) - lowerEyeMid
            lowerEyeVecNorm = norm2 lowerEyeVec
            upperEyeVec = (px,py) - upperEyeMid
            upperEyeVecNorm = norm2 upperEyeVec
            pupilRadius :: Float
            pupilRadius = eyeRadius / 2
            lowerPupilMid = lowerEyeMid - (lowerEyeVecNorm `timesF` pupilRadius)
            upperPupilMid = upperEyeMid - (upperEyeVecNorm `timesF` pupilRadius)


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

bossTick a = a

cylinder :: Transform -> Col -> Float -> Geometry
cylinder t col sw = Geo t (drawCylinder col col sw) cylinderPath

drawCylinder col scol sw t (_,_,z) 
  | outOfRange t z = return ()
  | otherwise = do
    fill col
    stroke scol
    strokeWeight sw
    ellipse c (sx/2, sy/2)
    where
        (sx,sy,sz) = scaling t
        c = centerOf $ transition t

cylinderPath t z 
  | outOfRange t z = []
  | otherwise = [p | p <- rectPath t z, insideEllipse f1 f2 a p]    
    where
        (sx,sy,sz) = scaling t
        (rx,ry) = (sx/2, sy/2)
        (cx,cy) = centerOf $ transition t
        a = if sx > sy then rx else ry
        focalOffset = if sx > sy then sqrt (rx^2 - ry^2) else sqrt (ry^2 - rx^2)
        (f1,f2) = if sx > sy 
                  then ((cx,cy) + (-focalOffset,0), (cx,cy) + (focalOffset,0))
                  else ((cx,cy) + (0,-focalOffset), (cx,cy) + (0,focalOffset))

insideEllipse :: P2 -> P2 -> Float -> P2 -> Bool
insideEllipse f1 f2 a p = 
    distf2 + distf1 < 2*a 
    where
        distf1 = veclength2 (f1 - p)
        distf2 = veclength2 (f2 - p)

rectPath t z
  | outOfRange t z = []
  | otherwise = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
    where 
        (sx,sy,sz) = scaling t
        (rx,ry) = (sx/2, sy/2)
        (cx,cy) = centerOf $ transition t
        minX = cx - rx
        maxX = cx + rx
        minY = cy - ry
        maxY = cy + ry



drawHenge t@(T (x,y,z) r (sx,sy,sz)) p@(px,py,_) = do
    drawCylinder col scol sw (T north r (radius,radius,sz)) p
    drawCylinder col scol sw (T northeast r (radius,radius,sz)) p
    drawCylinder col scol sw (T east r (radius,radius,sz)) p
    drawCylinder col scol sw (T southeast r (radius,radius,sz)) p
    drawCylinder col scol sw (T south r (radius,radius,sz)) p
    drawCylinder col scol sw (T southwest r (radius,radius,sz)) p
    -- west is the opening
    drawCylinder col scol sw (T northwest r (radius,radius,sz)) p
    where
        sw = 1
        hengeRadX = sx/8
        hengeRadY = sy/8
        left = - hengeRadX
        right = hengeRadX
        down = hengeRadY
        up = -hengeRadY
        sqrt2 = sqrt 2 / 2
        north = flip tupleAdd z $ (x,y + up)
        northeast = flip tupleAdd z $ (x + right * sqrt2,y + up * sqrt2)
        east = flip tupleAdd z $ (x + right,y)
        southeast = flip tupleAdd z $ (x + right * sqrt2,y + down * sqrt2)
        south = flip tupleAdd z $ (x,y + down)
        southwest = flip tupleAdd z $ (x + left * sqrt2,y + down * sqrt2)
        west = flip tupleAdd z $ (x + left,y)
        northwest = flip tupleAdd z $ (x + left * sqrt2,y + up * sqrt2)
        radius = sx/12
        inXrange = x + sx/4 >= px && x - sx/4 <= px 
        inYrange = y + sy/4 >= py && y - sy/4 <= py
        (col,scol) = if inXrange && inYrange then (white,black) else (black,black)

henge t = Geo t drawHenge hengePath

hengePath = const $ const []