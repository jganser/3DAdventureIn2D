{-# LANGUAGE DuplicateRecordFields #-}


module Objects(
      sphere 
    , ellipsoide
    , cube
    , fullCube
    , gLine
    , gRect
    , gFullRect
    , gSFullRect
    , gCirc
    , gSCirc
    , gEllipse
    , gSEllipse
    , cylinder
    , henge
    , movingActor
    , ellipsePath
    --,lineDistance
    ,fullCubePath
    --,
    ) where

import Graphics.Proc
import Drawable
import Geometry
import Actor
import ObjectUtils
import DayTime
import Colors

-- 2D Objects

-- Geometry of a line shape
-- bx,by -> x y coordnates of the start of the line
-- ex,ey -> x y coordnates of the end of the line
-- z     -> plane in which the rect will be visible
-- srtkW -> thickness of the outline
-- col   -> color of the stroke
gLine :: P2 -> P2 -> Z -> StrokeWeight -> Col -> Geometry
gLine (bx,by) (ex,ey) z strkW col = 
  Geo (T (bx,by,z) (0,0,0) (ex,ey,z)) (drawLine strkW col) (gLinePath strkW)

-- Geometry of a hollow rect shape
-- bx,by -> x y coordnates of the middle of the rect
-- ex,ey -> scale of the rect in x and y direction
-- z     -> plane in which the rect will be visible
-- srtkW -> thickness of the outline
-- col   -> color of the stroke
gRect :: P2 -> P2 -> Z -> StrokeWeight -> Col -> Geometry
gRect (bx,by) (ex,ey) z strkW col = 
  Geo (T (bx,by,z) (0,0,0) (ex,ey,0)) (drawRect col strkW) (rectPath strkW)

-- Geometry of a filled rect shape
-- bx,by -> x y coordnates of the middle of the rect
-- ex,ey -> scale of the rect in x and y direction
-- z     -> plane in which the rect will be visible
-- srtkW -> thickness of the outline
-- col   -> color of the stroke and the fill
gFullRect :: P2 -> P2 -> Z -> StrokeWeight -> Col -> Geometry
gFullRect (bx,by) (ex,ey) z strkW col = 
  Geo (T (bx,by,z) (0,0,0) (ex,ey,0)) (drawFullRect col col) fullRectPath

-- Geometry of a filled rect shape
-- bx,by -> x y coordnates of the middle of the rect
-- ex,ey -> scale of the rect in x and y direction
-- z     -> plane in which the rect will be visible
-- srtkW -> thickness of the outline
-- col   -> color of the fill
-- scol   -> color of the stroke
gSFullRect :: P2 -> P2 -> Z -> StrokeWeight -> Col -> Col -> Geometry
gSFullRect (bx,by) (ex,ey) z strkW col scol = 
  Geo (T (bx,by,z) (0,0,0) (ex,ey,0)) (drawFullRect col scol) fullRectPath

gCirc :: P2 -> Diameter -> Z -> Col -> Geometry
gCirc pos d = gEllipse pos (d,d) 

gEllipse :: P2 -> P2 -> Z -> Col -> Geometry
gEllipse (bx,by) (ex,ey) z col = 
  Geo (T (bx,by,z) (0,0,0) (ex,ey,0)) (drawEllipse col) ellipsePath

gSEllipse :: P2 -> P2 -> Z -> Col -> Col -> Geometry
gSEllipse (bx,by) (ex,ey) z col scol = 
  Geo (T (bx,by,z) (0,0,0) (ex,ey,0)) (drawSEllipse col scol) ellipsePath

gSCirc  :: P2 -> Diameter -> Z -> Col -> Col -> Geometry
gSCirc pos d = gSEllipse pos (d,d)

-- 3D Objects

sphere :: P3 -> Diameter -> Col -> Geometry
sphere transition width col = Geo (T transition (0,0,0) (width, width, width)) (drawEllipsoide col) ellipsoidePath

cube :: Transform -> StrokeWeight -> Col -> Geometry
cube t strkW col = Geo t (drawCube col strkW) (cubePath strkW)

fullCube :: Transform -> Col -> Geometry
fullCube t col = Geo t (drawFullCube col col) fullCubePath 

cylinder :: Transform -> Col -> StrokeWeight -> Geometry
cylinder t col sw = Geo t (drawCylinder col col sw) cylinderPath

ellipsoide :: Transform -> Col -> Geometry
ellipsoide t col = Geo t (drawEllipsoide col) ellipsoidePath

henge :: Transform -> Geometry
henge t = Geo t drawHenge hengePath

-- Draw functions
drawRect :: Col -> StrokeWeight -> Transform -> DayTime -> P3 -> Draw
drawRect = drawRectInRange outOf2DRange

drawCube :: Col -> StrokeWeight -> Transform -> DayTime -> P3 -> Draw
drawCube = drawRectInRange outOf3DRange

drawRectInRange :: (Transform -> Z -> Bool) -> Col -> StrokeWeight -> Transform -> DayTime -> P3 -> Draw
drawRectInRange rangeFunction col strkW t dt (_,_,z)  | rangeFunction t z = return ()
      | otherwise = do 
        stroke $ colorForDayTime col dt
        strokeWeight strkW
        linePath [leftLower, leftUpper, rightUpper, rightLower, leftLower]
        where 
            (sx,sy,sz) = scaling t
            (cx,cy) = centerOf $ transition t
            leftLower = (cx - sx/2, cy + sy/2)
            leftUpper = (cx - sx/2, cy - sy/2)
            rightLower = (cx + sx/2, cy + sy/2)
            rightUpper = (cx + sx/2, cy - sy/2)


drawFullRect :: Col -> Col -> Transform -> DayTime -> P3 -> Draw
drawFullRect = drawFullRectInRange outOf2DRange

drawFullCube :: Col -> Col -> Transform -> DayTime -> P3 -> Draw
drawFullCube = drawFullRectInRange outOf3DRange

drawFullRectInRange :: (Transform -> Z -> Bool) -> Col -> Col -> Transform -> DayTime -> P3 -> Draw
drawFullRectInRange rangeFunction col scol t dt (_,_,z) | rangeFunction t z = return ()
      | otherwise = do
        stroke $ colorForDayTime scol dt
        fill $ colorForDayTime col dt
        strokeWeight 1
        rectMode Corners
        rect leftLower rightUpper         
        where 
            (sx,sy,sz) = scaling t
            (cx,cy) = centerOf $ transition t
            leftLower = (cx - sx/2, cy + sy/2)
            rightUpper = (cx + sx/2, cy - sy/2)



drawLine :: StrokeWeight -> Col -> Transform -> DayTime -> P3 -> Draw
drawLine sw col t@(T (tx,ty,_) _ (sx,sy,_)) dt (_,_,z) | outOf2DRange t z = return ()
      | otherwise = do
        strokeWeight sw
        stroke $ colorForDayTime col dt
        line beginAdjusted endAdjusted
        where
            begin = (tx,ty)
            end = (sx,sy)
            ishorizontal = tx /= sx
            xPlus = if ishorizontal then sw/2 else 0
            yPlus = if ishorizontal then 0 else sw/2
            tOverS = if ishorizontal then tx < sx else ty < sy
            beginAdjusted = if tOverS then begin - (xPlus,yPlus) else begin + (xPlus,yPlus)
            endAdjusted = if tOverS then end + (xPlus,yPlus) else end - (xPlus,yPlus)
    
    
drawEllipsoide :: Col -> Transform -> DayTime -> P3 -> Draw
drawEllipsoide col t dt (_,_,z) | outOf3DRange t z = return ()
      | otherwise = do
        strokeFill $ colorForDayTime col dt
        strokeWeight 1
        ellipse (centerOf $ transition t) $ whAt t $ distE t z

drawEllipse :: Col -> Transform -> DayTime -> P3 -> Draw
drawEllipse col t dt (_,_,z) | outOf2DRange t z = return ()
      | otherwise = do
        strokeFill $ colorForDayTime col dt
        strokeWeight 1
        ellipse (centerOf $ transition t) (centerOf $ scaling t)

drawSEllipse :: Col -> Col -> Transform -> DayTime -> P3 -> Draw
drawSEllipse col scol t dt (_,_,z) | outOf2DRange t z = return ()
      | otherwise = do
        stroke $ colorForDayTime scol dt
        fill $ colorForDayTime col dt
        strokeWeight 1
        ellipse (centerOf $ transition t) (centerOf $ scaling t)

drawCylinder :: Col -> Col -> StrokeWeight -> Transform -> DayTime -> P3 -> Draw
drawCylinder col scol sw t dt (_,_,z) 
  | outOf3DRange t z = return ()
  | otherwise = do
    fill $ colorForDayTime col dt
    stroke $ colorForDayTime scol dt
    strokeWeight sw
    ellipse c (sx, sy)
    where
        (sx,sy,sz) = scaling t
        c = centerOf $ transition t

drawHenge :: Transform -> DayTime -> P3 -> Draw
drawHenge t@(T (x,y,z) r (sx,sy,sz)) dt p@(px,py,_) = do
    drawCylinder col scol sw (T north r (radius,radius,sz)) dt p
    drawCylinder col scol sw (T northeast r (radius,radius,sz)) dt p
    --drawCylinder col scol sw (T east r (radius,radius,sz)) dt p
    drawCylinder col scol sw (T southeast r (radius,radius,sz)) dt p
    drawCylinder col scol sw (T south r (radius,radius,sz)) dt p
    drawCylinder col scol sw (T southwest r (radius,radius,sz)) dt p
    drawCylinder col scol sw (T west r (radius,radius,sz)) dt p
    drawCylinder col scol sw (T northwest r (radius,radius,sz)) dt p
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
        radius = sx/24
        inXrange = x + sx/4 >= px && x - sx/4 <= px 
        inYrange = y + sy/4 >= py && y - sy/4 <= py
        (col,scol) = if inXrange && inYrange then (white,darkSlateGray) else (darkSlateGray,darkSlateGray)

-- Path functions

fullRectPath :: Transform -> Z -> Path
fullRectPath = fullRectPathInRange outOf2DRange

fullCubePath :: Transform -> Z -> Path
fullCubePath = fullRectPathInRange outOf3DRange

fullRectPathInRange :: (Transform -> Z -> Bool) -> Transform -> Z -> Path
fullRectPathInRange rangeFunction t z
  | rangeFunction t z = []
  | otherwise = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
    where 
        (sx,sy,sz) = scaling t
        (rx,ry) = (sx/2, sy/2)
        (cx,cy) = centerOf $ transition t
        minX = cx - rx
        maxX = cx + rx
        minY = cy - ry
        maxY = cy + ry

rectPath :: StrokeWeight -> Transform -> Z -> Path
rectPath = rectPathInRange outOf2DRange


cubePath :: StrokeWeight -> Transform -> Z -> Path
cubePath = rectPathInRange outOf3DRange

rectPathInRange :: (Transform -> Z -> Bool) -> StrokeWeight -> Transform -> Z -> Path
rectPathInRange rangeFunction sw t z | rangeFunction t z = []
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


cylinderPath :: Transform -> Z -> Path
cylinderPath t z 
  | outOf3DRange t z = []
  | otherwise = [p | p <- fullCubePath t z, insideEllipse f1 f2 a p]    
    where
        (sx,sy,sz) = scaling t
        (rx,ry) = (sx/2, sy/2)
        (cx,cy) = centerOf $ transition t
        a = if sx > sy then rx else ry
        focalOffset = if sx > sy then sqrt (rx^2 - ry^2) else sqrt (ry^2 - rx^2)
        (f1,f2) = if sx > sy 
                  then ((cx,cy) + (-focalOffset,0), (cx,cy) + (focalOffset,0))
                  else ((cx,cy) + (0,-focalOffset), (cx,cy) + (0,focalOffset))

ellipsePath :: Transform -> Z -> Path
ellipsePath t z 
  | outOf2DRange t z = []
  | otherwise = [p | p <- fullRectPath t z, insideEllipse f1 f2 a p]    
    where
        (wx,wy) = centerOf $ scaling t
        (rx,ry) = (wx/2, wy/2)
        (cx,cy) = centerOf $ transition t
        a = if wx > wy then rx else ry
        focalOffset = if wx > wy then sqrt (rx^2 - ry^2) else sqrt (ry^2 - rx^2)
        (f1,f2) = if wx > wy 
                  then ((cx,cy) + (-focalOffset,0), (cx,cy) + (focalOffset,0))
                  else ((cx,cy) + (0,-focalOffset), (cx,cy) + (0,focalOffset))


ellipsoidePath :: Transform -> Z -> Path
ellipsoidePath t z 
  | outOf3DRange t z = []
  | otherwise = [p | p <- fullCubePath t z, insideEllipse f1 f2 a p]    
    where
        (wx,wy) = whAt t $ distE t z
        (rx,ry) = (wx/2, wy/2)
        (cx,cy) = centerOf $ transition t
        a = if wx > wy then rx else ry
        focalOffset = if wx > wy then sqrt (rx^2 - ry^2) else sqrt (ry^2 - rx^2)
        (f1,f2) = if wx > wy 
                  then ((cx,cy) + (-focalOffset,0), (cx,cy) + (focalOffset,0))
                  else ((cx,cy) + (0,-focalOffset), (cx,cy) + (0,focalOffset))

gLinePath :: StrokeWeight -> Transform -> Z -> Path
gLinePath sw t z | outOf2DRange t z = []
      | otherwise = linePathing sw beg end
        where            
            (bx,by,_) = transition t
            beg = (bx,by)
            (ex,ey,_) = scaling t
            end = (ex,ey)

linePathing :: StrokeWeight -> P2 -> P2 -> Path
linePathing sw beg@(bx,by) end@(ex,ey) = [p | p <- rect, isOnLine sw beg end p]
    where
        rect = [(x,y) | x <- [bsx..esx], y <- [bsy..esy]]
        sr = sw
        bsx = if bx < ex then bx - sr else ex - sr
        esx = if bx < ex then ex + sr else bx + sr
        bsy = if by < ey then by - sr else ey - sr
        esy = if by < ey then ey + sr else by + sr


hengePath :: Transform -> Z -> Path
hengePath = const $ const []



