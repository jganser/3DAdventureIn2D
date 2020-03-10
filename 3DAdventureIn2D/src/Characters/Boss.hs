module Characters.Boss where

import Drawable
import Geometry
import Actor as A
import Objects
import ObjectUtils
import Constants
import Colors
import DayTime
import Graphics.Proc

boss :: (String,Actor)
boss = (name, bossT (T bossStart (0,0,0) bossSize))

-- Ticks
bossTick = idle --Constant Tick

name = "boss"

bossT :: Transform -> Actor
bossT t = A (Geo t (drawBoss False) fullCubePath) bossTick  False True False [] True --TODO maybe fullCubePath is not suited that well | TODO fill text

closeMouth :: Actor -> Actor
closeMouth = newDraw (drawBoss True)

vomit :: Actor -> Actor
vomit = newDraw drawBossWithoutMouth 

flee :: Actor -> Actor
flee a = a {actorTick = moveToAndIdle [bossStart - (0,0,12)] 20}

drawBoss :: Bool -> Transform -> DayTime -> P3 -> Draw
drawBoss mouthClosed t dt (px,py,pz) 
  | outOf3DRange t pz = return ()
  | otherwise = do 
        stroke $ colorForDayTime black dt
        strokeWeight 6
        -- corpus
        line leftUpper rightUpper
        line rightUpper rightLower
        line rightLower leftLower
        -- mouth --TODO
        when mouthClosed $ line (fst leftLower, leftFstThirdUp) (fst leftLower, leftSndThirdUp)
        -- round stuff
        strokeWeight 1
        -- eyes
        fill $ colorForDayTime white dt
        circle eyeRadius lowerEyeMid
        circle eyeRadius upperEyeMid
        -- pupils
        strokeFill $ colorForDayTime black dt
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

drawBossWithoutMouth :: Transform -> DayTime -> P3 -> Draw
drawBossWithoutMouth = drawBoss False

