module Characters.Boss where

import Geometry
import Actor
import Objects
import ObjectUtils
import Constants
import Colors
import DayTime
import Graphics.Proc

boss :: (String,Actor)
boss = ("boss", bossT (T bossStart (0,0,0) bossSize))

-- Ticks
bossTick = idle --Constant Tick



bossT :: Transform -> Actor
bossT t = A (Geo t drawBoss fullCubePath) bossTick  False True False [] True --TODO maybe fullCubePath is not suited that well | TODO fill text


drawBoss :: Transform -> DayTime -> P3 -> Draw
drawBoss t dt (px,py,pz) 
  | outOf3DRange t pz = return ()
  | otherwise = do 
        stroke $ colorForDayTime black dt
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

