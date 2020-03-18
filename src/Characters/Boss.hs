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
bossT t = A (Geo t (drawBoss False) fullCubePath) bossTick  False True False [] False 

closeMouth :: Actor -> Actor
closeMouth a = let a_1 = newDraw (drawBoss True) a in a_1 { blocksPlayer = True}

vomit :: Actor -> Actor
vomit = newDraw drawBossWithoutMouth 

flee :: Actor -> Actor
flee a = a {actorTick = moveToAndIdle [bossStart - (0,0,12)] 20}

drawBoss :: Bool -> Transform -> DayTime -> P3 -> Draw
drawBoss mouthClosed t dt (px,py,pz) 
  | outOf3DRange t pz = return ()
  | otherwise = do 
        stroke $ colorForDayTime darkSlateGray dt
        fill crimson
        rect leftLower rightUpper
        strokeWeight 4
        -- corpus
        line leftUpper rightUpper
        line rightUpper rightLower
        line rightLower leftLower
        -- mouth 
        when mouthClosed $ line (fst leftLower, leftFstSixthdUp) (fst leftLower, left5SixthUp)
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
            sixth = (snd leftUpper - snd leftLower)/6
            leftFstSixthdUp :: Float
            leftFstSixthdUp = sixth + snd leftLower
            left5SixthUp :: Float
            left5SixthUp = 5*sixth + snd leftLower
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

