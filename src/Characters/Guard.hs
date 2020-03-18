module Characters.Guard where

import Actor
import Geometry
import Objects
import Colors
import Constants
import Graphics.Proc

guard :: (String,Actor)
guard = (name, guardActor)

name = "guard"

guardActor :: Actor
guardActor = talkingCharacter guardGeometry guardDialog

guardGeometry :: Geometry
guardGeometry = gSCirc (79,294) (playerSize+2) 0 darkSlateGray gray 

moveGuard :: Actor -> Actor
moveGuard a = a {actorTick = moveToAndIdle [(830,295,0)] 80}

guardDialog :: [String]
guardDialog = [
    "Guard     : It is time."
  , "Guard     : ..."
  , "Guard     : I know it's sad, but the time is running out."
  , "Hero      : No, please give us more time..."
  , "Beloved   : Hush, I need to go. You know it my love..."
  , "Beloved   : It\'s for your sake as well."
  , "Beloved   : And I don\'t want somebody else to take my place as the sacrifice."
  ]

guardDialog2 :: [String]
guardDialog2 = [
    "Hero      : I know that the lottery was fair, but still... why us?"
  , "Guard     : I hate to do this. But we really need to go now."
  , "Beloved   : Oh, I love you so much. But still, I need to go! To save the village."
  , "Beloved   : And to save you! Live, for my sake, live for us both."
  , "Hero      : I love you, too. I promise I will never forget you!"
  , "Guard     : Alright, enough you, too. Like I said it's time."  
  ] -- Guard walks behind Female 