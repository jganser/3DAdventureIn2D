module Characters.OldShaman 
  (
    oldShaman, name, goToPlatform, goToEdge, lastDialog,
    moveWithPlatform, isInFirstTarget, isInSecondTarget
  ) where

import Actor
import Geometry
import Objects
import Graphics.Proc
import ObjectUtils
import Constants
import DayTime
import Colors
import Drawable


oldShaman :: (String,Actor)
oldShaman = (name , oldShamanActor) 

name = "oldShaman"

oldShamanActor :: Actor
oldShamanActor = talkingCharacter oldShamanGeo firstDialog

oldShamanGeo :: Geometry
oldShamanGeo = gSCirc (79,294) playerSize 10 azure purple

-- Pathing
-- the path the old shaman shall take

goToPlatform :: Actor -> Actor
goToPlatform a = a { actorTick = moveToAndIdle shamansFirstPath 85}

goToEdge :: Actor -> Actor
goToEdge a = a { actorTick = moveToAndIdle shamansSecondPath 75}

moveWithPlatform :: Actor -> Actor
moveWithPlatform a = a {actorTick = moveToAndIdle [(835,300,20)] 40}

shamansFirstPath :: [P3]
shamansFirstPath = [
    (79,294,10)
  , (80,100,10)  -- straight up into the middle
  , (400,100,10) -- right into the circ
  , (400,230,10) -- down
  , (255,230,10) -- left
  , (270,230,10) -- on plaza platform
  , firstPathTarget -- position on platform in the middle realm
  ]

shamansSecondPath :: [P3]
shamansSecondPath = [
    (835,300,20) -- start on the platform when it is in the upper realm
  , (922,300,20) -- going to the edge
  ]

firstPathTarget:: P3
firstPathTarget = (270,300,10)

secondPathTarget:: P3
secondPathTarget = (922,300,20)

isInFirstTarget :: Actor -> Bool
isInFirstTarget = (==) firstPathTarget . aPos

isInSecondTarget :: Actor -> Bool
isInSecondTarget = (==) secondPathTarget . aPos

-- Dialogs

firstDialog :: [String]
firstDialog = [
    " ???      : Ohmmm...          Ohmmm..."
  , "Hero      : What are you doing here?"
  , " ???      : I am meditating young friend."
  , " ???      : Letting go of the flat world to expand the view."
  , " ???      : Seeing reality as it\'s whole is my goal."
  , " ???      : Believe it or not I can grasp the \'low\' and the \'up\' from here."
  , " ???      : And at sometime I..."
  , "Hero      : The \'low\' and the 'up'? I do not understand..."
  , "Hero      : Is this not the otherworld?"
  , " ???      : Oh dear. Isn\'t it obvious that the world isn\'t flat as we are?"
  , "Hero      : Everything is flat. What else could it be?"
  , " ???      : Real.    Hehehe."
  , " ???      : Haven\'t you experienced the changes in the flat view?"
  , " ???      : It wasn\'t the world that changed."
  , " ???      : It was you."
  , " ???      : This flatland here is just of of many that are \'beside\' each other."
  , " ???      : Or in other words since \'beside\' doesn\'t fit, I call it \'up\' and low."
  , " ???      : Using just the term as the cylindric platform told me."
  , " ???      : Believe me when I say it wasn\'t easy to learn it\'s language..."
  , " ???      : But that\'s no the point here."
  , " ???      : The point is to ascend the flat and to become 'spacial'..."
  , " ???      : Now you know what it is I'm doing here, but what are you doing here?"
  , "Hero      : I want to save my beloved from the monster like the old shaman did."
  , " ???      : Ah, just like the old me, huh?"
  , "OldShaman : That\'s easy. Just grasp \'falling\', just as I did back then."
  , "OldShaman : You see falling is the ability to descend through the flatlands."
  , "OldShaman : Do it right above the monster and you will scare it away."
  , "OldShaman : It will spit you out and descend into the unknown 'low'."
  , "OldShaman : Ten to twenty years of meditation here with me and you should see the \'low\'."
  , "OldShaman : The you should be fine.        Ohmmm..."
  , "Hero      : I can\'t wait so long, can\'t you help me."
  , "OldShaman : Stop it, the long I talk with you the flatter I think."
  , "OldShaman : Just meditate over your experiences and my words...   Ohmmm..."
  , "Hero      : You can\'t... I can\'t..."
  , "Hero      : For the sake of my love, I have to get there now..."
  , "Hero      : If you don\'t help me, I\'ll keep talking and flatten your thoughts."
  , "OldShaman : Oh, enough. Allright."
  , "OldShaman : You\'re to attached to your love and the material flatness."
  , "OldShaman : I\'ll help you to get my silence back..."
  , "OldShaman : You see falling is steooing over the bounds..."
  , "OldShaman : And then descending naturally by letting go..."
  , "OldShaman : I guess you won\'t understand, words are just not enough to describe it."
  , "OldShaman : Therefore I am going to kick you. Follow me."
  , "Hero      : Why do you want to hurt me, old shaman?"
  , "OldShaman : Kick you down. Like helping you desend..."
  , "OldShaman : Pushing you over the edge..."
  , "OldShaman : Extending your horizon..."
  , "OldShaman : And even it it hurts, is your love not more important to you?"
  , "Hero      : You\'re right, your disciple follows you."
  , "OldShaman : Disciple... That has a nice ring to it."
  , "OldShaman : If you really want to be my disciple come back here."
  , "OldShaman : Come back here once you freed yourself from the material..."
  , "OldShaman : Now follow me" 
  ]

lastDialog :: [String]
lastDialog = [
    "OldShaman : This is the place."
  , "OldShaman : Although you might not see it, we are \'above\' the monster."
  , "OldShaman : Now come here."
  , "*kick*"
  , "Hero      : Aaaahhh..."
  ]