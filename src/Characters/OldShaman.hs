module Characters.OldShaman 
  (
    oldShaman, name, goToPlatform, goToEdge, lastDialog,
    moveWithPlatform, isInFirstTarget, isInSecondTarget,
    isOnTopLevel
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

name :: String
name = "old shaman"

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
  , (240,230,10) -- left ono plaza platform
  , (240,300,10) -- down from plaza platform
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

isOnTopLevel :: Actor -> Bool
isOnTopLevel = (==) 20 . trd . aPos

isInFirstTarget :: Actor -> Bool
isInFirstTarget = (==) firstPathTarget . aPos

isInSecondTarget :: Actor -> Bool
isInSecondTarget = (==) secondPathTarget . aPos

-- Dialogs

firstDialog :: [String]
firstDialog = [
    " ???       : Ohmmm...          Ohmmm..."
  , "Hero       : What are you doing here?"
  , " ???       : I am meditating young friend."
  , " ???       : Letting go of the flat world to expand my view."
  , " ???       : Seeing reality as it\'s whole is my goal."
  , " ???       : ..."
  , " ???       : Believe it or not I can grasp the \'low\'..."
  , " ???       : and the \'up\' from here."
  , " ???       : And at sometime I..."
  , "Hero       : The \'low\' and the 'up'? I do not understand..."
  , "Hero       : Is this not the otherworld?"
  , " ???       : Oh dear. Isn\'t it obvious!"
  , " ???       : Our world isn\'t flat as we are?"
  , "Hero       : Everything is flat. What else could it be?"
  , " ???       : Real.    Hehehe."
  , " ???       : Haven\'t you experienced the changes in the flat view?"
  , " ???       : It wasn\'t the world that changed."
  , " ???       : It was you."
  , " ???       : This flatland here is just one of of many."
  , " ???       : And they are all \'beside\' each other."
  , " ???       : Or in other words:"
  , " ???       : Since \'beside\' doesn\'t fit."
  , " ???       : Therefore, I call it \'up\' and \'low\'."
  , " ???       : Using just the term as the cylindric platform told me."
  , " ???       : Believe me when I say that it wasn\'t easy to learn it\'s language."
  , " ???       : They speak in \'up\'s and \'low\'s as well, you understand?"
  , " ???       : I had to bribe it with flowers to talk to me."
  , " ???       : And then it took me ages to understand the words spoken \'lower\' and \'higher\'."
  , " ???       : You look confused."
  , " ???       : A cylinder is a circle with a constant size. Within it's bounds."
  , " ???       : Constant along the \'up\' axis."
  , " ???       : But that\'s no the point here."
  , " ???       : The point is to ascend the flat and to become \'spacial\'!"
  , " ???       : ..."
  , " ???       : Now you know what it is that I am doing here."
  , " ???       : But what are you doing here?"
  , "Hero       : I want to save my beloved from the monster."
  , "Hero       : Just like the old shaman did."
  , " ???       : Ah, just like the old me, huh?"
  , "old shaman : That\'s easy. Just grasp \'falling\'."
  , "old shaman : It is what I did back then."
  , "old shaman : You see falling is the ability to descend!"
  , "old shaman : To descend through the flatlands."
  , "old shaman : Do it right above the monster."
  , "old shaman : That will scare it away."
  , "old shaman : It will spit you out."
  , "old shaman : And then descend into the unknown \'low\'."
  , "old shaman : Two to three years of meditation here with me..."
  , "old shaman : And you should see the \'low\'."
  , "old shaman : Then you should be fine. And able to fall at the right place."
  , "old shaman : Ohmmm..."
  , "Hero       : I can\'t wait so long, can\'t you help me."
  , "old shaman : Stop it, the longer I talk with you the flatter I think."
  , "old shaman : Just meditate over your experiences and my words..."
  , "old shaman : Who knows, you might grasp it within a month if your smart."
  , "old shaman : Ohmmm..."
  , "Hero       : You can\'t... I can\'t..."
  , "Hero       : For the sake of my love, I have to get there now..."
  , "Hero       : If you don\'t help me, I\'ll keep talking!"
  , "Hero       : And keep flattining your thoughts."
  , "old shaman : Oh, enough. Allright."
  , "old shaman : You\'re to attached to your love."
  , "old shaman : And to the material flatness, aswell."
  , "old shaman : I\'ll help you, to get my silence back..."
  , "old shaman : You see falling is stepping over the bounds..."
  , "old shaman : And then descending naturally by letting go..."
  , "old shaman : I guess you won\'t understand..."
  , "old shaman : Words are just not enough to describe it."
  , "old shaman : Therefore I am going to kick you. Follow me."
  , "Hero       : Why do you want to hurt me, old shaman?"
  , "old shaman : Kick you down. Like helping you desend..."
  , "old shaman : Pushing you over the edge..."
  , "old shaman : Extending your horizon..."
  , "old shaman : And even if it hurts, is your love that shallow?"
  , "Hero       : You\'re right, your disciple follows you."
  , "old shaman : Disciple... That has a nice ring to it."
  , "old shaman : If you really want to be my disciple come back here."
  , "old shaman : Come back once you freed yourself from the material..."
  , "old shaman : Now follow me!" 
  ]

lastDialog :: [String]
lastDialog = [
    "old shaman : This is the place."
  , "old shaman : Although you might not see it, we are \'above\' the monster."
  , "old shaman : Now come here."
  , "*kick*"
  , "Hero       : Aaaahhh..."
  ]