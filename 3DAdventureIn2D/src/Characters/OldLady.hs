module Characters.OldLady where

import Actor
import Objects
import Graphics.Proc
import Colors

oldLady :: (String,Actor)
oldLady = (name, oldLadyActor) -- todo

name = "oldLady"

oldLadyActor = talkingCharacter oldLadyGeo oldLadyDialog1

oldLadyGeo = gSCirc (875,500) 8 0 seaGreen azure

oldLadyDialog1 = [
    "Oldlady   : Hello dear! Welcome to my flower shop."
  , "Oldlady   : How may I help you, young fellow?"
  , "Hero      : Well you see..."
  , "Oldlady   : I am not getting younger! Young one."
  , "Oldlady   : Ask me a concrete question."    
  ]

oldLadyDialog2 = [
    "Hero      : Hello, old lady.",
    "Hero      : I am looking for cues about the vanishing.",
    "Hero      : The henge, where the rituals take place...",
    "Oldlady   : Keep talking, I haven\'t heard your question yet.",
    "Hero      : It seems like there were flowers sacrificed in the old times.",
    "Hero      : However, I never saw this practice in our times.",
    "Hero      : Do you know which flowers were used back in the old times?",
    "Hero      : I know I am just grasping straws here.",
    "Hero      : I just don\'t know what to do right now...",
    "Oldlady   : Hush, little one.",
    "Oldlady   : It\'s your beloved which was sacrificed, isn\'t it?",
    "Oldlady   : How insensitive of me, letting my bad mood out on you.",
    "Oldlady   : Ok, I will tell you what I know.",
    "Oldlady   : About the old days how you called them, my youth.",
    "Oldlady   : At them time we had this dazzling, handsome shaman.",
    "Oldlady   : But even though he was handsome he was never proud.",
    "Oldlady   : His father was a farmer you see, quite the modest background.",
    "Oldlady   : Where were I? What was I wanted to tell you?",
    "Hero      : About the flowers...",
    "Oldlady   : Ah, yes, the flowers. You see it was a hobby of his.",
    "Oldlady   : Thats why I started this shop. I wanted to impress him.",
    "Oldlady   : Hehe, that brings back joyful memories...",
    "Oldlady   : However, since it was his hobby and his connection to his past,",
    "Oldlady   : he was just sacrificing the most beautiful flowers of the year.",
    "Oldlady   : Nothing much to it, you see.",
    "Oldlady   : Thats why the new shaman stopped doing it.",
    "Hero      : So there was nothing special about the flowers?",
    "Hero      : It is my only clue, please! I don\'t want to loose my beloved.",
    "Oldlady   : Well, there was something.",
    "Oldlady   : In the year of the monster, it was a special kind of flower.",
    "Oldlady   : The shaman was its inventor.",
    "Oldlady   : I have a few here, after he vanished I got it from his garden.",
    "Oldlady   : Here take one that is in bloom with you.",
    "Oldlady   : May it ease your pain.",
    "Hero      : Thank you flower lady!",
    "Oldlady   : Not old lady anymore? Get going before you make me blush!"
  ]