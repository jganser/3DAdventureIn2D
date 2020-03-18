module Characters.Elder where

import Actor
import Objects
import Colors
import Graphics.Proc

elder :: (String,Actor)
elder = (name, elderActor)

name = "elder"

elderActor = talkingCharacter elderGeo elderDialog

elderGeo = gSCirc (25,145) 10 0 gold darkSlateGray

elderDialog = [
    "Elder   : Young one. Come here and listen to my voice.",
    "Elder   : Wait, you look troubled, child.",
    "Elder   : Don\'t tell me... it is your beloved, isn\'t it?",
    "Elder   : The one that is sacrificed, I mean.",
    "Hero    : Yeah, it is.",
    "Elder   : It\'s such a pity. In my youth we had a shaman.",
    "Elder   : And I tell you that shaman was of another caliber.",
    "Elder   : Hero material if you understand what I mean.",
    "Elder   : Defeated the monster with his vanishing magic.",
    "Elder   : He saved my wife, you know.",
    "Elder   : I was really lucky, you know. Unlike you.",
    "Elder   : Thanks to him we know the weakness of the monster.",
    "Elder   : He said it was in the second half of that beast.",
    "Elder   : But you can\'t reach there without the power of vanishing.",
    "Elder   : He never called it vanishing though.",
    "Elder   : Something about travelling otherworlds.",
    "Elder   : He also used some fantasy words for it that I don\'t remember.",
    "Elder   : If you want to look into it, check out the stone henge.",
    "Elder   : Ask the new shaman to let you pass.",
    "Elder   : I bet he will allow it to you, thanks to your circumstances."
  ]