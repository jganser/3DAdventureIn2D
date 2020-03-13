module Characters.Townpeople where

import Actor
import Graphics.Proc
import System.Random (randomRIO)
import Constants
import Colors
import Objects

townpeople :: P3 -> Pio Actor
townpeople (x,y,z) = do
    index <- liftIO $ randomRIO (0, length townDialog - 1)
    let text = townDialog !! index
    index <- liftIO $ randomRIO (0, length townColors - 1)
    let color = townColors !! index
    let scol = nextCol index
    return $ A (gSCirc (x,y) playerSize z color scol) idle False True False [text] True

createAllTownPeople :: [P3] -> Pio [Actor]
createAllTownPeople = mapM townpeople

townColors :: [Col]
townColors = [
    gold
  , goldenRod
  , cadetBlue 
  ]

nextCol :: Int -> Col
nextCol i = (!!) townColors $ mod (i+1) $ length townColors

townDialog :: [String]
townDialog = [
    "Townwoman : Leave me alone."
  , "Townman   : The thing with the monster is such a shame..."
  , "Townkid   : Everybody is talking about the monster. I hate it."
  , "Townman   : I\'m glad that it wasn\'t my daughter that was offered."
  , "Townwoman : Why did it come back?"
  , "Townwoman : It hasn\'t even been two generations of peace yet."
  ]