module Window where

import Graphics.Gloss


-- Control size of window
windowSize :: (Int, Int)
windowSize = (900, 900)

-- Resolution of monitor (Change to (1920,1080) for 1080p)
screenResolution :: (Int, Int)
screenResolution = (2560, 1440)

-- Offset window to center.
offset :: (Int, Int)
offset = ((fst screenResolution - fst windowSize) `div ` 2, (snd screenResolution - snd windowSize) `div` 2)

-- Define display for the game window.
windowDisplay :: Display 
windowDisplay = InWindow "2^λ - 2048 in Haskell" windowSize offset










