module Visuals where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Up, Down)
import Prelude hiding (Left, Right)


{- Visuals : Rendering the world : menu, tiles, and game over screen. -}


-- Establishing colours for elements, colourpicked from the game.
-- Stored as a tuple here, first being the colour for the tile, and 
-- second being the colour for the text. 
squareColour :: Int -> (Color, Color)
squareColour x
    | x == 0 = (y, y)
    | x == 2 = (makeColorI 238 228 219 255, makeColorI 132 122 122 255)
    | x == 4 = (makeColorI 236 224 200 255, makeColorI 117 104 98 255)
    | x == 8 = (makeColorI 242 177 123 255, white)
    | x == 16 = (makeColorI 234 142 79 255, white)
    | x == 32 = (makeColorI 245 124 95 255, white)
    | x == 64 = (makeColorI 246 93 59 255, white)
    | x == 128 = (makeColorI 236 207 115 255, white)
    | x == 256 = (makeColorI 237 204 97 255, white)
    | x == 512 = (makeColorI 238 199 78 255, white)
    | x == 1024 = (makeColorI 237 197 63 255, white)
    | x == 2048 = (makeColorI 238 194 45 255, white)
    | otherwise = (makeColorI 62 57 51 255, white)
    where y = makeColorI 216 193 179 255

backgroundColour :: Color
backgroundColour = makeColorI 54 54 54 255


--- Visuals for the menu screen
-- Where clause (with lambda function as it's neater here) used to make creating text easier.
titleScreen :: Picture
titleScreen = pictures [color backgroundColour $ rectangleSolid 900 900, 
                        createText (-100) 80 1.4 1.4 orange "2^λ",
                        createText (25) 200 1 1 orange "\\",
                        createText (-300) 0 0.24 0.24 (light $ light $ light black) "An implementation of 2048 in Haskell.",
                        createText (-410) (-300) 0.6 0.6 orange "Press 'space' to play!"] where
                            createText = \x1 y1 ex ey c t -> translate x1 y1 $ scale ex ey $ color c $ text t

-- Visuals for the game over screen
gameOverScreen :: Int -> Picture
gameOverScreen x = pictures [color (makeColorI 54 54 54 240) $ rectangleSolid 900 900, 
                            createText (-300) (80) 0.8 0.8 orange "Game over!",
                            createText (-180) (0) 0.4 0.4 white $ "Your score: " ++ show x,
                            createText (-400) (-250) 0.45 0.45 orange "Press 'space' to play again!"] where
                                createText = \x1 y1 ex ey c t -> translate x1 y1 $ scale ex ey $ color c $ text t
    

-- Count how many digits are in the number. Used as a multiplier for scale
-- Can do a simple function composition here with eta reduction for clarity.
countDigits :: Int -> Int
countDigits = (length . show)

-- Draws one square on the window.
-- Combination of let.. in and where to make code cleaner, and avoid computing values twice.
drawSquare :: Int -> Picture
drawSquare x = let
    makeSquare = color (fst $ squareColour x) $ rectangleSolid 161 161

    -- As numbers get bigger, size and position needs to be adjusted according to how many digits there are.
    -- Shift the numbers more to the left the longer they are.
    scalar :: Int -> (Picture -> Picture)
    scalar a y
        | a == 0    = scale 0 0 y
        | f < 6     = scale 0.4 0.4 $ translate (-50 - 35*(fromIntegral f - 1)) z $ color textColour y
        | otherwise = scale 0.3 0.3 $ translate (-230) z $ color textColour y
        where
            f = countDigits a
            z = -50
            textColour = snd $ squareColour x

    makeText = scalar x $ text $ show x
    in pictures [makeSquare, makeText]

-- Draw an entire row of squares. Each on the same elevation.
drawRow :: [Int] -> Picture
drawRow [a,b,c,d] = pictures [translate (-270) height $ drawSquare a,
                              translate (-89) height $ drawSquare b,
                              translate (89) height $ drawSquare c,
                              translate (270) height $ drawSquare d] where
                                  height = -320



-- Visuals for the running game - draws the entire grid and the score text.
playingScreen :: [[Int]] -> Int -> Picture
playingScreen [a, b, c, d] s = pictures [translate (0) (-50) $ color (makeColorI 188 174 165 255) $ rectangleSolid 740 740,
                                      drawRow a,
                                      translate (0) (180) $ drawRow b,
                                      translate (0) (360) $ drawRow c,
                                      translate (0) (540) $ drawRow d,
                                      translate (-200) 350 $ scale 0.5 0.5 $ color white $ text $ "Score: " ++ show s]




