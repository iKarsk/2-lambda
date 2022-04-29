module Main where

import Graphics.Gloss
import System.Random

import Window
import Visuals
import Game

{- For the sake of clarity and to make the program easier to read, 2^λ is split into four parts.
 - Main.hs contains just the main function that is called.
 - Game.hs contains all of the logic behind the code including:
        - New data types and game states
        - The render function
        - The input handler
 - Window.hs contains all the code relating to the window for the game.
        *You may want to change the resolution*
 - Visuals.hs contains all the Picture types, and relates to the visual elements of the game.
-}



{- Main entry point to program.
 - Passes initial game state and handles game events. -}

main :: IO ()
main = 
    do
        gen <- getStdGen
        play
            windowDisplay
            backgroundColour
            60
            (initGameState gen)
            render
            inputHandler
            (const id) {- The game does not rely on time (each movement is a response to user input), so the value will be constant. -}

