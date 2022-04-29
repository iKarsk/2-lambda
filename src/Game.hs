module Game where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.Pure.Game hiding (Up, Down, shift) -- Prevent interference with my own constructors and functions.
import qualified Graphics.Gloss.Interface.Pure.Game as GLOSS (KeyState (..)) -- Prevent interference with my own Down constructor.
import Prelude hiding (Left, Right)
import Data.List
import Data.Char (toLower)

import Visuals


{- Defining key data types. Logic works
    through updates to the game state
    after each keypress by user. -}

-- Define the world data type to represent game state.
-- Using record syntax makes it much easier to read, and more presentable.
data World = World{
    grid            :: [[Int]],
    score           :: Int,
    stage           :: Stage,
    randomGenerator :: StdGen
}

-- Define Eq instance for checking when game is finished.
instance Eq World where
    a == b = grid a == grid b

-- Defining my own type here makes the code significantly easier to read.
data Direction = Up | Down | Left | Right deriving(Eq, Show)

-- Type for establishing game state.
data Stage = Menu | Running | GameOver deriving (Eq, Show)


-- Initial game state: start on the menu, blank grid.
-- The generator needs to be set at runtime in main inside IO, so we pass it as an input here.
initGameState :: StdGen -> World 
initGameState gen = World{
    grid            = (replicate 4 . replicate 4) 0,
    score           = 0,
    stage           = Menu,
    randomGenerator = gen -- Need to escape IO
}



{- World rendering function -}

-- Render approrpiate world depending on stage.
-- Case statements are used in conjunction with guards to check for multiple (nested) predicates
-- and for neat code.
-- Using an as-pattern also allows me to give a name to the world parameter, whilst also
-- allowing me to get the needed values from the record.
render :: World -> Picture
render world@World{stage = x, grid=g, score = s} = 
    case x of
        Menu                     -> pictures [titleScreen]

        GameOver                 -> endScreen 

        Running 
            | checkGameEnd       -> endScreen
            | otherwise          -> pictures [playingScreen g s]

        where
            endScreen = pictures ([playingScreen g s] ++ [gameOverScreen s]) -- Want a transparent overlay for the end, join the grid and the end screen.
            checkGameEnd = move world Up == world && move world Left == world && move world Down == world && move world Right == world --Check whether game is over, might be no more moves.




{- inputHandler handles the input events for the game.
   Game logic performed by other logic functions.  -}

-- Here I used a combination of top level pattern matching, along with guards and case statements
-- to keep the code neat and to check for multiple, nested predicates (case Running -> detecting keypress)
-- Again, as-pattern used as syntactic sugar for pattern matching.
inputHandler :: Event -> World -> World

-- Handle Spacebar - Starts / ends game.
inputHandler (EventKey (SpecialKey KeySpace) GLOSS.Down _ _) world@World{stage = x}
    | x == Menu     = (addRandom . addRandom) world{stage = Running} -- Function composition is neater here.
    | otherwise     = (addRandom . addRandom) world { stage = Running, grid = (replicate 4 . replicate 4) 0, score = 0}

-- Handle directional movement using WASD.
inputHandler (EventKey (Char c) GLOSS.Down _ _) world@World{stage = x} = 
    case x of
        -- Debugging to find out CapsLock was on... Let's sanitise it!
        Running
            | toLower c == 'w' -> move world Up
            | toLower c == 'a' -> move world Left
            | toLower c == 's' -> move world Down
            | toLower c == 'd' -> move world Right
        _                      -> world

-- Any other input ignored
inputHandler _ world = world


-- Shifts elements to the right, combining the first instances of identical blocks
-- Stores the score as snd element in tuple.
-- Remove all the 0's in the row, as other elements will be shifted into them, using a filter. Reverse it so we can access rightmost elements.
-- Then, use explicit recursion to go through the list, combining any adjacent identical squares.
-- let.. in helps keep the code clean
shift :: [Int] -> ([Int], Int)
shift xs = let
    reduced = reverse $ filter (/= 0) xs

    merge (y1:(y2:ys))
        | y1 == y2  = y1 + y2 : merge ys
        | otherwise = y1 : merge (y2:ys)
    merge a = a

    merged = merge reduced

    -- Add any necessary 0's back into the row
    combined =  reverse (merged ++ (replicate (length xs - length merged) 0))

    -- We can find the score gained by finding the set difference (\\) which will contain the new score and folding over it.
    score = foldr (+) 0 (xs \\ combined)
    in
        (combined, score)


-- Move depending on the direction of the input WASD.
-- By rotating the board, we can use our initial merge function for all directions.
-- Again, using a combination of guards and case statements lets us deal with multiple,
-- nested predicates, and keeps the code clean.
move :: World -> Direction -> World
move world@World{grid = g, score = s} d = 
    case d of
        Up 
            | world == newWorld     -> world -- Check to see if there is a change. If there is no change, then we don't add a new random square.
            | otherwise             -> addRandom newWorld
                where
                    combined = map shift $ transpose g
                    newWorld = world{grid = transpose $ map fst $ combined, score = s + (sum $ map snd $ combined)}
        
        Left 
            | world == newWorld     -> world 
            | otherwise             -> addRandom newWorld
            where
                combined = map shift $ map reverse g
                newWorld = world{grid = map reverse $ map fst $ combined, score = s + (sum $ map snd $ combined)}

        Down 
            | world == newWorld     -> world
            | otherwise             -> addRandom newWorld
            where
                combined = map shift $ (transpose.reverse) g
                newWorld = world{grid = (reverse.transpose) $ map fst $ combined, score = s + (sum $ map snd $ combined)}

        Right 
            | world == newWorld     -> world
            | otherwise             -> addRandom newWorld
            where
                combined = map shift g
                newWorld = world{grid = map fst $ combined, score = s + (sum $ map snd $ combined)}



{- Square respawns:
   After every (successful) move, a new square must be added to the grid. -}

-- Find the coordinates of all the zeros in a given grid
-- Using a double list comprehension is the perfect choice for 'indexing' over each element.
-- Zipping our list with a (lazy) list of integers allows us to increment the row steadily,
-- where the second generator will increment over each column (value) in the selected row,
-- adding it to the final list if it matches the predicate.

zeroList :: [[Int]] -> [(Int, Int)]
zeroList xs = [(x,y) | (x,row) <- zip [0 ..] xs, (y, column) <- zip [0 ..] row, column == 0]

{-
This is obviously stupid .. we can have repeat squares / rows!!
zeroList :: [[Int]] -> [(Int, Int)]
zeroList xs = [(fromJust $ elemIndex r xs, fromJust $ elemIndex 0 r) | r <- xs, 0 `elem` r , c <- r, c == 0]
-}

-- Replace a value at given coordinates with a new value.
-- Split the list twice according to the row and column,
-- and "replace" the head of the last split with the new value.
replace :: [[Int]] -> (Int,Int) -> Int -> [[Int]]
replace grid (r,c) x = let

    (as,b:bs)  = splitAt r grid
    (gs, h:hs) = splitAt c b

    in
        as ++ [gs ++ x:hs] ++ bs


-- Adds a new '2' square in a random empty space in the grid, updating the new seed for the next random.
-- Since we only have two options, then if.. else are a good choice here.
-- let.. in also keeps the code tidy.
addRandom :: World -> World
addRandom world@World{grid = g, randomGenerator = gen} = 
    if length (zeroList g) == 0 then world
    else
        let
            allZeros = zeroList g
            (randomNumber, newSeed) = randomR (0, length allZeros - 1) gen

            -- Replace the randomly picked co-ordinate with '2'.
            newGrid = replace g (allZeros !! randomNumber) 2

        in world{grid = newGrid, randomGenerator = newSeed}



