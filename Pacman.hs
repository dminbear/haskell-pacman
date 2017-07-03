-- template for this program taken from https://github.com/beckyconning/haskell-snake
-- Super Flying Raccoon Group 
-- Sophia Chen, Alice Zhu, Dayoung (Dorothy) Min


import Data.List

import System.IO
import System.Timeout
import System.Random
import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Loops
import Control.Applicative


-- Global variables defining size, and positions of character 
type Vector = (Int, Int)
size = 15 
mazePositions = [(2,1), (3,1), (4,1), (11,1), (12,1), (13,1), (2,2), (13,2), (6,3), (8,3), (6,4), (7,4), (8,4), (3,6), (4,6), (7,6), (10,6), (11,6), (4,7), (6,7), (7,7), (8,7), (10,7), (3,8), (4,8), (7,8), (10,8), (11,8), (6,10), (7,10), (8,10), (6,11), (8,11), (2,12), (13,12), (2,13), (3,13), (4,13), (11,13),(12,13), (13,13)]
ghostPositions = [(8, -1)]
pacmanPosition = (4, 0)
totalLives = 2
specialFruitsPositions = [(0,3),(5,4)]

-- Defining the state of the game 
data State = State {
    board :: Int,
    pacman :: Vector,
    ghosts :: [Vector],
    fruits :: [Vector],
	specialFruits :: [Vector],
    move  :: Maybe Vector,
    points :: Int,
    lives :: Int,
    totalFruits :: Int,
    mazeWall :: [Vector] 
} deriving Show

-- Update the Game 
main :: IO State
main = clearScreen
    >> initialState 
    >>= (iterateUntilM gameOver step)
               
oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

-- Game layout 
initialState :: IO State
initialState = getStdGen 
    >>= \stdGen -> return State {
        board = size,
        pacman = pacmanPosition,
        ghosts = ghostPositions,
        fruits = getNotOccupiedBoardPositions ([pacmanPosition]++mazePositions++ghostPositions++specialFruitsPositions) size,
        specialFruits = specialFruitsPositions,
		move  = Just (1, 0),
        points = -1,
        lives = totalLives,
        totalFruits = length (getNotOccupiedBoardPositions ([pacmanPosition]++mazePositions++ghostPositions++specialFruitsPositions) size)-1,   
        mazeWall = mazePositions
    }

-- Find free positions which are not currently occupied 
getNotOccupiedBoardPositions :: [Vector] -> Int -> [Vector]
getNotOccupiedBoardPositions occupiedPositions boardSize = [(x, y) | y <- [0..(boardSize-1)], x <- [0..(boardSize-1)], not ((x, y) `elem` occupiedPositions)]

-- Random Elements
randomElem :: [a] -> StdGen -> Maybe (a, StdGen)
randomElem [] _  = Nothing
randomElem xs inputStdGen  = Just (element, stdGen)
    where indexStdGenTuple = randomR (0, length xs - 1) inputStdGen
          index            = fst indexStdGenTuple
          stdGen           = snd indexStdGenTuple
          element          = xs !! index

step :: State -> IO State
step state = sample sampleLength getInput 
    >>= \ inputMove ->
        displayState $ updateState state (vectorFromChar inputMove)

-- Ouput the features to be displayed in the game 
displayState :: State -> IO State
displayState state = setCursorPosition 0 0 
    >> putStrLn (getPoints (points state))
    >> putStrLn (getLives (lives state))
    >> putStr (render state) 
    >> return state

-- Keys to move pacman 
vectorFromChar :: Maybe Char -> Maybe Vector
vectorFromChar (Just 'w') = Just ( 0,  1)
vectorFromChar (Just 'a') = Just (-1,  0)
vectorFromChar (Just 's') = Just ( 0, -1)
vectorFromChar (Just 'd') = Just ( 1,  0)
vectorFromChar _          = Nothing

getInput :: IO Char
getInput = hSetEcho stdin False 
    >> hSetBuffering stdin NoBuffering
    >> getChar

-- Cases when the game is over
gameOver :: State -> Bool
gameOver (State {
    board = boardSize,
    pacman = pacmanCurrent,
    ghosts = ghostsCurrent,
    points = currentPoints,
    lives = currentLives, 
    totalFruits = totalPoints,
	specialFruits = currentSpecialFruit,
    mazeWall = mazeWalls
})

    | pacmanCurrent `elem` ghostsCurrent && currentLives ==0 = True
    | currentLives == 0 = True
    | currentPoints == totalPoints = True
    | otherwise                                 = False

-- Render the states
render :: State -> String
render state
    = unlines $ applyBorder (board state)
              $ map (renderRow state)
              $ buildBoard (board state)

-- Get relevant information for the game 
getPoints :: Int -> String
getPoints n = "Points:" ++ show n       

getLives :: Int -> String 
getLives n = "Lives:"  ++ show n

-- Render the Game               
applyBorder :: Int -> [String] -> [String]
applyBorder size renderedRows
    = border ++ map (\row -> "|" ++ row ++ "|") renderedRows ++ border
        where border = [replicate (size + 2) '|']

renderRow :: State -> [Vector] -> String
renderRow state = map (characterForPosition state)

characterForPosition :: State -> Vector -> Char
characterForPosition state position
    | position == pacman state                  = 'O'
    | position `elem` ghosts state              = '&'
    | position `elem` fruits state              = '.'
	| position `elem` specialFruits state       = '+' 
    | position `elem` mazeWall state            = 'N' 
    | otherwise                                 = ' '

	
-- Check Collisions 
pacmanHitGhost :: State -> Bool 
pacmanHitGhost state 
      = (pacman state) `elem` ghosts state

pacmanHasFruitInMouth :: State -> Bool
pacmanHasFruitInMouth state
      = (pacman state) `elem` fruits state

pacmanHasSpecialFruitInMouth :: State -> Bool
pacmanHasSpecialFruitInMouth state
	  = (pacman state) `elem` specialFruits state

-- Define the board 
buildBoard :: Int -> [[(Int, Int)]]
buildBoard size
    = [[(x, y) | x <- [0 .. size - 1]] | y <- reverse [0 .. size - 1]]

-- Update states, movement and pacman 
updateState :: State -> Maybe Vector -> State
updateState state inputMove
    = updateLives $ updateSpecialFruit $ updateFruit $ updateGhosts $ updatePacman $ updateMove state inputMove

updateMove :: State -> Maybe Vector -> State
updateMove state@(State { move = Just vector }) inputMove@(Just inputVector)= state { move = inputMove <|> move state }
updateMove state _ = state

updatePacman :: State -> State
updatePacman state@(State { move = (Just vector) })
    | hitWall (pacman state) vector (board state) (mazeWall state) = state 
    | otherwise = state { pacman = (pacman state) `vectorAdd` vector }
updatePacman state = state

-- Define movement of Ghosts
updateGhosts :: State -> State
updateGhosts state@(State {ghosts = currentGhosts}) = state { ghosts = updateGhostAction currentGhosts (pacman state) (board state) (mazeWall state) }

updateGhostAction :: [Vector] -> Vector -> Int -> [Vector] -> [Vector]
updateGhostAction currentGhostPositions pacmanPosition boardSize mazeWall = [vectorAdd s t | (s, t) <- zip currentGhostPositions newActions]
	where newActions = [trackPacman x y boardSize mazeWall| (x, y) <- getNewActions currentGhostPositions pacmanPosition]

getNewActions currentGhostPositions pacmanPosition = [(x,y) | x <- currentGhostPositions, y <- [pacmanPosition]]	
	
trackPacman :: Vector -> Vector -> Int -> [Vector] -> Vector
trackPacman (ghostPositionX, ghostPositionY) (pacmanPositionX, pacmanPositionY) boardSize mazeWall
    | (abs(ghostPositionY - pacmanPositionY) == 1 && (ghostPositionX - pacmanPositionX) == 0) || (abs(ghostPositionX - pacmanPositionX) == 1 && (ghostPositionY - pacmanPositionY) == 0) = vectorSubtract (pacmanPositionX, pacmanPositionY) (ghostPositionX, ghostPositionY)
	| ghostPositionY <= pacmanPositionY && not (hitWall (ghostPositionX, ghostPositionY) (0,  1) boardSize mazeWall)= ( 0,  1)
	| ghostPositionY >= pacmanPositionY && not (hitWall (ghostPositionX, ghostPositionY) (0,  -1) boardSize mazeWall)=  (0,  -1)
    | ghostPositionX <= pacmanPositionX && not (hitWall (ghostPositionX, ghostPositionY) (1,  0) boardSize mazeWall)= (1,  0)
	| otherwise = ( -1, 0)

-- Actions after hitting the wall 
hitWall :: Vector -> Vector -> Int -> [Vector] -> Bool
hitWall (pacmanHeadX, pacmanHeadY) (vectorX, vectorY) boardSize mazeWall
    | (pacmanHeadX+vectorX) >= boardSize || (pacmanHeadX+vectorX) < 0 = True
    | (pacmanHeadY+vectorY) >= boardSize || (pacmanHeadY+vectorY) < 0 = True
    | (pacmanHeadX+vectorX, pacmanHeadY+vectorY) `elem` mazeWall = True
    | otherwise = False

-- Update for points or lives 
updateFruit :: State -> State
updateFruit state
    | pacmanHasFruitInMouth state = state { fruits = [x | x <- fruits state, x /= (pacman state)],
                                            points = (points state)+1}                          
    | otherwise                  = state

updateSpecialFruit :: State -> State
updateSpecialFruit state
	| pacmanHasSpecialFruitInMouth state = state { specialFruits = [x | x <-  specialFruits state, x /= (pacman state)],
											lives = (lives state)+1}
	| otherwise                  = state
	
updateLives :: State -> State
updateLives state 
    | pacmanHitGhost state = state {lives = (lives state)-1}                                        
    | otherwise                  = state

-- Game calculations and updates 
vectorAdd :: Vector -> Vector -> Vector
vectorAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vectorSubtract :: Vector -> Vector -> Vector
vectorSubtract (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

sample :: Int -> IO a -> IO (Maybe a)
sample n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise =
        concurrently (timeout n f) (threadDelay n) 
            >>= \ (result, _) -> return result