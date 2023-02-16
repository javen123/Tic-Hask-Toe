module A1 where

import Data.Char (toUpper)

-- *** Assignment 1-1 *** --

-- Q#01

_SIZE_ = 3 :: Int

-- Q#02

_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex x = fromEnum (toUpper x) - 65

-- Q#04

_INVALID_MOVE_ = (-1 :: Int,-1 :: Int)

-- Q#05

_SEP_ = ['_','|','_']

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | Neither deriving (Show, Eq)


-- Q#07
data GameState = XWon | OWon | Tie | InProgress deriving (Show, Eq)


-- Q#08

type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)





-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer b = if b then X else O

getFirstPlayer_ b
    | b = X
    | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState gs  = case gs of XWon -> "X won the game"
                               OWon -> "O won the game"
                               Tie -> "Game ends in a tie"
                               InProgress -> "Game still in progress"
   

-- Q#11

switchPlayer :: Player -> Player
switchPlayer p 
    | p == X = O
    | p == O = X
    | otherwise = Neither


-- Q#12

showSquare :: Square -> String
showSquare p = case p of X -> "X"
                         O -> "O"
                         Neither -> "_"
                         