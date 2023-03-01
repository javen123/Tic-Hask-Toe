module A3 where

import A1
import A2

import Data.List (transpose)
import Data.Array (Ix(range))
import Control.Monad.RWS (All(getAll))
import System.Posix.Internals (puts)
import Data.Char
import Data.Text (replace, Text)
import qualified Data.Text as T
import qualified Data.Char as T

-- *** Assignment 3-1 ***

-- Q#01

showInts :: [Int] -> [String]
showInts = map show


_HEADER_ =  formatLine (" ": showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares = map showSquare


-- Q#03

formatRows :: [Row] -> [String]
formatRows = map (formatLine . showSquares)

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty row col 
    | null row = False
    | x == Neither = True
    | otherwise = False
    where x = row !! (col - 1)

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol = map tail


dropLastCol :: Board -> Board
dropLastCol = map init

-- Q#06

getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (x:xs) = head x : getDiag1 (dropFirstCol xs)



getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (x:xs) = last x : getDiag2 (dropLastCol xs)


getAllLines :: Board -> [Line]
getAllLines [] = []
getAllLines xs = getDiag1 xs : getDiag2 xs : xs ++ transpose xs

-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p b m = 
    let (x,_:xs) = splitAt (fst m) b 
    in x ++ replaceSquareInRow p (snd m) x : xs

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined

