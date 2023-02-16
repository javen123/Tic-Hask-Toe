{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import Text.Read (Lexeme(String))
import GHC.IO.Exception (IOErrorType(InvalidArgument))

-- *** Assignment 2-1 *** --

-- Q#01

promptPlayer :: Player -> String
promptPlayer x = concat ["Player ", show x, "'s turn: enter a row and column position"]

-- Q#02

_RANGE_ = [0..(_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']


readDigit :: Char -> Int
readDigit x = if isDigit x then read [x] :: Int else -1

-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ Neither


_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied = foldr ((&&) . notElem Neither) True


_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
    , [O, X, X]
    , [O, X, O]
    ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings = zip ['A'..'Z']

-- Q#07

formatLine xs = _SEP_ ++ intercalate _SEP_ xs ++ _SEP_

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = elem x [0.._SIZE_] && elem y [0.._SIZE_]

-- Q#09

stringToMove :: [Char] -> Move
stringToMove [] = _INVALID_MOVE_
stringToMove xs 
    | length xs > 2 = _INVALID_MOVE_
    | otherwise = (convertRowIndex $ head xs, readDigit $ last xs)


-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player column row = 
    let (x,_:xs) = splitAt column row
    in x ++ player : xs