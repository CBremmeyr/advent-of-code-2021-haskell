module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

import System.IO
import Control.Monad

f :: [String] -> [Int]
f = map read

main = do

    -- Get input from file
    let input = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        input = f singlewords
    -- print input

    -- Day 1.1
    let result = countTrues (isBigger input)
    print result

    -- Day 1.2
    let result = countTrues (isBigger (slidingSum 3 input))
    print result

    -- Clean up stuff
    hClose handle

isBigger :: [Int] -> [Bool]
isBigger [] = []
isBigger (x:[]) = []
isBigger (x:xs) = (x < (head xs)) : (isBigger xs)

countTrues :: [Bool] -> Int
countTrues xs = sum (map (\x -> if x then 1 else 0) xs)

slidingSum :: Int -> [Int] -> [Int]
slidingSum w xs = if length window == w then
                    (sum (take w xs)):(slidingSum w (tail xs))
                    else []
                where window = (take w xs)
