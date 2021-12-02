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
    print input

    -- Day 1.1
    let result = fun (isBigger input)
    print result

    -- Day 1.2
    let result = fun (isBigger (slidingSum input))
    print result

    -- Clean up stuff
    hClose handle

isBigger :: [Int] -> [Bool]
isBigger [] = []
isBigger (x:[]) = []
isBigger (x:xs) = (x < (head xs)) : (isBigger xs)

fun :: [Bool] -> Int
fun xs = sum (map (\x -> if x then 1 else 0) xs)

slidingSum :: [Int] -> [Int]
slidingSum xs = if length (take 3 xs) == 3 then
                    (sum (take 3 xs)):(slidingSum (tail xs))
                    else []
