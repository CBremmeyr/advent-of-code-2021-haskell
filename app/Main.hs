module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

import System.IO
import Control.Monad
import Data.List
import Data.Char

f :: [Char] -> [Int]
f xs = map (\x -> (digitToInt x)) xs

main = do

    -- Get input from file
    let input = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let lines_list = lines contents
        input = map f lines_list
    -- print input

    -- Day 3.2
    print ((o2_gen input) * (co2_gen input))

    -- Clean up stuff
    hClose handle

-- Day 3 stuff
o2_gen :: [[Int]] -> Int
o2_gen xs = get_ls_rate (>=) xs

co2_gen :: [[Int]] -> Int
co2_gen xs = get_ls_rate (<) xs

get_ls_rate :: (Int -> Int -> Bool) -> [[Int]] -> Int
get_ls_rate comp xs = toDec (map (\x -> if x == 1 then '1' else '0') res)
                      where res = (filter_step 0 (comp) xs) !! 0

filter_step :: Int -> (Int -> Int -> Bool) -> [[Int]] -> [[Int]]
filter_step n f (x:[]) = [x]
filter_step n comp xs = filter_step (n+1) comp (filter keeper_func xs)
                        where
                            keeper_func = \x -> (x !! n) == keeper
                            keeper = if comp (col_1s xs n) (col_0s xs n)
                                     then 1 else 0

col_1s :: [[Int]] -> Int -> Int
col_1s xs n = sum (ys !! n)
              where ys = transpose xs

col_0s :: [[Int]] -> Int -> Int
col_0s xs n = length (ys !! n) - sum (ys !! n)
              where ys = transpose xs

calc_pow :: [[Int]] -> Int
calc_pow xs = (ge_rate (> 0.5) col_avgs) * (ge_rate (< 0.5) col_avgs)
              where col_avgs = col_avg xs

ge_rate :: (Double -> Bool) -> [Double] -> Int
ge_rate comp xs = toDec bin_str
                  where bin_str = map
                            (\x -> if (comp x) then '1' else '0')
                            xs

col_avg :: [[Int]] -> [Double]
col_avg xs = map (\row -> doubleDiv (sum row) (length row)) ys
         where 
            ys = transpose xs
            doubleDiv a b = fromIntegral a / fromIntegral b
            

-- Day 2 stuff
sumDirs :: String -> [(String, Int)] -> Int
sumDirs dir xs = sum (map snd dir_matches)
                 where dir_matches = (filter ((== dir).fst) xs)

get_pos :: [(String, Int)] -> (Int, Int)    -- (h_pos, depth, aim)
get_pos xs = (fst3 final_pos, snd3 final_pos)
             where final_pos = foldl pos_step (0, 0, 0) xs

--          Move to apply    Current position   New position
pos_step :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
pos_step (h_pos, depth, aim) (dir, delta)
    | dir == "forward" = (h_pos + delta,            -- new h_pos
                          depth + (aim * delta),    -- new depth
                          aim)                      -- aim no change

    | dir == "down" = (h_pos, depth, aim + delta)   -- increase aim
    | dir == "up"   = (h_pos, depth, aim - delta)   -- decrease aim

-- Day 1 stuff
isBigger :: [Int] -> [Bool]
isBigger [] = []
isBigger (x:[]) = []
isBigger (x:xs) = (x < (head xs)) : (isBigger xs)

countTrues :: [Bool] -> Int
countTrues xs = sum (map (\x -> if x then 1 else 0) xs)

slidingSum :: Int -> [Int] -> [Int]
slidingSum w xs = if length window == w then
                    (sum window):(slidingSum w (tail xs))
                    else []
                where window = (take w xs)

-- Helper functions
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
