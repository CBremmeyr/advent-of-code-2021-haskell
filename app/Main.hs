module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc

import System.IO
import Control.Monad

f :: [String] -> [(String, Int)]
f xs = map mapper xs
     where 
        mapper x = (head x_split, read (last x_split) :: Int)
                   where x_split = words x

main = do

    -- Get input from file
    let input = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlelines = lines contents
        input = f singlelines
    -- print input

    -- Day 1.1
    -- let result = countTrues (isBigger input)
    -- print result

    -- Day 1.2
    -- let result = countTrues (isBigger (slidingSum 3 input))
    -- print result

    -- Day 2.1
    -- let forward_sum = sumDirs "forward" input
    -- let down_sum    = sumDirs "down" input
    -- let up_sum      = sumDirs "up" input

    -- print (forward_sum * (down_sum - up_sum))

    -- Day 2.2
    let pos = get_pos input
    print pos
    print ((fst pos) * (snd pos))

    -- Clean up stuff
    hClose handle

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
