module Main where

import Data.Char

parse :: String -> [Int]
parse = map digitToInt

unpairs :: [(Int, Int)] -> [Int]
unpairs pairs  = [a | (a, b) <- pairs, a == b]

solve :: [(Int, Int)] -> Int
solve = sum . unpairs

part1 :: [Int] -> Int
part1 x = solve pairs
      where pairs = zip x (drop 1 $ cycle x)

part2 :: [Int] -> Int
part2 x = solve pairs
      where pairs = zip x (drop (length x `div` 2) $ cycle x)

main :: IO ()
main = do
     input <- init <$> readFile "/home/avi/AoC2017/day1/input1"
     let parsed = parse input
     print $ part1 parsed
     print $ part2 parsed
