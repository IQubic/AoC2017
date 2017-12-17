module Main where

import Data.List

parse :: String -> [[String]]
parse = map words . lines

part1 :: [[String]] -> Int
part1 input = length [line | line <- input, nub line == line]

part2 :: [[String]] -> Int
part2 input = length [line | line <- input, nub (wordPerms line) == (wordPerms line)]
      	    where wordPerms = concatMap permutations

main :: IO ()
main = do
     input <- init <$> readFile "/home/avi/AoC2017/day4/input4"
     let parsed = parse input
     print $ part1 parsed
     print $ part2 parsed
