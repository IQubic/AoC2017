module Main where

import Data.Char

parse :: String -> [Int]
parse = map digitToInt

unpairs :: [(Int, Int)] -> [Int]
unpairs pairs  = [a | (a, b) <- pairs, a == b]

part1 :: [Int] -> Int
part1 x = sum $ unpairs pairs
      where pairs = zip x (drop 1 $ cycle x)

main :: IO ()
main = do
     input <- readFile "input1"
     let parsed = parse input
     print $ part1 parsed
