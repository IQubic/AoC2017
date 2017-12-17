module Main where

parse :: String -> [[Int]]
parse = map (map read . words) . lines

part1 :: [[Int]] -> Int
part1 = sum . (map lineDiff)
      where lineDiff line = maximum line - minimum line

part2 :: [[Int]] -> Int
part2 = sum . (map lineVal)
      where lineVal line = head [a `div` b | a <- line,  b <- line, a /= b, a `mod` b == 0]

main :: IO ()
main = do
     input <- init <$> readFile "/home/avi/AoC2017/day2/input2"
     let parsed = parse input
     print $ part1 parsed
     print $ part2 parsed
