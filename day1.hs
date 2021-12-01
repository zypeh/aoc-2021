module Main where

main :: IO ()
main = print . fst $ foldl (\(counter, prev) curr -> (if curr > prev then counter + 1 else counter, curr)) (0, maxBound) input
  where input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] :: [Int]
