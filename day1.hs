module Main where

main' :: IO ()
main' = print . fst $ foldl (\(counter, prev) curr -> (if curr > prev then counter + 1 else counter, curr)) (0, maxBound) input
  where input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] :: [Int]

main :: IO ()
main = print $ sonar input where input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] :: [Int]

sonar :: [Int] -> Int
sonar [] = 0
sonar (x:xs) = go (0, x) xs
  where
    go :: (Int, Int) -> [Int] -> Int
    go (counter, _) [] = counter
    go (counter, prev) (curr:rest) = go (if curr > prev then counter + 1 else counter, curr) rest
