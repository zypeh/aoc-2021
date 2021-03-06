{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Z.IO.FileSystem.Base (readFile)
import Z.IO (getArgs, printStdLn)

import qualified Z.Data.Parser as P
import Control.Applicative (many)
import Data.Either (fromRight)

main :: IO ()
main = do
  args <- getArgs
  src <- readFile (head $ tail args)
  let input = fromRight [] $ P.parse' (many $ P.int <* P.char8 '\n') src
  putStrLn $ "part1: " <> show (sonar input)
  putStrLn $ "part2: " <> show (sonar' input)

sonar :: [Int] -> Int
sonar [] = 0
sonar (x:xs) = go (0, x) xs
  where
    go :: (Int, Int) -> [Int] -> Int
    go (counter, _) [] = counter
    go (counter, prev) (curr:rest) = go (if curr > prev then counter + 1 else counter, curr) rest

sonar' :: [Int] -> Int
sonar' input = sonar $ (\(a, b, c) -> a + b + c) <$> zip3 input input' input''
  where
    input' = tail input
    input'' = tail input'
