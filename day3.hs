{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Z.IO.FileSystem.Base (readFile)
import Z.IO (getArgs, printStdLn)

import qualified Z.Data.Parser as P
import Control.Applicative (many)
import Data.Either (fromRight)
import Z.Data.Vector.Base (Bytes, countBytes)
import qualified Z.Data.ASCII as ASCII

import qualified Z.Data.Vector.Base as Vector
import qualified Z.Data.Vector as Vector
import Data.Bits (Bits(setBit))

main :: IO ()
main = do
  args <- getArgs
  src <- readFile (head $ tail args)
  let input = fromRight [] $ P.parse' (many $ P.takeWhile ASCII.isDigit <* P.char8 '\n') src
  let commonBits = analysis (length input) (Vector.transpose input)
  
  let gammaRate = convertFromBits commonBits
  let epsilonRate = convertFromBits (not <$> commonBits)

  print $ "Part 1: Power consumption is: " <> show (gammaRate * epsilonRate)

analysis :: Int -> [Bytes] -> [Bool]
analysis n = fmap (isMoreCommon n . countBytes 49)

convertFromBits :: [Bool] -> Int
convertFromBits bs = foldl (\acc (index, bool) -> if bool then setBit acc index else acc) 0 $ zip [11,10..0] bs

isMoreCommon :: Int -> Int -> Bool
isMoreCommon numberOfEntry numberOfOccurance = numberOfOccurance > (numberOfEntry `quot` 2)
