{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Z.IO.FileSystem.Base (readFile)
import Z.IO (getArgs, printStdLn)

import qualified Z.Data.Parser as P
import Z.Data.Parser (Parser)
import Control.Applicative (many)
import Data.Either (fromRight)
import Z.Data.Vector.Base (Bytes)

import qualified Z.Data.ASCII as ASCII

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving Show

data Pos = Pos Int Int

instance Show Pos where
  show (Pos x y) = "The final result is: " ++ show (x * y)

main :: IO ()
main = do
  args <- getArgs
  src <- readFile (head $ tail args)
  let input = fromRight [] $ P.parse' (many $ parseLog <* P.char8 '\n') src
  print $ go (Pos 0 0) input

go :: Pos -> [Command] -> Pos
go pos [] = pos
go (Pos x y) ((Forward n):rest) = go (Pos (x + n) y) rest
go (Pos x y) ((Down n):rest) = go (Pos x (y + n)) rest
go (Pos x y) ((Up n): rest) = go (Pos x (y - n)) rest

parseLog :: Parser Command
parseLog = do
  command <- P.takeTill ASCII.isSpace
  P.skipSpaces -- space
  n <- P.int
  pure $ case command of
    "forward" -> Forward n
    "up" -> Up n
    "down" -> Down n
    _ -> error "error unknown command"
