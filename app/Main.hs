module Main where

import Parser
import Interpreter
import Analysis

import qualified Data.Map as Map

import System.IO ( isEOF )
import System.Environment ( getArgs )

{-
Right progFib <- parseString <$> readFile "examples/fibonacci.im"
Right progIf <- parseString <$> readFile "examples/if.im"
Right progExample <- parseString <$> readFile "examples/example.im"
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> interpreter fileName
    _ -> usage

interpreter :: FilePath -> IO ()
interpreter fp = do
  code <- readFile fp
  case parseString code of
    Left err -> print err
    Right stmt -> print $ exec stmt Map.empty

usage :: IO ()
usage = putStrLn "usage: imperative [-c] <file>"
