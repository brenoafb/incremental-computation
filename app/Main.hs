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


xs = [1,0,2,0,3,4]

rdgBeforePropagation = rdgTokenCount xs

rdgAfterPropagation = propagateRdg rdg

resultBeforeChange = executeRdg rdgAfterPropagation

mptr = mkMPtr xs rdgAfterPropagation

mptrAtChangeLocation = mchange 0 (mforward $ mforward mptr)

(xs', rdgAfterChange) = mrewind mptrAtChangeLocation

rdgPropagatedAfterChange = propagateRdg rdgAfterChange

resultAfterChange = executeRdg rdgAfterChange
