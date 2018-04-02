{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import System.Console.Docopt
import System.Environment (getArgs)

import Pump

params :: Docopt
params = [docopt|
pump - a "compiler" for gzip and other deflate-based "languages"

Usage:
  pump [--simulate] <file>

Options:
  --simulate -s    Do simulation
|]

getArgOrExit = getArgOrExitWith params

main :: IO ()
main = do
  args <- parseArgsOrExit params =<< getArgs
  filename <- args `getArgOrExit` (argument "file")
  Pump.compile filename
