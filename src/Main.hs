{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import System.Console.Docopt
import System.Environment (getArgs)

import Pump

params :: Docopt
params = [docopt|
pump - a "compiler" for gzip and other deflate-based "languages"

Usage:
  pump [--simulate=<simfile>] <file>

Options:
  --simulate=<simfile>, -s    Save simulated output to <simfile>
|]

getArgOrExit = getArgOrExitWith params

main :: IO ()
main = do
  args <- parseArgsOrExit params =<< getArgs
  filename <- args `getArgOrExit` (argument "file")
  simfile <- return $ args `getArg` (longOption "simulate")
  Pump.compile filename simfile
