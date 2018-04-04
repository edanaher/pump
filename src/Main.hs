{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import System.Console.Docopt
import System.Environment (getArgs)

import Pump

params :: Docopt
params = [docopt|
pump - a "compiler" for gzip and other deflate-based "languages"

Usage:
  pump [options] <file> [options]

Options:
  --simfile=<simfile>    Save simulated output to <simfile>
  --output=<outfile>, -o Write output file to <outfile>
  --simulate, -s         Simulate and save to <simfile>.in/.out (defaults to <outfile>)
|]

getArgOrExit = getArgOrExitWith params

main :: IO ()
main = do
  args <- parseArgsOrExit params =<< getArgs
  filename <- args `getArgOrExit` (argument "file")
  simfile <- return $ args `getArg` (longOption "simfile")
  simulate <- return $ args `isPresent` (longOption "simulate")
  outfile <- return $ args `getArg` (longOption "output")
  Pump.compile filename outfile simulate simfile
