module Main where

import System.Directory (makeAbsolute)
import System.Environment (getExecutablePath)

import Foreign.Lua as Lua

dirpath :: String -> String
dirpath = reverse . dropWhile (/= '/') . reverse

dslFile :: IO FilePath
dslFile = do
  exePath <- getExecutablePath
  makeAbsolute $ (dirpath exePath) ++ "/../lua/dsl.lua"

main :: IO ()
main = do
  dslPath <- dslFile
  putStrLn dslPath
  Lua.runLua $ do
    Lua.openlibs

    Lua.dofile dslPath
    x <- Lua.getglobal "loaded" *> peek (-1)
    liftIO $ putStrLn x
