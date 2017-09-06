{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate)
import System.Directory (makeAbsolute)
import System.Environment (getArgs, getExecutablePath)

import Foreign.Lua as Lua

dirpath :: String -> String
dirpath = reverse . dropWhile (/= '/') . reverse

dslFile :: IO FilePath
dslFile = do
  exePath <- getExecutablePath
  makeAbsolute $ (dirpath exePath) ++ "/../lua/dsl.lua"

data Op =
    Rep Int Int
  | Data String
  deriving  (Show, Eq)

instance FromLuaStack Op where
  peek idx = do
    tp <- getField Lua.peek "type" :: Lua String
    case tp of
      "rep" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        return $ Rep (fromInteger $ toInteger from) (fromInteger $ toInteger len)
      "data" -> do
        str <- getField Lua.peek "string"
        return $ Data str
    where getField peek f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
            v <- peek (-1)
            Lua.remove (-1)
            return v


main :: IO ()
main = do
  [ file ] <- getArgs
  dslPath <- dslFile
  putStrLn dslPath
  Lua.runLua $ do
    Lua.openlibs

    Lua.dofile dslPath
    Lua.dofile file
    x <- Lua.getglobal "program" *> peek (-1)
    liftIO $ putStrLn $ show (x :: [Op])
