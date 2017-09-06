{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
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
  | Copy Int Int
  deriving  (Show, Eq)

instance FromLuaStack Op where
  peek idx = do
    tp <- getField Lua.peek "type" :: Lua String
    case tp of
      "rep" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        return $ Rep (fromInteger $ toInteger from) (fromInteger $ toInteger len)
      "copy" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        return $ Copy (fromInteger $ toInteger from) (fromInteger $ toInteger len)
      "data" -> do
        str <- getField Lua.peek "string"
        return $ Data str
    where getField peek f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
            v <- peek (-1)
            Lua.remove (-1)
            return v

readProgram :: String -> IO [Op]
readProgram filename = do
  dslPath <- dslFile
  Lua.runLua $ do
    Lua.openlibs

    Lua.dofile dslPath
    Lua.dofile filename
    Lua.getglobal "program" *> peek (-1)

data ByteOp =
    Bytes B.ByteString
  | Zero Int Int

toBytes :: Op -> B.ByteString
toBytes op = case op of
  Data str -> Char8.pack str
  _ -> "Unknown"

main :: IO ()
main = do
  [ filename ] <- getArgs
  program <- readProgram filename
  putStrLn $ show (map toBytes program)
