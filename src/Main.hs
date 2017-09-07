{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import System.Directory (makeAbsolute)
import System.Environment (getArgs, getExecutablePath)
import Data.Bits (shiftR, (.&.))

import Rep

import Foreign.Lua as Lua

dirpath :: String -> String
dirpath = reverse . dropWhile (/= '/') . reverse

dslFile :: IO FilePath
dslFile = do
  exePath <- getExecutablePath
  makeAbsolute $ (dirpath exePath) ++ "/../lua/dsl.lua"

data Op =
    Rep Int Int Int
  | Print String
  | Data B.ByteString
  | Copy Int Int
  deriving  (Show, Eq)

instance FromLuaStack Op where
  peek idx = do
    tp <- getField Lua.peek "type" :: Lua String
    case tp of
      "rep" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        at <- getField Lua.tointeger "at"
        return $ Rep (fromInteger $ toInteger from) (fromInteger $ toInteger len) (fromInteger $ toInteger at)
      "print" -> do
        str <- getField Lua.peek "string"
        return $ Print str
      "copy" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        return $ Copy (fromInteger $ toInteger from) (fromInteger $ toInteger len)
      "data" -> do
        str <- getField Lua.tostring "string"
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

intToBytes :: Int -> Int -> B.ByteString
intToBytes len n = B.pack [fromIntegral $ (n `shiftR` (8*i)) .&. 255 | i <- [0..len-1] ]

toBytes :: Op -> B.ByteString
toBytes op = case op of
  Data str -> str
  Print str -> "\000" `Char8.append`
               (intToBytes 2 $ length str) `Char8.append`
               (intToBytes 2 $ 65535 - length str) `Char8.append`
               Char8.pack str
  Rep from len at -> Rep.encode from len at
  _ -> "Unknown"

writeGzip filename bytes =
  B.writeFile filename (foldl B.append B.empty bytes)

main :: IO ()
main = do
  [ filename ] <- getArgs
  program <- readProgram filename
  bytes <- return $ map toBytes program
  putStrLn $ show bytes
  writeGzip "out.gz" bytes
