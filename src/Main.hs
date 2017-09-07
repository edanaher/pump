{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate)
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import System.Directory (makeAbsolute)
import System.Environment (getArgs, getExecutablePath)
import Data.Bits (shiftR, (.&.))
import Debug.Trace (trace)

import Rep

import qualified Foreign.Lua as Lua

dirpath :: String -> String
dirpath = reverse . dropWhile (/= '/') . reverse

dslFile :: IO FilePath
dslFile = do
  exePath <- getExecutablePath
  makeAbsolute $ (dirpath exePath) ++ "/../lua/dsl.lua"

data Op =
    Rep Int Int (Maybe Int) Bool
  | Print String Bool
  | Data B.ByteString
  | Copy Int Int
  deriving  (Show, Eq)

instance Lua.FromLuaStack Op where
  peek idx = do
    tp <- getField Lua.peek "type" :: Lua.Lua String
    case tp of
      "rep" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        at <- getFieldOpt (liftM fromInteger . liftM toInteger . Lua.tointeger) "at"
        final <- getFieldBool "final"
        return $ Rep (fromInteger $ toInteger from) (fromInteger $ toInteger len) at final
      "print" -> do
        str <- getField Lua.peek "string"
        final <- getFieldBool "final"
        return $ Print str final
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
          getFieldOpt peek f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
            isnil <- Lua.isnil (-1)
            if isnil then Lua.remove (-1) >> return Nothing else do
              v <- peek (-1)
              Lua.remove (-1)
              return $ Just v
          getFieldBool f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
            isnil <- Lua.isnil (-1)
            if isnil then Lua.remove (-1) >> return False else do
              v <- Lua.peek (-1)
              Lua.remove (-1)
              return $ v

readProgram :: String -> IO [Op]
readProgram filename = do
  dslPath <- dslFile
  Lua.runLua $ do
    Lua.openlibs

    Lua.dofile dslPath
    Lua.dofile filename
    Lua.getglobal "program" *> Lua.peek (-1)

data ByteOp =
    Bytes B.ByteString
  | Zero Int Int

intToBytes :: Int -> Int -> B.ByteString
intToBytes len n = B.pack [fromIntegral $ (n `shiftR` (8*i)) .&. 255 | i <- [0..len-1] ]

outputSize :: Op -> Int
outputSize (Print str _) = length str
outputSize (Rep _ len _ _) = len
outputSize _ = 0

toBytes :: Op -> Int -> B.ByteString
toBytes op pos = case op of
  Data str -> str
  Print str final -> (if final then "\001" else "\000") `Char8.append`
                     (intToBytes 2 $ length str) `Char8.append`
                     (intToBytes 2 $ 65535 - length str) `Char8.append`
                     Char8.pack str
  Rep from len (Just at) final -> Rep.encode from len at final
  Rep from len Nothing final -> Rep.encode from len pos final
  _ -> "Unknown"

writeGzip filename bytes =
  B.writeFile filename (foldl B.append B.empty bytes)

progToBytes :: [Op] -> [B.ByteString]
progToBytes program =
  reverse $ fst $ foldl (\ (prog, pos) op ->
      let bytes = toBytes op pos
          pos' = if pos == -1 then -- Hack - assume first print is start of executable
                   case op of Print _ _ -> outputSize op
                              _ -> -1
                 else pos + outputSize op
      in (bytes:prog, pos'))
    ([], -1) program

main :: IO ()
main = do
  [ filename ] <- getArgs
  program <- readProgram filename
  bytes <- return $ progToBytes program
  putStrLn $ show bytes
  writeGzip "out.gz" bytes
