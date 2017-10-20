{-# LANGUAGE OverloadedStrings #-}

module Language where

import Data.ByteString as B
import qualified Foreign.Lua as Lua

data Op =
    Rep Int Int (Maybe Int) Bool
  | Zero [(Int, Int)]
  | Print String Bool
  | PrintLen Int Bool
  | Data B.ByteString
  | Copy Int Int
  | Label String
  | Padding Int
  deriving  (Eq)

instance Show Op where
  show op = case op of
    Rep from len at final -> "Rep from=" ++ show from ++ " len=" ++ show len ++ " at=" ++ show at ++ " final=" ++ show final
    Zero ranges -> "Zero " ++ show ranges
    Print str final -> "Print " ++ show str ++ " final=" ++ show final
    PrintLen len final -> "Print " ++ show len ++ " final=" ++ show final
    Data bytes -> "Data " ++ show bytes
    Copy from len -> "Copy from=" ++ show from ++ " len=" ++ show len
    Label name -> name ++ ":"
    Padding len -> "[Padding " ++ show len ++ "]"

data Command = Com Op Source Int Int Int Int
  deriving (Eq)

instance Show Command where
  show (Com op src size osize pos opos) = show pos ++ "=>" ++ show opos ++ "; +" ++ show size ++ "=>" ++ show osize ++ "  " ++ show op ++ " <= " ++ show src

data ByteOp =
    Bytes B.ByteString
  | BZero [(Int, Int)]
  deriving (Eq, Show)

data Source =
    SrcNone
  | SrcLua (String, Int)
  | SrcCopy SrcedOp Int Int
  deriving (Eq, Show)

newtype SrcedOp = SrcedOp (Op, Source)
  deriving (Eq, Show)

newtype DslErr = DslErr (String, String, Int)
  deriving Eq

instance Show DslErr where
  show (DslErr (err, file, line)) = file ++ ":" ++ show line ++ " " ++ err

instance Lua.FromLuaStack DslErr where
  peek idx = do
    err <- getField Lua.peek "err"
    file <- getField Lua.peek "_file"
    line <- getField Lua.tointeger "_line"
    return $ DslErr (err, file, fromIntegral line)
    where
      getField peek f = do
        Lua.pushstring f
        Lua.gettable (idx - 1)
        isnil <- Lua.isnil (-1)
        v <- peek (-1)
        Lua.remove (-1)
        return v
