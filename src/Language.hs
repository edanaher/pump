{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Language where

import Data.ByteString as B
import Text.Printf (printf)

import Control.Lens ((^?), makeLenses)

import qualified Foreign.Lua as Lua

data Op =
    Rep { _from :: Int, _to :: Int, _at :: Maybe Int, _rsize :: Maybe Int, _final :: Bool }
  | Zero [(Int, Int)]
  | Print String Bool
  | PrintLen { _len :: Int, _final :: Bool }
  | Data B.ByteString
  | Copy { _from :: Int, _to :: Int }
  | Label { _label :: String }
  | Padding Int
  deriving  (Eq)

makeLenses ''Op

showMaybe str (Just x) = str ++ show x
showMaybe str Nothing = ""

instance Show Op where
  show op = case op of
    Rep from len at size final -> "Rep @" ++ show from ++ "+" ++ show len ++ showMaybe " at " at ++ showMaybe " size " size ++ (if final then " final" else "")
    Zero ranges -> "Zero " ++ show ranges
    Print str final -> "Print " ++ show str ++ " final=" ++ show final
    PrintLen len final -> "Print " ++ show len ++ " final=" ++ show final
    Data bytes -> "Data " ++ show bytes
    Copy from len -> "Copy @" ++ show from ++ "+" ++ show len
    Label name -> name ++ ":"
    Padding len -> "[Padding " ++ show len ++ "]"

data Source =
    SrcNone
  | SrcLua (String, Int)
  | SrcCopy SrcedOp Int Int
  deriving (Eq)

instance Show Source where
  show src = case src of
    SrcNone -> "_"
    SrcLua (file, line) -> file ++ ":" ++ show line
    SrcCopy parent which total -> "[" ++ show which ++ "/" ++ show total ++ "; " ++ show parent ++ "]"

newtype SrcedOp = SrcedOp (Op, Source)
  deriving (Eq)

instance Show SrcedOp where
  show (SrcedOp (op, src)) = show src ++ ": " ++ show op

data Command = Com { _op :: Op, _src :: Source, _size :: Int, _osize :: Int, _pos :: Int, _opos :: Int }
  deriving (Eq)

makeLenses ''Command

instance Show Command where
  show (Com op src size osize pos opos) = printf "%3d=>%3d +%2d=>%2d %s <= %s" pos opos size osize (show op) (show src)

data ByteOp =
    Bytes B.ByteString
  | BZero [(Int, Int)]
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
