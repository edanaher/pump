{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

module Language where

import Data.ByteString as B
import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Lens ((^?), makeLenses)

import qualified Foreign.Lua as Lua

import Data.Generics

type LabelMap = Map String Int

data Address =
    AddrI Int
  | AddrL String
  | AddrSum Address Address
  | AddrDiff Address Address
  deriving (Eq, Data, Typeable, Ord)

instance Show Address where
  show addr = case addr of
    AddrI n -> show n
    AddrL l -> l
    AddrSum a b -> "(" ++ show a ++ " + " ++ show b ++ ")"
    AddrDiff a b -> "(" ++ show a ++ " - " ++ show b ++ ")"

evalAddr :: (Map String Int) -> Address -> Int
evalAddr labels addr =
  case addr of
    AddrI n -> n
    AddrL l -> labels Map.! l
    AddrSum a b -> evalAddr labels a + evalAddr labels b
    AddrDiff a b -> evalAddr labels a - evalAddr labels b

(@!) = flip evalAddr

infixr 9 @!

data Op =
    Rep { _from :: Address, _len :: Address, _at :: Maybe Address, _rsize :: Maybe Int, _final :: Bool }
  | Zero [(Address, Address)]
  | Print String Bool
  | PrintLen { _len :: Address, _final :: Bool }
  | Data B.ByteString
  | DataInt { _value :: Address, _dsize :: Int }
  | Copy { _from :: Address, _len :: Address }
  | Clone { _index :: Int }
  | Label { _label :: String }
  | Padding Int
  deriving  (Eq, Data, Typeable, Ord)

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
    DataInt addr size -> "Data [" ++ show size ++ "] " ++ show addr
    Copy from len -> "Copy @" ++ show from ++ "+" ++ show len
    Label name -> name ++ ":"
    Padding len -> "[Padding " ++ show len ++ "]"
    Clone n -> "[Clone " ++ show n ++ "]"

data Source =
    SrcNone
  | SrcLua (String, Int)
  | SrcCopy SrcedOp Int Int
  deriving (Eq, Data, Typeable)

instance Show Source where
  show src = case src of
    SrcNone -> "_"
    SrcLua (file, line) -> file ++ ":" ++ show line
    SrcCopy parent which total -> "[" ++ show which ++ "/" ++ show total ++ "; " ++ show parent ++ "]"

newtype SrcedOp = SrcedOp (Op, Source)
  deriving (Eq, Data, Typeable)

instance Show SrcedOp where
  show (SrcedOp (op, src)) = show src ++ ": " ++ show op

data Command = Com { _op :: Op, _src :: Source, _size :: Int, _osize :: Int, _pos :: Int, _opos :: Int }
  deriving (Eq, Data, Typeable)

makeLenses ''Command

instance Show Command where
  show (Com op src size osize pos opos) = printf "%3d=>%3d +%2d=>%2d %s <= %s" pos opos size osize (show op) (show src)

data ByteOp =
    Bytes B.ByteString
  | BZero [(Int, Int)]
  deriving (Eq, Show)

type ComByte = (Command, ByteOp)

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
