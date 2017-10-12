module Language where

import Data.ByteString as B

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
