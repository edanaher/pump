module Language where

import Data.ByteString as B

data Op =
    Rep Int Int (Maybe Int) Bool
  | Print String Bool
  | PrintLen Int Bool
  | Data B.ByteString
  | Copy Int Int
  | Label String
  deriving  (Eq)

instance Show Op where
  show op = case op of
    Rep from len at final -> "Rep from=" ++ show from ++ " len=" ++ show len ++ " at=" ++ show at ++ " final=" ++ show final
    Print str final -> "Print " ++ show str ++ " final=" ++ show final
    PrintLen len final -> "Print " ++ show len ++ " final=" ++ show final
    Data bytes -> "Data " ++ show bytes
    Copy from len -> "Copy from=" ++ show from ++ " len=" ++ show len
    Label name -> name ++ ":"

data Command = Com Op Int Int Int Int
  deriving (Eq)

instance Show Command where
  show (Com op size osize pos opos) = show pos ++ "=>" ++ show opos ++ "; +" ++ show size ++ "=>" ++ show osize ++ "  " ++ show op

