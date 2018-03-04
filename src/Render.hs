module Render where

import qualified Data.ByteString as B
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Lens ((^.))

import Language

data LuaType =
    LRaw String
  | LStr String
  | LBytes B.ByteString
  | LInt Int
  | LBool Bool
  deriving (Eq)
instance Show LuaType where
  show v = case v of
    LRaw s -> s
    LStr s -> show s
    LBytes s -> show s
    LInt n -> show n
    LBool b -> if b then "true" else "false"

renderArgs :: Map String LuaType -> String
renderArgs vals =
  let parts = Map.elems $ Map.mapWithKey (\k v -> k ++ " = " ++ show v) vals
  in
  "{ " ++ intercalate ", " parts ++ "}"

renderOp :: [String] -> Op -> String
renderOp hints op = case op of
  Rep from len at final ->
    let at' = case at of Just at -> [("at", LInt at)]; _ -> []
        args = renderArgs $ Map.fromList $ [("from", LInt from), ("len", LInt len)] ++ at' ++ [("final", LBool final)]
    in "rep " ++ args
  Print str final ->
    let args = renderArgs $ Map.fromList $ [("string", LStr str), ("final", LBool final)]
    in "print " ++ args
  PrintLen len final ->
    let args = renderArgs $ Map.fromList $ [("len", LInt len), ("final", LBool final)]
    in "print " ++ args
  Data bytes ->
    let args = renderArgs $ Map.fromList $ [("string", LBytes bytes)]
    in "data " ++ args
  Label l -> "_" ++ show l
  Zero ranges ->
    let lranges = intercalate ", " $ map (\(a, b) -> "{ " ++ show a ++ ", " ++ show b ++ "}") ranges
        args = renderArgs $ Map.fromList $ [("ranges", LRaw lranges)]
    in "zero " ++ args
  _ -> show op
  
  
render :: [Command] -> Command -> String
render hints com = renderOp [] (com ^. op)
