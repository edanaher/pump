module Render where

import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as Char8
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.Printf (printf)

import Control.Lens ((^.))

import Language

escapeLuaByte b =
  if Char.isPrint (Char.chr $ fromEnum b)
  then B.singleton b
  else
    let digits = printf "%03d" (fromEnum b)
        padding = take (3 - length digits) $ repeat '0'
    in Char8.pack $ "\\" ++ padding ++ digits

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
    LStr s -> show $ concatMap (\c -> if Char.isPrint c then [c] else printf "%03d" (Char.ord c)) s
    LBytes s -> "\"" ++ Char8.unpack (B.concatMap escapeLuaByte s) ++ "\""
    LInt n -> show n
    LBool b -> if b then "true" else "false"

renderArgs :: Map String LuaType -> String
renderArgs vals =
  let parts = Map.elems $ Map.mapWithKey (\k v -> k ++ " = " ++ show v) vals
  in
  "{ " ++ intercalate ", " parts ++ "}"

renderOp :: [String] -> Op -> Int -> String
renderOp hints op comsize = case op of
  Rep from len at size final ->
    let at' = case at of Just at -> [("at", LInt at)]; _ -> []
        size' = case size of Just rsize -> [("size", LInt rsize)]; _ -> [("size", LInt comsize)]
        args = renderArgs $ Map.fromList $ [("from", LInt from), ("len", LInt len)] ++ at' ++ size' ++ [("final", LBool final)]
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
        args = renderArgs $ Map.fromList $ [("ranges", LRaw $ "{" ++ lranges ++ "}")]
    in "zero " ++ args
  _ -> show op
  
  
render :: [Command] -> Command -> String
render hints com = case com of
  Com op src size osize pos opos -> printf "--[[%3d=>%3d +%2d=>%2d]] %s" pos opos size osize (renderOp [] op size)
