module Render where

import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as Char8
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
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
  | LAddress Address
  | LInt Int
  | LBool Bool
  deriving (Eq)
instance Show LuaType where
  show v = case v of
    LRaw s -> s
    LStr s -> show $ concatMap (\c -> if Char.isPrint c then [c] else printf "%03d" (Char.ord c)) s
    LBytes s -> "\"" ++ Char8.unpack (B.concatMap escapeLuaByte s) ++ "\""
    LAddress s -> case s of
      AddrI n -> show n
      AddrL l -> "l." ++ l
      AddrSum a b -> "(" ++ show (LAddress a) ++ " + " ++ show (LAddress b) ++ ")"
      AddrDiff a b -> "(" ++ show (LAddress a) ++ " - " ++ show (LAddress b) ++ ")"
    LInt n -> show n
    LBool b -> if b then "true" else "false"

renderArgs :: Map String LuaType -> String
renderArgs vals =
  let parts = Map.elems $ Map.mapWithKey (\k v -> k ++ " = " ++ show v) vals
  in
  "{ " ++ intercalate ", " parts ++ "}"

renderCom :: Maybe String -> Command -> String
renderCom origsrc com = case com ^. op of
  Rep from len at rsize final ->
    let at' = case at of Just at -> [("at", LAddress at)]; _ -> []
        size' = case rsize of Just rsize -> [("size", LInt rsize)]; _ -> [("size", LInt $ com ^. size)]
        args = renderArgs $ Map.fromList $ [("from", LAddress from), ("len", LAddress len)] ++ at' ++ size' ++ [("final", LBool final)]
    in "rep " ++ args
  Print str final ->
    let args = renderArgs $ Map.fromList $ [("string", LStr str), ("final", LBool final)]
    in "print " ++ args
  PrintLen len final ->
    let args = renderArgs $ Map.fromList $ [("len", LAddress len), ("final", LBool final)]
    in "print " ++ args
  Data bytes ->
    case origsrc of
      Just s -> s
      Nothing ->
        let args = renderArgs $ Map.fromList $ [("string", LBytes bytes)]
        in "data " ++ args
  Label l -> "_" ++ show l
  Zero ranges ->
    let lranges = intercalate ", " $ map (\(a, b) -> "{ " ++ show (LAddress a) ++ ", " ++ show (LAddress b) ++ "}") ranges
        args = renderArgs $ Map.fromList $ [("ranges", LRaw $ "{" ++ lranges ++ "}")]
    in "zero " ++ args
  DataInt value size ->
    let args = renderArgs $ Map.fromList $ [("int", LAddress value), ("size", LInt size)]
    in "data " ++ args
  _ -> show $ com ^. op
  
  
render :: [String] -> Command -> String
render srcs com@(Com op src size osize pos opos) =
  let origsrc = case src of
        SrcLua (f, n) -> Just $ srcs !! (n - 1)
        _ -> Nothing
  in
  -- TODO: actually use the origsrc.  It breaks weirdly right now.
  printf "--[[%3d=>%3d +%2d=>%2d]] %s -- %s" pos opos size osize (renderCom {-origsrc-}Nothing com) (show origsrc)
