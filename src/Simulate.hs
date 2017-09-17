{-# LANGUAGE OverloadedStrings #-}
module Simulate where

import Language
import qualified Data.ByteString.Char8 as Char8
import Data.List (mapAccumL)
import Debug.Trace (trace)


simulateRep :: Int -> Int -> Int -> [Command] -> ([Command], Int, Int)
simulateRep from len pos coms =
  let start = dropWhile (\ (Com _ _ _ pos _) -> pos < from) coms
      base = takeWhile (\ (Com _ _ _ pos _) -> pos < from + len) start
      (coms', pos') = foldl (\ (coms, pos) (Com op size osize _ _) ->
          (Com op size osize pos 0:coms, pos + size)) ([], pos) base
  in
  --trace ("--- Produced from " ++ show from ++ "/" ++ show len ++ " => " ++ show pos ++ "," ++ show opos ++ ":\n" ++ (unlines $ map show $ reverse coms') ++ "--------" ) $
  (reverse coms', 0, pos')

simulate' :: [Command] -> Int -> Int -> [Command] -> [Command]
simulate' [] _ _ output = output
simulate' (Com op size osize pos opos:coms) printLen simpos output =
  if printLen < 0 then error "printLen went negative on simulation!" else
  let (out, printLen', simpos') =
        if printLen > 0 then
          --trace ("Eating " ++ show op ++ " at " ++ show pos ++ "/" ++ show opos) $
          case op of
            Label _ -> ([], printLen, simpos)
            _ -> ([Com op size osize simpos 0], printLen - size, simpos + size)
        else
          --trace ("simulating " ++ show op ++ " at " ++ show simpos ++ "/") $
          case op of
            Print str final -> ([Com (Data $ Char8.pack str) (length str) 0 simpos 0], 0, simpos + length str)
            PrintLen len final -> ([], len, simpos)
            Rep from len at final -> simulateRep from len simpos output
            _ -> ([], 0, simpos)
  in simulate' coms printLen' simpos' (output ++ out)

addLabels coms sims = case (coms, sims) of
  ([], sims) -> sims
  (_, []) -> []
  (com:coms', sim:sims') ->
    let (labelledSim, coms'', sims'') = case (com, sim) of
          (Com _ _ _ pos _, Com _ _ _ spos _) | pos < spos -> ([], coms', sim:sims')
          (Com _ _ _ pos _, Com _ _ _ spos _) | pos > spos -> ([sim], com:coms', sims')
          (Com (Label str) _ _ _ _, Com _ _ _ spos sopos) -> ([Com (Label str) 0 0 spos sopos], coms', sim:sims')
          _ -> ([sim], coms', sims')
    in
    labelledSim ++ (addLabels coms'' sims'')

outputSize :: Op -> Int
outputSize (Print str _) = length str
outputSize (PrintLen len _) = len
outputSize (Rep _ len _ _) = len
outputSize _ = 0

fixOutPoses :: [Command] -> [Command]
fixOutPoses sims = snd $ mapAccumL (\ (opos, eatlen) (Com op size osize pos _) ->
  let osize' = if eatlen > 0 || opos == -1 then 0 else outputSize op
      eatlen' =
        if eatlen > 0 then eatlen - size else
        if opos == -1 then 0 else
        case op of PrintLen len _ -> len
                   _ -> 0
      opos' =
        if opos == -1 then case op of
          Label "_start" -> 0
          _ -> -1
        else
          opos + osize'
  in ((opos', eatlen'), Com op size osize' pos opos)) (-1, 0) sims

fixUpSimulated :: [Command] -> [Command] -> [Command]
fixUpSimulated coms sims =
  fixOutPoses $ addLabels coms sims

simulate :: [Command] -> [Command]
simulate coms =
  let withoutHeader = dropWhile (\ (Com op _ _ _ _) -> op /= (Label "_start")) coms
      body = takeWhile (\ (Com op _ _ _ _) -> op /= (Label "_finish")) withoutHeader
      rawSim = simulate' body 0 0 []
  in
  fixUpSimulated coms rawSim
