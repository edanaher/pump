{-# LANGUAGE OverloadedStrings #-}
module Simulate where

import Language
import qualified Data.ByteString.Char8 as Char8
import Data.List (mapAccumL)
import Debug.Trace (trace)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens ((^.))


simulateRep :: Int -> Int -> Int -> [Command] -> ([Command], Int, Int)
simulateRep from len pos coms =
  let start = dropWhile (\ (Com _ _ _ _ pos _) -> pos < from) coms
      base = takeWhile (\ (Com _ _ _ _ pos _) -> pos < from + len) start
      (coms', pos') = foldl (\ (coms, pos) (Com op src size osize _ _) ->
          (Com op src size osize pos 0:coms, pos + size)) ([], pos) base
  in
  --trace ("--- Produced from " ++ show from ++ "/" ++ show len ++ " => " ++ show pos ++ "," ++ show opos ++ ":\n" ++ (unlines $ map show $ reverse coms') ++ "--------" ) $
  (reverse coms', 0, pos')

simulate' :: (Map String Int) -> [Command] -> Int -> Int -> [Command] -> [Command]
simulate' labels [] _ _ output = output
simulate' labels (Com op src size osize pos opos:coms) printLen simpos output =
  if printLen < 0 then error "printLen went negative on simulation!" else
  let (out, printLen', simpos') =
        if printLen > 0 then
          --trace ("Eating " ++ show op ++ " at " ++ show pos ++ "/" ++ show opos) $
          case op of
            Label _ -> ([], printLen, simpos)
            {-Zero ranges ->
                let adjust x = trace ("Adjusting " ++ show simpos ++ " from " ++ show pos) AddrI $ (x @! labels) + simpos - pos
                    ranges' = map (\(a,b) -> (adjust a, adjust b)) ranges in
                ([Com (Zero ranges') src size osize simpos 0], printLen - size, simpos + size)-}
            _ -> ([Com op src size osize simpos 0], printLen - size, simpos + size)
        else
          --trace ("simulating " ++ show op ++ " at " ++ show simpos ++ "/") $
          case op of
            Print str final -> ([Com (Data $ Char8.pack str) src (length str) 0 simpos 0], 0, simpos + length str)
            PrintLen len final -> ([], evalAddr labels len, simpos)
            Rep from len at _ final -> simulateRep (evalAddr labels from) (evalAddr labels len) simpos output
            _ -> ([], 0, simpos)
  in simulate' labels coms printLen' simpos' (output ++ out)

addLabels coms sims = case (coms, sims) of
  ([], sims) -> sims
  (tail, []) -> filter (\com -> case com ^. op of Label _ -> True; _ -> False)  tail
  (com:coms', sim:sims') ->
    let (labelledSim, coms'', sims'') = case (com, sim) of
          (Com _ _ _ _ pos _, Com _ _ _ _ spos _) | pos < spos -> ([], coms', sim:sims')
          (Com _ _ _ _ pos _, Com _ _ _ _ spos _) | pos > spos -> ([sim], com:coms', sims')
          (Com (Label str) _ _ _ _ _, Com _ _ _ _ spos sopos) -> ([Com (Label str) SrcNone 0 0 spos sopos], coms', sim:sims')
          _ -> ([sim], coms', sims')
    in
    labelledSim ++ (addLabels coms'' sims'')

outputSize :: (Map String Int) -> Op -> Int
outputSize labels (Print str _) = length str
outputSize labels (PrintLen len _) = evalAddr labels len
outputSize labels (Rep _ len _ _ _) = evalAddr labels len
outputSize labels (Copy _ _) = error $ "Copy command found in stream after copy elimination..."
outputSize labels _ = 0

fixOutPoses :: (Map String Int) -> [Command] -> [Command]
fixOutPoses labels sims = snd $ mapAccumL (\ (opos, eatlen) (Com op src size osize pos _) ->
  let osize' = if eatlen > 0 || opos == -1 then 0 else outputSize labels op
      eatlen' =
        if eatlen > 0 then eatlen - size else
        if opos == -1 then 0 else
        case op of PrintLen len _ -> evalAddr labels len
                   _ -> 0
      opos' =
        if opos == -1 then case op of
          Label "_start" -> 0
          _ -> -1
        else
          opos + osize'
  in ((opos', eatlen'), Com op src size osize' pos opos)) (-1, 0) sims

fixUpSimulated :: (Map String Int) -> [Command] -> [Command] -> [Command]
fixUpSimulated labels coms sims =
  fixOutPoses labels $ addLabels coms sims
  -- TODO Fix zeros.  It's not trivial what this even means...

simulate :: (Map String Int) -> [Command] -> [Command]
simulate labels coms =
  let withoutHeader = dropWhile (\ (Com op _ _ _ _ _) -> op /= (Label "_start")) coms
      body = takeWhile (\ (Com op _ _ _ _ _) -> op /= (Label "_finish")) withoutHeader
      rawSim = simulate' labels body 0 0 []
  in
  fixUpSimulated labels coms rawSim
