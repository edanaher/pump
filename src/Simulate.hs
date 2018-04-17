{-# LANGUAGE OverloadedStrings #-}
module Simulate where

import Language
import qualified Data.ByteString.Char8 as Char8
import Data.List (mapAccumL)
import Debug.Trace (trace)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens ((^.), (&), (.~))


simulateRep :: Int -> Int -> Int -> [ComByte] -> ([ComByte], Int, Int)
simulateRep from len pos coms =
  let start = dropWhile (\ (Com _ _ _ _ pos _, _) -> pos < from) coms
      base = takeWhile (\ (Com _ _ _ _ pos _, _) -> pos < from + len) start
      (coms', pos') = foldl (\ (coms, pos) (Com op src size osize _ _, bytes) ->
          ((Com op src size osize pos 0, bytes):coms, pos + size)) ([], pos) base
  in
  --trace ("--- Produced from " ++ show from ++ "/" ++ show len ++ " => " ++ show pos ++ "," ++ show opos ++ ":\n" ++ (unlines $ map show $ reverse coms') ++ "--------" ) $
  (reverse coms', 0, pos')

simulate' :: (Map String Int) -> [ComByte] -> Int -> Int -> [ComByte] -> [ComByte]
simulate' labels [] _ _ output = output
simulate' labels ((com,bytes):coms) printLen simpos output =
  if printLen < 0 then error "printLen went negative on simulation!" else
  let (out, printLen', simpos') =
        if printLen > 0 then
          --trace ("Eating " ++ show op ++ " at " ++ show pos ++ "/" ++ show opos) $
          case com ^. op of
            Label _ -> ([], printLen, simpos)
            {-Zero ranges ->
                let adjust x = trace ("Adjusting " ++ show simpos ++ " from " ++ show pos) AddrI $ (x @! labels) + simpos - pos
                    ranges' = map (\(a,b) -> (adjust a, adjust b)) ranges in
                ([Com (Zero ranges') src size osize simpos 0], printLen - size, simpos + size)-}
            _ -> ([(com & (pos .~ simpos) . (opos .~ 0), bytes) ], printLen - com ^. size, simpos + com ^. size)
        else
          --trace ("simulating " ++ show op ++ " at " ++ show simpos ++ "/") $
          case com ^. op of
            Print str final -> ([(Com (Data $ Char8.pack str) (com ^. src) (length str) 0 simpos 0, bytes)], 0, simpos + length str)
            PrintLen len final -> ([], evalAddr labels len, simpos)
            Rep from len at _ final -> simulateRep (evalAddr labels from) (evalAddr labels len) simpos output
            _ -> ([], 0, simpos)
  in simulate' labels coms printLen' simpos' (output ++ out)

addLabels :: [ComByte] -> [ComByte] -> [ComByte]
addLabels coms sims = case (coms, sims) of
  ([], sims) -> sims
  (tail, []) -> filter (\(com, _) -> case com ^. op of Label _ -> True; _ -> False)  tail
  ((com, bytes):coms', (sim, sbytes):sims') ->
    let (labelledSim, coms'', sims'') = case (com, sim) of
          (Com _ _ _ _ pos _, Com _ _ _ _ spos _) | pos < spos -> ([], coms', (sim, sbytes):sims')
          (Com _ _ _ _ pos _, Com _ _ _ _ spos _) | pos > spos -> ([(sim, sbytes)], (com,bytes):coms', sims')
          (Com (Label str) _ _ _ _ _, Com _ _ _ _ spos sopos) -> ([(Com (Label str) SrcNone 0 0 spos sopos, Bytes "")], coms', (sim,sbytes):sims')
          _ -> ([(sim, sbytes)], coms', sims')
    in
    labelledSim ++ (addLabels coms'' sims'')

outputSize :: (Map String Int) -> Op -> Int
outputSize labels (Print str _) = length str
outputSize labels (PrintLen len _) = evalAddr labels len
outputSize labels (Rep _ len _ _ _) = evalAddr labels len
outputSize labels (Copy _ _) = error $ "Copy command found in stream after copy elimination..."
outputSize labels _ = 0

fixOutPoses :: (Map String Int) -> [ComByte] -> [ComByte]
fixOutPoses labels sims = snd $ mapAccumL (\ (opos, eatlen) (Com op src size osize pos _, bytes) ->
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
  in ((opos', eatlen'), (Com op src size osize' pos opos, bytes))) (-1, 0) sims

fixUpSimulated :: (Map String Int) -> [ComByte] -> [ComByte] -> [ComByte]
fixUpSimulated labels coms sims =
  fixOutPoses labels $ addLabels coms sims
  -- TODO Fix zeros.  It's not trivial what this even means...

simulate :: (Map String Int) -> [ComByte] -> [ComByte]
simulate labels coms =
  let withoutHeader = dropWhile (\ (Com op _ _ _ _ _, _) -> op /= (Label "_start")) coms
      body = takeWhile (\ (Com op _ _ _ _ _, _) -> op /= (Label "_finish")) withoutHeader
      rawSim = simulate' labels body 0 0 []
  in
  fixUpSimulated labels coms rawSim
