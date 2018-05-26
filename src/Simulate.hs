{-# LANGUAGE OverloadedStrings #-}
module Simulate where

import Language
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import Data.List (mapAccumL, nub, find)
import Debug.Trace (trace)
import Data.Maybe (fromJust, isJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM)
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

extractBytes :: LabelMap -> [ComByte] -> [(Address, Address)] -> Int -> Int -> B.ByteString
extractBytes labels prog ranges from to =
  let suffix = dropWhile (\ (Com _ _ _ _ pos _, _) -> pos < from) prog
      range = takeWhile (\ (Com _ _ _ _ pos _, _) -> pos < to) suffix
      compacted = foldl Char8.append Char8.empty $ reverse $ map (\(com, bytes) ->
        case bytes of
          Bytes b -> b
          BZero _ -> error "Zero left in bytes on simulation"
        ) range
  in
  compacted

annotateZeros :: LabelMap -> [ComByte] -> [(Op, [(Int, B.ByteString)])]
annotateZeros labels coms =
  let zerocoms = filter (\(com, bytes) -> case com ^. op of Zero _ -> True; _ -> False) coms
      zeros = nub $ map (\(com, bytes) -> com ^. op) zerocoms
      annotatedZeros = map (\(Zero ranges) -> (Zero ranges, map (\(start, end) -> (start @! labels, extractBytes labels coms ranges (start @! labels) (end @! labels))) ranges)) zeros
  in
    annotatedZeros

correlateSomeZeros :: [(Op, [(Int, B.ByteString)])] -> [(Op, [(Int, B.ByteString)])] -> [[(Int, Int)]]-> Map Op Op
correlateSomeZeros comZeros simZeros zeroedRanges =
  let
    containsRange :: [(Int, B.ByteString)] -> (Int, Int) -> Bool
    containsRange ranges (start, finish) =
      any (\(s, bs) -> s < start && finish < s + B.length bs) ranges
    containsRanges :: [(Int, B.ByteString)] -> [(Int, Int)]  -> Bool
    containsRanges outer inner =
      all (containsRange outer) inner
    -- Note that this is m*n, when it should be m+n with sorted inputs.  It might matter later?
    zeroBytes :: [[(Int, Int)]] -> (Int, B.ByteString) -> (Int, B.ByteString)
    zeroBytes zranges (start, bytes) =
      let bytes' = foldl (\ bytes (s, f) ->
              if start < s && f < start + B.length bytes then
                let prefix = B.take (s - start) bytes
                    suffix = B.drop (f - start) bytes
                    middle = B.replicate (f - s) 0
                    result = B.concat [prefix, middle, suffix]
                in
                trace ("Zeroing bytes " ++ show (s, f) ++ " in\n" ++ show (start, bytes) ++ "\nto\n" ++ show result)
                result
              else
                bytes
            ) bytes (concat zranges)
      in
      trace ("Zeroing " ++ show zranges ++ " in " ++ show (start, bytes))
      (start, bytes')
    zeroAlreadyZeroed :: [(Int, B.ByteString)]  -> [(Int, B.ByteString)]
    zeroAlreadyZeroed ranges =
      let zranges = filter (containsRanges ranges) zeroedRanges
      in map (zeroBytes zranges) ranges
    matchZeros :: [(Int, B.ByteString)] -> [(Int, B.ByteString)] -> Bool
    matchZeros bytes simbytes = case (bytes, simbytes) of
      ([], []) -> True
      ((start, _):_, (start', _):_) -> trace ("Comparing\n" ++ show bytes ++ "\nwith\n" ++ show simbytes )all (\((s, b), (s', b')) -> s - start == s' - start' && b == b') (zip bytes simbytes)
      _ -> False
    findMatch (zero, bytes) = liftM (\p -> (fst p, zero)) $ find (\(simzero, simbytes) -> matchZeros (zeroAlreadyZeroed bytes) (zeroAlreadyZeroed simbytes)) simZeros
  in
  Map.fromList $ map fromJust $ filter isJust $ map findMatch comZeros

zeroZeroedRanges :: [(Int, Int)] -> (Op, [(Int, B.ByteString)]) -> (Op, [(Int, B.ByteString)])
zeroZeroedRanges zeroed (op, ranges) =
  (op, flip map ranges $ \(start, bytes) ->
    (start, bytes)
  )

correlateZeros :: LabelMap -> [(Op, [(Int, B.ByteString)])] -> [(Op, [(Int, B.ByteString)])] -> Map Op Op
correlateZeros labels comZeros simZeros =
  let correlateZeros' comZeros simZeros zeroedRanges =
        let zeroMap = correlateSomeZeros comZeros simZeros zeroedRanges
            zeroToInts (Zero ranges) = map (\(a,b) -> (a @! labels, b @! labels)) ranges
            ranges = map (\(sim, com) -> zeroToInts sim) $ Map.toList zeroMap
            simZeros' = filter (\(z, _) -> Map.notMember z zeroMap) simZeros
        in
        trace ("  ranges: " ++ show ranges ++ "; simZeros': " ++ show simZeros') $
        if simZeros == simZeros' then zeroMap else
        zeroMap `Map.union` correlateZeros' comZeros simZeros' (zeroedRanges ++ ranges)
  in
  correlateZeros' comZeros simZeros []


fixZeros :: LabelMap -> [ComByte] -> [ComByte] -> [ComByte]
fixZeros labels coms sims =
  let comZeros = annotateZeros labels coms
      simZeros = annotateZeros labels sims
      zeroMap = correlateZeros labels comZeros simZeros
      fixZero (com, bytes) = case com ^. op of
        Zero ranges -> case Map.lookup (Zero ranges) zeroMap of
          Nothing -> (com & op .~ Zero [], bytes)
          Just op' -> (com & op .~ op', bytes)
        _ -> (com, bytes)
  in
  trace ("Fixing zeros: \n" ++ unlines (map show comZeros)  ++ "  vs\n" ++ unlines (map show simZeros) ++ "\n  with map\n" ++ show zeroMap ++ "\n") map fixZero sims

fixUpSimulated :: (Map String Int) -> [ComByte] -> [ComByte] -> [ComByte]
fixUpSimulated labels coms sims =
  fixZeros labels coms $ fixOutPoses labels $ addLabels coms sims

simulate :: (Map String Int) -> [ComByte] -> [ComByte]
simulate labels coms =
  let withoutHeader = dropWhile (\ (Com op _ _ _ _ _, _) -> op /= (Label "_start")) coms
      body = takeWhile (\ (Com op _ _ _ _ _, _) -> op /= (Label "_finish")) withoutHeader
      rawSim = simulate' labels body 0 0 []
  in
  fixUpSimulated labels coms rawSim
