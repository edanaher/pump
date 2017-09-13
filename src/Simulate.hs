{-# LANGUAGE OverloadedStrings #-}
module Simulate where

import Language
import qualified Data.ByteString.Char8 as Char8
import Debug.Trace (trace)


simulateRep :: Int -> Int -> Int -> Int -> [Command] -> ([Command], Int, Int, Int)
simulateRep from len pos opos coms =
  let start = dropWhile (\ (Com _ _ _ pos _) -> pos < from) coms
      base = takeWhile (\ (Com _ _ _ pos _) -> pos < from + len) start
      (coms', pos', opos') = foldl (\ (coms, pos, opos) (Com op size osize _ _) ->
          (Com op size osize pos opos:coms, pos + size, opos + osize)) ([], pos, opos) base
  in
  --trace ("--- Produced from " ++ show from ++ "/" ++ show len ++ " => " ++ show pos ++ "," ++ show opos ++ ":\n" ++ (unlines $ map show $ reverse coms') ++ "--------" ) $
  (reverse coms', 0, pos', opos')

simulate' :: [Command] -> Int -> Int -> Int -> [Command] -> [Command]
simulate' [] _ _ _ output = output
simulate' (Com op size osize pos opos:coms) printLen simpos simopos output =
  if printLen < 0 then error "printLen went negative on simulation!" else
  let (out, printLen', simpos', simopos') =
        if printLen > 0 then
          --trace ("Eating " ++ show op ++ " at " ++ show pos ++ "/" ++ show opos) $
          ([Com op size osize simpos simopos], printLen - size, simpos + size, simopos + size)
        else
          --trace ("simulating " ++ show op ++ " at " ++ show simpos ++ "/" ++ show simopos) $
          case op of
            Print str final -> ([Com (Data $ Char8.pack str) (length str) 0 simpos simopos], 0, simpos + length str, simopos)
            PrintLen len final -> ([], len, simpos, simopos)
            Rep from len at final -> simulateRep from len simpos simopos output
            _ -> ([], 0, simpos, simopos)
  in simulate' coms printLen' simpos' simopos' (output ++ out)


simulate :: [Command] -> [Command]
simulate coms =
  let withoutHeader = dropWhile (\ (Com op _ _ _ _) -> op /= (Label "_start")) coms
      body = takeWhile (\ (Com op _ _ _ _) -> op /= (Label "_finish")) withoutHeader
  in simulate' body 0 0 0 []
