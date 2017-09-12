{-# LANGUAGE OverloadedStrings #-}
module Simulate where

import Language
import qualified Data.ByteString.Char8 as Char8


simulate' :: [Command] -> Int -> [Command] -> [Command]
simulate' [] _ output = output
simulate' (Com op size osize pos opos:coms) printLen output =
  if printLen < 0 then error "printLen went negative on simulation!" else
  let (out, printLen') =
        if printLen > 0 then
          ([Com op size osize 0 0], printLen - size)
        else
          case op of
            Print str final -> ([Com (Data $ Char8.pack str) (length str) 0 0 0], 0)
            PrintLen len final -> ([], len)
            _ -> ([], 0)
  in simulate' coms printLen' (output ++ out)


simulate :: [Command] -> [Command]
simulate coms =
  let withoutHeader = dropWhile (\ (Com op _ _ _ _) -> op /= (Label "_start")) coms
      body = takeWhile (\ (Com op _ _ _ _) -> op /= (Label "_finish")) withoutHeader
  in simulate' body 0 []
