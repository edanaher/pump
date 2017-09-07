{-# LANGUAGE OverloadedStrings #-}
module Rep where

import qualified Data.ByteString as B
import Data.Bits (shiftR, (.&.))
import Debug.Trace (trace)

toBits :: Int -> Int -> [Int]
toBits len n = [(n `shiftR` i) .&. 1 | i <- [0..len-1] ]

toBitsR len n = [(n `shiftR` i) .&. 1 | i <- reverse [0..len-1] ]

lg 0 = -1
lg 1 = 0
lg n = 1 + lg (n `div` 2)

unlg2 k | k < 0 = 0
unlg2 0 = 2
unlg2 1 = 3
unlg2 n = 2 * unlg2 (n - 2)

lg4 k | k < 4 = -10000
lg4 k | 4 <=k && k <= 7 = k - 4
lg4 k = 4 + lg4 (k `div` 2)

unlg4 k | k < 0 = 0
unlg4 k | 0 <= k && k <= 3 = k + 4
unlg4 k = 2 * unlg4 (k - 4)

distCode k | k < 4 = k
distCode k | k >= 4 = 2 + distCode (k `div` 2)

unDistCode n | n < 4 = n
unDistCode n | n >= 4 = 2 * unDistCode (n - 2)

packBits :: [Int] -> B.ByteString
packBits bits =
  let getByte bits = fromIntegral $ foldl (\n b -> 2*n + b) 0 bits
      getBytes [] = []
      getBytes bits = getByte (reverse $ take 8 bits) : getBytes (drop 8 bits)
  in B.pack $ getBytes bits

bitsForLen l =
  if l < 11 then toBitsR 7 (1 + (l - 3)) else
  let base = if l == 258 then toBitsR 8 $ 0xC5 else
             if l < 115  then toBitsR 7 $ lg4 (l - 3) + 5
                         else toBitsR 8 $ 0xC0 + lg4 (l - 3) - 19
      extra = if l == 258 then [] else
              toBits (lg (l - 3) - 2) ((l - 3) - unlg4 (lg4 (l - 3)))
  in base ++ extra

bitsForDist d =
  if d < 5 then toBitsR 5 (d - 1) else
  let base = toBitsR 5 $ distCode $ d - 1
      extra = toBits (lg (d - 1) - 1) $ (d - 1) - unDistCode (distCode $ d - 1)
  in base ++ extra


bits :: Int -> Int -> [Int]
bits dist len =
  [ 0, 1, 0 ] ++ bitsForLen len ++ bitsForDist dist ++ [ 0, 0, 0, 0, 0, 0, 0 ] ++ [1, 0, 0]

encode from len at =
  let dist = if from < 0 then -from else at - from
      reps = packBits $ bits dist len in
  reps `B.append` B.pack [0, 0, 255, 255]
