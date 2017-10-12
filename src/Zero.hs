module Zero where

import Debug.Trace (trace)
import Numeric (showHex)

import qualified Data.ByteString.Char8 as Char8
import Data.Word (Word64, Word32, Word8)
import Data.Digest.CRC32 (crc32, crc32Update)
import Data.Bits ((.&.), (.|.), xor, shift)
import qualified Data.ByteString as B
import qualified Data.Char as Char
import Data.List (unfoldr, find)

import Language

-- TODO: move into util?
packInt :: Int -> Int -> B.ByteString
packInt n size =
  Char8.pack $ map Char.chr $ unfoldr (\ (n, size) ->
      if size == 0 then Nothing else Just (n `mod` 256, (n `div` 256, size - 1))) (n, size)


crc :: [(Command, ByteOp)] -> Word32
crc program = foldl (\ c (com, bytes) ->
    case bytes of Bytes b -> crc32Update c b
                  BZero _ -> crc32Update c (Char8.pack "Zero"))
    (crc32 ([] :: [Word8])) program

crcForZero :: B.ByteString -> [ByteOp] -> Word32
crcForZero zeroSub bytes =
  foldl (\ c bytes -> case bytes of
    Bytes b -> crc32Update c b
    BZero _ -> crc32Update c zeroSub) 0 bytes

crcForZeroAtBit :: Int -> [ByteOp] -> Int
crcForZeroAtBit bit bytes =
  fromIntegral $ crcForZero (packInt (shift 1 bit) 4) bytes



solveDeltas :: [Int] -> Int -> [Int]
solveDeltas rows 1 = rows
solveDeltas rows n =
  let pivot = case find (\d -> d >= n && d <= 2 * n) rows of
        Just p -> p
        Nothing -> error $ "Failed to find pivot in " ++ unwords (map (flip showHex "") rows)
      adjustRow row = if row .&. n == 0 || row == pivot then row else row `xor` pivot
      rows' = map adjustRow rows
  in {-trace ("pivot is " ++ show pivot ++ " on " ++ show n)-} solveDeltas rows' (n `div` 2)

bruteForce :: Int -> [ByteOp] -> Int
bruteForce n bytes =
  (if n `mod` 10000000 == 0 then trace (show (n `div` 1000000) ++ "M => " ++ showHex (crcForZero (packInt n 4) bytes) "") else id) (
  if crcForZero (packInt n 4) bytes == 0 then
    n
  else
    bruteForce (n + 1) bytes)

flipBits n' n  =
  if n == 0 then n' else
  flipBits (n' * 2 + (n .&. 1)) (n `div` 2)

encode :: [(Int, Int)] -> [(Command, ByteOp)] -> B.ByteString
encode ranges program =
  let --com = Com (Label "") (LuaSrc0 0 0 0
      bytes = map snd program
      baseCrc = (fromIntegral $ crcForZero (Char8.replicate 4 '\000') bytes)
      crcDeltas = [ (crcForZeroAtBit i bytes `xor` baseCrc) | i <- [0..31] ]
      transposeN i = fromIntegral $ foldl (\n d -> 2*n + (d `shift` (-i) .&. 1)) 0 crcDeltas
      crcDeltas' = [ (transposeN i) `shift` 1 + (baseCrc `shift` (-i) .&. 1) | i <- [0..31] ]
      solvedDeltas = solveDeltas crcDeltas' (1 `shift` 32)
      ones = filter (\n -> n .&. 1 == 1)  solvedDeltas
      result = fromIntegral $ flipBits 0 $ (foldl (.|.) 0 $ ones) `shift` (-1)
      brute = bruteForce 0 bytes
  in
  --trace ("basecrc is " ++ showHex baseCrc "") $
  --trace ("deltas are " ++ (unwords $ map (\w -> showHex w "")  crcDeltas)) $
  --trace ("transposed deltas are " ++ (unwords $ map (\w -> showHex w "")  crcDeltas')) $
  --trace ("Solved deltas are " ++ (unwords $ map (\w -> showHex w "") solvedDeltas))
  --trace ("result is " ++ showHex result "")
  --trace ("brute is " ++ showHex brute "")
  --trace ("crc with zero is " ++ showHex (fromIntegral $ crcForZero (packInt brute 4) bytes) "") $
  packInt result 4
