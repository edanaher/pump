module Zero where

import Debug.Trace (trace)
import Numeric (showHex)

import qualified Data.ByteString.Char8 as Char8
import Data.Word (Word64, Word32, Word8)
import Data.Digest.CRC32 (crc32, crc32Update)
import Data.Bits ((.&.), (.|.), xor, shift)
import qualified Data.ByteString as B
import qualified Data.Char as Char
import Data.List (unfoldr, find, (\\))
import Control.Lens ((^.))

import Language

-- TODO: move into util?
packInt :: Int -> Int -> B.ByteString
packInt n size =
  Char8.pack $ map Char.chr $ unfoldr (\ (n, size) ->
      if size == 0 then Nothing else Just (n `mod` 256, (n `div` 256, size - 1))) (n, size)

extractRange :: [(Command, ByteOp)] -> (Int, Int) -> [ByteOp]
extractRange prog (from, to) =
  let suffix = dropWhile (\(com, _) -> com ^. pos < from) prog
      range = takeWhile (\(com, _) -> com ^. pos < to) suffix
  in
  trace ("Extracted range " ++ show (from, to) ++ " to\n" ++ unlines (map show range)) map snd range

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



solveDeltas :: Command -> [Int] -> Int -> [Int]
solveDeltas com rows 1 = rows
solveDeltas com rows n =
  let pivot = case find (\d -> d >= n && d < 2 * n) rows of
        Just p -> p
        Nothing -> error $ "Failed to find pivot for " ++ show com ++ "at " ++ show n ++ ": rows are\n" ++ unwords (map (flip showHex "") rows)
      adjustRow row = if row .&. n == 0 || row == pivot then row else row `xor` pivot
      rows' = map adjustRow rows
  in {-trace ("pivot is " ++ showHex pivot " on " ++ showHex n "")-} solveDeltas com rows' (n `div` 2)

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

snd3 (a,b,c) = b
-- TODO: Handle dep loops and interdepending zeros
topSort :: [(Command, ByteOp, [ByteOp])] ->  [(Command, ByteOp, [ByteOp])]
topSort zeros =
  let dependencies :: [(Command, ByteOp, [ByteOp])] -> (Command, ByteOp, [ByteOp]) -> [(Command, ByteOp, [ByteOp])]
      dependencies zeros (_, _, bytes) =
       flip filter zeros $ \(_, zero', _) -> flip any bytes $ \bytes -> bytes == zero'
      topSortFrom :: [(Command, ByteOp, [ByteOp])] -> (Command, ByteOp, [ByteOp]) -> [(Command, ByteOp, [ByteOp])]
      topSortFrom unvisited cur =
        let deps = dependencies unvisited cur in
        concatMap (topSortFrom $ unvisited \\ deps) deps ++ [cur]
      topSort' [] = []
      topSort' (h:r) =
        let partial = topSortFrom r h in
        partial ++ topSort' (r \\ partial)


  in trace ("dependencies:\n" ++ unlines (map (\(a,b,c) -> show (b, map snd3 $ dependencies zeros (a,b,c))) zeros)) topSort' zeros


encode :: Command -> [(Int, Int)] -> [(Command, ByteOp)] -> B.ByteString
encode com ranges program =
  let --com = Com (Label "") (LuaSrc0 0 0 0
      bytes = concatMap (extractRange program) ranges
      baseCrc = (fromIntegral $ crcForZero (Char8.replicate 4 '\000') bytes)
      crcDeltas = [ (crcForZeroAtBit i bytes `xor` baseCrc) | i <- [0..31] ]
      transposeN i = fromIntegral $ foldl (\n d -> 2*n + (d `shift` (-i) .&. 1)) 0 crcDeltas
      crcDeltas' = [ (transposeN i) `shift` 1 + (baseCrc `shift` (-i) .&. 1) | i <- [0..31] ]
      brute = bruteForce 0 bytes
      solvedDeltas = {--trace ("Brute is" ++ showHex brute "")--} solveDeltas com crcDeltas' (1 `shift` 32)
      ones = filter (\n -> n .&. 1 == 1)  solvedDeltas
      result = fromIntegral $ flipBits 0 $ (foldl (.|.) 0 $ ones) `shift` (-1)
  in
  --trace ("basecrc is " ++ showHex baseCrc "") $
  --trace ("deltas are " ++ (unwords $ map (\w -> showHex w "")  crcDeltas)) $
  --trace ("transposed deltas are " ++ (unwords $ map (\w -> showHex w "")  crcDeltas')) $
  --trace ("Solved deltas are " ++ (unwords $ map (\w -> showHex w "") solvedDeltas))
  --trace ("result is " ++ showHex result "")
  --trace ("brute is " ++ showHex brute "")
  --trace ("crc with zero is " ++ showHex (fromIntegral $ crcForZero (packInt brute 4) bytes) "") $
  packInt result 4

fix :: [(Command, ByteOp)] -> [(Command, ByteOp)]
fix program =
  let zeros = filter (\ (_, b) -> case b of BZero _ -> True; _ -> False) program
      zerosWithBytes = map (\(com, BZero ranges) -> (com, BZero ranges, concatMap (extractRange program) ranges)) zeros
      sorted = trace ("Zeros with bytes\n" ++ unlines (map show zerosWithBytes)) map (\(a,b,c) -> (a,b)) $ topSort zerosWithBytes
  in trace ("ZEROS:" ++ show sorted) $ foldl (\ prog (com, BZero ranges) ->
      let zbytes = Zero.encode com ranges prog
      in
      flip map prog $ \(com, bytes) ->
      if bytes == BZero ranges
      then (com, Bytes zbytes)
      else (com, bytes)) program sorted

