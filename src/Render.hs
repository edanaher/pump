module Render where

import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as Char8
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import Text.Printf (printf)
import Debug.Trace (trace)

import Control.Lens ((^.), (&), (.~))

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
    -- TODO: Until simulating  fixes zeros properly, the args aren't completely meaningful :(
    in "zero " ++ args
  DataInt value size ->
    let args = renderArgs $ Map.fromList $ [("int", LAddress value), ("size", LInt size)]
    in "data " ++ args
  Copy from len ->
    let args = renderArgs $ Map.fromList $ [("from", LAddress from), ("len", LAddress len)]
    in "data " ++ args
  _ -> show $ com ^. op


render :: [String] -> ComByte -> String
render srcs (com@(Com op src size osize pos opos), bytes) =
  let origsrc = case src of
        SrcLua (f, n) -> Just $ srcs !! (n - 1)
        _ -> Nothing
      bytes' = case bytes of
        BZero _ -> Char8.pack "Zero"
        Bytes b -> b
  in
  -- TODO: actually use the origsrc.  It breaks weirdly right now.
  --printf "--[[%3d=>%3d +%2d=>%2d]] %s -- %s" pos opos size osize (renderCom {-origsrc-}Nothing com) (show origsrc)
  printf "--[[%3d=>%3d +%2d=>%2d]] %s -- %s" pos opos size osize (renderCom {-origsrc-}Nothing com) (show $ LBytes bytes')

alignFields labels (input, ibytes) (output, obytes) =
  case (input ^. op, output ^. op) of
    (Rep _ _ iat _ ifinal, Rep _ _ oat _ ofinal) ->
      let at' = case (iat, oat) of
            (Just a, Nothing) | a @! labels == input ^. opos  -> iat
            _ -> iat
      in (output & ((op . at) .~ at'), obytes)

    _ -> (output, obytes)

checkCopy [] _ _ _ = []
checkCopy (h:r) copysrc start n = case h ^. src of
  SrcCopy copysrc' i count | copysrc' == copysrc && i == n ->
    if i == count - 1
    then [(copysrc, start)]
    else checkCopy r copysrc start (n + 1)
  _ -> []

collapseCopies :: LabelMap -> [ComByte] -> [ComByte] -> ([ComByte], [ComByte])
collapseCopies labels inputs outputs =
  let copies = map ((^. src) . fst) $ filter (\(com, _) -> case com ^. src of SrcCopy _ _ _ -> True; _ -> False) inputs
      copyIndices = nub $ map (\ (SrcCopy (SrcedOp (Copy from len, _)) _ _) -> (from, len)) copies
      dropLabels = filter (\(com, _) -> case com ^. op of Label _ -> False; _ -> True)
      copyCandidates :: [((Address, Address), [ComByte])]
      copyCandidates = concatMap (\ (start, len) ->
          let suffix = dropWhile (\(com, _) -> com ^. pos /= start @!labels) outputs
              range = takeWhile (\(com, _) -> com ^. pos <= start @! labels + len @! labels) suffix
          in if range == [] || (fst (last range) ^. pos /= start @! labels + len @! labels)
            then []
            else [((start, len), dropLabels (init range))]) copyIndices
      startsCopy (SrcCopy _ 0 _) = True
      startsCopy _ = False
      getSrc (SrcCopy (SrcedOp (op, _)) _ _) = op
      prefixMatch _ [] = True
      prefixMatch [] _ = False
      prefixMatch ((c,cb):cs) ((t,tb):ts) = cb == tb && prefixMatch cs ts
      substCopies input output = case (input, output) of
        ((i,ib):input', (o,ob):output') ->
          if startsCopy (i ^. src) then
            let copies = filter (prefixMatch output . snd) copyCandidates in
            case copies of
              [] -> let (input'', output'') = substCopies input' output'
                    in ((i,ib):input'', (o,ob):output'')
              (((start, len), copy):_) ->
                   let (input'', output'') = substCopies (drop (length copy) input) (drop (length copy) output)
                   in ((Com (getSrc $ i ^. src) SrcNone 0 0 0 0, Bytes (Char8.pack "")):input'',
                       (Com (Copy start len) SrcNone 0 0 0 0, Bytes (Char8.pack "")):output'')

          else
            let (input'', output'') = substCopies input' output'
            in ((i,ib):input'', (o,ob):output'')
        _ -> (input, output)
--  in trace ("copies are \n" ++ unlines (map (\(src, lines) -> show src ++ " => \n" ++ unlines (map show lines) ++ "\n\n") copies)) outputs
  in {--trace ("Copy indices: " ++ show copyIndices ++ " => candidates " ++ unlines (map (\((start, len), coms) -> show start ++ "+" ++ show len ++ "\n" ++ unlines (map show coms) ++ "\n" ) copyCandidates)) --} substCopies inputs outputs

cleanup :: LabelMap -> [ComByte] -> [ComByte] -> ([ComByte], [ComByte])
cleanup labels inputs outputs =
  let paired = zip (inputs ++ repeat (Com (Padding 0) SrcNone 0 0 0 0, Bytes (Char8.pack ""))) outputs -- This should be smarter
      cleaned = map (uncurry $ alignFields labels) paired
      decopied = collapseCopies labels inputs cleaned
  in
  decopied

renderProgram :: [String] -> LabelMap -> [ComByte] -> [ComByte] -> Bool -> ([String], [String])
renderProgram srcs labels inputs outputs rawsim =
  let (inputs', outputs') = if rawsim then (inputs, outputs) else cleanup labels inputs outputs
  in (map (render srcs) inputs', map (render srcs) outputs')
