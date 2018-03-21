{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module Pump where

import qualified Data.Char as Char
import Data.List (intercalate, mapAccumL, unfoldr)
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import System.Directory (makeAbsolute)
import System.Environment (getArgs, getExecutablePath)
import Data.Bits (shiftR, (.&.))
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Control.Lens ((^.), (^?), view)

import Language
import qualified Rep
import qualified Simulate
import qualified Zero
import qualified Render

import qualified Foreign.Lua as Lua

debug s v =
  trace s v
-- return ()

type LuaSources = (String, String, String, String)

dirpath :: String -> String
dirpath = reverse . dropWhile (/= '/') . reverse

dslPath :: IO FilePath
dslPath = do
  exePath <- getExecutablePath
  makeAbsolute $ (dirpath exePath) ++ "/../lua/dsl.lua"

showProg :: [Command] -> String
showProg coms = unlines $ map show coms

packInt :: Int -> Int -> B.ByteString
packInt n size =
  Char8.pack $ map Char.chr $ unfoldr (\ (n, size) ->
      if size == 0 then Nothing else Just (n `mod` 256, (n `div` 256, size - 1))) (n, size)

instance Lua.FromLuaStack Address where
  peek idx = do
    immediate <- Lua.tointegerx (-1)
    case immediate of
      Just n -> return $ AddrI $ fromIntegral n
      Nothing -> return $ AddrL "SOMTHING"

instance Lua.FromLuaStack SrcedOp where
  peek idx = do
    srcInfo <- getLuaSrc
    tp <- getField Lua.peek "type" `Lua.catchLuaError` (\ e -> Lua.throwLuaError $ "Error reading type at " ++ (printLuaSrc srcInfo) ++ ": " ++ show e) :: Lua.Lua String
    wrapSrc (SrcLua srcInfo) $ case tp of
      "rep" -> do
        from <- AddrI . fromInteger . toInteger <$> getField Lua.tointeger "from"
        len <- AddrI . fromInteger . toInteger <$> getField Lua.tointeger "len"
        at <- getFieldOpt (liftM (AddrI . fromInteger . toInteger) . Lua.tointeger) "at"
        size <- getFieldOpt (liftM fromInteger . liftM toInteger . Lua.tointeger) "size"
        final <- getFieldBool "final"
        return $ Rep from len at size final
      "print" -> do
        final <- getFieldBool "final"
        str <- getFieldOpt Lua.peek "string"
        case str of
          Just s -> return $ Print s final
          Nothing -> do
            len <- getField Lua.tointeger "len"
            return $ PrintLen (AddrI $ fromInteger $ toInteger len) final
      "copy" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        return $ Copy (AddrI $ fromInteger $ toInteger from) (AddrI $ fromInteger $ toInteger len)
      "data" -> do
        str <- getFieldOpt Lua.tostring "string"
        case str of
          Just s -> return $ Data s
          Nothing -> do
            n <- getField Lua.tointeger "int"
            size <- getField Lua.tointeger "size"
            return $ Data $ packInt (fromInteger $ toInteger n) (fromInteger $ toInteger size)
      "zero" -> do
        ranges <- getField Lua.peek "ranges" :: Lua.Lua [(Lua.LuaInteger, Lua.LuaInteger)]
        return $ Zero $ map (\(a, b) -> (AddrI (fromInteger $ toInteger a), AddrI (fromInteger $ toInteger b))) ranges
      "label" -> do
        str <- getField Lua.peek "name"
        return $ Label str
    where getLuaSrc = do
            file <- getFieldOpt Lua.peek "_file"
            line <- getFieldOpt Lua.tointeger "_line"
            case (file, line) of
              (Just file, Just line) -> return $ (file, fromIntegral line)
              _ -> return ("[Unknown source]", -1)
          printLuaSrc (file, line) = file ++ ":" ++ (show line)
          wrapSrc src op = op >>= \op' -> return $ SrcedOp (op', src)
          getField peek f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
            isnil <- Lua.isnil (-1)
            v <- peek (-1)
            Lua.remove (-1)
            return v
          getFieldOpt peek f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
            isnil <- Lua.isnil (-1)
            if isnil then Lua.remove (-1) >> return Nothing else do
              v <- peek (-1)
              Lua.remove (-1)
              return $ Just v
          getFieldBool f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
            isnil <- Lua.isnil (-1)
            if isnil then Lua.remove (-1) >> return False else do
              v <- Lua.peek (-1)
              Lua.remove (-1)
              return $ v

loadStringErr :: String -> String -> Lua.Lua (Either String ())
loadStringErr filename source = do
  status <- Lua.loadstring source
  if status == Lua.OK then do
    status <- Lua.call 0 0
    return (Right ())
  else do
    err <- Lua.peek (-1)
    return $ Left $ "Error loading source from '" ++ filename ++ "':" ++ err

readProgram :: LuaSources -> Maybe (Map String Int) -> IO (Either String [SrcedOp])
readProgram (filename, source, dslFile, dslSource) labels =
  --debug ("Reading program with labels " ++ show labels)
  Lua.runLua $ do
    Lua.openlibs
    r <- loadStringErr dslFile dslSource
    case r of
      Left err -> return $ Left err
      _a -> do
        r <- loadStringErr filename source
        case r of
          Left err -> return $ Left err
          _ -> (Lua.getglobal "errors" *> Lua.peekEither (-1) :: Lua.Lua (Either String [DslErr])) >>= \p -> case p of
            Left err -> return $ Left err
            Right errs -> if errs /= [] then return $ Left $ "Errors from dsl: \n" ++ unlines (map ((++) "    " . show) errs) else
              Lua.getglobal "program" *> Lua.peekEither (-1) >>= \p -> case p of
              Left err -> return $ Left err
              Right p -> return $ Right $ map (\ (SrcedOp (op, SrcLua (f, l))) -> SrcedOp (op, SrcLua (filename, l))) p

initSizes :: [SrcedOp] -> [Command]
initSizes = snd . mapAccumL (\(pos, opos) (SrcedOp (op, src)) ->
    let (size, osize) = case op of
          Rep _ len _ (Just size) _ -> (size, 99) -- Sketchy...
          Rep _ len _ Nothing _ -> (1, 99)
          Print str _ -> (length str + 5, length str)
          PrintLen len _ -> (5, 99)
          Data str -> (B.length str, 0)
          Copy _ len -> (99, 10)
          Zero _ -> (4, 0)
          Label _ -> (0, 0)
    in ((pos + size, opos + osize), (Com op src size osize (pos + size) (opos + osize)))) (0, 0)

getLabels :: [Command] -> Map String Int
getLabels =
  Map.fromList .
  map (\ com -> (com ^. op . label, com ^. pos)) .
  filter (\ com -> case com ^. op of
              Label str -> True
              _ -> False)

expandCopy :: LabelMap -> [Command] -> SrcedOp -> [SrcedOp]
expandCopy labels prog sop@(SrcedOp (Copy from len, _)) =
  let suffix = dropWhile (\(Com _ _ _ _ pos _) -> pos < labels !!! from) prog
      (Com _ _ _ _ _ startOpos) = head suffix
      coms = takeWhile (\(Com _ _ _ _ pos _) -> pos < evalAddr labels from + evalAddr labels len) suffix
      comsWithoutLabels = filter (\(Com op _ _ _ _ _) -> case op of Label _ -> False ; _ -> True) coms

  in trace ("Copy: " ++ show from ++ "," ++ show len ++ " => " ++ (unlines $ map show comsWithoutLabels))
     zipWith (\i (Com op src _ _ _ _) -> SrcedOp (op, SrcCopy sop i (length comsWithoutLabels))) [0..] comsWithoutLabels

expandCopies :: LabelMap -> [Command] -> [SrcedOp] -> [SrcedOp]
expandCopies labels prog ops =
  debug ("Expanding copies from \n" ++ (unlines $ map show ops) ++ "\n")
  concatMap (\ sop@(SrcedOp (op, src)) -> case op of
        Copy from len -> expandCopy labels prog sop
        _ -> [sop]
      ) ops

alignedZip :: [SrcedOp] -> [Command] -> [(SrcedOp, Command)]
alignedZip [] [] = []
alignedZip sops [] = map (\sop -> (sop, Com (Padding 0) SrcNone 0 0 0 0)) sops
alignedZip (sop@(SrcedOp (op, src)):sops) (com@(Com _ src' _ _ _ _):coms) =
  if src == src' then (sop, com):alignedZip sops coms else (sop, com):alignedZip sops coms



updateSizes :: Map String Int -> [Command] -> [SrcedOp] -> ([Command], Map String Int)
updateSizes labels prog ops = do
  _ <- trace ("Updating sizes starting from " ++ (unlines $ map show ops)) $ return ()
  (prog', labels', pos, opos, eatlen, eatfrom) <- return $ foldl (\ (prog', labels, pos, opos, eatlen, eatfrom) (SrcedOp (op, src), com) ->
        let opsize = toSize labels prog op opos
            labels' = case op of Label str -> Map.insert str pos labels
                                 _ -> labels
            pos' = pos + opsize
            osize' = if opos < 0 || eatlen > 0 then 0 else Simulate.outputSize labels op
            opos' = if opos == -1 then
                      case op of Label "_start" -> 0
                                 _ -> -1
                    else opos + osize'
            eatlen' = if eatlen > 0 then eatlen - (com ^. size) else
                      case op of PrintLen len _ -> evalAddr labels len
                                 _ -> 0
            eatfrom' = if eatlen > 0 then eatfrom else
                       case op of PrintLen len _ -> com
                                  _ -> eatfrom
        in
          --trace ("Eatlen is " ++ show eatlen ++ " => " ++ show eatlen' ++ "; osize'/opos' are " ++ show osize' ++ "/" ++ show opos' ++ " on + " ++ show com) $
          if eatlen' < 0 then error ("Eatlen went negative!\n" ++ "  Eating from " ++ show eatfrom ++ "\n  had " ++ show eatlen ++ " at " ++ show com) else
          ((Com op src opsize osize' pos opos):prog', labels', pos', opos', eatlen', eatfrom'))
        ([], Map.empty, 0, -1, 0, head prog) $ trace ((++) "Aligned zip:\n" $ unlines $ map show $ alignedZip ops prog) alignedZip ops prog
  trace ("Updated sizes: \n" ++ showProg (reverse prog')) $ (reverse prog', labels')

fixSizes :: Map String Int -> [Command] -> LuaSources -> IO [Command]
fixSizes labels prog luaSources = do
  _ <- trace ("Fixing sizes on:\n" ++ showProg prog) $ return 0
  ops' <- readProgram luaSources (Just labels) >>= \o -> case o of
            Right r -> return r
            Left err -> error err
  opsCopied <- return $ expandCopies labels prog ops'
  _ <- debug ("Copied:\n" ++ (unlines $ map show opsCopied) ++ "\n") $ return ()
  (prog', labels') <- return $ updateSizes labels prog opsCopied
  if prog == prog'
  then return prog
  else fixSizes labels' prog' luaSources


{- It's a bit more complicated than this; e.g.,
 -   print "ab"; print "cd"; rep 0 4; rep 4 2
 - is fine because the second rep only includes part of the first rep, but the
 - entire print.  Hopefully this won't happen in real programs. -}
isAligned :: [Command] -> Int -> Bool
isAligned coms target =
  target == 0 || any ((== target) . view opos) coms

printSrc = show . view src

checkCom :: LabelMap -> [Command] -> Command -> [String]
checkCom labels coms com = case com ^. op of
  Label str ->
    let matches = filter (((==) $ com ^. op) . view op) coms
        lines = map printSrc matches
    in if head matches /= com then ["Duplicate label: " ++ show str ++ " at:\n" ++ unlines (map ("    " ++) lines)] else []
  Rep from' len at' rsize final ->
    let at = case at' of Nothing -> com ^. opos; Just at -> evalAddr labels at
        from = if evalAddr labels from' >= 0 then evalAddr labels from' else at + from
        {-badAt = if isAligned coms from then [] else
                ["Misaligned rep starting at " ++ show from ++ " on " ++ printSrc com ++ ":\n    " ++ show com]
        badFrom = if isAligned coms (from + len) then [] else
                ["Misaligned rep ending at " ++ show (from + len) ++ " on " ++ printSrc com ++ ":\n    " ++ show com] -}
        -- this doesn't work...
        --badSize = if rsize == Nothing || rsize == Just (com ^. size) then [] else
        --        ["Wrong size on " ++ printSrc com ++ ":\n    " ++ show com]
        badFuture = if from < at then [] else
                ["Future rep on " ++ printSrc com ++ ":\n    " ++ show com]
        badShort = if labels !!! len >= 3 then [] else
                ["Short rep of " ++ show len ++ " on " ++ printSrc com ++ ":\n    " ++ show com]
    in
    {-badAt ++ badFrom ++-} badFuture ++ badShort-- ++ badSize
  _ -> []

checkStartLabel coms =
  if any (\(Com op _ _ _ _  _)-> case op of Label "_start" -> True; _ -> False) coms then [] else
  ["Missing \"_start\" label"]

sanityCheck :: LabelMap -> [Command] -> [String]
sanityCheck labels coms =
  checkStartLabel coms ++
  concatMap (checkCom labels coms) coms


intToBytes :: Int -> Int -> B.ByteString
intToBytes len n = B.pack [fromIntegral $ (n `shiftR` (8*i)) .&. 255 | i <- [0..len-1] ]

posToOpos :: [Command] ->  Int -> Int
posToOpos [] target = error "Ran out of command searching for pos"
posToOpos _ 0 = 0
posToOpos (Com op src size osize pos opos:coms') target =
    if target < pos then opos else --error $ "Converting pos " ++ show target ++ " to opos isn't a boundary" else
    if target == pos then opos else
    posToOpos coms' target

toBytes :: LabelMap -> [Command] -> Op -> Int -> B.ByteString
toBytes labels coms op opos = case op of
  Data str -> str
  Zero ranges -> Char8.pack "ZERO"
  Print str final -> (if final then "\001" else "\000") `Char8.append`
                     (intToBytes 2 $ length str) `Char8.append`
                     (intToBytes 2 $ 65535 - length str) `Char8.append`
                     Char8.pack str
  PrintLen len final -> (if final then "\001" else "\000") `Char8.append`
                          (intToBytes 2 $ evalAddr labels len) `Char8.append`
                          (intToBytes 2 $ 65535 - (evalAddr labels len))
  Rep from len (Just at) _ final -> Rep.encode (evalAddr labels from) (evalAddr labels len) (posToOpos coms (evalAddr labels at)) final
  Rep from len Nothing _ final -> Rep.encode (evalAddr labels from) (evalAddr labels len) opos final
  Label _ -> B.empty
  Copy from len -> B.empty
  _ -> error $ "Converting unknown op to bytes:\n  " ++ show op

toSize :: LabelMap -> [Command] -> Op -> Int -> Int
toSize labels coms op opos = case op of
  Rep _ _ _ (Just size) _ -> size
  Rep from len (Just at) Nothing final -> Rep.size (evalAddr labels from) (evalAddr labels len) (posToOpos coms (evalAddr labels at)) final
  Rep from len Nothing Nothing final -> Rep.size (evalAddr labels from) (evalAddr labels len) opos final
  _ -> B.length $ toBytes labels coms op opos

writeGzip filename bytes =
  B.writeFile filename $ foldl B.append B.empty $ map (\ (com, Bytes b) -> b) bytes

progToBytes :: (Map String Int) -> [Command] -> [(Command, ByteOp)]
progToBytes labels program =
  map (\ com@(Com op src size osize pos opos) -> case op of
            Zero ranges -> (com, BZero (map ( \(a, b) -> (evalAddr labels a, evalAddr labels b)) ranges))
            _ -> (com, Bytes $ toBytes labels program op opos)) program

fixZeros :: [(Command, ByteOp)] -> [(Command, ByteOp)]
fixZeros program =
  let (BZero ranges) = head $ map snd $ filter (\ (_, b) -> case b of BZero _ -> True; _ -> False) program
      zero = Zero.encode ranges program
  in trace ("ZEROS:" ++ show zero) $ map (\ (com, bytes) ->
      case bytes of Bytes b -> (com, bytes)
                    BZero _ -> (com, Bytes zero)) program

compile :: IO ()
compile = do
  [ filename ] <- getArgs
  source <- readFile filename
  dslFile <- dslPath
  dslSource <- readFile dslFile
  program <- readProgram (filename, source, dslFile, dslSource) Nothing >>= \e -> case e of
    Right r -> return r
    Left err -> error err
  withSizes <- return $ initSizes program
  labels <- trace ("Initial withsizes:\n" ++ unlines (map show withSizes)) return $ getLabels withSizes
  --fixedSizes <- fixSizes labels withSizes (filename, source, dslFile, dslSource)
  programWithLabels <- trace ("Initial labels are " ++ (unlines $ Map.elems $ Map.mapWithKey (\k v -> k ++ ": " ++ show v) labels)) readProgram (filename, source, dslFile, dslSource) (Just labels) >>= \o -> case o of
    Right r -> return r
    Left err -> error err
  fixedSizes <- return $ Rep.sizeReps (initSizes programWithLabels)
  labels <- return $ getLabels fixedSizes
  insanity <- return $ sanityCheck labels fixedSizes
  if insanity /= [] then error $ "Sanity check failed:\n" ++ (unlines $ map ((++) "  ") insanity)
    else do
    putStrLn $ unlines $ map show $ fixedSizes
    bytes <- return $ progToBytes labels fixedSizes
    zeroed <- return $ fixZeros bytes
    error "Success"
    putStrLn $ "\n===== Final code: ======\n" ++ (unlines $ map show zeroed)
    putStrLn $ "\n===== Final code expanded: ======\n" ++ (unlines $ map (Render.render (lines source) . fst) zeroed)
    putStrLn $ "\n===== Simulation: ======\n" ++ (unlines $ map show $ Simulate.simulate labels fixedSizes)
    putStrLn $ "\n===== Simulation expanded: ======\n" ++ (unlines $ map (Render.render (lines source)) $ Simulate.simulate labels fixedSizes)
    writeGzip "out.gz" zeroed
