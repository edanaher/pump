{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module Pump where

import qualified Data.Char as Char
import Data.List (intercalate, mapAccumL, unfoldr, sortBy)
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import System.Directory (makeAbsolute)
import System.Environment (getExecutablePath)
import Data.Bits (shiftR, (.&.))
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Control.Lens ((^.), (^?), view, (&), (.~))

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
      Nothing -> do
        label <- getField Lua.peek "case" :: Lua.Lua String
        case label of
          "label" -> liftM AddrL $ getField Lua.peek "value"
          "constant" -> liftM (AddrI . fromInteger . toInteger) $ getField Lua.tointeger "value"
          "plus" -> AddrSum <$> (getFieldInt Lua.peek 1) <*> (getFieldInt Lua.peek 2)
          "minus" -> AddrDiff <$> (getFieldInt Lua.peek 1) <*> (getFieldInt Lua.peek 2)
    where
      getField peek f = do
        Lua.pushstring f
        Lua.gettable (idx - 1)
        isnil <- Lua.isnil (-1)
        v <- peek (-1)
        Lua.remove (-1)
        return v
      getFieldInt peek n = do
        Lua.pushinteger $ fromInteger $ toInteger n
        Lua.gettable (idx - 1)
        isnil <- Lua.isnil (-1)
        v <- peek (-1)
        Lua.remove (-1)
        return v

instance Lua.FromLuaStack SrcedOp where
  peek idx = do
    srcInfo <- getLuaSrc
    tp <- getField Lua.peek "type" `Lua.catchLuaError` (\ e -> Lua.throwLuaError $ "Error reading type at " ++ (printLuaSrc srcInfo) ++ ": " ++ show e) :: Lua.Lua String
    wrapSrc (SrcLua srcInfo) $ case tp of
      "rep" -> do
        from <- getField Lua.peek "from"
        len <- getField Lua.peek "len"
        at <- getFieldOpt Lua.peek "at"
        size <- getFieldOpt (liftM fromInteger . liftM toInteger . Lua.tointeger) "size"
        final <- getFieldBool "final"
        return $ Rep from len at size final
      "print" -> do
        final <- getFieldBool "final"
        str <- getFieldOpt Lua.peek "string"
        case str of
          Just s -> return $ Print s final
          Nothing -> do
            len <- getField Lua.peek "len"
            return $ PrintLen len final
      "copy" -> do
        from <- getField Lua.peek "from"
        len <- getField Lua.peek "len"
        return $ Copy from len
      "data" -> do
        str <- getFieldOpt Lua.tostring "string"
        case str of
          Just s -> return $ Data s
          Nothing -> do
            n <- getField Lua.peek "int"
            size <- getField Lua.tointeger "size"
            return $ DataInt n (fromInteger $ toInteger size)
      "zero" -> do
        ranges <- getField Lua.peek "ranges" :: Lua.Lua [(Address, Address)]
        return $ Zero $ ranges
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
          DataInt str size -> (size, 0)
          Copy _ len -> (99, 10)
          Zero _ -> (4, 0)
          Label _ -> (0, 0)
    in ((pos + size, opos + osize), (Com op src size osize pos opos))) (0, 0)

getLabels :: [Command] -> LabelMap
getLabels =
  Map.fromList .
  map (\ com -> (com ^. op . label, com ^. pos)) .
  filter (\ com -> case com ^. op of
              Label str -> True
              _ -> False)

expandCopy :: LabelMap -> [Command] -> Command -> [Command]
expandCopy labels prog com =
  let Copy from len = com ^. op
      suffix = dropWhile (\com -> com ^. pos < from @! labels) prog
      startOpos = head suffix ^. opos
      coms = takeWhile (\com -> com ^. pos < from @! labels + len @! labels) suffix
      comsWithoutLabels = filter (\com -> case com ^. op of Label _ -> False ; _ -> True) coms

  in trace ("Copy: " ++ show from ++ " (" ++ show (from @! labels) ++ ")," ++ show len ++ " (" ++ show (len @! labels) ++ ") => " ++ (unlines $ map show comsWithoutLabels))
     zipWith (\i com' -> com' & src .~ SrcCopy (SrcedOp (com ^. op, com ^. src)) i (length comsWithoutLabels)) [0..] comsWithoutLabels

expandCopies :: LabelMap -> [Command] -> [Command]
expandCopies labels prog =
  debug ("Expanding copies from \n" ++ (unlines $ map show prog) ++ "\n")
  concatMap (\com -> case com ^. op of
        Copy from len -> expandCopy labels prog com
        _ -> [com]
      ) prog


updateSizes :: Map String Int -> [Command] -> [Command]
updateSizes labels prog = do
  _ <- trace ("Updating sizes starting from " ++ (unlines $ map show prog)) $ return ()
  (prog', pos, opos, eatlen, eatfrom) <- return $ foldl (\ (prog', pos, opos, eatlen, eatfrom) com ->
        let op' = com ^. op
            opsize = toSize labels prog op' opos
            pos' = pos + opsize
            osize' = if opos < 0 || eatlen > 0 then 0 else Simulate.outputSize labels op'
            opos' = if opos == -1 then
                      case op' of Label "_start" -> 0
                                  _ -> -1
                    else opos + osize'
            eatlen' = if eatlen > 0 then eatlen - (com ^. size) else
                      case op' of PrintLen len _ -> len @! labels
                                  _ -> 0
            eatfrom' = if eatlen > 0 then eatfrom else
                       case op' of PrintLen len _ -> com
                                   _ -> eatfrom
        in
          --trace ("Eatlen is " ++ show eatlen ++ " => " ++ show eatlen' ++ "; osize'/opos' are " ++ show osize' ++ "/" ++ show opos' ++ " on + " ++ show com) $
          if eatlen' < 0 then error ("Eatlen went negative!\n" ++ "  Eating from " ++ show eatfrom ++ "\n  had " ++ show eatlen ++ " at " ++ show com) else
          ((Com op' (com ^. src) opsize osize' pos opos):prog', pos', opos', eatlen', eatfrom'))
        ([], 0, -1, 0, head prog) $ trace ((++) "Formerly Aligned zip:\n" $ unlines $ map show $ prog) prog
  trace ("Updated sizes: \n" ++ showProg (reverse prog')) $ reverse prog'

updateRepSizes :: LabelMap -> [Command] -> [Command]
updateRepSizes labels prog =
  let updateRep com = case com ^. op of
        Rep from len at Nothing final ->
          let at' = case at of
                Just addr -> addr @! labels
                Nothing -> com ^. opos
          in
          com & osize .~ (len @! labels) & size .~ Rep.size (from @! labels) (len @! labels) at' final
        _ -> com
  in
  map updateRep prog

fixSizes :: [Command] -> [Command]
fixSizes prog =
  let labels = trace ("Fixing sizes:\n" ++ unlines (map show prog)) $ getLabels prog
      prog' = updateSizes labels prog
  in
  if prog == prog'
  then prog
  else fixSizes prog'



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
        badFrom = if isAligned coms (from + len @! labels) then [] else
                ["Misaligned rep ending at " ++ show (AddrSum from' len) ++ " on " ++ printSrc com ++ ":\n    " ++ show com] -}
        -- this doesn't work...
        badSize = if rsize == Nothing || rsize == Just (com ^. size) then [] else
                ["Wrong size on " ++ printSrc com ++ ":\n    " ++ show com]
        badFuture = if from < at then [] else
                ["Future rep on " ++ printSrc com ++ ":\n    " ++ show com]
        badShort = if len @! labels >= 3 then [] else
                ["Short rep of " ++ show len ++ " on " ++ printSrc com ++ ":\n    " ++ show com]
    in
    {-badAt ++ badFrom ++-} badFuture ++ badShort ++ badSize
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
  DataInt addr size -> intToBytes size (addr @! labels)
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

compile :: String -> Maybe String -> IO ()
compile filename simfile = do
  source <- readFile filename
  dslFile <- dslPath
  dslSource <- readFile dslFile
  program <- readProgram (filename, source, dslFile, dslSource) Nothing >>= \e -> case e of
    Right r -> return r
    Left err -> error err
  withSizes <- return $ initSizes program
  -- TODO: Sanity check labels existing/duplicates/etc.
  initLabels <- return $ getLabels withSizes
  withCopies <- return $ expandCopies initLabels withSizes
  fixedSizes <- return $ fixSizes withCopies
  labels <- return $ getLabels fixedSizes
  insanity <- return $ sanityCheck labels fixedSizes
  if insanity /= [] then error $ "Sanity check failed:\n" ++ (unlines $ map ((++) "  ") insanity)
    else do
    putStrLn $ unlines $ map show $ fixedSizes
    bytes <- return $ progToBytes labels fixedSizes
    zeroed <- return $ fixZeros bytes
    putStrLn $ "\n===== Final labels: ======\n" ++ (unlines $ map (\(l, n) -> l ++ ": " ++ show n) $ sortBy (\a b -> snd a `compare` snd b) $ Map.assocs labels)
    putStrLn $ "\n===== Final code: ======\n" ++ (unlines $ map show zeroed)
    putStrLn $ "\n===== Final code expanded: ======\n" ++ (unlines $ map (Render.render (lines source) . fst) zeroed)
    simulated <- return $ Simulate.simulate labels fixedSizes
    putStrLn $ "\n===== Simulation: ======\n" ++ (unlines $ map show simulated )
    putStrLn $ "\n===== Simulation expanded: ======\n" ++ (unlines $ map (Render.render (lines source)) simulated)
    writeGzip "out.gz" zeroed
    case simfile of
      Just simfile -> trace "Writing to simfile" writeFile simfile (unlines $ map (Render.render (lines source)) simulated)
      Nothing -> trace "simfile is empty" return ()
