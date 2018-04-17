{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module Pump where

import qualified Data.Char as Char
import Data.List (intercalate, mapAccumL, unfoldr, sortBy, (\\), nub)
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import System.Directory (makeAbsolute)
import System.Environment (getExecutablePath)
import System.FilePath (dropExtension)
import Data.Bits (shiftR, (.&.))
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Control.Lens ((^.), (^?), view, (&), (.~))
import Data.Generics

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

addAtToReps com = case com ^. op of
  Rep from len Nothing rsize final -> com & ((op . at) .~ (Just $ AddrI $ com ^. pos))
  _ -> com

expandCopy :: LabelMap -> [Command] -> Command -> [Command]
expandCopy labels prog com =
  let Copy from len = com ^. op
      rawExpand (com, n) = case com ^. op of
        Copy from len ->
          let suffix = dropWhile (\(com, n) -> com ^. pos < from @! labels) (zip prog [0..])
              coms = takeWhile (\(com, n) -> com ^. pos < from @! labels + len @! labels) suffix
          in concatMap rawExpand coms
        _ -> [(com, n)]
      coms = rawExpand (com, 0)
      comsWithoutLabels = filter (\(com, n) -> case com ^. op of Label _ -> False ; _ -> True) coms

      src' i = SrcCopy (SrcedOp (com ^. op, com ^. src)) i (length comsWithoutLabels)
  in trace ("Copy: " {--++ show (-99 ++ " (" ++ show (from @! labels) ++ ")," ++ show len ++ " (" ++ show (len @! labels)--} ++ ") => " ++ (unlines $ map show comsWithoutLabels))
     zipWith (\i (com', n) -> com & (src .~ src' i) . (op .~ Clone n)) [0..] comsWithoutLabels

expandCopies :: [Command] -> [Command]
expandCopies prog =
  let labels = getLabels prog
      ((_, deltas), unadjusted) = debug ("Expanding copies from \n" ++ (unlines $ map show prog) ++ "\n")
        mapAccumL (\(i, deltas) com -> case com ^. op of
              Copy from len ->
                let copied = expandCopy labels prog com
                in ((i + 1, (i, length copied - 1):deltas), copied)
              _ -> ((i + 1, deltas), [com])
            ) (0, []) prog

      adjust [] n n' = n
      adjust ((p, d):ds) n n' = trace ("Adjusting " ++ show n' ++ ", " ++ show p ++ " -> " ++ show (n + d)) $ if n' > p then adjust ds (n + d) n' else n
      adjusted = flip map (concat unadjusted) $ \ com -> case com ^. op of
        Clone n -> com & op . index .~ adjust (reverse deltas) n n
        _ -> com
  in
    trace ("Used " ++ show deltas ++ " to adjust \n " ++ showProg (concat unadjusted) ++ "\n to \n" ++ showProg adjusted) adjusted


updateSizes :: Map String Int -> [Command] -> [Command]
updateSizes labels prog = do
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
        ([], 0, -1, 0, head prog) prog
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
      prog' = trace ("\ESC[31;1mUpdating sizes:\n" ++ unlines (map show prog) ++ "\n\nfrom:\n" ++ unlines (map show prog) ++ "\ESC[0m") $ updateSizes labels prog
  in
  if prog == prog'
  then prog
  else trace ("Updated sizes:\n" ++ unlines (map show prog')) fixSizes prog'



{- It's a bit more complicated than this; e.g.,
 -   print "ab"; print "cd"; rep 0 4; rep 4 2
 - is fine because the second rep only includes part of the first rep, but the
 - entire print.  Hopefully this won't happen in real programs. -}
isAligned :: [Command] -> Int -> Bool
isAligned coms target =
  target == 0 || any ((== target) . view opos) coms

printSrc = show . view src

labelsUsed :: [Command] -> [String]
labelsUsed coms =
  everything (++) ([] `mkQ` (\addr -> case addr of AddrL l -> [l]; _ -> [])) coms

checkLabelsExist :: LabelMap -> [Command] -> [String]
checkLabelsExist labels coms =
  map ("Label used but not defined: " ++) $ nub (labelsUsed coms) \\ Map.keys labels

earlySanityCheck :: LabelMap -> [Command] -> [String]
earlySanityCheck labels coms =
   checkLabelsExist labels coms

checkCom :: LabelMap -> [Command] -> Command -> [String]
checkCom labels coms com = case com ^. op of
  PrintLen len final -> trace ("Len is" ++ show len) $
    if len @! labels >= 0 && len @! labels < 65536 then [] else
    ["Print len is " ++ show (len @! labels) ++ "; must be between 0 and 65535 at " ++ printSrc com]
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
posToOpos [] target = error $ "Ran out of command searching for pos " ++ show target ++ " to convert to opos"
posToOpos _ 0 = 0
posToOpos (Com op src size osize pos opos:coms') target =
    if target < pos then opos else --error $ "Converting pos " ++ show target ++ " to opos isn't a boundary" else
    if target == pos then opos else
    posToOpos coms' target

declone :: [Command] -> [Command]
declone prog =
  let resolve c = case c ^. op of
        Clone n -> c & op .~ (resolve (prog !! n) ^. op)
        Rep _ _ Nothing _ _ -> c & op . at .~ Just (AddrI $ c ^. pos)
        _ -> c
  in
  map resolve prog

toBytes :: LabelMap -> [Command] -> Op -> Int -> B.ByteString
toBytes labels coms op' opos' = case op' of
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
  Rep from len Nothing _ final -> Rep.encode (evalAddr labels from) (evalAddr labels len) opos' final
  Label _ -> B.empty
  Copy from len -> B.empty
  Clone n -> toBytes labels coms ((coms !! n) ^. op) ((coms !! n) ^. opos) -- TODO: This is the wrong opos
  _ -> error $ "Converting unknown op to bytes:\n  " ++ show op'

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

compile :: String -> Maybe String -> Bool -> Maybe String -> Bool -> IO ()
compile filename outfileOpt simulate simfileOpt rawsim = do
  basename <- return $ dropExtension filename
  outfile <- return $ case outfileOpt of
    Just outfile -> outfile
    Nothing -> basename ++ ".gz"
  simfile <- return $ case simfileOpt of
    Just simfile -> simfile
    Nothing -> dropExtension outfile
  source <- readFile filename
  dslFile <- dslPath
  dslSource <- readFile dslFile
  program <- readProgram (filename, source, dslFile, dslSource) Nothing >>= \e -> case e of
    Right r -> return r
    Left err -> error err
  withSizes <- return $ initSizes program
  -- TODO: Sanity check labels existing/duplicates/etc.
  initLabels <- return $ getLabels withSizes
  insanity <- return $ earlySanityCheck initLabels withSizes
  _ <- if insanity /= [] then error $ "Sanity check failed:\n" ++ (unlines $ map ((++) "  ") insanity) else return ()
  withClones <- return $ expandCopies withSizes
  fixedSizes <- return $ fixSizes withClones
  --error "Success"
  labels <- return $ getLabels fixedSizes
  insanity <- return $ sanityCheck labels fixedSizes
  _ <- if insanity /= [] then error $ "Sanity check failed:\n" ++ (unlines $ map ((++) "  ") insanity) else return ()
  putStrLn $ unlines $ map show $ fixedSizes
  decloned <- return $ declone fixedSizes
  bytes <- return $ trace ("decloned to:\n" ++ showProg decloned) progToBytes labels decloned
  zeroed <- return $ Zero.fix bytes
  putStrLn $ "\n===== Final labels: ======\n" ++ (unlines $ map (\(l, n) -> l ++ ": " ++ show n) $ sortBy (\a b -> snd a `compare` snd b) $ Map.assocs labels)
  putStrLn $ "\n===== Final code: ======\n" ++ (unlines $ map show zeroed)
  --putStrLn $ "\n===== Final code expanded: ======\n" ++ (unlines $ map (Render.render (lines source) . fst) zeroed)
  simulated <- return $ Simulate.simulate labels zeroed
  putStrLn $ "\n===== Simulation: ======\n" ++ (unlines $ map show simulated )
  --putStrLn $ "\n===== Simulation expanded: ======\n" ++ (unlines $ map (Render.render (lines source)) simulated)
  writeGzip outfile zeroed
  if simulate then do
    (input, output) <- return $ Render.renderProgram (lines source) labels zeroed simulated rawsim
    writeFile (simfile ++ ".in") $ unlines input
    writeFile (simfile ++ ".out") $ unlines output
  else
    return ()
