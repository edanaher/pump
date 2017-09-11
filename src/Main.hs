{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate, mapAccumL)
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import System.Directory (makeAbsolute)
import System.Environment (getArgs, getExecutablePath)
import Data.Bits (shiftR, (.&.))
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)

import Rep

import qualified Foreign.Lua as Lua

dirpath :: String -> String
dirpath = reverse . dropWhile (/= '/') . reverse

dslFile :: IO FilePath
dslFile = do
  exePath <- getExecutablePath
  makeAbsolute $ (dirpath exePath) ++ "/../lua/dsl.lua"

data Op =
    Rep Int Int (Maybe Int) Bool Bool
  | Print String Bool
  | PrintLen Int Bool Bool
  | Data B.ByteString
  | Copy Int Int
  | Label String
  deriving  (Eq)

instance Show Op where
  show op = case op of
    Rep from len at final isdata -> "Rep from=" ++ show from ++ " len=" ++ show len ++ " at=" ++ show at ++ " final=" ++ show final
    Print str final -> "Print " ++ show str ++ " final=" ++ show final
    PrintLen len final isdata -> "Print " ++ show len ++ " final=" ++ show final
    Data bytes -> "Data " ++ show bytes
    Copy from len -> "Copy from=" ++ show from ++ " len=" ++ show len
    Label name -> name ++ ":"

data Command = Com Op Int Int Int Int
  deriving (Eq)

instance Show Command where
  show (Com op size osize pos opos) = show pos ++ "=>" ++ show opos ++ "; +" ++ show size ++ "=>" ++ show osize ++ "  " ++ show op

showProg :: [Command] -> String
showProg coms = unlines $ map show coms

instance Lua.FromLuaStack Op where
  peek idx = do
    tp <- getField Lua.peek "type" :: Lua.Lua String
    case tp of
      "rep" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        at <- getFieldOpt (liftM fromInteger . liftM toInteger . Lua.tointeger) "at"
        final <- getFieldBool "final"
        isdata <- getFieldBool "isdata"
        return $ Rep (fromInteger $ toInteger from) (fromInteger $ toInteger len) at final isdata
      "print" -> do
        final <- getFieldBool "final"
        str <- getFieldOpt Lua.peek "string"
        case str of
          Just s -> return $ Print s final
          Nothing -> do
            len <- getField Lua.tointeger "len"
            isdata <- getFieldBool "isdata"
            return $ PrintLen (fromInteger $ toInteger len) final isdata
      "copy" -> do
        from <- getField Lua.tointeger "from"
        len <- getField Lua.tointeger "len"
        return $ Copy (fromInteger $ toInteger from) (fromInteger $ toInteger len)
      "data" -> do
        str <- getField Lua.tostring "string"
        return $ Data str
      "label" -> do
        str <- getField Lua.peek "name"
        return $ Label str
    where getField peek f = do
            Lua.pushstring f
            Lua.gettable (idx - 1)
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

loadLuaLabels :: Maybe (Map String Int) -> Lua.Lua ()
loadLuaLabels labels =
  case labels of
    Nothing -> return ()
    Just ls -> do
      Lua.push (Map.map (fromInteger . toInteger) ls :: Map String Lua.LuaInteger)
      Lua.setglobal "l"
      Lua.dostring "setmetatable(l, label_err_mt)"
      return ()

loadFileUnsafe :: String -> Lua.Lua ()
loadFileUnsafe filename = do
  status <- Lua.loadfile filename
  if status == Lua.OK then do
    status <- Lua.call 0 0
    return ()
  else do
    err <- Lua.peek (-1)
    error $ "Error loading file '" ++ filename ++ "':" ++ err

readProgram :: String -> Maybe (Map String Int) -> IO [Op]
readProgram filename labels = do
  dslPath <- dslFile
  Lua.runLua $ do
    Lua.openlibs

    loadFileUnsafe dslPath
    loadLuaLabels labels
    loadFileUnsafe filename
    Lua.getglobal "program" *> Lua.peek (-1)

initSizes :: [Op] -> [Command]
initSizes = snd . mapAccumL (\(pos, opos) op -> 
    let (size, osize) = case op of
          Rep _ len _ _ _ -> (8, len)
          Print str _ -> (length str + 5, length str)
          PrintLen len _ _ -> (5, len)
          Data str -> (B.length str, 0)
          Copy _ len -> (len, 10)
          Label _ -> (0, 0)
    in ((pos + size, opos + osize), (Com op size osize (pos + size) (opos + osize)))) (0, 0)

initLabels :: [Command] -> Map String Int
initLabels =
  Map.fromList .
  map (\ (Com (Label str) _ _ pos _) -> (str, pos)) .
  filter (\ (Com op size osize pos opos) -> case op of
              Label str -> True
              _ -> False)

updateSizes :: Map String Int -> [Command] -> [Op] -> ([Command], Map String Int)
updateSizes labels prog ops = do
  (prog', labels', pos, opos, eatlen) <- return $ foldl (\ (prog', labels, pos, opos, eatlen) (op, com) ->
        let bytes = toBytes prog op opos
            (Com _ size _ _ _) = com
            labels' = case op of Label str -> Map.insert str pos labels
                                 _ -> labels
            pos' = pos + B.length bytes
            osize' = if opos < 0 || eatlen > 0 then 0 else outputSize op
            opos' = if opos == -1 then
                      case op of Label "_start" -> 0
                                 _ -> -1
                    else opos + osize'
            eatlen' = if eatlen > 0 then eatlen - size else
                      case op of PrintLen len _ _ -> len
                                 _ -> 0
        in
          trace ("Eatlen is " ++ show eatlen ++ " => " ++ show eatlen' ++ "; osize'/opos' are " ++ show osize' ++ "/" ++ show opos' ++ " on + " ++ show com) $ if eatlen' < 0 then error "Eatlen went negative!" else
          ((Com op (B.length bytes) osize' pos opos):prog', labels', pos', opos', eatlen'))
        ([], Map.empty, 0, -1, 0) (zip ops prog)
  trace ("Updated sizes: \n" ++ showProg (reverse prog')) $ (reverse prog', labels')

fixSizes :: Map String Int -> [Command] -> String -> IO [Command]
fixSizes labels prog filename = do
  _ <- trace ("Fixing sizes on:\n" ++ showProg prog) $ return 0
  ops' <- readProgram filename $ Just labels
  (prog', labels') <- return $ updateSizes labels prog ops'
  if prog == prog'
  then return prog
  else fixSizes labels' prog' filename

data ByteOp =
    Bytes B.ByteString
  | Zero Int Int

intToBytes :: Int -> Int -> B.ByteString
intToBytes len n = B.pack [fromIntegral $ (n `shiftR` (8*i)) .&. 255 | i <- [0..len-1] ]

outputSize :: Op -> Int
outputSize (Print str _) = length str
outputSize (PrintLen len _ False) = len
outputSize (Rep _ len _ _ False) = len
outputSize _ = 0

posToOpos :: [Command] ->  Int -> Int
posToOpos [] target = error "Ran out of command searching for pos"
posToOpos _ 0 = 0
posToOpos (Com op size osize pos opos:coms') target =
    trace ("Searching for " ++ show target ++ " at " ++ show (Com op size osize pos opos)) $
    if target < pos then error "Converting pos to opos isn't a boundary" else
    if target == pos then opos else
    posToOpos coms' target

toBytes :: [Command] -> Op -> Int -> B.ByteString
toBytes coms op opos = case op of
  Data str -> str
  Print str final -> (if final then "\001" else "\000") `Char8.append`
                     (intToBytes 2 $ length str) `Char8.append`
                     (intToBytes 2 $ 65535 - length str) `Char8.append`
                     Char8.pack str
  PrintLen len final _ -> (if final then "\001" else "\000") `Char8.append`
                          (intToBytes 2 $ len) `Char8.append`
                          (intToBytes 2 $ 65535 - len)
  Rep from len (Just at) final _ -> trace ("MAGIC " ++ show from ++ "/" ++ show len ++ " at " ++ show at ++ "o" ++ show (posToOpos coms at)) $ Rep.encode from len (posToOpos coms at) final
  Rep from len Nothing final _ -> trace (show from ++ "/" ++ show len ++ " at " ++ "o" ++ show opos ++ " from \n" ++ showProg coms) $ trace (show ("opos: " ++ show (posToOpos coms from))) $ Rep.encode from len opos final
  Label _ -> B.empty
  _ -> error $ "Converting unknown op to bytes:\n  " ++ show op

writeGzip filename bytes =
  B.writeFile filename (foldl B.append B.empty bytes)

progToBytes :: [Command] -> [B.ByteString]
progToBytes program =
  map (\ (Com op size osize pos opos) -> toBytes program op opos) program


main :: IO ()
main = do
  [ filename ] <- getArgs
  program <- readProgram filename Nothing
  withSizes <- return $ initSizes program
  labels <- return $ initLabels withSizes
  fixedSizes <- fixSizes labels withSizes filename
  bytes <- return $ progToBytes fixedSizes
  putStrLn $ show bytes
  writeGzip "out.gz" bytes
