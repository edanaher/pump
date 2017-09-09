{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate)
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
  deriving  (Show, Eq)

data Command = Com Op Int
  deriving (Show, Eq)

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
initSizes = map $ \op -> Com op $ case op of
    Rep _ _ _ _ _ -> 8
    Print str _ -> length str + 5
    PrintLen len _ _ -> 5
    Data str -> B.length str
    Copy _ len -> len
    Label _ -> 0

initLabels :: [Command] -> Map String Int
initLabels program =
  fst $ foldl (\ (labels, pos) (Com op size) -> case op of
      Label str -> (Map.insert str pos labels, pos)
      _ -> (labels, pos + size)
  ) (Map.empty, 0) program

updateSizes :: Map String Int -> [Command] -> [Op] -> ([Command], Map String Int)
updateSizes labels prog ops = do
  (prog', labels', pos, opos) <- return $ foldl (\ (prog, labels, pos, opos) op ->
        let bytes = toBytes op opos
            labels' = case op of Label str -> Map.insert str pos labels
                                 _ -> labels
            pos' = pos + B.length bytes
            opos' = if opos == -1 then -- Hack - assume first print is start of executable
                      case op of Label "_start" -> 0
                                 _ -> -1
                    else opos + outputSize op
        in ((Com op (B.length bytes)):prog, labels', pos', opos'))
        ([], Map.empty, 0, -1) ops
  trace (show labels ++ " => " ++ show labels') $ (reverse prog', labels')

fixSizes :: Map String Int -> [Command] -> String -> IO [Command]
fixSizes labels prog filename = do
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

toBytes :: Op -> Int -> B.ByteString
toBytes op opos = case op of
  Data str -> str
  Print str final -> (if final then "\001" else "\000") `Char8.append`
                     (intToBytes 2 $ length str) `Char8.append`
                     (intToBytes 2 $ 65535 - length str) `Char8.append`
                     Char8.pack str
  PrintLen len final _ -> (if final then "\001" else "\000") `Char8.append`
                          (intToBytes 2 $ len) `Char8.append`
                          (intToBytes 2 $ 65535 - len)
  Rep from len (Just at) final _ -> Rep.encode from len at final
  Rep from len Nothing final _ -> {-trace (show from ++ "/" ++ show len ++ " at " ++ " / o" ++ show opos) $-} Rep.encode from len opos final
  Label _ -> B.empty
  _ -> error $ "Converting unknown op to bytes:\n  " ++ show op

writeGzip filename bytes =
  B.writeFile filename (foldl B.append B.empty bytes)

progToBytes :: [Command] -> [B.ByteString]
progToBytes program =
  let (prog, pos, opos) = foldl (\ (prog, pos, opos) (Com op size) ->
        let bytes = toBytes op opos
            pos' = pos + B.length bytes
            opos' = if opos == -1 then -- Hack - assume first print is start of executable
                      case op of Label "_start" -> 0
                                 _ -> -1
                    else opos + outputSize op
        in (bytes:prog, pos', opos'))
        ([], 0, -1) program
  in reverse prog

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
