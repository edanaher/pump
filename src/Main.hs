module Main where

import Foreign.Lua as Lua

main :: IO ()
main = Lua.runLua $ do
  Lua.openlibs
  Lua.dostring "x = 1 + 4"
  x <- Lua.getglobal "x" *> peek (-1)
  liftIO $ putStrLn x
