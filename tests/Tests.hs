import Test.HUnit
import qualified Data.ByteString.Char8 as Char8

import qualified Rep
import qualified Pump
import qualified Language as L

import Control.Exception
import Control.Monad

assertError :: (Exception e, Eq e) => e -> IO a -> IO ()
assertError ex action =
  handleJust (guard . (== ex)) (const $ return ()) $ do
    action
    assertFailure $ "Expected error: " ++ show ex

main = runTestTT tests

simplePrograms :: [(String, L.Op)]
simplePrograms = [
    ("data { string = \"some data\" }", L.Data (Char8.pack "some data")),
    ("print { string = \"this is a test\" }", L.Print "this is a test" False),
    ("print { len = 99 }", L.PrintLen 99 False),
    ("print { len = 99, final = true }", L.PrintLen 99 True)
  ]

testReadProgramSimple = TestLabel "Test reading programs" $ TestCase $ do
  dslSource <- readFile "lua/dsl.lua"

  mapM_ (\ (p, r) -> do
    p <- Pump.readProgram ("File", p, "dslfile", dslSource) Nothing
    p @?= [r]) simplePrograms

readTests = TestLabel "Reading program" $ TestList [
    testReadProgramSimple
  ]


testRepEncode = TestLabel "Test encoding reps" $ TestCase $ do
  Rep.encode 10 10 20 False @?= Char8.pack "B\176\NUL\NUL\NUL\NUL\255\255"
  Rep.encode 15 40 80 False @?= Char8.pack "\"\214\NUL\NUL\NUL\NUL\NUL\255\255"
  Rep.encode 15 40 80 True @?= Char8.pack "\"\214\NUL@\NUL\NUL\NUL\255\255"

testRepEncodeBackwards = TestLabel "Test encoding reps with negative from" $ TestCase $ do
  Rep.encode (-10) 10 20 False @?= Rep.encode 10 10 20 False
  Rep.encode (-1) 10 20 False @?= Rep.encode 100 10 101  False
  Rep.encode (-10) 10 20 True @?= Rep.encode 9 10 19 True

--testRepEncodeTooShort = TestLabel "Test encoding reps less than 3 long" $ TestCase $ do
--  assertError (ErrorCall "Error: repeat length of 2 is too small") $ return $ Rep.encode (-10) 2 20 False

repTests = TestLabel "Rep" $ TestList [
    testRepEncode,
    testRepEncodeBackwards
--    testRepEncodeTooShort
  ]

tests = TestList [ repTests, readTests ]
