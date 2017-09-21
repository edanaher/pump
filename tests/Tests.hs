import Test.HUnit
import Data.ByteString.Char8 as Char8

import Rep

import Control.Exception
import Control.Monad

assertError :: (Exception e, Eq e) => e -> IO a -> IO ()
assertError ex action =
  handleJust (guard . (== ex)) (const $ return ()) $ do
    action
    assertFailure $ "Expected error: " ++ show ex

main = runTestTT tests

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

tests = TestList [ repTests ]
