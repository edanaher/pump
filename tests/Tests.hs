import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import qualified Data.ByteString.Char8 as Char8
import Data.List (intercalate)

import qualified Rep
import qualified Pump
import qualified Language as L

import Control.Exception
import Control.Monad

main = do
  Counts _ _ errs fails <- runTestTT tests
  if errs > 0 || fails > 0 then exitFailure else exitSuccess

wrapSrc src =
 L.SrcLua ("File", 1)

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
    p' <- Pump.readProgram ("File", p, "dslfile", dslSource) Nothing
    p' @?= Right [L.SrcedOp (r, wrapSrc p)]) simplePrograms

errorPrograms :: [(String, String)]
errorPrograms = [
    ("parse error nonsense", "Error loading source from 'File':[string \"parse error nonsense\"]:1: syntax error near 'error'"),
    ("data { }", "Errors from dsl: \n    [string \"data { }\"]:1 Data command must have string, int, or from field\n"),
    ("print { }", "Errors from dsl: \n    [string \"print { }\"]:1 Print command must have string, len, or from field\n"),
    ("rep { }", "Errors from dsl: \n    [string \"rep { }\"]:1 Rep command must have from and len or to\n")
  ]

testReadProgramError = TestLabel "Testing error handling parsing programs" $ TestCase $ do
  dslSource <- readFile "lua/dsl.lua"
  mapM_ (\ (p, err) -> do
    p <- Pump.readProgram ("File", p, "dslfile", dslSource) Nothing
    p @?= Left err) errorPrograms


readTests = TestLabel "Reading program" $ TestList [
    testReadProgramSimple,
    testReadProgramError
  ]


testRepEncode = TestLabel "Test encoding reps" $ TestCase $ do
  Rep.encode 10 10 20 False @?= Char8.pack "B\176\NUL\NUL\NUL\NUL\255\255"
  Rep.encode 15 40 80 False @?= Char8.pack "\"\214\NUL\NUL\NUL\NUL\NUL\255\255"
  Rep.encode 15 40 80 True @?= Char8.pack "\"\214\NUL@\NUL\NUL\NUL\255\255"

testRepEncodeBackwards = TestLabel "Test encoding reps with negative from" $ TestCase $ do
  Rep.encode (-10) 10 20 False @?= Rep.encode 10 10 20 False
  Rep.encode (-1) 10 20 False @?= Rep.encode 100 10 101  False
  Rep.encode (-10) 10 20 True @?= Rep.encode 9 10 19 True

repTests = TestLabel "Rep" $ TestList [
    testRepEncode,
    testRepEncodeBackwards
  ]


testSanityCheck program = do
  dslSource <- readFile "lua/dsl.lua"
  program <- Pump.readProgram ("File", program, "dslfile", dslSource) Nothing
  case program of
    Left err -> (err @?= "[No error]") >> return []
    Right program -> do
      withSizes <- return $ Pump.initSizes program
      return $ Pump.sanityCheck withSizes

testMissingStartLabel= TestLabel "Test missing _start label" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"firstlabel\"",
                "_\"secondlabel\""
              ]
  insanity @?= ["Missing \"_start\" label"]

testDoubleLabel= TestLabel "Test duplicate labels" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"firstlabel\"",
                "_\"secondlabel\"",
                "_\"firstlabel\"",
                "_\"_start\""
              ]
  insanity @?= ["Duplicate label: \"firstlabel\" at:\n    File:1\n    File:3\n"]

testMisalignedRep = TestLabel "Test misaligned reps" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "print { string = \"1234\" }",
                "_\"_start\"",
                "rep { from = 0, len = 4 }",
                "rep { from = 5, len = 4 }"
              ]
  insanity @?= ["Misaligned rep starting at 5 on File:4:\n    25=>12; +8=>4  Rep from=5 len=4 at=Nothing final=False <= SrcLua (\"File\",4)",
                "Misaligned rep ending at 9 on File:4:\n    25=>12; +8=>4  Rep from=5 len=4 at=Nothing final=False <= SrcLua (\"File\",4)"]
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"_start\"",
                "print { string = \"1234\" }",
                "rep { from = 0, len = 4}",
                "rep { from = 4, len = 3}"
              ]
  insanity @?= ["Misaligned rep ending at 7 on File:4:\n    25=>11; +8=>3  Rep from=4 len=3 at=Nothing final=False <= SrcLua (\"File\",4)"]

testShortRep = TestLabel "Test short reps" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"_start\"",
                "print { string = \"ab\" }",
                "print { string = \"cd\" }",
                "rep { from = 0, len = 4 }",
                "rep { from = 0, len = 2}"
              ]
  insanity @?= ["Short rep of 2 on File:5:\n    30=>10; +8=>2  Rep from=0 len=2 at=Nothing final=False <= SrcLua (\"File\",5)"]


sanityTests = TestLabel "Sanity check" $ TestList [
    testMissingStartLabel,
    testDoubleLabel,
    --testMisalignedRep,
    testShortRep
  ]



tests = TestList [ repTests, readTests, sanityTests ]
