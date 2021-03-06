import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import qualified Data.ByteString.Char8 as Char8
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)

import qualified Rep
import qualified Zero
import qualified Pump
import qualified Language as L

import Control.Exception
import Control.Lens ((&), (.~), (^.))
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
    ("print { len = 99 }", L.PrintLen (L.AddrI 99) False),
    ("print { len = 99, final = true }", L.PrintLen (L.AddrI 99) True)
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
    ("rep { }", "Errors from dsl: \n    [string \"rep { }\"]:1 Rep command must have from and len or to\n"),
    ("print { string = \"" ++ ([1..70000] >> "a") ++ "\" }", "Errors from dsl: \n    [string \"print { string = \"aaaaaaaaaaaaaaaaaaaaaaaaaaa...\"]:1 Print string can't be longer than 65535\n")
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
      return $ Pump.sanityCheck Map.empty withSizes

testEarlySanityCheck program = do
  dslSource <- readFile "lua/dsl.lua"
  program <- Pump.readProgram ("File", program, "dslfile", dslSource) Nothing
  case program of
    Left err -> (err @?= "[No error]") >> return []
    Right program -> do
      withSizes <- return $ Pump.initSizes program
      labels <- return $ Pump.getLabels withSizes
      return $ Pump.earlySanityCheck labels withSizes

testMissingStartLabel= TestLabel "Test missing _start label" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"firstlabel\"",
                "_\"secondlabel\""
              ]
  insanity @?= ["Missing \"_start\" label"]

testDoubleLabel = TestLabel "Test duplicate labels" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"firstlabel\"",
                "_\"secondlabel\"",
                "_\"firstlabel\"",
                "_\"_start\""
              ]
  insanity @?= ["Duplicate label: \"firstlabel\" at:\n    File:1\n    File:3\n"]

testUnknownLabel = TestLabel "Test unknown label" $ TestCase $ do
  insanity <- testEarlySanityCheck $ intercalate "\n" [
                "_\"firstlabel\"",
                "rep { from = l.firstlabel, to = l.unknownlabel }"
              ]
  insanity @?= ["Label used but not defined: unknownlabel"]

testBadPrintLength = TestLabel "Test bad print length" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"_start\"",
                "print { len = -3 }"
              ]
  insanity @?= ["Print len is -3; must be between 0 and 65535 at File:2"]

testMisalignedRep = TestLabel "Test misaligned reps" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "print { string = \"1234\" }",
                "_\"_start\"",
                "rep { from = 0, len = 4 }",
                "rep { from = 5, len = 4 }"
              ]
  insanity @?= ["Misaligned rep starting at 5 on File:4:\n    25=>12; +8=>4  Rep @5+4 <= File:4",
                "Misaligned rep ending at 9 on File:4:\n    25=>12; +8=>4  Rep @5+4 <= File:4"]
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"_start\"",
                "print { string = \"1234\" }",
                "rep { from = 0, len = 4}",
                "rep { from = 4, len = 3}"
              ]
  insanity @?= ["Misaligned rep ending at 7 on File:4:\n    25=>11; +8=>3  Rep @4+3 <= File:4"]

testShortRep = TestLabel "Test short reps" $ TestCase $ do
  insanity <- testSanityCheck $ intercalate "\n" [
                "_\"_start\"",
                "print { string = \"ab\" }",
                "print { string = \"cd\" }",
                "rep { from = 0, len = 4 }",
                "rep { from = 0, len = 2}"
              ]
  insanity @?= ["Short rep of 2 on File:5:\n     15=>103 + 1=>99 Rep @0+2 <= File:5"]


sanityTests = TestLabel "Sanity check" $ TestList [
    testMissingStartLabel,
    testDoubleLabel,
    testUnknownLabel,
    testBadPrintLength,
    --testMisalignedRep,
    testShortRep
  ]

copyPrograms :: [(String, [String], [String])]
copyPrograms = [
    ("basic copy", [
        "data { string = \"some data\" }",
        "_\"endcopy\"",
        "data { from = 0, to = l.endcopy}"
      ], [
        "data { string = \"some data\" }",
        "_\"endcopy\"",
        "data { string = \"some data\" }"
    ]),
    ("multiple expanding copies", [
        "data { string = \"some data\" }",
        "data { string = \"some other data\" }",
        "_\"endcopy\"",
        "data { from = 0, to = l.endcopy}",
        "_\"second_copy\"",
        "data { string = \"more data\" }",
        "data { string = \"more other data\" }",
        "_\"end_second_copy\"",
        "data { from = l.second_copy, to = l.end_second_copy}"
      ], [
        "data { string = \"some data\" }",
        "data { string = \"some other data\" }",
        "_\"endcopy\"",
        "data { string = \"some data\" }",
        "data { string = \"some other data\" }",
        "_\"second_copy\"",
        "data { string = \"more data\" }",
        "data { string = \"more other data\" }",
        "_\"end_second_copy\"",
        "data { string = \"more data\" }",
        "data { string = \"more other data\" }"
    ]),
    ("recursive expanding copies", [
        "data { string = \"some data\" }",
        "data { string = \"some other data\" }",
        "_\"endcopy\"",
        "data { from = 0, to = l.endcopy}",
        "_\"end_second_copy\"",
        "data { from = l.endcopy, to = l.end_second_copy}"
      ], [
        "data { string = \"some data\" }",
        "data { string = \"some other data\" }",
        "_\"endcopy\"",
        "data { string = \"some data\" }",
        "data { string = \"some other data\" }",
        "_\"end_second_copy\"",
        "data { string = \"some data\" }",
        "data { string = \"some other data\" }"
    ]),
    ("long copies", [
        "data { from = l.copy, to = l.finish}",
        "data { from = l.copy, to = l.finish}",
        "_\"copy\"",
        "data { string = \"data 1\" }",
        "data { string = \"data 2\" }",
        "data { string = \"data 3\" }",
        "_\"finish\"",
        "data { from = l.copy, to = l.finish}"
      ], [
        "data { string = \"data 1\" }",
        "data { string = \"data 2\" }",
        "data { string = \"data 3\" }",
        "data { string = \"data 1\" }",
        "data { string = \"data 2\" }",
        "data { string = \"data 3\" }",
        "_\"copy\"",
        "data { string = \"data 1\" }",
        "data { string = \"data 2\" }",
        "data { string = \"data 3\" }",
        "_\"finish\"",
        "data { string = \"data 1\" }",
        "data { string = \"data 2\" }",
        "data { string = \"data 3\" }"
    ]),
    ("rep adjustments", [
        "data { from = l.copy, to = l.finish}",
        "_\"copy\"",
        "rep { from = 0, to = l.copy, size = 8 }",
        "rep { from = 0, to = l.copy, at = l.copy, size = 8 }",
        "rep { from = 0, to = l.copy, at = 0, size = 8 }",
        "_\"finish\""
      ], [
        "rep { from = 0, to = l.copy, at = 24, size = 8 }",
        "rep { from = 0, to = l.copy, at = l.copy, size = 8 }",
        "rep { from = 0, to = l.copy, at = 0, size = 8 }",
        "_\"copy\"",
        "rep { from = 0, to = l.copy, size = 8 }",
        "rep { from = 0, to = l.copy, at = l.copy, size = 8 }",
        "rep { from = 0, to = l.copy, at = 0, size = 8 }",
        "_\"finish\""
    ])
  ]

fromRight a = case a of
  Right x -> x
  Left err -> error err

expandClones prog = do
  dslSource <- readFile "lua/dsl.lua"
  prog' <- Pump.readProgram ("File", (intercalate "\n" prog), "dslfile", dslSource) Nothing
  withSizes <- return $ Pump.initSizes $ fromRight prog'
  withClones <- return $ Pump.expandCopies withSizes
  fixedSizes <- return $ Pump.fixSizes withClones
  return $ Pump.declone fixedSizes

stripSrcs prog =
  map (\com -> com & (L.src .~ L.SrcNone)) prog

testCopyProgram prog1 prog2 = do
  prog1' <- expandClones prog1
  prog2' <- expandClones prog2
  stripSrcs prog1' @?= stripSrcs prog2'

copyTests = TestLabel "Copies" $ TestList $ flip map copyPrograms $ \(name, prog1, prog2) ->
    TestLabel name $ TestCase $ testCopyProgram prog1 prog2

doZeros prog = do
  decloned <- expandClones prog
  labels <- return $ Pump.getLabels decloned
  bytes <- return $ Pump.progToBytes labels decloned
  return $ Zero.fix bytes

zeroPrograms = [("Basic", [
              "data { string = \"testdata\" }",
              "zero { ranges = {{0, l.finish }} }",
              "_\"finish\""
             ], ["\195<<\244"])
            ]

testZeroProgram prog targetZeros = do
  zeroed <- doZeros prog
  zeros <- return $ filter (\(c, _) -> case c ^. L.op of L.Zero _ -> True; _ -> False) zeroed
  zeroBytes <- return $ map (\(c, L.Bytes b) -> b) zeros
  zeroBytes @?= map Char8.pack targetZeros

zeroTests = TestLabel "Zeros" $ TestList $ flip map zeroPrograms $ \(name, program, zeros) ->
  TestLabel name $ TestCase $ testZeroProgram program zeros

tests = TestList [ repTests, readTests, sanityTests, copyTests, zeroTests ]
