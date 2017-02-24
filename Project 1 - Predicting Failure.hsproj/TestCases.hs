module TestCases where
  
import ABEParser

testCases = ["0 - 1 + 0 + 1 - 0 + 1", 
             "isZero 0",
             "isZero 2 / 0",
             "if 2 / (1-1) <= 7 then false else true",
             "1 + 18 / (if true && 7 <= 4 then 6 - 0 + 0 - 1 else 4)",
             "0 + 18 / 9 * (if true && 2 <= 4 && isZero 0 then 3 - 2 else 7)",
             "if isZero 0 / 3 then if 3 + 5 <= 8 && false then 5 else 7 else 2"
             ]

runParserTest = (map parseABE testCases)
runOptimizeTest = (map optimize runParserTest)
runInterpTest = (map interp testCases)
runOptimizedInterpTest = (map optimizedInterp testCases)

testParser = mapM_ print runParserTest
testOptimize = mapM_ print runOptimizeTest
testInterp = mapM_ print runInterpTest
testOptimizedInterp = mapM_ print runOptimizedInterpTest