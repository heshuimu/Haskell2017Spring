import Proj4Utils
import Q1Q2
import Q3
import Q4

{- CALL testTypeof AND testInterp TO RUN TEST -}

genericTestCases = ["app (lambda (x:Bool) in if x then 0 else 444 / 2 + (1 * 7 - 22)) (isZero (if (1 + 5 <= 3) && (true || false) then 0 else 1))", 
                    -- Arithmetic
                    "(app (lambda (n:Nat) in (app (lambda (f:Nat) in (app (lambda (n:Nat) in (app f 3)) 1)) (lambda (x:Nat) in x + n))) 5)",
                    -- Function in function = 8
                    "app (fix (lambda (ie:Nat) in (lambda (x:Nat) in if (isZero x) then x else x + app ie x - 1))) 4"
                    -- Recursion 1 + 2 + 3 + 4 = 10, provided by Prof.
                    ]
                    
                    
typeofInvoke str = typeof [] (parseFBAE str)

testTypeof = mapM_ print (map typeofInvoke genericTestCases)
testInterp = mapM_ print (map interp genericTestCases)