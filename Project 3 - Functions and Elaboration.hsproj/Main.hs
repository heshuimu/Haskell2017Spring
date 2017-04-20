import Q1
import Q2
import Q3

genericTestCases = ["app (lambda x in if x - 7 then 0 else 444 / 2 + 1 * 7) 4", 
                    -- Standard Arithmetic
                    "(app (lambda n in (app (lambda f in (app (lambda n in (app f 3)) 1)) (lambda x in x + n))) 5)"]
                    -- Scoping. Dyn = 4, Stat = 8. xs

cfbaeTestCases = ["bind x = 886 in x + 1", 
                  "app inc 4", 
                  "app dec 4", 
                  "app dec app inc app inc 4"]

testDynCFAE = mapM_ print (map interpDynCFAE genericTestCases)
testStatCFAE = mapM_ print (map interpStatCFAE genericTestCases)

testCFBAE = mapM_ print (map interpCFBAE cfbaeTestCases)