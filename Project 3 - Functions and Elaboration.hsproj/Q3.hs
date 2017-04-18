{-# LANGUAGE GADTs #-}

module Q3 where

import ParserUtils
import Proj3Utils
import Q2
  
--Part 1

  --Proj3Utils already contains a definition for CFBAE...
  
--Part 2

elabCFBAE :: CFBAE -> CFAE

elabCFBAE (NumX x) = (Num x)

elabCFBAE (PlusX x y) = (Plus (elabCFBAE x)(elabCFBAE y))

elabCFBAE (MinusX x y) = (Minus (elabCFBAE x)(elabCFBAE y))

elabCFBAE (MultX x y) = (Mult (elabCFBAE x)(elabCFBAE y))

elabCFBAE (DivX x y) = (Div (elabCFBAE x)(elabCFBAE y))

elabCFBAE (BindX i b e) = (App (Lambda i (elabCFBAE e)) (elabCFBAE b))

elabCFBAE (LambdaX x y) = (Lambda x (elabCFBAE y))

elabCFBAE (AppX x y) = let a = (elabCFBAE x)
                           b = (elabCFBAE y)
                         in (App a b)
                          
elabCFBAE (IdX x) = (Id x)

elabCFBAE (IfX x y z) = (If (elabCFBAE x) (elabCFBAE y) (elabCFBAE z))

--Part 3

evalCFBAE :: EnvS -> CFBAE -> CFAEValue

evalCFBAE env cfbae = evalStatCFBE env (elabCFBAE cfbae)

--Part 4

myPrelude :: EnvS

myPrelude = [("inc", (ClosureV "x" (Plus (Id "x") (Num 1)) [])), 
             ("dec", (ClosureV "x" (Minus (Id "x") (Num 1)) []))]

interpCFABE :: String -> CFAEValue

interpCFABE str = (evalCFBAE myPrelude (parseCFBAE str))