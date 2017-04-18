{-# LANGUAGE GADTs #-}

module Q2 where

import ParserUtils
import Proj3Utils

--Part 1

data CFAEValue where
  NumV :: Int -> CFAEValue
  ClosureV :: String -> CFAE -> EnvS -> CFAEValue
  deriving (Show,Eq)

type EnvS = [(String, CFAEValue)]

evalStatCFBE :: EnvS -> CFAE -> CFAEValue

evalStatCFBE env (Num x) = (NumV x)

evalStatCFBE env (Plus x y) = let (NumV t1) = (evalStatCFBE env x)
                                  (NumV t2) = (evalStatCFBE env y)
                                in (NumV (t1 + t2))
                             
evalStatCFBE env (Minus x y) = let (NumV t1) = (evalStatCFBE env x)
                                   (NumV t2) = (evalStatCFBE env y)
                                 in (NumV (t1 - t2))
                              
evalStatCFBE env (Mult x y) = let (NumV t1) = (evalStatCFBE env x)
                                  (NumV t2) = (evalStatCFBE env y)
                                in (NumV (t1 * t2))
                             
evalStatCFBE env (Div x y) = let (NumV t1) = (evalStatCFBE env x)
                                 (NumV t2) = (evalStatCFBE env y)
                               in (NumV (div t1 t2))
                            
evalStatCFBE env (Lambda x y) = (ClosureV x y env)

evalStatCFBE env (App x y) = let (ClosureV i b newE) = (evalStatCFBE env x)
                                 a = (evalStatCFBE env y)
                               in (evalStatCFBE ((i, a) : newE) b)
                            
evalStatCFBE env (Id x) = case (lookup x env) of
                            (Just x) -> x
                            (Nothing) -> error ("Unidentified ID: " ++ x)
                         
evalStatCFBE env (If x y z) = let (NumV t1) = (evalStatCFBE env x)
                                in if t1 == 0 then (evalStatCFBE env y) else (evalStatCFBE env z)
                               
--Part 2
                               
interpStatCFAE :: String -> CFAEValue

interpStatCFAE str = (evalStatCFBE [] (parseCFAE str))