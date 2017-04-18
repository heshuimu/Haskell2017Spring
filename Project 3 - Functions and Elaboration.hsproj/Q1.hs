{-# LANGUAGE GADTs #-}

module Q1 where

import ParserUtils
import Proj3Utils

--Part 1

type EnvD = [(String, CFAE)]

evalDynCFAE :: EnvD -> CFAE -> CFAE

evalDynCFAE env (Num x) = (Num x)

evalDynCFAE env (Plus x y) = let (Num t1) = (evalDynCFAE env x)
                                 (Num t2) = (evalDynCFAE env y)
                               in (Num (t1 + t2))
                             
evalDynCFAE env (Minus x y) = let (Num t1) = (evalDynCFAE env x)
                                  (Num t2) = (evalDynCFAE env y)
                                in (Num (t1 - t2))
                              
evalDynCFAE env (Mult x y) = let (Num t1) = (evalDynCFAE env x)
                                 (Num t2) = (evalDynCFAE env y)
                               in (Num (t1 * t2))
                             
evalDynCFAE env (Div x y) = let (Num t1) = (evalDynCFAE env x)
                                (Num t2) = (evalDynCFAE env y)
                              in (Num (div t1 t2))
                            
evalDynCFAE env (Lambda x y) = (Lambda x y)

evalDynCFAE env (App x y) = let (Lambda i b) = (evalDynCFAE env x)
                                a = (evalDynCFAE env y)
                              in (evalDynCFAE ((i, a) : env) b)
                            
evalDynCFAE env (Id x) = case (lookup x env) of
                           (Just x) -> x
                           (Nothing) -> error ("Unidentified ID: " ++ x)
                         
evalDynCFAE env (If x y z) = let (Num t1) = (evalDynCFAE env x)
                               in if t1 == 0 then (evalDynCFAE env y) else (evalDynCFAE env z)
                               
--Part 2
                               
interpDynCFAE :: String -> CFAE

interpDynCFAE str = (evalDynCFAE [] (parseCFAE str))