{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Q1Q2 where
  
import Proj4Utils

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)
  
type Env = [(String, FBAEVal)]
  
eval :: Env -> FBAE -> FBAEVal

eval env (Num x) = (NumV x)

eval env (Boolean x) = (BooleanV x)

eval env (Id x) = case (lookup x env) of
                    (Just v) -> v
                    (Nothing) -> error ("[Runtime]Unidentified ID: " ++ x ++ ", Env = " ++ (show env))
                     
eval env (Plus x y) = let (NumV t1) = (eval env x)
                          (NumV t2) = (eval env y)
                        in (NumV (t1 + t2))
                        
eval env (Minus x y) = let (NumV t1) = (eval env x)
                           (NumV t2) = (eval env y)
                        in (NumV (t1 - t2))
                        
eval env (Mult x y) = let (NumV t1) = (eval env x)
                          (NumV t2) = (eval env y)
                        in (NumV (t1 * t2))
                        
eval env (Div x y) = let (NumV t1) = (eval env x)
                         (NumV t2) = (eval env y)
                       in if t2 == 0 
                         then error "[Runtime] DivideByZeroException"
                         else (NumV (div t1 t2))
                         
eval env (And x y) = let (BooleanV t1) = (eval env x)
                         (BooleanV t2) = (eval env y)
                       in (BooleanV (t1 && t2))
                       
eval env (Or x y) = let (BooleanV t1) = (eval env x)
                        (BooleanV t2) = (eval env y)
                      in (BooleanV (t1 || t2))
                      
eval env (Leq x y) = let (NumV t1) = (eval env x)
                         (NumV t2) = (eval env y)
                       in (BooleanV (t1 <= t2))
                       
eval env (IsZero x) = let (NumV t1) = (eval env x)
                        in (BooleanV (t1 == 0))
                        
eval env (If x y z) = let (BooleanV t1) = (eval env x)
                        in if t1 then (eval env y) else (eval env z)
                         
eval env (Lambda i t b) = (ClosureV i b env)

eval env (Bind i b e) = let t1 = (eval env b)
                          in (eval ((i, t1) : env) e)
                          
eval env (App x y) = let (ClosureV i b newE) = (eval env x)
                         t1 = (eval env y)
                       in (eval ((i, t1) : newE) b)
                       
eval env (Fix f) = let (ClosureV i b e) = (eval env f)
                     in (eval ((i, (eval e (Fix (Lambda i TNum b)))) : e) b)