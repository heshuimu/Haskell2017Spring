{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Q3 where
  
import Proj4Utils

type Cont = [(String,TFBAE)]

typeof :: Cont -> FBAE -> TFBAE

typeof cont (Num _) = TNum

typeof cont (Boolean _) = TBool

typeof cont (Id x) = case (lookup x cont) of
                       Just x -> x
                       Nothing -> error ("[Type Check]Unidentified ID: " ++ x)

typeof cont (Plus x y) = let t1 = (typeof cont x)
                             t2 = (typeof cont y)
                           in if (t1 == TNum && t2 == TNum)
                             then TNum 
                             else error "[Type Check]Type Mismatch in Plus, requires Num and Num. "
                             
typeof cont (Minus x y) = let t1 = (typeof cont x)
                              t2 = (typeof cont y)
                            in if (t1 == TNum && t2 == TNum)
                              then TNum 
                              else error "[Type Check]Type Mismatch in Minus, requires Num and Num. "
                              
typeof cont (Mult x y) = let t1 = (typeof cont x)
                             t2 = (typeof cont y)
                           in if (t1 == TNum && t2 == TNum)
                             then TNum 
                             else error "[Type Check]Type Mismatch in Mult, requires Num and Num. "
                             
typeof cont (Div x y) = let t1 = (typeof cont x)
                            t2 = (typeof cont y)
                          in if (t1 == TNum && t2 == TNum)
                            then TNum 
                            else error "[Type Check]Type Mismatch in Div, requires Num and Num. "
                            
typeof cont (And x y) = let t1 = (typeof cont x)
                            t2 = (typeof cont y)
                          in if (t1 == TBool && t2 == TBool)
                            then TBool 
                            else error "[Type Check]Type Mismatch in And, requires Bool and Bool. "
                            
typeof cont (Or x y) = let t1 = (typeof cont x)
                           t2 = (typeof cont y)
                         in if (t1 == TBool && t2 == TBool)
                           then TBool 
                           else error "[Type Check]Type Mismatch in Or, requires Bool and Bool. "
                           
typeof cont (Leq x y) = let t1 = (typeof cont x)
                            t2 = (typeof cont y)
                          in if (t1 == TNum && t2 == TNum)
                            then TBool 
                            else error "[Type Check]Type Mismatch in Leq, requires Num and Num. "
                            
typeof cont (IsZero x) = let t1 = (typeof cont x)
                           in if (t1 == TNum)
                             then TBool 
                             else error "[Type Check]Type Mismatch in And, requires Num. "
                             
typeof cont (If x y z) = let t1 = (typeof cont x)
                             t2 = (typeof cont y)
                             t3 = (typeof cont z)
                           in if (t1 == TBool)
                                then if (t2 == t3) 
                                  then t2 
                                  else error "[Type Check]Type mismatch in If, requires same return type for both branches. "
                                else error "[Type Check]Type mismatch in If, requires Bool for condition. "
                            
typeof cont (Bind i v b) = let v' = (typeof cont v) in (typeof ((i, v') : cont) b)
                             
typeof cont (Lambda x d b) = let r = (typeof ((x,d):cont) b) in (d :->: r)
                               
typeof cont (App x y) = let tyY = typeof cont y
                            tyX = typeof cont x
                          in case tyX of
                             (tyXd :->: tyXr) -> case tyY of
                                                   (tyYd :->: tyYr) -> if (tyXd == tyYr)
                                                                         then tyXr
                                                                         else error ("[Type Check]Type mismatch in App, for two lambda functions, return type of function Y must match with in parameter type of X. " ++ (show tyXd) ++ " vs "++ (show tyYr))
                                                   notLambdaY -> if (tyXd == notLambdaY)
                                                                   then tyXr
                                                                   else error ("[Type Check]Type mismatch in App, parameter type does not match between function X and parameter Y. " ++ (show tyXd) ++ " vs "++ (show tyY))
                             _ -> if (tyX == tyY) then tyX else error ("[Type Check]Type mismatch in App, parameter type does not match between function token X and parameter Y. " ++ (show tyX) ++ " vs "++ (show tyY))

                               
typeof cont (Fix t) = let r:->:d = typeof cont t in d
                              