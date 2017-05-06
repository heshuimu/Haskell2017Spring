{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Q4 where
  
import Proj4Utils
import Q1Q2
import Q3

interp :: String -> FBAEVal

interp str = let x = (parseFBAE str)
               in case typeof [] x of
                 TNum -> eval [] x
                 TBool -> eval [] x
                 (_ :->: _) -> eval [] x