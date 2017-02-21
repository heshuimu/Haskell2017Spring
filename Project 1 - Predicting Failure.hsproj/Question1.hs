{-# LANGUAGE GADTs #-}

import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

data ABE where
  Number :: Int -> ABE
  Boolean :: Bool -> ABE
  ADD :: ABE -> ABE -> ABE
  SUB :: ABE -> ABE -> ABE
  MUL :: ABE -> ABE -> ABE
  DIV :: ABE -> ABE -> ABE
  AND :: ABE -> ABE -> ABE
  LEQ :: ABE -> ABE -> ABE
  EQ0 :: ABE -> ABE
  IFELSE :: ABE -> ABE -> ABE -> ABE
  deriving(Show, Eq)
    
expr :: Parser ABE
expr = buildExpressionParser operators term
operators = [[inFix "*" MUL AssocLeft, inFix "/" DIV AssocLeft],
             [inFix "+" ADD AssocLeft, inFix "-" SUB AssocLeft],
             [inFix "<=" LEQ AssocLeft, preFix "isZero" EQ0],
             [inFix "&&" AND AssocLeft]]
             
numberExpr :: Parser ABE
numberExpr = do i <- integer lexer
                return (Number (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            check <- expr
            reserved lexer "then"
            true_statement <- expr
            reserved lexer "else"
            false_statement <- expr
            return (IFELSE check true_statement false_statement)

isZeroExpr :: Parser ABE
isZeroExpr = do reserved lexer "isZero" 
                check <- expr 
                return (EQ0 check)

term = parens lexer expr 
                <|> numberExpr 
                <|> trueExpr 
                <|> falseExpr 
                <|> ifExpr 
                <|> isZeroExpr

parseABE = parseString expr

parseABEFile = parseFile expr

eval :: ABE -> (Either String ABE)
eval (Number n) = (Right (Number n))
eval (Boolean b) = (Right (Boolean b))
eval (ADD a b) = let a_eval = eval a
                     b_eval = eval b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Number a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Number b_num)) -> (Right (Number (a_num + b_num)))
                     (Right anythingElse) -> (Left "[Runtime] ADD: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] ADD: Type error in first parameter. 'Int' Expected. ")
eval (SUB a b) = let a_eval = eval a
                     b_eval = eval b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Number a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Number b_num)) -> (Right (Number (a_num - b_num)))
                     (Right anythingElse) -> (Left "[Runtime] SUB: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] SUB: Type error in first parameter. 'Int' Expected. ")
eval (MUL a b) = let a_eval = eval a
                     b_eval = eval b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Number a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Number b_num)) -> (Right (Number (a_num * b_num)))
                     (Right anythingElse) -> (Left "[Runtime] MUL: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] MUL: Type error in first parameter. 'Int' Expected. ")
eval (DIV a b) = let a_eval = eval a
                     b_eval = eval b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Number a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Number b_num)) -> if b_num == 0 then (Left "[Runtime] DIV: Divide-by-zero error. Check second parameter. ") else (Right (Number (div a_num b_num)))
                     (Right anythingElse) -> (Left "[Runtime] DIV: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] DIV: Type error in first parameter. 'Int' Expected. ")
eval (AND a b) = let a_eval = eval a
                     b_eval = eval b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Boolean a_bool)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Boolean b_bool)) -> (Right (Boolean (a_bool && b_bool)))
                     (Right anythingElse) -> (Left "[Runtime] AND: Type error in second parameter. 'Bool' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] AND: Type error in first parameter. 'Bool' Expected. ")
eval (LEQ a b) = let a_eval = eval a
                     b_eval = eval b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Number a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Number b_num)) -> (Right (Boolean (a_num <= b_num)))
                     (Right anythingElse) -> (Left "[Runtime] LEQ: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] LEQ: Type error in first parameter. 'Int' Expected. ")
eval (EQ0 a) = let a_eval = eval a
               in case a_eval of
                 (Left anything) -> a_eval
                 (Right (Number a_num)) -> (Right (Boolean (a_num == 0)))
                 (Right anythingElse) -> (Left "[Runtime] EQ0: Type error in parameter. 'Int' Expected. ")
eval (IFELSE a b c) = let a_eval = eval a
                  in case a_eval of
                    (Left anything) -> a_eval
                    (Right (Boolean a_bool)) -> if a_bool then (eval b) else (eval c)
                    (Right anythingElse) -> (Left "[Runtime] IFELSE: Type error in first parameter. 'Bool' Expected. ")

data TABE where
  TypeNumber :: TABE
  TypeBoolean :: TABE
  deriving(Show, Eq)
 
typeof :: ABE -> Either String TABE
typeof (Number a) = (Right TypeNumber)
typeof (Boolean b) = (Right TypeBoolean)
typeof (ADD a b) = let a_type = typeof a
                       b_type = typeof b
                   in case a_type of 
                     (Left anything) -> a_type
                     (Right TypeNumber) -> case b_type of
                       (Left anything) -> b_type
                       (Right TypeNumber) -> (Right TypeNumber)
                       (Right anythingElse) -> Left ("[Type Check] ADD: Type error in second parameter. 'Int' Expected. ")
                     (Right anythingElse) -> (Left "[Type Check] ADD: Type error in first parameter. 'Int' Expected. ")
typeof (SUB a b) = let a_type = typeof a
                       b_type = typeof b
                   in case a_type of 
                     (Left anything) -> a_type
                     (Right TypeNumber) -> case b_type of
                       (Left anything) -> b_type
                       (Right TypeNumber) -> (Right TypeNumber)
                       (Right anythingElse) -> Left ("[Type Check] SUB: Type error in second parameter. 'Int' Expected. ")
                     (Right anythingElse) -> (Left "[Type Check] SUB: Type error in first parameter. 'Int' Expected. ")
typeof (MUL a b) = let a_type = typeof a
                       b_type = typeof b
                   in case a_type of 
                     (Left anything) -> a_type
                     (Right TypeNumber) -> case b_type of
                       (Left anything) -> b_type
                       (Right TypeNumber) -> (Right TypeNumber)
                       (Right anythingElse) -> Left ("[Type Check] MUL: Type error in second parameter. 'Int' Expected. ")
                     (Right anythingElse) -> (Left "[Type Check] MUL: Type error in first parameter. 'Int' Expected. ")
typeof (DIV a b) = let a_type = typeof a
                       b_type = typeof b
                   in case a_type of 
                     (Left anything) -> a_type
                     (Right TypeNumber) -> case b_type of
                       (Left anything) -> b_type
                       (Right TypeNumber) -> 
                         if b == (Number 0)
                           then (Left "[Type Check] DIV: Divide-by-zero error. Check second parameter. ")
                         else (Right TypeNumber)
                       (Right anythingElse) -> Left ("[Type Check] ADD: Type error in second parameter. 'Int' Expected. ")
                     (Right anythingElse) -> (Left "[Type Check] ADD: Type error in first parameter. 'Int' Expected. ")
typeof (AND a b) = let a_type = typeof a
                       b_type = typeof b
                   in case a_type of 
                     (Left anything) -> a_type
                     (Right TypeBoolean) -> case b_type of
                       (Left anything) -> b_type
                       (Right TypeBoolean) -> (Right TypeBoolean)
                       (Right anythingElse) -> (Left "[Type Check] AND: Type error in second parameter. 'Bool' Expected. ")
                     (Right anythingElse) -> (Left "[Type Check] AND: Type error in first parameter. 'Bool' Expected. ")
typeof (LEQ a b) = let a_type = typeof a
                       b_type = typeof b
                   in case a_type of 
                     (Left anything) -> a_type
                     (Right TypeNumber) -> case b_type of
                       (Left anything) -> b_type
                       (Right TypeNumber) -> (Right TypeBoolean)
                       (Right anythingElse) -> (Left "[Type Check] LEQ: Type error in second parameter. 'Int' Expected. ")
                     (Right anythingElse) -> (Left "[Type Check] LEQ: Type error in first parameter. 'Int' Expected. ")
typeof (EQ0 a) = let a_type = typeof a
                   in case a_type of 
                     (Left anything) -> a_type
                     (Right TypeNumber) -> (Right TypeBoolean)
                     (Right anythingElse) -> (Left "[Type Check] EQ0: Type error in parameter. 'Int' Expected. ")
typeof (IFELSE a b c) = let a_type = typeof a
                            b_type = typeof b
                            c_type = typeof c
                        in case a_type of 
                          (Left anything) -> a_type
                          (Right TypeBoolean) -> case b_type of
                            (Left anything) -> b_type
                            (Right anything) -> case c_type of 
                              (Left anything) -> c_type
                              (Right anything) ->
                                if b_type == c_type
                                  then b_type
                                else (Left "[Type Check] IFELSE: Type mismatch of second parameter and third parameter. Both true and false branches should yield the same type. ")
                          (Right anythingElse) -> (Left "[Type Check] IFELSE: Type error in first parameter. 'Bool' Expected. ")
                           
interp :: String -> Either String ABE
interp e = let syntaxTree = (parseABE e) 
           in case (typeof syntaxTree) of
             (Right anything) -> (eval syntaxTree)
             (Left errorMessage) -> (Left errorMessage)
optimizedInterp e = let syntaxTree = (parseABE e)
                    in case (typeof syntaxTree) of
                      (Right anything) -> eval (optimize syntaxTree)
                      (Left errorMessage) -> (Left errorMessage)

optimize :: ABE -> ABE
optimize (Number n) = (Number n)
optimize (Boolean b) = (Boolean b)
optimize (ADD a b) = let a_optimized = optimize a
                         b_optimized = optimize b
                     in case a_optimized of
                       (Number 0) -> b_optimized
                       (a_optimized) -> case b_optimized of 
                         (Number 0) -> a_optimized
                         (b_optimized) -> (ADD a_optimized b_optimized)
optimize (SUB a b) = let a_optimized = optimize a
                         b_optimized = optimize b
                     in case b_optimized of 
                         (Number 0) -> a_optimized
                         (b_optimized) -> (SUB a_optimized b_optimized)
optimize (MUL a b) = let a_optimized = optimize a
                         b_optimized = optimize b
                     in case a_optimized of 
                       (Number 0) -> (Number 0)
                       (a_optimized) -> case b_optimized of 
                         (Number 0) -> (Number 0)
                         (b_optimized) -> (MUL a_optimized b_optimized)
optimize (DIV a b) = let a_optimized = optimize a
                         b_optimized = optimize b
                     in case a_optimized of 
                       (Number 0) -> (Number 0)
                       (a_optimized) -> (DIV a_optimized b_optimized)
optimize (AND a b) = let a_optimized = optimize a
                         b_optimized = optimize b
                     in case a_optimized of 
                       (Boolean False) -> (Boolean False)
                       (Boolean True) -> case b_optimized of 
                         (Boolean False) -> (Boolean False)
                         (Boolean True) -> (Boolean True)
                         (b_optimized) -> b_optimized
                       (a_optimized) -> case b_optimized of 
                         (Boolean False) -> (Boolean False)
                         (Boolean True) -> a_optimized
                         (b_optimized) -> (AND a_optimized b_optimized)
optimize (LEQ a b) = let a_optimized = optimize a
                         b_optimized = optimize b
                     in (LEQ a_optimized b_optimized)
optimize (EQ0 a) = let a_optimized = optimize a
                   in case a_optimized of 
                     (Number x) -> if x == 0 then (Boolean True) else (Boolean False)
                     (a_optimized) -> (EQ0 a_optimized)
optimize (IFELSE a b c) = let a_optimized = optimize a
                              b_optimized = optimize b
                              c_optimized = optimize c
                          in case a_optimized of
                            (Boolean True) -> b_optimized
                            (Boolean False) -> c_optimized
                            (a_optimized) -> (IFELSE a_optimized b_optimized c_optimized)
