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

-- Q1 Part 1

data AE where
    Num :: Int -> AE
    Plus :: AE -> AE -> AE
    Minus :: AE -> AE -> AE
    Multi :: AE -> AE -> AE
    Divis :: AE -> AE -> AE
    deriving(Show, Eq)
    
-- Q1 Part 2

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [[ inFix "*" Multi AssocLeft,               
               inFix "/" Divis AssocLeft],
             [ inFix "+" Plus AssocLeft, 
               inFix "-" Minus AssocLeft]]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

term = parens lexer expr <|> numExpr

parseAE = parseString expr
parseAEFile = parseFile expr

-- Q1 Part 3

eval :: AE -> AE
eval (Num n) = (Num n)
eval (Plus a b) = let (Num a_val) = eval a
                      (Num b_val) = eval b
                  in (Num (a_val + b_val))
eval (Minus a b) = let (Num a_val) = eval a
                       (Num b_val) = eval b
                  in (Num (a_val - b_val))
eval (Multi a b) = let (Num a_val) = eval a
                       (Num b_val) = eval b
                  in (Num (a_val * b_val))
eval (Divis a b) = let (Num a_val) = eval a
                       (Num b_val) = eval b
                  in (Num (div a_val b_val))
                  
interp = eval . parseAE

