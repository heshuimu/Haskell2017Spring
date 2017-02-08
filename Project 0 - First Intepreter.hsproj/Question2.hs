{-# LANGUAGE GADTs #-}

module Question2 where

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

-- Q2 Part 1

data AEE where
    Num :: Int -> AEE
    Plus :: AEE -> AEE -> AEE
    Minus :: AEE -> AEE -> AEE
    Multi :: AEE -> AEE -> AEE
    Divis :: AEE -> AEE -> AEE
    IfZero :: AEE -> AEE -> AEE -> AEE
    deriving(Show, Eq)
    
-- Q2 Part 2

expr :: Parser AEE
expr = buildExpressionParser operators term

operators = [[ inFix "*" Multi AssocLeft,               
               inFix "/" Divis AssocLeft],
             [ inFix "+" Plus AssocLeft, 
               inFix "-" Minus AssocLeft]]

numExpr :: Parser AEE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AEE
ifExpr = do reserved lexer "if0"
            check <- expr
            reserved lexer "then"
            true_statement <- expr
            reserved lexer "else"
            false_statement <- expr
            return (IfZero check true_statement false_statement)

term = parens lexer expr <|> numExpr <|> ifExpr

parseAEE = parseString expr
parseAEEFile = parseFile expr

-- Q2 Part 3

eval :: AEE -> AEE
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
eval (IfZero c t f) = let (Num c_val) = eval c
                  in if (c_val == 0) then (eval t) else (eval f)
                  
interp = eval . parseAEE



