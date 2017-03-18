{-# LANGUAGE GADTs #-}

module Project2 where
  
import System.IO.Unsafe

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

--
-- Simple caculator with variables extended Booleans and both static and
-- dynamic type checking.
--
-- Author: Perry Alexander
-- Editor: Haonan Li
-- Date: Wed Jul 13 11:24:46 CDT 2016
--
-- Source files for the Boolean Binding Arithmetic Expressions (BBAE)
-- language from PLIH
--

-- BBAE AST Definition

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  Seq :: BBAE -> BBAE -> BBAE
  Print :: BBAE -> BBAE
  Cons :: BBAE -> BBAE -> BBAE
  First :: BBAE -> BBAE
  Rest :: BBAE -> BBAE
  IsEmpty :: BBAE -> BBAE
  Empty :: BBAE
  deriving (Show,Eq)

-- Parser

expr :: Parser BBAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser BBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BBAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BBAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

trueExpr :: Parser BBAE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser BBAE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser BBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
            
seqExpr :: Parser BBAE
seqExpr = do reserved lexer "seq"
             f <- expr
             s <- expr
             return (Seq f s)

printExpr :: Parser BBAE
printExpr = do reserved lexer "print"
               t <- expr
               return (Print t)

consExpr :: Parser BBAE
consExpr = do reserved lexer "cons"
              f <- expr
              s <- expr
              return (Cons f s)

firstExpr :: Parser BBAE
firstExpr = do reserved lexer "first"
               t <- expr
               return (First t)
             
restExpr :: Parser BBAE
restExpr = do reserved lexer "rest"
              t <- expr
              return (Rest t)

isEmptyExpr :: Parser BBAE
isEmptyExpr = do reserved lexer "isEmpty"
                 t <- expr
                 return (IsEmpty t)

emptyExpr :: Parser BBAE
emptyExpr = do reserved lexer "empty"
               return Empty
             
term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> consExpr
       <|> firstExpr
       <|> restExpr              
       <|> isEmptyExpr
       <|> emptyExpr
       <|> printExpr
       <|> seqExpr
       
-- Parser invocation

parseBBAE = parseString expr

parseBBAEFile = parseFile expr

subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num x) = (Num x)
subst _ _ (Boolean x) = (Boolean x)
subst i v (Plus x y) = (Plus (subst i v x)(subst i v y))
subst i v (Minus x y) = (Minus (subst i v x)(subst i v y))
subst i v (Bind x y z) = if i==x
                            then (Bind x (subst i v y) z )
                            else (Bind x (subst i v y) (subst i v z))                 
subst i v (Id x) = if i == x then v else (Id x)
subst i v (And x y) = (And (subst i v x)(subst i v y))
subst i v (Leq x y) = (Leq (subst i v x)(subst i v y))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If x y z) = (If (subst i v x)(subst i v y)(subst i v z))

evals :: BBAE -> (Either String BBAE)
evals (Num n) = (Right (Num n))
evals (Boolean b) = (Right (Boolean b))
evals (Plus a b) = let a_eval = evals a
                       b_eval = evals b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Num a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Num b_num)) -> (Right (Num (a_num + b_num)))
                     (Right anythingElse) -> (Left "[Runtime] Plus: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] Plus: Type error in first parameter. 'Int' Expected. ")
evals (Minus a b) = let a_eval = evals a
                        b_eval = evals b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Num a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Num b_num)) -> (Right (Num (a_num - b_num)))
                     (Right anythingElse) -> (Left "[Runtime] SUB: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] SUB: Type error in first parameter. 'Int' Expected. ")
evals (And a b) = let a_eval = evals a
                      b_eval = evals b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Boolean a_bool)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Boolean b_bool)) -> (Right (Boolean (a_bool && b_bool)))
                     (Right anythingElse) -> (Left "[Runtime] AND: Type error in second parameter. 'Bool' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] AND: Type error in first parameter. 'Bool' Expected. ")
evals (Leq a b) = let a_eval = evals a
                      b_eval = evals b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Num a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Num b_num)) -> (Right (Boolean (a_num <= b_num)))
                     (Right anythingElse) -> (Left "[Runtime] LEQ: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] LEQ: Type error in first parameter. 'Int' Expected. ")
evals (IsZero a) = let a_eval = evals a
               in case a_eval of
                 (Left anything) -> a_eval
                 (Right (Num a_num)) -> (Right (Boolean (a_num == 0)))
                 (Right anythingElse) -> (Left "[Runtime] EQ0: Type error in parameter. 'Int' Expected. ")
evals (If a b c) = let a_eval = evals a
                  in case a_eval of
                    (Left anything) -> a_eval
                    (Right (Boolean a_bool)) -> if a_bool then (evals b) else (evals c)
                    (Right anythingElse) -> (Left "[Runtime] IFELSE: Type error in first parameter. 'Bool' Expected. ")
evals (Id id) = (Left "[Runtime] Id: Variable not declared")
evals (Bind i v b) = do
                        t <- (evals v)
                        (evals (subst i t b))
evals (Seq a b) = seq (evals a) (evals b)              
evals (Print a) = let a_eval = (evals a)
                  in case a_eval of 
                    (Left anything) -> a_eval
                    (Right x) -> seq (unsafePerformIO (print x)) (Right (Num 0))   
                    
interps :: String -> Either String BBAE
interps e = (evals (parseBBAE e))

-- env function

type Env = [(String,BBAE)]

eval :: Env -> BBAE -> (Either String BBAE)

eval env (Num n) = (Right (Num n))
eval env (Boolean b) = (Right (Boolean b))
eval env (Plus a b) = let a_eval = eval env a
                          b_eval = eval env b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Num a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Num b_num)) -> (Right (Num (a_num + b_num)))
                     (Right anythingElse) -> (Left "[Runtime] ADD: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] ADD: Type error in first parameter. 'Int' Expected. ")
eval env (Minus a b) = let a_eval = eval env a
                           b_eval = eval env b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Num a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Num b_num)) -> (Right (Num (a_num - b_num)))
                     (Right anythingElse) -> (Left "[Runtime] SUB: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] SUB: Type error in first parameter. 'Int' Expected. ")
eval env (And a b) = let a_eval = eval env a
                         b_eval = eval env b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Boolean a_bool)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Boolean b_bool)) -> (Right (Boolean (a_bool && b_bool)))
                     (Right anythingElse) -> (Left "[Runtime] AND: Type error in second parameter. 'Bool' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] AND: Type error in first parameter. 'Bool' Expected. ")
eval env (Leq a b) = let a_eval = eval env a
                         b_eval = eval env b
                 in case a_eval of 
                   (Left anything) -> a_eval
                   (Right (Num a_num)) -> case b_eval of 
                     (Left anything) -> b_eval
                     (Right (Num b_num)) -> (Right (Boolean (a_num <= b_num)))
                     (Right anythingElse) -> (Left "[Runtime] LEQ: Type error in second parameter. 'Int' Expected. ")
                   (Right anythingElse) -> (Left "[Runtime] LEQ: Type error in first parameter. 'Int' Expected. ")
eval env (IsZero a) = let a_eval = eval env a
               in case a_eval of
                 (Left anything) -> a_eval
                 (Right (Num a_num)) -> (Right (Boolean (a_num == 0)))
                 (Right anythingElse) -> (Left "[Runtime] EQ0: Type error in parameter. 'Int' Expected. ")
eval env (If a b c) = let a_eval = eval env a
                  in case a_eval of
                    (Left anything) -> a_eval
                    (Right (Boolean a_bool)) -> if a_bool then (eval env b) else (eval env c)
                    (Right anythingElse) -> (Left "[Runtime] IFELSE: Type error in first parameter. 'Bool' Expected. ")
eval env (Seq a b) = seq (eval env a) (eval env b)
eval env (Print a) = let a_eval = (eval env a)
                  in case a_eval of 
                    (Left anything) -> a_eval
                    (Right x) -> seq (unsafePerformIO (print x)) (Right (Num 0))
eval env (Bind i v b) = do
                           t <- (eval env v) 
                           eval ((i, t) : env) b
eval env (Id id) = case (lookup id env) of
                     Just x -> Right x
                     Nothing -> (Left "[Runtime] Id: Variable not declared")
-- list
eval env (Cons a b) = let a_eval = (eval env a)
                          b_eval = (eval env b)
                      in case a_eval of
                        (Left anything) -> a_eval
                        (Right anythingA) -> case b_eval of
                          (Left anything) -> b_eval
                          (Right anythingB) -> (Right (Cons anythingA anythingB))
eval env (First a) = let a_eval = (eval env a)
                     in case a_eval of
                       (Left anything) -> a_eval
                       (Right (Cons f r)) -> (Right f)
                       (Right anythingElse) -> (Left "[Runtime] First: Given parameter is not a list. ")
eval env (Rest a) = let a_eval = (eval env a)
                     in case a_eval of
                       (Left anything) -> a_eval
                       (Right (Cons f r)) -> (Right r)
                       (Right anythingElse) -> (Left "[Runtime] Rest: Given parameter is not a list. ")
eval env (Empty) = (Right (Empty))
eval env (IsEmpty a) = let a_eval = (eval env a)
                     in case a_eval of
                       (Left anything) -> a_eval
                       (Right Empty) -> (Right (Boolean True))
                       (Right (Cons f r)) -> (Right (Boolean False))
                       (Right anythingElse) -> (Left "[Runtime] IsEmpty: Given parameter is not a list. ")
    
interp :: String -> (Either String BBAE)
interp e = (eval [] (parseBBAE e))



               