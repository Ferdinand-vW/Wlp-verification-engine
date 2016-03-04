module GCL where

import Data.SBV(SInteger)

--data Program = Program [String] [Stmt]

data Stmt =
  Var [String] [Stmt] |
  Pre Expr                |
  Post Expr               |
  Inv Expr Stmt           |
  While Expr [Stmt]       |
  If Expr Stmt Stmt       |
  Assign Expr Expr        |
  Skip
  deriving (Eq,Show)

data Expr =
  Lit SInteger        |
  Name String         |
  ForAll String Expr  |
  Minus Expr Expr     |
  Plus Expr Expr      |
  Equal Expr Expr     |
  Lower Expr Expr     |
  LowerE Expr Expr    |
  And Expr Expr       |
  Or Expr Expr        |
  Not Expr            |
  Impl Expr Expr      |
  True_               |
  Repby Expr Expr
  deriving (Show,Eq)

data AsgTarget = AsgTarget String [Expr]
{-instance Show Expr where



  show (Lit s)        = show s
  show (Name s)       = s
  show (ForAll s e)   = "Forall " ++ s ++ " : " ++ show e
  show (Minus e1 e2)  = show e1 ++ " - " ++ show e2
  show (Plus e1 e2)   = show e1 ++ " + " ++ show e2
  show (Equal e1 e2)  = show e1 ++ " == " ++ show e2
  show (Lower e1 e2)  = show e1 ++ " < " ++ show e2
  show (LowerE e1 e2) = show e1 ++ " <= " ++ show e2
  show (And e1 e2)    = show e1 ++ " && " ++ show e2 
  show (Or e1 e2)     = show e1 ++ " || " ++ show e2 
  show (Not e1)       = "Not (" ++ show e1 ++ ")"
  show (Impl e1 e2)   = show e1 ++ " ==> "  ++ show e2
  show True_          = "True"-}

var :: [String] -> [Stmt] -> Stmt
var vars body = Var vars body

repby :: Expr  -> Expr -> Expr
repby var index = Repby var index

assume :: Expr -> Stmt
assume pre = Pre pre

assert :: Expr -> Stmt
assert post = Post post

inv :: Expr -> Stmt -> Stmt
inv pred body = Inv pred body

(.=) :: Expr -> Expr -> Stmt
(.=) expr1 expr2 = Assign expr1 expr2

ref :: String -> Expr
ref s = Name s

forall :: String -> Expr -> Expr
forall var' body = ForAll var' body

if_then_else :: Expr -> Stmt -> Stmt -> Stmt
if_then_else cond br1 br2 = If cond br1 br2

while :: Expr -> [Stmt] -> Stmt
while cond body = While cond body

i :: SInteger -> Expr
i n = Lit n

minus :: Expr -> Expr -> Expr
minus expr1 expr2 = Minus expr1 expr2

plus :: Expr -> Expr -> Expr
plus expr1 expr2 = Plus expr1 expr2

infixr 6 `minus`
infixr 6 `plus`
infixr 4 .==
infixr 4 .< 
infixr 4 .<=
infixr 4 .!
infixr 3 .&& 
infixr 2 .|| 
infixr 1 .= 
infixr 0 .==>

(.==) :: Expr -> Expr -> Expr
(.==) expr1 expr2 = Equal expr1 expr2

(.&&) :: Expr -> Expr -> Expr
(.&&) expr1 expr2 = And expr1 expr2

(.||) :: Expr -> Expr -> Expr
(.||) expr1 expr2 = Or expr1 expr2

(.!) :: Expr -> Expr
(.!) expr = Not expr

(.==>) :: Expr -> Expr -> Expr
(.==>) expr1 expr2 = Impl expr1 expr2

(.<) :: Expr -> Expr -> Expr
(.<) expr1 expr2 = Lower expr1 expr2

(.<=) :: Expr -> Expr -> Expr
(.<=) expr1 expr2 = LowerE expr1 expr2
