module GCL where

import Data.SBV(SInteger)

data Stmt =
  Program [String] [Stmt] |
  Pre Expr                |
  Post Expr               |
  Inv Expr Stmt           |
  While Expr [Stmt]       |
  If Expr Stmt Stmt       |
  Assign Expr Expr        |
  Skip
  deriving (Eq,Show)

data Expr =
  Lit SInteger       |
  Name String        |
  ForAll String Expr |
  Minus Expr Expr    |
  Plus Expr Expr     |
  Equal Expr Expr    |
  Lower Expr Expr    |
  LowerE Expr Expr   |
  And Expr Expr      |
  Or Expr Expr       |
  Not Expr           |
  Impl Expr Expr
  deriving (Eq,Show)


var :: [String] -> [Stmt] -> Stmt
var vars body = Program vars body

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
