module GCL where

import SyntaxTransformer

import Data.SBV(SInteger)

var :: [Var] -> [Stmt] -> Stmt
var vars body = Vars vars body

pcall :: String -> [Expr] -> [Expr] -> Stmt
pcall name args vars = PCall name args vars

repby :: Expr  -> Expr -> Expr
repby var index = Repby var index

assume :: Expr -> Stmt
assume pre = Pre pre

int :: String -> Var
int s = Int s

array :: String -> Var
array s = Array s

assert :: Expr -> Stmt
assert post = Post post

inv :: Expr -> Stmt -> Stmt
inv pred body = Inv pred body

(.=) :: Expr -> Expr -> Stmt
(.=) expr1 expr2 = Assign expr1 expr2

sim :: [Expr] -> [Expr] -> Stmt
sim e1 e2 = Sim e1 e2
 
ref :: String -> Expr
ref s = Name s

prog :: String -> [Var] -> [Var] -> [Stmt] -> Stmt
prog s params results stmts = Prog s params results stmts

forall :: String -> Expr -> Expr
forall var' body = ForAll var' body

if_then_else :: Expr -> [Stmt] -> [Stmt] -> Stmt
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

neg :: Expr -> Expr
neg expr = Not expr

(.==>) :: Expr -> Expr -> Expr
(.==>) expr1 expr2 = Impl expr1 expr2

(.<) :: Expr -> Expr -> Expr
(.<) expr1 expr2 = Lower expr1 expr2

(.<=) :: Expr -> Expr -> Expr
(.<=) expr1 expr2 = LowerE expr1 expr2
