module PrettyPrint where

import SyntaxTransformer

class PrettyPrint a where
  pp :: a -> String

instance PrettyPrint Stmt where
  pp (Vars vars body) = "Var" ++ show vars ++ (foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" body)
  pp (Assign e1 e2) = pp e1 ++ " := " ++ pp e2
  pp Skip = "Skip"
  pp (Pre e1) = "Assume " ++ pp e1
  pp (Post e1) = "Assert " ++ pp e1
  pp (If g s1 s2) = "If (" ++ pp g ++ ") {\n\t" ++
                    (foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" s1) ++ "}\n else {\n\t" ++
                    (foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" s2) ++ "}"
  pp (While g s) = "while (" ++ pp g ++ ")" ++ foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" s
  pp (Inv e s) = "inv (" ++ pp e ++ "): " ++ pp s
--data Stmt
--  | Vars     vars :: {[Var]}  body :: Body
--  | Prog    name :: String    params :: {[Var]} results :: {[Var]} body :: Body
--  | PCall   name :: String  args :: Exprs res :: Exprs
--  | Pre     expr :: Expr
--  | Post    expr :: Expr
--  | Inv     expr :: Expr      stmt :: Stmt
--  | While   expr :: Expr      body :: Body
--  | If      expr :: Expr      left :: Body    right :: Body
--  | Assign  expr1 :: Expr     expr2 :: Expr
--  | Sim     left :: Exprs      right :: Exprs
--  | Skip

instance PrettyPrint Expr where
  pp (Lit i) = show i
  pp (Name s) = s
  pp (ForAll s e) = "ForAll " ++ s ++ "(" ++ pp e ++ ")"
  pp (Minus e1 e2) = pp e1 ++ " - " ++ pp e2
  pp (Plus e1 e2) = pp e1 ++ " + " ++ pp e2
  pp (Equal e1 e2) = pp e1 ++ " == " ++ pp e2
  pp (Lower e1 e2) = pp e1 ++ " < " ++ pp e2
  pp (LowerE e1 e2) = pp e1 ++ " <= " ++ pp e2
  pp (And e1 e2) = pp e1 ++ " && " ++ pp e2
  pp (Or e1 e2) = pp e1 ++ " || " ++ pp e2
  pp (Impl e1 e2) = "(" ++ pp e1 ++ ")" ++ " --> " ++ "(" ++ pp e2 ++ ")"
  pp (Repby e1 e2) = pp e1 ++ "[" ++ pp e2 ++ "]"
  pp (Not e1) = "!(" ++ pp e1 ++ ")"
  pp True_ = "True"

