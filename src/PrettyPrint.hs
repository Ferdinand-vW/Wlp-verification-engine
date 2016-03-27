module PrettyPrint where

import Collect

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
  pp (Exists s e) = "Exists " ++ s ++ "(" ++ pp e ++ ")"
  pp (ForAll s e) = "ForAll " ++ s ++ "(" ++ pp e ++ ")"
  pp (Minus e1 e2) = pp e1 ++ " - " ++ pp e2
  pp (Plus e1 e2) = pp e1 ++ " + " ++ pp e2
  pp (Equal e1 e2) = pp e1 ++ " == " ++ pp e2
  pp (Lower e1 e2) = pp e1 ++ " < " ++ pp e2
  pp (LowerE e1 e2) = pp e1 ++ " <= " ++ pp e2
  pp (And e1 e2) = pp e1 ++ " && " ++ pp e2
  pp (Or e1 e2) = pp e1 ++ " || " ++ pp e2
  pp (Impl e1 e2) = "((" ++ pp e1 ++ ")" ++ " --> " ++ "(" ++ pp e2 ++ "))"
  pp (Repby e1 e2) = pp e1 ++ "[" ++ pp e2 ++ "]"
  pp (Not e1) = "!(" ++ pp e1 ++ ")"
  pp True_ = "True"


--instance Show Stmt where
--  show (Vars vars body) = "Var" ++ show vars ++ (foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" body)
--  show (Assign e1 e2) = pp e1 ++ " := " ++ pp e2
--  show Skip = "Skip"
--  show (Pre e1) = "Assume " ++ pp e1
--  show (Post e1) = "Assert " ++ pp e1
--  show (If g s1 s2) = "If (" ++ pp g ++ ") {\n\t" ++
--                    (foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" s1) ++ "}\n else {\n\t" ++
--                    (foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" s2) ++ "}"
--  show (While g s) = "while (" ++ pp g ++ ")" ++ foldr (\x y -> "\t" ++ pp x ++ "\n" ++ y) "" s
--  show (Inv e s) = "inv (" ++ pp e ++ "): " ++ pp s


--instance Show Expr where
--  show (Lit i) = show i
--  show (Name s) = s
--  show (ForAll s e) = "ForAll " ++ s ++ "(" ++ pp e ++ ")"
--  show (Minus e1 e2) = pp e1 ++ " - " ++ pp e2
--  show (Plus e1 e2) = pp e1 ++ " + " ++ pp e2
--  show (Equal e1 e2) = pp e1 ++ " == " ++ pp e2
--  show (Lower e1 e2) = pp e1 ++ " < " ++ pp e2
--  show (LowerE e1 e2) = pp e1 ++ " <= " ++ pp e2
--  show (And e1 e2) = pp e1 ++ " && " ++ pp e2
--  show (Or e1 e2) = pp e1 ++ " || " ++ pp e2
--  show (Impl e1 e2) = "((" ++ pp e1 ++ ")" ++ " --> " ++ "(" ++ pp e2 ++ "))"
--  show (Repby e1 e2) = pp e1 ++ "[" ++ pp e2 ++ "]"
--  show (Not e1) = "!(" ++ pp e1 ++ ")"
--  show True_ = "True"
