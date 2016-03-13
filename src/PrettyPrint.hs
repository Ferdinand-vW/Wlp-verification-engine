module PrettyPrint where

import SyntaxTransformer

class PrettyPrint a where
  pp :: a -> String

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
  pp (Impl e1 e2) = "(" ++ pp e1 ++ ")" ++ " --> \n" ++ "(" ++ pp e2 ++ ")"
  pp (Repby e1 e2) = pp e1 ++ "[" ++ pp e2 ++ "]"
  pp (Not e1) = "!(" ++ pp e1 ++ ")"
  pp True_ = "True"