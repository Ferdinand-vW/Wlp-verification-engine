module PrettyPrint where


import Data.List
import Collect

class PrettyPrint a where
  pp :: a -> String

instance PrettyPrint Stmt where
  pp (Vars vars body) = "Var" ++ show vars ++ "[\n" ++ (foldr (\x y -> "\t" ++ pp x ++ ",\n" ++ y) "" body) ++ "\n]\n"
  pp (Assign e1 e2) = pp e1 ++ " := " ++ pp e2
  pp Skip = "Skip"
  pp (Pre e1) = "Assume " ++ pp e1
  pp (Post e1) = "Assert " ++ pp e1
  pp (If g s1 s2) = "If (" ++ pp g ++ ") {\n\t" ++
                    (foldr (\x y -> "\t" ++ pp x ++ ",\n" ++ y) "" s1) ++ "}\n else {\n\t" ++
                    (foldr (\x y -> "\t" ++ pp x ++ ",\n" ++ y) "" s2) ++ "}"
  pp (While g s) = "while (" ++ pp g ++ ")" ++ foldr (\x y -> "\t" ++ pp x ++ ",\n" ++ y) "" s
  pp (Inv e s) = "inv (" ++ pp e ++ "): " ++ pp s
  pp (Sim e1 e2) = intercalate "," (map pp e1) ++ " := " ++ intercalate "," (map pp e2)
  pp (Prog name inP outP body) = "Program " ++ name ++ " (" ++ intercalate "," (map pp inP) ++ ") (" ++
                                   intercalate "," (map pp outP) ++ " )" ++ "{\n" ++ (concat $ map pp body) ++ "\n}\n"

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

instance PrettyPrint Var where
  pp (Int i) = "Int " ++ i
  pp (Array s) = "Array " ++ s
  pp (Univ x) = x
  pp (Exis x) = x
