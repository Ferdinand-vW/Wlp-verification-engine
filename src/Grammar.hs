

-- UUAGC 0.9.52.1 (Grammar.ag)
module Grammar(Stmt(..),Expr(..)) where

import Data.SBV(SInteger)

-- Expr --------------------------------------------------------
data Expr = Lit (SInteger)
          | Name (String)
          | PCall (String) (([Expr]))
          | ForAll (String) (Expr)
          | Minus (Expr) (Expr)
          | Plus (Expr) (Expr)
          | Equal (Expr) (Expr)
          | Lower (Expr) (Expr)
          | LowerE (Expr) (Expr)
          | And (Expr) (Expr)
          | Or (Expr) (Expr)
          | Impl (Expr) (Expr)
          | Not (Expr)
          | Repby (Expr) (Expr) (Expr)
          | True_
          deriving ( Eq,Show)
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (Lit _i) =
    (sem_Expr_Lit _i)
sem_Expr (Name _var) =
    (sem_Expr_Name _var)
sem_Expr (PCall _name _args) =
    (sem_Expr_PCall _name _args)
sem_Expr (ForAll _var _expr) =
    (sem_Expr_ForAll _var (sem_Expr _expr))
sem_Expr (Minus _expr1 _expr2) =
    (sem_Expr_Minus (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (Plus _expr1 _expr2) =
    (sem_Expr_Plus (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (Equal _expr1 _expr2) =
    (sem_Expr_Equal (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (Lower _expr1 _expr2) =
    (sem_Expr_Lower (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (LowerE _expr1 _expr2) =
    (sem_Expr_LowerE (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (And _expr1 _expr2) =
    (sem_Expr_And (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (Or _expr1 _expr2) =
    (sem_Expr_Or (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (Impl _expr1 _expr2) =
    (sem_Expr_Impl (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (Not _expr) =
    (sem_Expr_Not (sem_Expr _expr))
sem_Expr (Repby _expr1 _expr2 _expr3) =
    (sem_Expr_Repby (sem_Expr _expr1) (sem_Expr _expr2) (sem_Expr _expr3))
sem_Expr (True_) =
    (sem_Expr_True_)
-- semantic domain
type T_Expr = ( )
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( ) = sem
     in  (Syn_Expr))
sem_Expr_Lit :: SInteger ->
                T_Expr
sem_Expr_Lit i_ =
    (let
     in  ( ))
sem_Expr_Name :: String ->
                 T_Expr
sem_Expr_Name var_ =
    (let
     in  ( ))
sem_Expr_PCall :: String ->
                  ([Expr]) ->
                  T_Expr
sem_Expr_PCall name_ args_ =
    (let
     in  ( ))
sem_Expr_ForAll :: String ->
                   T_Expr ->
                   T_Expr
sem_Expr_ForAll var_ expr_ =
    (let
     in  ( ))
sem_Expr_Minus :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Minus expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_Plus :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Plus expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_Equal :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Equal expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_Lower :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Lower expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_LowerE :: T_Expr ->
                   T_Expr ->
                   T_Expr
sem_Expr_LowerE expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_And :: T_Expr ->
                T_Expr ->
                T_Expr
sem_Expr_And expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_Or :: T_Expr ->
               T_Expr ->
               T_Expr
sem_Expr_Or expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_Impl :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Impl expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_Not :: T_Expr ->
                T_Expr
sem_Expr_Not expr_ =
    (let
     in  ( ))
sem_Expr_Repby :: T_Expr ->
                  T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Repby expr1_ expr2_ expr3_ =
    (let
     in  ( ))
sem_Expr_True_ :: T_Expr
sem_Expr_True_ =
    (let
     in  ( ))
-- Stmt --------------------------------------------------------
data Stmt = Var (([String])) (([Stmt]))
          | Prog (String) (([Expr])) (([Stmt]))
          | Pre (Expr)
          | Post (Expr)
          | Inv (Expr) (Stmt)
          | While (Expr) (([Stmt]))
          | If (Expr) (([Stmt])) (([Stmt]))
          | Assign (Expr) (Expr)
          | Skip
          deriving ( Eq,Show)
-- cata
sem_Stmt :: Stmt ->
            T_Stmt
sem_Stmt (Var _vars _body) =
    (sem_Stmt_Var _vars _body)
sem_Stmt (Prog _name _params _body) =
    (sem_Stmt_Prog _name _params _body)
sem_Stmt (Pre _expr) =
    (sem_Stmt_Pre (sem_Expr _expr))
sem_Stmt (Post _expr) =
    (sem_Stmt_Post (sem_Expr _expr))
sem_Stmt (Inv _expr _stmt) =
    (sem_Stmt_Inv (sem_Expr _expr) (sem_Stmt _stmt))
sem_Stmt (While _expr _body) =
    (sem_Stmt_While (sem_Expr _expr) _body)
sem_Stmt (If _expr _left _right) =
    (sem_Stmt_If (sem_Expr _expr) _left _right)
sem_Stmt (Assign _expr1 _expr2) =
    (sem_Stmt_Assign (sem_Expr _expr1) (sem_Expr _expr2))
sem_Stmt (Skip) =
    (sem_Stmt_Skip)
-- semantic domain
type T_Stmt = ( )
data Inh_Stmt = Inh_Stmt {}
data Syn_Stmt = Syn_Stmt {}
wrap_Stmt :: T_Stmt ->
             Inh_Stmt ->
             Syn_Stmt
wrap_Stmt sem (Inh_Stmt) =
    (let ( ) = sem
     in  (Syn_Stmt))
sem_Stmt_Var :: ([String]) ->
                ([Stmt]) ->
                T_Stmt
sem_Stmt_Var vars_ body_ =
    (let
     in  ( ))
sem_Stmt_Prog :: String ->
                 ([Expr]) ->
                 ([Stmt]) ->
                 T_Stmt
sem_Stmt_Prog name_ params_ body_ =
    (let
     in  ( ))
sem_Stmt_Pre :: T_Expr ->
                T_Stmt
sem_Stmt_Pre expr_ =
    (let
     in  ( ))
sem_Stmt_Post :: T_Expr ->
                 T_Stmt
sem_Stmt_Post expr_ =
    (let
     in  ( ))
sem_Stmt_Inv :: T_Expr ->
                T_Stmt ->
                T_Stmt
sem_Stmt_Inv expr_ stmt_ =
    (let
     in  ( ))
sem_Stmt_While :: T_Expr ->
                  ([Stmt]) ->
                  T_Stmt
sem_Stmt_While expr_ body_ =
    (let
     in  ( ))
sem_Stmt_If :: T_Expr ->
               ([Stmt]) ->
               ([Stmt]) ->
               T_Stmt
sem_Stmt_If expr_ left_ right_ =
    (let
     in  ( ))
sem_Stmt_Assign :: T_Expr ->
                   T_Expr ->
                   T_Stmt
sem_Stmt_Assign expr1_ expr2_ =
    (let
     in  ( ))
sem_Stmt_Skip :: T_Stmt
sem_Stmt_Skip =
    (let
     in  ( ))