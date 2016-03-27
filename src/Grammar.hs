

-- UUAGC 0.9.52.1 (src/Grammar.ag)

-- Body --------------------------------------------------------
type Body = [Stmt]
-- cata
sem_Body :: Body ->
            T_Body
sem_Body list =
    (Prelude.foldr sem_Body_Cons sem_Body_Nil (Prelude.map sem_Stmt list))
-- semantic domain
type T_Body = ( )
data Inh_Body = Inh_Body {}
data Syn_Body = Syn_Body {}
wrap_Body :: T_Body ->
             Inh_Body ->
             Syn_Body
wrap_Body sem (Inh_Body) =
    (let ( ) = sem
     in  (Syn_Body))
sem_Body_Cons :: T_Stmt ->
                 T_Body ->
                 T_Body
sem_Body_Cons hd_ tl_ =
    (let
     in  ( ))
sem_Body_Nil :: T_Body
sem_Body_Nil =
    (let
     in  ( ))
-- Expr --------------------------------------------------------
data Expr = Lit (SInteger)
          | Name (String)
          | ForAll (String) (Expr)
          | Exists (String) (Expr)
          | Minus (Expr) (Expr)
          | Plus (Expr) (Expr)
          | Equal (Expr) (Expr)
          | Lower (Expr) (Expr)
          | LowerE (Expr) (Expr)
          | And (Expr) (Expr)
          | Or (Expr) (Expr)
          | Impl (Expr) (Expr)
          | Not (Expr)
          | Repby (Expr) (Expr)
          | True_
          deriving ( Eq,Show)
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (Lit _i) =
    (sem_Expr_Lit _i)
sem_Expr (Name _var) =
    (sem_Expr_Name _var)
sem_Expr (ForAll _var _expr) =
    (sem_Expr_ForAll _var (sem_Expr _expr))
sem_Expr (Exists _var _expr) =
    (sem_Expr_Exists _var (sem_Expr _expr))
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
sem_Expr (Repby _expr1 _expr2) =
    (sem_Expr_Repby (sem_Expr _expr1) (sem_Expr _expr2))
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
sem_Expr_ForAll :: String ->
                   T_Expr ->
                   T_Expr
sem_Expr_ForAll var_ expr_ =
    (let
     in  ( ))
sem_Expr_Exists :: String ->
                   T_Expr ->
                   T_Expr
sem_Expr_Exists var_ expr_ =
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
                  T_Expr
sem_Expr_Repby expr1_ expr2_ =
    (let
     in  ( ))
sem_Expr_True_ :: T_Expr
sem_Expr_True_ =
    (let
     in  ( ))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( )
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( ) = sem
     in  (Syn_Exprs))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let
     in  ( ))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let
     in  ( ))
-- Stmt --------------------------------------------------------
data Stmt = Vars (([Var])) (Body)
          | Prog (String) (([Var])) (([Var])) (Body)
          | PCall (String) (Exprs) (Exprs)
          | Pre (Expr)
          | Post (Expr)
          | Inv (Expr) (Stmt)
          | While (Expr) (Body)
          | If (Expr) (Body) (Body)
          | Assign (Expr) (Expr)
          | Sim (Exprs) (Exprs)
          | Skip
          deriving ( Eq,Show)
-- cata
sem_Stmt :: Stmt ->
            T_Stmt
sem_Stmt (Vars _vars _body) =
    (sem_Stmt_Vars _vars (sem_Body _body))
sem_Stmt (Prog _name _params _results _body) =
    (sem_Stmt_Prog _name _params _results (sem_Body _body))
sem_Stmt (PCall _name _args _res) =
    (sem_Stmt_PCall _name (sem_Exprs _args) (sem_Exprs _res))
sem_Stmt (Pre _expr) =
    (sem_Stmt_Pre (sem_Expr _expr))
sem_Stmt (Post _expr) =
    (sem_Stmt_Post (sem_Expr _expr))
sem_Stmt (Inv _expr _stmt) =
    (sem_Stmt_Inv (sem_Expr _expr) (sem_Stmt _stmt))
sem_Stmt (While _expr _body) =
    (sem_Stmt_While (sem_Expr _expr) (sem_Body _body))
sem_Stmt (If _expr _left _right) =
    (sem_Stmt_If (sem_Expr _expr) (sem_Body _left) (sem_Body _right))
sem_Stmt (Assign _expr1 _expr2) =
    (sem_Stmt_Assign (sem_Expr _expr1) (sem_Expr _expr2))
sem_Stmt (Sim _left _right) =
    (sem_Stmt_Sim (sem_Exprs _left) (sem_Exprs _right))
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
sem_Stmt_Vars :: ([Var]) ->
                 T_Body ->
                 T_Stmt
sem_Stmt_Vars vars_ body_ =
    (let
     in  ( ))
sem_Stmt_Prog :: String ->
                 ([Var]) ->
                 ([Var]) ->
                 T_Body ->
                 T_Stmt
sem_Stmt_Prog name_ params_ results_ body_ =
    (let
     in  ( ))
sem_Stmt_PCall :: String ->
                  T_Exprs ->
                  T_Exprs ->
                  T_Stmt
sem_Stmt_PCall name_ args_ res_ =
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
                  T_Body ->
                  T_Stmt
sem_Stmt_While expr_ body_ =
    (let
     in  ( ))
sem_Stmt_If :: T_Expr ->
               T_Body ->
               T_Body ->
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
sem_Stmt_Sim :: T_Exprs ->
                T_Exprs ->
                T_Stmt
sem_Stmt_Sim left_ right_ =
    (let
     in  ( ))
sem_Stmt_Skip :: T_Stmt
sem_Stmt_Skip =
    (let
     in  ( ))
-- Var ---------------------------------------------------------
data Var = Int (String)
         | Array (String)
         | Univ (String)
         | Exis (String)
         deriving ( Eq,Ord,Show)
-- cata
sem_Var :: Var ->
           T_Var
sem_Var (Int _s) =
    (sem_Var_Int _s)
sem_Var (Array _s) =
    (sem_Var_Array _s)
sem_Var (Univ _s) =
    (sem_Var_Univ _s)
sem_Var (Exis _s) =
    (sem_Var_Exis _s)
-- semantic domain
type T_Var = ( )
data Inh_Var = Inh_Var {}
data Syn_Var = Syn_Var {}
wrap_Var :: T_Var ->
            Inh_Var ->
            Syn_Var
wrap_Var sem (Inh_Var) =
    (let ( ) = sem
     in  (Syn_Var))
sem_Var_Int :: String ->
               T_Var
sem_Var_Int s_ =
    (let
     in  ( ))
sem_Var_Array :: String ->
                 T_Var
sem_Var_Array s_ =
    (let
     in  ( ))
sem_Var_Univ :: String ->
                T_Var
sem_Var_Univ s_ =
    (let
     in  ( ))
sem_Var_Exis :: String ->
                T_Var
sem_Var_Exis s_ =
    (let
     in  ( ))