

-- UUAGC 0.9.52.1 (SyntaxTransformer.ag)
module SyntaxTransformer(
Stmt(..), Expr(..), stmt_trans
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.SBV(SInteger)

{-# LINE 15 "SyntaxTransformer.ag" #-}

stmt_trans :: Stmt -> Stmt
stmt_trans stmt = transformed_Syn_Top $ wrap_Top (sem_Top (Top stmt)) (Inh_Top)
{-# LINE 18 "SyntaxTransformer.hs" #-}
-- Body --------------------------------------------------------
type Body = [Stmt]
-- cata
sem_Body :: Body ->
            T_Body
sem_Body list =
    (Prelude.foldr sem_Body_Cons sem_Body_Nil (Prelude.map sem_Stmt list))
-- semantic domain
type T_Body = Int ->
              (M.Map String String) ->
              ([String]) ->
              ( (S.Set String),Int,Body)
data Inh_Body = Inh_Body {countInh_Inh_Body :: Int,varMap_Inh_Body :: (M.Map String String),vars_Inh_Body :: ([String])}
data Syn_Body = Syn_Body {allVars_Syn_Body :: (S.Set String),countSyn_Syn_Body :: Int,fresh_Syn_Body :: Body}
wrap_Body :: T_Body ->
             Inh_Body ->
             Syn_Body
wrap_Body sem (Inh_Body _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh) = sem _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Body _lhsOallVars _lhsOcountSyn _lhsOfresh))
sem_Body_Cons :: T_Stmt ->
                 T_Body ->
                 T_Body
sem_Body_Cons hd_ tl_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Body
              _lhsOcountSyn :: Int
              _hdOcountInh :: Int
              _tlOcountInh :: Int
              _hdOvars :: ([String])
              _tlOvars :: ([String])
              _hdOvarMap :: (M.Map String String)
              _tlOvarMap :: (M.Map String String)
              _hdIallVars :: (S.Set String)
              _hdIcountSyn :: Int
              _hdIfresh :: Stmt
              _tlIallVars :: (S.Set String)
              _tlIcountSyn :: Int
              _tlIfresh :: Body
              _lhsOallVars =
                  ({-# LINE 107 "SyntaxTransformer.ag" #-}
                   S.union _hdIallVars _tlIallVars
                   {-# LINE 64 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 108 "SyntaxTransformer.ag" #-}
                   _hdIfresh : _tlIfresh
                   {-# LINE 69 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 109 "SyntaxTransformer.ag" #-}
                   _tlIcountSyn
                   {-# LINE 74 "SyntaxTransformer.hs" #-}
                   )
              _hdOcountInh =
                  ({-# LINE 110 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 79 "SyntaxTransformer.hs" #-}
                   )
              _tlOcountInh =
                  ({-# LINE 111 "SyntaxTransformer.ag" #-}
                   _hdIcountSyn
                   {-# LINE 84 "SyntaxTransformer.hs" #-}
                   )
              _hdOvars =
                  ({-# LINE 112 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 89 "SyntaxTransformer.hs" #-}
                   )
              _tlOvars =
                  ({-# LINE 113 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 94 "SyntaxTransformer.hs" #-}
                   )
              _hdOvarMap =
                  ({-# LINE 114 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 99 "SyntaxTransformer.hs" #-}
                   )
              _tlOvarMap =
                  ({-# LINE 115 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 104 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   (:) _hdIfresh _tlIfresh
                   {-# LINE 109 "SyntaxTransformer.hs" #-}
                   )
              ( _hdIallVars,_hdIcountSyn,_hdIfresh) =
                  hd_ _hdOcountInh _hdOvarMap _hdOvars
              ( _tlIallVars,_tlIcountSyn,_tlIfresh) =
                  tl_ _tlOcountInh _tlOvarMap _tlOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Body_Nil :: T_Body
sem_Body_Nil =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Body
              _lhsOcountSyn :: Int
              _lhsOallVars =
                  ({-# LINE 116 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 127 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 117 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 132 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 118 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 137 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 142 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
-- Expr --------------------------------------------------------
data Expr = Lit (SInteger)
          | Name (String)
          | PCall (String) (Exprs)
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
sem_Expr (PCall _name _args) =
    (sem_Expr_PCall _name (sem_Exprs _args))
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
sem_Expr (Repby _expr1 _expr2) =
    (sem_Expr_Repby (sem_Expr _expr1) (sem_Expr _expr2))
sem_Expr (True_) =
    (sem_Expr_True_)
-- semantic domain
type T_Expr = Int ->
              (M.Map String String) ->
              ([String]) ->
              ( (S.Set String),Int,Expr)
data Inh_Expr = Inh_Expr {countInh_Inh_Expr :: Int,varMap_Inh_Expr :: (M.Map String String),vars_Inh_Expr :: ([String])}
data Syn_Expr = Syn_Expr {allVars_Syn_Expr :: (S.Set String),countSyn_Syn_Expr :: Int,fresh_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh) = sem _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Expr _lhsOallVars _lhsOcountSyn _lhsOfresh))
sem_Expr_Lit :: SInteger ->
                T_Expr
sem_Expr_Lit i_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _lhsOallVars =
                  ({-# LINE 135 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 220 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 136 "SyntaxTransformer.ag" #-}
                   Lit i_
                   {-# LINE 225 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 137 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 230 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Lit i_
                   {-# LINE 235 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Name :: String ->
                 T_Expr
sem_Expr_Name var_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _lhsOallVars =
                  ({-# LINE 139 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 250 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 140 "SyntaxTransformer.ag" #-}
                   case M.lookup var_ _lhsIvarMap of
                       Nothing -> Name var_
                       Just s -> Name s
                   {-# LINE 257 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 143 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 262 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Name var_
                   {-# LINE 267 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_PCall :: String ->
                  T_Exprs ->
                  T_Expr
sem_Expr_PCall name_ args_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _argsOcountInh :: Int
              _argsOvarMap :: (M.Map String String)
              _argsOvars :: ([String])
              _argsIallVars :: (S.Set String)
              _argsIcountSyn :: Int
              _argsIfresh :: Exprs
              _lhsOallVars =
                  ({-# LINE 144 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 289 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 145 "SyntaxTransformer.ag" #-}
                   PCall name_ _argsIfresh
                   {-# LINE 294 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 146 "SyntaxTransformer.ag" #-}
                   _argsIcountSyn
                   {-# LINE 299 "SyntaxTransformer.hs" #-}
                   )
              _argsOcountInh =
                  ({-# LINE 147 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 304 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   PCall name_ _argsIfresh
                   {-# LINE 309 "SyntaxTransformer.hs" #-}
                   )
              _argsOvarMap =
                  ({-# LINE 37 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 314 "SyntaxTransformer.hs" #-}
                   )
              _argsOvars =
                  ({-# LINE 36 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 319 "SyntaxTransformer.hs" #-}
                   )
              ( _argsIallVars,_argsIcountSyn,_argsIfresh) =
                  args_ _argsOcountInh _argsOvarMap _argsOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_ForAll :: String ->
                   T_Expr ->
                   T_Expr
sem_Expr_ForAll var_ expr_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _exprOcountInh :: Int
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _lhsOallVars =
                  ({-# LINE 148 "SyntaxTransformer.ag" #-}
                   S.singleton var_
                   {-# LINE 343 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 149 "SyntaxTransformer.ag" #-}
                   ForAll _lookup     _exprIfresh
                   {-# LINE 348 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 150 "SyntaxTransformer.ag" #-}
                   _exprIcountSyn
                   {-# LINE 353 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 151 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh + 1
                   {-# LINE 358 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 152 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 363 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 153 "SyntaxTransformer.ag" #-}
                   case _lookup     == var_ of
                     True -> M.insert var_ var_ _lhsIvarMap
                     False -> M.adjust (\_ -> _lookup    ) var_ _lhsIvarMap
                   {-# LINE 370 "SyntaxTransformer.hs" #-}
                   )
              _lookup =
                  ({-# LINE 157 "SyntaxTransformer.ag" #-}
                   case L.find(== var_) _lhsIvars of
                      Nothing -> var_
                      Just s -> s ++ (show _lhsIcountInh)
                   {-# LINE 377 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   ForAll var_ _exprIfresh
                   {-# LINE 382 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Minus :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Minus expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 160 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 412 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 161 "SyntaxTransformer.ag" #-}
                   Minus _expr1Ifresh _expr2Ifresh
                   {-# LINE 417 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 162 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 422 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 163 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 427 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 164 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 432 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 165 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 437 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 166 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 442 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 167 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 447 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 168 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 452 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Minus _expr1Ifresh _expr2Ifresh
                   {-# LINE 457 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Plus :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Plus expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 169 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 489 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 170 "SyntaxTransformer.ag" #-}
                   Plus _expr1Ifresh _expr2Ifresh
                   {-# LINE 494 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 171 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 499 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 172 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 504 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 173 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 509 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 174 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 514 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 175 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 519 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 176 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 524 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 177 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 529 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Plus _expr1Ifresh _expr2Ifresh
                   {-# LINE 534 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Equal :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Equal expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 178 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 566 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 179 "SyntaxTransformer.ag" #-}
                   Equal _expr1Ifresh _expr2Ifresh
                   {-# LINE 571 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 180 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 576 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 181 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 581 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 182 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 586 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 183 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 591 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 184 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 596 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 185 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 601 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 186 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 606 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Equal _expr1Ifresh _expr2Ifresh
                   {-# LINE 611 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Lower :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Lower expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 187 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 643 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 188 "SyntaxTransformer.ag" #-}
                   Lower _expr1Ifresh _expr2Ifresh
                   {-# LINE 648 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 189 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 653 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 190 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 658 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 191 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 663 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 192 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 668 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 193 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 673 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 194 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 678 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 195 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 683 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Lower _expr1Ifresh _expr2Ifresh
                   {-# LINE 688 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_LowerE :: T_Expr ->
                   T_Expr ->
                   T_Expr
sem_Expr_LowerE expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 196 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 720 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 197 "SyntaxTransformer.ag" #-}
                   LowerE _expr1Ifresh _expr2Ifresh
                   {-# LINE 725 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 198 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 730 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 199 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 735 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 200 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 740 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 201 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 745 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 202 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 750 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 203 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 755 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 204 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 760 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   LowerE _expr1Ifresh _expr2Ifresh
                   {-# LINE 765 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_And :: T_Expr ->
                T_Expr ->
                T_Expr
sem_Expr_And expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 205 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 797 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 206 "SyntaxTransformer.ag" #-}
                   And _expr1Ifresh _expr2Ifresh
                   {-# LINE 802 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 207 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 807 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 208 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 812 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 209 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 817 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 210 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 822 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 211 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 827 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 212 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 832 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 213 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 837 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   And _expr1Ifresh _expr2Ifresh
                   {-# LINE 842 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Or :: T_Expr ->
               T_Expr ->
               T_Expr
sem_Expr_Or expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 214 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 874 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 215 "SyntaxTransformer.ag" #-}
                   Or _expr1Ifresh _expr2Ifresh
                   {-# LINE 879 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 216 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 884 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 217 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 889 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 218 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 894 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 219 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 899 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 220 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 904 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 221 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 909 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 222 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 914 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Or _expr1Ifresh _expr2Ifresh
                   {-# LINE 919 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Impl :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Impl expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 223 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 951 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 224 "SyntaxTransformer.ag" #-}
                   Impl _expr1Ifresh _expr2Ifresh
                   {-# LINE 956 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 225 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 961 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 226 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 966 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 227 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 971 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 228 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 976 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 229 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 981 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 230 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 986 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 231 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 991 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Impl _expr1Ifresh _expr2Ifresh
                   {-# LINE 996 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Not :: T_Expr ->
                T_Expr
sem_Expr_Not expr_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _exprOcountInh :: Int
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _lhsOallVars =
                  ({-# LINE 232 "SyntaxTransformer.ag" #-}
                   _exprIallVars
                   {-# LINE 1021 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 233 "SyntaxTransformer.ag" #-}
                   Not _exprIfresh
                   {-# LINE 1026 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 234 "SyntaxTransformer.ag" #-}
                   _exprIcountSyn
                   {-# LINE 1031 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 235 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1036 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 236 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1041 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 237 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1046 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Not _exprIfresh
                   {-# LINE 1051 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_Repby :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Repby expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 238 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 1081 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 239 "SyntaxTransformer.ag" #-}
                   Repby _expr1Ifresh _expr2Ifresh
                   {-# LINE 1086 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 240 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 1091 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 241 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1096 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 242 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 1101 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 243 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1106 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 244 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1111 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 245 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1116 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 246 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1121 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Repby _expr1Ifresh _expr2Ifresh
                   {-# LINE 1126 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Expr_True_ :: T_Expr
sem_Expr_True_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _lhsOallVars =
                  ({-# LINE 247 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1144 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 248 "SyntaxTransformer.ag" #-}
                   True_
                   {-# LINE 1149 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 249 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1154 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   True_
                   {-# LINE 1159 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = Int ->
               (M.Map String String) ->
               ([String]) ->
               ( (S.Set String),Int,Exprs)
data Inh_Exprs = Inh_Exprs {countInh_Inh_Exprs :: Int,varMap_Inh_Exprs :: (M.Map String String),vars_Inh_Exprs :: ([String])}
data Syn_Exprs = Syn_Exprs {allVars_Syn_Exprs :: (S.Set String),countSyn_Syn_Exprs :: Int,fresh_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh) = sem _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Exprs _lhsOallVars _lhsOcountSyn _lhsOfresh))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Exprs
              _lhsOcountSyn :: Int
              _hdOcountInh :: Int
              _tlOcountInh :: Int
              _hdOvars :: ([String])
              _tlOvars :: ([String])
              _hdOvarMap :: (M.Map String String)
              _tlOvarMap :: (M.Map String String)
              _hdIallVars :: (S.Set String)
              _hdIcountSyn :: Int
              _hdIfresh :: Expr
              _tlIallVars :: (S.Set String)
              _tlIcountSyn :: Int
              _tlIfresh :: Exprs
              _lhsOallVars =
                  ({-# LINE 121 "SyntaxTransformer.ag" #-}
                   S.union _hdIallVars _tlIallVars
                   {-# LINE 1207 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 122 "SyntaxTransformer.ag" #-}
                   _hdIfresh : _tlIfresh
                   {-# LINE 1212 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 123 "SyntaxTransformer.ag" #-}
                   _tlIcountSyn
                   {-# LINE 1217 "SyntaxTransformer.hs" #-}
                   )
              _hdOcountInh =
                  ({-# LINE 124 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1222 "SyntaxTransformer.hs" #-}
                   )
              _tlOcountInh =
                  ({-# LINE 125 "SyntaxTransformer.ag" #-}
                   _hdIcountSyn
                   {-# LINE 1227 "SyntaxTransformer.hs" #-}
                   )
              _hdOvars =
                  ({-# LINE 126 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1232 "SyntaxTransformer.hs" #-}
                   )
              _tlOvars =
                  ({-# LINE 127 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1237 "SyntaxTransformer.hs" #-}
                   )
              _hdOvarMap =
                  ({-# LINE 128 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1242 "SyntaxTransformer.hs" #-}
                   )
              _tlOvarMap =
                  ({-# LINE 129 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1247 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   (:) _hdIfresh _tlIfresh
                   {-# LINE 1252 "SyntaxTransformer.hs" #-}
                   )
              ( _hdIallVars,_hdIcountSyn,_hdIfresh) =
                  hd_ _hdOcountInh _hdOvarMap _hdOvars
              ( _tlIallVars,_tlIcountSyn,_tlIfresh) =
                  tl_ _tlOcountInh _tlOvarMap _tlOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Exprs
              _lhsOcountSyn :: Int
              _lhsOallVars =
                  ({-# LINE 130 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1270 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 131 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 1275 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 132 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1280 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 1285 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
-- Stmt --------------------------------------------------------
data Stmt = Var (([String])) (Body)
          | Prog (String) (([String])) (Body)
          | Pre (Expr)
          | Post (Expr)
          | Inv (Expr) (Stmt)
          | While (Expr) (Body)
          | If (Expr) (Body) (Body)
          | Assign (Expr) (Expr)
          | Skip
          deriving ( Eq,Show)
-- cata
sem_Stmt :: Stmt ->
            T_Stmt
sem_Stmt (Var _vars _body) =
    (sem_Stmt_Var _vars (sem_Body _body))
sem_Stmt (Prog _name _params _body) =
    (sem_Stmt_Prog _name _params (sem_Body _body))
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
sem_Stmt (Skip) =
    (sem_Stmt_Skip)
-- semantic domain
type T_Stmt = Int ->
              (M.Map String String) ->
              ([String]) ->
              ( (S.Set String),Int,Stmt)
data Inh_Stmt = Inh_Stmt {countInh_Inh_Stmt :: Int,varMap_Inh_Stmt :: (M.Map String String),vars_Inh_Stmt :: ([String])}
data Syn_Stmt = Syn_Stmt {allVars_Syn_Stmt :: (S.Set String),countSyn_Syn_Stmt :: Int,fresh_Syn_Stmt :: Stmt}
wrap_Stmt :: T_Stmt ->
             Inh_Stmt ->
             Syn_Stmt
wrap_Stmt sem (Inh_Stmt _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh) = sem _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Stmt _lhsOallVars _lhsOcountSyn _lhsOfresh))
sem_Stmt_Var :: ([String]) ->
                T_Body ->
                T_Stmt
sem_Stmt_Var vars_ body_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _bodyOcountInh :: Int
              _lhsOfresh :: Stmt
              _bodyOvars :: ([String])
              _bodyOvarMap :: (M.Map String String)
              _bodyIallVars :: (S.Set String)
              _bodyIcountSyn :: Int
              _bodyIfresh :: Body
              _lhsOallVars =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   S.union (S.fromList vars_) _bodyIallVars
                   {-# LINE 1352 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 41 "SyntaxTransformer.ag" #-}
                   _bodyIcountSyn
                   {-# LINE 1357 "SyntaxTransformer.hs" #-}
                   )
              _bodyOcountInh =
                  ({-# LINE 42 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh + 1
                   {-# LINE 1362 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 43 "SyntaxTransformer.ag" #-}
                   Var (L.union _remVars     _freshVars    ) _bodyIfresh
                   {-# LINE 1367 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvars =
                  ({-# LINE 44 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1372 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvarMap =
                  ({-# LINE 45 "SyntaxTransformer.ag" #-}
                   M.union _freshVarsMap     _lhsIvarMap
                   {-# LINE 1377 "SyntaxTransformer.hs" #-}
                   )
              _dupVars =
                  ({-# LINE 46 "SyntaxTransformer.ag" #-}
                   L.intersect _lhsIvars vars_
                   {-# LINE 1382 "SyntaxTransformer.hs" #-}
                   )
              _freshVars =
                  ({-# LINE 47 "SyntaxTransformer.ag" #-}
                   map (++ show _lhsIcountInh) _dupVars
                   {-# LINE 1387 "SyntaxTransformer.hs" #-}
                   )
              _freshVarsMap =
                  ({-# LINE 48 "SyntaxTransformer.ag" #-}
                   M.fromList $ zip _dupVars     _freshVars
                   {-# LINE 1392 "SyntaxTransformer.hs" #-}
                   )
              _remVars =
                  ({-# LINE 49 "SyntaxTransformer.ag" #-}
                   vars_ L.\\ _dupVars
                   {-# LINE 1397 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Var vars_ _bodyIfresh
                   {-# LINE 1402 "SyntaxTransformer.hs" #-}
                   )
              ( _bodyIallVars,_bodyIcountSyn,_bodyIfresh) =
                  body_ _bodyOcountInh _bodyOvarMap _bodyOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_Prog :: String ->
                 ([String]) ->
                 T_Body ->
                 T_Stmt
sem_Stmt_Prog name_ params_ body_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _bodyOcountInh :: Int
              _lhsOfresh :: Stmt
              _bodyOvarMap :: (M.Map String String)
              _bodyOvars :: ([String])
              _bodyIallVars :: (S.Set String)
              _bodyIcountSyn :: Int
              _bodyIfresh :: Body
              _lhsOallVars =
                  ({-# LINE 50 "SyntaxTransformer.ag" #-}
                   _bodyIallVars
                   {-# LINE 1427 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 51 "SyntaxTransformer.ag" #-}
                   _bodyIcountSyn
                   {-# LINE 1432 "SyntaxTransformer.hs" #-}
                   )
              _bodyOcountInh =
                  ({-# LINE 52 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh + 1
                   {-# LINE 1437 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 53 "SyntaxTransformer.ag" #-}
                   Prog name_ (L.union _remVars     _freshVars    ) _bodyIfresh
                   {-# LINE 1442 "SyntaxTransformer.hs" #-}
                   )
              _dupVars =
                  ({-# LINE 54 "SyntaxTransformer.ag" #-}
                   L.intersect _lhsIvars params_
                   {-# LINE 1447 "SyntaxTransformer.hs" #-}
                   )
              _freshVars =
                  ({-# LINE 55 "SyntaxTransformer.ag" #-}
                   map (++ show _lhsIcountInh) _dupVars
                   {-# LINE 1452 "SyntaxTransformer.hs" #-}
                   )
              _freshVarsMap =
                  ({-# LINE 56 "SyntaxTransformer.ag" #-}
                   M.fromList $ zip _dupVars     _freshVars
                   {-# LINE 1457 "SyntaxTransformer.hs" #-}
                   )
              _remVars =
                  ({-# LINE 57 "SyntaxTransformer.ag" #-}
                   params_ L.\\ _dupVars
                   {-# LINE 1462 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Prog name_ params_ _bodyIfresh
                   {-# LINE 1467 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvarMap =
                  ({-# LINE 37 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1472 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvars =
                  ({-# LINE 36 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1477 "SyntaxTransformer.hs" #-}
                   )
              ( _bodyIallVars,_bodyIcountSyn,_bodyIfresh) =
                  body_ _bodyOcountInh _bodyOvarMap _bodyOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_Pre :: T_Expr ->
                T_Stmt
sem_Stmt_Pre expr_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _exprOcountInh :: Int
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _lhsOallVars =
                  ({-# LINE 58 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1500 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 59 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1505 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 60 "SyntaxTransformer.ag" #-}
                   Pre _exprIfresh
                   {-# LINE 1510 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 61 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1515 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 62 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1520 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Pre _exprIfresh
                   {-# LINE 1525 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 34 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1530 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_Post :: T_Expr ->
                 T_Stmt
sem_Stmt_Post expr_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _exprOcountInh :: Int
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _lhsOallVars =
                  ({-# LINE 63 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1553 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 64 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1558 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 65 "SyntaxTransformer.ag" #-}
                   Post _exprIfresh
                   {-# LINE 1563 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 66 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1568 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 67 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1573 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Post _exprIfresh
                   {-# LINE 1578 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 34 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1583 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_Inv :: T_Expr ->
                T_Stmt ->
                T_Stmt
sem_Stmt_Inv expr_ stmt_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _stmtOcountInh :: Int
              _stmtOvars :: ([String])
              _stmtOvarMap :: (M.Map String String)
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _exprOcountInh :: Int
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _stmtIallVars :: (S.Set String)
              _stmtIcountSyn :: Int
              _stmtIfresh :: Stmt
              _lhsOallVars =
                  ({-# LINE 68 "SyntaxTransformer.ag" #-}
                   _stmtIallVars
                   {-# LINE 1613 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 69 "SyntaxTransformer.ag" #-}
                   _stmtIcountSyn
                   {-# LINE 1618 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 70 "SyntaxTransformer.ag" #-}
                   Inv _exprIfresh _stmtIfresh
                   {-# LINE 1623 "SyntaxTransformer.hs" #-}
                   )
              _stmtOcountInh =
                  ({-# LINE 71 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1628 "SyntaxTransformer.hs" #-}
                   )
              _stmtOvars =
                  ({-# LINE 72 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1633 "SyntaxTransformer.hs" #-}
                   )
              _stmtOvarMap =
                  ({-# LINE 73 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1638 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 74 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1643 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 75 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1648 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Inv _exprIfresh _stmtIfresh
                   {-# LINE 1653 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 34 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1658 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
              ( _stmtIallVars,_stmtIcountSyn,_stmtIfresh) =
                  stmt_ _stmtOcountInh _stmtOvarMap _stmtOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_While :: T_Expr ->
                  T_Body ->
                  T_Stmt
sem_Stmt_While expr_ body_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _bodyOcountInh :: Int
              _bodyOvars :: ([String])
              _bodyOvarMap :: (M.Map String String)
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _exprOcountInh :: Int
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _bodyIallVars :: (S.Set String)
              _bodyIcountSyn :: Int
              _bodyIfresh :: Body
              _lhsOallVars =
                  ({-# LINE 76 "SyntaxTransformer.ag" #-}
                   _bodyIallVars
                   {-# LINE 1690 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 77 "SyntaxTransformer.ag" #-}
                   _bodyIcountSyn
                   {-# LINE 1695 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 78 "SyntaxTransformer.ag" #-}
                   While _exprIfresh _bodyIfresh
                   {-# LINE 1700 "SyntaxTransformer.hs" #-}
                   )
              _bodyOcountInh =
                  ({-# LINE 79 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1705 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvars =
                  ({-# LINE 80 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1710 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvarMap =
                  ({-# LINE 81 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1715 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 82 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1720 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 83 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1725 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   While _exprIfresh _bodyIfresh
                   {-# LINE 1730 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 34 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1735 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
              ( _bodyIallVars,_bodyIcountSyn,_bodyIfresh) =
                  body_ _bodyOcountInh _bodyOvarMap _bodyOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_If :: T_Expr ->
               T_Body ->
               T_Body ->
               T_Stmt
sem_Stmt_If expr_ left_ right_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _leftOcountInh :: Int
              _rightOcountInh :: Int
              _lhsOfresh :: Stmt
              _leftOvars :: ([String])
              _leftOvarMap :: (M.Map String String)
              _rightOvars :: ([String])
              _rightOvarMap :: (M.Map String String)
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _exprOcountInh :: Int
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _leftIallVars :: (S.Set String)
              _leftIcountSyn :: Int
              _leftIfresh :: Body
              _rightIallVars :: (S.Set String)
              _rightIcountSyn :: Int
              _rightIfresh :: Body
              _lhsOallVars =
                  ({-# LINE 84 "SyntaxTransformer.ag" #-}
                   S.union _leftIallVars _rightIallVars
                   {-# LINE 1774 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 85 "SyntaxTransformer.ag" #-}
                   _rightIcountSyn
                   {-# LINE 1779 "SyntaxTransformer.hs" #-}
                   )
              _leftOcountInh =
                  ({-# LINE 86 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1784 "SyntaxTransformer.hs" #-}
                   )
              _rightOcountInh =
                  ({-# LINE 87 "SyntaxTransformer.ag" #-}
                   _leftIcountSyn
                   {-# LINE 1789 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 88 "SyntaxTransformer.ag" #-}
                   If _exprIfresh _leftIfresh _rightIfresh
                   {-# LINE 1794 "SyntaxTransformer.hs" #-}
                   )
              _leftOvars =
                  ({-# LINE 89 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1799 "SyntaxTransformer.hs" #-}
                   )
              _leftOvarMap =
                  ({-# LINE 90 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1804 "SyntaxTransformer.hs" #-}
                   )
              _rightOvars =
                  ({-# LINE 91 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1809 "SyntaxTransformer.hs" #-}
                   )
              _rightOvarMap =
                  ({-# LINE 92 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1814 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 93 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1819 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 94 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1824 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   If _exprIfresh _leftIfresh _rightIfresh
                   {-# LINE 1829 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 34 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1834 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
              ( _leftIallVars,_leftIcountSyn,_leftIfresh) =
                  left_ _leftOcountInh _leftOvarMap _leftOvars
              ( _rightIallVars,_rightIcountSyn,_rightIfresh) =
                  right_ _rightOcountInh _rightOvarMap _rightOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_Assign :: T_Expr ->
                   T_Expr ->
                   T_Stmt
sem_Stmt_Assign expr1_ expr2_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _lhsOallVars =
                  ({-# LINE 95 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1868 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 96 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1873 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 97 "SyntaxTransformer.ag" #-}
                   Assign _expr1Ifresh _expr2Ifresh
                   {-# LINE 1878 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 98 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1883 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 99 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1888 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 100 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1893 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 101 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1898 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Assign _expr1Ifresh _expr2Ifresh
                   {-# LINE 1903 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 34 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1908 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 34 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1913 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
sem_Stmt_Skip :: T_Stmt
sem_Stmt_Skip =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _lhsOallVars =
                  ({-# LINE 102 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1931 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 103 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1936 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 104 "SyntaxTransformer.ag" #-}
                   Skip
                   {-# LINE 1941 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 33 "SyntaxTransformer.ag" #-}
                   Skip
                   {-# LINE 1946 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh)))
-- Top ---------------------------------------------------------
data Top = Top (Stmt)
-- cata
sem_Top :: Top ->
           T_Top
sem_Top (Top _stmt) =
    (sem_Top_Top (sem_Stmt _stmt))
-- semantic domain
type T_Top = ( Stmt)
data Inh_Top = Inh_Top {}
data Syn_Top = Syn_Top {transformed_Syn_Top :: Stmt}
wrap_Top :: T_Top ->
            Inh_Top ->
            Syn_Top
wrap_Top sem (Inh_Top) =
    (let ( _lhsOtransformed) = sem
     in  (Syn_Top _lhsOtransformed))
sem_Top_Top :: T_Stmt ->
               T_Top
sem_Top_Top stmt_ =
    (let _lhsOtransformed :: Stmt
         _stmtOvarMap :: (M.Map String String)
         _stmtOvars :: ([String])
         _stmtOcountInh :: Int
         _stmtIallVars :: (S.Set String)
         _stmtIcountSyn :: Int
         _stmtIfresh :: Stmt
         _lhsOtransformed =
             ({-# LINE 26 "SyntaxTransformer.ag" #-}
              _stmtIfresh
              {-# LINE 1979 "SyntaxTransformer.hs" #-}
              )
         _stmtOvarMap =
             ({-# LINE 27 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 1984 "SyntaxTransformer.hs" #-}
              )
         _stmtOvars =
             ({-# LINE 28 "SyntaxTransformer.ag" #-}
              S.toList _stmtIallVars
              {-# LINE 1989 "SyntaxTransformer.hs" #-}
              )
         _stmtOcountInh =
             ({-# LINE 29 "SyntaxTransformer.ag" #-}
              0
              {-# LINE 1994 "SyntaxTransformer.hs" #-}
              )
         ( _stmtIallVars,_stmtIcountSyn,_stmtIfresh) =
             stmt_ _stmtOcountInh _stmtOvarMap _stmtOvars
     in  ( _lhsOtransformed))