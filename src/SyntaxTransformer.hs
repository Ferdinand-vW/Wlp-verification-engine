

-- UUAGC 0.9.52.1 (SyntaxTransformer.ag)
module SyntaxTransformer(
Stmt(..), Expr(..), ProgramInfo(..), stmt_ProgramInfo
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.SBV(SInteger)

{-# LINE 16 "SyntaxTransformer.ag" #-}

stmt_ProgramInfo :: Stmt -> ProgramInfo
stmt_ProgramInfo stmt = pInfo_Syn_Top $ wrap_Top (sem_Top (Top stmt)) Inh_Top
{-# LINE 19 "SyntaxTransformer.hs" #-}
-- Body --------------------------------------------------------
type Body = [Stmt]
-- cata
sem_Body :: Body ->
            T_Body
sem_Body list =
    (Prelude.foldr sem_Body_Cons sem_Body_Nil (Prelude.map sem_Stmt list))
-- semantic domain
type T_Body = ( (M.Map String ([String],[String],Body)),(S.Set String),Body)
data Inh_Body = Inh_Body {}
data Syn_Body = Syn_Body {allProgs_Syn_Body :: (M.Map String ([String],[String],Body)),allVars_Syn_Body :: (S.Set String),copy_Syn_Body :: Body}
wrap_Body :: T_Body ->
             Inh_Body ->
             Syn_Body
wrap_Body sem (Inh_Body) =
    (let ( _lhsOallProgs,_lhsOallVars,_lhsOcopy) = sem
     in  (Syn_Body _lhsOallProgs _lhsOallVars _lhsOcopy))
sem_Body_Cons :: T_Stmt ->
                 T_Body ->
                 T_Body
sem_Body_Cons hd_ tl_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Body
         _hdIallProgs :: (M.Map String ([String],[String],Body))
         _hdIallVars :: (S.Set String)
         _hdIcopy :: Stmt
         _tlIallProgs :: (M.Map String ([String],[String],Body))
         _tlIallVars :: (S.Set String)
         _tlIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 63 "SyntaxTransformer.ag" #-}
              M.union _hdIallProgs _tlIallProgs
              {-# LINE 53 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 64 "SyntaxTransformer.ag" #-}
              S.union _hdIallVars _tlIallVars
              {-# LINE 58 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              (:) _hdIcopy _tlIcopy
              {-# LINE 63 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 68 "SyntaxTransformer.hs" #-}
              )
         ( _hdIallProgs,_hdIallVars,_hdIcopy) =
             hd_
         ( _tlIallProgs,_tlIallVars,_tlIcopy) =
             tl_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Body_Nil :: T_Body
sem_Body_Nil =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Body
         _lhsOallProgs =
             ({-# LINE 65 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 83 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 66 "SyntaxTransformer.ag" #-}
              S.empty
              {-# LINE 88 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              []
              {-# LINE 93 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 98 "SyntaxTransformer.hs" #-}
              )
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
-- Expr --------------------------------------------------------
data Expr = Lit (SInteger)
          | Name (String)
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
type T_Expr = ( (S.Set String),Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {allVars_Syn_Expr :: (S.Set String),copy_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOallVars,_lhsOcopy) = sem
     in  (Syn_Expr _lhsOallVars _lhsOcopy))
sem_Expr_Lit :: SInteger ->
                T_Expr
sem_Expr_Lit i_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _lhsOallVars =
             ({-# LINE 69 "SyntaxTransformer.ag" #-}
              S.empty
              {-# LINE 166 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Lit i_
              {-# LINE 171 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 176 "SyntaxTransformer.hs" #-}
              )
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Name :: String ->
                 T_Expr
sem_Expr_Name var_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _lhsOallVars =
             ({-# LINE 70 "SyntaxTransformer.ag" #-}
              S.singleton var_
              {-# LINE 187 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Name var_
              {-# LINE 192 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 197 "SyntaxTransformer.hs" #-}
              )
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_ForAll :: String ->
                   T_Expr ->
                   T_Expr
sem_Expr_ForAll var_ expr_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _exprIallVars :: (S.Set String)
         _exprIcopy :: Expr
         _lhsOallVars =
             ({-# LINE 71 "SyntaxTransformer.ag" #-}
              S.union (S.singleton var_) _exprIallVars
              {-# LINE 211 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              ForAll var_ _exprIcopy
              {-# LINE 216 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 221 "SyntaxTransformer.hs" #-}
              )
         ( _exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Minus :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Minus expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 72 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 239 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Minus _expr1Icopy _expr2Icopy
              {-# LINE 244 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 249 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Plus :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Plus expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 73 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 269 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Plus _expr1Icopy _expr2Icopy
              {-# LINE 274 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 279 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Equal :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Equal expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 74 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 299 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Equal _expr1Icopy _expr2Icopy
              {-# LINE 304 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 309 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Lower :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Lower expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 75 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 329 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Lower _expr1Icopy _expr2Icopy
              {-# LINE 334 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 339 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_LowerE :: T_Expr ->
                   T_Expr ->
                   T_Expr
sem_Expr_LowerE expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 76 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 359 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              LowerE _expr1Icopy _expr2Icopy
              {-# LINE 364 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 369 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_And :: T_Expr ->
                T_Expr ->
                T_Expr
sem_Expr_And expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 77 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 389 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              And _expr1Icopy _expr2Icopy
              {-# LINE 394 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 399 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Or :: T_Expr ->
               T_Expr ->
               T_Expr
sem_Expr_Or expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 78 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 419 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Or _expr1Icopy _expr2Icopy
              {-# LINE 424 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 429 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Impl :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Impl expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 79 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 449 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Impl _expr1Icopy _expr2Icopy
              {-# LINE 454 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 459 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Not :: T_Expr ->
                T_Expr
sem_Expr_Not expr_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _exprIallVars :: (S.Set String)
         _exprIcopy :: Expr
         _lhsOallVars =
             ({-# LINE 81 "SyntaxTransformer.ag" #-}
              _exprIallVars
              {-# LINE 476 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Not _exprIcopy
              {-# LINE 481 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 486 "SyntaxTransformer.hs" #-}
              )
         ( _exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_Repby :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Repby expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 80 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 504 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Repby _expr1Icopy _expr2Icopy
              {-# LINE 509 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 514 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Expr_True_ :: T_Expr
sem_Expr_True_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Expr
         _lhsOallVars =
             ({-# LINE 82 "SyntaxTransformer.ag" #-}
              S.empty
              {-# LINE 528 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              True_
              {-# LINE 533 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 538 "SyntaxTransformer.hs" #-}
              )
     in  ( _lhsOallVars,_lhsOcopy))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( (S.Set String),Exprs)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {allVars_Syn_Exprs :: (S.Set String),copy_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOallVars,_lhsOcopy) = sem
     in  (Syn_Exprs _lhsOallVars _lhsOcopy))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Exprs
         _hdIallVars :: (S.Set String)
         _hdIcopy :: Expr
         _tlIallVars :: (S.Set String)
         _tlIcopy :: Exprs
         _lhsOallVars =
             ({-# LINE 85 "SyntaxTransformer.ag" #-}
              S.union _hdIallVars _tlIallVars
              {-# LINE 571 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              (:) _hdIcopy _tlIcopy
              {-# LINE 576 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 581 "SyntaxTransformer.hs" #-}
              )
         ( _hdIallVars,_hdIcopy) =
             hd_
         ( _tlIallVars,_tlIcopy) =
             tl_
     in  ( _lhsOallVars,_lhsOcopy))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Exprs
         _lhsOallVars =
             ({-# LINE 86 "SyntaxTransformer.ag" #-}
              S.empty
              {-# LINE 595 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              []
              {-# LINE 600 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 605 "SyntaxTransformer.hs" #-}
              )
     in  ( _lhsOallVars,_lhsOcopy))
-- ProgramInfo -------------------------------------------------
data ProgramInfo = ProgramInfo ((S.Set String)) ((M.Map String ([String],[String],Body)))
-- cata
sem_ProgramInfo :: ProgramInfo ->
                   T_ProgramInfo
sem_ProgramInfo (ProgramInfo _varMap _progMap) =
    (sem_ProgramInfo_ProgramInfo _varMap _progMap)
-- semantic domain
type T_ProgramInfo = ( )
data Inh_ProgramInfo = Inh_ProgramInfo {}
data Syn_ProgramInfo = Syn_ProgramInfo {}
wrap_ProgramInfo :: T_ProgramInfo ->
                    Inh_ProgramInfo ->
                    Syn_ProgramInfo
wrap_ProgramInfo sem (Inh_ProgramInfo) =
    (let ( ) = sem
     in  (Syn_ProgramInfo))
sem_ProgramInfo_ProgramInfo :: (S.Set String) ->
                               (M.Map String ([String],[String],Body)) ->
                               T_ProgramInfo
sem_ProgramInfo_ProgramInfo varMap_ progMap_ =
    (let
     in  ( ))
-- Stmt --------------------------------------------------------
data Stmt = Var (([String])) (Body)
          | Prog (String) (([String])) (([String])) (Body)
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
sem_Stmt (Var _vars _body) =
    (sem_Stmt_Var _vars (sem_Body _body))
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
type T_Stmt = ( (M.Map String ([String],[String],Body)),(S.Set String),Stmt)
data Inh_Stmt = Inh_Stmt {}
data Syn_Stmt = Syn_Stmt {allProgs_Syn_Stmt :: (M.Map String ([String],[String],Body)),allVars_Syn_Stmt :: (S.Set String),copy_Syn_Stmt :: Stmt}
wrap_Stmt :: T_Stmt ->
             Inh_Stmt ->
             Syn_Stmt
wrap_Stmt sem (Inh_Stmt) =
    (let ( _lhsOallProgs,_lhsOallVars,_lhsOcopy) = sem
     in  (Syn_Stmt _lhsOallProgs _lhsOallVars _lhsOcopy))
sem_Stmt_Var :: ([String]) ->
                T_Body ->
                T_Stmt
sem_Stmt_Var vars_ body_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _bodyIallProgs :: (M.Map String ([String],[String],Body))
         _bodyIallVars :: (S.Set String)
         _bodyIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 39 "SyntaxTransformer.ag" #-}
              _bodyIallProgs
              {-# LINE 692 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 40 "SyntaxTransformer.ag" #-}
              S.union (S.fromList vars_) _bodyIallVars
              {-# LINE 697 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Var vars_ _bodyIcopy
              {-# LINE 702 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 707 "SyntaxTransformer.hs" #-}
              )
         ( _bodyIallProgs,_bodyIallVars,_bodyIcopy) =
             body_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Prog :: String ->
                 ([String]) ->
                 ([String]) ->
                 T_Body ->
                 T_Stmt
sem_Stmt_Prog name_ params_ results_ body_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _bodyIallProgs :: (M.Map String ([String],[String],Body))
         _bodyIallVars :: (S.Set String)
         _bodyIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 41 "SyntaxTransformer.ag" #-}
              M.singleton name_ (params_, results_, _bodyIcopy)
              {-# LINE 727 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 42 "SyntaxTransformer.ag" #-}
              S.union (S.fromList (params_ ++ results_)) _bodyIallVars
              {-# LINE 732 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Prog name_ params_ results_ _bodyIcopy
              {-# LINE 737 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 742 "SyntaxTransformer.hs" #-}
              )
         ( _bodyIallProgs,_bodyIallVars,_bodyIcopy) =
             body_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_PCall :: String ->
                  T_Exprs ->
                  T_Exprs ->
                  T_Stmt
sem_Stmt_PCall name_ args_ res_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _argsIallVars :: (S.Set String)
         _argsIcopy :: Exprs
         _resIallVars :: (S.Set String)
         _resIcopy :: Exprs
         _lhsOallProgs =
             ({-# LINE 43 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 762 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 44 "SyntaxTransformer.ag" #-}
              S.union _argsIallVars _resIallVars
              {-# LINE 767 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              PCall name_ _argsIcopy _resIcopy
              {-# LINE 772 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 777 "SyntaxTransformer.hs" #-}
              )
         ( _argsIallVars,_argsIcopy) =
             args_
         ( _resIallVars,_resIcopy) =
             res_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Pre :: T_Expr ->
                T_Stmt
sem_Stmt_Pre expr_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _exprIallVars :: (S.Set String)
         _exprIcopy :: Expr
         _lhsOallProgs =
             ({-# LINE 45 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 795 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 46 "SyntaxTransformer.ag" #-}
              _exprIallVars
              {-# LINE 800 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Pre _exprIcopy
              {-# LINE 805 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 810 "SyntaxTransformer.hs" #-}
              )
         ( _exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Post :: T_Expr ->
                 T_Stmt
sem_Stmt_Post expr_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _exprIallVars :: (S.Set String)
         _exprIcopy :: Expr
         _lhsOallProgs =
             ({-# LINE 47 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 826 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 48 "SyntaxTransformer.ag" #-}
              _exprIallVars
              {-# LINE 831 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Post _exprIcopy
              {-# LINE 836 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 841 "SyntaxTransformer.hs" #-}
              )
         ( _exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Inv :: T_Expr ->
                T_Stmt ->
                T_Stmt
sem_Stmt_Inv expr_ stmt_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _exprIallVars :: (S.Set String)
         _exprIcopy :: Expr
         _stmtIallProgs :: (M.Map String ([String],[String],Body))
         _stmtIallVars :: (S.Set String)
         _stmtIcopy :: Stmt
         _lhsOallProgs =
             ({-# LINE 49 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 861 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 50 "SyntaxTransformer.ag" #-}
              S.union _exprIallVars _stmtIallVars
              {-# LINE 866 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Inv _exprIcopy _stmtIcopy
              {-# LINE 871 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 876 "SyntaxTransformer.hs" #-}
              )
         ( _exprIallVars,_exprIcopy) =
             expr_
         ( _stmtIallProgs,_stmtIallVars,_stmtIcopy) =
             stmt_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_While :: T_Expr ->
                  T_Body ->
                  T_Stmt
sem_Stmt_While expr_ body_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _exprIallVars :: (S.Set String)
         _exprIcopy :: Expr
         _bodyIallProgs :: (M.Map String ([String],[String],Body))
         _bodyIallVars :: (S.Set String)
         _bodyIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 51 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 898 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 52 "SyntaxTransformer.ag" #-}
              S.union _exprIallVars _bodyIallVars
              {-# LINE 903 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              While _exprIcopy _bodyIcopy
              {-# LINE 908 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 913 "SyntaxTransformer.hs" #-}
              )
         ( _exprIallVars,_exprIcopy) =
             expr_
         ( _bodyIallProgs,_bodyIallVars,_bodyIcopy) =
             body_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_If :: T_Expr ->
               T_Body ->
               T_Body ->
               T_Stmt
sem_Stmt_If expr_ left_ right_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _exprIallVars :: (S.Set String)
         _exprIcopy :: Expr
         _leftIallProgs :: (M.Map String ([String],[String],Body))
         _leftIallVars :: (S.Set String)
         _leftIcopy :: Body
         _rightIallProgs :: (M.Map String ([String],[String],Body))
         _rightIallVars :: (S.Set String)
         _rightIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 53 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 939 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 54 "SyntaxTransformer.ag" #-}
              S.union (S.union _exprIallVars _leftIallVars) _rightIallVars
              {-# LINE 944 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              If _exprIcopy _leftIcopy _rightIcopy
              {-# LINE 949 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 954 "SyntaxTransformer.hs" #-}
              )
         ( _exprIallVars,_exprIcopy) =
             expr_
         ( _leftIallProgs,_leftIallVars,_leftIcopy) =
             left_
         ( _rightIallProgs,_rightIallVars,_rightIcopy) =
             right_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Assign :: T_Expr ->
                   T_Expr ->
                   T_Stmt
sem_Stmt_Assign expr1_ expr2_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _expr1IallVars :: (S.Set String)
         _expr1Icopy :: Expr
         _expr2IallVars :: (S.Set String)
         _expr2Icopy :: Expr
         _lhsOallProgs =
             ({-# LINE 55 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 977 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 56 "SyntaxTransformer.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 982 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Assign _expr1Icopy _expr2Icopy
              {-# LINE 987 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 992 "SyntaxTransformer.hs" #-}
              )
         ( _expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Sim :: T_Exprs ->
                T_Exprs ->
                T_Stmt
sem_Stmt_Sim left_ right_ =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _leftIallVars :: (S.Set String)
         _leftIcopy :: Exprs
         _rightIallVars :: (S.Set String)
         _rightIcopy :: Exprs
         _lhsOallProgs =
             ({-# LINE 57 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 1013 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 58 "SyntaxTransformer.ag" #-}
              S.union _leftIallVars _rightIallVars
              {-# LINE 1018 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Sim _leftIcopy _rightIcopy
              {-# LINE 1023 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 1028 "SyntaxTransformer.hs" #-}
              )
         ( _leftIallVars,_leftIcopy) =
             left_
         ( _rightIallVars,_rightIcopy) =
             right_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Skip :: T_Stmt
sem_Stmt_Skip =
    (let _lhsOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOallVars :: (S.Set String)
         _lhsOcopy :: Stmt
         _lhsOallProgs =
             ({-# LINE 59 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 1043 "SyntaxTransformer.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 60 "SyntaxTransformer.ag" #-}
              S.empty
              {-# LINE 1048 "SyntaxTransformer.hs" #-}
              )
         _copy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              Skip
              {-# LINE 1053 "SyntaxTransformer.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              _copy
              {-# LINE 1058 "SyntaxTransformer.hs" #-}
              )
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
-- Top ---------------------------------------------------------
data Top = Top (Stmt)
-- cata
sem_Top :: Top ->
           T_Top
sem_Top (Top _stmt) =
    (sem_Top_Top (sem_Stmt _stmt))
-- semantic domain
type T_Top = ( ProgramInfo)
data Inh_Top = Inh_Top {}
data Syn_Top = Syn_Top {pInfo_Syn_Top :: ProgramInfo}
wrap_Top :: T_Top ->
            Inh_Top ->
            Syn_Top
wrap_Top sem (Inh_Top) =
    (let ( _lhsOpInfo) = sem
     in  (Syn_Top _lhsOpInfo))
sem_Top_Top :: T_Stmt ->
               T_Top
sem_Top_Top stmt_ =
    (let _lhsOpInfo :: ProgramInfo
         _stmtIallProgs :: (M.Map String ([String],[String],Body))
         _stmtIallVars :: (S.Set String)
         _stmtIcopy :: Stmt
         _lhsOpInfo =
             ({-# LINE 29 "SyntaxTransformer.ag" #-}
              ProgramInfo _stmtIallVars _stmtIallProgs
              {-# LINE 1088 "SyntaxTransformer.hs" #-}
              )
         ( _stmtIallProgs,_stmtIallVars,_stmtIcopy) =
             stmt_
     in  ( _lhsOpInfo))