

-- UUAGC 0.9.52.1 (src/Collect.ag)
module Collect(
Stmt(..), Expr(..), Var(..), ProgramInfo(..), collectVars, collectPrograms, collectRefs
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.SBV(SInteger)

{-# LINE 16 "src/Collect.ag" #-}

collectVars :: Stmt -> S.Set Var
collectVars stmt = allVars_Syn_Top $ wrap_Top (sem_Top (Top stmt (Lit 0))) Inh_Top

collectRefs :: Expr -> S.Set String
collectRefs expr = allRefs_Syn_Top $ wrap_Top (sem_Top (Top (Vars [] []) expr)) Inh_Top

collectPrograms :: Stmt -> M.Map String ([Var],[Var],Body)
collectPrograms stmt = allProgs_Syn_Top $ wrap_Top (sem_Top (Top stmt (Lit 0))) Inh_Top
{-# LINE 25 "src/Collect.hs" #-}
-- Body --------------------------------------------------------
type Body = [Stmt]
-- cata
sem_Body :: Body ->
            T_Body
sem_Body list =
    (Prelude.foldr sem_Body_Cons sem_Body_Nil (Prelude.map sem_Stmt list))
-- semantic domain
type T_Body = ( (M.Map String ([Var],[Var],Body)),(S.Set Var),Body)
data Inh_Body = Inh_Body {}
data Syn_Body = Syn_Body {allProgs_Syn_Body :: (M.Map String ([Var],[Var],Body)),allVars_Syn_Body :: (S.Set Var),copy_Syn_Body :: Body}
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
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Body
         _hdIallProgs :: (M.Map String ([Var],[Var],Body))
         _hdIallVars :: (S.Set Var)
         _hdIcopy :: Stmt
         _tlIallProgs :: (M.Map String ([Var],[Var],Body))
         _tlIallVars :: (S.Set Var)
         _tlIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 76 "src/Collect.ag" #-}
              M.union _hdIallProgs _tlIallProgs
              {-# LINE 59 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 77 "src/Collect.ag" #-}
              S.union _hdIallVars _tlIallVars
              {-# LINE 64 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              (:) _hdIcopy _tlIcopy
              {-# LINE 69 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 74 "src/Collect.hs" #-}
              )
         ( _hdIallProgs,_hdIallVars,_hdIcopy) =
             hd_
         ( _tlIallProgs,_tlIallVars,_tlIcopy) =
             tl_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Body_Nil :: T_Body
sem_Body_Nil =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Body
         _lhsOallProgs =
             ({-# LINE 78 "src/Collect.ag" #-}
              M.empty
              {-# LINE 89 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 79 "src/Collect.ag" #-}
              S.empty
              {-# LINE 94 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              []
              {-# LINE 99 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 104 "src/Collect.hs" #-}
              )
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
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
type T_Expr = ( (S.Set String),(S.Set Var),Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {allRefs_Syn_Expr :: (S.Set String),allVars_Syn_Expr :: (S.Set Var),copy_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOallRefs,_lhsOallVars,_lhsOcopy) = sem
     in  (Syn_Expr _lhsOallRefs _lhsOallVars _lhsOcopy))
sem_Expr_Lit :: SInteger ->
                T_Expr
sem_Expr_Lit i_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _lhsOallVars =
             ({-# LINE 82 "src/Collect.ag" #-}
              S.empty
              {-# LINE 176 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 83 "src/Collect.ag" #-}
              S.empty
              {-# LINE 181 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Lit i_
              {-# LINE 186 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 191 "src/Collect.hs" #-}
              )
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Name :: String ->
                 T_Expr
sem_Expr_Name var_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _lhsOallVars =
             ({-# LINE 84 "src/Collect.ag" #-}
              S.empty
              {-# LINE 203 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 85 "src/Collect.ag" #-}
              S.singleton var_
              {-# LINE 208 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Name var_
              {-# LINE 213 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 218 "src/Collect.hs" #-}
              )
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_ForAll :: String ->
                   T_Expr ->
                   T_Expr
sem_Expr_ForAll var_ expr_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _lhsOallVars =
             ({-# LINE 86 "src/Collect.ag" #-}
              S.union (S.singleton (Univ var_)) _exprIallVars
              {-# LINE 234 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 87 "src/Collect.ag" #-}
              _exprIallRefs
              {-# LINE 239 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              ForAll var_ _exprIcopy
              {-# LINE 244 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 249 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Exists :: String ->
                   T_Expr ->
                   T_Expr
sem_Expr_Exists var_ expr_ =
    (let _lhsOcopy :: Expr
         _lhsOallRefs :: (S.Set String)
         _lhsOallVars :: (S.Set Var)
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Exists var_ _exprIcopy
              {-# LINE 267 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 272 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 46 "src/Collect.ag" #-}
              _exprIallRefs
              {-# LINE 277 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 42 "src/Collect.ag" #-}
              _exprIallVars
              {-# LINE 282 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Minus :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Minus expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 88 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 303 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 89 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 308 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Minus _expr1Icopy _expr2Icopy
              {-# LINE 313 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 318 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Plus :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Plus expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 90 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 341 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 91 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 346 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Plus _expr1Icopy _expr2Icopy
              {-# LINE 351 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 356 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Equal :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Equal expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 92 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 379 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 93 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 384 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Equal _expr1Icopy _expr2Icopy
              {-# LINE 389 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 394 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Lower :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Lower expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 94 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 417 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 95 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 422 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Lower _expr1Icopy _expr2Icopy
              {-# LINE 427 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 432 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_LowerE :: T_Expr ->
                   T_Expr ->
                   T_Expr
sem_Expr_LowerE expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 96 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 455 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 97 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 460 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              LowerE _expr1Icopy _expr2Icopy
              {-# LINE 465 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 470 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_And :: T_Expr ->
                T_Expr ->
                T_Expr
sem_Expr_And expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 98 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 493 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 99 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 498 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              And _expr1Icopy _expr2Icopy
              {-# LINE 503 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 508 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Or :: T_Expr ->
               T_Expr ->
               T_Expr
sem_Expr_Or expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 100 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 531 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 101 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 536 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Or _expr1Icopy _expr2Icopy
              {-# LINE 541 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 546 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Impl :: T_Expr ->
                 T_Expr ->
                 T_Expr
sem_Expr_Impl expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 102 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 569 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 103 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 574 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Impl _expr1Icopy _expr2Icopy
              {-# LINE 579 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 584 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Not :: T_Expr ->
                T_Expr
sem_Expr_Not expr_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _lhsOallVars =
             ({-# LINE 106 "src/Collect.ag" #-}
              _exprIallVars
              {-# LINE 603 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 107 "src/Collect.ag" #-}
              _exprIallRefs
              {-# LINE 608 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Not _exprIcopy
              {-# LINE 613 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 618 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_Repby :: T_Expr ->
                  T_Expr ->
                  T_Expr
sem_Expr_Repby expr1_ expr2_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallVars =
             ({-# LINE 104 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 639 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 105 "src/Collect.ag" #-}
              S.union _expr1IallRefs _expr2IallRefs
              {-# LINE 644 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Repby _expr1Icopy _expr2Icopy
              {-# LINE 649 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 654 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Expr_True_ :: T_Expr
sem_Expr_True_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Expr
         _lhsOallVars =
             ({-# LINE 108 "src/Collect.ag" #-}
              S.empty
              {-# LINE 669 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 109 "src/Collect.ag" #-}
              S.empty
              {-# LINE 674 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              True_
              {-# LINE 679 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 684 "src/Collect.hs" #-}
              )
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( (S.Set String),(S.Set Var),Exprs)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {allRefs_Syn_Exprs :: (S.Set String),allVars_Syn_Exprs :: (S.Set Var),copy_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOallRefs,_lhsOallVars,_lhsOcopy) = sem
     in  (Syn_Exprs _lhsOallRefs _lhsOallVars _lhsOcopy))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Exprs
         _hdIallRefs :: (S.Set String)
         _hdIallVars :: (S.Set Var)
         _hdIcopy :: Expr
         _tlIallRefs :: (S.Set String)
         _tlIallVars :: (S.Set Var)
         _tlIcopy :: Exprs
         _lhsOallVars =
             ({-# LINE 112 "src/Collect.ag" #-}
              S.union _hdIallVars _tlIallVars
              {-# LINE 720 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 113 "src/Collect.ag" #-}
              S.union _hdIallRefs _tlIallRefs
              {-# LINE 725 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              (:) _hdIcopy _tlIcopy
              {-# LINE 730 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 735 "src/Collect.hs" #-}
              )
         ( _hdIallRefs,_hdIallVars,_hdIcopy) =
             hd_
         ( _tlIallRefs,_tlIallVars,_tlIcopy) =
             tl_
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallRefs :: (S.Set String)
         _lhsOcopy :: Exprs
         _lhsOallVars =
             ({-# LINE 114 "src/Collect.ag" #-}
              S.empty
              {-# LINE 750 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 115 "src/Collect.ag" #-}
              S.empty
              {-# LINE 755 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              []
              {-# LINE 760 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 765 "src/Collect.hs" #-}
              )
     in  ( _lhsOallRefs,_lhsOallVars,_lhsOcopy))
-- ProgramInfo -------------------------------------------------
data ProgramInfo = ProgramInfo ((S.Set Var)) ((M.Map String ([Var],[Var],Body)))
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
sem_ProgramInfo_ProgramInfo :: (S.Set Var) ->
                               (M.Map String ([Var],[Var],Body)) ->
                               T_ProgramInfo
sem_ProgramInfo_ProgramInfo varMap_ progMap_ =
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
type T_Stmt = ( (M.Map String ([Var],[Var],Body)),(S.Set Var),Stmt)
data Inh_Stmt = Inh_Stmt {}
data Syn_Stmt = Syn_Stmt {allProgs_Syn_Stmt :: (M.Map String ([Var],[Var],Body)),allVars_Syn_Stmt :: (S.Set Var),copy_Syn_Stmt :: Stmt}
wrap_Stmt :: T_Stmt ->
             Inh_Stmt ->
             Syn_Stmt
wrap_Stmt sem (Inh_Stmt) =
    (let ( _lhsOallProgs,_lhsOallVars,_lhsOcopy) = sem
     in  (Syn_Stmt _lhsOallProgs _lhsOallVars _lhsOcopy))
sem_Stmt_Vars :: ([Var]) ->
                 T_Body ->
                 T_Stmt
sem_Stmt_Vars vars_ body_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _bodyIallProgs :: (M.Map String ([Var],[Var],Body))
         _bodyIallVars :: (S.Set Var)
         _bodyIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 52 "src/Collect.ag" #-}
              _bodyIallProgs
              {-# LINE 852 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 53 "src/Collect.ag" #-}
              S.union (S.fromList vars_) _bodyIallVars
              {-# LINE 857 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Vars vars_ _bodyIcopy
              {-# LINE 862 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 867 "src/Collect.hs" #-}
              )
         ( _bodyIallProgs,_bodyIallVars,_bodyIcopy) =
             body_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Prog :: String ->
                 ([Var]) ->
                 ([Var]) ->
                 T_Body ->
                 T_Stmt
sem_Stmt_Prog name_ params_ results_ body_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _bodyIallProgs :: (M.Map String ([Var],[Var],Body))
         _bodyIallVars :: (S.Set Var)
         _bodyIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 54 "src/Collect.ag" #-}
              M.singleton name_ (params_, results_, _bodyIcopy)
              {-# LINE 887 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 55 "src/Collect.ag" #-}
              S.union (S.fromList (params_ ++ results_)) _bodyIallVars
              {-# LINE 892 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Prog name_ params_ results_ _bodyIcopy
              {-# LINE 897 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 902 "src/Collect.hs" #-}
              )
         ( _bodyIallProgs,_bodyIallVars,_bodyIcopy) =
             body_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_PCall :: String ->
                  T_Exprs ->
                  T_Exprs ->
                  T_Stmt
sem_Stmt_PCall name_ args_ res_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _argsIallRefs :: (S.Set String)
         _argsIallVars :: (S.Set Var)
         _argsIcopy :: Exprs
         _resIallRefs :: (S.Set String)
         _resIallVars :: (S.Set Var)
         _resIcopy :: Exprs
         _lhsOallProgs =
             ({-# LINE 56 "src/Collect.ag" #-}
              M.empty
              {-# LINE 924 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 57 "src/Collect.ag" #-}
              S.union _argsIallVars _resIallVars
              {-# LINE 929 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              PCall name_ _argsIcopy _resIcopy
              {-# LINE 934 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 939 "src/Collect.hs" #-}
              )
         ( _argsIallRefs,_argsIallVars,_argsIcopy) =
             args_
         ( _resIallRefs,_resIallVars,_resIcopy) =
             res_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Pre :: T_Expr ->
                T_Stmt
sem_Stmt_Pre expr_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _lhsOallProgs =
             ({-# LINE 58 "src/Collect.ag" #-}
              M.empty
              {-# LINE 958 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 59 "src/Collect.ag" #-}
              _exprIallVars
              {-# LINE 963 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Pre _exprIcopy
              {-# LINE 968 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 973 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Post :: T_Expr ->
                 T_Stmt
sem_Stmt_Post expr_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _lhsOallProgs =
             ({-# LINE 60 "src/Collect.ag" #-}
              M.empty
              {-# LINE 990 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 61 "src/Collect.ag" #-}
              _exprIallVars
              {-# LINE 995 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Post _exprIcopy
              {-# LINE 1000 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 1005 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Inv :: T_Expr ->
                T_Stmt ->
                T_Stmt
sem_Stmt_Inv expr_ stmt_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _stmtIallProgs :: (M.Map String ([Var],[Var],Body))
         _stmtIallVars :: (S.Set Var)
         _stmtIcopy :: Stmt
         _lhsOallProgs =
             ({-# LINE 62 "src/Collect.ag" #-}
              M.empty
              {-# LINE 1026 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 63 "src/Collect.ag" #-}
              S.union _exprIallVars _stmtIallVars
              {-# LINE 1031 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Inv _exprIcopy _stmtIcopy
              {-# LINE 1036 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 1041 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
         ( _stmtIallProgs,_stmtIallVars,_stmtIcopy) =
             stmt_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_While :: T_Expr ->
                  T_Body ->
                  T_Stmt
sem_Stmt_While expr_ body_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _bodyIallProgs :: (M.Map String ([Var],[Var],Body))
         _bodyIallVars :: (S.Set Var)
         _bodyIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 64 "src/Collect.ag" #-}
              M.empty
              {-# LINE 1064 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 65 "src/Collect.ag" #-}
              S.union _exprIallVars _bodyIallVars
              {-# LINE 1069 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              While _exprIcopy _bodyIcopy
              {-# LINE 1074 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 1079 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
         ( _bodyIallProgs,_bodyIallVars,_bodyIcopy) =
             body_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_If :: T_Expr ->
               T_Body ->
               T_Body ->
               T_Stmt
sem_Stmt_If expr_ left_ right_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _leftIallProgs :: (M.Map String ([Var],[Var],Body))
         _leftIallVars :: (S.Set Var)
         _leftIcopy :: Body
         _rightIallProgs :: (M.Map String ([Var],[Var],Body))
         _rightIallVars :: (S.Set Var)
         _rightIcopy :: Body
         _lhsOallProgs =
             ({-# LINE 66 "src/Collect.ag" #-}
              M.empty
              {-# LINE 1106 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 67 "src/Collect.ag" #-}
              S.union (S.union _exprIallVars _leftIallVars) _rightIallVars
              {-# LINE 1111 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              If _exprIcopy _leftIcopy _rightIcopy
              {-# LINE 1116 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 1121 "src/Collect.hs" #-}
              )
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
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
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _expr1IallRefs :: (S.Set String)
         _expr1IallVars :: (S.Set Var)
         _expr1Icopy :: Expr
         _expr2IallRefs :: (S.Set String)
         _expr2IallVars :: (S.Set Var)
         _expr2Icopy :: Expr
         _lhsOallProgs =
             ({-# LINE 68 "src/Collect.ag" #-}
              M.empty
              {-# LINE 1146 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 69 "src/Collect.ag" #-}
              S.union _expr1IallVars _expr2IallVars
              {-# LINE 1151 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Assign _expr1Icopy _expr2Icopy
              {-# LINE 1156 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 1161 "src/Collect.hs" #-}
              )
         ( _expr1IallRefs,_expr1IallVars,_expr1Icopy) =
             expr1_
         ( _expr2IallRefs,_expr2IallVars,_expr2Icopy) =
             expr2_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Sim :: T_Exprs ->
                T_Exprs ->
                T_Stmt
sem_Stmt_Sim left_ right_ =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _leftIallRefs :: (S.Set String)
         _leftIallVars :: (S.Set Var)
         _leftIcopy :: Exprs
         _rightIallRefs :: (S.Set String)
         _rightIallVars :: (S.Set Var)
         _rightIcopy :: Exprs
         _lhsOallProgs =
             ({-# LINE 70 "src/Collect.ag" #-}
              M.empty
              {-# LINE 1184 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 71 "src/Collect.ag" #-}
              S.union _leftIallVars _rightIallVars
              {-# LINE 1189 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Sim _leftIcopy _rightIcopy
              {-# LINE 1194 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 1199 "src/Collect.hs" #-}
              )
         ( _leftIallRefs,_leftIallVars,_leftIcopy) =
             left_
         ( _rightIallRefs,_rightIallVars,_rightIcopy) =
             right_
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
sem_Stmt_Skip :: T_Stmt
sem_Stmt_Skip =
    (let _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallVars :: (S.Set Var)
         _lhsOcopy :: Stmt
         _lhsOallProgs =
             ({-# LINE 72 "src/Collect.ag" #-}
              M.empty
              {-# LINE 1214 "src/Collect.hs" #-}
              )
         _lhsOallVars =
             ({-# LINE 73 "src/Collect.ag" #-}
              S.empty
              {-# LINE 1219 "src/Collect.hs" #-}
              )
         _copy =
             ({-# LINE 43 "src/Collect.ag" #-}
              Skip
              {-# LINE 1224 "src/Collect.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 43 "src/Collect.ag" #-}
              _copy
              {-# LINE 1229 "src/Collect.hs" #-}
              )
     in  ( _lhsOallProgs,_lhsOallVars,_lhsOcopy))
-- Top ---------------------------------------------------------
data Top = Top (Stmt) (Expr)
-- cata
sem_Top :: Top ->
           T_Top
sem_Top (Top _stmt _expr) =
    (sem_Top_Top (sem_Stmt _stmt) (sem_Expr _expr))
-- semantic domain
type T_Top = ( (M.Map String ([Var],[Var],Body)),(S.Set String),(S.Set Var))
data Inh_Top = Inh_Top {}
data Syn_Top = Syn_Top {allProgs_Syn_Top :: (M.Map String ([Var],[Var],Body)),allRefs_Syn_Top :: (S.Set String),allVars_Syn_Top :: (S.Set Var)}
wrap_Top :: T_Top ->
            Inh_Top ->
            Syn_Top
wrap_Top sem (Inh_Top) =
    (let ( _lhsOallProgs,_lhsOallRefs,_lhsOallVars) = sem
     in  (Syn_Top _lhsOallProgs _lhsOallRefs _lhsOallVars))
sem_Top_Top :: T_Stmt ->
               T_Expr ->
               T_Top
sem_Top_Top stmt_ expr_ =
    (let _lhsOallVars :: (S.Set Var)
         _lhsOallProgs :: (M.Map String ([Var],[Var],Body))
         _lhsOallRefs :: (S.Set String)
         _stmtIallProgs :: (M.Map String ([Var],[Var],Body))
         _stmtIallVars :: (S.Set Var)
         _stmtIcopy :: Stmt
         _exprIallRefs :: (S.Set String)
         _exprIallVars :: (S.Set Var)
         _exprIcopy :: Expr
         _lhsOallVars =
             ({-# LINE 37 "src/Collect.ag" #-}
              _stmtIallVars
              {-# LINE 1265 "src/Collect.hs" #-}
              )
         _lhsOallProgs =
             ({-# LINE 38 "src/Collect.ag" #-}
              _stmtIallProgs
              {-# LINE 1270 "src/Collect.hs" #-}
              )
         _lhsOallRefs =
             ({-# LINE 39 "src/Collect.ag" #-}
              _exprIallRefs
              {-# LINE 1275 "src/Collect.hs" #-}
              )
         ( _stmtIallProgs,_stmtIallVars,_stmtIcopy) =
             stmt_
         ( _exprIallRefs,_exprIallVars,_exprIcopy) =
             expr_
     in  ( _lhsOallProgs,_lhsOallRefs,_lhsOallVars))
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