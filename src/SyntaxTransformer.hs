

-- UUAGC 0.9.52.1 (SyntaxTransformer.)
module SyntaxTransformer(
Stmt(..), Expr(..), stmt_fresh, stmt_prog
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.SBV(SInteger)

{-# LINE 16 "SyntaxTransformer.ag" #-}

stmt_fresh :: Stmt -> Stmt
stmt_fresh stmt = fresh_Syn_Top $ wrap_Top (sem_Top (Top stmt)) Inh_Top
stmt_prog :: Stmt -> Stmt
stmt_prog stmt = prog_Syn_Top $ wrap_Top (sem_Top (Top stmt)) Inh_Top
{-# LINE 21 "SyntaxTransformer.hs" #-}

{-# LINE 312 "SyntaxTransformer.ag" #-}


fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thrd :: (a,b,c) -> c
thrd (a,b,c) = c
{-# LINE 34 "SyntaxTransformer.hs" #-}
-- Body --------------------------------------------------------
type Body = [Stmt]
-- cata
sem_Body :: Body ->
            T_Body
sem_Body list =
    (Prelude.foldr sem_Body_Cons sem_Body_Nil (Prelude.map sem_Stmt list))
-- semantic domain
type T_Body = (M.Map String ([String],[String],Body)) ->
              Int ->
              (M.Map String String) ->
              ([String]) ->
              ( (S.Set String),Int,(M.Map String ([String],[String],Body)),Body,Body)
data Inh_Body = Inh_Body {allProgs_Inh_Body :: (M.Map String ([String],[String],Body)),countInh_Inh_Body :: Int,varMap_Inh_Body :: (M.Map String String),vars_Inh_Body :: ([String])}
data Syn_Body = Syn_Body {allVars_Syn_Body :: (S.Set String),countSyn_Syn_Body :: Int,findProgs_Syn_Body :: (M.Map String ([String],[String],Body)),fresh_Syn_Body :: Body,prog_Syn_Body :: Body}
wrap_Body :: T_Body ->
             Inh_Body ->
             Syn_Body
wrap_Body sem (Inh_Body _lhsIallProgs _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog) = sem _lhsIallProgs _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Body _lhsOallVars _lhsOcountSyn _lhsOfindProgs _lhsOfresh _lhsOprog))
sem_Body_Cons :: T_Stmt ->
                 T_Body ->
                 T_Body
sem_Body_Cons hd_ tl_ =
    (\ _lhsIallProgs
       _lhsIcountInh
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
              _lhsOprog :: Body
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _hdOallProgs :: (M.Map String ([String],[String],Body))
              _tlOallProgs :: (M.Map String ([String],[String],Body))
              _hdIallVars :: (S.Set String)
              _hdIcountSyn :: Int
              _hdIfindProgs :: (M.Map String ([String],[String],Body))
              _hdIfresh :: Stmt
              _hdIprog :: Stmt
              _tlIallVars :: (S.Set String)
              _tlIcountSyn :: Int
              _tlIfindProgs :: (M.Map String ([String],[String],Body))
              _tlIfresh :: Body
              _tlIprog :: Body
              _lhsOallVars =
                  ({-# LINE 123 "SyntaxTransformer.ag" #-}
                   S.union _hdIallVars _tlIallVars
                   {-# LINE 90 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 124 "SyntaxTransformer.ag" #-}
                   _hdIfresh : _tlIfresh
                   {-# LINE 95 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 125 "SyntaxTransformer.ag" #-}
                   _tlIcountSyn
                   {-# LINE 100 "SyntaxTransformer.hs" #-}
                   )
              _hdOcountInh =
                  ({-# LINE 126 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 105 "SyntaxTransformer.hs" #-}
                   )
              _tlOcountInh =
                  ({-# LINE 127 "SyntaxTransformer.ag" #-}
                   _hdIcountSyn
                   {-# LINE 110 "SyntaxTransformer.hs" #-}
                   )
              _hdOvars =
                  ({-# LINE 128 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 115 "SyntaxTransformer.hs" #-}
                   )
              _tlOvars =
                  ({-# LINE 129 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 120 "SyntaxTransformer.hs" #-}
                   )
              _hdOvarMap =
                  ({-# LINE 130 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 125 "SyntaxTransformer.hs" #-}
                   )
              _tlOvarMap =
                  ({-# LINE 131 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 130 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 271 "SyntaxTransformer.ag" #-}
                   _hdIprog : _tlIprog
                   {-# LINE 135 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 272 "SyntaxTransformer.ag" #-}
                   M.union _hdIfindProgs _tlIfindProgs
                   {-# LINE 140 "SyntaxTransformer.hs" #-}
                   )
              _hdOallProgs =
                  ({-# LINE 273 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 145 "SyntaxTransformer.hs" #-}
                   )
              _tlOallProgs =
                  ({-# LINE 274 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 150 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   (:) _hdIfresh _tlIfresh
                   {-# LINE 155 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   (:) _hdIprog _tlIprog
                   {-# LINE 160 "SyntaxTransformer.hs" #-}
                   )
              ( _hdIallVars,_hdIcountSyn,_hdIfindProgs,_hdIfresh,_hdIprog) =
                  hd_ _hdOallProgs _hdOcountInh _hdOvarMap _hdOvars
              ( _tlIallVars,_tlIcountSyn,_tlIfindProgs,_tlIfresh,_tlIprog) =
                  tl_ _tlOallProgs _tlOcountInh _tlOvarMap _tlOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Body_Nil :: T_Body
sem_Body_Nil =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Body
              _lhsOcountSyn :: Int
              _lhsOprog :: Body
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _lhsOallVars =
                  ({-# LINE 132 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 181 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 133 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 186 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 134 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 191 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 275 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 196 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 276 "SyntaxTransformer.ag" #-}
                   M.empty
                   {-# LINE 201 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 206 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 211 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
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
type T_Expr = Int ->
              (M.Map String String) ->
              ([String]) ->
              ( (S.Set String),Int,Expr,Expr)
data Inh_Expr = Inh_Expr {countInh_Inh_Expr :: Int,varMap_Inh_Expr :: (M.Map String String),vars_Inh_Expr :: ([String])}
data Syn_Expr = Syn_Expr {allVars_Syn_Expr :: (S.Set String),countSyn_Syn_Expr :: Int,fresh_Syn_Expr :: Expr,prog_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog) = sem _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Expr _lhsOallVars _lhsOcountSyn _lhsOfresh _lhsOprog))
sem_Expr_Lit :: SInteger ->
                T_Expr
sem_Expr_Lit i_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _lhsOprog :: Expr
              _lhsOallVars =
                  ({-# LINE 151 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 287 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 152 "SyntaxTransformer.ag" #-}
                   Lit i_
                   {-# LINE 292 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 153 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 297 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Lit i_
                   {-# LINE 302 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Lit i_
                   {-# LINE 307 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 312 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
sem_Expr_Name :: String ->
                 T_Expr
sem_Expr_Name var_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _lhsOprog :: Expr
              _lhsOallVars =
                  ({-# LINE 155 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 328 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 156 "SyntaxTransformer.ag" #-}
                   case M.lookup var_ _lhsIvarMap of
                       Nothing -> Name var_
                       Just s -> Name s
                   {-# LINE 335 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 159 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 340 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Name var_
                   {-# LINE 345 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Name var_
                   {-# LINE 350 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 355 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _exprIprog :: Expr
              _lhsOallVars =
                  ({-# LINE 160 "SyntaxTransformer.ag" #-}
                   S.singleton var_
                   {-# LINE 379 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 161 "SyntaxTransformer.ag" #-}
                   ForAll _lookup     _exprIfresh
                   {-# LINE 384 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 162 "SyntaxTransformer.ag" #-}
                   _exprIcountSyn
                   {-# LINE 389 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 163 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh + 1
                   {-# LINE 394 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 164 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 399 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 165 "SyntaxTransformer.ag" #-}
                   case _lookup     == var_ of
                     True -> M.insert var_ var_ _lhsIvarMap
                     False -> M.adjust (\_ -> _lookup    ) var_ _lhsIvarMap
                   {-# LINE 406 "SyntaxTransformer.hs" #-}
                   )
              _lookup =
                  ({-# LINE 169 "SyntaxTransformer.ag" #-}
                   case L.find(== var_) _lhsIvars of
                      Nothing -> var_
                      Just s -> s ++ (show _lhsIcountInh)
                   {-# LINE 413 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   ForAll var_ _exprIfresh
                   {-# LINE 418 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   ForAll var_ _exprIprog
                   {-# LINE 423 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 428 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh,_exprIprog) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 172 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 461 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 173 "SyntaxTransformer.ag" #-}
                   Minus _expr1Ifresh _expr2Ifresh
                   {-# LINE 466 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 174 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 471 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 175 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 476 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 176 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 481 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 177 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 486 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 178 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 491 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 179 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 496 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 180 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 501 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Minus _expr1Ifresh _expr2Ifresh
                   {-# LINE 506 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Minus _expr1Iprog _expr2Iprog
                   {-# LINE 511 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 516 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 181 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 551 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 182 "SyntaxTransformer.ag" #-}
                   Plus _expr1Ifresh _expr2Ifresh
                   {-# LINE 556 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 183 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 561 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 184 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 566 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 185 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 571 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 186 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 576 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 187 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 581 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 188 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 586 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 189 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 591 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Plus _expr1Ifresh _expr2Ifresh
                   {-# LINE 596 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Plus _expr1Iprog _expr2Iprog
                   {-# LINE 601 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 606 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 190 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 641 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 191 "SyntaxTransformer.ag" #-}
                   Equal _expr1Ifresh _expr2Ifresh
                   {-# LINE 646 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 192 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 651 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 193 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 656 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 194 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 661 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 195 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 666 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 196 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 671 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 197 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 676 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 198 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 681 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Equal _expr1Ifresh _expr2Ifresh
                   {-# LINE 686 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Equal _expr1Iprog _expr2Iprog
                   {-# LINE 691 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 696 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 199 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 731 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 200 "SyntaxTransformer.ag" #-}
                   Lower _expr1Ifresh _expr2Ifresh
                   {-# LINE 736 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 201 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 741 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 202 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 746 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 203 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 751 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 204 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 756 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 205 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 761 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 206 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 766 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 207 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 771 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Lower _expr1Ifresh _expr2Ifresh
                   {-# LINE 776 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Lower _expr1Iprog _expr2Iprog
                   {-# LINE 781 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 786 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 208 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 821 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 209 "SyntaxTransformer.ag" #-}
                   LowerE _expr1Ifresh _expr2Ifresh
                   {-# LINE 826 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 210 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 831 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 211 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 836 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 212 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 841 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 213 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 846 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 214 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 851 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 215 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 856 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 216 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 861 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   LowerE _expr1Ifresh _expr2Ifresh
                   {-# LINE 866 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   LowerE _expr1Iprog _expr2Iprog
                   {-# LINE 871 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 876 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 217 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 911 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 218 "SyntaxTransformer.ag" #-}
                   And _expr1Ifresh _expr2Ifresh
                   {-# LINE 916 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 219 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 921 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 220 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 926 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 221 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 931 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 222 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 936 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 223 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 941 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 224 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 946 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 225 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 951 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   And _expr1Ifresh _expr2Ifresh
                   {-# LINE 956 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   And _expr1Iprog _expr2Iprog
                   {-# LINE 961 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 966 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 226 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 1001 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 227 "SyntaxTransformer.ag" #-}
                   Or _expr1Ifresh _expr2Ifresh
                   {-# LINE 1006 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 228 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 1011 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 229 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1016 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 230 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 1021 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 231 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1026 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 232 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1031 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 233 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1036 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 234 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1041 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Or _expr1Ifresh _expr2Ifresh
                   {-# LINE 1046 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Or _expr1Iprog _expr2Iprog
                   {-# LINE 1051 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 1056 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 235 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 1091 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 236 "SyntaxTransformer.ag" #-}
                   Impl _expr1Ifresh _expr2Ifresh
                   {-# LINE 1096 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 237 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 1101 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 238 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1106 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 239 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 1111 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 240 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1116 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 241 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1121 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 242 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1126 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 243 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1131 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Impl _expr1Ifresh _expr2Ifresh
                   {-# LINE 1136 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Impl _expr1Iprog _expr2Iprog
                   {-# LINE 1141 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 1146 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _exprIprog :: Expr
              _lhsOallVars =
                  ({-# LINE 244 "SyntaxTransformer.ag" #-}
                   _exprIallVars
                   {-# LINE 1173 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 245 "SyntaxTransformer.ag" #-}
                   Not _exprIfresh
                   {-# LINE 1178 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 246 "SyntaxTransformer.ag" #-}
                   _exprIcountSyn
                   {-# LINE 1183 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 247 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1188 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 248 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1193 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 249 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1198 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Not _exprIfresh
                   {-# LINE 1203 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Not _exprIprog
                   {-# LINE 1208 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 1213 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh,_exprIprog) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
              _lhsOprog :: Expr
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 250 "SyntaxTransformer.ag" #-}
                   S.union _expr1IallVars _expr2IallVars
                   {-# LINE 1246 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 251 "SyntaxTransformer.ag" #-}
                   Repby _expr1Ifresh _expr2Ifresh
                   {-# LINE 1251 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 252 "SyntaxTransformer.ag" #-}
                   _expr2IcountSyn
                   {-# LINE 1256 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 253 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1261 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 254 "SyntaxTransformer.ag" #-}
                   _expr1IcountSyn
                   {-# LINE 1266 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 255 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1271 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 256 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1276 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 257 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1281 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 258 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1286 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Repby _expr1Ifresh _expr2Ifresh
                   {-# LINE 1291 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Repby _expr1Iprog _expr2Iprog
                   {-# LINE 1296 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 1301 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
sem_Expr_True_ :: T_Expr
sem_Expr_True_ =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Expr
              _lhsOcountSyn :: Int
              _lhsOprog :: Expr
              _lhsOallVars =
                  ({-# LINE 259 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1320 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 260 "SyntaxTransformer.ag" #-}
                   True_
                   {-# LINE 1325 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 261 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1330 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   True_
                   {-# LINE 1335 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   True_
                   {-# LINE 1340 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 1345 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
               ( (S.Set String),Int,Exprs,Exprs)
data Inh_Exprs = Inh_Exprs {countInh_Inh_Exprs :: Int,varMap_Inh_Exprs :: (M.Map String String),vars_Inh_Exprs :: ([String])}
data Syn_Exprs = Syn_Exprs {allVars_Syn_Exprs :: (S.Set String),countSyn_Syn_Exprs :: Int,fresh_Syn_Exprs :: Exprs,prog_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog) = sem _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Exprs _lhsOallVars _lhsOcountSyn _lhsOfresh _lhsOprog))
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
              _lhsOprog :: Exprs
              _hdIallVars :: (S.Set String)
              _hdIcountSyn :: Int
              _hdIfresh :: Expr
              _hdIprog :: Expr
              _tlIallVars :: (S.Set String)
              _tlIcountSyn :: Int
              _tlIfresh :: Exprs
              _tlIprog :: Exprs
              _lhsOallVars =
                  ({-# LINE 137 "SyntaxTransformer.ag" #-}
                   S.union _hdIallVars _tlIallVars
                   {-# LINE 1396 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 138 "SyntaxTransformer.ag" #-}
                   _hdIfresh : _tlIfresh
                   {-# LINE 1401 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 139 "SyntaxTransformer.ag" #-}
                   _tlIcountSyn
                   {-# LINE 1406 "SyntaxTransformer.hs" #-}
                   )
              _hdOcountInh =
                  ({-# LINE 140 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1411 "SyntaxTransformer.hs" #-}
                   )
              _tlOcountInh =
                  ({-# LINE 141 "SyntaxTransformer.ag" #-}
                   _hdIcountSyn
                   {-# LINE 1416 "SyntaxTransformer.hs" #-}
                   )
              _hdOvars =
                  ({-# LINE 142 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1421 "SyntaxTransformer.hs" #-}
                   )
              _tlOvars =
                  ({-# LINE 143 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1426 "SyntaxTransformer.hs" #-}
                   )
              _hdOvarMap =
                  ({-# LINE 144 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1431 "SyntaxTransformer.hs" #-}
                   )
              _tlOvarMap =
                  ({-# LINE 145 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1436 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   (:) _hdIfresh _tlIfresh
                   {-# LINE 1441 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   (:) _hdIprog _tlIprog
                   {-# LINE 1446 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 1451 "SyntaxTransformer.hs" #-}
                   )
              ( _hdIallVars,_hdIcountSyn,_hdIfresh,_hdIprog) =
                  hd_ _hdOcountInh _hdOvarMap _hdOvars
              ( _tlIallVars,_tlIcountSyn,_tlIfresh,_tlIprog) =
                  tl_ _tlOcountInh _tlOvarMap _tlOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (\ _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Exprs
              _lhsOcountSyn :: Int
              _lhsOprog :: Exprs
              _lhsOallVars =
                  ({-# LINE 146 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1470 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 147 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 1475 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 148 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1480 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 1485 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   []
                   {-# LINE 1490 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   _prog
                   {-# LINE 1495 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfresh,_lhsOprog)))
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
sem_Stmt (Skip) =
    (sem_Stmt_Skip)
-- semantic domain
type T_Stmt = (M.Map String ([String],[String],Body)) ->
              Int ->
              (M.Map String String) ->
              ([String]) ->
              ( (S.Set String),Int,(M.Map String ([String],[String],Body)),Stmt,Stmt)
data Inh_Stmt = Inh_Stmt {allProgs_Inh_Stmt :: (M.Map String ([String],[String],Body)),countInh_Inh_Stmt :: Int,varMap_Inh_Stmt :: (M.Map String String),vars_Inh_Stmt :: ([String])}
data Syn_Stmt = Syn_Stmt {allVars_Syn_Stmt :: (S.Set String),countSyn_Syn_Stmt :: Int,findProgs_Syn_Stmt :: (M.Map String ([String],[String],Body)),fresh_Syn_Stmt :: Stmt,prog_Syn_Stmt :: Stmt}
wrap_Stmt :: T_Stmt ->
             Inh_Stmt ->
             Syn_Stmt
wrap_Stmt sem (Inh_Stmt _lhsIallProgs _lhsIcountInh _lhsIvarMap _lhsIvars) =
    (let ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog) = sem _lhsIallProgs _lhsIcountInh _lhsIvarMap _lhsIvars
     in  (Syn_Stmt _lhsOallVars _lhsOcountSyn _lhsOfindProgs _lhsOfresh _lhsOprog))
sem_Stmt_Var :: ([String]) ->
                T_Body ->
                T_Stmt
sem_Stmt_Var vars_ body_ =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _bodyOcountInh :: Int
              _lhsOfresh :: Stmt
              _bodyOvars :: ([String])
              _bodyOvarMap :: (M.Map String String)
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _bodyOallProgs :: (M.Map String ([String],[String],Body))
              _bodyIallVars :: (S.Set String)
              _bodyIcountSyn :: Int
              _bodyIfindProgs :: (M.Map String ([String],[String],Body))
              _bodyIfresh :: Body
              _bodyIprog :: Body
              _lhsOallVars =
                  ({-# LINE 46 "SyntaxTransformer.ag" #-}
                   S.union (S.fromList vars_) _bodyIallVars
                   {-# LINE 1572 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 47 "SyntaxTransformer.ag" #-}
                   _bodyIcountSyn
                   {-# LINE 1577 "SyntaxTransformer.hs" #-}
                   )
              _bodyOcountInh =
                  ({-# LINE 48 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh + 1
                   {-# LINE 1582 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 49 "SyntaxTransformer.ag" #-}
                   Var (L.union _remVars     _freshVars    ) _bodyIfresh
                   {-# LINE 1587 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvars =
                  ({-# LINE 50 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1592 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvarMap =
                  ({-# LINE 51 "SyntaxTransformer.ag" #-}
                   M.union _freshVarsMap     _lhsIvarMap
                   {-# LINE 1597 "SyntaxTransformer.hs" #-}
                   )
              _dupVars =
                  ({-# LINE 52 "SyntaxTransformer.ag" #-}
                   L.intersect _lhsIvars vars_
                   {-# LINE 1602 "SyntaxTransformer.hs" #-}
                   )
              _freshVars =
                  ({-# LINE 53 "SyntaxTransformer.ag" #-}
                   map (++ show _lhsIcountInh) _dupVars
                   {-# LINE 1607 "SyntaxTransformer.hs" #-}
                   )
              _freshVarsMap =
                  ({-# LINE 54 "SyntaxTransformer.ag" #-}
                   M.fromList $ zip _dupVars     _freshVars
                   {-# LINE 1612 "SyntaxTransformer.hs" #-}
                   )
              _remVars =
                  ({-# LINE 55 "SyntaxTransformer.ag" #-}
                   vars_ L.\\ _dupVars
                   {-# LINE 1617 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 279 "SyntaxTransformer.ag" #-}
                   Var vars_ _bodyIprog
                   {-# LINE 1622 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 280 "SyntaxTransformer.ag" #-}
                   _bodyIfindProgs
                   {-# LINE 1627 "SyntaxTransformer.hs" #-}
                   )
              _bodyOallProgs =
                  ({-# LINE 281 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 1632 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Var vars_ _bodyIfresh
                   {-# LINE 1637 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Var vars_ _bodyIprog
                   {-# LINE 1642 "SyntaxTransformer.hs" #-}
                   )
              ( _bodyIallVars,_bodyIcountSyn,_bodyIfindProgs,_bodyIfresh,_bodyIprog) =
                  body_ _bodyOallProgs _bodyOcountInh _bodyOvarMap _bodyOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_Prog :: String ->
                 ([String]) ->
                 ([String]) ->
                 T_Body ->
                 T_Stmt
sem_Stmt_Prog name_ params_ results_ body_ =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _bodyOcountInh :: Int
              _bodyOvars :: ([String])
              _bodyOvarMap :: (M.Map String String)
              _lhsOfresh :: Stmt
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _bodyOallProgs :: (M.Map String ([String],[String],Body))
              _bodyIallVars :: (S.Set String)
              _bodyIcountSyn :: Int
              _bodyIfindProgs :: (M.Map String ([String],[String],Body))
              _bodyIfresh :: Body
              _bodyIprog :: Body
              _lhsOallVars =
                  ({-# LINE 56 "SyntaxTransformer.ag" #-}
                   S.union (S.union (S.fromList params_) _bodyIallVars) (S.fromList results_)
                   {-# LINE 1674 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 57 "SyntaxTransformer.ag" #-}
                   _bodyIcountSyn
                   {-# LINE 1679 "SyntaxTransformer.hs" #-}
                   )
              _bodyOcountInh =
                  ({-# LINE 58 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh + 1
                   {-# LINE 1684 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvars =
                  ({-# LINE 59 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1689 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvarMap =
                  ({-# LINE 60 "SyntaxTransformer.ag" #-}
                   M.union (M.union _freshVarsMap     _lhsIvarMap) _freshVarsRMap
                   {-# LINE 1694 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 61 "SyntaxTransformer.ag" #-}
                   Prog name_ (L.union _remVars     _freshVars    ) (L.union _remVarsR     _freshVarsR    ) _bodyIfresh
                   {-# LINE 1699 "SyntaxTransformer.hs" #-}
                   )
              _dupVars =
                  ({-# LINE 62 "SyntaxTransformer.ag" #-}
                   L.intersect _lhsIvars params_
                   {-# LINE 1704 "SyntaxTransformer.hs" #-}
                   )
              _dupVarsR =
                  ({-# LINE 63 "SyntaxTransformer.ag" #-}
                   L.intersect _lhsIvars results_
                   {-# LINE 1709 "SyntaxTransformer.hs" #-}
                   )
              _freshVars =
                  ({-# LINE 64 "SyntaxTransformer.ag" #-}
                   map (++ show _lhsIcountInh) _dupVars
                   {-# LINE 1714 "SyntaxTransformer.hs" #-}
                   )
              _freshVarsR =
                  ({-# LINE 65 "SyntaxTransformer.ag" #-}
                   map (++ show _lhsIcountInh) _dupVarsR
                   {-# LINE 1719 "SyntaxTransformer.hs" #-}
                   )
              _freshVarsMap =
                  ({-# LINE 66 "SyntaxTransformer.ag" #-}
                   M.fromList $ zip _dupVars     _freshVars
                   {-# LINE 1724 "SyntaxTransformer.hs" #-}
                   )
              _freshVarsRMap =
                  ({-# LINE 67 "SyntaxTransformer.ag" #-}
                   M.fromList $ zip _dupVarsR     _freshVarsR
                   {-# LINE 1729 "SyntaxTransformer.hs" #-}
                   )
              _remVars =
                  ({-# LINE 68 "SyntaxTransformer.ag" #-}
                   params_ L.\\ _dupVars
                   {-# LINE 1734 "SyntaxTransformer.hs" #-}
                   )
              _remVarsR =
                  ({-# LINE 69 "SyntaxTransformer.ag" #-}
                   results_ L.\\ _dupVarsR
                   {-# LINE 1739 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 282 "SyntaxTransformer.ag" #-}
                   Prog name_ params_ results_ _bodyIprog
                   {-# LINE 1744 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 283 "SyntaxTransformer.ag" #-}
                   M.union _findProgs     _bodyIfindProgs
                   {-# LINE 1749 "SyntaxTransformer.hs" #-}
                   )
              _findProgs =
                  ({-# LINE 284 "SyntaxTransformer.ag" #-}
                   M.singleton name_ (params_,results_,_bodyIprog)
                   {-# LINE 1754 "SyntaxTransformer.hs" #-}
                   )
              _bodyOallProgs =
                  ({-# LINE 285 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 1759 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Prog name_ params_ results_ _bodyIfresh
                   {-# LINE 1764 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Prog name_ params_ results_ _bodyIprog
                   {-# LINE 1769 "SyntaxTransformer.hs" #-}
                   )
              ( _bodyIallVars,_bodyIcountSyn,_bodyIfindProgs,_bodyIfresh,_bodyIprog) =
                  body_ _bodyOallProgs _bodyOcountInh _bodyOvarMap _bodyOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_PCall :: String ->
                  T_Exprs ->
                  T_Exprs ->
                  T_Stmt
sem_Stmt_PCall name_ args_ res_ =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOfresh :: Stmt
              _lhsOcountSyn :: Int
              _argsOcountInh :: Int
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _argsOvarMap :: (M.Map String String)
              _argsOvars :: ([String])
              _resOcountInh :: Int
              _resOvarMap :: (M.Map String String)
              _resOvars :: ([String])
              _argsIallVars :: (S.Set String)
              _argsIcountSyn :: Int
              _argsIfresh :: Exprs
              _argsIprog :: Exprs
              _resIallVars :: (S.Set String)
              _resIcountSyn :: Int
              _resIfresh :: Exprs
              _resIprog :: Exprs
              _lhsOallVars =
                  ({-# LINE 70 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1805 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 71 "SyntaxTransformer.ag" #-}
                   PCall name_ _argsIfresh _resIfresh
                   {-# LINE 1810 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 72 "SyntaxTransformer.ag" #-}
                   _argsIcountSyn
                   {-# LINE 1815 "SyntaxTransformer.hs" #-}
                   )
              _argsOcountInh =
                  ({-# LINE 73 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1820 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 286 "SyntaxTransformer.ag" #-}
                   Var (_prms     ++ _res    ) $
                      [Assign (Name $ head _prms) (head _argsIprog)]
                        ++
                      _body
                        ++
                      [Assign (head _resIprog) (Name $ head _res    )]
                   {-# LINE 1830 "SyntaxTransformer.hs" #-}
                   )
              _prms =
                  ({-# LINE 292 "SyntaxTransformer.ag" #-}
                   fst3 _prog
                   {-# LINE 1835 "SyntaxTransformer.hs" #-}
                   )
              _res =
                  ({-# LINE 293 "SyntaxTransformer.ag" #-}
                   snd3 _prog
                   {-# LINE 1840 "SyntaxTransformer.hs" #-}
                   )
              _body =
                  ({-# LINE 294 "SyntaxTransformer.ag" #-}
                   thrd _prog
                   {-# LINE 1845 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 295 "SyntaxTransformer.ag" #-}
                   fromJust $ M.lookup name_ _lhsIallProgs
                   {-# LINE 1850 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 296 "SyntaxTransformer.ag" #-}
                   M.empty
                   {-# LINE 1855 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   PCall name_ _argsIfresh _resIfresh
                   {-# LINE 1860 "SyntaxTransformer.hs" #-}
                   )
              _argsOvarMap =
                  ({-# LINE 43 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1865 "SyntaxTransformer.hs" #-}
                   )
              _argsOvars =
                  ({-# LINE 42 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1870 "SyntaxTransformer.hs" #-}
                   )
              _resOcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1875 "SyntaxTransformer.hs" #-}
                   )
              _resOvarMap =
                  ({-# LINE 43 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1880 "SyntaxTransformer.hs" #-}
                   )
              _resOvars =
                  ({-# LINE 42 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1885 "SyntaxTransformer.hs" #-}
                   )
              ( _argsIallVars,_argsIcountSyn,_argsIfresh,_argsIprog) =
                  args_ _argsOcountInh _argsOvarMap _argsOvars
              ( _resIallVars,_resIcountSyn,_resIfresh,_resIprog) =
                  res_ _resOcountInh _resOvarMap _resOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_Pre :: T_Expr ->
                T_Stmt
sem_Stmt_Pre expr_ =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _exprOcountInh :: Int
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _exprIprog :: Expr
              _lhsOallVars =
                  ({-# LINE 74 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1914 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 75 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1919 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 76 "SyntaxTransformer.ag" #-}
                   Pre _exprIfresh
                   {-# LINE 1924 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 77 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 1929 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 78 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 1934 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 297 "SyntaxTransformer.ag" #-}
                   Pre _exprIprog
                   {-# LINE 1939 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 298 "SyntaxTransformer.ag" #-}
                   M.empty
                   {-# LINE 1944 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Pre _exprIfresh
                   {-# LINE 1949 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Pre _exprIprog
                   {-# LINE 1954 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1959 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh,_exprIprog) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_Post :: T_Expr ->
                 T_Stmt
sem_Stmt_Post expr_ =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _exprOvars :: ([String])
              _exprOvarMap :: (M.Map String String)
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _exprOcountInh :: Int
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _exprIprog :: Expr
              _lhsOallVars =
                  ({-# LINE 79 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 1986 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 80 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 1991 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 81 "SyntaxTransformer.ag" #-}
                   Post _exprIfresh
                   {-# LINE 1996 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 82 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2001 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 83 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2006 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 299 "SyntaxTransformer.ag" #-}
                   Post _exprIprog
                   {-# LINE 2011 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 300 "SyntaxTransformer.ag" #-}
                   M.empty
                   {-# LINE 2016 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Post _exprIfresh
                   {-# LINE 2021 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Post _exprIprog
                   {-# LINE 2026 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2031 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh,_exprIprog) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_Inv :: T_Expr ->
                T_Stmt ->
                T_Stmt
sem_Stmt_Inv expr_ stmt_ =
    (\ _lhsIallProgs
       _lhsIcountInh
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
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _exprOcountInh :: Int
              _stmtOallProgs :: (M.Map String ([String],[String],Body))
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _exprIprog :: Expr
              _stmtIallVars :: (S.Set String)
              _stmtIcountSyn :: Int
              _stmtIfindProgs :: (M.Map String ([String],[String],Body))
              _stmtIfresh :: Stmt
              _stmtIprog :: Stmt
              _lhsOallVars =
                  ({-# LINE 84 "SyntaxTransformer.ag" #-}
                   _stmtIallVars
                   {-# LINE 2068 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 85 "SyntaxTransformer.ag" #-}
                   _stmtIcountSyn
                   {-# LINE 2073 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "SyntaxTransformer.ag" #-}
                   Inv _exprIfresh _stmtIfresh
                   {-# LINE 2078 "SyntaxTransformer.hs" #-}
                   )
              _stmtOcountInh =
                  ({-# LINE 87 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2083 "SyntaxTransformer.hs" #-}
                   )
              _stmtOvars =
                  ({-# LINE 88 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2088 "SyntaxTransformer.hs" #-}
                   )
              _stmtOvarMap =
                  ({-# LINE 89 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2093 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 90 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2098 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 91 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2103 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 301 "SyntaxTransformer.ag" #-}
                   Inv _exprIprog _stmtIprog
                   {-# LINE 2108 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 302 "SyntaxTransformer.ag" #-}
                   _stmtIfindProgs
                   {-# LINE 2113 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Inv _exprIfresh _stmtIfresh
                   {-# LINE 2118 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Inv _exprIprog _stmtIprog
                   {-# LINE 2123 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2128 "SyntaxTransformer.hs" #-}
                   )
              _stmtOallProgs =
                  ({-# LINE 268 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 2133 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh,_exprIprog) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
              ( _stmtIallVars,_stmtIcountSyn,_stmtIfindProgs,_stmtIfresh,_stmtIprog) =
                  stmt_ _stmtOallProgs _stmtOcountInh _stmtOvarMap _stmtOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_While :: T_Expr ->
                  T_Body ->
                  T_Stmt
sem_Stmt_While expr_ body_ =
    (\ _lhsIallProgs
       _lhsIcountInh
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
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _exprOcountInh :: Int
              _bodyOallProgs :: (M.Map String ([String],[String],Body))
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _exprIprog :: Expr
              _bodyIallVars :: (S.Set String)
              _bodyIcountSyn :: Int
              _bodyIfindProgs :: (M.Map String ([String],[String],Body))
              _bodyIfresh :: Body
              _bodyIprog :: Body
              _lhsOallVars =
                  ({-# LINE 92 "SyntaxTransformer.ag" #-}
                   _bodyIallVars
                   {-# LINE 2172 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 93 "SyntaxTransformer.ag" #-}
                   _bodyIcountSyn
                   {-# LINE 2177 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 94 "SyntaxTransformer.ag" #-}
                   While _exprIfresh _bodyIfresh
                   {-# LINE 2182 "SyntaxTransformer.hs" #-}
                   )
              _bodyOcountInh =
                  ({-# LINE 95 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2187 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvars =
                  ({-# LINE 96 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2192 "SyntaxTransformer.hs" #-}
                   )
              _bodyOvarMap =
                  ({-# LINE 97 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2197 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 98 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2202 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 99 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2207 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 303 "SyntaxTransformer.ag" #-}
                   While _exprIprog _bodyIprog
                   {-# LINE 2212 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 304 "SyntaxTransformer.ag" #-}
                   _bodyIfindProgs
                   {-# LINE 2217 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   While _exprIfresh _bodyIfresh
                   {-# LINE 2222 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   While _exprIprog _bodyIprog
                   {-# LINE 2227 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2232 "SyntaxTransformer.hs" #-}
                   )
              _bodyOallProgs =
                  ({-# LINE 268 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 2237 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh,_exprIprog) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
              ( _bodyIallVars,_bodyIcountSyn,_bodyIfindProgs,_bodyIfresh,_bodyIprog) =
                  body_ _bodyOallProgs _bodyOcountInh _bodyOvarMap _bodyOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_If :: T_Expr ->
               T_Body ->
               T_Body ->
               T_Stmt
sem_Stmt_If expr_ left_ right_ =
    (\ _lhsIallProgs
       _lhsIcountInh
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
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _exprOcountInh :: Int
              _leftOallProgs :: (M.Map String ([String],[String],Body))
              _rightOallProgs :: (M.Map String ([String],[String],Body))
              _exprIallVars :: (S.Set String)
              _exprIcountSyn :: Int
              _exprIfresh :: Expr
              _exprIprog :: Expr
              _leftIallVars :: (S.Set String)
              _leftIcountSyn :: Int
              _leftIfindProgs :: (M.Map String ([String],[String],Body))
              _leftIfresh :: Body
              _leftIprog :: Body
              _rightIallVars :: (S.Set String)
              _rightIcountSyn :: Int
              _rightIfindProgs :: (M.Map String ([String],[String],Body))
              _rightIfresh :: Body
              _rightIprog :: Body
              _lhsOallVars =
                  ({-# LINE 100 "SyntaxTransformer.ag" #-}
                   S.union _leftIallVars _rightIallVars
                   {-# LINE 2286 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 101 "SyntaxTransformer.ag" #-}
                   _rightIcountSyn
                   {-# LINE 2291 "SyntaxTransformer.hs" #-}
                   )
              _leftOcountInh =
                  ({-# LINE 102 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2296 "SyntaxTransformer.hs" #-}
                   )
              _rightOcountInh =
                  ({-# LINE 103 "SyntaxTransformer.ag" #-}
                   _leftIcountSyn
                   {-# LINE 2301 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 104 "SyntaxTransformer.ag" #-}
                   If _exprIfresh _leftIfresh _rightIfresh
                   {-# LINE 2306 "SyntaxTransformer.hs" #-}
                   )
              _leftOvars =
                  ({-# LINE 105 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2311 "SyntaxTransformer.hs" #-}
                   )
              _leftOvarMap =
                  ({-# LINE 106 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2316 "SyntaxTransformer.hs" #-}
                   )
              _rightOvars =
                  ({-# LINE 107 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2321 "SyntaxTransformer.hs" #-}
                   )
              _rightOvarMap =
                  ({-# LINE 108 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2326 "SyntaxTransformer.hs" #-}
                   )
              _exprOvars =
                  ({-# LINE 109 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2331 "SyntaxTransformer.hs" #-}
                   )
              _exprOvarMap =
                  ({-# LINE 110 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2336 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 305 "SyntaxTransformer.ag" #-}
                   If _exprIprog _leftIprog _rightIprog
                   {-# LINE 2341 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 306 "SyntaxTransformer.ag" #-}
                   M.union _leftIfindProgs _rightIfindProgs
                   {-# LINE 2346 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   If _exprIfresh _leftIfresh _rightIfresh
                   {-# LINE 2351 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   If _exprIprog _leftIprog _rightIprog
                   {-# LINE 2356 "SyntaxTransformer.hs" #-}
                   )
              _exprOcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2361 "SyntaxTransformer.hs" #-}
                   )
              _leftOallProgs =
                  ({-# LINE 268 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 2366 "SyntaxTransformer.hs" #-}
                   )
              _rightOallProgs =
                  ({-# LINE 268 "SyntaxTransformer.ag" #-}
                   _lhsIallProgs
                   {-# LINE 2371 "SyntaxTransformer.hs" #-}
                   )
              ( _exprIallVars,_exprIcountSyn,_exprIfresh,_exprIprog) =
                  expr_ _exprOcountInh _exprOvarMap _exprOvars
              ( _leftIallVars,_leftIcountSyn,_leftIfindProgs,_leftIfresh,_leftIprog) =
                  left_ _leftOallProgs _leftOcountInh _leftOvarMap _leftOvars
              ( _rightIallVars,_rightIcountSyn,_rightIfindProgs,_rightIfresh,_rightIprog) =
                  right_ _rightOallProgs _rightOcountInh _rightOvarMap _rightOvars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_Assign :: T_Expr ->
                   T_Expr ->
                   T_Stmt
sem_Stmt_Assign expr1_ expr2_ =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _expr1Ovars :: ([String])
              _expr2Ovars :: ([String])
              _expr1OvarMap :: (M.Map String String)
              _expr2OvarMap :: (M.Map String String)
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _expr1OcountInh :: Int
              _expr2OcountInh :: Int
              _expr1IallVars :: (S.Set String)
              _expr1IcountSyn :: Int
              _expr1Ifresh :: Expr
              _expr1Iprog :: Expr
              _expr2IallVars :: (S.Set String)
              _expr2IcountSyn :: Int
              _expr2Ifresh :: Expr
              _expr2Iprog :: Expr
              _lhsOallVars =
                  ({-# LINE 111 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 2410 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 112 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2415 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 113 "SyntaxTransformer.ag" #-}
                   Assign _expr1Ifresh _expr2Ifresh
                   {-# LINE 2420 "SyntaxTransformer.hs" #-}
                   )
              _expr1Ovars =
                  ({-# LINE 114 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2425 "SyntaxTransformer.hs" #-}
                   )
              _expr2Ovars =
                  ({-# LINE 115 "SyntaxTransformer.ag" #-}
                   _lhsIvars
                   {-# LINE 2430 "SyntaxTransformer.hs" #-}
                   )
              _expr1OvarMap =
                  ({-# LINE 116 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2435 "SyntaxTransformer.hs" #-}
                   )
              _expr2OvarMap =
                  ({-# LINE 117 "SyntaxTransformer.ag" #-}
                   _lhsIvarMap
                   {-# LINE 2440 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 307 "SyntaxTransformer.ag" #-}
                   Assign _expr1Iprog _expr2Iprog
                   {-# LINE 2445 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 308 "SyntaxTransformer.ag" #-}
                   M.empty
                   {-# LINE 2450 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Assign _expr1Ifresh _expr2Ifresh
                   {-# LINE 2455 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Assign _expr1Iprog _expr2Iprog
                   {-# LINE 2460 "SyntaxTransformer.hs" #-}
                   )
              _expr1OcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2465 "SyntaxTransformer.hs" #-}
                   )
              _expr2OcountInh =
                  ({-# LINE 40 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2470 "SyntaxTransformer.hs" #-}
                   )
              ( _expr1IallVars,_expr1IcountSyn,_expr1Ifresh,_expr1Iprog) =
                  expr1_ _expr1OcountInh _expr1OvarMap _expr1Ovars
              ( _expr2IallVars,_expr2IcountSyn,_expr2Ifresh,_expr2Iprog) =
                  expr2_ _expr2OcountInh _expr2OvarMap _expr2Ovars
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
sem_Stmt_Skip :: T_Stmt
sem_Stmt_Skip =
    (\ _lhsIallProgs
       _lhsIcountInh
       _lhsIvarMap
       _lhsIvars ->
         (let _lhsOallVars :: (S.Set String)
              _lhsOcountSyn :: Int
              _lhsOfresh :: Stmt
              _lhsOprog :: Stmt
              _lhsOfindProgs :: (M.Map String ([String],[String],Body))
              _lhsOallVars =
                  ({-# LINE 118 "SyntaxTransformer.ag" #-}
                   S.empty
                   {-# LINE 2491 "SyntaxTransformer.hs" #-}
                   )
              _lhsOcountSyn =
                  ({-# LINE 119 "SyntaxTransformer.ag" #-}
                   _lhsIcountInh
                   {-# LINE 2496 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 120 "SyntaxTransformer.ag" #-}
                   Skip
                   {-# LINE 2501 "SyntaxTransformer.hs" #-}
                   )
              _lhsOprog =
                  ({-# LINE 309 "SyntaxTransformer.ag" #-}
                   Skip
                   {-# LINE 2506 "SyntaxTransformer.hs" #-}
                   )
              _lhsOfindProgs =
                  ({-# LINE 310 "SyntaxTransformer.ag" #-}
                   M.empty
                   {-# LINE 2511 "SyntaxTransformer.hs" #-}
                   )
              _fresh =
                  ({-# LINE 39 "SyntaxTransformer.ag" #-}
                   Skip
                   {-# LINE 2516 "SyntaxTransformer.hs" #-}
                   )
              _prog =
                  ({-# LINE 264 "SyntaxTransformer.ag" #-}
                   Skip
                   {-# LINE 2521 "SyntaxTransformer.hs" #-}
                   )
          in  ( _lhsOallVars,_lhsOcountSyn,_lhsOfindProgs,_lhsOfresh,_lhsOprog)))
-- Top ---------------------------------------------------------
data Top = Top (Stmt)
-- cata
sem_Top :: Top ->
           T_Top
sem_Top (Top _stmt) =
    (sem_Top_Top (sem_Stmt _stmt))
-- semantic domain
type T_Top = ( Stmt,Stmt)
data Inh_Top = Inh_Top {}
data Syn_Top = Syn_Top {fresh_Syn_Top :: Stmt,prog_Syn_Top :: Stmt}
wrap_Top :: T_Top ->
            Inh_Top ->
            Syn_Top
wrap_Top sem (Inh_Top) =
    (let ( _lhsOfresh,_lhsOprog) = sem
     in  (Syn_Top _lhsOfresh _lhsOprog))
sem_Top_Top :: T_Stmt ->
               T_Top
sem_Top_Top stmt_ =
    (let _lhsOfresh :: Stmt
         _stmtOvarMap :: (M.Map String String)
         _stmtOvars :: ([String])
         _stmtOcountInh :: Int
         _stmtOallProgs :: (M.Map String ([String],[String],Body))
         _lhsOprog :: Stmt
         _stmtIallVars :: (S.Set String)
         _stmtIcountSyn :: Int
         _stmtIfindProgs :: (M.Map String ([String],[String],Body))
         _stmtIfresh :: Stmt
         _stmtIprog :: Stmt
         _lhsOfresh =
             ({-# LINE 30 "SyntaxTransformer.ag" #-}
              _stmtIfresh
              {-# LINE 2558 "SyntaxTransformer.hs" #-}
              )
         _stmtOvarMap =
             ({-# LINE 31 "SyntaxTransformer.ag" #-}
              M.empty
              {-# LINE 2563 "SyntaxTransformer.hs" #-}
              )
         _stmtOvars =
             ({-# LINE 32 "SyntaxTransformer.ag" #-}
              S.toList _stmtIallVars
              {-# LINE 2568 "SyntaxTransformer.hs" #-}
              )
         _stmtOcountInh =
             ({-# LINE 33 "SyntaxTransformer.ag" #-}
              0
              {-# LINE 2573 "SyntaxTransformer.hs" #-}
              )
         _stmtOallProgs =
             ({-# LINE 34 "SyntaxTransformer.ag" #-}
              _stmtIfindProgs
              {-# LINE 2578 "SyntaxTransformer.hs" #-}
              )
         _lhsOprog =
             ({-# LINE 35 "SyntaxTransformer.ag" #-}
              _stmtIprog
              {-# LINE 2583 "SyntaxTransformer.hs" #-}
              )
         ( _stmtIallVars,_stmtIcountSyn,_stmtIfindProgs,_stmtIfresh,_stmtIprog) =
             stmt_ _stmtOallProgs _stmtOcountInh _stmtOvarMap _stmtOvars
     in  ( _lhsOfresh,_lhsOprog))