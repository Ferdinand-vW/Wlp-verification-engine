module Prover 
(proveImpl,provePred)
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import Control.Monad

import SyntaxTransformer

proveImpl e1 e2 = do
  let e1Vars = collectVars e1
      e2Vars = collectVars e2
      varSet = S.union e1Vars e2Vars
  vars <- return $ foldM (\y x -> do
    z <- sInteger x
    return $ M.insert x z y
    ) M.empty (S.toList varSet)
  prove $ do
    vars' <- vars
    pred1 <- mkPred vars' e1
    pred2 <- mkPred vars' e2
    return $ pred1 ==> pred2

provePred e = do
  let eVars = collectVars e
  vars <- return $ foldM (\y x -> do
    z <- sInteger x
    return $ M.insert x z y
    ) M.empty (S.toList eVars)
  prove $ do
    vars' <- vars
    mkPred vars' e

mkPred :: M.Map String SInteger -> Expr -> Predicate
mkPred vars (Equal e1 e2)  = do
  p1 <- mkSymInt vars e1
  p2 <- mkSymInt vars e2
  return $ p1 .== p2
mkPred vars (Lower e1 e2)  = do
  p1 <- mkSymInt vars e1 
  p2 <- mkSymInt vars e2
  return $ p1 .< p2
mkPred vars (LowerE e1 e2) = do
  p1 <- mkSymInt vars e1 
  p2 <- mkSymInt vars e2
  return $ p1 .<= p2
mkPred vars (And e1 e2)    = do
  p1 <- mkPred vars e1 
  p2 <- mkPred vars e2
  return $ p1 &&& p2
mkPred vars (Or e1 e2)     = do
  p1 <- mkPred vars e1 
  p2 <- mkPred vars e2
  return $ p1 ||| p2
mkPred vars (Not e)        = do
  p <- mkPred vars e 
  return $ bnot p
mkPred vars (Impl e1 e2)   = do
  p1 <- mkPred vars e1
  p2 <- mkPred vars e2
  return $ p1 ==> p2
mkPred vars (ForAll s e)   = do
  s1 <- forall s :: Symbolic (SInteger)
  p <- mkPred (M.insert s s1 vars) e
  return $ p
mkPred vars True_ = do
  return true
mkPred vars _ = error "Should not occur"


mkSymInt :: M.Map String SInteger -> Expr -> Symbolic SInteger
mkSymInt vars (Minus e1 e2) = mkInt vars (-) e1 e2
mkSymInt vars (Plus e1 e2) = mkInt vars (+) e1 e2
mkSymInt vars (Lit i) = return i
mkSymInt vars (Name s) = return $ fromJust $ M.lookup s vars

mkInt :: M.Map String SInteger -> (SInteger -> SInteger -> SInteger) -> Expr -> Expr -> Symbolic SInteger
mkInt _ op (Lit i) (Lit j) = return $ i `op` j
mkInt vars op e1 (Lit j) = do
  s1 <- mkSymInt vars e1
  return $ s1 `op` j
mkInt vars op (Lit i) e2 = do
  s2 <- mkSymInt vars e2
  return $ i `op` s2
mkInt vars op e1 (Name s) = do
  let s2 = fromJust $ M.lookup s vars
  s1 <- mkSymInt vars e1
  return $ s1 `op` s2
mkInt vars op (Name s) e2 = do
  let s1 = fromJust $ M.lookup s vars
  s2 <- mkSymInt vars e2
  return $ s1 `op` s2
mkInt vars op e1 e2 = do
  s1 <- mkSymInt vars e1
  s2 <- mkSymInt vars e2
  return $ s1 `op` s2

collectVars :: Expr -> S.Set String
collectVars True_ = S.empty
collectVars (Lit i) = S.empty
collectVars (Name s) = S.singleton s
collectVars (Minus e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Plus e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Equal e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Lower e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (LowerE e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (And e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Or e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Not e) = collectVars e
collectVars (Impl e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (ForAll s e) = S.difference (collectVars e) (S.singleton s)
collectVars expr = error $ "Could not identify: " ++ show expr
