module Prover 
(proveImpl,provePred)
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import Control.Monad

import GCL (Expr(..),Stmt(..))

data Array = Array | AInteger deriving (Show,Eq,Ord)


--proveImpl :: Expr -> Expr -> IO SBV.ThmResult
proveImpl e1 e2 = do
  let e1Vars = collectVars e1
      e2Vars = collectVars e2
      varSet = S.filter (\(a,t) -> t==AInteger) $ S.union e1Vars e2Vars
      arrSet = S.filter (\(a,t) -> t==Array) $ S.union e1Vars e2Vars
  putStrLn $ "Varset: " ++ show varSet
  putStrLn $ "Arrset: " ++ show e2Vars
  vars <- return $ foldM (\y x -> do
    z <- sInteger x
    return $ M.insert x z y
    ) M.empty (fst $ unzip $ S.toList varSet)
  arr <- return $ foldM (\y x -> do
    z <- newArray x Nothing :: Symbolic (SArray Integer Integer)
    return $ M.insert x z y
    ) M.empty (fst $ unzip $ S.toList arrSet)
  prove $ do
    vars' <- vars
    arr' <- arr
    pred1 <- mkPred vars' arr' e1
    pred2 <- mkPred vars' arr' e2
    return $ pred1 ==> pred2


----provePred :: Expr -> IO SBV.ThmResult
provePred e = do
  let eVars = S.filter (\(a,t) -> t==AInteger) $ collectVars e
      arrSet = S.filter (\(a,t) -> t==Array) $ collectVars e
  vars <- return $ foldM (\y x -> do
    z <- sInteger x
    return $ M.insert x z y
    ) M.empty (fst $ unzip $ S.toList eVars)
  arr <- return $ foldM (\y x -> do
    z <- newArray x Nothing :: Symbolic (SArray Integer Integer)
    return $ M.insert x z y
    ) M.empty (fst $ unzip $ S.toList arrSet)
  prove $ do
    vars' <- vars
    arr' <- arr
    mkPred vars' arr' e


mkPred :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> Expr -> Predicate
mkPred vars arr (Equal e1 e2)  = do
  p1 <- mkSymInt vars arr e1
  p2 <- mkSymInt vars arr e2
  return $ p1 .== p2
mkPred vars arr (Lower e1 e2)  = do
  p1 <- mkSymInt vars arr e1 
  p2 <- mkSymInt vars arr e2
  return $ p1 .< p2
mkPred vars arr (LowerE e1 e2) = do
  p1 <- mkSymInt vars arr e1 
  p2 <- mkSymInt vars arr e2
  return $ p1 .<= p2
mkPred vars arr (And e1 e2)    = do
  p1 <- mkPred vars arr e1 
  p2 <- mkPred vars arr e2
  return $ p1 &&& p2
mkPred vars arr (Or e1 e2)     = do
  p1 <- mkPred vars arr e1 
  p2 <- mkPred vars arr e2
  return $ p1 ||| p2
mkPred vars arr (Not e)        = do
  p <- mkPred vars arr e 
  return $ bnot p
mkPred vars arr (Impl e1 e2)   = do
  p1 <- mkPred vars arr e1
  p2 <- mkPred vars arr e2
  return $ p1 ==> p2
mkPred vars arr (ForAll s e)   = do
  s1 <- forall s :: Symbolic SInteger
  mkPred (M.insert s s1 vars) arr e
mkPred vars arr True_ = return true
mkPred vars arr _ = error "Should not occur"

mkSymInt :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> Expr -> Symbolic SInteger
mkSymInt vars arr (Minus e1 e2) = mkInt vars arr (-) e1 e2
mkSymInt vars arr (Plus e1 e2) = mkInt vars arr (+) e1 e2
mkSymInt vars arr (Lit i) = return i
mkSymInt vars arr (Name s) = return $ fromJust $ M.lookup s vars
mkSymInt vars arr (Repby (Name s) (Lit i)) = return $ readArray (fromJust $ M.lookup s arr) i

mkInt :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> (SInteger -> SInteger -> SInteger) -> Expr -> Expr -> Symbolic SInteger
mkInt _ _ op (Lit i) (Lit j) = return $ i `op` j
mkInt vars arr op e1 (Lit j) = do
  s1 <- mkSymInt vars arr e1
  return $ s1 `op` j
mkInt vars arr op (Lit i) e2 = do
  s2 <- mkSymInt vars arr e2
  return $ i `op` s2
mkInt vars arr op e1 (Name s) = do
  let s2 = fromJust $ M.lookup s vars
  s1 <- mkSymInt vars arr e1
  return $ s1 `op` s2
mkInt vars arr op (Name s) e2 = do
  let s1 = fromJust $ M.lookup s vars
  s2 <- mkSymInt vars arr e2
  return $ s1 `op` s2
mkInt vars arr op (Repby (Name s) index) e2 = do
  index' <- mkSymInt vars arr index
  let s1 = readArray (fromJust $ M.lookup s arr) index'
  s2 <- mkSymInt vars arr e2
  return $ s1 `op` s2
mkInt vars arr op e1 (Repby (Name s) index) = do
  index' <- mkSymInt vars arr index
  let s1 = readArray (fromJust $ M.lookup s arr) index'
  s2 <- mkSymInt vars arr e1
  return $ s1 `op` s2
mkInt vars arr op e1 e2 = do
  s1 <- mkSymInt vars arr e1
  s2 <- mkSymInt vars arr e2
  return $ s1 `op` s2

collectVars :: Expr -> S.Set (String, Array)
collectVars True_ = S.empty
collectVars (Lit i) = S.empty
collectVars (Name s) = S.singleton (s, AInteger)
collectVars (Minus e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Plus e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Equal e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Lower e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (LowerE e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (And e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Or e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (Not e) = collectVars e
collectVars (Impl e1 e2) = S.union (collectVars e1) (collectVars e2)
collectVars (ForAll s e) = S.difference (collectVars e) (S.singleton (s, AInteger))
collectVars (Repby (Name s) i) = S.singleton (s, Array) 
collectVars expr = error $ "Could not identify: " ++ show expr

--Check the type of the var
varType :: [String] -> [String] -> String -> Maybe Array
varType var arr s
    | elem s var = Just AInteger
    | elem s arr = Just Array
    | otherwise = Nothing