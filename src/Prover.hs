{-# LANGUAGE ScopedTypeVariables #-}

module Prover 
(proveImpl)
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import Control.Monad

import PrettyPrint
import Collect
import Transformer(toPrenexNF, mkFreshExpr)


--In the proveImpl we prove the implication.
--First we collect all the vaiables
-- In proveImpl we first collect all the variables and arrays,
-- Second we calculate Prenex normal form of the e1 e2 implication.
-- Third we calculate the Predicate form the Expr
-- In the last step we proof the predicate
proveImpl :: [Var] -> Expr -> Expr -> IO ThmResult
proveImpl vars e1 e2 = do
  let varMap = M.fromList $ zip (map nameOf vars) vars
      e1Vars = collectRefs e1
      e2Vars = collectRefs e2
      allVars = S.union e1Vars e2Vars --Determine which variables are integers and which are arrays
      intList = M.keys $ M.filterWithKey
        (\k a -> case a of
                Int s -> S.member k allVars
                _ -> False) varMap
      arrList = M.keys $ M.filterWithKey 
        (\k a -> case a of
                Array s -> S.member k allVars
                _ -> False) varMap
  ints <- return $ foldM (\y x -> do --Initialize the variables
    z <- sInteger x
    return $ M.insert x z y
    ) M.empty intList
  arrays <- return $ foldM (\y x -> do
    z <- newArray x Nothing :: Symbolic (SArray Integer Integer)
    return $ M.insert x z y
    ) M.empty arrList
  let expr = toPrenexNF $ snd $ mkFreshExpr 0 M.empty (e1 `Impl` e2)
  prove $ do
    ints' <- ints
    arrays' <- arrays
    mkPred ints' arrays' expr

--mkPred converts an Expr to the Predicate of the Data.SBV package.
mkPred :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> Expr -> Predicate
mkPred vars arr (Equal e1 e2)  = do
  p1 <- mkSymEq vars arr e1
  case p1 of
      Left sInt1 -> do
        Left sInt2 <- mkSymEq vars arr e2
        return $ sInt1 .== sInt2
      Right sArr1 -> do
        Right sArr2 <- mkSymEq vars arr e2
        return $ sArr1 .== sArr2
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
  s' <- forall s :: Symbolic SInteger
  mkPred (M.insert s s' vars) arr e
mkPred vars arr (Exists s e)   = do
  s' <- exists s :: Symbolic SInteger
  mkPred (M.insert s s' vars) arr e
mkPred vars arr True_ = return true
mkPred vars arr _ = error "Should not occur"

--mkSymEq will either return an SInteger or an Array.
-- Left is sInteger
-- Right is an Array
mkSymEq :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> Expr -> Symbolic (Either SInteger (SArray Integer Integer))
mkSymEq vars arr (Name s) = 
  case M.lookup s arr of
    Just arr' -> return $ Right arr'
    Nothing -> do 
      sInt <- mkSymInt vars arr (Name s)
      return $ Left sInt
mkSymEq vars arr expr = do
  sInt <- mkSymInt vars arr expr
  return $ Left sInt

--mkSymInt calculates the symbolic integer of an Expression
mkSymInt :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> Expr -> Symbolic SInteger
mkSymInt vars arr (Minus e1 e2) = mkInt vars arr (-) e1 e2
mkSymInt vars arr (Plus e1 e2) = mkInt vars arr (+) e1 e2
mkSymInt vars arr (Lit i) = return i
mkSymInt vars arr (Name s) = return $ fromJust $ M.lookup s vars
mkSymInt vars arr (Repby (Name s) (Lit index)) = return $ readArray (fromJust $ M.lookup s arr) index
mkSymInt vars arr (Repby (Name s) (Name index)) = return $ readArray (fromJust $ M.lookup s arr) (fromJust $ M.lookup index vars)
mkSymInt vars arr (Repby (Name s) index) = do
                  index' <- mkSymInt vars arr index 
                  return $ readArray (fromJust $ M.lookup s arr) index'
mkSymInt vars arr expr = error $ show expr

--mkInt is used to perform some basic math operations on two Expressions. 
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

nameOf :: Var -> String
nameOf (Int s) = s
nameOf (Array s) = s
nameOf (Univ s) = s