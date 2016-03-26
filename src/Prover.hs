{-# LANGUAGE ScopedTypeVariables #-}

module Prover 
(proveImpl)
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import Control.Monad

import SyntaxTransformer 
import PrettyPrint
--proveImpl :: Expr -> Expr -> IO SBV.ThmResult
proveImpl vars e1 e2 = do
  let varMap = M.fromList $ zip (map nameOf vars) vars
      e1Vars = collectRefs e1
      e2Vars = collectRefs e2
      allVars = S.union e1Vars e2Vars
      intList = M.keys $ M.filterWithKey
        (\k a -> case a of
                Int s -> True && S.member k allVars
                _ -> False) varMap
      forallList = M.keys $ M.filterWithKey
        (\k a -> case a of
                Univ s -> True && S.member k allVars
                _ -> False) varMap
      arrList = M.keys $ M.filterWithKey 
        (\k a -> case a of
                Array s -> True && S.member k allVars
                _ -> False) varMap
  ints <- return $ foldM (\y x -> do
    z <- sInteger x
    return $ M.insert x z y
    ) M.empty intList
  arrays <- return $ foldM (\y x -> do
    z <- newArray x Nothing :: Symbolic (SArray Integer Integer)
    return $ M.insert x z y
    ) M.empty arrList
  univs <- return $ foldM (\y x -> do
  z <- forall x :: Symbolic SInteger
  return $ M.insert x z y
  ) M.empty forallList
  prove $ do
    ints' <- ints
    arrays' <- arrays
    univs' <- univs
    pred1 <- mkPred (M.union ints' univs') arrays' e1
    pred2 <- mkPred (M.union ints' univs') arrays' e2
    return $ pred1 ==> pred2


----provePred :: Expr -> IO SBV.ThmResult
{-provePred e = do
  let eVars = S.filter (\(a,t) -> t==T_Integer) $ collectVars e
      arrSet = S.filter (\(a,t) -> t==T_Array) $ collectVars e
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
    p <- mkPred vars' arr' e
    error $ show p
    return p-}

test :: IO ()
test = do
  p <- prove $ do
    a <- newArray "a" Nothing :: Symbolic (SArray Integer Integer)
    j <- sInteger "j"
    i <- sInteger "i"
    n <- sInteger "n"
    r <- sInteger "r"
    c <- sInteger "c"
    d <- sInteger "d"
    min <- sInteger "min"
    x <- forall "x" :: Symbolic SInteger
    return $ 
        ((j .< n &&& j .<= i &&& j .< r &&&
          (j .== i ==> r .== i) &&&
          (j .<= i ==> r .< i) &&&
          min .== readArray a r &&&
          (j .<= x &&& x .< i 
            ==>
          readArray a r .<= readArray a x))) &&& (i .>= n)
        ==> 
        (j .<= x &&& x .< n ==>
          readArray a r .<= readArray a x)
  print p

test2 :: IO ()
test2 = do
  p <- prove $ do
    a <- newArray "a" Nothing :: Symbolic (SArray Integer Integer)
    j <- sInteger "j"
    i <- sInteger "i"
    n <- sInteger "n"
    r <- sInteger "r"
    f1 <- (forAll_ (\(x :: SInteger) -> j .< x &&& x .< i 
              ==>
            readArray a r .<= readArray a x))
    f2 <- (forAll_ (\(x :: SInteger) -> j .<= x &&& x .< n ==>
                      readArray a r .<= readArray a x))
    min <- sInteger "min"
    return $ 
        ((j .< n &&& j .<= i &&& j .< r &&&
          (j .== i ==> r .== i) &&&
          (j .<= i ==> r .< i) &&&
          min .== readArray a r &&&
          f1 &&& (i .>= n)))
        ==> 
        f2
  print p


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
  --s1 <- forall s :: Symbolic SInteger
  mkPred vars arr e
mkPred vars arr True_ = return true
mkPred vars arr _ = error "Should not occur"

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


calcExpr :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> Expr -> Expr ->  (SInteger -> SInteger -> SInteger) -> Symbolic Expr
calcExpr vars arr (Plus e1 (Lit e2)) q op = do
  preCalc  <- mkInt vars arr op q e1
  postCalc <- mkInt vars arr (+) (Lit preCalc) (Lit e2)
  return $ Lit postCalc
calcExpr vars arr (Plus e1 e2) q op = do
  preCalc <- mkInt vars arr op q e1
  calcExpr vars arr e2 (Lit preCalc) (+)
calcExpr vars arr (Minus e1 (Lit e2)) q op = do
  preCalc <- mkInt vars arr op q e1
  postCalc <- mkInt vars arr (-) (Lit preCalc) (Lit e2)
  return $ Lit postCalc
calcExpr vars arr (Minus e1 e2) q op = do
  preCalc <- mkInt vars arr op q e1
  calcExpr vars arr e2 (Lit preCalc) (-)
calcExpr vars arr (Lit e1) q _ = return $ Lit e1
calcExpr vars arr (Name e1) q _ = return $ Name e1



--mkSymInt :: M.Map String SInteger -> M.Map String (SArray Integer Integer) -> Expr -> Symbolic SInteger
--mkSymInt vars arr (Minus e1 e2@(Lit x)) = do
--    mkInt vars arr (-) e1 e2
--mkSymInt vars arr (Minus e1 e2@(Name x)) = do
--    mkInt vars arr (-) e1 e2
--mkSymInt vars arr (Minus e1 e2) = do
--    Lit preCalc <- calcExpr vars arr e2 e1 (-)
--    return preCalc
--mkSymInt vars arr (Plus e1 e2@(Lit x)) = do
--    mkInt vars arr (-) e1 e2
--mkSymInt vars arr (Plus e1 e2@(Name x)) = do
--    mkInt vars arr (-) e1 e2
--mkSymInt vars arr (Plus e1 e2) = do
--    Lit preCalc <- calcExpr vars arr e2 e1 (+)
--    return preCalc
--mkSymInt vars arr (Lit i) = return i
--mkSymInt vars arr (Name s) = return $ fromJust $ M.lookup s vars
--mkSymInt vars arr (Repby (Name s) (Lit index)) = return $ readArray (fromJust $ M.lookup s arr) index
--mkSymInt vars arr (Repby (Name s) (Name index))  = return $ readArray (fromJust $ M.lookup s arr) (fromJust $ M.lookup index vars)
--mkSymInt vars arr (Repby (Name s) index) = do
--                  index' <- mkSymInt vars arr index 
--                  return $ readArray (fromJust $ M.lookup s arr) index'
--mkSymInt vars arr expr = error $ (show expr) ++ "hola"


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

{-collectVars :: Expr -> S.Set String
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
collectVars (ForAll s e) = S.union (S.singleton s) (collectVars e)
collectVars (Repby (Name e1) (Name e2)) = S.union (S.singleton e1) (S.singleton e2)
collectVars (Repby (Name s) i) = S.union (S.singleton s) (collectVars i)
collectVars expr = error $ "Could not identify: " ++ show expr-}

--Check the type of the var
{-varType :: [String] -> [String] -> String -> Maybe Ty
varType var arr s
    | elem s var = Just T_Integer
    | elem s arr = Just T_Array
    | otherwise = Nothing-}

nameOf :: Var -> String
nameOf (Int s) = s
nameOf (Array s) = s
nameOf (Univ s) = s