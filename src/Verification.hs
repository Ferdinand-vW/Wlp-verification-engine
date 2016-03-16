module Verification where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as MA
import qualified Data.SBV as SBV

import Data.SBV(modelExists)
import SyntaxTransformer
import Transformer
import GCL
import Prover
import Control.Monad

import PrettyPrint
import Examples

verifyProgram :: Stmt -> IO ()
verifyProgram stmt = do
  let ((Vars xs stmts), vars) = transform stmt
      Pre pre = head stmts
  putStrLn "------------------------"
  putStrLn $ show (Vars xs stmts)
  putStrLn "-------------------------"
  (invs,w) <- foldWlp vars True_ (tail stmts)

  when (not $ null invs) $ do
    putStrLn "------------------------------"
    putStrLn "Invariants are not valid:"
    mapM_ (\x -> do
        putStrLn $ pp x
        putStrLn "") invs

  putStrLn "-----------------------------------"
  putStrLn $ pp pre
  putStrLn $ pp w
  proveImpl vars pre w >>= print
      

foldWlp :: [Var] -> Expr -> [Stmt] -> IO ([Expr],Expr)
foldWlp vars e s = foldr (\x y -> do
                      (xs,expr) <- y
                      (ys,expr') <- wlp vars x expr 
                      return $ (xs ++ ys, expr')) (return $ ([],e)) s

wlp :: [Var] -> Stmt -> Expr -> IO ([Expr],Expr)
wlp vars Skip q = do
    return ([],q)
wlp vars (Vars vars' s) q = do
    p <- foldWlp vars q s
    return p
wlp vars (Assign (Name s) e2) q = do
    let assign = assignQ q (s, Nothing) e2
    return $ ([],assign)
wlp vars (Assign (Repby (Name s) i) e2) q = do
    let assign = assignQ q (s,Just i) e2
    return $ ([],assign)
wlp vars (Post e) q = do
    return $ ([],e .&& q)
wlp vars (Pre e) q = do
    return $ ([],e .==> q)
wlp vars (If g s1 s2) q = do
    (xs,e1) <- foldWlp vars q s1
    (ys,e2) <- foldWlp vars q s2
    return $ (xs ++ ys, (g .&& e1) .|| (neg g .&& e2))
wlp vars (Inv i (While g s)) q  = do
    impl1Val <- implIsValid vars (i .&& neg g) q
    (xs,wlpOfS) <- foldWlp vars i s
    impl2Val <- implIsValid vars (i .&& g) wlpOfS
    let implInv1 = if impl1Val then [] else [i .&& neg g .==> q]
        implInv2 = if impl2Val then xs else [i .&& g .==> wlpOfS]
        invs = implInv1 ++ implInv2
    if null invs
      then return (invs,i)
      else return (invs,neg g .&& q)
wlp vars (Prog _ _ _ _) q = return ([],q)
wlp _ (While e1 b) q = error "We do not allow a While without an invariant.."
wlp _ _ _ = error "Not supported by our wlp function"


--The assignQ is for now only allowing variable names.
assignQ :: Expr -> (String, Maybe Expr) -> Expr -> Expr
assignQ (Lit i)        ref expr = Lit i
assignQ (Name s)       ref expr 
                             | s == fst ref = expr --a is the assigned value.
                             | otherwise = Name s
assignQ (ForAll s e)   ref expr = ForAll s $ assignQ e ref expr
assignQ (Minus e1 e2)  ref expr = Minus  (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Plus e1 e2)   ref expr = Plus   (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Equal e1 e2)  ref expr = Equal  (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Lower e1 e2)  ref expr = Lower  (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (LowerE e1 e2) ref expr = LowerE (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (And e1 e2)    ref expr = And    (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Or e1 e2)     ref expr = Or     (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Impl e1 e2)   ref expr = Impl   (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Not e1)       ref expr = Not    e1
assignQ True_          ref expr = True_
assignQ (Repby (Name s) index) ref expr | snd ref /= Nothing && index == (MA.fromJust $ snd ref) &&  s == fst ref = expr
                                        | s == fst ref && snd ref == Nothing = Repby expr (assignQ index ref expr)
                                        | otherwise = (Repby (Name s) (assignQ index ref expr))
assignQ expr _ _ = error $ show expr

implIsValid :: [Var] -> Expr -> Expr -> IO Bool
implIsValid vars e1 e2 = do
  validity <- proveImpl vars e1 e2
  putStrLn $ show validity
  return $ not $ modelExists validity