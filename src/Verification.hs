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

import Examples

verifyProgram :: Stmt -> IO ()
verifyProgram stmt = do
  let (Var xs stmts) = transform stmt
      Pre pre = head stmts
  putStrLn "------------------------"
  putStrLn $ show (Var xs stmts)
  putStrLn "-------------------------"
  (invs,w) <- foldWlp True_ (tail stmts)

  when (not $ null invs) $ do
    putStrLn "------------------------------"
    putStrLn "Invariants are not valid:"
    mapM_ (putStrLn . show) invs

  putStrLn "-----------------------------------"
  putStrLn $ show pre
  putStrLn $ show w
  proveImpl pre w >>= print
      

foldWlp :: Expr -> [Stmt] -> IO ([Expr],Expr)
foldWlp e s = foldr (\x y -> do
                      (xs,expr) <- y
                      (ys,expr') <- wlp x expr 
                      return $ (xs ++ ys, expr')) (return $ ([],e)) s

wlp :: Stmt -> Expr -> IO ([Expr],Expr)
wlp Skip q = do
    return ([],q)
wlp (Var vars s) q = do
    p <- foldWlp q s
    return p
wlp (Assign (Name s) e2) q = do
    let assign = assignQ q (s, Nothing) e2
    return $ ([],assign)
wlp (Assign (Repby (Name s) i) e2) q = do
    let assign = assignQ q (s,Just i) e2
    return $ ([],assign)
wlp (Post e) q = do
    return $ ([],e .&& q)
wlp (Pre e) q = do
    return $ ([],e .==> q)
wlp (If g s1 s2) q = do
    (xs,e1) <- foldWlp q s1
    (ys,e2) <- foldWlp q s2
    return $ (xs ++ ys, (g .&& e1) .|| ((.!)g .&& e2))
wlp (Inv i (While g s)) q  = do
    impl1Val <- implIsValid (i .&& (.!)g) q
    (xs,wlpOfS) <- foldWlp i s
    impl2Val <- implIsValid (i .&& g) wlpOfS
    let implInv1 = if impl1Val then [] else [i .&& (.!) g .==> q]
        implInv2 = if impl2Val then xs else [i .&& g .==> wlpOfS]
        invs = implInv1 ++ implInv2
    if null invs
      then return (invs,i)
      else return (invs,(.!)g .&& q)
wlp (Prog _ _ _ _) q = return ([],q)
wlp (While e1 b) q = error "We do not allow a While without an invariant.."
wlp _ _ = error "Not supported by our wlp function"


--The assignQ is for now only allowing variable names.
assignQ :: Expr -> (String, Maybe Expr) -> Expr -> Expr
assignQ (Lit i)        ref expr = Lit i
assignQ (Name s)       ref expr | s == fst ref = expr --a is the assigned value.
                             | otherwise = Name s
assignQ (ForAll s e)   ref expr = ForAll s $ assignQ e ref expr
assignQ (Minus e1 e2)  ref expr = Minus  (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Plus e1 e2)   ref expr = Plus   (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Equal e1 e2)  ref expr = Equal  (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Lower e1 e2)  ref expr = Lower  (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (LowerE e1 e2) ref expr = LowerE (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (And e1 e2)    ref expr = And    (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Or e1 e2)     ref expr = Or     (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Not e1)       ref expr = Not    e1
assignQ True_          ref expr = True_
assignQ (Repby (Name s) index) ref expr | snd ref /= Nothing && index == (MA.fromJust $ snd ref) &&  s == fst ref = expr
                                              | otherwise = (Repby (Name s) index)

implIsValid :: Expr -> Expr -> IO Bool
implIsValid e1 e2 = do
  validity <- proveImpl e1 e2
  putStrLn $ show validity
  return $ not $ modelExists validity