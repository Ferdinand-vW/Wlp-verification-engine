module Verification where

import Control.Concurrent
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
  mvar <- newEmptyMVar
  forkIO (readInput mvar)
  putStrLn ""
  putStrLn "Press space to start verification\n"
  (invs,w) <- foldWlp mvar vars True_ (tail stmts)

  when (not $ null invs) $ do
    putStrLn "------------------------------"
    putStrLn "Invariants are not valid:"
    mapM_ (\x -> do
        putStrLn $ pp x
        putStrLn "") invs

  putStrLn "\n\n\n-----------------------------"
  putStrLn "RESULT: "
  putStrLn "Given precondition: "
  putStrLn $ pp pre
  putStrLn "Calculated precondition: " 
  putStrLn $ pp w
  putStrLn ""
  putStrLn "Now proving implication: "
  proveImpl vars pre w >>= print
      

foldWlp :: MVar Char -> [Var] -> Expr -> [Stmt] -> IO ([Expr],Expr)
foldWlp mvar vars e s = foldr (\x y -> do
                      (xs,expr) <- y
                      (ys,expr') <- wlp mvar vars x expr
                      return $ (xs ++ ys, expr')) (return $ ([],e)) s

wlp :: MVar Char -> [Var] -> Stmt -> Expr -> IO ([Expr],Expr)
wlp mvar vars Skip q = do
    doStep mvar
    stepPrint q Skip q
    return ([],q)
wlp mvar vars (Vars vars' s) q = do
    p <- foldWlp mvar vars q s
    return p
wlp mvar vars (Assign (Name s) e2) q = do
    doStep mvar
    let assign = assignQ q (s, Nothing) e2
    stepPrint q (Assign (Name s) e2) assign
    return $ ([],assign)
wlp mvar vars (Assign (Repby (Name s) i) e2) q = do
    doStep mvar
    let assign = assignQ q (s,Just i) e2
    stepPrint q (Assign (Repby (Name s) i) e2) assign
    return $ ([],assign)
wlp mvar vars (Post e) q = do
    doStep mvar
    stepPrint q (Post e) (e .&& q)
    return $ ([],e .&& q)
wlp mvar vars (Pre e) q = do
    doStep mvar
    stepPrint q (Pre e) (e .==> q)
    return $ ([],e .==> q)
wlp mvar vars (If g s1 s2) q = do
    (xs,e1) <- foldWlp mvar vars q s1
    (ys,e2) <- foldWlp mvar vars q s2
    stepPrint q (If g s1 s2) ((g .&& e1) .|| (neg g .&& e2))
    return $ (xs ++ ys, (g .&& e1) .|| (neg g .&& e2))
wlp mvar vars (Inv i (While g s)) q  = do
    impl1Val <- implIsValid vars (i .&& neg g) q
    (xs,wlpOfS) <- foldWlp mvar vars i s
    impl2Val <- implIsValid vars (i .&& g) wlpOfS
    let implInv1 = if impl1Val then [] else [i .&& neg g .==> q]
        implInv2 = if impl2Val then xs else [i .&& g .==> wlpOfS]
        invs = implInv1 ++ implInv2
    if null invs
      then do
        stepPrint q (Inv i (While g s)) i
        return (invs,i)
      else do
        stepPrint q (Inv i (While g s)) (neg g .&& q)
        return (invs,neg g .&& q)
wlp mvar vars (Prog _ _ _ _) q = return ([],q)
wlp mvar _ (While e1 b) q = error "We do not allow a While without an invariant.."
wlp mvar _ _ _ = error "Not supported by our wlp function"


--The assignQ is for now only allowing variable names.
assignQ :: Expr -> (String, Maybe Expr) -> Expr -> Expr
assignQ (Lit i)        ref expr = Lit i
assignQ (Name s)       ref expr 
                             | s == fst ref = expr --a is the assigned value.
                             | snd ref /= Nothing && (Name s) == (MA.fromJust $ snd ref) = error $ show expr
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
                                        | snd ref == Nothing && s == fst ref = Repby expr (assignQ index ref expr)
                                        | otherwise = (Repby (Name s) (assignQ index ref expr))
assignQ expr _ _ = error $ show expr

implIsValid :: [Var] -> Expr -> Expr -> IO Bool
implIsValid vars e1 e2 = do
  validity <- proveImpl vars e1 e2
  putStrLn $ show validity
  return $ not $ modelExists validity

readInput :: MVar Char -> IO ()
readInput mvar = do
  c <- getChar
  putMVar mvar c
  readInput mvar

doStep :: MVar Char -> IO ()
doStep mvar = do
  c <- takeMVar mvar
  case c of
    ' ' -> return ()
    _ -> doStep mvar

stepPrint :: Expr -> Stmt -> Expr -> IO ()
stepPrint q stmt p = do
  putStrLn $ "Pre: " ++ pp p
  putStrLn $ "Stmt: " ++ pp stmt
  putStrLn $ "Post: " ++ pp q
  putStrLn ""
  putStrLn "-----------------------------\n"