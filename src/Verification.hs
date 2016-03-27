module Verification where

import Control.Concurrent
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as MA
import qualified Data.SBV as SBV

import Data.SBV(modelExists)
import Collect
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
  writeFile "Output.txt" ""
  t <- forkIO (readInput mvar)
  putStrLn ""
  putStrLn "Press space to start verification\n"
  (invs,w) <- foldWlp mvar vars True_ (tail stmts)

  when (not $ null invs) $ do
    putStrLn "------------------------------"
    putStrLn "Invariants are not valid:"
    mapM_ (\x -> do
        putStrLn $ pp x
        putStrLn "-----------------------------\n\n") invs

  putStrLn "\n\n\n-----------------------------"
  putStrLn "RESULT: "
  putStrLn "Given precondition: "
  putStrLn $ pp pre
  putStrLn "Calculated precondition: " 
  putStrLn $ pp w
  putStrLn ""
  putStrLn "Now proving implication: "
  proveImpl vars pre w >>= print
  killThread t
      

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

    implPrint "Proving invariant I AND Not G ==> Q: " (i .&& neg g) q

    (xs,wlpOfS) <- foldWlp mvar vars i s
    implPrint "Proving invariant I AND G ==> I:" (i .&& g) i
    impl2Val <- implIsValid vars (i .&& g) wlpOfS
    let implInv1 = if impl1Val then [] else [i .&& neg g .==> q]
        implInv2 = if impl2Val then xs else [i .&& g .==> wlpOfS] ++ xs
        invs = implInv1 ++ implInv2
    if impl1Val && impl2Val
      then do
        stepPrint q (Inv i (While g s)) i
        return (invs,i)
      else do
        stepPrint q (Inv i (While g s)) (neg g .&& q)
        return (invs,neg g .&& q)
wlp mvar vars (While g s) q = do
    putStrLn "Loop reduction"
    putStrLn ("Q: " ++ show q)
    fixpoint <- loopReduction 10 (neg g .==> q) g s q mvar vars
    putStrLn "End loop reduction"
    return ([],fixpoint)
wlp mvar vars (Prog _ _ _ _) q = return ([],q)
wlp mvar _ _ _ = error "Not supported by our wlp function"

--In the loop reducation we try to calculate the fixpoint.
--The first parameter is an int, which determines the amount of iterations.
--The second parameters is the previous calculated post condition
--It will try to implicate the previous calculated post condition with the current calculated post condition.
--g stands for the condition
--s is the body of the while
loopReduction :: Int -> Expr -> Expr -> [Stmt] -> Expr -> MVar Char -> [Var] -> IO Expr
loopReduction 0 e1 g s q mvar vars = do
  (_,w) <- foldWlp mvar vars e1 s
  let fixpoint = (g .&& w) .|| (neg g .&& q)
  impl1Val <- implIsValid vars e1 fixpoint
  if impl1Val 
      then return fixpoint
      else do 
        putStrLn "No fixpoint"
        return (neg g .&& q)
loopReduction k e1 g s q mvar vars = do
  (_,w) <- foldWlp mvar vars e1 s
  let fixpoint = (g .&& w) .|| (neg g .&& q)
  putStrLn $ show k
  impl1Val <- implIsValid vars e1 fixpoint
  if impl1Val 
      then return fixpoint
      else loopReduction (k-1) fixpoint g s q mvar vars

--The assignQ is for now only allowing variable names.
assignQ :: Expr -> (String, Maybe Expr) -> Expr -> Expr
assignQ (Lit i)        ref expr = Lit i
assignQ (Name s)       ref expr 
                             | s == fst ref = expr --a is the assigned value.
                             -- | snd ref /= Nothing && (Name s) == (MA.fromJust $ snd ref) = error $ show expr ++ " " ++ (show $ snd ref) ++ " " ++ s
                             | otherwise = Name s
assignQ (ForAll s e)   ref expr = ForAll s $ assignQ e ref expr
assignQ (Minus e1 e2)  ref expr = Minus  (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Plus e1 e2)   ref expr = Plus   (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ e@(Equal e1 e2)  ref expr = permExpr e ref expr
assignQ e@(Lower e1 e2)  ref expr = permExpr e ref expr
assignQ e@(LowerE e1 e2) ref expr = permExpr e ref expr
assignQ (And e1 e2)    ref expr = And    (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Or e1 e2)     ref expr = Or     (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Impl e1 e2)   ref expr = Impl   (assignQ e1 ref expr) (assignQ e2 ref expr)
assignQ (Not e1)       ref expr = Not    (assignQ e1 ref expr)
assignQ True_          ref expr = True_
assignQ (Repby (Name s) index) ref expr | snd ref /= Nothing && index == (MA.fromJust $ snd ref) &&  s == fst ref = expr
                                        | snd ref == Nothing && s == fst ref = Repby expr (assignQ index ref expr)
                                        | otherwise = (Repby (Name s) (assignQ index ref expr))
assignQ expr _ _ = error $ show expr ++ "Last assign pattern"

permExpr :: Expr -> (String, Maybe Expr) -> Expr -> Expr
permExpr (Lower e1 e2)  ref@(s, Nothing) args = Lower (assignQ e1 ref args) (assignQ e2 ref args)
permExpr (LowerE e1 e2) ref@(s, Nothing) args = LowerE (assignQ e1 ref args) (assignQ e2 ref args)
permExpr (Equal e1 e2)  ref@(s, Nothing) args = Equal (assignQ e1 ref args) (assignQ e2 ref args)
permExpr expr (s, Just i) args = 
  let xs = permArrays s $ findArrays expr
      perms = permutations xs
      permExprs = map (\ys -> permutation expr xs ys i .==> replaceArray ys s args expr) perms
  in case length xs > 0 of
        True -> case length permExprs <= 0 of
                  False -> foldr1 (\x y -> x .&& y) permExprs
                  True -> error "error"
        False -> expr

permutation :: Expr -> [Expr] -> [Expr] -> Expr -> Expr
permutation e all pExprs index = 
  let negs = all L.\\ pExprs
      eqExprs = foldr1 (.&&) (map (\x -> x .== index) pExprs)
  in case length (negs ++ pExprs) <= 0 of
        False -> foldr1 (.&&) (map (.== index) pExprs ++ map (neg . (.==) index) negs)
        True -> error $ pp e

replaceArray :: [Expr] -> String -> Expr -> Expr -> Expr
replaceArray xs ref expr (Lower e1 e2) = Lower (replaceArray xs ref expr e1) (replaceArray xs ref expr e2) 
replaceArray xs ref expr (LowerE e1 e2) = LowerE (replaceArray xs ref expr e1) (replaceArray xs ref expr e2)
replaceArray xs ref expr (Equal e1 e2) = Equal (replaceArray xs ref expr e1) (replaceArray xs ref expr e2)
replaceArray xs ref expr (Minus e1 e2) = Minus (replaceArray xs ref expr e1) (replaceArray xs ref expr e2)
replaceArray xs ref expr (Plus e1 e2) = Plus (replaceArray xs ref expr e1) (replaceArray xs ref expr e2)
replaceArray xs ref expr (Repby (Name s) index)
  | s == ref && index `elem` xs = expr
  | otherwise = Repby (Name s) index
replaceArray _ _ expr e = e


permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = xs : L.nub [ zs | (x,ys) <- selections xs, zs <- permutations ys ]

selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

permArrays :: String -> [Expr] -> [Expr]
permArrays s xs = map (\(Repby _ i) -> i) $ filter (\(Repby (Name s1) index) -> s1 == s) xs

findArrays :: Expr -> [Expr]
findArrays (Lower e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (LowerE e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (Equal e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (Minus e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (Plus e1 e2) = findArrays e1 ++ findArrays e2
findArrays (Repby s i) = [Repby s i]
findArrays (Name s) = []
findArrays (Lit i) = []




implIsValid :: [Var] -> Expr -> Expr -> IO Bool
implIsValid vars e1 e2 = do
  validity <- proveImpl vars e1 e2
  appendFile "Output.txt" (show validity)
  return $ not $ modelExists validity

readInput :: MVar Char -> IO ()
readInput mvar = do
  c <- getChar
  putMVar mvar c
  readInput mvar

doStep :: MVar Char -> IO ()
doStep mvar = do
  c <- readMVar mvar
  case c of
      'n' -> return ()
      ' ' -> do 
            takeMVar mvar
            return ()

implPrint :: String -> Expr -> Expr -> IO ()
implPrint s e1 e2 = do
  let str = "\n--------------------------\n" ++
            s ++ "\n" ++
            pp e1 ++ "\n" ++
            ".==>\n" ++
            pp e2 ++ "\n" ++
            "-----------------------\n" 
  appendFile "Output.txt" str

stepPrint :: Expr -> Stmt -> Expr -> IO ()
stepPrint q stmt p = do
  let str = "\nPre: " ++ pp p ++ "\n" ++
            "Stmt: " ++ pp stmt ++ "\n" ++
            "Post: " ++ pp q ++ "\n" ++
            "---------------------------------\n"
  appendFile "Output.txt" str
  putStrLn $ "Pre: " ++ pp p
  putStrLn $ "Stmt: " ++ pp stmt
  putStrLn $ "Post: " ++ pp q
  putStrLn ""
  putStrLn "-----------------------------\n"