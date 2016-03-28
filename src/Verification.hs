module Verification where

import Control.Concurrent
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import qualified Data.SBV as SBV

import Data.SBV(modelExists)
import Collect
import Transformer
import GCL
import Prover
import Control.Monad

import PrettyPrint
import Examples


verify :: Stmt -> IO ()
verify stmt = do
  let (Vars xs stmts, vars) = transform stmt
      Pre pre = head stmts

  mvar <- newEmptyMVar
  t <- forkIO (readInput mvar)
  putStrLn ""
  putStrLn "Press space to start verification step-by-step or press N to skip to result\n"
  (invs,w) <- foldWlp mvar vars True_ (tail stmts)

  unless (null invs) $ do
    putStrLn "------------------------------"
    putStrLn "Invariants are not valid:"
    mapM_ (\x -> do
        putStrLn $ pp x
        putStrLn "\n\n") invs

  putStrLn "\n\n\n-----------------------------"
  putStrLn "RESULT: "
  putStrLn "Given precondition: "
  putStrLn $ pp pre
  putStrLn "Calculated precondition: " 
  putStrLn $ pp w
  putStrLn ""
  putStrLn "Now proving implication: "
  proveImpl vars pre w >>= putStrLn . show
  killThread t
      

foldWlp :: MVar Char -> [Var] -> Expr -> [Stmt] -> IO ([Expr],Expr)
foldWlp mvar vars e s = foldr (\x y -> do
                      (xs,expr) <- y
                      (ys,expr') <- wlp mvar vars x expr
                      return (xs ++ ys, expr')) (return ([],e)) s

--The wlp function will calculate the WLP.
--We do by pattern matching over the statements.
--Most of the cases pretty straightforward, so we will not explain all cases.
wlp :: MVar Char -> [Var] -> Stmt -> Expr -> IO ([Expr],Expr)
wlp mvar vars Skip q = do
    doStep mvar
    stepPrint q Skip q
    return ([],q)
wlp mvar vars (Vars vars' s) q = foldWlp mvar vars q s
--The assignQ function goes over the q and tries to find the reference and substitute the old value with e2.
--The reference is a tuple. The left element of this tuple is the reference Expr and the right element is used as an option for an array with an index.. 
wlp mvar vars (Assign (Name s) e2) q = do
    doStep mvar
    let assign = assignQ q (s, Nothing) e2
    stepPrint q (Assign (Name s) e2) assign
    return ([],assign)
wlp mvar vars (Assign (Repby (Name s) i) e2) q = do
    doStep mvar
    let assign = assignQ q (s,Just i) e2
    stepPrint q (Assign (Repby (Name s) i) e2) assign
    return ([],assign)
wlp mvar vars (Post e) q = do
    doStep mvar
    stepPrint q (Post e) (e .&& q)
    return ([],e .&& q)
wlp mvar vars (Pre e) q = do
    doStep mvar
    stepPrint q (Pre e) (e .==> q)
    return ([],e .==> q)
wlp mvar vars (If g s1 s2) q = do
    (xs,e1) <- foldWlp mvar vars q s1
    (ys,e2) <- foldWlp mvar vars q s2
    stepPrint q (If g s1 s2) ((g .&& e1) .|| (neg g .&& e2))
    return (xs ++ ys, (g .&& e1) .|| (neg g .&& e2))
-- In order to calculate the WLP of the while we first have to proof the correctness of an invariant.
-- First we calculate the wlp s i
-- Second, we proof the (i && not g) => q
-- Third, we proof (i && g) => wlp s i
-- If the invariant is not correct, we still continue calculating the WLP. However, it will be reported that the while is not correct.
wlp mvar vars (Inv i (While g s)) q  = do
    impl1Val <- implIsValid vars (i .&& neg g) q
    (xs,wlpOfS) <- foldWlp mvar vars i s
    impl2Val <- implIsValid vars (i .&& g) wlpOfS
    let implInv1 = if impl1Val then [] else [i .&& neg g .==> q]
        implInv2 = if impl2Val then xs else (i .&& g .==> wlpOfS) : xs
        invs = implInv1 ++ implInv2
    if impl1Val && impl2Val
      then do
        stepPrint q (Inv i (While g [])) i
        return (invs,i)
      else do
        stepPrint q (Inv i (While g [])) (neg g .&& q)
        return (i : xs,neg g .&& q)
wlp mvar vars (While g s) q = do
    putStrLn "Loop reduction"
    putStrLn ("Q: " ++ show q)
    fixpoint <- loopReduction 10 (neg g .==> q) g s q mvar vars
    putStrLn "End loop reduction"
    return ([],fixpoint)
wlp mvar vars Prog{} q = return ([],q)
wlp mvar _ _ _ = error "Not supported by our wlp function"

--In loopReducation we try to calculate the fixpoint.
--The first parameter is an int, which determines the amount of iterations. We will stop iterating when k is 0.
--The second parameters is the previous calculated post condition
--The g is the condition of the while
--the s is the body of the while
--To verify an fixpoint,  we try to imply the post condition with the previous post condition 
--However, when we do not find an fixpoint we return (neg g .&& q)
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
  print k
  impl1Val <- implIsValid vars e1 fixpoint
  if impl1Val 
      then return fixpoint
      else loopReduction (k-1) fixpoint g s q mvar vars

-- The AssignQ function is used to subsitute an Expr with an other Expr.
-- First we try to find the corresponding reference in q.
-- We do this by pattern matching over all the Expression in q.
-- When the corresponding reference is found we will subsitute the corresponding reference for expr.
assignQ :: Expr -> (String, Maybe Expr) -> Expr -> Expr
assignQ (Lit i)        ref expr = Lit i
assignQ (Name s)       ref expr 
        | s == fst ref = expr
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
assignQ (Repby (Name s) index) ref expr | isJust (snd ref) && index == fromJust (snd ref) &&  s == fst ref = expr
                                        | isNothing (snd ref) && s == fst ref = Repby expr (assignQ index ref expr)
                                        | otherwise = Repby (Name s) (assignQ index ref expr)
assignQ expr _ _ = error $ show expr


--If we are assigning to an array that is indexed, then we will have to compare the index of that array
--with the indexes that are used in the current wlp. We make permutations of these comparisons and for each
--permutation we do the relevant substitution.
permExpr :: Expr -> (String, Maybe Expr) -> Expr -> Expr
permExpr (Lower e1 e2)  ref@(s, Nothing) args = Lower (assignQ e1 ref args) (assignQ e2 ref args)
permExpr (LowerE e1 e2) ref@(s, Nothing) args = LowerE (assignQ e1 ref args) (assignQ e2 ref args)
permExpr (Equal e1 e2)  ref@(s, Nothing) args = Equal (assignQ e1 ref args) (assignQ e2 ref args)
permExpr expr (s, Just i) args = 
  let xs = permArrays s $ findArrays expr
      perms = permutations xs
      permExprs = map (\ys -> permutation expr xs ys i .==> replaceArray ys s args expr) perms
  in case length xs > 0 of --In the case that we could not find any indexed arrays in the wlp
        True -> foldr1 (.&&) permExprs
        False -> expr

--Generates a single permutation
permutation :: Expr -> [Expr] -> [Expr] -> Expr -> Expr
permutation e all pExprs index = 
  let negs = all L.\\ pExprs
      eqExprs = foldr1 (.&&) (map (.== index) pExprs)
  in case length (negs ++ pExprs) <= 0 of
        False -> foldr1 (.&&) (map (.== index) pExprs ++ map (neg . (.==) index) negs)
        True -> error $ pp e

--Substitution
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


--Find all possible permutations
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = xs : L.nub [ zs | (x,ys) <- selections xs, zs <- permutations ys ]

--General selection function
selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

--We can only substitute arrays that have the correct name
permArrays :: String -> [Expr] -> [Expr]
permArrays s xs = map (\(Repby _ i) -> i) $ filter (\(Repby (Name s1) index) -> s1 == s) xs

--Find all indexed arrays
findArrays :: Expr -> [Expr]
findArrays (Lower e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (LowerE e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (Equal e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (Minus e1 e2)  = findArrays e1 ++ findArrays e2
findArrays (Plus e1 e2) = findArrays e1 ++ findArrays e2
findArrays (Repby s i) = [Repby s i]
findArrays (Name s) = []
findArrays (Lit i) = []

--Connects to the prover to determine whether the implication is valid
implIsValid :: [Var] -> Expr -> Expr -> IO Bool
implIsValid vars e1 e2 = do
  validity <- proveImpl vars e1 e2
  return $ not $ modelExists validity

--Some interactivity functions
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