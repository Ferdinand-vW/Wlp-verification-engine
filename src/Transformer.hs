module Transformer where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import Data.SBV(modelExists)
import GCL
import Prover
import Control.Monad

s1 :: Stmt
s1 = var ["x" , "y"] 
            [ assume (i 0 .<  ref "x") ,
                inv (i 0 .<= ref "x")
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 1) ]),
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]

s2 :: Stmt
s2 = var ["x" , "y"] 
            [ assume (i 2 .<=  ref "x") ,
                inv (i 0 .<= ref "x")
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 2) ]),
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]





verifyProgram :: Stmt -> IO ()
verifyProgram (Var xs s) = do
  let allVars = S.toList $ collectVars (Var xs s)
      (Var xs' s') = snd $ updateVars allVars M.empty 0 (Var xs s)
      Pre pre = head s'
  putStrLn $ show allVars
  putStrLn $ show (Var xs' s')
  (invs,w) <- foldWlp True_ (tail s')

  when (not $ null invs) $ do
    putStrLn "------------------------------"
    putStrLn "Invariants are not valid:"
    mapM_ (putStrLn . show) xs

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
    let assign = assignQ q s e2
    return $ ([],assign)
wlp (Post e) q = do
    return $ ([],e .&& q)
wlp (Pre e) q = do
    return $ ([],e .==> q)
wlp (If g s1 s2) q = do
    (xs,e1) <- wlp s1 q
    (ys,e2) <- wlp s2 q
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
wlp (While e1 b) q = error "We do not allow a While without an invariant.."
wlp _ _ = error "Not supported by our wlp function"

--The assignQ is for now only allowing variable names.
assignQ :: Expr -> String -> Expr -> Expr
assignQ (Lit i)        ref expr = Lit i
assignQ (Name s)       ref expr | s == ref = expr --a is the assigned value.
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
--assignQ (Repby var index value) n = Repby (assignQ var n) index (assignQ value n)

name :: Expr -> String
name (Name s) = s
name _ = error "Not yet allowed"

updateVars :: [String] -> M.Map String String -> Int -> Stmt -> (Int,Stmt)
updateVars vars newvars n Skip = (n,Skip)
updateVars vars newvars n (Assign e1 e2) = 
  let (n',e1') = updateVarInExpr vars newvars n e1
      (n'',e2') = updateVarInExpr vars newvars n' e2
  in  (n'', Assign e1' e2')
updateVars vars newvars n (Pre e) = 
  let (n',e') = updateVarInExpr vars newvars n e
  in (n',Pre e')
updateVars vars newvars n (Post e) = 
  let (n',e') = updateVarInExpr vars newvars n e
  in (n', Post e')
updateVars vars newvars n (If g s1 s2) = 
  let (n',g') = updateVarInExpr vars newvars n g
      (n'',s1') = updateVars vars newvars n' s1
      (n''',s2') = updateVars vars newvars n'' s2
  in  (n''',If g' s1' s2')
updateVars vars newvars n (Inv i (While g s)) = 
  let (n',i') = updateVarInExpr vars newvars n i
      (n'',g') = updateVarInExpr vars newvars n' g
      (n''', s') = foldr (\x (k,ys) -> let (k', st) = updateVars vars newvars k x
                                       in (k',st:ys)) (n'',[]) s
  in  (n''',Inv i' (While g' s'))
updateVars vars newvars n (Var vars' stmts) = 
  let dupVars = L.intersect vars vars'
      freshVars = map (++ show n) dupVars
      freshVarsMap = M.fromList $ zip dupVars freshVars
      remVars = vars' L.\\ dupVars
      (n',stmts') = foldr (\x (i,y) -> let (i',x') = updateVars vars (M.union freshVarsMap newvars) i x
                                       in (i',x':y)) (n+1,[]) stmts
  in (n',Var (L.union remVars freshVars) stmts')

updateVarInExpr :: [String] -> M.Map String String -> Int -> Expr -> (Int,Expr)
updateVarInExpr vars newvars n (Lit i) = (n,Lit i)
updateVarInExpr vars newvars n (Name s) = 
  case M.lookup s newvars of
    Nothing -> (n,Name s)
    Just s' -> (n,Name s')
updateVarInExpr vars newvars n (Plus e1 e2) = 
  let (n', e1') = updateVarInExpr vars newvars n e1
      (n'', e2') = updateVarInExpr vars newvars n' e2
  in  (n'', Plus e1' e2')
updateVarInExpr vars newvars n (Minus e1 e2) = 
  let (n', e1') = updateVarInExpr vars newvars n e1
      (n'',e2') = updateVarInExpr vars newvars n' e2
  in  (n'', Minus e1' e2')
updateVarInExpr vars newvars n (Equal e1 e2) = 
  let (n', e1') = updateVarInExpr vars newvars n e1
      (n'',e2') = updateVarInExpr vars newvars n' e2
  in  (n'', Equal e1' e2')
updateVarInExpr vars newvars n (Lower e1 e2) = 
  let (n', e1') = updateVarInExpr vars newvars n e1
      (n'', e2') = updateVarInExpr vars newvars n' e2
  in  (n'', Lower e1' e2')
updateVarInExpr vars newvars n (LowerE e1 e2) = 
  let (n', e1') = updateVarInExpr vars newvars n e1
      (n'', e2') = updateVarInExpr vars newvars n' e2
  in  (n'', LowerE e1' e2')
updateVarInExpr vars newvars n (And e1 e2) = 
  let (n', e1') = updateVarInExpr vars newvars n e1
      (n'', e2') = updateVarInExpr vars newvars n' e2
  in  (n'', And e1' e2')
updateVarInExpr vars newvars n (Or e1 e2)     = 
  let (n', e1') = updateVarInExpr vars newvars n e1
      (n'', e2') = updateVarInExpr vars newvars n' e2
  in  (n'', Or e1' e2')
updateVarInExpr vars newvars n (Not e) = 
  let (n', e') = updateVarInExpr vars newvars n e
  in (n', Not e')
updateVarInExpr vars newvars n (ForAll s e)   =
  case L.find (==s) vars of
    Nothing -> let varEntry = M.insert s s newvars
                   (n', e') = updateVarInExpr (s : vars) varEntry n e
               in (n',ForAll s e')
    Just s' -> let newVar = s' ++ show n
                   updateVars = M.adjust (\_ -> newVar) s' newvars
                   (n', e') = updateVarInExpr vars updateVars (n + 1) e
               in (n',ForAll newVar e')

collectVars :: Stmt -> S.Set String
collectVars (Var xs stmts) = S.union (S.fromList xs) (foldr (S.union . collectVars) S.empty stmts)
collectVars (Inv _ stmt) = collectVars stmt
collectVars (While _ stmts) = foldr (S.union . collectVars) S.empty stmts
collectVars (If _ stmt1 stmt2) = S.union (collectVars stmt1) (collectVars stmt2)
collectVars _ = S.empty

implIsValid :: Expr -> Expr -> IO Bool
implIsValid e1 e2 = do
  validity <- proveImpl e1 e2
  putStrLn $ show validity
  return $ not $ modelExists validity

mergeEither :: (a -> a -> a) -> Either [a] a -> Either [a] a -> Either [a] a
mergeEither _ (Left xs) (Left ys) = Left (xs ++ ys)
mergeEither _ (Left xs) _ = Left xs
mergeEither _ _ (Left ys) = Left ys
mergeEither op (Right e1) (Right e2) = Right (e1 `op` e2)
