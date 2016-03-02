module Transformer where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import GCL
import Prover
import Control.Monad

s1 :: Stmt
s1 = var ["x" , "y"] 
            [ assume (i 0 .<  ref "x") ,
                inv (i 0 .<= ref "x" )
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 1) ]),
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]

s2 :: Stmt
s2 = var ["x" , "y"] 
            [ assume (i 2 .<=  ref "x") ,
                inv (i 3 .<= ref "x" )
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 2) ]),
                var ["x","y","z"]
                  [ref "y" .= ref "x" `minus` i 5 `minus` ref "z"],
                ref "y"  .= ref "x" `minus` i 2,
                ref "z" .= ref "z",
                assert (ref "y" .== i 0)
            ]



verifyProgram :: Stmt -> IO ()
verifyProgram (Var xs s) = do
  putStrLn $ show s
  let allVars = S.toList $ collectVars (Var xs s)
      (Var _ s') = snd $ updateVars allVars M.empty 0 (Var xs s)
      Pre pre = head s'
  putStrLn $ show allVars
  putStrLn $ show s'
  putStrLn "Do WLp"
  w <- foldWlp True_ (tail s')
  putStrLn "Done with Wlp"
  putStrLn $ show pre
  putStrLn $ show w
  proveImpl pre w >>= print

foldWlp :: Expr -> [Stmt] -> IO Expr
foldWlp e s = foldr (\x y -> do
                        y' <- y
                        wlp x y') (return e) s

--Wlp without IO
--wlp :: Stmt -> Expr -> Expr
--wlp (Var vars s) q = foldr (\x y -> wlp x y) True_ s
--wlp Skip q = q
--wlp (Assign e1 e2) q = assignQ q (name e1,name e2)
--wlp (Post e)      q    = e .&& q
--wlp (Pre e)       q    = e .==> q
--wlp (If g s1 s2) q    = (g .&& wlp s1 q) .&& ((.!) g .&& wlp s2 q)
--wlp (Inv i (While g s)) q  = ((i .&& (.!) g) .==> q) .&& ((i .&& g) .==> i)
--wlp (While e1 b)  q    = error "We do not allow a While without an invariant.."
--wlp _ _ = error "Not supported by our wlp function"


wlp :: Stmt -> Expr -> IO Expr
wlp Skip q = do
    return q
wlp (Var vars s) q = do
    putStrLn "var"
    p <- foldWlp True_ s
    return p
wlp (Assign (Name s) e2) q = do
    let assign = assignQ q s e2
    putStrLn $ s ++ " q:" ++ show q --Why can't I print e2?
    return $ assign
wlp (Post e) q = do
    putStrLn $ "post: " ++ show (e .&& q)
    return $ e .&& q
wlp (Pre e) q = do
    putStrLn $ "pre : " ++ show (e .==> q)
    return $ e .==> q
wlp (If g s1 s2) q = do
    putStrLn "if"
    p <- wlp s1 q
    return p 
wlp (Inv i (While g s)) q  = do
    putStrLn $ "inv: " ++ show i 
    putStrLn $ "cond: " ++ show g
    putStrLn $ "while not true " ++ show (i .&& (.!) g)
    putStrLn $ "q: " ++ show q
    --return $ ((i .&& (.!) g) .==> q) .&& ((i .&& g) .==> i)
    return i
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
      remVars = vars L.\\ dupVars
      (n',stmts') = foldr (\x (i,y) -> let (i',x') = updateVars vars (M.union freshVarsMap newvars) i x
                                       in (i',x':y)) (n+1,[]) stmts
 in if null vars'
      then (n',Var (L.union remVars freshVars) stmts')
      else (n',Var (L.union remVars freshVars) stmts'){-error $ show stmts ++ " " ++
                   show dupVars ++ "  " ++ show freshVars 
                   ++ "  " ++ show remVars ++ " " ++ 
                   show vars ++ " " ++ show vars'-}
 --

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
    Nothing -> let (n', e') = updateVarInExpr vars newvars n e
               in (n',ForAll s e')
    Just s' -> let newVar = s' ++ show n
                   (n', e') = updateVarInExpr (newVar : vars) newvars n' e'
               in (n',ForAll newVar e')

collectVars :: Stmt -> S.Set String
collectVars (Var xs stmts) = S.union (S.fromList xs) (foldr (S.union . collectVars) S.empty stmts)
collectVars (Inv _ stmt) = collectVars stmt
collectVars (While _ stmts) = foldr (S.union . collectVars) S.empty stmts
collectVars (If _ stmt1 stmt2) = S.union (collectVars stmt1) (collectVars stmt2)
collectVars _ = S.empty 