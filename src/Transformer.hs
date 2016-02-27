module Transformer where

import qualified Data.List as L

import GCL
import Prover

s1 :: Stmt
s1 = var ["x" , "y"] 
            [ assume (i 0 .<  ref "x") ,
                inv (i 0 .<= ref "x" )
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 1) ]),
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]

verifyProgram :: Stmt -> IO ()
verifyProgram (Var xs s) = do
  let (Var _ s') = snd $ updateVars xs 0 (Var [] s)
      w = foldr (\x y -> wlp x y) True_ (tail s')
      Pre pre = head s
  proveImpl pre w >>= print

wlp :: Stmt -> Expr -> Expr
wlp (Var vars s) q = foldr (\x y -> wlp x y) True_ s
wlp Skip q = q
wlp (Assign e1 e2) q = assignQ q (name e1,name e2)
wlp (Post e)      q    = e .&& q
wlp (Pre e)       q    = e .==> q
wlp (If g s1 s2) q    = (g .&& wlp s1 q) .&& ((.!) g .&& wlp s2 q)
wlp (Inv i (While g s)) q  = ((i .&& (.!) g) .==> q) .&& ((i .&& g) .==> i)
wlp (While e1 b)  q    = error "We do not allow a While without an invariant.."
wlp _ _ = error "Not supported by our wlp function"


--The assignQ is for now only allowing variable names.
assignQ :: Expr -> (String,String) -> Expr
assignQ (Lit e)        n = i e
assignQ (Name n)       (a,t) | n == t = Name a --a is the assigned value.
                             | otherwise = Name n
assignQ (ForAll s e)   n  = ForAll s $ assignQ e n
assignQ (Minus e1 e2)  n  = Minus  (assignQ e1 n) (assignQ e2 n)
assignQ (Plus e1 e2)   n  = Plus   (assignQ e1 n) (assignQ e2 n)
assignQ (Equal e1 e2)  n  = Equal  (assignQ e1 n) (assignQ e2 n)
assignQ (Lower e1 e2)  n  = Lower  (assignQ e1 n) (assignQ e2 n)
assignQ (LowerE e1 e2) n  = LowerE (assignQ e1 n) (assignQ e2 n)
assignQ (And e1 e2)    n  = And    (assignQ e1 n) (assignQ e2 n)
assignQ (Or e1 e2)     n  = Or     (assignQ e1 n) (assignQ e2 n)
assignQ (Not e1)       n  = Not    e1
assignQ True_          n  = True_

name :: Expr -> String
name (Name s) = s
name _ = error "Not yet allowed"

--updateVars :: [String] -> Int -> [Stmt] -> (Int,[Stmt])
--updateVars vars n stmts = foldr (\x (n',ys) -> updateVars' vars) (n,[]) stmts

updateVars :: [String] -> Int -> Stmt -> (Int,Stmt)
updateVars vars n Skip = (n,Skip)
updateVars vars n (Assign e1 e2) = let (n',e1') = updateVarInExpr vars n e1
                                       (n'',e2') = updateVarInExpr vars n'' e2'
                                   in (n'', Assign e1' e2')
updateVars vars n (Pre e) = let (n',e') = updateVarInExpr vars n' e
                            in (n',Pre e')
updateVars vars n (Post e) = let (n',e') = updateVarInExpr vars n e
                             in (n', Post e')
updateVars vars n (If g s1 s2) = let (n',g') = updateVarInExpr vars n g
                                     (n'',s1') = updateVars vars n' s1
                                     (n''',s2') = updateVars vars n'' s2
                                 in (n''',If g' s1' s2')
updateVars vars n (Inv i (While g s)) = let (n',i') = updateVarInExpr vars n i
                                            (n'',g') = updateVarInExpr vars n' g
                                            (n''', s') = foldr (\x (k,ys) -> let (k', st) = updateVars vars k x
                                                                             in (k',st:ys)) (n'',[]) s
                                        in (n''',Inv i' (While g' s'))
updateVars vars n (Var vars' stmts) = let dupVars = L.intersect vars vars'
                                          freshVars = map (++ show n) dupVars
                                          remVars = vars L.\\ dupVars
                                          (n',stmts') = foldr (\x (i,y) -> let (i',x') = updateVars dupVars i x
                                                                           in (i',x':y)) (n+1,[]) stmts
                                     in (n',Var (L.union remVars freshVars) stmts')

updateVarInExpr :: [String] -> Int -> Expr -> (Int,Expr)
updateVarInExpr vars n (Lit i) = (n,Lit i)
updateVarInExpr vars n (Name s) =
  case L.find (==s) vars of
    Nothing -> (n,Name s)
    Just s' -> (n,Name (s'++show n))
updateVarInExpr vars n (Plus e1 e2)   = let (n', e1') = updateVarInExpr vars n e1
                                            (n'', e2') = updateVarInExpr vars n' e2
                                        in (n'', Plus e1' e2')
updateVarInExpr vars n (Minus e1 e2)  = let (n', e1') = updateVarInExpr vars n e1
                                            (n'',e2') = updateVarInExpr vars n' e2
                                        in (n'', Minus e1' e2')
updateVarInExpr vars n (Equal e1 e2)  = let (n', e1') = updateVarInExpr vars n e1
                                            (n'',e2') = updateVarInExpr vars n' e2
                                        in (n'', Equal e1' e2')
updateVarInExpr vars n (Lower e1 e2)  = let (n', e1') = updateVarInExpr vars n e1
                                            (n'', e2') = updateVarInExpr vars n' e2
                                        in (n'', Lower e1' e2')
updateVarInExpr vars n (LowerE e1 e2) = let (n', e1') = updateVarInExpr vars n e1
                                            (n'', e2') = updateVarInExpr vars n' e2
                                        in (n'', LowerE e1' e2')
updateVarInExpr vars n (And e1 e2)    = let (n', e1') = updateVarInExpr vars n e1
                                            (n'', e2') = updateVarInExpr vars n' e2
                                        in (n'', And e1' e2')
updateVarInExpr vars n (Or e1 e2)     = let (n', e1') = updateVarInExpr vars n e1
                                            (n'', e2') = updateVarInExpr vars n' e2
                                        in (n'', Or e1' e2')
updateVarInExpr vars n (Not e)        = let (n', e') = updateVarInExpr vars n e
                                        in (n', Not e')
updateVarInExpr vars n (ForAll s e)   =
  case L.find (==s) vars of
    Nothing -> let (n', e') = updateVarInExpr vars n e
               in (n',ForAll s e')
    Just s' -> let newVar = s' ++ show n
                   (n', e') = updateVarInExpr (newVar :vars) n' e'
               in (n',ForAll newVar e')