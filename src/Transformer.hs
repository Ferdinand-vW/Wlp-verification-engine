module Transformer where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as MA
import qualified Data.SBV as SBV

import Data.SBV(modelExists)
import SyntaxTransformer
import GCL
import Prover
import Control.Monad

s1 :: Stmt
s1 = var ["x" , "y","n"] 
            [ assume (i 0 .<  ref "x") ,
                (ref "n") .= (forall "n" (forall "n" (ref "n"))),
                (ref "n") .= (forall "n" (forall "n" (ref "n"))),
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
                var ["x","z"]
                  [
                    ref "x" .= ref "y",
                    var ["x","z"]
                      [
                        ref "z" .= forall "x" (forall "x" (ref "x" .< ref "y"))
                      ]
                  ],
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]


s3 :: Stmt
s3 = var ["a", "i", "j","k"] [
             assume (i 0 .<=  ref "i") , 
            ref "a" `repby` ref "i" .=  i 10,
            ref "a" `repby` ref "j" .=  i 11,
            assert (ref "a" `repby` ref "i" .== i 10)
          ]

swap :: Stmt
swap = var ["a", "i", "j", "tmp", "c", "b"] 
            [assume ((ref "a" `repby` ref "i" .== ref "c") .&& (ref "a" `repby` ref "j" .== ref "b")), 
            ref "tmp" .=  ref "a" `repby` ref "i",
                ref "a" `repby` ref "i" .=  ref "a" `repby` ref "j",
                ref "a" `repby` ref "j" .=  ref "i",
                assert ((ref "tmp" .== ref "b") .&& (ref "a" `repby` ref "i" .== ref "c") .&& (ref "a" `repby` ref "j" .== ref "b"))
            ]

swap' :: Stmt
swap' = var ["i", "j", "tmp", "c", "b"] 
            [assume ((ref "i" .== ref "b") .&& (ref "j" .== ref "c")), 
            ref "tmp" .= ref "i",
                ref "i" .=  ref "j",
                ref "j" .=  ref "tmp",
                assert ((ref "i" .== ref "c") .&& (ref "j" .== ref "b"))
            ]


swap'' :: Stmt
swap'' = var ["a", "i", "j", "tmp", "c", "b"] 
            [assume ((ref "a" `repby` i 0 .== ref "b") .&& (ref "a" `repby` i 1 .== ref "c")), 
            ref "tmp" .=  ref "a" `repby` i 0,
                ref "a" `repby` i 0 .=  ref "a" `repby` i 1,
                ref "a" `repby` i 1 .=  ref "tmp",
                assert ((ref "tmp" .== ref "b") .&& (ref "a" `repby` i 0 .== ref "c") .&& (ref "a" `repby` i 1 .== ref "b"))
            ]
verifyProgram :: Stmt -> IO ()
verifyProgram stmt = do
  --let allVars = S.toList $ collectVars (Var xs s)
      --(Var xs' s') = snd $ freshVars allVars M.empty 0 (Var xs s)
      --Pre pre = head s'
  let (Var xs stmts) = stmt_trans stmt
      Pre pre = head stmts
  putStrLn $ show (Var xs stmts)
  (invs,w) <- foldWlp True_ (tail stmts)

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
assignQ (Repby (Name s) index) ref expr | index == (MA.fromJust $ snd ref) &&  s == fst ref = expr
                                              | otherwise = (Repby (Name s) index)


name :: Expr -> String
name (Name s) = s
name _ = error "Not yet allowed"

{-freshVarsStmt :: [String] -> M.Map String String -> Int -> Stmt -> (Int,Stmt)
freshVarsStmt vars newvars n Skip = (n,Skip)
freshVarsStmt vars newvars n (Assign e1 e2) = 
  let (n',e1') = freshVarsExpr vars newvars n e1
      (n'',e2') = freshVarsExpr vars newvars n' e2
  in  (n'', Assign e1' e2')
freshVarsStmt vars newvars n (Pre e) = 
  let (n',e') = freshVarsExpr vars newvars n e
  in (n',Pre e')
freshVarsStmt vars newvars n (Post e) = 
  let (n',e') = freshVarsExpr vars newvars n e
  in (n', Post e')
freshVarsStmt vars newvars n (If g s1 s2) = 
  let (n',g') = freshVarsExpr vars newvars n g
      (n'',s1') = freshVarsStmt vars newvars n' s1
      (n''',s2') = freshVarsStmt vars newvars n'' s2
  in  (n''',If g' s1' s2')
freshVarsStmt vars newvars n (Inv i (While g s)) = 
  let (n',i') = freshVarsExpr vars newvars n i
      (n'',g') = freshVarsExpr vars newvars n' g
      (n''', s') = foldr (\x (k,ys) -> let (k', st) = freshVarsStmt vars newvars k x
                                       in (k',st:ys)) (n'',[]) s
  in  (n''',Inv i' (While g' s'))
freshVarsStmt vars newvars n (Var vars' stmts) = 
  let dupVars = L.intersect vars vars'
      freshVars = map (++ show n) dupVars
      freshVarsMap = M.fromList $ zip dupVars freshVars
      remVars = vars' L.\\ dupVars
      (n',stmts') = foldr (\x (i,y) -> let (i',x') = freshVarsStmt vars (M.union freshVarsMap newvars) i x
                                       in (i',x':y)) (n+1,[]) stmts
  in (n',Var (L.union remVars freshVars) stmts')

freshVarsExpr :: [String] -> M.Map String String -> Int -> Expr -> (Int,Expr)
freshVarsExpr vars newvars n (Lit i) = (n,Lit i)
freshVarsExpr vars newvars n (Name s) = 
  case M.lookup s newvars of
    Nothing -> (n,Name s)
    Just s' -> (n,Name s')
updateVarInExpr vars newvars n (Repby var index) =
  let (n', var') = updateVarInExpr vars newvars n var
  in (n', Repby var' index)
  in  (n'', Plus e1' e2')
freshVarsExpr vars newvars n (Minus e1 e2) = 
  let (n', e1') = freshVarsExpr vars newvars n e1
      (n'',e2') = freshVarsExpr vars newvars n' e2
  in  (n'', Minus e1' e2')
freshVarsExpr vars newvars n (Equal e1 e2) = 
  let (n', e1') = freshVarsExpr vars newvars n e1
      (n'',e2') = freshVarsExpr vars newvars n' e2
  in  (n'', Equal e1' e2')
freshVarsExpr vars newvars n (Lower e1 e2) = 
  let (n', e1') = freshVarsExpr vars newvars n e1
      (n'', e2') = freshVarsExpr vars newvars n' e2
  in  (n'', Lower e1' e2')
freshVarsExpr vars newvars n (LowerE e1 e2) = 
  let (n', e1') = freshVarsExpr vars newvars n e1
      (n'', e2') = freshVarsExpr vars newvars n' e2
  in  (n'', LowerE e1' e2')
freshVarsExpr vars newvars n (And e1 e2) = 
  let (n', e1') = freshVarsExpr vars newvars n e1
      (n'', e2') = freshVarsExpr vars newvars n' e2
  in  (n'', And e1' e2')
freshVarsExpr vars newvars n (Or e1 e2)     = 
  let (n', e1') = freshVarsExpr vars newvars n e1
      (n'', e2') = freshVarsExpr vars newvars n' e2
  in  (n'', Or e1' e2')
freshVarsExpr vars newvars n (Not e) = 
  let (n', e') = freshVarsExpr vars newvars n e
  in (n', Not e')
freshVarsExpr vars newvars n (ForAll s e)   =
  case L.find (==s) vars of
    Nothing -> let varEntry = M.insert s s newvars
                   (n', e') = freshVarsExpr (s : vars) varEntry n e
               in (n',ForAll s e')
    Just s' -> let newVar = s' ++ show n
                   freshVars = M.adjust (\_ -> newVar) s' newvars
                   (n', e') = freshVarsExpr vars freshVars (n + 1) e
               in (n',ForAll newVar e')

collectVars :: Stmt -> S.Set String
collectVars (Var xs stmts) = S.union (S.fromList xs) (foldr (S.union . collectVars) S.empty stmts)
collectVars (Inv _ stmt) = collectVars stmt
collectVars (While _ stmts) = foldr (S.union . collectVars) S.empty stmts
collectVars (If _ stmt1 stmt2) = S.union (collectVars stmt1) (collectVars stmt2)
collectVars _ = S.empty-}

implIsValid :: Expr -> Expr -> IO Bool
implIsValid e1 e2 = do
  validity <- proveImpl e1 e2
  putStrLn $ show validity
  return $ not $ modelExists validity
