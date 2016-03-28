module Transformer (
transform, toPrenexNF, mkFreshExpr
)where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import qualified Data.SBV as SBV

import PrettyPrint
import Data.SBV(modelExists)
import Collect
import Control.Monad

--The transform function will first collect information about vars,refs and program calls.
--After the collection it will transform the code with mkFreshStmt. The 0 after mkFreshStmt is the intial value to introduce new variables.
transform :: Stmt -> (Stmt, [Var])
transform stmt =
  let vars = collectVars stmt
      progMap = collectPrograms stmt
      varsL = map nameOf $ S.toList vars
      varMap = M.fromList $ zip varsL varsL
      freshStmt = snd $ mkFreshStmt 0 varMap progMap stmt
  in (freshStmt, S.toList (collectVars freshStmt))


mkFreshStmts :: Int -> M.Map String String -> M.Map String ([Var],[Var],[Stmt]) -> [Stmt] -> (Int,[Stmt])
mkFreshStmts n varMap progMap stmts =
  foldr (\x (y,stmts') -> let (n',stmt) = mkFreshStmt y varMap progMap x
                          in (n',stmt : stmts')) (n,[]) stmts

--
mkFreshStmt :: Int -> M.Map String String -> M.Map String ([Var],[Var],[Stmt])-> Stmt -> (Int,Stmt)
mkFreshStmt n varMap progMap (Vars xs stmts) =
  let dupVars = filter (\x -> M.member (nameOf x) varMap) xs
      freshVars = map (modifyName (++ show n)) dupVars
      remVars = xs L.\\ dupVars
      newVarMap = M.union (M.fromList (zip (map nameOf dupVars) (map nameOf freshVars))) varMap
      (n1,newStmts) = mkFreshStmts (n + 1) newVarMap progMap stmts
  in (n1, Vars (L.union remVars freshVars) newStmts)
mkFreshStmt n varMap progMap (PCall name args vars) =
  let (n1,expr1) = mkFreshExprs n varMap args
      (n2,expr2) = mkFreshExprs n1 varMap vars
      (prms,resv,body) = fromJust $ M.lookup name progMap
      dupPrms = filter (\x -> M.member (nameOf x) varMap) prms 
      dupResv = filter (\x -> M.member (nameOf x) varMap) resv
      freshPrms = map (modifyName (++ show n)) dupPrms
      freshResv = map (modifyName (++ show n)) dupResv
      remPrms = prms L.\\ dupPrms
      remResv = resv L.\\ dupResv
      newVarMap = M.union (M.fromList (zip ((map nameOf dupPrms) ++ (map nameOf dupResv)) 
                          ((map nameOf freshPrms) ++ (map nameOf freshResv)))) varMap
      (n3,body1) = mkFreshStmts (n + 1) newVarMap progMap body
      allPrms = L.union remPrms freshPrms
      prmsName = map nameOf allPrms
      allResv = L.union remResv freshResv
      resvName = map nameOf allResv
      transformed = Vars (L.union allPrms allResv)
          (
            map (\(x,y) -> Assign (Name x) y) (zip prmsName expr1)
            ++
            body1
            ++
            map (\(x,y) -> Assign x (Name y)) (zip expr2 resvName)
          )
  in (n3,transformed)
mkFreshStmt n varMap progMap (Sim vars1 vars2) =
  let (n1,expr1) = mkFreshExprs n varMap vars1
      (n2,expr2) = mkFreshExprs n1 varMap vars2
      varsList = foldr (\x y -> name x : y) [] vars1
      exprToTuples = foldr (\x y -> exprToTuple x : y) [] vars1
      dupVars = filter (\x -> M.member x varMap) varsList
      oldVars = [fromJust $ M.lookup x varMap | x <- dupVars]
      newVars = map (++ show n) dupVars
      newVarsLeft = M.fromList $ map (\(x,y) -> (x ++ show n,y)) exprToTuples
      newexprToTuplesRight = [(fromJust $ M.lookup x varMap,y) | (x,y) <- exprToTuples , x `M.member` varMap]
      freshMap = M.fromList $ zip oldVars newVars
      modRight = foldr (\x y -> (modifyExpr x varMap freshMap) : y) [] vars2
      newVarsMap = foldr (\x y -> toVar x : y) [] $ M.toList newVarsLeft
      headStmp = foldr (\(x,y) z -> (Assign (Name x) (Name y)) : z) [] $ zip newVars oldVars
      bodyStmp = foldr (\(x,y) z -> (Assign x (correctNameBody y varMap)): z ) [] $ zip expr1 modRight
      transformed = Vars newVarsMap $ headStmp ++ bodyStmp  
  in  (n2,transformed)
mkFreshStmt n varMap progMap (Pre expr) =
  let (n1,expr1) = mkFreshExpr n varMap expr
  in (n1,Pre expr1)
mkFreshStmt n varMap progMap (Post expr) =
  let (n1,expr1) = mkFreshExpr n varMap expr
  in (n1,Post expr1)
mkFreshStmt n varMap progMap (Inv cond stmt) = 
  let (n1,expr1) = mkFreshExpr n varMap cond
      (n2,stmt1) = mkFreshStmt n1 varMap progMap stmt
  in (n2, Inv expr1 stmt1)
mkFreshStmt n varMap progMap (While cond body) =
  let (n1,expr1) = mkFreshExpr n varMap cond
      (n2,stmts1) = mkFreshStmts n1 varMap progMap body
  in (n2,While expr1 stmts1)
mkFreshStmt n varMap progMap (If cond left right) = 
  let (n1,expr1) = mkFreshExpr n varMap cond
      (n2,stmt1) = mkFreshStmts n1 varMap progMap left
      (n3,stmt2) = mkFreshStmts n2 varMap progMap right
  in (n3,If expr1 stmt1 stmt2)
mkFreshStmt n varMap progMap (Assign left right) = 
  let (n1,expr1) = mkFreshExpr n varMap left
      (n2,expr2) = mkFreshExpr n1 varMap right
  in (n2,Assign expr1 expr2)
mkFreshStmt n varMap progMap stmt = (n,stmt)

mkFreshExprs :: Int -> M.Map String String -> [Expr] -> (Int,[Expr])
mkFreshExprs n varMap exprs =
  foldr (\x (y,exprs') ->  let (n',expr) = mkFreshExpr y varMap x
                          in (n',expr : exprs')) (n,[]) exprs

mkFreshExpr :: Int -> M.Map String String -> Expr -> (Int,Expr)
mkFreshExpr n varMap (Lit i) = (n,Lit i)
mkFreshExpr n varMap (Name s) = 
  case M.lookup s varMap of
    Nothing -> (n,Name s)
    Just s' -> (n,Name s')
mkFreshExpr n varMap (ForAll s expr) =
  let newVarMap = if M.member s varMap
                    then M.adjust (\s' -> s' ++ show n) s varMap
                    else M.insert s (s ++ show n) varMap
      (n1,expr1) = mkFreshExpr (n + 1) newVarMap expr
  in case M.lookup s newVarMap of
      Just s' -> (n1, ForAll s' expr1)
      Nothing -> error "undefined"
mkFreshExpr n varMap (Exists s expr) =
  let newVarMap = if M.member s varMap
                    then M.adjust (\s' -> s' ++ show n) s varMap
                    else M.insert s (s ++ show n) varMap
      (n1,expr1) = mkFreshExpr (n + 1) newVarMap expr
  in case M.lookup s newVarMap of
      Just s' -> (n1, Exists s' expr1)
      Nothing -> error "undefined"
mkFreshExpr n varMap (Minus e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, Minus expr1 expr2)
mkFreshExpr n varMap (Plus e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, Plus expr1 expr2)
mkFreshExpr n varMap (Equal e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, Equal expr1 expr2)
mkFreshExpr n varMap (Lower e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, Lower expr1 expr2)
mkFreshExpr n varMap (LowerE e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, LowerE expr1 expr2)
mkFreshExpr n varMap (And e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, And expr1 expr2)
mkFreshExpr n varMap (Or e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, Or expr1 expr2)
mkFreshExpr n varMap (Impl e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, Impl expr1 expr2)
mkFreshExpr n varMap (Repby e1 e2) = 
  let (n1,expr1) = mkFreshExpr n varMap e1
      (n2,expr2) = mkFreshExpr n1 varMap e2
  in (n2, Repby expr1 expr2)
mkFreshExpr n varMap (Not expr) = 
  let (n1,expr1) = mkFreshExpr n varMap expr
  in (n1, Not expr1)
mkFreshExpr n varMap expr = (n,expr)

name :: Expr -> String
name (Name s) = s
name (Repby (Name s) _) = s

exprToTuple :: Expr -> (String,Maybe Expr)
exprToTuple (Name s) = (s, Nothing)
exprToTuple (Repby (Name s) index) = (s, Just index)

toVar :: (String,Maybe Expr) -> Var
toVar (s, Nothing) = Int s
toVar (s, x) = Array s 

modifyExpr :: Expr -> M.Map String String -> M.Map String String -> Expr
modifyExpr (Lit i)        old new = Lit i
modifyExpr (Name s)       old new = Name $ newVar s old new
modifyExpr (ForAll s e)   old new = ForAll (newVar s old new) $ modifyExpr e old new
modifyExpr (Minus e1 e2)  old new = Minus  (modifyExpr e1 old new) (modifyExpr e2 old new)
modifyExpr (Plus e1 e2)   old new = Plus   (modifyExpr e1 old new) (modifyExpr e2 old new)
modifyExpr (Equal e1 e2)  old new = Equal  (modifyExpr e1 old new) (modifyExpr e2 old new)
modifyExpr (Lower e1 e2)  old new = Lower  (modifyExpr e1 old new) (modifyExpr e2 old new)
modifyExpr (LowerE e1 e2) old new = LowerE (modifyExpr e1 old new) (modifyExpr e2 old new)
modifyExpr (And e1 e2)    old new = And    (modifyExpr e1 old new) (modifyExpr e2 old new)
modifyExpr (Or e1 e2)     old new = Or     (modifyExpr e1 old new) (modifyExpr e2 old new)
modifyExpr (Not e1)       old new = Not    e1
modifyExpr True_          old new = True_
modifyExpr (Repby (Name s) index) old new = Repby (Name $ newVar s old new) (modifyExpr index old new)

newVar :: String -> M.Map String String -> M.Map String String -> String
newVar s old new | M.member s old = newVar' (fromJust $ M.lookup s old) new
                 | otherwise = s
newVar' :: String -> M.Map String String -> String
newVar' old new | M.member old new = fromJust $ M.lookup old new
                | otherwise = old

--The correctNameBody is used in the body of the sim assignments.
correctNameBody :: Expr -> M.Map String String -> Expr
correctNameBody (Repby x (Name n)) vars | M.member n vars = Repby x (Name $ fromJust $ M.lookup n vars)
                                        | otherwise = Repby x $ Name n
correctNameBody (Name s) vars | M.member s vars = Name $ (fromJust $ M.lookup s vars)
                              | otherwise = error "Something is wrong!"
correctNameBody (Lit s) _ = Lit s
correctNameBody s _ = s

modifyName :: (String -> String) -> Var -> Var
modifyName f (Int s)   = Int (f s)
modifyName f (Array s) = Array (f s)
modifyName f (Univ s)  = Univ (f s)

nameOf :: Var -> String
nameOf (Int s) = s
nameOf (Array s) = s
nameOf (Univ s) = s
nameOf (Exis s) = s

toPrenexNF :: Expr -> Expr
toPrenexNF (And (ForAll s e1) e2) =
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in ForAll s $ toPrenexNF (And pnf1 pnf2)
toPrenexNF (And (Exists s e1) e2) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in Exists s $ toPrenexNF (And e1 e2)
toPrenexNF (Or (ForAll s e1) e2) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in ForAll s $ toPrenexNF (Or e1 e2)
toPrenexNF (Or (Exists s e1) e2) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in Exists s $ toPrenexNF (Or e1 e2)
toPrenexNF (And e1 (ForAll s e2)) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in ForAll s $ toPrenexNF (And pnf1 pnf2)
toPrenexNF (And e1 (Exists s e2)) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in Exists s $ toPrenexNF (And pnf1 pnf2)
toPrenexNF (Or e1 (ForAll s e2)) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in ForAll s $ toPrenexNF (Or pnf1 pnf2)
toPrenexNF (Or e1 (Exists s e2)) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in Exists s $ toPrenexNF (Or pnf1 pnf2)
toPrenexNF (Not (Exists s e1)) = 
  let pnf1 = toPrenexNF e1
  in ForAll s $ toPrenexNF (Not pnf1)
toPrenexNF (Not (ForAll s e1)) = 
  let pnf1 = toPrenexNF e1
  in Exists s $ toPrenexNF (Not pnf1)
toPrenexNF (Impl (ForAll s e1) e2) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in Exists s $ toPrenexNF (Impl pnf1 pnf2)
toPrenexNF (Impl (Exists s e1) e2) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in ForAll s $ toPrenexNF (Impl pnf1 pnf2)
toPrenexNF (Impl e1 (ForAll s e2)) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in ForAll s $ toPrenexNF (Impl pnf1 pnf2)
toPrenexNF (Impl e1 (Exists s e2)) = 
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in Exists s $ toPrenexNF (Impl pnf1 pnf2)
toPrenexNF (And e1 e2) =
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in case pnf1 == e1 && pnf2 == e2 of
      True -> And e1 e2
      False -> toPrenexNF (And pnf1 pnf2)
toPrenexNF (Or e1 e2) =
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in case pnf1 == e1 && pnf2 == e2 of
      True -> Or e1 e2
      False -> toPrenexNF (Or pnf1 pnf2)
toPrenexNF (Impl e1 e2) =
  let pnf1 = toPrenexNF e1
      pnf2 = toPrenexNF e2
  in case pnf1 == e1 && pnf2 == e2 of
      True -> Impl e1 e2
      False -> toPrenexNF (Impl pnf1 pnf2)
toPrenexNF (Not e1) =
  let pnf1 = toPrenexNF e1
  in case pnf1 == e1 of
      True -> Not e1
      False -> toPrenexNF (Not pnf1)
toPrenexNF (Exists s e) = Exists s (toPrenexNF e)
toPrenexNF (ForAll s e) = ForAll s (toPrenexNF e)
toPrenexNF e = e

freeVars :: S.Set Var -> Expr -> [String]
freeVars vars expr = 
  let allRefs = S.toList $ collectRefs expr
      quantifiers = map nameOf $ filter (\x -> case x of
                                    Univ s -> True
                                    Exis s -> True
                                    _ -> False) (S.toList vars)
  in  allRefs L.\\ quantifiers

toExistential :: S.Set Var -> Expr -> Expr -> Expr
toExistential vars e1 e2 = 
  let fv1 = freeVars vars e1
      fv2 = freeVars vars e2
      uv = fv1 L.\\ fv2
  in convertToExist uv e2

convertToExist :: [String] -> Expr -> Expr
convertToExist xs (Plus e1 e2) = undefined