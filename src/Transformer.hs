module Transformer (
transform
)where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import qualified Data.SBV as SBV

import Data.SBV(modelExists)
import SyntaxTransformer
import Control.Monad

transform :: Stmt -> Stmt
transform stmt =
  let (ProgramInfo vars progMap) = stmt_ProgramInfo stmt
      varsL = S.toList vars
      varMap = M.fromList $ zip varsL varsL
  in snd $ mkFreshStmt 0 varMap progMap stmt


mkFreshStmts :: Int -> M.Map String String -> M.Map String ([String],[String],[Stmt]) -> [Stmt] -> (Int,[Stmt])
mkFreshStmts n varMap progMap stmts =
  foldr (\x (y,stmts') -> let (n',stmt) = mkFreshStmt y varMap progMap x
                          in (n',stmt : stmts')) (n,[]) stmts

mkFreshStmt :: Int -> M.Map String String -> M.Map String ([String],[String],[Stmt])-> Stmt -> (Int,Stmt)
mkFreshStmt n varMap progMap (Var xs stmts) =
  let dupVars = filter (\x -> M.member x varMap) xs
      freshVars = map (++ show n) dupVars
      remVars = xs L.\\ dupVars
      newVarMap = M.union (M.fromList (zip dupVars freshVars)) varMap
      (n1,newStmts) = mkFreshStmts (n + 1) newVarMap progMap stmts
  in (n1, Var (L.union remVars freshVars) newStmts)
mkFreshStmt n varMap progMap (PCall name args vars) =
  let (n1,expr1) = mkFreshExprs n varMap args
      (n2,expr2) = mkFreshExprs n1 varMap vars
      (prms,resv,body) = fromJust $ M.lookup name progMap
      dupPrms = filter (\x -> M.member x varMap) prms   
      dupResv = filter (\x -> M.member x varMap) resv
      freshPrms = map (++ show n) dupPrms
      freshResv = map (++ show n) dupResv
      remPrms = prms L.\\ dupPrms
      remResv = resv L.\\ dupResv
      newVarMap = M.union (M.fromList (zip (dupPrms ++ dupResv) (freshPrms ++ freshResv))) varMap
      (n3,body1) = mkFreshStmts (n + 1) newVarMap progMap body
      allPrms = L.union remPrms freshPrms
      allResv = L.union remResv freshResv
      transformed = Var (L.union allPrms allResv)
          (
            map (\(x,y) -> Assign (Name x) y) (zip allPrms expr1)
            ++
            body1
            ++
            map (\(x,y) -> Assign x (Name y)) (zip expr2 allResv)
          )
  in (n3,transformed)
mkFreshStmt n varMap progMap (Sim vars1 vars2) =
  let (n1,expr1) = mkFreshExprs n varMap vars1
      (n2,expr2) = mkFreshExprs n1 varMap vars2
      varsList = foldr (\x y -> name x : y) [] vars1
      varTypes = foldr (\x y -> varType x : y) [] vars1
      dupVars = filter (\x -> M.member x varMap) varsList
      oldVars = [fromJust $ M.lookup x varMap | x <- dupVars]
      newVars = map (++ show n) dupVars
      newVarsLeft = M.fromList $ map (\(x,y) -> (x ++ show n,y)) varTypes
      newVarTypesRight = [(fromJust $ M.lookup x varMap,y) | (x,y) <- varTypes , x `M.member` varMap]
      freshMap = M.fromList $ zip oldVars newVars
      modRight = foldr (\x y -> (modifyExpr x varMap freshMap) : y) [] vars2
      headStmp = foldr (\(x,y) z -> (Assign (correctType x (fromJust $ M.lookup x newVarsLeft)) 
                                            (correctType y $ fromJust $ M.lookup y (M.fromList newVarTypesRight))) : z) [] $ zip newVars oldVars
      bodyStmp = foldr (\(x,y) z -> (Assign x y): z ) [] $ zip expr1 modRight
      transformed = Var [] $ headStmp ++ bodyStmp    
  in (n2,transformed)
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
                    then M.adjust (\_ -> s) s varMap
                    else varMap
      (n1,expr1) = mkFreshExpr (n + 1) newVarMap expr
  in case M.lookup s newVarMap of
        Nothing -> (n1, ForAll s expr1)
        Just s' -> (n1, ForAll s expr1)
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

varType :: Expr -> (String,Maybe Expr)
varType (Name s) = (s, Nothing)
varType (Repby (Name s) index) = (s, Just index)

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
modifyExpr (Repby (Name s) index) old new = Repby (Name $ newVar s old new) index

newVar :: String -> M.Map String String -> M.Map String String -> String
newVar s old new | M.member s old = fromJust $ M.lookup (fromJust $ M.lookup s old) new
                 | otherwise = s

correctType :: String -> Maybe Expr -> Expr
correctType s Nothing = Name s
correctType s (Just i) = Repby (Name s) i