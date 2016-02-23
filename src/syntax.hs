data Program = ProgramName String deriving Show

data Stmt = Skip 
            | Assert Expr
            | Assume Expr
            | AsgTargets Exprs
            | Seq Stmt Stmt
            | If Stmt Stmt
            | Inv Expr Expr Stmt
            | Var Variables Stmt
            deriving Show


data Variable = Variable Name Type deriving Show
data BoundVariable = BoundVariable Name Type deriving Show

data Expr = Literal
            | N Name
            | Expr `BinaryOp` Expr
            | Not Expr
            | Name Expr
            | Forall BoundVariable Expr
            deriving Show

data BinaryOp = Plus 
        | Min 
        | Conjunction 
        | Disjunction 
        | Implication
        | Lt
        | Lte
        | Eq
        deriving Show

data Type = T PrimitiveType | A ArrayType deriving Show
data PrimitiveType = B Bool | I Int deriving Show

--Types to make it nicer to read.
type AsgTargets = [Name]
type Parameters = [Variable]
type Exprs = [Expr]
type Variables = [Variable]
type ArrayType = [PrimitiveType]
type Name = String