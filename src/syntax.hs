data Program = ProgramName String deriving Show

data Statement = Skip 
            | Assert Expression
            | Assume Expression
            | AsgTargets Expressions
            | Seq Statement Statement
            | If Statement Statement
            | Inv Expression Expression Statement
            | Var Variables Statement
            deriving Show


data Variable = Variable Name Type deriving Show
data BoundVariable = BoundVariable Name Type deriving Show

data Expression = Literal
            | N Name
            | Expression `BinaryOp` Expression
            | Not Expression
            | Name Expression
            | Forall BoundVariable Expression
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

type AsgTargets = [Name]
type Parameters = [Variable]
type Expressions = [Expression]
type Variables = [Variable]
type ArrayType = [PrimitiveType]
--Some code to make it nicer to read.
type Name = String