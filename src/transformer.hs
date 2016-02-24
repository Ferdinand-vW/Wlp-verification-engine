module Transformer where
import GCL

s1 :: Stmt
s1 = var ["x" , "y"] 
            [ assume (i 0 .<  ref "x") ,
                inv (i 0 .<= ref "x" )
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 1) ]),
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]

wlp :: Stmt -> Expr -> Expr
wlp Skip q = q
wlp (e1 `Assign` e2) q = assignQ q (name e1,name e2)
wlp (Post e)      q    = e .&& q
wlp (Pre e)       q    = e .==> q
wlp (If g s1 s2) q    = (g .&& wlp s1 q) .&& ((.!) g .&& wlp s2 q)
wlp (Inv i (While g s)) q  = (i .&& (.!) g) .==> q .|| (i .&& g) .==> i
wlp (While e1 b)  q    = error "We do not allow a While without an invariant.."
wlp (Program n s) q    = undefined --What do I need to this at this case?
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

name :: Expr -> String
name (Name s) = s
name _ = error "Not yet allowed"