module Examples where

import SyntaxTransformer
import GCL

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

s3 = var ["x","y"]
      [
        assume (ref "x" .< i 0),
        prog "inc2" ["a"] ["y"]
          [
            ref "y" .= ref "a" `plus` i 1
          ],
        prog "inc" ["b"] ["z"]
          [
            pcall "inc2" [ref "b"] [ref "z"]
          ],
        pcall "inc" [ref "x"] [ref "x"], 
        assert (ref "x" .<= i 0) 
      ]


{-s3 :: Stmt
s3 = var ["a", "i", "j","k"] [
             assume (i 0 .<=  ref "i") , 
            (ref "a" `repby` ref "i").=  i 10,
            (ref "a" `repby` ref "j") .=  i 11,
            assert ((ref "a" `repby` ref "j") .== i 12)
          ]-}

swap :: Stmt
swap = var ["a", "i", "j", "tmp", "c", "b"] 
            [assume ((ref "a" `repby` ref "i" .== ref "b") .&& (ref "a" `repby` ref "j" .== ref "c")), 
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
                assert ((ref "a" `repby` i 0 .== ref "c") .&& (ref "a" `repby` i 1 .== ref "b"))
            ]

simCode :: Stmt
simCode = var ["a","b"] 
            [assume ((ref "a" .== ref "b")), 
                (sim [ref "a",ref "b"] [(ref "a" `minus` i 1),(ref "b" `minus` i 1)]),
                assert ((ref "a" .== ref "b"))
            ]