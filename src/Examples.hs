module Examples where

import Collect
import GCL
import Transformer

loopExample :: Stmt
loopExample = var [int "y"]
         [
            assume (i 0 .<= ref "y"),
            while (i 0 .< ref "y") [ref "y" .= ref "y" `minus` i 1],
            assert (ref "y" .== i 0)
         ]

plusExample :: Stmt
plusExample = 
 var [int "a", int "b"]
    [
      assume (i 1 .== ref "b"),
      ref "a" .= ref "b" `minus` i 1,
      assert (ref "a" .== i 0)
    ]

loop1 :: Stmt
loop1 = var [int "i", int "N", array "a", int "s"] [
            assume ((i 0 .== ref "i") .&& (ref "s" .== i 0) .&& (i 0 .<= ref "N")),
            while (ref "i" .< ref "N") [
                assert ((i 0 .<= ref "i") .&& (ref "i" .< ref "N")),
                ref "s" .= ref "s" `plus` ref "a" `repby` ref "i",
                ref "i" .= ref "i" `plus` i 1
            ],
            assert True_
        ]

loop2 :: Stmt
loop2 = var [int "i", int "N", array "a", int "s", int "k"] [
            assume (i 0 .== ref "i" .&& i 0 .<= ref "N" .&& i 0 .<= ref "k" .&& ref "k" .< ref "N"),
            while (ref "i" .< ref "N") [
                ref "i" .= ref "i" `plus` i 1
            ],
            assert (i 0 .<= ref "k" .&& ref "k" .< ref "N"),
            ref "s" .= ref "a" `repby` ref "k",
            assert True_
        ]


s1 :: Stmt
s1 = var [int "x",int "y",int "n"] 
            [ assume (i 0 .<  ref "x") ,
                inv (i 0 .<= ref "x")
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 1) ]),
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]

s2 :: Stmt
s2 = var [int "x" , int "y"] 
            [ assume (i 2 .<=  ref "x") ,
                inv (i 0 .<= ref "x")
                    (while (i 0 .< ref "x")  [(ref "x") .= (ref "x" `minus` i 2) ]),
                var [int "x",int "z"]
                  [
                    ref "x" .= ref "y",
                    var [int "x",int "z"]
                      [
                        ref "z" .= forall "x" (forall "x" (ref "x" .< ref "y"))
                      ]
                  ],
                ref "y"  .= ref "x",
                assert (ref "y" .== i 0)
            ]

s3 = var [int "x",int "y"]
      [
        assume (ref "x" .< i 0),
        prog "inc2" [int "a"] [int "y"]
          [
            ref "y" .= ref "a" `plus` i 1
          ],
        prog "inc" [int "b"] [int "z"]
          [
            pcall "inc2" [ref "b"] [ref "z"]
          ],
        pcall "inc" [ref "x"] [ref "x"], 
        assert (ref "x" .<= i 0) 
      ]

forallExample :: Stmt
forallExample = var [int "j",int "i",int "N",int "min",int "a"]
                  [
                    assume (forall "x" (ref "j" .< ref "x" .&& ref "x" .< ref "i" .&& ref "i" .< ref "N" .==>
                      (ref "min" .< ref "a"))),
                    assert (forall "y" (ref "j" .< ref "y" .&& ref "y" .< ref "i" .&& ref "i" .< ref "N" .==>
                      (ref "min" .< ref "a")))
                  ]

simArray :: Stmt
simArray = var [array "a",array "b"] 
            [
              assume ((ref "a" .== ref "b")),
              ref "b" .= i 6,
              (sim [ref "a" `repby` i 0,ref "b"] [ref "b" `minus` i 1,i 5]),
              (sim [ref "a" `repby` i 0,ref "b"] [ref "a" `repby` i 0 `minus` i 1,i 4]),
              assert ((ref "a" `repby` i 0 .== ref "b"))            
            ]   

simCode :: Stmt
simCode = var [int "a", int "b"] 
            [assume ((ref "a" .== ref "b")),
                ref "b" .= i 6,
                (sim [ref "a",ref "b"] [ref "b" `minus` i 1,i 5]),
                (sim [ref "a",ref "b"] [ref "a" `minus` i 1,i 4]),
                assert ((ref "a" .== ref "b"))
            ]     

swap' :: Stmt
swap' = var [int "i", int "j", int "tmp", int "c", int "b"] 
            [
              assume ((ref "i" .== ref "b") .&& (ref "j" .== ref "c")), 
              ref "tmp" .= ref "i",
              ref "i" .=  ref "j",
              ref "j" .=  ref "tmp",
              assert ((ref "i" .== ref "c") .&& (ref "j" .== ref "b"))
            ]

swap :: Stmt
swap = prog "swap" [int "i", int "j", array "a"] [array "a'"]
        [
          var [int "tmp", int "b", int "c"]
            [
              assume ((ref "a" `repby` ref "i" .== ref "b") .&& (ref "a" `repby` ref "j" .== ref "c")),
              ref "tmp" .=  ref "a" `repby` ref "i",
              ref "a" `repby` ref "i" .=  ref "a" `repby` ref "j",
              ref "a" `repby` ref "j" .=  ref "tmp",
              assert ((ref "a" `repby` ref "i" .== ref "c") .&& (ref "a" `repby` ref "j" .== ref "b"))
            ],
          ref "a'" .= ref "a"
        ]

callSwap :: Stmt
callSwap = var [array "a", int "i", int "j", int "c", int "d"]
            [
              assume ((ref "a" `repby` ref "i" .== ref "c") .&& (ref "a" `repby` ref "j" .== ref "d")),
              swap,
              pcall "swap" [ref "i", ref "j", ref "a"] [ref "a"],
              assert ((ref "a" `repby` ref "i" .== ref "d") .&& (ref "a" `repby` ref "j" .== ref "c"))
            ]

minind' :: Stmt
minind' = var [array "a",int "i",int "N",int "r",int "min",int "j"]
            [
                assume (ref "i" .< ref "N"),  
                ref "j" .= ref "i",
                ref "r" .= ref "i",
                ref "min" .= ref "a" `repby` ref "i",
                inv (forall "x" (ref "j" .< ref "N" .&& ref "j" .<= ref "i" .&& ref "j" .<= ref "r" .&& ref "i" .<= ref "N" .&&


                                (ref "j" .== ref "i" .==> ref "r" .== ref "i") .&&
                                (ref "j" .< ref "i" .==> ref "r" .< ref "i") .&&
                                ref "min" .== ref "a" `repby` ref "r" .&&
                                (ref "j" .<= ref "x" .&& ref "x" .< ref "i" .==> ref "a" `repby` ref "r" .<= ref "a" `repby` ref "x")))
                  (while (ref "i" .< ref "N")
                    [
                      if_then_else (ref "a" `repby` ref "i" .< ref "min")
                        [
                          ref "min" .= ref "a" `repby` ref "i",
                          ref "r" .= ref "i"
                        ]
                        [
                          Skip
                        ],
                      ref "i" .= ref "i" `plus` i 1
                    ]),
                  assert (forall "y" (ref "j" .<= ref "y" .&& ref "y" .< ref "N" .==> 
                                   (ref "a" `repby` ref "r" .<= ref "a" `repby` ref "y")))
            ]

minind :: Stmt
minind = prog "minind" [int "i", int "N", array "a"] [int "i'", int "N'", int "r'", array "a'"]
         [
              var [int "min", int "r", int "j"]
                [
                assume (ref "i" .< ref "N"),
                ref "j" .= ref "i",
                ref "r" .= ref "i",
                ref "min" .= ref "a" `repby` ref "i",
                 inv ( ref "j" .< ref "N" .&& ref "j" .<= ref "i" .&& ref "j" .<= ref "r" .&& ref "i" .<= ref "N" .&&
                      (ref "j" .== ref "i" .==> ref "r" .== ref "i")                      .&&
                      (ref "j" .< ref "i" .==> ref "r" .< ref "i")                        .&&
                      ref "min" .== ref "a" `repby` ref "r"                               .&&
                      forall "x" (ref "j" .<= ref "x" .&& ref "x" .< ref "i" .==> ref "a" `repby` ref "r" .<= ref "a" `repby` ref "x"))
                  (while (ref "i" .< ref "N")
                    [
                      if_then_else (ref "a" `repby` ref "i" .< ref "min")
                        [
                          ref "min" .= ref "a" `repby` ref "i",
                          ref "r" .= ref "i"
                        ]
                        [
                          Skip
                        ],
                      ref "i" .= ref "i" `plus` i 1
                    ]),
                assert (forall "y" (ref "j" .<= ref "y" .&& ref "y" .< ref "N" .==> 
                                   (ref "a" `repby` ref "r" .<= ref "a" `repby` ref "y"))),
                ref "r'" .= ref "r",
                ref "i'" .= ref "i",
                ref "N'" .= ref "N",
                ref "a'" .= ref "a"
                ]
  ]

--We can only verify this call if we return all variables
callMinind :: Stmt
callMinind = var [int "i", int "N", array "a", int "r"] [
        assume (ref "i" .< ref "N"),
        minind,
        pcall "minind" [ref "i", ref "N", ref "a"] [ref "i", ref "N", ref "r", ref "a"],
        assert (forall "z" (ref "i" .<= ref "z" .&& ref "z" .< ref "N" .==> 
                                   (ref "a" `repby` ref "r" .<= ref "a" `repby` ref "z")))
    ]


--Unfortunately, we were not able to verify sort afterall.
--The problem is when trying to proof the invariants of minind
--We cannot prove that I && not g ==> Q, where Q consists of the
--calculated wlp of sort before the call to minind and the post condition
--of within minind. The wlp before the call to minind contains variables
--that are not used within I && not g. This means that we can never prove that the
--invariant holds. We were thinking of making such variables existential quantifiers,
--but that would allow also allow invariants/postconditions that are simply incorrect.
--We are also certain that sort's invariant/pre/post condition are correct. If we always
--return the invariant, even if the invariant is incorrect, then we can prove sort.
sort :: Stmt
sort = var [int "m", int "i", int "N", array "a"]
        [
          assume (i 0 .<= ref "N"),
          minind,
          swap,
          ref "i" .= i 0,
         inv (  (ref "N" .== i 0 .|| ref "i" .<= ref "N" `minus` i 1) .&&
                  (forall "k" (i 0 .<= ref "k" .&& ref "k" .< ref "i" `minus` i 1 
                      .==> 
                  ref "a" `repby` ref "k" .<= ref "a" `repby` (ref "k" `plus` i 1))) .&&
                  (forall "k" (i 0 .<= ref "k" .&& ref "k" .< ref "i"
                     .==> 
                  forall "l" (ref "i" .<= ref "l" .&& ref "l" .< ref "N" .==> ref "a" `repby` ref "k" .<= ref "a" `repby` ref "l"))))
          (while (ref "i" .< ref "N" `minus` i 1)
                        [
                          pcall "minind" [ref "i" `plus` i 1, ref "N", ref "a"] [ref "i", ref "N", ref "m", ref "a"],
                          if_then_else (ref "a" `repby` ref "m" .< ref "a" `repby` ref "i")
                            [
                              pcall "swap" [ref "i", ref "m", ref "a"] [ref "a"]
                            ]
                            [
                              Skip
                            ],
                          ref "i" .= ref "i" `plus` i 1
                        ]),
          assert (forall "k" (i 0 .<= ref "k" .&& ref "k" .< (ref "N" `minus` i 1) 
                      .==> 
                      ref "a" `repby` ref "k" .<= ref "a" `repby` (ref "k" `plus` i 1)))
        ]