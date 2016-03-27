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

--sample :: Expr
sample = i 3 `minus` i 1 `minus` i 3

plusExample :: Stmt
plusExample = 
 var [int "a", int "b"]
    [
      assume (i 1 .== ref "b"),
      ref "a" .= ref "b" `minus` i 1,
      assert (ref "a" .== i 0)
    ]


example :: Stmt
example = var [array "a", int "i", int "j", int "k"]
  [
    assume ((ref "i" .== ref "j" .==> i 0 .== ref "a" `repby` ref "k") .&&
            (neg (ref "i" .== ref "j") .==> ref "a" `repby` ref "j" .== ref "a" `repby` ref "k") .&&
            (ref "i" .== ref "k" .==> ref "a" `repby` ref "j" .== i 0) .&&
            (neg (ref "i" .== ref "k") .==> ref "a" `repby` ref "j" .== ref "a" `repby` ref "k")),
    assert ((ref "i" .== ref "j" .&& ref "i" .== ref "k" .==> i 0 .== i 0) .&&
            (ref "i" .== ref "j" .&& neg (ref "i" .== ref "k") .==> i 0 .== ref "a" `repby` ref "k") .&&
            (neg (ref "i" .== ref "j") .&& ref "i" .== ref "k" .==> ref "a" `repby` ref "j" .== i 0) .&&
            (neg (ref "i" .== ref "j") .&& neg (ref "i" .== ref "k") .==> ref "a" `repby` ref "j" .== ref "a" `repby` ref "k"))
  ]

example2 :: Stmt
example2 = var [array "a", int "i", int "j", int "k"]
  [
    assume ((ref "i" .== ref "j" .&& ref "i" .== ref "k" .==> i 0 .== i 0) .&&
            (ref "i" .== ref "j" .&& neg (ref "i" .== ref "k") .==> i 0 .== ref "a" `repby` ref "k") .&&
            (neg (ref "i" .== ref "j") .&& ref "i" .== ref "k" .==> ref "a" `repby` ref "j" .== i 0) .&&
            (neg (ref "i" .== ref "j") .&& neg (ref "i" .== ref "k") .==> ref "a" `repby` ref "j" .== ref "a" `repby` ref "k")),
    assert ((ref "i" .== ref "j" .==> i 0 .== ref "a" `repby` ref "k") .&&
            (neg (ref "i" .== ref "j") .==> ref "a" `repby` ref "j" .== ref "a" `repby` ref "k") .&&
            (ref "i" .== ref "k" .==> ref "a" `repby` ref "j" .== i 0) .&&
            (neg (ref "i" .== ref "k") .==> ref "a" `repby` ref "j" .== ref "a" `repby` ref "k"))
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


{-s3 :: Stmt
s3 = var ["a", "i", "j","k"] [
             assume (i 0 .<=  ref "i") , 
            (ref "a" `repby` ref "i").=  i 10,
            (ref "a" `repby` ref "j") .=  i 11,
            assert ((ref "a" `repby` ref "j") .== i 12)
          ]-}

test :: Stmt
test = var [array "a", array "b"]
        [
          assume (neg $ ref "a" .== ref "b"),
          ref "a" .= ref "b",
          assert (ref "a" .== ref "b")
        ]

swapTest' = var [array "a", array "e", int "tmp", int "i", int "j", int "c", int "b"]
            [
              assume ((ref "a" `repby` ref "i" .== ref "b") .&& (ref "a" `repby` ref "j" .== ref "c")),
              ref "tmp" .=  ref "a" `repby` ref "i",
              ref "a" `repby` ref "i" .=  ref "a" `repby` ref "j",
              ref "a" `repby` ref "j" .=  ref "tmp",
              ref "e" .= ref "a",
              assert ((ref "e" `repby` ref "i" .== ref "c") .&& (ref "e" `repby` ref "j" .== ref "b"))
            ]



swap' :: Stmt
swap' = var [int "i", int "j", int "tmp", int "c", int "b"] 
            [assume ((ref "i" .== ref "b") .&& (ref "j" .== ref "c")), 
            ref "tmp" .= ref "i",
                ref "i" .=  ref "j",
                ref "j" .=  ref "tmp",
                assert ((ref "i" .== ref "c") .&& (ref "j" .== ref "b"))
            ]


swap'' :: Stmt
swap'' = var [array "a", int "i", int "j", int "tmp", int "c", int "b"] 
            [assume ((ref "a" `repby` i 0 .== ref "b") .&& (ref "a" `repby` i 1 .== ref "c")), 
            ref "tmp" .=  ref "a" `repby` i 0,
                ref "a" `repby` i 0 .=  ref "a" `repby` i 1,
                ref "a" `repby` i 1 .=  ref "tmp",
                assert ((ref "a" `repby` i 0 .== ref "c") .&& (ref "a" `repby` i 1 .== ref "b"))
            ]

simCode :: Stmt
simCode = var [int "a", int "b"] 
            [assume ((ref "a" .== ref "b")),
                ref "b" .= i 6,
                (sim [ref "a",ref "b"] [ref "b" `minus` i 1,i 5]),
                (sim [ref "a",ref "b"] [ref "a" `minus` i 1,i 4]),
                assert ((ref "a" .== ref "b"))
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
            [assume ((ref "a" .== ref "b")),
                ref "b" .= i 6,
                (sim [ref "a" `repby` i 0,ref "b"] [ref "b" `minus` i 1,i 5]),
                (sim [ref "a" `repby` i 0,ref "b"] [ref "a" `repby` i 0 `minus` i 1,i 4]),
             
                   assert ((ref "a" `repby` i 0 .== ref "b"))            ]

                  
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

swapCall :: Stmt
swapCall = var [array "a", int "i", int "j", int "c", int "d"]
            [
              assume ((ref "a" `repby` ref "i" .== ref "c") .&& (ref "a" `repby` ref "j" .== ref "d")),
              swap,
              pcall "swap" [ref "i", ref "j", ref "a"] [ref "a"],
              assert ((ref "a" `repby` ref "i" .== ref "d") .&& (ref "a" `repby` ref "j" .== ref "c"))
            ]

--pcall "swap" [ref "i", ref "j", ref "a"] [ref "i", ref "j", ref "a"],
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

callMinind :: Stmt
callMinind = var [int "i", int "N", array "a", int "r"] [
        assume (ref "i" .< ref "N"),
        minind,
        pcall "minind" [ref "i", ref "N", ref "a"] [ref "r"],
        assert (forall "z" (ref "i" .<= ref "z" .&& ref "z" .< ref "N" .==> 
                                   (ref "a" `repby` ref "r" .<= ref "a" `repby` ref "z")))
    ] 

test4 :: Stmt
test4 = var [array "a", array "b", int "i", int "j", int "k", int "l", int "r"]
  [
    assume (forall "y" (ref "k" .<= ref "y" .&& ref "y" .< ref "l" .==> 
                       (ref "a" `repby` ref "r" .<= ref "a" `repby` ref "y"))),
    assert (forall "z" (ref "i" .<= ref "z" .&& ref "z" .< ref "l" .==> 
                       (ref "a" `repby` ref "r" .<= ref "a" `repby` ref "z")))
  ]

example3 = var [int "i", int "j"]
  [
    assume (ref "i" .== ref "j"),
    inc,
    pcall "inc" [ref "j", ref "i"] [ref "j"],
    assert (ref "i" `plus` ref "i" .== ref "j")
  ]

inc = prog "inc" [int "a", int "b"] [int "a'"]
  [
    ref "a" .= ref "a" `plus` ref "b",
    ref "a'" .= ref "a"
  ]

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