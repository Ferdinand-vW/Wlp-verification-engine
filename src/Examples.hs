module Examples where

import SyntaxTransformer
import GCL
import Transformer

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

swap :: Stmt
swap = prog "swap" [int "i", int "j", array "a"] [array "a'"]
        [
          var [int "tmp"]
            [
              --assume ((ref "a" `repby` ref "i" .== ref "b") .&& (ref "a" `repby` ref "j" .== ref "c")), 
              ref "tmp" .=  ref "a" `repby` ref "i",
              ref "a" `repby` ref "i" .=  ref "a" `repby` ref "j",
              ref "a" `repby` ref "j" .=  ref "tmp"
              --assert ((ref "a" `repby` ref "i" .== ref "c") .&& (ref "a" `repby` ref "j" .== ref "b"))
            ],
          ref "a'" .= ref "a"
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

swapTest = var [array "a", array "b", int "i", int "j", int "c", int "d"]
            [
              assume (ref "a" `repby` ref "i" .== ref "c" .&& ref "a" `repby` ref "j" .== ref "d"),
              swap,
              pcall "swap" [ref "i", ref "j", ref "a"] [ref "a"],
              assert (ref "a" `repby` ref "i" .== ref "d" .&& ref "a" `repby` ref "j" .== ref "c")
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
simCode = var [array "a", array "b"] 
            [assume ((ref "a" .== ref "b")),
                ref "b" .= i 6,
                (sim [ref "a",ref "b"] [ref "b" `minus` i 1,i 5]),
                (sim [ref "a",ref "b"] [ref "a" `minus` i 1,i 4]),
                assert ((ref "a" .== ref "b"))
            ]

forallExample :: Stmt
forallExample = var [int "j",int "i",int "N",int "min",array "a"]
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

                  
minind :: Stmt
minind = prog "minind" [array "a",int "i",int "N"] [int "r"]
            [
              assume (ref "i" .< ref "N"),
              var [int "min",int "j"]
                [
                
                ref "j" .= ref "i",
                ref "r" .= ref "i",
                ref "min" .= ref "a" `repby` ref "i",
                inv (forall "x" (ref "j" .< ref "N" .&& ref "j" .<= ref "i" .&& ref "j" .<= ref "r" .&&


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
                    ])
                ],
              assert (forall "x" (ref "j" .<= ref "x" .&& ref "x" .< ref "N" .==> 
                                   (ref "a" `repby` ref "r" .<= ref "a" `repby` ref "x")))
            ]

sort :: Stmt
sort = var [array "a",array "a'",int "i",int "N"]
        [
          assume (ref "i" .< ref "N"),
          minind,
          swap,
          ref "i" .= i 0,
           inv (forall "x" (forall "y" ((i 0 .<= ref "x" .&& ref "x" .<= ref "y" .&& ref "y" .<= ref "i") 
            .==> ref "a" `repby` ref "x" .<= ref "a" `repby` ref "y")))
            (while (ref "i" .< ref "N" `minus` i 1)
                          [
                            var [int "m"]
                              [
                                pcall "minind" [ref "a", ref "i" `plus` i 1, ref "N"] [ref "m"],
                                if_then_else (ref "a" `repby` ref "m" .< ref "a" `repby` ref "i")
                                  [
                                    pcall "swap" [ref "a", ref "i", ref "m"] [ref "a"]
                                  ]
                                  [
                                    Skip
                                  ],
                                ref "i" .= ref "i" `plus` i 1
                              ]
                            
                          ])
          --ref "a'" .= ref "a"
        ]