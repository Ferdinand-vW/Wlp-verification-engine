module Examples where

import SyntaxTransformer
import GCL
import Transformer

s1 :: Stmt
s1 = var ["x","y","n"] 
            [ assume (i 0 .<  ref "x") ,
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
swap = prog "swap" ["i", "j", "a"] ["a"]
        [
          var ["tmp", "c", "b"]
            [
              --assume ((ref "a" `repby` ref "i" .== ref "b") .&& (ref "a" `repby` ref "j" .== ref "c")), 
              ref "tmp" .=  ref "a" `repby` ref "i",
              ref "a" `repby` ref "i" .=  ref "a" `repby` ref "j",
              ref "a" `repby` ref "j" .=  ref "tmp"
              --assert ((ref "a" `repby` ref "i" .== ref "c") .&& (ref "a" `repby` ref "j" .== ref "b"))
            ]
          --ref "a'" .= ref "a"
        ]

swapTest = var ["a", "i", "j", "z"]
            [
              assume (ref "a" `repby` ref "i" .== ref "z" `repby` ref "i"),
              swap,
              pcall "swap" [ref "i", ref "j", ref "a"] [ref "z"],
              assert (ref "a" `repby` ref "i" .== ref "z" `repby` ref "j")
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
                ref "b" .= i 6,
                (sim [ref "a",ref "b"] [ref "b" `minus` i 1,i 5]),
                (sim [ref "a",ref "b"] [ref "a" `minus` i 1,i 4]),
                assert ((ref "a" .== ref "b"))
            ]

forallExample :: Stmt
forallExample = var ["j","i","N","min","a"]
                  [
                    assume (forall "x" (ref "j" .< ref "x" .&& ref "x" .< ref "i" .&& ref "i" .< ref "N" .==>
                      (ref "min" .< ref "a"))),
                    assert (forall "y" (ref "j" .< ref "y" .&& ref "y" .< ref "i" .&& ref "i" .< ref "N" .==>
                      (ref "min" .< ref "a")))
                  ]

simArray :: Stmt
simArray = var ["a","b"] 
            [assume ((ref "a" .== ref "b")),
                ref "b" .= i 6,
                (sim [ref "a" `repby` i 0,ref "b"] [ref "b" `minus` i 1,i 5]),
                (sim [ref "a" `repby` i 0,ref "b"] [ref "a" `repby` i 0 `minus` i 1,i 4]),
             
                   assert ((ref "a" `repby` i 0 .== ref "b"))            ]

                  
minind :: Stmt
minind = prog "minind" ["a","i","N"] ["r"]
            [
              assume (ref "i" .< ref "N"),
              var ["min", "j"]
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
sort = var ["a","a'","i","N"]
        [
          assume (ref "i" .< ref "N"),
          minind,
          swap,
          ref "i" .= i 0,
           inv (forall "x" (forall "y" ((i 0 .<= ref "x" .&& ref "x" .<= ref "y" .&& ref "y" .<= ref "i") 
            .==> ref "a" `repby` ref "x" .<= ref "a" `repby` ref "y")))
            (while (ref "i" .< ref "N" `minus` i 1)
                          [
                            var ["m"]
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