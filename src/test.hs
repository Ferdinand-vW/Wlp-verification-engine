import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Prover
import GCL

main = do
  testProve
  testProveImpl

testProve = proveImpl (ref "y" .== ref "m") (ref "y" .== ref "m" `plus` i 5) >>= print
testProveImpl = proveImpl (forall "x" (ref "x" .== i 5)) (forall "x" (ref "x" .< i 5 .|| i 5 .< ref "x")) >>= print

exampleCode = var ["x","y"]
  [ assume (i 0 .< ref "x"),
    inv (i 0 .< ref "x")
      (while (i 0 .< ref "x") [ ref "x" .= (ref "x" `minus` i 1) ]),
    ref "y" .= ref "x",
    assert (ref "y" .== i 0)
  ]