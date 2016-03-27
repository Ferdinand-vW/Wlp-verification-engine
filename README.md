# Wlp-verification-engine
Ferdinand van Walree 3874389 and Matthew Swart 5597250

This project uses the following compiler verions:
- Haskell version: 7.10.2-a
- uuagc-0.9.52.1

In order to verify an example you need to run "build.sh" and type: verify <function from examples.hs>.
E.g: "verify minind"

The build.sh script builds the engine and loads the correct Haskell file.

How to run an example manually?
Go to the "src" directory in bash and run the following commands:
- ghci
- :l Verification.hs
- verify <function>
The <function> needs to be a Haskell function that uses as syntax the functions of GCL.hs.
All of our own examples are added in the examples.hs.
E.g. to verify "minind" from examples.hs, you need to write: "verify minind"

---Overview structure---
Examples.hs
In the Examples.hs we added several examples. To verify them you can either run buid.sh or load Verification.hs.

Verification.hs
In Verification.hs we calculate the WLP. There is also a function "verify" in this file, with this function you can verify examples.
E.g: "verify minind"

Prover.hs
The code to prove a WLP is located in the Prover.hs.

PrettyPrint.hs
In PrettyPrint.hs is all the code to pretty print GCL code.

Grammar.ag
The datatypes in order to write GCL in Haskell is implemented in Grammer.ag

Collect.ag
This file is used to collect all the vars,refs and program calls.

Transformer.hs
In Transformer.hs we introduce fresh variables, transform simultaneous assignments and transform program calls to the correct form. 

GCL.hs
The syntax to write GCL is added in GCL.hs