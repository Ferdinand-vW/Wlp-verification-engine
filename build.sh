cabal configure
cabal build
cabal install
uuagc -Hdfcsw src/Collect.ag
ghci src/Verification.hs -isrc