cabal configure
cabal build
uuagc -Hdfcsw src/Collect.ag
ghci src/Verification.hs -isrc