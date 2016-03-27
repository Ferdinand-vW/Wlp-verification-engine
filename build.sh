#cabal configure
#cabal build
uuagc -Hdfcsw src/Grammer.ag
uuagc -Hdfcsw src/Collect.ag
ghci src/Verification.hs -isrc