.PHONY: StrLit
StrLit: StrLit.hs
	ghc -Wall -O --make StrLit.hs
	strip StrLit
