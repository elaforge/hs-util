.PHONY: StrLit
StrLit: StrLit.hs
	ghc -Wall -Wno-name-shadowing -O --make StrLit.hs
	strip StrLit
