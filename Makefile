CABAL ?= cabal
GHC ?= ghc
STACK ?= stack

.PHONY : all
all :
	$(STACK) build

.PHONY : clean
clean :
	rm -rf .stack-work

.PHONY : test
test :
	$(STACK) test
