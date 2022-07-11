
.PHONY: run

run:
	stack ghci src/Solver/Main.hs

.PHONY: build

build:
	stack build
