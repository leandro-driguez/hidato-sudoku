
.PHONY: run

run:
	stack ghci app/Main.hs

.PHONY: build

build:
	stack build
