all: build test

build:
	./build.ss

test:
	./unit-tests.ss

doc:
	raco pkg install --skip-installed
	# TODO: figure out how to make a search bar for only `glow`
	scribble --htmls --dest doc scribblings/glow.scrbl

# Build using nix-build
nix:
	./build.ss nix

.DUMMY: all build test doc nix
.PHONY: all build test doc nix
