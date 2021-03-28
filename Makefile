all: build test

build:
	./build.ss

test:
	./unit-tests.ss

# Build using nix-build
nix:
	./build.ss nix

.DUMMY: all build test nix
.PHONY: all build test nix
