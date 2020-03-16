all: build test

build:
	./build.ss

test:
	./unit-tests.ss

.DUMMY: all build test
