# Command 

build: 
	ghc --make -O -o game Main.hs

prof: 
	ghc --make -prof -o game Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f Game
	rm -f *.hi
	rm -f *.o

test: build
	./game --test
