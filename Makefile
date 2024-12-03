# Commands
build: 
	ghc -main-is Game --make -O -o game Game.hs
	ghc -main-is Testing --make -O -o test Testing.hs

prof: 
	ghc -main-is Game --make -prof -o game Game.hs
	ghc -main-is Testing --make -prof -o test Testing.hs

all: 
	build game
	build test

# Cleaning commands:
clean:
	rm -f game
	rm -f test
	rm -f *.hi
	rm -f *.o

game: build
	./game --game

test: build
	./test --test
