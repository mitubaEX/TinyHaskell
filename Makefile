all: main.hs
	stack ghc -- --make main.hs

clean:
	rm -f main ./*.hi ./*.o
