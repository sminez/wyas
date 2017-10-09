parse:
	ghc -o parse --make parse.hs

clean:
	rm *.o
	rm *.hi
	rm parse
