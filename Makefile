all: ParserAbs.o ErrM.o TypeCheckerEnv.o TypeChecker.o Parser.o Scanner.o javac.o
	ghc --make javac 

%.o: %.hs
	ghc -c $<

Scanner.hs: Scanner.x
	alex $^

Parser.hs: Parser.y
	happy $^

clean:
	$(RM) *.o *.hi Parser.hs Scanner.hs javac

