all: ParserAbs.o ErrM.o TypeCheckerEnv.o TypeChecker.o LLVMAbs.o LLVMEnv.o LLVMGenerator.o LLVMPrinter.o Parser.o Scanner.o jlc.o
	ghc --make jlc
	mv jlc ../

%.o: %.hs
	ghc -c $<

Scanner.hs: Scanner.x
	alex $^

Parser.hs: Parser.y
	happy $^

clean:
	$(RM) *.o *.hi ../lib/Runtime.class Parser.hs Scanner.hs ../jlc

