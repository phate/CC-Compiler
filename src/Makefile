all: java ParserAbs.o ErrM.o TypeCheckerEnv.o TypeChecker.o JVMAbs.o JVMEnv.o JVMGenerator.o JVMPrinter.o Parser.o Scanner.o jlc.o
	ghc --make jlc
	mv jlc ../

java: Runtime.java
	javac $<
	mv Runtime.class ../lib/

%.o: %.hs
	ghc -c $<

Scanner.hs: Scanner.x
	alex $^

Parser.hs: Parser.y
	happy $^

clean:
	$(RM) *.o *.hi ../lib/Runtime.class Parser.hs Scanner.hs ../jlc

