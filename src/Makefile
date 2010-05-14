all: llvm

llvm: llvmr ParserAbs.o Desugarer.o ErrM.o TypeCheckerEnv.o TypeChecker.o LLVMAbs.o LLVMEnv.o LLVMGenerator.o LLVMPrinter.o Parser.o Scanner.o jlc.o
	ghc --make jlc
	mv jlc ../

llvmr: Runtime.ll
	llvm-as $<
	mv Runtime.bc ../lib/runtime.bc

jvm: javar ParserAbs.o ErrM.o TypeCheckerEnv.o TypeChecker.o JVMAbs.o JVMEnv.o JVMGenerator.o JVMPrinter.o Parser.o Scanner.o jlc.o
	ghc --make jlc
	mv jlc ../

javar: Runtime.java
	javac $<
	mv Runtime.class ../lib/

%.o: %.hs
	ghc -c $<

Scanner.hs: Scanner.x
	alex $^

Parser.hs: Parser.y
	happy $^

clean:
	$(RM) *.o *.hi ../lib/Runtime.class ../lib/runtime.bc Parser.hs Scanner.hs ../jlc

