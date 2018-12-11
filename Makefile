cmd := ghc -dynamic

jialc: JIALC.hs
	$(cmd) -o jialc JIALC.hs

jialt: JIALT.hs
	$(cmd) -o jialt JIALT.hs

lexer: Lexer.hs
	$(cmd) Lexer.hs

Lexer.hs: Lexer.x
	alex Lexer.x

MsgTypeLexer.hs: MsgTypeLexer.x
	alex MsgTypeLexer.x

Parser.hs: Parser.y Lexer.hs
	happy Parser.y

LexerTest.hs: Lexer.hs

ParserTest.hs: Parser.hs

ltest: LexerTest.hs
	runhaskell LexerTest

ptest: ParserTest.hs
	runhaskell ParserTest

test: LexerTest.hs ParserTest.hs AnalysisTest.hs AlgTest.hs CodeGeneratorTest.hs
	runhaskell LexerTest
	runhaskell ParserTest
	runhaskell AnalysisTest
	runhaskell AlgTest
	runhaskell CodeGeneratorTest

mtest: MTest.hs Lexer.hs Parser.hs MsgTypeLexer.hs
	runhaskell MTest

analysis: Analysis.hs
	$(cmd) Analysis.hs

atest: AnalysisTest.hs
	runhaskell AnalysisTest.hs

algtest:
	runhaskell AlgTest.hs

ctest: CodeGeneratorTest.hs
	runhaskell CodeGeneratorTest.hs

clean:
	rm Main Lexer.hs Parser.hs *.o *.hi
