cmd := ghc -dynamic

main: Lexer.hs Parser.hs
	$(cmd) Main.hs

lexer: Lexer.hs
	$(cmd) Lexer.hs

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y Lexer.hs
	happy Parser.y

LexerTest.hs: Lexer.hs

ParserTest.hs: Parser.hs

ltest: LexerTest.hs
	runhaskell LexerTest

ptest: ParserTest.hs
	runhaskell ParserTest

test: LexerTest.hs ParserTest.hs
	runhaskell LexerTest
	runhaskell ParserTest

mtest: MTest.hs Lexer.hs Parser.hs
	runhaskell MTest

clean:
	rm Main Lexer.hs Parser.hs *.o *.hi
