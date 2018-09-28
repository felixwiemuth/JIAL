cmd := ghc -dynamic

main: Lexer.hs Parser.hs
	$(cmd) Main.hs

lexer: Lexer.hs
	$(cmd) Lexer.hs

test: LexerTest.hs Lexer.hs
	runhaskell LexerTest

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y

clean:
	rm Main Lexer.hs Parser.hs *.o *.hi
