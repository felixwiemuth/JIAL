cmd := ghc -dynamic

main: Lexer.hs
	$(cmd) Main.hs

lexer: Lexer.hs
	$(cmd) Lexer.hs

test: LexerTest.hs Lexer.hs
	runhaskell LexerTest

Lexer.hs: Lexer.x
	alex Lexer.x
