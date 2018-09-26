{
module Lexer (Token(..), scanner, main) where

import Control.Monad
}

%wrapper "monadUserState"

$whitespace = [\ \t\b]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]
$letter     = [a-zA-Z]                                       -- alphabetic characters
$ident      = [$letter $digit _]                             -- identifier character

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*

state :-

<0>             @identifier  { getVariable }
<0>             $whitespace+ ;
<0>             "/*"         { enterNewComment `andBegin` cmt }
<cmt> "/*"         { embedComment }
<cmt> "*/"         { unembedComment }
<cmt> .            ;
<cmt> \n           { skip }

{
data Token = EOF
           | ID String
  deriving (Show, Eq)

alexEOF :: Alex Token
alexEOF = return EOF

data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len

embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

state_initial :: Int
state_initial = 0

getVariable (p, _, _, input) len = return $ ID s
  where
    s = take len input

scanner :: String -> Either String [Token]
scanner str = 
  let loop = do
        tok <- alexMonadScan
        if tok == EOF
          then return []
          else do toks <- loop; return (tok:toks)
  in runAlex str loop

-- main = print $ scanner "This /* is */ a /* nested /* comment */ foo */ test"

main = do
  s <- getContents
  print (scanner s)
}
