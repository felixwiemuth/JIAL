{
module Lexer (Token(..), scanner) where

import Control.Monad
}

%wrapper "monadUserState"

$all        = [\x00-\x10ffff]
$whitespace = [\ \t\b]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]
$letter     = [a-zA-Z]                                       -- alphabetic characters
$ident      = [$letter $digit _]                             -- identifier character


$normalchar = $all # [\"] -- all but single control characters

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*



-- Comments and strings
-- In normal code, inside a string everything is a normal character except for the end-string control character
-- In a comment, everything is a normal character except for end of comment (*/ or linebreak) (strings have no effect)



state :-

<0>  $normalchar  { getNormalChar }
-- <0>             $whitespace+ ;
<0>   \"           { mkTs BeginString `andBegin` str }
<0>   "/*"         { enterNewComment `andBegin` cmt }
<cmt> "/*"         { embedComment }
<cmt> "*/"         { unembedComment }
<cmt> .            ;
<cmt> \n           { skip }
<str> [^\"]        { getStringChar }
<str> \"           { mkTs EndString `andBegin` 0 }

{
data Token = EOF
           | NormalChar Char -- any character
           | BeginString
           | EndString
           | ID String
           | StringChar Char
  deriving (Show, Eq)

alexEOF :: Alex Token
alexEOF = return EOF

data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                   }

-- Make a token without using input ("simple token")
-- mkTs
mkTs t = token (\ _ _ -> t)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())


beginString input len =
    do skip input len

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


getNormalChar (p, _, _, input) len = return $ NormalChar c
  where
    c = input!!0

getVariable (p, _, _, input) len = return $ ID s
  where
    s = take len input

getStringChar (p, _, _, input) len = return $ StringChar c
  where
    c = input!!0

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
  print $ "Input:  " ++ s
  print $ "Tokens: " ++ (show (scanner s))


}
