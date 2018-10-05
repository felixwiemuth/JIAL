-- Embedded block comment lexing from https://stackoverflow.com/a/28142583/

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


$normalchar = $all # [\" \{ \} \;] -- all but single control characters
$whenchar = $all # [\{]
$endoflinecmt = [\n]

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*
@space      = [$whitespace]+

@linecmt = "//"(.)*$endoflinecmt

-- Comments and strings
-- In normal code, inside a string everything is a normal character except for the end-string control character
-- In a comment, everything is a normal character except for end of comment (*/ or linebreak) (strings have no effect)



state :-

<0>  $normalchar   { mkTchar NormalChar }
-- <0>             $whitespace+ ;
<0>   \;           { mkTs StmntSep }
<0>   \"           { mkTs BeginString `andBegin` str }
<0>   @linecmt     { skip }
<0>   "/*"         { enterNewComment `andBegin` cmt }
<0>   \{           { beginBlock }
<0>   \}           { endBlock }
<0>   "input"      { mkTs BeginInput `andBegin` inp } -- after "input" keyword switch to "input mode"
<0>   "send"       { mkTs Send }
<0>   "reply"       { mkTs Reply }
<cmt> "/*"         { embedComment }
<cmt> "*/"         { unembedComment }
<cmt> .            ;
<cmt> \n           { skip }
<str> [^\"]        { mkTchar StringChar }
<str> \"           { mkTs EndString `andBegin` 0 }
-- In "input mode", ony a parameter list with whitespaces is allowed
<inp> \(           { mkTs BeginParamList }
<inp> \)           { mkTs EndParamList }
<inp> \,           { mkTs ParamSep }
<inp> "when"       { mkTs BeginWhen `andBegin` whn } -- after "when" keyword switch to "when mode"
<inp> @space       { mkTstr Space }
<inp> @identifier  { mkTstr Id }
<whn> $whenchar    { mkTchar NormalChar } -- in "when mode", we take all characters until the next "{"
<whn> \{           { beginBlock `andBegin` 0} -- the "when" part ends with a "{"

{
data Token = EOF
           | NormalChar Char -- any character (restricted in some modes)
           | StmntSep
           | Send
           | Reply
           | BeginString
           | EndString
           | BeginBlock Int -- block with depth (starts at 0)
           | EndBlock Int
           | StringChar Char
           | BeginInput
           | BeginParamList
           | EndParamList
           | ParamSep
           | Space String -- Sequence of whitespaces
           | Id String
           | BeginWhen -- then "when" keyword
  deriving (Show, Eq)

alexEOF :: Alex Token
alexEOF = return EOF

data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                     lexerCommentDepth  :: Int
                   , lexerBlockDepth :: Int
                   }

-- Make a token without using input ("simple token")
-- mkTs
mkTs t = token (\ _ _ -> t)

-- Make a token that takes the matched string
mkTstr t = \(p, _, _, input) len -> return $ t (take len input)

-- Like mkTstr, just for one char
mkTchar t = \(p, _, _, input) len -> return $ t (input!!0)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 0

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

getLexerBlockDepth :: Alex Int
getLexerBlockDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerBlockDepth ust)

setLexerBlockDepth :: Int -> Alex ()
setLexerBlockDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerBlockDepth=ss}}, ())

beginString input len =
    do skip input len

beginBlock input len =
    do d <- getLexerBlockDepth
       setLexerBlockDepth (d + 1)
       return $ BeginBlock d

endBlock input len =
    do d <- getLexerBlockDepth
       setLexerBlockDepth (d - 1)
       return $ EndBlock (d - 1)

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
