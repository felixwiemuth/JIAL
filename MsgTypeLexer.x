-- NOTE: This is a shortened/adapter version of Lexer.x
-- Lexes a message type file, producing NormalCharBlocks to be put as-is in the resulting file and produces for each message type definition a "MsgTypeName" token followed by "Id" tokens (each representing a parameter type)
-- Embedded block comment lexing from https://stackoverflow.com/a/28142583/

{
module MsgTypeLexer (Token(..), scanner) where

import Control.Monad
}

%wrapper "monadUserState"

$all        = [\x00-\x10ffff]
$w          = [\ \t\b]
$wn         = [\ \t\b\n]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]
$letter     = [a-zA-Z]                                       -- alphabetic characters
$ident      = [$letter $digit _]                             -- identifier character


$normalchar = $all # [\" \{ \} \;] -- all but single control characters
$whenchar = $all # [\{]
$endoflinecmt = [\n]
$allbutn = $all # [\n]
$kp = [$wn \{] -- keyword prefix

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*
@space      = [$wn]+  -- space including newline

@linecmt = "//"(.)*$endoflinecmt

-- Comments and strings
-- In normal code, inside a string everything is a normal character except for the end-string control character
-- In a comment, everything is a normal character except for end of comment (*/ or linebreak) (strings have no effect)

@packagedef = "package "($allbutn)*[\n]*
@importdef = "import "($allbutn)*[\n]*

state :-

-- 0 state always starts at beginning of line
<0>   @packagedef   { mkTstr NormalCharBlock }
<0>   @importdef    { mkTstr NormalCharBlock }
<0>   @linecmt      { mkTs $ NormalCharBlock "\n" } -- line comment ends with a newline
<0>   "/*"          { enterNewComment `andBegin` cmt } -- switch to "comment" mode
-- Everything else is the definition of a message type
<0>   @identifier   { mkTstr MsgTypeName `andBegin` m } -- switch to "message type" mode

-- Parse the parameter list of a message type definition
<m>   @space        { skip }
<m>   @identifier   { mkTstr Id }
<m>   \(            { skip }
<m>   \)            { skip }
<m>   \,            { skip }
<m>   \n            { begin 0 }

-- read as normal char until end of line
<n> ($allbutn)+    { mkTstr NormalCharBlock }
<n> \n             { mkTs (NormalCharBlock "\n") `andBegin` 0 }

<cmt> "/*"         { embedComment }
<cmt> "*/"         { unembedComment }
<cmt> .            ;
<cmt> \n           { skip }

{
data Token = EOF
           | NormalCharBlock String
           | MsgTypeName String
           | Id String
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
