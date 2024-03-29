-- Embedded block comment lexing from https://stackoverflow.com/a/28142583/

{
module Lexer (Token(..), scanner) where

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
$kp = [$wn \{] -- keyword prefix

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*
@space      = [$wn]+  -- space including newline

@linecmt = "//"(.)*$endoflinecmt

-- Comments and strings
-- In normal code, inside a string everything is a normal character except for the end-string control character
-- In a comment, everything is a normal character except for end of comment (*/ or linebreak) (strings have no effect)



state :-

-- Some keywords must start a new line (input) or be preceded by some space and be followed by some space
<0>  \n  ^ "task" / @space { mkTs BeginTaskHeader `andBegin` tsk } -- "task" keyword must start a new line
<0>  $wn  ^ "input" / @space   { mkTs BeginInput `andBegin` inp } -- "input" keyword can be preceded by new line or spcace; switch to "input mode"
<0>  $kp ^ "send" / @space  { mkTs Send `andBegin` srp } -- switch to "send/reply" mode
<0>  $wn ^ "to" / @space       { mkTs To } -- ("send/reply" mode is only active until beginning of parameter list)
<0>  $kp ^ "reply" / @space    { mkTs Reply `andBegin` srp } -- switch to "send/reply" mode
<0>   \;           { mkTs StmntSep }
<0>   \"           { mkTs (NormalChar '"') `andBegin` str } -- switch to "string" mode
<0>   @linecmt     { mkTs $ NormalChar '\n' } -- line comment ends with a newline
<0>   "/*"         { enterNewComment `andBegin` cmt } -- switch to "comment" mode
<0>   \{           { beginBlock }
<0>   \}           { endBlock }
<0>  $normalchar   { mkTchar NormalChar }
-- Comment mode
<cmt> "/*"         { embedComment }
<cmt> "*/"         { unembedComment }
<cmt> .            ;
<cmt> \n           { skip }
-- String mode
<str> [^\"]        { mkTchar NormalChar }
<str> \"           { mkTs (NormalChar '"') `andBegin` 0 } -- switch back to normal mode
-- "Task header" mode
<tsk> @space       { mkTstr Space }
<tsk> @identifier  { mkTstr Id }
<tsk> \{           { beginBlock `andBegin` 0 } -- the "input" part ends with a "{"
-- In "input mode", ony a parameter list with whitespaces is allowed
<inp> \(           { mkTs BeginParamList }
<inp> \)           { mkTs EndParamList }
<inp> \,           { mkTs ParamSep }
<inp> $wn ^ "when" / @space  { mkTs BeginWhen `andBegin` whn } -- the "input" part ends with a "when", switch to "when mode"
<inp> @space       { mkTstr Space }
<inp> @identifier  { mkTstr Id }
<inp> \{           { beginBlock `andBegin` 0 } -- the "input" part ends with a "{"
-- "When part" mode
<whn> $whenchar    { mkTchar NormalChar } -- in "when mode", we take all characters until the next "{"
<whn> \{           { beginBlock `andBegin` 0 } -- the "when" part ends with a "{"
-- "Send/reply" mode
<srp> @space       { mkTstr Space }
<srp> @identifier  { mkTstr Id }
<srp> \(           { mkTs BeginParamList `andBegin` 0 } -- we mark the beginning of the parameter list but don't need the rest

{
data Token = EOF
           | NormalChar Char -- any character (restricted in some modes)
           | BeginTaskHeader
           | BeginTaskBody
           | EndTask
           | StmntSep
           | Reply
           | Send
           | To
           | BeginString
           | EndString
           | BeginAction
           | EndAction
           -- | BeginBlock Int -- block with depth (starts at 0)
           -- | EndBlock Int
           -- | StringChar Char
           | BeginInput
           | BeginParamList
           | ParamSep
           | EndParamList
           | BeginWhen -- then "when" keyword
           | Space String -- Sequence of whitespaces
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

beginBlock input len =
    do d <- getLexerBlockDepth
       setLexerBlockDepth (d + 1)
       return $
         case d of
           0 -> BeginTaskBody
           1 -> BeginAction
           _ -> NormalChar '{'

endBlock input len =
    do d <- getLexerBlockDepth
       setLexerBlockDepth (d - 1)
       return $
         case d-1 of
           0 -> EndTask
           1 -> EndAction
           _ -> NormalChar '}'

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
