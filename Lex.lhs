------------------------------------------------------------------------------
-- The ADP Compiler 
-- Copyright (C) 2001-2008 Peter Steffen, Christian Lang, Marco Ruether, 
--                         Georg Sauthoff, Stefanie Schirmer
--
-- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
-- Updates: http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html
------------------------------------------------------------------------------

This file is part of ADPC (The ADP Compiler).

ADPC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

ADPC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ADPC.  If not, see <http://www.gnu.org/licenses/>.



> module Lex(

>   Token(..),
>   tlexUF, tlexGrammar, tlexAlgebra,
>   tlexSignature, tlexAlgebraTypeDecl,
>   lexer,
>   rev_Lex

> ) where

> import Data.Char
> import Tools

> rev_Lex =  "$Revision$"

> data Token  
>       = TokenUsing
>       | TokenNext
>       | TokenAlt
>       | TokenWith
>       | TokenChoice
>       | TokenP     | TokenTT
>       | TokenTabulated
>       | TokenListed | TokenListedj
>       | TokenNonTabulated
>       | TokenName String
>       | TokenConstructor String 
>	| TokenEq    | TokenComma  | TokenColon | TokenPlusPlus | TokenRightArrow | TokenLeftArrow | TokenDot |  TokenDotDot | TokenBackslash
>	| TokenOB    | TokenCB
>       | TokenOB2   | TokenCB2
>       | TokenOB3   | TokenCB3
>       | TokenAnd   | TokenOr 
>       | TokenInfixOpP1 String    -- priority 1
>       | TokenInfixOpP2 String    -- priority 2
>       | TokenInfixOpP3 String    -- priority 3
>       | TokenIf | TokenThen | TokenElse 
>       | TokenLet | TokenWhere | TokenEndWhere
>       | TokenNewLine 
>       | TokenDefTerminal | TokenDefFilter | TokenDefLA | TokenDefLC | TokenInfinite | TokenAddIf

>       | TokenComb String | TokenTTComb | TokenSTTComb | TokenTTSComb | TokenSTSComb | TokenHHComb | TokenDBComb  
>       | TokenDollar | TokenEOF

>       -- for signatures:
>       | TokenPipe | TokenData | TokenType

>                           deriving (Show) 


ein einfacher Lexer:


> tlexGrammar          = (0::Int)
> tlexUF               = (1::Int)
> tlexAlgebra          = (2::Int)
> tlexSignature        = (3::Int)
> tlexAlgebraTypeDecl  = (4::Int)

Falls innerhalb des Algebrabereichs, werden newlines auch an den Parser übergeben:

lexer cont s =
   case s of
      '\n':s  ->  \line -> lexer cont s (line + 1)
      ... rest of lexical analysis ...

> addline (nl, t, s) = (nl+1, t, s)

> lexer :: Int -> String -> (Int, Token, String)
> lexer ls  [] = (0, TokenEOF,[])

> lexer ls  ('\n':cs) | ls == tlexUF              = addline(lexer ls cs)
>                     | ls == tlexAlgebra         = algrest 
>                     | ls == tlexGrammar         = rest 
>                     | ls == tlexSignature       = rest 
>                     | ls == tlexAlgebraTypeDecl = rest 
>                         where
>                           algrest = case cs of
>                                    ('>':cs') -> (1, TokenNewLine, cs')
>                                    []        -> (1, TokenEOF,[])
>                                    otherwise -> (1, TokenNewLine,  nextline cs)
>                           rest = case cs of
>                                    ('>':cs') -> addline(lexer ls cs')
>                                    []        -> (1,TokenEOF,[])
>                                    otherwise -> addline(lexer ls (nextline cs))

> lexer ls  (' ' :cs)    = lexer ls  cs
> lexer ls  ('\t' :cs)   = lexer ls  cs
> lexer ls  ('\'' :cs)   = (0, TokenName $ "\'" ++ c ++ "\'", tail rest) where (c,rest) = span (/= '\'') cs
> -- lexer ls  ('\'':cs)    = (0, (TokenName var), rest)                    where (var, rest) = ('\'':take 2 cs, drop 2 cs)
> lexer ls  ('\"' :cs)   = (0, TokenName $ "\"" ++ c ++ "\"", tail rest) where (c,rest) = span (/= '\"') cs
> lexer ls  ('\\' :cs)   = (0, TokenBackslash, cs)
> lexer ls  ('&':'&':cs) = (0, TokenAnd, cs)
> lexer ls  ('|':'|':cs@(c:_)) | c /= '|' = (0, TokenOr, cs)
> lexer ls  ('=':'=':cs) = (0, TokenInfixOpP3 "==", cs)
> lexer ls  ('<':'=':cs) = (0, TokenInfixOpP3 "<=", cs)
> lexer ls  ('>':'=':cs) = (0, TokenInfixOpP3 ">=", cs)
> lexer ls  ('/':'=':cs) = (0, TokenInfixOpP3 "/=", cs)
> lexer ls  ('=' :cs) = (0, TokenEq ,   cs)
> lexer ls  ('(' :cs) = (0, TokenOB ,   cs)
> lexer ls  (')' :cs) = (0, TokenCB ,   cs)
> lexer ls  ('[' :cs) = (0, TokenOB2 ,  cs)
> lexer ls  (']' :cs) = (0, TokenCB2 ,  cs)
> lexer ls  ('{' :cs) = (0, TokenOB3 ,  cs)
> lexer ls  ('}' :cs) = (0, TokenCB3 ,  cs)
> lexer ls  ('$' :cs) = (0, TokenDollar ,  cs)
> lexer ls  (',' :cs) = (0, TokenComma, cs)
> lexer ls  (':' :cs) = (0, TokenColon, cs)
> lexer ls  ('+':'+':cs@(c:_)) | c /= '+' && c /= '~' =  (0, TokenPlusPlus,   cs)
> lexer ls  ('-':'>':cs) = (0, TokenRightArrow, cs)
> lexer ls  ('<':'-':cs) = (0, TokenLeftArrow,  cs)
> lexer ls  ('+':c:cs)   | not (isSpecialChar c) = (0, TokenInfixOpP2 "+", c:cs)
> lexer ls  ('-':c:cs)   | not (isSpecialChar c) = (0, TokenInfixOpP2 "-", c:cs)
> lexer ls  ('*':c:cs)   | not (isSpecialChar c) = (0, TokenInfixOpP1 "*", c:cs)
> lexer ls  ('/':c:cs)   | not (isSpecialChar c) = (0, TokenInfixOpP1 "/", c:cs)
> lexer ls  ('>':c:cs)   | not (isSpecialChar c) = (0, TokenInfixOpP3 ">", c:cs)
> lexer ls  ('<':c:cs)   | not (isSpecialChar c) = (0, TokenInfixOpP3 "<", c:cs)
> lexer ls  ('.':c:cs)     | c /= '.' && c /= '~' = (0, TokenDot,       c:cs)
> lexer ls  ('.':'.':c:cs) | c /= '.' = (0, TokenDotDot,    c:cs)
> lexer ls  ('-':'-':cs) = case span isSymbolPart cs of 
>                                ("endwhere", rest) -> (0, TokenEndWhere ,   rest)
>                                (var,        rest) -> lexer ls (nextline rest)
> lexer ls  txt@(c: cs) = let pattern | isSymbolPart2 c = span isSymbolPart2 txt
>                                     | otherwise       = span isSpecialChar txt
>                         in case pattern of
>       ("<<<",        rest)     ->  (0, TokenUsing ,      rest)
>       ("~~~",        rest)     ->  (0, TokenNext ,       rest)
>       ("-~~",        rest)     ->  (0, TokenNext ,       rest)
>       ("~~-",        rest)     ->  (0, TokenNext ,       rest)
>       ("+~~",        rest)     ->  (0, TokenNext ,       rest)
>       ("~~+",        rest)     ->  (0, TokenNext ,       rest)
>       ("+~+",        rest)     ->  (0, TokenNext ,       rest)

>       ("++~",        rest)     ->  (0, TokenNext ,       rest)
>       ("~++",        rest)     ->  (0, TokenNext ,       rest)
>       ("+++",        rest)     ->  (0, TokenNext ,       rest)

>       (".~~",        rest)     ->  (0, TokenNext ,       rest)
>       ("~~.",        rest)     ->  (0, TokenNext ,       rest)

>       ("~~",         rest)     ->  (0, TokenTTComb ,     rest)  
>       ("*~~",        rest)     ->  (0, TokenSTTComb ,    rest)  
>       ("~~*",        rest)     ->  (0, TokenTTSComb ,    rest)  
>       ("*~*",        rest)     ->  (0, TokenSTSComb ,    rest)  
>       ("^^^",        rest)     ->  (0, TokenHHComb ,     rest)  
>       ("/\\\\/",     rest)     ->  (0, TokenDBComb ,     rest)  

>       ("|||",        rest)     ->  (0, TokenAlt ,        rest)
>       ("|"  ,        rest)     ->  (0, TokenPipe,        rest)
>       ("...",        rest)     ->  (0, TokenChoice ,     rest)
>       ("`with`",     rest)     ->  (0, TokenWith ,       rest)
>       ("p",          rest)     
>          | ls == tlexGrammar   ->  (0, TokenP ,          rest)
>          | otherwise           ->  (0, TokenName "p",    rest)
>       ("tt",         rest)     ->  (0, TokenTT ,         rest)
>       ("tabulated",  rest)     ->  (0, TokenTabulated ,   rest)
>       ("listed",     rest)     ->  (0, TokenListed,       rest)
>       ("listedj",    rest)     ->  (0, TokenListedj,      rest)
>       ("nontabulated",  rest)  ->  (0, TokenNonTabulated ,rest)
>       ("if",         rest)     ->  (0, TokenIf ,         rest)
>       ("then",       rest)     ->  (0, TokenThen ,       rest)
>       ("else",       rest)     ->  (0, TokenElse ,       rest)
>       ("let",        rest)     ->  (0, TokenLet ,        rest)
>       ("where",      rest)     ->  (0, TokenWhere ,      rest)
>       ("data",       rest)     ->  (0, TokenData ,       rest)
>       ("type",       rest)     ->  (0, TokenType ,       rest)
>       ("terminal" , rest)      ->  (0, TokenDefTerminal  , rest)
>       ("addif",     rest)      ->  (0, TokenAddIf        , rest)
>       ("filter"   , rest)      ->  (0, TokenDefFilter    , rest)
>       ("lookahead", rest)      ->  (0, TokenDefLA        , rest)
>       ("directdef", rest)      ->  (0, TokenDefLC        , rest)
>       ("Infinite", rest)       ->  (0, TokenInfinite     , rest)

>       ("infixl", rest)         -> addline(lexer ls (nextline rest))

>       (var, rest) -> let result 
>                           | ls==tlexUF                         = (0, (TokenName $ replacePrime var), rest) 
>                           | ls==tlexGrammar && isSpecialChar c = (0, (TokenComb var)               , rest)
>                           | otherwise = case c of 
>                                '+'  ->  (0, (TokenInfixOpP2 "+") ,  cs)
>                                '-'  ->  (0, (TokenInfixOpP2 "-") ,  cs)
>                                '*'  ->  (0, (TokenInfixOpP1 "*") ,  cs)
>                                '/'  ->  (0, (TokenInfixOpP1 "/") ,  cs)
>                                '>'  ->  (0, (TokenInfixOpP3 ">") ,  cs)
>                                '<'  ->  (0, (TokenInfixOpP3 "<") ,  cs)
                       
>                                otherwise -> ret where 
>                                                 (var, rest) = span isSymbolPart2 (c:cs)
>                                                 ret = if      (isUpper c)   then (0, TokenConstructor $ replacePrime var, rest)  
>                                                       else                       (0, TokenName        $ replacePrime var, rest) 
>                       in result

> nextline s =  s' where
>               (_, s') = span ((/=) '\n') s

               tail' [] = []
               tail' a  = tail a 

> replacePrime var = replaceChar var '\'' "_Pr"

> isSymbolPart, isSymbolPart2 :: Char -> Bool
> isSymbolPart c  = not (elem c "\n\t =()[],")
> isSymbolPart2 c = isAlphaNum c || elem c "\'`_" -- not (elem c "\n\t =()[],+-*/")

> isSpecialChar :: Char -> Bool
> isSpecialChar c = elem c "+~-<.*^/|\\>!"

> lextest [] = []
> lextest s = tok: (lextest s') where
>                (ln, tok, s') = lexer tlexGrammar s

> lexFile f = do
>             inp <- readFile f
>             putStrLn (show (lextest inp))

