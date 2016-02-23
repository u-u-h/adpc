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



> {
> module AlggenParse where 
> import Data.Char

> }

> %monad { P } { thenP } { returnP }
> %lexer { lexer } { TEOF }

> %name pSignatures

> %tokentype { Token }


> %token 
>	'('		{ OpenPar }
>	')'		{ ClosePar }
>       ident           { Ident $$ }
>       ','             { Comma }
>       '='             { Equal }
>       '|'             { Pipe }
>       data            { DataSym }

> %%


> Signatures : {- empty -}             { [] }
>            | Signature Signatures    { ($1 : $2) }

> Signature : data ident '=' Operators { ($2, $4) }

> Operators : Operator                 { [$1] }
>           | Operator '|' Operators   { $1 : $3 }

> Operator : ident Arguments           { ($1, $2) }

> Arguments : {- empty -}              { [] }
>           | Argument Arguments       { $1 : $2 }

> Argument : ident                     { Id $1 }
>          | Tupel                     { $1 }

Tupel : '(' Argument ',' TArgs ')'   { Tupel ($2:$4) }

> Tupel : '(' ')'                      { Tupel [] }
>       | '(' TArgs ')'                { Tupel $2 }

> TArgs : Argument                     { [$1] }
>       | Argument ',' TArgs           { $1 : $3 }

> {


> type Name = String
> data Argument = Id String |
>                 Tupel [Argument]     deriving (Eq, Show)

> type Operator = (Name, [Argument])
> type Signature = (Name, [Operator])

> ppSignature (n, ops) = "data " ++ n ++ "\n" ++ spc ++ "=" ++ mapsep ("\n" ++ spc ++ "|") ppOperator ops ++ "\n" ++ derivingStr
>    where derivingStr = "                       deriving (Eq, Show)\n"
> ppOperator (n, args) = spc ++ n ++ " " ++ mapsep " " ppArgument args 
> ppArgument (Id n) = n
> ppArgument (Tupel t) = "(" ++ mapsep ", " ppArgument t ++ ")"


> mapsep sep f xs = sepList sep (map f xs)

> sepList _ []       = ""
> sepList _ [x]      = x
> sepList t (x:xs)   = x ++ t ++ sepList t xs
> spc = "  "

Scanner:
----------

> data Token =
>   Ident String | DataSym | Pipe | Equal | OpenPar | ClosePar | Comma | TEOF
>        deriving (Eq, Show)

> isAOrD c = isDigit c || isAlpha c || c == '_'

> lexer :: (Token -> P a) -> P a
> lexer cont s = cont token s' where
>         (token, s') = scanner s

> scanner :: String -> (Token,String)
> scanner []        = (TEOF, [])
> scanner (' ':xs)  = scanner xs
> scanner ('\n':'>':xs) = scanner xs
> scanner ('\n':xs) = scanner $ dropWhile (/='\n') xs
> scanner ('\t':xs) = scanner xs
> scanner (x:xs)    | isAlpha x  = scanIdent  (x:xs)
>                   | otherwise  = scanToken  (x:xs) 

>  where
>    isalphanum c = isAlpha c || isDigit c || c == '_'
>    scanIdent x = (token, rest) 
>     where  
>      (ident, rest) = span isAOrD x
>      keywords      = [("data", DataSym)]
>      keyword       = [ t | (s, t) <- keywords, s == ident]
>      token         = case keyword of
>                        []        -> Ident ident
>                        otherwise -> head keyword

>    scanToken ('|':rest)     = (Pipe           , rest) 
>    scanToken ('=':rest)     = (Equal          , rest) 
>    scanToken (')':rest)     = (ClosePar       , rest) 
>    scanToken ('(':rest)     = (OpenPar        , rest)
>    scanToken (',':rest)     = (Comma          , rest)

    scanToken (x:rest)       = error ("unknown symbol " ++ [x])


Hilfsfunktion: 

 scanFile f = do
               inp <- readFile f
               putStrLn (show (scanner inp))


> data ParseResult a = Ok a | Failed String
> type P a = String -> ParseResult a

> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s ->
>     case m s of 
>         Ok a -> k a s
>         Failed e -> Failed e

> returnP :: a -> P a
> returnP a = \s -> Ok a

> failP :: String -> P a
> failP err = \s -> Failed err

> catchP :: P a -> (String -> P a) -> P a
> catchP m k = \s ->
>     case m s of
>        Ok a -> Ok a
>        Failed e -> k e s


> happyError :: P a
> happyError  = \s -> error "Parse error"

> parse  = catP.pSignatures where
>           catP(Ok a) = a
>           catP(Failed s) = error s

> parseFile f = do
>               inp <- readFile f
>               putStrLn(show(parse inp))


> }
