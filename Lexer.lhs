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




> module Lexer where

> import Data.List
> import ParseMonad
> import Tools

Der Type LineInc stellt die Anzahl an Zeilenvorschueben dar, damit
der Parser mitzaehlen kann in welcher Zeile er sich gerade befindet.


> lexer :: String -> (LineInc, Token, String)
> lexer ('\n':as) = incLine $ lexer as
> lexer as        = basicScanner as

> testLexer f = do
>               inp <- readFile f
>               print $ test inp
>   where
>     test [] = []
>     test s = let (_, tok, rest) = lexer s
>              in tok : test rest

==================================================================

> data Token = AlgebraTypeToken                  -- "#algebratype"
>            | AlgebraToken                      -- "#algebra"
>            | GrammarToken                      -- "#grammar"
>            | UserDefsToken                     -- "#userdefs"
>            | MainCodeToken String              -- "#main"
>            | TargetCodeToken String            -- "#target"
>            | TypeSynToken                      -- "#typesyn"
>            | ExternToken String                -- "#extern"
>            | FilterToken                       -- "#filter"
>            | LineToken                         -- "#line"
>            | TParser
>            | TInt
>            | TInteger
>            | TFloat 	
>            | TDouble   
>            | TChar     
>            | TString   
>            | TBool     
>            | TVoid     
>            | TCNum  
>            | TCIntegral
>            | TCFloating
>            | TypeIdent String                  -- speichert den Typnamen
>            | Where | If | Then | Else | Type   -- wichtige Schluesselworte
>            | Let | EndWhere
>            | Tabulated | Listed | Listedj      -- ADP-Schluesselworte
>            | Nontabulated | With | Axiom 
>            | DefTerminal | DefFilter
>            | DefAddIf | DefLA | DefLC
>            | Infinite                              -- Schluesselworte fuer #userdefs{...}
>            | OpenBr | CloseBr                      -- normale Klammern
>            | OpenCurlBr | CloseCurlBr              -- geschweifte Klammern
>            | OpenSqBr | CloseSqBr                  -- eckige Klammern
>            | Colon | DoubleColon | Semicolon       -- ":", "::" ";"
>            | Assign | RArrow | LArrow | Comma      -- "=", "->", "<-", ","
>            | RArrow2 | Pipe | DotDot | Backslash   -- "|", "..", "\"
>            | EQ | LT | LE | GT | GE | NE       -- Vergleichsoperatoren
>            | Add | Sub | Mult | Div |Exp       -- "+", "-", "*", "/" "^"
>            | And | Or | Append                 -- "&&", "||", "++"
>            | ADPUses | ADPNext String | ADPOr  -- "<<<", "~~~", "|||"
>            | ADPSelect | ADPTT                 -- "...", "><"
>            | ADPNextTT | ADPNextSTT            -- Next-Kombinator-Varianten
>            | ADPNextTTS | ADPNextSTS           -- zur Verwendung in benutzer-
>            | ADPNextHHH                        -- eigenen Definitionen
>            | Ident String                      -- speichert einen Identifier
>            | Underscore                        -- der Unterstrich
>            | Num Double                        -- speichert die Buchstaben die
>                                                -- eine Zahl darstellen (Int bzw. Float)
>            | String String                     -- ein Stringliteral
>            | Char Char                         -- ein Chracterliteral
>            | ADPComment                        -- ein Kommentar, wir nur zur Kennzeichnung gebraucht

>            | EOF                               -- ein EndOfFile fuer den Parser

>               deriving (Show, Eq)



> basicScanner :: String -> (LineInc, Token, String)
> basicScanner []       = (LineInc 0, EOF, "")
> basicScanner (a:as)   | a == '\n' || a == '\r' = incLine rest
>                       | whitespace a           = rest
>     where rest = basicScanner as
> basicScanner ('#':as) | x == "algebratype" = (LineInc 0, AlgebraTypeToken, xs)
>                       | x == "algebra"     = (LineInc 0, AlgebraToken, xs)
>                       | x == "grammar"     = (LineInc 0, GrammarToken, xs)
>                       | x == "userdefs"    = (LineInc 0, UserDefsToken, xs)
>                       | x == "typesyn"     = (LineInc 0, TypeSynToken, xs)
>                       | x == "extern"      = let (ident, rest) = splitDC xs
>                                                  ws = words ident 
>                                              in case ws of
>                                                  [ident] -> let ident' = replaceChar ident '\'' "_Pr"
>                                                             in if head ident' == '(' && last ident' == ')' then
>                                                                  (LineInc 0, ExternToken (tail $ init $ ident'), rest)
>                                                             else (LineInc 0, ExternToken ident', rest)
>                                                  ident   -> error $ "cannot process extern identifier: " ++ show ident
>                       | x == "filter"      = (LineInc 0, FilterToken, xs)
>                       | x == "main"        = let (l, s, rest) = getArea as False "main{" "#}"
>                                              in (LineInc l, MainCodeToken s, rest)
>                       | x == "target"      = let (l, s, rest) = getArea as False "target{" "#}"
>                                              in (LineInc l, TargetCodeToken s, rest)
>                       | x == "line"        = let (def, rest) = span (/= ';') xs 
>                                                  newlines    = length $ filter (== '\n') def
>                                                  (modname,line) = case words def of
>                                                                          [a,b]     -> (a,(read b) :: Int)
>                                                                          otherwise -> error $ "#line syntax error. required form: #line <module name> <number>"
>                                              in moduleSwitch modname (line+newlines) (basicScanner (tail rest))
> --                    | otherwise          = error "Fehler: Metasymbol nicht vorhanden!"
>     where (x, xs) = span (identChar) as
> basicScanner ('`':as) | x == "with" && head xs == '`' = (LineInc 0, With, tail xs)
> --                    | otherwise          = error "Fehler: nur With ist als Infixfunktionsanwendung erlaubt."
>     where (x, xs) = span (identChar) as
> basicScanner ('\'':ch:qt:as) | qt == '\''                    = (LineInc 0, Char ch, as)
>                              | ch == '\\' && head as == '\'' = (LineInc 0, Char (scanQuoteChar qt), tail as)
>                              | otherwise                     = error "Unproperly terminated character literal."
> basicScanner ('\"':as) | head xs == '\"' = (LineInc 0, String x, tail xs)
>                        | otherwise       = error "Unproperly terminated string literal."
>     where (x, xs) = span ((/=) '\"') as
> basicScanner (a:as) | isEndWhere (a:as) = let xs = drop (length endWhereToken) (a:as)
>                                           in (LineInc 0, EndWhere, xs)
>                     | identStartChar a  = let (ident, xs) = span (identChar) (a:as)
>                                           in (LineInc 0, scanIdent ident, xs)
>                     | upperCaseChar a   = let (ident, xs) = span (identChar) (a:as)
>                                           in (LineInc 0, scanType ident, xs)
>                     | numberChar a      = let (number, xs) = scanNumber (a:as)
>                                           in (LineInc 0, number, xs)
>                     | a == '(' && as /= [] && head as == ')' 
>                                         = (LineInc 0, TVoid, tail as)
>                     | braceChar a       = (LineInc 0, scanBrace a, as)
>                     | specialChar a     = let (special, xs) = span (specialChar) (a:as)
>                                               specialToken = scanSpecial special
>                                           in case specialToken of
>                                                  ADPComment  -> basicScanner $ dropWhile (not . isNewline) xs
>                                                  otherwise   -> (LineInc 0, specialToken, xs)
> basicScanner as        = error $ "scanner: unknown token \"" ++ (take 10 as) ++ "...\""

> splitDC :: String -> (String, String)
> splitDC = split [] 
>  where
>    split _  [] = error $ "syntax error for #extern declaration"
>    split ident (':':':':rest) = (ident, "::" ++ rest)
>    split ident (x:rest)       = split (ident ++ [x]) rest 

> scanBrace '(' = OpenBr
> scanBrace ')' = CloseBr
> scanBrace '[' = OpenSqBr
> scanBrace ']' = CloseSqBr
> scanBrace '{' = OpenCurlBr
> scanBrace '}' = CloseCurlBr
> scanBrace ';' = Semicolon
> scanBrace ',' = Comma

> scanQuoteChar 'r'  = '\r'
> scanQuoteChar 'n'  = '\n'
> scanQuoteChar 't'  = '\t'
> scanQuoteChar '\\' = '\\'

> scanNumber (a:as) = let (numHead, xs) = span numberChar (a:as)
>                         (numTail, xs') = if head xs == '.' then span numberChar (tail xs)
>                                                            else ("", xs)
>                         num = if numTail == "" then numHead
>                                                else numHead ++ "." ++ numTail
>                         num' | all (flip elem ("." ++ ['0'..'9'])) num && length (filter (== '.') num) <= 1 = read num :: Double
>                              | otherwise = error $ "scanner: no number: " ++ (a:as)
>                     in (Num num', xs')

> scanSpecial "=>"  = RArrow2
> scanSpecial "="   = Assign
> scanSpecial "->"  = RArrow
> scanSpecial "<-"  = LArrow
> scanSpecial "|"   = Pipe
> scanSpecial ".."  = DotDot
> scanSpecial "\\"  = Backslash
> scanSpecial "=="  = Lexer.EQ
> scanSpecial "<"   = Lexer.LT
> scanSpecial "<="  = Lexer.LE
> scanSpecial ">"   = Lexer.GT
> scanSpecial ">="  = Lexer.GE
> scanSpecial "/="  = NE
> scanSpecial "+"   = Add
> scanSpecial "-"   = Sub
> scanSpecial "*"   = Mult
> scanSpecial "/"   = Div
> scanSpecial "^"   = Exp
> scanSpecial "&&"  = And
> scanSpecial "||"  = Or
> scanSpecial "++"  = Append
> scanSpecial ":"   = Colon
> scanSpecial "::"  = DoubleColon
> scanSpecial "<<<" = ADPUses
> scanSpecial "~~~" = ADPNext "~~~"

> -- Varianten des Next-Kombinators; werden ignoriert
> scanSpecial "-~~" = ADPNext "~~~"
> scanSpecial "~~-" = ADPNext "~~~"
> scanSpecial "-~-" = ADPNext "~~~"
> scanSpecial "+~~" = ADPNext "~~~"
> scanSpecial "~~+" = ADPNext "~~~"
> scanSpecial "+~+" = ADPNext "~~~"
> scanSpecial ".~~" = ADPNext "~~~"
> scanSpecial "~~." = ADPNext "~~~"
> scanSpecial "++~" = ADPNext "~~~"
> scanSpecial "~++" = ADPNext "~~~"

> -- Next-Kombinatoren, die zur Definition benutzereigener Kombinatoren
> -- verwendet werden koennen:
> scanSpecial "~~"  = ADPNextTT    -- (TT  = tilde tilde)
> scanSpecial "*~~" = ADPNextSTT   -- (STT = star tilde tilde)
> scanSpecial "~~*" = ADPNextTTS
> scanSpecial "*~*" = ADPNextSTS
> scanSpecial "^^^" = ADPNextHHH

> scanSpecial "|||" = ADPOr
> scanSpecial "..." = ADPSelect
> scanSpecial "><"  = ADPTT
> scanSpecial "--"  = ADPComment
> scanSpecial s     = ADPNext s  -- alle anderen werden hier erstmal als moegliche Next-Kombinatoren
>                                -- geliefert.

> scanType "Parser"     = TParser
> scanType "Int"        = TInt
> scanType "Integer"    = TInteger
> scanType "Float"      = TFloat
> scanType "Double"     = TDouble
> scanType "Char"       = TChar
> scanType "String"     = TString
> scanType "Bool"       = TBool
> scanType "Num"        = TCNum
> scanType "Integral"   = TCIntegral
> scanType "Floating"   = TCFloating

> scanType "Infinite"  = Infinite
> scanType ident       = TypeIdent ident

> scanIdent "where"         = Where
> scanIdent "if"            = If
> scanIdent "then"          = Then
> scanIdent "else"          = Else
> scanIdent "type"          = Type
> scanIdent "let"           = Let
> scanIdent "tabulated"     = Tabulated
> scanIdent "listed"        = Listed
> scanIdent "listedj"       = Listedj
> scanIdent "nontabulated"  = Nontabulated
> scanIdent "axiom"         = Axiom
> scanIdent "terminal"      = DefTerminal
> scanIdent "filter"        = DefFilter
> scanIdent "addif"         = DefAddIf
> scanIdent "lookahead"     = DefLA
> scanIdent "directdef"     = DefLC
> scanIdent "_"             = Underscore
> scanIdent ident           = Ident $ replaceChar ident '\'' "_Pr"  -- ' durch "_Pr" ersetzen



Hilfsfunktionen

> whitespace :: Char -> Bool
> whitespace c = elem c " \n\t\r"

> isNewline :: Char -> Bool
> isNewline c = elem c "\n\r"

> specialChar :: Char -> Bool
> specialChar c = elem c "=-></+-*&|:~.^!\\"

> braceChar c = elem c "()[]{};,"    -- ";" is a brace char, because it belongs to single character class
> identChar c = lowerCaseChar c || upperCaseChar c || numberChar c || elem c "'_"
> identStartChar c = lowerCaseChar c || c == '_'
> lowerCaseChar c = elem c ['a'..'z']
> upperCaseChar c = elem c ['A'..'Z']
> numberChar c = elem c ['0'..'9']

> endWhereToken = "--endwhere"
> isEndWhere s = take (length endWhereToken) s == endWhereToken


> data LineInc = LineInc Int
>              | ModuleSwitch String Int

> incLine :: (LineInc, Token, String) -> (LineInc, Token, String)
> incLine (LineInc li,        t, s) = (LineInc $ li+1, t, s)
> incLine (ModuleSwitch m li, t, s) = (ModuleSwitch m li, t, s)

> moduleSwitch :: String -> Int -> (LineInc, Token, String) -> (LineInc, Token, String)
> moduleSwitch modName line (LineInc li,        t, s) = (ModuleSwitch modName (line + li), t, s)
> moduleSwitch modName line (ModuleSwitch m li, t, s) = (ModuleSwitch m li, t, s)

> getArea :: String -> Bool -> String -> String -> (Int, String, String)
> getArea inp includeFrame begin end = 
>     if includeFrame then (max 1 (length inp') + 1, begin ++ "\n" ++ drop 2 (unlines inp') ++ end ++ "\n", unlines $ tail rest) 
>                     else (max 1 (length inp') + 1, drop 2 (unlines inp'), unlines $ tail rest) 
>  where
>   (_, _, (_, inp', rest)) = extractArea (lines inp) begin end

>   tail' [] = []
>   tail' xs = tail xs
>   extractArea :: [String] -> String -> String -> (String, Int, ([String],[String],[String]))
>   extractArea ls s epat = (pa, start, (a, b', c))
>     where
      
>       b' = case b of
>              []        -> []
>              otherwise -> ['\n']:b
      
>       start = (length a) -1 
      
>       (pa,(a,b,c)) = extractArea' False ls s epat
      
>       extractArea' _     [] _     _    = ("", ([],[],[]))
>       extractArea' False (l:ls) s epat = (pa++pa'', (l:a, b, c))
>           where 
>             (pa, (a,b,c))  = extractArea' (l'==s) ls s epat
>             (l', pa')      = rmPar l
>             pa''           = if (l'==s) then pa' else ""
      
>       extractArea' True (l:ls) s epat = if l == epat then ("",([],[],(l:ls))) else ("", (a, l:b, c)) 
>          where 
>             (_,(a,b,c)) = extractArea' True ls s epat
      
>       rmPar s = (b ++ tail' e, tail' p)
>          where
>             (b,b') = span ((/=) '[') s
>             (p,e)  = span ((/=) ']') b'
>             tail' [] = []
>             tail' x  = tail x

> testgetArea incl f = 
>   do
>     inp <- readFile f
>     (lines, cont, rest) <- return $ getArea inp incl "#target{" "#}"
>     putStrLn $ "lines: " ++ show lines
>     putStrLn $ "content: " ++ show cont
>     putStrLn $ "rest: " ++ show rest

