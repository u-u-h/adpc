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

> module PreProcParser (

>    preprocparser,
>    getArea, testgetArea,

> ) where

> }

> %monad { P } { thenP } { returnP }
> %lexer { lexerP } { EOF }


> %name preprocess
> %tokentype { Token }


> %token
>     fill              { Fill $$ }
>     keyword           { Keyword $$ }
>     space             { Space }
>     tab               { Tab }
>     return            { Return }
>     openbrace         { OpenBrace }
>     closebrace        { CloseBrace }
>     openSQbracket     { OpenSquareBracket }
>     closeSQbracket    { CloseSquareBracket }
>     hash              { Hash }
>     verbatimcode      { VerbCode $$ }
>     greater           { Greater }
>     equal             { Equal }
>     doubledoubledot   { DoubleDoubleDot }
>     where             { Where }


Es muessen hier keine Assoziativitaeten oder Bindungsstaerken
festgelegt werden.

> %%

> Start : Source  {% returnP $1 }

> Source : Blocks  { snd $1 }
>        | Lines   { concatMap (\ x -> "\n") $1 }
>        |         { "" }


> Blocks : Block         { $1 }
>        | Block Blocks  { (0, snd $1 ++ snd $2) }
>        | Block Lines   { (0, snd $1) }

> Block : ADPBlock        { $1 }
>       | Lines ADPBlock  { (0, concatMap (\ x -> "\n") $1 ++ snd $2) }

--------------------------

> ADPBlock : Opening BlockLines Closing        { (0, let header = snd $1 
>                                              in header ++ processBlockLines header $2 ++ snd $3) }
>          | keyword SomeChars return          { (0, $1 ++ trd3 $2 ++ "\n") }
>          | verbatimcode                      { (0, $1 ) }

> Opening : hash fill SomeWhiteSpace SquareBracedRegion openbrace SomeWhiteSpace return  { (0, "#" ++ $2 ++ snd $3 ++ $4 ++ "{\n") }

> SquareBracedRegion :                                                                 { "" }
>                    | openSQbracket SomeCharsWithoutBR closeSQbracket SomeWhiteSpace  { "[" ++ $2 ++ "]" ++ snd $4 }

> Closing : closebrace SomeWhiteSpace return   { (0, "}\n") }

--------------------------

Zeilendefinitionen ausserhalb der Bloecke:

> Lines : SomeLine        { [$1] }
>       | SomeLine Lines  { $1 : $2 }

> SomeLine : LitLine      { trd3 $1 }
>          | CommentLine  { $1 }
>          | return       { "\n" }   -- empty line

> LitLine : greater return           { (-1, WSPline, ">\n") }
>         | greater SomeChars return { (if snd3 $2 == WSPline then -1 else fst3 $2, snd3 $2, ">" ++ trd3 $2 ++ "\n") }

> CommentLine : AnyCharButGreater return            { "\n" }
>             | AnyCharButGreater SomeChars return  { "\n" }

--------------------------

Zeilendefinitionen innerhalb der Bloecke. Der einzige Unterschied
liegt in der Behandlung von Kommentarzeilen, die keine geschweifte
Klammer enthalten duerfen.

> BlockLines : SomeBlockLine             { [$1] }
>            | SomeBlockLine BlockLines  { $1 : $2 }

> SomeBlockLine : LitLine           { $1 }
>               | BlockCommentLine  { $1 }
>               | return            { (-1, Empty, "\n") }   -- empty line

> BlockCommentLine : SomeBlockChars return { (-1, Empty, trd3 $1 ++ "\n") }

--------------------------

> SomeChars : AnyChar            { $1 }
>           | AnyChar SomeChars  { (if fst3 $1 == 0 then 0 else fst3 $1 + fst3 $2, lineType (snd3 $1) (snd3 $2), trd3 $1 ++ trd3 $2) }

> AnyChar : WhiteSpace        { (fst $1, WSPline, [snd $1]) }
>         | equal             { (0, EQline, "=") }
>         | doubledoubledot   { (0, DDline, "::") }
>         | where             { (0, WHline, "where") }
>         | fill              { (0, Empty, $1) }
>         | openSQbracket     { (0, Empty, "[") }
>         | closeSQbracket    { (0, Empty, "]") }
>         | openbrace         { (0, Empty, "{") }
>         | closebrace        { (0, Empty, "}") }
>         | greater           { (0, Empty, ">") }

die letzte Regel erzeugt einen shift/reduce conflict, weil durch
sie in der Regel "Lines : SomeLine {...}" dadurch eine gueltige
Zeile definiert werden kann, die nur eine schliessende geschreifte
Klammer enthaelt, dieses aber zugleich der Regel "Closing" entspricht,
die direkt im Anschluss an die Regel "Lines" in "ADPBlock" verwendung
findet.

--------------------------

> AnyCharButGreater : WhiteSpace        { (fst $1, WSPline, [snd $1]) }
>                   | equal             { (0, EQline, "=") }
>                   | doubledoubledot   { (0, DDline, "::") }
>                   | where             { (0, WHline, "where") }
>                   | fill              { (0, Empty, $1) }
>                   | openSQbracket     { (0, Empty, "[") }
>                   | closeSQbracket    { (0, Empty, "]") }
>                   | openbrace         { (0, Empty, "{") }
>                   | closebrace        { (0, Empty, "}") }

--------------------------

Diese Regeln werden definiert, damit innerhalb eines Blockes
keine Kommentarzeile eine geschweifte Klammer enthalten kann.

> SomeBlockChars : AnyBlockChar            { $1 }
>                | AnyBlockChar SomeChars  { (if fst3 $1 == 0 then 0 else fst3 $1 + fst3 $2, lineType (snd3 $1) (snd3 $2), trd3 $1 ++ trd3 $2) }

> AnyBlockChar : WhiteSpace        { (fst $1, WSPline, [snd $1]) }
>              | equal             { (0, EQline, "=") }
>              | doubledoubledot   { (0, DDline, "::") }
>              | where             { (0, WHline, "where") }
>              | fill              { (0, Empty, $1) }
>              | openSQbracket     { (0, Empty, "[") }
>              | closeSQbracket    { (0, Empty, "]") }

--------------------------

> SomeCharsWithoutBR : CharsWithoutBR                     { $1 }
>                    | CharsWithoutBR SomeCharsWithoutBR  { $1 ++ $2 }

> CharsWithoutBR : WhiteSpace        { [snd $1] }
>                | equal             { "=" }
>                | doubledoubledot   { "::" }
>                | where             { "where" }
>                | fill              { $1 }

--------------------------

> SomeWhiteSpace :                           { (0, "") }
>                | WhiteSpace SomeWhiteSpace { (fst $1 + fst $2, snd $1 : snd $2) }

> WhiteSpace : space { (1, ' ') }
>            | tab   { (3, '\t') }


=====================================================================

> {



> -- data ParseResult a = Ok { getResult :: a } | Failed (String, Int)
> data ParseResult a = Ok a
>                   | Failed (String, Int)
> type P a = String -> LineNumber -> ParseResult a


> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s li ->
>     case m s li of 
>         Ok a -> k a s li
>         Failed e -> Failed e

> returnP :: a -> P a
> returnP a = \s li -> Ok a

> failP :: (String,Int) -> P a
> failP err = \s li -> Failed err

> catchP :: P a -> ((String,Int) -> P a) -> P a
> catchP m k = \s li ->
>    case m s li of
>       Ok a -> Ok a
>       Failed e -> k e s li


> failed (Failed _) = True
> failed (Ok _    ) = False

> getFailed (Failed s) = s
> getOk     (Ok s)     = s


> type LineNumber = Int

Zunaechst der Tokentyp, bei dem nur zwischen verschiedenen Arten
von Whitespace und den dazwischen befindenden Buchstaben unterschieden
wird.


> data Token = Space
>            | Tab
>            | Return
>            | OpenBrace
>            | CloseBrace
>            | OpenSquareBracket
>            | CloseSquareBracket
>            | Hash
>            | Equal
>            | Greater
>            | DoubleDoubleDot
>            | Where
>            | Fill String
>            | Keyword String
>            | VerbCode String  -- verbatim code; wird nicht vorverarbeitet
>            | EOF
>     deriving (Show, Eq)


> scanner :: String -> (LineInc, Token, String)
> scanner []                  = (0, EOF, "")
> scanner [c]                 | c == '\n' || c == '\r'  = (1, Return, "")
>                             | otherwise               = scanner (c:"\n")
> scanner ('-':'-':cs)        | take 8 cs == "endwhere" = (0, Fill ";\n--endwhere", drop 8 cs)
>                             | otherwise = let (comment, rest) = span (/= '\n') cs
>                                           in incLine $ scanner rest
> scanner ('i':'n':'f':'i':'x':cs) = let (_, rest) = span (/= '\n') cs
>                                    in incLine $ scanner rest
> scanner ('\n':cs)           = (1, Return, cs)
> scanner ('\r':cs)           = (1, Return, cs)
> scanner (' ':cs)            = (0, Space, cs)
> scanner ('\t':cs)           = (0, Tab, cs)
> scanner xs@('#':cs)         | take 4  cs == "main"        = getAreaToken xs True "#main{" "#}"
>                             | take 6  cs == "target"      = getAreaToken xs True "#target{" "#}"
>                             | take 8  cs == "userdefs"    = (0, Hash, cs)
>                             | take 11 cs == "algebratype" = (0, Hash, cs)
>                             | take 7  cs == "grammar"     = (0, Hash, cs)
>                             | take 7  cs == "algebra"     = (0, Hash, cs)
>                             | take 7  cs == "typesyn"     = (0, Keyword "#typesyn", drop 7 cs)
>                             | take 6  cs == "extern"      = (0, Keyword "#extern", drop 6 cs)
>                             | take 6  cs == "filter"      = (0, Keyword "#filter", drop 6 cs)
>                             | take 7  cs == "Grammar"     = (0, Keyword "#Grammar", drop 7 cs)
>                             | take 4  cs == "line"        = (0, Keyword "#line", drop 4 cs)
>                             | otherwise                   = (0, Fill "#", cs)
> scanner ('{':cs)            = (0, OpenBrace, cs)
> scanner ('}':cs)            = (0, CloseBrace, cs)
> scanner ('=':'=':cs)        = (0, Fill "==", cs)
> scanner ('<':'=':cs)        = (0, Fill "<=", cs)
> scanner ('>':'=':cs)        = (0, Fill ">=", cs)
> scanner ('/':'=':cs)        = (0, Fill "/=", cs)
> scanner ('>':cs)            = (0, Greater, cs)
> scanner ('[':cs)            = (0, OpenSquareBracket, cs)
> scanner (']':cs)            = (0, CloseSquareBracket, cs)
> scanner ('=':cs)            = (0, Equal, cs)
> scanner cs@(':':':':':':_)  = let (dds, rest) = span (== ':') cs
>                               in (0, Fill dds, rest)
> scanner (':':':':cs)        = (0, DoubleDoubleDot, cs)
> scanner (':':cs)            = (0, Fill ":", cs)
> scanner cs                  = let (fill, rest) = span (\ c -> not $ elem c " \t\n\r={}[]#:") cs
>                                   scanRest = scanner rest
>                               in case fill of
>                                       "where"   -> (0, Where, rest)
>                                       otherwise -> (0, Fill fill, rest)


> length' :: [a] -> Integer
> length' [] = (0::Integer)
> length' (a:as) = (1::Integer) + length' as


> fst3 (a,_,_) = a
> snd3 (_,b,_) = b
> trd3 (_,_,c) = c

> getAreaToken :: String -> Bool -> String -> String -> (Int, Token, String)
> getAreaToken inp includeFrame begin end 
>   = let (l, s, rest) = getArea inp includeFrame begin end 
>     in  (l, VerbCode s, rest)

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

> type LineInc = Int

> incLine :: (LineInc, Token, String) -> (LineInc, Token, String)
> incLine (li, t, s) = (li+1, t, s)


> data LineType = EQline     -- Der Text bisher enthaelt ein Gleichheitszeichen
>               | WHline     -- der Text enthaelt bisher ein where
>               | DDline     -- wenn eine Zeile "::" enthaelt
>               | WSPline    -- wenn eine Zeile nur aus Whitespaces oder Kommentaren besteht
>               | Empty      -- die Zeile enthaelt keines der besonderen Zeichen
>     deriving (Show, Eq)

> lineType :: LineType -> LineType -> LineType
> lineType WHline _        = WHline
> lineType _ WHline        = WHline
> lineType EQline _        = EQline
> lineType _ EQline        = EQline
> lineType DDline _        = DDline
> lineType _ DDline        = DDline
> lineType WSPline WSPline = WSPline
> lineType _ _             = Empty


> processBlockLines :: String -> [(Integer, LineType, String)] -> String
> processBlockLines header = isSpecialBlock $ tail header
>     where isSpecialBlock header | take 8 header  == "userdefs"    = concatMap trd3
>                                 | take 11 header == "algebratype" = concatMap killLit
>                                 | otherwise                       = (concatMap killLit) . addSemic
>           killLit (_,_,l@('>':cs)) = cs
>           killLit (_,_,l)          = "\n"
>           addSemic []                           = []
>           addSemic (l@(ind, lineType, line):ls) = let rest = addSemic ls
>                                                       succLT = succLineType rest
>                                                   in if ind == -1 then (ind, succLT, "\n") : rest  -- Kommentarzeile rausschmeissen
>                                                                   else if (lineType == DDline) ||
>                                                                           (succLT == EQline && (lineType /= WHline && lineType /= DDline && lineType /= WSPline))
>                                                                                 then (ind, lineType, replaceLast line ";\n") : rest
>                                                                                 else l : rest
>           succLineType []              = EQline
>           succLineType ((_, lt, _):ls) = lt
>           replaceLast :: String -> String -> String
>           replaceLast [] _           = ""
>           replaceLast [c] rplmnt     = rplmnt
>           replaceLast (c:cs) rplmnt  = c : replaceLast cs rplmnt


=====================================================================

Dieser Teil definiert die Monade fuer den monadischen Parser

> lexerP :: (Token -> P a) -> P a
> lexerP cont s line = let (newl, token, s') = scanner s in cont token s' (line+newl)


=====================================================================

Die Fehlerbehandlungsroutine:

> -- happyError :: [Token] -> a
> -- happyError tokens = error $ "Parse Error. Following Tokens are " ++ (show tokens)

> getLineNo :: P LineNumber
> getLineNo = \ s l -> Ok l

> -- happyError :: P a
> happyError = getLineNo `thenP` (\line -> failP ("preproc: parse error: line " ++ show line, line))

> preprocparser :: String -> String
> preprocparser inp = case preprocess inp 1 of
>                       Ok res -> res
>                       Failed err -> error "preproc error"

> }
