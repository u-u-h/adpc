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



%include tex/lhs2TeX.fmt

%----------------------------------------------------------------------------------------------------
\subsection{lexikalische Analyse}
%----------------------------------------------------------------------------------------------------


where-Konstruktionen muessen jeweils mit --endwhere abgeschlossen
werden.

> {
> module Parse (parse, lexer, ppParse, extractArea, ADPProgram, ppADPProgram,
>               ParseResult(Ok, Failed), failed, getFailed, catchEr, rev_Parse) where
> import Char
> import Tools
> import StringLib
> import Syntax
> import MathExp
> import Expr
> import TLData
> import Lex
> import PrettyPrint

> }


%----------------------------------------------------------------------------------------------------
\subsection{syntaktische Analyse}
%----------------------------------------------------------------------------------------------------

> %monad { P } { thenP } { returnP }
> %lexer { lexerP } { TokenEOF }

> %name pGrammar Grammar
> %name pAlgebra Algebra
> %name pUserFunctions UserFunctions
> %name pSignature Signature
> %name pAlgebraTypeDecl AlgebraTypeDecl 

> %tokentype { Token }

> %token 
>       kusing          { TokenUsing }
>       knext           { TokenNext }
>       kalt            { TokenAlt } 
>       kwith           { TokenWith }
>       kchoice         { TokenChoice }
>       p               { TokenP }
>       tt              { TokenTT }
>       tabulated       { TokenTabulated }
>       listed          { TokenListed }
>       listedj         { TokenListedj }
>       nontabulated    { TokenNonTabulated }           
>       name            { TokenName $$ }
>       constr          { TokenConstructor $$ }
>	'='		{ TokenEq }
>	'('		{ TokenOB }
>	')'		{ TokenCB }
>	'['		{ TokenOB2 }
>	']'		{ TokenCB2 }
>	--'{'		{ TokenOB3 }
>	--'}'		{ TokenCB3 }
>       '$'             { TokenDollar }
>       ','             { TokenComma }
>       ':'             { TokenColon }
>       "++"            { TokenPlusPlus }
>       "->"            { TokenRightArrow }
>       "<-"            { TokenLeftArrow }
>       '.'             { TokenDot }
>       ".."            { TokenDotDot }
>       "&&"            { TokenAnd }
>       "||"            { TokenOr }
>       backslash       { TokenBackslash }
>       infixOpP1       { TokenInfixOpP1 $$ }
>       infixOpP2       { TokenInfixOpP2 $$ }
>       infixOpP3       { TokenInfixOpP3 $$ }
>       if              { TokenIf }
>       then            { TokenThen }
>       else            { TokenElse }
>       let             { TokenLet }
>       where           { TokenWhere }
>       endwhere        { TokenEndWhere }
>       newline         { TokenNewLine }
>       defTerminal     { TokenDefTerminal }
>       addIf           { TokenAddIf }
>       defFilter       { TokenDefFilter }
>       defLA           { TokenDefLA }
>       defLC           { TokenDefLC }
>       infinite        { TokenInfinite }

>       comb            { TokenComb $$ }
>       ttcomb          { TokenTTComb }
>       sttcomb         { TokenSTTComb }
>       ttscomb         { TokenTTSComb }
>       stscomb         { TokenSTSComb }
>       hhcomb          { TokenHHComb }
>       dbcomb          { TokenDBComb }

>       -- for signatures:
>       '|'             { TokenPipe }
>       data            { TokenData }
>       type            { TokenType }

}


Bindungsstärken und Assoziativitäten der Kombinatoren

 infix  5 ...
 infixr 6 ||| , |<|
 infixl 7 ~~~ , ~~+, +~~, ~~- , -~~, +~+
 infixl 7 ~~ , *~~, ~~*, *~*, ^^^
 infix  8 <<<
 -- infix 9 `with` 


choice, alt, next, using
(Bindungsstaerke wird nach unten groesser)

> %left '='
> %left let if then else

> %right "||"
> %right "&&"
> %nonassoc infixOpP3
> %right ':' "++"
> %left infixOpP2
> %left infixOpP1


> %nonassoc "<-" ".."
> %right '.'

 %left FArg
 %left FName
 %left FArgs
 %left FAppl

> %nonassoc kchoice
> %left kalt
> %nonassoc kusing
> %left knext comb
> %left kwith
> %nonassoc AlgAppl

> %%

> Grammar :: { [ProdSource] }
> Grammar : Prods { $1 }

> Prods :: { [ProdSource] }
> Prods : Prod Prods    { $1 : $2 }
>       | Prod          { [$1] }

> Prod :: { ProdSource }
> Prod : name '=' Unit                                     { ($1 :==== ("", Freetabulated, ST (True, True), $3, [])) }
>      | name '=' Unit where Prods endwhere                { ($1 :==== ("", Freetabulated, ST (True, True), $3, $5)) }
>      | name '=' tabulated Unit                           { ($1 :==== ("", Tabulated,     ST (True, True), $4, [])) }
>      | name '=' tabulated Unit where Prods endwhere      { ($1 :==== ("", Tabulated,     ST (True, True), $4, $6)) }
>      | name '=' listed Unit                              { ($1 :==== ("", Tabulated,     ST (True, False), $4, [])) }
>      | name '=' listed Unit where Prods endwhere         { ($1 :==== ("", Tabulated,     ST (True, False), $4, $6)) }
>      | name '=' listedj Unit                             { ($1 :==== ("", Tabulated,     ST (False, True), $4, [])) }
>      | name '=' listedj Unit where Prods endwhere        { ($1 :==== ("", Tabulated,     ST (False, True), $4, $6)) }
>      | name '=' nontabulated Unit                        { ($1 :==== ("", Nontabulated,  ST (True, True), $4, [])) }
>      | name '=' nontabulated Unit where Prods endwhere   { ($1 :==== ("", Nontabulated,  ST (True, True), $4, $6)) }

>      --- Index ranges:
>      | name '=' tabulated '(' Ranges ',' Unit ')'                       { ($1 :==== ("", Tabulated, $5, $7, [])) }
>      | name '=' tabulated '(' Ranges ',' Unit ')' where Prods endwhere  { ($1 :==== ("", Tabulated, $5, $7, $10)) }
>      -------------------

>      | '(' comb ')' '=' '(' ttcomb ')' '(' AInt ',' AInt ')' '(' AInt ',' AInt ')'   
>                               { CombDef $2 (CombYSize (ST($9, $11))       (ST($14, $16))) }
>      | '(' comb ')' '=' '(' sttcomb ')' AInt                 '(' AInt ',' AInt ')'   
>                               { CombDef $2 (CombYSize (ST($8, Infinite)) (ST($10,  $12)))  }
>      | '(' comb ')' '=' '(' ttscomb ')' '(' AInt ',' AInt ')' AInt                   
>                               { CombDef $2 (CombYSize (ST($9, $11))      (ST($13, Infinite))) }
>      | '(' comb ')' '=' '(' stscomb ')' AInt AInt                                    
>                               { CombDef $2 (CombYSize (ST($8, Infinite)) (ST($9, Infinite))) }

>      | '(' comb ')' '=' '(' hhcomb ')' name name                
>                               { CombDef $2 (CombLA $8 $9) }

>      | '(' comb ')' '=' '(' dbcomb ')' '(' '(' AInt ',' AInt ')' ',' '(' AInt ',' AInt ')' ')'   
>                                        '(' '(' AInt ',' AInt ')' ',' '(' AInt ',' AInt ')' ')'   
>                                        name 
>                               { CombDef $2 (CombDB (TT ($10, $12) ($16,$18)) (TT($23, $25) ($29,$31)) (strtoInt $34)) }

>      | name DirectDefArgs                           '=' ListCompr        { DirectDefSource $1 (ST $2) $4 }
>      | name '(' DirectDefArgs ',' DirectDefArgs ')' '=' ListCompr        { DirectDefSource $1 (TT $3 $5) $8 }

> DirectDefArgs :: { (String,String) }
> DirectDefArgs : '(' name ',' name ')'   {($2, $4)}

> ---------- Index ranges:
> Ranges :: { TrackMode (Bool, Bool) }
> Ranges : '(' name ',' name ')'                     { ST (strtoBool $2, strtoBool $4) }
>        | '(' name ',' name ',' name ',' name ')'   { TT (strtoBool $2, strtoBool $4)
>                                                         (strtoBool $6, strtoBool $8) }
> --------------------------

List Comprehension
-------------------

> ListCompr :: { ListCompr }
> ListCompr     : '[' Rhs '|' ListComprDefs ']'      { ($2, $4) }

> ListComprDefs :: { [LCExp] }
> ListComprDefs : ListComprDef                       { [$1] }
>               | ListComprDef ',' ListComprDefs     { $1 : $3 }
> ListComprDef :: { LCExp }
> ListComprDef  : Rhs                                { LCExp $1 }

--------------------------------------------------------------------------------

> Unit :: { Unit }
> Unit : p name                           { Terminal ($2,[]) }
>      | AlgAppl kusing Unit              { ($1 :<<< $3) }
>      | Unit kchoice name                { ($1 :... $3) }
>      | Unit knext  Unit                 { ($1 :~~~ $3) }
>      | Unit comb   Unit                 { ($1 :~~! ($2, $3)) }
>      | Unit kalt   Unit                 { ($1 :||| $3) }
>      | Unit kwith Filter                { ($1 `With` $3) }
>      | Symbol                           { Terminal $1 }       -- Terminale und Nichtterminale erst vollst. als Terminal
>      | '(' Unit ')'                     { $2 } 
>      | tt '(' Unit ',' Unit ')'         { TTUnit $3 $5 }
>      | backslash '(' name ',' name ')' "->" ListCompr
>                                         { ListCompr $8 (ST ($3,$5)) (ST (MathExp.Number 0, MathExp.Infinite))}

> -- pragmatic solution : { f args } 
> AlgAppl :: {String, [String] }
> AlgAppl : name                          { ($1, []) }
>         | '$' name AlgApplArgs '$'      { ($2, $3) }
> AlgApplArgs :: { [String] }
> AlgApplArgs : name                      { [$1] }
>             | name AlgApplArgs          { $1:$2 }

> Symbol :: { (String, [String]) }
> Symbol   : name                               { ($1, []) }
>          | '(' name TermArgs ')'              { ($2, $3) }

> TermArgs :: { [String] }
> TermArgs : name TermArgs                      { $1 : $2 }
>          | name                               { [$1] }

> Filter :: { (String, [String]) }
> Filter : name                     { ($1, []) }   
>        | '(' name FilterArgs ')'  { ($2, $3) }
> FilterArgs :: { [String] }
> FilterArgs : name FilterArgs      { $1 : $2 }
>            |                      { [] }

Algebra:
----------

> NewLines :: { [String] }
> NewLines : newline          { [] }
>          | newline NewLines { [] }
> OptNewLines :: { [String] }
> OptNewLines : {- empty -}   { [] }
>             | NewLines      { [] }

> Algebra :: { ([AlgTypeDef], [AlgDef]) }
> Algebra : OptNewLines AlgebraTypeDef NewLines AlgebraBody  { ($2, $4) }
>         | OptNewLines AlgebraBody                          { ([], $2) }

Algebra-Typedef:

> AlgebraTypeDef :: { [AlgTypeDef] }
> AlgebraTypeDef :  ATypeDef NewLines AOrderDef OptNewLines where { [($1, $3)] }

> -- Algebra-Typangabe hier auch mit Tupeln, Listen, etc.:
> ATypeDef :: { (String, String, [SigArgument]) }
> ATypeDef : name ':' ':' constr SigArguments    { ( $1, $4, $5 ) }

> AOrderDef :: { [String] }
> AOrderDef : name '=' OptNewLines '(' AOrderDefNames ')'       { $5 }
> AOrderDefNames :: { [String] }
> AOrderDefNames : name                                         { [$1] }
>                | name ',' OptNewLines AOrderDefNames          { $1 : $4 }


Algebrafunktionen:

> AlgebraBody :: { [AlgDef] }
> AlgebraBody : {- empty -}                    { [] }
>             | AlgFunction  AlgebraBody       { ( $1 : $2) }

> AlgFunction :: { AlgDef }
> AlgFunction : name Args '='  Rhs                   NewLines { ($1, [], $2, $4) }

             | name Args '=' '(' Rhs ',' TRhs ')'   NewLines { ($1, [], $2, ExpTupel ($5: $7)) }

Algebrafunktionsargumente:

> Args :: { [SigArgument] }
> Args : {- empty -}                   { [] } 
>      | Arg Args                      { $1 : $2 }

> Arg :: { SigArgument }
> Arg  : name                          { SigId $1 }
>      | '(' Arg ',' TArgs ')'         { SigTupel ($2: $4) }

> TArgs :: { [SigArgument] }
> TArgs : Arg                          { [$1] }
>       | Arg ',' TArgs                { $1 : $3 }

Tupeled rhs:

> TRhs :: { [Exp] }
> TRhs : Rhs                          { [$1] }
>      | Rhs ',' TRhs                 { $1 : $3 }

> Rhs :: { Exp }
> Rhs : Rhs infixOpP2 Rhs             { (ExpIOp $1 $2 $3) }
>     | Rhs infixOpP1 Rhs             { (ExpIOp $1 $2 $3) }
>     | Rhs infixOpP3 Rhs             { (ExpIOp $1 $2 $3) }
>     | Rhs "||"      Rhs             { (ExpIOp $1 "||" $3) }
>     | Rhs "&&"      Rhs             { (ExpIOp $1 "&&" $3) }
>     | Rhs ':'       Rhs             { (ExpCons   $1 $3) }
>     | Rhs "++"  Rhs                 { (ExpAppend $1 $3) }
>     | infixOpP2 Rhs                 { processSign $1 $2 }
>     | FAppl                         { functionToConstant $1 }
>     | '(' Rhs ')'                   { $2 }                   
>     | '(' Rhs ',' TRhs ')'          { ExpTupel ($2:$4) }
>     | '[' name name ']'             { (ExpChoice $2 (ExpVar $3)) }
>     | if Rhs then Rhs else Rhs      { (ExpIf $2 $4 $6) }
>     | Rhs '.' Rhs                   { (ExpDot $1 $3) }
>     | Rhs "<-" Rhs                  { (ExpIn $1 $3) }
>     | '[' Rhs ".." Rhs ']'          { (ExpEnum $2 $4) }
>     | let Rhs '=' Rhs               { (ExpLet $2 $4) }

> FAppl :: { Exp }
> FAppl : FName FArgs                 { ExpPOp $1 $2 }         

> FName :: { String }
> FName : name                        { $1 }
>       | p                           { "p" }
>       | constr                      { $1 }

> FArgs :: { [Exp] }
> FArgs : {- empty -}                 { [] }
>       | FArg FArgs                  { $1 : $2 }

> FArg :: { Exp }
> FArg : name                         { ExpVar $1 }
>      | p                            { ExpVar "p" }
>      | '(' Rhs ')'                  { $2 }
>      | '(' Rhs ',' TRhs ')'         { ExpTupel ($2:$4) }


Signature:

> Signature : {- empty -}                     { [] }
>           | TypeDefs SignatureDef TypeDefs  { [($2, $1 ++ $3)] }


> -- typedefs:
> TypeDefs : {- empty -}                 { [] }
>          | TypeDef TypeDefs            { $1 : $2 }
> TypeDef : type constr '=' SigArgument  { ($2, $4) }

> -- Signature
> SignatureDef : data constr '=' Operators   { ($2, $4) }

> Operators : Operator                       { [$1] }
>           | Operator '|' Operators         { $1 : $3 }

> Operator : constr SigArguments             { ($1, $2) }

> SigArguments : {- empty -}                 { [] }
>              | SigArgument SigArguments    { $1 : $2 }
> SigArgument : constr                       { SigId    $1 }
>             | name                         { SigId    $1 }
>             | SigTupel                     { SigTupel $1 }
>             | SigList                      { SigList  $1 }

> SigTupel : '(' SigArgument ',' SigTArgs ')'   { $2 : $4 }
> SigTArgs : SigArgument                        { [$1] }
>          | SigArgument ',' SigTArgs           { $1 : $3 }

> SigList : '[' SigArgument ']'                 { $2 }

Algebra type declaration:

> AlgebraTypeDecl : {- empty -}                                           { [] } 
>                 | type constr TypeArgs '=' '(' AlgebraTypeDeclFcts ')'  { [($2, $3, $6)] }

> TypeArgs : {- empty -}             { [] }
>          | name TypeArgs           { $1 : $2 }

> AlgebraTypeDeclFcts : AlgebraTypeDeclFct                                { [$1] }
>                     | AlgebraTypeDeclFct ',' AlgebraTypeDeclFcts        { $1 : $3 }

> AlgebraTypeDeclFct : AlgebraTypeDeclType                                { [$1] }
>                    | AlgebraTypeDeclType "->" AlgebraTypeDeclFct        { $1 : $3 }

> AlgebraTypeDeclType : SigArgument                                       { $1 }

Definition von eigenen Funktionen:

> UserFunctions : {- empty -}                { [] }
>               | UserFunction UserFunctions { ( $1 : $2) }

Single-Track:

> UserFunction : defTerminal name '=' '(' AInt ',' AInt ')' name       { (UFTerm  $2 $9 False, ST ($5, $7)) } 
>              | defTerminal name '=' '(' AInt ',' AInt ')' name addIf { (UFTerm  $2 $9 True,  ST ($5, $7)) } 
>              | defFilter   name '=' '(' AInt ',' AInt ')'            { (UFFilter $2,         ST ($5, $7)) }
>              | defLA       name '=' '(' AInt ',' AInt ')'            { (UFLA     $2,         ST ($5, $7)) }
>              | defLC       name '=' '(' AInt ',' AInt ')'            { (UFLC     $2,         ST ($5, $7)) }

Two-Track:

>              | defFilter   name '=' '(' AInt ',' AInt ')' '(' AInt ',' AInt ')'  { (UFFilter $2, TT ($5, $7) ($10, $12)) }
>              | defLC       name '=' '(' AInt ',' AInt ')' '(' AInt ',' AInt ')'  { (UFLC     $2, TT ($5, $7) ($10, $12)) }

> AInt : infinite  { Infinite }
>      | name      { Number (strtoInt $1) }

> {


> rev_Parse =  "$Revision$"

----------------------------------------------------------------------

> data ParseResult a = Ok a | Failed (String, Int)
> type P a = String -> Int -> Int -> ParseResult a

> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s li ls ->
>     case m s li ls of 
>         Ok a -> k a s li ls 
>         Failed e -> Failed e

> returnP :: a -> P a
> returnP a = \s li ls -> Ok a

> failP :: (String,Int) -> P a
> failP err = \s li ls -> Failed err

> catchP :: P a -> ((String,Int) -> P a) -> P a
> catchP m k = \s li ls ->
>    case m s li ls of
>       Ok a -> Ok a
>       Failed e -> k e s li ls

> lexerP :: (Token -> P a) -> P a
> lexerP cont s line ls = let (newl, token, s') = lexer ls s in cont token s' (line+newl) ls 

----------------------------------------------------------------------


Online parser functions:
--------------------------
processSign: baut Vorzeichen in ExpVar ein, oder erzeugt ein -1 * rhs

> processSign "+" rhs = rhs
> processSign "-" (ExpVar value) = ExpVar ("-" ++ value)
> processSign "-" rhs            = ExpIOp (ExpVar "-1") "*" rhs
> processSign x _                = pattErr "processSign" x

functionToConstant: wandelt nullstellige Funktionen in Konstanten (ExpVar) um

> functionToConstant (ExpPOp name@(n:ns) args) | isUpper n  = ExpConstr name (map functionToConstant args)
>                                              | args == [] = nameToExpr name
>                                              | otherwise  = ExpPOp    name (map functionToConstant args)
> functionToConstant (ExpIOp a o b) = ExpIOp (functionToConstant a) o (functionToConstant b)
> functionToConstant x = x

nameToExpr: wandelt Strings in Expressions vom Typ ExpVar, ExpConstr, ExpChar oder ExpString um

> nameToExpr n@(s:ss) | s == cqu  = ExpChar   $ head ss 
>                     | s == squ  = ExpString $ init ss
>                     | isUpper s = ExpConstr n []
>                     | otherwise = ExpVar n
>  where
>    cqu = chr 39
>    squ = chr 34

> strtoBool :: String -> Bool
> strtoBool s = strtoInt s == 1

--------------------------------------------------------------------------------------------------------------

> happyError :: P a
> happyError s li ls = failP (errmsg,li) s li ls   where
>                errmsg = 
>                 "\n\nParse error in line " ++ show li ++ "!\n" 

>               {-  ++  "----------------------------------\n" ++
>                 "Unfortunately, this systems lacks for an informative error message.\n" ++
>                 "But to give you some hints, common issues are:\n" ++
>                 " - terminal parsers and filter functions with arguments must be put in \n" ++
>                 "   parentheses, e.g. ... ~~~ ((char 'c') ~~~ astring) `with` (minsize 8) ...\n" ++
>                 " - combinators must be enclosed in spaces, e.g. ~~~ astring~~~ astring ~~~\n" ++
>                 "                                                           ^-- wrong!\n" ++
>                 " - missing --endwhere \n\n" -}
>                  

> data SepFct = UFTerm String String Bool | UFFilter String | UFLA String | UFLC String deriving (Show)

> getArea inp begin end = (start, unlines inp') where
>                             (_, start, (_, inp', _)) = extractArea (lines inp) begin end

> lexUserFunctions   inp = getArea inp "userdefs{" "}"
> extractTargetCode  inp = getArea inp "target{" "#}"
> extractMainCode    inp = getArea inp "main{" "#}"
> extractMainAddCode inp = getArea inp "mainadd{" "#}"
> extractOutputCode  inp = getArea inp "output{" "#}"

> lexGrammar inp | err == [] = Ok (axiom, start, inp')
>                | otherwise = Failed (err,-1)                 where

>                      (axiom, start, area) = extractArea (lines inp) "grammar{" "}"
>                      inp' = unlines(snd3 area)

>                      err | inp'  == [] = ("\nno productions found!\n"++
>                                             "try to denote your grammar with \ngrammar[axiom]{\n.....\n}\n")
>                          | axiom == "" = ("\nno axiom found!\n" ++ 
>                                             "try to denote your grammar with \ngrammar[axiom]{\n.....\n}\n")
>                          | otherwise = []


> lexAlgebra inp = map unlineContent algebras 
>   where
>     algebras = extractAreas (lines inp) "algebra{" "}"
>     unlineContent (name, start, content) = (name, start, unlines content)

> lexSignature       inp = getArea inp "signature{" "}"
> lexAlgebraTypeDecl inp = getArea inp "algebratype{" "}"

> sepFct ((UFTerm   s dt addif, ST ys):dfs) = ((s, dt, addif, ys):ts, fs, las, lcs) where (ts, fs, las, lcs) = sepFct dfs
> sepFct ((UFFilter s,    ys):dfs)          = (ts, (s, ys):fs, las, lcs)            where (ts, fs, las, lcs) = sepFct dfs
> sepFct ((UFLA     s, ST ys):dfs)          = (ts, fs, (s, ys):las, lcs)            where (ts, fs, las, lcs) = sepFct dfs
> sepFct ((UFLC     s,    ys):dfs)          = (ts, fs,         las, (s, ys):lcs)    where (ts, fs, las, lcs) = sepFct dfs
> sepFct []                                 = ([], [], [], [])

> failed (Failed _) = True
> failed (Ok _    ) = False

> getFailed (Failed s) = s
> getOk     (Ok s)     = s

> type ADPProgram = (UserFunctions,                     -- user functions 
>                    (String, String, String, String),  -- user code
>                    (String, [ProdSource]),            -- (axiom, productions)
>                    [AlgDefs],                         -- algebras
>                    [SignatureArea] ,                  -- signature
>                    [AlgebraTypeDecl])                   -- algebratype

> ppADPProgram (a,b,c,d,e,f) = 
>       "(\n" ++ 
>       show a ++ ",\n\n" ++ 
>       show b ++ ",\n\n" ++ 
>       show c ++ ",\n\n" ++ 
>       show d ++ ",\n\n" ++ 
>       show e ++ ",\n\n" ++ 
>       show f ++ ",\n\n" ++ 
>       ")\n"

> parse :: String -> ParseResult ADPProgram
> parse inp  -- check errors ------------
>            | failed userfcts      = Failed (getFailed userfcts)
>            | failed lGrammar      = Failed (getFailed lGrammar)
>            | failed grammar       = Failed (getFailed grammar)
>            | any failed algebras  = Failed (getFailed (head (filter failed algebras)))
>            | failed signature     = Failed (getFailed signature)
>            | failed algebraType   = Failed (getFailed algebraType)
>            ----------------------------
>            | otherwise       = Ok (userfcts'', (targetcode,maincode,mainaddcode,outputcode), 
>                                                (axiom, grammarE), algebrasE, signatureE, algebraTypeE) where

>                (startU, u)        = lexUserFunctions inp
>                userfcts           = pUserFunctions u startU tlexUF 
>                (Ok userfctsE)     = userfcts
>                userfcts''         = sepFct userfctsE

>                (_, targetcode)    = extractTargetCode inp
>                (_, maincode)      = extractMainCode inp
>                (_, mainaddcode)   = extractMainAddCode inp
>                (_, outputcode)    = extractOutputCode inp

>                lGrammar                = lexGrammar inp
>                (Ok (axiom, startG,g))  = lGrammar
>                grammar                 = pGrammar g startG tlexGrammar
>                (Ok grammarE)           = grammar 

>                lalgebras                    = lexAlgebra inp
>                algebras                     = map pAlg lalgebras
>                   where  
>                   pAlg (nameA, startA, alg) = pAlgebra alg (startA-1) tlexAlgebra
>                algebrasE'                   = map attachName (map getOk algebras)
>                    where
>                      attachName ([],_) = error $ "no algebra type given"
>                      attachName alg@((((name,_,_),_):_), _) = (name, alg)
>                algebrasE          = case algebrasE' of
>                                      []        -> []
>                                      otherwise -> init algebrasE'

>                (startS, s)        = lexSignature inp
>                signature          = pSignature s startS tlexSignature
>                (Ok signatureE)    = signature

>                (startAT, at)      = lexAlgebraTypeDecl inp
>                algebraType        = pAlgebraTypeDecl at startAT tlexAlgebraTypeDecl
>                (Ok algebraTypeE)  = algebraType


> catchEr (Ok a) = a
> catchEr (Failed (s,line)) = error ("this is a monadic error message:\nline: " ++ show line ++ "\n" ++ s)


> extractAreas :: [String] -> String -> String -> [(String, Int, [String])]
> extractAreas ls s epat = extractAreas' 0 ls s epat
> extractAreas' line ls s epat = res
>   where
>     (pa, start, (_, content, rest)) = extractArea ls s epat
>     res = case content of
>             []        -> [(pa, start+line+1, content)]
>             otherwise -> (pa, start+line+1, content):extractAreas' (line+start+length content) rest s epat

> extractArea :: [String] -> String -> String -> (String, Int, ([String],[String],[String]))
> extractArea ls s epat = (pa, start, (a, b, c))
>   where

     b' = case b of
            []        -> []
            otherwise -> ['\n']:b

>     start = (length a) + 1 

>     (pa,(a,b,c)) = extractArea' False ls s epat

>     extractArea' _     [] _     _    = ("", ([],[],[]))
>     extractArea' False (l:ls) s epat = (pa++pa'', (l:a, b, c))
>         where 
>           (pa, (a,b,c))  = extractArea' (l'==s) ls s epat
>           (l', pa')      = rmPar l
>           pa''           = if (l'==s) then pa' else ""

>     extractArea' True (l:ls) s epat = if l == epat then ("",([],[],(l:ls))) else ("", (a, l:b, c)) 
>        where 
>           (_,(a,b,c)) = extractArea' True ls s epat

> rmPar s = (b ++ tail' e, tail' p)
>        where
>           (b,b') = span ((/=) '[') s
>           (p,e)  = span ((/=) ']') b'
>           tail' [] = []
>           tail' x  = tail x


Testfunktionen
---------------

> testea f = do
>             inp <- readFile f
>             (pa, start, (a, b, c))  <- return (extractArea (lines inp) "grammar{" "}")
>             putStrLn ("parameter: " ++ pa ++ "\n")
>             putStrLn ("start: " ++ show start ++ "\n")
>             putStrLn "------- a: --------"
>             putStrLn (unlines a)
>             putStrLn "------- b: --------"
>             putStrLn (unlines b)
>             putStrLn "------- c: --------"
>             putStrLn (unlines c)

> testeas f = do
>             inp <- readFile f
>             areas  <- return (extractAreas (lines inp) "grammar{" "}")
>             sequence_ (map ppArea areas)
>   where
>     ppArea (pa,start,content) = do
>             putStrLn ("parameter: " ++ pa ++ "\n")
>             putStrLn ("start: " ++ show start ++ "\n")
>             putStrLn "------- content: --------"
>             putStrLn (unlines content)

> parseFile p f = do 
>                  inp <- readFile f
>                  putStrLn ((show . p) inp)


> ppParse f = do
>             inp <- readFile f
>             ((ufTerms, ufFilter, ufLA, ufLC), (targetcode,maincode,mainaddcode, outputcode), 
>                                               (axiom,ps), afs, sign, algTD)  <- return (catchEr (parse inp)) 
>             putStrLn "\nUser functions:\n-----------------"
>             putStrLn "Terminals:\n"
>             putStrLn (unlines (map ppUfsT ufTerms))
>             putStrLn "Filter:\n"
>             putStrLn (unlines (map ppUfsF ufFilter))
>             putStrLn "Lookahead functions:\n"
>             putStrLn (unlines (map ppUfsLA ufLA))
>             putStrLn "Directdefs:\n"
>             putStrLn (unlines (map ppUfsF ufLC))
>             putStrLn ("axiom = " ++ axiom ++ "\n")
>             putStrLn "Productions:\n----------------"
>             putStrLn $ pretty ps
>             putStrLn "Algebra functions:\n---------------------"
>             sequence_ (map ppAfs afs)
>             putStrLn "Signature:"
>             putStrLn (concatMap ppSignatureArea sign)
>             putStrLn "Algebra type declaration:"
>             putStrLn (concatMap ppAlgebraTypeDecl algTD)

>    where
>      ppAfs (name, (typedef,content)) = do
>             putStrLn $ "name: " ++ name
>             putStrLn "------------------------"
>             sequence_ (map ppTD typedef)
>             putStrLn (unlines (map ppAlgDef' content))

>      ppAlgDef' x = ppAlgDef x ++ "\n" ++ show x

>      ppTD ((name, constr, args), order) = do
>            putStrLn (name ++ " :: " ++ constr ++ " " ++ mapsep " " ppSigArgument args ++ "\n")
>            putStrLn (name ++ " =\n   " ++ "(" ++ sepList ", " order ++ ")\n   where\n")

> ppUfsF  (f, ys)         = f ++ " -> " ++ ppYSize ys
> ppUfsLA (f, ys)         = f ++ " -> " ++ ppSYSize ys
> ppUfsT (f, dt, addif, ys) = f ++ " -> " ++ ppSYSize ys ++ " " ++ dt ++ " " ++ if addif then " addif" else ""



> }
