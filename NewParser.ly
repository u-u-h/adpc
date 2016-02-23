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

> module NewParser (

>    parse,
>    ParseResult (..),

> ) where

> import Data.Char
> import Lexer
> import ParseMonad
> import qualified ParseTree
> import Lift

> }


> %monad { P } { thenP } { returnP }
> %lexer { lexerP } { EOF }

> %name parse1
> %tokentype { Token }


> %token
>     num           { Num $$ }
>     char          { Char $$ }
>     string        { String $$ }
>     ident         { Ident $$ }
>     underscore    { Underscore }
>     typename      { TypeIdent $$ }
>     algtype       { AlgebraTypeToken }
>     algebra       { AlgebraToken }
>     grammar       { GrammarToken }
>     typesyn       { TypeSynToken }
>     extern        { ExternToken $$ }

>     parser        { TParser } 
>     t_int         { TInt }
>     t_integer     { TInteger } 
>     t_float       { TFloat }
>     t_double      { TDouble }
>     t_char        { TChar }
>     t_string      { TString }
>     t_bool        { TBool }
>     t_void        { TVoid }
>     tc_num        { TCNum }
>     tc_integral   { TCIntegral }
>     tc_floating   { TCFloating }

>     where         { Where }
>     if            { If }
>     then          { Then }
>     else          { Else }
>     type          { Type }
>     let           { Let }
>     with          { With }
>     axiom         { Axiom }
>     tabulated     { Tabulated }
>     listed        { Listed }
>     listedj       { Listedj }
>     nontabulated  { Nontabulated }
>     ';'           { Semicolon }
>     '='           { Assign }
>     ','           { Comma }
>     '->'          { RArrow }
>     '=>'          { RArrow2 }
>     '<-'          { LArrow }
>     '|'           { Pipe }
>     '..'          { DotDot }
>     backslash     { Backslash }
>     '::'          { DoubleColon }
>     '('           { OpenBr }
>     ')'           { CloseBr }
>     '{'           { OpenCurlBr }
>     '}'           { CloseCurlBr }
>     '['           { OpenSqBr }
>     ']'           { CloseSqBr }
>     '+'           { Add }
>     '-'           { Sub }
>     '*'           { Mult }
>     '/'           { Div }
>     '^'           { Exp }
>     '&&'          { And }
>     '||'          { Or }
>     '++'          { Append }
>     '>'           { Lexer.GT }
>     '>='          { Lexer.GE }
>     '<'           { Lexer.LT }
>     '<='          { Lexer.LE }
>     '=='          { Lexer.EQ }
>     '/='          { Lexer.NE }
>     '<<<'         { ADPUses }
>     '~~~'         { ADPNext $$ }
>     '|||'         { ADPOr }
>     '...'         { ADPSelect }
>     '><'          { ADPTT }
>     '~~'          { ADPNextTT }
>     '*~~'         { ADPNextSTT }
>     '~~*'         { ADPNextTTS }
>     '*~*'         { ADPNextSTS }
>     '^^^'         { ADPNextHHH }


Assoziativität und Bindungsstärke festlegen

 %nonassoc ',' '<-'
 %nonassoc '..'  '|' 

> %right if then else

> %nonassoc '>' '>=' '<=' '<' '==' '/=' 

> %right '++' '->'
> %left '&&' '||' 
> %nonassoc '...'
> %left '+' '-'
> %left '|||'
> %nonassoc '<<<'
> %left '*' '/' '~~~' 

> %left '^'
> %right NEG
> %left with
> %nonassoc '><'

> %%


> Program : '{' ADPDefBlocks '}' {% assignBlocks $2 }

> lineno :: { ParseTree.LineNumber }
>         : {- empty -}       {% getLineNo }

> ADPDefBlocks : ADPDefBlock                   { [$1] }
>              | ADPDefBlock ';' ADPDefBlocks  { $1 : $3 }

> ADPDefBlock : BAlgebraTypeDef   { AlgebraTypeDef $1 }
>             | BAlgebraDef       { AlgebraDef $1 }
>             | BGrammarDef       { GrammarDef $1 }
>             | BTypeSynDef       { TypeSynDef $1 }
>             | BExternDef        { ExternDef $1 }
>     ---        | BGrammar          { Grammar $1 }

=====================================================================

> BTypeSynDef :: {ParseTree.TypeSyn }
> BTypeSynDef : lineno typesyn typename '=' TypeDef   { ParseTree.TypeSyn $1 $3 $5 }

> BExternDef :: {ParseTree.TypeDecl }
> BExternDef : lineno extern '::' TypeDef   { ParseTree.TypeDecl $1 $2 $4 }

> BAlgebraTypeDef : lineno algtype '{' type typename TypeParamList '=' TypeDef '}'   { ParseTree.AlgebraType $1 $5 $6 $8 }

> TypeParamList : ident                { [$1] }
>               | ident TypeParamList  { $1 : $2 }

======================================================================
Typedef allgemein
======================================================================

> AtomType :: {ParseTree.AtomType}
> AtomType : t_int           { ParseTree.TInt }
>          | t_integer       { ParseTree.TInteger } 
>          | t_float         { ParseTree.TFloat }
>          | t_double        { ParseTree.TDouble }
>          | t_char          { ParseTree.TChar }
>          | t_string        { ParseTree.TString }
>          | t_bool          { ParseTree.TBool }
>          | t_void          { ParseTree.TVoid }
>          | typename        { ParseTree.TTypeSyn $1 }
>          | ident           { ParseTree.TTypeVar $1 }

> TypeClass :: { ParseTree.TypeClass }
> TypeClass : tc_num      ident    { ParseTree.TCNum      $2 }
>           | tc_integral ident    { ParseTree.TCIntegral $2 }
>           | tc_floating ident    { ParseTree.TCFloating $2 }
         

> TypeDef :: { ParseTree.TypeDef }
> TypeDef : AtomType                          { ParseTree.SingleType $1 }
>         | parser TypeDef %prec NEG          { ParseTree.TParser $2 }
>         | TypeDef '->' TypeDef              { ParseTree.FunType $1 $3 }
>         | '[' TypeDef ']'                   { ParseTree.ListType $2 }
>         | '(' TypeDef ',' TupelTypeDefs ')' { ParseTree.TypeTuple ($2:$4) }
>         | '(' TypeDef ')'                   { $2 }
>         | TypeClass '=>' TypeDef            { ParseTree.Constraint $1 $3 }

> TupelTypeDefs :: { [ParseTree.TypeDef] }
> TupelTypeDefs : TypeDef                     { [$1]  }
>               | TypeDef ',' TupelTypeDefs   { $1:$3 }

======================================================================
Typedef fuer die Algebratypangabe (ohne idents)
======================================================================

> AtomType2 :: {ParseTree.AtomType}
> AtomType2 : t_int           { ParseTree.TInt }
>          | t_integer       { ParseTree.TInteger } 
>          | t_float         { ParseTree.TFloat }
>          | t_double        { ParseTree.TDouble }
>          | t_char          { ParseTree.TChar }
>          | t_string        { ParseTree.TString }
>          | t_bool          { ParseTree.TBool }
>          | t_void          { ParseTree.TVoid }
>          | typename        { ParseTree.TTypeSyn $1 }

> TypeDef2 :: { ParseTree.TypeDef }
> TypeDef2 : AtomType2                          { ParseTree.SingleType $1 }
>         | parser TypeDef2 %prec NEG           { ParseTree.TParser $2 }
>         | TypeDef2 '->' TypeDef2              { ParseTree.FunType $1 $3 }
>         | '[' TypeDef2 ']'                    { ParseTree.ListType $2 }
>         | '(' TypeDef2 ',' TupelTypeDefs2 ')' { ParseTree.TypeTuple ($2:$4) }
>         | '(' TypeDef2 ')'                    { $2 }
>         | TypeClass '=>' TypeDef              { ParseTree.Constraint $1 $3 }

> TupelTypeDefs2 :: { [ParseTree.TypeDef] }
> TupelTypeDefs2 : TypeDef2                     { [$1]  }
>                | TypeDef2 ',' TupelTypeDefs2   { $1:$3 }


=====================================================================

> BAlgebraDef : lineno algebra '[' ident ']' '{' AlgebraType AlgebraFunDef '}' { ParseTree.Algebra $1 $7 $8 }

> AlgebraType : lineno ident '::' typename TypeDefs { ParseTree.AlgebraFunType $1 $2 $4 $5 }

> TypeDefs :: { [ParseTree.TypeDef] }
> TypeDefs : TypeDef2           { [$1] }
>          | TypeDef2 TypeDefs  { $1 : $2 } 

> AlgebraFunDef : lineno ident '=' AlgebraTuple where '{' FunDefs '}' { ParseTree.AlgebraFunDef $1 $2 $4 $7 }

> AlgebraTuple : '(' ident ',' AlgebraTupleIdents ')'  { $2 : $4 }

> AlgebraTupleIdents : ident                         { [$1] }
>                    | ident ',' AlgebraTupleIdents  { $1 : $3 }

=====================================================================

> --- BGrammar :: { (String, String, ParseTree.TypeDef, [String]) }
> --- BGrammar : grammar2 ident ident TypeDef AlgebraTuple ';'   { ($2,$3,$4,$5) }

> BGrammarDef :: { ParseTree.Grammar }
> BGrammarDef : lineno grammar '{' ident Dummyidents '=' axiom ident where '{' 
>                   AlgebraTuple '=' ident ';' GrammarFunDefs '}' '}' 
>               { ParseTree.Grammar $1 $4 $8 (ParseTree.TypeTuple [ParseTree.SingleType ParseTree.TInt, ParseTree.SingleType ParseTree.TInt]) $11 $15 }

> Dummyidents :: { [String] }
> Dummyidents :                   { [] }
>             | ident Dummyidents { [] }

> GrammarFunDefs :: { [ParseTree.GrammarFunDef] }
> GrammarFunDefs : GrammarFunDef                     { [$1] }
>                | GrammarFunDef ';' GrammarFunDefs  { $1 : $3 }

> GrammarFunDef :: { ParseTree.GrammarFunDef }
> GrammarFunDef 
>   : lineno ident Params '=' Production                      WherePr    { ParseTree.GrammarFunDef $1 $2 $3 ParseTree.TTFree $5 $6 }
>   | lineno ident Params '=' tabulated '(' Production ')'    WherePr    { ParseTree.GrammarFunDef $1 $2 $3 (ParseTree.TTTabulated (True, True))  $7 $9 }
>   | lineno ident Params '=' listed  '(' Production ')'      WherePr    { ParseTree.GrammarFunDef $1 $2 $3 (ParseTree.TTTabulated (True, False)) $7 $9 }
>   | lineno ident Params '=' listedj '(' Production ')'      WherePr    { ParseTree.GrammarFunDef $1 $2 $3 (ParseTree.TTTabulated (False, True)) $7 $9 }
>   | lineno ident Params '=' nontabulated '(' Production ')' WherePr    { ParseTree.GrammarFunDef $1 $2 $3 ParseTree.TTNontabulated $7 $9 }
>   | lineno '(' '~~~' ')' '=' '(' '~~' ')' '(' num ',' num ')'  '(' num ',' num ')' 
>      { ParseTree.CombinatorDef $1 $3 (ParseTree.CombinatorTT (d2i $10, d2i $12) (d2i $15, d2i $17)) }
>   | lineno '(' '~~~' ')' '=' '(' '*~~' ')' num                 '(' num ',' num ')' 
>      { ParseTree.CombinatorDef $1 $3 (ParseTree.CombinatorSTT (d2i $9) (d2i $11, d2i $13)) }
>   | lineno '(' '~~~' ')' '=' '(' '~~*' ')' '(' num ',' num ')' num                 
>      { ParseTree.CombinatorDef $1 $3 (ParseTree.CombinatorTTS (d2i $10, d2i $12) (d2i $14)) }
>   | lineno '(' '~~~' ')' '=' '(' '*~*' ')' num                 num                 
>      { ParseTree.CombinatorDef $1 $3 (ParseTree.CombinatorSTS (d2i $9) (d2i $10)) }
>   | lineno '(' '~~~' ')' '=' '(' '^^^' ')' ident               ident               
>     { ParseTree.CombinatorDef $1 $3 (ParseTree.CombinatorHHH $9 $10) }


> WherePr :: { [ParseTree.GrammarFunDef] }
> WherePr : {- empty -}                    { [] }
>         | where '{' GrammarFunDefs '}'   { $3 }

> Production :: { ParseTree.Production }
> Production :  Production '...' lineno ident            { ParseTree.CombinatorAp $3 $1 "..." (ParseTree.ProdExpr $3 (ParseTree.Ident $4)) }
>            |  Production '~~~' lineno Production       { ParseTree.CombinatorAp $3 $1 $2 $4 }
>            |  FunApExpression '<<<' lineno Production  { ParseTree.CombinatorAp $3 (ParseTree.ProdExpr $3 $1) "<<<" $4 }
>            |  Production '|||' lineno Production       { ParseTree.CombinatorAp $3 $1 "|||" $4 }
>            |  Production with lineno FilterExpression  { ParseTree.CombinatorAp $3 $1 "with" (ParseTree.ProdExpr $3 $4) }
>            |  Production '><' lineno Production        { ParseTree.CombinatorAp $3 $1 "><" $4 }
>            |  FunApExpression  lineno                 { ParseTree.ProdExpr $2 $1 }
>            | '(' Production ')'                       { $2 }
>            | backslash lineno Param '->' '['  Expression '|' LCExpressions ']' 
>                                                { ParseTree.ListComprehension $2 $3 $6 $8 }


> LCExpressions :: { [ParseTree.LCExpression] }
> LCExpressions : LCExpression                             { [$1] }
>               | LCExpression ',' OptBrace LCExpressions  { $1 : $4 }

> OptBrace : {- empty -}   { [] }
>          | '}'           { [] }

> LCExpression :: { ParseTree.LCExpression }
> LCExpression : lineno Expression                       { ParseTree.LCFilter $1 $2 }
>              | lineno let '{' Param '=' Expression     { ParseTree.LCLet $1 $4 $6 }  
>              -- (Das Semikolon vor let ist nur wegen der aktuellen Vorverarbeitung notwendig,
>              --  mit vollstaendiger Abseitsregel kann dieses verschwinden) 
>              | lineno Expression '<-' Expression       { ParseTree.LCGenerator $1 $2 $4 }

> FilterExpression :: { ParseTree.Expression }
> FilterExpression : FunApExpression          { $1 }
>                  | '(' FunApExpression ')'  { $2 }

=====================================================================

> FunDefs :: { [ParseTree.FunDef] }
> FunDefs : FunDef               { [$1] }
>         | FunDef ';' FunDefs   { $1 : $3 }

> FunDef :: { ParseTree.FunDef }
> FunDef : lineno ident Params '=' Expression         { ParseTree.FunDef $1 $2 $3 $5}
>        | lineno ident Params '=' '[' Expression ']' { ParseTree.FunDef $1 $2 $3 $6}

> Params :: { [ParseTree.Parameter] }
> Params :               { [] }
>        | Param Params  { $1 : $2 }

> Param :: { ParseTree.Parameter }
> Param : ident                          { ParseTree.Parameter $1 }
>       | underscore                     { ParseTree.Wildcard }
>       | '(' Param ',' TupleParams ')'  { ParseTree.TupleParam ($2 : $4) }


> TupleParams :: { [ParseTree.Parameter] }
> TupleParams : Param                  { [$1] }
>             | Param ',' TupleParams  { $1 : $3 }

> Expression :: { ParseTree.Expression }
> Expression : AtomarExpression                              { $1 }
>            | '-' AtomarExpression  %prec NEG               { NewParser.negate $2 }
>            | if Expression then Expression else Expression { ParseTree.If $2 $4 $6 }
>            | Expression '+' Expression                     { ParseTree.BinOp $1 ParseTree.Add $3 }
>            | Expression '-' Expression                     { ParseTree.BinOp $1 ParseTree.Sub $3 }
>            | Expression '*' Expression                     { ParseTree.BinOp $1 ParseTree.Mul $3 }
>            | Expression '/' Expression                     { ParseTree.BinOp $1 ParseTree.Div $3 }
>            | Expression '^' Expression                     { ParseTree.BinOp $1 ParseTree.Exp $3 }
>            | Expression '<' Expression                     { ParseTree.BinOp $1 ParseTree.LT $3 }
>            | Expression '<=' Expression                    { ParseTree.BinOp $1 ParseTree.LE $3 }
>            | Expression '>' Expression                     { ParseTree.BinOp $1 ParseTree.GT $3 }
>            | Expression '>=' Expression                    { ParseTree.BinOp $1 ParseTree.GE $3 }
>            | Expression '==' Expression                    { ParseTree.BinOp $1 ParseTree.EQ $3 }
>            | Expression '/=' Expression                    { ParseTree.BinOp $1 ParseTree.NE $3 }
>            | Expression '&&' Expression                    { ParseTree.BinOp $1 ParseTree.And $3 }
>            | Expression '||' Expression                    { ParseTree.BinOp $1 ParseTree.Or $3 }
>            | Expression '++' Expression                    { ParseTree.BinOp $1 ParseTree.App $3 }
>            | '[' Expression '..' Expression ']'            { $2 ParseTree.:..: $4 }
> --            | Expression '+' Expression                     { $1 ParseTree.:+: $3 }
> --            | Expression '-' Expression                     { $1 ParseTree.:-: $3 }
> --            | Expression '*' Expression                     { $1 ParseTree.:*: $3 }
> --            | Expression '/' Expression                     { $1 ParseTree.:/: $3 }
> --            | Expression '<' Expression                     { $1 ParseTree.:<: $3 }
> --            | Expression '<=' Expression                    { $1 ParseTree.:<=: $3 }
> --            | Expression '>' Expression                     { $1 ParseTree.:>: $3 }
> --            | Expression '>=' Expression                    { $1 ParseTree.:>=: $3 }
> --            | Expression '==' Expression                    { $1 ParseTree.:==: $3 }
> --            | Expression '/=' Expression                    { $1 ParseTree.:/=: $3 }
> --            | Expression '&&' Expression                    { $1 ParseTree.:&&: $3 }
> --            | Expression '||' Expression                    { $1 ParseTree.:||: $3 }
> --            | Expression '++' Expression                    { $1 ParseTree.:++: $3 }


> AtomarExpression :: { ParseTree.Expression }
>                  : FunApExpression                             { $1 }
>                  | num                                         { ParseTree.Num $1 }
>                  | char                                        { ParseTree.Char $1 }
>                  | string                                      { ParseTree.String $1 }
>                  | '(' Expression ')'                          { $2 }
>                  | '(' Expression ',' TupleExpressionList ')'  { ParseTree.Tuple ($2 : $4) }

> TupleExpressionList :: { [ParseTree.Expression] }
> TupleExpressionList : Expression                          { [$1] }
>                     | Expression ',' TupleExpressionList  { $1 : $3 }

> FunApExpression :: { ParseTree.Expression }
> FunApExpression : ident ExpressionParamList  { buildFunAp $1 $2 }

> ExpressionParamList :: { [ParseTree.Expression] }
> ExpressionParamList :                                          { [] }
>                     | ParameterExpression ExpressionParamList  { $1 : $2 }

Die ParameterExpression ist fast das selbe wie eine AtomarExpression, nur
dass nicht die FunApExpression darueber rekursiv definiert ist. In dieser
Regel wird ein Identifier fuer sich genommen erkannt, da es hier um die
Liste der Parameter geht, in der Funktionsaufrufe nur klammert vorkommen
duerfen.

> ParameterExpression :: { ParseTree.Expression }
> ParameterExpression : ident                                       { ParseTree.Ident $1 }
>                     | num                                         { ParseTree.Num $1 }
>                     | char                                        { ParseTree.Char $1 }
>                     | string                                      { ParseTree.String $1 }
>                     | '(' Expression ')'                          { $2 }
>                     | '(' Expression ',' TupleExpressionList ')'  { ParseTree.Tuple ($2 : $4) }

=====================================================================

> {

> parse :: P ParseTree.ADPProgram
> parse inp (modname, line) = parse1 inp (modname, line)


=====================================================================

Zum Testen des Parsers:

-- > testParser fileName = do
-- >                       file' <- readFile fileName
-- >                       putStrLn file'
-- >                       putStrLn "parser:"
-- >                       case parse file (fileName, 1) of
-- >                            Ok res  -> do putStrLn $ show res
-- >                                          putStrLn $ ParseTree.prettyPrint res
-- >                                          res <- return $ adpLiftWheres '$' res
-- >                                          putStrLn "lifted:"
-- >                                          putStrLn $ ParseTree.prettyPrint res
-- >                            Failed err -> fail $ show err


-- > testScanner fileName = do
-- >                        file' <- readFile fileName
-- >                        putStrLn file'
-- >                        putStrLn "testing scanner"
-- >                        print $ tokens file'
-- >     where tokens []  = []
-- >           tokens str = let (_, token, str') = lexer str
-- >                        in token : tokens str'

=====================================================================

Dieser Teil definiert die Monade fuer den monadischen Parser

> lexerP :: (Token -> P a) -> P a
> lexerP cont s (mname, line) = let (lineinc, token, s') = lexer s 
>                                   line' = case lineinc of
>                                                  (LineInc       inc)   -> (mname, line+inc)
>                                                  (ModuleSwitch  mod l) -> (mod, l)
>                               in  cont token s' line'

=====================================================================

Eine Datenstruktur um die Reihenfolge der Definitionsbloecke
in einem ADP-Programm beliebig zu halten. Mit dem Nichtterminal
"DefBlock" soll der Parser einen beliebigen Block lesen koennen,
der dann in einer zusammenfuehrenden Phase aus der Liste aller
erkannten Bloecke sich die passenden Bloecke raussucht und in
der Programmdatenstruktur speichert.

> data DefBlock = MainCodeBlock String
>               | TargetCodeBlock String
>               | AlgebraTypeDef ParseTree.AlgebraType
>               | AlgebraDef ParseTree.Algebra
>               | GrammarDef ParseTree.Grammar
>               | TypeSynDef ParseTree.TypeSyn
>               | ExternDef ParseTree.TypeDecl


Mit der Funktion "assignBlocks" wird die Liste von Bloecken

> assignBlocks :: [DefBlock] -> P ParseTree.ADPProgram
> assignBlocks blks = let (mc, tc, u, algt, alg, g, ts, ed, fd) = map9 blks
>                     in ok (mc,tc) u algt alg g ts ed fd 
>     where map9 []  = ([], [], [], [], [], [], [], [], [])
>           map9 (a:as) = let (mc, tc, u, algt, alg, g, ts, ed, fd) = map9 as
>                         in case a of
>                              (MainCodeBlock defs)   -> (defs:mc, tc, u, algt, alg, g, ts, ed, fd)
>                              (TargetCodeBlock defs) -> (mc, defs:tc, u, algt, alg, g, ts, ed, fd)
>                              (AlgebraTypeDef defs)  -> (mc, tc, u, defs:algt, alg, g, ts, ed, fd)
>                              (AlgebraDef defs)      -> (mc, tc, u, algt, defs:alg, g, ts, ed, fd)
>                              (GrammarDef defs)      -> (mc, tc, u, algt, alg, defs:g, ts, ed, fd)
>                              (TypeSynDef defs)      -> (mc, tc, u, algt, alg, g, defs:ts, ed, fd)
>                              (ExternDef defs)       -> (mc, tc, u, algt, alg, g, ts, defs:ed, fd)

>           ok (mc,tc) u algt alg g ts ed fd 
>                                   | length mc > 1     = failP ("Too many #main definitions.", 1)
>                                   | length tc > 1     = failP ("Too many #target definitions.", 1)
>                                   | length algt == 0  = failP ("No #algebratype definition given.", 1)
>                                   | length algt > 1   = failP ("Too many #algebratype definitions.", 1)
>                                   | length alg == 0   = failP ("No #algebra defined.", 1)
>                                   | length g == 0     = failP ("No #Grammar definition given.", 1)
>                                   | length g > 1      = failP ("Too many grammars defined.", 1)
>                                   | otherwise         = returnP $ ParseTree.ADPProgram 
>                                                                   ts 
>                                                                   ed
>                                                                   (head algt) 
>                                                                   alg 
>                                                                   (head g)

=====================================================================

Die Fehlerbehandlungsroutine:

> getLineNo :: P ParseTree.LineNumber
> getLineNo = \ s l -> Ok l

> happyError :: P a
> happyError = getLineNo `thenP` (\(modname,line) -> failP (modname ++ ", line " ++ show line ++ ": parse error.", line))

Hilfsfunktionen:

> d2i :: Double -> Int
> d2i s = let (a,b) = span (/= '.') (show s)
>         in if b /= ".0"  then error $ "parser: integer value required: " ++ show s
>                          else (read a) :: Int

> negate :: ParseTree.Expression -> ParseTree.Expression
> negate (ParseTree.Num n) = ParseTree.Num (-1 * n)
> negate exp               = ParseTree.BinOp (ParseTree.Num (-1)) ParseTree.Mul exp

> buildFunAp :: String -> [ParseTree.Expression] -> ParseTree.Expression
> buildFunAp ident []   = ParseTree.Ident ident
> buildFunAp ident args = ParseTree.FunAp ident args

> }
