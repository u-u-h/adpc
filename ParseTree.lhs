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



> module ParseTree where

Ein paar aussagekraeftige Typsynonyme:

> type Name       = String
> type Axiom      = String
> type TypeName   = String
> type Identifier = String

Mit Hilfe des Typs LineNumber repraesentieren wir Zeilennummern. Dabei
speichern wir jeweils den Modul-Namen und die entsprechende
Zeilennummer:

> type LineNumber = (String, Int)
> ppLine (mod, l) = "[" ++ mod ++ ", line " ++ show l ++ "]"

Im Typ ADPProgram sammeln wir alle Daten eines ADP-Programms:

> data ADPProgram = ADPProgram
>                       [TypeSyn]    -- Typsynonyme
>                       [TypeDecl]   -- Typdeklarationen
>                       AlgebraType  -- Algebratyp 
>                       [Algebra]    -- mehrere Algebren
>                       Grammar      -- Die Grammatik
>                                       deriving (Show, Eq)

Typsynonyme:

> data TypeSyn = TypeSyn LineNumber Name TypeDef
>         deriving (Show, Eq)

Externe Typdeklaration:

> data TypeDecl = TypeDecl LineNumber Name TypeDef
>         deriving (Show, Eq)

Der Algebratyp wird zusammengesetzt aus dem Namen des
Typs, einer Liste weiterer Typparameter (nur als Namen)
und einer Liste von Typdefinitionen

> data AlgebraType = AlgebraType LineNumber TypeName [String] TypeDef
>         deriving (Show, Eq)


Eine Algebradefinition besteht aus dem Funktionstypen,
und der Algebrafunktionsdefinition

> data Algebra = Algebra LineNumber AlgebraFunType AlgebraFunDef
>         deriving (Show, Eq)


Die Grammatikdefinition enthaelt den Namen des Startsymbols der
Grammatik (dem Axiom), die Liste von Algebrafunktionen (vgl
AlgebraFundDef) und einer Liste von Definitionen von Grammatikregeln.

> data Grammar = Grammar LineNumber Name Axiom TypeDef [Name] [GrammarFunDef]
>         deriving (Show, Eq)


Die Definition der Algebrafunktionen. Sie besteht aus
dem Namen der Algebrafunktion, einer Liste von Bezeichnern,
die als Ergebnistupel die Algebra ausmachen, und einer
Liste von einzelnen Funktionsdefinitionen, wodurch die
Algebrafunktionen definiert werden.

> data AlgebraFunDef = AlgebraFunDef LineNumber Name [Name] [FunDef]
>         deriving (Show, Eq)


Der Algebrafunktionstyp setzt sich aus dem Namen der
Algebrafunktion, dem Namen des Typs und einer Liste
von eventuellen Typparametern (als String) zusammen.

> data AlgebraFunType = AlgebraFunType LineNumber Name TypeName [TypeDef]
>         deriving (Show, Eq)


In der Grammatik koennen zwei Arten von Funktionen definiert werden:

1. Grammatik-Regeln (GrammarFunDef) Im Falle verschachtelter
   where-Konstrukte werden die lokal definieren Funktionen in der
   [GrammarFunDef]-Liste gespeichert.
2. Benutzerdefinierte Kombinatoren (CombinatorDef)


> data GrammarFunDef = GrammarFunDef LineNumber Identifier [Parameter] TabulationType Production [GrammarFunDef]
>                    | CombinatorDef LineNumber Identifier CombinatorDef
>         deriving (Show, Eq)


Es gibt fuenf Arten von benutzerdefinierten Kombinatoren

> data CombinatorDef = CombinatorTT (Int, Int) (Int, Int)  -- (~~)
>                    | CombinatorSTT Int       (Int, Int)  -- (*~~)
>                    | CombinatorTTS (Int, Int) Int        -- (~~*) 
>                    | CombinatorSTS Int        Int        -- (*~*)
>                    | CombinatorHHH String     String     -- (^^^)
>         deriving (Show, Eq)

Eine Grammatikproduktion kann entweder eine reine Produktion sein,
oder mit den Schluesselworten "tabulated" oder "nontabulated"
versehen werden.

> data TabulationType = TTFree
>                     | TTTabulated (Bool, Bool)
>                     | TTNontabulated
>         deriving (Show, Eq)


ADP_Produktionen bestehen aus Terminalparsern,
die mit Hilfe von Infixoperatoren kombiniert werden.

> data Production = ProdExpr          LineNumber Expression
>                 | CombinatorAp      LineNumber Production String Production
>                 | ListComprehension LineNumber Parameter Expression [LCExpression]
>         deriving (Show, Eq)


CombinatorAp (CombinatorAp (ProdExpr (Ident "f")) "<<<" (ProdExpr (Ident "achar")))
             "~~~"
             ProdExpr (Ident "test") 

Ein Beispiel fuer List comprehensions:

  test = tabulated (
    \(i,j) -> [ d a | a = (j - i), (a >= 2), k <- [i .. j]] |||
    nil <<< empty                                           ... h)


(ListComprehension 
   (TupleParam [Parameter "i",Parameter "j"]) 
   (FunAp "d" [Ident "a"]) 
   [LCLet (Parameter "a") (BinOp (Ident "j") Sub (Ident "i")),
    LCFilter (BinOp (Ident "a") GE (Num 2.0)),
    LCGenerator (Ident "k") (Ident "i" :..: Ident "j")]

> data LCExpression = LCFilter     LineNumber Expression
>                   | LCLet        LineNumber Parameter  Expression
>                   | LCGenerator  LineNumber Expression  Expression
>         deriving (Show, Eq)


Eine Algebra-Funktionsdefinition:

> data FunDef = FunDef LineNumber Identifier [Parameter] Expression
>         deriving (Show, Eq)


Ein Parameter besteht entweder aus einem einfachen Bezeichner, oder
aus einem Tupel von Bezeichnern.  Wildcards werden mit einem
gesonderten Datenkonstruktor dargestellt.

> data Parameter = Wildcard                -- Unterstrich "_" 
>                | Parameter Identifier    -- Bezeichner einer Variablen
>                | TupleParam [Parameter]  -- Tupel 
>         deriving (Show, Eq)

Infixoperatoren; zur Verwendung innerhalb der Algebrafunktionen:

> data BinOp = LT | GT | LE | GE | EQ | NE
>            | Mul | Div | Add | Sub | Exp | App | Cons
>            | Or | And
>         deriving (Show, Eq)


Ein Ausdruck besteht aus einem atomaren Ausdruck,
oder der kombination zweier Ausdruecke mit Hilfe eines
binaeren Operators. 

> data Expression = Ident Identifier                     -- Bezeichner einer Variable
>                 | Char Char                            -- Buchstabenkonstante
>                 | String String                        -- Zeichenkettenkostante
>                 | Num Double                           -- numerisches Literal
>                 | Tuple [Expression]                   -- Tupelausdruecke
>                 | If Expression Expression Expression
>                 | BinOp Expression BinOp Expression   -- Ersatz fuer die folgenden Operatoren
>                 | FunAp Identifier [Expression]
>                 | Expression :..: Expression          -- Enumeration; wird nur bei List Comprehensions
>                                                       -- verwendet
>         deriving (Show, Eq)

> expressionMap :: (Expression -> [a]) -> Expression -> [a]
> expressionMap f (Tuple foo) = concatMap (expressionMap f) foo
> expressionMap f (If a b c) = g a ++ g b ++ g c
>  where g = expressionMap f
> expressionMap f foo@(BinOp a _ c) = f foo ++ g a ++ g c
>  where g = expressionMap f
> expressionMap f foo@(FunAp _ exps) = f foo ++ concatMap g exps
>  where g = expressionMap f
> expressionMap f (a :..: b) = g a ++ g b
>  where g = expressionMap f
> expressionMap f foo = f foo



Ein einzelner Type kann aus einem Typparameter (als Identifier), einem
Tupel, oder einer Liste mit bestimmtem Elementtyp.

> data AtomType        --   Haskell     C
>                      -----------------------
>   = TInt             --   Int         int
>   | TInteger         --   Integer     long
>   | TFloat 	       --   Float       float 
>   | TDouble          --   Double      double
>   | TChar            --   Char        char
>   | TString          --   String      *char
>   | TBool            --   Bool        char
>   | TVoid            --   ()          void

>   | TTypeVar String  -- Typvariable
>   | TTypeSyn String  -- Typsynonym
>         deriving (Show, Eq)

> data TypeClass
>   = TCNum      String  -- Typklasse Num      = [Int, Integer, Float, Double]
>   | TCIntegral String  -- Typklasse Integral = [Int, Integer]
>   | TCFloating String  -- Typklasse Floating = [Float, Double]
>         deriving (Show, Eq)

Instanzen:

> instances = [(TCNum,      [TInt, TInteger, TFloat, TDouble]),
>              (TCIntegral, [TInt, TInteger]),
>              (TCFloating, [TFloat, TDouble])]


> data TypeDef = SingleType AtomType
>              | FunType TypeDef TypeDef
>              | TypeTuple [TypeDef]
>              | ListType TypeDef
>              | TParser TypeDef  -- Parser x
>              | Constraint TypeClass TypeDef 
>         deriving (Show, Eq)


=====================================================================

Der PrettyPrinter fuer die Datenstruktur:

> class PrettyPrint a where
>     prettyPrint :: a -> String


> instance (PrettyPrint a) => PrettyPrint [a] where
>     prettyPrint = concatMap prettyPrint


> instance PrettyPrint ADPProgram where
>     prettyPrint (ADPProgram tss tds algt alg g) = 
>                                          prettyPrint tss ++
>                                          prettyPrint tds ++ 
>                                          prettyPrint algt ++ 
>                                          prettyPrint alg ++ 
>                                          prettyPrint g

> instance PrettyPrint TypeSyn where
>     prettyPrint (TypeSyn l n t)  = ppLine l ++ "#typesyn " ++ n ++ " :: " ++ prettyPrint t ++ "\n"

> instance PrettyPrint TypeDecl where
>     prettyPrint (TypeDecl l n t)  = ppLine l ++ "#extern " ++ n ++ " :: " ++ prettyPrint t ++ "\n"

> instance PrettyPrint AlgebraType where
>     prettyPrint (AlgebraType l tn ts tdefs) = ppLine l ++ "#algebratype{\n" ++
>                                               "> type " ++ tn ++ (concatMap (\ t -> " " ++ t) ts) ++ "= " ++ prettyPrint tdefs ++
>                                               "\n}\n"

> instance PrettyPrint AtomType where
>     prettyPrint TInt         = "Int"
>     prettyPrint TInteger     = "Integer"
>     prettyPrint TFloat       = "Float"
>     prettyPrint TDouble      = "Double"
>     prettyPrint TChar        = "Char"
>     prettyPrint TString      = "String"
>     prettyPrint TBool        = "Bool"
>     prettyPrint TVoid        = "()"
>     prettyPrint (TTypeVar v) = v
>     prettyPrint (TTypeSyn v) = v

> instance PrettyPrint TypeClass where
>     prettyPrint (TCNum i)       = "Num " ++ i
>     prettyPrint (TCIntegral i)  = "Integral " ++ i
>     prettyPrint (TCFloating i)  = "Floating " ++ i


> instance PrettyPrint TypeDef where
>     prettyPrint (SingleType t)  = prettyPrint t
>     prettyPrint (FunType t1 t2) = "(" ++ prettyPrint t1 ++ " -> " ++  prettyPrint t2 ++ ")"
>     prettyPrint (TypeTuple ts)  = "(" ++ (concatMapSep "," prettyPrint ts) ++ ")"
>     prettyPrint (ListType t)    = "[" ++ prettyPrint t ++ "]"
>     prettyPrint (Constraint tc t) = prettyPrint tc ++ " => " ++ prettyPrint t
>     prettyPrint (TParser t)  = "Parser " ++ prettyPrint t

> instance PrettyPrint Algebra where
>     prettyPrint (Algebra l at ad) = ppLine l ++ "#algebra{\n" ++
>                                     prettyPrint at ++
>                                     prettyPrint ad ++
>                                     "}\n"


> instance PrettyPrint AlgebraFunType where
>     prettyPrint (AlgebraFunType l n tn tdefs) = ppLine l ++ "\n> " ++ n ++ " :: " ++ tn ++ " " ++ concatMapSep " " prettyPrint tdefs ++ "\n"


> instance PrettyPrint AlgebraFunDef where
>     prettyPrint (AlgebraFunDef l name params fundef) = ppLine l ++ "\n> " ++ name ++ " = (" ++ (concatMapSep "," id params) ++ ") where\n>    " ++
>                                                      concatMapSep "\n>    " prettyPrint fundef ++ "\n"


> instance PrettyPrint FunDef where
>     prettyPrint (FunDef l id ps exp) = ppLine l ++ id ++ " " ++ concatMapSep " " prettyPrint ps ++ " = " ++ prettyPrint exp


> instance PrettyPrint Parameter where
>     prettyPrint Wildcard       = "_"
>     prettyPrint (Parameter p)  = p
>     prettyPrint (TupleParam ps) = "(" ++ concatMapSep "," prettyPrint ps ++ ")"


> instance PrettyPrint Grammar where
>     prettyPrint (Grammar l name ax td afs gdefs) = ppLine l ++ "#grammar[" ++ ax ++ "]{\n" ++
>                                                    "name: "  ++ name ++ "\n" ++
>                                                    "axiom: "  ++ ax ++ "\n" ++
>                                                    "index type: " ++ prettyPrint td ++ "\n"++
>                                                    "algebra functions: " ++ concatMap (\x -> x ++ " ") afs ++ "\n" ++
>                                                    concatMap prettyPrint gdefs ++
>                                                    "}\n"


> instance PrettyPrint GrammarFunDef where
>     prettyPrint (GrammarFunDef l id args tt prod defs) 
>                                            = ppLine l ++ "\n>  " ++ id ++ " " ++ concatMapSep " " prettyPrint args ++ " = " ++ 
>                                              prettyPrint tt ++ "\n" ++ ">  " ++ prettyPrint prod ++ 
>                                              if defs == [] then "" else
>                                                ">  where\n" ++
>                                                prettyPrint defs ++ " --endwhere"
>     prettyPrint (CombinatorDef l id cd)      = ppLine l ++ "\n>  (" ++ id ++ ") = " ++ prettyPrint cd ++ "\n"


> instance PrettyPrint CombinatorDef where
>     prettyPrint (CombinatorTT l r)  = "(~~) " ++ show l ++ " " ++ show r 
>     prettyPrint (CombinatorSTT l r) = "(*~~) " ++ show l ++ " " ++ show r 
>     prettyPrint (CombinatorTTS l r) = "(~~*) " ++ show l ++ " " ++ show r 
>     prettyPrint (CombinatorSTS l r) = "(*~*) " ++ show l ++ " " ++ show r 
>     prettyPrint (CombinatorHHH l r) = "(^^^) " ++ l ++ " " ++ r 

> instance PrettyPrint TabulationType where
>     prettyPrint (TTFree)                    = ""
>     prettyPrint (TTTabulated (True, True))  = "tabulated"
>     prettyPrint (TTTabulated (True, False)) = "listed"
>     prettyPrint (TTTabulated (False, True)) = "listedj"
>     prettyPrint (TTNontabulated)            = "nontabulated"


> instance PrettyPrint Production where
>     prettyPrint (ProdExpr l exp) = ppLine l ++ prettyPrint exp
>     prettyPrint (CombinatorAp l p1 comb p2) = ppLine l ++ prettyPrint p1 ++ " " ++ comb' ++ " " ++ prettyPrint p2
>         where comb' = if length comb > 5 then "(" ++ comb ++ ")" else comb
>     prettyPrint (ListComprehension l par exp exps) 
>                                      = ppLine l ++ "\\" ++ prettyPrint par ++ " -> [ " ++ prettyPrint exp ++ " | " ++ 
>                                               concatMapSep ", " prettyPrint exps ++ "]"

> instance PrettyPrint Expression where
>     prettyPrint (Ident id)        = id
>     prettyPrint (Char c)          = "\'" ++ [c] ++ "\'"
>     prettyPrint (String s)        = "\"" ++ s ++ "\""
>     prettyPrint (Num n)           = show n
>     prettyPrint (Tuple es)        = "(" ++ concatMapSep "," prettyPrint es ++ ")"
>     prettyPrint (If e1 e2 e3)     = "if " ++ prettyPrint e1 ++ " then " ++ prettyPrint e2 ++ " else " ++ prettyPrint e3
>     prettyPrint (BinOp e1 op e2)  = "(" ++ prettyPrint e1 ++ " " ++ (show op) ++ " " ++ prettyPrint e2 ++ ")"
>     prettyPrint (FunAp f args)    = f ++ concatMapSep " " prettyPrint args
>     prettyPrint (e1 :..: e2)      = "[" ++ prettyPrint e1 ++ " .. " ++ prettyPrint e2  ++ "]"
>     prettyPrint x                 = error $ "Expression.prettyPrint: undefined pattern: " ++ show x

> instance PrettyPrint LCExpression  where
>     prettyPrint (LCFilter l exp)      = ppLine l ++ prettyPrint exp
>     prettyPrint (LCLet l par exp)     = ppLine l ++ prettyPrint par ++ " = " ++ prettyPrint exp
>     prettyPrint (LCGenerator l e1 e2) = ppLine l ++ prettyPrint e1 ++ " <- " ++ prettyPrint e2

Hilfsfunktion:

> concatMapSep :: String -> (a -> String) -> [a] -> String
> concatMapSep sep f []      = ""
> concatMapSep sep f [l]     = f l
> concatMapSep sep f (l:ls)  = f l ++ sep ++ concatMapSep sep f ls

