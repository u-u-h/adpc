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



%include lhs2TeX.fmt
%include dss.fmt

%% TODO
%% =====
%% V Unterscheidung von recursiven Funktionen und Prozeduren
%% V Table access nicht per "!"
%% V Kommentare mitfuehren
%% - Variablen ueber state monad


%% Problem:
%% komplette Code-Erklaerung anhand von Haskell-Program
%% vs.
%% abstrahierte Code-Erklaerung vgl. dss, da, etc.


%% komplett                            | abstrahiert
%% ------------------------------------------------------------------------
%% - sauberer                          | - konsistent mit dss, da 
%% - inkonsistent mit dss, da          | - weniger zu Kommentieren

%% Hauptproblem: 
%% Datentyp

%if False
Problem: haskell code recht komplex. Für komplette erläuterung zu
umfangsreich. problem: gekürztzte erläiterung nicht überall so ohne
weiteres möglich. Mögliche Lösungen: verscuehn, den haskell-code
soweit zu vereinfachen, dass eine relativ einfache erläöuterung
ausreicht.

möglichkeiten
-------------
1. code vereinfachen und beschreiben
2. code gar nicht beschreiben, sondern nur am beispiel zeigen
3. Am Beispiel beschreiben und Quellcode verbatim im Anhang

3 Stufen
IL -> LC blöd zu beschreiben
LC -> LC listeneliminierung
LC -> TL einfacb zu beschreiben

%endif

%if False

> module ListCompr where

> import Char
> import List
> import Constants
> import Dss
> import PrettyPrint
> import Tools
> import MathExp
> import Expr
> import TL
> import TLData

%endif



\section{Code generation}

In the following we describe the last steps of the translation: the
code generation. This is done in a multi-phase process. In the first
step we translate the intermediate language $\mathcal{S}$ into a second
intermediate language $\mathcal{LC}$, the language of list
comprehensions. The programs in this language are then optimized by
eliminating unnecessary list constructs. After this optimization we
translate the language $\mathcal{LC}$ into the target language
$\mathcal{TL}$. These translations are summarized as follows:

\begin{alignat}{3}
& \mathcal{S} \rightarrow \mathcal{LC}  && \qquad \text{compile to
  list comprehensions} \nonumber \\
& \mathcal{LC} \rightarrow \mathcal{LC} && \qquad \text{list elimination}\nonumber \\ 
& \mathcal{LC} \rightarrow \mathcal{TL} && \qquad \text{target code generation}\nonumber 
\end{alignat}

\subsection{Intermediate language for list comprehensions}
%% ==============================================================================

%format G_LC        = "\mathcal{G}_{\mathcal{LC}}"

%if False

Die Sprache LC der Listenbeschreibunen ist wie folgt definiert. 

\begin{alignat}{3}
  |G_LC|:& \qquad & |LCProd|      & \; \rightarrow \quad && | Name = LCexp|          \nonumber \\
         &        & |LCexp|       & \; \rightarrow \quad && | LC Exp LCQualifier|^+  \nonumber \\
         &        &               & \; \rightarrow \quad && | LCAppend LCexp LCexp | \nonumber \\
         &        &               & \; \rightarrow \quad && | LCAppl FctName LCexp | \nonumber \\
         &        &               & \; \rightarrow \quad && | LCEnum Exp Exp       | \nonumber \\
         &        &               & \; \rightarrow \quad && | LCTabAccess Name Subscripts  | \nonumber \\
         &        &               & \; \rightarrow \quad && | LCIf Exp LCexp LCexp | \nonumber \\
         &        &               & \; \rightarrow \quad && | LCEmpty | \nonumber \\
         &        & |LCQualifier| & \; \rightarrow \quad && | Filter Exp | \nonumber \\
         &        &               & \; \rightarrow \quad && | Generator Exp LCExp | \nonumber 
\end{alignat}

Erklaärungen für die einzelnen Konstrukte dieser sprache zeigt die
folgende Tabelle:

\begin{tabular}{l||l}
LC & Hauptelement der Sprache LC: Listenbeschgreibung. Eine Listenbeschreibung
besteht aus dem Kopf (Exp) und einer beliebigen Zahl von
Qualifiern. Ein Qualifier ist dabei entweder ein Filter oder ein
Generator. \\
LCAppend    & Konkatenation zweier Listenbeschreibunen\\
LCAppl      & Funktionsanwendung\\
LCEnum      & List enumeration, eg. [i..j] \\
LCTabAccess & Tabellenzugriff \\
LCIf        & If-Konstrukt \\
LCEmpty     & The empty list
\end{tabular}


\begin{alignat}{3}
         &        & |Exp|         & \; \rightarrow \quad  && | ExpChar Char     | \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpString String | \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpNum Double    | \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpVoid          | \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpVar String    | \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpPOp OpName [Exp] | \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpIOp Exp OpName Exp| \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpIf  Exp Exp Exp | \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpInput  InputString MathExp| \nonumber \\
         &        &               & \; \rightarrow \quad && | ExpInputS InputString SSubScripts| \nonumber \\
\end{alignat}

%endif

As intermediate language $\mathcal{LC}$ we use the language of list
comprehensions. The following algebraic data type defines its abstract
syntax:

> data LCexp = LC          String Exp [LCQualifier]
>            | LCAppend    LCexp LCexp
>            | LCAppl      String LCexp
>            | LCEnum      Exp Exp
>            | LCTabAccess String SubScripts
>            | LCIf        Exp LCexp LCexp
>            | LCEmpty
>                  deriving (Show, Eq)

> data LCQualifier = Filter Exp
>                  | Generator Exp LCexp  
>                  | Let Exp Exp
>                  deriving (Show, Eq)

> type LCProd = (String, LCexp)

The main construct of a list comprehension is element |LC|. It
contains an expression (Exp, see Section~\ref{sec:algebrafunctions})
as head of the list comprehension and a list of qualifiers
(|LCQualifier|) as the body. A qualifier then is either a filter, a
generator or a let expression. |LCAppend| appends two list
comprehensions. This is used for applications of the |:/||||||/|
combinator.  An application of a choice function is mapped to a
|LCAppl| construct, which applies a function to a list
comprehension. |LCEnum| represents an enumeration of the form |[a
.. b]|. Nonterminals are mapped to a |LCTabAccess| construct. |LCIf|
is used for filter applications and finally, |LCEmpty| represents the
empty list.

%if False

> ppLCprod :: LCProd -> String
> ppLCprod (name, lcexp) = name ++ " = " ++ pretty lcexp

> ppLCprodLatex :: [LCProd] -> String
> ppLCprodLatex ps = "\\documentclass{article}\n\\usepackage{amsmath}\n\\begin{document}\n\\begin{alignat}{3}\n" ++
>                    concatMap (\(name, lcexp) -> name ++ "_{i,j} & \\; = \\; && " ++ postproc (prettyLang Latex (rmAppls "" lcexp)) ++ 
>                    "\\nonumber\\\\\n") ps ++ 
>                    "\\end{alignat}\n\\end{document}" 
>   where
>     rmAppls appl (LCAppend a b) = LCAppend (rmAppls appl a) (rmAppls appl b)
>     rmAppls appl (LCAppl ap exp) | ap == appl = rmAppls appl exp
>                                  | otherwise  = LCAppl ap (rmAppls ap exp)
>     rmAppls appl (LCIf exp a b) = LCIf exp (rmAppls appl a) (rmAppls appl b)
>     rmAppls _ x = x

>     postproc = elimtbl . addbs
>     elimtbl [] = []
>     elimtbl ('t':'b':'l':'\\':'_':xs) = elimtbl xs
>     elimtbl (x:xs)                    = x:elimtbl xs
>     addbs   [] = []
>     addbs   ('\\':'_':xs) = '\\':'_':addbs xs
>     addbs   ('_':'{':xs)  = '_':'{':addbs xs
>     addbs   ('_':xs)      = '\\':'_':addbs xs
>     addbs   (x:xs)        = x:addbs xs

> instance Pretty LCexp where

>     pretty (LC _ exp exp2) = "[ " ++ pretty exp ++ (if length exp2 == 0 then " " else " | ") ++ mapsep ", " pretty exp2 ++ "]"
>     pretty (LCAppend e1 e2) = pretty e1 ++ " ++ " ++ pretty e2
>     pretty (LCAppl name exp) = name ++ "(" ++ pretty exp ++ ")"
>     pretty (LCEnum e1 e2)     = "[" ++ pretty e1 ++ " .. " ++ pretty e2 ++ "]"
>     pretty (LCTabAccess n (ST (i,j))) = n ++ "!(" ++ pretty i ++ "," ++ pretty j ++ ")"
>     pretty (LCIf e1 e2 e3)         = "if " ++ pretty e1 ++ " then " ++ pretty e2 ++ " else " ++ pretty e3
>     pretty (LCEmpty)               = "[]"

>     prettyLang Latex (LC _ exp exp2) = "[ " ++ prettyLang Latex exp ++ (if length exp2 == 0 then " " else " | ") ++ mapsep ", " (prettyLang Latex) exp2 ++ "]"
>     prettyLang Latex (LCAppend e1 e2) = prettyLang Latex e1 ++ " +\\!\\!+ \\nonumber \\\\\n &   && " ++ prettyLang Latex e2
>     prettyLang Latex (LCAppl name exp) = name ++ "(" ++ " \\nonumber \\\\\n &   && " ++ prettyLang Latex exp ++ ")"
>     prettyLang Latex (LCEnum e1 e2)     = "[" ++ prettyLang Latex e1 ++ " .. " ++ prettyLang Latex e2 ++ "]"
>     prettyLang Latex (LCTabAccess n (ST (i,j))) = n ++ "_{" ++ prettyLang Latex i ++ "," ++ prettyLang Latex j ++ "}"
>     prettyLang Latex (LCIf e1 e2 e3)         = "\\text{if } " ++ prettyLang Latex e1 ++ " \\text{ then } " ++ prettyLang Latex e2 ++ 
>                                                " \\nonumber \\\\\n &   && " ++ "\\phantom{\\text{if } " ++ prettyLang Latex e1 ++ "}" ++
>                                                 " \\text{ else } " ++ prettyLang Latex e3
>     prettyLang Latex (LCEmpty)               = "[]"
>     prettyLang _     x = pretty x

> instance Pretty LCQualifier where
>     pretty (Filter exp) = pretty exp
>     pretty (Generator exp lcexp) = pretty exp ++ " <- " ++ pretty lcexp
>     pretty (Let exp1 exp2)       = "let " ++ pretty exp1 ++ " = " ++ pretty exp2

>     prettyLang Latex (Filter exp) = prettyLang Latex exp
>     prettyLang Latex (Generator exp lcexp) = p exp ++ " \\leftarrow " ++ prettyLang Latex lcexp
>       where
>         p (ExpVar (k:[])) = k:"_{1}"
>         p (ExpVar (k:n))  = k:"_{" ++ n ++ "}"
>         p (ExpTupel exps) = "(" ++ mapsep "," p exps ++ ")"
>         p x = pattErr "LCQualifier.prettyLang" x
>     prettyLang Latex (Let exp1 exp2)       = "let " ++ prettyLang Latex exp1 ++ " = " ++ prettyLang Latex exp2
>     prettyLang _ x = pretty x

> meVar v = ExpME (Var v)

%endif

%format ExpME = "\!"

\subsection{Terminal parsers}
%% ==============================================================================

Terminal symbol are represented as list comprehensions. See also the
terminal parser definitions in Section~\ref{sec:lexical parsers}.

%if False

> terminals :: [(String, (Int, [Exp] -> LCexp))]
> terminals =
>   [("empty",   (0, \_ -> LC "empty" ExpVoid [])),
>    ("loc",     (0, \[ExpME i,ExpME j] -> LC "loc" (ExpME j) [])),
>    ("achar",   (0, \[ExpME i,ExpME j] -> LC "achar" (ExpInput "z" j) [])),
>    ("base",    (0, \[ExpME i,ExpME j] -> LC "base" (ExpInput "z" j) [])),
>    ("lbase",   (0, \[ExpME i,ExpME j] -> LC "lbase" (ExpME j) [])),
>    ("astring", (0, \[ExpME i,ExpME j] -> LC "astring" (ExpTupel [ExpME i, ExpME j]) [])),
>    ("region",  (0, \[ExpME i,ExpME j] -> LC "region" (ExpTupel [ExpME i, ExpME j]) [])),
>    ("uregion", (0, \[ExpME i,ExpME j] -> LC "uregion" (ExpTupel [ExpME i, ExpME j]) [])),
>    ("string",  (1, \[ExpString s,ExpME i,ExpME j] -> 
>                     LC "string" (ExpInputS "z" (i,j)) [Filter (ExpIOp (ExpInputS "z" (i,j)) "==" 
>                                                                (ExpString s))])),
>    ("char",    (1, \[ExpChar c,ExpME i,ExpME j] -> 
>                     LC "char" (ExpInput "z" j) [Filter (ExpIOp (ExpInput "z" j) "==" 
>                                                                (ExpChar c))]))]

%endif

< terminals :: [(String, (Int, [Exp] -> LCexp))]
< terminals =
<   [("empty",    (0, \_ -> LC "empty" ExpVoid [])),
<    ("achar",    (0, \[ExpME i,ExpME j] -> LC "achar" (ExpInput "z" j) [])),
<    ("astring",  (0, \[ExpME i,ExpME j] -> LC "astring" (ExpTupel [ExpME i, ExpME j]) [])),
<    ("astringp", (0, \[ExpME i,ExpME j] -> LC "astringp" (ExpTupel [ExpME i, ExpME j]) [])),
<    ("char",     (1, \[ExpChar c,ExpME i,ExpME j] -> 
<                     LC "char" (ExpInput "z" j) 
<                       [Filter (ExpIOp (ExpInput "z" j) "==" (ExpChar c))]))]
<    ("string",  (1, \[ExpString s,ExpME i,ExpME j] -> 
<                     LC "string" (ExpInputS "z" (i,j)) 
<                       [Filter (ExpIOp (ExpInputS "z" (i,j)) "==" (ExpString s))])),
<    ("loc",      (0, \[ExpME i,ExpME j] -> LC "loc" (ExpME j) [])),


%if False

\subsection{Filters}
%% ==============================================================================

Filter werden durch den Datentyp Exp repräsentiert.

> filters :: [(String, (Int, [Exp] -> Exp))]
> filters = 
>   [("basepairing",  (0, \[ExpME i, ExpME j] -> ExpPOp "basepairing" [ExpME i, ExpME j])),
>    ("stackpairing", (0, \[ExpME i, ExpME j] -> ExpPOp "stackpairing" [ExpME i, ExpME j])),
>    ("contains_region",  (1, \[_, ExpME i, ExpME j] -> ExpPOp "contains_region" [ExpME i, ExpME j])),
>    ("minsize",      (1, \[ExpNum v, ExpME i, ExpME j] -> ExpIOp (ExpME (j :- i)) ">=" (ExpNum v))),
>    ("maxsize",      (1, \[ExpNum v, ExpME i, ExpME j] -> ExpIOp (ExpME (j :- i)) "<=" (ExpNum v))),
>    ("size",         (2, \[ExpNum min, ExpNum max, ExpME i, ExpME j] -> 
>                                     ExpIOp (ExpIOp (ExpME (j :- i)) ">=" (ExpNum min)) "&&" 
>                                    (ExpIOp (ExpME (j :- i)) "<=" (ExpNum max))))]

%endif



\begin{figure}[htbp]
\begin{fminipagec}
\textbf{Overview: Translating into language of list comprehension}

The function |sToLC| translates the intermediate language
$\mathcal{S}$ into the language of list comprehensions, $\mathcal{LC}$.


< sToLC : {-"\text{ $\mathcal{S} \rightarrow \mathcal{LC}$ transformation}"-}
< sToLC u_p -> lc_p

\begin{tabular}{p{6cm}p{6cm}}
\textit{Input}      & \textit{Output} \\
|u_p| $\mathcal{S}$-production    & |lc_p|: list comprehension expression
\end{tabular}
    
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{$\mathcal{S} \rightarrow \mathcal{LC}$ transformation}
\label{fig:def:stolc}
\end{figure}

\subsection{$\mathcal{S} \rightarrow \mathcal{LC}$ translation}
%% ==============================================================================

%if False

> sToLCProds :: [AlgDefs] -> [ILProd] -> [(String, LCexp)]
> sToLCProds algfs ps = map (\(name,_,il2) -> (name,sToLC il2)) ps

>  where

%endif

%format (ST a) = a

We implement the transformation $\mathcal{S} \rightarrow \mathcal{LC}$
by structural recursion. Figure~\ref{fig:def:stolc} gives an overview.

The first definition is that of the terminal symbol |ILTerminal|. The
function |lookupTerminal| fetches the corresponding definition out of
the list |terminals| defined above. We then apply the index pair
$(i,j)$ and the additional arguments on the terminal's definition.

>  sToLC :: ILUnit -> LCexp 
>  sToLC (ILTerminal (name, args) (ST (i,j))) = lookupTerminal name args [ExpME i, ExpME j]

%if False

>   where
>    lookupTerminal name args params = case lookup name terminals of
>                                             Just (nargs,f) -> f (map str2exp args ++ params)
>                                             Nothing -> error ("unknown terminal " ++ name)

%endif

|ILNonterminal| is directly mapped on |LCTabAccess| of language
$\mathcal{LC}$:

>  sToLC (ILNonterminal name (ST (i,j))) = LCTabAccess name (ST (i,j))

An application of combinator |:/||||||/| is mapped on |LCAppend|:

>  sToLC (p :/|||/ q)                    = LCAppend (sToLC p) (sToLC q)

We use the function |getAlgebraDef| to fetch the definition of the
choice function and translate this definition to |LCAppl|:

>  sToLC (p :/.../ h)                    = LCAppl (getAlgebraDef h) (sToLC p)

%if False

>   where
>    (algName,atype,args,rhs) = head (getAlgDefList algfs h)
>    getAlgebraDef h = case rhs of
>                ExpChoice fct (ExpVar _) -> fct
>                otherwise                -> error ("unknown kind of choice function: " ++ show rhs)

%endif

When processing filter applications, we distinguish between two kinds
of filters: The length-limiting filters |size|, |minsize| and
|maxsize| and all other filters. In this translation we do not need to
handle the length-limiting-filters, since we have already incorporated
them during the subscript-calculation. All other filters are direcly
transformed into an |LCIf|-expression.

>  sToLC (p `ILwith` ((name, args),(ST (i,j)))) = 
>    if elem name ["size", "minsize", "maxsize"] then sToLC p
>    else LCIf (lookupFilter name [ExpME i, ExpME j]) (sToLC p) LCEmpty

%if False

>    where
>      lookupFilter name params = case lookup name filters of
>                     Just (nargs, f) -> if length args /= nargs then error ( "wrong arity for filter " ++ name)
>                                        else f (map str2exp args ++ params)
>                     Nothing -> error ( "unknown filter " ++ name)

%endif

The application of the |:/<<</| combinator is the central equation of
the |sToLC| transformation. This is directly compiled into a list
comprehension.  The function |getAlgebraDef| fetches the definition for
the algebra function f. We store the arguments of this algebra
function in the list |arguments_f|, and the definition of the algebra
function in the variable |rhs|. The expressions that are to be
applied to this algebra function are converted into a list by the
function |flat|. These are then translated by structural recursion on
function |sToLC| and assigned to the corresponding arguments of the
function's definition (|zip arguments_f argument|). The yield size
check stored in the variable |bounds| is directly mapped to
a filter application in the list comprehension. Loops and variable
bindings are translated to generators of the list comprehension.

<  sToLC ((f, bounds, loops) :/<<</ p) = LC rhs (Filter bounds ++ 
<                                                map Generator loops ++ 
<                                                map Generator binding)
<  where
<    (arguments_f,rhs) = getAlgebraDef algfs f

<    arguments = map (sToLC) (flat p)
<    flat (p :/~~~/ q) = flat p ++ flat q
<    flat p            = [p]
<    binding = zip arguments_f arguments

%if False      

>  sToLC ((((f, _),_), bounds, loops) :/<<</ p) = LC cmt rhs ([bounds'] ++ loops' ++ binding')
>    where
>     cmt = f ++ " <<< " ++ pretty p
      
>     -- hole fuer jede Algebra in algfs genau eine Algebrafunktion mit Namen f:
>     (_,_,arguments_f,rhs) = head (getAlgDefList algfs f)
>     arguments = map (sToLC) (flat p)
>       where
>         flat (p :/~~~/ q) = flat p ++ flat q
>         flat p            = [p]
>     binding = zip arguments_f arguments
>     binding' = map bind binding
>       where
>         bind (SigId var, lcexp)    = Generator (ExpVar var) lcexp
>         bind (SigTupel tup, lcexp) = Generator (gentup (SigTupel tup)) lcexp
>           where
>             gentup (SigTupel tup) = ExpTupel (map gentup tup)
>             gentup (SigId    var) = ExpVar var
      
>     bounds' = Filter (generateBounds bounds)
>     loops' = map generateLoops loops
>       where
>         generateLoops (var, from, to) = Generator (ExpVar var) (LCEnum (ExpME from) (ExpME to))
    
>     -- for lookahead combinator:
>     generateBounds (ST (bl, bu), ST (lal, lar))  
>       | includesLAME lal || includesLAME lar -- lal and lar acn contain Max/Min and LA, so we catch all patterns...
>                     = ExpIOp  (ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME lal)) "&&"
>                               (ExpIOp  (ExpME (calcME (bu :- bl))) "<=" (ExpME lar))
      
>     -- the rest can contain numbers or expressions with variables:
>     generateBounds (ST (bl, bu), ST (ysl, Infinite))  = ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME (ysl))
>     generateBounds (ST (bl, bu), ST (ysl, ysu)) 
>        | ysl == ysu = ExpIOp  (ExpME (calcME (bu :- bl))) "==" (ExpME (ysl)) 
>        | otherwise  = ExpIOp  (ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME (ysl))) "&&"
>                               (ExpIOp  (ExpME (calcME (bu :- bl))) "<=" (ExpME (ysu)))
      
>     pretty (ILTerminal (t,_) _)  = t
>     pretty (ILNonterminal nt _)  = nt
>     pretty (p :/~~~/ q)          = pretty p ++ " ~~~ " ++ pretty q
>     pretty (p :/|||/ q)          = pretty p ++ " ||| " ++ pretty q
>     pretty (p :/.../ h)          = pretty p ++ " ... " ++ h
>     pretty (p `ILwith` ((f,_),_)) = pretty p ++ " `with` " ++ f
>     pretty ((((f, _),_),_,_ ) :/<<</ p) = "f " ++ " <<< " ++ pretty p
>     pretty x = pattErr "sToLC.pretty"  x

%endif           

%if False

> str2exp ('\'':c:'\'':[]) = ExpChar c
> str2exp n                | all isDigit' n = ExpNum ((read n)::Double)
>     where isDigit' c = isDigit c || c == '.'
> str2exp x                = pattErr "str2exp" x

%endif

As example we use the ADP-program for a global alignment:

< alignment  = tabulated(
<                nil <<< (char '$')                    |||
<                d   <<< achar -~~ alignment           |||
<                i   <<<           alignment ~~- achar |||
<                r   <<< achar -~~ alignment ~~- achar ... h);

< nil _   = 0
< d a s   = s - 1
< i   s a = s - 1
< r a s b = if a == b then s + 1 else s - 1

The subscript analysis generates the following $\mathcal{S}$-program:

< alignment!(i, j) =
<    h[
<       if (j-i) == 1 then {
<          nil((char '$')(i,j))
<       }
<       ++
<       if (j-i) >= 2 then {
<          d(achar(i,i+1), alignment!(i+1,j))
<       }
<       ++
<       if (j-i) >= 2 then {
<          i(alignment!(i,j-1), achar(j-1,j))
<       }
<       ++
<       if (j-i) >= 3 then {
<          r(achar(i,i+1), alignment!(i+1,j-1), achar(j-1,j))
<       }
<    ]

The transformation $\mathcal{S} \rightarrow \mathcal{LC}$ then yields
the following list comprehension:

< alignment = 
<   maximum(
<     [ 0 | (j-i) == 1, a1 <- [ z!j | z!j == '$']] ++ 
<     [ a2 - 1 | (j-i) >= 2, a1 <- [ z!(i+1) ], 
<                            a2 <- alignment!(i+1,j)] ++ 
<     [ a1 - 1 | (j-i) >= 2, a1 <- alignment!(i,j-1), 
<                            a2 <- [ z!j ]] ++ 
<     [ (if a1 == a3 then a2 + 1 else a2) - 1 | 
<                (j-i) >= 3, a1 <- [ z!(i+1) ], 
<                            a2 <- alignment!(i+1,j-1), 
<                            a3 <- [ z!j ]])

\subsection{List elimination}

In the above example it is remarkable, that all list comprehensions
can only contain atomic elements. For example, the generator |a1 <-
[z!(i+1)]| is in any case only singleton. The table |alignment| can
also contain only maximally one element, since it closes with the
choice function |maximum|. Therefore the generator |a2 <- alignment!
(i+1, j)]| can also hold maximally one element. With this information
we remove superfluous generators from the list comprehensions. We
achive this by moving the corresponding generators direcly into the
heads of the list comprehensions. This optimization then leads to the
following $\mathcal{LC}$-program:

< alignment = 
<   maximum(
<     [ 0 | (j-i) == 1, z!j == '$'] ++ 
<     [ tbl_alignment(i+1, j) - 1 | (j-i) >= 2] ++ 
<     [ tbl_alignment(i, j-1) - 1 | (j-i) >= 2] ++ 
<     [ (if z!(i+1) == z!j then tbl_alignment(i+1, j-1) + 1 
<                          else tbl_alignment(i+1, j-1)) - 1 | 
<                                   (j-i) >= 3])

In the following, we will translate this intermediate code into the
target program.

%if False

\subsection{Derive result type: atom vs. List}
%% ==============================================================================

> data ResType = List | Atom
>                  deriving (Show, Eq)

> ppResType (name, rt) = name ++ ": " ++ show rt

> deriveResultTypes ::  [(String, LCexp)] -> [(String, ResType)]
> deriveResultTypes ps = fixit wrk init (length ps) ps "deriveResultTypes: does not terminate"
>   where
>     init = map (\(name, _) -> (name, List)) ps
>     wrk :: [(String, ResType)] -> (String, LCexp) -> (String, ResType)
>     wrk current (name, LCAppl fct _)    | elem fct ["maximum", "minimum", "sum"] = (name, Atom)
>     wrk current (name, LCTabAccess n _)  = case lookup n current of
>                                                  Just rt -> (name, rt)
>                                                  Nothing -> error "deriveResultTypes"
>     wrk _ (name, _)                    = (name, List)                           

\subsection{Eliminiate Lists}
%% ==============================================================================

> eliminateLists :: [(String, ResType)] -> [(String, LCexp)] -> [(String, LCexp)]
> eliminateLists rts ps = map elimProd ps
>   where
>     elimProd (name, lc) = (name, elim lc)
>     elim (LC cmt exp exp2)  =  LC cmt exp' exp2'
>       where
>         (bind,exp2') = let (x,y) = unzip (map b exp2) in (map rmVar (concat x), concat y)
>           where
>             rmVar (ExpVar v, def) = (v,def)
>             b :: LCQualifier -> ([(Exp,Exp)],  [LCQualifier])
>             b (Generator (ExpTupel tup) (LC _ (ExpTupel tup2) [])) = if length tup /= length tup2 then error "eliminateLists: wrong tupel binding"
>                                                                    else (zip tup tup2, [])
>             b (Generator e (LC _ e2 []))                = ([(e, e2)], [])
>             b (Generator e (LC _ e2 [Filter e3]))       = ([(e, e2)], [Filter e3])
>             b (gen@(Generator e (LCTabAccess name ss))) = case lookup name rts of
>                                                                Just Atom -> ([(e, ExpTLVar (VANonterm (ptbl prefixes ++ name) ss))], [])
>                                                                otherwise -> ([], [gen])
>             b (Generator e lcexp)                     = ([], [Generator e (elim lcexp)])
>             b other                                   = ([], [other])
>         exp' = insertVarBinds bind exp
>     elim (LCAppend lc1 lc2)   = LCAppend (elim lc1) (elim lc2)
>     elim (LCAppl name lc)    = LCAppl name (elim lc)
>     elim (LCIf exp lc1 lc2) = LCIf exp (elim lc1) (elim lc2)
>     elim x                  = x

%endif

\subsection{Target code generation}
\label{sec:codegen}
%% ==============================================================================

The following algebraic datatype defines the abstract syntax of the
target language:

< data TL = TLVar Ident
<         | TLAssign Ident TL
<         | TLIf Exp [TL] [TL]
<         | TLComment String
<         | TLFor Ident Exp Exp [TL]
<         | TLNonterm Ident Subscripts
<         | TLExpr Exp
<                deriving (Show, Eq)

Here, |TLVar| holds a variable in the target language. |TLAssign| is
an assignment of a target language construct to a variable. The |TLIf|
constructs represents an if-Expression. |TLComment| is a comment in
the target language. |TLFor| is a for-loop, |TLNonterm| a use of a
nonterminal symbol. This is then either mapped to a table access (in
case of a tabulated nonterminal) or a function call (in case of a
nontabulated nonterminal). Finally, |TLExpr| can contain an expression
in form of the datatype |Exp|.


%if False

> lccodegen recur rts lcps = map codegen lcps
>   where
>     codegen (name, lcexp) 
>         | recursive = TLFD [] TLInt (pfct prefixes ++ name) [(["i"], TLInt), (["j"], TLInt)]  (tempVarDecl++loopVarDecl) code (TLVar v)
>         | otherwise = TLFD [] TLVoid(pfct prefixes ++ name) [(["i"], TLInt), (["j"], TLInt)] (tempVarDecl++loopVarDecl) (code ++ assignResult) (TLVar v)
>       where
>         resultType = case lookup name rts of
>                        Just t    -> t
>                        otherwise -> error "lccodegen"
>         recursive = elem name recur
>         assignResult = [TLAssign (VANonterm (ptbl prefixes ++ name) (ST (Var "i",Var "j")), TLVoid) (TLVar v)]

>         tempVarDecl = mergeDecls (map tvd (reverse vlist)) where 
>                         tvd (Direct n, dt) = ([n], dt)
>         loopVarDecl = if length lvs == 0 then [] else [(sort (nub lvs), TLInt)]

>         -- loopVarDecl = if length lvs == 0 then [] else [(sort (nub lvs), TLInt)]

>         ((v,code),(vlist,lvs,_)) = runSM_LC ([],[],"") (cg lcexp)

%endif

\begin{figure}[htbp]
\begin{fminipagec}
\textbf{Overview: Code generation}

The function |cg| translates the language of list comprehensions,
$\mathcal{LC}$, into the target language $\mathcal{TL}$.

< cg : {-"\text{ code generation}"-}
< cg lc_p -> (v_result, tc_p)

\begin{tabular}{p{6cm}p{6cm}}
\textit{Input}                          & \textit{Output} \\
|lc_p|: list comprehension expression   & |v_result|: result variable\\
                                        & |tc_p|: target code\\
\textit{States} & \\
temporary variables & \\
loop variables & \\
current choice function & \\
\end{tabular}
    
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Code generation}
\label{fig:def:cg}
\end{figure}

The function cg (Figure~\ref{fig:def:cg}) is implemented via a state
monad. The state consists of the following elements:

\begin{itemize}
\item A list of temporary variables. With the function |newTempVar| we
  can fetch a new temporary variable from the store of variables. The
  store itself is unlimited.
\item The list of loop variables. With the function |addLoopVar| we
  can add an additional loop var to the store of variables.
\item The current choice function. This function can be changed with
  function |changeChoice| and accessed with function |getChoice|.
\end{itemize}

We begin our presentation with element |LCAppl|, the application of a
choice function. Here the current function is simply changed with the
state monad function |changeChoice|:

> cg (LCAppl chc lc) = 
>  do
>    changeChoice chc
>    cg lc

With the application of a concatenation, the two expressions are first
translated by structural recursion and the results are stored in the
variables |v1| and |v2|. Then a new temporary variable |v3| is
introduced for storing the overall result of the concatenation. The
code for the actual selection is generated by the function
|cgChoiceSingleElement|.

> cg (LCAppend lc1 lc2) = 
>  do
>    (v1,code1) <- cg lc1
>    (v2,code2) <- cg lc2
>    v3         <- newTempVar TLInt
>    chc        <- getChoice
>    chcCode    <- return (cgChoiceSingleElement v3 chc v1 v2)
>    return (v3, code1 ++ code2 ++ chcCode)  

The compilation of an If-construct behaves similarly. The variables |v1|
and |v2| hold the results for the then- and the else-case. The variable
|v3| then stores the overall result. The If-construct is directly
compiled to an If-construct of the target language:

> cg (LCIf exp lc1 lc2) = 
>  do
>    (v1,code1) <- cg lc1
>    (v2,code2) <- cg lc2
>    v3         <- newTempVar TLInt
>    code       <- return [TLIf exp (code1 ++ [TLAssign v3 (TLVar v1)])
>                                   (code2 ++ [TLAssign v3 (TLVar v2)])]
>    return (v3, code)

A table access in the $\mathcal{LC}$-language is compiled directly
to a table access in the $\mathcal{TL}$ language:

%if False

> cg (LCTabAccess nt ss) = 
>  do
>    v1   <- newTempVar TLInt
>    code <- return [TLAssign v1 (TLVar (VANonterm (ptbl prefixes ++ nt) ss, TLVoid))]
>    return (v1, code)

%endif

< cg (LCTabAccess nt ss) = 
<  do
<    v1   <- newTempVar TLInt
<    code <- return [TLAssign v1 (TLNonterm nt ss)]
<    return (v1, code)


For the empty list an assignment of the zero value is produced
(function |nilval|). The zero value depends on the choice
function. With maximization the zero value is INTMIN, with
minimization it is INTMAX, and with summation it is 0.

%if False

> cg LCEmpty = 
>  do
>    v1   <- newTempVar TLInt
>    chc  <- getChoice
>    code <- return [TLAssign v1 (TLExp (ExpNum (nilval chc)))]
>    return (v1, code)

%endif

< cg LCEmpty = 
<  do
<    v1   <- newTempVar TLInt
<    chc  <- getChoice
<    code <- return [TLAssign v1 (nilval chc)]
<    return (v1, code)

The largest part of the translation takes place during the actual list
comprehension. Here, the additional function |cgLC| serves as
translator for the qualifiers of the list comprehension.

> cg (LC cmt exp lcs) = 
>  do
>    comment <- return [TLComment [cmt]]
>    (v, code) <- cgLC [] exp lcs
>    return (v, comment ++ code)

%if False

> cg x = pattErr "cg" x

%endif

%% ==============================================================================

The function |cgLC| has three arguments. The variable |bind| stores
the bindings of the algebra function's variables to those of the
corresponding qualifiers. The variable |exp| stores the definition of
the algebra function. The third argument is the list of qualifiers for
the list comprehension.

A filter-qualifier is translated into an If-construct of the target
language:

%if False

> cgLC bind exp ((Filter filterExp):lcs) = 
>  do
>    (v1,code1) <- cgLC bind exp lcs
>    chc   <- getChoice
>    code2 <- return [TLIf filterExp code1 [TLAssign v1 (TLExp (ExpNum (nilval chc)))]]
>    return (v1,code2)

%endif

< cgLC bind exp ((Filter filterExp):lcs) = 
<  do
<    (v1,code1) <- cgLC bind exp lcs
<    chc   <- getChoice
<    code2 <- return [TLIf filterExp code1 [TLAssign v1 (nilval chc)]]
<    return (v1,code2)


An enumeration generator is translated into a loop of the target
language:

%if False

> cgLC bind exp ((Generator (ExpVar k) (LCEnum (ExpME from) (ExpME to))):lcs) =
>  do 
>    (v1,code1) <- cgLC bind exp lcs
>    addLoopVar k
>    v2    <- newTempVar TLInt
>    chc   <- getChoice
>    code2 <- return [TLAssign v2 (TLExp (ExpNum (nilval chc))), 
>                     TLFor k from to (code1 ++ cgChoiceSingleElement v2 chc v2 v1)]
>    return (v2,code2)

%endif

< cgLC bind exp ((Generator (ExpVar k) (LCEnum (ExpME from) (ExpME to))):lcs) =
<  do 
<    (v1,code1) <- cgLC bind exp lcs
<    addLoopVar k
<    v2    <- newTempVar TLInt
<    chc   <- getChoice
<    code2 <- return [TLAssign v2 (nilval chc), 
<                     TLFor k from to (code1 ++ cgChoiceSingleElement v2 chc v2 v1)]
<    return (v2,code2)


All other generators are recursively compiled via |cg lcexp| and the
result variable of this compilation (|v1|) is bound to the
corresponding generator-variable (|a|).

%if False

> cgLC bind exp ((Generator (ExpVar a) lcexp):lcs) =
>  do
>    (v1,code1) <- cg lcexp
>    bind2      <- return [(a, ExpTLVar (fst v1))]
>    (v2,code2) <- cgLC (bind ++ bind2) exp lcs
>    return (v2,code1 ++ code2)

%endif

< cgLC bind exp ((Generator (ExpVar a) lcexp):lcs) =
<  do
<    (v1,code1) <- cg lcexp
<    bind2      <- return [(a, v1)]
<    (v2,code2) <- cgLC (bind ++ bind2) exp lcs
<    return (v2,code1 ++ code2)


If the list of qualifiers is empty, the actual body of the list
comprehension can be produced.  The function |insertVarbinds| replaces
all occurences of the qualifier variables by the corresponding
variables of the target language:

> cgLC bind exp [] = 
>  do 
>    v <- newTempVar TLInt
>    code <- return [TLAssign v (TLExp (insertVarBinds bind exp))]
>    return (v, code)

%if False

> cgLC _ _ x = pattErr "cgLC" x


> nilval chc = case chc of
>             "maximum" -> -6500000.0
>             "minimum" ->  6500000.0
>             "sum"     ->  0.0


> ppV = ppVarAccessC

> gettype :: VarAccess -> DataType
> gettype (v,dt) = dt

> cgChoiceSingleElement v@(_,dt) chc v1n@(v1,_) v2n@(v2,_) 
>   | elem dt [TLInt, TLReal] =  case chc of
>     "maximum" -> [assignIF v (ExpIOp (ExpTLVar v1) ">" (ExpTLVar v2)) (tlvar v1) (tlvar v2)]
>     "minimum" -> [assignIF v (ExpIOp (ExpTLVar v1) "<" (ExpTLVar v2)) (tlvar v1) (tlvar v2)]
>     "sum"     -> [TLAssign v (TLExp (ExpIOp (ExpTLVar v1) "+" (ExpTLVar v2)))]
>     x         -> [commentBox ( ["ERROR: cgChoiceSingleElement: undefined pattern",
>                                 ppV v ++ " = " ++ chc ++ "(" ++ ppV v1n ++ ", " ++ ppV v2n ++ ")"])]

> -- fuer Effizienzvergleiche:
> assIF = "std"
> assignIF v exp v1 v2 | assIF == "std" = TLAssignIF v exp v1 v2
>                      | otherwise      = TLIfs exp [TLAssign v v1] [TLAssign v v2]

%endif

For our example, the function |cg| generates to following target code:

\begin{numberedsource}
static void calc_alignment(int i, int j)
{
   int v1, v2, v3, v4, v5, v6, v7;

   /* nil <<< char '$' */
   if ((j-i) == 1) {
      if (z[j] == '$') {
         v1 = 0;
      }
      else {
         v1 = INTMIN;
      }
   }
   else {
      v1 = INTMIN;
   }
   /* d <<< achar ~~~ alignment */
   if ((j-i) >= 2) {
      v2 = tbl_alignment(i+1, j) - 1;
   }
   else {
      v2 = INTMIN;
   }
   v3 = v1 > v2 ? v1 : v2;
   /* i <<< alignment ~~~ achar */
   if ((j-i) >= 2) {
      v4 = tbl_alignment(i, j-1) - 1;
   }
   else {
      v4 = INTMIN;
   }
   v5 = v3 > v4 ? v3 : v4;
   /* r <<< achar ~~~ alignment ~~~ achar */
   if ((j-i) >= 3) {
      v6 = (z[i+1] == z[j]) ? 
         tbl_alignment(i+1, j-1) + 1 : 
         tbl_alignment(i+1, j-1) - 1 ;
   }
   else {
      v6 = INTMIN;
   }
   v7 = v5 > v6 ? v5 : v6;
   tbl_alignment(i, j) = v7;
}
\end{numberedsource}

%if False

\subsection{State Monad}
%% ==============================================================================

> type MType_LC = ([VarAccess],[String],String)

> data SM_LC a = SM_LC (MType_LC -> (a,MType_LC))  -- The monadic type

> instance Monad SM_LC where
>   -- defines state propagation
>   SM_LC c1 >>= fc2         =  SM_LC (\s0 -> let (r,s1) = c1 s0 
>                                                 SM_LC c2 = fc2 r in
>                                              c2 s1)
>   return k              =  SM_LC (\s -> (k,s))

> -- run a computation in the SM monad
> runSM_LC                   :: MType_LC -> SM_LC a -> (a,MType_LC)
> runSM_LC s0 (SM_LC c)         =  c s0

> newTempVar :: DataType -> SM_LC VarAccess
> newTempVar dt = SM_LC (\(tempvars, loopvars, chc) -> let 
>                                                    tvN n = Direct ("v" ++ show n)
>                                                    v    = (tvN (length tempvars + 1), dt)
>                                                 in (v, (v:tempvars, loopvars, chc)))

> addLoopVar :: String -> SM_LC ()
> addLoopVar v = SM_LC (\(tempvars, loopvars, chc) -> ((), (tempvars, v:loopvars, chc)))

> changeChoice :: String -> SM_LC ()
> changeChoice chc = SM_LC (\(tempvars, loopvars, _) -> ((), (tempvars, loopvars, chc)))

> getChoice :: SM_LC String
> getChoice = SM_LC (\(tempvars, loopvars, chc) -> (chc, (tempvars, loopvars, chc)))

%endif

