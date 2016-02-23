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
%include tex/Dss.fmt

%if code 

> module Dss(

>   DrecResult(..),
>   ILLCExp(..),
>   ILUnit(..),
>   VarName,
>   ILLoop, ILBounds, ILProd,
>   drecProd,
>   drecProds,
>   dss,
>   dss_test,
>   ilStdInd,
>   ppILBounds,
>   ppILBounds',
>   ppILP,
>   ppILProd,
>   rev_Dss

> ) where

> import Data.Char
> import Tools
> import Constants
> import StringLib
> import Parse
> import Syntax
> import Track
> import Yieldsize
> import TLData
> import MathExp
> import Expr
> import PrettyPrint

> import Adptrans    -- ADP-Transformationen


> rev_Dss =  "$Revision$"

%----------------------------------------------------------------------------------------------------
\subsection{Zwischensprache}
%----------------------------------------------------------------------------------------------------

> type ILProd     = (Nt, Int, ILUnit)
> type ILBounds   = (SubScripts, YSize)
> type ILLoop     = (VarName, MathExp, MathExp)

> data ILUnit = 
>   ILTerminal      Term      SubScripts                          |
>   ILNonterminal   Nt        SubScripts                          |   
>   ILUnit          :/~~~/    ILUnit                              |  
>   ILUnit          :/|||/    ILUnit                              |  
>   ILUnit          :/.../    Function                            |
>   ILUnit          `ILwith`  (Filter,SubScripts)                 |
>   (AutoAlgType, ILBounds, [ILLoop]) :/<<</ ILUnit               |
>   ILTTUnit        ILUnit    ILUnit                              |
>   ILListCompr     ILListCompr ILBounds
>        deriving (Eq, Show)


> type VarName = String
> type FctName = String
> type TypeName = String


> instance Pretty ILUnit where
>     pretty = ppILUnit 0
>     pretty' False = ppILUnitSimple -- for generating comments
>     pretty' True = ppILUnit 0

> printSimple :: PPOpt Bool
> printSimple = PPOptBool True

----------------------------------------------------------------------------------------------------

> type ILListCompr = (Exp, [ILLCExp])
> ppILListCompr ind (body, exprs)  = "[" ++ pretty body ++ " | " ++ mapsep (",\n" ++ spc (ind + stdInd)) ppILLCExp exprs ++ "]"

> data ILLCExp = ILLCExp Exp |
>                ILLCUnit Exp ILUnit               deriving (Show, Eq)
> ppILLCExp (ILLCExp e)    = pretty e
> ppILLCExp (ILLCUnit e u) = pretty e ++ " <- " ++ ppILU u 

> instance Pretty ILLCExp where
>     pretty = ppILLCExp

----------------------------------------------------------------------------------------------------

Prettyprinter:

> ppILP (n, lc, u) = n ++ "!(i, j) =\n"  ++ ppILU u ++ "\n"

> ppILU (ILTerminal    t s) = ppTerm t ++ ppSubScripts s
> ppILU (ILNonterminal a s) = a ++ "!" ++ ppSubScripts s
> ppILU (a :/~~~/ b)        = ppILU a ++ " :/~~~/ " ++ ppILU   b

{-"$\>\>$\hspace{5cm}"-} 

> ppILU (a :/|||/ b)            = ppILU a ++ "        " ++ "{-" ++ squ ++ "$\\>\\>$\\hspace{5cm}" ++ squ ++ "-}  :/|||/\n " ++ ppILU b where
>                           squ = [chr 34]
>                           bo  = [chr 123]
>                           bc  = [chr 125]

> ppILU (a :/.../ f)           =  ppILU  a ++ " :/.../ " ++ f

> ppILU (p `ILwith` ((f, a), s)) = ppILU p ++ " `ILwith` " ++  f ++ ppSubScripts s

> ppILU (((f, dts),  bnd, loops) :/<<</ u) = "(" ++ ppAlgAppl f ++ ", " ++ pploops loops ++ ") :/<<</ " ++ ppILU u ++ ldef loops where
>                       pploops [] = "[]"
>                       pploops l  = "loops"

>                       ldef [] = ""
>                       ldef l  = "\n    where loops = " ++ pploops' l ++ "\n"

>                       pploops' l  = "[" ++ sepList ", " (map pploops'' l) ++ "]"
>                       pploops'' (k, ks, ke) = "(" ++ k ++ ", " ++ pretty ks ++ ", " ++ pretty ke ++ ")"

> ppILU (ILTTUnit a b) = "tt(" ++ ppILU a ++ ", " ++ ppILU b ++ ")" 
> ppILU (ILListCompr lc _) = ppILListCompr 0 lc

-------------------------------

> ilStdInd :: Int
> ilStdInd = 3

> ppILProd :: ILProd -> String
> ppILProd (n, lc, u)                   =  -- "used loop variables: " ++ show lc ++ "\n\n" ++ 
>                                          n ++ "!(i, j) =\n\n" ++ spc ilStdInd ++ ppILUnit ilStdInd u ++ "\n"

> ppILUnit :: Int -> ILUnit -> String
> ppILUnit ind  (ILTerminal    t s)  = ppTerm t ++ ppSubScripts s
> ppILUnit ind  (ILNonterminal a s)  = a ++ "!" ++                    ppSubScripts s
> ppILUnit ind  (a :/~~~/ b)           = ppILUnit ind  a ++ ", " ++ ppILUnit ind  b

> ppILUnit ind (a :/|||/ b)            = sepList ("\n" ++ spc ind ++ "++\n" ++ spc ind) (map (ppILUnit ind) (flatAlt (a :/|||/ b)))
>                                        where
>                                          flatAlt (a :/|||/ b) = flatAlt a ++ flatAlt b
>                                          flatAlt a            = [a]

> ppILUnit ind  (a :/.../ f)           = f ++ "[\n"  ++ spc (ind + ilStdInd) 
>                                          ++ ppILUnit (ind + ilStdInd )  a 
>                                          ++ "\n" ++ spc ind ++ "]"

> ppILUnit ind  (p `ILwith` ((f, a), s)) = "if " ++ f ++ "(" ++ sepList' ", " a ++ ppSubScripts s ++ ")"
>                                                ++  " then {\n" ++ spc (ind+ilStdInd) 
>                                                ++ ppILUnit (ind+ilStdInd)  p 
>                                                ++ "\n" ++ spc ind ++  "}"

> ppILUnit ind  (((f, dts),  bnd, loops) :/<<</ u) = "if " 
>                                            ++ ppILBounds bnd ++ " then {\n"  ++ spc (ind +ilStdInd) 
>                                            ++ concatMap (ppILLoop (ind + ilStdInd)) loops 
>                                            ++ spc (ilStdInd * (max 0 ((length loops) - 1)))
>                                            ++ ppAutoAlgType (f, dts) ++ "\n" 
>                                            ++ spc (ind +ilStdInd) ++ spc (ilStdInd * (max 0 ((length loops) )))  
>                                            ++ ppAlgAppl f ++ "(" 
>                                            ++ ppILUnit (ind+ilStdInd)  u 
>                                            ++ ")\n" ++ spc ind ++ "}"

> ppILUnit ind  (ILTTUnit a b) = "tt(" ++ ppILUnit ind a ++ ", " ++ ppILUnit ind b ++ ")" 
> ppILUnit ind  (ILListCompr lc _) = ppILListCompr ind lc 

> ppILBounds :: ILBounds -> String
> ppILBounds bnd = pretty $ ppILBounds' bnd

> ppILBounds' (ST (bl, bu), ST (ysl, Infinite))  = ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME ysl)
> ppILBounds' (ST (bl, bu), ST (ysl, ysu)) 
>      | ysl == ysu = ExpIOp  (ExpME (calcME (bu :- bl))) "==" (ExpME ysl)
>      | otherwise  = ExpIOp  (ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME ysl)) "&&"
>                             (ExpIOp  (ExpME (calcME (bu :- bl))) "<=" (ExpME ysu))

> ppILBounds' (TT s1 s2, TT y1 y2) = ExpIOp (ppILBounds' (ST s1, ST y1)) "&&"
>                                              (ppILBounds' (ST s2, ST y2))

> ppILLoop :: Int -> ILLoop -> String
> ppILLoop ind (lv, begin, end) = "for " ++ lv ++ " = " ++ pretty begin 
>                                        ++ " to " ++ pretty end 
>                                        ++ " do\n" ++ spc (ind + ilStdInd)


--------------------------------------------

Simple prettyprinter for ILUnit
Used to create readable comments in target code:

> ppILUnitSimple :: ILUnit -> String
> ppILUnitSimple  (ILTerminal    t s)  = ppTerm t 
> ppILUnitSimple  (ILNonterminal a s)  = "p " ++ a 
> ppILUnitSimple  (a :/~~~/ b)         = pp' a ++ " ~~~ " ++ pp' b
>    where 
>      pp' p@(ILTerminal    t s) = ppILUnitSimple p
>      pp' p@(ILNonterminal a s) = ppILUnitSimple p
>      pp' p@(a :/~~~/ b)        = ppILUnitSimple p
>      pp' p                     = "(" ++ ppILUnitSimple p ++ ")"
> ppILUnitSimple (a :/|||/ b)          = mapsep " ||| " ppILUnitSimple (flatAlt (a :/|||/ b))
>                                        where
>                                          flatAlt (a :/|||/ b) = flatAlt a ++ flatAlt b
>                                          flatAlt a            = [a]

> ppILUnitSimple  (a :/.../ f)           = ppILUnitSimple a ++ " ... " ++ f

> ppILUnitSimple  (p `ILwith` ((f, a), s)) = ppILUnitSimple p ++ " `with` " ++ ppf (f,a)
>                                            where
>                                              ppf (f, []) = f
>                                              ppf (f, a)  = "(" ++ f ++ " " ++ sepList " " a ++ ")"

> ppILUnitSimple  (((f, dts),  bnd, loops) :/<<</ u) = ppAlgAppl f ++ " <<< " ++ ppILUnitSimple u
> ppILUnitSimple  (ILTTUnit a b) = "tt(" ++ ppILUnitSimple a ++ ", " ++ ppILUnitSimple b ++ ")" 
> ppILUnitSimple  (ILListCompr lc _) = ppILListCompr 0 lc


----------------------------------------------

> type LoopCount  = Int
> data DrecResult = Dr [ILLoop] LoopCount [SigArgument] (YSize, ILUnit) 
>      deriving Show

%endif

%----------------------------------------------------------------------------------------------------
\section{Indexermittlung}
\label{sec:dss}
%----------------------------------------------------------------------------------------------------

\begin{flushright}
  \emph{No subscripts, no errors! \cite {EVE:GIE:2000}}
\end{flushright}

Eine der großen Stärken der Algebraischen Dynamischen Programmierung
ist die Abwesenheit von Indizes. Wo keine Indizes verwendet werden
können, können auch keine Indizierungsfehler gemacht werden. In der
klassischen dynamischen Programmierung stellen diese ein
ernstzunehmendes Problem und eine häufige Fehlerquelle dar (s.a.
Kapitel \ref{chap:adpintro}).

Bei der Implementierung eines ADP Algorithmus in einer imperativen
Programmiersprache werden wir allerdings kaum auf ihre Verwendung
verzichten können. In den folgenden Abschnitten beschreiben wir daher
ein Verfahren zur Indexermittlung.

\subsection{Unterwortkonvention}
\label{sec:subwordconv}

Eine zentrale Eigenschaft der Algebraischen Dynamischen Programmierung
ist die Art des Zugriffs auf Unterworte der Eingabe. Sei $x$ ein Wort
der Länge $n$ mit $x = x_1...x_n$. Das Indexpaar |(i,j)| bezeichne ein
Unterwort von $x$ mit |(i,j)| = $x_{i+1}...x_j$. Daraus ergeben sich
einige interessante Eigenschaften:
\begin{alignat}{2}
 |(0,n)|          & |=| && x \nonumber \\
 |(0,k) ++ (k,n)| & |=| && x \nonumber \\
 ||\hs{(i,j)}||   & |=| && j-i \quad \mbox{ mit } i |<=| j\nonumber 
\end{alignat}

\paragraph{Beispiel:} $_0H_1E_2L_3L_4O_5$

Diese Art des Zugriffs mag auf den ersten Blick recht unübersichtlich
erscheinen, bei der Indexermittlung wird sich die Unterwortkonvention
aber als äußerst praktisch erweisen.

%format G_S        = "\mathcal{G}_{\mathcal{S}}"
%format pG_S       = "\phantom{\mathcal{G}_{\mathcal{S}}:}"
%format ILProd     = "\overset{\mathcal{S}}{\mathrm{Production}}"
%format ILUnit     = "\overset{\mathcal{S}}{\mathrm{Unit}}"
%format Subscripts = "\mathrm{Subscripts}"
%format Function   = "\mathrm{Function}"
%format Bounds     = "\mathrm{Bounds}"
%format Loop       = "\mathrm{Loop}"
%format MathExp    = "\mathrm{MathExp}"

\subsection{Zwischensprache}
\label{sec:il}

Bis zu dieser Stelle haben wir alle Transformationen auf der
abstrakten Syntax der Ausgangssprache (Abschnitt \ref{sec:syntax})
durchgeführt. Für die Indexermittlung gehen wir nun über in die
Zwischensprache $\mathcal{S}$. Diese beschreiben wir durch die
Grammatik |G_S|:
\begin{alignat}{3}
 |G_S|:    & \qquad & |ILProd|      & \; \rightarrow \quad && |ntID| =        |ILUnit|                                  \nonumber \\
           &        & |ILUnit|      & \; \rightarrow \quad && |ILTerminal|    \;\;|terminalID| \;\; |Subscripts|        \nonumber \\   
           &        &               & \; \rightarrow \quad && |ILNonterminal| \;\;|ntID|       \;\; |Subscripts|        \nonumber \\   
           &        &               & \; \rightarrow \quad && |ILUnit|        |:/~~~/|      |ILUnit|                    \nonumber \\   
           &        &               & \; \rightarrow \quad && |Function|      |:/<<</|      |ILUnit|                    \nonumber \\   
           &        &               & \; \rightarrow \quad && |ILUnit|        |`ILwith`|    |(filterID, Subscripts)|    \nonumber \\   
           &        &               & \; \rightarrow \quad && |ILUnit|         \hs{:/|||/}  |ILUnit|                    \nonumber \\
           &        &               & \; \rightarrow \quad && |ILUnit|        |:/.../|      |functionID|                \nonumber 
\end{alignat}
Gegenüber der abstrakten Ausgangssyntax fällt auf, daß die drei
Varianten des |#~~~| Kombinators (|#~~~|, |#~~| und |^^^|) nun nur
noch in einer Ausführung vorliegen. Diese werden wir im unten
beschriebenen Verfahren zusammenfassen. Zusätzlich ist für Terminale,
Nichtterminale und Filter jeweils ein Ausdruck für die Indizes
hinzugekommen (|Subscripts|).
\begin{alignat}{3}
           &        & |Subscripts|  & \; \rightarrow \quad && |(MathExp, MathExp)|                                      \nonumber \\
           &        & |MathExp|     & \; \rightarrow \quad && number                                                    \nonumber \\
           &        &               & \; \rightarrow \quad && varID                                                     \nonumber \\
           &        &               & \; \rightarrow \quad && |MathExp| \; |:+| \; |MathExp|                            \nonumber \\
           &        &               & \; \rightarrow \quad && |MathExp| \; |:-| \; |MathExp|                            \nonumber \\
           &        &               & \; \rightarrow \quad && |max MathExp MathExp|                                     \nonumber \\
           &        &               & \; \rightarrow \quad && |min MathExp MathExp|                                     \nonumber \\
           &        &               & \; \rightarrow \quad && |(MathExp)|                                               \nonumber 
\end{alignat}
Die Anwendung einer Algebrafunktion ist ebenfalls um einige
Informationen erweitert. Das Feld |Bounds| kann in späteren
Übersetzungsphasen zur Längenüberprüfung der Eingabe verwendet werden.
|Loop| enthält eine beliebige Anzahl neu erzeugter innerer Schleifen.
\begin{alignat}{3}
 |pG_S|    & \qquad & |Function|    & \; \rightarrow \quad && |(functionID, Bounds, Loop{-"^*"-})|                      \nonumber \\
           &        & |Bounds|      & \; \rightarrow \quad && |(Subscripts, YSize)|                                     \nonumber \\   
           &        & |Loop|        & \; \rightarrow \quad && |(varID, MathExp, MathExp)|                               \nonumber 
\end{alignat}

%if code

Together with the results from yield size analysis and the corresponding yield size operators

< (l_p, u_p) <+> (l_q, u_q)             = (l_p `plus'` l_q, u_p `plus'` u_q)
< (l_p, u_p) <|> (l_q, u_q)             = (min' l_p l_q, max' u_p u_q)
< (l_p, u_p) <^> (l_f, u_f)             = (max' l_p l_f, min' u_p u_f)
< cnst (l_x, u_x)                       = l_x == u_x

we obtain an elegant way for subscripts calculation:

%endif

\subsection{Einstieg}
\label{sec:dss:basics}


\begin{figure}[htbp]
\begin{fminipagec}
\textbf{Überblick: Indexermittlung (1)}

Die Funktion |dss| berechnet die Indizes und transformiert die
Ausgangssprache in die Sprache $\mathcal{S}$.

< dss : {-"\text{ derive subscripts}"-}
< dss (i,j) u_p -> (ys_p, u_ps)

\begin{tabular}{p{6cm}p{6cm}}
\textit{Eingaben}                & \textit{Ausgaben} \\
|(i,j)|: Indexausdrücke          & |ys_p|: Kranzlänge von |u_p| \\
|u_p|: Produktionsdefinition     & |u_ps|: Mit Indizes angereicherte Produktionsdefinition |u_p|
\end{tabular}
    
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Die Indexermittlungsfunktion |dss| (vereinfacht)}
\label{fig:def:dss1}
\end{figure}



% yield size constraint ausblenden:

%format ysc_p        = " "
%format ysc_q        = " "
%format ysc_f        = " "
%format ysc_unit     = " "
%format <^>          = " "

Für einen übersichtlichen Einstieg in die Beschreibung des Verfahrens
wollen wir uns zunächst auf die Behandlung von über den |#~~~|
Kombinator verketteten terminalen Parsern beschränken.

Sei $p$ eine Produktion mit $p = u_p$. Entsprechend Abschnitt
\ref{sec:tabellierung} gehen wir davon aus, daß wir letztlich eine
doppelte Schleife der Form

%{
%include tex/pascal.fmt

<    for j:=0 to n do
<       for i:=j to 0 do
<          u_ps(i, j)

%}

im imperativen Zielprogramm erzeugen wollen. Dabei sei |u_ps| die mit
Indizes angereicherte Version der Produktion $p$.  Wir initialisieren
das Verfahren daher mit |dss (i, j) u_p|. Wie schon in Abschnitt
\ref{ysa:optimize} werden dabei neben der Zielstruktur die
Kranzlängen mitgeführt.  \renewcommand{\aeq}{$\>\hspace*{ -.2cm}$}

< dss ufs tms ysizes loops lc (i, j) ysc_unit (Terminal t)  {-"\aeq"-} = Dr loops lc dts_unit (ys_t <^> ysc_unit, ILTerminal t (i,j))
< dss ufs tms ysizes loops lc (i, j) ysc_unit  (p :~~~  q)  {-"\aeq"-} = Dr loops_unit lc_unit dts_unit (ys_p <+> ys_q, p' :/~~~/ q')
<    where
<    Dr loops_p lc_p dts_p (ys_p, p') = dss ufs tms ysizes loops   lc   (i, j_p)  ysc_p  p
<    Dr loops_q lc_q dts_q (ys_q, q') = dss ufs tms ysizes loops_p lc_p (i_q, j)  ysc_q  q

Für die Indexvariablen $j_p$ und $i_q$ gibt es nun drei alternative
Möglichkeiten.  Falls $p$ eine konstante Kranzlänge aufweist, können
wir ohne weiteres $j_p = i + l_p$ setzen. Entsprechend sei $i_q=j-l_q$
für eine konstante Kranzlänge von $q$. Falls weder $p$ noch $q$ eine
konstante Kranzlänge besitzen, muß eine innere Schleife eingeführt
werden. Wir verwenden hier die Schleifenvariable $k$. Ansonsten können
wir aufgund der Unterwortkonvention auf jeden Fall $j_p=i_q$ setzen.
Dabei sei |cnst ys_p = cnst (l_p, u_p) = l_p == u_p|.
\begin{equation}
\label{eq:svars}
\begin{split}
&|j_p| = 
  \begin{cases}
  |k|                     & \text{falls } |not cnst ys_p && not cnst ys_q|\\
  |calcME i :+ l_p|       & \text{falls } |cnst ys_p|\\
  |i_q|                   & \text{sonst}\hspace{9cm}
  \end{cases}\\
\\
&|i_q| =
  \begin{cases}
  |k|                     & \text{falls } |not cnst ys_p && not cnst ys_q|\\
  |calcME j :- l_q|       & \text{falls } |cnst ys_q|\\
  |j_p|                   & \text{sonst}\hspace{9cm}
  \end{cases}
\end{split}
\end{equation}

Die Schleifengrenzen einer neuen inneren Schleife $k$ ergeben sich aus
dem Überlappungsbereich der beiden Parser $p$ und $q$. Abbildung
\ref{fig:iterationk} soll dieses verdeutlichen. Man beachte auch die
direkte Korrespondenz zu den Definitionen der verschiedenen
Variationen des |#~~| Kombinators (Abbildung
\ref{fig:concretelengthnextcombinators}):
\begin{equation}
\label{eq:loopbnds}
\begin{split}
&|k_start| =
  \begin{cases}
  |i :+ l_p| & \text{falls } |u_q == Infinite|\\
  |max (i :+ l_p) (j :- u_q)| & \text{sonst }\hspace{6cm}
  \end{cases}\\
\\
&|k_end| =
  \begin{cases}
  |j :- l_q| & \text{falls } |u_p == Infinite|\\
  |min (i :+ u_p) (j :- l_q)| & \text{sonst }\hspace{6.4cm}
  \end{cases}
\end{split}
\end{equation}

\begin{figure}[h]
\begin{fminipagec}

\begin{center}
\qquad |pppppppppppppppppppppppppppppppppp|        \hfill \phantom{$q$}      \\
           \hfill\hspace*{4cm} |qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq| \hfill \phantom{|p|}         \\
$_||\!$\hrulefill $_||$    \\
\quad |i + l_p| \phantom{|ppppppppppppppppp|} |j - u_q| \phantom{|ppp|} |i + u_p| \phantom{|ppppppppp|} \hfill |j - l_q| \phantom{|ppp|}\\
$i$ \hfill $\underset{k}{_< .................. _>}$ \hfill $j$ 
\end{center}

\begin{center}
\begin{tabular}{ll}
|k >= i + l_p| & \qquad |k <= j - l_q|\\
|k >= j - u_q| & \qquad |k <= i + u_p|
\end{tabular}\\
\bigskip

$\Longrightarrow |k <- [max (i + l_p, j - u_q) ... min (i+u_p, j - l_q)]|$

\end{center}

\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Iterationsbereich einer Schleife $k$}
\label{fig:iterationk}
\end{figure}

\begin{figure}[h]
\begin{fminipagec}

< (~~) :: (Int,Int) -> (Int,Int) -> Parser (a -> b) -> Parser a -> Parser b 
< (~~) (l_p, u_p) (l_q, u_q) p q (i, j) 
<      = [x y | k <- [max (i + l_p) (j - u_q) .. min (i + u_p) (j - l_q)], 
<         x <- p (i, k), y <- q (k, j)] 

< (~~*) :: (Int,Int) -> Int -> Parser (a -> b) -> Parser a -> Parser b 
< (~~*) (l_p, u_p) l_q p q (i, j) 
<      = [x y | k <- [(i + l_p) .. min (i + u_p) (j - l_q)], 
<               x <- p (i, k), y <- q (k, j)] 

< (*~~) :: Int -> (Int,Int) -> Parser (a -> b) -> Parser a -> Parser b 
< (*~~) l_p (l_q, u_q) p q (i, j) 
<       = [x y | k <- [max (i + l_p) (j - u_q) .. (j - l_q)], 
<           x <- p (i, k), y <- q (k, j)] 

< (*~*) :: Int -> Int -> Parser (a -> b) -> Parser a -> Parser b 
< (*~*) l_p l_q p q (i, j) 
<       = [x y | k <- [(i + l_p) .. (j - l_q)], 
<                x <- p (i, k), y <- q (k, j)] 

\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Nextkombinatoren mit konkreten Längenvorgaben}
\label{fig:concretelengthnextcombinators}
\end{figure}

\paragraph{Beispiel 1.} 
Sei $p$ eine Produktion mit |p = achar ~~~ astringp|. Die Indizes für $p$
berechnen sich wie folgt:

\renewcommand{\aeq}{$\>\hspace*{1.9cm}$}

< dss (i,j) (achar ~~~ astringp)            {-"\aeq"-} = (ys_p <+> ys_q, p' :/~~~/ q')
<       where

<       (ys_p, p') = dss (i, j_p) achar     {-"\aeq"-} = ((1,1), achar (i, j_p))
<       (ys_q, q') = dss (i_q, j) astringp  {-"\aeq"-} = ((1, Infinite), astringp (i_q, j))
<                  
<   = ((2, Infinite), achar (i,j_p) :/~~~/ astringp (i_q,j))

Nach Gleichung \ref{eq:svars} gilt hier |j_p = i + l_p = i + 1| und |i_q = j_p = i+1|. Also:

< dss (i,j) (achar ~~~ astringp) 
<   = ((2, Infinite), achar (i,i+1) :/~~~/ astringp (i+1,j))

\paragraph{Beispiel 2.}
Für dieses Beispiel sei |p = astringp ~~~ astringp|:

< dss (i,j) (astringp ~~~ astringp)          {-"\aeq"-} = (ys_p <+> ys_q, p' :/~~~/ q')
<       where

<       (ys_p, p') = dss (i, j_p) astringp   {-"\aeq"-} = ((1, Infinite), astringp (i, j_p))
<       (ys_q, q') = dss (i_q, j) astringp   {-"\aeq"-} = ((1, Infinite), astringp (i_q, j))
<                  
<   = ((2, Infinite), astringp (i,j_p) :/~~~/ astringp (i_q,j))

Da weder |ys_p| noch |ys_q| konstant ist, muß eine Schleife eingeführt
werden. Die Schleifengrenzen berechnen sich dabei entsprechend
Gleichung \ref{eq:loopbnds} zu |k_start = i + l_p = i + 1| und |k_end
= j - l_q = j - 1|. Also:

< dss (i,j) (astringp ~~~ astringp)
<   = ((2, Infinite), astringp (i,k) :/~~~/ astringp (k,j)) {-"\qquad\!"-} where k <- [i+1 ... j-1]


\subsection{Kranzlängenbeschränkung}

%yield size constraint wieder einblenden:
%include tex/Dss.fmt

\begin{figure}[htbp]
\begin{fminipagec}
\textbf{Überblick: Indexermittlung (2)}

Die Funktion |dss| berechnet die Indizes und transformiert die
Ausgangssprache in die Sprache $\mathcal{S}$.

< dss : {-"\text{ derive subscripts}"-}
< dss (i,j) {-"\;"-} ysc_unit {-"\;"-} u_p -> (ys_p, u_ps)

\begin{tabular}{p{6cm}p{6cm}}
\textit{Eingaben}                            & \textit{Ausgaben} \\
|(i,j)|: Indexausdrücke                      & |ys_p|: Kranzlänge von |u_p| \\
|u_p|: Produktionsdefinition                 & |u_ps|: Mit Indizes angereicherte Produktionsdefinition |u_p|\\
|ysc_unit|: Kranzlängenbeschränkung für |u_p|
\end{tabular}
    
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Die Indexermittlungsfunktion |dss|}
\label{fig:def:dss2}
\end{figure}


\renewcommand{\aeq}{$\>\hspace*{1.4cm}$}

Für die Behandlung von Filterausdrücken ist es notwendig, die
Kranzlängenbeschränkung wieder mit in die Berechnung aufzunehmen. Die
Anwendung eines Filters $f$ auf einen Ausdruck $q$ mit Kranzlänge
|ys_q| resultiert in |ys_q <^> ysc_f| als neue Kranzlänge für $q$.
Wir erweitern dementsprechend die Funktion |dss| um ein zusätzliches
Argument |ysc_unit| für die Kranzlängenbeschränkung. Dieses wird mit
|(0, Infinite)| initialisiert.

> dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} (Terminal t)     {-"\aeq"-} = Dr loops lc dts_unit (ys_t <^> ysc_unit, ILTerminal t ijs)

\vspace{ -1.7\baselineskip}
%if code

>    where
>    ys_t               = fst4 $ termDef ufs t
>    dts_unit           = [snd4 $ termDef ufs t]

%endif

> dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} (Nonterminal nt) {-"\aeq"-} = Dr loops lc dts_unit (ys_nt <^> ysc_unit , ILNonterminal nt ijs)

%if code

>    where
>    dts_unit = [SigId "_Result"]
>    ys_nt    =  head' [y | (n,y) <- ysizes, n == nt]    
>                           ("unknown nonterminal " ++ nt) 

%endif

Filterausdrücke selbst können ebenfalls einer
Kranzlängenbeschränkung unterliegen. Daher wird diese für |(p
`With` f)| mit |ysc_f <^> ysc_unit| aktualisiert:

> dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} (p `With` f)  = Dr loops_p lc_p dts_p (ys_p, p' `ILwith` (f, ijs)) 
>    where  
>    Dr loops_p lc_p dts_p (ys_p, p') = dss ufs tms ysizes loops lc ijs (ysc_f <^> ysc_unit) p

%if code

>    (ysc_f, _, _) = filterDef ufs f

%endif

Für den |#~~| Kombinator werden die benutzerdefinierten Vorgaben
|ysc_p| und |ysc_q| entsprechend an $p$ und $q$ weitergegeben.  Die
Indexvariablen $j_p$ und $i_q$ berechnen sich wie in Gleichung
\ref{eq:svars}.
\renewcommand{\aeq}{$\>\hspace*{ -2.4cm}$}

Single-Track Next-Combinator
------------------------------

> dss ufs tms ysizes loops lc (ST (i, j)) {-"\;"-}ysc_unit{-"\;"-} ((p, ysc_p) :~~ (q, ysc_q)) = Dr loops_unit lc_unit dts_unit (ys_unit, p' :/~~~/ q')
>    where
>    Dr loops_p lc_p dts_p (ys_p, p') {-"\aeq"-} = dss ufs tms ysizes loops   lc   (ST (i, j_p))  {-"\;"-}ysc_p{-"\;"-}  p
>    Dr loops_q lc_q dts_q (ys_q, q') {-"\aeq"-} = dss ufs tms ysizes loops_p lc_p (ST (i_q, j))  {-"\;"-}ysc_q{-"\;"-}  q
>    ys_unit                          {-"\aeq"-} = (ys_p <+> ys_q) <^> ysc_unit

%if code

>    dts_unit = dts_p ++ dts_q

>    oneStepLoop = k_start == k_end      

>    j_p        | not (cnst ys_p) && not (cnst ys_q)  = if oneStepLoop then  k_start
>                                                                      else  Var k   
>               | cnst ys_p  = calcME (i :+ l_p)
>               | otherwise   = i_q


>    i_q        | not (cnst ys_p) && not (cnst ys_q)  = if  oneStepLoop then k_start
>                                                                       else Var k
>               | cnst ys_q  = calcME (j :- l_q)
>               | otherwise   = j_p

>    loops_unit     | not (cnst ys_p) && not (cnst ys_q) &&  not oneStepLoop = loop: loops_q  
>                   | otherwise                                              = loops_q

>    lc_unit        | not (cnst ys_p) && not (cnst ys_q) &&  not oneStepLoop = 1 + lc_q  
>                   | otherwise                                              = lc_q

>    loop       = (k, k_start, k_end)
>    k          = "k" ++ (if lc_q==0 then "" else show (lc_q+1)) 


>    k_start  = case u_q of
>                   Infinite  -> calcME      (i :+ l_p)                -- (b1)
>                   otherwise -> calcME (Max (i :+ l_p) (j :- u_q))    -- (b2)

>    k_end    = case u_p of
>                   Infinite  -> calcME                 (j :- l_q)     -- (e1)
>                   otherwise -> calcME (Min (i :+ u_p) (j :- l_q))    -- (e2)

>    ST (l_p, u_p) = ys_p
>    ST (l_q, u_q) = ys_q

%endif

Two-Track Next-Combinator
------------------------------

> dss ufs tms ysizes loops lc (TT (i1, j1) (i2, j2)) {-"\;"-}ysc_unit{-"\;"-} ((p, ysc_p) :~~ (q, ysc_q)) = Dr loops_unit lc_unit dts_unit (ys_unit, p' :/~~~/ q')
>    where
>    Dr loops_p lc_p dts_p (ys_p@(TT ys_p1 ys_p2), p') {-"\aeq"-} = dss ufs tms ysizes loops   lc   (TT (i1, j1_p) (i2, j2_p))  {-"\;"-}ysc_p{-"\;"-}  p
>    Dr loops_q lc_q dts_q (ys_q@(TT ys_q1 ys_q2), q') {-"\aeq"-} = dss ufs tms ysizes loops_p lc_p (TT (i1_q, j1) (i2_q, j2))  {-"\;"-}ysc_q{-"\;"-}  q
>    ys_unit                                          {-"\aeq"-} = (ys_p <+> ys_q) <^> ysc_unit

%if code

>    dts_unit = dts_p ++ dts_q

>    ((i1_q,j1_p), lc1,     loops1) = genLoop (i1,j1) ys_p1 ys_q1 lc_q
>    ((i2_q,j2_p), lc_unit, loops2) = genLoop (i2,j2) ys_p2 ys_q2 lc1

>    loops_unit = loops1 ++ loops2 ++ loops_q

>    genLoop (i,j) ys_p ys_q lc = ((i_q,j_p), lc', loop') 
>      where
>      (lc', loop') | not (cnstST ys_p) && not (cnstST ys_q) &&  not oneStepLoop = (lc+1, [loop])
>                   | otherwise                                              = (lc,   [])

>      oneStepLoop = k_start == k_end      

>      j_p        | not (cnstST ys_p) && not (cnstST ys_q)  = if oneStepLoop then  k_start
>                                                                        else  Var k   
>                 | cnstST ys_p  = calcME (i :+ l_p)
>                 | otherwise    = i_q


>      i_q        | not (cnstST ys_p) && not (cnstST ys_q)  = if  oneStepLoop then k_start
>                                                                         else Var k
>                 | cnstST ys_q  = calcME (j :- l_q)
>                 | otherwise    = j_p

>      loop       = (k, k_start, k_end)
>      k          = "k" ++ (if lc==0 then "" else show (lc+1)) 

>      k_start  = case u_q of
>                     Infinite  -> calcME      (i :+ l_p)                -- (b1)
>                     otherwise -> calcME (Max (i :+ l_p) (j :- u_q))    -- (b2)

>      k_end    = case u_p of
>                     Infinite  -> calcME                 (j :- l_q)     -- (e1)
>                     otherwise -> calcME (Min (i :+ u_p) (j :- l_q))    -- (e2)

>      (l_p, u_p) = ys_p
>      (l_q, u_q) = ys_q

%endif

Two-Track Coupled Next-Combinator /\\/
-------------------------------------------------

> dss ufs tms ysizes loops lc (TT (i1, j1) (i2, j2)) ysc_unit ((p,ysc_p) :/\\/ (q,ysc_q,d)) = Dr loops_unit lc_unit dts_unit (ys_unit, p' :/~~~/ q')
>    where
>    Dr loops_p lc_p dts_p (ys_p@(TT ys_p1 ys_p2), p')  = dss ufs tms ysizes loops   lc   (TT (i1, j1_p) (i2, j2_p))  ysc_p  p
>    Dr loops_q lc_q dts_q (ys_q@(TT ys_q1 ys_q2), q')  = dss ufs tms ysizes loops_p lc_p (TT (i1_q, j1) (i2_q, j2))  ysc_q  q
>    ys_unit                                            = (ys_p <+> ys_q) <^> ysc_unit

%if code

>    dts_unit = dts_p ++ dts_q

>    -- hier Einschraenkung: es muessen auf jeden Fall 2 Schleifen erzeugt werden,
>    -- fuer allgemeinere Faelle sind hier noch Aenderungen notwendig:
>    ((i1_q,j1_p), lc1,     loops1@[(k1,k1_start,k1_end)]) = genLoop (i1,j1) ys_p1 ys_q1 lc_q
>    ((i2_q,j2_p), lc_unit, loops2@[(k2,k2_start,k2_end)]) = genLoop (i2,j2) ys_p2 ys_q2 lc1


   = [f y | i1 <= j1, i2 <= j2, k1 <- [i1..j1], k2 <- [max (k1-d) i2..min (k1+d) j2], f <- r ((i1,k1),(i2,k2)), 
                                                                                      y <- q ((k1,j1),(k2,j2))]

>    loops2' = [(k2, (Max ((Var k1) :- (Number d)) k2_start), Min ((Var k1) :+ (Number d)) k2_end)] 

>    loops_unit = loops1 ++ loops2' ++ loops_q

>    genLoop (i,j) ys_p ys_q lc = ((i_q,j_p), lc', loop') 
>      where
>      (lc', loop') | not (cnstST ys_p) && not (cnstST ys_q) &&  not oneStepLoop = (lc+1, [loop])
>                   | otherwise                                                  = (lc,   [])

>      oneStepLoop = k_start == k_end      

>      j_p        | not (cnstST ys_p) && not (cnstST ys_q)  = if oneStepLoop then  k_start
>                                                                        else  Var k   
>                 | cnstST ys_p  = calcME (i :+ l_p)
>                 | otherwise    = i_q


>      i_q        | not (cnstST ys_p) && not (cnstST ys_q)  = if  oneStepLoop then k_start
>                                                                         else Var k
>                 | cnstST ys_q  = calcME (j :- l_q)
>                 | otherwise    = j_p

>      loop       = (k, k_start, k_end)
>      k          = "k" ++ (if lc==0 then "" else show (lc+1)) 

>      k_start  = case u_q of
>                     Infinite  -> calcME      (i :+ l_p)                -- (b1)
>                     otherwise -> calcME (Max (i :+ l_p) (j :- u_q))    -- (b2)

>      k_end    = case u_p of
>                     Infinite  -> calcME                 (j :- l_q)     -- (e1)
>                     otherwise -> calcME (Min (i :+ u_p) (j :- l_q))    -- (e2)

>      (l_p, u_p) = ys_p
>      (l_q, u_q) = ys_q

%endif


Den |#~~~| Kombinator können wir nun als Spezialfall des |#~~|
Kombinators behandeln:

> dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-}  (p :~~~  q) = dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} ((p, initys) :~~  (q, initys)) 
>   where
>   initys = case (tma_u tms p, tma_u tms q) of
>     (MST, MST) ->  ST (Number 0, Infinite)
>     (MTT, MTT) ->  TT (Number 0, Infinite) (Number 0, Infinite)
>     otherwise  ->  error "error in mixed single-/two-track definitions!"

Das gleiche gilt für den lookahead-Kombinator |^^^|. Die Ausdrücke
|ysc_la_p| und |ysc_la_q| resultieren in lookahead-Tabellenzugriffen
in den Schleifen der Zielsprache.

< dss ufs tms ysizes loops lc (i, j) {-"\;"-}ysc_unit{-"\;"-} ((p, la_p) :^^^ (q, la_q)) =  
<   dss ufs tms ysizes loops lc (i, j) {-"\;"-}ysc_unit{-"\;"-} ((p, ysc_la_p) :~~ (q, ysc_la_q))

%if code

LookAhead: Funktioniert nur fuer single track:
------------------------------------------------

> dss ufs tms ysizes loops lc (ST (i,j)) ysc_unit ((p, la_p) :^^^ (q, la_q)) = Dr loops_unit lc_unit dts_unit (ys_unit, p' :/~~~/ q')
>   where 
>   Dr loops_unit lc_unit dts_unit (tex_ys_unit, p' :/~~~/ q') = dss ufs tms ysizes loops lc (ST (i,j)) ysc_unit ((p, ysc_la_p) :~~ (q, ysc_la_q))

>   ysc_la_p = ST ((LA la_p LAL (i,j)), (LA la_p LAU (i,j)))
>   ysc_la_q = ST ((LA la_q LAL (i,j)), (LA la_q LAU (i,j)))
>   ys_lap  = laBounds ufs la_p
>   ys_laq  = laBounds ufs la_q
>   ys_unit = (ys_lap <+> ys_laq) <^> ysc_unit

%endif

Die während der Verarbeitung eines Audrucks $p$ aufgesammelten
Schleifen |tex_loops_p| werden an die Algebrafunktionsanwendung
|tex_f| gebunden. Zusätzlich werden die aktuellen Indizes |(i,j)|
zusammen mit der Kranzlänge |ys_p| gespeichert, um
Längenüberprüfungen in der Zielsprache zu ermöglichen.

> dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} (f :<<< p) = Dr loops lc_p dts_unit (ys_p, (tex_f, (ijs,ys_p), tex_loops_p) :/<<</ p')
>    where
>    Dr loops_p lc_p dts_p (ys_p, p') {-"\aeq"-} = dss ufs tms ysizes [] lc ijs {-"\;"-}ysc_unit{-"\;"-} p

%if code 

>    tex_loops_p = loops_p
>    tex_f       = (f, dts_p)
>    dts_unit    = [SigId "_Result"]


%endif

Über den \hs{#|||} Kombinator verbundene Ausdrücke werden vollständig symmetrisch behandelt:

> dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} (p :||| q) = Dr loops_q lc_q dts_unit (ys_unit, (p' :/|||/ q'))
>    where
>    Dr loops_p lc_p dts_p (ys_p, p') {-"\aeq"-} = dss ufs tms ysizes loops   lc   ijs  {-"\;"-}ysc_unit{-"\;"-} p
>    Dr loops_q lc_q dts_q (ys_q, q') {-"\aeq"-} = dss ufs tms ysizes loops_p lc_p ijs  {-"\;"-}ysc_unit{-"\;"-} q
>    ys_unit                          {-"\aeq"-} = ys_p <|> ys_q

%if code

>    dts_unit | dts_p == dts_q = dts_p
>             | otherwise = error ("type error in expression\n" ++
>                                  pretty p ++ "|||" ++ pretty q) 

%endif

Bleibt als letztes noch der Auswahlkombinator:

> dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} (p :... h) = Dr loops_p lc_p dts_p (ys_p, (p' :/.../ h))
>    where
>    Dr loops_p lc_p dts_p (ys_p, p') {-"\aeq"-} = dss ufs tms ysizes loops lc ijs {-"\;"-}ysc_unit{-"\;"-} p

Und der TT-Operator:

> dss ufs tms ysizes loops lc ijs@(TT ijs1 ijs2) (TT ysc_unit1 ysc_unit2) (TTUnit p q) = Dr loops_q lc_q dts_unit (ys_unit, (ILTTUnit p' q'))
>    where
>    Dr loops_p lc_p dts_p (ST ys_p, p') {-"\aeq"-} = dss ufs tms ysizes loops   lc   (ST ijs1) (ST ysc_unit1) p
>    Dr loops_q lc_q dts_q (ST ys_q, q') {-"\aeq"-} = dss ufs tms ysizes loops_p lc_p (ST ijs2) (ST ysc_unit2) q
>    ys_unit                             {-"\aeq"-} = TT ys_p ys_q
>    dts_unit | length dts_p /= 1 || length dts_q /= 1 = error $ "type error in expression\n" ++
>                                                                "tt(" ++ pretty p ++ ", " ++ pretty q ++ ")"
>             | otherwise = [SigTupel [head dts_p, head dts_q]]

DirectDef:

> dss ufs tms ysizes loops lc ijs ysc_unit (ListCompr (lc_e, lc_defs) iv ys_lc) = Dr loops lc dts_unit (ys_lc, (ILListCompr (lc_e, lc_defs') (ijs,ys_lc)))
>    where
>    lc_defs' = map dss_lc lc_defs
>    dss_lc (LCExp e)       = ILLCExp (insertVarBinds (bind ExpME iv ijs) e)
>    dss_lc (LCUnit e u ss) = ILLCUnit e u'
>      where Dr _ _ _ (_, u') = dss ufs tms ysizes loops lc (updateIndices (bind id iv ijs) ss) (initialConstraint ysc_unit) u

>    bind cf (ST (ni,nj)) (ST (ui,uj))                         = [(ni, cf ui), (nj, cf uj)]
>    bind cf (TT (ni1,nj1) (ni2,nj2)) (TT (ui1,uj1) (ui2,uj2)) = [(ni1, cf ui1), (nj1, cf uj1), (ni2, cf ui2), (nj2, cf uj2)]
>    bind _ _ _                                                = error "dss.bind: mixed ST/TT situation"

>    updateIndices bind (ST (i,j))             =  ST ((insertVarBindsME bind i),(insertVarBindsME bind j))
>    updateIndices bind (TT (i1,j1) (i2,j2))   =  TT ((insertVarBindsME bind i1),(insertVarBindsME bind j1))
>                                                    ((insertVarBindsME bind i2),(insertVarBindsME bind j2))
>    -- updateIndices _ _                         =  error "dss.updateIndices: mixed ST/TT situation"

>    -- hier setzen wir das yield-size constraint erstmal auf 0:
>    initialConstraint (ST _)   = (ST (Number 0, Infinite))
>    initialConstraint (TT _ _) = (TT (Number 0, Infinite) (Number 0, Infinite))

>    dts_unit    = [SigId "_Result"]


> dss _ _ _ _ _ _ _ p = pattErr "dss" p

\paragraph{Beispiel 3.} Wir wollen nun betrachten, wie sich der neue
Aspekt der Kranzlängenbeschränkung bei der Indexermittlung verhält.
Dazu erweitern wir die Produktion aus Beispiel 2 um den
längenbeschränkenden Filter |maxsize 7|.  Sei |p = astringp ~~~
(astringp `with` (maxsize 7))|.

\renewcommand{\aeq}{$\>\hspace*{6.9cm}$}

< dss (i,j) (0, Infinite) (astringp ~~~ (astringp `with` (maxsize 7))) = 
< dss (i,j) (0, Infinite) ((astringp, (0, Infinite)) ~~ ((astringp `with` (maxsize 7)), (0, Infinite))) =
<       (ys_p <+> ys_q, p' :/~~~/ q')
<       where

<       (ys_p, p') = dss (i, j_p) (0, Infinite) astringp  = ((1, Infinite), astringp (i, j_p))
<       (ys_q, q') = dss (i_q, j) (0, Infinite) (astringp `with` (maxsize 7))  
<                  = ((1, Infinite) <^> (0,7), astringp (i_q,j) `ILwith` ((maxsize 7) (i_q,j)))
<                  = ((1, 7), astringp (i_q,j) `ILwith` ((maxsize 7) (i_q,j)))

Wieder sind weder |ys_p| noch |ys_q| konstant.  Die Schleifengrenzen
berechnen sich dann entsprechend Gleichung \ref{eq:loopbnds} zu
|k_start = max (i + l_p) (j - u_q) = max (i + 1) (j - 7)| und |k_end =
j - l_q = j - 1|. Also:

< dss (i,j) (0, Infinite) (astringp ~~~ (astringp `with` (maxsize 7))) 
<   = ((2, Infinite), astringp (i,k) :/~~~/ (astringp (k,j) `ILwith` ((maxsize 7) (k,j)))) 
<       where k <- [max (i + 1) (j - 7) ... j-1]


%if code

> drecProds ufs tms ysizes = map (drecProd ufs tms ysizes)
> drecProd ufs tms ysizes p = (n, lc, p_u') 
>    where   

>    (n :=== (_, _, _, p_u)) = p

>    Dr loops lc dts (_,  p_u') = case getTMode tms n of
>                                   MST -> dss ufs tms ysizes [] 0 (ST ((Var "i"), (Var "j")))  (ST (Number 0, Infinite)) p_u
>                                   MTT -> dss ufs tms ysizes [] 0 (TT ((Var "i1"), (Var "j1")) ((Var "i2"), (Var "j2")))  
>                                                                  (TT (Number 0, Infinite) (Number 0, Infinite)) p_u

Testfunktionen:

> dss_test u = u'
>   where
>   Dr loops lc dts (_, u') = dss ([],[],[],[]) [] [] [] 0 ijs ysc u 
>   (ijs, ysc) = case (tma_u [] u) of
>     MST ->  (ST ((Var "i"), (Var "j")),ST (Number 0, Infinite))
>     MTT ->  (TT ((Var "i1"), (Var "j1")) ((Var "i2"), (Var "j2")),
>              TT (Number 0, Infinite) (Number 0, Infinite))


> file_dss f = do
>     inp <- readFile f
>     (ufs, _, (_,ps), _, _, _) <- return $ catchEr (parse inp)
>     ps                        <- return $ prodSourceToProd ps
>     tms                       <- return $ tmaProds [] ps
>     ysizes                    <- return $ head $ ysaProds ufs tms ps
>     der                       <- return $ drecProds ufs tms ysizes (mergeProds ufs [] [] ps) -- wenn tabelliert
>     sequence_ $ map (putStrLn. ppILProd) der

%endif



