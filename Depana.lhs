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

> module Depana(

>   da,
>   ppDepTab,
>   ppDepTabTex,
>   rev_Depana,
>   sortDeps,
>   tabulateDeps

> ) where

> import Tools
> import StringLib
> import MathExp
> import Syntax
> import Dss
> import Yieldsize
> import Track

> rev_Depana =  "$Revision$"

%endif

%format d_p        = " d_p"
%format d_q        = " d_q"

%format l_r        = " l_r"
%format l_l        = " l_l"

%format nt'        = " \bar{nt}"

%\newpage

%----------------------------------------------------------------------------------------------------
\section{Abhängigkeitsanalyse}
\label{sec:depana}
%----------------------------------------------------------------------------------------------------

\begin{figure}[htbp]
\begin{fminipagec}
\textbf{Überblick: Abhängigkeitsanalyse}

Die Funktion |da| ermittelt die Abhängigkeit zwischen zwei Produktionen.

< da : {-"\text{ dependency analysis}"-}
< da q (l_l, l_r) ysc_p u_p -> (ys_p, p -> q)

\begin{tabular}{p{6cm}p{6cm}}
\textit{Eingaben}               & \textit{Ausgaben} \\
|q|: gesuchtes Nichtterminal    & |ys_p|: Kranzlänge von |p| \\
|l_l|: linksseitige Kranzlänge  & |p -> q|: Abhängigkeit zwischen |p| und |q| \\
|l_r|: rechtsseitige Kranzlänge &                                             \\
|ysc_p|: Kranzlängenbeschränkung für die Produktion |p| &                     \\
|u_p|: Produktionsdefinition     &
\end{tabular}
    
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Die Abhängigkeitsermittlungsfunktion |da|}
\label{fig:def:da}
\end{figure}


%if code

da: Abhängigkeit zwischen n und Unit: u --> n

> da :: UserFunctions -> TModes -> YSizes -> String -> YSize -> YSize -> Unit -> (YSize,Bool)

%endif

Vor der Ausgabe des Zielprogramms muß die Berechnungsreihenfolge für
die Rekurrenzen ermittelt werden. Es muß sichergestellt sein, daß ein
Wert bei seiner Verwendung bereits berechnet wurde. Bei funktionalen
Programmiersprachen ist dies ein integraler Bestandteil des
Ausführungsmodells, bei imperativen Sprachen nicht und muß daher bei
der Übersetzung berücksichtigt werden.

In Produktionen kann nur auf Worte von gleicher oder kleinerer Länge
zugegriffen werden. Durch die Konstruktion der Schleifen können wir
weiterhin gewährleisten, daß die Berechnung mit aufsteigender Länge
der Eingabeworte durchgeführt wird. Für die Abhängigkeitsanalyse
genügt es also, nach denjenigen Ausdrücken zu suchen, in denen auf
Ergebnisse in der gleichen Schleifeniteration zugegriffen wird.

Abbildung \ref{fig:da1} zeigt ein Beispiel. An den Ergebnissen der
Indexermittlung ist sofort ersichtlich, daß p3 auf jeden Fall
\textbf{vor} $p1$ und $p2$ berechnet werden muß, da jeweils das
Ergebnis der gleichen Iteration verwendet wird ($p3!(i,j)$). Für das
Auftreten von $p1$ innerhalb von $p2$ erschließt sich dieses erst
durch genaueres Untersuchen der beiden inneren Schleifen, aber auch
hier ist eine Abhängigkeit gegeben. Alle anderen Fälle sind für die
Abhängigkeitsanalyse irrelevant. Durch eine geeignete Sortierung
dieser Abhängigkeiten ergibt sich die notwendige
Berechnungsreihenfolge $p3, p1, p2$.

\begin{figure}[htbp]
\begin{fminipagec}

\begin{Code}



 p1 = tabulated(f1 <<< achar ~~~ p p2 ~~~ achar       \hs{|||}
                f2 <<< empty                          \hs{|||}
                p p3)

 p2 = tabulated(f3 <<< astring ~~~ p p1 ~~~ astring   \hs{|||}
                p p3) 

 p3 = tabulated(f4 <<< achar   ~~~ p p1) 

\end{Code}

\rule{\linewidth}{1pt}

Indexermittlung:

\begin{Code}


p1!(i, j) =

   f1(achar(i, i+1), p2!(i+1, j-1), achar(j-1, j))
   ++
   f2(empty(i, j))
   ++
   p3!(i, j)

p2!(i, j) =

   for k2 = i to j do
      for k = i to k2 do
         f3(astring(i, k), p1!(k, k2), astring(k2, j))
   ++
   p3!(i, j)

p3!(i, j) =

   f4(achar(i, i+1), p1!(i+1, j))

\end{Code}

\rule{\linewidth}{1pt}

Abhängigkeiten:

\begin{center}
\begin{tabular}{c||c||c||c}
$\rightarrow$ & p1&p2&p3\\
\hline
p1&&x&\\
p2&&&\\
p3&x&x&\\
\end{tabular} 
$\Rightarrow$ Berechnungsreihenfolge: p3, p1, p2
\end{center}


\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Beispiel für eine Abhängigkeitsanalyse}
\label{fig:da1}
\end{figure}



$p \rightarrow q$ bezeichne eine Verwendung der Produktion $q$ in der
Produktion $p$ in der gleichen Schleifeniteration und impliziert
damit, daß die Produktion $q$ vor $p$ berechnet werden muß. Das
Verfahren orientiert sich dabei an der Indexermittlung. Anstelle der
Indizes werden hier nun die links- bzw. rechtsseitigen Kranzlängen
$l_l$ und $l_r$ berechnet und durchgereicht. Im Falle eines
Nichtterminals |nt'| wird anhand dieser dann überprüft, ob ein Zugriff
in der gleichen Schleifeniteration, also in der Form |nt'!(i,j)|
stattfinden kann.

Sei $p$ gegeben durch $p = u_p$.

$p \rightarrow  q \quad$ gdw $\quad$ |da q (0, 0) (0,Infinite) u_p| $\qquad$
mit

\renewcommand{\aeq}{$\>\hspace*{1.7cm}$}

> da ufs tms ysizes nt borders ysc_unit (Terminal t)      {-"\aeq"-} = (ys_t <^> ysc_unit, False)

\vspace{ -1.7\baselineskip}
%if code

>    where
>    (ys_t, _ , _, _) = termDef ufs t

%endif

> da ufs tms ysizes nt borders ysc_unit (Nonterminal nt') {-"\aeq"-} = (ys_nt' <^> ysc_unit , nt == nt' && zeroBorder)

\vspace{ -1.7\baselineskip}
%if code

>    where
>    zeroBorder = case borders of
>                  ST (l_l, l_r)                ->  l_l == (Number 0) && l_r == (Number 0)
>                  TT (l1_l, l1_r) (l2_l, l2_r) ->  l1_l == (Number 0) && l1_r == (Number 0) && l2_l == (Number 0) && l2_r == (Number 0)
>    ys_nt'   =  head' [y | (n,y) <- ysizes, n == nt']    
>                           ("unknown nonterminal " ++ nt') 

%endif
\renewcommand{\aeqb}{$\>\hspace*{ -2.5cm}$}

Single-Track:

> da ufs tms ysizes nt (ST (l_l, l_r)) ysc_unit ((p, ysc_p) :~~ (q, ysc_q)) {-"\aeq"-} = (ys_unit, d_p || d_q)
>    where
>    (ys_p, d_p) {-"\aeqb"-} = da ufs tms ysizes nt (ST (l_l, l_r `plus'` l_q)) ysc_p p
>    (ys_q, d_q) {-"\aeqb"-} = da ufs tms ysizes nt (ST (l_l `plus'` l_p, l_r)) ysc_q q
>    ys_unit     {-"\aeqb"-} = (ys_p <+> ys_q) <^> ysc_unit

\vspace{ -1.7\baselineskip}
%if code

>    ST (l_p, u_p) = ys_p
>    ST (l_q, u_q) = ys_q

%endif

Two-Track:

> da ufs tms ysizes nt (TT (l1_l, l1_r)(l2_l, l2_r)) ysc_unit ((p, ysc_p) :~~ (q, ysc_q)) {-"\aeq"-} = (ys_unit, d_p || d_q)
>    where
>    (ys_p, d_p) {-"\aeqb"-} = da ufs tms ysizes nt (TT (l1_l, l1_r `plus'` l1_q) (l2_l, l2_r `plus'` l2_q)) ysc_p p
>    (ys_q, d_q) {-"\aeqb"-} = da ufs tms ysizes nt (TT (l1_l `plus'` l1_p, l1_r) (l2_l `plus'` l2_p, l2_r)) ysc_q q
>    ys_unit     {-"\aeqb"-} = (ys_p <+> ys_q) <^> ysc_unit

\vspace{ -1.7\baselineskip}
%if code

>    TT (l1_p, u1_p) (l2_p, u2_p) = ys_p
>    TT (l1_q, u1_q) (l2_q, u2_q) = ys_q

%endif

> da ufs tms ysizes nt (TT (l1_l, l1_r)(l2_l, l2_r)) ysc_unit ((p, ysc_p) :/\\/ (q, ysc_q, d)) {-"\aeq"-} = (ys_unit, d_p || d_q)
>    where
>    (ys_p, d_p) {-"\aeqb"-} = da ufs tms ysizes nt (TT (l1_l, l1_r `plus'` l1_q) (l2_l, l2_r `plus'` l2_q)) ysc_p p
>    (ys_q, d_q) {-"\aeqb"-} = da ufs tms ysizes nt (TT (l1_l `plus'` l1_p, l1_r) (l2_l `plus'` l2_p, l2_r)) ysc_q q
>    ys_unit     {-"\aeqb"-} = (ys_p <+> ys_q) <^> ysc_unit

\vspace{ -1.7\baselineskip}
%if code

>    TT (l1_p, u1_p) (l2_p, u2_p) = ys_p
>    TT (l1_q, u1_q) (l2_q, u2_q) = ys_q

%endif


> da ufs tms ysizes nt borders ysc_unit ((p, la_p) :^^^ (q, la_q)) =  da ufs tms ysizes nt borders ysc_unit ((p, ysc_lap) :~~ (q, ysc_laq))

\vspace{ -1.7\baselineskip}
%if code

>   where
>   ysc_lap  = laBounds ufs la_p
>   ysc_laq  = laBounds ufs la_q

%endif
\renewcommand{\aeq}{$\>\hspace*{0.5cm}$}

> da ufs tms ysizes nt borders ysc_unit  (p :~~~  q) {-"\aeq"-} = da ufs tms ysizes nt borders ysc_unit ((p, initys) :~~  (q, initys)) 
>   where
>   initys = case (tma_u tms p, tma_u tms q) of
>     (MST, MST) ->  ST (Number 0, Infinite)
>     (MTT, MTT) ->  TT (Number 0, Infinite) (Number 0, Infinite)
>     otherwise  ->  error "error in mixed single-/two-track definitions!"

> da ufs tms ysizes nt borders ysc_unit (f :<<< p)   {-"\aeq"-} = da ufs tms ysizes nt borders ysc_unit p
> da ufs tms ysizes nt borders ysc_unit (p `With` f) {-"\aeq"-} = da ufs tms ysizes nt borders (ysc_f <^> ysc_unit) p

\vspace{ -1.7\baselineskip}
%if code

>    where  
>    (ysc_f, _, _) = filterDef ufs f

%endif

> da ufs tms ysizes nt borders ysc_unit (p :||| q) {-"\aeq"-} =  (ys_p <|> ys_q, d_p || d_q)
>    where
>    (ys_p, d_p) = da ufs tms ysizes  nt  borders  ysc_unit p
>    (ys_q, d_q) = da ufs tms ysizes  nt  borders  ysc_unit q

> da ufs tms ysizes nt (TT ijs1 ijs2) (TT ysc_unit1 ysc_unit2) (TTUnit p q) {-"\aeq"-} =  (ys_unit, d_p || d_q)
>    where
>    (ST ys_p, d_p) = da ufs tms ysizes  nt  (ST ijs1) (ST ysc_unit1) p
>    (ST ys_q, d_q) = da ufs tms ysizes  nt  (ST ijs2) (ST ysc_unit2) q
>    ys_unit        = TT ys_p ys_q

> da ufs tms ysizes nt borders ysc_unit (ListCompr lc _ ys_lc)  {-"\aeq"-} = (ys_lc <^> ysc_unit, False)

> da ufs tms ysizes nt borders ysc_unit (p :... f) {-"\aeq"-} = da ufs tms ysizes nt borders ysc_unit p

Die Berechnungsreihenfolge wird mittels einer topologischen Sortierung
auf $\rightarrow$ ermittelt. $P$ sei die Menge der Produktionen und
$D$ eine Liste der in der notwendigen Berechnungsreihenfolge
sortierten Produktionen. 

\begin{tabbing}
\qquad\=\hspace{\lwidth}\=\hspace{\cwidth}\=\+\kill
$\hskip10.50em\relax\Conid{D} \;=\;[\mskip1.5mu \mskip1.5mu]$\\
$\hskip10.50em\relax\text{while}\;\Conid{P}\not= \{\mskip1.5mu \mskip1.5mu\}$\\
$\hskip10.50em\relax\hskip1.00em\relax\Conid{R} \;=\;\{\mskip1.5mu \Varid{p}\mid \Varid{p} ,\;\Varid{q}\in \Conid{P} ,\;\Varid{p}\not\to \Varid{q}\mskip1.5mu\}$\\
$\hskip10.50em\relax\hskip1.00em\relax\Conid{D} \;=\;\Conid{D}\plus \Conid{R}$\\
$\hskip10.50em\relax\hskip1.00em\relax\Conid{P} \;=\;\Conid{P}\backslash \Conid{R}$
\end{tabbing}

Im Falle zirkulär voneinander abhängiger Produktionen terminiert das
Verfahren in dieser Form nicht. Durch eine zusätzliche Überprüfung auf
$R=\{\}$ können wir dieses allerdings abfangen und eine entsprechende
Fehlermeldung generieren. Die Eingabe liefert in diesem Fall keine
endlichen Ableitungen.

Eine Definition der Abhängigkeitsananalyse auf Parsermengen findet
sich in \cite{GIE:STE:2002}.


%if code

Tabellierung der Abhängigkeiten:

> tabulateDeps ::  UserFunctions -> TModes -> YSizes -> [Prod] -> [(Nt, Nt, Bool)] 
> tabulateDeps ufs tms ysizes x = [(n', n,  snd(daCall n n' u')) | (n :=== _) <- x, (n' :=== (_, _, _, u')) <- x]
>  where daCall n n' u' = case getTMode tms n' of
>                           MST -> da ufs tms ysizes n (ST (Number 0, Number 0)) (ST (Number 0, Infinite)) u'
>                           MTT -> da ufs tms ysizes n (TT (Number 0, Number 0) (Number 0, Number 0))
>                                                      (TT (Number 0, Infinite) (Number 0, Infinite)) u'

Sortierung:

> sortDeps :: [Prod] -> [(Nt, Nt, Bool)] -> [Nt]
> -- sortDeps [(n :=== _)] _ = [n]   -- ein einzelnes NT muss nicht sortiert werden
> sortDeps ps deps = sortDeps' nps deps where
>                       nps = map (\(n :=== _) -> n) ps

> sortDeps' _ [] = []
> sortDeps' ps deps = ret where 

>              canGo = filter (noDep deps) ps

>              newdeps = [ (n, n', b) | (n, n', b) <- deps, notElem n canGo, notElem n' canGo ]
>              newps   = [ p | p <- ps, notElem p canGo ]
>              ret = if newdeps == deps
>                      then [] -- error "cannot solve dependencies in source program" 
>                      else canGo ++ sortDeps' newps newdeps

>              noDep [] _                = True
>              noDep ((n, _, b): recs) x = ((n == x && not b) || n /= x) && noDep recs x


Prettyprinter nach Text und Latex:

> ppDepTab deps = concatMap ppDepTab' deps where
>   ppDepTab' (n, n', True) = n ++ " -> " ++ n' ++ "\n"
>   ppDepTab' _             = ""


> ppDepTabTex :: [Prod] -> [(Nt, Nt, Bool)] -> String
> ppDepTabTex ps deps = header ++ body ++ trailer
>  where
>    nps = map (\(n :=== _) -> n) ps
>    lps = length nps

>    lins [] []     = []
>    lins (p:ps) ds = ppn p ++ "&" ++  sepList "&" (map (ppBool . thd3) (getf lps ds)) ++ "\\\\\n" ++ lins ps (dropf lps ds)

>    header = "\\begin{tabular}{" ++ sepList "|" (replicate (lps+1) "c") ++ "}\n" ++
>             "$\\rightarrow$ & " ++ sepList "&" (map ppn nps) ++ "\\\\\n\\hline\n"

>    body = lins nps deps
>    trailer = "\\end{tabular}\n"

>    ppBool False = ""
>    ppBool True = "x"

>    getf 0 _      = []
>    getf n (l:ls) = l:getf (n-1) ls

>    dropf 0  l     = l
>    dropf n (l:ls) = dropf (n-1) ls

>    ppn ""     = ""
>    ppn (x:xs) = (if x=='_' then "\\_" else [x]) ++ ppn xs

%endif