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
%include tex/dss.fmt

%if code

> module Yieldsize(

>   (<+>), (<|>), (<^>),
>   cnst,
>   cnstST,
>   enc,
>   fix,
>   initYSize,
>   ppComb,
>   rev_Yieldsize,
>   trNext,
>   trNextVar,
>   trans,
>   ysaProd,
>   ysaProds,

> ) where

> import Constants
> import Tools
> import StringLib
> import MathExp
> import Syntax
> import Parse
> import Track
> import PrettyPrint

> rev_Yieldsize =  "$Revision$"

%endif

\cleardoublepage

\chapter{Kranzlängenanalyse}
\label{chp:ysa}

\section{Motivation}

In diesem Abschnitt beschreiben wir ein Verfahren zur Analyse der
Kranzlängen in ADP-$\mathcal{L}$-Programmen. Die hierbei gewonnenen Ergebnisse
können in mehreren Phasen der Übersetzung genutzt werden:

\begin{itemize}
\item Beim Entwurf der Rekurrenzen kann auf die Verwendung
  spezialisierter Next-Kombinatoren verzichtet werden. Bei größeren
  Programmen ist dieses relativ aufwendig und kann zu Fehlern führen.
  Mit Hilfe des hier beschriebenen Verfahrens ist es möglich, das
  Quellprogramm in einem Vorverarbeitungsschritt automatisch mit dem
  jeweils optimalen Kombinatorensatz auszustatten und wieder in
  ADP-Notation auszugeben. Der Entwickler eines Algorithmus hat somit
  die Möglichkeit, die Rekurrenzen unter Haskell ausgiebig zu testen,
  ohne sich über die Wahl der optimalen Kombinatoren Gedanken machen
  zu müssen. Darüberhinaus können wir ein Programm, in dem von
  vornherein spezialisierte Kombinatoren verwendet wurden, auf
  Korrektheit überprüfen (Abschnitt \ref{ysa:optimize}).
  
\item Das Wissen über die Kranzlängen ermöglicht eine elegante
  Bestimmung der Indizes bei der Ermittlung der Rekurrenzen für die
  imperative Zielsprache. Gleichzeitig unterstützt es das Finden
  optimaler Werte für die Bereiche der dabei erzeugten Schleifen.
  (Abschnitt \ref{sec:dss}).
  
\item Im Zusammenhang mit der Ausgabe des Zielprogramms muß die
  Berechnungsreihenfolge für die Rekurrenzen ermittelt werden. Es muß
  sichergestellt sein, daß ein Wert bei seiner Verwendung bereits
  berechnet wurde. Bei funktionalen Programmiersprachen ist dies ein
  integraler Bestandteil des Ausführungsmodells, bei imperativen
  Sprachen nicht. Die Ergebnisse der im folgenden beschriebenen
  Analyse können dazu genutzt werden, eine geeignete
  Berechnungsreihenfolge im Zielprogramm festzulegen (Abschnitt
  \ref{sec:depana}).
  
\item Als Nebeneffekt der Kranzlängenanalyse zeigen wir, daß (bzw.
  unter welchen Voraussetzungen) das Halteproblem für Kranzparser
  entscheidbar ist (Abschnitt \ref{sec:fixpoint}).

\end{itemize}

\section{Kranzlängenanalyse}
\label{sec:ysa}

%format ysa_u = " ysa_u"
%format ysa_p = " ysa_p"

Die Grundidee des Verfahrens basiert darauf, daß für terminale Parser
bekannt ist, welche Größe deren Ergebnisse annehmen können. Der Parser
\emph{astringp} beispielsweise liefert mindestens einelementige, aber
nach oben unbeschränkte Ergebnisse. Der Parser \emph{char} wiederum
ist auf genau ein Element beschränkt. Darauf aufbauend können wir die
Kranzlängen der übrigen Parser berechnen.

Sei $\bbbn^{\infty} = \bbbn \cup \{\infty\}$ und |ys_p =
(l_p, u_p)| $\in (\bbbn^{\infty}, \bbbn^{\infty})$ die
Kranzlänge eines Parsers |p|. Für die in Abschnitt
\ref{sec:termbib} vorgestellten terminalen Parser können wir die
Kranzlängen wie folgt festlegen:
%format ys_empty        =  " \ysize_{empty}"
%format ys_achar        =  " \ysize_{achar}"
%format ys_astring      =  " \ysize_{astring}"
%format ys_astringp     =  " \ysize_{astringp}"
%format ys_charc        =  " \ysize_{char \, c}"
%format ys_strings      =  " \ysize_{string \, s}"
%format ys_loc          =  " \ysize_{loc}"
\begin{alignat}{2}
& |ys_empty        |      && =  (0,0)            \nonumber \\
& |ys_achar        |      && =  (1,1)            \nonumber \\ 
& |ys_astring      |      && =  (0,\infty)       \nonumber \\
& |ys_astringp     |      && =  (1,\infty)       \nonumber \\
& |ys_charc        |      && =  (1,1)            \nonumber \\
& |ys_strings      |      && =  (||s||,||s||)    \nonumber \\
& |ys_loc          |      && =  (0,0)            \nonumber
\end{alignat}

Die Durchführung der Berechnung wollen wir schrittweise anhand einiger
Beispiele entwickeln. Sei |p| eine Produktion mit |p = achar ~~~
astringp|. Intuitiv können wir feststellen, daß sich die Kranzlänge
|ys_p| durch
\begin{alignat}{2}
&  |ys_p|   && = (l_{achar} + l_{astringp}, \; u_{achar} + u_{astringp}) \nonumber \\
&           && = (1         + 1,            \; 1         + \infty) =   |(2, Infinite)| \nonumber
\end{alignat}
berechnen läßt. Für diesen Zusammenhang definieren wir einen Operator
|<+>|, der die Kranzlänge zweier Parser in beschriebener Weise
addiert:
\begin{equation}
|ys_p <+> ys_q  = (l_p, u_p) <+> (l_q, u_q)  = (l_p `plus'` l_q, u_p `plus'` u_q)|    
\end{equation}
Unsere Beispielrechnung können wir nun also auch durch |ys_p =
ys_achar <+> ys_astringp| beschreiben. Entsprechend definieren wir
einen Operator \hs{<|>} mit
\begin{equation}
\hs{ys_p <|> ys_q  = (l_p, u_p) <|> (l_q, u_q)  = (min' l_p l_q, max' u_p u_q)},
\end{equation}
den wir für die Kranzlängenberechnung alternativer Parser verwenden
wollen. Es ist offensichtlich, daß z.B. für |p = achar| \hs{|||}
|astringp| folgendes gelten muß:
\begin{center}
\begin{tabular}{ccccccc}
 |ys_p| & = & |ys_achar| & \hs{<|>} & |ys_astringp|   &   &             \\
        & = & |(1,1)|    & \hs{<|>} & |(1,Infinite)|  & = & |(1,Infinite)|
\end{tabular}
  
\end{center}
Die Kombination beider Operatoren zeigt das folgende Beispiel:
\begin{alignat}{2}
& |p|    & = \;\; &|achar ~~~ achar| \hspace{3cm} \hs{|||}                   \nonumber \\
&        &        &|string| \text{ ,,hello''} |~~~| |string| \text{ ,,world''} \nonumber \\
\nonumber \\
& |ys_p| & = \;\; & (|(1,1)|   |<+>| |(1,1)|) \hs{<|>}    \nonumber \\
&        &        & (|(5,5)|   |<+>| |(5,5)|)             \nonumber \\
&        & = \;\; & |(2,2)| \hs{<|>} |(10,10)| = |(2,10)| \nonumber
\end{alignat}
Für die Behandlung von Filtern führen wir einen weiteren Operator ein:
\begin{equation}
|ys_p <^> ysc_f = (l_p, u_p) <^> (l_f, u_f)  = (max' l_p l_f, min' u_p u_f)|
\end{equation}
$|ysc_f| \in (\bbbn^{\infty}, \bbbn^{\infty})$ nennen wir
dabei \emph{Kranzlängenbeschränkung}. Ein weiteres Beispiel soll
dieses verdeutlichen:
\begin{center}
\begin{tabular}{ccccccc}
 |p|    & = & |astringp|      & |`with`| & |(size 4 8)| &   &  \\
 |ys_p| & = & |(1, Infinite)| & |<^>|    & |(4,8)|      & = & |(4,8)| 
\end{tabular}
\end{center}
Mittels struktureller Rekursion können wir nun die
Kranzlängenberechnung für eine Produktion |p| mit |p=u_p|
vollständig beschreiben.

\newcommand{\aeq}{$\>\hspace*{.5cm}$}
\newcommand{\aeqb}{$\>\hspace*{ -1cm}$}
\begin{center}
\begin{minipage}{0.8\textwidth}

|ys_p| = |ysa_u u_p| $\quad$ mit

%----------------------------------------------------------------------------------------------------
%   Beginn Implementierung
%----------------------------------------------------------------------------------------------------


%if code

-- > (l_p, u_p) <+> (l_q, u_q)             = case ((l_p, u_p), (l_q, u_q)) of
-- >                                           ((Bottom, Bottom),(Bottom, Bottom)) -> (Bottom, Bottom)
-- >                                           ((Bottom, Bottom), _)               -> (l_q, u_q)
-- >                                           (_, (Bottom, Bottom))               -> (l_p, u_p)
-- >                                           otherwise                           -> (l_p `plus'` l_q, u_p `plus'` u_q)

-- > (l_p, u_p) <|> (l_q, u_q)             = case ((l_p, u_p), (l_q, u_q)) of
-- >                                           ((Bottom, Bottom),(Bottom, Bottom)) -> (Bottom, Bottom)
-- >                                           ((Bottom, Bottom), _)               -> (l_q, u_q)
-- >                                           (_, (Bottom, Bottom))               -> (l_p, u_p)
-- >                                           otherwise                           -> (min' l_p l_q, max' u_p u_q)

-- > (l_p, u_p) <^> (l_f, u_f)             = case ((l_p, u_p), (l_f, u_f)) of
-- >                                           ((Bottom, Bottom), _)      -> (Bottom, Bottom)
-- >                                           otherwise                  -> if (l_f `gr'` u_p) ||
-- >                                                                            (l_p `gr'` u_f) then (Bottom, Bottom) 
-- >                                                                                            else (max' l_p l_f, min' u_p u_f)


Single-track Yieldsize calculation:

-- > (l_p, u_p) <!+> (l_q, u_q)             = (l_p `plus'` l_q, u_p `plus'` u_q)
-- > (l_p, u_p) <!|> (l_q, u_q)             | l_q `gr'` u_q = (l_p, u_p)
-- >                                        | l_p `gr'` u_p = (l_q, u_q)
-- >                                        | otherwise     = (min' l_p l_q, max' u_p u_q)
-- > (l_p, u_p) <!^> (l_f, u_f)             = (max' l_p l_f, min' u_p u_f)
-- > cnstST (l_x, u_x)                      = l_x == u_x

neuer Ansatz:

> (l_p, u_p) <!+> (l_q, u_q)             = (l_p `plus'` l_q, u_p `plus'` u_q)
> (l_p, u_p) <!|> (l_q, u_q)             = (min' l_p l_q, max' u_p u_q)
> (l_p, u_p) <!^> (l_f, u_f)             = let (l_r, u_r) = (max' l_p l_f, min' u_p u_f)
>                                          in if l_r `gr'` u_r then (Infinite, Number 0)
>                                                              else (l_r, u_r) 
> cnstST (l_x, u_x)                      = l_x == u_x


Two-track Yieldsize calculation:

> (ST yp)      <+> (ST yq)      = ST (yp <!+> yq)
> (TT y1p y2p) <+> (TT y1q y2q) = TT (y1p <!+> y1q) (y2p <!+> y2q)
> _            <+> _            = error "mixed single/two-track while yield size calculation (<+>)"

> (ST yp)      <|> (ST yq)      = ST (yp <!|> yq)
> (TT y1p y2p) <|> (TT y1q y2q) = TT (y1p <!|> y1q) (y2p <!|> y2q)
> _            <|> _            = error "mixed single/two-track while yield size calculation (<|>)"

> (ST yp)      <^> (ST yq)      = ST (yp <!^> yq)
> (TT y1p y2p) <^> (TT y1q y2q) = TT (y1p <!^> y1q) (y2p <!^> y2q)
> _            <^> _            = error "mixed single/two-track while yield size calculation (<^>)"

> cnst (ST y)     = cnstST y
> cnst (TT y1 y2) = cnstST y1 && cnstST y2

> ysa_u :: UserFunctions -> YSizes -> Unit -> YSize     

%endif
\vspace{ -\baselineskip}

> ysa_u ufs ysizes (Terminal t)       {-"\aeq"-} = ys_t

\vspace{ -1.7\baselineskip}
%if code

>    where
>      ys_t       = fst4 $ termDef ufs t

%endif

> ysa_u ufs ysizes (Nonterminal nt)   {-"\aeq"-} = ys_nt

\vspace{ -1.7\baselineskip}
%if code

>    where
>      ys_nt = getYSize ysizes nt 

%endif

> ysa_u ufs ysizes (p :~~~ q)                       {-"\aeq"-} = ysa_u ufs ysizes p <+> ysa_u ufs ysizes q
> ysa_u ufs ysizes ((p, ysc_p) :~~ (q, ysc_q))      {-"\aeq"-} = ( ysa_u ufs ysizes p <^> ysc_p) <+> (ysa_u ufs ysizes q <^> ysc_q)
> ysa_u ufs ysizes ((p, ysc_p) :/\\/ (q, ysc_q,d )) {-"\aeq"-} = ( ysa_u ufs ysizes p <^> ysc_p) <+> (ysa_u ufs ysizes q <^> ysc_q)
> ysa_u ufs ysizes ((p, la_p) :^^^ (q, la_q))       {-"\aeq"-} =   ysa_u ufs ysizes ((p, ysc_lap) :~~ (q, ysc_laq)) {-"\footnotemark"-}

\vspace{ -1.7\baselineskip}
%if code

>       where
>       ysc_lap = laBounds ufs la_p
>       ysc_laq = laBounds ufs la_q

\begin{alignat}{2}
& laBounds \;\; la & \; = &\;  (min \{l_{la} \;||\;\forall\, i,j: (l_{la}, u_{la}) \in la!(i,j)\},  \nonumber \\
&                  &      &\;\; max \{u_{la} \;||\;\forall\, i,j: (l_{la}, u_{la}) \in la!(i,j)\})  \nonumber \hspace*{1.9cm}
\end{alignat}
%endif

> ysa_u ufs ysizes (c :<<< p)     {-"\aeq"-} = ysa_u ufs ysizes p
> ysa_u ufs ysizes (p `With` f)   {-"\aeq"-} = ysa_u ufs ysizes p <^> ysc_f

\vspace{ -1.7\baselineskip}
%if code

>    where 
>      ysc_f      = fst3 (filterDef ufs f)

%endif

> ysa_u ufs ysizes (p :||| q)     {-"\aeq"-} = ysa_u ufs ysizes p <|> ysa_u ufs ysizes q
> ysa_u ufs ysizes (p :... h)     {-"\aeq"-} = ysa_u ufs ysizes p

> ysa_u ufs ysizes (TTUnit p q)   {-"\aeq"-} = case (ys_p, ys_q) of 
>                                                    (ST yp, ST yq) -> TT yp yq 
>                                                    otherwise      -> error "type error in mixed single-/two-track definition!"
>     where
>       ys_p = ysa_u ufs ysizes p 
>       ys_q = ysa_u ufs ysizes q
                                                    
> ysa_u ufs ysizes (ListCompr _ _ ys) {-"\aeq"-} = ys


\end{minipage}
\end{center}
\footnotetext{Die Kranzlängenbeschränkungen |ysc_lap|und
  |ysc_laq|sind dabei die Wertebereiche der lookahead-Funktionen und
  müssen vom Benutzer bereitgestellt werden (Abschnitt \ref{chp:usage:la}).}

\subsection{Fixpunktiteration}
\label{sec:fixpoint}

Bei der Behandlung von Nichtterminalen sind wir dabei von einer
bekannten Kranzlänge |ys_nt| für ein Nichtterminal |nt|
ausgegangen. Da Produktionen wechselseitig voneinander abhängen
können, läßt sich dieses an dieser Stelle allerdings nicht
voraussetzen. Wir initialisieren daher die Kranzlängen aller
Produktionen gemäß $|init| = \{ |ys_nt| := |(Infinite, 0)| \; || \;
(nt = u) \in |P| \}$ und führen die Berechnung mittels eines
Fix\-punkt\-iterationsverfahrens durch. Dabei sei |P| die Menge der
Produktionen und $|Y| = \{|ys_nt| := (l_{nt}, u_{nt}) \;||\; (nt = u)
\in P\}$ die Menge der entsprechenden Kranzlängen. Es gilt dann
\begin{center}
\begin{minipage}{0.7\textwidth}
$\qquad Y = fix \;\; init \quad \mbox{mit}$ \\
$\hspace*{1.4cm}$ $fix \;\; |current| = 
\begin{cases}
|current|            & \text{für } |current == new| \\
fix \; |new|         &  \text{sonst}  
\end{cases}$\\

$\qquad\qquad|new| = \{ |ys_nt := ysa_u u| \; || \; |(nt = u)| \in |P| \}$
\end{minipage}
\end{center}

Hier stellt sich nun die Frage nach der Terminierung. Die Funktion
|ysa_u| ist monoton fallend in der ersten Komponente $l$ und monoton
steigend in der zweiten Komponente $u$. $l$ konvergiert dabei immer,
da $\bbbn^{\infty}$ nach unten beschränkt ist. $u$ muß nicht
notwendigerweise konvergieren, wir können allerdings leicht zeigen,
daß $u_p = \infty$ gelten muß, falls |u_p| nach $||P||$ Iterationen
immer noch steigt:

Sei $P = \{p_1, ..., p_n\}, n=||P||$. $p_i \rightarrow p_j$ bezeichne
ein Auftreten des Nichtterminals $p_j$ in der Produktion $p_i$.
Entsprechend bezeichne $p_i \rightarrow^* p_j = p_i \rightarrow p_j
\hs{||} p_i \rightarrow p_k \rightarrow^* p_j$ eine Abhängigkeit der
Produktion $p_i$ von $p_j$. Falls die Produktionen zyklisch
voneinander abhängen, kann dieser Zyklus maximal die Länge $||P||$
besitzen. In diesem Fall hat die Produktion eine unendliche Ableitung,
so daß nach $||P||$ Iterationen immer noch steigende |u_p| auf $\infty$
gesetzt werden können. 

Ein Beispiel:

%format ys_test  =  " \ysize_{test}"
\begin{center}
\begin{tabular}{ccccccccc}
    & |test|     & = & |achar| & |~~~| & |test|           & |~~~| & |achar| &  \hs{|||} \\
    &            &   & |empty|                                                          \\[4mm]
1.  & |ys_test|  & = & |(1,1)| & |<+>| & |(Infinite, 0)|  & |<+>| & |(1,1)| &  \hs{<|>} \\
    &            &   & |(0,0)|                                                          \\
    &            & = & |(0,2)|                                                          \\[4mm]
2.  & |ys_test|  & = & |(1,1)| & |<+>| & |(0,2)|          & |<+>| & |(1,1)| &  \hs{<|>} \\
    &            &   & |(0,0)|                                                          \\
    &            & = & |(0,4)|                                                          \\
    &            & ...                                                                  \\
    &            & = & |(0, Infinite)|
\end{tabular}
\end{center}

Es ist offensichtlich, daß |ys_test = (0, Infinite)| gelten muß. Ohne
das oben beschriebene Abbruchkriterium würde das Verfahren hier
allerdings nicht terminieren. Für das Auftreten einer
Kranzlängenbeschränkung muß allerdings ein Sonderfall eingeführt
werden: Nachdem die Kranzlängen in Frage kommender Produktionen auf
$\infty$ gesetzt wurden, müssen diese wieder mit der
Kranzlängenbeschränkung verrechnet werden.

Gleichzeitig können wir unbrauchbare Produktionen ermitteln. Eine
Produktion |p| ist unbrauchbar, falls $l_p = \infty$ nach Erreichen
des Fixpunkts. $p$ kann in diesem Fall keine endliche Ableitung
besitzen. Ein Weglassen des terminalen Parsers |empty| in obigem
Beispiel verdeutlicht dieses:

\begin{center}
\begin{tabular}{cccccccc}
    & |test|     & = & |achar|          & |~~~| & |test|           & |~~~| & |achar  |  \\[4mm]
1.  & |ys_test|  & = & |(1,1)|          & |<+>| & |(Infinite, 0)|  & |<+>| & |(1,1)|    \\
    &            & = & |(Infinite, 2)|                                                  \\[4mm]
2.  & |ys_test|  & = & |(1,1)|          & |<+>| & |(Infinite, 2)|  & |<+>| & |(1,1)|    \\
    &            & = & |(Infinite, 4)|                                                  \\
    &            & ...                                                                  \\
    &            & = & |(Infinite, Infinite)|
\end{tabular}
\end{center}

|test| kann in diesem Fall nicht terminieren.

%----------------------------------------------------------------------------------------------------
\subsection{Bezüge zu anderen Verfahren}
%----------------------------------------------------------------------------------------------------

Die Kranzlängenanalyse orientiert sich an der Technik der abstrakten
Interpretation \cite{Cousot77}. Die abstrakte Interpretation wird
unter anderem im Übersetzerbau zur korrekten Durchführung von
Programmanalysen verwendet \cite{WIL:MAU:1996}. Ein klassisches
Beispiel ist die Striktheitsanalyse von Programmen der funktionalen
Programmierung \cite{Burn85, PJ87, PeV93}. Wesentliche Eigenschaft der
abstrakten Interpretation ist dabei die Abstraktion der Domäne der
Eingabedaten auf eine abstrakte Domäne. In unserer Anwendung ist dies
die Domäne der Kranzlängen $(\bbbn^{\infty}, \bbbn^{\infty})$. Eine
Einbettung in die Formalismen der klassischen abstrakten
Interpretation ist allerdings ein recht schwieriges Unterfangen. Wir
abstrahieren hier nicht direkt auf den Ein- und Ausgaben eines
Programms, sondern auf einer Baumgrammatik. Man kann sich vorstellen,
daß sich die Kranzlängenanalyse als Anwendung der Grammatikflußanalyse
\cite{MW91, WIL:MAU:1996} beschreiben läßt. Hier wurde allerdings ein
direkter Ansatz gewählt.

Eine Definition der Kranzlängenanalyse auf Parsermengen findet sich in
\cite{GIE:STE:2002}.

%if code

> ysaProds :: UserFunctions -> TModes -> [Prod] -> [YSizes]
> ysaProds ufs tms ps = e where
>                 init  = initYSize ps tms
>                 count = length ps
>                 e     = fix ufs init count count [] ps

> ysaProd :: UserFunctions -> YSizes -> Prod -> (Nt, YSize)
> ysaProd ufs ysizes (n :=== (_, _, _, u)) = (n, ysa_u ufs ysizes u)
>                                                     

> initYSize :: [Prod] -> TModes -> YSizes
> initYSize [] [] = []
> initYSize ((n :=== _):ps) ((_,mode):tms) = (n, initys mode) : initYSize ps tms where
>   initys MST = ST (Infinite, Number 0)
>   initys MTT = TT (Infinite, Number 0) (Infinite, Number 0)
> initYSize [] _ = []
> initYSize x y = pattErr "initYSize" (x,y)

> fix :: UserFunctions -> YSizes -> Int -> Int -> [YSizes] -> [Prod] -> [YSizes]
> fix ufs currentResults counter maxcount ress ps = e where
>              newResults = map (ysaProd ufs currentResults) ps
>              e | newResults  == currentResults = case checkLowInf newResults of
>                                                    False -> newResults : ress
>                                                    True  -> error 
>                                                      ("\nsource program has useless nonterminals!" 
>                                                       ++ "\n\ncurrent results for yield size analysis:\n" 
>                                                       ++ replicate 42 '-' ++ "\n"
>                                                       ++ unlines ( map ppYSizes (reverse (newResults : ress))))

>                | counter     < 0               = fix ufs (incToInf currentResults newResults) maxcount maxcount (newResults: currentResults: ress) ps

>                | otherwise                     = fix ufs newResults                      (counter - 1) maxcount (currentResults:ress) ps

>              incToInf :: YSizes -> YSizes -> YSizes 
>              incToInf [] []  = []
>              incToInf ((p,ys_p): ps) ((q,ys_q): qs) = ((q, new): qs') where
>                new = case (ys_p, ys_q) of
>                        (ST ys_p    , ST ys_q)     -> ST $ update ys_p      ys_q
>                        (TT y1p y2p , TT y1q y2q)  -> TT (update y1p y1q) (update y2p y2q)

>                update (l_p, u_p) (l_q, u_q) = case (u_p, u_q) of
>                                                  (Infinite, _)        -> (l_q, Infinite)
>                                                  (_       , Infinite) -> (l_q, Infinite)
>                                                  (Number a, Number b) -> (l_q, if b > a then Infinite else (Number b))
>                                                  -- fuer yield sizes mit variablen:
>                                                  (a,        b)        -> (l_q, if b `gr'` a then Infinite else b)
>                                                  -- x                    -> pattErr "ysa.fix" x
>                qs'  = incToInf ps  qs

>              checkLowInf []                  = False
>              checkLowInf ((_,ys): ps)        = checkys ys || checkLowInf ps where
>                checkys (ST (Infinite, _))    = True
>                checkys (TT (Infinite, _) _)  = True
>                checkys (TT _ (Infinite, _))  = True
>                checkys _                     = False


Testfunktion:

> file_ysa f = do
>     inp <- readFile f
>     (ufs, _, (_,ps), _, _, _) <- return $ catchEr $ parse inp
>     ps                        <- return $ prodSourceToProd ps
>     tms                       <- return $ tmaProds [] ps
>     putStrLn $ ppTModes tms
>     putStrLn $ ppYSizes $ head $ ysaProds ufs tms ps


% ----------------------------------------------------------------------------------------------------
\section{test area}
% ----------------------------------------------------------------------------------------------------

\begin{fminipage}

\footnotesize{

< p1 = tabulated ( p p2 )
< p2 = tabulated ( f2 <<< base ~~~ p p3 ~~~ base )
< p3 = tabulated ( f3 <<< base ~~~ p p4 ~~~ base )
< p4 = tabulated ( f4 <<< base ~~~ p p5 ~~~ base )
< p5 = tabulated ( f5 <<< base ~~~ p p6 ~~~ base )
< p6 = tabulated ( p p1 ||| f3a <<< empty)

< ind_p = tabulated (f5 <<< base ~~~ base ~~~ base)

\begin{center}
\begin{minipage}{0.8\linewidth}
\begin{multicols*}{2}
\raggedcolumns

\begin{enumerate}
\item 
< p1 -> (Infinite,0)
< p2 -> (Infinite,0)
< p3 -> (Infinite,0)
< p4 -> (Infinite,0)
< p5 -> (Infinite,0)

....

\item 
< p1 -> (6,Infinite)
< p2 -> (8,Infinite)
< p3 -> (2,Infinite)
< p4 -> (4,Infinite)
< p5 -> (6,Infinite)

\end{enumerate}

\bigskip

\bigskip

\bigskip

\end{multicols*}
\end{minipage}
\end{center}

}

\end{fminipage}
% ----------------------------------------------------------------------------------------------------

%endif


%----------------------------------------------------------------------------------------------------
\section{Kombinatoroptimierung}
\label{ysa:optimize}
%----------------------------------------------------------------------------------------------------

Wie in Abschnitt \ref{sec:boundedyields} eingeführt, multipliziert
jedes Auftreten eines |#~~~| Kombinators die Laufzeit um den Faktor
der Eingabelänge. In einem Ausdruck |p ~~~ q| wird über alle möglichen
Unterworte von $p$ und $q$ iteriert. Falls $p$ oder $q$ eine konstante
oder wenigstens nach oben beschränkte Kranzlänge besitzen, ist dieses
ein unnötiger Aufwand.

Gängige Vorgehensweise bei der Entwicklung eines ADP-Prototypen ist
nun, bei Bedarf optimierte Varianten dieses Kombinators zu verwenden.
So hat |r -~~ p| bzw. |p ~~- r| die gleiche asymptotische Laufzeit wie
$p$ für sich allein genommen. Entsprechend kann man Varianten für
andere Fälle entwickeln oder direkt den |#~~| Kombinator mit Angabe
konkreter Kranzlängen verwenden. Bei umfangreicheren Prototypen
kann die manuelle Kombinatoroptimierung allerdings schnell
unübersichtlich werden. Eine unbeabsichtigte Einschränkung des
Berechnungsbereichs läßt das Programm dabei zwar korrekt ausführen,
kann aber zu falschen Ergebnissen führen.

Mit Hilfe der Kranzlängenanalyse können wir den Schritt der
Kombinatoroptimierung vollständig automatisieren. Alle dabei
benötigten Ergebnisse werden von der Analyse ermittelt. Die
Transformation gliedert sich dabei in zwei Phasen.

\begin{itemize}
\item In der ersten Phase werden alle Auftreten des |#~~~| Kombinators
  durch den mit Kranzlängen angereicherten |#~~| Kombinator ersetzt.
\item Um die Lesbarkeit des optimierten Programms zu erhöhen, werden
  danach -- falls vorhanden -- spezialisierte Varianten eingesetzt. In
  Fällen, in denen keine Variante verfügbar ist, wird eine neue
  erzeugt.
\end{itemize}

%----------------------------------------------------------------------------------------------------
\subsection{Kombinatoranreicherung}
%----------------------------------------------------------------------------------------------------

%if code

> trNext :: UserFunctions -> YSizes -> [Prod] -> [Prod]
> trNext ufs ysizes ps = map (trNextProd ufs ysizes) ps

> trNextProd :: UserFunctions -> YSizes -> Prod -> Prod
> trNextProd ufs ysizes (n :=== (cm, tab, tr, u)) = (n :=== (cm', tab, tr, u')) where
>    (ysize, u') = enc ufs ysizes u
>    cm'         = cm -- ++ "-- production "++ n ++
>                     --    ", yield size: " ++ ppYSize ysize ++ "\n"

%endif

\begin{figure}[htbp]
\begin{fminipagec}
\textbf{Überblick: Kombinatoranreicherung}

> ys_clash (ST ys_p') (ST ys_q') = (l_p' `gr'` u_p' || l_q' `gr'` u_q') where
>           (l_p', u_p') = ys_p'
>           (l_q', u_q') = ys_q'

> ys_clash (TT (lp1,up1) (lp2,up2)) (TT (lq1,uq1) (lq2,uq2)) = 
>           (lp1 `gr'` up1 || lp2 `gr'` up2 || lq1 `gr'` uq1 || lq2 `gr'` uq2)  


Die Funktion |enc| ersetzt den ``Standardkombinator'' |#~~~| durch den
mit Kranzlängen angereicherten Kombinator |#~~|.

< enc : {-"\text{ enrich next combinators }"-}
< enc u_p -> (ys_p, u_p')

\begin{tabular}{p{6cm}p{6cm}}
\textit{Eingaben}              & \textit{Ausgaben} \\
|u_p|: Produktionsdefinition   & |ys_p|: Kranzlänge von |u_p|\\
                               & |u_p'|: optimierte Produktion |p|
\end{tabular}
    
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Die Kombinatoranreicherungsfunktion |enc|}
\label{fig:def:enc}
\end{figure}


Sei $p$ eine Produktion mit $p = u_p$. Die Kombinatoroptimierung wird
mittels |(ys_p, u_p') = enc u_p| initialisiert. Die Transformationsfunktion
|enc| kopiert dabei die Eingabestruktur und führt gleichzeitig die
Kranzlängen mit. Ein Ausdruck |p ~~~ q| wird dann durch den mit
Kranzlängen angereicherten Ausdruck |(p', ys_p) :~~ (q', ys_q)|
ersetzt.

|(ys_p, u_p') = enc u_p| $\quad$ mit

%if code

> enc :: UserFunctions -> YSizes -> Unit -> (YSize,Unit)         

%endif

> enc ufs ysizes (Terminal t)     {-"\aeq"-} = (ys_t, Terminal t)

%if code

>    where
>    ys_t       = fst4 $ termDef ufs t

%endif
Die Kranzlänge |ys_nt| eines Nichtterminals |nt| wird direkt aus
dem Resultat der Kranzlängenanalyse verwendet:

> enc ufs ysizes (Nonterminal nt) {-"\aeq"-} = (ys_nt, Nonterminal nt)

\vspace{ -1.7\baselineskip}
%if code

>    where
>    ys_nt = getYSize ysizes nt 

%endif

> enc ufs ysizes (p :~~~ q)       {-"\aeq"-} = (ys_p <+> ys_q, (p', ys_p) :~~ (q', ys_q))  
>    where
>        (ys_p ,p') {-"\aeqb"-} = enc ufs ysizes p
>        (ys_q, q') {-"\aeqb"-} = enc ufs ysizes q

Mittels des \!|#~~|\! Kombinators kann der Benutzer von vornherein konkrete
Längen\-vorgaben machen. Ein Ausdruck |((p, ysc_fl) :~~ (q, ysc_fr))|
ist dabei semantisch equivalent zu |(p `with` fl) :~~~ (q `with` fr)|.
Abbildung \ref{fig:ysa1} zeigt ein Beispiel. 

\begin{figure}[htbp]
\begin{fminipagec}
%{
%format where = "\mathbf{where}"
Eine Produktion

< test = f <<< (astringp `with` size 4 9) ~~~ (astringp `with` minsize 3)

kann auch als 

< test = f <<< astringp ~~! astringp
<        where
<          (~~!) = (~~*) (4,9) 3

geschrieben werden. Der Kombinator |~~*| ist dabei ein Spezialfall des
|#~~| Kombinators für einen nach oben unbeschränkten Parser auf der
rechten Seite.
%}
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Zur Semantik des |#~~| Kombinators}
\label{fig:ysa1}
\end{figure}

Hier kann allerdings der Fall auftreten, daß die Vorgaben |ysc_p| und
|ysc_q| des Benutzer mit den während der Kranzlängenanalyse
ermittelten Werte kollidieren. Beispielsweise macht eine Produktion

%{
%format where = "\mathbf{where}"

< test = f <<< (astringp `with` maxsize 3) ~~! astringp
<          where
<            (~~!) = (~~*) (5,10) 1     

%}

keinen Sinn, da |ys_p <^> ysc_p| = |(1,3) <^> (5,10)| = |(5,3)|. In
diesem Fall wird also eine Fehlermeldung ausgegeben:

> enc ufs ysizes ((p, ysc_p) :~~ (q, ysc_q))

$\qquad\qquad = \begin{cases}
     |error|                                          &  \text{für } \; |l_p' `gr'` u_p'| \hs{||} |l_q' `gr'` u_q'| \\
     |(ys_p' <+> ys_q', (p', ys_p') :~~ (q', ys_q'))| &  \text{sonst}
\end{cases}$

%if code

>     = if ys_clash ys_p' ys_q'  then  error 
>                               ("\n\nuser defined combinator definition " ++
>                               ppCombParams (CombYSize ysc_p ysc_q) ++ "\n" ++
>                               "clashes with yield size analysis:\n\n" ++
>                               pretty' False p ++ " -> " ++ ppYSize ys_p ++ "\n" ++
>                               pretty' False q ++ " -> " ++ ppYSize ys_q ++ "\n")

>        else  (ys_p' <+> ys_q', (p', ys_p') :~~ (q', ys_q'))

%endif

>     where
>        (ys_p, p') {-"\aeqb"-} = enc ufs ysizes p
>        (ys_q, q') {-"\aeqb"-} = enc ufs ysizes q
>        ys_p'      {-"\aeqb"-} = ys_p <^> ysc_p
>        ys_q'      {-"\aeqb"-} = ys_q <^> ysc_q


> enc ufs ysizes ((p, ysc_p) :/\\/ (q, ysc_q,d))
>     = if ys_clash ys_p' ys_q'  then  error 
>                               ("\n\nuser defined combinator definition " ++
>                               ppCombParams (CombDB ysc_p ysc_q d) ++ "\n" ++
>                               "clashes with yield size analysis:\n\n" ++
>                               pretty' False p ++ " -> " ++ ppYSize ys_p ++ "\n" ++
>                               pretty' False q ++ " -> " ++ ppYSize ys_q ++ "\n")

>        else  (ys_p' <+> ys_q', (p', ys_p') :/\\/ (q', ys_q', d))
>     where
>        (ys_p, p') {-"\aeqb"-} = enc ufs ysizes p
>        (ys_q, q') {-"\aeqb"-} = enc ufs ysizes q
>        ys_p'      {-"\aeqb"-} = ys_p <^> ysc_p
>        ys_q'      {-"\aeqb"-} = ys_q <^> ysc_q


Die oben beschriebene Überprüfung wird auch bei der Transformation des
|^^^| Kombinators durchgeführt. Auch hier können Benutzerangaben mit
den Resultaten der Kranzlängenanalyse kollidieren:

> enc ufs ysizes ((p, la_p) :^^^ (q, la_q))

$\qquad\qquad = \begin{cases}
     |error|                                          & \text{für } \; |l_p' `gr'` u_p'| \hs{||} |l_q' `gr'` u_q'| \\
     |(ys_p' <+> ys_q', (p', la_p) :^^^ (q', la_q))|  & \text{sonst}
\end{cases}$

%if code

>     = if ys_clash ys_p' ys_q' then error
>                               ("\n\nlookahead combinator definition (^^^) " ++
>                                la_p ++ ppYSize ysc_lap ++ " "  ++
>                                la_q ++ ppYSize ysc_laq ++ "\n" ++
>                                "clashes with yield size analysis:\n\n" ++
>                                pretty' False p ++ " -> " ++ ppYSize ys_p ++ "\n" ++
>                                pretty' False q ++ " -> " ++ ppYSize ys_q ++ "\n")

>        else  (ys_p' <+> ys_q', (p', la_p) :^^^ (q', la_q))

%endif

>     where
>        (ys_p, p') {-"\aeqb"-} = enc ufs ysizes p
>        (ys_q, q') {-"\aeqb"-} = enc ufs ysizes q
>        ys_p'      {-"\aeqb"-} = ys_p <^> ysc_lap
>        ys_q'      {-"\aeqb"-} = ys_q <^> ysc_laq

%if code

>        ysc_lap      = laBounds ufs la_p
>        ysc_laq      = laBounds ufs la_q

%endif



> enc ufs ysizes (c :<<< p)    {-"\aeqb"-} = (ys_p, c :<<< p')   
>    where (ys_p, p')              {-"\aeqb"-} = enc ufs ysizes p


> enc ufs ysizes (p :||| q)    {-"\aeqb"-} = (ys_p <|> ys_q, p' :||| q')  
>    where
>        (ys_p, p')                {-"\aeqb"-} = enc ufs ysizes p
>        (ys_q, q')                {-"\aeqb"-} = enc ufs ysizes q

Bei der Transformation einer Filteranwendung ist zu beachten, daß wir
diese für rein längenbeschränkende Filter weglassen könnten.
Beispielweise wird der Ausdruck

< astringp ~~~ (astringp `with` minsize 3)

in den Ausdruck

< (astringp, (1,Infinite)) {-"\;"-} ~~ {-"\;"-}((astringp `with` minsize 3), (3,Infinite))

transformiert, was die zusätzliche Filteranwendung |minsize 3|
überflüssig macht. Aus Gründen der Lesbarkeit und Nachvollziehbarkeit
der optimierten Fassung wollen wir diesen Aufruf aber dennoch
behalten. Die Laufzeit wird durch diese zusätzliche Filteranwendung
auch nur um einen konstanten Wert erhöht.

> enc ufs ysizes (p `With` f) {-"\aeqb"-} =  (ys_p <^> ysc_f, p' `With` f)  
>    where  (ys_p, p')               {-"\aeqb"-} = enc ufs ysizes p

%if code

>           ysc_f      = fst3 (filterDef ufs f)

man kann überlegen, bei längenbeschränkenden Filtern auch im
Haskell-Code kein with-Konstrukt mehr zu erzeugen. Dies ist eigentlich
schon durch die Spezial-Kombinatoren abgedeckt. Ein Weglassen erhöht
die Effizienz allerdings auch nicht wesentlich, von daher ist es
möglicherweise übersichtlicher, sie doch drinzubehalten. Hier die
Funktion, um sie rauszuschmeißen:

< enc ufs ysizes (p `With` (f, args))   = 
<    ((max' l_p l_f, min' u_p u_f), p'')  where

<        ((l_p, u_p), p') = enc ufs ysizes p
<        ((l_f, u_f), fFct)  = filterDef ufs f args
<        p'' = case (fFct (Var "_") (Var "_")) of
<                Cnst 0    -> p'
<                otherwise -> p' `With` (f, args)

%endif

> enc ufs ysizes (p :... h)    {-"\aeqb"-} = (ys_p, p' :... h)  
>    where  (ys_p, p')             {-"\aeqb"-} = enc ufs ysizes p

> enc ufs ysizes (TTUnit p q)  {-"\aeqb"-} = (ys_tt, TTUnit p' q')
>     where
>       (ys_p, p') = enc ufs ysizes p 
>       (ys_q, q') = enc ufs ysizes q
>       ys_tt      = case (ys_p, ys_q) of 
>                     (ST yp, ST yq) -> TT yp yq 
>                     otherwise      -> error "type error in mixed single-/two-track definition!"

> enc ufs ysizes lc@(ListCompr _ _ ys) {-"\aeqb"-} = (ys, lc)


Abbildung \ref{fig:opt1} zeigt ein Beispiel.

\begin{figure}[htbp]
\begin{fminipagec}
\begin{Code}


p1 = tabulated (
     ((f <<< ((string ``Hello'') ~~~ (string ``world''))) \hs{#|||}
     (r <<< ((achar ~~~ astring) ~~~ astringp))))        
    
\end{Code}

\rule{\linewidth}{1pt}

\begin{Code}

-- production p1, yield size: (2,Infinite)
p1 = tabulated (
     ((f <<< ((string ``Hello'') ((~~) (5,5) (5,5)) (string ``world'')))\!\hs{#|||}
     (r <<< ((achar ((~~) (1,1)        (0,Infinite)) astring) 
                    ((~~) (1,Infinite) (1,Infinite)) astringp))))                                       
  
\end{Code}

\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Beispiel für die Kombinatoranreicherung}
\label{fig:opt1}
\end{figure}


%if code

Testfunktion:

> ppTrNext f = do
>               inp                  <- readFile f
>               (ufs,_,(_,ps),_,_,_) <- return (catchEr (parse inp))
>               ps                   <- return $ prodSourceToProd ps
>               tms                  <- return $ tmaProds [] ps
>               tps                  <- return (trNext ufs (head (ysaProds ufs tms ps)) ps)
>               putStrLn (concatMap (prettyOpts $ PPOptBool False) tps)

%endif

%----------------------------------------------------------------------------------------------------
\subsection{Einsetzen der Varianten}
%----------------------------------------------------------------------------------------------------

Dieses Beispiel können wir nun direkt als optimierte Version des
Eingabeprogramms ausgeben. Diese ist allerdings aufgrund der
vollständigen Verwendung des |#~~| Kombinators alles andere als lesbar.
In einer zweiten Transformationsphase werden daher für eine Reihe von
häufig verwendeten Spezialfällen vordefinierte Kombinatoren
eingesetzt. Folgende Tabelle zeigt eine Übersicht:

\begin{center}
\begin{tabular}{ccc}
|ys_p|   &  |ys_q|  &  Variante \\
\hline
|(Number 0, Infinite)| & |(Number 0, Infinite)| & |~~~|  \\
|(Number 1, Infinite)| & |(Number 0, Infinite)| & |+~~|  \\ 
|(Number 0, Infinite)| & |(Number 1, Infinite)| & |~~+|  \\
|(Number 1, Infinite)| & |(Number 1, Infinite)| & |+~+|  \\ 
|(Number 1, Number 1)| &          \_            & |-~~|  \\ 
         \_            & |(Number 1, Number 1)| & |~~-|  \\ 
\end{tabular}
\end{center}

%format SPC = "\;" 

Für alle anderen Fälle wird ein Kombinator der Form $|~~!|\!\!^+ =
|(~~) ys_p SPC ys_q|$ erzeugt, wobei sich die Anzahl der ,,!'' nach der
Zahl der schon für die jeweilige Produktion erzeugten Kombinatoren
richtet. Wie schon in Abbildung \ref{fig:ysa1} angedeutet, werden dabei
für nach oben unbeschränkte Kranzlängen Spezialfälle des |#~~|
Kombinators verwendet: 

\begin{center}
\begin{tabular}{lcccccc}
 |(~~*)| & |ys_p| & |l_q|  & = & |(~~)| &  |ys_p|            & |(l_q, Infinite)| \\
 |(*~~)| & |l_p|  & |ys_q| & = & |(~~)| &  |(l_p, Infinite)| & |ys_q|            \\ 
 |(*~*)| & |l_p|  & |l_q|  & = & |(~~)| &  |(l_p, Infinite)| & |(l_q, Infinite)| 
\end{tabular}
\end{center}

Abbildung \ref{fig:opt2} zeigt dies für das Beispiel aus Abbildung \ref{fig:opt1}.

\begin{figure}[htbp]
\begin{fminipagec}
\begin{Code}


-- production p1, yield size: (2,Infinite)
p1 = tabulated (
     ((f <<< ((string ``Hello'') ((~~) (5,5) (5,5)) (string ``world'')))\!\hs{#|||}
     (r <<< ((achar ((~~) (1,1)        (0,Infinite)) astring) 
                    ((~~) (1,Infinite) (1,Infinite)) astringp))))                                       
    
\end{Code}

\rule{\linewidth}{1pt}

\begin{Code}

-- production p1, yield size: (2,Infinite)
p1 = tabulated (
     ((f <<< ((string ``Hello'') ~~! (string ``world''))) \hs{#|||}
      (r <<< ((achar -~~ astring) +~+ astringp))))
      where
        (~~!) = (~~) (5,5) (5,5)  

\end{Code}

\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Beispiel für das Einsetzen der Varianten}
\label{fig:opt2}
\end{figure}

%if code

> data ChNextRes = ChNextRes String [ProdSource]
> chNextVariation :: YSize -> YSize -> [ProdSource] ->  ChNextRes

> chNextVariation (ST (Number 0, Infinite)) (ST (Number 0, Infinite)) combs = ChNextRes "~~~" combs
> chNextVariation (ST (Number 1, Infinite)) (ST (Number 0, Infinite)) combs = ChNextRes "+~~" combs
> chNextVariation (ST (Number 0, Infinite)) (ST (Number 1, Infinite)) combs = ChNextRes "~~+" combs
> chNextVariation (ST (Number 1, Infinite)) (ST (Number 1, Infinite)) combs = ChNextRes "+~+" combs
> chNextVariation (ST (Number 1, Number 1)) (ST _                   ) combs = ChNextRes "-~~" combs
> chNextVariation (ST _                   ) (ST (Number 1, Number 1)) combs = ChNextRes "~~-" combs

In any other case a definition of a new combinator |:~~!| is created.

< chNextVariation ys_p                 ys_q                 combs = ChNextRes "~~!" combs

The length ranges for |:~~!| are saved and result in a combinator
definition in the transformed version. The number of "!"\ in the
combinator name then depends on the number of combinators already
created for the corresponding production:

<     .....
<     where
<     infixl 7 :~~!
<     (:~~!) = (:~~) ys_p ys_q


> chNextVariation l r combs = ret where
>                  comb = [n | (CombDef n (CombYSize l' r')) <- combs, l==l', r==r']
>                  ret  = case comb of
>                            [] -> ChNextRes combName ((CombDef combName (CombYSize l r)):combs) where
>                                      combName = "~~" ++ 
>                                                 replicate ((length combs) + 1) '!'
>                            otherwise -> ChNextRes (head comb)  combs


> trNextVar :: [Prod] -> [ProdSource]
> trNextVar ps = map trNextVarProd ps 

> trNextVarProd :: Prod -> ProdSource 
> trNextVarProd (n :=== (cm, tab, tr, u)) = (n :==== (cm, tab, tr, u', reverse newCombs))  where
>      (newCombs,u') = trNextVarUnit [] u 
> trNextVarProd (DirectDef a b c)         = DirectDefSource a b c 

For a given unit |u_p| |trNextVarUnit u_p| recursively traverses |u_p|
and replaces each occurence of the |:~~| combinator with the special
combinator |:~~!| which holds the variation to be used:

In dieser Phase liegen alle Nextkombinatoren in der Form 

<  ((~~) (l,u) (l',u')) 

vor. Spezialfälle wie |((~~) (1,1) (l',u'))| oder |((~~) (l,u) (1,1))|
können nun direkt auf vordefinierte Kombinatoren wie |-~~| bzw. |~~-|
umgesetzt werden. Falls kein vordefinierter Kombinator vorhanden ist,
wird ein neuer konstruiert. z.B. wird für |((~~) (2,2) (5,9))| eine
Definition |(~~!) = (~~) (2,2) (5,9)| aufgebaut, in der Produktion
erscheint dann der neue Kombinator |~~!|.

trNextVar bekommt die Liste der Produktionen und liefert die Liste der
transformierten Produktionen und die Liste der neuen Kombinatoren in
der Form |([Prod],[(String,YSize,YSize)])|. Der obige neue Kombinator
wird dann z.B. als |("~~!", (Number 2, Number 2)|, |(Number 5, Number 9))|
zurückgeliefert.

Die Funktion durchläuft dazu rekursiv alle Produktionen und ersetzt
die Nextkombinatoren in der beschriebenen Form (chNextVariation,
s.u). Die Liste der neuen Kombinatoren wird dabei komplett
mitgeführt, d.h. in der aktuellen Form werden die neuen Kombinatoren
global definiert und ganz am Ende des Programms ausgegeben. Man kann
sie sicherlich auch lokal den einzelnen Produktionen zuordnen, dann
wird die Ausgabe aber möglicherweise etwas unübersichtlich ($\rightarrow$
Geschmacksache).

> trNextVarUnit combs ((p, ys_p) :~~  (q, ys_q))  = (combs''', p' :~~! (combName, q')) where 
>      (combs', p')        = trNextVarUnit combs  p 
>      (combs'',q')        = trNextVarUnit combs' q
>      ChNextRes combName combs''' = chNextVariation ys_p ys_q combs'' 

> trNextVarUnit combs ((p, ys_p) :/\\/  (q, ys_q, d))     =  (newComb:combs'', p' :~~! (combName, q'))  where 
>      (combs', p')        = trNextVarUnit combs  p 
>      (combs'',q')        = trNextVarUnit combs' q
>      newComb             = CombDef combName (CombDB ys_p ys_q d)
>      combName            = "~~" ++ replicate ((length combs'') + 1) '!'

> trNextVarUnit combs ((p, la_p) :^^^ (q, la_q))  = (newComb:combs'', p' :~~! (combName, q')) where 
>      (combs', p')        = trNextVarUnit combs  p 
>      (combs'',q')        = trNextVarUnit combs' q
>      newComb             = CombDef combName (CombLA la_p la_q) 
>      combName            = "~~" ++ replicate ((length combs'') + 1) '!'

> trNextVarUnit combs x    = collectUnit trNextVarUnit combs x

Die Funktion chNextVariation wählt entweder einen vordefinierten
Spezialkombinator oder konstruiert einen neuen.  |~~~|, |-~~|, |~~-|, |+~~|,
|~~+| und |+~+| sind als Spezialfälle vorhanden, hier wird die Liste der
Kombinatoren unverändert gelassen. Andernfalls wird zwischen zwei
Fällen unterschieden:

\begin{itemize}
\item als erstes wird überprüft, ob möglicherweise schon eine
  Definition konstruiert wurde, dann wird der Name des entsprechenden
  Kombinators zurückgegeben.
\item Falls nicht, wird ein neuer Name eingeführt, dieser wird
  zurückgegeben und die Kombinatorliste wird entsprechend erweitert.
\end{itemize}

Beispiele:

\begin{itemize}

\item 
< Yieldsize> chNextVariation (Number 1, Number 1) (Number 1, Infinite) 
<               [("~~!", (Number 2, Number 2), (Number 5, Number 9))]
< 
< ("-~~",[("~~!",(Number 2,Number 2),(Number 5,Number 9))])    
< 

$\Longrightarrow$ |-~~| wird zurückgegeben, |combs| bleibt unverändert.

\item 
< Yieldsize> chNextVariation (Number 2, Number 2) (Number 5, Number 9)
<               [("~~!", (Number 2, Number 2), (Number 5, Number 9))]
< 
< ("~~!",[("~~!",(Number 2,Number 2),(Number 5,Number 9))])   

$\Longrightarrow$ für |((~~) (2,2) (5,9))| liegt schon eine Def. vor (|~~!|). Diese
    wird zurückgegeben, |combs| wird nicht geändert.

\item 
< Yieldsize> chNextVariation (Number 2, Number 4) (Number 5, Number 9) 
<               [("~~!", (Number 2, Number 2), (Number 5, Number 9))]
< 
< ("~~!!",[("~~!!",(Number 2,Number 4),(Number 5,Number 9)),
<          ("~~!", (Number 2,Number 2),(Number 5,Number 9))])  

$\Longrightarrow$ hier wird |~~!!| neu eingeführt.

\end{itemize}



%----------------------------------------------------------------------------------------------------
\section{Zusammenführung der Übersetzungsphasen}
%----------------------------------------------------------------------------------------------------


trans führt die yield size analysis und die beiden Phasen der
Transformation durch:

> trans :: UserFunctions -> TModes -> [Prod] -> [ProdSource]
> trans ufs tms ps = trNextVar (trNext ufs (head (ysaProds ufs tms ps)) ps)

ppTrans gibt den Code der Produktionen und der neu definierten
Kombinatoren übersichtlich aus:

> ppTrans :: UserFunctions -> TModes -> [Prod] -> IO ()
> ppTrans ufs tms ps = do
>               ps' <- return (trans ufs tms ps)
>               putStrLn (concatMap (prettyOpts $ PPOptBool False) ps')
 
transFile liest eine Eingabedatei, parst diese und führt dann die
Transformation durch:

> transFile :: String -> IO ()
> transFile f = do
>                  inp <- readFile f
>                  (ufs, _, (_,ps), _, _, _)    <- return (catchEr(parse inp))
>                  (_,_,(header, _, trailer))   <- return (extractArea (lines inp) "grammar{" "}")
>                  putStrLn (unlines header)
>                  ps                           <- return $ prodSourceToProd ps
>                  tms                          <- return $ tmaProds [] ps
>                  ppTrans ufs tms ps
>                  putStrLn (unlines trailer)

ppCombs bekommt die Liste der neu definierten Kombinatoren. Über
fixityDecl werden diese als infix deklariert. ppComb gibt die
Definition eines einzelnen Kombinators aus:

> ppCombs :: [(String, YSize, YSize)] -> String
> ppCombs combs = fDecl ++ combDef where
>                                   combNames  = map fst3 combs
>                                   fDecl      = fixityDecl combNames
>                                   combDef    = unlines (map (ppComb maxlength) combs)
>                                   maxlength  = length (maximum combNames)

> fixityDecl :: [String] -> String  
> fixityDecl [] = ""
> fixityDecl f  = "infixl 7 " ++ (sepList ", " f) ++"\n"

Ausgabe einer einzelnen Kombinatordefinition. An dieser Stelle werden
zusätzlich Sonderfälle für teilweise unbeschränkte Kranzlängen
behandelt.

Standardfall:

< Yieldsize> ppComb 3 ("~~!", (Number 2, Number 2), (Number 4, Number 5))
< "(~~!) = (~~) (2,2) (4,5)"  

Es kann aber auch der Fall eintreten, daß für einen Kombinator auf
einer Seite oder gar beidseitig unbeschränkte Längen vorliegen. In
diesen Situation werden Variationen des |#~~| - Kombinators verwendet
(|*~~|, |~~*| und |*~*|) (Def. s.u.).

\begin{itemize}
\item 
< Yieldsize> ppComb 3 ("~~!", (Number 2, Infinite), (Number 4, Number 5))
< "(~~!) = (*~~) 2 (4,5)"

\item 
< Yieldsize> ppComb 3 ("~~!", (Number 2, Number 2), (Number 4, Infinite))
< "(~~!) = (~~*) (2,2) 4"

\item 
< Yieldsize> ppComb 3 ("~~!", (Number 2, Infinite), (Number 4, Infinite))
< "(~~!) = (*~*) 2 4"              
\end{itemize}

> ppComb :: Int -> (String,YSize,YSize) -> String  

> ppComb ind (c, ST (Number l, Infinite), ST (Number r, Infinite)) = 
>        "(" ++ c ++ ")" ++ spc (ind - length c) ++ " = " 
>        ++ "(*~*) " ++ show l ++ " " ++ show r

> ppComb ind (c, ST (Number l, Infinite), ST r)                    = 
>        "(" ++ c ++ ")" ++ spc (ind - length c) ++ " = " 
>        ++ "(*~~) " ++ show l ++ " " ++ ppSYSize r

> ppComb ind (c, ST l, ST (Number r, Infinite))                    = 
>        "(" ++ c ++ ")" ++ spc (ind - length c) ++ " = " 
>        ++ "(~~*) " ++ ppSYSize l ++ " " ++ show r

> ppComb ind (c, ST  l, ST r)                                      = 
>        "(" ++ c ++ ")" ++ spc (ind - length c) ++ " = " 
>        ++  "(~~) " ++ ppSYSize l ++ " " ++ ppSYSize r

> ppComb ind (c, TT l1 l2, TT r1 r2)                               =
>        "(" ++ c ++ ")" ++ spc (ind - length c) ++ " = " 
>        ++  "(!~~) " ++ ppYSize (TT l1 l2) ++ " " ++ ppYSize (TT r1 r2)



Definition der |~~| - Variationen:

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

%endif