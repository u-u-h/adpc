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

> module Codegen1(

>   codegen1,
>   rev_Codegen1

> ) where

> import Constants
> import Codegen
> import Dss
> import MathExp
> import Expr
> import Syntax
> import StringLib
> import TLData
> import TL
> import Structs
> import Tools
> import Algebras

> rev_Codegen1 =  "$Revision$"

%endif

% ----------------------------------------------------------------------------------------------------
%  Formatierung
% ----------------------------------------------------------------------------------------------------

%format v_in     = " v^{\,in}"
%format v_out    = " v^{\,out}"
%format v_out_p  = " v^{\,out}_p"
%format v_out_q  = " v^{\,out}_q"
%format cg1Unit  = " cg_u\,"

%format (tmpVar1 (a)) = " \mathbf{v}\,[" a "]"
%format TLVar        = "\negthickspace"
%format TLNil        = " \mathbf{nil}"
%format (TLAssign (a) (b)) = a "\;\mathbf{:=}\;" b

%%format TLIf a b c    = "\mathbf{if}\;" a "\;\mathbf{then}\;" b "\;\mathbf{else}\;" c
%format TLIf    = "\mathbf{if}\;"

%format TLFor a b c d = "\mathbf{for}\;" a "\;\mathbf{:=}\;" b "\;\mathbf{to}\;" c "\;\mathbf{do}\;" d
%format TLFA =  "\negthickspace"

%format copyList = " \mathbf{copyList}" 
%format append = " \mathbf{append}" 

%format code_p        = " code_p"
%format code_q        = " code_q"

%format (id (a))    = a
%format tex_t       = "t"
%format TLExp (a)  = a
%format (TLRec a b) = a "!" b

%format v_out_loops = " v^{\,out}_{lp}"
%format code_loops   = " code_{lp}"
%format loops = "loops"
%format cg1Loops = " cg_{lp}"
%format cg1Function = " cg_{f}"
%format cg1Function' = " cg_{a}"
%format vs_out_p = " vs^{\,out}_p"




% ----------------------------------------------------------------------------------------------------
%  Environments
% ----------------------------------------------------------------------------------------------------


\newenvironment{tog}{\vspace{.5\baselineskip}\begin{minipage}{\textwidth}}{\vspace{.5\baselineskip}\end{minipage}}

% For sources
\newenvironment{sourcen}%
% begin environment
{\VerbatimEnvironment\begin{Sbox}\begin{minipage}{0.97\textwidth}\begin{BVerbatim}}%
% end environment
{\end{BVerbatim}\end{minipage}\end{Sbox}
\begin{center}\fbox{\TheSbox}\end{center}}



%if code

% Test area
% ----------

% > drtest5 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("base", []) :~~~ Terminal ("base", []))

% \begin{center}
% \eval{ deriveRec drtest5  }  -- oder \perform
% \end{center}


\begin{figure}[htbp]
  \begin{center}

\begin{centerbox}

  \begin{center}
    links \hfill rechts \\
    links fnksdndf \hfill rechts
  \end{center}

Datenstruktur nach der Indexermittlung:

< p4!(i, j) =
<    p3!(i, j) `ILwith` equal(i, j)        {-"$\>\>$\hspace{5cm}"-}  :/|||/
<    (dd, []) :/<<</ base(i, i+1) :/~~~/ p4!(i+1, j)        {-"$\>\>$\hspace{5cm}"-}  :/|||/
<    (ii, []) :/<<</ p4_ins!(i, j-1) :/~~~/ base(j-1, j) :/.../ h
<
< p4_ins!(i, j) =
<    p3!(i, j) `ILwith` equal(i, j)        {-"$\>\>$\hspace{5cm}"-}  :/|||/
<    (ii, []) :/<<</ p4_ins!(i, j-1) :/~~~/ base(j-1, j) :/.../ h
 

< componentsN!(i, j) =
<    (nil, []) :/<<</ empty(i, j)        {-"$\>\>$\hspace{5cm}"-}  :/|||/
<    closedcomponents!(i, j)        {-"$\>\>$\hspace{5cm}"-}  :/|||/
<    (ul, []) :/<<</ (ss, []) :/<<</ region(i, j) :/.../ pp        {-"$\>\>$\hspace{5cm}"-}  :/|||/
<    (cons, loops) :/<<</ (ss, []) :/<<</ region(i, k) :/.../ pp :/~~~/ closedcomponents!(k, j)
<       where loops = [(k, i+1, j-5)]
<    :/.../ pp          

Zielcode:

\begin{verbatim}

   procedure calc_p1(i: integer; j: integer);
   var
      v: array[0..9] of pointer;

   begin
      if equal(i, j) then begin
         v[0] := tp_base(i, i+1);
         v[1] := tp_uregion(i+1, j-1);
         v[2] := af_nil(v[1]);
         v[3] := copyList(p1[i+1, j-1]);
         v[4] := append(v[2], v[3]);
         v[5] := af_h(v[4]);
         v[6] := tp_base(j-1, j);
         v[7] := af_m(v[0], v[5], v[6]);
         v[8] := v[7];
      end
      else begin
         v[8] := nil;
      end;
      v[9] := af_h(v[8]);
      p1[i, j] := v[9];
   end;              

\end{verbatim}
\end{centerbox}

\caption{Beispiel}
\label{fig:bsp}
\end{center}
\end{figure}

%endif


%----------------------------------------------------------------------------------------------------
\section{Codeerzeugung}
\label{sec:codegen}
%----------------------------------------------------------------------------------------------------


Mit den Ergebnissen der Indexermittlung und der Abhängigkeitsanalyse
haben wir nun alle für die imperative Codeerzeugung benötigten
Informationen beisammen. Die Codeerzeugung gliedert sich dabei in drei
Teile: den zentralen Bereich der Übersetzung der Rekurrenzen
(Abschnitt \ref{sec:cg1}), die Bereitstellung der Algebrafunktionen
(Abschnitt \ref{sec:cg1:alg}) und die Konstruktion der Hauptschleife
(Abschnitt \ref{sec:cg1:main}).

\subsection{Zielsprache}
\label{sec:tl}

Wir gehen von einer sehr einfachen imperativen Zielsprache
$\mathcal{T}$ aus, die lediglich Zuweisungen, Abfragen und Schleifen
zuläßt\footnote{Die konkrete Implementation der Codeerzeugung enthält
natürlich noch weitere Konstrukte. Diese sind hier aus Gründen der
Übersicht nicht aufgeführt.}:

%format G_T        = "\mathcal{G}_{\mathcal{T}}"
%format pG_T       = "\phantom{\mathcal{G}_{\mathcal{T}}:}"
%format Subscripts = " \mathrm{Subscripts}"
%format Assignment = " \mathrm{Assignment}"
%format Variable   = " \mathrm{Variable}"
%format Code       = " \mathrm{Code}"
%format MathExp    = " \mathrm{MathExp}"

%{
%include tex/pascal.fmt

\begin{alignat}{3}
 |G_T|:    & \qquad & |Code|       & \; \rightarrow \quad && |Assignment|                                                \nonumber \\
           &        &              & \; \rightarrow \quad && |if filterID (Subscripts) then Code else  Code|             \nonumber \\
           &        &              & \; \rightarrow \quad && |for varID := MathExp to MathExp do Code|                   \nonumber \\
           &        &              & \; \rightarrow \quad && |while Expr do Code|                                        \nonumber \\
           &        & |Assignment| & \; \rightarrow \quad && |Variable| := |nil|                                         \nonumber \\ 
           &        &              & \; \rightarrow \quad && |Variable| := |functionID Variable|^+                       \nonumber \\
           &        &              & \; \rightarrow \quad && |Variable| := |terminalID Subscripts|                       \nonumber \\
           &        &              & \; \rightarrow \quad && |Variable| := |Variable|                                    \nonumber 
\end{alignat}

Zusätzlich zu den bereits in den Indizes bzw. den mathematischen
Ausdrücken (Abschnitt \ref{sec:il}) enthaltenen Variablen benötigen
wir ein Array |tmpVar1 ()| von Zeigern und eine Möglichkeit des
Zugriffs auf die Rekurrenztabellen:

\begin{alignat}{3}
           & \qquad & |Variable|   & \; \rightarrow \quad && |(tmpVar1 (number))|                                         \nonumber \\
           &        &              & \; \rightarrow \quad && |ntID!Subscripts|  \hspace*{3.4cm}                          \nonumber
\end{alignat}

%}

\subsection{Übersetzung der Rekurrenzen}
\label{sec:cg1}

\begin{figure}[htbp]
\begin{fminipagec}
\textbf{Überblick: Codeerzeugung}

Die Funktion |cg1Unit| erzeugt Zielcode aus der Zwischensprache $\mathcal{S}$.

< cg1Unit : {-"\text{ code generation}"-}
< cg1Unit v_in u_p -> (v_out, code_p)

\begin{tabular}{p{6cm}p{6cm}}
\textit{Eingaben}                               & \textit{Ausgaben} \\
|v_in|: aktuelle Zeigernummer                   & |v_out|: Zeiger der auf das Ergebnis von |code_p| zeigt \\
|u_p| : Produktionsdefintion aus $\mathcal{S}$  & |code_p|: Zielcode aus $\mathcal{T}$
\end{tabular}
    
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Die Codeerzeugungsfunktion |cg1Unit|}
\label{fig:def:cg}
\end{figure}



Zur Vereinfachung der Übersetzung beginnen wir mit der Annahme, daß
alle terminalen Parser als externe Funktionen in der Zielsprache
vorliegen. Abbildung \ref{fig:achar} zeigt eine mögliche
Pascal-Implementierung\footnote{Für die Beschreibung der Codeerzeugung
wollen wir Pascal verwenden, das System stellt aber auch eine
Codeausgabe in C zur Verfügung. Eine große Hilfe war hier
\cite{GLA:1993}} für den Parser |achar|. \texttt{x[]} ist dabei die
global definierte Eingabe.

\begin{figure}[htbp]
\begin{fminipagec}
\begin{Code}



   achar :: Array Int base -> Parser base
   achar x (i,j) = [x!j || i+1 == j]
    
\end{Code}

\rule{\linewidth}{1pt}

\begin{numberedsource}
   function tp_achar(i,j: integer): p_t_char;
   var t : p_t_char;
   begin
     if i+1 = j then begin
       new (t);
       t.value:=x[j];
       t.next:=nil;
       tp_achar := t;
     end
     else tp_achar := nil;
   end;

   type t_char = record
     value : char;
     next  : pointer;
   end;
   type p_t_char =  ^t_char;
\end{numberedsource}
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Der Parser |achar| in Pascal}
\label{fig:achar}
\end{figure}
Zur Vermeidung von potentiellen Namenskonflikten in der Zielsprache
ist hier der Funktionsname um den Präfix \texttt{tp\_} erweitert. Die
Funktion ist eine direkte Übersetzung der Parserfunktion in Haskell,
d.h. die Parserergebnisse werden als Zeiger auf eine verkettete Liste
(hier vom Typ \texttt{t\_char}) zurückgegeben. \texttt{value} enthält
in diesem Fall das Zeichen der Eingabe und \texttt{next} einen Zeiger
auf das nächste Listenelement\footnote{Bei terminalen Parsern ist die
  Ergebnisliste in den meisten Fällen maximal einelementig, innerhalb
  des ADP-Konzepts spricht allerdings nichts gegen eine Verwendung
  terminaler Parser mit mehrelementigen Ergebnissen. Daher hier die
  vielleicht etwas ,,überdimensionierte'' Definition}. Falls der
Parser nicht erfolgreich ist (hier im Fall $i+1 \not= j$), wird eine
leere Liste geliefert (\texttt{nil}). Für die in Abschnitt
\ref{sec:termbib} beschriebenen terminalen Parser ist im Rahmen dieser
Arbeit eine Bibliothek entstanden. Parser für weitere Anwendungsfälle
können dementsprechend direkt in der Zielsprache implementiert werden.

Weiterhin nehmen wir an, daß alle benötigten Algebrafunktionen
ebenfalls als externe Funktionen vorliegen. In Abschnitt
\ref{sec:cg1:alg} beschreiben wir eine Strategie zur automatischen
Erzeugung dieser Funktionen.

Aus Gründen der Übersicht wird für jede Rekurrenz eine separate
Funktion erzeugt. Die Ausgangssprache für die Codeerzeugung ist
$\mathcal{S}$ (Abschnitt \ref{sec:il}). Sei $p$ eine Produktion aus
$\mathcal{S}$ mit $p = u_p$ und |(v_out_p, code_p) = cg1Unit (-1) u_p|
das Ergebnis der Codeerzeugungsfunktion |cg1Unit|.  Die generierte
Funktion für $p$ hat dann die Form:

%{

%include tex/pascal.fmt

< procedure calc_p (i: integer; j: integer) 
< var 
<    v: array[0..v_out_p] of pointer
<   
< begin
<    code_p

<    p[i,j] := (tmpVar1 v_out_p)
< end

%}

%if code

> cg1Prod ufs (n, cloops, u) = ret where
>         (v, out) = cg1Unit ufs (-1) u
>         ret      = (TLFD [] TLVoid (pfct prefixes ++ n) 
>                            [(["i"],TLInt),(["j"],TLInt)]
>                            (tempVarDecl ++ loopVarDecl)
>                            (out ++ [TLAssign (toVA (ArrayElem [Var "i", Var "j"] (Direct n)))
>                                              (TLVar (tmpVar1 v))]) (tlvar (Direct "_")))
>         tempVarDecl = [(["v"], (Array [Number (v+1)] (PointerOf TLVoid)))]
>         loopVarDecl = if cloops > 0 then map lv ("" :(map show [2..cloops])) else []
>         lv n = ([("k" ++ n)], TLInt)

%endif

Das Array $\textbf{v}$ enthält dabei die innerhalb von |code_p|
verwendeten temporären Zeiger. |tmpVar1 v_out_p| zeigt auf das Ergebnis
von |code_p|.

Die Funktion |cg1Unit| durchläuft rekursiv die zu übersetzende Eingabe
und liefert jeweils den entsprechenden Zielcode zusammen mit der
Nummer desjenigen temporären Zeigers, der auf das Ergebnis dieses
Codeabschnitts zeigt.

Wie oben beschrieben werden terminale Parser als externe Funktionen
der Zielsprache aufgerufen. |tmpVar1 (v_in+1)| enthält dann einen
Zeiger auf das Ergebnis von |t (i,j)|:

> tmpVar1 i = toVA $ ArrayElem [Number i] (Direct "v")

> cg1Unit ufs v_in (ILTerminal t (ST (i,j)))   = (v_in+1, [TLAssign (tmpVar1 (v_in+1)) (TLExp (tex_t (i,j)))]) 

%if code

>           where  
>              tex_t     = thd4(termDef ufs t) 

%endif

Für ein Nichtterminal |nt| wird ein Zugriff auf den entsprechenden
Tabelleneintrag |(TLRec nt (i, j))| erzeugt. Da die Ergebnistabellen
verkettete Listen enthalten, wird an dieser Stelle mittels der
externen Funktion |copyList| eine vollständige Kopie des Eintrags
angelegt. |tmpVar1 (v_in+1)| zeigt dann auf den Anfang der
Ergebnisliste:

> cg1Unit ufs v_in (ILNonterminal nt s)   = (v_in+1, [TLAssign (tmpVar1 (v_in+1)) (TLFA copyList [(tlNonterm nt s)])])

%if code

>           where
>              copyList = "copyList"

%endif

Alternative Ergebnisse werden mittels der externen Funktion |append|
konkateniert. Die Funktion |append| hängt dabei zwei verkettete Listen
aneinander.

> cg1Unit ufs v_in (p :/|||/ q)  = ((v_out_q+1), 
>                                   code_p ++ code_q 
>                                   ++ [TLAssign (tmpVar1 (v_out_q+1)) (TLFA append [TLVar (tmpVar1 v_out_p), TLVar (tmpVar1 v_out_q)])]) 
>           where
>              (v_out_p, code_p)  = cg1Unit ufs v_in p
>              (v_out_q, code_q)  = cg1Unit ufs v_out_p q

%if code

>              isTerm (ILTerminal t s) = True
>              isTerm _                = False

>              append | isTerm p = "app_" ++ dt p
>                     | isTerm q = "app_" ++ dt q
>                     | otherwise = "append" 

>              dt (ILTerminal t _) = error "Codegen1: ppDataTypePascal(snd4(termDef ufs t))"

%endif

Die Auswahlfunktion |h| liegt ebenfalls als externe Funktion der
Zielsprache vor und arbeitet wie das Haskell-Pendant auf Listen von
Ergebnissen. Wir erzeugen also |code_p| und rufen |h| mit |tmpVar1
v_out_p| auf. Der Zeiger |(tmpVar1 (v_out_p+1))| zeigt auf das
Auswahlergebnis, welches wiederum eine verkette Liste sein kann. Es
ist also möglich, Auswahlfunktionen zu verwenden, die mehr als ein
Ergebnis auswählen (s.a. Abschnitt \ref{sec:evalalgs}).

> cg1Unit ufs v_in (p :/.../ h)               = ((v_out_p+1), code_p ++ [TLAssign (tmpVar1 (v_out_p+1)) (TLFA tex_h [TLVar (tmpVar1 v_out_p)])]) 
>           where
>              (v_out_p, code_p) = cg1Unit ufs v_in p

%if code

>              tex_h = ("af_" ++ h)

%endif

Für Filter wird eine if-Abfrage erzeugt. Falls die Filterbedingung
|f(i,j)| erfüllt ist, enthält |tmpVar1 (v_out_p+1)| nach Abarbeitung
des entsprechenden Blocks einen Zeiger auf das Ergebnis von
|code_p|. Andernfalls ist |tmpVar1 (v_out_p+1)| leer:

\renewcommand{\aeq}{$\>\hspace*{ -.2cm}$}
\newcommand{\cgthen}{$\>\hspace*{1.7cm}$\mathbf{then}\;\;}
\newcommand{\cgelse}{$\>\hspace*{1.7cm}$\mathbf{else}\;\;}

> cg1Unit ufs v_in (p `ILwith` (f, s))   = ((v_out_p+1), 
>                      [TLIf (id(tex_f s))  {-"\cgthen"-} (id (code_p ++ [TLAssign (tmpVar1 (v_out_p+1)) (TLVar (tmpVar1 v_out_p))]))
>                                               {-"\cgelse"-} [(TLAssign (tmpVar1 (v_out_p+1)) TLNil)]]) 
>           where
>              (v_out_p, code_p) = cg1Unit ufs v_in p   

%if code

>              tex_f       = (thd3(filterDef ufs f)) 

%endif

Bei der Übersetzung von Algebrafunktionsaufrufen unterscheiden wir
zwei Fälle. Falls die an die Algebrafunktion |f| gebundene
Schleifenliste leer ist, wird mittels |cg1Function ufs v_in tex_f p|
der Funktionsaufruf der Algebrafunktion |f| mit den Argumenten |p|
generiert (s.u.):

> cg1Unit ufs v_in ((f, _,  []) :/<<</ p) = cg1Function ufs v_in tex_f p

%if code

>           where ((tex_f,_),_)  = f

%endif

Andernfalls wird eine Schleifenstruktur erzeugt. In |tmpVar1
v_out_loops| werden dabei die Ergebnisse nach folgender Strategie
gesammelt:

< TLAssign (tmpVar1 v_out_loops) TLNil
< TLFor k k_start k_end
<   code_p
<   (tmpVar1 (v_out_loops)) := append [TLVar (tmpVar1 v_out_p), TLVar (tmpVar1 v_out_loops)]

Der Schleifenrumpf |code_p| wird dabei wie oben mittels |cg1Function
ufs v_in tex_f p| erzeugt. Die Funktion |cg1Loops| bettet diesen in die
Schleifenkonstruktion ein:
\renewcommand{\aeq}{$\>\hspace*{ -2.0cm}$}
\renewcommand{\aeqb}{$\>\hspace*{1.9cm}$}

> cg1Unit ufs v_in ((f, _,  loops) :/<<</ p) = (v_out_loops, [TLAssign (tmpVar1 v_out_loops) TLNil] ++ code_loops)
>    where
>        (v_out_p, code_p) = cg1Function ufs v_in tex_f p

>        v_out_loops {-"\aeq"-} = (v_out_p+1)
>        code_loops  {-"\aeq"-} = cg1Loops loops (code_p ++ [(TLAssign (tmpVar1 v_out_loops) (TLFA append [TLVar (tmpVar1 v_out_p), TLVar (tmpVar1 v_out_loops)]))])

>        cg1Loops []                          {-"\aeqb"-} code = code
>        cg1Loops ((k, k_start, k_end):loops) {-"\aeqb"-} code = 
>                            [TLFor k k_start k_end (cg1Loops loops code)]

%if code

>        ((tex_f,_),_)  = f
>        append = "append"

%endif

%format (map a b) = a "\;" b

Für eine Algebrafunktion |f| wird ein Funktionsaufruf |f vs_out_p|
generiert, wobei die Liste |vs_out_p| die Zeiger auf die Ergebnisse
der in |p| enthaltenen Argumente enthält. |p| selbst wird dabei von
der Funktion |cg1Function'| übersetzt, die alle innerhalb von |p|
durch |:/~~~/| verbundenen Argumente verarbeitet und dabei die
Ergebniszeigerliste |vs_out_p| aufbaut:
\renewcommand{\aeq}{$\>\hspace*{1.5cm}$}
\renewcommand{\aeqb}{$\>\>\hspace*{5.0cm}$}

> cg1Function ufs v_in f p = ((v_out_p+1), code_p ++ [TLAssign (tmpVar1 (v_out_p+1)) (TLFA tex_f (map TLVar vs_out_p))]) 
>           where
>              (v_out_p, vs_out_p, code_p)        {-"\aeq"-} = cg1Function' ufs v_in p

>              cg1Function' ufs v_in (p :/~~~/ q) {-"\aeq"-} = (v_out_q, vs_out_p ++ [tmpVar1 v_out_q], code_p ++ code_q) 
>                                 where
>                                    (v_out_p, vs_out_p, code_p) {-"\aeqb"-} = cg1Function' ufs v_in p
>                                    (v_out_q, code_q)           {-"\aeqb"-} = cg1Unit ufs v_out_p q

>              cg1Function' ufs v_in p            {-"\aeq"-} = (v_out_p, [tmpVar1 v_out_p], code_p)
>                                 where
>                                    (v_out_p,code_p)            {-"\aeqb"-} = cg1Unit ufs v_in p

%if code

>              tex_f = ("af_" ++ f)

%endif

Abbildung \ref{fig:cg1ex} demonstriert die Codeerzeugung am Beispiel
zweier Produktionen. Es enthält eine Reihe von möglichen Fällen.

\begin{figure}[htbp]
\begin{fminipagec}

\begin{Code}



 match = tabulated(
         (r <<< achar  ~~~ p inner ~~~ achar) 
             `with` (pairing equal) ... h)

 inner = tabulated(
         cons <<< astring ~~~ astring ~~~ p match  \hs{|||}
         nil  <<< astringp         ... h)

\end{Code}

\rule{\linewidth}{1pt}

\begin{numberedsource}
procedure calc_match(i: integer; j: integer);
var  v: array[0..5] of pointer;
begin
   if equal(x[i+1], x[j]) then begin
      v[0] := tp_achar(i, i+1);
      v[1] := copyList(inner[i+1, j-1]);
      v[2] := tp_achar(j-1, j);
      v[3] := af_r(v[0], v[1], v[2]);
      v[4] := v[3];
   end
   else begin
      v[4] := nil;
   end;
   v[5] := af_h(v[4]);
   match[i, j] := v[5];
end;

procedure calc_inner(i: integer; j: integer);
var v: array[0..8] of pointer;
    k, k2 : integer;
begin
   v[4] := nil;
   for k2:=i to j-3 do begin
      for k:=i to k2 do begin
         v[0] := tp_astring(i, k);
         v[1] := tp_astring(k, k2);
         v[2] := copyList(match[k2, j]);
         v[3] := af_cons(v[0], v[1], v[2]);
         v[4] := append(v[3], v[4]);
      end;
   end;
   v[5] := tp_astringp(i, j);
   v[6] := af_nil(v[5]);
   v[7] := append(v[4], v[6]);
   v[8] := af_h(v[7]);
   inner[i, j] := v[8];
end;                 
\end{numberedsource}
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Beispiel für die Codeerzeugung}
\label{fig:cg1ex}
\end{figure}


%----------------------------------------------------------------------------------------------------
\subsection{Übersetzung der Algebrafunktionen}
\label{sec:cg1:alg}
%----------------------------------------------------------------------------------------------------

Bei der Erzeugung des Programmcodes für die Rekurrenzen haben wir
vorausgesetzt, daß alle Algebrafunktionen als externe Funktionen der
Zielsprache vorliegen. Im folgenden soll nun eine Strategie zur
automatischen Generierung dieser Funktionen beschrieben werden.

Alle Argumente einer Algebrafunktion können Listen enthalten, daher
muß diese auf alle möglichen Argumentkombinationen angewendet werden.
Sei $f$ eine $n$-stellige Algebrafunktion und seien $A_1...A_n$ die
Argumentlisten, dann beschreibt $F = \{f\;a_1...a_n \; || \; a_1 \in
A_1,\, ..., a_n \in A_n\}$ die Menge der notwendigen Funktionsaufrufe.
Dies legt eine imperative Implementierung in der folgenden Form nahe:

\begin{tog}
%{
%include tex/pascal.fmt
%format vd1 = "\vdots"

<  result := nil
<  while a_1 <> nil do
<      vd1
<      while a_n <> nil do
<         result := append (f a_1...a_n, result)
<         a_n := a_n.next
<         vd1
<      a_1 := a_1.next

%}
\end{tog}

Die Liste |result| enthält dann die Ergebnisse der
Algebrafunktionsanwendungen. Die eigentliche Übersetzung der Funktion
ist dabei etwas komplizierter. Da für eine Algebrafunktion im Prinzip
der vollständige Sprachschatz von Haskell verwendet werden kann, ist
eine allgemeingültige Übersetzung nur schwer zu realisieren. Wir
beschränken uns daher auf Ausdrücke, die Variablen, numerische Werte,
die infix-Operatoren |+,-,*,/| und beliebige Funktionsaufrufe
enthalten dürfen. Die Idee an dieser Stelle ist, daß die
Funktionsaufrufe dabei nicht übersetzt werden, sondern als
Funktionsaufrufe in der Zielsprache bestehen bleiben. Die
Implementationen dieser Funktionen müssen dann manuell hinzugefügt
werden, was aber in den meisten Fällen einen vertretbaren Aufwand
darstellen dürfte.

\paragraph{Beispiel:} Die Algebrafunktion

< r a s b = s + if a == b then 1 else 0

erfüllt nicht die oben beschriebenen Forderungen. Durch eine
Transformation in

< r a s b = s + isEqual(a,b)
< isEqual (a,b) = if a == b then 1 else 0

können wir dieses erreichen. |isEqual| muß manuell bereitgestellt
werden (Abbildung \ref{fig:algr}).

Die automatische Übersetzung beschränkt sich dann auf das Ersetzen der
Variablenausdrücke durch die korrespondierenden Zeigerzugriffe in der
Zielsprache. Für obiges Beispiel bedeuted dies:

\begin{tog}
%{
%include tex/pascal.fmt

<  result := nil
<  while a_1 <> nil do
<      while a_2 <> nil do
<          while a_3 <> nil do
<             result := append (a_2 + isEqual(a_1, a_3), result)
<             a_3 := a_3.next
<          a_2 := a_2.next
<      a_1 := a_1.next

%}
\end{tog}

Abbildung \ref{fig:algr} zeigt die konkrete Implementation in Pascal.
Entsprechend dem in Abschnitt \ref{sec:cg1} vorgestellten Ergebnistyp
\texttt{t\_char} für den Parser |achar| ist \texttt{t\_result} hier der
Ergebnistyp der Rekurrenztabellen. Über die drei verschachtelten
Schleifen wird gewährleistet, daß die Funktion alle möglichen
Argumentkombinationen durchläuft. Die eigentliche Berechnung findet in
Zeile 23 statt. Die Variablen der Ausgangsfunktion wurden hier also
durch die entsprechenden Variablenzugriffe auf die verzeigerten
Argumentlisten ersetzt.

\begin{figure}[htbp]
\begin{fminipagec}
\begin{numberedsource}
function isEqual (a,b : char) : integer;
begin
  if a=b then isEqual := 1 else isEqual := 0;
end;

function af_r(a_1: p_t_char; a_2: p_t_result; a_3: p_t_char):p_t_result;
var
   retval: p_t_result;
   tmp: p_t_result;
   tmp_a_1: p_t_char;
   tmp_a_2: p_t_result;
   tmp_a_3: p_t_char;

begin
   retval := nil;
   tmp_a_1 := a_1;
   while tmp_a_1 <> nil do begin
      tmp_a_2 := a_2;
      while tmp_a_2 <> nil do begin
         tmp_a_3 := a_3;
         while tmp_a_3 <> nil do begin
            new(tmp);
            tmp^.value := tmp_a_2^.value + 
                          isEqual(tmp_a_1^.value, tmp_a_3^.value);
            tmp^.next := nil;
            retval := append(tmp, retval);
            tmp_a_3 := tmp_a_3^.next;
         end;
         tmp_a_2 := tmp_a_2^.next;
      end;
      tmp_a_1 := tmp_a_1^.next;
   end;
   freemem_char(a_1);
   freemem_result(a_2);
   freemem_char(a_3);
   af_r := retval;
end;          
\end{numberedsource}
\end{fminipagec}
\vspace{ -1\baselineskip}
\caption{Die Algebrafunktion \texttt{r} in Pascal}
\label{fig:algr}
\end{figure}

\subsubsection{Speicher freigeben}

Bei der Berechnung der Rekurrenzen wird für den Aufbau der
Argumentlisten Speicher allokiert. Nach Anwendung einer
Algebrafunktion kann dieser freigegeben werden. Dieses wird in obigem
Beispiel durch die Funktion \texttt{freemem} realisiert.


%if code

> getAlgFctsProds :: Int -> [ILProd] -> [AutoAlgType]
> getAlgFctsProds mode ps =  (concatMap getAlgFctsProd ps)  where  -- mode = 0 -> algebra-compile, mode /=0 -> sign. compile
>   getAlgFctsProd (_, _, u) = getAlgFctsUnit u

>   getAlgFctsUnit :: ILUnit -> [AutoAlgType]
>   getAlgFctsUnit (ILTerminal _ _) = []
>   getAlgFctsUnit (ILNonterminal _ _) = []
>   getAlgFctsUnit (a :/|||/ b) = getAlgFctsUnit a ++ getAlgFctsUnit b
>   getAlgFctsUnit (a :/.../ h) = if mode ==0  then ((h,[]), [SigId "_Result"]) : getAlgFctsUnit a 
>                                              else getAlgFctsUnit a
>   getAlgFctsUnit (p `ILwith` _) = getAlgFctsUnit p
>   getAlgFctsUnit ((af, _, _) :/<<</ u) = af : getAlgFctsUnit u
>   getAlgFctsUnit (p :/~~~/ q) =  getAlgFctsUnit p ++  getAlgFctsUnit q

> mergeAF :: [AutoAlgType] -> [AutoAlgType]
> mergeAF [a] = [a]
> mergeAF (a:b) = ret where
>           (f_a, d_a) = a
>           b' = [ (x, dx) | (x, dx) <- b, x == f_a ]
>           (f_b, d_b) = head' b' $ "mergeAF: head []"
>           ret = case b' of
>             []        -> a: mergeAF b
>             otherwise -> case (d_a == d_b) of
>                            True -> mergeAF b
>                            False -> error ("inconsistent use of algebra function " ++ ppAlgAppl f_a ++ ":\n" ++
>                                            ppAutoAlgType (f_a, d_a) ++ "\n" ++
>                                            ppAutoAlgType (f_b, d_b) ++ "\n")


------------------------------------------------------------------------------


> compileAlgs :: [AlgDef] -> [AutoAlgType] ->  [TL]
> compileAlgs   adfs afs = map (compileAlg  adfs) afs

> compileAlg :: [AlgDef] -> AutoAlgType ->  TL
> compileAlg  adfs ((f,_), dts) = TLFD [] (PointerOf (StructOf "result" [])) ("af_" ++ f) params localVars 
>                                                    (code++freemem) (TLVar resultVar)

>   where

>   -- werfe die Tupel raus und ersetze die Haskell-Typen durch TLData-Typen:
>   fdts = concatMap upd dts where
>     upd (SigId typ)     = [haskellTypeToDatatype typ]
>     upd (SigTupel typs) = concatMap upd typs
>     upd (SigList _)     = error "list type for algebra arguments not yet supported."

>   def            = [ (nd, args, rhs) | (nd, _, args, rhs) <- adfs,  nd == f]
>   (args, rhs)    = case def of
>                     []        -> ([],   TLComment ["insert algebra definition here"])
>                     otherwise -> (args, TLExp rhs) where (_, args, rhs) = head def 

>   localVars = case rhs of 
>            TLExp (ExpChoice h x) -> [(["retval"], PointerOf (StructOf "result" []))]
>            otherwise                -> [(["retval","tmp"], PointerOf (StructOf "result" []))]
>                                        ++ map lvDecl params 
>                                 where
>                                   lvDecl ([n], r) = (["tmp_" ++ n], r)  

>   params = map argDecl (zip [1..] fdts)
>   argDecl (p, dt) = ([("a_" ++ show p)], (PointerOf dt)) 

>   resultVar = toVA $ Direct "retval"
>   tmpVar1   = (Direct "tmp", makeListStruct TLVoid)

>   code = case rhs of 
>            TLExp (ExpChoice h x) -> [TLAssign resultVar (TLFA h [(tlvar (Direct "a_1"))])]
>            otherwise                -> [TLAssign resultVar TLNil] ++ foreach params innercode

>   freemem = map freemem' params where
>      freemem' ([name], PointerOf (StructOf dt [])) = TLFA ("freemem_" ++ dt) [tlvar (Direct name)]


>   foreach [] code = code
>   foreach (([fa], _): fas) code = [TLAssign (toVA lvar) (tlvar (Direct fa))] ++
>                                   [TLWhileNN (toVA lvar) ((foreach fas code) ++ [TLAssign (toVA lvar) (tlvar ((Pointer lvar):.(Direct "next")))])]
>                                    where
>                                     lvar = Direct ("tmp_" ++ fa)

>   innercode = [TLAlloc MTDynamic tmpVar1 (ExpNum 1) (StructOf "result" [])] ++
>               [TLAssign (listItem tmpVar1) ((insertVarAccess argBinds rhs))] ++
>               [TLAssign (listNext tmpVar1) TLNil] ++
>               [TLAssign resultVar (TLFA "append" [TLVar tmpVar1, TLVar resultVar])] 

>   -- TODO: dieses funktioniert momentan nicht!!
>   argBinds = concatMap argUse (zip3 [1..] dts args)
>     where
>        argUse (p, (SigTupel [SigId a, SigId b]), (SigId s))  = [(s, ExpPOp "toRegion" 
>                                                                [ExpTLVar ((Pointer (Direct ("tmp_a_" ++ show p))) :. (Direct "ri")),
>                                                                 ExpTLVar ((Pointer (Direct ("tmp_a_" ++ show p))) :. (Direct "rj"))])]
>        argUse (p, _                            , (SigId s))  = [(s, ExpTLVar ((Pointer (Direct ("tmp_a_" ++ show p))) :. (Direct "value")))]
>        argUse (p, _          ,(SigTupel [SigId i, SigId j])) = [(i, ExpTLVar ((Pointer (Direct ("tmp_a_" ++ show p))) :. (Direct "ri"))),
>                                                                 (j, ExpTLVar ((Pointer (Direct ("tmp_a_" ++ show p))) :. (Direct "rj")))]

>   insertVarAccess bs (TLComment c) = TLComment c
>   insertVarAccess bs (TLExp rhs) = TLExp (insertVarAccess' bs rhs) where

>     insertVarAccess' bs (ExpVar j)  = ret where 
>                                           nb = [ va | (bn, va) <- bs, bn == j]
>                                           ret = case nb of
>                                                []        -> ExpVar j
>                                                otherwise -> head nb

>     insertVarAccess' bs (ExpIOp a op b)  = ExpIOp (insertVarAccess' bs a ) op (insertVarAccess' bs b)
>     insertVarAccess' bs (ExpPOp f a)     = ExpPOp f (map (insertVarAccess' bs) a)
>     insertVarAccess' bs (ExpIf e a b)    = ExpIf (insertVarAccess' bs e) (insertVarAccess' bs a) (insertVarAccess' bs b)
>     insertVarAccess' bs x                = x

%endif

%----------------------------------------------------------------------------------------------------
\subsection{Hauptprogramm}
\label{sec:cg1:main}
%----------------------------------------------------------------------------------------------------

Die Berechnung der Tabellen findet in zwei verschachtelten Schleifen
statt. Sei |n| die Länge der Eingabe |x|, |P| die Menge der
Produktionen und |ax| das Axiom:

\enlargethispage*{\baselineskip}
\begin{tog}
%{
%include tex/pascal.fmt

< procedure mainloop
< var
<    i, j: integer

< begin
<    for j:=0 to n do
<       for i:=j downto 0 do 
<          calc_p(i, j) {-"\qquad \forall p \in P"-}

<    printAxiom(ax[0, n])
< end

%}
\end{tog}

Die Reihenfolge der Berechnungen ergibt sich aus den Abhängigkeiten
zwischen den Produktionen (siehe Abschnitt \ref{sec:depana}).

Damit ist nun die imperative Codeerzeugung komplett. In Kapitel
\ref{chap:tests} findet sich einige Effizienzvergleiche des erzeugten
Codes gegenüber kompiliertem Haskell-Code der Ausgangsprogramme.

%if code

> cg1Main :: [String] -> String -> TL      
> cg1Main recs axiom = TLFD [] TLVoid "mainloop" 
>                           []
>                           [(["i","j"], TLInt)]
>                           [TLFor "j" (Number 0) (Var "n") [
>                              TLFor "i" (Var "j") (Number 0) 
>                                (map ppCalc recs)
>                           ],
>                           TLFA "printAxiom" [tlvar (ArrayElem [Number 0, Var "n"] (Direct axiom))]] 
>                           (tlvar (Direct "_")) where 
>                               ppCalc a = TLFA (pfct prefixes ++ a) [tlvar (Direct "i"), tlvar (Direct "j")] 


> codegen1 ufs (userCode,maincode,mainaddcode,outputCode) frame algdefs recs axiom il = 
>                                      header' ++ 
>                                      [TLLayout userCode] ++ 
>                                      tcomment "algebra definitions" ++ algcode ++ 
>                                      [TLDecls globalvars] ++ 
>                                      tcomment "productions" ++ map (cg1Prod ufs) il  ++ 
>                                      tcomment "main loop" ++ [cg1Main recs axiom] ++ 
>                                      (if maincode == [] then trailer' else [TLLayout maincode])

>                                where
>                                globalvars = map decl recs 
>                                decl n = ([n], Array [Number standardDim,Number standardDim] (PointerOf (StructOf "result" [])))
>                                (header, trailer) = frame
>                                header'  = header
>                                trailer' = trailer
>                                tcomment s = [tlLongComment s]

>                                -- provisorisch: nur die erste Algebra aus der Liste:
>                                algdefs' | length algdefs == 0 = [] 
>                                         | otherwise           = getAlgDef (head algdefs)
>                                algcode  = compileAlgs algdefs' (mergeAF (getAlgFctsProds 0 il))

%endif
