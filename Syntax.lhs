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
\section{Quellsprache}
%----------------------------------------------------------------------------------------------------

%----------------------------------------------------------------------------------------------------
\subsection{abstrakte Syntax}
%----------------------------------------------------------------------------------------------------

> module Syntax(

>   Unit(..),
>   CombParams(..),
>   Prod(..), ProdSource(..),
>   LCExp(..), ListCompr,

>   Nt, Function,
>   Term, Filter, Axiom, TabulationType(..),
>   TRange, TRanges,

>   prodSourceToProd,
>   getTRange, ppTRange, ppTRanges, ppTRangesElem, prodToTRanges,
>   collectTabulated,
>   collectUnit,
>   mapUnit,
>   paren,
>   ppCombParams,
>   ppTerm,
>   rev_Syntax,
>   stdInd,
>   wrkUnit,

> ) where

> import Tools
> import Constants
> import MathExp
> import Expr
> import PrettyPrint

> rev_Syntax =  "$Revision$"

> infix  8 :<<<
> infixl 7 :~~~ , :~~ , :~~! 
> infixr 6 :|||
> infix  5 :...
> infix `With`

Der Datentyp für Produktionen in etwas erweiterter Form. Enthält
jetzt zusätzlich einen Kommentar und eine Liste von Produktionen. Als
Kommentar kann jeweils ein String gespeichert werden, der bei der
Codeausgabe zusammen mit dem Code der Produktion ausgegeben wird
(z.B. die errechnete yield size). Dies ist sehr hilfreich, um die
Ergebnisse der Transformation nachvollziehen zu können.

Die Liste [Prod] ist notwendig zur Verarbeitung verschachtelter
where-Konstruktionen. Jede Produktion kann also eine Liste weiterer
Produktionen enthalten. z.B.:

-- > bsp1 = ["test" :=== ("",Nonterminal "a" :~~~ Nonterminal "b",
-- >                                              ["a" :=== ("",Terminal "base",[]),
-- >                                               "b" :=== ("",Terminal "region",[])])]      

< Yieldsize> ppProds bsp1
< test = a ~~~ b
<      where
<      a = base
<      b = region   

> data CombParams = CombYSize YSize YSize          |    -- Schleifenbereichs-einschraenkende Kombinatoren
>                   CombLA    Function Function    |    -- LookAhead Kombinatoren
>                   CombDB    YSize YSize Int           -- Distance Bound Kombinatoren

>                                deriving (Show, Eq)

> type Axiom = String

> data TabulationType = Tabulated | Nontabulated | Freetabulated
>                                deriving (Show, Eq)

> data ProdSource = Nt :==== (Comment, TabulationType, TRange, Unit, [ProdSource])   |
>                   DirectDefSource Nt IndexVars  ListCompr                          | 
>                   CombDef  VarNext  CombParams
>                                deriving (Show, Eq)

> data Prod = Nt :===  (Comment, TabulationType, TRange, Unit)                       |
>             DirectDef Nt IndexVars  ListCompr
>                                deriving (Show, Eq)

> -- simple converter, only for debugging
> prodSourceToProd :: [ProdSource] -> [Prod]
> prodSourceToProd = concatMap conv
>   where
>     conv (nt :==== (a,b,c,d,wps)) = [nt :=== (a,b,c,d)] ++ concatMap conv wps
>     conv (DirectDefSource a b c)  = [DirectDef a b c]
>     conv (CombDef _ _)            = []

> collectTabulated :: [Prod] -> [Nt]
> collectTabulated  = concatMap collect 
>   where
>     collect (name :=== (_, Tabulated, _, _)) = [name]
>     collect _                                = []


> type TRange = TrackMode (Bool, Bool)
> ppTRange r = case r of
>                (ST a)   -> pp a
>                (TT a b) -> "(" ++ pp a ++ "," ++ pp b ++ ")"
>   where
>     pp (a, b) = "(" ++ ppBool a ++ "," ++ ppBool b ++ ")"
>     ppBool False = "no"
>     ppBool True  = "yes"

> type TRanges = Assoc Nt TRange
> getTRange :: TRanges -> Nt -> TRange
> --getTRange ranges nt = lookupA1 ranges nt "TRange"
> getTRange ranges nt = case [ b | (a,b) <- ranges, a == nt ] of
>                         [] -> ST (True,True)
>                         xs -> head xs
> ppTRanges :: TRanges -> String
> ppTRanges = ppAssoc ppTRange
> ppTRangesElem :: (Nt, TRange) -> String
> ppTRangesElem = ppAssocElem ppTRange

> prodToTRanges :: [Prod] -> TRanges
> prodToTRanges = map (\(n :=== (_,_,ri,_)) -> (n,ri))

> trangeToFct :: TRange -> String
> trangeToFct (ST (True, True))   = "tabulated"
> trangeToFct (ST (False, True))  = "listedj"
> trangeToFct (ST (True, False))  = "listed"
> trangeToFct (TT (True, True) (True, True))     = "tabulated1111"
> trangeToFct (TT (True, True) (True, False))    = "tabulated1110"
> trangeToFct (TT (True, True) (False, True))    = "tabulated1101"
> trangeToFct (TT (True, True) (False, False))   = "tabulated1100"
> trangeToFct (TT (True, False) (True, True))    = "tabulated1011"
> trangeToFct (TT (True, False) (True, False))   = "tabulated1010"
> trangeToFct (TT (True, False) (False, True))   = "tabulated1001"
> trangeToFct (TT (True, False) (False, False))  = "tabulated1000"
> trangeToFct (TT (False, True) (True, True))    = "tabulated0111"
> trangeToFct (TT (False, True) (True, False))   = "tabulated0110"
> trangeToFct (TT (False, True) (False, True))   = "tabulated0101"
> trangeToFct (TT (False, True) (False, False))  = "tabulated0100"
> trangeToFct (TT (False, False) (True, True))   = "tabulated0011"
> trangeToFct (TT (False, False) (True, False))  = "tabulated0010"
> trangeToFct (TT (False, False) (False, True))  = "tabulated0001"
> trangeToFct (TT (False, False) (False, False)) = "tabulated0000"
> trangeToFct x = pattErr "trangeToFct"  x

> type IndexVars = TrackMode (String, String)
> ppIndexVars s = pp s 
>   where
>     pp (ST s)     = ppij s
>     pp (TT s1 s2) = ppij s1 ++ " " ++ ppij s2
>     ppij (i,j)       = "(" ++ show i ++ "," ++ show j ++ ")"

> type ListCompr = (Exp, [LCExp])
> ppListCompr ind (body, exprs)  = "[" ++ pretty body ++ " | " ++ mapsep (",\n" ++ spc (ind + stdInd)) ppLCExp exprs ++ "]"

> data LCExp = LCExp Exp |
>              LCUnit Exp Unit SubScripts    deriving (Show, Eq)
> ppLCExp (LCExp e) = pretty e
> ppLCExp (LCUnit e u ss) = pretty e ++ " <- " ++ pretty' False u ++ " " ++ ppSubScripts ss

Unit enthält zusätzlich zu den Konstruktoren für die
Standardkombinatoren die Konstruktoren |:~~| und |:~~!|. 

\begin{itemize}
\item In der ersten Phase der Transformation werden die Next-Kombinatoren
  (|:~~~|) komplett durch Next-Kombinatoren mit konkreten yield sizes
  ersetzt (|:~~|).  
\item In Phase 2 werden diese dann durch einen konkreten Kombinator
  ersetzt (|:~~!|). VarNext enhält dann entweder einen vordefinierten
  Spezialfall (|-~~|, |~~-|, |~~+|, ...) oder einen neudefinierten
  Kombinator der Form |~~!|, |~~!!|, ..., dessen Definition mit ausgegeben
  werden muß.
\end{itemize}

> data Unit = 
>   Terminal          Term                        |
>   Nonterminal       Nt                          |   
>   ListCompr         ListCompr IndexVars YSize   |
>   AlgAppl           :<<<    Unit                |
>   Unit              :|||    Unit                |
>   Unit              `With`  Filter              |
>   Unit              :...    Function            |
>   Unit              :~~~    Unit                |
>   (Unit, YSize)     :~~     (Unit, YSize)       |
>   Unit              :~~!    (VarNext, Unit)     |
>   (Unit, Function)  :^^^    (Unit, Function)    |
>   (Unit, YSize)     :/\\/   (Unit, YSize, Int)  |
>   TTUnit            Unit    Unit

>                                deriving (Show, Eq)

> type Term  = (String, [String])
> ppTerm (a, args)  = if args==[] then a else "(" ++ a ++ " " ++ sepList " " args ++ ")"

> type Nt       = String

> type Filter   = (String, [String])
> type Function = String
> type VarNext  = String
> type Comment  = String


Prettyprinter für Produktionen. Kann in allen Phasen der
Transformation genutzt werden:

Standard-Einrückung:

> stdInd :: Int
> stdInd = 5

Ausgabe von ~~* und Co direkt als Prettyprinter (was ja auch am
logischten ist, die Dinger werden beim Parsen verallgemeinert und
beim Prettyprinten wieder spezisiert):

> ppCombParams (CombYSize (ST(Number l, Infinite))  (ST(Number r, Infinite))) = 
>        "(*~*) " ++ show l ++ " " ++ show r

> ppCombParams (CombYSize (ST(Number l, Infinite))  r)                    = 
>        "(*~~) " ++ show l ++ " " ++ ppYSize r

> ppCombParams (CombYSize l (ST(Number r, Infinite)))                     = 
>        "(~~*) " ++ ppYSize l ++ " " ++ show r

> ppCombParams (CombYSize l@(ST _)  r@(ST _))                             = 
>        "(~~) " ++ ppYSize l ++ " " ++ ppYSize r

> ppCombParams (CombYSize l@(TT _ _)  r@(TT _ _))                        = 
>        "(!~~) " ++ ppYSize l ++ " " ++ ppYSize r


> ppCombParams (CombLA fl fr)  = "(^^^) " ++ fl ++ " " ++ fr
> ppCombParams (CombDB l@(TT _ _)  r@(TT _ _) d)                         = 
>        "(/\\\\/) " ++ ppYSize l ++ " " ++ ppYSize r ++ " " ++ show d



> instance Pretty Prod where
>      pretty = ppProd True 0
>      pretty' False = ppProd False 0

> instance Pretty ProdSource where
>      pretty = ppProdSource True 0
>      pretty' False = ppProdSource False 0

> ppProdSource :: Bool -> Int -> ProdSource -> String
> ppProdSource ppa ind (a :==== (cm, tab, tr, b, ps)) = ppcm ++ ppLit ppa ++ spc ind ++ a ++ " = " ++ ppto ++ ppu ++ pptc ++ ppwh where
>                                   ppcm = if cm == "" then "" else "\n" ++ ppLit ppa ++ spc ind ++ cm
>                                   ppto = if tab == Tabulated then trangeToFct tr ++ "(\n" ++ ppLit ppa ++ spc (ind + 3 + length a) else ""
>                                   pptc = if tab == Tabulated then ")" else ""
>                                   ppu  = prettyOpts (PPOptList [PPOptBool ppa, PPOptInt (ind + 3 + length a)]) b
>                                   ppwh = case ps of
>                                           [] ->  "\n" 
>                                           otherwise -> "\n" ++ ppLit ppa ++ spc (ind + stdInd) ++ "where\n"
>                                                        ++ concatMap (ppProdSource ppa (ind + stdInd)) ps 
>                                                        ++ if not ppa then 
>                                                              "\n" ++ ppLit ppa ++ spc (ind + stdInd) ++ "--endwhere\n"
>                                                           else []
> ppProdSource ppa ind (DirectDefSource name ijs listcomp) = spc ind ++ name ++ " " ++ ppIndexVars ijs ++  " =" ++ ppListCompr ind listcomp
> ppProdSource ppa ind (CombDef n params) = (if not ppa then ppLit ppa ++ spc ind ++ "infixl 7 " ++ n ++ "\n" else "")
>                                            ++ ppLit ppa ++ spc ind ++ "(" ++ n ++ ") = " ++ ppCombParams params ++ "\n"


> ppProd :: Bool -> Int -> Prod -> String
> ppProd ppa ind (a :=== (cm, tab, tr, b)) = ppcm ++ ppLit ppa ++ spc ind ++ a ++ " = " ++ ppto ++ ppu ++ pptc ++ "\n" where
>                                   ppcm = if cm == "" then "" else "\n" ++ ppLit ppa ++ spc ind ++ cm
>                                   ppto = if tab == Tabulated then trangeToFct tr ++ "(\n" ++ ppLit ppa ++ spc (ind + 3 + length a) else ""
>                                   pptc = if tab == Tabulated then ")" else ""
>                                   ppu  = prettyOpts (PPOptList [PPOptBool ppa, PPOptInt (ind + 3 + length a)]) b
> ppProd ppa ind (DirectDef name ijs listcomp) = spc ind ++ name ++ " " ++ ppIndexVars ijs ++  " =" ++ ppListCompr ind listcomp


> instance Pretty Unit where
>      pretty x = ppUnit x True False 0
>      pretty' b x = ppUnit x b False 0
>      prettyOpts (PPOptList [PPOptBool b, PPOptInt i]) x = ppUnit x b False i


> ppUnit :: Unit -> Bool -> Bool -> Int -> [Char]   
> ppUnit (Terminal t)            ppa spa ind = ppTerm t
> ppUnit (Nonterminal a)         ppa spa ind = a
> ppUnit (ListCompr lc _ _)      ppa spa ind = ppListCompr ind lc
> ppUnit (a :<<< b)              ppa spa ind = paren (ppa || spa) (ppAlgAppl a ++ " <<< " ++ ppUnit b ppa True ind)
> ppUnit (a :~~~ b)              ppa spa ind = paren ppa (ppUnit a ppa spa ind ++ " ~~~ " ++ ppUnit b ppa spa ind)
> ppUnit ((p, pys) :~~ (q, qys)) ppa spa ind = paren ppa (ppUnit p ppa spa ind ++ 
>                                             " ((~~) " ++ ppYSize pys ++ " "  ++ 
>                                                          ppYSize qys ++ ") " ++
>                                             ppUnit q ppa spa ind) 
> ppUnit (a :~~! (vNext, b))     ppa spa ind = paren ppa (ppUnit a ppa spa ind ++ " " ++ vNext ++ " " ++ ppUnit b ppa spa ind)
> ppUnit ((p, pf) :^^^ (q, qf))  ppa spa ind = paren ppa (ppUnit p ppa spa ind ++ 
>                                             " ((^^^) " ++ pf ++ " "  ++ qf ++ ") " ++
>                                             ppUnit q ppa spa ind)
> ppUnit ((p, pys) :/\\/ (q, qys, d)) ppa spa ind = paren ppa (ppUnit p ppa spa ind ++ 
>                                             " ((/\\\\/) " ++ ppYSize pys ++ " "  ++ 
>                                                              ppYSize qys ++ " " ++ show d ++ ") " ++
>                                             ppUnit q ppa spa ind) 
> ppUnit (a :||| b)              ppa spa ind = paren (ppa || spa) (ppUnit a ppa spa ind ++ " ||| " ++ newline  ++ ppUnit b ppa spa ind)
>                                              where 
>                                                newline = if spa then "" else "\n" ++ ppLit ppa ++ spc ind
> ppUnit (a `With` (f, args))    ppa spa ind = "((" ++ ppUnit a ppa spa ind  ++ ") `with` " ++ 
>                                             (if args == [] then f else "(" ++ f ++ " " ++ mapsep " " pparg args ++ ")" ) ++ ")" 
>     where pparg s = let (a,b) = span (/= '.') s
>                     in if (b == ".0") then a else s
> ppUnit (a :... b)              ppa spa ind = paren ppa (ppUnit a ppa spa ind  ++ " ... "  ++ b)
> ppUnit (TTUnit a b)            ppa spa ind = "tt(" ++ ppUnit a ppa spa ind ++ ", " ++ ppUnit b ppa spa ind ++ ")"

> paren ppa x = if ppa then "(" ++ x ++ ")" else x
> ppLit True  = ""
> ppLit False = ">   "


structural recursion over units:
----------------------------------

> wrkUnit wrk appFct nil (Terminal a)                = nil
> wrkUnit wrk appFct nil (Nonterminal a)             = nil
> wrkUnit wrk appFct nil (ListCompr lc _ ys)         = nil -- TODO: hier ggf. auch reingehen
> wrkUnit wrk appFct nil (f :<<< a)                  = wrk a
> wrkUnit wrk appFct nil (a :||| b)                  = appFct (wrk a) (wrk b)
> wrkUnit wrk appFct nil (a `With` f)                = wrk a
> wrkUnit wrk appFct nil (a :... f)                  = wrk a
> wrkUnit wrk appFct nil (p :~~~ q)                  = appFct (wrk p) (wrk q)
> wrkUnit wrk appFct nil ((p, p_c) :~~ (q, q_c))     = appFct (wrk p) (wrk q)
> wrkUnit wrk appFct nil (p       :~~! (vn, q))      = appFct (wrk p) (wrk q)
> wrkUnit wrk appFct nil ((p, p_c) :/\\/ (q, q_c,d)) = appFct (wrk p) (wrk q)
> wrkUnit wrk appFct nil ((p, la_p) :^^^ (q, la_q))  = appFct (wrk p) (wrk q)
> wrkUnit wrk appFct nil (TTUnit a b)                = appFct (wrk a) (wrk b)
> -- wrkUnit _    _      _   x                           = pattErr "wrkUnit" x

> mapUnit wrk x = snd $ collectUnit wrk' 0 x where
>    wrk' cll x = (cll, wrk x)

> collectUnit wrk cll (Terminal a)                = (cll, Terminal a)
> collectUnit wrk cll (Nonterminal a)             = (cll, Nonterminal a)
> collectUnit wrk cll (ListCompr (e,defs) iv ys)  = (cll', ListCompr (e, defs') iv ys)       where
>       (cll', defs') = collectLCs cll defs
>       collectLCs cll []     = (cll, [])
>       collectLCs cll (d:ds) = (cll'', d':ds')
>         where
>           (cll', d')   = collectLC cll d
>           (cll'', ds') = collectLCs cll' ds
>       collectLC cll (LCExp e)       = (cll, LCExp e)
>       collectLC cll (LCUnit e u ss) = (cll', LCUnit e u' ss)
>         where
>           (cll', u') = wrk cll u
> collectUnit wrk cll (f :<<< a)                  = (cll', f :<<< a')                    where
>       (cll', a') = wrk cll a
> collectUnit wrk cll (p :||| q)                  = (cll'', p' :||| q')                  where
>       (cll', p') = wrk cll  p
>       (cll'',q') = wrk cll' q
> collectUnit wrk cll (a `With` f)                = (cll', a' `With` f)                  where
>       (cll', a') = wrk cll a
> collectUnit wrk cll (a :... f)                  = (cll', a' :... f)                    where
>       (cll', a') = wrk cll a
> collectUnit wrk cll (p :~~~ q)                  = (cll'', p' :~~~ q')                  where
>       (cll', p') = wrk cll  p
>       (cll'',q') = wrk cll' q
> collectUnit wrk cll ((p, p_c) :~~ (q, q_c))     = (cll'',(p', p_c) :~~ (q', q_c))      where
>       (cll', p') = wrk cll  p
>       (cll'',q') = wrk cll' q
> collectUnit wrk cll (p       :~~! (vn,q))       = (cll'',p' :~~! (vn,q'))              where
>       (cll', p') = wrk cll  p
>       (cll'',q') = wrk cll' q
> collectUnit wrk cll ((p, p_c) :/\\/ (q, q_c,d)) = (cll'',(p', p_c)  :/\\/ (q', q_c,d)) where
>       (cll', p') = wrk cll  p
>       (cll'',q') = wrk cll' q
> collectUnit wrk cll ((p, la_p) :^^^ (q, la_q))  = (cll'',(p', la_p) :^^^  (q', la_q))  where
>       (cll', p') = wrk cll  p
>       (cll'',q') = wrk cll' q
> collectUnit wrk cll (TTUnit p q)                = (cll'',TTUnit p' q')                 where
>       (cll', p') = wrk cll  p
>       (cll'',q') = wrk cll' q
> -- collectUnit _ _   x                              = pattErr "collectUnit" x

