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



> module WidthAna(

>   TC(..),

>   anaEff,
>   anaEffAll,
>   bestEff,
>   createNameIds,
>   deriveCycleFree,
>   deriveTableNeeded,
>   eaProds,
>   maxNameLength,
>   ppAllEffs,
>   ppEff1,
>   ppEff1b,
>   ppEff1c,
>   ppEff2,
>   ppEffs2,
>   ppGraph,
>   ppGraph4,
>   rev_WidthAna,
>   annotateEff,
>   tabProds,
>   transformEffterms,
>   ppEffExport,
>   exportEffterms

> ) where

> import Data.List
> import Constants
> import Tools
> import StringLib
> import Yieldsize
> import Syntax
> import MathExp
> import Adptrans
> import Poly
> import Track

> import System.IO.Unsafe
> import Foreign
> import Foreign.C.Types
> import Foreign.Ptr

> foreign import ccall "tabulate" tabulate :: Ptr CInt -> Ptr CInt

> rev_WidthAna =  "$Revision$"

Arten von Tabellenkonfigurationen
-----------------------------------

> data TC = TCGood | TCOptimal | TCApprox | TCExtern | TCNone   deriving (Eq, Show)


Namen auf Ints abbilden:
--------------------------

> type NameIdList = [(String, Int)]

> createNameIds :: [Prod] -> NameIdList
> createNameIds pss = zip (map getn pss) [1..] 
>   where
>     getn (n :=== _)        = n
>     getn (DirectDef n _ _) = n

> getName :: NameIdList -> Int -> String
> getName nameIds id = head [ name | (name, id') <- nameIds, id == id' ]

> getId :: NameIdList -> String -> Int
> getId nameIds name = head [ id | (name', id) <- nameIds, name == name' ]


Maximale Produktionsnamenslaenge ermitteln:
----------------------------------------------------

> maxNameLength :: NameIdList -> Int
> maxNameLength pss = maximum $ map  (length.fst) pss


Datentyp Effizienz
----------------------------------------------------

EffUserConf und Eff unterscheiden sich lediglich um den Tabulated-Eintrag.

> --               (Name, Tabulated, Circles, Eff.Term, Runtime)
> type EffUserConf = (Int, TabulationType, Poly, PolyExp, Poly)

> --      (Name, Tabulated, Circles, Eff.Term, Runtime)
> type Eff = (Int, Bool, Poly, PolyExp, Poly)

> ppEff1 :: NameIdList -> Int -> EffUserConf -> String
> ppEff1 nameIds ml (n, t, c, p, r) = n' ++ spc ++ "  =  " ++ ppPolyExp nameIds p  ++"\n"  where
>       n'  = getName nameIds n
>       spc = replicate (max 0 (ml - length n')) ' '

> ppEff1b :: NameIdList -> Int -> EffUserConf -> String
> ppEff1b nameIds ml (n, t, c, p, r) = n' ++ spc ++ "  :  " ++ userconf  ++ "\n"  where
>       n'       = getName nameIds n
>       spc      = replicate (max 0 (ml - length n')) ' '
>       userconf | t == Tabulated     = "Tabulated" 
>                | t == Nontabulated  = "Nontabulated"
>                | otherwise          =  "free"

> ppEff1c :: NameIdList -> Int -> String
> ppEff1c nameIds n = getName nameIds n ++ "\n"  

> ppEffs2 :: NameIdList -> Int -> [Eff] -> String
> ppEffs2 nameIds ml = concatMap (ppEff2 nameIds ml)

> ppEff2 :: NameIdList -> Int -> Eff -> String
> ppEff2 nameIds ml (n, t, c, p, r) = n' ++ spc ++ "  ->  " ++ circ ++ tab ++ runt ++"\n"  where
>       tab      = if t then "   ~~~ Tabulated ~~~  "  else  "                      "
>       circ     = "   " ++ c' ++ " circles  " 
>       runt     = "   Runtime: " ++ ppPoly r
>       c' = if all (==0) c then "0" else ppPoly c
>       n'  = getName nameIds n
>       spc = replicate (max 0 (ml - length n')) ' '

Phase 1: Effizienzterme ermitteln:
----------------------------------------------------

> eaProds :: Bool -> UserFunctions -> TModes -> YSizes -> NameIdList -> [Prod] -> [EffUserConf]
> eaProds tiuc ufs tms ysizes nameIds ps = concatMap (eaProd ufs tms ysizes) ps where

>   eaProd :: UserFunctions -> TModes -> YSizes -> Prod -> [EffUserConf]
>   eaProd ufs tms ysizes (n :=== (_, istab, _, u)) = [(nId', istab', [0], ds, [0])]
>      where
>        (_,_,ds) = ea ufs tms ysizes initys u
>        -- falls Parameter "ignore user conf." gesetzt, dann istab=False:
>        istab' = if tiuc then Freetabulated else istab
>        nId'   = getId nameIds n 
>        initys = case getTMode tms n of
>         MST ->  ST (Number 0, Infinite)
>         MTT ->  TT (Number 0, Infinite) (Number 0, Infinite)


>   ea :: UserFunctions -> TModes -> YSizes -> YSize -> Unit -> (Poly, YSize, PolyExp)
>   ea ufs tms ysizes ysc_unit (Terminal t)       = ([1], ys_t <^> ysc_unit, Poly [1])
>      where
>      (ys_t, _ , _, _) = termDef ufs t


>   ea ufs tms ysizes ysc_unit (Nonterminal nt)  = ([1], ys_nt <^> ysc_unit, PNt ntId')
>      where
>        ys_nt = getYSize ysizes nt 
>        ntId' = getId nameIds nt

Single track:
----------------

>   ea ufs tms ysizes ysc_unit ((p, ST ysc_p) :~~ (q, ST ysc_q))  = (new_n, ys_unit, runtime_unit)
>      where
>      (n_p, ys_p, r_p)  = ea ufs tms ysizes (ST ysc_p) p
>      (_  , ys_q, r_q)  = ea ufs tms ysizes (ST ysc_q) q   -- wir koennen n_q ignorieren, da der Next-Kombinatorbaum nach links waechst

>      ys_unit              = (ys_p <+> ys_q) <^> ysc_unit       -- ys_unit moeglicherweise Einfluss auf runtime!!!!

>      (runtime_unit, new_n) | not (cnst ys_p) && not (cnst ys_q) &&
>                              (limited ys_p || limited ys_q)     = ((Poly looplen :** r_p) :++ ((Poly (looplen *** n_p)) :** r_q), looplen *** n_p)
>                            | not (cnst ys_p) && not (cnst ys_q) = ((n :** r_p) :++ ((Poly ([0,1] *** n_p)) :** r_q),     [0,1] *** n_p)
>                            | otherwise                          = (r_p :++ r_q,                                                    n_p)

>      limited (ST (Number _, Number _)) = True
>      limited _                         = False
>      ys_len (ST (Number i, Number j))  = Number $ j - i
>      ys_len _                          = Infinite
>      looplen                           = [min (ys_len ys_p) (ys_len ys_q)]
>        where
>          min Infinite (Number x) = x
>          min (Number x) Infinite = x
>          min (Number x) (Number y) = if x < y then x else y
>          min Infinite Infinite   = error $ "ea.looplen: min Infinite Infinite"

>      n = Poly [0,1] 

Two track:
------------

>   ea ufs tms ysizes ysc_unit ((p, ysc_p) :~~ (q, ysc_q))  = (new_n, ys_unit, runtime_unit)
>      where
>      (n_p, ys_p@(TT ys_p1 ys_p2), r_p)  = ea ufs tms ysizes ysc_p p
>      (_  , ys_q@(TT ys_q1 ys_q2), r_q)  = ea ufs tms ysizes ysc_q q
>      ys_unit                          = (ys_p <+> ys_q) <^> ysc_unit       -- ys_unit moeglicherweise Einfluss auf runtime!!!!

>      runtime_1 | not (cnstST ys_p1) && not (cnstST ys_q1) = n 
>                | otherwise                                = one

>      runtime_2 | not (cnstST ys_p2) && not (cnstST ys_q2) = n 
>                | otherwise                                = one

>      runtime12 = runtime_1 *** runtime_2

>      -- dieses ist noch nicht ganz zu ende gedacht, momentan ist es einfach eine Kopie von ST
>      -- vielleicht muss man noch beruecksichtigen, dass die Schleifen der beiden Tracks ja
>      -- unabhaengig voneinander laufen
>      (runtime_unit, new_n) = if runtime12 `maxP` [1] then (((Poly runtime12) :** r_p) :++ ((Poly (runtime12 *** n_p)) :** r_q),    runtime12 *** n_p)
>                                                      else (r_p  :++  r_q,                                                                        n_p)

>      n   = [0,1]
>      one = [1]


>   ea ufs tms ysizes ysc_unit ((p, la_p) :^^^ (q, la_q)) =  
>     ea ufs tms ysizes ysc_unit ((p, ysc_lap) :~~ (q, ysc_laq))
>      where
>        ysc_lap  = laBounds ufs la_p
>        ysc_laq  = laBounds ufs la_q


>   ea ufs tms ysizes ysc_unit (p :~~~  q)  =  ea ufs tms ysizes ysc_unit ((p, initys) :~~  (q, initys)) 
>     where
>       initys = case (tma_u tms p, tma_u tms q) of
>         (MST, MST) ->  ST (Number 0, Infinite)
>         (MTT, MTT) ->  TT (Number 0, Infinite) (Number 0, Infinite)
>         otherwise  ->  error "error in mixed single-/two-track definitions!"

>   ea ufs tms ysizes ysc_unit (f :<<< p)    =  ([1], ys, r)
>       where (_, ys,r) = ea ufs tms ysizes ysc_unit p

>   ea ufs tms ysizes ysc_unit (p `With` f)  = ([1], ys, r)
>      where  
>      (_, ys, r)    = ea ufs tms ysizes (ysc_f <^> ysc_unit) p
>      (ysc_f, _, _) = filterDef ufs f


>   ea ufs tms ysizes ysc_unit (p :||| q)  =  ([1], ys_p <|> ys_q, r_p :++ r_q)
>      where
>      (_, ys_p, r_p) = ea ufs tms ysizes ysc_unit p
>      (_, ys_q, r_q) = ea ufs tms ysizes ysc_unit q

>   ea ufs tms ysizes (TT ysc_unit1 ysc_unit2) (TTUnit p q)  =  ([1], TT ys_p ys_q, r_p :++ r_q)
>      where
>      (_, ST ys_p, r_p) = ea ufs tms ysizes (ST ysc_unit1) p
>      (_, ST ys_q, r_q) = ea ufs tms ysizes (ST ysc_unit2) q

>   ea ufs tms ysizes ysc_unit (p :... f)  = ea ufs tms ysizes ysc_unit p

>   ea ufs tms ysizes ysc_unit (ListCompr _ _ _) = ([1], ST (Number 0, Infinite), Poly [1])

>   ea ufs tms ysizes ysc_unit x           = pattErr "WidthAna.lhs: ea" x

 ea ufs tms ysizes (P nt)                       = (ysToW ys_nt, RInt 1)
    where
      ys_nt =   head' [y | (YS n y _) <- filterYSize ysizes,  n == nt] 
                      ("unknown nonterminal " ++ nt)


----------------------------------------------------------------------------------------------

Phase 1b: Ermittle Nichtterminale, die auf jeden Fall tabelliert werden muessen
---------------------------------------------------------------------------------
Diese Phase wird nur zur Ermittlung der optimalen Kandidaten
verwendet.  Wir ermitteln die optimale Laufzeit durch Berechnung der
Laufzeit mit voller Tabellenkonfiguration. Danach wird jeweils ein NT
aus der Konfiguration herausgenommen und dann die Eff. berechnet.
Falls sich hier die Laufzeit verschlechtert, muss dieses NT auf jeden
Fall tabelliert werden.

> deriveTableNeeded tc ds 
>    | tc == TCGood                      = concatMap (\n -> if calcCirc n `maxP` [1]  then [n] else []) [1..count]
>    | tc == TCOptimal || tc == TCApprox || tc == TCExtern
>                                        = concatMap (\n -> if calcEff n > besteff    then [n] else []) [1..count]
>    where
>      count     = length ds
>      calcEff  n = maximum  ( map (\(_,_,_,_,r) -> length(cPoly r)) ((anaEff . anaCirc . (map (userToEff n))) ds))
>      calcCirc n = maximumP ( map (\(_,_,c,_,_) -> cPoly c)         ((         anaCirc . (map (userToEff n))) ds))
>      besteff   = calcEff 0   -- 0 wird als ID nicht verwendet

>      userToEff nontab (n, Tabulated,     c, e, r) = (n, True,  c, e, r)
>      userToEff nontab (n, Nontabulated,  c, e, r) = (n, False, c, e, r)
>      userToEff nontab (n, Freetabulated, c, e, r) | nontab == n = (n, False, c, e, r)
>                                                   | otherwise   = (n, True,  c, e, r)


Phase 1c: Ermittle Nichtterminale, die nicht tabelliert werden muessen
-------------------------------------------------------------------------
Bei der Ermittlung der guten Konf. gilt:
Die aktuelle Effizienztermliste ds enthaelt an dieser Stelle die
User-Annotierung.  Wir koennen also die Funktion anaCirc (s.u.) direkt
auf ds loslassen. Alle Effizienzterme, die nach der Analyse eine [0]
im Feld "c" enthalten, sind nicht an Zyklen beteiligt, und koennen
somit als nicht zu tabellieren vorgemerkt werden:

Fuer optimale Konfigurationen wird die Laufzeit ausgerechnet. Alle
NTs, die eine Konstante Laufzeit haben, muessen nicht tabelliert
werden.

> deriveCycleFree tc tablist ds = (nub result) \\ tablist
>    where
>      result
>        | tc == TCGood    = concatMap (\(n,_,c,_,_) -> if cPoly c == [0]       then [n] else []) ((         anaCirc . (map userToEff)) ds)
>        | tc == TCOptimal || tc == TCApprox || tc == TCExtern
>                          = concatMap (\(n,_,_,_,r) -> if length(cPoly r) == 1 then [n] else []) ((anaEff . anaCirc . (map userToEff)) ds)
>      userToEff (n, Tabulated,     c, e, r) = (n, True,  c, e, r)
>      userToEff (n, Nontabulated,  c, e, r) = (n, False, c, e, r)
>      userToEff (n, Freetabulated, c, e, r) | elem n tablist = (n, True,  c, e, r)
>                                            | otherwise      = (n, False, c, e, r)



Phase 2: Rekursive Abhaengigkeiten ermitteln:
----------------------------------------------------

> anaCirc :: [Eff] -> [Eff]
> anaCirc ds = map (anaCirc' ds) ds
> anaCirc' dall (n, t, c, p, r) = (n, t, c', p, r) where
>    c'   = ana [n] p

>    ana cs (Poly p)   = [0]
>    ana cs (p :++ q)  = ana cs p +++ ana cs q

>    -- hier Problem: rek. Schleifen beruecksichtigen.
>    ana cs ((Poly nn) :** q)  = nn *** ana cs q 
>    ana cs (p :** (Poly nn))  = ana cs p *** nn
>    ana cs (p :** q)          = ana cs p +++ ana cs q  

>    ana cs (PNt nt)   | istab           =  [0]
>                      | n == nt         =  [1]
>                      | elem nt cs      =  [0]
>                      | otherwise       =  ana (nt:cs) getp
>                      where
>                       (istab, getp) = head [ (t,p) | (n', t, _, p, _) <- dall, n' == nt]



Phase 3: Effizienz berechnen:
----------------------------------------------------

> anaEff :: [Eff] -> [Eff]
> anaEff effs = anaEff' [] effs where

>   anaEff' _   []                  = []
>   anaEff' glr ((n, t, c, p, _):e) = (n, t, c, p, r):anaEff' glr' e  where

>     (glr', r) = ana glr [n] (n, t, c, p)

>     ana glr cs (n, t, c, p)    -- falls von n mehr als 1 Schleife wieder zu n zurueckfuehrt,
>                                -- ist das ganze sowieso exponentiell:
>                                | c `maxP`   [1]    = (glr,expo)
>                                -- falls fuer n schon ein Ergbenis in der globales Ergebnisliste glr
>                                -- vorliegt, nehmen wir dieses:
>                                | inGlr             = (glr,result)
>                                -- bei einem Zyklengrad von 1 wird das Ergenis mit n multipliziert:
>                                | cPoly c == [1]    = let (glr',r) =  ec glr p
>                                                          r' = [0,1] *** r
>                                                      in ((n,r'):glr', r')
>                                -- ansonsten wird das von ec berechnete Ergebnis geliefert:
>                                | otherwise         = let (glr',r) = ec glr p
>                                                      in ((n,r):glr', r)
>       where
>       (inGlr, result) | results == [] = (False, [])
>                       | otherwise     = (True, head results)
>          where results = [ r | (n', r) <- glr, n' == n]

>       ec glr (Poly p)  = (glr,p)
>       ec glr (p :** q) = (glr'', p' *** q') where
>                            (glr', p')  = ec glr  p 
>                            (glr'', q') = ec glr' q
>       ec glr (p :++ q) = (glr'', p' +++ q') where
>                            (glr', p')  = ec glr  p 
>                            (glr'', q') = ec glr' q
>       ec glr (PNt nt)  | istab             = (glr,[1])
>                        | not $ elem nt cs  = (glr', eff_nt)
>                        | otherwise         = (glr,[0])
>                        where
>                          getnt@(_, istab, _, _) = head [ (n', t, c, p) | (n', t, c, p, _) <- effs, n' == nt]

>                          (inGlr, result) | results == [] = (False, [])
>                                          | otherwise     = (True, head results)
>                                where results = [ r | (n', r) <- glr, n' == nt]

>                          (glr', eff_nt) | inGlr     = (glr, result)
>                                         | otherwise = ana glr (nt:cs) getnt

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

Graphen ausgeben:
-------------------

Version 1: keine Kanten zu tabellierten Produktionen 

> ppGraph :: NameIdList -> [Eff] ->  String
> ppGraph nameIds effs = "digraph G {\n" ++ concatMap ppGraph' effs ++ "}" where
>   ppGraph' (n, t, c, p, r) = ppG (getNodes p) where
>     ppG [] = getName nameIds n ++ ";\n"
>     ppG x  = concatMap ppG' x 
>     ppG' x = getName nameIds n ++ " -> " ++ getName nameIds x ++ ";\n" 
>   getNodes (a :++ b) = getNodes a ++ getNodes b
>   getNodes (a :** b) = getNodes a ++ getNodes b
>   getNodes (Poly _)  = []
>   getNodes (PNt n)   = if istab n then [] else [n]
>   istab nt           = head [ t | (n', t, _, _, _) <- effs, n' == nt]

Version 2: gestrichelte Kanten

> ppGraph2 :: NameIdList -> [Eff] ->  String
> ppGraph2 nameIds effs = "digraph G {\n" ++ concatMap ppGraph' effs ++ concatMap dottedNode effs ++ "}" where
>   ppGraph' (n, t, c, p, r) = ppG (getNodes p) where
>     ppG [] = getName nameIds n ++ ";\n"
>     ppG x  = concatMap ppG' x 
>     ppG' x = getName nameIds n ++ " -> " ++ getName nameIds x ++ dotted ++ ";\n" where
>       dotted = if istab x then " [style=dotted] " else ""  -- oder invis fuer unsichtbar
>   getNodes (a :++ b) = getNodes a ++ getNodes b
>   getNodes (a :** b) = getNodes a ++ getNodes b
>   getNodes (Poly _)  = []
>   getNodes (PNt n)   = [n]
>   istab nt           = head [ t | (n', t, _, _, _) <- effs, n' == nt]
>   dottedNode (n, False, _, _, _) = ""
>   dottedNode (n, True,  _, _, _) = getName nameIds n ++ " [style=dotted] ;\n"

Version 3: gestrichelte Kanten, Zirkel in _ROT_

TODO:
- Kanten von Knoten, die nicht zweifach rekursiv sind, die aber an einem
  zweifachen  Zirkel beteiligt sind, werden nicht rot gezeichnet
  -> tabtest20

> ppGraph3 :: NameIdList -> [Eff] ->  String
> ppGraph3 nameIds effs = "digraph G {\n" ++ concatMap ppGraph' effs ++ concatMap dottedNode effs ++ "}"
>   where
>   ppGraph' (n, t, c, p, r) = ppG (getNodes p) where
>     ppG [] = getName nameIds n ++ ";\n"
>     ppG x  = concatMap ppG' x 
>     ppG' x = getName nameIds n ++ " -> " ++ getName nameIds x ++ dotted ++ inCircle ++ ";\n" where
>       dotted   = if istab x then " [style=dotted] " else ""  -- oder invis fuer unsichtbar
>       inCircle = if inCirc n x then " [color=red] " else ""
>   getNodes (a :++ b) = getNodes a ++ getNodes b
>   getNodes (a :** b) = getNodes a ++ getNodes b
>   getNodes (Poly _)  = []
>   getNodes (PNt n)   = [n]
>   istab nt           = head [ t | (n', t, _, _, _) <- effs, n' == nt]
>   nodes nt           = head [ p | (n', _, _, p, _) <- effs, n' == nt]
>   circles nt         = head [ c | (n', _, c, _, _) <- effs, n' == nt]
>   dottedNode (n, False, _, _, _) = ""
>   dottedNode (n, True,  _, _, _) = getName nameIds n ++ " [style=dotted] ;\n"
>   inCirc  n x = (circles n) `maxP` [1] && inCirc' n [] x
>   inCirc' n s x | istab x   = False
>                 | n == x    = True
>                 | elem x s  = False
>                 | otherwise = any (inCirc' n (x:s)) (getNodes (nodes x))

Version 4: 

> ppGraph4 :: NameIdList -> [Eff] ->  String
> ppGraph4 nameIds effs = "digraph G {\n" ++ concatMap ppGraph' effs ++ concatMap dottedNode effs ++ "}"
>   where
>   ppGraph' (n, t, c, p, r) = ppG (getNodes False p) where
>     ppG [] = getName nameIds n ++ ";\n"
>     ppG x  = concatMap ppG' x 
>     ppG' (lp,x) = if lp then str ++ str else str where
>       str = getName nameIds n ++ " -> " ++ getName nameIds x ++ dotted ++ inCircle ++ ";\n" 
>       dotted   = if istab x then " [style=dotted] " else ""  -- oder invis fuer unsichtbar
>       inCircle = if inCirc n (lp,x) then " [color=red] " else ""
>   getNodes lp (a :++ b) = getNodes lp a ++ getNodes lp b
>   getNodes lp ((Poly a) :** b) | a == [0,1] || a `maxP` [0,1] = getNodes True b
>   getNodes lp (a :** (Poly b)) | b == [0,1] || b `maxP` [0,1] = getNodes True a
>   getNodes lp (a :** b)                         = getNodes lp a ++ getNodes lp b
>   getNodes lp (Poly _)  = []
>   getNodes lp (PNt n)   = [(lp,n)]
>   istab nt           = head [ t | (n', t, _, _, _) <- effs, n' == nt]
>   nodes nt           = head [ p | (n', _, _, p, _) <- effs, n' == nt]
>   circles nt         = head [ c | (n', _, c, _, _) <- effs, n' == nt]
>   dottedNode (n, False, _, _, _) = ""
>   dottedNode (n, True,  _, _, _) = getName nameIds n ++ " [style=dotted] ;\n"
>   inCirc  n x = (circles n) `maxP` [1] && inCirc' n [] x
>   inCirc' n s (_,x) | istab x   = False
>                     | n == x    = True
>                     | elem x s  = False
>                     | otherwise = any (inCirc' n (x:s)) (getNodes False (nodes x))


-------------------------------------------------------------------------------------------

> ppAllEffs nameIds ml effs = "---------------------------------------------\n" ++ 
>                             ppEffs2 nameIds ml effs ++ "\n" ++ 
>                             ppGraph nameIds effs ++ "\n\n" ++ 
>                             ppGraph3 nameIds effs ++ "\n\n" ++ 
>                             ppGraph4 nameIds effs ++ "\n\n" ++ 
>                             ppMatrix nameIds effs ++ "\n" 

Adjazenzmatrizen ausgeben:

> ppMatrix nameIds effs = ppMatrix1 nameIds effs ++ ppMatrix2 nameIds effs 

> ppMatrix1 :: NameIdList -> [Eff] ->  String
> ppMatrix1 nameIds effs = header ++ body ++ trailer where
>   names = map fst5 effs
>   s = length names
>   header  = "\\begin{vmatrix}\n"
>   trailer = "\\end{vmatrix}\n"
>   body = mapsep "\\\\\n" mline effs ++ "\n"
>   mline (n,t,c,p,r) = mapsep " & " (ppLine (getNodes p)) names where
>     ppLine uses nt | count>0    && not (istab nt) = show count
>                    | otherwise                    = "0"
>       where
>       count = cn 0 nt uses 
>       cn n nt [] = n
>       cn n nt (x:xs) | nt == x   = cn (n+1) nt xs
>                      | otherwise = cn  n    nt xs 
>   getNodes (a :++ b) = getNodes a ++ getNodes b
>   getNodes (a :** b) = getNodes a ++ getNodes b
>   getNodes (Poly _)  = []
>   getNodes (PNt n)   = [n]
>   istab nt           = head [ t | (n', t, _, _, _) <- effs, n' == nt]

> ppMatrix2 :: NameIdList -> [Eff] ->  String
> ppMatrix2 nameIds effs = header ++ body ++ trailer where
>   names = map fst5 effs
>   s = length names
>   header  = "A = ["
>   trailer = "]\n"
>   body = mapsep " ; " mline effs 
>   mline (n,t,c,p,r) = mapsep " " (ppLine (getNodes p)) names where
>     ppLine uses nt | count>0    && not (istab nt) = show count
>                    | otherwise                    = "0"
>       where
>       count = cn 0 nt uses 
>       cn n nt [] = n
>       cn n nt (x:xs) | nt == x   = cn (n+1) nt xs
>                      | otherwise = cn  n    nt xs 
>   getNodes (a :++ b) = getNodes a ++ getNodes b
>   getNodes (a :** b) = getNodes a ++ getNodes b
>   getNodes (Poly _)  = []
>   getNodes (PNt n)   = [n]
>   istab nt           = head [ t | (n', t, _, _, _) <- effs, n' == nt]


--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------

Phasen 2+3 fuer alle Tabellierungen durchfuehren:
----------------------------------------------------

> _includeExp = True

> anaEffAll :: [Int] -> [Int] -> [EffUserConf]-> [[Eff]]
> anaEffAll tablesNeeded noncyclic ds = e where
>                    count    = length ds
>                    -- im tab-feld von ds liegt die aktuelle User-Tabellenkonfiguration
>                    -- diese verwenden wir, um die Liste aller möglichen Konfigurationen 
>                    -- mittels perm zu erzeugen:
>                    tablist    = concatMap (\(n, t, _, _, _) -> if t==Tabulated    then [n] else []) ds
>                    nontablist = concatMap (\(n, t, _, _, _) -> if t==Nontabulated then [n] else []) ds
>                    tabs     = perm (tablist ++ tablesNeeded) (nontablist ++ noncyclic) [1..count]

>                    -- alte Fassung: auch expos werden zurueckgegeben und bei
>                    -- der Auswahl beruecksichtigt --> ueberfluessig
>                    e  | _includeExp  = map (anaEff . anaCirc . (init ds)) tabs

>                    -- neue Fassung: expos werden aussortiert:
>                       | otherwise    = filter (not.isExp) (map (anaEff . anaCirc . (init ds)) tabs)

>                    isExp []               = False
>                    isExp ((_,_,_,_,r):es) = r == expo || isExp es
                     
>                    perm :: [Int] -> [Int] -> [Int] -> [[Bool]]
>                    perm tablist nontablist is = perm' (map mkChoice is)
>                      where
>                        mkChoice i | elem i tablist    = [True] 
>                                   | elem i nontablist = [False]
>                                   | otherwise         = [True, False]
>                        perm' [] = [[]]
>                        perm' (i:is) = [x:xs | x <- i, xs <- perm' is]


>                    init :: [EffUserConf] -> [Bool] -> [Eff]
>                    init [] [] = []
>                    init [] _  = error "this should not happen!!"
>                    init ((n, _, c, p, r):ds) (t:ts) = (n, t, c, p, r): init ds ts


maximale Effizienz fuer gegebene Tabellierung ermitteln:
---------------------------------------------------------

> maxEff :: [Eff] -> (Poly, Int)
> maxEff ws = maxEff' ([0], 0) ws where
>   maxEff' r [] = r
>   maxEff' (rm, tabs) ((_, t, _, _, r):wss) = e where
>      tabCount t     = if t then 1 else 0
>      r'             = r -- if t then multPoly r [0,0,1] else r
>      e              = maxEff' ((sumPoly rm r'), tabs + tabCount t) wss  -- hier max oder sum!!!!

Beste auswaehlen:
----------------------------------------------------

> dim_expo      = dim expo
>   where
>     dim r = [lengthP r - 1]

> bestEff :: TC -> [[Eff]] -> [Eff]     
> bestEff TCGood ws = bestEff' (maxEff first, first) ws where
>   first    = head ws
>   bestEff' (_,     best)        []       = best
>   bestEff' ((rbest,tbest),best) (cur:ws) = e        where
>      (rcur, tcur)  = maxEff cur
>      dim_rcur      = dim rcur
>      dim_rbest     = dim rbest
>      newbest       
>                    | dim_rcur >= dim_expo   = ((rbest,tbest),best)
>                    | tcur < tbest           = ((rcur, tcur),cur)
>                    | tcur   ==   tbest  &&
>                      rcur `minP` rbest      = ((rcur, tcur),cur)
>                    | otherwise              = ((rbest,tbest),best)

Hier noch Reihenfolge anpassen:
1. es darf nicht exponentiell sein
2. die anzahl der tabellen muss kleiner sein
3. falls die Anzahl tasbellen gleich dem besten, dann nehme diejenige mit der kleineren Laufzeit.

>      e     = bestEff' newbest ws

>      dim r = [lengthP r - 1]


> -- for TCOptimal, TCApprox, TCExtern
> bestEff _ ws = bestEff' (maxEff first, first) ws where
>   first    = head ws
>   bestEff' (_,     best)        []       = best
>   bestEff' ((rbest,tbest),best) (cur:ws) = e        where
>      (rcur, tcur)  = maxEff cur
>      dim_rcur      = dim rcur
>      dim_rbest     = dim rbest
>      newbest       
>                    | dim_rcur  <    dim_rbest     = ((rcur, tcur),cur)

>                    | dim_rcur  ==   dim_rbest     &&
>                      tcur      <    tbest         = ((rcur, tcur),cur)

>                    | dim_rcur  ==   dim_rbest     &&
>                      tcur      ==   tbest         &&
>                      rcur   `minP`  rbest         = ((rcur, tcur),cur)

>                    | otherwise                    = ((rbest,tbest),best)

>      e     = bestEff' newbest ws

>      dim r = [lengthP r - 1]



Auswahlstrategien:
- ohne flataf (bzw. flatcf) --> sehr genau
- flataf + sumPoly + minP   --> Haeufigkeit der hoechsten asympt. Effizienzklasse wird beruecksichtigt
- flataf + maxPoly + minP   --> Haeufigkeit wird nicht beruecksichtigt.
- Erweiterung: Haeufigkeit wird beruecksichtigt, allerdings nur fuer minimale Anzahl der tabs. Bsp:
   --> flataf + sumPoly + anderen Guard

f2 ->   runtime: 1     ~~~ Tabulated ~~~       2 * n , 5 tab
f4 ->   runtime: n     ~~~ Tabulated ~~~
f3 ->   runtime: n     ~~~ Tabulated ~~~
g2 ->   runtime: 1     ~~~ Tabulated ~~~
g4 ->   runtime: 1     ~~~ Tabulated ~~~

f2 ->   runtime: 1     ~~~ Tabulated ~~~       3 * n,  4 tab    <=======
f4 ->   runtime: n     ~~~ Tabulated ~~~
f3 ->   runtime: n     ~~~ Tabulated ~~~
g2 ->   runtime: 1     ~~~ Tabulated ~~~
g4 ->   runtime: n  

f2 ->   runtime: 1     ~~~ Tabulated ~~~       4 * n,  4 tab 
f4 ->   runtime: n     ~~~ Tabulated ~~~
f3 ->   runtime: n     ~~~ Tabulated ~~~
g2 ->   runtime: n  
g4 ->   runtime: n     ~~~ Tabulated ~~~

----------------------------------

Todo
----
- Filter beruecksichtigen
- bei ea moeglicherweise Schleifengrenzen beruecksichtigen und 
  optimaleres n eintragen
- (n :** r_p) :++ (n :** r_q) bei ea passt moeglicherweise doch nicht -> siehe tabtest11

    base    ~~-  base     -->  1 + 1        =  2
    region  ~~~  region   -->  n*1  +  n*1  =  2n
    n       ~~~  n        -->  n*n  +  n*n  =  2n^2     ## falsch ##
                          -->  n * (n*n)    =  n^3
    n       ~~~  region   -->  n*n  +  n*1  =  n^2 + n  ||   <-- genauer
                               n * (n*1)    =  n^2

----------------------------------------------------------------------------------------------------

Tabellierungen im Programm einsetzen:
-------------------------------------------

> annotateEff :: [EffUserConf] -> [Int] -> [Eff]
> annotateEff effs tabbed = map ann effs 
>   where
>     ann (n,_,c,e,r) = (n,elem n tabbed,c,e,r)

> getTab effs nameIds n = istab where
>        istab  = head [ t | (n', t, c, p, _) <- effs, n' == nId']
>        nId'   = getId nameIds n 


> tabProds :: [(Nt, TRange)] -> [Eff] -> NameIdList -> [Prod] -> [Prod]
> tabProds ranges effs nameIds ps = map tabProd ps 
>   where
>   tabProd :: Prod -> Prod
>   tabProd (n :=== (cm, _, tr, u)) = (n :=== (cm, tab, range, u))
>     where
>     tab   = if getTab effs nameIds n then Tabulated else Nontabulated
>     range = let r = [ ri | (nt, ri) <-ranges, n == nt ]
>             in if r == [] then tr 
>                           else head r


----------------------------------------------------------------------------------------------------
C - Export
---------------

> type EffExportDep = (Int, Poly)    -- Eine einzelne Abhaengigkeit

> --                (Name, Tabulated, Runtime, Dependencies)
> type EffExportConf = (Int, TabulationType, Poly, [EffExportDep])

> ppEffExport :: NameIdList -> Int -> EffExportConf -> String
> ppEffExport nameIds ml (n, t, r, deps) = n' ++ spc ++ "  ->  " ++ userconf ++ runt ++"\n" ++ concatMap ppDep deps  where
>       userconf | t == Tabulated     = "Tabulated      " 
>                | t == Nontabulated  = "Nontabulated   "
>                | otherwise          = "free           "
>       runt     = "   Runtime: " ++ ppPoly r
>       n'  = getName nameIds n
>       spc = replicate (max 0 (ml - length n')) ' '

>       ppDep (nt, dep) = nt' ++ spc ++ "    -> " ++ ppPoly dep ++ " usages\n"
>         where
>           nt' = getName nameIds nt
>           spc = replicate (max 0 (ml - length nt')) ' '


> transformEffterms :: [EffUserConf] -> [EffExportConf]
> transformEffterms ds = map transform ds
>   where
>     transform (nt, tab, circ, effterm, _) = (nt, tab, calcRuntime nt, calcDeps effterm)

>     runtimes = map (\(nt,_,_,_,r) -> (nt, cPoly r)) ((anaEff . anaCirc . (map tabulateIt)) ds)
>     tabulateIt (n, _, c, e, r) = (n, True,  c, e, r)
>     calcRuntime nt = head' [ r | (n, r) <- runtimes, n == nt] $ "transformEffterms: nonterminal not found: " ++ show nt

>     calcDeps :: PolyExp -> [(Int, Poly)]
>     calcDeps effterm = sumEffs $ calc effterm
>       where
>       calc :: PolyExp -> [(Int, Poly)]
>       calc (Poly p)    = []
>       calc (PNt nt)    = [(nt, [1])]
>       calc (p1 :++ p2) = calc p1 ++ calc p2
>       calc (p1 :** p2) = map multEff (calc p1 ++ calc p2)
>         where
>           multEff (nt, nteff) = (nt, eff)
>           eff = calcP p1 *** calcP p2

>       calcP (Poly p) = p
>       calcP (PNt _)  = [1]
>       calcP (p1 :++ p2) = calcP p1 +++ calcP p2
>       calcP (p1 :** p2) = calcP p1 *** calcP p2

>     sumEffs :: [(Int, Poly)] -> [(Int, Poly)]
>     sumEffs deps = map summ nts
>       where
>       nts = nub $ map fst deps
>       summ nt = (nt, foldr1 (+++) [ eff | (n, eff) <- deps, n == nt])



> exportEffterms :: Integer -> TC -> Bool -> Int -> Int -> Int -> NameIdList -> [(Nt, Poly)] -> [EffUserConf] -> [Int]
> exportEffterms vl tc returnResults tadd taddc taddn nameIds range ds = unsafePerformIO $ expo eff
>   where
>     eff = transformEffterms ds 

>     expo :: [EffExportConf] -> IO [Int]
>     expo effs  = exportArr $ serialize effs

>     serialize effs = [tctype tc] ++ [returnMode] ++ [vlevel vl] ++ add_tables ++ [length effs] ++ concatMap seri effs
>       where
>         seri (n, tab, runtime, deps) = [n] ++ [userconf tab] ++ serPoly (getRange n) ++ serPoly runtime ++ [length deps] ++ concatMap seriDep deps 
>         userconf tab | tab == Tabulated     = 0
>                      | tab == Nontabulated  = 1
>                      | otherwise            = 2
>         tctype TCGood    = 1
>         tctype TCOptimal = 2
>         tctype TCApprox  = 3
>         tctype TCExtern  = 4 
>         tctype x         = pattErr "exportEffterms.tctype" x
>         returnMode       = if returnResults then 2 else 1
>         vlevel vl | vl == target = 1
>                   | vl == trace  = 2
>                   | vl >  trace  = 3
>         add_tables = [tadd, taddc, taddn]
>         getRange n = head' [ p | (nt, p) <- range, getName nameIds n == nt] $ "exportEffterms: index range not found: " ++ show n
>         seriDep (nt, dep) = [nt] ++ serPoly dep

>         serPoly dep = [length dep] ++ dep


> exportArr :: [Int] -> IO [Int]
> exportArr ls = do 
>     ioArr <- newArray (map fromIntegral ls )
>     result <- peekArray0 0 (tabulate ioArr)
>     -- putStrLn "Returned to Haskell:"
>     -- print result
>     return (map fromIntegral result)

