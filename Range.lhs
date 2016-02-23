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



> module Range(

>   analyseDeps,
>   analyseDeps2,
>   analyseRange,
>   rangeToPoly,
>   ppAnalyseDeps,
>   ppAnalysedRange,
>   rev_Range,

> ) where

> import Data.List
> import Parse
> import Syntax
> import Dss
> import Tools
> import Poly
> import MathExp
> import StringLib
> import Track
> import Yieldsize
> import Adptrans
> import PrettyPrint

> rev_Range =  "$Revision$"

----------------------------------------------------------------------------------------------------
neuer Ansatz: Ausgehend von dss-Ergebnissen
TODO: funktioniert nicht in mixed ST/TT-Programmen.

> ppAnalysedRange (nt, r) = nt ++ " : " ++ ppTRange r

> rangeToPoly :: [(Nt, TRange)] -> [(Nt, Poly)]
> rangeToPoly ranges = map (\(nt, rind) -> (nt, rtp rind)) ranges
>   where
>     rtp (ST (a,b))       = mkP $ b2n a + b2n b
>     rtp (TT (a,b) (c,d)) = mkP $ b2n a + b2n b + b2n c + b2n d

>     b2n True  = 1
>     b2n False = 0
>     mkP n = replicate n 0 ++ [1]

> analyseRange :: [String] -> TModes -> [(Nt,[Nt])] -> [ILProd] -> [(Nt, TRange)]
> analyseRange crfilter tms deps ps = (fixit merge init count nts $ "Index range analysis does not terminate!\n" ++ 
>                                                                   "Probably your input grammar is incorrect.\n")
>                                     ++ map (\s -> ("contains_" ++ s, ST (True, True))) crfilter
>   where
>     init  = map setRange nts
>     count = length nts

>     nts   = map fst3 ps
>     units = map thd3 ps

>     setRange nt = let ss = usage nt 
>                   in case getTMode tms nt of
>                        MST -> (nt, ST (setR $ getI ss,  setR $ getJ  ss))
>                        MTT -> (nt, TT (setR $ getI1 ss, setR $ getJ1 ss) 
>                                       (setR $ getI2 ss, setR $ getJ2 ss))

>     setR s = let nubs = (nub s)
>              in length nubs > 1 || (length nubs == 1 && not (elem (head nubs) 
>                                                              [Var "i", Var "j", Var "i1", Var "j1", Var "i2", Var "j2"]))

>     getI ss  = map (\(ST (i,j)) -> i) ss
>     getJ ss  = map (\(ST (i,j)) -> j) ss
>     getI1 ss = map (\(TT (i1,j1) (i2,j2)) -> i1) ss
>     getI2 ss = map (\(TT (i1,j1) (i2,j2)) -> i2) ss
>     getJ1 ss = map (\(TT (i1,j1) (i2,j2)) -> j1) ss
>     getJ2 ss = map (\(TT (i1,j1) (i2,j2)) -> j2) ss

>     usage nt = map snd $ filter (\(name, _) -> name == nt) usages
>     usages = concatMap anr units
>     anr (ILTerminal _ _)     = []
>     anr (ILNonterminal n ss) = [(n,ss)]
>     anr (p :/~~~/ q)         = anr p ++ anr q
>     anr (p :/|||/ q)         = anr p ++ anr q
>     anr (p :/.../ _)         = anr p 
>     anr (p `ILwith` _)       = anr p
>     anr (_ :/<<</ p)         = anr p
>     anr (ILTTUnit p q)       = anr p ++ anr q
>     -- TODO:
>     anr (ILListCompr _ _)    = []

>     getDeps nt = head' [ dep | (n, dep) <- deps, nt == n] $ "analyseRange: deps not found for " ++ nt

>     merge :: [(Nt, TRange)] -> Nt -> (Nt, TRange)
>     merge ranges nt = case getTMode tms nt of
>                          MST -> (nt, ST (or $ getI deps,  or $ getJ  deps))
>                          MTT -> (nt, TT (or $ getI1 deps, or $ getJ1 deps) 
>                                         (or $ getI2 deps, or $ getJ2 deps))
>          where
>            deps = [rng | (n, rng) <- ranges, elem n (getDeps nt)]


----------------------------------------------------------------------------------------------------


> ppAnalyseDeps (nt, deps) = nt ++ " depends on: " ++ sepList ", " deps
> analyseDeps axiom ps = map (\nt -> (nt, dep nt)) nts

>   where

>     nts = map fst3 ps

>     dep nt = nub $ concatMap snd $ filter (\(name, _) -> name == nt) deps
>     deps = nub $ ad [axiom] [] (getDef axiom)
>     ad stack cll (ILTerminal _ _)     = cll
>     ad stack cll (ILNonterminal nt _) 
>                                       | elem nt stack         = cll ++ [(nt, nt:stack)]
>                                       | elem nt (map fst cll) = cll
>                                       | otherwise             = ad (nub (nt:stack)) (nub (cll ++ [(nt, nt:stack)])) $ getDef nt
>     ad stack cll (p :/~~~/ q)         = ad stack (ad stack cll p) q
>     ad stack cll (p :/|||/ q)         = ad stack (ad stack cll p) q
>     ad stack cll (p :/.../ _)         = ad stack cll p 
>     ad stack cll (p `ILwith` _)       = ad stack cll p
>     ad stack cll (_ :/<<</ p)         = ad stack cll p
>     ad stack cll (ILTTUnit p q)       = ad stack (ad stack cll p) q
>     -- TODO
>     ad stack cll (ILListCompr _ _)    = []

>     getDef nt = head' [def | (n,_,def) <- ps, n == nt] $ "analyseDeps: unknown nonterminal " ++ nt


> -- zweite Implementierung: effizienter 
> analyseDeps2 axiom ps = depend $ reachable $ usage ps

>   where
>     usage ps = map (\(n, _, def) -> (n, nub $ use def)) ps
>       where 
>         use (ILTerminal _ _)     = []
>         use (ILNonterminal nt _) = [nt]
>         use (p :/~~~/ q)         = use p ++ use q
>         use (p :/|||/ q)         = use p ++ use q
>         use (p :/.../ q)         = use p 
>         use (p `ILwith` q)       = use p 
>         use (p :/<<</ q)         = use q 
>         use (ILTTUnit p q)       = use p ++ use q
>         -- TODO
>         use (ILListCompr _ _)    = []

>     reachable uses = fixit reach uses (length uses) (map fst uses) "analyseDeps: fixpoint iteration does not terminate"
>        where
>          reach uses n = let use = getUse uses n 
>                         in  (n, nub $ use ++ concatMap (getUse uses) use)
>          getUse uses n = head' [ use | (nt, use) <- uses, n == nt ] $ "analyseDeps2: nonterminal not found " ++ n

>     depend uses = map dep (map fst uses)
>        where
>          dep n = (n, nub ([n] ++ map fst (filter (\(nt, use) -> elem n use) uses)))



