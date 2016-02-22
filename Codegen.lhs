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



TODO - Tabellenaccess
---------------------
V bei -ta b Speichergroesse +1 !!!
V -ita funktioniert nicht mit Offset
V speichergroessenangabe fuer block in alloc auch mit hilfe von indexmap
- warum ist tb langsamer als b? offset-tabellenzugriff?
- fortran und pascal
- yieldsize mit reinrechnen 
- twotrack

V shift und block vielleicht auch mit offset (mgl. wahlweise)
V triang., offset und shifted beliebig kombinierbar. z.B. bt, bto, bo, bs, btos, b, bts, bos
- spalten- und zeilenweise wahlweise

> module Codegen(

>   IMMode(..),
>   codegen,
>   freemem,
>   freemem',
>   getYSize,
>   ppV,
>   rev_Codegen

> ) where

> import Time
> import Char
> import List
> import Constants   -- globale Konstanten
> import Tools
> import Track
> import StringLib
> import MathExp
> import Expr
> import Syntax
> import Dss
> import TLData
> import TL
> import Structs
> import IL2
> import PrettyPrint

> import Algebras

> import Range
> import Beautify

> rev_Codegen =  "$Revision$"

%----------------------------------------------------------------------------------------------------
\subsection{Codeerzeugung}
%----------------------------------------------------------------------------------------------------

aus IL2:

> ppV = ppVarAccessC

> tvN n = Direct $ "v" ++ show n -- ArrayElem [Number n] (Direct "v")

> newtv tvs dt = (v, tvs') 
>   where 
>     v    = (tvN (length tvs + 1), dt)
>     -- unique names:
>     -- v    = (Direct $ specialChoiceName ("v" ++ show (length tvs + 1)) dt, dt)
>     tvs' = v:tvs


specialChoiceName f dt = f ++ "_" ++ filter isAlphaNum (show dt)

> freemem' opts freeStructCode v@(_,dt) 
>   | not (freeNeeded opts) = []
>   | isListStruct v        = [commentLn $ "free " ++ ppV v] ++ 
>                             [TLAssign helper (TLVar v),
>                              TLWhileNN helper (
>                                freeStructCode ++
>                                [TLAssign helper (tlvar ((fst helperc) :. (Direct "next"))),
>                                TLFA "free" [TLVar v],
>                                TLAssign v (TLVar helper)])]
>   | otherwise = error $ "freemem: tried to generate code for freeing atomar variable " ++ ppV v ++ "."
>                    
>   where
>     helper  = (Direct "hlp", TLVoid)
>     helperc = (Pointer (Cast dt (fst helper)), dt)

> freemem opts v                                          | freeNeeded opts = freemem' opts []             v
>                                                         | otherwise       = []
> freememStructList opts (sigName, enumAlgName) v@(_,dt)  | freeNeeded opts = freemem' opts (freeStructCode opts (sigName, enumAlgName) helperc) v
>                                                         | otherwise       = []
>   where
>     helper  = Direct "hlp"
>     helperc = Cast dt helper

      
> freeStructCode opts (sigName, enumAlgName) v 
>    | freeNeeded opts = [TLFA fctname [tlvar (((Pointer v):.(Direct "item")):.(Direct enumAlgName))]]
>    | otherwise       = []
>    where 
>      fctname        = pfree prefixes ++ pstr prefixes ++ sigName



> rangeAlt False r = Number obsoleteTableDim
> rangeAlt True  r = r 

> -- notwendig, fuer TT-Nichtterminale, fuer die keine expliziten
> -- Index-Ranges angegeben wurden:
> getRangeTT ranges n = case getTRange ranges n of
>      TT (b1, b2) (b3, b4) -> (b1,b2,b3,b4)
>      ST (True, True)      -> (True,True,True,True) -- Init. vom Parser
>      x                    -> pattErr "getRangeTT" x

Indizes umrechnen:
--------------------

> -- Index-map-Mode: IMA -> Access, IMM -> Makro
> data IMMode = IMA | IMM

> optSub a (Number 0) = a
> optSub a b          = a :- b 


> calculateOffset opts mode ysizes (ranges, tabs) n (ST (i,j)) = shifted
>   where
>    init    | elem TASTriangularBlock $ getOptTAS opts = (j :* (j :+ Number 1)) :/ (Number 2)
>            | otherwise                                = (Var "n" :+ Number 1) :* j
>    shifted | elem TASBlockInterleave $ getOptTAS opts = let nTables = length tabs
>                                                         in init :* (Number nTables)
>            | otherwise                                = init

> -- bei Zugriffen keine Änderung:
> indexMap opts IMA ysizes imData n s = s

> -- bei der Makrodefinition umrechnen:
> indexMap opts mode ysizes (ranges, tabs) n (ST (i,j)) 
>           -- Pointer-Modus und listed werden gleichbehandelt:
>           | elem TASPointers (getOptTAS opts) ||
>             ri == False || rj == False         = ST (rangeAlt ri i, 
>                                                      rangeAlt rj ((j :- i) `optSub` l_n)) 
>           -- ab hier: Block
>           | otherwise         = ST (shifted, Number obsoleteTableDim) 
>    where
>      init    | elem TASOffset          $ getOptTAS opts = Offset j
>              | otherwise                                = calculateOffset opts mode ysizes (ranges, tabs) n (ST (i,j)) 
>      shifted | elem TASBlockInterleave $ getOptTAS opts = let nTables = length tabs
>                                                               numbered = zip tabs [0..]
>                                                               thisNumber = head' [ num | (n', num) <- numbered, n == n'] 
>                                                                                  $ "indexMap: unknown NT: " ++ n ++ "\ntabs: " ++ show tabs
>                                                           in (init :+ (i :* (Number nTables))) :+ (Number thisNumber)
>              | otherwise            = init :+ i

>      ST (l_n, _) = getYSize ysizes n 
>      ST (ri, rj) = getTRange ranges (dropOptPrefix n)

>      dropOptPrefix n 
>         | isPrefix (parr prefixes) n     = dropPrefix (parr prefixes) n
>         | isPrefix (pdiffarr prefixes) n = dropPrefix (pdiffarr prefixes) n
>         | otherwise                      = n 


> indexMap opts mode ysizes (ranges,tabs) n (TT (i1,j1) (i2,j2)) = TT (rangeAlt ri1 i1, rangeAlt rj1 ((j1 :- i1) `optSub` l1_n)) 
>                                                                     (rangeAlt ri2 i2, rangeAlt rj2 ((j2 :- i2) `optSub` l2_n)) where
>           TT (l1_n, _) (l2_n, _) = getYSize ysizes n 
>           (ri1, rj1, ri2, rj2)   = getRangeTT ranges n
> -- indexMap opts mode ysizes ranges n x = pattErr "indexMap" x

> indexMap2List opts mode ysizes imData n s = case indexMap opts mode ysizes imData n s of
>                              ST (a,b)       -> [a,b]
>                              TT (a,b) (c,d) -> [a,b,c,d]

> index2List s = case s of
>                  ST (a,b)       -> [a,b]
>                  TT (a,b) (c,d) -> [a,b,c,d]

> list2Index s = case s of
>                  [a,b]     -> ST (a,b)
>                  [a,b,c,d] -> TT (a,b) (c,d)
>                  otherwise -> pattErr "list2Index" s

-------------------------------------

> fetchAlgs v = case toplevelElemsStruct v of
>                            [calcAlg, enumAlg] -> (calcAlg, enumAlg)
>                            otherwise          -> error $ "backtracing currently only works with algebra " ++
>                                                          "combination <calcAlg> *** <enumAlg>"


----------------------------------------

> listAppend v v1 v2 =
>                 [commentLn $ ppV v ++ " = " ++ ppV v1 ++ " ++ " ++ ppV v2] ++ 
>                 [TLIfs (ExpIOp (ExpTLVar (fst v1)) "==" ExpNil)            -- if (v1==NULL) 
>                       (if v==v2 then [] else [TLAssign v (TLVar v2)])     -- return(v2); else
>                       [TLIf (ExpIOp (ExpTLVar (fst v2)) "==" ExpNil)       -- if v2 != NULL then 
>                          [TLAssign v (TLVar v1)]
>                          [TLAssign (listNext (listLast v1)) (TLVar v2),   --   v1->last->next = v2;
>                           TLAssign (listLast v1) (TLVar (listLast v2)),
>                           TLAssign v (TLVar v1)]]]                        --   v1->last = v2->last;


Operations on temp. variables:

Hier fehlt noch der Fall, dass v atomar ist, aber v1 oder v2 eine
Liste. Dies sollte aber durch die Typermittlung ausgeschlossen sein.

> mergeResults opts cnd chc v v1 v2 
>         | getOptBT opts == BTList  && not (any isListStruct [v,v1,v2]) =
>             let (calcAlg_v,  enumAlg_v)  = fetchAlgs v
>                 (calcAlg_v1, enumAlg_v1) = fetchAlgs v1
>                 (calcAlg_v2, enumAlg_v2) = fetchAlgs v2
>             in  [commentLn "collect candidates with optimal score"] ++
>                 [TLIf (compareEqExp calcAlg_v1 calcAlg_v2)
>                    {- then -} ([TLAssign calcAlg_v (TLVar calcAlg_v1)] ++ mergeResults opts cnd chc enumAlg_v enumAlg_v1 enumAlg_v2)
>                    {- else -} (cgChoiceSingleElement v chc v1 v2)]

>         | not $ any isListStruct [v,v1,v2] = cgChoiceSingleElement v chc v1 v2
>         | otherwise                        = listAppend v v1 v2

----------------------------------------

> wrapResults opts chc bounds v tvs value = 
>    let (vout, ntvs) = newtv tvs (makeListStruct (snd v))
>    in  (vout, ntvs, [TLIf bounds
>                           ([TLAlloc MTTemp vout (ExpNum 1) (pointerType (snd vout)),
>                             TLAssign (listItem vout) value] 
>                             ++ listStop vout)
>                      (assignNil chc vout)])  

> wrapResultsGen opts v tvs value = 
>    let (vout, ntvs) = newtv tvs (makeListStruct (snd v))
>    in  (vout, ntvs, [TLAlloc MTTemp vout (ExpNum 1) (pointerType (snd vout)),
>                      TLAssign (listItem vout) value] 
>                       ++ listStop vout)

> ---------------------------------------------------------------------------------------------
> cgCopyList allocMode v@(vn,dt) src =
>       [TLAssign v TLNil,
>        TLAssign h src,
>        TLWhileNN h ([
>          TLAlloc allocMode h2 (ExpNum 1) (pointerType dt),
>          TLAssign (listItem h2c) (TLVar (listItem hc))] ++
>          listStop h2c ++
>          -- append
>          [TLIf (ExpIOp (ExpTLVar (fst v)) "==" ExpNil)        
>                 [TLAssign v (TLVar h2c)]
>                     [TLAssign (listNext (listLast v)) (TLVar h2c),
>                     TLAssign (listLast v) (TLVar h2c)]] ++
>          [TLAssign h (tlvar ((Pointer (fst hc)) :. (Direct "next")))])]
>     where
>       h   = (Direct "hlp", TLVoid)
>       hc  = (Cast dt (fst h), dt)
>       h2  = (Direct "hlp2", TLVoid)
>       h2c = (Cast dt (fst h2), dt)
         

> ---------------------------------------------------------------------------------------------
> nilAtom chc = case expToChoice chc of
>    ChoiceMax          -> ExpNum (-10000000)
>    ChoiceMin          -> ExpNum 10000000
>    ChoiceMin_unstable -> ExpNum 10000000
>    ChoiceSum          -> ExpNum 0
>    ChoiceSum_tuple    -> ExpVar "null_tuple"
>    -- ChoiceId           -> ExpNum 9999999
>    otherwise          -> error $ "nilAtom: undefined pattern: " ++ ppChoice chc

> ---------------------------------------------------------------------------------------------
> -- ifNotNil
> ifNotNil chc       (v, PointerOf (StructOf _ [("item", dt), _, _])) = ExpIOp (ExpTLVar v) "/=" ExpNil
> ifNotNil chc       (v, PointerOf (StructOf _ _))                    = ExpIOp (ExpTLVar v) "/=" ExpNil
> ifNotNil chc       (v, TLInt)                                       = ExpIOp (ExpTLVar v) "/=" (nilAtom chc)
> ifNotNil chc       (v, TLReal)                                      = ExpIOp (ExpTLVar v) "/=" (nilAtom chc)
> ifNotNil chc       (v, (StructOf _ dts))                            = (\(n,dt) -> ifNotNil [(head chc)] (v :. (Direct n), dt)) (head dts)
> ifNotNil chc v  = ExpString $ "/* ERROR: illegal ifNotNil operation: if " ++ ppV v ++ " /= " ++ ppChoice chc ++ " */"
> -- ifNotNil chc v  = pattErr "ifNotNil" (chc,v)


> ---------------------------------------------------------------------------------------------
> -- assignNil
> assignNil chc       v@(_, PointerOf (StructOf _ [("item", dt), _, _])) = [(TLAssign v TLNil)]
> assignNil chc       v@(_, PointerOf (StructOf _ _))                    = [(TLAssign v TLNil)]
> assignNil chc       v@(_, TLInt)                             = [TLAssign v (TLExp $ nilAtom chc)]
> assignNil chc       v@(_, TLReal)                            = [TLAssign v (TLExp $ nilAtom chc)]

> assignNil chc  str@(v, (StructOf _ dts))  
>               -- bei TupelStrukturen wird jeweils die gleiche Auswahlfunktion zugrunde gelegt
>               -- | isTupelStruct str  = concatMap (\(n,dt) -> assignNil chc (v :. (Direct n), dt)) dts 
>               -- anderer Ansatz: fuer Tupelstrukturen nur die erste Choicefunction, danach zuweisung = 0 ("sum")
>               | isTupelStruct str  = concatMap (\((n,dt),chc) -> assignNil [chc] (v :. (Direct n), dt)) 
>                                                                  (zip dts ((head chc):repeat exp_choiceSum))
>               -- bei anderen Strukturen die entsprechende aus der Algebra
>               | otherwise          = concatMap (\((n,dt),chc) -> assignNil [chc] (v :. (Direct n), dt)) (zip dts chc')
>                    where
>                    -- kleiner Hack: chc an dieser Stelle teilweise nur einlementig
>                    chc' | length chc == length dts = chc
>                         | otherwise                = replicate (length dts) (head chc)

> -- Strings werden auf NULL gesetzt:
> assignNil chc v@(_, PointerOf (TLChar)) = [(TLAssign v TLNil)]
> assignNil chc v = [TLComment $ ["ERROR: illegal assignNil operation: " ++ ppV v ++ " = " ++ ppChoice chc]]
> -- assignNil chc v = pattErr "assignNil" (chc,v)

> ---------------------------------------------------------------------------------------------
> -- compareEqExp
> compareEqExp v1@(_, StructOf _ dts1) v2@(_, StructOf _ dts2) = 
>    expMakeAnd (map (\(e1,e2) -> compareEqExp e1 e2) (zip (allElemsStruct v1) (allElemsStruct v2)))
> compareEqExp (v1, (PointerOf TLChar)) (v2, (PointerOf TLChar)) = ExpPOp "!strcmp" [ExpTLVar v1,ExpTLVar v2]
> compareEqExp (v1, _)                  (v2, _)                  = ExpIOp (ExpTLVar v1) "==" (ExpTLVar v2)


> ---------------------------------------------------------------------------------------------
> -- cgChoiceSingleElement
> cgChoiceSingleElement v@(_,dt) chc v1n@(v1,_) v2n@(v2,_) 
>   | elem dt [TLInt, TLReal] =  case expToChoice chc of
>     ChoiceMax -> [assignIF v (ExpIOp (ExpTLVar v1) ">" (ExpTLVar v2)) (tlvar v1) (tlvar v2)]
>     ChoiceMin -> [assignIF v (ExpIOp (ExpTLVar v1) "<" (ExpTLVar v2)) (tlvar v1) (tlvar v2)]
>     ChoiceMin_unstable -> [TLIf (ExpIOp (ExpVar "unstable") "&&" (ExpIOp (ExpTLVar v2) ">=" (ExpNum 0))) 
>                                 [(TLAssign (toVA v2) (TLExp (nilAtom chc)))] [],
>                            assignIF v (ExpIOp (ExpTLVar v1) "<" (ExpTLVar v2)) (tlvar v1) (tlvar v2)]
>     -- alter Ansatz: hier hab es Probleme beim suboptimalen Backtrace
>     -- ChoiceMin_unstable -> [assignIF v (ExpIOp (ExpTLVar v1) "<" (ExpTLVar v2)) (tlvar v1) (tlvar v2),
>     --                        TLIf (ExpIOp (ExpVar "unstable") "&&" (ExpIOp (ExpTLVar (fst v)) ">=" (ExpNum 0))) 
>     --                            [(TLAssign v (TLExp (nilAtom chc)))] []]
>     ChoiceSum       -> [TLAssign v (TLExp (ExpIOp (ExpTLVar v1) "+" (ExpTLVar v2)))]
>     ChoiceSum_tuple -> [TLAssign v (TLExp (ExpPOp "sum_tuple" [ExpTLVar v1, ExpTLVar v2]))]
>     x         -> [commentBox $ ["ERROR: cgChoiceSingleElement: undefined pattern",
>                                 ppV v ++ " = " ++ ppChoice chc ++ "(" ++ ppV v1n ++ ", " ++ ppV v2n ++ ")"]]

> cgChoiceSingleElement v@(_, (StructOf _ dts)) chc  v1 v2 | expToChoice chc == ChoiceSum 
>                                                           = [commentLn $ ppV v ++ " = " ++ ppChoice chc ++ "(" ++ ppV v1 ++ ", " ++ ppV v2 ++ ")"] ++
>                                                             concatMap cgStr (zip [1..] (allElemsStruct v))
>   where
>     cgStr (1, (name, _)) = [TLAssign (toVA $ updStructName v name) (TLExp (ExpIOp (ExpTLVar (updStructName v1 name)) "+" 
>                                                                                   (ExpTLVar (updStructName v2 name))))]
>     cgStr (_, (name, _)) | v == v1   = []
>                          | otherwise = [TLAssign (toVA $ updStructName v name) (TLVar (toVA (updStructName v1 name)))]

> cgChoiceSingleElement v@(_, (StructOf _ dts)) chc  v1 v2 | expToChoice chc == ChoiceSum_tuple 
>                                                           = [commentLn $ ppV v ++ " = " ++ ppChoice chc ++ "(" ++ ppV v1 ++ ", " ++ ppV v2 ++ ")"] ++
>                                                             concatMap cgStr (zip [1..] (allElemsStruct v))
>   where
>     cgStr (1, (name, _)) = [TLAssign (toVA $ updStructName v name) (TLExp (ExpPOp "sum_tuple" [
>                                                                                   (ExpTLVar (updStructName v1 name)),
>                                                                                   (ExpTLVar (updStructName v2 name))]))]
>     cgStr (_, (name, _)) | v == v1   = []
>                          | otherwise = [TLAssign (toVA $ updStructName v name) (TLVar (toVA (updStructName v1 name)))]


> cgChoiceSingleElement v@(_, (StructOf _ dts)) chc  v1 v2  = [commentLn $ ppV v ++ " = " ++ ppChoice chc ++ "(" ++ ppV v1 ++ ", " ++ ppV v2 ++ ")"] ++
>                                                             -- TODO: dies wird mit Tupel-Strukturen nicht mehr funktionieren:
>                                                             cgStr (filter canOpt (zip (allElemsStruct v) chc))
>   where
>     canOpt ((_,TLInt),_)     = True
>     canOpt ((_,TLReal),_)    = True
>     canOpt _                 = False

>     cgStr [((name,_),chc)]     = case expToChoice [chc] of 
>                                     ChoiceMin_unstable -> 
>                                              [TLIf (ExpIOp (ExpVar "unstable") "&&" (ExpIOp (ExpTLVar (updStructName v2 name)) ">=" (ExpNum 0))) 
>                                                    [(TLAssign (updStructNameDT v2 name) (TLExp (nilAtom [chc])))] [],
>                                               assignIF v (ExpIOp (ExpTLVar (updStructName v1 name)) (op chc) 
>                                                                  (ExpTLVar (updStructName v2 name))) (TLVar v1) (TLVar v2)]
>                                     -- alter Ansatz: hier hab es Probleme beim suboptimalen Backtrace
>                                     -- ChoiceMin_unstable -> 
>                                     --              [assignIF v (ExpIOp (ExpTLVar (updStructName v1 name)) (op chc) 
>                                     --                                  (ExpTLVar (updStructName v2 name))) (TLVar v1) (TLVar v2),
>                                     --               TLIf (ExpIOp (ExpVar "unstable") "&&" 
>                                     --                       (ExpIOp (ExpTLVar (updStructName v name)) ">=" (ExpNum 0))) 
>                                     --                       [(TLAssign (updStructNameDT v name) (TLExp (nilAtom [chc])))][]]
>                                     otherwise -> [assignIF v (ExpIOp (ExpTLVar (updStructName v1 name)) (op chc) 
>                                                                      (ExpTLVar (updStructName v2 name))) (TLVar v1) (TLVar v2)]
>     cgStr (((name,_),chc): ss) = [TLIfs (compareEqExp (updStructNameDT v1 name) (updStructNameDT v2 name))
>                            -- then
>                            (cgStr ss) 
>                            -- else
>                            [assignIF v (ExpIOp (ExpTLVar (updStructName v1 name)) (op chc)  
>                                                (ExpTLVar (updStructName v2 name))) (TLVar v1) (TLVar v2)]]

>     cgStr x                    = pattErr "cgStr" x

>     op chc = case expToChoice [chc] of
>                ChoiceMax          -> ">"
>                ChoiceMin          -> "<"
>                ChoiceMin_unstable -> "<"
>                otherwise -> error $ "choice function " ++ ppChoice [chc] ++ " not supported on structures"

> -- hier erstmal Kommentar anstelle von Fehlermeldung
> cgChoiceSingleElement v chc v1 v2 = [commentBox $ ["ERROR: cgChoiceSingleElement: undefined pattern:",
>                                                    ppv v ++ " = " ++ ppChoice chc ++ "(" ++ ppv v1 ++ ", " ++ ppv v2 ++ ")"]]
>   where ppv (v, dt) = "(" ++ ppV (v,dt) ++ ", " ++ pretty dt ++ ")"

> -- cgChoiceSingleElement v chc v1 v2 = error $ "cgChoiceSingleElement: undefined pattern:\n" ++
> --                                            ppv v ++ " = " ++ ppChoice chc ++ "(\n   " ++ ppv v1 ++ ",\n   " ++ ppv v2 ++ ")"
> --  where ppv (v, dt) = "(" ++ ppV (v,dt) ++ ", " ++ pretty dt ++ ")"

> -- fuer Effizienzvergleiche:
> assIF = "std"
> assignIF v exp v1 v2 | assIF == "std" = TLAssignIF v exp v1 v2
>                      | otherwise      = TLIfs exp [TLAssign v v1] [TLAssign v v2]


> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig (term@(IL2Exp e))= (head tvs, tvs, lvs, [TLComment ["code for il2exp"]])
> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig (term@(IL2Terminal bnd ift dt exp))
>     = (vout, ntvs, lvs, [TLIf cond (assign vout exp) (assignNil chc vout)])
>     where
>       bounds = cgBounds bnd
>       cond | ift == [] = bounds
>            | otherwise = expMakeAnd [bounds,(head ift)]

>       (vout, ntvs) = newtv tvs (makeListStruct dt)

>       assign  v@(_, dt) exp = [TLAlloc MTTemp v (ExpNum 1) (pointerType dt),   -- Allokiere Speicher
>                                TLAssign (listItem v) (TLExp exp)]          -- Weise Wert aus der Tabelle zu
>                                 ++ listStop v                              -- setze Listenende auf nil


> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig (IL2Nonterminal f s)
>     -- spezialfall fuer Backtrace
>     -- Bedingung: single candidate backtrace, innerhalb von foreach und rekursive Funktion
>     -- Dieses muss speziell behandelt werden, tabellenzugriffe werden schon direkt in die rhss geinlined:
>     | isBTSingle opts &&
>       infe && elem f recur      = let (calcAlg, enumAlg) = fetchAlgs vout 
>                                       indizes            = case s of
>                                                              (ST (i,j))          -> [TLExp (ExpME i),  TLExp (ExpME j)]
>                                                              (TT (i1,j1)(i2,j2)) -> [TLExp (ExpME i1), TLExp (ExpME j1),
>                                                                                      TLExp (ExpME i2), TLExp (ExpME j2)]
>                                   in (vout, ntvs, lvs, comment ++ [commentBox ["in for each = true"]] ++
>                                                           [TLAssign calcAlg (TLFA (pfct prefixes ++ f) (map meToTL (index2List s))),
>                                                            TLAssign enumAlg (TLFA (pnew prefixes ++ pNTID prefixes) 
>                                                                                 $ [tlvar (Direct (pback prefixes ++ f))] ++ indizes)])

>     -- Backtrace:  atomares Ergebnis,  atomarer Kontext
>     | isBTSingle opts            = let comment2 = [] -- hier vielleicht noch Kommentar ergaenzen
>                                        (calcAlg, enumAlg) = fetchAlgs vout
>                                        indizes            = case s of
>                                                               (ST (i,j))          -> [TLExp (ExpME i),  TLExp (ExpME j)]
>                                                               (TT (i1,j1)(i2,j2)) -> [TLExp (ExpME i1), TLExp (ExpME j1),
>                                                                                       TLExp (ExpME i2), TLExp (ExpME j2)]
>                                        resultEntry | elem f recur = (TLFA  (pfct prefixes ++ f) (map meToTL (index2List s)))
>                                                    | otherwise    = (tlNonterm (ptbl prefixes ++ f) (indexMap opts IMA ysizes imData f s))

>                                    in  (vout, ntvs, lvs, comment ++ comment2 ++ 
>                                                     [TLIf bounds 
>                                                           [TLAssign calcAlg resultEntry,
>                                                            TLAssign enumAlg (TLFA (pnew prefixes ++ pNTID prefixes) 
>                                                                                 $ [tlvar (Direct (pback prefixes ++ f))] ++ indizes)]
>                                                            -- else 
>                                                           (assignNil chc vout)])


>     -- Backtrace: atomares Ergebnis,  Listenkontext
>     | elem (getOptBT opts) ([BTCompleteList] ++ btSubOpts)
>                                 = let comment2 = [commentBox ["Nonterminal " ++ f ++ " is implemented as a " ++ kindOfFct,
>                                                               "function which yields atomar results. Since we are in list context,",
>                                                               "we need to wrap the result of " ++ f ++ " into a single list element."]]
>                                       kindOfFct = if elem f recur then "recursive" else "tabulated"
>                                       (voutR, ntvsR) = newtv tvs $ snd vout -- (makeListStruct (snd vout))
>                                       (calcAlg, enumAlg) = fetchAlgs (listItem vout) 
>                                       indizes            = case s of
>                                                              (ST (i,j))          -> [TLExp (ExpME i),  TLExp (ExpME j)]
>                                                              (TT (i1,j1)(i2,j2)) -> [TLExp (ExpME i1), TLExp (ExpME j1),
>                                                                                      TLExp (ExpME i2), TLExp (ExpME j2)]
>                                       resultEntry | elem f recur = (TLFA  (pfct prefixes ++ f) (map meToTL (index2List s)))
>                                                   | otherwise    = (tlNonterm (ptbl prefixes ++ f) (indexMap opts IMA ysizes imData f s))

>                                   in  (voutR, ntvsR, lvs, comment ++ comment2 ++ 
>                                                     [TLIf bounds 
>                                                           ([TLAlloc MTTemp voutR (ExpNum 1) (pointerType (snd voutR)),
>                                                             TLAssign calcAlg resultEntry,
>                                                             TLAssign enumAlg (TLFA (pnew prefixes ++ pNTID prefixes) 
>                                                                                  $ [tlvar (Direct (pback prefixes ++ f))] ++ indizes)]
>                                                             ++ listStop voutR)
>                                                            -- else 
>                                                           (assignNil chc voutR)])


>     -- innerhalb von for each, rekursiver Funktionsaufruf
>     -- Ergebnistyp und Kontext an dieser Stelle egal:
>     | infe && elem f recur      = (vout, ntvs, lvs, comment ++ [commentBox ["in for each = true"]] ++
>                                                                [TLAssign vout (TLFA (pfct prefixes ++ f) (map meToTL (index2List s)))])
>     -- Listenstruktur innerhalb von for each, Tabellenzugriff:
>     -- atomare Ergebnistypen muessen an dieser Stelle nicht beruecksichtigt werden, da
>     -- diese schon in die rhss geinlined wurden.
>     | infe && isListStruct vout = (vout, ntvs, lvs, comment ++ [commentBox ["in for each = true"]] ++
>                                                                [TLAssign vout (tlNonterm f' (indexMap opts IMA ysizes imData f s))])

>     -- ab hier: einzeln auftretendes Nichtterminal
>     ------------------------------------------------------

      Insgesamt sind hier 8 Fälle abzuhandeln:

          atomares Ergebnis        |  atomarer Kontext   |  rekursiver Aufruf
     --------------------------------------------------------------------------
      1.                           |         X           |         X 
      2.                           |         X           |          
      3.                           |                     |         X 
      4.                           |                     |           
      5.           X               |         X           |         X 
      6.           X               |         X           |           
      7.           X               |                     |         X 
      8.           X               |                     |           

>     -- Fall 1: rekursiver Aufruf, Listenstruktur, aber atomarer Kontext
>     | isListStruct vout && isAtomar opts chc && elem f recur 
>                                 = let (voutR, ntvsR)   = newtv ntvs  (getListItemDataType (snd vout))
>                                       (voutIt, ntvsIt) = newtv ntvsR (snd vout)
>                                       comment2 = [commentBox ["This is a very special case: Nonterminal " ++ f ++ " is implemented as a ",
>                                                               "recursive function and yields a list of results. Since we are in atomar context",
>                                                               "here, we can optimize the results of "++ f ++ " in place.",
>                                                               ppV vout ++ " holds the result list of " ++ f ++ ", we use " ++ ppV voutIt ++ " to ",
>                                                               "iterate over " ++ ppV vout ++ " and store the optimal result in " ++ ppV voutR ++ ".",
>                                                               "Afterwards, " ++ ppV vout ++ " can be removed."]]
>                                   in (voutR, ntvsIt, lvs, comment ++ comment2 ++
>                                                     [TLAssign vout (TLFA (pfct prefixes ++ f) (map meToTL (index2List s)))] ++
>                                                     assignNil chc voutR ++
>                                                     [TLAssign voutIt (TLVar vout)] ++
>                                                     [TLWhileNN voutIt 
>                                                        (cgChoiceSingleElement voutR chc voutR (listItem voutIt) ++
>                                                        [listStep voutIt])] ++
>                                                      freemem opts vout)
>     -- Fall 4: Tabellenzugriff, Listenstruktur, aber atomarer Kontext
>     | isListStruct vout && isAtomar opts chc 
>                                 = let (voutR, ntvsR)   = newtv ntvs  (getListItemDataType (snd vout))
>                                       comment2 = [commentBox ["This is a very special case: Nonterminal " ++ f ++ " is implemented as a ",
>                                                               "tabulated function and yields a list of results. Since we are in atomar context",
>                                                               "here, we can optimize the results of "++ f ++ " in place.",
>                                                               "We use " ++ ppV vout ++ " to iterate over the result list and",
>                                                               "store the optimal result in " ++ ppV voutR ++ "."]]
>                                   in (voutR, ntvsR, lvs, comment ++ comment2 ++
>                                                     [TLIf bounds 
>                                                           [TLAssign vout (tlNonterm f' (indexMap opts IMA ysizes imData f s))] 
>                                                           (assignNil chc vout)] ++
>                                                     assignNil chc voutR ++
>                                                     [TLWhileNN vout 
>                                                        (cgChoiceSingleElement voutR chc voutR (listItem vout) ++
>                                                        [listStep vout])])

>     -- Fall 2:Tabellenzugriff, Listenstruktur und Listenkontext: Hier muss die Tabellenliste kopiert werden:
>     | isListStruct vout && not (elem f recur)  
>                                 = let comment2 = [commentBox ["We have to copy the result list of nonterminal " ++ f ++ "."]]
>                                   in (vout, ntvs, lvs, 
>                                       comment ++ comment2 ++ 
>                                       [TLIf bounds 
>                                         (cgCopyList MTDynamic vout (tlNonterm f' (indexMap opts IMA ysizes imData f s))) (assignNil chc vout)])

>     -- Fall 7: rekursiver Aufruf, atomares Ergebnis, aber Listenkontext
>     | not (isListStruct vout) && not (isAtomar opts chc) && elem f recur 
>                                 = let comment2 = [commentBox ["Nonterminal " ++ f ++ " is implemented as a recursive",
>                                                               "function which yields atomar results. Since we are in list context,",
>                                                               "we need to wrap the result of " ++ f ++ " into a single list element."]]
>                                       (voutR, ntvsR, wCode) = wrapResults opts chc bounds vout ntvs (TLFA (pfct prefixes ++ f) (map meToTL (index2List s)))
>                                   in  (voutR, ntvsR, lvs, comment ++ comment2 ++ wCode)

      Für rekursive Funktionsaufrufe gibt es an dieser Stelle noch zwei Fälle:
      1. Funktion liefert Listenstruktur, im Listenkontext
         -> hier kann der Zeiger direkt zugewiesen werden, der Speicher muss im späteren Verlauf wieder freigegen werden
      2. Funktion liefert atomaren Wert,  im atomaren Kontext     
         -> der einfachste Fall: einfach den Wert zuweisen

      In beiden Faellen wird _keine_ If-then-else assignnil Abfrage benoetigt, da dies schon von der rekursiven Fkt. erledigt wird.

>     | elem f recur   = (vout, ntvs, lvs, comment ++ [TLAssign vout (TLFA (pfct prefixes ++ f) (map meToTL (index2List s)))])

>     -- Fall 8: Tabellenzugriff, atomares Ergebnis, aber Listenkontext
>     | not (isListStruct vout) && not (isAtomar opts chc) 
>                                 = let comment2 = [commentBox ["Nonterminal " ++ f ++ " is implemented as a tabulated",
>                                                               "function which yields atomar results. Since we are in list context,",
>                                                               "we need to wrap the result of " ++ f ++ " into a single list element."]]
>                                       (voutR, ntvsR, wCode) = wrapResults opts chc bounds vout ntvs (tlNonterm f' (indexMap opts IMA ysizes imData f s))
>                                   in  (voutR, ntvsR, lvs, comment ++ comment2 ++ wCode)

>     -- Fall 6: Tabellenzugriff, atomares Ergebnis, atomarer Kontext
>     | otherwise           = (vout, ntvs, lvs, comment ++ -- [commentBox ["Nonterminal " ++ f ++ " is implemented as a tabulated",
>                                                     --              "function which yields atomar results."]] ++ 
>                                                     [TLIf bounds 
>                                                           [TLAssign vout (tlNonterm f' (indexMap opts IMA ysizes imData f s))] 
>                                                      (assignNil chc vout)])
>     where
>       comment = [commentLn $ ppV vout ++ " = " ++ "p " ++ f]
>       f'     = ptbl prefixes ++ f
>       bounds = cgBounds (s, ysize)
>       ysize  = getYSize ysizes f

>       (vout, ntvs) = newtv tvs (il2Type opts False il2dt algs (IL2Nonterminal f s))


> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig (IL2Alt [p])  
>     = cgUnit pName ysizes imData recur opts chc False cnd tvs lvs il2dt algs sig p
> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig (IL2Alt (a:b))      
>     = (vout, ntvs, lvs'', a' ++ b' ++ mcode)
>   where
>     (v',   tvs',  lvs',  a')  = cgUnit pName ysizes imData recur opts chc False cnd tvs  lvs  il2dt algs sig a
>     (v'',  tvs'', lvs'', b')  = cgUnit pName ysizes imData recur opts chc False cnd tvs' lvs' il2dt algs sig (IL2Alt b)
>     (v''', tvs''')      = newtv tvs'' (il2Type opts (isAtomar opts chc) il2dt algs (IL2Alt (a:b)))
>     (vout, ntvs, mcode) 
>       | mapsToEq isListStruct [v''',v'',v'] = (v''', tvs''',mergeResults opts cnd chc v''' v' v'')
>       -- makeSingleList: in Faellen, in denen ein atomares Element in eine Liste integriert werden muss, muss 
>       -- erst eine einelementige Liste erzeugt werden
>       | otherwise = 
>         let 
>            (nv',  ntvs',  allocCode_v')  = makeSingleList tvs'''  v'  a
>            (nv'', ntvs'', allocCode_v'') = makeSingleList ntvs'   v'' (IL2Alt b)
>            makeSingleList tvs v il2 | isListStruct v = (v, tvs,  [])
>                                     | otherwise = let il2chc = fst3 $ deriveIL2Type opts (isAtomar opts chc) algs il2dt il2
>                                                       comment2 = [commentBox ["The result in " ++ ppV v ++ " is atomar. Since we are in list context,",
>                                                                     "we need to wrap " ++ ppV v ++ " into a single list element (" ++ ppV v' ++ ")."]]
>                                                       (v', tvs', wCode) = wrapResults opts chc (ifNotNil il2chc v) v tvs (TLVar v)
>                                      in (v', tvs', comment2 ++ wCode)
>         in (v''', ntvs'', allocCode_v' ++ allocCode_v'' ++ mergeResults opts cnd chc v''' nv' nv'')

--------------------------------------------------------------------------------------------------------------------
Auswahl
----------------------------------------------------------------------

> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig (IL2Choice f a) 
>    -- als erstes den ***-Fall:
>    | getOptCG opts == CGp             = let (vout, ntvs, chcCode)  = cgChoicePairing opts f v' tvs'
>                                         in  (vout, ntvs, lvs',  a' ++ chcCode ++ freemem opts v') 

>    | not (isListStruct v') && expToChoice (map fth4 f) == ChoiceMin_unstable 
>                                       = let  code =  [TLIf (ExpIOp (ExpVar "unstable") "&&" 
>                                                            (ExpIOp (ExpTLVar $ fst (getRelevantElem v')) ">=" (ExpNum 0))) 
>                                                            [(TLAssign (getRelevantElem v') (TLExp (nilAtom (map fth4 f))))] []]
>                                         in (v', tvs', lvs', a' ++ code)

>    | not $ isListStruct v' = body    -- wenn Ergebnistyp atomar, wurde schon vorher ausgewaehlt; dann passiert hier nichts
>    | expToChoice (map fth4 f)  == ChoiceId       = body    -- bei h=id ebenfalls

>    -- wenn im Backtrace-Modus, koennen als Ergebnis der Auswahl auch mehrere Elemente entstehen:
>    | getOptBT opts == BTCompleteList  = let (vout, ntvs, chcCode)  = cgChoiceBTCL opts (map fth4 f) v' tvs'
>                                         in  (vout, ntvs, lvs', a' ++ chcCode ++ freemem opts v') 
>    | elem (getOptBT opts) [BTSubOpt, BTSubOptCut]
>                                       = let (vout, ntvs, chcCode)  = cgChoiceBTSO opts (map fth4 f) v' tvs'
>                                         in  (vout, ntvs, lvs', a' ++ chcCode ++ freemem opts v') 
>    | getOptBT opts == BTPF            = let (vout, ntvs, chcCode)  = cgChoiceBTPF opts (map fth4 f) v' tvs'
>                                         in  (vout, ntvs, lvs', a' ++ chcCode ++ freemem opts v') 
>    | otherwise                        = let (vout, ntvs,chcCode)   = cgChoice  opts f v' tvs'
>                                         in  (vout, ntvs, lvs', a' ++ chcCode  ++ freemem opts v') 
>      where
>        body@(v', tvs', lvs', a')       = cgUnit pName ysizes imData recur opts (map fth4 f) False cnd tvs lvs il2dt algs sig a 

>        -- *** - Modus
>        ---------------------------------------------------------------------
>        cgChoicePairing opts chc v@(vn,dt) tvs = (v_target2, tvs5, 
>            comment ++ 
>            [commentLn $ "build list for first structure elements (" ++ ppV v_target ++ ")"] ++
>            code_strip ++ 
>            [commentLn $ "optimize first list (" ++ ppV v_chosen ++ ")"] ++
>            code_choice ++ 
>            [commentLn $ "maybe nub it... (" ++ ppV v_nubbed ++ ")"] ++
>            code_nub ++ 
>            [commentLn $ "build result (" ++ ppV v_target2 ++ ")"] ++
>            code_strip2)
>          where
>            comment = [commentLn $ "***"] 
>            (v_it, tvs') = newtv tvs dt
>            structVars = toplevelElemsStruct (listItem v_it)

>            ([v_target], tvs'', code_strip) = strip tvs' [] [head structVars] 
>            (v_chosen, tvs''', code_choice) = cgChoice opts [head chc] v_target tvs''
>            (v_nubbed, tvs'''', code_nub)  -- im Falle atomarer Auswahl ist kein nub notwendig 
>                                           | elem (expToChoice [fth4 $ head chc]) 
>                                                   [ChoiceMin, ChoiceMin_unstable, ChoiceMax, ChoiceSum, ChoiceSum_tuple] = (v_chosen, tvs''', [])
>                                           -- ansonsten schon:
>                                           | otherwise = 
>                                               let dt = snd (listItem v_chosen)
>                                                   nub_name  = tlvar $ Direct $ specialChoiceName "nub"  dt
>                                                   sort_name = tlvar $ Direct $ specialChoiceName "sort" dt
>                                               in (v_chosen, tvs''', [TLAssign v_chosen (TLFA "nub" [TLFA "sort" [TLVar v_chosen, sort_name], nub_name])])
>            (v_target2, tvs5, code_strip2) = let (v_globCollect, tvs9) = newtv tvs'''' (snd v)
>                                                 (v_itt, tvs', code)   = forEach v_nubbed tvs9
>                                                          (strip_code ++ [commentLn $ "optimize second list"] ++ code_choice ++ codeFe2)
>                                                 ([v_stripped], tvs'', strip_code) = strip tvs' cond [head $ tail structVars]
>                                                 (v_chosen, tvs''', code_choice) = cgChoice opts [head $ tail chc] v_stripped tvs'' 
>                                                 cond = [compareEqExp (listItem v_itt) (head structVars)]
>                                                 (v_it3, v_it2, tvs99, codeFe2) = let (vtmp, tvs10) = newtv tvs''' (snd v)
>                                                                                      (v_it3, tvs11, code) = 
>                                                                                        (forEach v_chosen tvs10 
>                                                                                           ([TLAlloc MTDynamic v_it (ExpNum 1) (pointerType (snd v_it)),
>                                                                                             TLAssign (head structVars)        (TLVar (listItem v_itt)),
>                                                                                             TLAssign (head (tail structVars)) (TLVar (listItem v_it3))]
>                                                                                              ++ listStop v_it
>                                                                                              ++ listAppend vtmp v_it vtmp)) 
>                                                                            in (v_it3, vtmp, tvs11, code )
>                                                 comment = [commentBox [ppV v_nubbed   ++ " holds the results for first algebra",
>                                                                        ppV v_itt      ++ " iterates over " ++ ppV v_nubbed,
>                                                                        ppV v_stripped ++ " holds the corresponding results for second algebra",
>                                                                        ppV v_chosen   ++ " holds optimized version of " ++ ppV v_stripped,
>                                                                        ppV v_it3      ++ " iterates over " ++ ppV v_chosen,
>                                                                        ppV v_it2      ++ " collects the results" ]]
>                                               in (v_it2, tvs99, comment ++ [TLAssign v_it2 TLNil] ++ code)
>            

-- >               (v_coll, tvs'')         = newtv tvs' (makeListStruct (snd v2))
-- >               code_v1' = code_v1 ++ [TLAssign v1 (TLFA "nub" [TLFA "sort" [TLVar v1]])]


>            strip tvs cond [] = ([], tvs, [])
>            strip tvs cond (v_content:vars) = (v_target:v_targets, tvs''', code ++ code_vars)
>               where
>                 (v_target, tvs') = newtv tvs (makeListStruct $ snd v_content)
>                 (tvs'',code)     = stripList tvs' cond v v_it v_target v_content
>                 (v_targets, tvs''', code_vars)  = strip tvs'' cond vars

>            stripList tvs cond v_start v_it v_target v_content = (tvs', code)
>               where
>                 (vtmp, tvs', wrapCode) = wrapResultsGen opts (listItem v_target) tvs (TLVar v_content)
>                 code = [TLAssign v_target TLNil,
>                         TLAssign v_it (TLVar v_start),
>                         TLWhileNN v_it
>                            (ifCond 
>                               (wrapCode ++ listAppend v_target vtmp v_target) ++
>                            [listStep v_it])]
>                 ifCond code | length cond == 0 = code
>                             | otherwise        = [TLIf (head cond) code []]

>            forEach v tvs code = (v', tvs', code')
>                 where
>                   (v', tvs') = newtv tvs (snd v)
>                   code' = [TLAssign v' (TLVar v)] ++
>                           [TLWhileNN v'
>                             (code ++
>                              [listStep v'])]


>        -- Auswahl des besten Elements im CGl-Modus
>        ----------------------------------------------------------------------
>        cgChoice opts chc v@(vn,dt) tvs = case (expToChoice chcExps) of
>                                            ChoiceId         -> (v,tvs,[commentLn "h = id: no operation necessary"])
>                                            ChoiceExp exp    -> cgChoiceExp opts (head chc) v tvs
>                                            ChoiceExternal f -> let (vret, tvs') = newtv tvs dt
>                                                                in  (vret, tvs', comment vret ++ [TLAssign vret (TLFA f [TLVar v])])
>                                            otherwise        -> (vret, tvs'', comment vret ++ code)
>          where
>            chcExps = map fth4 chc
>            (vtmp, tvs')  = newtv tvs  dt
>            (vret, tvs'') = newtv tvs' dt

>            comment vret = [commentLn $ ppV vret ++ " = " ++ ppChoice chcExps ++ "(" ++ ppV v ++ ")"] 

>            code        = [TLIf (ExpIOp (ExpTLVar vn) "/=" ExpNil)
>                               ([TLAlloc MTTemp vret (ExpNum 1) (pointerType dt),
>                                 TLAssign vtmp (TLVar v),
>                                 TLAssign (listItem vret) (TLVar (listItem vtmp))] ++
>                                 listStop vret ++
>                                 [listStep vtmp,
>                                 TLWhileNN vtmp 
>                                   (cgChoiceSingleElement (listItem vret) chcExps (listItem vret) (listItem vtmp) ++ 
>                                   [listStep vtmp])
>                                 ])
>                                 -- else
>                                 [TLAssign vret TLNil]]

>            cgChoiceExp opts (name, _, [SigId arg], exp) v tvs = (vret, tvs', comment ++ [TLAssign vret (TLExp exp')])
>               where
>                (vret, tvs') = newtv tvs (snd v)
>                comment = [commentLn $ ppV vret ++ " = " ++ pretty exp']
>                exp' = insertVarBinds [(arg, ExpTLVar $ fst v)] exp
>            cgChoiceExp _ x _ _ = pattErr "cgChoiceExp" x


>        -- Auswahl im SubOpt-Modus
>        ----------------------------------------------------------------------
>        cgChoiceBTSO opts chc v@(vn,dt) tvs = (vEnumList, tvs'''', comment ++ code)
>          where
>            (calcAlg, enumAlg) = fetchAlgs (listItem v)
>            (vtmp,  tvs')        = newtv tvs   dt
>            (vOpt,  tvs'')       = newtv tvs'  (snd calcAlg)

>            (vAlloc,    tvs''')  = newtv tvs'' dt
>            (vEnumList, tvs'''') = newtv tvs''' dt -- (snd enumAlg)

>            comment = [commentLn $ ppV vOpt ++ " = " ++ ppChoice chc ++ "(" ++ ppV v ++ ")"]

>            diffExp = (ExpPOp soAbsFct [ExpIOp (ExpTLVar (fst $ addTupel vOpt)) "-" (ExpTLVar $ fst $ addTupel (fst (fetchAlgs (listItem vtmp))))])
>            -- Falls hier eine Tupel-Struktur vorliegt, vergleichen wir nur das erste Tupel-Element:
>            addTupel v | isTupelStruct v = structElem v "tup1"
>                       | otherwise       = v
>            vEnum   = (snd (fetchAlgs (listItem vtmp)))
>            code = -- Optimum suchen
>                   (assignNil chc vOpt) ++
>                   [TLAssign vtmp (TLVar v),
>                    TLWhileNN vtmp
>                      (cgChoiceSingleElement vOpt chc vOpt (fst (fetchAlgs (listItem vtmp))) ++
>                      [listStep vtmp])
>                   ] ++
>                   -- Liste der suboptimalen Kandidaten aufbauen
>                   [TLAssign vtmp (TLVar v)] ++
>                   (assignNil chc vEnumList) ++
>                   [TLWhileNN vtmp
>                      ([TLIf (ExpIOp diffExp "<=" (ExpTLVar (Direct "diff")))
>                        ([TLFA (pupdate prefixes ++ pstr prefixes ++ fst sig)
>                                    [TLVar (listItem vEnum), TLExp (ExpIOp (ExpTLVar (Direct "diff")) "-" diffExp)]] ++
>                         [TLAlloc MTTemp vAlloc (ExpNum 1) (pointerType (snd vAlloc)),
>                          TLAssign (listItem vAlloc) (TLVar (listItem vtmp))] ++
>                         listStop vAlloc ++ 
>                         listAppend vEnumList vAlloc vEnumList)
>                        -- else 
>                        []] ++
>                        [listStep vtmp])
>                   ]

>        -- Auswahl im PF-Modus
>        ----------------------------------------------------------------------
>        cgChoiceBTPF opts chc v@(vn,dt) tvs = 
>          case expToChoice chc of
>            -- external choice function
>            (ChoiceExternal fct) -> (vItr, tvs', comment ++ [TLAssign vItr (TLFA fct [TLVar v])])
>            -- summation
>            otherwise -> (vItr, tvs'''', comment ++ code)
>          where
>            (calcAlg, enumAlg) = fetchAlgs (listItem v)
>            (vItr,  tvs')        = newtv tvs    dt            -- iterator
>            (vOpt,  tvs'')       = newtv tvs'   (snd calcAlg) -- complete sum
>            (vRsum, tvs''')      = newtv tvs''  TLInt         -- running sum
>            (vRnd,  tvs'''')     = newtv tvs''' TLInt         -- random value

>            comment = [commentLn $ ppV vOpt ++ " = " ++ ppChoice chc ++ "(" ++ ppV v ++ ")"]

>            addTupel v | isTupelStruct v = structElem v "tup1"
>                       | otherwise       = v

>            code = -- Optimum suchen
>                   (assignNil chc vOpt) ++
>                   [TLAssign vItr (TLVar v),
>                    TLWhileNN vItr
>                      (cgChoiceSingleElement vOpt chc vOpt (fst (fetchAlgs (listItem vItr))) ++
>                      [listStep vItr])
>                   ] ++
>                   -- Zufallsauswahl
>                   [TLAssign vRsum (tlnumber 0)] ++
>                   [TLAssign vRnd (TLFA "drandom" [TLVar $ getRelevantElem vOpt])] ++ 
>                   [TLAssign vItr (TLVar v)] ++
>                   [TLWhileNN vItr
>                      (sumCode ++
>                       [TLIf (ExpIOp (ExpTLVar (fst vRsum)) ">=" (ExpTLVar (fst vRnd)))
>                             -- then
>                             ((listStop vItr) ++ [TLBreak])
>                             -- else
>                             [listStep vItr]]
>                      )]

>            sumCode = case expToChoice chc of
>                 ChoiceSum       -> [TLAssign vRsum (TLExp $ ExpIOp (ExpTLVar (fst vRsum)) "+" (ExpTLVar $ fst $ addTupel (fst (fetchAlgs (listItem vItr)))))]
>                 ChoiceSum_tuple -> [TLAssign vRsum (TLExp $ ExpPOp "sum_tuple" [(ExpTLVar (fst vRsum)),
>                                                                                 (ExpTLVar $ fst $ addTupel (fst (fetchAlgs (listItem vItr))))])]
>                 otherwise       -> error $ "choice function " ++ ppChoice chc ++ " not supported in pf-mode"
        
>        -- fuer ***/Backtrace-Modus; hier koennen als Ergebnis der Auswahl auch mehrere Elemente entstehen:
>        ----------------------------------------------------------------------
>        cgChoiceBTCL opts chc v@(vn,dt) tvs = (vret, tvs''', comment ++ code)
>          where
>            (vtmp, tvs')   = newtv tvs  dt
>            (vret, tvs'')  = newtv tvs' dt
>            (vhlp, tvs''') = newtv tvs'' dt

>            (op,op2) = case expToChoice chc of
>                          ChoiceMax          -> (">","<")
>                          ChoiceMin          -> ("<",">")
>                          ChoiceMin_unstable -> ("<",">")
>                          ChoiceSum          -> ("+","+")
>                          ChoiceSum_tuple    -> ("+","+")
>                          otherwise          -> error $ "choice function " ++ ppChoice chc ++ " not supported on structures"

>            comment = [commentBox ["This part implements the choice function for combination of",
>                                   "algebras " ++ sepList " *** " (map fst algs) ++ ":",
>                                   ppV vret ++ " = " ++ ppChoice chc ++ "(" ++ ppV v ++ ")"]]

>            code        = [TLIf (ExpIOp (ExpTLVar vn) "/=" ExpNil)
>                               ([TLAlloc MTTemp vret (ExpNum 1) (pointerType dt),
>                                 TLAssign vtmp (TLVar v),
>                                 TLAssign (listItem vret) (TLVar (listItem vtmp))] ++
>                                 listStop vret ++
>                                 [listStep vtmp,
>                                 TLWhileNN vtmp 
>                                   (cgChoiceSingle vret chc (listItem vret) (listItem vtmp) ++ 
>                                   [listStep vtmp])
>                                 ])
>                                 -- else
>                                 [TLAssign vret TLNil]]

>            -- Auswahl eines einzelnen Elements
>            ----------------------------------------------------------------------
>            codeSingle v v1cmp v1 v2cmp v2 = [TLIf (ExpIOp (ExpTLVar $ fst v1cmp) "==" (ExpTLVar $ fst v2cmp))
>                                                    -- falls das Ergebnis gleich dem aktuellen Optimum ist, wird die Optimumsliste um dieses Element erweitert.
>                                                    -- fuer Enum kann man das so machen, im allgemeinen Fall muessten an dieser Stelle aber die jeweils
>                                                    -- zweiten Elemente kombiniert werden:
>                                                    ([TLAlloc MTTemp vhlp (ExpNum 1) (pointerType (snd vhlp)),
>                                                      TLAssign (listItem vhlp) (TLVar v2),
>                                                      TLAssign (listNext vhlp) (TLVar v), 
>                                                      TLAssign (listLast vhlp) (TLVar (listLast v)),
>                                                      TLAssign v (TLVar vhlp)])
>                                                    ([TLIf (ExpIOp (ExpTLVar $ fst v1cmp) op2 (ExpTLVar $ fst v2cmp)) 
>                                                           (freememStructList opts sig v ++
>                                                            [TLAlloc MTTemp v (ExpNum 1) (pointerType (snd v)),
>                                                             TLAssign (listItem v) (TLVar v2)] ++
>                                                             listStop v)
>                                                            (freeStructCode opts sig (listPointer (fst v2cmp)))])]

>            cgChoiceSingle :: VarAccess -> [Exp] -> VarAccess -> VarAccess -> [TL]
>            cgChoiceSingle  v chc v1@(_,TLInt) v2  = codeSingle v v1 v1 v2 v2
>            cgChoiceSingle  v chc v1@(_,TLReal) v2  = codeSingle v v1 v1 v2 v2

>            cgChoiceSingle v chc v1@(_, (StructOf _ dts)) v2  = [commentLn $ ppV v ++ " = " ++ ppChoice chc ++ "(" ++ ppV v1 ++ ", " ++ ppV v2 ++ ")"] ++
>                                                                         cgStr (filter canOpt (allElemsStruct v1))
>              where
>                 canOpt (_,TLInt)         = True
>                 canOpt (_,TLReal)        = True
>                 canOpt _                 = False

>                 cgStr [(name,_)]     = codeSingle v (updStructNameDT v1 name) v1 (updStructNameDT v2 name) v2
>                 cgStr ((name,_): ss) = [TLIfs (compareEqExp (updStructNameDT v1 name) (updStructNameDT v2 name))
>                                        -- then
>                                        (cgStr ss) 
>                                        -- else
>                                        (codeSingle v (updStructNameDT v1 name) v1 (updStructNameDT v2 name) v2)]

>            -- hier erstmal Kommentar anstelle von Fehlermeldung
>            cgChoiceSingle v chc v1 v2 = [commentBox $ ["ERROR: cgChoiceSingle: undefined pattern:",
>                                                               ppv v ++ " = " ++ ppChoice chc ++ "(" ++ ppv v1 ++ ", " ++ ppv v2 ++ ")"]]
>              where ppv (v, dt) = "(" ++ ppV (v,dt) ++ ", " ++ pretty dt ++ ")"

>            -- cgChoiceSingle v chc v1 v2 = error $ "cgChoiceSingle: undefined pattern:\n" ++
>            --                                             ppv v ++ " = " ++ ppChoice chc ++ "(\n   " ++ ppv v1 ++ ",\n   " ++ ppv v2 ++ ")"
>            --   where ppv (v, dt) = "(" ++ ppV (v,dt) ++ ", " ++ pretty dt ++ ")"



> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig (IL2Filter f p) = (vout, 
>                                                                          ntvs, lvs', 
>                                                                          [TLIf f (p' ++ [(TLAssign vout (TLVar v'))]) 
>                                                                                         (assignNil chc vout)]) 
>                                             where
>                                              (v', tvs', lvs', p') = cgUnit pName ysizes imData recur opts chc False cnd tvs lvs il2dt algs sig p   
>                                              (vout,ntvs)    = newtv tvs' (il2Type opts (isAtomar opts chc) il2dt algs p)

> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig thisCalc@(IL2Calc cmt bnd cUnits rhss) =  
>      (completeV, completeTvs, (collectCUVars cUnits) ++ completeLvs, completeCode)
>   where        
>      (completeV, completeTvs, completeLvs, completeCode) = ifbounds bnd (
>                                                              processCUnits cUnits (
>                                                                foreach tvs lvs complx (
>                                                                  rhss
>                                                                )
>                                                              )
>                                                            )

------- alt: ---------

      (completeV, completeTvs, completeCode) = ifbounds bnd (
                                                  for loops (
                                                    locAssigns lass (
                                                      ifterms ift (
                                                        foreach tvs complx (
                                                          rhss

-------------------------------------------------------------------------

>      comment = [commentLn $ ppV completeV ++ " = " ++ cmt]
>      startComment = [commentLn "start of"] ++ comment
>      endComment   = comment ++ [commentLn "finished"] ++ [TLLayout "\n"]

>      ifbounds bnd thn  = (v', tvs', lvs, startComment ++ [TLIf (cgBounds bnd) thn' (assignNil chc v')] ++ endComment) 
>          where
>            (v', tvs', lvs, thn') = thn

>      -- hole die zu uebersetzenden complx-IL2-Ausdrücke. Hier sind nur die ersten aus rhss relevant, da sie 
>      -- in allen rhss identisch sind:
>      complx :: [(Int, String, IL2)]   -- idNumber, Comment, IL2
>      complx = map addID (zip [1..] (snd5 (head rhss))) where
>                 addID (n,(cmt, _, il2)) = (n, cmt, il2)

>      foreach tvs lvs complx rhss = case complx of
>                     []        -> (vBody, tvsBody, lvs, body)
>                     otherwise -> case (any (isListStruct.thd4) codeBind) of 
>                          True  -> (vForEach, tvsForEach, lvsComplx, complxCode ++
>                                                                     (assignNil chc vForEach) ++ bodyForEach ++ freememcode)
>                          False -> (vForEach, tvsForEach, lvsComplx, complxCode ++ bodyForEach)

>              where

>                -- body enthält die Zuweisungen der Algebra-Rhs:
>                body | containsExpVar "rmThis" outerContextExp         =              tvalloc ++ assignCode
>                     -- im subopt-modus koennen wir durch eine zusaetzliche Abfrage verhindern,
>                     -- dass ueberfluessige Strukturen aufgebaut werden. Dies geht allerdings nur, 
>                     -- wenn die aktuelle Produktion tabelliert ist, ansonsten muessten wir fuer jede
>                     -- abfrage nochmal den aktuellen Wert berechnen, was sicherlich aufwaendiger ist.
>                     | elem (getOptBT opts) [BTSubOpt, BTSubOptCut] && 
>                       not (elem pName recur)                          = [TLIf ifExp (tvalloc ++ assignCode) [TLAssign vBody TLNil]]
>                     | otherwise                                       =              tvalloc ++ assignCode
>                    where
>                     assigns = map assignRhs rhss
>                     assignCode = concatMap snd assigns
>                     outerContextExp = getFirst $ fst (head assigns)
>                     getFirst (ExpTupel exps) = head exps
>                     getFirst exp             = exp
>                     optVal = VANonterm (ptbl prefixes ++ pName) (indexMap opts IMA ysizes imData pName (ST (Var "i",Var "j")))
>                     optVal' | isTupelStruct (elemStruct (Direct "v",(getListItemDataType (ntType opts il2dt algs pName))) 1)
>                                                  = optVal :. (Direct "tup1")
>                             | otherwise          = optVal
>                     ifExp = ExpIOp (ExpPOp "abs" [ExpIOp (ExpTLVar optVal') "-" outerContextExp]) "<=" (ExpVar "diff")

>                (vBody,tvsBody) = newtv tvs (il2Type opts (isAtomar opts chc) il2dt algs thisCalc)

>                -- muessen wir Speicher allokieren?
>                tvalloc | not $ isListStruct vBody  =  []        -- nein
>                        | otherwise              =  
>                            [TLAlloc MTTemp vBody (ExpNum 1) (pointerType (snd vBody))]   -- Allokiere Speicher
>                            ++ listStop vBody                                      -- setze Listenende auf nil

>                -- assignRhs: erzeugt Zuweisungen für eine einzelne Algebra:
>                assignRhs (algName, complx, algType, outerContext, rhs) = 
>                              case rhs of
>                                    (ExpTupel _ )      -> let (ExpTupel texprs) = rhs' 
>                                                              ntexprs = zip [1..] texprs
>                                                              assigns = concatMap assign ntexprs
>                                                              assign (n,e) = assignResultTup vBody n (TLExp e)
>                                                          in (outerContextExp, outerContextC ++ assigns)
>                                    otherwise          ->    (outerContextExp, outerContextC ++ assignResult vBody (TLExp rhs'))
>                   where

>                     outerContextC = [] -- [TLComment $ ["outer Context:"] ++ map pp outerContext] ++ [TLComment $ ["merged:"] ++ [prettyLang C outerContextExp]]
>                        where
>                          pp (args, rhs) = "args: "++ show args ++ " exp: " ++ prettyLang C rhs

>                     -- Kombiniere die Algebra-Variablen mit den TL-Variablen
>                     complxBind = concatMap merge (zip [1..] complx) where
>                        merge (id, (_, varList, _)) | relevantIL2 == [] = []
>                                                    | otherwise         = map (\(algVar, structContext) -> (algVar, structContext, tlvar)) varList

>                          where
>                            relevantIL2 = [ tlvar | (id', tlvar) <- tlVarBind, id' == id ]
>                            r = head relevantIL2
>                            tlvar  | not $ isListStruct r = r 
>                                   | otherwise            = listItem r

>                     rhs' = transformExp rhs
>                     transformExp exp = insertVarAccess False complxBind (updateIndices $ compileConstr complxBind exp)

>                     assignResult n res = [TLAssign var res]
>                       where 
>                         var | cnd       = structElem (varItem n) algName
>                             | otherwise = varItem n
>                     assignResultTup n e res = [TLAssign var res]
>                       where
>                         var | cnd       = structElem (structElem (varItem n) algName) ("tup" ++ show e)
>                             | otherwise = structElem (varItem n)                      ("tup" ++ show e)

>                     -- calcOuterContext: wir berechnen den aeusseren Kontext (nur fuer die erste Algebra)
>                     outerContextExp = snd $ calcOuterContext (head rhss)
>                     calcOuterContext (algName, _, _, outerContext, rhs) = (\(vs, exp) -> (vs, transformExp exp)) bind
>                        where
>                          bind = bind' (([], rhs): outerContext)
>                          bind' [exp] = exp
>                          bind' ((b1,exp1):(b2,exp2):exps) = bind' ((b2, insertVarBinds bs exp2):exps)
>                              where bs = zip (map fst b2) (flat exp1)
>                                    flat (ExpTupel exps) = concatMap flat exps
>                                    flat x               = [x]


>                freememcode 
>                   -- im backtrace-modus darf an dieser Stelle nichts freigegeben werden:
>                   | getOptBT opts /= BTNone  = []
>                   | otherwise                = concatMap ((freemem opts) . fst) free 
>                   where
>                     bind      = zip tlComplxVars (map thd3 complx)
>                     free      = filter isList bind
>                     isList (v, IL2Nonterminal f _) | elem f recur && isListStruct v  = True
>                                                    | otherwise                       = False
>                     isList (v, _)                  | isListStruct v                  = True
>                                                    | otherwise                       = False

>                -- codeBind :: [Comment, Id, TLVar, [TL]] enthaelt
>                -- die Bindungen des erzeugten Codes fuer die
>                -- Complx-Exprs. TLVar enthaelt jeweils des Ergebnis
>                -- des Codeausdrucks:

>                (tlv, tvsComplx, lvsComplx, codeBind) = compileCmplx tvsBody lvs complx
>                   where
>                   compileCmplx tvs lvs [(id,cmt,il2)]    
>                                  = (v', tvs', lvs', [(cmt, id, v', il2')])  where 
>                                    (v', tvs', lvs', il2')  = cgUnit pName ysizes imData recur opts leakChoice True cnd tvs lvs il2dt algs sig (addOuterContext il2)
>                   compileCmplx tvs lvs ((id,cmt,il2):ts) 
>                                  = (vt'', tvs'', lvs'', (cmt, id, v', il2'):ts') where
>                                    (v',   tvs',  lvs', il2')  = cgUnit pName ysizes imData recur opts leakChoice True cnd tvs lvs il2dt algs sig (addOuterContext il2)
>                                    (vt'', tvs'', lvs'', ts')  = compileCmplx tvs' lvs' ts
>                   leakChoice = case getOptLCF opts of
>                                  False -> _chc_init
>                                  True  -> chc 

>                   outerContext = (\(_,complx,_,ctx,rhs) -> 
>                                  -- falls hier Ausdruecke mit mehreren complx-Expressions vorliegen, wird hier die 
>                                  -- dummy-Variable "rmThis" eingefuehrt, damit spaeter an diesen Stellen keine
>                                  -- CutOff-Abfragen erstellt werden. Ansonsten muessen wir parallel in alle complx-
>                                  -- Ausdruecke hineinsteigen, und die auesseren Kontexte entsprechend verteilen...
>                                  if length complx > 1 then ([("xx",[])], ExpIOp (ExpVar "xx") "+" (ExpVar "rmThis")):ctx
>                                                       else (snd3 $ head complx, rhs):ctx) (head rhss)    -- == inner rhs

>                   addOuterContext (IL2Calc n b cu (i:is)) = IL2Calc n b cu ((addCtx i):is)
>                       where
>                         addCtx (n, ca, dt, ctx, rhs)  = (n, ca, dt, outerContext, rhs)
>                   addOuterContext (IL2Alt il2s)       = IL2Alt (map addOuterContext il2s)
>                   addOuterContext (IL2Choice ads il2) = IL2Choice ads (addOuterContext il2)
>                   addOuterContext (IL2Filter exp il2) = IL2Filter exp (addOuterContext il2)
>                   addOuterContext (IL2TTUnit i1 i2)   = IL2TTUnit (addOuterContext i1) (addOuterContext i2)
>                   addOuterContext x                   = x

>                complxCode   = concatMap fth4 codeBind   -- der Target-Code fuer die complx-Ausdruecke
>                tlComplxVars = map       thd4 codeBind   -- die Ergebnis-Variablen der Code-Blöcke

>                -- Bindung der il2-Ausdrücke an die Ergebnisvariablen im Zielcode,
>                -- dadurch eindeutige Identifikation:

>                (vForEach, tvsForEach, tlVarBind, bodyForEach) 
>                     -- falls ueber irgendeinen der complx-Ausdruecke iteriert werden muss, muss eine
>                     -- merging-Function mitgeliefert werden:
>                     | any (isListStruct.thd4) codeBind  = let
>                          (vMerge,tvsMerge)              = newtv tvsComplx (il2Type opts (isAtomar opts chc) il2dt algs thisCalc)
>                          mergeCode                      = mergeResults opts cnd chc vMerge vBody vMerge
>                          in       cgLoop vMerge tvsMerge codeBind (body ++ mergeCode)
>                     | otherwise = cgLoop vBody    tvsComplx  codeBind  body

>                cgLoop v tvs [] body          = (v,    tvs,  [],body) 
>                cgLoop v tvs (cbind:cbs) body 
>                      -- falls Ergebnis in Listenstruktur, muss iteriert werden:
>                      | isListStruct tlvar = (v, ntvs, (id, vout):vbind', comment ++ 
>                                                                             [TLAssign vout (TLVar tlvar),
>                                                                              TLWhileNN vout (body' ++ next)])
>                      -- falls nicht iteriert werden muss, bleibt die Originalbindung (id,tlvar) erhalten:
>                      | otherwise          = (v', tvs', (id, tlvar):vbind', body')
>                      where
>                        (cmt, id, tlvar, tlCode)   = cbind
>                        comment = [commentBox [ppV vBody ++ " stores the result of a single application of algebra function",
>                                              ppV tlvar ++ " holds the results of expression",
>                                              "     " ++ cmt,
>                                              "", 
>                                              "we use " ++ ppV vout ++ " to iterate over " ++ ppV tlvar ++ " and",
>                                              ppV vForEach ++ " to collect the results"]]
>
>                        (v', tvs',vbind', body') = cgLoop v tvs cbs body
>                        next                     = [listStep vout]
>                        -- vout ist die "Iterations-Hilfsvariable". Diese hat den gleichen Typ wie tlvar:
>                        (vout,ntvs)              = newtv tvs' (snd tlvar)

>      processCUnits [] body     = body
>      processCUnits (u:us) body | isCULoop u = for loops (processCUnits cUnits body)
>         where
>           (loops, cUnits) = getCULoops (u:us)

>           for ls body = case ls of 
>                          []        -> (v', tvs',   lvs,  body') 
>                          otherwise -> (v'', tvs'', lvs, (assignNil chc v'') ++
>                                                            cgloops ls (body' ++ mergeCode))
>              where
>                (v', tvs', lvs, body')   = body
>                (v'',tvs'')              = newtv tvs' (il2Type opts (isAtomar opts chc) il2dt algs thisCalc)
>                mergeCode                = mergeResults opts cnd chc v'' v' v''
>                cgloops [] body          = body
>                cgloops (l:ls) body      = [TLFor k from to (cgloops ls body)] where
>                                            (k, from, to) = l

>      processCUnits (u:us) body | isCUAssign u = locAssigns assigns (processCUnits cUnits body)
>         where
>           (assigns, cUnits) = getCUAssigns (u:us)

>           locAssigns lass code = let (v, tvs, lvs, body) = code
>                                      lassCode = map (\(n, e) -> cgAssign n e) lass -- TLAssign (toVA $ n) (TLExp e)) lass
>                                      cgAssign n e | isNonterm e = TLAssignIF (toVA n) bnd (TLExp e) (TLExp (nilAtom chc'))
>                                                   | otherwise   = TLAssign   (toVA n)     (TLExp e)
>                                       where
>                                         isNonterm (ExpTLVar v) = isVANonterm v
>                                         isNonterm _            = False
>                                         nonterm (ExpTLVar v)   = getVANonterm v
>                                         (name,ss)              = nonterm e
>                                         nonterm_name = drop (length (ptbl prefixes)) name
>                                         nonterm_ys   = getYSize ysizes nonterm_name
>                                         bnd          = cgBounds (ss, nonterm_ys)
>                                         (ExpTLVar var) = e
>                                         -- wenn kein Tupel oder erstes Tupel-Element, dann wird die erste chc-Fct genommen
>                                         -- ansonsten sum -> assign = 0
>                                         chc' | tupelElemNumber var <= 1 = chc
>                                              | otherwise                = [exp_choiceSum]
>                                         
>                                  in (v, tvs, lvs, lassCode ++ body)

>      processCUnits (u:us) body | isCUIf u = ifterms (expMakeAndList ifs) (processCUnits cUnits body)
>         where
>           (ifs, cUnits) = getCUIfs (u:us)

>           ifterms ift thn = case ift of 
>                              []        -> (v', tvs', lvs, thn')
>                              otherwise -> (v', tvs', lvs, [TLIf (head ift) thn' (assignNil chc v')]) 
>                            where
>                              (v', tvs', lvs, thn') = thn

>      processCUnits (u:us) body = pattErr "processCUnits" u

>      -- in rhs::Exp koennen bei Verwendung von cg2v Nonterminal-Konstruktionen
>      -- stehen, diese muessen noch auf die speicheroptimierten Tabellen-Indizes
>      -- umgerechnet werden
>      -- gleichzeitig wird im Backtrace-Modus der NT-Strukturzugriff wieder aufgeloest:
>      updateIndices rhs = upd rhs
>          where
>            upd (ExpTLVar v)  | isVANonterm v = if getOptBT opts == BTNone then ExpTLVar (updn v)
>                                                                           else ExpTLVar (updn $ getNonterm v)
>                 where

>                   getNonterm (VANonterm n s) = (VANonterm n s)
>                   getNonterm (e :. (Direct el))  = if take 3 el   == "tup" then (getNonterm e) :. (Direct el)
>                                                                            else  getNonterm e
>                   getNonterm x                   = x

>                   updn (VANonterm nt s) = VANonterm nt (indexMap opts IMA ysizes imData (drop (length $ ptbl prefixes) nt) s)
>                   updn (nt :. e)        = (updn nt) :. e
>            upd x             = mapExp upd x

>      -- compileConstr: wandelt Konstruktoren in Rhs in Funktionsaufrufe um:
>      compileConstr bs (ExpConstr name exps)    | getOptBT opts == BTNone = ExpPOp (pnew prefixes ++ name) (concatMap compileConstr' exps)
>                                                -- backtrace-Modus:
>                                                | otherwise        = ExpPOp (pnew prefixes ++ name ++ "_" ++ fctType) args
>           where
>             args = concatMap conv (concatMap compileConstr' exps)
>             conv (ExpTLVar v) | isVANonterm v && isFirstElement v = case getVANonterm v of
>                (nt,ST (i,j)         ) -> [ExpVar $ backName nt, ExpME i,  ExpME j]
>                (nt,TT (i1,j1)(i2,j2)) -> [ExpVar $ backName nt, ExpME i1, ExpME j1, ExpME i2, ExpME j2]
>             -- im SubOptCut-Modus wollen wir alle Argumente ausser den Struktur-Zeiger herauswerfen
>             -- TODO: hier gibt es Probleme, falls Algebrafunktionen mit mehr als 10 Argumenten verwendet werden!
>             conv x@(ExpVar v) | getOptBT opts == BTSubOptCut =
>                                if elem v (map fst3 bs) && (length v == 2 || (length v == 3 && v!!2 == '1')) then [x]
>                                                                                                             else []
>             conv x  | getOptBT opts == BTSubOptCut = []
>                     | otherwise                    = [x]

>             backName nt = pback prefixes ++ drop (length $ ptbl prefixes) nt
>             -- ein Hack, um mit Tupel-Ergebnissen von Algebren umzugehen:
>             -- wir nehmen an, dass sich der Struktur-Teil immer im ersten Tupelement befindet
>             isFirstElement ((v :. (Direct algName)) :. (Direct tup)) = tup == "tup1"
>             isFirstElement _                                         = True

>             fctType = concatMap snd $ nubBy eq (map f args)
>               where
>                 -- hier filtern wir diejenigen Argumente fuer strukturierte Tupel-Ausdruecke heraus, die den
>                 -- gleichen Praefix haben. Dies sind diejenigen, die zu dem gleichen Ausdruck gehoeren
>                 -- TODO: hier gibt es Probleme, falls Algebrafunktionen mit mehr als 10 Argumenten verwendet werden!
>                 eq (a1,"s") (a2,"s") = a1 == a2
>                 eq _        _        = False
>                 f (ExpVar v) | isPrefix (pback prefixes) v = (v,"f")
>                              -- wenn fuer die Variable v eine Bindung vorliegt, handelt es sich um einen strukturierten Ausdruck:
>                              | elem v        (map fst3 bs) = (take 2 v,"s")
>                              | otherwise                   = ("","")
>                 f _                                        = ("","")

>      compileConstr bs x  = mapExp (compileConstr bs) x 

>      -- fieser Hack, muss noch ordentlich gemacht werden!!!
>      compileConstr' (ExpTupel exps)             = concatMap compileConstr' exps
>      compileConstr' x                           = [x]


>      -- inNS gibt an, ob wir uns in einem new_struct_ - Aufruf
>      -- befinden. Moeglicherwise kann man die verschiedenen
>      -- Ersetzungphasen auch etwas umsortieren
>      insertVarAccess :: Bool -> [(String, [String], VarAccess)] -> Exp ->  Exp
>      insertVarAccess inNS bs (ExpVar j)  = ret where 
>                                       nb = [ (sc,va,dt) | (bn, sc, (va,dt)) <- bs, bn == j]
>                                       ret = case nb of
>                                         []         -> ExpVar j
>                                         otherwisse -> let (sc, va, dt) = head nb
>                                                           (calcAlg', enumAlg') = fetchAlgs (va, dt)
>                                                           (_ :. (Direct calcAlg),_ ) = calcAlg' 
>                                                           (_ :. (Direct enumAlg),_ ) = enumAlg' 
>                                                           [sc1,sc2] = sc
>                                                           res | getOptBT opts == BTNone   = (structAccess va sc)
>                                                               -- Tupel-Struktur im BT-Mode:
>                                                               | length sc == 2 && sc1 == enumAlg && sc2 == "tup1"
>                                                                                           = (structAccess va [enumAlg])

>                                                               | length sc == 2 && sc1 == enumAlg && take 3 sc2 == "tup"
>                                                                                           = (structAccess va [calcAlg, sc2])
>                                                               | otherwise                 = (structAccess va sc)
>                                                       in ExpTLVar res

>      insertVarAccess inNS bs (ExpPOp f a)     = ExpPOp f (map (insertVarAccess inNS' bs) a)
>                                               where inNS' = inNS || isPrefix (pnew prefixes) f 

>      insertVarAccess inNS bs x                = mapExp (insertVarAccess inNS bs) x


> cgUnit pName ysizes imData recur opts chc infe cnd tvs lvs il2dt algs sig x = pattErr "cgUnit" x

> -- for lookahead combinator:
> cgBounds (ST (bl, bu), ST (lal, lar))  
>   | includesLAME lal || includesLAME lar -- lal and lar acn contain Max/Min and LA, so we catch all patterns...
>                 = ExpIOp  (ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME lal)) "&&"
>                           (ExpIOp  (ExpME (calcME (bu :- bl))) "<=" (ExpME lar))

> -- the rest can contain numbers or expressions with variables:
> cgBounds (ST (bl, bu), ST (ysl, Infinite))  = ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME (ysl))
> cgBounds (ST (bl, bu), ST (ysl, ysu)) 
>    | ysl == ysu = ExpIOp  (ExpME (calcME (bu :- bl))) "==" (ExpME (ysl)) 
>    | otherwise  = ExpIOp  (ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME (ysl))) "&&"
>                           (ExpIOp  (ExpME (calcME (bu :- bl))) "<=" (ExpME (ysu)))


> -- for twotrack
> cgBounds (TT s1 s2, TT y1 y2) = ExpIOp (cgBounds (ST s1, ST y1)) "&&"
>                                        (cgBounds (ST s2, ST y2))




> codegen (tl, filename, cmdl, (version,time), (targetcode,mainCode,mainaddCode,outputCode), 
>          frame, trackMode, tms, ysizes, ranges, recur, recs, 
>          axiom, orgOpts, cnd, cndBT, il2dt, il2dtBT, enumAlgName, algfs, algfsBT, 
>          specialChoice, ppAlg, signature, crfilter) (il2, il2BT) =

>   -- global processing:
>   let proc1  | getOptITA opts             = snd $ collectTLs macroWorker [] targetCode  -- inline table access macros
>              | otherwise                  = targetCode
>       proc2  | getOptBT optsBT /= BTNone  = removeObsoleteFcts proc1                    -- remove obsolete constructor functions
>              | otherwise                  = proc1
>       proc3  | getOptIF  opts             = inlineFunctions proc2                       -- inline functions
>              | otherwise                  = proc2
>       proc4  | getOptNC  opts             = snd $ collectTLs commentWorker 0 proc3      -- suppress comments
>              | otherwise                  = proc3
>       proc5  | getOptO   opts             = optimizeTL1 proc4                           -- target code optimization -O
>              | otherwise                  = proc4
>       proc6  | getOptO2  opts             = optimizeTL2 proc5                           -- target code optimization -O2
>              | otherwise                  = proc5
>       proc7                               = cleanUpTL proc6                             -- clean up code
>       proc8  | getOptBEA opts             = beautifyTL proc7                            -- beautify code
>              | otherwise                  = proc7

>   in proc8

>   where

>   targetCode =
>          [tlLongComments $ ["compiled by the ADP compiler, version " ++ version ++ "    " ++ calendarTimeToString time,
>                             "source file: " ++ (if elem '>' filename then "<no file>" else filename),
>                             "command: ", cmdl]] ++
>          ifFortran ([TLLayout $ "PROGRAM " ++ stripName filename ++ "\n"])                   ++
>          header'                                                                             ++
>          gcIncludes                                                                          ++
>          libListIncludes                                                                     ++
>          -- wenn targetcode vorhanden, muessen die Eingabevariablen dort deklariert werden,
>          -- ansonsten werden sie automatisch ausgegeben
>          (if targetcode == [] then []
>             else
>               optionalBlock "user code"                                  [TLLayout targetcode]) ++
>          optionalBlock "data structures"                                 updatedDataStructs  ++
>          optionalBlock "supporting functions for objective functions"    specialChoiceCode   ++
>          optionalBlock "backtrace variables"                             backtraceVars       ++
>          signatureCode                                                                       ++
>          optionalBlock "table access"                                    tableAccessMacros   ++ 
>          optionalBlock "table declarations"                              tableDecls          ++ 
>          (ifFortran $ [tlLongComment "main code"] ++ 
>                       (if mainCode == [] then trailer' else [TLLayout mainCode]) ++
>                                                             [TLLayout "CONTAINS\n"]) ++
>          optionalBlock "forward declarations"                            forwardDecls        ++ 
>          optionalBlock "table calculations"                              productions'        ++
>          optionalBlock "forward declarations for backtracing functions"  forwardDeclsBT      ++ 
>          optionalBlock "backtracing code"                                backtrace           ++
>          optionalBlock "table memory allocation"                         memAlloc            ++
>          optionalBlock "free memory"                                     memFree             ++
>          optionalBlock "table move for window mode"                      cgMoveTables        ++
>          optionalBlock "main dynamic programming loop"                   mainLoop            ++
>          (ifNotFortran (if mainCode == [] then trailer' else [TLLayout mainCode]))           ++
>          ifFortran ([TLLayout "END PROGRAM"])


>   -- Tools:
>   ifFortran    code      = if tl == F then code else []
>   ifNotFortran code      = if tl /= F then code else []
>   stripName              = filter isAlphaNum
>   optionalBlock cmt code | code == []               = []
>                          | code == [TLTypeDecls []] = []
>                          | code == [TLDefines   []] = []
>                          | code == [TLLayout    []] = []
>                          | otherwise                = [tlLongComment cmt] ++ code

>   -- Unterscheide zwischen normalen Optionen und dem Optionssatz mit enthaltener Backtrace-Option
>   -- fuer die meisten Codeerzeugungen wird die Backtrace-Option ignoriert:
>   (opts, optsBT) = case orgOpts of
>                      CompileOptions (rt, bt, soa, arr, lcf, nc, tas, ita, ifct, toptO, toptO2, bea, gc, off, window)     ->
>                        (CompileOptions (rt, BTNone, soa, arr, lcf, nc, tas, ita, ifct, toptO, toptO2, bea, gc, off, window), 
>                         CompileOptions (rt, bt    , soa, arr, lcf, nc, tas, ita, ifct, toptO, toptO2, bea, gc, off, window))

>   -- Standard-header und -trailer ------------------------------------------------------------------------------
>   (header, trailer) = frame
>   header'           = header
>   trailer'          = trailer

>   -- GC - Includes -------------- ------------------------------------------------------------------------------
>   gcIncludes | getOptGC opts == GCbdw = [TLLayout $ 
>                                          "#include \"gc.h\"\n" ++
>                                          "#define calloc(N,S) GC_MALLOC((N)*(S))\n"]
>              | otherwise              = []

>   -- libList - Includes -------------- ------------------------------------------------------------------------------
>   libListIncludes = []   -- currently deactivated
>   -- libListIncludes | length specialChoice /= 0 = [TLLayout $ 
>   --                                                "#include \"libList.c\"\n"] ++ [TLDecls [(["k_best"], soDiffDatatype)]]
>   --                 | otherwise                 = []


>   -- Track-Mode ------------------------------------------------------------------------------------------------
>   globalTrackMode = maxTrackMode tms

>   -- fuer das Index-Mapping benoetigte Daten -------------------------------------------------------------------
>   imData = (ranges, recs \\ recur)

>   -- Nur die Namen der Algebren: -------------------------------------------------------------------------------
>   algebras = algfs 

>   -- Uebersetze Signatur ---------------------------------------------------------------------------------------
>   sigName        = fst (head signature)
>   signatureCode  = updDataStructures $ compileSignature trackMode enumAlgName axiom optsBT il2dtBT algfsBT ppAlg signature

>   -- Uebersetze Produktionen und aktualisiere die Datenstrukturen ----------------------------------------------
>   productions :: [(String, (VAccess, DataType), [TL])]
>   productions    = map (cgProd opts cnd algebras il2dt) il2

>   (dataStructures, productions') = collectDataStructures (0,0,[]) (concatMap thd3 productions)

>   -- Erzeuge Backtracing-Code und aktualisiere die Datenstrukturen ----------------------------------------------
>   (dataStructuresBT, backtrace) | getOptBT optsBT /= BTNone = collectDataStructures dataStructures 
>                                                                      (concatMap thd3 (map (cgProd optsBT cndBT algfsBT il2dtBT) il2BT))
>                                 | otherwise                 = (dataStructures, [])

>   -- neue Datenstrukturen koennen im Produktions- und im Backtrace-Berich erzeugt werden.
>   -- die aktualisierten Strukturen liegen dann in ncds' bzw. dataStructuresBT:
>   updDataStructures :: [TL] -> [TL]
>   updDataStructures tl = snd (collectDataStructures dataStructuresBT tl)

>   updatedDataStructs = updDataStructures $ mergeDataStructures dataStructuresBT

>   -- Makros fuer Table access ----------------------------------------------------------------------------------
>   tableAccessMacros :: [TL]
>   tableAccessMacros  | decls == [] = [] 
>                      | otherwise   = updDataStructures decls
>     where 
>      decls = cgTableAccessMacros (concatMap tbls productions)
>      tbls :: (String, VarAccess, [TL]) -> [(String, DataType)]
>      tbls (n, (_,dt), _) | elem n recur = []
>                          | otherwise    = [(n,dt)]


>   -- Uebersetze TabellenDeklarationen, Speicherallokation und Hauptschleife ------------------------------------
>   -- aktualisiere jeweils die Datenstrukturen, die waehrend cgProd erzeut wurden -------------------------------
>   tableDecls  | decls == [] = [] 
>               | otherwise   = updDataStructures [TLDecls decls] 
>     where 
>      decls = (map (cgDecl parr) ((concatMap tbls productions) ++ map gencr crfilter)) ++
>              if getOptBT optsBT == BTSubOptCut then 
>              (map (cgDecl pdiffarr) ((concatMap tbls productions) ++ map gencr crfilter)) else []  
>      tbls (n,v,_) | elem n recur = []
>                   | otherwise    = [(n,v)]
>      gencr s = ("contains_" ++ s, (Direct "", TLChar)) 
>   forwardDecls   = updDataStructures (cgForward   recs)
>   forwardDeclsBT = updDataStructures (cgForwardBT recs)
>   memAlloc       = updDataStructures cgAlloc
>   mainLoop       = updDataStructures cgMain 

>   -- backtrace variables
>   backtraceVars | elem (getOptBT optsBT) [BTList, BTCompleteList, BTSubOpt] 
>                                                 = let dt = btWorkDataType optsBT sigName il2dtBT algfsBT axiom
>                                                   in  updDataStructures 
>                                                         [TLDecls $ [(["pp_next", "pp_initA", "removeAddr"], 
>                                                                           PointerOf dt), 
>                                                                     (["pp_init"], PointerOf $ PointerOf dt),
>                                                                     (["pp_initC"], TLInt),
>                                                                     (["copy_depth"], TLInt),
>                                                                     (["backtrace_tree"], dt)]
>                                                                     ++ outputVars]
>                 | otherwise                     = []
>      where 
>        outputVars | getOptBT optsBT == BTSubOpt    = [(["pp_outp", "result_prettyprint"], PointerOf TLChar)] ++ 
>                                                      [(["traceback_diff"], soDiffDatatype)] ++
>                                                      [(["rmAllowed"], TLChar)] 
>                   | getOptBT optsBT == BTSubOptCut = [(["traceback_diff"], soDiffDatatype)]
>                   | otherwise                      = []

>   -- supporting functions for objectives
>   specialChoiceCode = updDataStructures $ cgSpecialChoice specialChoice

------------------------------------------------------------------------------------------------------------------
------ Uebersetzung der Produktionen
------------------------------------------------------------------------------------------------------------------

>   cgProd opts cnd algebras il2dt (n, _ , u) = (n, v, [functionDef]) 
>     where
>     (v_u, tvs_u, lvs, out) = cgUnit n ysizes imData recur opts init_choice False cnd [] [] il2dt algebras (sigName, enumAlgName) u
>     (v, tvs, optGCCode)    | getOptGC opts == GCown && getOptBT opts == BTNone && isListStruct v_u
>                                                     = let (v', tvs') = newtv tvs_u (snd v_u)
>                                                           code = [commentLn "copy result list"] ++ 
>                                                                  (cgCopyList MTTemp v' (TLVar v_u)) ++
>                                                                  [commentLn "clear temporary memory"] ++ 
>                                                                  [TLFA "clear_tmp" []]
>                                                       in (v', tvs', code)
>                            | otherwise              = (v_u, tvs_u, [])
>     init_choice            = case concatMap getchc il2 of
>                               [] -> error $ "cgProd: found no production with choice function"
>                               xs -> head xs
>        where
>          getchc (n, _, IL2Choice f _) = [map fth4 f]
>          getchc _                     = []
>     functionDef            = (TLFD (longComment $ comment ++ " for production " ++ n) 
>                                    functionRes functionName
>                                    args
>                                    (tempVarDecl ++ loopVarDecl)
>                                    (ifCutoff
>                                      (ifBnd (out ++ optGCCode ++ assignResult) ++ optionalSubOptCode))
>                                    retVar)
>       where
>         comment      | getOptBT opts /= BTNone  = "backtracing code"
>                      | otherwise                = "table calculation"
>         functionRes  | getOptBT opts /= BTNone  = btWorkDataType opts sigName il2dt algebras axiom 
>                      | elem n recur             = snd v
>                      | otherwise                = TLVoid
>         functionName | getOptBT opts /= BTNone  = pback prefixes ++ n
>                      | otherwise                = pfct  prefixes ++ n
>         assignResult | getOptBT opts /= BTNone  = [commentLn "build candidate structures"]
>                      | elem n recur             = []
>                      | otherwise                = [commentLn "assign table entry result"]  ++ assign
>         retVar       | getOptBT opts == BTSubOptCut = TLVar v'
>                      | getOptBT opts /= BTNone  = let res | getOptBT opts == BTCompleteList = fst v
>                                                           | getOptBT opts == BTSubOpt       = fst v'
>                                                           | getOptBT opts == BTPF           = fst v'
>                                                           | otherwise                       = fst v :. (Direct enumAlgName)
>                                                   in TLFA (pbuild prefixes ++ pstr prefixes ++ sigName) 
>                                                      [TLFA (pcopy prefixes ++ pstr prefixes ++ sigName) [TLVar (toVA res)]]
>                      | elem n recur             = TLVar v
>                      | otherwise                = tlvar (Direct "_")

>     tempVarDecl = mergeDecls (map tvd (reverse tvs')) where 
>                     tvd (Direct n, dt) = ([n], dt)

>     loopVarDecl = if length lvs == 0 then [] else [(sort $ nub lvs, TLInt)]
>     ifBnd code | getOptBT opts /= BTNone = code
>                | elem n recur            = code
>                | otherwise               = ifbounds code
>     ifbounds assign = [TLIf (cgBounds (indx, getYSize ysizes n)) assign []]
>     assign = [TLAssign (toVA (VANonterm n' (indexMap opts IMA ysizes imData n indx))) (TLVar v)] 
>            where n' = ptbl prefixes ++ n

>     (args, indx) = let diffArg | elem (getOptBT opts) [BTSubOpt, BTSubOptCut] = [(["diff"], soDiffDatatype)]
>                                | otherwise                                    = []
>                    in case getTMode tms n of
>                       MST -> ([(["i"],TLInt),(["j"],TLInt)] ++ diffArg,
>                               ST (Var "i", Var "j"))
>                       MTT -> ([(["i1"],TLInt),(["j1"],TLInt),(["i2"],TLInt),(["j2"],TLInt)] ++ diffArg,
>                               TT (Var "i1", Var "j1") (Var "i2", Var "j2"))

>     ifCutoff code | getOptBT opts /= BTSubOptCut = code
>                   | otherwise = [TLIf (ExpIOp (ExpVar "diff") "<=" (ExpTLVar difftbl))
>                                       [TLAssign v' TLNil]  -- then 
>                                       -- else
>                                       ([TLAssign (toVA difftbl) (TLVar (Direct "diff", TLVoid))] ++
>                                        code)] 
>           where
>              difftbl = VANonterm n' (indexMap opts IMA ysizes imData n indx)
>               where n' = pdifftbl prefixes ++ n


>     (v', tvs', optionalSubOptCode) | getOptBT opts == BTSubOpt      = (vEnumList, ntvs'', code) 
>                                    | getOptBT opts == BTPF          = (vEnumList, ntvs'', code)  
>                                    | getOptBT opts == BTSubOptCut   = (vCutOff, ntvs''', code ++ 
>                                                                       [TLAssign vCutOff 
>                                                                         (TLFA (pbuild prefixes ++ pstr prefixes ++ sigName) [TLVar vEnumList])])
>                                    | otherwise                      = (v, tvs, [])
>         where
>           (calcAlg, enumAlg)   = fetchAlgs (listItem vtmp)
>           (vtmp,      ntvs')   = newtv tvs   (snd v)
>           (vEnumList, ntvs'')  = newtv ntvs' (snd enumAlg)
>           (vCutOff,   ntvs''') = newtv ntvs'' (snd enumAlg)
>           code = [TLAssign vtmp (TLVar v),
>                   TLAssign vEnumList TLNil,
>                   TLWhileNN vtmp
>                     (listAppend vEnumList enumAlg vEnumList ++
>                      [listStep vtmp])
>                  ] ++ freemem --obsolete
>                    ++ [TLFA "memory_clear" [TLVar $ toVA $ Direct "adp_dynmem"]]

>           freemem | getOptGC opts == GCown = [commentLn "clear temporary memory"] ++ [TLFA "clear_tmp" []]
>                   | otherwise              = []

------------------------------------------------------------------------------------------------------------------
------ Table-Access-Makros
------------------------------------------------------------------------------------------------------------------

>   cgTableAccessMacros :: [(String, DataType)] -> [TL]
>   cgTableAccessMacros ps 
>     | getOptBT optsBT == BTSubOptCut = 
>         optionalOffset ++ [TLDefines $ concatMap (macroDef ptbl parr) ps' ++ 
>         concatMap (macroDef pdifftbl pdiffarr) ps']
>     | otherwise                      = 
>         optionalOffset ++ [TLDefines $ concatMap (macroDef ptbl parr) ps']
>     where

XXX where to we get the type for the contains regions stuff?

>       ps' = ps ++ map (\s -> ("contains_" ++ s, TLVoid)) crfilter

>       optionalOffset 
>         | elem TASOffset $ getOptTAS opts =
>             [TLDecls [([poffset prefixes], PointerOf TLInt)]]
>         | otherwise                       = []

>       macroDef :: (Prefixes -> String) -> (Prefixes -> String) -> (String, DataType) -> [(DataType, String, [String], [TL])]
>       macroDef pr1 pr2 (n, dt) 
>         | elem n recur = []
>         | otherwise    = 
>             [(dt, pr1 prefixes ++ n, args, 
>               [tlvar $ VANonterm (pr2 prefixes ++ n) indx ])]
>         where
>           indx  = indexMap opts IMM ysizes imData n 
>             (list2Index (map (\a -> Var $ "(" ++ a ++ ")") args))
>           args  = case getTMode tms n of
>                          MST -> ["I","J"]
>                          MTT -> ["I1","J1","I2","J2"]


------------------------------------------------------------------------------------------------------------------
------ Tabellen-Deklarationen
------------------------------------------------------------------------------------------------------------------

>   cgDecl prefix (n, (_, restype)) = ([prefix prefixes ++ n], dim)
>     where
>       dim   =  case getTMode tms n  of
>                       MST -> case getOptTAS opts of
>                                     [TASPointers]  -> let ST (r1, r2) = getTRange ranges n
>                                                           dim = if r1 && r2 then 2 else 1
>                                                        in pDepth dim rest
>                                     otherwise      ->     pDepth 1   rest
>                       MTT -> case getOptTAS opts of
>                                     [TASPointers]  -> pDepth 4 rest
>                                     otherwise      -> pDepth 1 rest
>       rest | prefix prefixes == pdiffarr prefixes = soDiffDatatype
>            | otherwise                            = restype


------------------------------------------------------------------------------------------------------------------
------ Tabellen-Speicherallokation
------------------------------------------------------------------------------------------------------------------

>   cgAlloc | allocs == [] = []
>           | otherwise    = [TLFD [] TLVoid "tableAlloc"  [] [decls] (optionalOffsetCode ++ allocs) (tlvar (Direct "_"))]
>     where 
>       -- lokale Variablen
>       decls = case globalTrackMode of
>                   MST -> (["i", "dim1", "dim2"], TLInt)
>                   MTT -> (["i", "j", "k", "dim1", "dim2", "dim3", "dim4"], TLInt)

>       allocs  | getOptBT optsBT == BTSubOptCut = concatMap (alloc parr) recs ++ concatMap (alloc pdiffarr) recs
>               | otherwise                      = concatMap (alloc parr) recs
>       alloc prefix n | elem n recur  = []
>                      | otherwise     = case getYSize ysizes n of
>         (ST (yl,yu))             ->   allocArray2 (prefix prefixes ++ n) resType (Var "n") yl  (ys2b (Var "n") yu) rst1 rst2
>         (TT (yl1,yu1) (yl2,yu2)) ->   allocArray4 (prefix prefixes ++ n) resType (Var "m") yl1 (ys2b (Var "m") yu1)
>                                                                                  (Var "n") yl2 (ys2b (Var "n") yu2)
>                                                                                   rtt1 rtt2 rtt3 rtt4

>         where
>          ys2b l Infinite = l
>          ys2b _ ys_u     = ys_u
>          (rst1, rst2) = case getTRange ranges n of
>                           ST (r1, r2) -> (r1, r2)
>          (rtt1, rtt2, rtt3, rtt4) = getRangeTT ranges n

>          resType | prefix prefixes == pdiffarr prefixes = soDiffDatatype
>                  | otherwise                            = ntType opts il2dt algebras n        

>       -- optionaler Code zum Ausrechnen des Offsets:
>       optionalOffsetCode | not(elem TASOffset $ getOptTAS opts)  = []
>                          | otherwise = [TLComment ["calculate offset"],
>                                         TLAlloc MTStatic (Direct (poffset prefixes), TLInt) (ExpME (Var "n" :+ Number 1)) TLInt,
>                                         TLFor "i" (Number 0) (Var "n") 
>                                           [TLAssign (ArrayElem [Var "i"] (Direct (poffset prefixes)), TLInt) 
>                                              (TLExp $ ExpME $ calculateOffset opts IMM ysizes imData "nop" (ST ((Var "nop"), (Var "i"))))]]

>   allocArray2 :: [Char] -> DataType -> MathExp -> MathExp -> MathExp -> Bool -> Bool -> [TL]  
>   allocArray2 arrName restype l1 yl1 yu1 r1 r2 
>     -- zweidimensionale Pointer-Strukturen nicht in Fortran:
>     | getOptTAS opts == [TASPointers] && tl == F = codeF
>     | otherwise                                  = codeC
>     where
>     tlvar = TLExp . ExpME
>     codeF =
>       [commentLn $ "memory allocation for " ++ arrName ++ ", yield size: " ++ ppYSize (ST (yl1, yu1)),
>        TLAssign(Direct "dim1",TLInt) (tlvar (if r1 then (l1 :- yl1) else (Number 0))),
>        TLAssign(Direct "dim2",TLInt) (tlvar (if r2 then (yu1 :- yl1) else (Number 0))),
>        TLAllocArr (Direct arrName, PointerOf restype) [(ExpME (Number 0), ExpME (Var "dim1")),
>                                                        (ExpME (Number 0), ExpME (Var "dim2"))]]

>     codeC 
>       | (elem TASBlock (getOptTAS opts) && not (elem TASBlockInterleave $ getOptTAS opts)) || 
>         (elem TASPointers (getOptTAS opts) && not (r1 && r2)) = 
>          let size = if r1 && r2 then (head $ indexMap2List opts IMM ysizes imData arrName (ST ((Var "n"), (Var "n")))) :+ (Number 1)
>                                 else (Var "n" :+ Number 1)
>          in [TLAlloc MTDynamic (Direct arrName, restype) (ExpME size) restype]

>       | elem TASBlockInterleave $ getOptTAS opts = 
>          let size = (head $ indexMap2List opts IMM ysizes imData (drop (length (parr prefixes)) arrName) (ST ((Var "n"), (Var "n")))) 
>                     :+ (Number (length tables))
>              tables     = recs \\ recur
>              firstTable = parr prefixes ++ head tables
>          in if firstTable == arrName 
>             then [TLAlloc MTDynamic (Direct arrName, restype) (ExpME size) restype]
>             else [TLAssign (Direct arrName, restype) (TLVar (Direct firstTable, restype))]

>       | getOptTAS opts == [TASPointers] =
>         [commentLn $ "memory allocation for " ++ arrName ++ ", yield size: " ++ ppYSize (ST (yl1, yu1)),
>          TLAssign(Direct "dim1",TLInt) (tlvar (if r1 then (l1 :- yl1) else (Number 0))),
>          TLAlloc MTDynamic (Direct arrName, PointerOf restype) (ExpME (Var "dim1" :+ Number 1)) (PointerOf restype),
>          TLFor "i" (Number 0) (Var "dim1") [
>             TLAssign(Direct "dim2",TLInt) (tlvar (if r2 then (Min ((l1 :- (Var "i")) :- yl1) (yu1 :- yl1)) else (Number 0))),
>             TLAlloc MTDynamic (ArrayElem [Var "i"] (Direct arrName), restype) (ExpME (Var "dim2" :+ Number 1)) restype]]

>   -- hier noch pruefen, ob obere Beschraenkung nicht mit dreiecksmatrix kollidiert (siehe allocArray4s_v_restr)
>   -- dies wuerde auch fuer allocArray2 gelten
>   allocArray4 :: [Char] -> DataType -> MathExp -> MathExp -> MathExp -> MathExp -> MathExp -> MathExp 
>                                   -> Bool -> Bool -> Bool -> Bool -> [TL]  
>   allocArray4 arrName restype l1 yl1 yu1 l2 yl2 yu2 r1 r2 r3 r4 
>     | tl == F    = codeF
>     | otherwise  = codeC
>     where
>     tlvar = TLExp . ExpME
>     arr idx = ArrayElem (map Var idx) (Direct arrName)
>     codeF = 
>       [commentLn $ "memory allocation for " ++ arrName ++ ", yield size: " ++ ppYSize (TT (yl1, yu1) (yl2, yu2)),
>        TLAssign(Direct "dim1",TLInt) (tlvar (if r1 then (l1 :- yl1) else (Number 0))),
>        TLAssign(Direct "dim2",TLInt) (tlvar (if r2 then (yu1 :- yl1) else (Number 0))),
>        TLAssign(Direct "dim3",TLInt) (tlvar (if r3 then (l2 :- yl2) else (Number 0))),
>        TLAssign(Direct "dim4",TLInt) (tlvar (if r4 then (yu2 :- yl2) else (Number 0))),
>        TLAllocArr (Direct arrName, PointerOf restype) [(ExpME (Number 0), ExpME (Var "dim1")),
>                                                        (ExpME (Number 0), ExpME (Var "dim2")),
>                                                        (ExpME (Number 0), ExpME (Var "dim3")),
>                                                        (ExpME (Number 0), ExpME (Var "dim4"))]]

>     codeC =
>       [commentLn $ "memory allocation for " ++ arrName ++ ", yield size: " ++ ppYSize (TT (yl1, yu1) (yl2, yu2)),
>        TLAssign(Direct "dim1",TLInt) (tlvar (if r1 then (l1 :- yl1) else (Number 0))),
>        TLAlloc MTDynamic (Direct arrName, pDepth 3 restype) (ExpME (Var "dim1" :+ Number 1)) (pDepth 3 restype),
>        TLFor "i" (Number 0) (Var "dim1") [
>           TLAssign(Direct "dim2",TLInt) (tlvar (if r2 then (Min ((l1 :- (Var "i")) :- yl1) (yu1 :- yl1)) else (Number 0))),
>           TLAlloc MTDynamic (arr ["i"], pDepth 2 restype) (ExpME (Var "dim2" :+ Number 1)) (pDepth 2 restype),
>           TLFor "j" (Number 0) (Var "dim2") [
>             TLAssign(Direct "dim3",TLInt) (tlvar (if r3 then (l2 :- yl2) else (Number 0))),
>             TLAlloc MTDynamic (arr ["i","j"], pDepth 1 restype) (ExpME (Var "dim3" :+ Number 1)) (pDepth 1 restype),
>             TLFor "k" (Number 0) (Var "dim3") [
>               TLAssign(Direct "dim4",TLInt) (tlvar (if r4 then (Min ((l2 :- (Var "k")) :- yl2) (yu2 :- yl2)) else (Number 0))),
>               TLAlloc MTDynamic (arr ["i","j","k"], pDepth 0 restype) (ExpME (Var "dim4" :+ Number 1)) (pDepth 0 restype)]]]]

------------------------------------------------------------------------------------------------------------------
------ Free memory
------------------------------------------------------------------------------------------------------------------

>   memFree :: [TL]
>   memFree = [TLFD [] TLVoid "freeall" [] [] code (tlvar (Direct "_"))]
>     where
>       code = [TLFA "adplib_free" [tlvar (Direct "opts"), tlvar (Direct "seq")],
>               TLMacro "@RNALIB_FREE@"] ++
>               concatMap free1 recs ++
>               concatMap free2 crfilter
>       free1 n | elem n recur = []
>               | otherwise    = [TLFA "free" [tlvar (Direct ("arr_" ++ n))]]
>       free2 n = [TLFA "free" [tlvar (Direct ("arr_contains_" ++ n))]]

------------------------------------------------------------------------------------------------------------------
------ Movetables
------------------------------------------------------------------------------------------------------------------

>   cgMoveTables :: [TL]
>   cgMoveTables | getOptW optsBT == False = []
>                | otherwise = [TLFD [] TLVoid "movetables" [(["p"], TLInt)] vars
>                     [TLFor "j" (Var "p") (Var "n") [
>                        TLFor "i" (Var "p") (Var "j") 
>                           (concatMap move recs)]]
>                     (tlvar (Direct "_"))]
>     where 
>     -- lokale Variablen:
>     vars  = case globalTrackMode of
>               MST  -> [(["i","j"], TLInt)]
>               MTT  -> [(["i1","j1","i2","j2"], TLInt)]
>     move n | elem n recur || (not (rst1 && rst2)) = []
>            | otherwise = [TLAssign (toVA (VANonterm n' (ST (Var "i" :- Var "p", Var "j" :- Var "p"))))
>                                      (tlvar (VANonterm n' (ST (Var "i", Var "j"))))]
>       where
>          n' = ptbl prefixes ++ n
>          (rst1, rst2) = case getTRange ranges n of
>                           ST (r1, r2) -> (r1, r2)

------------------------------------------------------------------------------------------------------------------
------ Mainloop
------------------------------------------------------------------------------------------------------------------

>   cgMain = [TLFD [] TLVoid "mainloop" [] (vars ++ helper ++ helperBT ++ helperW) 
>                 (callTmpAlloc                       ++    -- Speicherallokation f. temp. Speicher
>                  callAlloc                          ++    -- Speicherallokation
>                  allocBT (declToVA [head helperBT]) ++    -- Speicherallokation f. backtrace
>                  cgWindow (
>                    callContainsRegion                 ++    -- Aufrufe fuer die contains_region Tabellen
>                    mainloops                          ++    -- Schleifen mit Funktionsaufrufen
>                    if outputCode /= [] then           -- falls benutzerdefinierter Ausgabe-Code vorhanden
>                       [TLLayout outputCode] 
>                    else 
>                    (printAxioms                        ++     -- Ausgabe des optiomalen Scores
>                     codeBT (declToVA [head helperBT]))))       -- backtrace-Aufrufe
>            (tlvar (Direct "_"))]
>     where 

>     -- lokale Variablen:
>     vars  = case globalTrackMode of
>               MST  -> [(["i","j"], TLInt)]
>               MTT  -> [(["i1","j1","i2","j2"], TLInt)]
>     helper= concatMap fdir printedAxioms
>       where fdir (n,rec,(_, dt)) 
>               | elem rec recur && isListDT dt  = [(["v"++ show n, "v"++ show (n+1), "result_score"] ,dt)]
>               | elem rec recur                 = [(["v"++ show (n+1), "result_score"] ,dt)]
>               | getOptBT optsBT == BTSubOpt    = [(["v"++ show (n+1), "result_score"] ,dt)]
>               | getOptBT optsBT == BTPF        = [(["v"++ show (n+1), "result_score"] ,dt)]
>               | getOptBT optsBT == BTSubOptCut = [(["v"++ show (n+1), "result_score"] ,dt)]
>               | isListDT dt                    = [(["v"++ show n, "result_score"] ,dt)]
>               | otherwise                      = []
>     helperBT  | getOptBT optsBT /= BTNone      = [(["l"], btWorkDataType optsBT sigName il2dtBT algfsBT axiom),
>                                                   (["next"], btWorkDataType optsBT sigName il2dtBT algfsBT axiom), 
>                                                   (["score"], soDiffDatatype)] 
>               | otherwise                      = []
>     helperW   | getOptW optsBT == False        = []
>               | otherwise                      = [(["pos", "startj"], TLInt)]

>     -- Aufruf der Speicherallokation f. temp. Speicher
>     callTmpAlloc | getOptGC opts /= GCown = []
>                  | otherwise              = [TLFA "init_tmp" []]

>     -- Aufruf der Speicherallokation
>     callAlloc | functionCalls == [] = []
>               | otherwise           = [TLFA "tableAlloc" []]

>     -- Aufrufe fuer die Vorberechnung der contains_region Tabellen
>     callContainsRegion = map gen crfilter
>        where
>          gen s = TLAssign 
>                     (toVA (Direct $ "arr_contains_" ++ s)) 
>                     (TLFA ("calc_contains_region") [TLVar (toVA (Direct "z")),
>                                                     TLVar (toVA (Direct "n")),
>                                                     TLLayout $ "\"" ++ s ++ "\""])


>     cgWindow code | getOptW optsBT == False = code
>                   | otherwise               = 
>                        [TLForI "pos" (Number 0) ((Var "seq" :# Var "original_length") :- (Var "opts" :# Var "window_size")) (Var "opts" :# Var "window_step")
>                             ([TLAssign (Pointer (Direct "opts") :. Direct "window_pos", PointerOf (StructOf "toptions" [])) (tlvar (Direct "pos")),
>                               TLIf (ExpIOp (ExpVar "pos") "==" (ExpNum 0.0))
>                                  [TLAssign (toVA (Direct "startj")) (tlnumber 0)]  
>                                  [TLAssign (toVA (Direct "startj")) 
>                                     (meToTL ((Var "n" :- (Var "opts" :# Var "window_step")) :+ (Number 1)))],
>                               TLIf (ExpIOp (ExpVar "pos") ">" (ExpNum 0.0))
>                                  [TLFA "movetables" [tlvar (Pointer (Direct "opts") :. Direct "window_step")],
>                                   TLFA "shift_input" [tlvar (Direct "opts"), tlvar (Direct "seq"), tlvar (Direct "1")]] 
>                                  []] ++
>                               code)]

>     -- aeussere Schleifen
>     mainloops | functionCalls == [] = []
>               | otherwise           = [(loops functionCalls)]
>     loops body = case globalTrackMode of
>             MST -> let startj | getOptW optsBT = Var "startj"
>                               | otherwise      = Number 0
>                    in
>                      TLFor "j" startj (Var "n") [
>                        TLFor "i" (Var "j") (Number 0) 
>                           body
>                       ]
>             MTT  -> TLFor "j1" (Number 0) (Var "m") [
>                        TLFor "i1" (Var "j1") (Number 0) [
>                          TLFor "j2" (Number 0) (Var "n") [
>                            TLFor "i2" (Var "j2") (Number 0)
>                               body
>                       ]]]

>     -- Aufruf der Tabellenberechnungsfunktionen
>     functionCalls = concatMap ppCalc recs
>     ppCalc a | elem a recur = []
>              | otherwise    = case condition of
>                                 []        -> fcall
>                                 otherwise -> [TLIf (orCond condition) fcall []]
>        where

>        orCond [c]    = c
>        orCond (c:cs) = ExpIOp c "&&" (orCond cs)

>        condition = case getTMode tms a of
>           MST -> cc ri  (Var "i")  (Number 0) ++ cc rj  (Var "j")  (Var "n")
>           MTT -> cc ri1 (Var "i1") (Number 0) ++ cc rj1 (Var "j1") (Var "m") ++
>                  cc ri2 (Var "i2") (Number 0) ++ cc rj2 (Var "j2") (Var "n")
>           where
>             ST (ri, rj)            = getTRange ranges a
>             (ri1, rj1, ri2, rj2)   = getRangeTT ranges a 

>             cc True  v n = []
>             cc False v n = [ExpIOp (ExpME v) "==" (ExpME n)]

>        fcall | getOptBT optsBT == BTSubOptCut = [TLAssign (toVA difftbl) (tlnumber (-1))] ++ calcCall
>              | otherwise                      = calcCall
>          where
>            calcCall = [TLFA (pfct prefixes ++ a) dim]
>            (dim, indx) = case getTMode tms a of
>                     MST -> ([tlvar (Direct "i"), tlvar (Direct "j")],
>                            ST (Var "i", Var "j"))
>                     MTT -> ([tlvar (Direct "i1"), tlvar (Direct "j1"), tlvar (Direct "i2"), tlvar (Direct "j2")],
>                            TT (Var "i1", Var "j1") (Var "i2", Var "j2"))

>            difftbl = VANonterm a' (indexMap opts IMA ysizes imData a indx)
>               where a' = pdifftbl prefixes ++ a



>     -- Axiom-Ausgabe
>     ---------------------------------------------------------------------------------------------------------------------------------
>     printAxioms | getOptARR opts = [TLPrint ("\\\\documentclass[10pt]{article}\\n\\\\usepackage{lscape}\\n" ++ 
>                                              "\\\\begin{document}\\n\\\\landscape\\n{\\\\tiny\\n\\n") []] ++
>                                    body ++
>                                    [TLPrint "\\n}\\n\\\\end{document}\\n" []]
>                 | otherwise      = body
>       where body = concatMap printAxiom printedAxioms

>     showOnlyAxiom = getOptAR opts
>     printedAxioms | showOnlyAxiom  = [prAx (1,axiom)]
>                   | otherwise      = map prAx (zip [1,3..] (map fst3 il2))
>        where
>           prAx (n,rec) | elem rec recur = (n, rec, (Direct ("v" ++ show (n+1)),            ntType opts il2dt algebras rec))
>                        | otherwise      = (n, rec, (VANonterm (ptbl prefixes++rec) dimRec, ntType opts il2dt algebras rec))
>             where 
>               dimRec 
>                 -- falls alle Ergebnisse ausgegeben werden sollen, werden hier i und j eingesetzt:
>                 | getOptARR opts 
>                   = case getYSize ysizes rec of
>                      (ST (l,u))           -> indexMap opts IMA ysizes imData rec (ST (Var "i", Var "j"))
>                      (TT (l1,u1) (l2,u2)) -> indexMap opts IMA ysizes imData rec (TT (Var "i1", Var "j1") (Var "i2", Var "j2"))
>                 | otherwise 
>                   = case getYSize ysizes rec of
>                      (ST (l,u))           -> indexMap opts IMA ysizes imData rec (ST (Number 0, (Var "n")))
>                      (TT (l1,u1) (l2,u2)) -> indexMap opts IMA ysizes imData rec (TT (Number 0, (Var "m")) (Number 0, (Var "n")))

>     printAxiom :: (Int, String, VarAccess) -> [TL]
>     printAxiom (n,rec,ax) = [commentLn $ "show axiom: " ++ rec] ++ printRecName ++
>                             if not (getOptARR opts) 
>                                 -- falls nur Ergebnis (0,n):
>                                 then printAxiomSingle (n, rec, ax) 
>                                 -- ansonsten wird hier die Schleife zusammengebaut:
>                                 else [TLPrint "\\n\\\\begin{tabular}{c" [],
>                                       TLFor "i" (Number 0) (Var "n") [TLPrint "|c" []],
>                                       TLPrint "}\\n $_i\\\\backslash ^j$ & 0 " [],
>                                       TLFor "i" (Number 1) (Var "n") [TLPrint "&%d" [tlvar (Direct "i")]],
>                                       TLPrint "\\\\\\\\\\n\\\\hline\\n" [],
>                                       TLFor "i" (Number 0) (Var "n") [
>                                          TLPrint "%d" [tlvar (Direct "i")],
>                                          TLFor "j" (Number 0) (Var "n") (
>                                             [TLPrint "&" []] ++
>                                             [TLIf (ExpIOp (ExpTLVar (Direct "j")) "<" (ExpTLVar (Direct "i"))) 
>                                               [TLPrint "-" []]
>                                               [TLIf bounds (printAxiomSingle (n, rec, ax))
>                                                            [TLPrint "/" []]]]),
>                                          TLPrint "\\\\\\\\\\n" []],
>                                       TLPrint "\\\\end{tabular}\\n\\\\newpage\\n" []]

>       where
>         ysize  = getYSize ysizes rec
>         bounds = case ysize of
>                    (ST (l,u)) -> cgBounds (ST (Var "i", Var "j"), ysize)
>                    otherwise  -> error "4-dimensional table output currently not supported"
>         printRecName | showOnlyAxiom = []
>                      | otherwise     = [TLPrint ("Result for production " ++ rec ++ ":\\n") []]


>     -- ein einzelnes Axiom ausgeben; an dieser Stelle wird zwischen listen und atomaren Ergebnissen unterschieden:
>     printAxiomSingle (n,rec,ax) | isListStruct ax =
>                                     nontabAssign  ++ 
>                                     optionalOFH "[" ++
>                                     [TLAssign helper (TLVar ax),
>                                     TLWhileNN helper 
>                                       (printAxiomV (listItem helper) ++ 
>                                       [listStep helper] ++
>                                       seperator)] ++ 
>                                     optionalOFH "]\\n"
>                                 | otherwise = 
>                                     nontabAssign  ++ printAxiomV ax 
>      where
>        seperator = if getOptOF opts == OFHaskell then [TLIf (ExpIOp (ExpTLVar (fst helper)) "/=" ExpNil) [TLPrint "," []] []] 
>                                                  else [TLPrint "\\n" []]

>        helper = (Direct "result_score",  snd ax)
>        nontabAssign | elem rec recur               = [TLAssign (toVA (Direct "result_score")) (TLFA (pfct prefixes ++ rec) dim)]
>                     | elem (getOptBT optsBT) [BTSubOpt,BTSubOptCut]  
>                                                    = [TLAssign (toVA (Direct "result_score")) (TLVar ax)]
>                     | otherwise                    = []
>          where
>            dim | getOptARR opts 
>                  = case getTMode tms rec of
>                      MST -> [tlvar (Direct "i"), tlvar (Direct "j")]
>                      MTT -> error "4-dimensional table output currently not supported"
>                | otherwise 
>                   = case getTMode tms rec of
>                      MST -> [tlnumber 0, tlvar (Direct "n")]
>                      MTT -> [tlnumber 0, tlvar (Direct "m"), tlnumber 0, tlvar (Direct "n")]

>     optionalOFH  s = if getOptOF opts == OFHaskell then [TLPrint s []] else []
>     optionalOFHL s = if elem (getOptOF opts) [OFHaskell,OFHaskellLines] then [TLPrint s []] else []

>     printAxiomV :: VarAccess -> [TL]
>     printAxiomV v | getOptBT optsBT == BTSubOpt = [TLMacro "@OUTPUT_OPTIMAL@", TLMacro "@OUTPUT_SUBOPT_START@"]
>     printAxiomV v | isTupelStruct v = [TLPrint "(" []] ++  
>                                       mapsep [TLPrint "," []] printAxiomV (elemsStruct v) ++
>                                       [TLPrint ")" []]
>                   | isStruct v      = optionalOFHL "(" ++ 
>                                       mapsep seperator paS (zip (elemsStructNames v) (elemsStruct v)) ++
>                                       optionalOFHL ")" 
>                   | isEnumPointer v = [TLFA (ppp prefixes ++ getUserTypeName v) [TLVar v]]
>                   | otherwise       = if elem (getOptOF opts) [OFHaskell,OFHaskellLines] && snd v == (PointerOf TLChar) 
>                                       then [TLPrint ("\\\"" ++ formatTag v ++ "\\\"") [TLVar v]]
>                                       else [TLPrint           (formatTag v)         [TLVar v]]
>                   where

>                     seperator = case getOptOF opts of
>                                   OFHaskell      -> [TLPrint "," []]
>                                   OFHaskellLines -> [TLPrint "," []]
>                                   OFSpace        -> [TLPrint " " []]
>                                   OFLines        -> [TLPrint "\\n" []]

>                     paS (name, v) 
>                        | getOptOF opts == OFHaskell      = printAxiomV v 
>                        | getOptOF opts == OFHaskellLines = printAxiomV v 
>                        | getOptOF opts == OFSpace        = printAxiomV v 
>                        | getOptOF opts == OFLines        = [TLPrint (name ++ ": ") []] ++ printAxiomV v 
>                        | otherwise = error $ "output format " ++ show (getOptOF opts) ++ " not implemented"
>                     formatTag (_,dt) = head' [ fmt | (_,fmt,n) <- knownHaskellTypes, dt == n] ("formatTag: unknown type " ++ show dt)

>     -- Backtrace-Aufrufe
>     ---------------------------------------------------------------------------------------------------------------------------------

>     allocBT l | elem (getOptBT optsBT) [BTList, BTCompleteList, BTSubOpt]
>                = [TLAlloc MTStatic (Direct "pp_init", snd l)  (ExpME (Var "n" :* Number 2)) (PointerOf $ snd l),
>                   TLAlloc MTStatic (Direct "pp_initA", snd l) (ExpME (Var "n" :* Number 2)) (            snd l)]
>               | otherwise = []

>     codeBT l | getOptBT optsBT == BTNone   = []
>              | elem (getOptBT optsBT) [BTSingle, BTPF]
>                                            = [TLAssign l (TLFA (pback prefixes ++ axiom) dim),
>                                               TLFA (ppp prefixes ++ pstr prefixes ++ sigName) [TLVar l],
>                                               TLPrint "\\n" []]
>              | getOptBT optsBT == BTSubOptCut = [TLFA (pback prefixes ++ axiom) (dim ++ optionalDistVar)]
>              | elem (getOptBT optsBT) [BTList, BTCompleteList, BTSubOpt]
>                                            = [TLAssign (toVA (Direct "copy_depth")) (tlnumber 0)] ++ 
>                                              [TLAssign l (TLFA (pback prefixes ++ axiom) (dim ++ optionalDistVar)),
>                                               TLAssign (Direct "backtrace_tree", snd l) (TLVar l),
>                                               TLWhile (ExpIOp (ExpIOp (ExpTLVar (fst l))            "/=" ExpNil) "||"
>                                                               (ExpIOp (ExpTLVar (Direct "pp_next")) "/=" ExpNil))
>                                                 ([TLAssign (toVA $ Direct "pp_next") TLNil,
>                                                   TLAssign (toVA (Direct "pp_initC")) (tlnumber (-1))] ++
>                                                   optionalOutputReset ++
>                                                   [optionalScoreAssign (TLFA (ppp prefixes ++ pstr prefixes ++ sigName) [TLVar l])] ++
>                                                   outputSubOpt ++
>                                                   [TLIf (ExpIOp (ExpTLVar (Direct "pp_next")) "/=" ExpNil) 
>                                                      -- then
>                                                      [TLFA (pbuild prefixes ++ pstr prefixes ++ sigName) 
>                                                           [TLVar $ listNext (toVA (Pointer (Direct "pp_next")))],
>                                                       TLAssign (toVA (Direct "next")) (TLVar $ listNext (toVA (Pointer (Direct "pp_next")))),
>                                                       TLIf (ExpIOp (ExpTLVar (Direct "removeAddr")) "/=" ExpNil)
>                                                          -- then
>                                                          [TLFA ("free_" ++ pstr prefixes ++ sigName) 
>                                                             [TLExp (ExpNum 0), TLVar (toVA (Pointer (Direct "removeAddr")))]]
>                                                          -- else 
>                                                          [],
>                                                       TLAssign (toVA (Pointer (Direct "pp_next"))) (TLVar (toVA (Direct "next"))),
>                                                       TLIf (ExpIOp (ExpTLVar (Direct "pp_initC")) "/=" (ExpNum (-1))) 
>                                                          -- then     
>                                                          [TLFor "i" (Number 0) (Var "pp_initC")
>                                                             [TLAssign (toVA $ Pointer $ ArrayElem [Var "i"] $ Direct "pp_init")
>                                                                       (tlvar           $ ArrayElem [Var "i"] $ Direct "pp_initA")]]
>                                                          -- else
>                                                          []
>                                                      ] 
>                                                      -- else
>                                                      [TLAssign (toVA (Direct "next")) (TLVar (listNext (toVA (Direct "l")))),
>                                                       TLIf (ExpTLVar (fst (listNext l))) 
>                                                           [TLFA (pbuild prefixes ++ pstr prefixes ++ sigName) [TLVar (listNext l)]]
>                                                           [],
>                                                       TLFA ("free_" ++ pstr prefixes ++ sigName) 
>                                                          [TLExp (ExpNum 0), TLVar (toVA (Direct "l"))],
>                                                       TLAssign (toVA (Direct "l")) (TLVar (toVA (Direct "next")))]
>                                                   {- TLPrint "\\n" [] -} ]),
>                                               TLMacro "@OUTPUT_SUBOPT_END@"]
>              -- alter Teil mit richtigem Stack; wird nicht mehr verwendet:
>              | otherwise                   = [TLComment ["allocate stack space"],
>                                               TLAlloc MTStatic (Direct "stack", PointerOf restype) (ExpME (Var "n" :+ Var "n")) (PointerOf restype),
>                                               TLAssign l (TLFA (pback prefixes ++ axiom) dim),
>                                               TLWhileNN l 
>                                                 [initStack, pushStack (tlvar (Address resname)),
>                                                 TLFA (ppp prefixes ++ pstr prefixes ++ sigName) [TLVar l],
>                                                 TLPrint "\\n" []]]
>        where
>           resname = fst l
>           restype = snd l
>           dim = case getTMode tms axiom of
>                     MST -> [tlnumber 0, tlvar (Direct "n")]
>                     MTT -> [tlnumber 0, tlvar (Direct "m"), tlnumber 0, tlvar (Direct "n")]
>           optionalDistVar       | getOptBT optsBT == BTSubOpt    = [tlvar $ Direct "traceback_diff"]
>                                 | getOptBT optsBT == BTSubOptCut = [tlvar $ Direct "traceback_diff"] 
>                                 | otherwise                   = []
>           optionalOutputReset   | getOptBT optsBT == BTSubOpt = [TLAssign (toVA $ ArrayElem [Number 0] (Direct "result_prettyprint")) (tlnumber 0),
>                                                                  TLAssign (toVA $ Direct "pp_outp") (tlvar $ Direct "result_prettyprint"),
>                                                                  TLAssign (toVA $ Direct "rmAllowed") (tlnumber 1),
>                                                                  TLAssign (toVA $ Direct "removeAddr") TLNil]

>                                 | otherwise                   = []
>           optionalScoreAssign p | getOptBT optsBT == BTSubOpt = TLAssign (toVA (Direct "score")) p
>                                 | getOptBT optsBT == BTPF     = TLAssign (toVA (Direct "score")) p
>                                 | otherwise                   = p
>           outputSubOpt          | elem (getOptBT optsBT) [BTSubOpt, BTPF] 
>                                                               = [TLIf diffCond
>                                                                    [TLMacro "@OUTPUT_SUBOPT@"]
>                                                                    []]
>                                 | otherwise                   = []
>                where
>                  diffCond = (ExpPOp "is_suboptimal" 
>                    [ExpTLVar $ Direct "result_score",
>                     ExpTLVar $ Direct "score",
>                     ExpTLVar $ Direct "traceback_diff"
>                    ])

------------------------------------------------------------------------------------------------------------------
------ Forward-Declarationen der nichttabellierten Produktionen
------------------------------------------------------------------------------------------------------------------

>   cgForward ps = concatMap functionDef ps
>     where
>       functionDef n | elem n recur = [TLFD [] functionRes (pfct prefixes ++ n) args [] [] (tlvar (Direct "_"))]
>                     | otherwise    = []
>         where
>           functionRes  = ntType opts il2dt algebras n
>           args         = case getTMode tms n of
>                            MST -> [(["i"],TLInt),(["j"],TLInt)]
>                            MTT -> [(["i1"],TLInt),(["j1"],TLInt),(["i2"],TLInt),(["j2"],TLInt)]


------------------------------------------------------------------------------------------------------------------
------ Forward-Declarationen fuer die Backtrace-Funktionen
------------------------------------------------------------------------------------------------------------------

>   cgForwardBT ps | getOptBT optsBT == BTNone = []
>                  | otherwise                 = concatMap functionDef ps
>     where
>       functionDef n = [TLFD [] functionRes (pback prefixes ++ n) args [] [] (tlvar (Direct "_"))]
>         where
>           functionRes  = btWorkDataType optsBT sigName il2dtBT algfsBT n 
>           args         = let diffArg | getOptBT optsBT == BTSubOpt    = [(["diff"], soDiffDatatype)]
>                                      | getOptBT optsBT == BTSubOptCut = [(["diff"], soDiffDatatype)]
>                                      | otherwise                      = []
>                          in case getTMode tms n of
>                            MST -> [(["i"],TLInt),(["j"],TLInt)]  ++ diffArg
>                            MTT -> [(["i1"],TLInt),(["j1"],TLInt),(["i2"],TLInt),(["j2"],TLInt)] ++ diffArg

------------------------------------------------------------------------------------------------------------------
------ Hilfsfunktionen fuer spezielle Auswahlfunktionen
------------------------------------------------------------------------------------------------------------------

>   cgSpecialChoice :: [SpecialChoiceUse] -> [TL]
>   cgSpecialChoice specialChoice = map cgSpec specialChoice
>     where
>       cgSpec (kind, name, dt) = TLFD [] resType name args [(["result"], resType)] body (tlvar (Direct "result"))
>         where
>         resType | kind == SpecialNub                         = TLChar
>                 | elem kind [SpecialSortUp, SpecialSortDown] = TLInt
>                 | otherwise                                  = error $ "cgSpecialChoice: unsupported: " ++ show kind

>         args    | elem kind [SpecialNub, SpecialSortUp, SpecialSortDown] = [(["p"],PointerOf TLVoid),(["q"],PointerOf TLVoid)]
>                 | otherwise                                              = error $ "cgSpecialChoice: unsupported: " ++ show kind

>         body | kind == SpecialNub &&
>                dt   == (PointerOf TLChar)  = [TLAssign   (toVA $ Direct "result") (TLExp $ ExpPOp "!strcmp" [casted "p",casted "q"])]
>              | kind == SpecialNub          = [TLAssign   (toVA $ Direct "result") (TLExp $ ExpIOp (casted "p") "==" (casted "q"))]

>              | kind == SpecialSortUp && 
>                dt   == (PointerOf TLChar)  = [TLAssign   (toVA $ Direct "result") (TLExp $ ExpPOp "strcmp" [casted "p",casted "q"])]
>              | kind == SpecialSortDown && 
>                dt   == (PointerOf TLChar)  = [TLAssign   (toVA $ Direct "result") (TLExp $ ExpPOp "strcmp" [casted "q",casted "p"])]

>              | elem kind [SpecialSortUp, SpecialSortDown] 
>                    = let op = if kind == SpecialSortUp then ">" else "<" 
>                      in [TLAssignIF (toVA $ Direct "result") (ExpIOp (casted "p") op (casted "q")) (tlnumber 1) (tlnumber (-1))]
>              | otherwise                                              = error $ "cgSpecialChoice: unsupported: " ++ show kind

>         casted v = ExpTLVar $ Pointer $ Cast (PointerOf dt) (Direct v)

------------------------------------------------------------------------------------------------------------------
------ Aufsammeln der generierten Strukturen
------------------------------------------------------------------------------------------------------------------

>   collectDataStructures :: (Int, Int, [(String, [(String, DataType)])]) -> [TL] -> ((Int, Int, [(String, [(String, DataType)])]), [TL])
>   collectDataStructures = collectTLs cdsWorker

>   cdsWorker :: WorkersTL (Int, Int, [(String,[(String,DataType)])])
>   cdsWorker = makeWorkersTL cdsWorker (NOP, NOP, NOP, NOP, NOP, DO wrkDataType, NOP) where

>     wrkDataType (n, m, dts) (StructOf na []) = ((n, m, dts), StructOf na [])
>     wrkDataType (n, m, dts) foo@(StructOf _ strct) | found == [] = ((left, right, (sName, strct): dts'), StructOf sName [])
>                                             | otherwise   = ((n', m', dts'), StructOf (fst (head (found))) [])
>                                where
>                                  found :: [(String, [(String, DataType)])]
>                                  found               = [ (name, s) | (name, s) <- dts', s == strct]
>                                  (sName, left, right) | isTupel foo = ("tupel" ++ show (m'+1), n', m'+1)
>                                                       | otherwise  = (ptstr prefixes ++ show (n'+1), n'+1, m')
>                                  ((n', m', dts'), struct') = cdsds (n,m,dts) strct

>                                  cdsds (n, m, dts) []           = ((n, m, dts), [])
>                                  cdsds (n, m, dts) ((na,dt):ds) = ((n'', m'', dts''), ((na, dt'): ds'))
>                                   where
>                                     ((n',  m', dts'),  dt') = wrkDataType (n, m, dts)   dt
>                                     ((n'', m'', dts''), ds') = cdsds       (n', m', dts') ds
>     wrkDataType cll x = collectDataType cdsWorker cll x


>   mergeDataStructures = cdsToTypeDecl.mergeStruct.reverse.thd3

>   -- Konvertierung der aufgesammelten dts-Liste in eine TypeDecl:
>   --------------------------------------------------------------------------------------------------------------
>   cdsToTypeDecl dts = [TLTypeDecls (map toTD dts)] 
>     where 
>       toTD (name, [("next", _), ("last", _), ("item", dt)]) = StructDecl name 
>                                                              [(["next"], PointerOf (StructOf name [])), 
>                                                               (["last"], PointerOf (StructOf name [])),
>                                                               (["item"], dt)]
>       toTD (name, vds) = StructDecl name (map (\(n,d) -> ([n],d)) vds)

>   -- Dieses ist notwendig, um verschachtelte Strukturen durch neue Strukturnamen zu ersetzen:
>   --------------------------------------------------------------------------------------------------------------
>   mergeStruct :: [(String, [(String, DataType)])] -> [(String, [(String, DataType)])]
>   mergeStruct dts = map ms dts
>     where
>     ms  (n, strct) = (n, (map ms' strct))
>     ms' (n,(StructOf name [])) = (n,StructOf name [])
>     ms' (n,(StructOf name dt)) | found == [] = (n,(StructOf name dt)) -- error $ "mergeStruct: unknown structure " ++ show dt
>                                | otherwise   = (n,StructOf (fst (head found)) [])
>                               where
>                                  found = [ (name, s) | (name, s) <- dts, s == dt]
>     ms' (n, x) = (n, x)


------------------------------------------------------------------------------------------------------------------
------ suppress comments in target code
------------------------------------------------------------------------------------------------------------------

>   commentWorker :: WorkersTL Int
>   commentWorker = makeWorkersTL commentWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where
>     wrkTL cll (TLComment c)   = (cll, TLComment [])
>     wrkTL cll (TLLayout "\n") = (cll, TLLayout "")
>     wrkTL cll (TLFD _ a b c d tls g) = (cll', TLFD [] a b c d tls' g)
>        where (cll', tls')   = collectTLs commentWorker cll tls
>     wrkTL cll x             = collectTL  commentWorker cll x


------------------------------------------------------------------------------------------------------------------
------ Tabellenzugriffs-Makros ersetzen
------------------------------------------------------------------------------------------------------------------

>   macroWorker :: WorkersTL [(DataType, String, [String], [TL])]
>   macroWorker = makeWorkersTL macroWorker (DO wrkTL, NOP, NOP, NOP, DO wrkVAccess, NOP, NOP) where

>     wrkTL defs (TLDefines d) = (d ++ defs, TLDefines d') 
>        where 
>          d' = filter isTableMacro d
>          isTableMacro (_,n, _, _) = (ptbl prefixes) /= take (length (ptbl prefixes)) n

>     wrkTL defs tl                = collectTL macroWorker defs tl

>     wrkVAccess defs (VANonterm n ss) = (defs, VANonterm nTab newss)
>                                      where
>                                         (args, va)  = head' [ (args,va) | (_, name, args, [TLVar (va, _)]) <- defs, name == n] 
>                                                             $ "no #define for nonterminal " ++ n
>                                         bindArgs = zip args (index2List ss)
>                                         (VANonterm nTab ssTab) = va
>                                         newss = list2Index $ map (updss bindArgs) (index2List ssTab)

>                                         updss ba (Var v)    | head v == '(' = 
>                                                                 let var = takeWhile (/= ')') (tail v)
>                                                                 in head' [ newv | (name, newv) <- ba, name == var] "updss: error"
>                                                             | otherwise = Var v
>                                         updss ba x          = mapMathExp (updss ba) x

>     wrkVAccess defs va               = collectVAccess macroWorker defs va

------------------------------------------------------------------------------------------------------------------
------ Ueberfluessige Konstuktor-Funktionen entfernen
------------------------------------------------------------------------------------------------------------------

>   removeObsoleteFcts :: [TL] -> [TL]
>   removeObsoleteFcts tls = phase2
>     where

>     usedFcts = fst $ collectTLs collectWorker [] tls
>     phase2   = snd $ collectTLs removeWorker usedFcts tls

>     collectWorker :: WorkersTL [String]
>     collectWorker = makeWorkersTL collectWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where

>       wrkTL cll fa@(TLFA name args)  | isPrefix (pnew prefixes) name = (name:cll, fa)
>                                      | otherwise                     = (cll, fa)
>       wrkTL cll fa@(TLAssign _ (TLExp (ExpPOp name _)))
>                                      | isPrefix (pnew prefixes) name = (name:cll, fa)
>                                      | otherwise                     = (cll, fa)
>       wrkTL cll tl                   = collectTL collectWorker cll tl

>     removeWorker :: WorkersTL [String]
>     removeWorker = makeWorkersTL removeWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where

>       wrkTL cll fd@(TLFD _ _ name _ _ _ _) 
>                                      | isPrefix (pnew prefixes) name && not (elem name cll) = (cll, TLBlock [])
>                                      | otherwise                                            = (cll, fd)
>       wrkTL cll tl                   = collectTL removeWorker cll tl

------------------------------------------------------------------------------------------------------------------
------ Inline functions
------------------------------------------------------------------------------------------------------------------
TODO
----
- mainloop in main inlinen, _alle_ Variablen lokal in main
- probleme falls gleiche namen bei unterschiedlichen typen
- parameteruebergabe passt noch nicht
- ueberpruefen, ob mainloop ueberhaupt ingelined wurde, falls nicht ausgabe ans ende

>   inlineFunctions :: [TL] -> [TL]
>   inlineFunctions tls = snd $ collectTLs inlineFDWorker ([], "main", []) tls
>     where

>     inlineFDWorker :: WorkersTL ([TL], String, [(String, [VarDecl])])
>     inlineFDWorker = makeWorkersTL inlineFDWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where

>       wrkTL (decls, curFD, locDecls) fd@(TLFD cmt a name b vds tls d) | checkInline fd = ((newFD:decls, curFD, locDecls''), TLLayout [])
>                                                                       | otherwise = (cll, newFD)
>           where
>             (cll@(_,_,locDecls'), tls') = collectTLs inlineFDWorker (decls, name, locDecls) tls
>             newDecls    = concat (lookupA locDecls' name) ++ vds
>             newFD       = TLFD cmt a name b (mergeDecls newDecls) tls' d
>             locDecls''  = replaceA locDecls' name newDecls

>       wrkTL (decls, curFD, locDecls) (TLDecls vardecls)             = ((decls, curFD, locDecls'), TLLayout [])
>           where
>             locDecls' = appendA locDecls curFD vardecls

>       wrkTL (decls, curFD, locDecls) fa@(TLFA name args)   | decl == [] = ((decls, curFD, locDecls),  fa)
>                                                            | otherwise  = ((decls, curFD, locDecls'), 
>                                                                            TLBlock $ [longCommentToBox cmt] ++ body)
>           where
>             decl                             = [ decl | decl@(TLFD _ _ name' _ _ _ _) <- decls, name'== name]
>             [TLFD cmt _ _ _ vardecls body _] = decl
>             locDecls' = appendA locDecls curFD vardecls

>       wrkTL cll@(decls, curFD, locDecls) la@(TLLayout l) | not $ stringContains "mainloop" l = (cll, la)
>                                                          | otherwise = (cll, TLBlock main')
>           where
>             main  = lines l
>             main' = procMain 1 [] main
>             procMain n cll []     = outputCll cll
>             procMain n cll (l:ls) | n == declLine               = outputCll cll ++ 
>                                                                   [tlLongComment "collected variables"] ++ [TLLayout "\n"] ++ 
>                                                                   [TLDecls locDecls'] ++ [TLLayout "\n"] ++ 
>                                                                   procMain (n+1) [] (l:ls)
>                                   | stringContains "mainloop" l = outputCll cll ++ 
>                                                                   body   ++ 
>                                                                   procMain (n+1) [] ls
>                                   | otherwise                   = procMain (n+1) (l:cll) ls
>                                   
>             outputCll cll = [TLLayout $ unlines (reverse cll)]
>             declLine | tl == C       = 3
>                      | tl == F       = 1
>                      | tl == Pascal  = 2
>                      | otherwise     = 1

>             locDecls' =  mergeDecls (concat ((lookupA locDecls "main") ++ (lookupA locDecls "mainloop")))
>             [TLFD _ _ _ _ _ body _] = [ decl | decl@(TLFD _ _ name' _ _ _ _) <- decls, name'== "mainloop"]

>       wrkTL (decls, curFD, locDecls) tl                     = collectTL inlineFDWorker (decls, curFD, locDecls) tl


>     checkInline (TLFD _ rt name _ _ body _) = rt==TLVoid && body /= [] 
>     stringContains s = any (s `isPrefixOf`) . tails

>     mergeDecls  ds = map (\(dt, vl) -> (nub vl, dt)) $ mergeDecls' [] ds
>     mergeDecls' cll []     = cll
>     mergeDecls' cll ((vs,dt):ds)            = mergeDecls' (appendA cll dt vs) ds


------------------------------------------------------------------------------------------------------------------
------ Clean up
------------------------------------------------------------------------------------------------------------------

>   cleanUpTL :: [TL] -> [TL]
>   cleanUpTL tls = phase2 
>     where

>     phase1 = snd $ wrkTLs cleanUpWorker1 0 tls
>     phase2 = snd $ wrkTLs cleanUpWorker2 0 phase1

>     cleanUpWorker1 :: WorkersTL Int
>     cleanUpWorker1 = makeWorkersTL cleanUpWorker1 (NOP, DO wrkTLs, NOP, NOP, NOP, NOP, NOP) where
>       wrkTLs cll []                                = (cll, [])
>       wrkTLs cll ((TLLayout ""):  tls)             = wrkTLs cll tls
>       wrkTLs cll ((TLComment []): tls)             = wrkTLs cll tls
>       wrkTLs cll ((TLDefines []): tls)             = wrkTLs cll tls
>       wrkTLs cll ((TLComment x):(TLComment y):tls) = (cll', (TLComment (x++y)):tls') where (cll', tls') = wrkTLs cll tls
>       wrkTLs cll (tl:tls)                          = (cll'', tl': tls')
>         where
>           (cll',  tl')  = collectTL  cleanUpWorker1 cll  tl
>           (cll'', tls') = wrkTLs     cll' tls

>     cleanUpWorker2 :: WorkersTL Int
>     cleanUpWorker2 = makeWorkersTL cleanUpWorker2 (NOP, DO wrkTLs, NOP, NOP, NOP, NOP, NOP) where
>       wrkTLs cll []                                = (cll, [])
>       wrkTLs cll ((TLComment [_,_]):(TLComment x):tls) = wrkTLs cll ((TLComment x):tls)
>       wrkTLs cll (tl:tls)                          = (cll'', tl': tls')
>         where
>           (cll',  tl')  = collectTL  cleanUpWorker2 cll  tl
>           (cll'', tls') = wrkTLs     cll' tls


------------------------------------------------------------------------------------------------------------------
------ Target code optimization (if reduction)
------------------------------------------------------------------------------------------------------------------

>   optimizeTL1 :: [TL] -> [TL]
>   optimizeTL1 tls = snd $ collectTLs optBoundWorker ([],[]) tls

>   optBoundWorker :: WorkersTL ([Exp],[(String, MathExp)])
>   optBoundWorker = makeWorkersTL optBoundWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where
>     wrkTL cll@(oBnd,assigns) (TLIf bnd codeThen codeElse) 
>          | isSatisfied bnd oBnd = wrkTL cll (TLBlock codeThen)
>          | otherwise            = (cll, TLIf bnd codeThen' codeElse')
>          where
>            (_, codeThen') = collectTLs optBoundWorker ((bnd' ++ oBnd),assigns) codeThen
>                           where
>                             bnd' = case bnd of
>                                      (ExpIOp bnd1 "&&" bnd2) -> [bnd1, bnd2]
>                                      otherwise               -> [bnd]
>            (_, codeElse') = collectTLs optBoundWorker  cll                 codeElse

>            isSatisfied iBnd oBnds = any (isSat iBnd) oBnds
>            isSat iBnd oBnd = isSat' (normalize oBnd) (normalize iBnd)
>              where
>               isSat' (ExpIOp (ExpME v1) ">=" (ExpME (Number n1)))  -- outer Bound
>                      (ExpIOp (ExpME v2) ">=" (ExpME (Number n2)))  -- inner Bound
>                      = (v1 == v2 && n2 <= n1)  ||                                -- Abfrage wurde schon durch aeussere Abfrage erfuellt
>                        (calcME (insertVarBindsME assigns 
>                             (calcME (insertVarBindsME assigns v2))) 
>                             `geq` (Number n2))  -- Durch Bindung der Variablen an die Schleifenstartvariablen
>                                                 -- wird die Abfrage ueberfluessig
>               isSat' _ _ = False
>               geq (Number a) (Number b) = a >= b
>               geq _          _          = False

>            normalize (ExpIOp (ExpME (v1 :- (Number n1))) ">=" (ExpME (Number n2))) = ExpIOp (ExpME v1) ">=" (ExpME (Number (n1+n2)))
>            normalize x                                                             = x

>     wrkTL cll@(bnd, assigns) (TLFor v begin end tls) = (cll, TLFor v begin end tls')
>          where
>            (_, tls') = collectTLs optBoundWorker (bnd, a1 ++ a2 ++ assigns) tls
>            a1 = [(v, begin)]
>            a2 = case end of
>                       (Var jj)                 -> [(jj, Var v)]   -- jj kann auch eine andere loop-variable sein
>                       ((Var jj) :- (Number n)) -> [(jj, Var v :+ (Number n))]
>                       otherwise                -> []

>     wrkTL cll x           = collectTL  optBoundWorker cll x


------------------------------------------------------------------------------------------------------------------
------ Target code optimization (eliminate redundant expressions)
------------------------------------------------------------------------------------------------------------------

>   optimizeTL2 :: [TL] -> [TL]
>   optimizeTL2 tls = snd $ collectTLs optTLFDWorker [] tls


>   optTLFDWorker :: WorkersTL [a]
>   optTLFDWorker = makeWorkersTL optTLFDWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where

>     wrkTL cll (TLFD cmt retType name params locVars body retExpr) = (cll', (TLFD cmt retType name params locVars' body'' retExpr))
>          where cll'      = cll
>                locVars'  = if length usedLocVars == 0 then locVars else locVars ++ newLocVars
>                newLocVars = [(map (\n -> 'o':(show n)) (fst $ unzip usedLocVars), TLInt)]
>                usedLocVars = nub replacedLocVars
>                (cllExpr,_) = collectSubExpr body
>                replaceList = zip [1..] $ nub $ snd $ unzip $ filter (\(n,exp) -> n >= 2) cllExpr
>                ((_,replacedLocVars),body') = replaceSubExpr (replaceList,[]) body
>                body'' = (map (\(n,expr) -> TLAssign (Direct $ 'o':show n, TLInt) (TLExp expr)) usedLocVars) ++ body'

>     wrkTL cll x          = collectTL optTLFDWorker cll x


Sammelt alle in dem Codeblock einer Fubnktionsdefinition vorkommenden Ausdruecke.
Die gefundenen Ausdruecke werden in eine Liste von Tupeln einsortiert, in der
das Vorkommen eines Ausdrucks im Programmblock gezaehlt wird. Der TL-Ausdruck
wird von dem Collector nicht veraendert.

>   collectSubExpr = collectTLs collectSubExprWorker []

>   collectSubExprWorker :: WorkersTL [(Int, Exp)]
>   collectSubExprWorker = makeWorkersTL collectSubExprWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where

>     wrkTL cll tl@(TLAssign varAccess (TLExp expr)) = (cll', tl)
>          where cll' = addCollector cll expr

>     wrkTL cll tl@(TLFA fnctName params) = (cll',tl)
>          where (cll',_) = collectTLs collectSubExprWorker cll params

>     wrkTL cll tl@(TLIf expr tl1 tl2) = (addCollector cll'' expr, tl)
>          where (cll',tl1') = collectTLs collectSubExprWorker cll tl1
>                (cll'',tl2') = collectTLs collectSubExprWorker cll' tl2

>     wrkTL cll tl@(TLFor varName me1 me2 tls) = (cll',tl)
>          where (cll',_) = collectTLs collectSubExprWorker cll tls

>     wrkTL cll x          = collectTL collectSubExprWorker cll x

>     addCollector cll e = insertAllExps cll $ filter filterK $ splitExp e
>--     addCollector cll e = insertAllExps cll $ splitExp e
>                 where insertExp :: [(Int,Exp)] -> Exp -> [(Int,Exp)]
>                       insertExp [] exp = [(1,exp)]
>                       insertExp ((n,e):es) exp = if exp == e then (n+1,e):es else (n,e):insertExp es exp

>                       splitExp :: Exp -> [Exp]
>                       splitExp exp@(ExpPOp opName es) = exp : concatMap splitExp es
>                       splitExp exp@(ExpIOp e1 opName e2) = exp : splitExp e1 ++ splitExp e2
>                       splitExp exp@(ExpIf e1 e2 e3) = exp : splitExp e1 ++ splitExp e2 ++ splitExp e3
>--                       splitExp exp@(ExpTLVar (VANonterm name (ST (me1,me2)))) = []
>                       splitExp e = if isAtomicExpr e then [] else [e]

>                       insertAllExps :: [(Int,Exp)] -> [Exp] -> [(Int,Exp)]
>                       insertAllExps cll [] = cll
>                       insertAllExps cll (e:es) = let cll' = insertAllExps cll es in insertExp cll' e

>                       filterK :: Exp -> Bool
>                       filterK (ExpPOp opName es) = all filterK es
>                       filterK (ExpIOp e1 opName e2) = filterK e1 && filterK e2
>                       filterK (ExpIf e1 e2 e3) = filterK e1 && filterK e2 && filterK e3
>                       filterK (ExpVar name) = nameContainsK name
>                       filterK (ExpME me) = filterMeK me
>                       filterK (ExpTLVar (VANonterm name (ST (me1,me2)))) = filterMeK me1 && filterMeK me2
>                       filterK x = False

>                       filterMeK :: MathExp -> Bool
>                       filterMeK (Var name) = nameContainsK name
>                       filterMeK (me1 :- me2) = filterMeK me1 && filterMeK me2
>                       filterMeK (me1 :+ me2) = filterMeK me1 && filterMeK me2
>                       filterMeK (me1 :* me2) = filterMeK me1 && filterMeK me2
>                       filterMeK (me1 :/ me2) = filterMeK me1 && filterMeK me2
>                       filterMeK (Min me1 me2) = filterMeK me1 && filterMeK me2
>                       filterMeK (Max me1 me2) = filterMeK me1 && filterMeK me2
>                       filterMeK x = True

>                       nameContainsK :: String -> Bool
>                       nameContainsK name = case name of
>                                            "k" -> False
>                                            ('k':num) -> if all isDigit num then False else True
>                                            ('l':'c':'_':_) -> False
>                                            otherwise -> True


Ersetzt die gefundenen Unterausdruecke im Programmcode durch variablen

>   replaceSubExpr :: ([(Int,Exp)],[(Int,Exp)]) -> [TL] -> (([(Int,Exp)],[(Int,Exp)]),[TL])
>   replaceSubExpr = collectTLs replaceSubExprWorker

>   replaceSubExprWorker :: WorkersTL ([(Int,Exp)],[(Int,Exp)])
>   replaceSubExprWorker = makeWorkersTL replaceSubExprWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where

>     wrkTL cll tl@(TLAssign varAccess (TLExp e)) = let (cll',expr') = replaceExp cll e
>                                                   in (cll', TLAssign varAccess (TLExp expr'))

>     wrkTL cll tl@(TLFA fnctName params) = (cll',TLFA fnctName params')
>          where (cll',params') = replaceSubExpr cll params

>     wrkTL cll tl@(TLIf expr tl1 tl2) = (cll''', TLIf expr' tl1' tl2')
>          where (cll',expr') = replaceExp cll expr
>                (cll'',tl1') = replaceSubExpr cll' tl1
>                (cll''',tl2') = replaceSubExpr cll'' tl2

>     wrkTL cll tl@(TLFor varName me1 me2 tls) = (cll',TLFor varName me1 me2 tls')
>          where (cll',tls') = replaceSubExpr cll tls

>     wrkTL cll x          = collectTL replaceSubExprWorker cll x

>     replaceExp :: ([(Int,Exp)],[(Int,Exp)]) -> Exp -> (([(Int,Exp)],[(Int,Exp)]),Exp)
>     replaceExp cll@(c1,c2) exp = if isInList then ((c1,eee:c2),ExpVar $ "o" ++ show varNum)
>                                              else case exp of
>                                                   (ExpPOp opName es) -> let (cll',es') = mapReplaceExp cll es
>                                                                         in (cll',ExpPOp opName es')
>                                                   (ExpIOp e1 opName e2) -> let (cll',e1') = replaceExp cll e1
>                                                                                (cll'',e2') = replaceExp cll' e2
>                                                                            in (cll'',ExpIOp e1' opName e2')
>                                                   (ExpIf e1 e2 e3) -> let (cll',e1') = replaceExp cll e1
>                                                                           (cll'',e2') = replaceExp cll' e2
>                                                                           (cll''',e3') = replaceExp cll'' e3
>                                                                       in (cll''',ExpIf e1' e2' e3')
>                                                   x            -> (cll,x)
>                                  where foundExp = findExp (fst cll) exp
>                                        isInList = case foundExp of (Just res) -> True; Nothing -> False
>                                        (Just eee@(varNum,_)) = foundExp
>     mapReplaceExp :: ([(Int,Exp)],[(Int,Exp)]) -> [Exp] -> (([(Int,Exp)],[(Int,Exp)]),[Exp])
>     mapReplaceExp cll [] = (cll,[])
>     mapReplaceExp cll (e:es) = let (cll',e') = replaceExp cll e
>                                    (cll'',es') = mapReplaceExp cll' es
>                                in (cll'',e':es')
>     findExp :: [(Int,Exp)] -> Exp -> Maybe (Int,Exp)
>     findExp [] exp = Nothing
>     findExp ((n,e):es) exp = if e == exp then Just (n,e) else findExp es exp


Prueft, ob ein Ausdruck atomar ist

> isAtomicExpr :: Exp -> Bool
> isAtomicExpr (ExpVar _) = True
> isAtomicExpr (ExpME me) = atomarME me
> isAtomicExpr _          = False


