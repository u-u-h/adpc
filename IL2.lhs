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



> module IL2(

>   CalcUnit(..),
>   Choice(..),
>   IL2(..),
>   IL2Type,
>   collectCUVars,
>   deriveIL2Type,
>   deriveIL2Types,
>   expToChoice,
>   exp_choiceSum,
>   getCUAssigns,
>   getCUIfs,
>   getCULoops,
>   il2Type,
>   isAtomar,
>   isCUAssign,
>   isCUIf,
>   isCULoop,
>   newLine,
>   noResult,
>   ntType,
>   ph1IL2,
>   ph1IL2Prod,
>   ph1IL2Prods,
>   ph2IL2,
>   ph2IL2Prod,
>   ph2IL2Prods,
>   ppChoice,
>   ppIL2,
>   ppIL2Prod,
>   ppIL2Prodstex,
>   ppIL2Type,
>   rev_IL2,

> ) where

> import Data.Char
> import Data.List
> import Tools
> import Constants
> import MathExp
> import StringLib
> import Syntax
> import TLData
> import Structs
> import Dss
> import Expr
> import PrettyPrint

> rev_IL2 =  "$Revision$"

> type IL2Prod = (String, Int, IL2)
> data IL2 =
>   --               bounds   if    dt       ausdruck
>   IL2Terminal      ILBounds [Exp] DataType Exp                  | 
>   IL2Nonterminal   String  SubScripts                           |
>   IL2Alt           [IL2]                                        |
>   IL2Choice        [AlgDef] IL2                                 |
>   IL2Filter        Exp   IL2                                    |
>   IL2Calc  String ILBounds   [CalcUnit] [(String,[ComplexArg], DataType, AlgContext, Exp)]   |  -- von ph2IL2 erzeugt
>        -- comment                        algname
>   IL2Calc2 String ILBounds   [CalcUnit] [(String,[ComplexArg2],DataType,Exp)]   |  -- von ph1IL2 erzeugt
>        -- comment                        algname            
>   IL2TTUnit  IL2 IL2  | 
>   IL2Exp  Exp    -- helper for list comprehensions

>                                                                                   deriving (Eq, Show)

> type AlgContext = [([(String, [String])],Exp)]

> data CalcUnit =
>       CULoop   ILLoop |
>       CUIf     Exp    |
>       CUAssign (VAccess, Exp) 

>                            deriving (Eq, Show)

> collectCUVars :: [CalcUnit] -> [String]
> collectCUVars cUnits = concatMap coll cUnits
>   where
>     coll (CULoop (v, _, _))       = [v]
>     coll (CUIf _          )       = []
>     coll (CUAssign (Direct v, _)) = [v]

> isCULoop (CULoop _)     = True
> isCULoop _              = False

> isCUIf (CUIf _)         = True
> isCUIf _                = False

> isCUAssign (CUAssign _) = True
> isCUAssign _            = False

> getCULoops cUnits = (map (\(CULoop l) -> l) loops, rest)
>     where (loops, rest) = span isCULoop cUnits

> getCUIfs cUnits = (map (\(CUIf l) -> l) ifs, rest)
>     where (ifs, rest) = span isCUIf cUnits

> getCUAssigns cUnits = (map (\(CUAssign l) -> l) assigns, rest)
>     where (assigns, rest) = span isCUAssign cUnits


> ppCalcUnits ind [] = ""
> ppCalcUnits ind ((CULoop (lv, begin, end)):units) = 
>     spc ind ++ "for " ++ lv ++ " = " ++ pretty begin ++ " to " ++ pretty end ++ " do\n"  ++ ppCalcUnits (ind + ilStdInd) units
> ppCalcUnits ind ((CUIf exp):units) = 
>     spc ind ++ "if " ++ pretty exp ++ " then\n" ++ ppCalcUnits (ind + ilStdInd) units
> ppCalcUnits ind ((CUAssign (n, exp)):units) = 
>     spc ind ++ pretty n ++ " = " ++ pretty exp ++ "\n" ++ ppCalcUnits ind units

> countLoopsCU :: [CalcUnit] -> Int
> countLoopsCU u = length $ filter isCULoop u

> countIfsCU :: [CalcUnit] -> Int
> countIfsCU u = length $ filter isCUIf u

> countAssignsCU :: [CalcUnit] -> Int
> countAssignsCU u = length $ filter isCUAssign u



ComplexArg wird verwendet, um "komplexe" Algebra-Argumente an
Variablen in der Rhs einer Algebrafunktion zu binden.
Ein "komplexes" Agument kann sein:
- ein beliebig strukturierter IL2-Ausdruck
- in cg2l auch Nonterminals

Über diese Argumente werden dann in der Codeerzeugung foreach -
Schleifen gebaut.

Inhalt: 
1. Source als Kommentar
2. Name der zu bindenden Variablen
3. Struktur-Kontext der Variablen, z.B. v.enum.tup1  => ["enum", "tup1"]
4. an die Variable gebundener IL2-Ausdruck

> type ComplexArg  = (String, [(String, [String])], IL2)
> type ComplexArg2 = (String, SigArgument, IL2)

------------------------------------------------

> data Choice = ChoiceId | ChoiceMax | ChoiceMin | ChoiceMin_unstable | ChoiceSum | ChoiceSum_tuple | 
>               ChoiceExternal String | ChoiceExp Exp | ChoicePair [Choice]  
>              deriving (Eq, Show)

> expToChoice :: [Exp] -> Choice
> expToChoice [(ExpChoice "id"       _)]           = ChoiceId
> expToChoice [(ExpChoice "maximum"  _)]           = ChoiceMax
> expToChoice [(ExpChoice "minimum"  _)]           = ChoiceMin
> expToChoice [(ExpChoice "minimum_unstable" _)]   = ChoiceMin_unstable
> expToChoice [(ExpChoice "sum"      _)]           = ChoiceSum
> expToChoice [(ExpChoice "sum_tuple"_)]           = ChoiceSum_tuple
> expToChoice [(ExpChoice f          _)]           = ChoiceExternal f
> expToChoice [exp]                                = ChoiceExp exp
> expToChoice (c:cs)                               = ChoicePair (map (\c -> expToChoice [c])(c:cs))
> expToChoice []                                   = ChoiceId
> -- expToChoice x                                    = pattErr "expToChoice" x

> ppChoice = ppChoice' . expToChoice 

> ppChoice' ChoiceId           = "id"
> ppChoice' ChoiceMax          = "maximum"
> ppChoice' ChoiceMin          = "minimum"
> ppChoice' ChoiceMin_unstable = "minimum_unstable"
> ppChoice' ChoiceSum          = "sum"
> ppChoice' ChoiceSum_tuple    = "sum_tuple"
> ppChoice' (ChoiceExternal f) = f
> ppChoice' (ChoiceExp e)      = pretty e
> ppChoice' (ChoicePair chc)   = mapsep "/" ppChoice' chc


> exp_choiceSum  = (ExpChoice "sum" $ ExpVar "x")
> exp_choiceId   = (ExpChoice "id"  $ ExpVar "x")

----------------------------------------------------------------------------------------------------

listNeeded: Ermittelt anhand der IL-Produktionen, ob als Ergebnistyp
Listen benoetigt werden. 
Ergebnis: [(Nonterminal-Name, List needed)]

> type ListNeeded = (String,Bool)
> ppListNeeded (nt, l) = nt ++ " -> " ++ show l

> genListNeeded :: CompileOptions -> [AlgDefs] -> [ILProd] -> [ListNeeded]
> genListNeeded opts algs prods = fixit drProd init (length prods) prods "genListNeeded: no fixpoint reached..."
>   where
>     init = map (\(n, _, _) -> (n, False)) prods 

>     drProd currentResults (n,_,u) = (n, list) 
>       where list = needed currentResults u

>     needed :: [(String, Bool)] -> ILUnit -> Bool
>     needed ntTypes (ILTerminal t _)       = False
>     needed ntTypes (ILNonterminal n _)    = head' [ list | (n', list) <- ntTypes, n' == n]
>                                                   $ "genListNeeded: unknown nonterminal " ++ n
>     needed ntTypes (p :/~~~/ q)           = needed ntTypes p || needed ntTypes q
>     needed ntTypes (p :/|||/ q)           = True
>     needed ntTypes (p :/.../ cf)          | length algs == 0 = True
>                                           | otherwise        = not $ isAtomar opts choice
>       where
>         choice = map (\alg -> getAlgDefExpr alg cf) algs

>     needed ntTypes (p `ILwith` f)         = needed ntTypes p
>     needed ntTypes ((_,_,loops) :/<<</ p) | loops == [] = needed ntTypes p
>                                           | otherwise   = True
>     needed ntTypes (ILTTUnit p q)         = needed ntTypes p || needed ntTypes q

> lNeeded :: [ListNeeded] -> String -> Bool
> lNeeded lnd nt = head' [ l | (n,l) <- lnd, n == nt]
>                        $ "lNeeded: unknown nonterminal " ++ nt ++ "."
> atomarNt :: [ListNeeded] -> String -> Bool
> atomarNt lnd nt = not $ lNeeded lnd nt

--------------------------------------------------------------------------------------------


Typ-Ermittlung:

> type IL2Type = (String, [Exp], DataType)
> ppIL2Type (n, chc, dt) = n ++ " : choice context: " ++ mapsep "/" pretty chc ++ ", type: " ++ ppDataTypeC dt ++ "\n"
> ppIL2Types ts          = concatMap ppIL2Type ts

setType entscheidet, ob fuer einen gegebenen Typ eine Liste erstellt werden muss. 
1. Falls es sich bei dem Typ schon um eine Liste handelt, wird diese zurueckgegeben
2. Falls wir uns in einem atomaren Kontext befinden, wird keine Liste benoetigt
3. andernfalls wird ein Listentyp generiert.

> setType :: Bool -> DataType -> DataType
> setType atom typ | isListDT typ && atom = getListItemDataType typ
>                  | isListDT typ         = typ
>                  | atom                 = typ
>                  | otherwise            = typ  -- vorher: makeListStruct typ

isAtomar entscheidet, ob eine gegebene Auswahlfunktion ein atomares
Ergebnis liefert. Im Fall von ***/Backtrace nehmen wir an, dass die
Auswahlfunktion auf jeden Fall immer eine Liste liefert:

> isAtomar opts _     | inCGList opts          = False
> isAtomar opts [chc] = elem (expToChoice [chc]) [ChoiceMax, ChoiceMin, ChoiceMin_unstable, ChoiceSum, ChoiceSum_tuple] -- , ChoiceId]
> isAtomar opts chc   = all (\c -> isAtomar opts [c]) chc

> pptp chc atom inStr x t = (chc, inStr ++ ppIL2 0 x ++ ",\natom context: " ++ show atom ++ ":\n" ++ 
>                                 show t ++ "\n-----------------------\n",t)

> deriveIL2Type' :: CompileOptions -> Bool ->[AlgDefs] -> [IL2Type] -> IL2 -> ([Exp], String,DataType)
> deriveIL2Type' opts atom algs ntTypes x@(IL2Terminal _ _ typ _) = pptp _chc_init atom "" x $ setType atom typ
> deriveIL2Type' opts atom algs ntTypes x@(IL2Nonterminal n _)    = pptp _chc_init atom "" x $ setType atom typ
>   where
>     typ = head' [ dt | (n', _, dt) <- ntTypes, n' == n]
>                 $ "deriveIL2Type': unknown nonterminal " ++ n ++ "\ncurrentResults:\n" ++ show ntTypes
> deriveIL2Type' opts atom algs ntTypes x@(IL2Alt alts)  | tAlts == [] = pptp _chc_init atom inStr x $ makeListStruct TLVoid
>                                                       | allEq tAlts = pptp _chc_init atom inStr x $ setType atom $ dt' $ maxType (head tAlts) tAlts
>                                                       | otherwise   = error $ "deriveIL2Type': type error\n" ++ show tAlts
>                                                         where
>                                                           results = map (deriveIL2Type' opts atom algs ntTypes) alts
>                                                           inStr = concatMap snd3 results
>                                                           tAlts =  filter notVoid $ map thd3 results
>                                                           allEq (t:ts) = all (\x -> dt t == dt x) ts
>                                                           notVoid TLVoid                                         = False
>                                                           notVoid (PointerOf (StructOf _ [_,_,("item",TLVoid)])) = False
>                                                           notVoid _                                              = True
>                                                           dt t | isListDT t = getListItemDataType t
>                                                                | otherwise  = t
>                                                           dt' t | isListDT t = t
>                                                                 | otherwise  = makeListStruct t
>                                                           maxType mt []     = mt
>                                                           maxType mt (t:ts) | isListDT t = t
>                                                                             | otherwise  = maxType mt ts
>                                                        
> deriveIL2Type' opts atom algs ntTypes x@(IL2Choice cf il2) = pptp (map fth4 cf) atom inStr x til2
>   where
>     (_, inStr,til2) = deriveIL2Type' opts atom' algs ntTypes il2
>     atom' | length algs == 0 = False
>           | otherwise        = isAtomar opts (map fth4 cf)

> deriveIL2Type' opts atom algs ntTypes x@(IL2Filter _ il2)  = pptp _chc_init atom inStr x $ setType atom $ til2
>   where 
>     (_,inStr,til2) = deriveIL2Type' opts atom algs ntTypes il2

IL2Calc und IL2Calc2 werden durch eine gemeinsame Funktion
bearbeitet. In den hier relevanten Teilen unterscheiden sie sich
nicht.

> deriveIL2Type' opts atom algs ntTypes (IL2Calc2 cmt bnd cUnits rhss) = 
>      deriveIL2Type' opts atom algs ntTypes (IL2Calc cmt bnd cUnits rhss')
>   where
>     rhss' = map (\(algname, complx, dt, exp) -> (algname, map ccmp complx, dt, [], exp)) rhss
>     ccmp (_,_,il2) = ("",[],il2)   -- il2 ist der einzige Ausdruck, der hier benoetigt wird.

> deriveIL2Type' opts atom algs ntTypes x@(IL2Calc _ _ cUnits rhss) 
>   -- il2 ist in allen rhss identisch, daher reicht (head rhss)
>   | atomar (head rhss) && countLoopsCU cUnits == 0
>                           = pptp _chc_init atom inStr' x $ setType atom $ makeListStruct typ  -- Standardmaessig doch erstmal alles listen,
>   | otherwise             = pptp _chc_init atom inStr' x $ setType atom $ makeListStruct typ  -- entscheidung erfolgt ueber setType atom
>   where
>     typ | length algs == 0 = TLResult
>         | length algs == 1 = getDT (head algs)
>         | otherwise        = StructOf "" (map (\n -> (fst n, getDT n)) algs)
>     getDT (alg,_) = head' [ dt | (n, _, dt, _, _) <- rhss, n == alg]
>                           $ "deriveIL2Type': unknown algebra " ++ alg

>     atomar  (_,complx,_,_,_) = all atomar' complx
>     atomar' (_,_,il2)        = not $ isListDT $ thd3 $ deriveIL2Type' opts False algs ntTypes il2

>     inStr = concatMap (\(_, compl,_, _,_) -> concatMap (\(_,_,il2) -> snd3 $ deriveIL2Type' opts False algs ntTypes il2) compl) rhss
>     inStr' | inStr == [] = []
>            | otherwise   = "inner complex-Args:\n====================>>>\n" ++ inStr ++ 
>                            "<<<===============================================\n"

> -- fieser Hack fur TT: momentan ist das allerdings sowieso der einzige unterstuetzte TT-Modus:
> deriveIL2Type' opts atom algs ntTypes (IL2TTUnit x@(IL2Terminal _ _ _ _) _) = deriveIL2Type' opts atom algs ntTypes x

> deriveIL2Type' opts atom algs ntTypes x@(IL2Exp e) = pptp _chc_init atom "" x TLVoid

> deriveIL2Type' _ _ _ _ x = pattErr "deriveIL2Type'" x

Wrapper fuer deriveIL2Type; falls Listen-Codegenerierung, dann werden die Ergebnisse auf jeden Fall in Listen gepackt:

> deriveIL2Type :: CompileOptions -> Bool ->[AlgDefs] -> [IL2Type] -> IL2 -> ([Exp], String, DataType)
> -- fuer Terminale und Nichtterminale aendert sich nichts:
> deriveIL2Type opts atom algs ntTypes x@(IL2Terminal _ _ _ _) = deriveIL2Type' opts atom algs ntTypes x
> deriveIL2Type opts atom algs ntTypes x@(IL2Nonterminal  _ _) = deriveIL2Type' opts atom algs ntTypes x
> deriveIL2Type opts atom algs ntTypes x = (chc, cmt, updateType dt)
>   where
>     (chc, cmt, dt) = deriveIL2Type' opts atom algs ntTypes x
>     updateType typ | inCGList opts && not (isListDT typ) = makeListStruct typ
>                    | otherwise                           = typ

---------------------------------------------------------------

> deriveIL2Types :: CompileOptions -> [AlgDefs] -> [IL2Prod] -> (String,[IL2Type])
> deriveIL2Types opts algs il2s = fix algs init il2s
>   where
>     init = map (\(n, _, _) -> (n, _chc_init, makeListStruct TLVoid)) il2s 
>     fix algs currentResults il2s 
>         | currentResults == newResults  = (inStr, newResults)
>         | otherwise                     = fix algs newResults il2s
>         where
>           results    = map drProd il2s 
>           newResults = map snd results
>           inStr      = mapsep "\n==================================================================\n" fst results
>           drProd (n,_,u) = ("Nonterminal: " ++ n ++ "\n" ++ inStr,(n, chc, typ)) where
>             (chc, inStr, typ) = deriveIL2Type opts False algs currentResults u

---------------------------------------------------------------
Hilfsfunktionen:

> -- ntType liefert den Datentyp eines Nichtterminals bei gegebener Liste der 
> -- beteiligten Algebren und des Compile-Modus:
> ntType :: CompileOptions -> [IL2Type] -> [AlgDefs] -> String -> DataType
> ntType opts il2dt algs nt 
>   | inCGList opts && not (isListDT typ) = typ -- makeListStruct typ -- fuer nonterminals doch nur den Originaltyp -- makeListStruct typ 
>   | otherwise                           = typ
>   where 
>     typ         = head' [ dt | (n, _, dt) <- il2dt, n == nt]
>                         $ "unknown nonterminal " ++ nt

> -- ntChoiceContext liefert die fuer ein Nichtterminal relevante Auswahlfunktion
> ntChoiceContext :: CompileOptions -> [IL2Type] -> [AlgDefs] -> String -> [Exp]
> ntChoiceContext opts il2dt algs nt = typ
>   where 
>     typ = head' [ chc | (n, chc, _) <- il2dt, n == nt]
>                 $ "unknown nonterminal " ++ nt

> -- il2Type liefert den Datentyp eines IL2-Konstrukts bei gegebener Liste der
> -- beteiligten Algebren und des Compile-Modus:
> il2Type :: CompileOptions -> Bool -> [IL2Type] -> [AlgDefs] -> IL2 -> DataType
> il2Type opts atom il2dt algs il2 = thd3 $ deriveIL2Type opts atom algs il2dt il2


> deriveNtType opts ntTypes name = deriveIL2Type opts False [] ntTypes (IL2Nonterminal name (ST (Var "i", Var "j")))

Ermittelt, ob eine Algebrafunktion angewendet wird:

> isAlgebraUsed (IL2Terminal _ _ _ _) = False
> isAlgebraUsed (IL2Nonterminal _ _)  = True
> isAlgebraUsed (IL2Alt alts)         = any isAlgebraUsed alts
> isAlgebraUsed (IL2Choice _ il2)     = isAlgebraUsed il2
> isAlgebraUsed (IL2Filter _ il2)     = isAlgebraUsed il2
> isAlgebraUsed (IL2Calc _ _ _ _)     = True

------------------------------------------------

Prettyprinter:

> ppIL2Prod :: IL2Prod -> String
> ppIL2Prod (n, _, u)                 = "\n" ++ n ++ "!(i, j) =\n\n" ++ ppIL2 ilStdInd u ++ "\n"

> ppIL2 ind (IL2Exp e)                = pretty e
> ppIL2 ind (IL2TTUnit e1 e2)         = spc ind ++ "TT(" ++ ppIL2 ind e1 ++ ", " ++ ppIL2 ind e2 ++ ")"
> ppIL2 ind (IL2Terminal _ ift _ exp) = spc ind ++ "[" ++ pretty exp ++ if ift == [] then "]" else " | " ++ pretty (head ift) ++ "]"
> ppIL2 ind (IL2Nonterminal n s) = spc ind ++ n ++ "!" ++ ppSubScripts s
> ppIL2 ind (IL2Alt a)       = sepList ("\n" ++ spc ind ++ "++ \n")  (map (ppIL2 ind) a) ++ "\n" 
> ppIL2 ind (IL2Choice h a)  = spc ind ++ mapsep "," (pretty.fth4) h ++ "[\n" ++  ppIL2 (ind+ilStdInd)  a ++ "\n" ++ spc ind ++ "]"
> ppIL2 ind (IL2Filter m t)  = spc ind ++ "if " ++ pretty m ++ " then{\n" ++ ppIL2 (ind+ilStdInd)  t ++ "\n" ++ spc ind ++ "}"
> ppIL2 ind (IL2Calc cmt bnd cUnits rhss) = spc ind ++ "if " ++ ppILBounds bnd ++ " then {\n" 
>                                            ++ ppCalcUnits ind cUnits 
>                                            ++ spcd (cloops+1) ++ 
>                                            forEach
>                                            ++ sepList "; " (map (pretty.fith5) rhss) ++ "\n" 
>                                            ++ whereComplx
>                                            ++ spc ind ++ "}"   

>                                       where
>                                          complx = snd5 $ head rhss
>                                          forEachComplx = filter withoutChoice complx where
>                                            withoutChoice (_, _, IL2Choice _ _) = False
>                                            withoutChoice _                        = True

>                                          complxVs = map snd3 forEachComplx
>                                          forEach = case complxVs of
>                                                     [] -> ""
>                                                     otherwise -> "for each " ++ mapsep ", " ppcomplxV complxVs ++ " do\n" ++ spcd(cloops+cift+2)
>                                          ppcomplxV s = mapsep "," fst s ++ " <- " ++ mapsep "," (\(s,_) -> map toUpper s) s
>                                          whereComplx = case complx of
>                                                     [] -> ""
>                                                     otherwise -> "\n" ++ (spcd (cloops + cift +1)) ++ 
>                                                                  "where\n" ++ concatMap (ppcomplx (ind + ilStdInd * (cloops + cift + 2))) complx
>                                          ppcomplx ind (_, s, il2) = spc ind ++ ns ++ " =\n" ++ ppIL2 (ind+ilStdInd) il2 ++ "\n" where
>                                                                   ns = case (elem s complxVs) of
>                                                                               True  -> mapsep "," (\(s,_) -> map toUpper s) s
>                                                                               False -> mapsep "," fst s

>                                          cloops  = countLoopsCU cUnits
>                                          cift    = countIfsCU cUnits
>                                          ccomplx = length forEachComplx
>                                          spcd n = spc (ind + ilStdInd * n)

> ppIL2 ind (IL2Calc2 cmt bnd cUnits rhss) = spc ind ++ "if " ++ ppILBounds bnd ++ " then {\n" 
>                                            ++ ppCalcUnits ind cUnits
>                                            ++ spcd (cloops+1)
>                                            ++ sepList "; " (map (pretty.fth4) rhss) ++ "\n" 
>                                            ++ whereComplx
>                                            ++ spc ind ++ "}"   

>                                       where
>                                          complx = snd4 $ head rhss
>                                          whereComplx = case complx of
>                                                     [] -> ""
>                                                     otherwise -> (spcd (cloops + cift +1)) ++ 
>                                                                  "where\n" ++ 
>                                                                  concatMap (ppcomplx (ind + ilStdInd * (cloops + cift + 2))) complx
>                                          ppcomplx ind (cmt, args, il2) = spc ind ++ "/* " ++ cmt ++ " */\n" ++ 
>                                                                          spc ind ++ ppSigArgument args ++ " =\n" ++ 
>                                                                          ppIL2 (ind+ilStdInd) il2 ++ "\n" where

>                                          cloops  = countLoopsCU cUnits
>                                          cift    = countIfsCU cUnits
>                                          spcd n = spc (ind + ilStdInd * n)

%----------------------------------------------------------------------------------------------------
latex-recs:
%----------------------------------------------------------------------------------------------------

\begin{alignat}{3}
&open!(i, j) && = \quad && \sum\limits_{k=i+5}^{j-1} closed!(i,k) \\
&            && + && \sum\limits_{k=i+6}^{j-1} illeft!(i, k) \\
\\
&closed!(i,j) && = && 1 \quad \mbox{for } (j-i)>=5,\; 0 \mbox{ otherwise} \\
&             && + && closed!(i+1, j-1) \\
&             && + && open!(i+1, j-1)\\
\\
&closed!(i,j) && = && \begin{cases}
                         \begin{alignedat}{2}
                            &   \quad&& 1 \quad \mbox{for } (j-i)>=5,\; 0 \mbox{ otherwise} \\
                            & + \quad&& closed!(i+1, j-1) \\
                            & + \quad&& open!(i+1, j-1)\\
                         \end{alignedat} & \text{for } basepairing(i,j)\\
                         0 & \text{otherwise}
                      \end{cases}
\end{alignat}

> noResult chc = case expToChoice chc of
>   ChoiceSum        -> "0"
>   ChoiceSum_tuple  -> "null_tuple"
>   otherwise        -> "{}"

> newLine = " \\nonumber \\\\\n"

> ppIL2Prodstex :: [IL2Prod] -> String
> ppIL2Prodstex ps = "\\begin{alignat}{3}\n" ++ sepList newLine (map ppIL2Prodtex ps) ++ "\\nonumber\n\\end{alignat}\n"


> ppIL2Prodtex :: IL2Prod -> String
> ppIL2Prodtex (n, _, u)                 = "&" ++ ppNameTex n ++ "!(i, j) && = \\quad && " ++ ppIL2tex [] ilStdInd u 

> ppIL2tex chc ind (IL2Exp e)                = prettyLang Latex e
> ppIL2tex chc ind (IL2Terminal _ ift _ exp) = spc ind ++ "[" ++ prettyLang Latex exp
>                                                      ++ if ift == [] then "]" else " | "
>                                                      ++ prettyLang Latex (head ift) ++ "]"
> ppIL2tex chc ind (IL2Nonterminal n s) = ppNameTex n ++ "!" ++ ppSubScripts s ++ newLine
> ppIL2tex chc ind (IL2Alt a)       = sepList ("&       && + \\quad && ")  (map (ppIL2tex chc ind) a)
> ppIL2tex chc ind (IL2Choice h a)  = ppIL2tex (map fth4 h) (ind+ilStdInd) a 
> ppIL2tex chc ind (IL2Filter m t)  = "\\text{if } \\neg\\;" ++ prettyLang Latex m ++ "\\text{ then }" ++ noResult chc ++ " \\text{ else } " ++ newLine
>                                             ++ "&       &&   \\quad && " ++ ppIL2tex chc (ind+ilStdInd)  t
> ppIL2tex chc ind (IL2Calc cmt bnd cUnits rhss) = showIF
>                                            ++ ppCalcUnits ind cUnits
>                                            ++ forEach
>                                            ++ sepList "; " (map ((prettyLang Latex).fith5) rhss)
>                                            ++ whereComplx
>                                            ++ newLine

>                                       where

>                                          complx = snd5 $ head rhss
>                                          showIF = case ifNeccs of
>                                             True -> "\\text{if } \\neg\\;" ++ ppILBounds bnd ++ " \\text{ then } " ++ noResult chc ++ " \\text{ else } "
>                                             False -> ""

>                                          ifNeccs = not (countLoopsCU cUnits > 0 || isNT (fith5 $ head rhss)) where
>                                             isNT (ExpTLVar v) = isVANonterm v 
>                                             isNT _            = False

>                                          forEachComplx = filter withoutChoice complx where
>                                            withoutChoice (_, _, IL2Choice _ _) = False
>                                            withoutChoice _                     = True

>                                          complxVs = map snd3 forEachComplx
>                                          forEach = case complxVs of
>                                                     [] -> ""
>                                                     otherwise -> "for each " ++ sepList ", " (map ppcomplxV complxVs) ++ " do\n" ++ spcd(cloops+cift+2)
>                                          ppcomplxV s = mapsep "," fst s ++ " <- " ++ mapsep "," (\(s,_) -> map toUpper s) s
>                                          whereComplx = case complx of
>                                                     [] -> ""
>                                                     otherwise -> "\n" ++ (spcd (cloops + cift +1)) ++ 
>                                                                  "where\n" ++ concatMap (ppcomplx (ind + ilStdInd * (cloops + cift + 2))) complx
>                                          ppcomplx ind (_, s, il2) = spc ind ++ ns ++ " =\n" ++ ppIL2tex chc (ind+ilStdInd) il2 ++ "\n" where
>                                                                        ns = case (elem s complxVs) of
>                                                                               True  -> mapsep "," (\(s,_) -> map toUpper s) s
>                                                                               False -> mapsep "," fst s

>                                          cloops  = countLoopsCU cUnits
>                                          cift    = countIfsCU cUnits
>                                          ccomplx = length forEachComplx
>                                          spcd n = spc (ind + ilStdInd * n)


----------------------------------------------------------------------------------------------------
Transformation IL -> IL2: Alle Argumente werden in IL2Calc.Complx eingesetzt

> ph1IL2Prods :: UserFunctions -> [String] -> [AlgDefs] -> CompileOptions -> Bool -> [ILProd] -> [IL2Prod]
> ph1IL2Prods ufs recur algfs opts cnd ps = ph1Helper (ph1IL2Prod ufs recur algfs opts cnd) ps

Enumerate all tupel at this stage, because we need this information later for
the Table Access Macros (to be to able pp the TL to languages, which care about
types)

> ph1Helper f xs = snd $ mapAccumL g (0, []) xs
>   where
>    g (n, zs) x = ( (n', zs'), (a, b, new))
>      where
>        (a, b, c) = f x
>        ((n', zs'), new) = enumerateTupel (n, zs) c

>    enumerateTupel acc@(_, _) (IL2Calc2 x y z xs) = (left, (IL2Calc2 x y z right))
>      where
>        (left, right) = mapAccumL (f) acc xs
>        f bar@(n', zs) foo@(a, b, dt, e)
>          | isTupel dt = ((m, ys), (a, b, StructOf (("tupel") ++ (show m)) rest, e))
>          | otherwise = (bar, foo)
>         where
>          (m, ys) = if elem dt zs then (o + 1, zs) else (n'+1, zs ++ [dt])
>          (Just o) = elemIndex dt zs
>          (StructOf _ rest) = dt
>    enumerateTupel acc (IL2Choice xs il2) = (left, IL2Choice xs right)
>     where (left, right) = enumerateTupel acc il2
>    enumerateTupel acc (IL2Alt xs) = (left, IL2Alt right)
>     where (left, right) = mapAccumL enumerateTupel acc xs
>    enumerateTupel acc (IL2Filter exp il2) = (left, IL2Filter exp right)
>     where (left, right) = enumerateTupel acc il2
>    enumerateTupel acc foo = (acc, foo)


> ph1IL2Prod :: UserFunctions -> [String] -> [AlgDefs] -> CompileOptions -> Bool -> ILProd -> IL2Prod
> ph1IL2Prod ufs recur algfs opts cnd (n, k, u) = (n, k, ph1IL2 ufs recur algfs opts cnd u)

> ph1IL2 :: UserFunctions -> [String] -> [AlgDefs] -> CompileOptions -> Bool -> ILUnit ->  IL2
> ph1IL2 ufs recur algfs opts cnd (ILTerminal t (ST (ti,tj))) = IL2Terminal bnd ift dt exp 
>   where
>     (_, _, _, tcode) = termDef ufs t
>     (exp, ift) = case tcode (ti,tj) of
>        TCEmpty                 -> (ExpME ti,        [])
>        TCChar inp j            -> (ExpInput inp j,  [])
>        TCIFChar c inp j        -> (ExpInput inp j,  [ExpIOp (ExpInput inp j) "==" (ExpChar c)])
>        TCIFiupac c inp j       -> (ExpME j,  [ExpPOp "iupac_base" [ExpChar c, ExpInput inp j]])
>        TCIFNotChar c inp j     -> (ExpInput inp j,  [ExpIOp (ExpInput inp j) "/=" (ExpChar c)])
>        TCRegion inp (i,j)      -> (ExpTupel [ExpME i, ExpME j], [])
>        TCIFString s inp (i,j)  -> (ExpInputS inp (i,j), [ExpPOp "seqeq" [ExpVar inp, ExpString s, ExpME i, ExpME j]])
>        TCLoc i                 -> (ExpME i,        [])
>        TCUser f fargs          -> (ExpPOp f fargs,  [])
>        TCIFUser f fargs        -> (ExpPOp f fargs,  [ExpPOp ("chk_" ++ f) fargs])

>     bnd     = (ST (ti,tj), ys_t)
>     ys_t    = fst4 $ termDef ufs t
>     dt      = toDataType (snd4 $ termDef ufs t) where
>        toDataType (SigId t)       = haskellTypeToDatatype t
>        toDataType (SigTupel args) = makeTupelStruct (map toDataType args)
>        toDataType (SigList l)     = makeListStruct (toDataType l)

> ph1IL2 ufs recur algfs opts cnd (ILNonterminal nt s) = IL2Nonterminal nt s
> ph1IL2 ufs recur algfs opts cnd (p :/~~~/ q)         = error (
>                                      "type error!\n" ++
>                                       "no evaluation algebra function for\n    " ++ pretty (p :/~~~/ q) ++ 
>                                       "\nfound.\n")
> ph1IL2 ufs recur algfs opts cnd (p :/|||/ q)   = IL2Alt (p' ++ q') where
>                                   p' = getAlt p
>                                   q' = getAlt q
>                                   getAlt x = case x' of
>                                                IL2Alt    a -> a
>                                                otherwise   -> [x'] 
>                                     where
>                                       x' = ph1IL2 ufs recur algfs opts cnd x
> ph1IL2 ufs recur algfs opts cnd (p :/.../ h)   = IL2Choice h'' p' where
>                                  p' = ph1IL2 ufs recur algfs opts cnd p
>                                  h' = getAlgDefList algfs h 
>                                  h'' = case h' of
>                                          []        -> [("h", [], [SigId "x"], ExpChoice ("af_" ++ h) (ExpVar "x"))]
>                                          -- in den BT-Modi ist immer nur die erste Auswahlfunktion relevant:
>                                          otherwise -> case getOptBT opts of 
>                                                              BTNone    -> h' 
>                                                              otherwise -> [head h']

> ph1IL2 ufs recur algfs opts cnd (p `ILwith` (f, s)) = p'' where
>                                  p'                  = ph1IL2 ufs recur algfs opts cnd p
>                                  (_, genCode, fcode) = filterDef ufs f
>                                  p''                 = case genCode of
>                                                          True  -> IL2Filter (fcode s) p'
>                                                          False -> p'

> ph1IL2 ufs recur algfs opts cnd (ILTTUnit u1 u2) = IL2TTUnit (ph1IL2 ufs recur algfs opts cnd u1)
>                                                                (ph1IL2 ufs recur algfs opts cnd u2)

> ph1IL2 ufs recur algfs opts cnd ((((f,algArgs), dts), bnd, loops) :/<<</ p) = IL2Calc2 cmt bnd cUnits compiledAlgebras
>       where
>          -- Kommentar fuer den Zielcode
>          cmt = pretty' False ((((f,algArgs), dts), bnd, loops) :/<<</ p)

>          -- an dieser Stelle werden nur Loops in die cUnits integriert:
>          cUnits = map CULoop loops

>          -- hole fuer jede Algebra in algfs _genau_ eine Algebrafunktion mit Namen f:
>          algf = getAlgDefList algfs f

>          compiledAlgebras = map compileRhs algf
>          compileRhs (algName,atype,args,rhs) = (algName,complx,algType,rhs) where
                
>                -- binde die Algebra-Argumente an die Units der Grammatik:
>                argumentBind = case (length args) == (length algArgs + length units) of
>                           False -> error ("algebra function " ++ f ++ " with arity " ++ show (length args) ++
>                                           " is used\n" ++
>                                           "with " ++ show (length algArgs + length units) ++ " arguments.\n")
>                           True  -> zip args (algArgs' ++ units')
>                   where
>                      units = getNext p 
>                      getNext (p :/~~~/ q)     = getNext p ++ getNext q
>                      getNext x                = [x]
>                      units' = map ph1 units
>                        where ph1 unit = (pretty' False unit, ph1IL2 ufs recur algfs opts cnd unit)
>                      algArgs' = map ph1 algArgs
>                        where ph1 arg  = ("algebra argument: " ++ arg, IL2Exp (ExpVar arg))

>                complx = map conv argumentBind
>                   where
>                     conv (arg, (cmt,il2unit)) = (cmt, arg, il2unit)

>                algType = snd $ algTypetoDataType opts (last atype) 

> ph1IL2 ufs recur algfs opts cnd lc@(ILListCompr (lc_e, lc_defs) lc_bnd) = IL2Calc2 cmt lc_bnd cUnits compiledAlgebras
>       where
>          -- Kommentar fuer den Zielcode
>          cmt = take 50 (pretty' False lc) ++ " ... "

>          -- hole Algebrafunktion
>          (f,fargs) = case lc_e of
>                ExpPOp name args -> (name, args)
>                otherwise        -> error $ "list comprehensions currently only supported with algebra function usage"

>          assigns = concatMap get lc_defs
>            where
>              get (ILLCExp e)    = []
>              get (ILLCUnit e u) = [(e,u)]

>          cUnits = concatMap get lc_defs
>            where
>              get (ILLCExp (ExpIn (ExpVar lv) (ExpEnum start end))) = [CULoop (lv, expToMathExp start, expToMathExp end)]
>              get (ILLCExp (ExpLet (ExpVar n) e))                   = [CUAssign (Direct n,e)]
>              get (ILLCExp (ExpLet (ExpTupel ts) (ExpPOp nonterm subscripts))) = concatMap genBind (zip [1..] ts)
>                  where 
>                    genBind (n, ExpVar "lc__") = []
>                    genBind (n, ExpVar t)   = [CUAssign (Direct t, 
>                                                ExpTLVar ((VANonterm (ptbl prefixes ++ nonterm) 
>                                                                    (expTupelToSS subscripts)):.(Direct $ "tup" ++ show n)))]
>                    genBind x               = pattErr "ph1IL2.genBind" x
>              get (ILLCExp (ExpLet v _))          = error $ "local assignment to expression " ++ pretty v ++ " not supported"
>              get (ILLCExp e)               = [CUIf e]
>              get (ILLCUnit _ _)            = []
>              -- get x                         = pattErr "ph1IL2.get" x

>          -- hole fuer jede Algebra in algfs _genau_ eine Algebrafunktion mit Namen f:
>          algf = getAlgDefList algfs f

>          compiledAlgebras = map compileRhs algf
>          compileRhs (algName,atype,args,rhs) = (algName,complx,algType,rhs) where
                
>                -- binde die Algebra-Argumente an die Units der Grammatik:
>                argumentBind = case (length args) == (length fargs) of
>                           False -> error ("algebra function " ++ f ++ " with arity " ++ show (length args) ++
>                                           " is used\n" ++
>                                           "with " ++ show (length fargs) ++ " arguments.\n")
>                           True  -> concatMap collectArgs (zip args fargs)
>                   where

                      collectArgs (arg, farg) = (arg, head' [ unit | (e, unit) <- assigns, e == farg] $ 
                                                  "ph1IL2: unknown argument " ++ pretty farg ++ " in list comprehension for " ++ f)

>                      collectArgs (arg, farg) = let b = [ unit | (e, unit) <- assigns, e == farg] 
>                                                in case b of
>                                                     []        -> [(arg, farg, [])] 
>                                                     otherwise -> [(arg, farg, [head b])]

>                complx = map conv argumentBind
>                   where
>                     conv (arg, farg, []) = ("lcbind: " ++ show arg ++ " = " ++ pretty res,  arg,  IL2Exp res) 
>                                      where bind = [(n,e) | (CUAssign (Direct n, e)) <- cUnits]
>                                            res  = farg -- insertVarBinds bind farg
>                     conv (arg, _, [unit]) = (pretty' False unit, 
>                                             arg, 
>                                             ph1IL2 ufs recur algfs opts cnd unit)

>                algType = snd $ algTypetoDataType opts (last atype) 


mark the tupel structs, we are interested in (i.e. they are present in TL)
just useful for internal traces

> markStruct (StructOf _ bar) foo = StructOf foo bar

Ermittle den Rueckgabetyp dieser Funktionsanwendung:
wenn der Typ nicht in knownHaskellTypes enthalten ist, nehmen wir an dieser Stelle an,
das es sich um den Signaturtyp handelt. Ist natuerlich nicht ganz sauber...

> algTypetoDataType :: CompileOptions -> SigArgument -> (Bool, DataType)
> algTypetoDataType opts (SigId t)       = let dt     = [dt | (n,_,dt) <- knownHaskellTypes, n == t]
>                                              struct = PointerOf (StructOf (pstr prefixes ++ t) [])
>                                          in case dt of 
>                                             []        -> (True, case getOptBT opts of
>                                                             BTList      -> makeListStruct struct 
>                                                             BTSubOpt    -> makeListStruct struct 
>                                                             BTSubOptCut -> makeListStruct struct 
>                                                             BTPF        -> makeListStruct struct 
>                                                             otherwise   ->                struct)
>                                             otherwise -> (False, head dt)
> algTypetoDataType opts (SigTupel args) = let tupDTs      = map (algTypetoDataType opts) args
>                                              tupelStruct = markStruct (makeTupelStruct (map snd tupDTs)) "HAHA"
>                                          in case getOptBT opts of
>                                              BTNone    -> (False, tupelStruct)
>                                              otherwise -> if any fst tupDTs then (True,  snd $ head $ filter fst tupDTs)
>                                                                             else (False, tupelStruct)
> algTypetoDataType opts (SigList l)     = let (sig, dt) = algTypetoDataType opts l
>                                          in  (sig, makeListStruct dt)


----------------------------------------------------------------------------------------------------
Transformation IL2 -> IL2 : Einsetzen der Argumente in die Algebrafunktionen 

> ph2IL2Prods opts recur ntTypes cnd ps = map (ph2IL2Prod opts recur ntTypes cnd) ps
> ph2IL2Prod  opts recur ntTypes cnd (n, k, u) = (n, k, ph2IL2 opts recur ntTypes cnd u)

> ph2IL2 opts recur ntTypes cnd il2@(IL2Exp                 _)   = il2
> ph2IL2 opts recur ntTypes cnd il2@(IL2Terminal      _ _ _ _)   = il2
> ph2IL2 opts recur ntTypes cnd il2@(IL2Nonterminal   _ _)       = il2
> ph2IL2 opts recur ntTypes cnd il2@(IL2TTUnit        _ _)       = il2
> ph2IL2 opts recur ntTypes cnd     (IL2Alt           il2s)      = IL2Alt  (map (ph2IL2 opts recur ntTypes cnd) il2s)
> ph2IL2 opts recur ntTypes cnd     (IL2Choice        c il2)     = IL2Choice c  (ph2IL2 opts recur ntTypes cnd il2)
> ph2IL2 opts recur ntTypes cnd     (IL2Filter        f il2)     = IL2Filter f  (ph2IL2 opts recur ntTypes cnd il2)
> ph2IL2 opts recur ntTypes cnd (IL2Calc2 cmt bnd  cUnits                      rhss) = 
>                                IL2Calc  cmt bnd (cUnits ++ map CUIf ifTerms) (map snd compiledRhss)
>   where
>     -- die if-terme sind fuer alle algebren gleich:
>     ifTerms = fst $ head compiledRhss

>     compiledRhss = map compileRhs rhss
>     compileRhs (algName, complx, algType, algExpr) = (ifTerm, (algName, complx', algType, [], algExpr'))
>       where
>         -- argsBindCCs bindet die Argumente und liefert drei Kategorien:
>         --   directBind  : diese Ausdruecke koennen direkt in der Rhs eingesetzt werden
>         --   ifs         : zusaetzliche If-Abfragen, z.B. bei (char 'f') auf der Eingabe
>         --   complx'     : Argumente, fuer die komplexerer Code erzeugt werden muss, z.B. bei
>         --                 verschachtelten Unit-Strukturen

>         (directBind, ifs, complx') = argsBindCCs complx

>         -- die If-Abfragen mit && verknuepfen:
>         ifTerm = expMakeAndList ifs
                                 
>         -- aktualisiere die Rhs mit den neuen Variablenbindungen
>         -- ansonsten bleibt die Rhs unveraendert:
>         algExpr' = insertVarBinds directBind algExpr

>         -- Lege Standard-Strukturzugriff fest: 
>         algebraElem = case cnd of
>                             False -> []
>                             True  -> [algName]
>         tupElem n = algebraElem ++ ["tup" ++ show n]
>         nonTermElem n s e = ExpTLVar $ structAccess (VANonterm (ptbl prefixes ++ n) s) e

>         -- argsBindCCs: wrapper-Funktion fuer argsBindCC
>         argsBindCCs []                       = ([], [], [])
>         argsBindCCs ((cmt, arg, il2): binds) = (b ++ b', ift ++ ift', complx ++ complx') 
>                where
>                   (b,  ift,  complx ) = argsBindCC  (cmt, arg, il2)
>                   (b', ift', complx') = argsBindCCs binds

>         -- argsBindCC: hier passiert die eigentliche Variablenbindung:
>         argsBindCC (cmt, args, IL2Filter iftF il2) = (binds, iftF:ift, complx) where
>             (binds,ift,complx) = argsBindCC (cmt, args, il2)

>         argsBindCC (cmt, SigId arg,                     IL2Exp exp)                                 = ([(arg, exp)], [], [])
>         argsBindCC (cmt, SigTupel args,                 IL2Exp (ExpTupel exps))                     = (zip args' exps, [],[])
>           where args' = map s args
>                 s (SigId a) = a
>                 s x         = pattErr "argsBindCC.s"  x

>         argsBindCC (cmt, SigId arg,                     IL2Terminal bnd ift dt exp)                 = ([(arg, exp)],       ift, [])
>         argsBindCC (cmt, SigTupel [SigId a1, SigId a2], IL2Terminal bnd ift dt (ExpTupel [e1, e2])) = ([(a1, e1),(a2, e2)],ift, [])

         argsBindCC (cmt, SigTupel [SigId a1, SigId a2], IL2TTUnit (IL2Terminal bnd1 ift1 dt1 exp1)
                                                                   (IL2Terminal bnd2 ift2 dt2 exp2)) = ([(a1, exp1),(a2, exp2)], ift1 ++ ift2, [])

>         argsBindCC (cmt, SigTupel [a1, a2], IL2TTUnit (IL2Terminal bnd1 ift1 dt1 exp1)
>                                                       (IL2Terminal bnd2 ift2 dt2 exp2)) = (binds, ift1 ++ ift2, [])
>            where
>               binds = bind a1 exp1 ++ bind a2 exp2
>               bind (SigId a) e                             = [(a,e)]
>               bind (SigTupel [a1, a2]) (ExpTupel [e1, e2]) =  bind a1 e1 ++ bind a2 e2

>         argsBindCC (cmt, SigId arg, IL2Alt alts) 
>                                           -- checken, ob alles Terminale:
>                                           | all isTerminal alts = 
>                                                -- wenn alle Ausdruecke gleich, dann Zusammenfassen moeglich:
>                                                if allEqual termExpr 
>                                                       -- wenn einer der If-Terme leer ist, eruebrigt sich die ganze Abfrage:
>                                                  then if any (==[]) ifTerms then ([(arg,head termExpr)], [],                           [])
>                                                                             else ([(arg,head termExpr)], [expMakeOr (concat ifTerms)], [])
>                                                  else   complxConcat
>                                           | otherwise = complxConcat
>                 where
>                      complxConcat = ([],[],[(cmt,[(arg,algebraElem)], ph2IL2 opts recur ntTypes cnd (IL2Alt alts))])

>                      isTerminal (IL2Terminal _ _ _ _) = True
>                      isTerminal _                     = False

>                      termExpr = map (\(IL2Terminal _ _ _ exp) -> exp) alts
>                      ifTerms  = map (\(IL2Terminal _ ift _ _) -> ift) alts
>                      allEqual l = length (nub l) == 1

>         -- hier ist möglicherweise noch der fall (f <<< p) interessant....
>         -- Aber: darf eigentlich nicht sein, da auch für z.B. nil <<< uregion mit nil _ = 0 trotzdem noch
>         -- Abfragen generiert werden müssen...

>         argsBindCC (cmt, SigId arg, IL2Nonterminal nt s) 
>                             | isListDT typ || elem nt recur = ([],             [], [(cmt, [(arg, algebraElem)], IL2Nonterminal nt s)])
>                             | otherwise                     = ([(arg, expNT)], [], [])

>                    where
>                      typ = thd3 $ deriveNtType opts ntTypes nt
>                      expNT  =  nonTermElem nt s algebraElem

>         argsBindCC (cmt, SigTupel args, IL2Nonterminal nt s) = (binds, [], complx) where
>                      binds  = let nargs    = zip [1..] args
>                                   typ      = thd3 $ deriveNtType opts ntTypes nt
>                                   bindings = map bind nargs
>                                   bind (n, SigId e) = (e, nonTermElem nt s (tupElem n))
>                                   bind (_, x)       = pattErr "argsBindCC.binds.bind" x
>                                   ret | isListDT typ || elem nt recur = []
>                                       | otherwise                     = bindings
>                               in ret 
>                      complx = let nargs    = zip [1..] args
>                                   bindings = [(cmt, varList, IL2Nonterminal nt s)]
>                                   varList  = map bind nargs
>                                   bind (n, SigId e) = (e, tupElem n)
>                                   bind (_, x)       = pattErr "argsBindCC.complx.bind" x
>                                   typ      = thd3 $ deriveNtType opts ntTypes nt
>                                   ret | isListDT typ || elem nt recur = bindings
>                                       | otherwise                     = []
>                               in ret 

>         argsBindCC (cmt, SigId arg, il2)     = ([], [], [(cmt, [(arg, algebraElem)], ph2IL2 opts recur ntTypes cnd il2)])
>         argsBindCC (cmt, SigTupel args, il2) = ([], [], complx) 
>                    where
>                      nargs  = zip [1..] args
>                      complx = [(cmt, varList, ph2IL2 opts recur ntTypes cnd il2)]
>                      varList = map bind nargs

>                      bind (n, SigId e) = (e, tupElem n)
>                      bind (_, x)       = pattErr "argsBindCC.bind" x

>         argsBindCC x  = pattErr "argsBindCC" x


> ph2IL2 opts recur ntTypes cnd x = pattErr "ph2IL2" x
