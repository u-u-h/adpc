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

> module MathExp(

>   MathExp(..),
>   LALU(..),

>   SubScripts, SSubScripts,
>   YSize, SYSize, YSizes,

>   atomarME,
>   calcME,
>   gr',
>   includesLAME,
>   insertVarBindsME,
>   kString,
>   mapMathExp,
>   max',
>   min',
>   plus',
>   ppLoopVarTex,
>   ppMathExp,
>   getYSize, ppSYSize, ppYSize, ppYSizes, ppYSizesElem,
>   ppSubScripts,
>   rev_MathExp,

> ) where

> import Tools
> import Constants
> import PrettyPrint

> rev_MathExp =  "$Revision$"

%----------------------------------------------------------------------------------------------------
\subsection{Mathematische Ausdrücke}
%----------------------------------------------------------------------------------------------------

> data LALU = LAL | LAU                            deriving (Show,Eq)

> type SYSize = (MathExp, MathExp)
> type YSize = TrackMode SYSize
> type YSizes = Assoc String YSize
> ppSYSize (l,u) = "(" ++ ppMathExp l ++ "," ++ ppMathExp u ++ ")"
> ppYSize (ST y)      = ppSYSize y
> ppYSize (TT y1 y2)  = "(" ++ ppSYSize y1 ++ "," ++ ppSYSize y2 ++ ")"
> getYSize ysizes n = lookupA1 ysizes n "YSize" 
> ppYSizesElem :: (String, YSize) -> String
> ppYSizesElem = ppAssocElem ppYSize
> ppYSizes :: YSizes -> String
> ppYSizes = ppAssoc ppYSize

> ppSSubScripts (i,j)       = "(" ++ ppMathExp i ++ "," ++ ppMathExp j ++ ")"
> ppSubScripts (ST y)      = ppSSubScripts y
> ppSubScripts (TT y1 y2)  = "(" ++ ppSSubScripts y1 ++ "," ++ ppSSubScripts y2 ++ ")"

> type SSubScripts = (MathExp, MathExp)
> type SubScripts = TrackMode SSubScripts

> gr' :: MathExp -> MathExp -> Bool
> gr' Infinite _              = True
> gr' _        Infinite       = False
> gr' (Number a) (Number b)   = a > b
> gr' (LA _ _ _) _            = True
> gr' _ (LA _ _ _)            = False
> gr' v _                     = includesVarME v
> -- gr' _ v                     = not (includesVarME v)

> plus' a b                   = calcME (calcME a :+ calcME b)
> min' a b                    = calcME (Min (calcME a) (calcME b))
> max' a b                    = calcME (Max (calcME a) (calcME b))

> infixr :+, :-, :*, :/

> data MathExp = 
>       Infinite                          |
>       Number Int                        |
>       Var String                        | 
>       MathExp :# MathExp                | -- Deref Operator, only for
>                                           -- Var  useful
>       MathExp :- MathExp                |
>       MathExp :+ MathExp                |
>       MathExp :* MathExp                |
>       MathExp :/ MathExp                |
>       Max MathExp MathExp               |
>       Min MathExp MathExp               |
>       -- Zielsprachenelemente: normalerweise waere es das einfachste, hier TL
>       -- verwenden zu koennen. Dies geht aber nicht, da wir sonst zyklische
>       -- Modulabhaengigkeiten bekommen. Also:
>       LA String LALU (MathExp, MathExp) |  -- for lookahead-functions
>       Offset MathExp                       -- for table access offset

>                                        deriving (Eq, Show)


> instance Pretty MathExp where
>     pretty = ppMathExpM Normal
>     prettyLang = ppMathExpM

> kString = "k" -- "\\alpha"
> ppLoopVarTex ('k':[]) = kString ++ "_{1}"
> ppLoopVarTex ('k':xs) = kString ++ "_{" ++ xs ++ "}"
> ppLoopVarTex x        = x 

> atomarME (Number _ )  = True
> atomarME (Var  _ )    = True
> atomarME _            = False 


> ppMathExp    e = ppMathExpM Normal e
> ppMathExpTex e = ppMathExpM Latex  e

> ppMathExpM mode m             = ppMathExpM' mode (calcME m)
> ppMathExpM' mode Infinite     = "Infinite"
> ppMathExpM' mode (Number a)   = show a

Java translation does not need '->' ...

> ppMathExpM' Java ((Var a) :# (Var b)) = a ++ "." ++ b

> ppMathExpM' mode (a :# b) = ppMathExpM' mode a ++ "->" ++ ppMathExpM' mode  b

> ppMathExpM' mode (Var a)      | mode == Latex = ppLoopVarTex a 
>                               | otherwise     = a
                              
> ppMathExpM' mode (a :+ b)     = ppMathExpM' mode a ++ "+" ++ ppMathExpM' mode b
> ppMathExpM' mode (a :* b)     = a' ++ "*" ++ b' 
>                           where
>                             a' = if atomarME a then ppMathExpM' mode a else "(" ++ ppMathExpM' mode a ++ ")"
>                             b' = if atomarME b then ppMathExpM' mode b else "(" ++ ppMathExpM' mode b ++ ")"
> ppMathExpM' mode (a :/ b)     = a' ++ "/" ++ b' 
>                           where
>                             a' = if atomarME a then ppMathExpM' mode a else "(" ++ ppMathExpM' mode a ++ ")"
>                             b' = if atomarME b then ppMathExpM' mode b else "(" ++ ppMathExpM' mode b ++ ")"

> ppMathExpM' mode (a :- b)     = ppMathExpM' mode a ++ "-" ++ b' 
>                           where
>                             b' = if atomarME b then ppMathExpM' mode b else "(" ++ ppMathExpM' mode b ++ ")"

Java

> ppMathExpM' Java (Max a b) = "Math.max(" ++ ppMathExpM' Java a ++ ", " ++ 
>   ppMathExpM' Java b ++ ")"

> ppMathExpM' Java (Min a b) = "Math.min(" ++ ppMathExpM' Java a ++ ", " ++ 
>   ppMathExpM' Java b ++ ")"

> ppMathExpM' mode (Max a b)         = l ++ "max(" ++ ppMathExpM' mode a ++ ", " ++ ppMathExpM' mode b ++ ")"
>                                   where l = if mode==Latex then "\\" else ""
> ppMathExpM' mode (Min a b)         = l ++ "min(" ++ ppMathExpM' mode a ++ ", " ++ ppMathExpM' mode b ++ ")"
>                                   where l = if mode==Latex then "\\" else ""

fuer lookahead nehmen wir nun doch die direkte Zielcode-Ausgabe, da
ansonsten die in ExpME m enthaltenen Ausdruecke falsch ausgegeben
werden:

> ppMathExpM' mode (LA f LAL (i, j)) = f ++ concatMap (\e -> "[" ++ ppMathExp e ++ "]") [i, j, Number 0] 
> ppMathExpM' mode (LA f LAU (i, j)) = f ++ concatMap (\e -> "[" ++ ppMathExp e ++ "]") [i, j, Number 1] 

> ppMathExpM' F    (Offset s)        = poffset prefixes ++ "(" ++ ppMathExp s ++ ")"
> ppMathExpM' mode (Offset s)        = poffset prefixes ++ "[" ++ ppMathExp s ++ "]"

 ppMathExpM' mode (LA f LAL (i, j)) = l "low" ++ "(" ++ f ++ "(" ++ ppMathExpM' mode i ++ ", " ++ ppMathExpM' mode j ++ "))"
                                   where l x = if mode==Latex then "\\text{" ++ x ++ "}"  else x
 ppMathExpM' mode (LA f LAU (i, j)) = l "up" ++ "("  ++ f ++ "(" ++ ppMathExpM' mode i ++ ", " ++ ppMathExpM' mode j ++ "))"
                                   where l x = if mode==Latex then "\\text{" ++ x ++ "}"  else x


--------------------------------------------------------------

> calcME Infinite    = Infinite
> calcME (Number a)  = (Number a)

> calcME (Infinite :+ a)   = Infinite
> calcME (a :+ Infinite)   = Infinite
> calcME ((Number 0) :+ a) = calcME a
> calcME (a :+ (Number 0)) = calcME a
> calcME ((Number a) :+ (Number b)) = Number (a+b)

> --neu, fuer Tabellengroessenberechnung:
> calcME ((Number a) :- (Number b)) = Number (a-b)
> calcME (a :-          (Number 0)) = calcME a
> -------------

> calcME (Max Infinite a) = Infinite
> calcME (Max a Infinite) = Infinite

wenn in dem Ausdruck max(0,v) eine Variable enthalten ist, dann nehmen wir an, dass diese auf jeden Fall groesser als 0/1 ist:

> calcME (Max (Number n) v) | n <= 1 && includesVarME v = v
> calcME (Max v (Number n)) | n <= 1 && includesVarME v = v

entsprechendes fuer min(0,v):

> calcME (Min (Number n) v) | n <= 1 && includesVarME v = (Number n)
> calcME (Min v (Number n)) | n <= 1 && includesVarME v = (Number n)

Dieser Spezialfall tritt bei der Yieldsize-Analyse auf, wenn Variablen in den yield sizes enthalten sind:
(min(5, max(5, minlen)+1)+2)

> calcME (Min (Number n1) ((Max (Number n2) _) :+ (Number n3))) | n1 == n2 && n3 >= 0 = Number n1

-----------------------------------------

> calcME (Min Infinite a) = calcME a
> calcME (Min a Infinite) = calcME a

> calcME (Max (Number a) (Number b)) = Number (max a b)
> calcME (Min (Number a) (Number b)) = Number (min a b)

 calcME (Max a b) = case calcME ((calcME'' a) :- (calcME'' b)) of
                                    (Number n)  -> if n < 0 then calcME b else calcME a
                                    otherwise -> Max (calcME a) (calcME b)

 calcME (Min a b) = case calcME ((calcME'' a) :- (calcME'' b)) of
                                    (Number n)  -> if n < 0 then calcME a else calcME b
                                    otherwise -> Min (calcME a) (calcME b)

> calcME (LA f lu bd) = LA f lu bd
> calcME (Offset s)   = Offset s

> calcME a = calcME'' n'  where 
>                (n, x) = calcME' a
>                x'     = calcME'' x
>                n'     | n==0 = x'   
>                       | n<0  = x' :- Number (-n)
>                       | n>0  = x' :+ Number n


----------------------------------------------------------------

calcME': Zahlen aufsammeln:

> calcME' (Infinite)      = (0, Infinite)
> calcME' (Number a)      = (a,  Number a)

> calcME' (Var  a)        = (0,  Var a)
> calcME' (Max a b)       = (0, Max a b)
> calcME' (Min a b)       = (0, Min a b)
> calcME' (LA f lu bd)    = (0, LA f lu bd)
> calcME' (Offset s)      = (0, Offset s)
> calcME' (a :* b)        = (0, a :* b)
> calcME' (a :/ b)        = (0, a :/ b)

> calcME' (Number a :+ b) = (a+n, b')  where (n, b') = calcME' b
> calcME' (a :+ Number b) = (b+n, a')  where (n, a') = calcME' a
> calcME' ((Var a) :+ b)  = (n, (Var a) :+ b')  where (n, b') = calcME' b
> calcME' (a :+ b)        = (na + nb, a' :+ b') where
>                                  (na, a') = calcME' a
>                                  (nb, b') = calcME' b

> calcME' (Number a :- b) = (a-n, b')  where (n, b') = calcME' b
> calcME' (a :- Number b) = (n-b, a')  where (n, a') = calcME' a
> calcME' ((Var a) :- b)  = (-n, (Var a) :- b')  where (n, b') = calcME' b
> calcME' (a :- b)        = (na - nb, a' :- b') where
>                                  (na, a') = calcME' a
>                                  (nb, b') = calcME' b

Java extension

> calcME' x@(a :# b) = (0, x)



-----------------------------------------------------------------

calcME'': Variablenaufhebungen eliminieren:

> calcME'' ((Number a) :- (Number b)) = Number (a-b)

> calcME'' (((Var a) :- (Var b)) :+ c) = if a==b then calcME'' c else 
>                                          (((Var a) :- (Var b)) :+ calcME'' c)
> calcME'' (c :+ ((Var a) :- (Var b))) = if a==b then calcME'' c else 
>                                          (calcME'' c :+ ((Var a) :- (Var b)))
> calcME'' (((Var a) :- (Var b)) :- c) = if a==b then calcME'' (Number 0 :- c) else 
>                                          (((Var a) :- (Var b)) :- calcME'' c)
> calcME'' (c :- ((Var a) :- (Var b))) = if a==b then calcME'' c else 
>                                          (calcME'' c :- ((Var a) :- (Var b)))
> calcME'' (a :+ b)                    = (calcME'' a) :+ (calcME'' b)

ansonsten:

> calcME'' x = x

----------------------------------------------------------------------------------------------------
Collector for MathExp

> collectMathExp wrk cll (Infinite) = (cll, Infinite)
> collectMathExp wrk cll (Number i) = (cll, Number i)
> collectMathExp wrk cll (Var s)    = (cll, Var s) 

> collectMathExp wrk cll (m1 :- m2)        = (cll'', m1' :- m2')        where (cll'', m1', m2') = collectMathExp2 wrk cll m1 m2
> collectMathExp wrk cll (m1 :+ m2)        = (cll'', m1' :+ m2')        where (cll'', m1', m2') = collectMathExp2 wrk cll m1 m2
> collectMathExp wrk cll (m1 :* m2)        = (cll'', m1' :* m2')        where (cll'', m1', m2') = collectMathExp2 wrk cll m1 m2
> collectMathExp wrk cll (m1 :/ m2)        = (cll'', m1' :/ m2')        where (cll'', m1', m2') = collectMathExp2 wrk cll m1 m2
> collectMathExp wrk cll (Max m1 m2)       = (cll'', Max m1' m2')       where (cll'', m1', m2') = collectMathExp2 wrk cll m1 m2
> collectMathExp wrk cll (Min m1 m2)       = (cll'', Min m1' m2')       where (cll'', m1', m2') = collectMathExp2 wrk cll m1 m2
> collectMathExp wrk cll (LA s l (m1, m2)) = (cll'', LA s l (m1', m2')) where (cll'', m1', m2') = collectMathExp2 wrk cll m1 m2
> collectMathExp wrk cll (Offset s)        = (cll',  Offset s')         where (cll', s')        = wrk cll s

> collectMathExp2 wrk cll m1 m2 = (cll'', m1', m2')
>   where
>     (cll',  m1') = wrk cll  m1
>     (cll'', m2') = wrk cll' m2


> mapMathExp wrk x = snd $ collectMathExp wrk' 0 x where
>    wrk' cll x = (cll, wrk x)

----------------------------------------------------------------------------------------------------

Tools:

> insertVarBindsME :: [(String, MathExp)] -> MathExp -> MathExp
> insertVarBindsME bs (Var v) = let nv = [ nv | (v', nv) <- bs, v == v'] in
>                               case nv of
>                                     []        -> Var v
>                                     otherwise -> head nv
> insertVarBindsME bs x      = mapMathExp (insertVarBindsME bs) x


> includesVarME :: MathExp -> Bool
> includesVarME m = fst (iVar False m)
>   where
>     iVar _    v@(Var _) = (True, v)
>     iVar cll  x         = collectMathExp iVar cll x

> includesLAME :: MathExp -> Bool
> includesLAME m = fst (iVar False m)
>   where
>     iVar _    v@(LA _ _ _) = (True, v)
>     iVar cll  x            = collectMathExp iVar cll x


> isNumberME (Number _) = True
> isNumberME _          = False
