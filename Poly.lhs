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



> module Poly(

>   PolyExp(..),

>   Poly,

>   (+++), (***),
>   cPoly,
>   expo,
>   lengthP,
>   maxP,
>   maxPoly,
>   maximumP,
>   minP,
>   multPoly,
>   ppPoly,
>   ppPolyExp,
>   rev_Poly,
>   sumPoly

> ) where

> import Tools

> rev_Poly =  "$Revision$"

> type Poly = [Int]

> expo :: Poly
> expo = (replicate 20 0) ++ [1] 

> ppPoly x = sepList " + " (filter ((/=)"") (map ppR (reverse (zip x [0..])))) where
>   ppR (0,_) = ""
>   ppR (a,0) = show a
>   ppR (a,1) = show' a ++ "n"
>   ppR (a,n) = show' a ++ "n^" ++ show n 
>   show' 1 = ""
>   show' a = show a

> (+++) :: Poly -> Poly -> Poly
> x +++ y = sumPoly x y

> (***) :: Poly -> Poly -> Poly
> x *** y = multPoly x y

Addition von zwei Polynomen:

> sumPoly :: Poly -> Poly -> Poly
> sumPoly [] a = a
> sumPoly a [] = a
> sumPoly (a:aa) (b:bb) = (a+b):sumPoly aa bb

Multiplikation von zwei Polynomen:

> multPoly :: Poly -> Poly -> Poly
> multPoly [] a     = []
> multPoly a  []    = []
> multPoly (x:xs) b = sumPoly (map ((*)x) b) (multPoly xs (0:b))

Polynome exponentieren:

> expPoly :: Poly -> Poly -> Poly
> expPoly a b = expPoly' (n a) (n b) where

>   n x  | a0 x      = [0]
>        | otherwise = x

>   a0    = all (==0)

>   expPoly' [0] [0]   = [1]
>   expPoly' [0] _     = [0]
>   expPoly' a e       | a0 e                               = [1]
>                      | length e > 1 && not (a0 $ tail e)  = expo
>                      | otherwise                          = m a (head e)
>                      where 
>                          m a 1 = a
>                          m a e = a *** m a (e-1)

Abschliessende Nullen abschneiden:

> lengthP :: Poly -> Int
> lengthP p = lengthP' 1 0 p where
>   lengthP' l a []    = l
>   lengthP' l a (0:p) = lengthP'  l     (a+1) p
>   lengthP' l a (_:p) = lengthP'  (a+1) (a+1) p

> cPoly [] = [0]
> cPoly x  = take (lengthP x) x 

Maximum von zwei Polynomen:

> maxPoly :: Poly -> Poly -> Poly
> maxPoly a b | la < lb = b
>             | la > lb = a
>             | otherwise =  reverse (max (reverse a) (reverse b))
>             where
>               la = lengthP a
>               lb = lengthP b

> maxP :: Poly -> Poly -> Bool
> maxP x y | x == y    = False
>          | x == m    = True
>          | otherwise = False
>          where
>             m = maxPoly x y

> maximumP :: [Poly] -> Poly
> maximumP = foldl1 maxPoly

Miminum von zwei Polynomen:

> minPoly :: Poly -> Poly -> Poly
> minPoly a b | la < lb = a
>             | la > lb = b
>             |  otherwise = reverse (min (reverse a) (reverse b))
>             where
>               la = lengthP a
>               lb = lengthP b

> minP :: Poly -> Poly -> Bool
> minP x y | x == y    = False
>          | x == m    = True
>          | otherwise = False
>          where
>             m = minPoly x y

Polynom - Ausdrücke
------------------------------------------

> infixr :++, :**

> data PolyExp = 
>       Poly Poly            |
>       PNt  Int             |
>       PolyExp :++ PolyExp  |
>       PolyExp :** PolyExp     deriving Show

> ppPolyExp nameIds (Poly p) = ppPoly p
> ppPolyExp nameIds (PNt n)  = head [ name | (name, id') <- nameIds, n == id' ]
> ppPolyExp nameIds (p :++ q) = pa (ppPolyExp nameIds p ++ " + " ++ ppPolyExp nameIds q) where
>    pa s = "(" ++ s ++ ")"
> ppPolyExp nameIds (p :** q) = pa (ppPolyExp nameIds p ++ " * " ++ ppPolyExp nameIds q) where
>    pa s = "(" ++ s ++ ")"

