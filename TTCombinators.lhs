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



> module TTCombinators(

>   (+~), (~+),
>   (<<<), (|||),
>   (><<), (~-),
>   (...),
>   axiom,
>   bindParserCombinators,
>   mk,
>   region,
>   table,
>   uregion

> ) where

> import Array

Lexical parsers
----------------

> type Subword  = (Int,Int)
> type Parser b = Subword -> [b]

> region, uregion :: Parser (Int,Int)
> uregion (i,j) = [(i,j)| i <= j]
> region (i,j)  = [(i,j)| i < j]

> xbase', ybase' :: Array Int Char -> Parser Char
> xbase' x (i,j)   = [x!j| i+1 == j]
> ybase' y (i,j)   = [y!j| i+1 == j]

> charx'           ::  Eq a => Array Int a -> a -> Parser a
> charx' x c (i,j) =  [c | i+1 == j, x!j == c]

> chary'           ::  Eq a => Array Int a -> a -> Parser a
> chary' y c (i,j) =  [c | i+1 == j, y!j == c]


> empty' :: Int -> Int -> Parser ()
> empty' m n (i,j)   = [()| i == m && j == n]

Parser combinators
-------------------

> infix 5 ...
> infixr 6 |||
> infixl 7 +~, -~, ~+, ~-
> infix 8 <<<

> (q ||| r) (i,j) = q (i,j) ++ r (i,j)

> (f <<< q) (i,j) = [f s | s <- q (i,j)]
> (f ><< q) (i,j) = [f | s <- q (i,j)]

> (q +~ r) m (i,j) = [s t | k <- [i..m], s <- q (i,k), t <- r (k,j)]
> (q ~+ r) n (i,j) = [s t | k <- [j..n], s <- q (i,k), t <- r (j,k)]

> (q -~ r) m (i,j) = [s t | i < m, s <- q (i,i+1), t <- r (i+1,j)]
> (q ~- r) n (i,j) = [s t | j < n, s <- q (i,j+1), t <- r (j,j+1)]

> (q ... h) (i,j) = h (q (i,j))

> axiom q  = q (0,0)

Tabulation
-----------

> table      :: Int -> Int -> Parser b -> Parser b
> table m n q = (!) $ array ((0,0),(m,n))
>                     [((i,j),q (i,j)) | i<- [0..m], j<- [0..n]]

Create array from List
-----------------------

> mk :: [a] -> Array Int a
> mk xs = array (1,n) (zip [1..n] xs) where n = length xs

Bind input
-----------

> bindParserCombinators inpX inpY = (x, y, xbase, ybase, charx, chary, empty, (+~~), (~~+), (-~~), (~~-), tabulated) where
>   x         = mk inpX
>   y         = mk inpY
>   (_,m)     = bounds x
>   (_,n)     = bounds y
>   xbase     = xbase' x
>   ybase     = ybase' y
>   charx     = charx' x
>   chary     = chary' y
>   empty     = empty' m n
>   infixl 7 +~~, -~~, ~~+, ~~-
>   (+~~) q r = (q +~ r) m
>   (~~+) q r = (q ~+ r) n
>   (-~~) q r = (q -~ r) m
>   (~~-) q r = (q ~- r) n
>   tabulated = table m n

> bindSpezialCombinators x y = ((++~), (~++)) where
>   (++~) q r (i,j) = [s t | k <- [i+1..m], s <- q (i,k), t <- r (k,j)]
>   (~++) q r (i,j) = [s t | k <- [j+1..n], s <- q (i,k), t <- r (j,k)]
>   (_,m)           = bounds x
>   (_,n)           = bounds y
