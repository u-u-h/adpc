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



> module CoreTools where
> import List

> pattErr n p = error $ n ++ ": undefined pattern " ++ show p

> sepList _ []       = []
> sepList _ [x]      = x
> sepList t (x:xs)   = x ++ t ++ sepList t xs

> mapsep sep f xs = sepList sep (map f xs)

> fst3 (a,_,_) = a
> snd3 (_,a,_) = a
> thd3 (_,_,a) = a

> fst4 (a,_,_,_) = a
> snd4 (_,a,_,_) = a
> thd4 (_,_,a,_) = a
> fth4 (_,_,_,a) = a

> spc n = replicate n ' '

> head' [] err = error err
> head' x  _   = head x

> maybe [] = Nothing
> maybe x  = Just $ head x

> target = 0
> trace  = 1
> debug  = 2

> fix wrk currentResult = fix' (-1) wrk currentResult  -- (-1) => counting disabled
>   where
>    fix' n wrk currentResult = e 
>      where
>        newResults = wrk currentResult
>        e | newResults  == currentResult = newResults 
>          | n == 0                       = newResults 
>          | otherwise                    = fix' (n-1) wrk newResults 

> anyCommon a b = length (intersect a b) > 0

> unlit inputFile inp | take 3 (filter (/= ' ') (reverse inputFile)) /= "shl" = inp
>                     | otherwise =  unlines $ map tail $ filter l $ lines inp
>    where l ('>':_) = True
>          l _       = False

> endsWith name suffix = suffix == (reverse $ take (length suffix) $ reverse name)


Association list helpers:
--------------------------

> getVal vars v = case [ c | (v', c) <- vars, v' == v] of
>                    [] -> Nothing
>                    x  -> Just $ head x

> elemVal vars v = elem v $ map fst vars
> justVal vars v = case getVal vars v of
>                          (Just v) -> v
>                          Nothing  -> error $ "justVal: not defined: " ++ v

> ppAssoc pp vals = unlines $ map (\(n, val) -> n ++ " -> " ++ pp val) vals

> getDef defs name = case [ def | def@(n, _, _) <- defs, n == name] of
>                          [] -> Nothing
>                          x  -> Just $ head x
