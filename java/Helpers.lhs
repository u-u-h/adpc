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


> module Helpers

> where

> import List
> import Maybe


a generalized words

> split :: (Eq a) => a -> [a] -> [[a]]
> split c xs = f [] [] xs
>  where
>   f sofar s (z:zs) 
>     | z == c = f ((reverse s):sofar) [] zs
>     | otherwise = f sofar (z:s) zs
>   f sofar [] [] = reverse sofar
>   f sofar s  [] = reverse ((reverse s):sofar)

> unsplit :: (Eq a) => a -> [[a]] -> [a]
> unsplit c xs = concatMap (++[c]) xs

> strictLookup :: (Eq a, Show a, Show b) => [([a], [b])] -> [a] -> [b]
> strictLookup l s = f $ lookup s l
>  where
>   f (Just a) = a
>   f _ = error ("Couldn't loopup: " ++ (show s) ++ " in " ++ (show l))


> sep :: a -> [[a]] -> [a]
> sep c (x:[]) = x
> sep c (x:xs) = x ++ [c] ++ (sep c xs)


ghc > 6.4 has it in Data.List

> isInfixOf s (x:xs) = or [ isPrefixOf s xs , isInfixOf s xs]
> isInfixOf s [] = False

better use System.FilePath if available

> pathSeparator = '/'

> splitFileName :: FilePath -> (String, String)
> splitFileName s 
>  | length s == 1 = (s, ".")
>  |  otherwise = (unsplit c a, concat b)
>  where
>    x = split pathSeparator s
>    (a, b) = splitAt (length x - 1) x
>    c = pathSeparator

> splitFileExt :: FilePath -> (String, String)
> splitFileExt s
>  | null x = (s, "")
>  | otherwise = (a ,b)
>  where
>   x = split '.' s
>   y = reverse x
>   b = head y
>   a = sep '.' $ reverse $ tail y

> splitFilePath :: FilePath -> (String, String, String)
> splitFilePath path = (dir,name,ext)
>   where
>     (dir,basename) = splitFileName path
>     (name,ext)     = splitFileExt  basename

