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



> module LocalDef(

>   DefStruct(..),
>   getDef,
>   ppDefStruct,
>   ppDefStructs,
>   rev_LocalDef,
>   showCnt,
>   updateDef

> ) where

> import Tools

> rev_LocalDef =  "$Revision$"

%----------------------------------------------------------------------------------------------------
\section{Kontextabhängige Filterung der Ergebnisse}
%----------------------------------------------------------------------------------------------------

> data DefStruct a = DefStruct String a [DefStruct a] 
>                                                     deriving (Show, Eq)

> ppDefStruct pp ind (DefStruct n ys yss) = spc ind ++ n ++ " -> " ++ pp ys ++ "\n" ++ 
>                                             concatMap (ppDefStruct pp (ind + 3)) yss
> ppDefStructs pp = concatMap (ppDefStruct pp 0)


> visibleDefs :: [String] -> [DefStruct a] -> [DefStruct a]  
> visibleDefs cnt ys = flatten (visibleDefs' cnt ys)  
>   where
>     visibleDefs' :: [String] -> [DefStruct a] -> [DefStruct a]  
>     visibleDefs' _ []  = []
>     visibleDefs' cnt ((DefStruct n y wys):ys) = sortDefs ((DefStruct n y wys'): visibleDefs' cnt ys) 
>         where  wys' = case cnt of 
>                         []      -> []
>                         (tc:rc) -> if tc==n then visibleDefs' rc wys else []

>     sortDefs :: [DefStruct a] -> [DefStruct a]  
>     sortDefs [] = []
>     sortDefs ((DefStruct n y []):ys)  = sortDefs ys ++ [(DefStruct n y [])]
>     sortDefs ((DefStruct n y wys):ys) = (DefStruct n y wys) : sortDefs ys

>     flatten :: [DefStruct a] -> [DefStruct a]
>     flatten [] = []
>     flatten ((DefStruct n y wys):ys) = flatten wys ++ [(DefStruct n y [])] ++ flatten ys


> getDef :: [String] -> [DefStruct a] -> String -> a
> getDef cnt struct name = head' [c | (DefStruct n c _) <- visibleDefs cnt struct,  n == name] 
>                                ("unknown nonterminal " ++ name ++ showCnt cnt)

> flattenDef :: [DefStruct a] -> [(String, a)]
> flattenDef struct = [(n,c) | (DefStruct n c _) <- struct]

> updateDef :: (Show a) => [String] -> String -> a -> [DefStruct a] -> [DefStruct a]
> updateDef cnt name value [] = []
> updateDef []     name value ((DefStruct d vOrg wds):ds) | name == d = (DefStruct d value wds):ds
>                                                         | otherwise = (DefStruct d vOrg  wds):updateDef [] name value ds
> updateDef (c:cs) name value ((DefStruct d vOrg wds):ds) | c == d    = (DefStruct d vOrg  (updateDef cs name value wds)):ds
>                                                         | otherwise = (DefStruct d vOrg  wds):updateDef (c:cs) name value ds
> updateDef _ _ _ p = pattErr "updateDef" p 

> showCnt n = " in context " ++ sepList "." n

