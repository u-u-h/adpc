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


> module Macro (macroFilter, parseTemplate)
> where

> import Constants(Output)
> import Tools(mapsep)
> import TL
> import PrettyPrint


> macroWorker :: WorkersTL (String->String)
> macroWorker = makeWorkersTL macroWorker (DO wrkTL, NOP, NOP, NOP, NOP, NOP, NOP) where
>   wrkTL cll (TLMacro text) = (cll, TLLayout $ replace text)
>      where replace = cll
>   wrkTL cll x             = collectTL  macroWorker cll x

> macroFilter replace list = snd $ collectTLs macroWorker replace list

> parseTemplate :: String -> [ (String, String) ]
> parseTemplate s = parseFirst s
>  where
>   parseFirst ('@':xs) = (('@':as) ++ "@", reverse left):(parseFirst right)
>    where
>     (as, _:bs) = span (/= '@') xs
>     (left, right) = parseSecond bs
>   parseFirst (_:rest) = parseFirst rest
>   parseFirst [] = []

>   parseSecond ('%':'{':rest) = parseStop [] rest
>   parseSecond (_:rest) = parseSecond rest
>   parseSecond [] = error "Parse Error: No %{ found"

>   parseStop sofar ('%':'}':rest) = (sofar, rest)
>   parseStop sofar (s:'%':'}':rest) = (s:sofar, rest)
>   parseStop sofar (s:rest) = parseStop (s:sofar) rest
>   parseStop sofar [] = error $ "Parse Error: missing %} after" ++ 
>     reverse sofar


substitutes @FOO@ style macros in a String

fixme well, this is really a design error (using @macros@ in the adpcompile
output, thus eliminate the need for this! 

I.e. the need to pseudo-parse the output is not good.

=> fixed: see above

