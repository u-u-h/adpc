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



> module Track where

> import Tools
> import Parse
> import Syntax
> import PrettyPrint

> rev_Track =  "$Revision$"

Single-/Two-Track
------------------

> data TMode = MST | MTT   deriving (Show, Eq)
> type TModes = Assoc Nt TMode
> getTMode assoc n = lookupA1 assoc n "TMode"

> instance Pretty TMode where
>     pretty = ppTMode

> ppTMode MST  = "single track"
> ppTMode MTT  = "two track"
> ppTModes :: TModes -> String
> ppTModes = ppAssoc ppTMode
> ppTModesElem :: (Nt, TMode) -> String
> ppTModesElem = ppAssocElem ppTMode

> maxTM _ MTT = MTT
> maxTM MTT _ = MTT
> maxTM _   _ = MST 

Track mode analysis
---------------------

> tmaProds :: [String] -> [Prod] -> TModes
> tmaProds crfilter ps = (fixit tmaProd init count ps $ "Track mode analysis does not terminate!\n" ++ 
>                                                       "Probably your input grammar is incorrect.\n")
>                      ++ map (\s -> ("contains_" ++ s, MST)) crfilter
>   where
>     init  = initTM ps
>     count = length ps

>     initTM ::  [Prod] -> TModes
>     initTM [] = []
>     initTM ((n :=== (_, _, _, _)):ps) = (n,MST) : initTM ps 

>     tmaProd :: TModes -> Prod -> (Nt, TMode)
>     tmaProd tmodes (n :=== (_, _, _, u)) = (n, tma_u tmodes u)

> tma_u :: TModes -> Unit -> TMode
> tma_u tmodes (Nonterminal nt)    = tm_nt
>    where  tm_nt = getTMode tmodes nt 
> tma_u tmodes (TTUnit  _ _)          = MTT
> tma_u params x                      = wrkUnit (tma_u params) maxTM MST x

extract maximal track mode from TModes:
------------------------------------------

> maxTrackMode :: TModes -> TMode
> maxTrackMode [] = MST
> maxTrackMode ((_,mode):ps) = maxTM mode (maxTrackMode ps)


