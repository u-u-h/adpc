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



> module CoreConstants where

> rev_CoreConstants =  "$Revision$ "

target languages:

> notl :: Int
> notl = -1

> c:: Int
> c  = 0

> pascal:: Int
> pascal  = 1

> fortran:: Int
> fortran  = 2

> haskell :: Int
> haskell = 3

> latex :: Int
> latex = 4

> pptln :: Output -> String
> pptln C       = "C"
> pptln F       = "Fortran"
> pptln Pascal  = "Pascal"

> data Output = Normal | C | F | Pascal | Latex deriving Eq

verbosity level:

> target     = 0
> trace      = target     + 1
> latextrace = trace      + 1
> tracemore  = latextrace + 1
> debug      = tracemore  + 1
> specialtrace = debug + 1

compile modes:

> imperativeCM = 0
> latexCM      = 1
> optimizeCM   = 2
> tabulateCM   = 3
> binaryCM     = 4

input:

> _inpx = "x"
> _inpy = "y"
> _inpz = "z"

> _inplx = "m"
> _inply = "n"
> _inplz = "n"

> standardDim = 1000 :: Int

table access:

> -- wird verwendet um durch alternative table access schemes ueberfluessig gewordene Tabellendimensionen zu kennzeichnen
> obsoleteTableDim = -1 :: Int

identifier:

> data Prefixes = Prefixes { ptbl, parr, poffset, pfct, palg, pstr, ptstr, pnew, psigid, ppp, pbuild, pfree, pback, pNTID, pupdate :: String} 
>                  deriving Show

> prefixes = Prefixes { 
>   ptbl    = "tbl_",
>   parr    = "arr_",
>   poffset = "offset",
>   pfct    = "calc_",
>   palg    = "alg_",
>   pstr    = "str_",
>   ptstr   = "str",
>   pnew    = "new_",
>   psigid  = "SIGID_",
>   ppp     = "pp_",
>   pbuild  = "build_",
>   pfree   = "free_",
>   pback   = "back_",
>   pNTID   = "_NTID",
>   pupdate = "update_"}

> idFct = ("_id", [])

> _chc_init = [] -- "chc_init"

 --  geht auch:
 p2 = prefixes { ptbl = "tbdfsl_" }

system commands:

> stdCmdRm  = "rm -f"

> stdCmdcc :: Output -> String
> stdCmdcc C       = "gcc"
> stdCmdcc F       = "f90 -fpp -free" 
> stdCmdcc Pascal  = "gpc"
> stdCmdcc _       = "no standard compiler available for target language"

> stdFileext :: Output -> String
> stdFileext C       = ".c"
> stdFileext F       = ".f90" 
> stdFileext Pascal  = ".pas"
> stdFileext _       = "no standard file extension available for target language"

