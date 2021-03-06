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


> module Option
> where


> import System.Console.GetOpt
> import System.Environment(getArgs, getProgName)
> import System.Exit(exitFailure)
> import Control.Monad
> import System.IO


> getArguments def options = do
>   args <- getArgs
>   progName <- getProgName
>   let (actions, nonOptions, errors) = getOpt Permute options args
>   when (errors /= []) (hPutStrLn stderr 
>     (unlines errors ++ usageInfo progName options) >> exitFailure)
>   let opts = foldl (\a b -> b a) def actions
>   return (opts, nonOptions)
