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


> module Main

> where

> import RandSeq

> import Option

> import System
> import System.Console.GetOpt
> import IO
> import Monad

> data Options = Options {
>   optAlphabet :: [String],
>   optHelp     :: Bool,
>   optLength   :: Int,
>   optSeed     :: Int
> } deriving Show

> defaultOptions = Options { optAlphabet = ["a", "c", "g", "u"],
>                            optHelp = False,
>                            optLength = 0,
>                            optSeed = 1
>                            }

> options :: [ OptDescr (Options -> Options) ]
> options = [
>   Option "h" ["help"] (NoArg (\opt -> opt { optHelp = True }) )
>     "show this nice help screen.",
>   Option "n" ["all"] (NoArg (\opt -> opt {
>            optAlphabet = ["a", "c", "g", "u", "n"]  } ) )
>     "set Alphabet to a, c, g, u, n",
>   Option "a" ["alphabet"] (ReqArg (\s opt ->
>     opt { optAlphabet = words s } ) "a1 a2 ...")
>     "whitespace delimited list of the alphabet",
>   Option "l" ["length"] (ReqArg (\s opt -> 
>     opt { optLength = read s } ) "int") "sequence length",
>   Option "s" ["seed"] (ReqArg (\s opt ->
>     opt { optSeed = read s } ) "int") "random generator seed value" ]


> main = do
>   (opts, args) <- getArguments defaultOptions options
>   progName <- getProgName
>   when (optHelp opts) (putStrLn (usageInfo progName options) 
>                        >> exitWith ExitSuccess)
>   print_list $ rand_array (list2array (optAlphabet opts)) (optLength opts)
>                           (optSeed opts)
