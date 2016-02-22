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

Option handling follows
http://groups.google.de/group/fa.haskell/browse_thread/thread/85f157f58d7cd122/48f8140f7c216c32
High-level technique for program options handling, Zielonka, 2004

> where

> import System
> import System.Console.GetOpt
> import Monad
> import IO


> data Options = Options {
>                          optPrefix  :: String,
>                          optVerbose :: Bool,
>                          optHelp    :: Bool
>                        } deriving Show

> defaultOptions = Options { optPrefix = "/vol/adpc", optVerbose = False,
>                            optHelp = True }

> options :: [ OptDescr (Options -> Options) ]
> options = [
>   Option "p" ["prefix"] 
>     (ReqArg (\arg opt -> opt { optPrefix = arg, optHelp = False }) "PATH")
>   "path prefix, which the installer should use for ADPC",
>   Option "h" ["help"] (NoArg (\opt -> opt { optHelp = True }) )
>   "show this nice help screen."
>   ]

> main :: IO ()
> main = do
>   args <- getArgs
>   progName <- getProgName
>   let (actions, nonOptions, errors) = getOpt Permute options args
>   when (errors /= []) (hPutStrLn stderr 
>     (unlines errors ++ usageInfo progName options) >> exitFailure)
>   let opts = foldl (\a b -> b a) defaultOptions actions
>   when (optHelp opts) (putStrLn (usageInfo progName options) 
>                        >> exitWith ExitSuccess)

>   writeConfig opts
>   writeMakefile opts

   putStrLn (show opts ++ " ++ " ++ show errors ++ show args)


> writeConfig :: Options -> IO ()
> writeConfig opts = writeFile "ConfigOutput.lhs" ("> module ConfigOutput\n" ++
>   "> where\n> prefix = \"" ++ optPrefix opts ++ "\"\n")

> writeMakefile opts = writeFile "config.mf" ("PREFIX = " ++ optPrefix opts ++ "\n")


foo a [] = a
foo a (b:bs) = foo (b a) bs

equals

foldl (\x y -> y x) a (b:bs)
