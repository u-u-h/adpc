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


> module Sed(


> sedString, systemReport, makeSedSubst, VarReplace

> )where

> import System(system)

> type VarReplace = [(String, String)]

> sedString :: String -> VarReplace -> String -> String
> sedString _  _     [] = []
> sedString file repl  ('@':xs) = let (var,rest)  = span (/='@') xs
>                                     isVar  = case rest of
>                                                '@':_     -> not (elem '\n' var)
>                                                otherwise -> False
>                                     result | isVar = case (lookup var repl) of
>                                                  Just s    ->        s   ++        sedString file repl (tail rest)
>                                                  otherwise -> error $ "@" ++ var ++ "@ in file " ++
>                                                                       file ++ " not defined."
>                                            | otherwise  =     "@"             ++ sedString file repl xs
>                                 in result
> 
> sedString file repl  (x:xs) = x : sedString file repl xs

> sedScript sedfile infile outfile = do
>     systemReport $ "sed -f " ++ sedfile ++ " < " ++ infile ++ " > " ++ outfile

> sedScriptInplace sedfile infile = do
>     sedScript sedfile infile "adpc_temp_sed"
>     systemReport $ "mv adpc_temp_sed " ++ infile

> sedExpr expr infile outfile = do
>     systemReport $ "sed " ++ expr ++ " < " ++ infile ++ " > " ++ outfile

> sedExprInplace expr infile = do
>     sedExpr expr infile "adpc_temp_sed"
>     systemReport $ "mv adpc_temp_sed " ++ infile


> makeSedSubst a b = "s/@" ++ a ++ "@/" ++ convert b ++ "/g"

> makeSedExpr vars = concatMap (\(a,b) -> "-e '" ++ makeSedSubst a b ++ "' ") vars
> replaceVars vars infile outfile = do
>       sedExpr (makeSedExpr vars) infile outfile
>       systemReport $ "cat " ++ outfile

> replaceVarsInplace vars infile = do
>       sedExpr (makeSedExpr vars) infile "adpc_temp_sed"
>       systemReport $ "mv adpc_temp_sed " ++ infile
>       systemReport $ "cat " ++ infile




> convert s = concatMap conv s
>   where
>     conv c = case lookup c sedchars of
>                     Just s -> s
>                     Nothing -> [c]

>     sedchars = [('/',"\\/"), ('\n', "\\\n")]


Tests
============================================================

> testSedString :: VarReplace -> String -> IO ()
> testSedString repl file = do
>                           inp <- readFile file
>                           putStr $ sedString "" repl inp

> testSedStringFile :: VarReplace -> String -> String -> IO ()
> testSedStringFile repl file outfile = do
>                           inp <- readFile file
>                           writeFile outfile $ sedString "" repl inp


> makefile :: VarReplace
> makefile = [("PROGNAME","Elm\namun"), ("PREFIX","/vol/adpc")]


> suboptmodule :: VarReplace
> suboptmodule = [
>    ("OUTPUTOPTIMAL"    , "printf(\"%d\\n\", result_score);"),
>    ("OUTPUTSUBOPTIMAL" , "printf(\"%d %s\\n\", result_score, result_prettyprint);"),
>    ("MODULENAME"       , "elamun_seller"),
>    ("MODULEMAININIT"   , ""), 
>    ("MODULEMAINFINISH" , "")]




Tools
============================================================

> systemReport s = do
>     putStrLn $ "system call: " ++ s
>     system s



Doku
============================================================

sed -e 's/a/A/' -e 's/b/B/' <old >new
