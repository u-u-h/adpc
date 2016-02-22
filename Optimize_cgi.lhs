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



> module Optimize_cgi (

>  main_optimize_cgi

> ) where

> import Char
> import CGI
> import CGITools
> --import SM
> --import Constants
> import Compile

> framework   = "optimize.cgiframe.html"
> rcsID       = {- rcsTransform -} "$Author$$Date$"
> binName     = "Compile"
> examples    = ["ElMamun","MatrixMult","GlobSim","LocSim","AffineGlobSim","AffineLocSim","Fib"]

> optimize_cgi env
>        | initCall          = return (delMarker input, "", "", exstring, "", "checked")
>        | exampleCall       = return ("", "" ,"" ,exstring, exchoice, checked)
>        | otherwise         = do
>                                inp'            <- return $ "grammar[nn]{\n" ++ convertnl input ++ "\n}\n"
>                                (res,errorline) <- cmain (["-z", inp'] ++ if tabulateChoice then ["-cto", "-iuc"] else [])
>                                if errorline == (-1) 
>                                     then return (delMarker input, successmsg, clean res, exstring, "", checked)
>                                     else return (mark (convertnl input) errorline, errmsg errorline, "", exstring, "", checked)
            

>  where
>    initCall    = case (lookup "input" env) of
>                   Just input -> False
>                   Nothing    -> True
>    goCall      = case (lookup "optimize" env) of
>                   Just input -> True
>                   Nothing    -> False
>    exampleCall = case (lookup "example" env) of
>                   Just input -> True
>                   Nothing    -> False

>    input    = lookup' "input"      env ""
>    exchoice = lookup' "exchoice"   env (head examples)
>    tabulateChoice = case (lookup "tabulate" env) of
>                       Just input -> True
>                       Nothing    -> False
>    checked = if tabulateChoice then "checked" else ""

>    exstring  = showOptions 1 (getAlgPos exchoice examples) examples

>    lookup' name env init = case (lookup name env) of
>             Just x -> x
>             Nothing -> init

>    errmsg errorline = "<FONT color=\"#ff0000\">Syntax error in line " ++ 
>                       (show (errorline - 1)) ++ "! (see marker)</FONT>"
>    successmsg = "<FONT color=\"#0000ff\">Optimization successful." ++ 
>               " See <a href=\"#output\">output window</a>.</FONT>"

>    mark input errorline = unlines(mark' (lines input) 1 (errorline - 1))
>     where mark' []     _ _ = []
>           mark' (x:xs) n m = if n==m then (x ++ "     -- << --"):xs else x:mark' xs (n+1) m

>    clean ss = fil2 $ unlines $ filter fil $ arrange "|." $ arrange "<" $ tail $ init $ init $ lines ss
>      where
>        fil [] = True
>        fil ('-':'-':'e':'n':'d':_)     = False
>        fil ('-':'-':' ':'p':'r':'o':_) = False
>        fil (c:cs)                      = fil cs
>        fil2 [] = []
>        fil2 ('\n':'\n':cs)             = '\n' : fil2 cs
>        fil2 (c:cs)                     = c: fil2 cs


> getAlgPos alg algs = head [ n | (n, a) <- zip [1..] algs, alg == a]

> delMarker [] = []
> delMarker ('-':'-':' ':'<':'<':' ':'-':'-':xs) = delMarker xs
> delMarker (x:xs)                               = x:delMarker xs

> -- main_optimize_cgi :: IO ()
> main_optimize_cgi env = 
>   do
>     inp                                          <- readFile ("/data/adp/data/" ++ framework )
>     (input,err,output,exstring,exfile,tabulated) <- optimize_cgi env
>     input' <- if exfile == "" then return (input) else readFile ("/data/adp/compiler/examples/" ++ exfile ++ ".opt")
>     inp'                                         <- return (replace inp ([input',err,output,exstring,binName,rcsID,tabulated]))
>     return $ show (Content{mime = prose inp' })

> arrange :: [Char] -> [String] -> [String]
> arrange sep lines = map arr lines
>   where
>     maxpos = maximum $ map getpos lines
>     getpos l = getpos' 0 l
>     getpos' n []     = 0
>     getpos' n (c:cs) | elem c sep = n
>                      | otherwise  = getpos' (n+1) cs
>     arr l = arr' 0 l
>     arr' _ []        = []
>     arr' n xs@(c:cs) | elem c sep = replicate (maxpos - n) ' ' ++ xs
>                      | otherwise  = c:arr' (n+1) cs
