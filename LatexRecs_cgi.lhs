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



> module LatexRecs_cgi (

>   main_latexrecs_cgi

> ) where

> import Char
> import CGI
> import CGITools
> import SM
> import Constants
> import Phases
> import Compile

> framework   = "latexrecs.cgiframe.html"
> rcsID       = "$Author$$Date$" 
> tbinName     = "Compile"
> examples    = 
>    [("ElMamun","ElMamun2"),
>     ("MatrixMult","MatrixMult"),
>     ("GlobSim","GlobSim"),
>     ("LocSim","LocSim"),
>     ("AffineGlobSim","AffineGlobSim"),
>     ("AffineLocSim","AffineLocSim"),
>     ("Fib","Fib")]

> fname file = case [ f' | (f,f') <- examples, f == file] of
>               [] -> []
>               [x] -> x

> latexrecs_cgi env = 
>  do
>     let  input  = lookup' "input"      env ""
>     let  axiom  = getAxiom input
>     let  inp'   = "grammar[" ++ axiom ++ "]{\n" ++ convertnl input ++ "\n}\n"
>     let  initCall    = case (lookup "input" env) of
>                      Just input -> False
>                      Nothing    -> True
>     let  exampleCall = case (lookup "example" env) of
>                      Just input -> True
>                      Nothing    -> False

>     (res, errorline) <- if initCall || exampleCall then return ("", -1)
>                                                    else cmain $ ["-l", inp']

>     return $ wrapper env initCall exampleCall input res errorline
>    
>  where
>    getAxiom s = filter (/= ' ') $ tail $ fst $ span (/= '=') s

>    wrapper env initCall exampleCall input res errorline = (input',err,ret,example,exfile) 
>     where

>       goCall      = case (lookup "latexrecs" env) of
>                      Just input -> True
>                      Nothing    -> False
>       exchoice = lookup' "exchoice"   env (fst (head examples))

>       exstring  = showOptions 1 (getAlgPos exchoice (map fst examples)) (map fst examples)


>       -- alte variante:
>       -- (_, (res,errorline)) = runSM ([],-1) (compile_allSM latexCM target nocg notl False "" inp')
>       (input', err, ret, example,exfile)  | initCall          = (delMarker input, "", "", exstring, "")
>                                           | exampleCall       = ("", "" ,"" ,exstring, exchoice)
>                                           | errorline == (-1) = (delMarker input, successmsg, res, exstring, "")
>                                           | otherwise         = (mark (convertnl input) errorline, errmsg, "", exstring, "")
>                        where
>                           errmsg = "<FONT color=\"#ff0000\">Syntax error in line " ++ 
>                                     (show (errorline - 1)) ++ "! (see marker)</FONT>"
>                           successmsg = "<FONT color=\"#0000ff\">Compilation successful." ++ 
>                                        " See <a href=\"#output\">output window</a>.</FONT>"

>                           mark input errorline = unlines(mark' (lines input) 1 (errorline - 1))
>                           mark' []     _ _ = []
>                           mark' (x:xs) n m = if n==m then (x ++ "     -- << --"):xs else x:mark' xs (n+1) m

>    lookup' name env init = case (lookup name env) of
>             Just x -> x
>             Nothing -> init

> getAlgPos alg algs = head [ n | (n, a) <- zip [1..] algs, alg == a]

> delMarker [] = []
> delMarker ('-':'-':' ':'<':'<':' ':'-':'-':xs) = delMarker xs
> delMarker (x:xs)                               = x:delMarker xs

> latexframe "" = ""
> latexframe x = header ++ x ++ trailer where
>   header  = "\\documentclass{article}\n\\usepackage{amsmath}\n\\begin{document}\n\n"
>   trailer = "\\end{document}\n"

> -- main_latexrecs_cgi :: IO ()
> main_latexrecs_cgi env = 
>   do
>     inp                                    <- readFile ("/data/adp/data/" ++ framework )
>     (input,err,output,exstring,exfile)     <- latexrecs_cgi env
>     output'                                <- return(latexframe output)
>     input' <- if exfile == "" then return (input) else readFile ("/data/adp/compiler/examples/" ++ fname exfile ++ ".lhs")
>     inp'                                   <- return (replace inp ([input',err,output',exstring,tbinName,rcsID]))
>     return $ show (Content{mime = prose inp' }) 
