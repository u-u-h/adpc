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



> module Alggen_cgi (

>   main_alggen_cgi

> )where

> import Data.Char
> import CGI
> import CGITools
> import Alggen
> import Compile

> framework   = "alggen.cgiframe.html"
> rcsID       = "$Author: psteffen $$Date: 2006/01/20 13:54:21 $"
> binName     = "adp_Compile"
> fileName    = "Alggen"
> examples    = ["ElMamun","MatrixMult","GlobSim","LocSim","AffineGlobSim","AffineLocSim","Fib"]


> alggen_wrapper env = do
>    inp'      <- return $ convertnl input
>    result    <- if      initCall || exampleCall then return ""  
>                 else if downloadCall            then alggen Merge signature
>                 else                            alggen Merge inp'

>    returnValue <- return $ let
>       ret 
>         -- the returnValue are: (input', err, ret, example,exfile,(downloadCall,downloadLink))
>         | initCall          = (input, "", "", exstring, "", (False, ""))
>         | exampleCall       = ("", "" ,"", exstring, exchoice, (False, ""))
>         | downloadCall      = ("", "", result, exstring, exchoice, downloadOption)
>         | errorline == (-1) = (input, successmsg, result, exstring, "", downloadOption)
>         | otherwise         = (mark (convertnl input) errorline, errmsg, "", exstring, "", (False, ""))
>      in ret

>    return returnValue 

>  where
>    -- error messages currently disabled:
>    errorline = -1

>    initCall     = case (lookup "input" env) of
>                    Just input -> False
>                    Nothing    -> True
>    goCall       = case (lookup "generate" env) of
>                    Just input -> True
>                    Nothing    -> False
>    exampleCall  = case (lookup "example" env) of
>                    Just input -> True
>                    Nothing    -> False
>    downloadCall = case lookup "download" env of
>                    Just _    -> True
>                    Nothing   -> False

>    input    = lookup' "input"      env ""
>    exchoice = lookup' "exchoice"   env (head examples)

>    exstring  = showOptions 1 (getAlgPos exchoice examples) examples

>    signature   = convertnl $ lookup' "input" env ""

>    testData = "> data Bill = Mult Bill Char Bill |" ++
>               "              Add  Bill Char Bill |" ++
>               "              Ext  Bill Char      |" ++
>               "              Val  Char   "

>    errmsg = "<FONT color=\"#ff0000\">Syntax error in line " ++ 
>              (show (errorline - 1)) ++ "! (see marker)</FONT>"
>    successmsg = "(<FONT color=\"#0000ff\">Algebras successful generated." ++ 
>                 " See <a href=\"#output\">output window</a>" ++
>                 ", or " ++ downloadLink ++ ".</FONT>)"

>    -- downloadOption = (downloadCall, downloadLink)
>    downloadOption = (downloadCall, localUsageMessage)
>    downloadLink = "save this <a href=\"/cgi-bin/adp_Compile?cgitype=Alggen&download=local" ++ downloadParam ++
>                   "\">link</a> as \"<i>MyName</i><tt>.lhs</tt>\"."
>    downloadParam = "&input=" ++ ppURL signature

>    localUsageMessage = "<H4 id=\"body_header\">For usage on your local machine:</H4>" ++
>                        "<ul><li>Save the generated files to disk:<BR>" ++
>                        "<ul><li>" ++ downloadLink ++ " " ++
>                        "At some point you will want to edit this file, and type in your own " ++
>                        "evaluation algebra. But you can already play with it as is.<br>" ++
>                        "<li>Download <a href=\"/adp/src/ADPCombinators.lhs\">ADPCombinators.lhs</a><BR>" ++
>                        "</ul></li>" ++
>                        "<li>Call \"<tt>hugs </tt><i>MyName</i><tt>.lhs</tt>\" in a terminal " ++
>                        "window to start the example. You might want to download the Haskell " ++
>                        "interpreter <tt>hugs</tt> from" ++
>                        " <a href=\"http://www.haskell.org/hugs\">http://www.haskell.org/hugs</A>.</li></ul>"

>    ppURL = convertURL . cropSpaces

>    cropSpaces []           = []
>    cropSpaces (' ':' ':xs) = cropSpaces (' ':xs)
>    cropSpaces (x:xs)       = x : cropSpaces xs

>    mark input errorline = unlines(mark' (lines input) 1 (errorline - 1))
>    mark' []     _ _ = []
>    mark' (x:xs) n m = if n==m then (x ++ "     -- << --"):xs else x:mark' xs (n+1) m

>    lookup' name env init = case (lookup name env) of
>             Just x -> x
>             Nothing -> init

> getAlgPos alg algs = head [ n | (n, a) <- zip [1..] algs, alg == a]

> delMarker [] = []
> delMarker ('-':'-':' ':'<':'<':' ':'-':'-':xs) = delMarker xs
> delMarker (x:xs)                               = x:delMarker xs

> -- main_alggen_cgi :: IO ()
> main_alggen_cgi env = 
>   do
>     inp <- readFile ("/data/adp/data/" ++ framework )
>     header  <- readFile ("/data/adp/data/" ++ fileName ++ ".header.html")
>     (input,err,output,exstring,exfile,downloadOption)  <- alggen_wrapper env
>     let (downloadCall, downloadLink) = downloadOption
>     input'  <- if exfile == "" then return (input)
>                                else readFile ("/data/adp/alggen/examples/" ++ exfile ++ ".lhs")
>     inp'  <- return (replace' inp ([(0,input'),(1,err),(2,output),
>                                     (3,exstring),(4,binName),(5,rcsID),
>                                     (6,header),(7,downloadLink)]))
>     if downloadCall then return $ show $ Content{mime = textPlain output }
>                     else return $ show $ Content{mime = prose inp' } 

