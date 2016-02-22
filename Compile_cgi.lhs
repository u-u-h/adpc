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



> module Compile_cgi(

>  main_compile_cgi

> ) where

> import Char
> import CGI
> import CGITools
> import Compile


> framework   = "compiler.cgiframe.html"
> rcsID       = "$Author$$Date$"
> binName     = "adp_Compile"
> examples    = ["ElMamun", "Fib", "GlobSim", "LocSim", "AffineGlobSim", "AffineLocSim"]


> compiler_wrapper env = returnValues where
>    initCall     = case lookup "grammar" env of
>                    Just _    -> False
>                    Nothing   -> True
>    goCall       = case lookup "compile" env of
>                    Just _    -> True
>                    Nothing   -> False
>    exampleCall  = case lookup "example" env of
>                    Just _    -> True
>                    Nothing   -> False
>    downloadCall = case lookup "download" env of
>                    Just _    -> True
>                    Nothing   -> False

>    grammar   = convertnl $ lookup' "grammar"    env ""
>    axiom     = firstIdent grammar
>    alg_type  = convertnl $ lookup' "alg_type"   env ""
>    alg_name  = firstIdent algebra
>    algebra   = convertnl $ lookup' "algebra"    env ""
>    exchoice  = convertnl $ lookup' "exchoice"   env (head examples)

>    exstring  = showOptions 1 (getAlgPos exchoice examples) examples

>    inp' = "source generated from form:\n" ++
>           "target{\n" ++
>           "char *z;\n" ++
>           "int n;\n" ++
>           "#}\n" ++
>           "\n" ++
>           "main{\n" ++
>           "int main(int argc, char **argv)                \n" ++
>           "{                                              \n" ++
>           "   n=strlen(argv[1]);                          \n" ++
>           "   z = (char *) calloc(n+2, sizeof(char));     \n" ++
>           "   strcpy(z, \" \");                             \n" ++
>           "   strcat(z, argv[1]);                         \n" ++
>           "\n" ++
>           "   mainloop();                                 \n" ++
>           "   exit(0);                                    \n" ++
>           "}                                              \n" ++
>           "#}\n\n" ++

>           "algebratype{\n" ++
>             alg_type ++
>           "}\n\n" ++
>           "algebra[" ++ alg_name ++ "]{\n" ++
>             algebra ++
>           "}\n\n" ++
>           "grammar[" ++ axiom ++ "]{\n" ++
>             grammar ++
>           "\n\n\n}\n"

>    cOptions          = ["-c", inp', "-al", alg_name, "-O", "-ta", "bto", "-lcf"]

>    -- returnValues are ((goCompile, cOptions), successMessage, exampleStr, exampleChoice, downloadCall)
>    returnValues  | initCall               = ("", "", "", (False, []), "", exstring, "", downloadCall)
>                  | exampleCall            = ("", "", "", (False, []), "", exstring, exchoice, downloadCall)
>                  | goCall || downloadCall = (grammar, alg_type, algebra, (goCall, cOptions),
>                                              successmsg, exstring, "", downloadCall)
>                         where
>--                           errmsg = "<FONT color=\"#ff0000\">Syntax error in line " ++ 
>--                                     (show (errorline - 1)) ++ "! (see marker)</FONT>"
>                           successmsg = "<FONT color=\"#0000ff\">C-Code successful generated." ++ 
>                                        " See <a href=\"#output\">output window</a>" ++
>                                        " or download it directly <a href=\"" ++ downloadLink ++ "\">here</a></FONT>"
>                           downloadLink = "/cgi-bin/adp_Compile?cgitype=Compile&download=local" ++ downloadParam
>                           downloadParam = "&grammar=" ++ ppURL grammar ++ "&axiom=" ++ ppURL axiom ++
>                                           "&alg_type=" ++ ppURL alg_type ++ "&alg_name=" ++ ppURL alg_name ++
>                                           "&algebra=" ++ ppURL algebra

>                           ppURL = convertURL . cropSpaces

>                           cropSpaces []           = []
>                           cropSpaces (' ':' ':xs) = cropSpaces (' ':xs)
>                           cropSpaces (x:xs)       = x : cropSpaces xs

>                           mark input errorline = unlines(mark' (lines input) 1 (errorline - 1))
>                           mark' []     _ _ = []
>                           mark' (x:xs) n m = if n==m then (x ++ "     -- << --"):xs else x:mark' xs (n+1) m

>    lookup' name env init = case (lookup name env) of
>             Just x -> x
>             Nothing -> init

>    firstIdent :: String -> String
>    firstIdent [] = []
>    firstIdent (x:xs) = if (isIdentStartChar x) then x:takeWhile isIdentChar xs
>                                                  else firstIdent xs
>       where isIdentStartChar c = isAlpha c || elem c "_"
>             isIdentChar c = isAlphaNum c || elem c "_"


> getAlgPos alg algs = head [ n | (n, a) <- zip [1..] algs, alg == a]


> delMarker [] = []
> delMarker ('-':'-':' ':'<':'<':' ':'-':'-':xs) = delMarker xs
> delMarker (x:xs)                               = x:delMarker xs


> split     :: String -> [String]
> split ""   = []
> split s    = let (l,s') = break ('§'==) s
>              in l : case s' of []      -> []
>                                (_:s'') -> split s''


> -- main_compile_cgi :: IO ()
> main_compile_cgi env = 
>   do
>     inp <- readFile ("/data/adp/data/" ++ framework )
>     cParams@(_, _, _, (goCompile, cOptions), err, exstring, exfile, downloadCall)  <- return $ compiler_wrapper env
>     (grammar, alg_type, algebra, (goCompile, cOptions), err, exstring, exfile, downloadCall)
>           <- if exfile == "" then return cParams
>                              else do exFile  <- readFile ("/data/adp/compiler/examples/" ++ exfile ++ ".exmpl")
>                                      (grammar : alg_type : algebra : _ )  <- return $ split exFile
>                                      return (grammar, alg_type, algebra, (goCompile, cOptions),
>                                              err, exstring, exfile, downloadCall)
>     (output',_)  <- if goCompile || downloadCall  then cmain cOptions
>                                               else return ("", -1)
>     inp'  <- if downloadCall then return output'
>                              else return (replace' inp ([(0,grammar),(1,alg_type),(2,algebra),(3,err),(4,output'),
>                                                        (5,binName),(6,exstring)]))
>     if downloadCall then return $ show $ Content{mime = textPlain output' }
>                     else return $ show $ Content{mime = prose inp' } 



> second :: [String] -> String
> second (_:x:xs) = x
> second _ = ""

> showCompile = do 
>               (output,_) <- cmain testOptions
>               ls         <- return $ lines output
>               putStrLn output


Testcall: cmain ["-c", , "-al", "", "-O", "-ta", "bto", "-lcf"]

> testOptions = ["-c", inpX, "-al", "unit", "-O", "-ta", "bto", "-lcf"]
> inpX = 
>    "Der Compiler liegt unter \n" ++
>    "/homes/psteffen/pk/adpcompile.sparc\n" ++
>    "\n" ++
>    "Compiler-Optionnen anzeigen:\n" ++
>    "adpcompile.sparc -h\n" ++
>    "\n" ++
>    "sehr interessant sind auch die verschiedenen Ausgabestufen:\n" ++
>    "-vl s   zeigt ein paar ausgewaehlte Uebersetzungsschritte an\n" ++
>    "-vl r   etwas mehr\n" ++
>    "-vl rr  noch mehr\n" ++
>    "\n" ++
>    "------------------------------------------------------------\n" ++
>    "Die Quelldatei muss zusaetzlich annotiert werden, damit der Compiler\n" ++
>    "die relevanten Bereiche findet:\n" ++
>    "z.B. \n" ++
>    "--grammar[Axiom]{     (hier auskommentiert)\n" ++
>    "....\n" ++
>    "}\n" ++
>    "fuer den Bereich der Grammatik.\n" ++
>    "\n" ++
>    "------------------------------------------------------------\n" ++
>    "Kompiliert affine.lhs mit Algebra unit:\n" ++
>    "adpcompile.sparc -c affine.lhs -al unit -ta bto -O -o affine.c\n" ++
>    "\n" ++
>    "-al unit gibt an, welche Algebra verwendet werden soll.\n" ++
>    "-ta bto gibt an, dass die Tabellen als Speicherblock gespeichert\n" ++
>    " werden sollen, (b), dreieckig (t) und mit einem Offset als\n" ++
>    " Optimierung (o)\n" ++
>    "-O ist eine Zielcode Optimierung. Einfach mal ohne -O compilieren und\n" ++
>    " die Resultate vergleichen\n" ++
>    "\n" ++
>    "------------------------------------------------------------\n" ++
>    "Erzeugt Backtrace-Code:\n" ++
>    "adpcompile.sparc -c affine.lhs -al unit enum -cs unit -bt s -ta bto -O -o affine.c\n" ++
>    "\n" ++
>    "-bt s erzeugt backtrace-code mit einzelnen Ergebnissen (s)\n" ++
>    "-al unit enum wird fuer den backtrace-Code benoetigt\n" ++
>    "-cs unit erzeugt automatisch eine algebra enum anhand der algebra unit\n" ++
>    "\n" ++
>    "> module AffineLocSim where\n" ++
>    "\n" ++
>    "> import Array\n" ++
>    "> import ADPCombinators\n" ++
>    "> import List\n" ++
>    "\n" ++
>    "target{\n" ++
>    "#define  open    (-15)\n" ++
>    "#define  extend  (-1)\n" ++
>    "char *z;\n" ++
>    "int n;\n" ++
>    "#}\n" ++
>    "\n" ++
>    "main{\n" ++
>    "int main(int argc, char **argv)                \n" ++
>    "{                                              \n" ++
>    "   n=strlen(argv[1]);                          \n" ++
>    "   z = (char *) calloc(n+2, sizeof(char));     \n" ++
>    "   strcpy(z, \" \");                             \n" ++
>    "   strcat(z, argv[1]);                         \n" ++
>    "\n" ++
>    "   mainloop();                                 \n" ++
>    "   exit(0);                                    \n" ++
>    "}                                              \n" ++
>    "#}\n" ++
>    "\n" ++
>    "\n" ++
>    "algebratype{\n" ++
>    "\n" ++
>    "> type AffineLocsim_Algebra alphabet answer = (\n" ++
>    ">   (Int, Int) -> answer,                     -- nil\n" ++
>    ">   alphabet -> answer -> answer,             -- d\n" ++
>    ">   answer -> alphabet -> answer,             -- i\n" ++
>    ">   alphabet -> answer -> alphabet -> answer, -- r\n" ++
>    ">   alphabet -> answer -> answer,             -- dx\n" ++
>    ">   answer -> alphabet -> answer,             -- ix\n" ++
>    ">   answer -> alphabet -> answer,             -- skipr\n" ++
>    ">   alphabet -> answer -> answer,              -- skipl\n" ++
>    ">   [answer] -> [answer]                     -- h\n" ++
>    ">   )\n" ++
>    "\n" ++
>    "}\n" ++
>    "\n" ++
>    "algebra[unit]{\n" ++
>    "\n" ++
>    "> unit :: AffineLocsim_Algebra Char Int\n" ++
>    "> unit = (nil, d, i, r, dx, ix, skip_right, skip_left, h)  where\n" ++
>    ">    nil a   = 0\n" ++
>    ">    d x s   = s + open + extend\n" ++
>    ">    i s y   = s + open + extend\n" ++
>    ">    r a s b = s + if a==b then 4 else -3\n" ++
>    ">    dx x s  = s + extend\n" ++
>    ">    ix s y  = s + extend\n" ++
>    ">    skip_right a b = a\n" ++
>    ">    skip_left  a b = b\n" ++
>    ">    h l     = [maximum l]\n" ++
>    "\n" ++
>    "}\n" ++
>    "\n" ++
>    ">  -- simple definitions for open, extend and w:\n" ++
>    ">    open   = (-15)\n" ++
>    ">    extend = (-1)\n" ++
>    "\n" ++
>    "\n" ++
>    "The yield grammar:\n" ++
>    "\n" ++
>    "> affinelocsim alg f = axiom skipR where\n" ++
>    ">   (nil, d, i, r, dx, ix, h) = alg\n" ++
>    "\n" ++
>    ">   skip_right a b = a\n" ++
>    ">   skip_left  a b = b\n" ++
>    "\n" ++
>    "grammar[skipR]{\n" ++
>    "\n" ++
>    ">   skipR     = skip_right <<<           skipR ~~- achar |||\n" ++
>    ">               skipL                                    ... h\n" ++
>    "\n" ++
>    ">   skipL     = skip_left  <<< achar -~~ skipL           |||\n" ++
>    ">               alignment                                 ... h\n" ++
>    "\n" ++
>    ">   alignment  = tabulated(\n" ++
>    ">                nil <<< astring                        |||\n" ++
>    ">                d   <<< achar  -~~ xDel                |||\n" ++
>    ">                i   <<<            xIns      ~~- achar |||\n" ++
>    ">                r   <<< achar  -~~ alignment ~~- achar ... h)\n" ++
>    "\n" ++
>    ">   xDel      = tabulated (\n" ++
>    ">                alignment              |||\n" ++
>    ">                dx <<< achar  -~~ xDel ... h )\n" ++
>    "\n" ++
>    ">   xIns      = tabulated (\n" ++
>    ">                alignment             |||\n" ++
>    ">                ix <<< xIns ~~- achar ... h )\n" ++
>    "\n" ++
>    "}\n" ++
>    "\n" ++
>    "Bind input:\n" ++
>    "\n" ++
>    ">   z         = mk f\n" ++
>    ">   (_,n)     = bounds z\n" ++
>    ">   achar     = acharSep' z '$'\n" ++
>    ">   tabulated = table n\n" ++
>    ">   axiom     = axiom' n\n" ++
>    "\n"
