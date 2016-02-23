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


> import Data.Char
> import System.Environment

> main				:: IO ()
> main				=  do
>                                  args <- getArgs 
>                                  cmain args
>                                  return ()

> cmain ["1", fi, fo]  = convert '*' fi fo
> cmain ["1", fi]      = convert '*' fi "stdout"
> cmain ["1-", fi, fo] = convert '-' fi fo
> cmain ["1-", fi]     = convert '-' fi "stdout"
> cmain ["1:", fi, fo] = convert ':' fi fo
> cmain ["1:", fi]     = convert ':' fi "stdout"
> cmain ["2", fi, fo, format] = prepod fi fo format
> cmain _        = putStrLn $ "call: man2c <mode> <input file> <output file> [format]\n" ++
>                             "  mode 1: man2c,  mode 2: prepod"

> convert opchar f fo = do
>                       inp <- readFile f
>                       if fo == "stdout" then putStrLn $ conv opchar inp
>                        else              writeFile fo $ conv opchar inp               

> conv '*' s =
>    "if (manoptmode == '-')\n" ++ conv '-' s ++
>    "else\n"                   ++ conv ':' s 

> conv opchar s = "> module Man where\n\n> showLongHelp manopt = case manopt of \n" ++
>             concatMap pp (opt opchar s) ++
>             (if opchar == '-' then 
>                   ">  x -> error $ \"unknown command line option -\" ++ x"  
>              else "   default: fprintf(stderr, \"unknown command :%c\\n\", manopt);\n}\n")
>  where
>    pp (o, text) = ">  \"" ++ o ++ "\" -> " ++ " \"" ++ [opchar] ++ o 
>                   ++ text ++ (if last text == 'n' && last (init text) == '\\' then "" else "\\n") ++ 
>                   "\"\n \n"

> opt opchar ('O':'P':'T':'I':'O':'N':'S':rest) = getopt opchar ('\n':rest)
> opt opchar ('\n':'O':'P':'T':'I':'O':'N':'S':rest) = getopt opchar ('\n':rest)
> opt opchar (x:xs)                                  = opt opchar xs
> opt opchar []                                      = []

> getopt opchar ('\n':'\n':' ':' ':' ':' ':oc:rest) | oc == opchar = (o, reverse text) : getopt opchar rest'
>  where
>    (o, rest1) = span (/= ' ') rest 
>    (text, rest') = gt "" rest1

>    gt text rest@('\n':'\n':' ':' ':' ':' ':'-':_) = (text, rest)
>    gt text rest@('\n':'\n':' ':' ':' ':' ':':':_) = (text, rest)
>    gt text ('\n':c:cs) | isAlpha c                = (text, "")
>    gt text ('\n':' ':' ':c:cs) | isAlpha c        = gt text ('\n' : snd (span (/= '\n') cs))
>    gt text rest@('\n':' ':' ':' ':' ':c:cs)       = gt (c:'n':'\\':text) cs
>    gt text ('%':cs)                               = gt ('%':'%':text) cs
>    gt text ('"':cs)                               = gt ('"':'\\':text) cs
>    gt text ('\n':cs)                              = gt ('n':'\\':text) cs
>    gt text ('\\':'n':cs)                          = gt ('n':'\\':'\\':text) cs
>    gt text ('\\':'t':cs)                          = gt ('t':'\\':'\\':text) cs
>    gt text ('\\':'e':cs)                          = gt ('e':'\\':'\\':text) cs
>    gt text (c:cs)                                 = gt (c:text) cs
>    gt text []                                     = (text,[])

> getopt opchar rest@('\n':' ':' ':' ':' ':oc:_:_) | oc == opchar = getopt opchar ('\n':rest)
> getopt opchar (x:xs) = getopt opchar xs
> getopt _ []          = []

----------------------------------------------------------------------------------------------------

> prepod f fo format = do
>                inp <- readFile f
>                if fo == "stdout" then putStrLn $ prep inp format
>                 else                writeFile fo $ prep inp format          

> prep inp format = unlines $ pp True (lines inp)
>   where
>     pp _ []         = []
>     pp printit (l:ls) | ismodeCommand l = pp (ismode l format) ls
>                       | isendCommand  l = pp True ls
>                       | printit         = l : pp printit ls
>                       | otherwise       = pp printit ls

> ismodeCommand ('=':'m':'o':'d':'e':_) = True
> ismodeCommand _                       = False

> isendCommand ('=':'e':'n':'d':'m':'o':'d':'e':_)  = True
> isendCommand _                                    = False

> ismode ('=':'m':'o':'d':'e':rest) format | head (head (words rest)) == '!' &&
>                                            tail (head (words rest)) == format = False
>                                          | head (head (words rest)) == '!'    = True
>                                          | otherwise  = elem format (words rest)
>                                                  
