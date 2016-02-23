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



> module TLFrame(

>   getFrame,
>   rev_TLFrame

> ) where

> import Constants   -- globale Konstanten
> import Tools
> import TL
> import TLData
> import Data.Char
> import Track

> rev_TLFrame =  "$Revision$"

> getFrame :: t2 -> TMode -> Output -> t1 -> t -> ([TL], [TL])
> getFrame uc ml tl bt sc = case (ml, tl) of
>               (MST, C)      -> (c_headerST,    c_trailerST)
>               (MTT, C)      -> (c_headerTT,    c_trailerTT)
>               (MST, F)      -> (f_headerST,    f_trailerST)
>               (MTT, F)      -> (f_headerTT,    f_trailerTT)
>               (MST, Pascal) -> (pascal_header, pascal_trailer)

>               (MST, Java)   -> (java_header, java_trailer)
>               (MTT, Java)   -> (java_header, java_trailer)

>   where

>   c_headerST = [TLLayout  (unlines $ [
>       "#include <stdio.h>",
>       "#include <stdlib.h>",
>       "#include <string.h>",
>       "#include <errno.h>",
>       "",
>       "#include \"options.h\"",
>       "#include \"adplib.h\"",
>       ""]),
>       TLMacro "@C_ADDITIONAL_HEADER@",
>       TLLayout (unlines $ ["",
>       "static void *hlp;  /* help pointer */",
>       "static void *hlp2; /* help pointer */",
>       "",
>       "static tsequence *seq;",
>       "static toptions  *opts;",
>       "static char *z;",
>       "static int   n;"])]

>   c_headerTT = c_headerST


>   c_trailerST = [TLBlock
>       [TLLayout "int main_", TLMacro "@MODULENAME@", TLLayout "(toptions *_opts, tsequence *_seq)"],
>       TLLayout (unlines $ ["{",
>       "   opts = _opts;",
>       "   seq  = _seq;",
>       "   z    = _seq->seq - 1;",
>       "   n    = _seq->length;",
>       "",
>       "   adplib_init(opts,seq,&z,&n);",
>       "   result_prettyprint = (char *) myalloc(adp_statmem, 30*n*sizeof(char));"]),
>       TLMacro "@MODULEMAININIT@",
>       TLLayout "   mainloop();",
>       TLMacro "@MODULEMAINFINISH@",
>       TLLayout"}"]


>   c_trailerTT = c_trailerST

>   f_headerST = [ TLLayout $ insertQuotes (
>    "character*70000 :: arg             \n"++
>    "character       :: z(1:70000)      \n"++
>    "integer         :: n               \n")]

>   f_headerTT = [ TLLayout $ insertQuotes (
>    "character*70000 :: arg             \n"++
>    "character       :: x(1:70000)      \n"++
>    "integer         :: m               \n"++
>    "character       :: y(1:70000)      \n"++
>    "integer         :: n               \n")]

>   f_trailerST = [ TLLayout $ insertQuotes (
>    "CALL getarg(1, arg)        \n"++
>    "n=1                        \n"++
>    "DO WHILE(arg(n:n)/=' ')    \n"++
>    "  z(n) = arg(n:n)          \n"++
>    "  n=n+1                    \n"++
>    "END DO                     \n"++
>    "n=n-1                      \n"++
>    "                           \n"++
>    "CALL mainloop()            \n") ]

>   f_trailerTT = [ TLLayout $ insertQuotes (
>    "CALL getarg(1, arg)        \n"++
>    "m=1                        \n"++
>    "DO WHILE(arg(m:m)/=' ')    \n"++
>    "  x(m) = arg(m:m)          \n"++
>    "  m=m+1                    \n"++
>    "END DO                     \n"++
>    "m=m-1                      \n"++
>    "                           \n"++
>    "CALL getarg(2, arg)        \n"++
>    "n=1                        \n"++
>    "DO WHILE(arg(n:n)/=' ')    \n"++
>    "  y(n) = arg(n:n)          \n"++
>    "  n=n+1                    \n"++
>    "END DO                     \n"++
>    "n=n-1                      \n"++
>    "                           \n"++
>    "CALL mainloop()            \n") ]


>   pascal_header = 
>    [TLLayout "\n"]

>   pascal_trailer = 
>    [TLLayout "\n"]

>   java_header = [ TLLayout (unlines $
>        [ "/* Java Header */",
>        "\n"]),
>      TLMacro "@PACKAGE@",
>      TLLayout (unlines $ [
>        "\n",
>        "import ADPC.DoubleTupel;",
>        "import ADPC.IntTupel;",
>        "import ADPC.Traversable;",
>        "import Tree.Node;",
>        "import static ADPC.ADP.*;",
>        "import optparse.Option;\n"]),
>      TLMacro "@HEADER@",
>      TLLayout "\n\n\n",
>      TLBlock [TLLayout "public class Algebra_", TLMacro "@ALGEBRA_NAME@",
>        TLLayout " extends ", TLMacro "@EXTENDS@", TLLayout " {\n"],
>      TLLayout (unlines  ["interface Backtrace {",
>        "  public str1 back(int i, int j, int diff);",
>        "}\n",
>        "interface SetStr1 {",
>        "  void set(str1 foo);",
>        "}\n",
>        "int n;"]),
>      TLMacro "@SEQUENCE@"
>      ]

>   java_trailer = [TLLayout ( unlines $
>      [ "/* Java Trailer */\n",
>        "public void getTree(Node n) {",
>           "backtrace_tree.traverse(n);",
>        "}\n",
>        "void update_str_Signature(str_Signature c, double diff)",
>        "{",
>        "  update_str_Signature(c, (int) diff);",
>        "}",
>        "\n",
>        "void printExit(Exception e) {",
>        "  System.err.println(\"Fatal internal error\");",
>        "  e.printStackTrace();",
>        "  System.err.println(e.getMessage());",
>        "  System.exit(1);",
>        "}\n"]),
>        TLBlock [ TLLayout "@Option (opt = 'a', gnu = \"algebra\", desc = \"toggle selected algebra off/on\",\nmultiple = true, arg = \"",
>          TLMacro "@ALGEBRA_NAME@",
>           TLLayout "\" )\n public boolean enabled = true;"],
>        TLMacro "@IDENT@",
>        TLMacro "@TRAILER@",
>        TLLayout "\n}\n\n"
>      ]


> insertQuotes :: String -> String
> insertQuotes [] = []
> insertQuotes ('@':s) = (chr 34):insertQuotes s
> insertQuotes (c:s)   = c:insertQuotes s


