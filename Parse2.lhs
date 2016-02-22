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



> module Parse2 (
>   parse2
> ) where

> import Parse
> import NewParser
> import Convert
> import ParseMonad
> import ParseTree

> parse2 :: String -> String -> ParseMonad.ParseResult ParseTree.ADPProgram
> parse2 inp filename = NewParser.parse inp (filename, 1)

-- > convert :: String -> String
-- > convert []                                                        = []
-- > convert ('\n':'u':'s':'e':'r':'d':'e':'f':'s':'{':xs)             = "\n#userdefs{" ++ convert xs
-- > convert ('\n':'a':'l':'g':'e':'b':'r':'a':'t':'y':'p':'e':'{':xs) = "\n#algebratype{" ++ convert xs
-- > convert ('\n':'g':'r':'a':'m':'m':'a':'r':'[':xs)                 = "\n#grammar[" ++ convert xs
-- > convert ('\n':'a':'l':'g':'e':'b':'r':'a':'[':xs)                 = "\n#algebra" ++ convert (tail $ snd $ span (/= ']') xs)
-- > convert ('\n':'m':'a':'i':'n':'{':xs)                             = "\n#main{"  ++ convert xs
-- > convert ('\n':'t':'a':'r':'g':'e':'t':'{':xs)                     = "\n#target{" ++ convert xs
-- > convert ('u':'s':'e':'r':'d':'e':'f':'s':'{':xs)                  = "#userdefs{" ++ convert xs
-- > convert ('a':'l':'g':'e':'b':'r':'a':'t':'y':'p':'e':'{':xs)      = "#algebratype{" ++ convert xs
-- > convert ('g':'r':'a':'m':'m':'a':'r':'[':xs)                      = "#grammar[" ++ convert xs
-- > convert ('a':'l':'g':'e':'b':'r':'a':'[':xs)                      = "#algebra" ++ convert (tail $ snd $ span (/= ']') xs)
-- > convert ('m':'a':'i':'n':'{':xs)                                  = "#main{"  ++ convert xs
-- > convert ('t':'a':'r':'g':'e':'t':'{':xs)                          = "#target{" ++ convert xs
-- > convert ('$':xs)                                                  = convert xs
-- > convert (x:xs)                                                    = x:convert xs
