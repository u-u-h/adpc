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


> -- | Transformed hsutils/layout.hs to literate and made layout
> --   a non-monadic function.
> module Layout(layout, stripComments, processImports)
> where

> import L(l)
> import HsLexerPass1
> import HsLayoutPre(layoutPre)
> import List(partition)

available in ghc > 6.4

> import Helpers(pathSeparator)

> layout = concatMap tokenString . 
>   uncurry (mergeBy (cmpBy tokenPos)) .
>   apFst (flip l [] . layoutPre) .
>   partition (notWhite.fst) .
>   lexerPass0
>     where
>       tokenPos = fst . snd
>       tokenString = snd . snd


> stripComments s = unlines $ map g (lines s)
>   where
>     g l@('>':x) = x
>     g _         = ""

> -- | Inlines files, referenced with '#import' statements
> processImports :: String  -- ^ prefix
>                -> String -- ^ searchpath
>                -> String -- ^ filename
>                -> Int -- ^ line number
>                -> String -- ^ file content
>                -> IO String
> processImports prefix path ifile ln "" = return ""
> processImports prefix path ifile ln ('#':'i':'m':'p':'o':'r':'t':xs) = 
>            do
>              (mfile, rest) <- return $ span ((/=) '\n') xs
>              mfile <- return $ head (words mfile)
>              let  syspath = (prefix ++ [pathSeparator] ++ "share" ++
>                              [pathSeparator] ++ "adpc" ++ [pathSeparator] ++
>                               "lib" ++ [pathSeparator] ++ mfile)
>              input <- catch (readFile mfile)
>                       (\_ -> catch (if path == [] then
>                        readFile syspath else
>                        (readFile (path ++ [pathSeparator] ++ mfile)))
>                         (\_ -> catch 
>                          (readFile syspath)
>                            (\_ -> error $ "file " ++ mfile ++ " not found.")))
>              input <- return $ stripComments input
>              processed <- processImports prefix path mfile 1 input
>              rest'     <- processImports prefix path ifile (ln+1) rest
>              return $ "#line " ++ mfile ++ " 0\n" ++ processed ++ 
>                       " #line " ++ ifile ++ " " ++ show (ln-1) ++ "\n" ++ rest'
> processImports prefix path ifile ln ('\n':xs) = do
>                         xs' <- processImports prefix path ifile (ln+1) xs
>                         return ('\n':xs')
> processImports prefix path ifile ln (x:xs) = do
>                         xs' <- processImports prefix path ifile ln xs
>                         return (x:xs')


Utilities

> apFst f (x,y) = (f x,y)
> cmpBy f x y   = f x `compare` f y

> mergeBy cmp [] ys  = ys
> mergeBy cmp xs []  = xs
> mergeBy cmp a@(x:xs) b@(y:ys) = case x `cmp` y of
>                                  GT -> y : mergeBy cmp a ys 
>                                  _  -> x : mergeBy cmp xs b
