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



> module CoreTypeCheck where


Dieses Paket laeuft nur unter GHC oder GHCI
(mit dem Aufruf "ghci -cpp -fglasgow-exts -package lang")


> import CoreParser
> import CoreSyntax
> import CoreTools

> import Typecheck.HsSyn
> import CoreTI

> import Typecheck.AnnotatedHsSyn
> import Typecheck.Representation
>     (Scheme)



testfunction

> testTI :: String -> IO (Prog, [(String, Scheme)])
> testTI fileName = do
>     file <- readFile fileName
>     let file = unlit fileName file
>     let (prog, types) = typeCheck file True
>     putStrLn "running testTI"
>     putStrLn $ show types
>     return $ (prog, types)


transforms the program and calculates the types
of the functions

> typeCheck :: String -> Bool -> (Prog, [(String, Scheme)])
> typeCheck inp checkType | checkType = (prog, typeAssoc)
>                         | otherwise = (prog, [])
>     where moduleSyntax = parseHsSource inp
>           prog = parse inp -- transformCore moduleSyntax
>           types = typeCheckHsModule moduleSyntax
>           typeAssoc = map (\ (n, scm) -> (tAHsName n, scm)) types


transformation functions for AnnotatedHsSyn

> tAHsName :: AHsName -> String
> tAHsName (AQual aMod hsIdent) = tAHsIdentifier hsIdent
> tAHsName (AUnQual hsIdent)    = tAHsIdentifier hsIdent

> tAHsIdentifier :: AHsIdentifier -> String
> tAHsIdentifier (AHsIdent name) = name
> tAHsIdentifier (AHsSymbol name) = name
> tAHsIdentifier (AHsSpecial name) = name
