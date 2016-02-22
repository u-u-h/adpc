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



> module CoreSyntax where
> import Char
> import CoreTools

Core expressions:
---------------------

> data Core 
>      = Var String
>      | Num Int
>      | Value Value
>      | Ap Core Core
>      | Lc [Core]
>      | LcLet [Def]         -- Let, welches nur in Lc-Definitionen vorkommt
>      | Constr String 
>      | Arg :<- Core
>      | Core :.. Core
>      | Let [Def] Core
>      | Lambda [Arg] Core
>      | If Core Core Core   -- da wir kein Case unterstuetzen, gibt es wenigstens if
>                            deriving (Show, Eq)

> data Value = VFloat Float
>            | VChar Char
>            | VString String deriving (Show, Eq)

> type Def = (String, [Arg], Core)
> data Arg = ArgVar String
>          | ArgTuple [Arg]
>          | ArgWildCard     -- wird nur in Tupelbindungen unterstuetzt, und noch vor der ersten Nutzung entfernt
>                            deriving (Show, Eq)

> type Prog = [Def]

Prettyprinter:
-------------------

> stdInd = 5 :: Int

> ppCore = ppCore' 0

> ppCore' ind (Var v) | elem v binOps  = "(" ++ v ++ ")"
>                     | otherwise      = v
> ppCore' ind (Constr c) = c
> ppCore' ind (Value (VFloat f)) = show f
> ppCore' ind (Value (VChar ch)) = "'" ++ ch : "'"
> ppCore' ind (Value (VString s)) = "\"" ++ s ++ "\""
> ppCore' ind (Num n) | n < 0     = "(" ++ show n ++ ")"
>                     | otherwise = show n
> ppCore' ind (Lc [])        = "[]"
> ppCore' ind (Lc [c])       = "[ " ++ ppCore' ind c ++ " ]"
> ppCore' ind (Lc (c:cs:[])) = "[ " ++ ppCore' ind c ++ " | " ++ ppCore' ind cs ++ " ]"
> ppCore' ind (Lc (c:cs))    = let header = "[ " ++ ppCore' ind c ++ " | \n" 
>                                  lh = length header
>                              in  header ++ spc (ind + stdInd) ++ mapsep (",\n" ++ spc (ind + stdInd)) (ppCore' (ind + stdInd)) cs ++ " ]"
> ppCore' ind (c1 :<- c2) = let header = ppArg c1 ++ " <- " 
>                           in  header ++ ppCore' (ind + stdInd) c2
> ppCore' ind (c1 :.. c2) = "[ " ++ ppCore' ind c1 ++ " .. " ++ ppCore' ind c2 ++ " ]"
> ppCore' ind (LcLet decls) = "let " ++ mapsep (",\n" ++ spc (ind + stdInd)) ppDef decls
> ppCore' ind (Let defs core) = "let\n" ++ concatMap (ppDef (ind + stdInd)) defs ++ 
>                                spc ind ++ "in " ++ ppCore' ind core
>    where ppDef ind (n, args, c) = spc ind ++ n ++ " " ++ mapsep " " ppArg args ++ " = " ++ ppCore' ind c ++ "\n"
> ppCore' ind (Lambda args core)  = "\\" ++ mapsep " " ppArg args ++ " -> " ++ ppCore' ind core
> ppCore' ind (If c t e)          = "if " ++ ppCore' ind c ++ " then " ++ ppCore' ind t ++ " else " ++ ppCore' ind e
> ppCore' ind (Ap (Ap (Var f) e1) e2) | elem f binOps = optPar f "(" ++ ppCore' ind e1 ++ optNL f ++ ppCore' ind e2 ++ optPar f ")"
>    where optNL "++"               = " ++ " -- "\n" ++ spc ind ++ "++\n" ++ spc ind
>          optNL (x:xs) | isAlpha x = " " ++ x:xs ++ " "
>          optNL x                  = x
>          optPar "++" _ = ""
>          optPar _    p = p

> ppCore' ind x           = let (r, args) = findRedex x []
>                           in case r of
>                               (Constr "(,)") -> "(" ++ mapsep ", " (ppCore' ind) args ++ ")"
>                               otherwise      -> "(" ++ ppOps ppCore' ind r ++ " " ++ mapsep " " ppCoreArg args ++ ")"

>     where
>       ppCoreArg a@(Ap _ _) = "(" ++ ppCore' ind a ++ ")"
>       ppCoreArg a          =        ppCore' ind a
>       ppOps pp ind (Var v) | elem v binOps = "(" ++ pp ind (Var v) ++ ")"
>       ppOps pp ind r                       = pp ind r


> binOps = ["+","-","*","/","==","/=","<=",">=","<",">","!","$","!!","<<<","><<","~~~","+~+","+~~","~~+","~~","-~~","-~-","~~-","*~*","*~~","~~*","++","with","...","|||","&&","||"]



> ppArg (ArgVar "i[]")  = "i"
> ppArg (ArgVar "j[]")  = "j"
> ppArg (ArgVar "i2[]") = "i2"
> ppArg (ArgVar "j2[]") = "j2"
> ppArg (ArgVar v)     = v
> ppArg (ArgTuple args) = "(" ++ mapsep ", " ppArg args ++ ")"
> ppArg ArgWildCard = "_"

> ppDef (n, args, c) = name ++ " " ++ mapsep " " ppArg args ++ " = " ++ ppCore c
>     where name | elem n binOps  = if n == "with" then n
>                                                  else "(" ++ n ++ ")"
>                | otherwise      = n
> ppProg defs = unlines $ map ppDef defs

> ppStrings = unlines

> ppProgHaskell tab tabArgs defs = indent ">" 3 $ unlines $ map ppDefH defs
>   where
>     ppDefH d@(n, args, c) | elem n tab = n ++ " = tabulated(\\" ++ ppCore (renameVars (justVal tabArgs n)) ++ " ->\n" ++ indent "" 3 (ppCore c ++ ")")
>                           | otherwise  = ppDef d
>     indent c n s = unlines $ map ((++) (c ++ spc n)) (lines s)


Configuration
---------------

> spcId = '['
> spcId2 = "]"
> makeVarName name n = name ++ [spcId] ++ n ++ spcId2


Rename Variables:
------------------

> renameVars = snd . rename (1,1,[]) 
>   where
>     rename cll@(n,k,nm) (Var v) 
>                             | v == "i[]"                    =                           (cll,                     Var "i")
>                             | v == "j[]"                    =                           (cll,                     Var "j")
>                             | v == "i2[]"                   =                           (cll,                     Var "i2")
>                             | v == "j2[]"                   =                           (cll,                     Var "j2")
>                             | elem spcId v && elemVal nm v  =                           (cll,                     Var $ justVal nm v)
>                             | elem spcId v && head v == 'k' = let v' = "k" ++ show k in ((n,   k+1, (v, v'): nm), Var v')
>                             | elem spcId v && head v == 'l' = let v' = "k" ++ show k in ((n,   k+1, (v, v'): nm), Var v')
>                             | elem spcId v                  = let v' = "v" ++ show n in ((n+1, k,   (v, v'): nm), Var v')
>                             | otherwise                     =                           (cll,                     Var v)
>     rename cll x       = collectCore rename cll x



Structural recursion:
--------------------------

> mapCore wrk x = snd $ collectCore wrk' 0 x where
>    wrk' cll x = (cll, wrk x)

> mapCores wrk = map (mapCore wrk)

> collectCores wrk cll [] = (cll, [])
> collectCores wrk cll (c:cs) = (cll'', c':cs')
>       where (cll',  c')  = wrk cll c
>             (cll'', cs') = collectCores wrk cll' cs

> collectCoreOp wrk cll op c1 c2 = (cll'', op c1' c2')
>       where (cll',  c1') = wrk cll  c1
>             (cll'', c2') = wrk cll' c2

> collectCore wrk cll e@(Var _)   = (cll, e)
> collectCore wrk cll e@(Num _)   = (cll, e)
> collectCore wrk cll e@(Value _) = (cll, e)
> collectCore wrk cll (Ap c1 c2) = collectCoreOp wrk cll Ap c1 c2 
> collectCore wrk cll (Lc (c1:c2)) = (cll'', Lc (c1':c2'))
>       where (cll',  c1') = wrk cll  c1
>             (cll'', c2') = collectCores wrk cll' c2
> collectCore wrk cll (c1 :<- c2) = (cll'', (varToArgVar c1') :<- c2')
>       where (cll',  c1') = wrk cll (argVarToVar c1)
>             (cll'', c2') = wrk cll' c2
> collectCore wrk cll (c1 :.. c2) = collectCoreOp wrk cll (:..) c1 c2 
> collectCore wrk cll (Constr s)   = (cll, Constr s)
> collectCore wrk cll (Let defs c) = (cll'', Let defs' c')
>       where (cll',  defs') = collectDefs cll defs
>             (cll'', c')    = wrk cll' c
>             collectDefs cll [] = (cll, [])
>             collectDefs cll (d:ds) = (cll'', d':ds')
>                  where (cll', d')   = collectDef  cll d
>                        (cll'', ds') = collectDefs cll' ds
>             collectDef cll (n, args, core) = (cll'', (n, map varToArgVar args', core'))
>                  where (cll', args')  = collectCores wrk cll (map argVarToVar args)
>                        (cll'', core') = wrk cll' core
> collectCore wrk cll (Lambda args core) = (cll'', Lambda (map varToArgVar args') core')
>       where (cll', args')  = collectCores wrk cll (map argVarToVar args)
>             (cll'', core') = wrk cll' core          
> collectCore wrk cll (If c t e)  = (cll3, If c' t' e')
>       where (cll1, c') = wrk cll  c 
>             (cll2, t') = wrk cll1 t 
>             (cll3, e') = wrk cll2 e 

> collectCore wrk cll x           = pattErr "collectCore" x


> argVarToVar (ArgVar v) = Var v
> argVarToVar x          = pattErr "argVarToVar" x
> varToArgVar (Var v)    = ArgVar v
> varToArgVar x          = pattErr "varToArgVar" x

Tools
-------

> -- generate core expressions:
> appl f [] = Var f
> appl f x  = Ap (appl f (init x)) (last x)
> applv f x = appl f (map Var x)
> applc f [] = f
> applc f x  = Ap (applc f (init x)) (last x)


> genP f (e:es) = genP' es (appl "<<<" [Var f, Var e])
>   where
>     genP' []     res = res
>     genP' (e:es) res = genP' es (appl "~~~" [res,Var e])

> genAlt [e] = e
> genAlt (e:es) = appl "|||" [e, (genAlt es)]

> -- Find Redex and collect argument nodes:
> findRedex (Ap e a) args = findRedex e (a:args)
> findRedex x        args = (x,args)

> makeAnd [x]    = x
> makeAnd (x:xs) = Ap (Ap (Var "&&") x) (makeAnd xs)

> makeOr [x]    = x
> makeOr (x:xs) = Ap (Ap (Var "||") x) (makeOr xs)


