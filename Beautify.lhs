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



TODO
-----
- includes an den anfang
- kommentare rausschm.
- grammatik-Problem: der Teil links neben einem Leeryeichen ist auch
  relevant. Falls links von einem leeryeichen ein sep. ist, koennen
  wir das Leerzeichen löschen.

> module Beautify (getBeaPattern,
>                  beautify,
>                  beautifyTL,
>                  rev_Beautify ) where

> import Data.Char
> import Tools
> import TL
> import TLData
> import SM

> import TTCombinators


> rev_Beautify =  "$Revision$"


----------------------------------------------------------------------------------------------------

> getBeaPattern :: (a, Bool, String) -> IO (a, Bool, String)
> getBeaPattern (rn,False,_) = (return (rn,False,""))
> getBeaPattern (rn,True,"") = (return (rn,True,""))
> getBeaPattern (rn,True,f)  = do
>                              inp <- readFile f
>                              (return (rn,True,inp))

> beautify :: (a, Bool, String) -> String -> String
> beautify (_,False,_)       code = code
> beautify (_,True,"")       code = renice code

FIXME: probably unused, mainglob causes serious problems with gcc 6.6

>-- beautify (_,True,pattern)  code = mainglob code pattern

> renice  [] = []
> renice ('#':s) = "\n" ++ define ++ "\n" ++ renice rest where
>   (define,rest) = span (/= '\n') ('#':s)
> renice (' ':' ':s)  = renice (snd $ span (== ' ') s)
> renice ('\n':s) = renice s
> renice (s:ss)   = s: renice ss

----------------------------------------------------------------------------------------------------


The signature:

--> data Alignment = Nil                    |
-->                  D  Char Alignment      |
-->                  SP  Alignment Char     |
-->                  I  Alignment Char      |
-->                  NL  Char Alignment Char |
-->                  R  Char Alignment Char 
-->                                            deriving (Eq, Show)

Algebra type:

--> type Globsim_Algebra alphabet answer = (
-->   answer,                       -- nil
-->   alphabet -> answer -> answer,             -- d
-->   answer -> alphabet -> answer,             -- i
-->   answer -> alphabet -> answer,             -- i
-->   alphabet -> answer -> alphabet -> answer, -- nl
-->   alphabet -> answer -> alphabet -> answer, -- r
-->   [answer] -> [answer]                      -- h
-->   )


Algebra cross product:

--> infix ***
--> (***) :: Eq answer1 =>
-->          Globsim_Algebra alphabet answer1 ->
-->          Globsim_Algebra alphabet [answer2] ->
-->          Globsim_Algebra alphabet (answer1, [answer2])
--> alg1 *** alg2 = (nil, d, sp, i, nl, r, h) where
-->    (nil1, d1, sp1, i1, nl1, r1, h1) = alg1
-->    (nil2, d2, sp2, i2, nl2, r2, h2) = alg2
--> 
-->    nil   = (nil1  , nil2  )
-->    d x (s1,s2)   = (d1 x s1, d2 x s2)
-->    sp (s1,s2)x   = (sp1 s1 x, sp2 s2 x)
-->    i   (s1,s2) y = (i1 s1 y, i2 s2 y)
-->    nl x (s1,s2) y = (nl1 x s1 y, nl2 x s2 y)
-->    r x (s1,s2) y = (r1 x s1 y, r2 x s2 y)
--> 
-->    h xs = [(x1,x2)| x1 <- h1 [ y1 | (y1,y2) <- xs],
-->                     x2 <- [head' ( h2 [ y2 | (y1,y2) <- xs, y1 == x1])]]
-->     where
-->       head' [] = []
-->       head' x  = head x


The yield grammar:

--> globsim alg inpX inpY  = axiom alignment where
-->   (nil, d, sp, i, nl, r, h) = alg

-->   alignment  = tabulated(
-->                 nil ><< empty                            |||
-->                 d  <<< charx ' '  -~~  xDel              |||
-->                 d  <<< charx ' '  -~~  xIns              |||
-->                 r  <<< xbase      -~~  alignment ~~- ybase ... h)

-->   xDel  =  tabulated(
-->             d  <<<  charx ' '  -~~  xDel              |||
-->             r  <<<  sepx -~~ alignment ~~- ybase      ... h) 

-->   xIns  = tabulated(
-->             sp <<<  xIns ~~- ybase                |||
-->             r  <<<  sepx -~~ alignment ~~- ybase  ... h)

-->   sepx  (i,j) =  [mX!j | i+1 == j, elem (mX!j) canSep]


Bind input:

-->   mX = mk inpX

-->   infixl 7  -~~, ~~-
-->   (_, _, xbase, ybase, charx, chary, empty, _, _, (-~~), (~~-), tabulated) 
-->     = bindParserCombinators inpX inpY 

--> prettyprint :: Globsim_Algebra Char String
--> prettyprint = (nil, d, sp, i, nl, r, h) where
-->   nil         = ""
-->   d x l       = l
-->   sp l x      = if x=='\n' then '\n':l else gap:l
-->   i l x       = gap:l
-->   nl c   l '\n' = c:'\n':l

-->   r ' '  l '\n' = '\n':l
-->   r c  l '\n'   = c:'\n':l
-->   r x l y     = x:l
-->   h           = id
-->   gap         = ' '


--> unit :: Globsim_Algebra Char Int
--> unit = (nil, d, sp, i, nl, r, h) where
-->   nil     = 0
-->   d x s   = s + 1
-->   i s y   = s + 1
-->   sp s y  | y == ' '  = s + 1
-->           | y == '\n' = s + 3
-->           | otherwise = s - 1
-->   nl _   s _      = s + 4

-->   r ' '  s ' '    = s + 4
-->   r _    s ' '    = s - 8
-->   r ' '  s '\n'   = s + 4
-->   r _    s '\n'   = s - 1000
-->   r _    s _      = s

-->   h []    = []
-->   h xs    = [maximum xs]


--> mainglob code pattern = globAll (createInput code pattern)


--> createInput code pattern = todo
-->  where
-->    todo     = cr pattern (preProc code)

-->    cr pattern []   = []
-->    cr pattern code = (c, pattern) : cr pattern (rest)
-->       where

-->       (c,rest) = sp 0 code

-->       lc = length pattern - countspc pattern + perc(countspc code) 30
-->       sp n []     = ([],[])
-->       sp n (c:cs) | n<lc       = let(c',cs') = sp (n+1) cs in (c:c', cs') 
-->                   | otherwise  = if elem c canSep then ([c],cs)
-->                                                   else 
-->                                                   let(c',cs') = sp (n+1) cs in (c:c', cs') 
-->       perc x p = (x * p) `div` 100


--> globAll todo = fout $ unlines $ filter (notempty) (map (\(x,y) -> snd $ head' $ globsim (unit *** prettyprint) x y) todo)
-->   where
-->     head' [] = (0,"")
-->     head' x  = head x

-->     fout []  = []
-->     fout ['\n']    = []
-->     fout ('\n':'\n':cs) = fout('\n':cs)
-->     fout ('\n':cs) = if (head (dropWhile (==' ') cs) == '\n') then tail (dropWhile (==' ') cs) 
-->                                                               else '\n':fout cs
-->     fout (c:cs) = c:fout cs

-->     notempty [] = False
-->     notempty s = not ( all (\c -> elem c " \n") s)



--> splitI code []   = []
--> splitI code (p:ps) = (take lc code):splitI (drop lc code) ps
-->      where
-->        lc = length p - countspc p 

--> countspc [] = 0
--> countspc (' ':s) = 1 +countspc s
--> countspc (c:s)    = countspc s

--> remNl [] = []
--> remNl ('\n':cs) = ' ':remNl cs
--> remNl (c:cs)    = c:remNl cs

--> remSpc2 [] = []
--> remSpc2 (' ':' ':cs) = ' ':remSpc2 ( snd (span (==' ') cs))
--> remSpc2 (c:cs)    = c:remSpc2 cs


--> insS [] = []
--> insS (c1:c2:cs) | elem [c1,c2] canSep2 = ' ':c1:c2:' ':insS cs
--> insS (c:cs)     | elem c        canSep = ' ':c:' ':insS cs
--> insS (c:cs) = c : insS cs

--> preProc = remSpc2 . insS . remNl 

--> canSep = ";=*+-(){}[],><?:\n"
--> canSep2 = ["++","--","!=","==","<=",">=","+=","-=","*=","**"]

--> insSFile f = 
-->    do
-->    inp <- readFile f
-->    writeFile (f ++ ".ins") $ preProc inp


----------------------------------------------------------------------------------------------------

> beautifyTL tls = snd $ collectTLs beautifyWorker (0,[]) tls

> newName (n,cll) name | found == [] = ((n+1, (name, newName): cll), newName)
>                      | otherwise   = ((n, cll), head found)
>   where
>     found = [ new | (name', new) <- cll, name' == name]
>     newName = (\n -> (chr ((n `div` 10)+97)) : show (n `mod` 10)) n

> newNames cll []     = (cll, [])
> newNames cll (n:ns) = (cll'', n':ns')
>   where
>     (cll', n')   = newName  cll n
>     (cll'', ns') = newNames cll' ns

 beautifyWorker :: WorkersTL (Int, [(String, String)])

> beautifyWorker = makeWorkersTL beautifyWorker (DO wrkTL, NOP, DO wrkVarDecls, NOP, DO wrkVAccess, DO wrkDataType, DO wrkTypeDecl) where

>   wrkTL cll (TLFD cmt dt na ds1 ds2 tls tl) = (cll5, TLFD cmt dt' na' ds1' ds2' tls' tl')
>                                where
>                                  (cll', na')  = newName cll na
>                                  (cll1, dt')  = wrkDataType   cll'  dt
>                                  (cll2, ds1') = wrkVarDecls   cll1 ds1
>                                  (cll3, ds2') = wrkVarDecls   cll2 ds2
>                                  (cll4, tls') = wrkTLs beautifyWorker       cll3 tls
>                                  (cll5, tl')  = wrkTL         cll4 tl

>   wrkTL cll (TLFA f tls) = (cll', TLFA f' tls')
>                             where 
>                               (cllf, f')   = newName cll f
>                               (cll', tls') = wrkTLs beautifyWorker cllf tls
>   wrkTL cll (TLFor v a b tls) = (cll', TLFor v' a b tls') 
>                             where 
>                               (cllv, v')   = newName cll v
>                               (cll', tls') = wrkTLs beautifyWorker cllv tls

>   wrkTL cll (TLDefines defs) = (cll', TLDefines defs')
>                             where
>                               (cll', defs')        = collectDefines cll defs
>                               collectDefines cll []  = (cll, [])
>                               collectDefines cll ((dt, nm, args, tls):ds) = (cll'', (dt, nm', args, tls'):ds')
>                                 where
>                                   (clln, nm')   = newName cll nm
>                                   (cll',  tls') = wrkTLs beautifyWorker cll tls
>                                   (cll'', ds' ) = collectDefines cll' ds


>   wrkTL cll tl  = collectTL beautifyWorker cll tl



>   wrkDataType cll (StructOf na strct) = (cll', StructOf na' strct') where
>                                              (cllna, na')   = newName cll na
>                                              (cll', strct') = collectStruct  cllna strct
>                                              collectStruct cll []     = (cll, [])
>                                              collectStruct cll ((n,dt):ss) = (cll'', (n', dt'):ss')
>                                                 where
>                                                   (clln, n')   = newName cll n
>                                                   (cll', dt')  = wrkDataType      clln dt
>                                                   (cll'', ss') = collectStruct     cll' ss
>   wrkDataType cll dt = collectDataType beautifyWorker cll dt


>   wrkTypeDecl cll (TypeDef name dt) = (cll', TypeDef name' dt')
>       where 
>        (clln, name') = newName cll name
>        (cll', dt') = wrkDataType clln dt
>   wrkTypeDecl cll (StructDecl name vds) = (cll', StructDecl name' vds')
>       where 
>        (clln, name') = newName cll name
>        (cll', vds') = wrkVarDecls clln vds



>   wrkVarDecls cll [] = (cll, [])
>   wrkVarDecls cll ((na,dt):ds) = (cll'', ((na', dt'): ds'))
>     where
>       (cllna, na') = newNames cll na
>       (cll',  dt') = wrkDataType cllna  dt
>       (cll'', ds') = wrkVarDecls  cll' ds


>   wrkVAccess cll (Direct n) = (cll', Direct n') where (cll', n') = newName cll n
>   wrkVAccess cll (VANonterm n ss) = (cll', VANonterm n' ss) where (cll', n') = newName cll n
>   wrkVAccess cll x                = collectVAccess beautifyWorker cll x
