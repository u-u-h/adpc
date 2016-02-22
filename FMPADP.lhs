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



> module Main(

>--   ExitSuccess,
>   getArgs,
>   getEnv,
>   system,

> ) where


> import System

> import "/homes/psteffen/adp/compile/FMP/FMP"
> import "/homes/psteffen/adp/compile/FMP/FMPTree"
> import "/homes/psteffen/adp/compile/FMP/FMPMpsyntax"
> import "/homes/psteffen/adp/compile/FMP/FMPPicture"
> import "/homes/psteffen/adp/compile/FMP/FMPTypes"

> import Parse
> import Syntax
> import Range
> import Tools
> import Adptrans

> -- call from command line:
> main				:: IO ()
> main				=  getArgs >>= cmain

> cmain [] = putStrLn "usage: fmpadp <input> <output>"
> cmain (inp:out:_) = draw1 inp out


> draw1 f fo = do
>     inp <- readFile f
>     (ufs, _, (_,prods), _, _, _) <- return $ catchEr $ parse inp
>     nts <- return $ collectNTs prods
>     prods <- return $ replTerms nts prods
>     -- (prods, ranges) <- return $ extrRange prods
>     prods <- return $ liftWheres '-' prods
>     drawtree prods fo

> draw2 f = do
>     inp <- readFile f
>     (ufs, _, (_,prods), _, _, _) <- return $ catchEr $ parse inp
>     nts <- return $ collectNTs prods
>     prods <- return $ replTerms nts prods
>     -- (prods, ranges) <- return $ extrRange prods
>     prods <- return $ liftWheres '-' prods
>     drawtree2 prods


> drawtree2 :: [Prod] -> IO ()
> drawtree2 ps = sequence_ (map (\(n,pic) -> generate "Tree" n pic) (zip [1..] (alignProds (map drawProd ps))))


> drawtree :: [Prod] -> String -> IO ()
> drawtree ps fo = generate fo 1 (foldr1 (|==|) (alignProds (map drawProd ps)))

> alignProds ((_,pic1):(True,_):(_,pic2):pics) = [topRow [pic1, space, sepLine, space, pic2]] ++ alignProds pics
>   where 
>     sepLine = toPicture (vec (0,0) .-. vec (0,55))
>     space = hspace 6

> alignProds ((_,pic):pics)                    = pic : alignProds pics
> alignProds []                                = []


    besides ::  IsPicture a => a -> a -> Picture
    besides  a b = overlay [ref ((0::Int) <+ SW) .= ref ((1::Int) <+ NW)] [a,b]


> drawProd ("besides" :===_) = (True, empty)
> drawProd (n :=== (_, _, _, u)) = (False, rowSepByTop 2 [toPicture (ppn n), arr, drawtree'' u])
>    where arr = toPicture $ (fs("$\\rightarrow$" ++ cL))

> -- drawtree'' (Tabulata) = drawtree'' a
> drawtree'' (a :... h)    = drawtree'' a
> drawtree'' a             = sepl (empty, (map drawtreeWith (flat a)), empty)

> drawtreeWith (t@(Terminal (n,_)) `With` f) = (False, toPicture $ ppTerminal n ++ " " ++ drawFilter f)
> drawtreeWith (a `With` f)   = 
>       (False, 
>        topRow ([toPicture $ sepl (optBraceO, 
>                                   map drawtreeWith (flat a), 
>                                   topRow [optBraceC, toPicture $ drawFilter f])]))
>     where
>       (optBraceO, optBraceC) = if length (flat a) > 1 then (toPicture "(", toPicture ")")
>                                                       else (empty,empty)

> drawtreeWith (Terminal ("newline", [])) = (True, empty)
> drawtreeWith a                          = (False, toPicture $ drawtree' a)

> drawFilter (name, args) = fs $ "\\textbf{with} " ++ name ++ " " ++ sepList " " args ++ cL

> sepl (front,xs,back) = foldr1 (|==|) (addOpts (map (map snd) (lines xs)))
>   where

>    lines []   = []
>    lines s    = let (l,s') = break fst s
>                 in l : case s' of []      -> []
>                                   (_:s'') -> lines s''

>    addOpts :: [[Picture]] -> [Picture]
>    addOpts [a] = [topRow $ [front] ++ [sepl' a] ++ [back]]
>    addOpts ls | length ls == 2 = [topRow [front, sepl' (head ls ++ [empty])], topRow [sepl' (last ls), back]]
>               | otherwise      = [topRow [front, sepl' (head ls ++ [empty])]] ++ map sepl' (tail(init ls)) ++ 
>                                                                                 [topRow [sepl' (last ls), back]]

>    sepl' :: [Picture] -> Picture
>    sepl' [x]    = x
>    sepl' (x:xs) = x ||| (fs "$|$") ||| sepl' xs
>      where
>       (|||)				:: (IsPicture a, IsPicture b) => a -> b -> Picture
>       p1 ||| p2			=  topRow [toPicture p1, toPicture p2]

> topRow				:: IsPicture a => [a] -> Picture
> topRow				=  rowSepByTop 0

> midRow				:: IsPicture a => [a] -> Picture
> midRow				=  rowSepByMid 0

> bottomRow				:: IsPicture a => [a] -> Picture
> bottomRow				=  rowSepByBottom 0


> (|==|)			:: (IsPicture a, IsPicture b) => a -> b -> Picture
> p1 |==| p2			=  columnSepByL 8 [toPicture p1, toPicture p2]


> rowSepByTop			:: IsPicture a => Numeric -> [a] -> Picture
> rowSepByTop hSep ps		=  overlay [ ref (i <+ NE) + vec(hSep,0) .= ref (i+1 <+ NW)
>					   | i <- [0..length ps - 2] ]
>					   ps

> rowSepByMid			:: IsPicture a => Numeric -> [a] -> Picture
> rowSepByMid hSep ps		=  overlay [ ref (i <+ E) + vec(hSep,0) .= ref (i+1 <+ W)
>		   			      | i <- [0..length ps - 2] ]
>					      ps


> rowSepByBottom			:: IsPicture a => Numeric -> [a] -> Picture
> rowSepByBottom hSep ps		=  overlay [ ref (i <+ SE) + vec(hSep,0) .= ref (i+1 <+ SW)
>		   			      | i <- [0..length ps - 2] ]
>					      ps

> columnSepByL			:: IsPicture a => Numeric -> [a] -> Picture
> columnSepByL vSep ps		=  overlay [ ref (i <+ SW) - vec(0, vSep) .= ref (i+1 <+ NW)
>					   | i <- [0..length ps - 2] ]
>					   ps


> flat (a :||| b) = flat a ++ flat b
> flat a          = [a]

> -- drawtree' (Tabulated a) = drawtree' a
> -- drawtree' (P a)         = node (ppn a) []

> drawtree' (a :||| b)    = drawtree' a -- (toPicture (drawtree' a)) ||| (toPicture (drawtree' b))

> drawtree' ((f,_) :<<< b)    = node (ppn (treeNode f)) (map (edge.drawtree') (flat b)) where
>                            flat (a :~~~ b) = flat a ++ flat b
>                            flat a          = [a]
> drawtree' (a :... h)     = drawtree' a
> drawtree' (Terminal (t,_))      = node (ppTerminal t) []
> drawtree' (Nonterminal t)       = node (ppn t) []
> drawtree' (a `With` f)      = drawtree' a


> ppTerminal x = fs $ "\\emph{" ++ ppn' x ++ "}" ++ cL
> ppn x =  fs $ ppn' x ++ cL 
> ppn' ""     = ""
> ppn' (x:xs) = (if x=='_' then "\\_" else [x]) ++ ppn' xs

> treeNode (x:xs)= x:xs -- toUpper x:xs
> fs x = "\\footnotesize{" ++ x ++ "}"

> -- center line:
> cL = "\\phantom{pP}\\hspace*{-.43cm}"


