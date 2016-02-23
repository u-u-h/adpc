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



> module LatexRecs(

>   pString,
>   ppILProdstex,
>   ppString,
>   rev_LatexRecs

> ) where

> import Data.Char
> import Tools
> import Constants
> import MathExp
> import StringLib
> import Syntax
> import TLData
> import Dss
> import Expr
> import IL2
> import PrettyPrint

> rev_LatexRecs =  "$Revision$"

TODO:
-------
+ ++ nach rechts
+ einrueckungen ueber qquad, nicht ueber &   && &&
+ terminale Parser inlinen
+ filter
+ $ und andere Sonderzeichen
+ yield size abfrage (j-i) <= 1 klammern weg
+ schleifenvariablen ki --> k_i
+ wenn moeglich, umbruch bei zu langen zeilen
- unterscheidung zwischen achar und acharsep
+ term. Parser + Filter in allen Variationen testen --> diplom/termtest.lhs
- Problem bei nichttabellierten rekursiven Produktionen --> generelles Problem
+ char mgl. ueber ||
- wesentlich aufraeumen
- LatexRecs_cgi aufraeumen
- Phases aufraeumen

> noResult' = "[~]"

> ppChar inp j       = inp ++ "_{" ++ prettyLang Latex j ++ "}"
> ppString inp (i,j) = inp ++ "_{" ++ (prettyLang Latex $ calcME (i :+ (Number 1))) ++ "} \\dots " ++ inp ++ "_{" ++ prettyLang Latex j ++ "}"
> ppRegion (i,j)     = "(" ++ prettyLang Latex i ++ "," ++ prettyLang Latex j ++ ")"
> ppLoc j            = prettyLang Latex j


> ppTermCode (TCEmpty)           = "()"
> ppTermCode (TCChar c j)        = ppChar c j
> ppTermCode (TCIFChar c _ _)    = quoteChar c
> ppTermCode (TCIFNotChar _ c j) = ppChar c j
> ppTermCode (TCRegion _ (i,j))  = ppRegion (i,j)
> ppTermCode (TCIFString s _ _)  = quoteString s
> ppTermCode (TCLoc j)           = ppLoc j
> ppTermCode x = pattErr "ppTermCode" x

> ppTermCondition (TCIFChar    c s j)      = [ppChar s j ++ " = " ++ quoteChar c]
> ppTermCondition (TCIFNotChar c s j)      = [ppChar s j ++ " \\neq "   ++ quoteChar c]
> ppTermCondition (TCIFString s inp (i,j)) = [ppString inp (i,j) ++ " = " ++ quoteString s]
> ppTermCondition _                        = []


> quoteChar c   = "\\mathnormal{\'" ++ "\\symbol{" ++ show(ord c)++ "}" ++ "\'}"
> quoteString s = "\\text{\"" ++ s ++"\"}"

> newLine' = " \\nonumber \\\\\n& "
> idents ind = concat(replicate ind indString)

Config:
-----------------------------

> separateEnvs = False
> maxLineCount = (140::Int)
> indString    = "\\qquad "
> indCount     = (5::Int)
> appString    = " +\\!\\!+ "
> linebreakString = "" -- " <cont> "
> pString = "p" -- "\\xi"
> fString = "f" -- "\\gamma"

-------------------------------

> ppILProdstex :: UserFunctions -> [Nt] -> [ILProd] -> String
> ppILProdstex ufs deps ps 
>                     | not separateEnvs = 
>                              "\\begin{align}\n& " ++ loops ++ 
>                              sepList (newLine' ++ newLine') (map (ppILProdtex ufs 2) psSorted) ++ 
>                              "\\nonumber\n\\end{align}\n"
>                     | separateEnvs  = concatMap pp psSorted
>                     where 
>                       loops = "\\text{for } j=0 \\text{ to } l \\text{ do}" ++ newLine' ++ idents 1 ++
>                               "\\text{for } i=0 \\text{ to } j \\text{ do}" ++ newLine'
>                       pp p = "\\begin{align}\n& " ++ ppILProdtex ufs 0 p ++ 
>                              "\\nonumber\n\\end{align}\n"
>                       psSorted = sort deps ps
>                       sort [] _       = []
>                       sort (d:ds) ps  = (head [(a,b,c) | (a,b,c)<-ps, a == d]):sort ds ps


> ppILProdtex :: UserFunctions -> Int -> ILProd -> String
> ppILProdtex ufs ind (n, _, u) = idents ind ++ ppNameTex n ++ "_{i, j} = " ++ nw ++ ppILtex ufs (ind+ind') u 
>    where
>      (nw,ind') = case u of
>                  (_ :/.../ _) -> ("",0)
>                  (_ :/|||/ _) -> (newLine',1)
>                  otherwise    -> (newLine' ++ idents 1, 1)


> simplBnd p@(ExpIOp a ('=':'=':[]) b) | a == b    = []
>                                      | otherwise = [p]
> simplBnd p                           = [p]

> ppILtex ufs ind (ILTerminal t (ST (i,j))) = "[" ++ tparam ++ " | " ++ sepList ", " (bnd ++ condition) ++ "]"
>     where
>       (ys,_,_,termcode) = termDef ufs t
>       tparam = ppTermCode (termcode (i,j))

>       condition = ppTermCondition (termcode (i,j))

>       bnd = ppILBounds (ST (i,j),ys) where
>         ppILBounds :: ILBounds -> [String]
>         ppILBounds bnd = map (prettyLang Latex) (simplBnd $ ppILBounds' bnd) 

> ppILtex ufs ind (ILTTUnit (ILTerminal t1 (ST (i1,j1)))
>                           (ILTerminal t2 (ST (i2,j2)))) = "[" ++ tparam1 ++ ", " ++ tparam2 ++ 
>                                                          " | " ++ sepList ", " (bnd1 ++ bnd2 ++ condition1 ++ condition2) ++ "]"
>     where
>       (ys1,_,_,termcode1) = termDef ufs t1
>       (ys2,_,_,termcode2) = termDef ufs t2
>       tparam1 = ppTermCode (termcode1 (i1,j1))
>       tparam2 = ppTermCode (termcode2 (i2,j2))

>       condition1 = ppTermCondition (termcode1 (i1,j1))
>       condition2 = ppTermCondition (termcode2 (i2,j2))

>       bnd1 = ppILBounds (ST (i1,j1),ys1) 
>       bnd2 = ppILBounds (ST (i2,j2),ys2) 

>       ppILBounds :: ILBounds -> [String]
>       ppILBounds bnd = map (prettyLang Latex) (simplBnd $ ppILBounds' bnd) 


> ppILtex ufs ind (ILNonterminal n (ST (i,j)))           = ppNameTex n ++ "_{" ++ prettyLang Latex i ++ ", " ++ prettyLang Latex j ++ "}" 
> ppILtex ufs ind (ILNonterminal n (TT (i1,j1) (i2,j2))) = ppNameTex n ++ "_{" ++ prettyLang Latex i1 ++ ", " ++ prettyLang Latex j1 ++ ", " ++
>                                                                                 prettyLang Latex i2 ++ ", " ++ prettyLang Latex j2 ++ "}" 
> ppILtex ufs ind (p :/|||/ q)      | allifChars lpqs = idents ind ++ altifs
>                                   | otherwise       = sepList (appString ++ newLine') lpqs'
>                                   where
>                                      lpqs  = linearize (p :/|||/ q)
>                                      lpqs' = map ((++)(idents ind).(ppILtex ufs ind)) lpqs 
>                                      linearize (p :/|||/ q) = linearize p ++ linearize q
>                                      linearize p            = [p]

>                                      allifChars []     = True
>                                      allifChars (x:xs) = case x of
>                                         (ILTerminal t (ST(i,j))) ->
>                                             let (_,_,_,termcode) = termDef ufs t
>                                             in case termcode (i,j) of
>                                                  (TCIFChar _ _ _)    -> allifChars xs
>                                                  (TCIFNotChar _ _ _) -> allifChars xs
>                                                  otherwise         -> False
>                                         otherwise -> False

>                                      altifs = "[" ++ tparam ++ " | " ++ bnd' ++ sepList " \\vee " conditions ++ "]"
>                                         where
>                                           (ILTerminal t (ST(i,j))) = head lpqs
>                                           tparam               = ppChar "z" j
>                                           (ys,_,_,termcode)    = termDef ufs t
>                                           bnd = ppILBounds (ST (i,j),ys) where
>                                             ppILBounds :: ILBounds -> [String]
>                                             ppILBounds bnd = map (prettyLang Latex) (simplBnd $ ppILBounds' bnd) 
>                                           bnd' = if bnd == [] then "" else concat bnd ++ ", "
>                                           conditions = ppCnd lpqs where
>                                              ppCnd []     = []
>                                              ppCnd (x:xs) = condition ++ (ppCnd xs) where
>                                                (ILTerminal t (ST(i,j))) = x
>                                                (ys,_,_,termcode)    = termDef ufs t
>                                                condition = ppTermCondition(termcode (i,j))

> ppILtex ufs ind (p :/.../ h)      = h ++ "(" ++ newLine' ++ idn ++ ppILtex ufs (ind+1) p ++ ")"
>                                     where
>                                       idn = case p of
>                                         (_ :/|||/ _) ->  ""
>                                         otherwise    -> idents (ind+1)

> ppILtex ufs ind (p `ILwith` ((f,a),ST (i,j)))  = "[ " ++ fString ++ " | " ++ condition ++ 
>                                               ", " ++ fString ++ " \\in" ++ newLine' ++ idn ++ 
>                                                ppILtex ufs (ind+1) p ++ "]"
>                                             where
>                                                (_,_,filtercode) = filterDef ufs (f,a)
>                                                condition = prettyLang Latex (filtercode (ST(i,j)))
>                                                idn = case p of
>                                                        (_ :/|||/ _) ->  ""
>                                                        otherwise    -> idents (ind+1)

> ppILtex ufs ind (p `ILwith` ((f,a),TT (i1,j1) (i2,j2)))  = "[ " ++ fString ++ " | " ++ condition ++ 
>                                               ", " ++ fString ++ " \\in" ++ newLine' ++ idn ++ 
>                                                ppILtex ufs (ind+1) p ++ "]"
>                                             where
>                                                (_,_,filtercode) = filterDef ufs (f,a)
>                                                condition = prettyLang Latex (filtercode (TT(i1,j1)(i2,j2)))
>                                                idn = case p of
>                                                        (_ :/|||/ _) ->  ""
>                                                        otherwise    -> idents (ind+1)

> ppILtex ufs ind ((((f,_),_), bnd, loops) :/<<</ rhs) = "[" ++ ppNameTex f ++ "(" ++ sepList "," svars ++ ") | " ++
>                                             listArgs 0 (sbnd ++ sloops ++ concat srhs) ++ "]"

>                                       where

>                                          listArgs _ []     = ""
>                                          listArgs n (x:xs) = ret where
>                                            ret = case (n + addx1 + addind > maxLineCount) of
>                                               True -> linebreakString ++ newLine' ++ idents ind ++ incInd x ++ comma ++ 
>                                                       listArgs addx2 (map incInd xs)
>                                               False -> x ++ comma ++ listArgs (n + addx2) xs 
>                                            comma = case xs of
>                                               [] -> ""
>                                               otherwise -> ", "

>                                            addind = ind * indCount
>                                            addx1 = findNL 0 x
>                                            addx2 = findNL 0 (reverse x)

>                                            findNL n []       = n
>                                            findNL n ('\n':_) = n
>                                            findNL n (x:xs)   = findNL (n+1) xs

>                                            incInd x = x     -- erstmal deaktiviert
>                                            -- incInd [] = ""
>                                            -- incInd ('\\':'\\':'\n':'&':' ':xs) = "<inc>\\\\\n& " ++ idents 1 ++ incInd xs
>                                            -- incInd (x:xs)            = x:incInd xs

>                                          lrhs = linearize rhs

>                                          linearize (p :/~~~/ q) = linearize p ++ linearize q
>                                          linearize p            = [p]

>                                          count = length lrhs
>                                          vars = map (\(a,b) -> a++"{"++b++"}") (zip (repeat (pString ++ "_")) (map show [1..count])) 

>                                          svars = map ppVars (zip vars lrhs) where
>                                            ppVars (v, rhs) = 
>                                              case rhs of
>                                                (ILTerminal t (ST(i,j))) -> let (_,_,_,termcode) = termDef ufs t
>                                                                            in ppTermCode (termcode (i,j))
>                                                (ILTTUnit (ILTerminal t1 (ST(i1,j1)))
>                                                          (ILTerminal t2 (ST(i2,j2))))
>                                                                         -> let (_,_,_,termcode1) = termDef ufs t1
>                                                                                (_,_,_,termcode2) = termDef ufs t2
>                                                                            in ppTermCode (termcode1 (i1,j1)) ++ ", " ++
>                                                                               ppTermCode (termcode2 (i2,j2))
>                                                otherwise  -> v

>                                          sbnd = ppILBounds bnd
>                                          sloops = map ppLoops loops where
>                                            ppLoops (lv, begin, end) = 
>                                               prettyLang Latex begin ++ " \\leq " ++ ppLoopVarTex lv ++ " \\leq " ++ 
>                                               prettyLang Latex end 

>                                          srhs = map ppRhs (zip vars lrhs) where
>                                            ppRhs (v, rhs) = 
>                                               case rhs of
>                                                 (ILTerminal t (ST(i,j))) -> condition where
>                                                    (_,_,_,termcode) = termDef ufs t
>                                                    condition = ppTermCondition(termcode (i,j))

>                                                 otherwise        -> [v ++ " \\in " ++  nw ++ ppILtex ufs (ind+ind') rhs]
>                                                    where
>                                                    (nw, ind') = case rhs of
>                                                      (_ :/~~~/ _) -> (newLine', 1)
>                                                      (_ :/|||/ _) -> (newLine', 1)
>                                                      (_ :/<<</ _) -> (newLine' ++ idents (ind+1), 1)
>                                                      otherwise    -> ("", 0)

>                                          ppILBounds :: ILBounds -> [String]
>                                          ppILBounds bnd = map (prettyLang Latex) (simplBnd $ ppILBounds' bnd) 

> ppILtex ufs ind x = pattErr "ppILtex" x

> specTex []     = []
> specTex (x:xs) = x' ++ specTex xs
>   where 
>     specs = [('$', "\\$"), ('\\', "\\backslash"), ('&', "\\&")]

>     isSpec x = [(c, s) | (c,s) <- specs, c == x]
>     x' = case isSpec x of
>            []      -> [x]
>            [(_,s)] -> s

> {-

> data TermCode = TCEmpty | TCChar MathExp | TCIFChar Char MathExp | TCIFNotChar Char MathExp | TCRegion SubScripts | 
>                 TCIFString String SubScripts | TCLoc MathExp | TCUser String [Exp] | TCIFUser String [Exp] 

> -}
