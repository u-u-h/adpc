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



> module Alggen (

>   AlggenMode(..),
>   alggen_parts,
>   arrange

> ) where


> import Prelude
> import Char
> import AlggenParse
> import List

> toLowerS  = map toLower

> -- getOpNames :: Signature -> [Name]
> getOpNames ops = map fst ops

> filterOps sigNames ops = filter f ops
>   where
>     f (name, []) = not(elem name sigNames)
>     f _          = True

> genTypes :: Name -> Signature -> String
> genTypes ans (name, ops) = concatMap showOp ops ++ obj where
>    showOp (n, args) = spc ++ toLowerS n ++ " :: " ++ sepList " -> " (args' ++ [ans]) ++ "\n" where
>      args' = map showArg args
>      showArg (Id id) = if id == name then ans else id
>      showArg (Tupel args) = "(" ++ mapsep "," showArg args ++ ")"
>    obj = spc ++ "h :: [" ++ ans ++ "] -> " ++ "[" ++ ans ++ "]\n"


> mergeSignature :: [Signature] -> Signature
> mergeSignature signatures = ("Signature", nub $ map rename ops)
>   where
>     sigNames = map fst signatures
>     ops      = filterOps sigNames $ concatMap snd signatures
>     rename (name, args) = (name, map ren args)
>       where
>         ren (Id n) | elem n sigNames = Id "Signature"
>                    | otherwise       = Id n
>         ren (Tupel args)             = Tupel (map ren args)


Generiert die Typdefinition und liefert zusaetzlich einen
String des Alphabettypes zurueck.

> genAlgType :: Name -> Name -> [Signature] -> (String, String)
> genAlgType answer alphabet input_signatures = ("type " ++ "Algebra " ++ (if alphabetName /= "" then (alphabet ++ " ") else "") ++ answer_types ++ 
>                                              " = " ++ "(\n" ++ unlines (arrange "-" definitions) ++ spc ++ ")", alphabetName)
>    where multiple = length input_signatures > 1
>          sigNames = map fst input_signatures
>          ops      = concatMap (\(name, ops) -> map (\op -> (name, op)) (filterOps sigNames ops)) input_signatures
>          answer_name name = answer ++ "_" ++ name
>          answer_types | multiple  = mapsep " " (\name -> answer_name name) sigNames
>                       | otherwise = answer

>          definitions = signatures ++ lines choiceFuns
>          signatures = map (\(name, n) -> spc ++ (showSignature name n) ++ ",") ops

>          choiceFuns | multiple  = mapsep ",\n" (\name -> choiceFun (answer_name name)) sigNames 
>                     | otherwise = choiceFun answer
>          choiceFun ans = spc ++ "[" ++ ans ++ "] -> [" ++ ans ++ "]"

>          showSignature name (n, args) = concatMap (\n -> showSig n ++ " -> ") args ++ (if multiple then answer_name name else answer)

>          showSig :: Argument -> [Char]
>          showSig (Id tName) | elem tName sigNames && multiple  = answer_name tName
>                             | elem tName sigNames              = answer
>                             | tName == alphabetName            = alphabet
>                             | otherwise                        = tName
>          showSig tup@(Tupel args) | tupName == alphabetName = alphabet
>                                   | otherwise = tupName
>              where tupName = collectNames tup

>          alphabetName = case nub $ filter (\x -> not (x == "()" || elem x sigNames)) $ concatMap ((map collectNames) . snd) (map snd ops) of
>                              []        -> ""
>                              [n]       -> n
>                              xs        -> if elem "Char" xs || elem "AChar" xs then "Char" else ""

>          collectNames :: Argument -> String
>          collectNames (Id name) = name
>          collectNames (Tupel ans) = "(" ++ mapsep "," collectNames ans ++ ")"


> genEnum :: [Signature] -> String -> String
> genEnum signatures alphType = enumType ++ "enum = (" ++ mapsep ", " toLowerS opNames ++ ", " ++ hs ++ ") where\n" ++ concat (arrange "=" functions)
>  where
>    multiple = length signatures > 1

>    sigNames = map fst signatures
>    ops      = filterOps sigNames $ concatMap snd signatures

>    functions = map showOp opNames ++ obj 
>    opNames  = getOpNames  ops
>    showOp :: Name -> String
>    showOp n = spc ++ toLowerS n ++ " = " ++ n ++ "\n" 
>    obj | multiple  = map (\n -> spc ++ "h_" ++ n ++ "= id\n")  sigNames 
>        | otherwise = [spc ++ "h = id\n"]
>    hs  | multiple  = mapsep ", " (\n -> "h_" ++ n) sigNames
>        | otherwise = "h"
>    enumType = "enum :: " ++ "Algebra " ++ alphType ++ " " ++ sepList " " sigNames ++ "\n"


> genCount :: [Signature] -> String -> String
> genCount signatures alphType = countType ++ "count = (" ++ mapsep ", " toLowerS opNames ++ ", " ++ hs ++ ") where\n" ++ concat (arrange "=" functions) 
>   where
>    multiple = length signatures > 1
>    sigNames = map fst signatures
>    ops      = filterOps sigNames $ concatMap snd signatures

>    functions = map showOp ops ++ obj
>    opNames  = getOpNames ops
>    showOp :: Operator -> String
>    showOp (n, args) = spc ++ toLowerS n ++ " " ++ sepList " " args' ++ " = " ++ count ++ "\n" where
>        nargs = zip ['a'..] args
>        args' = map ((:[]).fst) nargs
>        nodes = map ((:[]).fst) (filter isName nargs) where
>            isName (_, (Tupel _)) = False
>            isName (_, (Id n))    = elem n sigNames
>        count = case nodes of 
>                 [] -> "1"
>                 otherwise -> sepList " * " nodes 
>    obj | multiple  = concatMap (\n -> genobj ("h_" ++ n)) sigNames
>        | otherwise = genobj "h"
>    hs  | multiple  = mapsep ", " (\n -> "h_" ++ n) sigNames
>        | otherwise = "h"
>    genobj h = [spc ++ h ++ " [] = []\n", spc ++ h ++ " xs = [sum xs]\n"]
>    countType = "count :: " ++ "Algebra " ++ alphType ++ concat (replicate (length signatures) " Int")  ++ "\n"


> genStar :: [Signature] -> String -> String
> genStar signatures alphType = starType ++ "infix ***\nalg1 *** alg2 = (" ++ mapsep ", " toLowerS opNames ++ ", " ++ hs "" ++ ") where\n" ++ 
>                       spc ++ "(" ++ mapsep ", " ((flip (++) "1").toLowerS) opNames ++ ", " ++ hs "1" ++ ") = alg1\n" ++
>                       spc ++ "(" ++ mapsep ", " ((flip (++) "2").toLowerS) opNames ++ ", " ++ hs "2" ++ ") = alg2\n\n" ++
>                       concat (arrange "=" (map showOp ops)) ++ "\n" ++ concat (arrange "=" obj)
>  where
>    multiple = length signatures > 1
>    sigNames = map fst signatures
>    ops      = filterOps sigNames $ concatMap snd signatures

>    opNames  = getOpNames ops

>    starType = "(***) :: " ++ eqans ++ " => \n   (" ++ "Algebra " ++ alphType ++ " " ++ ans 1 ++ ") -> \n   (" ++
>               "Algebra " ++ alphType ++ " " ++ ans 2 ++ ") -> \n   " ++
>               "Algebra " ++ alphType ++ " " ++ ans_pair ++ "\n"
>      where
>        eqans    | multiple  = "(" ++ mapsep ", " (\n -> "Eq answer1_" ++ n) sigNames ++ ")" 
>                 | otherwise = "Eq answer1"
>        ans num  | multiple  = mapsep " " (\n -> "answer" ++ show num ++ "_" ++ n) sigNames
>                 | otherwise = "answer" ++ show num 
>        ans_pair | multiple  = mapsep " " (\n -> "(answer1_" ++ n ++ ", answer2_" ++ n ++ ")") sigNames
>                 | otherwise = "(answer1,answer2)"

>    showOp :: Operator -> String
>    showOp (n, args) = spc ++ toLowerS n ++ " " ++ sepList " " nargs' ++ " = " ++ star ++ "\n" where
>        nargs = zip ['a'..] args
>        nargs' = map enr nargs where
>          enr (a, (Tupel _)) = [a]
>          enr (a, (Id n))    | elem n sigNames = "(" ++ [a] ++ "1," ++ [a] ++ "2)"
>                             | otherwise       = [a]
>        star = "(" ++ star' "1" ++ ", " ++ star' "2" ++ ")" where
>            star' i = toLowerS n ++ i ++ " " ++ mapsep " " enr nargs where
>              enr (a, (Tupel _)) = [a]
>              enr (a, (Id n))    | elem n sigNames = [a] ++ i
>                                 | otherwise       = [a]

>    hs num  | multiple  = mapsep ", " (\n -> "h_" ++ n ++ num) sigNames
>            | otherwise = "h" ++ num

>    obj | multiple  = map (\n -> genobj ("h_" ++ n)) sigNames
>        | otherwise = [genobj "h"] 

>    genobj h = spc ++ h ++          " xs = [(x1,x2)| x1 <- nub $ " ++ h ++ "1 [ y1 | (y1,y2) <- xs],\n"         ++
>               spc ++ extraspc ++   "                x2 <-       " ++ h ++ "2 [ y2 | (y1,y2) <- xs, y1 == x1]]\n"
>      where extraspc | multiple  = replicate (2 + maximum (map length sigNames)) ' '
>                     | otherwise = " "

> genGrammar :: AlggenMode -> [Signature] -> String -> (String, String, String)
> genGrammar mode signatures alphType = ("grammar alg f = axiom " ++ toLowerS (head sigNames) ++
>                                             " where\n" ++ bindAlgebra ++ "\n", grammar, bindInput)
>     where 
>           multiple = length signatures > 1 && mode == NoMerge
>           sigNames = map fst signatures
>           ops      = concatMap snd signatures

>           functionList = let fs = map toLowerS $ getOpNames $ filterOps sigNames ops
>                          in if mode == Merge then nub fs else fs

>           bindAlgebra = spc ++ "(" ++ mapsep ", " id functionList ++ ", " ++ hs ++ ") = alg\n"
>           hs  | multiple  = mapsep ", " (\n -> "h_" ++ n) sigNames
>               | otherwise = "h"

>           grammar = arr $ concatMap genG signatures
>             where
>               arr ss = unlines $ arrange "|." $ arrange "~" $ arrange "<" $ lines ss

>           genG (ntName,ops) = spc ++ toLowerS ntName ++ " = tabulated(\n" ++
>                               spc ++ spc ++ mapsep (" |||\n" ++ spc ++ spc) transformOp ops ++ " ... " ++ hf ++ ")\n\n"
>              where
>                hf  | multiple  = "h_" ++ ntName
>                    | otherwise = "h"

>           transformOp (name,[])   = (toLowerS name)
>           transformOp (name,args) = (toLowerS name) ++ " <<< " ++ mapsep " ~~~ " mapName args

>           mapName :: Argument -> String
>           mapName arg | elem name sigNames   = toLowerS name
>                       | name == alphType     = "achar"
>                       | name == "Char"       = "achar"        -- Alternative: "base"
>                       | name == "()"         = "empty"
>                       | name == "Empty"      = "empty"
>                       | name == "(Int,Int)"  = "astring"      -- Alternative: "region"
>                       | name == "Subword"    = "astring"
>                       | name == "Region"     = "astring"
>                       | name == "AString"    = "astring"
>                       | otherwise            = "achar"
>               where name = extractName arg

>           bindInput = spc ++ "z         = mk f\n" ++
>                       spc ++ "(_,n)     = bounds z\n" ++ 
>                       spc ++ "achar     = achar' z\n" ++
>                       spc ++ "axiom     = axiom' n\n"

>           extractName :: Argument -> String
>           extractName (Id name) = name
>           extractName (Tupel ans) = "(" ++ mapsep "," extractName ans ++ ")"


Hilfsfunktion zum debuggen

> data AlggenMode = Merge | NoMerge deriving Eq

> alggen_parts :: AlggenMode -> String -> (String, String, (String, String, String))
> alggen_parts mode inp = ("Haskell header:\n\n" ++ header ++ optionalParsedSig ++ "\nThe signature:\n\n" ++ signature ++
>                         "\nAlgebra type:\n\n" ++ algType ++ "\nEnumeration algebra:\n\n" ++ enumAlg ++
>                         "\nCounting algebra:\n\n" ++ countAlg ++ "\nAlgebra product operation:\n\n" ++ starOp ++
>                         "\nThe yield grammar:\n\n", axiom, (grammarP1,grammarP2,grammarP3))
>    where header    =  enlit  "import ADPCombinators\nimport Array\nimport List\n"
>          optionalParsedSig | length parsedSig > 1 && mode == Merge =
>                                "\nSignature merge mode activated. Input signature was:\n\n" ++ 
>                                concatMap ppSignature parsedSig
>                            | otherwise = ""
>          parsedSig = parse $ '\n' : inp  -- durch die neugestaltung des Scanners muss dieser kleine Trick angewendet werden ($'\n':...)
>          mergedSig | length parsedSig > 1 && mode == Merge = [mergeSignature parsedSig]
>                    | otherwise                             = parsedSig
>          signature = enlit $ concatMap ppSignature mergedSig
>          (generatedAlgs, alphabetType) = genAlgType "answer" "alphabet" mergedSig
>          algType   = enlit $ generatedAlgs
>          enumAlg   = enlit $ genEnum mergedSig alphabetType
>          countAlg  = enlit $ genCount mergedSig alphabetType
>          starOp    = enlit $ genStar  mergedSig alphabetType
>          generatedGrammar = genGrammar mode parsedSig alphabetType
>          axiom     = toLowerS $ fst $ head parsedSig
>          grammarP1 = enlit $ fst3 $ generatedGrammar
>          grammarP2 = enlit $ snd3 $ generatedGrammar
>          grammarP3 = enlit $ thd3 $ generatedGrammar


Hilfsfunktion um .lhs format aus einer Liste an Strings zu erstellen.

> enlit :: [Char] -> [Char]
> enlit ss = unlines $ map (\l -> "> " ++ l) $ lines ss


Hilfsfunktion, um aus einer Liste von Strings im .lhs-Format 
eine Liste von Strings ohne vorangestelltes "> " zu machen.

> unlit :: [[Char]] -> [[Char]]
> unlit = map unlit'
> unlit' :: [Char] -> [Char]
> unlit' l = unlines $ map remLit $ lines l
>     where remLit ('>':' ':xs) = xs
>           remLit ('>':xs) = xs      -- nur fuer den Fall, dass es kein Space nach der spitzen Klammer gibt
>           remLit xs      = xs

Hilfsfunktion zum tabulierten Ausgeben des Codes. Die beiden
Parameter-Listen muessen die gleiche Laenge haben. Andernfalls
werden die Elemente der laengeren Lsite ignoriert.

> tabular :: [String] -> [String] -> [String]
> tabular [] _   = []
> tabular xs ys  = map (\(x,y) -> x ++ (replicate (maxWidth - length x) ' ') ++ y) $ zip xs ys
>     where maxWidth = maximum (map length xs)

> arrange :: [Char] -> [String] -> [String]
> arrange sep lines = map arr lines
>   where
>     maxpos = maximum $ map getpos lines
>     getpos l = getpos' 0 l
>     getpos' n []     = 0
>     getpos' n (c:cs) | elem c sep = n
>                      | otherwise  = getpos' (n+1) cs
>     arr l = arr' 0 l
>     arr' _ []        = []
>     arr' n xs@(c:cs) | elem c sep = replicate (maxpos - n) ' ' ++ xs
>                      | otherwise  = c:arr' (n+1) cs

> fst3 (a,_,_) = a
> snd3 (_,a,_) = a
> thd3 (_,_,a) = a
