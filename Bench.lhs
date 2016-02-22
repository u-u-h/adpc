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
- gnuplot support
  -> richtiges parsen der latex-ausgabe notwendig
- damit auch moeglich: 
    - richtige reihenfolge fuer die erste spalte
    - automatisches Vergleichen der Ergebnisse : Benchsuite -> Testsuite
    - Automatisches Ausrechnen von Geschwindigkeits-faktoren
- unterstuetzung fuer fasta-input files
- optionen fuer random-sequenzen

> module Main where
> import System
> import Char

> main				:: IO ()
> main				=  getArgs >>= nmain

> rev_Bench = "$Revision$ "
> rev       = getRev rev_Bench
> binName   = "bsg"
> ninfo     =
>     "Usage: " ++ binName ++ " [options] \n" ++
>     "Options:\n" ++
>     "  -h                        Display this information\n" ++
>     "  -f  <name>                Specify file name prefix\n" ++
>     "  -s  <name> ... <name>     Specify file name suffixes\n" ++
>     "  -i  <name> [<name>]       Specify input sequences (max. 2)\n" ++
>     "  -$                        Separate two sequences with '$'\n" ++
>     "  -if <file>                Specify input sequence file\n" ++ 
>     "  -fo                       Show input file in output\n" ++
>     "  -a                        Run all-against-all (only with -if)\n" ++
>     "  -n  <number>              Specify input sequence replicator\n" ++
>     "  -m                        Add memory analysis to script\n" ++
>     "  -t  <command>             Specify time command\n"  ++  
>     "  -fs <string>              Specify gnu time format string\n"  ++  
>     "  -d  <string> ... <string> Specify benchmark documentation\n"  ++  
>     "  -q                        Suppress additional output\n"  ++  
>     "  -q2                       Suppress time measurements\n"  ++  
>     "  -c                        Add command line argument to output\n"  ++  
>     "  -lf                       Add LaTeX document framework to output\n"  ++  
>     "  -v                        Show version\n" ++
>     "script execution:\n" ++
>     "  " ++ binName ++ " ... | benchit [<outputfile>] [>  <texfile>]     or\n" ++
>     "  " ++ binName ++ " ... | err2std [>  <texfile>]  (for -q2 mode)"
> paramList = ["-v","-h","-s","-i","-$","-n","-f","-t","-fs","-m","-d","-c","-q","-lf","-if","-a","-q","-fo"]
> standardTimeCommand    = "gtime -f "
> standardFormatString   = "%U"
> standardMemoryHandling = False
> standardDoc            = []
> standardQuietMode      = False
> standardQuietMode2     = False
> standardReplicator     = 1 :: Int

> nmain [] = putStrLn noArgs where
>    noArgs = 
>      binName ++ ": missing file arguments\n"++
>      "Try `" ++ binName ++ " -h' for more information."

> nmain s = res where
>             res | elem "-h" s = do
>                                 putStrLn ninfo
>                 | elem "-v" s = putStrLn $
>                     "benchmark script generator, version " ++ "0." ++ show rev
>                 | otherwise   = do
>                                 input           <- inputIO
>                                 inputFileString <- inputFileStringIO
>                                 bench args doc latexFrame quiet quiet2 command formatstr mem suffixes dollar file inputFileString input n

>             arg n = head' (drop (n-1) s) where
>                   head' [] = []
>                   head' x  = head x

>             suffixes = case getParams s "-s" of
>                          []  -> [""] 
>                          x   -> x

>             file     = head $ getPar s "-f" "file name prefix"
>             mem = case elem "-m" s of
>                    False -> standardMemoryHandling
>                    True  -> True
>             quiet = case elem "-q" s of
>                    False -> standardQuietMode
>                    True  -> True
>             quiet2 = case elem "-q2" s of
>                    False -> standardQuietMode2
>                    True  -> True

>             latexFrame = elem "-lf" s 

>             inp   = getPar s "-i" "input sequences"
>             inputIO = case elem "-if" s of
>                         False -> case inp of
>                                   [a]       -> return [(("",a), ("",""))]
>                                   [a,b]     -> return [(("",a), ("",b))]
>                                   otherwise -> error "more than two input sequences specified."
>                         True -> do
>                                 fileinp <- parseInputFile allagall (head $ getPar s "-if" "input file name")
>                                 return fileinp
>             inputFileStringIO = case elem "-fo" s of
>                         False -> return ""
>                         True  -> case elem "-if" s of
>                                  False -> return ""
>                                  True  -> parseInputFileForOutput quiet (head $ getPar s "-if" "input file name")

>             dollar   = elem "-$" s
>             allagall = elem "-a" s

>             n        = case getParams s "-n" of
>                         []        -> standardReplicator
>                         [x]       -> strtoInt x
>                         otherwise -> error "more than one replicator specified."
>             command  = case getParams s "-t" of
>                         []        -> standardTimeCommand
>                         [x]       -> x
>                         otherwise -> error "more than one time command specified."

>             formatstr  = case getParams s "-fs" of
>                          []        -> standardFormatString
>                          [x]       -> x
>                          otherwise -> error "more than one time command specified."

>             doc       = case getParams s "-d" of
>                          []        -> standardDoc
>                          x         -> x
>             args      = case elem "-c" s of
>                          False -> ""
>                          True -> "% command line:\\n% " ++ binName ++ " " ++ sepList " " (map spaceArg s) ++ "\\n"

>             getPar s p e = case getParams s p of
>                             [] -> error $ "no " ++ e ++ " given!\nTry `" ++ binName ++ " -h' for more information."
>                             x  -> x

>             getParams [] _     = []
>             getParams (s:ss) p | s == p    = getParams' ss 
>                                | otherwise = getParams ss p
>                 where
>                 getParams' []     = []
>                 getParams' (s:ss) | elem s paramList = []
>                                   | otherwise        = s:getParams' ss

>             spaceArg a = if elem ' ' a then "\"" ++ a ++ "\"" else a

> bench args doc latexFrame quiet quiet2 command formatstr mem suffixes dollar file inputFileString input n = putStrLn outp where
>         outp   = -- compile ++ 
>                  inputFileString ++ 
>                  lframe_header ++ 
>                  (if args == "" then "" else echoErr args) ++
>                  concatMap (echoErr.replBS.replSC) doc ++ echoErr "" ++
>                  echoHost ++
>                  echoErr "\\medskip" ++ 
>                  echoErr table1 ++
>                  echoErr table2 ++
>                  echoErr table3 ++
>                  concatMap (bench' file 1 n) input  ++
>                  echoErr table4 ++
>                  lframe_trailer

>         compile = "ghc -O --make " ++ file ++ ".lhs -o " ++ file ++ "ghc\n" ++
>                   "gcc -O3 " ++ file ++ "cg1.c"  ++ " -o " ++ file ++ "cg1\n"   ++ 
>                   "gcc -O3 " ++ file ++ "cg2.c"  ++ " -o " ++ file ++ "cg2\n"   ++
>                   --"gcc -O3 " ++ file ++ "cg2v.c" ++ " -o " ++ file ++ "cg2v\n"  ++
>                   "echo ****************************************************************"

>         bench' :: String -> Int -> Int -> ((String,String),(String,String)) -> String
>         bench' file n mn ((descr1,input1), (descr2,input2)) =
>                 if (n-1)==mn then "" else
>                   concatMap b (zip [1..] suffixes) ++ 
>                   echoStd "" ++
>                   echoStd "-----------------------------------------------------------" ++
>                   bench' file (n+1) mn ((descr1,input1), (descr2,input2))
>             where
>               b (c,n) = echoStd "" ++ 
>                         echoStd (file ++ n ++ ", " ++ "input length " ++ showlength input1' input2') ++
>                         echoStd "----------------------------" ++ 
>                         echoStdOpt descr1 ++ 
>                         echoStdOpt descr2 ++
>                         (if descr1 ++ descr2 == "" then "" else echoStd "----------------------------") ++ 
>                         timecommand c ++ file ++ n ++ " " ++ hc ++ input1' ++ separator ++ input2' ++ hc ++ "\n" 
>                            where (separator,hc) | dollar    = ("$","'")
>                                                 | otherwise = (" ","")

>               input1' = concat (replicate n input1)
>               input2' = concat (replicate n input2)
>               timecommand c = case quiet2 of
>                                True  -> ""
>                                False -> command ++ format ++ " " ++ memtime 
>                 where
>                 format = squ ++ header ++ " & " ++ formatstr ++ " " ++ trailer ++ squ
>                 squ = [chr 34]
>                 header  = if c==1 then showlength input1' input2' else ""
>                 trailer = if c == (length suffixes) then " " ++ replicate 8 '\\' else ""
>               memtime = case mem of
>                  False -> ""
>                  True  -> "memtime "
>               showlength i1 i2 = show (length i1) ++ if (length i2) ==  0 then "" else ", " ++ show (length i2)

>         ------------------------------------
>         -- LaTeX-table
>         ------------------------------------
>         table1 = "\\\\begin{tabular}{" ++ format ++ "}" where
>            format | mem       = "cl" ++ concat (replicate (2 * length suffixes - 1) "|r")
>                   | otherwise = "l"  ++ concat (replicate (length suffixes)         "|r")
>         table2 = kb1 ++ ll ++ kb2 ++ header ++ "\\\\\\\\" where
>            kb1 | mem = "& KB "
>                | otherwise = ""
>            ll = if length(snd(snd(head input))) == 0 then "n" else "m,n"
>            kb2 | mem       = " & " ++ replSC (head suffixes)
>                | otherwise = ""
>            header | mem       = concatMap (\s -> " & KB & " ++ replSC s) (tail suffixes) 
>                   | otherwise = concatMap (\s -> " & "      ++ replSC s)       suffixes
>         table3 = "\\hline"
>         table4 = "\\end{tabular}"
>         lframe_header  = if latexFrame then echoErr "\\documentclass{article}" ++ echoErr "\\\\begin{document}" else ""
>         lframe_trailer = if latexFrame then echoErr "\\end{document}" else ""

>         echoStd    s = if quiet then "" else "echo '" ++ s ++ "'\n"
>         echoStdOpt s = if quiet then "" else if s =="" then "" else "echo '" ++ s ++ "'\n"

>         echoErr    s = if quiet2 then "" else "echo '" ++ s ++ "'  >/dev/stderr\n"
>         echoHost     = if quiet2 then "" else "echo host: $HOSTNAME   >/dev/stderr\n" ++ echoErr ""

> replBS [] = []
> replBS ('\\':'\\':xs) = "\\\\\\\\\n" ++ replBS xs
> replBS ('\\':xs)      = "\\\\" ++ replBS xs
> replBS (x:xs)         = x:replBS xs

> replSC [] = []
> replSC ('_':xs)  = "\\_" ++ replSC xs
> replSC (x:xs)    = x:replSC xs

--------------------------------------------------------------------------------
Tools:
---------

> getRev :: String -> Int
> getRev r = read (drop 2 $ fst $ span (/= ' ') $ tail $ snd $ span (/= ' ') r) 

> strtoInt :: String -> Int
> strtoInt s = strtoInt' 0 (reverse s) where
>   strtoInt' _ []     = 0
>   strtoInt' e (c:cs) = (digitToInt c)*(10^e) + strtoInt' (e+1) cs

> sepList _ []       = ""
> sepList _ [x]      = x
> sepList t (x:xs)   = x ++ t ++ sepList t xs

> parseInputFile all file = do
>                           inp <- readFile file
>                           l   <- return $ extract all inp
>                           return l
>   where
>   extract all s = ls'
>     where
>     ls = lines s
>     ls' = case all of
>            False -> map (upd.span (/=' ')) ls
>            True  -> allagainstall ls
>     upd(i1,i2) = (("",i1),("", case i2 of
>                       ""        -> ""
>                       otherwise -> tail i2))
>     allagainstall seqs = res
>       where
>       seqn = zip [1..] seqs
>       combs = [(a,b) | a <- seqn, b <- seqn]
>       fcall ((n1,s1),(n2,s2)) = (("sequence " ++ show n1 ++ " against", s1),("sequence " ++ show n2, s2))
>       res  = map fcall combs

> parseInputFileForOutput quiet file = do
>                                inp <- readFile file
>                                res <- return $ extr inp
>                                return res
>   where
>   extr inp  = echoStd ("sequence input file: " ++ file) ++ 
>               echoStd  "------------------------------------" ++
>               concatMap upd (zip [1..] (lines inp))
>   upd (n,s) = echoStd ("sequence " ++ show n ++ ":") ++ echoStd s  
>   echoStd s = if quiet then "" else "echo '" ++ s ++ "'\n"
