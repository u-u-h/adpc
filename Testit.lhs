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



> module Main where
> import List
> import System
> import Char

> binName   = "testit"
> configvars = [("$ADPC", "adpcompile"),
>               ("$CC"  , "cc")]

> main				:: IO ()
> main				=  getArgs >>= nmain

> rev_Testit = "$Revision$ "
> rev       = getRev rev_Testit

> ninfo     =
>     "Usage: " ++ binName ++ " <file> [options] \n" ++
>     "Options:\n" ++
>     "  -h                                   Display this information\n" ++
>     "  <file>                               Use test file <file>\n" ++
>     "  -o                                   Show program output\n" ++
>     "  -m                                   monochrome mode\n" ++
>     "  -v                                   Show version\n" ++
>     "  -c <name> <opts> <input> <k> <algs>  create test script\n" ++
>     "  -a                                   derive results automatically\n" ++
>     "  -p <name> <algs>                     create main function for ***-operator"
> paramList = ["-v","-h","-o","-m","-c","-p","-a"]

> nmain [] = putStrLn noArgs where
>    noArgs = 
>      binName ++ ": missing file arguments\n"++
>      "Try `" ++ binName ++ " -h' for more information."

> nmain s = res where
>             res | elem "-h" s = do
>                                 putStrLn ninfo
>                 | elem "-v" s = putStrLn $
>                     "test script generator, version " ++ "0." ++ show rev
>                 | elem "-c" s = genTests (elem "-a" s) name algs stdOpts input std_k
>                 | elem "-p" s = genMain name' algs'
>                 | otherwise   = process (elem "-o" s) (not $ elem "-m" s) (head s)
>             (name:stdOpts:input:std_k:algs) = tail ((nub s) \\ ["-a"])
>             (name':algs')                   = tail s

----------------------------------------------------------------------------------------------------
generate test script
-------------------------

> tas2 = ["", "-ta p", "-ta b", "-ta bt", "-ta bo", "-ta bi", "-ta bto", "-ta bti", "-ta btio"]
> tas = ["-ta bto"]
> cgs = ["", "-cg l"]

> type Config = 
>   ([String],  -- Algebren
>    String,    -- Codegen - Mode
>    String)    -- Table access

> configs algebras = single ++ paired
>   where
>     single = [([alg],   cg, ta)      |  ta <- tas, cg <- cgs, alg <- algebras]
>     paired = if length algebras > 1 then [([a1,a2], "-cg p", ta) |  ta <- tas ,a1 <- algebras, a2 <- algebras]
>                                     else []

> genTest auto name stdOpts input std_k (algebras, cg, ta) = do
>   putStrLn $ "; ---------------------------------------- " 
>   putStrLn $ "; " ++ sepList " " (algebras ++ [cg] ++ [ta]) 
>   putStrLn $ "; ----------------------------------------" 
>   putStrLn $ name 
>   putStrLn $ "$ADPC -c " ++ name ++ ".lhs -o $TARGET.c " ++ stdOpts ++ 
>               " -al " ++ sepList " " algebras ++ " " ++ ta ++ " " ++ cg
>   putStrLn $ "$CC -lm -lgc -ldl $TARGET.c -o $TARGET"
>   putStrLn $ "$TARGET " ++ input ++ " " ++ std_k ++ optSort
>   if auto then do 
>                system $ name ++ "_ghc \'" ++ sepList " *** " algebras ++ "\' " ++ input ++ " " ++ std_k ++ 
>                         optSort ++ " > " ++ name ++ ".testout"
>                result <- readFile $name ++ ".testout"
>                putStr result                
>                else
>                putStrLn $ "<< enter input here >> "
>   putStrLn $ "$$$" 
>     where optSort = if auto then " | sort " else ""

> genTests auto name algs stdOpts input std_k = sequence_ (map (genTest auto name stdOpts input std_k) (configs algs))

----------------------------------------------------------------------------------------------------
generate main function
----------------------------

> genMain name algs = putStrLn $
    
>     "> main ::  IO()\n"++
>     "> main  = do\n"++
>     ">         [arg1,arg2,arg3] <- getArgs\n"++
>     ">         let input      = arg2\n"++
>     ">             k          = (read arg3) :: Int\n"++
>     ">             res = case arg1 of\n"++
>    genCalls  ++
>     ">             in putStr res"

>    where
>      genCalls = concatMap genCall1 algs ++
>                 concatMap genCall2 [(a1,a2) | a1 <- algs, a2 <- algs]
>      genCall1 a1       = ">                 \"" ++ a1 ++ "\" -> " ++
>                          "unlines $ map show $ " ++ name ++ " (" ++ enr a1 ++ ") input\n"
>      genCall2 (a1, a2) = ">                 \"" ++ a1 ++ " *** " ++ a2 ++ "\" -> " ++
>                          "unlines $ map show $ " ++ name ++ " (" ++ enr a1 ++ " *** " ++ enr a2 ++ ") input\n"
>      enr a = if take 2 (reverse a) == "k_" then "(" ++ a ++ " k)" else a

----------------------------------------------------------------------------------------------------
test processing
----------------------

> process verbose cm file = do
>                  inp <- readFile file
>                  parsed <- return $ parse $ lines inp
>                  parsed <- return $ map updateParse parsed
>                  putStrLn $ "test script: " ++ file
>                  failed <- (mapM (processTest verbose cm) parsed)
>                  putStrLn $ "tests run:    " ++ show (length parsed) ++ "\n" ++
>                             "tests failed: " ++ show (sum failed) 

> parse inp | isEmpty inp = []
>           | otherwise   = parseEntry e: parse (tail rest)
>           where 
>             isEmpty ss = elem (nub $ concat ss) [" ",""]
>             (e, rest) = span ((/=) "$$$") inp

> parseEntry e = (unlines desc, targetname, adpc, cc, call, result)
>    where
>      (desc, rest)                     = span (\l -> head l == ';') e
>      (targetname:adpc:cc:call:result) = rest

> updateParse (desc, targetname, adpc, cc, call, result) = (desc, targetname, upd adpc, upd cc, upd call, result)
>    where upd s = replace s (("$TARGET", targetname):configvars)

> processTest verbose cm (desc, targetname, adpc, cc, call, result) = 

>   do
>    putStrLn desc 
>    sysOut  $ "rm " ++ targetname ++ " " ++ targetname ++ ".c" ++ " " ++ outFile ++ "; touch " ++ outFile
>    sysOut   adpc
>    sysOut   cc
>    putStrLn call
>    system   $ call ++ " > " ++ outFile
>    output   <- readFile outFile
>    if verbose then putStrLn output else return ()
>    okay     <- return $ output == unlines result
>    putStrLn $ if okay then color cm green $ targetname ++ ": TEST OK"                                         
>                       else color cm red   $ targetname ++ ": TEST WRONG"
>    return (if okay then 0 else 1)

>   where 
>     outFile = targetname ++ ".testout"
>     sysOut s = do
>                putStrLn s
>                system s

> color False _ s = s
> color True c s  = "\x1b[00;" ++ show c  ++"m"  ++ s ++ "\x1b[00;30m"

> red   = 31
> green = 32

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

> replace ::  String -> [(String, String)] -> String
> replace ss []      = ss
> replace ss (r:rps) = replace (rs ss r) rps
>   where
>     rs []        _       = []
>     rs ss@(x:xs) (r, nr) = let lr = length r
>                            in if take lr ss == r then nr ++ rs (drop lr ss) (r,nr) else x:rs xs (r,nr)
