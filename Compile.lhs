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



> -- | Parses the options from the command line.
> -- And some alggen stuff.
> module Compile(

>   cmain,
>   cmainInt,
>   compile,
>   alggen

> ) where


> import System
> import Char
> import List
> import Constants
> import Tools
> import MathExp
> import TLData
> import Structs
> import Expr
> import StringLib
> import Syntax
> import Lex
> import Parse
> import Yieldsize
> import Adptrans
> import Dss
> import Depana
> import TL
> import IL2
> import Codegen
> import Codegen1
> import TLFrame
> import Poly
> import WidthAna
> import LatexRecs
> import SM
> import Phases

> import Track
> import Range
> import PrettyPrint

> import Algebras
> import Alggen
> import Beautify
> import Man

> import ParseMonad
> import ParseTree
> import TL(TL)

> rev_Compile =  "$Revision$ "

> copyright = 
>   "Copyright (C) 2001-2008 Peter Steffen, Marco Ruether, Christian Lang,\n" ++ 
>   "                        Georg Sauthoff\n" ++ 
>   "\n" ++ 
>   "Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.\n" ++ 
>   "Updates: http://www.techfak.uni-bielefeld.de/~psteffen/\n"


> binName = "adpcompile"
> verpref = "0.9"
> info_short =
>     "Usage: " ++ binName ++ " [options] \n" ++
>     "Options:\n" ++
>     "  -h                     Display this information\n" ++
>     "  -H <option>  	        Display detailed information on <option>\n"++
>     "  -c <file>              Compile to imperative target code\n" ++
>     "  -z <file>              Optimize combinators in ADP source code\n" ++
>     "  -l <file>              Generate recurrences typeset in LaTeX\n" ++
>     "  -g <file>              Generate ADP template for signature\n" ++
>     "  -tc                    Check program for type errors\n" ++
>     "Table design:\n" ++
>     "  -tg <file>  -ctg       Derive  good   table configurations\n" ++
>     "  -to <file>  -cto       Derive optimal table configurations\n" ++
>     "  -tx <file>  -ctx       Derive approx. optimal table configurations\n" ++
>     "  -iuc                   Ignore user annotated table configuration\n" ++
>     "  -tadd <number>         Maximal number of additional tables\n" ++
>     "  -taddc <number>        Necessary constant factor improvement for add. tables\n" ++
>     "  -taddn <number>        Expected input length for constant factor improvement\n" ++
>     "Target code generation:\n" ++
>     "  -bt <mode>             Generate backtracing code (enumeration algebra needed)\n" ++
>     "                            mode: s -> single, so -> suboptimal\n" ++
>     "  -W                     Include window mode\n" ++
>     "  -al <alg> .. <alg>     Specify order of algebra usage\n" ++
>     "  -alpp <alg> .. <alg>   Specify pretty printing algebras\n" ++
>     "  -cs  <alg>             Automatically generate signature and enumeration\n" ++
>     "                         for algebra <alg>\n" ++ 
>     "  -vl <verbosity level>  Specify output verbosity level\n" ++
>     "                            verbosity level: \n" ++
>     "                               t -> target, r -> trace, rr -> detailed trace\n" ++
>     "                               d -> debug\n" ++
>     "  -o <filename>          Output to <filename>\n" ++
>     "  -v                     Show version"

> info_long =
>     "Usage: " ++ binName ++ " [options] \n" ++
>     "Options:\n" ++
>     "  -h                     Display this information\n" ++
>     "  -H <option>  	        Display detailed information on <option>\n"++
>     "  -c <file>              Compile to imperative target code\n" ++
>     "  -b <file>              Compile to binary target\n" ++
>     "  -z <file>              Optimize combinators in ADP source code\n" ++
>     "  -l <file>              Generate recurrences typeset in LaTeX\n" ++
>     "  -g <file>              Generate ADP template for signature\n" ++
>     "  -tc                    Check program for type errors\n" ++
>     "Table design:\n" ++
>     "  -tg <file>  -ctg       Derive  good   table configurations\n" ++
>     "  -to <file>  -cto       Derive optimal table configurations\n" ++
>     "  -tx <file>  -ctx       Derive approx. optimal table configurations\n" ++
>     "  -iuc                   Ignore user annotated table configuration\n" ++
>     "  -tp                    Only preprocess table configuration\n" ++
>     "  -sp                    Skip preprocessing of table configuration\n" ++
>     "  -tadd <number>         Maximal number of additional tables\n" ++
>     "  -taddc <number>        Necessary constant factor improvement for add. tables\n" ++
>     "  -taddn <number>        Expected input length for constant factor improvement\n" ++
>     "Target code generation:\n" ++
>     "  -tl <language>         Target language for imperative code generation\n" ++
>     "                            language: c -> C, f -> Fortran, p -> Pascal\n" ++
>     "  -cg <codegen level>    Code generation level\n" ++
>     "                            codegen level: b, l, v, p\n" ++
>     "  -bt <mode>             Generate backtracing code (enumeration algebra needed)\n" ++
>     "                            mode: cl -> complete list, s -> single, l -> list,\n" ++
>     "                                  so -> suboptimal, soc -> suboptimal cut\n" ++
>     "                                  pf -> sampling\n" ++
>     "  -nc                    Suppress comments in target code\n" ++
>     "  -ar                    Generate code to show entry (0,n) for all tables\n" ++
>     "  -arr                   Generate code to show all table results (0,0)..(m,n)\n" ++
>     "  -of <format>           Specify output format, formats: h, hl, s, l\n" ++
>     "  -lcf                   Leak choice functions into inner expressions(Caution!)\n" ++
>     "  -ta <scheme>           Use table allocation scheme <scheme>\n" ++
>     "                            scheme:  p -> pointers, b[options]* -> block\n" ++
>     "                            options: t -> triangular, o -> offset array, \n" ++ 
>     "                                     i -> interleaved\n" ++ 
>     "  -ita                   Inline table access macros\n" ++
>     "  -if                    Inline functions where possible\n" ++
>     "  -W                     Include window mode\n" ++
>     "  -O  -O2                Target code optimizations\n"++
>     "  -gc <mode>             Generate garbage collected code, modes: cc, bdw, own\n" ++
>     "  -al <alg> .. <alg>     Specify order of algebra usage\n" ++
>     "  -alpp <alg> .. <alg>   Specify pretty printing algebras\n" ++
>     "  -cs  <alg>             Automatically generate signature and enumeration\n" ++
>     "                         for algebra <alg>\n" ++ 
>     "  -vl <verbosity level>  Specify output verbosity level\n" ++
>     "                            verbosity level: \n" ++
>     "                               t -> target, r -> trace, rr -> detailed trace\n" ++
>     "                               s -> special trace, l -> lhs2tex optimized trace\n"++ 
>     "                               d -> debug\n" ++
>     "  -path <path>           Specify search path for imports\n" ++
>     "  -sl <label>            Stop compiler after phase <label>\n" ++
>     "  -cc <command>          Specify compiler command line\n" ++
>     "  -ap <filename>         Apply patch after compilation\n" ++
>     "  -o <filename>          Output to <filename>\n" ++
>     "  -v                     Show version"

> paramList = ["-h","-hh","-H","-c","-z","-l","-g","-tg","-to","-tx","-te","-th","-ctg","-cto","-ctx",
>              "-tl","-cg","-vl","-sl","-o","-v","-vv","-nc","-rn","-bea",
>              "-ar","-arr","-of","-lcf","-ta","-ita","-if","-W", "-O","-O2","-gc","-al","-alpp","-cs","-b","-cc","-ap","-bt",
>              "-iuc","-tp","-sp","-tadd","-taddc","-taddn","-pc","-pn","-tc", "-path"]

> -- | Convenient function to call from an haskell interpreter.
> compile :: String -- ^ options seperated via whitespace
>         -> IO (String, Int)
> compile args = cmain (splitArgs args)

> -- | Prints a Message to stdout and return success
> putStrLn' :: (Num a) => String -> IO (String, a)
> putStrLn' s = do
>               putStrLn s
>               return ("", -1)

> cmain:: [String] -> IO (String, Int)
> cmain s = cmainInt s (ParseMonad.Failed ("Old case", 1)) id

> -- | Parses the the argument vector (from the command line) and calls the appropriate functions.
> cmainInt :: [String] -- ^ argument vector
>       -> ParseMonad.ParseResult ParseTree.ADPProgram -- ^ optional parsed input
>       -> ([TL] -> [TL]) -- ^ optional filter fn for TL Code
>       -> IO (String, Int) -- ^ Tuple of compiler output and return code, /= -1 means error
> cmainInt [] _ _ = putStrLn' noArgs where
>    noArgs = 
>      binName ++ ": missing file arguments\n"++
>      "Try `" ++ binName ++ " -h' for more information."

> cmainInt s parseTree tlFilter = res where
>             res | elem "-h" s = putStrLn' info_short
>                 | elem "-hh" s = putStrLn' info_long
>                 | elem "-H" s = case getParams s "-H" of
>                                      [] -> error "unknown option for -H"
>                                      [x] -> putStrLn' (showLongHelp x)
>                                      x   -> error "more than one parameter for -H"
>                 | elem "-v" s = putStrLn' $
>                     "ADP compiler " ++ version ++ "\n" ++
>                     copyright
>                 | elem "-vv" s = putStrLn' $
>                     "ADP compiler " ++ version ++ "\n" ++
>                     copyright ++
>                     "\nrevisions:\n" ++ revs
>                 | elem "-g" s  = do
>                                  alggen_file Merge fi fo
>                                  return ("", -1)
>                 | otherwise   = compile_all cmdl version ar arr off mode cc ap vl sl cg bt lcf tas ita ifct 
>                                             window toptO toptO2 gc tl nc 
>                                             (rn,bea,beaf) (tc,iuc,tp,sp,tadd,taddc,taddn) algs alpp cs (pc,pn) fi fo parseTree
>                                             tlFilter typecheck searchpath

>             version = verpref ++ " (rev " ++ show rev ++ ")"

>             delSF [] = []
>             delSF ("-c":s:xs) = "-c":(if elem '>' s then "<no file>" else s):xs
>             delSF (x:xs) = x : delSF xs
>             cmdl = binName ++ " " ++ (sepList " " $ delSF s)

>             -- compile mode
>             mode = altParams s [("-c",imperativeCM),("-b",binaryCM),("-z",optimizeCM),("-l",latexCM),
>                                 ("-tg",tabulateCM),("-th",tabulateCM),("-to",tabulateCMextern),("-tx",tabulateCMextern),
>                                 ("-te",tabulateCMextern)] imperativeCM
>             -- verbosity level
>             vl   = altList s "-vl" [("t", target), ("r",trace), ("rr",tracemore), ("l",latextrace), ("s", specialtrace), ("d",debug)] 
>                            target "verbosity level"
>             -- stop-label
>             sl  = case getParams s "-sl" of
>                      []        -> "labl_nostop"
>                      [x]       -> x
>                      x         -> error $ "more than one stop label specified: " ++ sepList " " x
>             -- codegen level
>             cg   = altList s "-cg" [("b",CGb),("v",CGv),("l",CGl),("p",CGp)] CGv "codegen level"
>             -- generate backtracing code
>             bt   = altList s "-bt" [("cl",BTCompleteList), ("s", BTSingle), ("l", BTList), 
>                                     ("so", BTSubOpt), ("soc", BTSubOptCut), ("pf", BTPF)] 
>                                     BTNone "backtrace mode"
>             -- target language
>             tl   = altList s "-tl"
>               [("c",C),("f",F),("p",Pascal), ("j", Java)] C "target language"
>             -- suppress comments
>             nc   = altParams s [("-nc", True)] False
>             -- rename names
>             rn  = altParams s [("-rn", True)] False
>             -- beautify code
>             bea  = altParams s [("-bea", True)] False
>             -- beautify code file
>             beaf  = case getParams s "-bea" of
>                      []        -> ""
>                      [x]       -> x
>                      x         -> error $ "cannot beautify file " ++ sepList " " x
>             -- what table configurations needed
>             tc = altParams s [("-tg", TCGood),("-to", TCOptimal),("-tx", TCApprox),("-te", TCExtern),("-th", TCOptimal),
>                               ("-ctg", TCGood),("-cto", TCOptimal),("-ctx", TCApprox)] TCNone
>             -- ignore user annotated table configuration
>             iuc = altParams s [("-iuc", True)] False
>             -- only preprocess table configuration
>             tp   = altParams s [("-tp", True)] False
>             -- skip preprocessing of table configuration
>             sp   = altParams s [("-sp", True)] False
>             -- number of additional tables
>             tadd = case getParams s "-tadd" of
>                      []  -> 0
>                      [x] -> if all isDigit x then (read x) :: Int
>                                              else error $ "number required for parameter -tadd: "  ++ x
>                      otherwise ->                 error $ "number required for parameter -tadd."
>             -- necessary constant factor improvement for additional tables:
>             taddc = case getParams s "-taddc" of
>                      []  -> 50
>                      [x] -> if all isDigit x then (read x) :: Int
>                                              else error $ "number required for parameter -taddc: "  ++ x
>                      otherwise ->                 error $ "number required for parameter -taddc."
>             -- expected input length for constant factor improvement:
>             taddn = case getParams s "-taddn" of
>                      []  -> -1
>                      [x] -> if all isDigit x then (read x) :: Int
>                                              else error $ "number required for parameter -taddn: "  ++ x
>                      otherwise ->                 error $ "number required for parameter -taddn."
>             -- show only axiom
>             ar   = altParams s [("-ar", False)] True
>             -- show all results
>             arr  = altParams s [("-arr", True)] False
>             -- specify output format
>             off  = altList s "-of" [("h",OFHaskell), ("hl",OFHaskellLines), ("s", OFSpace), ("l", OFLines)] OFSpace "output format"
>             -- leak choice function into inner expressions
>             lcf   = altParams s [("-lcf", True)] False
>             -- table allocation scheme
>             tas   = altListSort s "-ta" [("p",[TASPointers]),  
>                                          ("b", [TASBlock]), ("bi", [TASBlock, TASBlockInterleave]), ("bt", [TASBlock, TASTriangularBlock]), 
>                                          ("bo", [TASBlock, TASOffset]), ( "bit", [TASBlock, TASBlockInterleave, TASTriangularBlock]), 
>                                          ("bio",  [TASBlock, TASBlockInterleave, TASOffset]), ("bto", [TASBlock,TASTriangularBlock,TASOffset]), 
>                                          ( "bito", [TASBlock,TASTriangularBlock,TASBlockInterleave, TASOffset])]
>                                          [TASPointers] "table allocation scheme"

>             -- inline table access macros
>             ita   = altParams s [("-ita", True)] False
>             -- inline functions
>             ifct   = altParams s [("-if", True)] False
>             -- include window mode 
>             window  = altParams s [("-W", True)] False
>             -- target code optimization
>             toptO   = altParams s [("-O", True)] False
>             -- target code optimization
>             toptO2   = altParams s [("-O2", True)] False
>             -- generate garbage collected code
>             gc   = altList s "-gc" [("cc",GCcc), ("bdw", GCbdw), ("own", GCown)] GCNone "garbage collecting mode"
>             -- c compiler command line
>             cc   = case sepList " " $ getParams s "-cc" of
>                      ""  -> stdCmdcc tl
>                      x   -> x
>             -- pretty printing algebra
>             ap  = case getParams s "-ap" of
>                      []        -> ""
>                      [x]       -> x
>                      x         -> error $ "more than one patch file given: " ++ sepList " " x
>             -- algebra order
>             algs = getParams s "-al"
>             -- pretty printing algebra
>             alpp  = getParams s "-alpp" 
>             -- automatically create signature
>             cs    = case getParams s "-cs" of
>                      []        -> ""
>                      [x]       -> x
>                      x         -> error $ "more than one algebra specified: " ++ sepList " " x

>             -- parser check (parse1 vs parse2)
>             pc    = altParams s [("-pc", True)] False
>             pn    = altList s "-pn" [("1", 1),("2", 2)] 2 "parser number"

>             -- output file
>             fo   = case getParams s "-o" of
>                      []        -> if altParams s [("-n", True)] False then [] else "stdout"
>                      [x]       -> x
>                      x         -> error $ "cannot handle output file " ++ sepList " " x

>             -- input file
>             fi   = case concatMap (getParams s) ["-c","-b","-z","-l","-g","-tg","-to","-tx","-te","-th"] of
>                      []        -> error "no input file given"
>                      [x]       -> x
>                      x         -> error $ "cannot handle input file " ++ sepList " " x

>             -- typecheck
>             typecheck   = altParams s [("-tc", True)] False

>             searchpath = case getParams s "-path" of
>                      []        -> ""
>                      [x]       -> x
>                      x         -> error "more than one searchpath given"

Tools:

> -- | Returns the arguments of an option
> getParams :: [String] -- ^ argv
>           -> String -- ^ Option, e.g. -foo
>           -> [String] -- ^ list of successive arguments
> getParams [] _     = []
> getParams (s:ss) p | s == p    = getParams' ss 
>                    | otherwise = getParams ss p
>      where
>        getParams' []     = []
>        getParams' (s:ss) | elem s paramList = []
>                          | otherwise        = s:getParams' ss

> -- | Chooses the corresponding value from a set of nullary options or the default.
> altParams :: (Eq a) => [String] -- ^ argv
>           -> [(String, a)] -- ^ combinations of options and values, e.g. '-foo'
>           -> a -- ^ default value
>           -> a
> altParams s alts std | used == [] = std
>                      | otherwise  = head used
>       where used = [ res | (p, res) <- alts, elem p s]

> -- | Returns the corresponding value to the first argument of an option or the default
> altList :: (Eq a) => [String] -- ^ argv
>         -> String -- ^ option
>         -> [(String,a)] -- ^ possible arguments and their mapped values
>         -> a -- ^ default value 
>         -> String -- ^ verbose desc of option for error message
>         -> a
> altList s param alts std name = case getParams s param of
>                                  [] -> std
>                                  x  | used == [] -> error $ "cannot handle " ++ name ++ " " ++ sepList " " x
>                                     | otherwise  -> head used 
>                                     where used = [ res | (p, res) <- alts, [p] == x]

> altListSort :: (Eq a) => [String] -> String -> [(String,a)] -> a -> String -> a
> altListSort s param alts std name = case getParams s param of
>                                      [] -> std
>                                      [x] | used == [] -> errmsg [x]
>                                          | otherwise  -> head used 
>                                          where used = [ res | (p, res) <- alts, sort p == sort x]
>                                      x -> errmsg x
>     where errmsg x = error $ "cannot handle " ++ name ++ " " ++ sepList " " x


> splitArgs a = filter (/="") (spla a)
>   where
>     spla     :: String -> [String]
>     spla ""   = []
>     spla s    = let (l,s') = break (' '==) s
>                  in l : case s' of []      -> []
>                                    (_:s'') -> spla s''

> rev = maximum $ map (getRev.snd) revisions
> revs = mapsep "\n" srev (sort revisions)
>   where
>     mlen = maximum $ map (length.fst) revisions
>     srev (n, v) = n ++ spc (mlen - length n + 1) ++ ": " ++ show (getRev v) 

> revisions = [
>   ("Constants", rev_Constants),
>   ("Tools", rev_Tools),
>   ("MathExp", rev_MathExp),
>   ("TLData", rev_TLData),
>   ("Structs", rev_Structs),
>   ("Expr", rev_Expr),
>   ("StringLib", rev_StringLib),
>   ("Syntax", rev_Syntax),
>   ("Lex", rev_Lex),
>   ("Parse", rev_Parse),
>   ("Yieldsize", rev_Yieldsize),
>   ("Adptrans", rev_Adptrans),
>   ("Dss", rev_Dss),
>   ("Depana", rev_Depana),
>   ("TL", rev_TL),
>   ("IL2", rev_IL2),
>   ("Codegen", rev_Codegen),
>   ("Codegen1", rev_Codegen1),
>   ("TLFrame", rev_TLFrame),
>   ("Poly", rev_Poly),
>   ("WidthAna", rev_WidthAna),
>   ("LatexRecs", rev_LatexRecs),
>   ("SM", rev_SM),
>   ("Phases", rev_Phases),
>   ("Compile", rev_Compile),
>   ("Track", rev_Track),
>   ("Range", rev_Range),
>   ("Algebras", rev_Algebras),
>   ("Beautify", rev_Beautify),
>   ("PrettyPrint", rev_PrettyPrint)]


Some stuff for Alggen to avoid circular dependencies:

> alggen_file :: AlggenMode -> String -> String -> IO()
> alggen_file mode f fo = do
>                 inp <- if elem '>' f then return f else readFile f
>                 res <- alggen mode inp
>                 if fo == "stdout" then putStr res
>                                   else writeFile fo res

> alggen :: AlggenMode -> String -> IO String
> alggen mode inp = do
>                     (algs, axiom, (g1,g2,g3)) <- return $ alggen_parts mode inp
>                     g2'                       <- optimize axiom g2
>                     return $ algs ++ g1 ++ g2' ++ "Bind input:\n\n" ++ g3 ++ tableBind g2'
>    where
>      tableBind grammar = (if elem 't' usedTabulation then spc ++ "tabulated = table n\n" else "") ++
>                          (if elem 'i' usedTabulation then spc ++ "listed    = list n\n" else "") ++
>                          (if elem 'j' usedTabulation then spc ++ "listedj   = listj n\n" else "")
>        where
>          spc = ">   "
>          usedTabulation = findtab [] grammar
>            where
>              findtab xs [] = xs
>              findtab xs ('t':'a':'b':'u':'l':'a':'t':'e':'d':cs) = findtab ('t':xs) cs
>              findtab xs ('l':'i':'s':'t':'e':'d':'j':cs)         = findtab ('j':xs) cs
>              findtab xs ('l':'i':'s':'t':'e':'d':cs)             = findtab ('i':xs) cs
>              findtab xs (c:cs)                                   = findtab xs       cs




> optimize :: String -> String -> IO String
> optimize axiom input = do
>                   inp' <- return $  "grammar[" ++ axiom ++ "]{\n" ++ input ++ "\n}\n"
>                   (result,_) <- cmain ["-z",inp',"-cto","-iuc"]
>                   return $ getResult result
>    where
>      getResult s = fil2 $ unlines $ map fil $ arrange "|." $ arrange "<" $ tail $ init $ init $ lines s
>      fil xs = fil' xs xs
>        where
>          fil' xs [] = xs
>          fil' xs ('-':'-':' ':'p':'r':'o':_) = ""
>          fil' xs ('-':'-':'e':'n':'d':_)     = ""
>          fil' xs (c:cs)                      = fil' xs cs
>      fil2 [] = []
>      fil2 ('\n':'\n':'\n':cs)        = '\n' :'\n' : fil2 cs
>      fil2 (c:cs)                     = c: fil2 cs

