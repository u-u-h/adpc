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


> module Main

> where


> import System
> import System.Console.GetOpt
> import Monad
> import IO
> import List
> import Maybe

are not available in ghc 6.4 - thus until now they are in Helpers ...
 import System.FilePath
 import Data.List(isInfixOf)

> import Helpers
> import Macro
> import Option

> import Layout(layout,stripComments,processImports)

> import ConfigOutput

> import Parse2(parse2)
> import ParseMonad
> import ParseTree
> import Compile(cmainInt)
> import Constants

> import Directory

> import TC(getTCresult)


Option handling follows
http://groups.google.de/group/fa.haskell/browse_thread/thread/85f157f58d7cd122/48f8140f7c216c32
High-level technique for program options handling, Zielonka, 2004

> data OutputMode = RNAMode | NormalMode | ADMode deriving (Show, Eq)

> data Options = Options {
>   optLanguage :: Output,
>   optList :: Bool,
>   optHelp :: Bool,
>   optMode :: OutputMode,
>   optAlgebras :: [String],
>   optTemplate:: Maybe String,
>   optPrefix :: String,
>   optPurePrefix :: Bool,
>   optSearchPath :: String,
>   optFlags :: [String],
>   optOutputDir :: String,
>   optPackage :: String,
>   optVerbose :: Bool,
>   optTypeCheck :: Bool,

>   optFilename :: String,
>   optProgname :: String
> } deriving Show

> defaultOptions = Options { optLanguage = Java,
>                            optList = False,
>                            optHelp = False,
>                            optMode = ADMode,
>                            optAlgebras = [],
>                            optTemplate = Nothing,
>                            optPrefix = prefix,
>                            optPurePrefix = False,
>                            optSearchPath = "",
>                            optFlags = [],
>                            optOutputDir = [],
>                            optPackage = [],
>                            optVerbose = False,
>                            optTypeCheck = False,

>                            optFilename = [],
>                            optProgname = [] }

> options :: [ OptDescr (Options -> Options) ]
> options = [
>   Option "j" ["java"] (NoArg (\opt -> opt { optLanguage = Java }) )
>     "create Java output",
>   Option "c" [] (NoArg (\opt -> opt { optLanguage = C}) )
>     "create C output",
>   Option "r" ["rna"] (NoArg (\opt -> opt { optMode = RNAMode }))
>     "force RNA output Mode",
>   Option "n" ["normal"] (NoArg (\opt -> opt { optMode = NormalMode }))
>     "force Normal output Mode",
>   Option "a" ["algebras"] (ReqArg (\s opt ->
>     opt { optAlgebras = split ',' s } ) "a1,..,an")
>     "list of algebras separated by commas, default: all",
>   Option "t" ["template"] (ReqArg (\s opt ->
>     opt { optTemplate = Just s } ) "FILENAME")
>     "filename which contains the @@-Template",
>   Option "p" ["prefix"] (ReqArg (\s opt ->
>     opt { optPrefix = s } ) "PATH")
>     "Path were to look for the default template files (+share/adpc/templates).",
>   Option "i" ["include"] (ReqArg (\s opt ->
>     opt { optSearchPath = s } ) "PATH")
>     "Searchpath for imports",
>   Option "f" ["flags"] (ReqArg (\s opt ->
>     opt { optFlags = words s } ) "f1 ... fn")
>     ("additional flags which are passed to adpcompile; they should overwrite"
>     ++ " the default ones; don't forget to shell-quote them!"),
>   Option "l" ["list"] (NoArg (\opt -> opt { optList = True }) )
>     "list all algebras",
>   Option "o" ["output"] (ReqArg (\s opt ->
>     opt { optOutputDir = s } ) "PATH")
>     ("output dir where to put the generated java files," ++ 
>      " default: cwd + package-dir"),
>   Option "x" ["package"] (ReqArg (\s opt ->
>     opt { optPackage = s } ) "STRING")
>     "java package to use, default is the filename without .lhs suffix",
>   Option "v" ["verbose"] (NoArg (\opt -> opt { optVerbose = True }) )
>     "print some additional internal state",
>   Option "h" ["help"] (NoArg (\opt -> opt { optHelp = True }) )
>     "show this nice help screen.",
>   Option "" ["pure-prefix"] (NoArg (\opt -> opt { optPurePrefix = True }) )
>     "Don't use the share/apdc/templates suffix for the prefix.",
>   Option "w" ["typecheck"] (NoArg (\opt -> opt {optTypeCheck = True } ) )
>     "Typecheck the ADP-source."
>   ]


> getParse opts = do
>   input <- readFile (optFilename opts)
>   let stripped = stripComments input
>   processed <- processImports (optPrefix opts) (optSearchPath opts)
>                               (optFilename opts) 1 stripped
>   let l = layout processed
>   let parsed = parse2 l (optFilename opts)
>   return parsed

> getAlgebras (Ok (ParseTree.ADPProgram _ _ _ algebras _)) =
>   map getName algebras
>   where
>     getName (Algebra _ (AlgebraFunType _ name _ _) _) = name

> splitAlgebras :: [String] -> ([String], [String])
> splitAlgebras = partition (\a -> or [a == "prettyprint", a == "pp"])

if no pretty printing algebra is found, no -alpp means, that adpcompile
automatically generates an enumerate-PrettyPrinter

> getPPFlag :: [String] -> [String]
> getPPFlag [] = [] 
> getPPFlag pps = ["-alpp", head pps]

> errExit s = hPutStrLn stderr s >> exitFailure

> infoLn = hPutStrLn stderr

> _defaultFlags =
>   ["-O", "-lcf", "-ta", "bt", "-bt", "so", "-gc", "cc", 
>    "-iuc", "-cto", "-n", "-tadd", "3", "-taddc", "30"]

> defaultFlags RNAMode = _defaultFlags ++ ["-W"]
> defaultFlags NormalMode = _defaultFlags

> ppFileSuffix Java = ".java"

> ppOutputMode RNAMode = "rna"
> ppOutputMode NormalMode = "normal"

> algebraName name = "Algebra_" ++ name

> ppPackage opts = "package " ++ optPackage opts ++ ";"

> ppIdent algebra = "String ident = \"" ++ algebra ++ "\";"

> generateAlgebra parseTree flags opts ms algebra = do
>   infoLn ("Generating Code for Algebra: " ++ algebra)
>   let macros = ("@ALGEBRA_NAME@", algebra):("@PACKAGE@", ppPackage opts):("@IDENT@", ppIdent algebra):ms
>   (output, err) <- cmainInt ((optFlags opts) ++ flags ++ (defaultFlags (optMode opts)) ++ 
>     ["-c", optFilename opts, "-al", algebra, "enum", "-cs", algebra, "-tl",
>       ppOutputOption (optLanguage opts)])

>       parseTree (macroFilter (strictLookup macros))

       parseTree (macroFilter id)

>   when (err /= -1) (errExit output)

>   writeFile ((optOutputDir opts) ++ [pathSeparator] ++
>     (algebraName algebra) ++ 
>     (ppFileSuffix (optLanguage opts)))
>     output

   putStrLn output

> constructTmplName opts = optPrefix opts ++ 
>   suffix
>   ++ ps ++ 
>   ppOutputMode (optMode opts) ++ ".tmpl"
>    where
>      ps = [pathSeparator]
>      suffix = if optPurePrefix opts then "" else
>         ps ++ "share" ++ 
>         ps ++ "adpc" ++ ps ++ "templates" 


> readMacros opts = do
>   let filename = if optTemplate opts == Nothing 
>                     then constructTmplName opts 
>                     else fromJust (optTemplate opts)
>   input <- readFile filename
>   return $ parseTemplate input

> construct a b algs = map (++a)
>   (map (b++)  algs)

> constructImports opts algs =
>   concat $ construct ";\n" ("import " ++ (optPackage opts) ++ ".Algebra_")  algs

> constructItr list prefix suffix = zipWith f [1..] list
>   where
>     f a _ = prefix ++ n ++ suffix
>      where n = (show a)

> constructAlgebraCalls algs = 
>   concat $ constructItr algs "  alg_" ".init(seq, opts);\n" ++
>     constructItr algs "  alg_" ".start();\n"

> ppAlg RNAMode = "RnaAlgebra"
> ppAlg NormalMode = "StdAlgebra"

> constructAlgebraInit algs opts = concat $ zipWith f x algs
>   where
>    f a b = a ++ b ++ "();\n"
>    x = constructItr algs ("  " ++ (ppAlg (optMode opts)) ++ " alg_") " = new Algebra_"

> constructAlgebraOpt algs = 
>   concat $ constructItr algs "  op.init(alg_" ");\n"

> compileMainfile opts macros algs Java =
>   "package " ++ (optPackage opts) ++ ";\n\n" ++
>   constructImports opts algs ++ "\n" ++
>   strictLookup macros "@MAIN_HEADER@" ++
>   "public class " ++ (optProgname opts) ++ " {\n" ++
>   strictLookup macros "@MAIN_BODY@" ++
>   constructAlgebraInit algs opts ++
>   strictLookup macros "@MAIN_2ND@" ++
>   constructAlgebraOpt algs ++
>   strictLookup macros "@MAIN_3RD@" ++
>   constructAlgebraCalls algs ++
>   strictLookup macros "@MAIN_TAIL@"


> generateMainfile opts macros algs = do
>   let output = compileMainfile opts macros algs (optLanguage opts)
>   writeFile ((optOutputDir opts) ++ [pathSeparator] ++ (optProgname opts) ++
>     (ppFileSuffix (optLanguage opts)))
>     output

> makeOutputDir opts = do
>   r <- doesFileExist (optOutputDir opts)
>   when r
>     (errExit ("Output directory " ++ (optOutputDir opts)
>                ++ " exists as a file!"))
>   r <- doesDirectoryExist (optOutputDir opts)
>   when (not r)
>     (createDirectory (optOutputDir opts))
 
> generateCode parseTree opts algebras = do
>   let (pps, algs) = splitAlgebras algebras
>   let ppFlag = getPPFlag pps
>   macros <- readMacros opts
>   makeOutputDir opts
>   mapM_ (generateAlgebra parseTree ppFlag opts macros) algs 
>   generateMainfile opts macros algs
>   return()

> setFilename opts names = do
>   when (length names /= 1)
>     (hPutStrLn stderr ("You have to specify _ONE_ filename.") >> exitFailure)
>   return opts { optFilename = head names }

only Debug Test

> ppAlgebra (Ok (ParseTree.ADPProgram _ _ _ algebras _)) = show algebras

> autodetectOutput (Ok (ParseTree.ADPProgram _ _ _ algebras _))
>  | or $ map searchAlg algebras = RNAMode
>  | otherwise = NormalMode
>  where
>   searchAlg (Algebra _ _ (AlgebraFunDef _ _ _ funDefs)) =
>     or $ concatMap searchFunDef funDefs
>   searchFunDef (FunDef _ _ _ expression) =  expressionMap f expression
>   f (FunAp ident _) = [isInfixOf "energy" ident]
>   f _ = [False]

> setMode opts p = do
>   let a = autodetectOutput p
>   when (optMode opts == ADMode)
>     (infoLn ("Autodetected: " ++ show a))
>   return opts { optMode = if optMode opts == ADMode then a else optMode opts }

> setOpts opts = opts { optProgname = progname,
>                       optPackage = new_p,
>                       optOutputDir = o ++ map (\x -> if x == '.' then pathSeparator else x) new_p }

>   where
>     (_, progname, _) = splitFilePath (optFilename opts)
>     p = optPackage opts
>     new_p = if p == [] then progname else p
>     o = if optOutputDir opts == [] then [] else optOutputDir opts ++
>            [pathSeparator]

> main :: IO ()
> main = do
>   (opts, names) <- getArguments defaultOptions options
>   progName <- getProgName
>   when (optHelp opts) (putStrLn (usageInfo progName options) 
>                        >> exitWith ExitSuccess)
>   opts <- setFilename opts names
>   p <- getParse opts
>   when (failed p)
>     $ errExit $ show p

>   when (optTypeCheck opts)
>     $ putStrLn $ fst $ getTCresult p

>   let algebras = getAlgebras p
>   opts <- setMode opts p
>   let final_opts = setOpts opts

>   if optList opts
>     then putStr (unlines algebras)
>     else generateCode p final_opts algebras

>   when (optVerbose opts)
>     $ infoLn $ show final_opts

   putStrLn (show l)
   putStrLn (show parsed)

   putStrLn (ppAlgebra p)

