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



> module CoreParser where

> import Char
> import List
> import Language.Haskell.Parser
> import Language.Haskell.Syntax

> import Infix

> import CoreTools
> import CoreSyntax




Load a given program, all dependent modules and parses them.

> parseWithImport :: String -> IO Prog
> parseWithImport fileName = do
>                            (progs, infixMap) <- loadAll basicInfixMap ["System", "List", "Char", "Array"] [fileName]
>                            let decls = concatMap ( \ (HsModule sl mdl expSpec impSpec decls) -> decls ) progs
>                            let progs' = concatMap tHsDecl $ filter filterDecl $ infixer infixMap decls
>                            return progs'
>      where filterDecl decl = case decl of
>                              (HsFunBind _) -> True
>                              (HsPatBind _ _ _ _) -> True
>                              otherwise -> False


Loads a list of programs and returns the parse results and
the resulting InfixMap

> loadAll :: InfixMap -> [String] -> [String] -> IO ([HsModule],InfixMap)
> loadAll infixMap loadedFiles fileNames = do
>                                         --  putStrLn $ "files to load: " ++ (foldl1 (++) fileNames)
>                                          files' <- mapM loadFile fileNames
>                                          let loadedFiles' = fileNames ++ loadedFiles
>                                              (progs,infixMap',importDecls) = processModules infixMap files' loadedFiles'
>                                          (progs',infixMap'') <- if fileNames == [] then return (progs,infixMap')
>                                                                                    else loadAll infixMap' loadedFiles' importDecls
>                                          return (progs' ++ progs, infixMap'')


Loads the named file and does some preprocessing
(like unlit of the literate script)

> loadFile :: String -> IO String
> loadFile fileName = do
>                     putStrLn $ "loading " ++ fileName
>                     let fileName1 | endsWith fileName ".lhs" || endsWith fileName ".hs" = fileName
>                                   | otherwise                                           = fileName ++ ".hs"
>                     let fileName2 | endsWith fileName ".lhs" || endsWith fileName ".hs" = fileName
>                                   | otherwise                                           = fileName ++ ".lhs"
>                     file1 <- catch (readFile fileName1) (\_ -> return emptyFileTag)
>                     file2 <- if file1 == emptyFileTag then readFile fileName2
>                                                       else return file1
> --                    file2 <- if file1 == emptyFileTag then catch (readFile fileName2) (\_ -> return emptyFileTag)
> --                                                      else return file1
>                     let usedFileName | file1 == emptyFileTag = fileName2
>                                      | otherwise             = fileName1
>                     let file' = unlit usedFileName file2
>                     return file'


Constant needed by loadFile

> emptyFileTag :: String
> emptyFileTag = "{" ++ "-_emptyFile_-" ++ "}" -- zusammengesetzt: "{-_emptyFile_-}"


Parses a single module, and returns all important information
in a tuple: the HsModule itself, the InfixMap of defined operators
and a List of Strings naming all dependent modules

> processModule :: InfixMap -> String -> (HsModule, InfixMap,[String])
> processModule infixMap inp = let prog = parseString inp
>                                  infixMap' = insertInfixMap infixMap prog
>                              in (prog, infixMap', getImportDecls prog)


Parses all given Programs (2. parameter), and returns
am InfixMap resulting from the List of Modules, a List
of Modules, that havn't been loaded so far.

> processModules :: InfixMap -> [String] -> [String] -> ([HsModule],InfixMap,[String])
> processModules infixMap [] is     = ([],infixMap,[])
> processModules infixMap (m:ms) is = let (ms', infixMap',importDecls2) = processModules infixMap ms is
>                                         (m', infixMap'', importDecls1) = processModule infixMap' m
>                                         importDecls = (filter (not . (flip elem is)) importDecls1) ++ importDecls2
>                                     in (m':ms', infixMap'', importDecls)


> readTest :: String -> IO ()
> readTest fileName = do
>                     file1 <- catch (readFile fileName) ( \ _ -> return "file can't be opened")
>                     file2 <- catch (readFile $ fileName ++ ".lhs") ( \ _ -> return "can't be found" )
>                     putStrLn $ show file1
>                     putStrLn $ show file2


transforms a program given by a string into a core program

> parse :: String -> Prog
> parse = (fst . transformCore) . parseString

starts the parser and returns the result, if there is one,
otherwise it displays an error message
(this can be modified, the return type will be the
data type Maybe, with "Just result" or "Nothing" in case
of an error)

> parseString :: String -> HsModule
> parseString inp = case parseModule inp of
>      ParseOk result          -> result
>      ParseFailed srcLoc msg  -> error $ msg ++ " at " ++ (show srcLoc)


passes the modules filename to the parser

> parseWithFilename :: String -> String -> HsModule
> parseWithFilename name inp = case doParse of
>      ParseOk result          -> result
>      ParseFailed srcLoc msg  -> error $ msg ++ " at " ++ (show srcLoc)
>     where doParse = parseModuleWithMode (ParseMode {parseFilename=name}) inp


transforms the result of the parser in the desired core structure

> transformCore :: HsModule -> (Prog, [String])
> transformCore pTree = let (HsModule sl mdl expSpec impSpec decls) = pTree
>                           infixMap = insertInfixMap basicInfixMap pTree
>                           importDecls = map ( \ (HsImportDecl _ (Module name) _ _ _) -> name) impSpec
>                       in (concatMap tHsDecl $ filter filterDecl $ infixer infixMap decls, importDecls)
>      where filterDecl decl = case decl of
>                              (HsFunBind _) -> True
>                              (HsPatBind _ _ _ _) -> True
>                              otherwise -> False


Read the import declarations and returns a list ofmodule
names to be imported

> getImportDecls :: HsModule -> [String]
> getImportDecls pTree = let (HsModule sl mdl expSpec impSpec decls) = pTree
>                        in map ( \ (HsImportDecl _ (Module name) _ _ _) -> name ) impSpec


Inserts top infix declarations of the module into the given infix map

> insertInfixMap :: InfixMap -> HsModule -> InfixMap
> insertInfixMap map pTree = let (HsModule sl mdl expSpec impSpec decls) = pTree
>                            in insertTopInfixDecls mdl map decls


An basic InfixMap of operators and their fixity

> basicInfixMap :: InfixMap
> basicInfixMap = insertTopInfixDecls (Module "Prelude") emptyInfixMap basicInfixDecls
>    where smbl = HsVarOp . HsSymbol
>          smbl' = HsVarOp . HsIdent
>          basicInfixDecls = [HsInfixDecl srcLoc HsAssocRight 9 [smbl "."],
>                             HsInfixDecl srcLoc HsAssocLeft 9 [smbl "!!"],
>                             HsInfixDecl srcLoc HsAssocRight 8 [smbl "^", smbl "^^", smbl "**"],
>                             HsInfixDecl srcLoc HsAssocLeft 7 [smbl "*", smbl "/", smbl' "`quot`",
>                                                               smbl' "`rem`", smbl' "`div`", smbl' "`mod`",
>                                                               smbl ":%", smbl "%"],
>                             HsInfixDecl srcLoc HsAssocLeft 6 [smbl "+", smbl "-"],
>                             HsInfixDecl srcLoc HsAssocRight 5 [smbl ":", smbl "++"],
>                             HsInfixDecl srcLoc HsAssocNone 4 [smbl "==", smbl "/=", smbl "<",
>                                                               smbl "<=", smbl ">=", smbl ">",
>                                                               smbl' "`elem`", smbl' "`notElem`"],
>                             HsInfixDecl srcLoc HsAssocRight 3 [smbl "&&"],
>                             HsInfixDecl srcLoc HsAssocRight 2 [smbl "||"],
>                             HsInfixDecl srcLoc HsAssocLeft 1 [smbl ">>", smbl ">>="],
>                             HsInfixDecl srcLoc HsAssocRight 1 [smbl "=<<"],
>                             HsInfixDecl srcLoc HsAssocRight 0 [smbl "$", smbl "$!", smbl "`seq`"]]
>          srcLoc = SrcLoc { srcFilename = "MyPrelude", srcLine = 1, srcColumn = 1}


test function:

> fixInfix pTree = let (HsModule sl mdl expSpec impSpec decls) = pTree
>                      infixMap = insertTopInfixDecls mdl emptyInfixMap decls
>                  in infixer infixMap decls


transforms a single function declaration 

> tHsDecl :: HsDecl -> [Def]
> tHsDecl (HsFunBind fns) = map tHsMatch fns
> tHsDecl (HsPatBind sl hsPat rhs whdcls) = result patts -- [(name, [], core)]
>     where patts = tHsPat hsPat
>           result (ArgVar name)    = [(name, [], core)]
>           result tpl@(ArgTuple _) = [("", [tpl], core)]
>           core = if length whdcls == 0 then tHsRhs rhs
>                                        else Let letDecls $ tHsRhs rhs
>           letDecls = concatMap tHsDecl whdcls

transforms a single HsMatch into a Core structure

> tHsMatch :: HsMatch -> Def
> tHsMatch (HsMatch sl hsName pts rhs whdcls) = 
>     let name = tHsName hsName
>         core = if length whdcls == 0 then tHsRhs rhs
>                                      else Let letDecls $ tHsRhs rhs
>         params = map tHsPat pts
>         letDecls = concatMap tHsDecl whdcls
>     in (name, params, core)


transforms the right hand side of a function definition.

> tHsRhs :: HsRhs -> Core
> tHsRhs (HsUnGuardedRhs hsExp) = tHsExp hsExp
> tHsRhs x                      = error "guards not supported!"


extracts the name out of this data type. HsIdent is used for
identifiers written with normal characters, HsSymbol is used
for names written with special characters

> tHsName :: HsName -> String
> tHsName (HsIdent name)  = name
> tHsName (HsSymbol name) = name


transforms a qualified name definition into the name string.

> tHsQName :: HsQName -> String
> tHsQName (UnQual hsName) = tHsName hsName
> tHsQName (Special sc)    = tHsSpecialCon sc
> tHsQName x               = error $ "unsupported qualified name definition." ++ show x


transforms spesial constructor data types
into their string representation.

> tHsSpecialCon :: HsSpecialCon -> String
> tHsSpecialCon HsUnitCon = "()"
> tHsSpecialCon HsListCon = "[]"
> tHsSpecialCon (HsTupleCon n) = "(" ++ (replicate (n-1) ',') ++ ")"
> tHsSpecialCon HsCons = "(:)"


transforms an operator name into a String

> tHsOp :: HsOp -> String
> tHsOp (HsVarOp var) = tHsName var
> tHsOp (HsConOp con) = tHsName con


transforms a qualified variable name operator into a string

> tHsQOp (HsQVarOp op) = tHsQName op
> tHsQOp (HsQConOp op) = tHsQName op


transforms an expression

> tHsExp (HsLit lit)           = tHsLiteral lit
> tHsExp (HsVar v)             = Var $ tHsQName v
> tHsExp (HsApp exp1 exp2)     = Ap (tHsExp exp1) (tHsExp exp2)
> tHsExp (HsInfixApp exp1 op exp2) = Ap (Ap (Var $ tHsQOp op) (tHsExp exp1))
>                                       (tHsExp exp2)
> tHsExp (HsListComp r cs)     = Lc ((tHsExp r) : (map tHsStmt cs))
> tHsExp (HsEnumFromTo a b)    = (tHsExp a) :.. (tHsExp b)
> tHsExp (HsIf e1 e2 e3)       = If (tHsExp e1) (tHsExp e2) (tHsExp e3)
> tHsExp (HsParen exp)         = tHsExp exp
> tHsExp (HsCon name)          = Constr $ tHsQName name
> tHsExp (HsList [])           = Constr "[]" -- Lc [] -- Constr "[]"
> tHsExp (HsList [c])          = Lc [tHsExp c]
> tHsExp (HsList (c:cs))       = foldr ( \ e1 e2 -> Ap (Ap (Var "++") e1) e2 ) (Lc [tHsExp c]) $ map ( \ c -> Lc [tHsExp c]) cs
> tHsExp (HsTuple exps)        = applc (Constr "(,)") (map tHsExp exps)
> tHsExp (HsLet hsDecls exp)   = Let (concatMap tHsDecl hsDecls) (tHsExp exp)
> tHsExp (HsLambda _ args exp) = Lambda (map tHsPat args) (tHsExp exp)
> tHsExp (HsNegApp hsExp)      = Ap (Var "neg") (tHsExp hsExp)
> tHsExp exp                   = pattErr "tHsExp" exp


transforms a statement like structure defining generators
and so on listed in a list comprehension.

> tHsStmt :: HsStmt -> Core
> tHsStmt (HsGenerator sl hsPat hsExp) = (tHsPat hsPat) :<- (tHsExp hsExp)
> tHsStmt (HsQualifier exp) = tHsExp exp
> tHsStmt (HsLetStmt decls) = LcLet $ concatMap tHsDecl decls

> tHsPat :: HsPat -> Arg
> tHsPat (HsPVar (HsIdent ident)) = ArgVar ident
> tHsPat (HsPTuple args)          = ArgTuple $ map tHsPat args
> tHsPat (HsPWildCard)            = ArgWildCard
> tHsPat x                        = error $ "pattern definition not supported."
>                                   ++ "\n" ++ (show x)


transforms a source literal into a core literal

> tHsLiteral :: HsLiteral -> Core
> tHsLiteral (HsInt n)       = Num (read (show n) :: Int)
> tHsLiteral (HsString str)  = Value (VString str)
> tHsLiteral (HsChar ch)     = Value (VChar ch)
> -- tHsLiteral (HsFrac f)     = Value (VFloat)   -- stimmt das?
> tHsLiteral x               = error $ "unsupported literal definition."
>                                      ++ "\n" ++ (show x)


===========================================================================

Beispielprobleme, die geloest werden sollen:

die Funktion
  f x y = map (\z -> x*z) [1,2,3]
wird zu
  f x y = let sc x z in map (sc x) [1,2,3]

die Funktion
  g x = let f y z = y*z in foldl f 1 [1,2,3]
wird zu
  g_f y z = y*z
  g x = foldl g_f 1 [1,2,3]

die Funktion
  f x y = map (\z -> x*z) [1,2,3]
wird zu
  f x y = let sc x z = x*z in map (sc x) [1,2,3]
wird zu
  f_sc x z = x*z
  f x y = map (f_sc x) [1,2,3]



> -- for testing the lambda lifter
> -- ====================================
> testLambda = putStrLn . ppProg . lambdaLift . parse
> -- testLambda1 = putStrLn . ppProg . rename . removeLambda . freeVars . removeTuple . parse
> -- testLambda2 = putStrLn . ppProg . removeLambda . freeVars . removeTuple . parse
> -- testLambda3 = putStrLn . ppProg . removeTuple . parse
> -- testLambda4 = putStrLn . ppProg . parse
> testLambdaIO iFileName oFileName = do
>                                    file <- readFile iFileName
>                                    let file' = unlit iFileName file
>                                        outp = ppProg $  lambdaLift $ parse file'
>                                    writeFile oFileName $ outp
> --                                   putStr $ outp
> -- BUG: Hugs scheint nicht in der Lage zu sein
> --      die Variable 'file' ein zweites mal zu
> --      verwenden -> Segmentation Fault
> -- testLambda3 iFileName oFileName = do
> --                                   file <- readFile iFileName
> --                                   let file = unlit iFileName file
> --                                       outp = ppProg $  lambdaLift $ parse file
> -- --                                  writeFile oFileName $ outp
> --                                   putStr $ outp -- hier koennte auch 'file' stehen
> -- ====================================

the lambda lifter:

> lambdaLift :: Prog -> Prog
> lambdaLift prog = let (prog', ns) = removeTuple initialNameSupply prog
>                   in (removeLet . rename . removeLambda . freeVars) prog'


befreit den ParseTree von Musterbindungen an Tupel

> removeTuple :: NameSupply -> Prog -> (Prog, NameSupply)
> removeTuple ns prog = let ns = foldl buildNameSupply initialNameSupply $ map fst3 prog
>                           foldTupleDef (defs, ns) def = let (defs', ns') = removeTupleDef ns def
>                                                         in (defs' ++ defs, ns')
>                       in foldl foldTupleDef ([], ns) prog


> removeTupleDef :: NameSupply -> Def -> ([Def], NameSupply)
> removeTupleDef ns (name, args, core) | name == "" = let (lazyFunName, ns') = nameSupply ns "_tuple_binder_"
>                                                         (core', ns'') = removeTupleCore ns' core
>                                                         lazyFun = (lazyFunName, [], core')
>                                                         (argFuns, ns''') = removeTupleArgs ns'' (Var lazyFunName) args
>                                                     in (lazyFun : argFuns, ns''')
>                                      | otherwise  = let (core', ns') = removeTupleCore ns core
>                                                     in ([(name, args, core')], ns')


> removeTupleCore :: NameSupply -> Core -> (Core, NameSupply)
> removeTupleCore ns (Num n)              = (Num n, ns)
> removeTupleCore ns (Value v)            = (Value v, ns)
> removeTupleCore ns (Var v)              = (Var v, ns)
> removeTupleCore ns (Constr cn)          = (Constr cn, ns)
> removeTupleCore ns (Ap e1 e2)           = let (e1', ns') = removeTupleCore ns e1
>                                               (e2', ns'') = removeTupleCore ns' e2
>                                           in (Ap e1' e2', ns'')
> removeTupleCore ns (If e1 e2 e3)        = let (e1', ns') = removeTupleCore ns e1
>                                               (e2', ns'') = removeTupleCore ns' e2
>                                               (e3', ns''') = removeTupleCore ns'' e3
>                                           in (If e1' e2' e3', ns''')
> removeTupleCore ns lc@(Lc [])           = (lc, ns)
> removeTupleCore ns lc@(Lc (core:cores)) = (lc, ns)
> removeTupleCore ns (LcLet defs)         = (LcLet defs, ns)
> removeTupleCore ns (arg :<- core)       = (arg :<- core, ns)
> removeTupleCore ns (core1 :.. core2)    = (core1 :.. core2, ns)
> removeTupleCore ns (Let defs core)      = let (defs', ns') = removeTuple ns defs
>                                               (core', ns'') = removeTupleCore ns' core
>                                           in (Let defs' core', ns'')
> removeTupleCore ns (Lambda args core)   = let (core', ns') = removeTupleCore ns core
>                                           in (Lambda args core', ns')
> removeTupleCore ns x                    = pattErr "removeTupleCore" x


> removeTupleArgs :: NameSupply -> Core -> [Arg] -> ([Def], NameSupply)
> removeTupleArgs ns core [arg] = removeTupleArg ns core arg


> removeTupleArg :: NameSupply -> Core -> Arg -> ([Def], NameSupply)
> removeTupleArg ns core (ArgVar v)     = ([(v, [], core)], ns)
> removeTupleArg ns core (ArgWildCard)  = ([], ns)
> removeTupleArg ns core (ArgTuple tpl) = let len = length tpl
>                                             extractorName n = "nth_" ++ (show len) ++ "_" ++ (show n)
>                                             defineExtractor name n = (name, [ArgTuple $ map ( \ i -> ArgVar $ "x" ++ (show i)) [1..len]],
>                                                                             (Var $ "x" ++ (show n)))
>                                             genExtractor ns name n | inNameSupply name ns = ([], ns)
>                                                                    | otherwise            = ([defineExtractor name n], buildNameSupply ns name)
>                                             foldTupleArgs (defs, ns) (n, arg) = let nth_name = extractorName n
>                                                                                     extractCore = (Ap (Var $ nth_name) core)
>                                                                                     (nth_def, ns') = genExtractor ns nth_name n
>                                                                                     (defs', ns'') = removeTupleArg ns' extractCore arg
>                                                                                 in (defs' ++ nth_def ++ defs, ns'')
>                                         in foldl foldTupleArgs ([], ns) $ zip [1..] tpl


freeVars annotiert das Programm an jedem Knoten mit einer Menge von
Strings, die anzeigt, welche Variablen in dem jeweiligen Ausdruck
benutzt werden.

> freeVars :: Prog -> AProg ASet
> freeVars = map (freeVarsDef emptyASet)


> freeVarsDef :: ASet -> Def -> ADef ASet
> freeVarsDef fv (name, args, core) = (name, args, freeVarsCore (unionASets fv $ argsToASet args) core)


> freeVarsCore :: ASet -> Core -> ACore ASet
> freeVarsCore fv (Num n)            = (emptyASet, ANum n)
> freeVarsCore fv (Value n)          = (emptyASet, AValue n)
> freeVarsCore fv (Var vn)           | elemInASet vn fv = (listToASet [vn], AVar vn)
>                                    | otherwise        = (emptyASet, AVar vn)
> -- freeVarsCore fv (Constr cn)        | elemInASet cn fv = (listToASet [cn], AConstr cn)
> --                                    | otherwise        = (emptyASet, AConstr cn)
> freeVarsCore fv (Constr cn)        = (emptyASet, AConstr cn)
> freeVarsCore fv (Ap e1 e2)         = let ae1@(fv1, _) = freeVarsCore fv e1
>                                          ae2@(fv2, _) = freeVarsCore fv e2
>                                      in (unionASets fv1 fv2, AAp ae1 ae2)
> freeVarsCore fv (If e1 e2 e3)      = let ae1@(fv1, _) = freeVarsCore fv e1
>                                          ae2@(fv2, _) = freeVarsCore fv e2
>                                          ae3@(fv3, _) = freeVarsCore fv e3
>                                      in (foldr1 unionASets [fv1,fv2,fv3], AIf ae1 ae2 ae3)
> freeVarsCore fv (e1 :.. e2)        = let ae1@(fv1, _) = freeVarsCore fv e1
>                                          ae2@(fv2, _) = freeVarsCore fv e2
>                                      in (unionASets fv1 fv2, ae1 :::.. ae2)
> freeVarsCore fv (e1 :<- e2)        = (fv', e1 :::<- e2')
>     where e2'@(fv',_) = freeVarsCore fv e2
> --freeVarsCore fv (Lambda args core) = let fv' = unionASets fv $ argsToASet args
> --                                     in (fv', ALambda args $ freeVarsCore fv core)
> freeVarsCore fv (Lambda args core) = let arg_fv = argsToASet args
>                                          core'@(fv', _) = freeVarsCore (unionASets fv $ arg_fv) core
>                                      in (differenceASets arg_fv fv', ALambda args core')
> freeVarsCore fv (Lc [])            = (emptyASet, ALc [])
> freeVarsCore fv (Lc (exp:defs))      = (fv', ALc (exp':defs'))
>     where exp'@(fvExp, _) = freeVarsCore defs_fv exp
>           (defs', defs_fv) = doDefs defs fv
>           fv' = foldr unionASets emptyASet $ map fst defs'
>           doDefs :: [Core] -> ASet -> ([ACore ASet], ASet)
>           doDefs [] fv     = ([], fv)
>           doDefs (d:ds) fv = let exp' = freeVarsCore fv d
>                                  defVars = getDefinedVars d
>                                  (exps', fv') = doDefs ds (unionASets fv defVars)
>                              in (exp':exps', fv')
>           getDefinedVars :: Core -> ASet
>           getDefinedVars ((ArgVar v) :<- e)  = listToASet [v]
>           getDefinedVars (LcLet defs)        = listToASet $ map fst3 defs
>           getDefinedVars _                   = []  -- das sind Faelle, in denen Ap-Knoten einen boolschen Ausdruck konstruieren
> freeVarsCore fv (LcLet defs)       = let defs' = map (freeVarsDef fv) defs
>                                          fv' = foldr unionASets emptyASet $ map (fst . thd3) defs'
>                                      in (fv', ALcLet defs')
> freeVarsCore fv (Let defs exp)     = (fv', ALet defs''' exp')
>     where binders         = listToASet $ map fst3 defs
>           defs'           = map (freeVarsDef (unionASets fv binders)) defs
>           defs''          = map ( \ (name, args, (fv, core)) -> (name, args, (differenceASets (argsToASet args) fv, core))) defs'
>           dependList      = analyseDeps $ map ( \ (name, _, (fv, _)) -> (name, fv)) defs''
>           dependList'     = let filterName name = concatMap (fst . thd3) $ filter ( \ (n,_,_) -> n == name) defs''
>                             in map ( \ (name, fv) -> (name, concatMap filterName fv)) dependList
>           defs'''         = map ( \ ((_,fv'), (name, args, (fv, core))) -> (name, args, (differenceASets binders $ unionASets fv' fv, core))) $ zip dependList' defs''
>           expVars         = unionASets binders fv
>           exp'@(fvExp, _) = freeVarsCore expVars exp
>           fv'             = differenceASets binders $ foldr unionASets fvExp $ map (fst . thd3) defs'''
> freeVarsCore fv x                  = pattErr "freeVarsCore" x


> removeLambda :: AProg ASet -> Prog
> removeLambda = map (removeLambdaDef initialNameSupply emptyAssoc)


> removeLambdaDef :: NameSupply -> Assoc Core -> ADef ASet -> Def
> removeLambdaDef ns assc (name, args, core) = let (defs, ns', core') = removeLambdaCore ns assc core
>                                                  core'' | defs == [] = core'
>                                                         | otherwise  = Let defs core'
>                                              in (name, args, core'')


Entfernt Lambda-Ausdruecke, und liefert ein 3-Tupel an Ergebnissen.
Das eine Ergebnis sind alle neuen Supercombinatoren, die in neue
Let-Ausdruecke eingefuegt werden, das andere Ergebnis ist der
Term mit den ersetzten Lambda-Ausdruecken.
   NameSupply <= liefert neue Namen, die noch nciht benutz wurden
   Assoc Core <= enthaelt ein Mapping von Funktionsnamen, deren 
                 Aufrufe mit mehr Parametern versehen werden muessen
   ACore ASet <= annotierter Syntaxbaum, aus dessen Annotation ersichtlich
                 ist, welche Variablen in einem Unterausdruck gebraucht werden

> removeLambdaCore :: NameSupply -> Assoc Core -> ACore ASet -> ([Def], NameSupply, Core)
> removeLambdaCore ns assc (fv, ANum n)                   = ([], ns, Num n)
> removeLambdaCore ns assc (fv, AValue n)                 = ([], ns, Value n)
> removeLambdaCore ns assc (fv, AVar v)                   = ([], ns, getAssoc assc v (Var v))
> removeLambdaCore ns assc (fv, AConstr cn)               = ([], ns, getAssoc assc cn (Constr cn))
> removeLambdaCore ns assc (fv, AAp e1 e2)                = let (sc1, ns', e1') = removeLambdaCore ns assc e1
>                                                               (sc2, ns'', e2') = removeLambdaCore ns' assc e2
>                                                           in (sc1 ++ sc2, ns'', Ap e1' e2')
> removeLambdaCore ns assc (fv, AIf e1 e2 e3)             = let (defs1, ns1, e1') = removeLambdaCore ns assc e1
>                                                               (defs2, ns2, e2') = removeLambdaCore ns1 assc e2
>                                                               (defs3, ns3, e3') = removeLambdaCore ns2 assc e3
>                                                               defs = defs1 ++ defs2 ++ defs3
>							        core = If e1' e2' e3'
>                                                               core' | defs == [] = core
>                                                                     | otherwise  = Let defs core
>                                                           in ([], ns3, core')
> removeLambdaCore ns assc (fv, ALambda args core)        = let (newName, ns') = nameSupply ns "sc"
>                                                               (defs, ns'', core') = removeLambdaCore ns' assc core
>                                                           in ((newName, map (ArgVar) fv ++ args, core'):defs, ns'',
>                                                               foldl (Ap) (Var newName) $ map (Var) fv)
> removeLambdaCore ns assc (fv, ALet letDefs core)        = let assc' = mapLetDef letDefs ++ assc
>                                                               letDefs' = map (\ c@(_,_,(fv,_)) -> (fv, removeLambdaDef initialNameSupply assc' c)) letDefs
>                                                               letDefs'' = map fixArity letDefs'
>                                                               (defs, ns', core') = removeLambdaCore ns assc' core
>  --                                                             core'' | defs == [] = core'
>  --                                                                    | otherwise  = Let defs core'
>                                                           in ([], ns' , Let (defs ++ letDefs'') core')
>     where mapLetDef :: [ADef ASet] -> Assoc Core
>           mapLetDef []                            = []
>           mapLetDef ((name, args, (fv, core)):ds) | paramDiff == [] = rest
>                                                   | otherwise       = (name, foldl (Ap) (Var name) $ map (Var) paramDiff) : rest
>                                                     where paramDiff = differenceASets (argsToASet args) fv
>                                                           rest = mapLetDef ds
>           fixArity :: (ASet, Def) -> Def
>           fixArity (fv, (name, args, core)) = (name, map (ArgVar) (enumASet $ differenceASets (argsToASet args) fv) ++ args, core)
> removeLambdaCore ns assc (fv, ALc [])                   = ([], ns, Lc [])
> removeLambdaCore ns assc (fv, ALc (core:cores))         = let (letDefs1, ns', core') = removeLambdaCore ns assc core
>                                                               (letDefs2, ns'', cores') = mapLc ns' cores
>							        letDefs = letDefs1 ++ letDefs2
>                                                               result | letDefs == [] = Lc (core' : cores')
>							               | otherwise     = Let letDefs $ Lc (core' : cores')
>                                                           in ([], ns'', result)
>     where mapLc :: NameSupply -> [ACore ASet] -> ([Def], NameSupply, [Core])
>           mapLc ns []     = ([], ns, [])
>           mapLc ns (c:cs) = let (defs1, ns', c') = removeLambdaCore ns assc c
>                                 (defs2, ns'', cs') = mapLc ns' cs
>                             in (defs1 ++ defs2, ns'', c':cs')
> removeLambdaCore ns assc (fv, arg :::<- core)           = let (letDefs, ns', core') = removeLambdaCore ns assc core
>                                                           in (letDefs, ns', arg :<- core')
> removeLambdaCore ns assc (fv, core1 :::.. core2)        = let (letDefs1, ns', core1') = removeLambdaCore ns assc core1
>                                                               (letDefs2, ns'', core2') = removeLambdaCore ns' assc core2
>                                                           in (letDefs1 ++ letDefs2, ns'', core1' :.. core2')
> removeLambdaCore ns assc (fv, ALcLet defs)              = let defs' = map (removeLambdaDef initialNameSupply assc) defs
>                                                           in ([], ns, LcLet defs')
> removeLambdaCore ns assc x                              = pattErr "removeLambdaCore" x


TODO
ueber die Definition von LcLet noch einmal nachdenken!



> rename :: Prog -> Prog
> rename prog = map (renameDef emptyNameContext True emptyAssoc) prog

> renameDef :: NameContext -> Bool -> Assoc String -> Def -> Def
> renameDef nc rplc assc (name, args, core) = let newName | nc == emptyNameContext || not rplc = name
>                                                         | otherwise                          = makeName nc name
>                                             in (newName, args, renameCore newName rplc assc core)

> renameCore :: NameContext -> Bool -> Assoc String -> Core -> Core
> renameCore nc rplc assc (Var v)             = Var $ getAssoc assc v v
> renameCore nc rplc assc (Num n)             = Num n
> renameCore nc rplc assc (Value n)           = Value n
> renameCore nc rplc assc (Ap e1 e2)          = Ap (renameCore nc rplc assc e1) (renameCore nc rplc assc e2)
> renameCore nc rplc assc (Constr c)          = Constr $ getAssoc assc c c  -- ohne Fehlermeldung, darf das sein ?
> renameCore nc rplc assc (Let defs core)     = let assc' = (map ((\ x -> (x, makeName nc x)) . fst3) defs) ++ assc
>						    defs' = map (renameDef nc rplc assc') defs
>                                                   core' = renameCore nc rplc assc' core
>                                               in (Let defs' core')
> renameCore nc rplc assc (Lc [])             = Lc []
> renameCore nc rplc assc (Lc (core:defs))    = Lc ((renameCore nc rplc assc core) : (map (renameCore nc rplc assc) defs))
> renameCore nc rplc assc (LcLet defs)        = LcLet $ map (renameDef nc rplc assc) defs
> renameCore nc rplc assc (arg :<- core)      = arg :<- (renameCore nc rplc assc core)
> renameCore nc rplc assc (core1 :.. core2)   = (renameCore nc rplc assc core1) :.. (renameCore nc rplc assc core2)
> renameCore nc rplc assc (Lambda _ _)        = error "renameCore: Lamda-Ausdruecke sollten bereits entfernt worden sein!"
> renameCore nc rplc assc (If e1 e2 e3)       = If (renameCore nc rplc assc e1) (renameCore nc rplc assc e2) (renameCore nc rplc assc e3)
> renameCore nc rplc assc  x                  = pattErr "renameCore" x




> removeLet :: Prog -> Prog
> removeLet = concatMap removeLetDef

> removeLetDef :: Def -> [Def]
> removeLetDef (name, args, core) = let (defs, core') = removeLetCore core
>                                   in (name, args, core'):defs

entfernt alle Let-Definitionen.
  NameContesxt  <= Praefix der umgebenden Superkombinatornamen
  Assoc         <= Mapper von "alter Superkombinatorname" -> "neuer Superkombinatorname"
  Core          <= der zu bearbeitende Ausdruck
  ([Def], Core) <= eine Liste von eingesammelten Superkombinatordefinitionen und der neue
                   Ausdruck, befreit von Let-Definitionen

> removeLetCore :: Core -> ([Def], Core)
> removeLetCore n@(Num _)             = ([], n)
> removeLetCore v@(Value _)           = ([], v)
> removeLetCore v@(Var _)             = ([], v)
> removeLetCore c@(Constr _)          = ([], c)
> removeLetCore (Ap e1 e2)            = let (defs1, e1') = removeLetCore e1
>                                           (defs2, e2') = removeLetCore e2
>                                       in (defs1 ++ defs2, Ap e1' e2')
> removeLetCore (Let defs core)       = let defs' = concatMap removeLetDef defs
>                                           (defs'', core') = removeLetCore core
>                                       in (defs' ++ defs'', core')
> removeLetCore (Lambda args core)    = error "removeLetCore: unprovided core input! Entered a lambda expression"
> removeLetCore lc@(Lc [])            = ([], lc)
> removeLetCore (Lc (args:core))      = ([], Lc (args:core))
> removeLetCore (LcLet defs)          = ([], LcLet $ concatMap removeLetDef defs)
> removeLetCore (arg :<- core)        = let (defs, core') = removeLetCore core
>                                       in (defs, arg :<- core')
> removeLetCore (core1 :.. core2)     = let (defs1, core1') = removeLetCore core1
>                                           (defs2, core2') = removeLetCore core2
>                                       in (defs1 ++ defs2, core1' :.. core2')
> removeLetCore (If e1 e2 e3)         = let (defs1, e1') = removeLetCore e1
>                                           (defs2, e2') = removeLetCore e2
>                                           (defs3, e3') = removeLetCore e3
>                                       in (defs1 ++ defs2 ++ defs3, If e1' e2' e3')
> removeLetCore  x                    = pattErr "removeLetCore" x



=========================================================================


> testBindVariable = bindVariable . parse

> testBindVariableDef inp = let prog = parse inp
>                               def = head prog
>                               assc = [("f", [("x","glob"),("y","_"),("z","_")])]
>                               (prog', bf) = bindVariableDef assc prog def
>                           in bindSC bf prog'

sucht auf globale Superbombinatoren fixierte Variablen
und ersetzt diese durch den Superkombinator

> bindVariable :: Prog -> (Prog, Assoc (Assoc String))
> bindVariable prog = let main = case findSC "main" prog of
>                                     Nothing  -> error "No Supercombinator 'main' defined."
>                                     Just def -> def
>                         (prog', bf) = bindVariableDef emptyAssoc prog main
>                     in (bindSC bf prog', bf)


die Funktion bindet alle Ausdruecke mit Superkombinatoren
die gebunden werden koennen. Die Parameter sind
  Assoc (Assoc String) <= Tabelle mit fixierten Parametern einzelner Funktionen:
                          Parameter, die nicht gebunden werden koennen, werden
                          durch den String "_" dargestellt. Alle anderen Superkombinatoren
                          werden durch ihren Namen angegeben.
  Prog                 <= Liste mit den Superkombinatoren des Programms
  Def                  <= die Produktion von der ausgegangen wird
  Rueckgabewert : Das veraenderte Programm, und eine Tabelle mit saemtlichen
                  VariableBindings, die bis dahin aufgesammelt worden sind.

> bindVariableDef :: Assoc (Assoc String) -> Prog -> Def -> (Prog, Assoc (Assoc String))
> bindVariableDef bf prog (name, args, core) = let (core', prog', bf') = bindVariableCore bf name prog core
>                                                  -- core'' = bindSCCore (getAssoc bf' name emptyAssoc) core'
>                                              in (updateProg prog' (name, args, core'), bf')
>     where updateProg [] def@(name, _, _)             = [def]
>           updateProg (p@(n,_,_):ps) def@(name, _, _) | name == n = def : ps
>                                                      | otherwise = p : updateProg ps def


> bindVariableCore :: Assoc (Assoc String) -> String -> Prog -> Core -> (Core, Prog, Assoc (Assoc String))
> bindVariableCore vb scName prog (Num n)            = (Num n, prog, vb)
> bindVariableCore vb scName prog (Value v)          = (Value v, prog, vb)
> bindVariableCore vb scName prog (Var v)            = (Var v, prog, vb)
> bindVariableCore vb scName prog (Constr cn)        = (Constr cn, prog, vb)
> bindVariableCore vb scName prog ap@(Ap e1 e2)      = let assc = bindParameter ap prog
>                                                          assc' :: [(String, String)]
>                                                          assc' = map (\ (name, core) -> (name, mapParam core)) assc
>                                                          assc'' :: Assoc String
>                                                          assc'' = mapBindings varBind assc'
>                                                          funName = getPureName $ getFunApName ap
>                                                          varBind = getAssoc vb funName emptyAssoc  -- emptyAssoc symbolisiert "kein Ergebnis gefunden"
>                                                          isRec = case findSC funName prog of
>                                                                       Nothing   -> False
>                                                                       Just sc   -> True
>                                                          rec = case findSC funName prog of
>                                                                     Nothing   -> (prog, emptyAssoc)
>                                                                     Just sc   -> bindVariableDef (updateAssoc vb funName assc'') prog sc
>                                                          (prog', vb') | varBind == [] = rec  -- die Definition ist noch nicht behandelt worden => Rekursion
>                                                                       | otherwise     = (prog, updateAssoc vb funName $ mergeBindings assc'' varBind)  -- 
>                                                      in (Ap e1 e2, prog', vb')
>     where mapParam (Var name) = name
>           mapParam _           = "_"
> bindVariableCore vb scName prog (If e1 e2 e3)      = let (e1', prog', vb') = bindVariableCore vb scName prog e1
>                                                          (e2', prog'', vb'') = bindVariableCore vb' scName prog e2
>                                                          (e3', prog''', vb''') = bindVariableCore vb'' scName prog e3
>                                                      in (If e1' e2' e3', prog''', vb''')
> bindVariableCore vb scName prog (Lc (core:cores))  = (Lc (core:cores), prog, vb)
> bindVariableCore vb scName prog (LcLet defs)       = (LcLet defs, prog, vb)
> bindVariableCore vb scName prog (arg :<- core)     = (arg :<- core, prog, vb)
> bindVariableCore vb scName prog (core1 :.. core2)  = (core1 :.. core2, prog, vb)
> -- bindVariableCore vb scName prog(Let defs core)    = let vb' = removeTuple vb defs
> --                                                           vb'' = bindVariableCore vb' scName prog core
> --                                                       in (Let defs core, prog, vb'')
> bindVariableCore vb scName prog (Lambda arg core)  = let (_, prog', vb') = bindVariableCore vb scName prog core
>                                                      in (Lambda arg core, prog', vb')


Die Funktion mapBindings verbindet Variablen in einer Funktionsanwendung
mit einer Belegung durch einen Funktionsaufruf.
Parameter:
  Assoc String  <= eine Tabelle, die Parameternamen der Funktion mit gebundenen
                   Werten durch einen Aufruf verknuepft
  Assoc String  <= eine Tabelle, die Parameternamen der aufgerufenen Funtion
                   mit Aufrufparametern verbindet

> mapBindings :: Assoc String -> Assoc String -> Assoc String
> mapBindings varBind asscParam = map ( \ (name, p) -> (name, getAssoc varBind p p)) asscParam


Verbindet zwei Tabellen von Parameterbindungen miteinander. Jeder
parameter einer Funktion, der in einem Aufruf an den gleichen
Superkombinator gegbunden wird, bleibt erhalten. wenn an zwei verschiedene
Werte gebunden wird, dann wird der String "_" als Bindung eingesetzt,
als Zeichen der unbindbarkeit.

> mergeBindings :: Assoc String -> Assoc String -> Assoc String
> mergeBindings bind1 bind2 = map mrgBnd bind1
>     where mrgBnd (name, p1) | p1 == getAssoc bind2 name "_" = (name, p1)
>                             | otherwise                     = (name, "_")

Testfunktion fuer "bindParameter".
ACHTUNG: es wird mit dem Term des ersten Superkombinators gearbeitet

> testBindParam :: String -> Assoc Core
> testBindParam p = let (_,_,c)= head (parse p);
>                   in bindParameter (c) $ parse p


Diese Funktion bindet die Parameter im Application-Tree
an die Parameternamen der Funktion die aufgerufen wird.
Das hilft, um die Bindung von Variablen an Superkombinatoren
zu ermittlen.
ACHTUNG: Problem beim Binden an Tuple, weil da auch ein Baum stehen
         stehen kann, der einen Funktionsaufruf darstellt, und dann
         nicht direkt an die Parameter gebunden werden kann.
Parameter:
  Core     <= Der Term, in dem der 
  [Def]    <= Die Liste der definierten Superkombinatoren,

> bindParameter :: Core -> [Def] -> Assoc Core
> bindParameter apTree prog = let funName = getPureName $ getFunApName apTree
>                                 funArgs = getFunApArgs apTree
>                                 args = case findSC funName prog of
>                                             Nothing -> []
>                                             Just (_,args,_) -> args
>                                 assc = concatMap mapBind $ zip args funArgs
>                             in assc
>     where mapBind (ArgVar v, core) = [(v, core)]
>           mapBind (ArgTuple args, core) = []
>           mapBind (ArgWildCard, core) = []


Liefert den Namen der Funktion, der mit dem Application-Tree
die Argumente uebergeben werden

> getFunApName :: Core -> Core
> getFunApName (Constr c) = Constr c
> getFunApName (Var v)    = Var v
> getFunApName (Ap e1 e2) = getFunApName e1

> getPureName :: Core -> String
> getPureName (Constr c) = c
> getPureName (Var v)    = v


Liefert die Argumente, die der Funktion uebergeben werden

> getFunApArgs :: Core -> [Core]
> getFunApArgs = reverse . getFunApArgs'
>     where getFunApArgs' (Ap e1 e2) = e2 : getFunApArgs' e1
>           getFunApArgs' x = []


Sucht aus der Liste der Superkombinatoren den entsprechenden
mit gegebenen Naman heraus.

> findSC :: String -> Prog -> Maybe Def
> findSC name [] = Nothing
> findSC name (p@(n, args, def):ps) | name == n = Just p
>                                   | otherwise = findSC name ps


Baut eine Tabelle auf, die aus Variablennamen und "_" besteht, mit der
all die Parameter gekennzeichnet werden, die in Funktionsanwendungen
gestrichen werden sollen. Jedem Funktionsnamen wird eine Liste
von Aufrufparametern gegeben, wobei mit "_" gekennzeichnete Parameter
gestrichen werden koennen

> buildDelList :: Assoc (Assoc String) -> Prog -> Assoc [String]
> buildDelList assc prog = map (buildDelListDef assc) prog


> buildDelListDef :: Assoc (Assoc String) -> Def -> (String, [String])
> buildDelListDef assc (name, args, core) = let args' = mapArgs (getAssoc assc name emptyAssoc) args
>                                           in (name, args')
>     where mapArgs assc = concatMap (mapArg assc)
>           mapArg assc (ArgVar v)      | v' == "_" = ["_"]
>                                       | otherwise = ["*"]
>               where v' = getAssoc assc v "_"
>           mapArg assc (ArgTuple args) | args' == []        = ["_"]
>                                       | nub args' == ["_"] = ["_"]
>                                       | otherwise          = ["*"]
>               where args' = mapArgs assc args


Zum Testen von buildDelList

> testBuildDelList inp = let prog = parse inp
>                            assc = [("main",[]),("f",[("a","_")]),("g",[("a","_"),("b","glob")])]
>                            -- assc = snd $ bindVariable prog
>                        in buildDelList assc prog


Die Funktion "bindSC" bindet globale Superkombimatoren
an die Parameter im Aufruf einer Funktion

> bindSC :: Assoc (Assoc String) -> Prog -> Prog
> bindSC assc prog = let argList = buildDelList assc prog
>                    in map (bindSCDef assc argList) prog


> bindSCDef :: Assoc (Assoc String) -> Assoc [String] -> Def -> Def
> bindSCDef assc argList (name, args, core) = let args' = mapArgs (getAssoc assc name emptyAssoc) args
>                                                 core' = bindSCCore assc name argList core
>                                             in (name, args', core')
>     where mapArgs assc = concatMap (mapArg assc)
>           mapArg assc (ArgVar v)      | v' == "_" = [ArgVar v]
>                                       | otherwise = []
>               where v' = getAssoc assc v "_"
>           mapArg assc (ArgTuple args) | args' == [] = []
>                                       | otherwise   = [ArgTuple args']
>               where args' = mapArgs assc args


> bindSCCore :: Assoc (Assoc String) -> String -> Assoc [String] -> Core -> Core
> bindSCCore ns fn al (Num n)            = Num n
> bindSCCore ns fn al (Value v)          = Value v
> bindSCCore ns fn al (Var v)            | newVar == "_" = Var v
>                                        | otherwise     = Var newVar
>     where newVar = getAssoc (getAssoc ns fn emptyAssoc) v "_"
> bindSCCore ns fn al (Constr cn)        = Constr cn
> bindSCCore ns fn al ap@(Ap e1 e2)      = let e1' = bindSCCore ns fn al e1
>                                              e2' = bindSCCore ns fn al e2
>                                              funName = getPureName $ getFunApName ap
>                                              args = getFunApArgs ap
>                                              args' :: [Core]
>                                              args' = snd $ unzip $ filter ( \ (a, _) -> a /= "_" ) $ zip (getAssoc al funName emptyAssoc) args
>                                              -- ap' | args' == [] = Var funName
>                                              -- ap' | args' == [] = Var $ show (getAssoc al funName emptyAssoc)
>                                              -- ap' | args' == [] = Var $ show args
>                                              ap' | args' == [] = Var $ show ns
>                                                  | otherwise   =  foldl Ap (Var funName) args'
>                                          in ap'
> bindSCCore ns fn al (If e1 e2 e3)      = let e1' = bindSCCore ns fn al e1
>                                              e2' = bindSCCore ns fn al e2
>                                              e3' = bindSCCore ns fn al e3
>                                          in If e1' e2' e3'
> bindSCCore ns fn al (Lc (core:cores))  = Lc (core:cores)
> bindSCCore ns fn al (LcLet defs)       = LcLet defs
> bindSCCore ns fn al (arg :<- core)     = arg :<- core
> bindSCCore ns fn al (core1 :.. core2)  = core1 :.. core2
> bindSCCore ns fn al (Let defs core)    = let defs' = bindSC ns defs
>                                              core' = bindSCCore ns fn al core
>                                          in Let defs' core'
> bindSCCore ns fn al (Lambda args core) = let core'= bindSCCore ns fn al core
>                                          in Lambda args core'


Zum Testen:

> testGlobal inp = let prog = parse inp
>                      sig = calcSig prog
>                      rplcProg = replaceGlobs sig prog
>                  in putStr ("\nOldProgram:\n" ++ (ppProg prog) ++ "\nSignatures:\n" ++ (show sig) ++ "\n\ntransformed Program:\n" ++ (ppProg rplcProg))


Baut aus einem programm eine DependList um die Abhaengigkeiten
zu loesen. Das ist notwendig, um sich zyklisch aufrufende Funktionen
korrekt zu behandeln. Probleme treten auf, wenn eine Variable durch
einen Aufruf gebunden wird, und diese Bindung durchgereicht wird, bis an
einer anderen Stelle festgestellt wird, dass die gleiche Variable
auch noch an einen anderen Wert gebunden wird, wodurch die bisherigen
Berechnungen invalidiert wuerden.

> genDepList :: [(Name,[(Name,Core)])] -> Prog -> [(Name,[(Name,[(Name,Core)])])]
> genDepList funCalls prog = map (genDepListDef funCalls prog) prog

> genDepListDef :: [(Name,[(Name,Core)])] -> Prog -> Def -> (Name,[(Name,[(Name,Core)])])
> genDepListDef funCalls prog (name, args, core) = (name, genDepListCore (concatMap mapArgs args) funCalls name prog core)


bekommt eine Liste mit definierten ParameterNamen die
von aussen gegeben sind.
Liefert eine Liste von Tupeln pro angewandte Funktion, mit den
gebundenen Parametern. Die Parameter sind:
    [String]               <= Liste der als Parameter definierten Variablenamen
    [(Name,[(Name,Core)])] <= Tabelle, die jeder Funktion die Liste der Parameterbindungen zuordnet
    Name                   <= Name der Funktion
    Prog                   <= Alle definierten Superkombinatoren
    Core                   <= Der zu untersuchende Ausdruck

> genDepListCore :: [String] -> [(Name,[(Name,Core)])] -> Name -> Prog -> Core -> [(Name,[(Name,Core)])]
> genDepListCore vl fc name prog (Num n)              = []
> genDepListCore vl fc name prog (Value v)            = []
> genDepListCore vl fc name prog (Constr c)           = []
> genDepListCore vl fc name prog (Var v)              = [(v,[])]
> genDepListCore vl fc name prog ap@(Ap e1 e2)        = let funName = getPureName $ getFunApName ap
>                                                           funArgs = getFunApArgs ap
>                                                           funArgs' = map (replaceParam bindings) funArgs
>                                                           bindings = getAssoc fc name emptyAssoc ++ map ( \ n -> (n,Var "_") ) vl
>                                                           res = case findSC funName prog of
>                                                                  Just (_,args,_) -> zip (concatMap mapArgs args) funArgs'
>                                                                  Nothing         -> []
>                                                       in [(funName, res)]
>     where mapArgs (ArgVar v)       = [v]
>           mapArgs (ArgTuple args)  = concatMap mapArgs args
>           mapArgs ArgWildCard      = ["_"]
>           replaceParam fc (Var v)  = getAssoc fc v (Var v)
>           replaceParam fc x        = x
> genDepListCore vl fc name prog (Lc [])              = []  -- TODO
> genDepListCore vl fc name prog (Lc (core:defs))     = []  -- TODO
> genDepListCore vl fc name prog (LcLet defs)         = []  -- TODO
> genDepListCore vl fc name prog (arg :<- core)       = []  -- TODO
> genDepListCore vl fc name prog (core1 :.. core2)    = genDepListCore vl fc name prog core1 ++ genDepListCore vl fc name prog core2
> -- genDepListCore vl fc name prog (Let defs core)      = []  -- gibts nicht, schon beseitigt
> -- genDepListCore vl fc name prog (Lambda _ _)         = []  -- gibts nicht, ...
> genDepListCore vl fc name prog (If e1 e2 e3)        = genDepListCore vl fc name prog e1
>                                                       ++ genDepListCore vl fc name prog e2
>                                                       ++ genDepListCore vl fc name prog e3
> genDepListCore vl fc name prog x                    = pattErr "genDepListCore" x


Liefert eine DependList mit allen Funktionen, die von den Funktionen des
Programms aufgerufen werden.

> getFunDeps = map ( \ (n,fn) -> (n, map fst fn) )


revertDeps kehrt die Abhaengigkeiten um: es wird ermittelt, welche
Funktion durch welche Funktionen aufgerufen wird (mittelbar oder
unmittelbar)

> revertDeps :: DependList -> DependList
> revertDeps deps = map ( \ (name,_) -> (name, concatMap ( \ (n, ds) -> if elem name ds then [n] else []) deps)) deps


Die mitgegebene DependList zeigt in diesem Fall alle Funktionen an, die
eine bestimmte Funktion aufrufen.

> calcSig :: Prog -> Assoc (Assoc Core)
> calcSig prog = let sigList :: [(Name,[(Name,[(Name,Core)])])]
>                    sigList = genDepList funCalls prog  -- liefert eine Liste mit allen Funktionsaufrufen und deren Parameter die wirklich uebergeben werden.
>                    depList :: [(Name,[Name])]
>                    depList = revertDeps $ getFunDeps sigList
>                    funCalls :: [(Name,[(Name,Core)])]
>                    funCalls = map ( \ (name, deps) -> (name, mergeSigs name deps) ) depList
>                    mergeSigs :: Name -> [Name] -> [(Name,Core)]
>                    mergeSigs name deps = foldl1' mergeBindings' $ map snd $ mergeSigs' name deps
>                    mergeSigs' :: Name -> [Name] -> [(Name,[(Name,Core)])]
>                    mergeSigs' name deps = filter ( \ (n,args) -> name == n ) $ concatMap ( \ n -> getAssoc sigList n emptyAssoc ) deps
>                in funCalls


Hilfsfunktion fuer calcSig: loest das Problem von "foldl1" falls kein Element
in der Liste existiert.

> foldl1' :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
> foldl1' f [] = []
> foldl1' f [a] = a
> foldl1' f (a:as) = f a $ foldl1' f as


=========================================================================
=========================================================================


Ersetzen von globalen Superkombinatoren:
========================================

entfernt parameter aus den Funktionsaufrufen, wenn diese
als konstante Superkombinatoren global bekannt sind.

> fixGlobs :: Prog -> Prog
> fixGlobs prog = replaceGlobs (calcSignatures prog) prog



Zum Testen:
===========

> testProg1 = parse testInp1
> testProg2 = parse testInp2
> testInp1 = "main inp = f inp glob;" ++
>            "f x y = g y x;" ++
>            "g alg i = h glob2 alg i;" ++
>            "h x y j = j"
> testInp2 = "main a b = f (g a glob) (h a b);" ++ 
>            "f x y = x + y;" ++ 
>            "g x y = i x;" ++
>            "h u v = i v;" ++
>            "i a = a"


> testCalcSignatures1 inp = let prog = parse inp
>                               res = (ppProg prog) ++
>                                     "\ncalculated Signatures\n" ++
>                                     (show $ calcSignatures prog) ++
>                                     "\n\ntransformed program:\n" ++
>                                     (ppProg $ fixGlobs prog)
>                           in putStr res

> testCalcSignatures2 fileName = do
>                                file <- readFile fileName
>                                let file' = unlit fileName file
>                                let prog = (fixGlobs . lambdaLift . parse) file'
> ---                               let prog = (lambdaLift . parse) file'
>                                putStr $ ppProg prog


> testCalcSignatures3 fileName = do
>                                file <- readFile fileName
>                                let file' = unlit fileName file
>                                let prog = (fixGlobs . lambdaLift . parse) file'
> ---                               let prog = (lambdaLift . parse) file'
>                                let newFileName = "Transformed_" ++ fileName
>                                writeFile newFileName $ ppProg prog
>                                putStr $  "'" ++ newFileName ++ "'" ++ " written to disk."


> testCalcSignatures4 fileName = do
>                                file <- readFile fileName
>                                prog <- parseWithImport fileName
>                                let newFileName = "Transformed_" ++ fileName
>                                let prog' = (fixGlobs . lambdaLift) prog
>                                writeFile newFileName $ ppProg prog'
>                                putStr $  "wrote '" ++ newFileName ++ "' to disk."


Berechnet die Liste der fixierten Aufrufparameter:
==================================================

Startfunktion fuer die Errechnung der Tabelle der Uebergabeparameter
aller Funktionen.

> calcSignatures :: Prog -> Assoc (Assoc Core)
> calcSignatures prog = let res = case findSC "main" prog of
>                                      Just (_,args,_) -> sig [] [("main", map ( \ n -> (n,Var "_")) $ concatMap mapArgs args)] prog
>                                      Nothing         -> error "Can't find supercombinator 'main'"
>                       in res


Die Funktion bekommt eine art Liste mit bereits ermittelten
Bindungen (initial leer), und einer Liste mit Bindungen, denen
noch nechgegangen werden muss. Es wird solange ein Element
aus der ToDo Liste genommen, und mit deren Hilfe neue Bindungen
erzeugt, bis sich nichts mehr geaendert hat, und somit keine
neuen Bindungen entstanden sind.
Die Funktion "sig" bekommt drei Parameter
    bs    <= Tabelle, die jeder bereits aufgerufenen Funktion
             die gebundenen oder ungebundenen Parameter zuordnet
    todo  <= Liste, die eine Reihe von Funktionen speichert, die
             aus bereits aufgerufenen Funktionen aufgerufen worden
             sind. Es muss noch errechnet werden, welchen Einfluss
             diese Werte auf die Fixierung der Parameter der
             aufgerufenen Funktionen hat
    prog  <= Das Programm selbst, um es an verwendete Unterfunktionen
             zur wieteren Berechnung zu uebergeben.
Aufgerufen wird die Funktion mit
    sig [] [("main",[("inp", Var "_")])] $ testProg1

> sig :: [(Name, [(Name,Core)])] -> [(Name, [(Name,Core)])] -> Prog -> [(Name, [(Name,Core)])]
> sig bs [] prog = bs
> sig bs todo prog = let done@(funName,assc) = head todo
>                        newBinds = case findSC funName prog of
>                                        Just (name, args, core) -> bindParams (assc ++ (map ( \ n -> (n,Var "_") ) $ concatMap mapArgs args)) prog core
>                                        Nothing                 -> []
>                        bs' = renewBindings bs newBinds
>                        changedBinds = findChangedBindings bs' newBinds
>                        bs'' = insertNewBindings bs' newBinds
>                        res = sig bs'' ((tail todo) ++ changedBinds) prog
>                    in res


Ersetzt bestehende Bindungsinformationen aus der ersten Liste
durch neu ermittelte Bindungen, die in der zweiten Liste gespeichert
sind.

> renewBindings :: Assoc (Assoc Core) -> Assoc (Assoc Core) -> Assoc (Assoc Core)
> renewBindings [] newBinds     = []
> renewBindings (b@(name, binds):bs) newBinds | elemAssoc newBinds name = (name, (mergeBindings' binds $ getAssoc newBinds name [])) : rest
>                                             | otherwise               = b : rest
>     where rest = renewBindings bs newBinds


geht die Liste der neuen Bindungsinformationen durch und ermittelt
all diejenigen Bindungen, die sich im Vergleich zu den alten
Bindungsinformationen geaendert haben.

> findChangedBindings :: Assoc (Assoc Core) -> Assoc (Assoc Core) -> Assoc (Assoc Core)
> findChangedBindings bs [] = []
> findChangedBindings bs (n@(name,bns):ns) | elemAssoc bs name = if equalBinds bns newBind then rest
>                                                                                          else (name,newBind) : rest
>                                          | otherwise         = n : rest
>     where newBind = getAssoc bs name []
>           rest = findChangedBindings bs ns
>           equalBinds sig1 sig2 = all (eqBinds sig2) sig1
>           eqBinds sig (name, core) | elemAssoc sig name = (getAssoc sig name (Var "dummyCore")) == core
>                                    | otherwise          = False


Fuegt neue Variablenbindungen in die Liste der Bindungen ein.
Dabei werden alle Bindungen aus dem zweiten Parameter in die
Liste, die durch den ersten Parameter uebergeben werden, eingefuegt,
wenn dort nicht bereits eine Bindungsinformation zum jeweiligen
Superkombinator enthalten ist.

> insertNewBindings :: Assoc (Assoc Core) -> Assoc (Assoc Core) -> Assoc (Assoc Core)
> insertNewBindings bs [] = bs
> insertNewBindings bs (nb@(n,_):ns) | elemAssoc bs n = insertNewBindings bs ns
>                                    | otherwise      = nb : insertNewBindings bs ns


Hilfsfunktion fuer calcSig: fuegt zwei Parametersets zusammen.
Gemeinsamkeiten bleiben erhalten.

> mergeBindings' :: Assoc Core -> Assoc Core -> Assoc Core
> mergeBindings' sig1 sig2 = map mrgBnd sig1
>     where mrgBnd (name, core1) | core1 == getAssoc sig2 name (Var "_") = (name, core1)
>                                | otherwise                             = (name, Var "_")


"bindParams" bindet die einer Funktion uebergebenen Parameter an die
entsprechenden Variablen im Funktionsbaum. Dabei entstehen neue
"Bindungen" von Parameter an Funktionen, die durch diese Funktion
selbst aufgerufen werden.

> bindParams :: Assoc Core -> Prog -> Core -> [(Name,[(Name,Core)])]
> bindParams bind prog (Num n)              = []
> bindParams bind prog (Value v)            = []
> bindParams bind prog (Constr c)           = []
> bindParams bind prog (Var v)              = [] -- [(v,[])]
> bindParams bind prog ap@(Ap e1 e2)        = let funName = getPureName $ getFunApName ap
>                                                 funArgs = getFunApArgs ap
>                                                 funArgs' = map (replaceParam bind) funArgs
>                                                 res = case findSC funName prog of
>                                                        Just (_,args,_) -> mapArgs args funArgs -- zip (concatMap mapArgs args) funArgs'
>                                                        Nothing         -> []
>                                                 res' = map ( \ (name,core) -> (name,replaceParam bind core)) res
>                                             in (funName, res') : concatMap (bindParams bind prog) funArgs
> --                                            in (show funArgs, res) : concatMap (bindParams bind prog) funArgs
>     where mapArgs :: [Arg] -> [Core] -> [(Name,Core)]
>           mapArgs [] _ = []
>           mapArgs _ [] = []
>           mapArgs (a:as) (c:cs) = mapArgs' a c ++ mapArgs as cs
>               where mapArgs' :: Arg -> Core -> [(Name,Core)]
>                     mapArgs' (ArgVar v) core      = [(v,core)]
>                     mapArgs' (ArgTuple tpl) core  = let funName = getFunApName core
>                                                         funArgs = getFunApArgs core
>                                                     in if funName == Constr "(,)" then mapArgs tpl funArgs
>                                                                                   else []
>                     mapArgs' ArgWildCard core     = []
>           replaceParam :: Assoc Core -> Core -> Core
>           replaceParam fc (Var v)  = getAssoc fc v (Var v)
>           replaceParam fc x        = Var "_" -- x
> bindParams bind prog (Lc [])              = []
> bindParams bind prog (Lc (core:defs))     = bindParams bind prog core ++ concatMap (bindParams bind prog) defs
> bindParams bind prog (LcLet defs)         = []  -- TODO
> bindParams bind prog (arg :<- core)       = bindParams bind prog core
> bindParams bind prog (core1 :.. core2)    = bindParams bind prog core1 ++ bindParams bind prog core2
> -- bindParams bind prog (Let defs core)      = []  -- gibts nicht, schon beseitigt
> -- bindParams bind prog (Lambda _ _)         = []  -- gibts nicht, ...
> bindParams bind prog (If e1 e2 e3)        = bindParams bind prog e1 ++ bindParams bind prog e2 ++ bindParams bind prog e3
> bindParams bind prog x                    = pattErr "bindParams" x



"replaceGlobs" ersetzt alle gebundenen Parameter durch
die entsprechenden Superkombinatoren. Die Anzahl der
Paramter wird verringert, und jeder Aufruf einer solchen
Funktion wird auch entsprechend reduziert.

> replaceGlobs :: [(Name,[(Name,Core)])] -> Prog -> Prog
> replaceGlobs fn prog = map (replaceGlobsDef fn prog) prog

> replaceGlobsDef :: [(Name,[(Name,Core)])] -> Prog -> Def -> Def
> replaceGlobsDef fn prog (name,args,core) = let bindings = getAssoc fn name emptyAssoc
>                                                args' = concatMap mapArgs args
>                                                mapArgs :: Arg -> [Arg]
>                                                mapArgs (ArgVar v) = if (getAssoc bindings v (Var "_")) == Var "_" then [ArgVar v] else []
>                                                mapArgs (ArgTuple tpl) = let tpl' = concatMap mapArgs tpl
>                                                                         in case length tpl' of
>                                                                                 0         -> []
>                                                                                 1         -> tpl'
>                                                                                 otherwise -> [ArgTuple tpl']
>                                                mapArgs ArgWildCard    = [ArgWildCard]
>                                            in (name, args', replaceGlobsCore fn name prog core)

> replaceGlobsCore :: Assoc (Assoc Core) -> Name -> Prog -> Core -> Core
> replaceGlobsCore fc name prog n@(Num _)         = n
> replaceGlobsCore fc name prog v@(Value _)       = v
> replaceGlobsCore fc name prog c@(Constr _)      = c
> replaceGlobsCore fc name prog vr@(Var v)        = let bnd = getAssoc assc v vr
>                                                   in if bnd == Var "_" then vr else bnd
>     where assc = getAssoc fc name emptyAssoc
> replaceGlobsCore fc name prog ap@(Ap e1 e2)     = let funName = getFunApName ap
>                                                       pureFunName = getPureName funName
>                                                       funArgs = getFunApArgs ap
>                                                       funArgs' = map (replaceGlobsCore fc name prog) funArgs
>                                                       bindings = getAssoc fc name emptyAssoc
>                                                       bnds2 = getAssoc fc pureFunName emptyAssoc -- ist ein Binding der aufgerufenen Funktion
>                                                       res = case findSC pureFunName prog of
>                                                               Just (_,args,_) -> mapArgs bindings bnds2 args funArgs'
>                                                               Nothing         -> map (replaceGlobsCore fc name prog) funArgs
>                                                    in foldl Ap funName res
>     where mapArgs :: Assoc Core -> Assoc Core -> [Arg] -> [Core] -> [Core]
>           mapArgs bnds1 bnds2 [] []          = []
>           mapArgs bnds1 bnds2 _ []           = []
>           mapArgs bnds1 bnds2 [] core        = core
>           mapArgs bnds1 bnds2 (a:as) (c:cs)  = mapArgs' a c ++ mapArgs bnds1 bnds2 as cs
>               where mapArgs' :: Arg -> Core -> [Core]
>                     mapArgs' (ArgVar v) (Var vr)   = let paramBind = getAssoc bnds1 vr (Var vr)
>                                                          callBind = getAssoc bnds2 v (Var "_")
>                                                      in if callBind == Var "_" then if paramBind == Var "_" then [Var vr] else [paramBind]
>                                                                                else []
>                     mapArgs' (ArgVar v) core       = [core]
>                     mapArgs' (ArgTuple args) core  = let funName = getFunApName core
>                                                          funArgs = getFunApArgs core
>                                                          funArgs' = mapArgs bnds1 bnds2 args funArgs
>                                                          -- Wichtig: falls ein globaler Superkombinator ersetzt
>                                                          -- werden soll, der an eine Tupelvariable gebunden wird,
>                                                          -- besteht die Chance, dass der Superkombinator selber
>                                                          -- ein Tupel zusammen baut mit Hilfe eines Konstruktors.
>                                                          followSC (Var v) = let core = case findSC v prog of
>                                                                                             Just (_,_,core) -> followSC core
>                                                                                             Nothing         -> Var v
>                                                                             in core
>                                                          followSC x       = x
>                                                      in if funName == Constr "(,)" then [foldl Ap funName $ mapArgs bnds1 bnds2 args funArgs']
> --                                                                                    else [followSC core]
>                                                                                    else [core]
> --                                                      in case length funArgs' of
> --                                                              0         -> ???
> --                                                              1         -> funArgs' -- [Ap funName $ head funArgs']
> --                                                              otherwise -> [foldl Ap funName funArgs']
>                     mapArgs' (ArgWildCard) core    = [core]
> replaceGlobsCore fc name prog lc@(Lc [])        = lc
> replaceGlobsCore fc name prog (Lc (core:defs))  = let defs' = map (replaceGlobsCore fc name prog) defs
>                                                   in Lc ((replaceGlobsCore fc name prog core) : defs')
>                                                   -- in Lc ((Var "X") : defs)
> replaceGlobsCore fc name prog (LcLet defs)      = LcLet defs  -- TODO?!?
> replaceGlobsCore fc name prog (arg :<- core)    = arg :<- replaceGlobsCore fc name prog core
> replaceGlobsCore fc name prog (core1 :.. core2) = replaceGlobsCore fc name prog core1 :.. replaceGlobsCore fc name prog core2
> replaceGlobsCore fc name prog (If e1 e2 e3)     = If (replaceGlobsCore fc name prog e1) (replaceGlobsCore fc name prog e2) (replaceGlobsCore fc name prog e3)
> -- replceGlobsCore fc name prog (Let defs core)      = []  -- gibts nicht, schon beseitigt
> -- replceGlobsCore fc name prog (Lambda _ _)         = []  -- gibts nicht, ...
> replaceGlobsCore fc name prog x                 = pattErr "replaceGlobsCore" x



=========================================================================
=========================================================================


Mengenoperationen:
==================

Die Mengen ASet stellen eine Liste von Elementen dar,
auf der Mengenoperationen ausgefuehrt werden koennen.

> type ASet = [String]


helper functions for ASets

> emptyASet :: ASet
> emptyASet = []

> unionASets :: ASet -> ASet -> ASet
> unionASets s1 s2 = nub $ s1 ++ s2

> intersectionASets :: ASet -> ASet -> ASet
> intersectionASets s1 s2 = filter (flip elemInASet s1) s2

> differenceASets :: ASet -> ASet -> ASet
> differenceASets s1 s2 = filter (not . (flip elemInASet s1)) s2

> listToASet :: [String] -> ASet
> listToASet lst = lst

> elemInASet :: String -> ASet -> Bool
> elemInASet e aSet = elem e aSet

> enumASet :: ASet -> [String]
> enumASet = id

> argsToASet :: [Arg] -> ASet
> argsToASet args = listToASet $ concatMap mapArgs args



> {-

Alte Version:
Mit NameSupply wird eine Liste an verfuegbaren Namen
verwaltet, mit deren Hilfe schnell ein neuer eindeutiger Name
generiert werden kann.

> type NameSupply = [Int]

> initialNameSupply :: NameSupply
> initialNameSupply = [1..]

> nameSupply :: NameSupply -> String -> (String, NameSupply)
> nameSupply ns baseName = (baseName ++ (show $ head ns), tail ns)

> -}


Hier eine Alternative zum NameSupply, bei der eine
Liste bereits definierter Namen gefuehrt wird, ein Flag,
ob der Name ohne Index benutzt wurde, und einer Liste,
welche Endziffern davon schon vergeben sind.

> type NameSupply = Assoc (Bool, [Int])

Initial ist der NameSpace leer.

> initialNameSupply :: NameSupply
> initialNameSupply = []

Diese Funktion beschafft einen neuen Bezeichner. Es
wird geprueft, ob der gewuenschte Bezeichner bereits im
NameSpace eingetragen ist, in dem Fall wird der
naechtverfuegbare genommen.

> nameSupply :: NameSupply -> String -> (String, NameSupply)
> nameSupply ns name = (newName, updateAssoc ns strippedName (pureName', tail freeNumbers))
>     where stripName name = reverse $ dropWhile (isDigit) $ reverse name
>           getPostfixNumber name = reverse $ takeWhile (isDigit) $ reverse name
>           strippedName = stripName name
>           numberAsString = getPostfixNumber name
>           pureName = numberAsString == ""
>           postfixNumber = read $ numberAsString :: Int
>           (pureName', freeNumbers) = getAssoc ns strippedName (pureName, [1..])
>           newName = strippedName ++ (show $ head freeNumbers)

> buildNameSupply :: NameSupply -> String -> NameSupply
> buildNameSupply ns name = updateAssoc ns strippedName assocEntry
>     where stripName name = reverse $ dropWhile (isDigit) $ reverse name
>           getPostfixNumber name = reverse $ takeWhile (isDigit) $ reverse name
>           strippedName = stripName name
>           numAsString = getPostfixNumber name
>           pureName = numAsString == ""
>           postfixNumber = if pureName then 0 else read numAsString :: Int
>           (pureName', freeNumbers) = getAssoc ns strippedName (pureName, [])
>           nameIsNew = freeNumbers == []
>           assocEntry = if nameIsNew then if pureName then (pureName, [0..])
>                                                      else (pureName, removeNum [0..] postfixNumber)
>                                     else if pureName then (pureName, freeNumbers)
>                                                      else (pureName', removeNum freeNumbers postfixNumber)

Prueft, ob ein Bezeichner im NameSupply gespeichert ist.

> inNameSupply :: String -> NameSupply -> Bool
> inNameSupply name ns = isInSupply
>     where strippedName = reverse $ dropWhile (isDigit) $ reverse name
>           numAsString = reverse $ takeWhile (isDigit) $ reverse name
>           (pureName, freeNumbers) = getAssoc ns strippedName (False, [])
>           isInSupply = if numAsString == "" then pureName
>                                             else not $ elemNum freeNumbers $ (read numAsString :: Int)

Der erste Parameter ist eine Liste von freien Indizes,
der zweite Parameter ist ein Index, der vergeben wurde.
Das Ergebnis ist die Liste von freien Indizes, aus der
der vergebene Index geloescht wurde.

> removeNum :: [Int] -> Int -> [Int]
> removeNum [] num     = []
> removeNum (n:ns) num | num > n   = n : removeNum ns num
>                      | num == n  = ns
>                      | otherwise = (n:ns)

Der erste Parameter ist eine Liste von noch freien
Parametern. Ist der zweite Parameter darin enthalten
wird True geliefert. Ist die Liste leer, so ist der
zu pruefende zweite Index ebenfalls noch frei, und es
wird True geliedert. Falls der Wert 'num' nicht
gefunden werden konnte, wird False geliefert.

> elemNum :: [Int] -> Int -> Bool
> elemNum [] num = True
> elemNum (n:ns) num | num > n   = elemNum ns num
>                    | num == n  = True
>                    | otherwise = False


Ein NameContext gibt an, innerhalb welcher Funktion
sich eine andere Superkombinator-Definition befindet.
Dafuer wird einfach der Name der umgebenden Superkombinator-
Definitionen aneinander gehaengt.
Der NameContext wird als Praefix benutzt, um Superkombinatoren
die innerhalb eines Let-Ausdrucks definiert sind umzubenennen.

> type NameContext = String

> emptyNameContext = ""

> makeName :: NameContext -> String -> String
> makeName nc name = nc ++ "_" ++ name


getAssoc ist eine Funktion die Schluessel auf Werte abbildet

> type Assoc a = [(String, a)]

> emptyAssoc = []

> getAssoc :: Assoc a -> String -> a -> a
> getAssoc [] key alt         = alt
> getAssoc ((k,v):as) key alt | key == k  = v
>                             | otherwise = getAssoc as key alt

> elemAssoc :: Assoc a -> String -> Bool
> elemAssoc [] key          = False
> elemAssoc ((k,v):as) key  | key == k  = True
>                           | otherwise = elemAssoc as key

> updateAssoc :: Assoc a -> String -> a -> Assoc a
> updateAssoc [] key value         = [(key, value)]
> updateAssoc ((k,v):as) key value | key == k  = (k, value):as
>                                  | otherwise = (k,v) : updateAssoc as key value


defined in CoreSyntax.lhs:

< data Arg = ArgVar String
<          | ArgTuple [Arg]

> mapArgs :: Arg -> [String]
> mapArgs args = mapArg [] args
> mapArg as (ArgVar name)    = name : as
> mapArg as (ArgTuple args)  = (concatMap mapArgs args) ++ as

ein annotierter Ausdruck

> type ACore a = (a, A_Core a)

ein erweiterter Ausdruck, der als Teilterme
nur annotierte Ausdruecke enthaelt.

> data A_Core a
>      = AVar String
>      | ANum Int
>      | AValue Value
>      | AAp (ACore a) (ACore a)
>      | ALc [ACore a]
>      | ALcLet [ADef a]
>      | AConstr String
>      | Arg :::<- (ACore a)
>      | (ACore a) :::.. (ACore a)
>      | ALet [ADef a] (ACore a)
>      | ALambda [Arg] (ACore a)
>      | AIf (ACore a) (ACore a) (ACore a)
>                            deriving (Show, Eq)

< data ALetStmt = Arg :::<- ACore
<               | ALcLet ACore
<               | AQual ACore
<                            deriving (Show, Eq)

> type ADef a = (String, [Arg], ACore a)

> type AProg a = [ADef a]


==========================================================================


Abhaengigkeitsanalyse:
======================

Abhaengigkeitsanalyse fuer lokal definierte Funktionen

> analyseDeps :: DependList -> DependList
> analyseDeps = extractDeps . mkTree


Datentypen:
===========

> type Name = String


> type DependList = [(Name, [Name])]


> data DependGraph = Node Name [DependGraph]
>                  | SelfRef                deriving (Show, Eq)


Hilfsfunktionen:
================

> mkTree :: DependList -> [DependGraph]
> mkTree xs = tree
>     where tree = map mapDep xs
>           mapDep (name, args) = Node name (link args)
>           link []     = []
>           link (a:as) = (filter ( \ (Node name deps) -> a == name) tree) ++ link as


> extractDeps :: [DependGraph] -> DependList
> extractDeps dTree = map extractNode dTree
>     where extractNode (Node name deps) = (name, nub $ concatMap (extractNode' [name]) deps)
>           extractNode' :: [Name] -> DependGraph -> [Name]
>           extractNode' nodeNames (Node name deps) | elem name nodeNames = []
>                                                   | otherwise           = name : concatMap (extractNode' (name:nodeNames)) deps
>           extractNode' nodeNames SelfRef        = [head $ nodeNames]
