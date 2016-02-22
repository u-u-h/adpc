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



> module CoreTI where


========================
Das Modul muss mit dem ghci geladen werden, da es
Erweiterungen benutzt. Der Aufruf sieht so aus:

ghci -cpp -fglasgow-exts -package lang

========================


> import Monad
>     (when)

> import Typecheck.MultiModuleBasics        -- (ModuleInfo(..), joinModuleInfo)

> import Typecheck.HsParser
>     (parse)

> import Typecheck.HsSyn
>     (HsModule, 
>     SrcLoc (..))

> import Typecheck.AnnotatedHsSyn
>     (ASrcLoc (..),
>      bogusASrcLoc,
>      AModule(..),
>      AHsModule,

>      AHsDecl,
>      AHsName (..),
>      AModule (..))

> import Typecheck.HsParseMonad
>     (ParseResult (..))

> import Maybe
>     (mapMaybe)

> import Typecheck.Env
>     (listToEnv,
>      getNamesFromEnv,
>      Env,
>      envToList,
>      pprintEnv,
>      joinEnv,
>      showEnv)

> import Typecheck.HaskellPrelude
>     (tyconsMembersHaskellPrelude, 
>      preludeDefs, 
>      preludeSynonyms,
>      preludeTyconAndClassKinds,
>      preludeClasses,
>      preludeInfixDecls,
>      preludeDataCons)


> -- import Typecheck.Opts                     (processCmdLine,
> --                                  makeUsageInfo,
> --                                  usageHeader)

> -- import Typecheck.CPUTime                  (getCPUTime,
> --                                  cpuTimePrecision)


> -- import Typecheck.Utils                    (rJustify,
> --                                  lJustify,
> --                                  getAModuleName,
> --                                  doDump)
> import Typecheck.Utils
>     (maybeGetDeclName,
>      rJustify,
>      lJustify,
>      Binding (..),
>      getAModuleName,
>      getDeclName,
>      fromAHsName,
>      doDump)

> import qualified Typecheck.PPrint
>     (render)

> import Typecheck.Class
>     (emptyClassHierarchy,
>      --addInstancesToHierarchy,
>      printClassHierarchy,
>      -- instanceToTopDecls,
>      addClassToHierarchy,
>      ClassHierarchy,
>      classMethodAssumps)

> import Typecheck.FiniteMaps
>     (listToFM,
>      filterFM)

> import Typecheck.Type
>     (assumpToPair,
>      assumpId)

> import Typecheck.Representation
>     (Kind,
>      Scheme,
>      Assump (..))

> import Typecheck.SynConvert
>     (toAHsModule,
>      fromAHsModule,
>      fromAHsDecl)

> import Typecheck.HsParsePostProcess
>     (fixFunBindsInModule)

> import Typecheck.Infix
>     (infixer)

> import Typecheck.KindInference
>     (KindEnv,
>      kiModule)

> import Typecheck.Rename
>     (renameTidyModule,
> --     printIdentTable,
>      IdentTable)

> import Typecheck.TidyModule
>     (tidyModule, 
>      TidyModule (..),
>      tidyModuleToAHsModule)

> import Typecheck.Desugar
>     (desugarTidyModule)

> import Typecheck.TypeSigs
>     (collectSigs,
>      listSigsToSigEnv)

> import Typecheck.DataConsAssump
>     (dataConsEnv)

> import Typecheck.FiniteMaps
>     (addToFM)

> import Typecheck.DependAnalysis
>     (getBindGroups)

> import Typecheck.DeclsDepends
>     (getDeclDeps)

> import Typecheck.TIMain
>     (makeProgram,
>      tiProgram)



> -- typeCheck :: HsModule -> [(AHsName, Scheme)]
> typeCheckHsModule moduleSyntax = 
>   let dumps = []           -- the should be no dump

> --      moduleSyntax = parseHsSource inp
> --      moduleSyntax = hsModule
>       moduleSyntaxFixedFunBinds = fixFunBindsInModule moduleSyntax
>       annotatedSyntax = toAHsModule moduleSyntaxFixedFunBinds 
>       preludeModInfo = ModuleInfo {
>                            moduleName = AModule "Prelude",
>                            varAssumps = (listToEnv $ map assumpToPair preludeDefs),
>                            tyconsMembers = tyconsMembersHaskellPrelude, 
>                            dconsAssumps = (listToEnv $ map assumpToPair preludeDataCons),
>                            classHierarchy = listToEnv preludeClasses,
>                            kinds = (listToEnv preludeTyconAndClassKinds),
>                            infixDecls = preludeInfixDecls,
>                            synonyms = preludeSynonyms
>                           }

> --      initialModInfo = joinModuleInfo preludeModInfo importedModInfo
>       initialModInfo = preludeModInfo

>       -- call the type inference code for this module 
>       (moduleEnv, 
>        dataConEnv,
>        newClassHierarchy, 
>        newKindInfoTable,
>        moduleIds,
>        moduleRenamed,
>        moduleSynonyms) = tiModule dumps annotatedSyntax initialModInfo

>       -- this is the modInfo to print into an intermediate file
>       modInfo = ModuleInfo { varAssumps = moduleEnv, 
>                              moduleName = Typecheck.Utils.getAModuleName annotatedSyntax,
>                              dconsAssumps = dataConEnv, 
>                              classHierarchy = newClassHierarchy,
>                              kinds = newKindInfoTable,
>                              tyconsMembers = Typecheck.MultiModuleBasics.getTyconsMembers moduleRenamed,
>                              infixDecls = Typecheck.MultiModuleBasics.getInfixDecls moduleRenamed,
>                              synonyms = moduleSynonyms }

>       -- possibly write the intermediate file, if command line specifies so
>       --case intermediateFile of
>       --  Nothing                        -> return ()
>       --  Just possibleIntermediateName  -> writeModuleInfo possibleIntermediateName annotatedSyntax modInfo

> --  in initialModInfo  -- Typecheck.PPrint.render $ pprintEnv kindInfo}
> --  in Typecheck.PPrint.render $ pprintEnv moduleEnv
> --  in Typecheck.PPrint.render $ pprintEnv moduleEnv
>   in envToList moduleEnv




---------------------------------------------


> tiModule :: [String]                    -- dump flags  
>          -> AHsModule                   -- syntax of module Typecheck.after parsing
>          -> ModuleInfo                  -- info about imported entities
>          -> (Env Scheme,          -- output variable assumptions (may be local, and pattern variables) 
>              Env Scheme,          -- output data cons assumptions 
>              ClassHierarchy,      -- output class Hierarchy 
>              KindEnv,             -- output kinds 
>              IdentTable,          -- info about identifiers in the module
>              AHsModule,           -- renamed module Typecheck.
>              [AHsDecl])           -- synonyms defined in this module

> tiModule dumps modSyntax imports =
>      let importVarEnv = varAssumps imports
>          importDConsEnv = dconsAssumps imports
>          importClassHierarchy = classHierarchy imports
>          importKindEnv = kinds imports
>          importSynonyms = synonyms imports
>          importTyconMembers = tyconsMembers imports
> 
> 
> -- print the name of the module Typecheck.being typed
>
>          moduleName = getAModuleName modSyntax
> --     putStr $ "\n\n ---- Type checking " ++ show moduleName ++ " ----\n\n"
>    
> -- print the syntax tree: depending on command line arguments
> 
> 
> -- split the module Typecheck.into seperate components
> 
>          tidyMod = tidyModule modSyntax
> 
> -- make all pattern bindings simple and remove type synonyms, convert do-notation into expression form
> 
>          desugaredTidyModule = desugarTidyModule importSynonyms tidyMod
> 
> -- uniquely rename variables and generate a table of information about identifiers
> 
> 
>          -- TODO: we probably need to worry about synonyms and 
>          --       the like as well but at the moment we can live
>          --       with vars and datacons only.
>          importedNames = getNamesFromEnv importVarEnv 
>                       ++ getNamesFromEnv importDConsEnv
>                       ++ getNamesFromTycons importTyconMembers
>                       ++ getNamesFromEnv importClassHierarchy 
>                       ++ getNamesFromEnv importKindEnv         
>                      --  ++ getNamesFromInfix  -- shouldn't need this as we get
>                      -- them as part of getting their types in the varEnv
>          -- because we need to know to rename True to Prelude.True
>          -- as well, and this is a convenient way to do it:
>          getNamesFromTycons :: [(AHsName, [AHsName])] -> [AHsName]
>          getNamesFromTycons = concatMap snd 
> 
>          (renamedTidyModule', identTable) = renameTidyModule importedNames desugaredTidyModule
>          -- we pass in the imported infix decls and also the ones from the local module
>          renamedTidyModule = Typecheck.Infix.infixer (tidyInFixDecls renamedTidyModule' ++ infixDecls imports) renamedTidyModule'
> 
> -- All the names are getting qualified but they are unqualified by fromAHsModule
> 
>      -- separate the renamed decls apart
>          rTyDecls    = tidyTyDecls    renamedTidyModule 
>          rDataDecls  = tidyDataDecls  renamedTidyModule 
>          rNewTyDecls = tidyNewTyDecls renamedTidyModule 
>          rClassDecls = tidyClassDecls renamedTidyModule 
>          rInstDecls  = tidyInstDecls  renamedTidyModule 
>          rTySigs     = tidyTySigs     renamedTidyModule 
>          rFunBinds   = tidyFunBinds   renamedTidyModule 
>          rPatBinds   = tidyPatBinds   renamedTidyModule 
> 
> 
> -- collect all the type signatures from the module Typecheck.(this must be done after renaming)
> 
>          allTypeSigs = (collectSigs (rFunBinds ++ rPatBinds)) ++ rTySigs
> 
> -- kind inference for all type constructors type variables and classes in the module
> 
>          classAndDataDecls = rDataDecls ++ rNewTyDecls ++ rClassDecls
> 
>          kindInfo = kiModule importKindEnv classAndDataDecls
> 
> -- collect types for data constructors
> 
>          localDConsEnv = dataConsEnv moduleName kindInfo (rDataDecls ++ rNewTyDecls)
> 
>          globalDConsEnv = localDConsEnv `joinEnv` importDConsEnv
> 
> -- generate the class hierarchy skeleton
> 
>          classHierarchyData 
>             = foldl (flip (addClassToHierarchy moduleName kindInfo)) importClassHierarchy rClassDecls
> 
>      -- add type class instances to the class hierarchy XXX this is broken
> 
>          cHierarchyWithInstances 
>             -- = addInstancesToHierarchy kindInfo classHierarchy (rinstanceDecls ++ rdataDecls)
>             = classHierarchyData

> {-
>  -- lift the instance methods up to top-level decls
> 
>          liftedInstances 
>             = concatMap (instanceToTopDecls cHierarchyWithInstances) rinstanceDecls
>          liftedInstanceNames 
>             = mapMaybe maybeGetDeclName liftedInstances 
>          identTableWithInstances
>             = foldl (\fm name -> addToFM name (bogusASrcLoc, Instance) fm) identTable liftedInstanceNames
> 
> -}

> -- build an environment of assumptions for all the type signatures
> 
>          sigEnv = listSigsToSigEnv kindInfo allTypeSigs 
> 
>          classMethodAssumptions = classMethodAssumps cHierarchyWithInstances
>      
>          classMethodNames = map (\assump -> assumpId assump) classMethodAssumptions
> 
>          identTableWithMethods
>             -- = foldl (\fm name -> addToFM name (bogusASrcLoc, ClassMethod) fm) identTableWithInstances classMethodNames 
>             = foldl (\fm name -> addToFM name (bogusASrcLoc, ClassMethod) fm) identTable classMethodNames 
> 
> -- binding groups for top-level variables
> 
>          programBgs 
>             = getBindGroups (rFunBinds ++ rPatBinds) getDeclName getDeclDeps
> 
>          program = makeProgram sigEnv programBgs

> -- type inference/checking for all variables
>
>          localVarEnv
>             = tiProgram 
>                  moduleName                     -- name of the module
>                  sigEnv                         -- environment of type signatures
>                  kindInfo                       -- kind information about classes and type constructors
>                  cHierarchyWithInstances        -- class hierarchy with instances
>                  globalDConsEnv                 -- data constructor type environment 
>                  importVarEnv                   -- type environment
>                  program                        -- binding groups

>      in (localVarEnv, 
>          localDConsEnv,
>          cHierarchyWithInstances, 
>          kindInfo, 
>          identTableWithMethods, 
>          tidyModuleToAHsModule renamedTidyModule,
>          tidyTyDecls tidyMod) 


---------------------------------------------



parses the haskell source code into
the haskell data structure

> parseHsSource :: String -> HsModule
> parseHsSource s = case parse s (SrcLoc 1 1) 0 [] of
>                       Ok state e -> e
>                       Failed err -> error err
