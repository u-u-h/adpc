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


%include lhs2TeX.fmt
%format <-        = "\char''30"
%format not = "not"
%if False

> module Annotate where
> import ParseTree
> import TypecheckTree
> import TypecheckMonad
> import Tools
> import List
> import Char
> import Monad

%endif

\subsection{Module Annotate    }

The parse tree is prepared for the type check algorithm 
by annotating all program parts with type information.
The module @Annotate.lhs@ converts the parse of an ADP program 
to a type check tree. 
The type definition for that tree is located in the module @TypecheckTree.lhs@, 
 which is discussed in \ref{typechecker}.
The type information from the algebra type is applied to generate
concrete types for each algebra function.
Meanwhile, potential errors and type mismatches are reported. 

\subsubsection{Annotation of an ADP Program    }


The function |annotate| executes the conversion from parse tree to type check 
tree in a monadic computation. 
The type check monad we use here, is introduced in \ref{monad}.
To convert an ADP program, all constituents have to be checked,
transformed, and combined into the new type.
The conversion is done via structural recursion.

> annotate :: ADPProgram -> TCMonad TCADPProgram
> annotate (ADPProgram tsyns exts at algs gr) = do
>      tsyns' <- annotTSyns tsyns
>      exts'  <- annotExts tsyns' exts
>      env1   <- (tsyns' `addEnvs` exts') >>= nubEnv
>      algs'  <- annotAlgs env1 at algs
>      gr'    <- annotGramm gr
>      return (TCADPProgram env1 algs' gr')
>  where


\subsubsection*{Annotation of Type Synonyms     }

The definitions of all type synonyms are collected by the parser in a list.
The function |annotTSyns| transforms a list of type synonym definitions 
to an environment of typed variables, 
in which all type synonyms are fully expanded.
A type error will be reported if the expansion fails 
(see function |expandSyns| in section \ref{monad}).

> annotTSyns :: [TypeSyn] -> TCMonad Env
> annotTSyns tsyns = annotTSyns' $ map toTypedVar tsyns
>    where
>     toTypedVar (TypeSyn ln n tdef) = (n:>:(annotTypeDef tdef), ln)

Type synonyms can also be used inside the definitions of type synonyms.
Thus, the synonyms inside the definition of any newly defined synonym must 
be expanded. If the expansion fails, an error is reported (error 
 \ref{LocSim.TypeSyn.UnresolvedTypeSyn.adp}).
This function takes a list of type synonyms 
and for each of the synonyms, expands the \emph{synonyms inside} it.
\newpage

>     annotTSyns' :: Env -> TCMonad Env
>     annotTSyns' tsyns =  mapM (annotTSyn tsyns) tsyns
>      where
>       annotTSyn :: Env -> TypedVar1 -> TCMonad TypedVar1
>       annotTSyn env (n:>:t, ln) = do
>          t' <- expandSyns env t `errorIn` (show ln)
>          return (n:>:t',ln)



\subsubsection*{Annotation of External Functions     }

An ADP program may contain type definitions of external functions 
(indicated by keyword |#extern|).
These definitions may also contain type synonyms.
We have to expand all type synonyms inside the type signatures of external
functions.

> annotExts :: Env -> [TypeDecl] -> TCMonad Env
> annotExts tsyns e =  mapM (annotExt tsyns) e

The above function expands the following one to work on lists:

>        where annotExt :: Env -> TypeDecl -> TCMonad TypedVar1
>              annotExt env (TypeDecl ln f td)  = do
>                t <-  expandSyns env (annotTypeDef td)
>                return (f:>:t,ln)


\subsubsection{Annotation of an Algebra     }

The first two compulsory main constituents of an ADP program are 
one or more algebras and their algebra type.
The function @annotAlgs@ transforms each element of a list of algebras.

> annotAlgs :: Env -> AlgebraType -> [Algebra] -> TCMonad [TCAlgebra]
> annotAlgs env1 at algs = mapM (annotAlg env1 at) algs

The function @annotAlg@, which transforms a single algebra, gets the 
environment of typed variables as its first argument.
This environment contains types of external definitions and type synonyms.
The second and third argument are the list of algebras and the algebra type,
which will be made concrete for each algebra by assignments to the algebra 
type variables.

\begin{enumerate}
% 1
\item First, the function checks if the algebra type is suitable
for the given algebra 
(error \ref{LocSim.Algebra.AlgebraTypeNameMismatch.adp}).

%2
\item Types are assigned to the algebra type variables by the algebra.
Our function checks if the correct number of parameters was provided. 
If it did not provide the right number of parameters,
this is an error 
(error \ref{LocSim.Algebra.NumberOfAlgebraTypeVariables.adp}).

%3
\item Then we check if the number of function types that are defined in the algebra 
type equals the number of functions defined in the algebra 
(error \ref{LocSim.Algebra.NumberOfFunctionsVsFunctionTypes.adp}).
\end{enumerate}

If all tests succeed, we bind the assigned types to the algebra type 
variables and convert each of the algebra functions to the 
new data type used in the type check tree. 
Finally, the algebra function definitions are ordered according to their 
occurrence in the tuple which represents the algebra.

> annotAlg :: Env -> AlgebraType -> Algebra -> TCMonad TCAlgebra
> annotAlg exts  at@(AlgebraType ln1 atn atvar tupleTdefs) 
>                   alg@(Algebra ln2 aft afd) = do
>   if ( atn /= aatn ) -- 1. same type name in algebra type and algebra?
>    then typeError $ m1
>    else if ( length atvar /= length atpar ) -- 2. algebra type variables
>     then typeError $ m2
>     else do
>     appendTrace $ "algebra type variables bound by algebra "++show an++
>                   ": \n"++(unlines . map show) (zip atvar atpar)
>     funTs' <- annotFunTs exts ln1 atvar atpar funTs
>     if ( length funs /= length funTs' ) -- 3. functions and types
>        then typeError $ m3 funTs'
>        else do 
>             -- sort functions according to the tuple of types
>             funs'   <- sortLikeTuple tuple funs
>             funs''  <- zipWithM annotAlgFunDef funTs' funs'
>             return (TCAlgebra ln2 an funs'')
>  `inAlg` (an,ln2)

The following messages are created on errors.

>   where

>      m1 = "Defined algebra type is "++atn++", \nbut algebra "++show an
>         ++" has type "++show aatn++"."
>      m2 = "Not the correct number of algebra type variables specified:\n"
>         ++numerus (length atvar) "variable"++" in algebra type, but "
>         ++numerus (length atpar) "parameter"++" given."
>      m3 ts = numerus (length ts) "function type"++" in algebra type, \nbut "
>            ++numerus (length funs) "function"++" in algebra."

To traverse the algebra and its components, we use the following functions.
\newpage

>      funTs = getTDefs tupleTdefs
>        where getTDefs :: TypeDef -> [TypeDef]
>              getTDefs (TypeTuple tds) = tds
>      an    = findAN   aft
>        where findAN :: AlgebraFunType -> Name
>              findAN   (AlgebraFunType ln an tn tds) = an
>      aatn  = findAATN aft
>        where findAATN :: AlgebraFunType -> TypeName
>              findAATN (AlgebraFunType ln an tn tds) = tn
>      atpar = findATP  aft
>        where findATP  (AlgebraFunType ln an tn tds) = tds
>      funs  = findAFDFD afd
>        where findAFDFD (AlgebraFunDef ln n ns fds) = fds
>      tuple = findAFN   afd
>        where findAFN   (AlgebraFunDef ln n ns fds) = ns

\subsubsection*{Annotation of The Function Type of an Algebra Function    }

To process the function type of an algebra function definition, 
we first check if all constituents of the function type are allowed here.
Then we replace the algebra type variables by the concrete types that
have been specified by the algebra.
The function types are augmented with the line number of the algebra type
before they are returned. 
The function |errorIn| in the last line augments type errors with context 
information. It will be explained in section \ref{monad}.

> annotFunTs :: Env -> LineNumber -> [Name] -> [TypeDef] -> [TypeDef] 
>                   -> TCMonad [(Type,LineNumber)]
> annotFunTs env ln atvar atpar funTs = do
>    mapM allowed funTs
>    atvars <- annotATVars env ln atvar atpar
>    let funTs' = annotTypeDefs funTs
>    let ts = concatMap typeToList funTs'
>    let unused = [v| v <- atvar, (toType v) `notElem` ts]
>    if (unused /= []) 
>       then appendWarning $ "unused algebra type "
>                          ++ numerus' (length unused) "parameter" 
>                          ++": "
>                          ++ concatWith ", " unused
>       else done
>    funTs'' <- mapM (expandVars atvars) funTs'
>    return $ zip funTs'' (repeat ln)
>  `errorIn` ("algebra type"++show ln)
>   where 

Some types are not allowed in the algebra type, 
for example the type |Parser b| is just allowed in the grammar 
(error \ref{LocSim.Algebratype.IllegalType.adp}).
The types are checked by the following function.

>            toType v = (TVar (v:>:Any))
>            allElem vars types = all (\var -> var `elem` types) vars
>            allowed :: TypeDef -> TCMonad ()
>            allowed t = case t of
>                SingleType at -> done
>                FunType t1 t2 -> do
>                     allowed t1
>                     allowed t2
>                     done
>                TypeTuple tds -> done
>                ListType  td  -> done
>                _             -> typeError $ 
>                 "Only single types or tuples or lists or variables "
>                 ++"may be used in algebra type.\n"
>                 ++"Type "++prettyPrint t++" not allowed."

The algebra type variables will now be prepared for their replacement in 
the algebra type. 
Again, not all types may be assigned to algebra type variables. 
Type synonyms can be used here and we have to expand them.
Variables and assigned types are then combined to typed variables.
The line number of the algebra type is included.

> annotATVars :: Env -> LineNumber -> [Name] -> [TypeDef] -> TCMonad Env
> annotATVars env ln vars pars = do
>                mapM allowed pars
>                pars' <- mapM (expandSyns env) (annotTypeDefs pars)
>                let tvars = zipWith (:>:) vars pars'
>                return (zip tvars (repeat ln))

Some types are not allowed to be assigned to algebra type variables,
it would be illegal to use a variable here 
(error \ref{LocSim.Algebra.IllegalTypeAssignedToAlgebratypeVariable.adp}).

>   where 
>             allowed :: TypeDef -> TCMonad ()
>             allowed t = case t of
>                SingleType at -> done
>                TypeTuple tds -> done
>                _             -> typeError $ 
>                 "Only single types or tuples "
>                 ++"may be assigned to algebra type variables.\n"
>                 ++"Type "++prettyPrint t++" not allowed."

With this information the algebra function definitions can be finally be 
transformed.\\
The function |annotAlgFunDef| is applied to each algebra function definition.
First, a list of types is extracted from the corresponding function type,
in order to zip the list of parameters with their types.
The number of parameters must match the number of types 
(errors 
\ref{LocSim.Algebra.AlgebraFunctionTooManyParameters.adp} and
\ref{LocSim.Algebra.AlgebraFunctionNotEnoughParameters.adp}
).
We also compute a list of all bindings that are created by the parameters.
With each of these bindings we store the number of the argument that produced 
it. 
Then the result type of the algebra function is computed.
Finally, the algebra function definition is returned in the new form.

> annotAlgFunDef :: (Type, LineNumber) -> FunDef -> TCMonad TCAFunDef
> annotAlgFunDef (t, lnt) (FunDef lnf f ps exp) = do
>    -- extract a list of types in order to zip parameters with their types
>    let ts  = typeToList t
>    let lps = length ps
>    let lts = (length ts)-1 -- without result type
>    if (lps > lts)
>      then typeError $ " Too many parameters"++message lts lps lnt lnf
>      else done
>    if (lps < lts)
>      then typeError $ " Not enough parameters"++message lts lps lnt lnf
>      else --if (lps == lts)
>           do
>            ps'         <- zipWithM annotPar ps (init ts)
>            bindings    <- getParBindings ps (init ts)
>            let ft       = last ts
>            let exp'     = annotExp exp
>            return (TCAFunDef lnt lnf f ps' bindings ft exp')
>   `inAFunDef` (f,lnf)
>  where 
>    message lts lps lnt lnf = ": \n Algebra function declared with "
>       ++numerus lts "parameter"++" in "++show lnt++",\n but used with "
>       ++numerus lps "parameter"++" in "++show lnf++"."

To annotate a single parameter of an algebra function with its type,
we have to traverse compound parameters via structural recursion.
In the basic cases we return pairs of parameter and type.
For tuple parameters, the type has to be a tuple type with the right
arity (error 
\ref{LocSim.Algebra.AlgebraFunctionTupleParameterTupleLength.adp}).
Wildcards are kept like any other parameter, although they create 
no bindings.
\newpage

> annotPar :: Parameter -> Type -> TCMonad TCParam
> annotPar parameter typ = do
>  case parameter of
>   Wildcard           -> return (TCVar "_", typ)
>   Parameter s        -> return (TCVar s,  typ)
>   TupleParam spars   -> case typ of
>     TVar (f:>:t) -> do (tuple', t') <- annotPar (TupleParam spars) t
>                        return (tuple', TVar (f:>:t'))
>     TSyn (f:>:t) -> do (tuple', t') <- annotPar (TupleParam spars) t
>                        return (tuple', TSyn (f:>:t'))
>     TTuple ts    -> if (length spars)==(length ts)
>                      then do
>                       varsNpars     <- zipWithM annotPar spars ts
>                       let (spars', ts') = unzip varsNpars
>                       return (TCTuple spars', TTuple ts')
>                      else typeError $ "Tuple parameter "
>                             ++prettyPrint parameter++" is a "
>                             ++show (length spars)++"-tuple, but type is a "
>                             ++show (length ts)++"-tuple."
>     t            -> typeError $ "Tuple parameter "++prettyPrint parameter
>                   ++" declared with type "++prettyPrint t
>                   ++", which is no tuple type."
>   p                  ->   typeError "Annotate.annotPar"

An algebra function may have an arbitrary number of parameters,
of which each one, except for a wildcard, creates a binding.
When we annotate an algebra function definition, 
we collect a list of these bindings.
Two parameters of an algebra function are not allowed to have the same name
(error 
\ref{LocSim.Algebra.AlgebraFunctionParameterMultipleTimes.adp}).

> getParBindings :: [Parameter] -> [Type] -> TCMonad [TypedVar]
> getParBindings ps ts = do
>     allBindings <- zipWithM getParBinding ps ts
>     let tvs = concat allBindings
>     case (double tvs) of
>        []                      -> return tvs
>        (id:>:t,times):doubles  -> typeError $ "Parameter "++show id
>                                            ++" used "++numerus times "time"
>                                            ++"."
>    where double vs = [(head g,length g) | 
>                       g <- groupBy (comparing varname) (sort vs), 
>                       (length g) > 1 ]
>          comparing :: (Eq a) => (b -> a) -> b -> b -> Bool
>          comparing p x y = (p x) == (p y)
>          varname (v:>:t) = v

The above function works on lists of parameters and types.
This function gets all variable bindings from a single parameter 
and its type. This means in particular, 
that it gets one variable binding from a single variable or 
it gets a list of variable bindings from a tuple of variables.
We check if a tuple type has a corresponding parameter 
that is also a tuple (error 
\ref{LocSim.Algebra.AlgebraFunctionNoTupleTypeForTupleParameter.adp}).

> getParBinding :: Parameter -> Type -> TCMonad [TypedVar]
> getParBinding parameter typ = case parameter of
>   Wildcard           ->   return []
>   Parameter p        ->   return [p:>:typ]
>   TupleParam ps      ->   case typ of
>     TSyn (f:>:t) -> getParBinding parameter t
>     TVar (f:>:t) -> getParBinding parameter t
>     TTuple ts    -> do 
>                     bs     <- zipWithM getParBinding ps ts
>                     return $ concat bs
>     t            -> typeError $ "found type "++show t
>                   ++" for tuple, which is not a tuple type"
>   x                  ->   typeError "Annotate.annotPar"

As we have seen, the algebra function definitions are sorted during the 
annotation of an algebra according to the tuple of function names, 
which represents the algebra.
The function for this task is defined here.
It reports an error, if not every function has a type and vice versa,
or if a function or a type appear multiple times 
(errors 
\ref{LocSim.Algebra.AlgebraFunctionDeclaredAndDefinedMultipleTimes.adp}
to
\ref{LocSim.Algebra.AlgebraFunctionNotDeclaredAndNotDefined.adp}).

> sortLikeTuple :: [Name] -> [FunDef] -> TCMonad [FunDef]
> sortLikeTuple decls defs = case (dupdecl, dupdef) of
>       (a:as,b:bs) -> typeError $ mapMssg m1 dupdecl ++";\n"
>                               ++ mapMssg m2 dupdef++"."
>       (a:as,  []) -> typeError $ mapMssg m1 dupdecl ++"."
>       ([],  b:bs) -> typeError $ mapMssg m2 dupdef
>                               ++". \nPattern matching not allowed in ADP."
>       ([],    []) -> case (undef, undecl) of
>         (a:as,b:bs) -> typeError $ mapMssg m3 undef++";\n"
>                                 ++ mapMssg m4 undecl ++"."
>         (a:as,  []) -> typeError $ mapMssg m3 undef++"."
>         ([],  b:bs) -> typeError $ mapMssg m4 undecl ++"."
>         ([],    []) -> return sorted
>   where

The following functions help to generate the error messages.
\newpage

>          mapMssg a = (concatWith ", ").(map a)
>          dupErrMssg messg (f,times) = (errMssg messg f)++" "
>                                     ++numerus times "time"
>          errMssg    messg  f        = "Function "++show f++" "++messg
>          m1 = dupErrMssg "is declared in algebra head"
>          m2 = dupErrMssg "is defined"
>          m3 = errMssg "declared in algebra head, but not defined"
>          m4 = errMssg "defined, but not declared in algebra head"
>
>          dupdecl = duplicate decls
>          dupdef  = duplicate defs'
>          duplicate as = [(head g,length g) | g <- group (sort as), 
>                                              length g > 1]
>
>          undef  = [d | d <- decls, d `notElem` defs']
>          undecl = [d | d <- defs',  d `notElem` decls]
>
>          defs' = map getId defs
>          getId (FunDef lnt ident ps e) = ident

The result is finally computed here.

>          sorted = [ def | id<-decls, 
>                           def@(FunDef lnt ident ps e)<-defs, id==ident]

\subsubsection{Annotation of the Grammar     }

The next main constituent of an ADP program is the grammar.
In contrast to the algebras, which have a specified algebra type,
we do not have any ``a priori'' type information about the grammar.\\
The grammar functions are then separated into grammar productions |prods| 
and definitions of user defined combinators |combs|.
We check if the axiom is among the set of nonterminals before returning the 
transformed grammar.

> annotGramm :: Grammar -> TCMonad TCGrammar
> annotGramm (Grammar ln gn ax gt alg gfds) = do
>       eitherProdsCombs <- mapM annotGrammFunDef gfds
>       let (prods,combs) = catEithers eitherProdsCombs
>       let nonterminals = map getId prods
>       if ((not.elem ax) nonterminals) 
>                then typeError $ "Axiom "++show ax
>                               ++" not found in set of nonterminals."
>                else return (TCGrammar ln gn alg prods combs)
>    `inGr` (gn,ln)
>  where
>   getId (TCGFunDef ln id tcExp) = id
>   catEithers :: [Either a b] -> ([a],[b])
>   catEithers es = ([x | Left x <- es], [x | Right x <- es])

The conversion of a grammar production has to be divided according to the 
different constructors of that type.

In ADP, the programmers may define their own combinators.
These combinator definitions are also represented as grammar function 
definitions, with the constructor @CombinatorDef@.

The main purpose of a grammar production is the definition of a 
grammar nonterminal symbol, with the constructor @GrammarFunDef@.
The pattern matching has three cases here.\\
\begin{itemize}
\item The first case is a definition with parameters. Grammar nonterminals may 
not have any parameters in ADP yet\footnote{although this might be added 
in the future} (error 
\ref{LocSim.Grammar.GrammarFunctionHasParameters.adp}
). 
\item The second case is a definition where some definitions from a 
@where@-clause have not been lifted. This should not happen.
\item The third case is a correct definition of a nonterminal.
We have to transform the production to a type check expression.
This is done in the function |annotProd|.
\end{itemize}
In this way, the function returns either a grammar production
or a user-defined combinator.

> annotGrammFunDef :: GrammarFunDef
>                  -> TCMonad (Either TCGFunDef (Name,LineNumber))
> annotGrammFunDef gfd = case gfd of
>   CombinatorDef ln id combinatorDef -> return (Right (id,ln))
>   GrammarFunDef ln id (par:pars) tabtype prod gFuns ->
>              typeError $ "Grammar function "++id
>                        ++" has parameters."
>   GrammarFunDef ln id pars tabtype prod (gFun:gFuns) ->
>              typeError $ "Annotate.annotGrammFunDef: "
>                        ++"where-functions not lifted in "
>                        ++"grammar function "++id
>   GrammarFunDef ln id [] tabtype prod [] -> do
>              tcExp <- annotProd prod `inGFunDef` (id,ln)
>              return (Left (TCGFunDef ln id tcExp))
>   x       -> error "Annotate.annotGrammFunDef"


We convert each production to a type check expression. 
As suggested before, combinator usage is treated just as a function 
application to two arguments here.
The conversion of an expression is done in the separate function |annotExp| as 
described below.
List comprehensions are not checked yet (error \ref{LocSim.Grammar.ListComprehension.adp}).

> annotProd :: Production -> TCMonad TCExp
> annotProd prod = case prod of
>   ProdExpr          ln exp            -> return $ annotExp exp
>   CombinatorAp      ln p1 c p2        -> do
>                                 p1' <- annotProd p1
>                                 p2' <- annotProd p2
>                                 return (TCApp c [p1',p2'])
>   ListComprehension ln par exp lcexps -> typeError 
>                                 $ "List comprehensions "
>                                 ++"not checked yet."
>   x       -> error "Annotate.annotProd"


\subsubsection*{Annotation of an Expression     }

This function annotates an expression.
Subexpressions in compound expressions are annotated recursively.
This function converts the right-hand side expressions of
algebra function definitions \emph{and} grammar function definitions.
This unified representation facilitates checking algebra function definitions 
and grammar productions with the same mechanism.

> annotExp :: Expression -> TCExp
> annotExp exp = case exp of
>     Ident s                    -> TCVar     s
>     Char c                     -> TCChar    c
>     String s                   -> TCString  s
>     Num  s                     -> TChNum    s
>     Tuple exps                 -> TCTuple (map annotExp exps)
>     If e1 e2 e3                -> TCIf (annotExp e1) 
>                                        (annotExp e2) (annotExp e3)
>     BinOp e1 ParseTree.Add e2  -> TCApp "+"  [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.Sub e2  -> TCApp "-"  [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.Mul e2  -> TCApp "*"  [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.Div e2  -> TCApp "/"  [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.Exp e2  -> TCApp "^"  [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.LT e2   -> TCApp "<"  [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.LE e2   -> TCApp "<=" [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.GT e2   -> TCApp ">"  [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.GE e2   -> TCApp ">=" [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.EQ e2   -> TCApp "==" [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.NE e2   -> TCApp "/=" [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.And e2  -> TCApp "&&" [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.Or e2   -> TCApp "||" [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.App e2  -> TCApp "++" [annotExp e1,annotExp e2]
>     BinOp e1 ParseTree.Cons e2 -> TCApp ":"  [annotExp e1,annotExp e2]
>     BinOp e1 bop e2            -> error $ "Annotate.annotExp: "
>                                        ++"unknown binary operator "
>                                        ++ show bop
>     FunAp x  []       -> TCVar x
>     FunAp s  exps     -> TCApp s ((map annotExp) exps)
>     x                 -> error "Annotate.annotExp"


\subsubsection*{Annotation of Types     }

To annotate a list of type definitions, 
we have to transform each element to the new representation.

> annotTypeDefs :: [TypeDef] -> [Type]
> annotTypeDefs tds = map annotTypeDef tds


The transformation of a single type definition is a simple mapping between 
the two representations.
Type variables and type synonyms are initialised with type |Any|.
Their types will be specified during the following steps of computation.

> annotTypeDef :: TypeDef -> Type
> annotTypeDef typedef = case typedef of
>     SingleType (TTypeVar a) -> TVar (a:>:Any) -- type not known yet
>     SingleType (TTypeSyn a) -> TSyn (a:>:Any)
>     SingleType TInt         -> tInt
>     SingleType TInteger     -> tInteger
>     SingleType TFloat       -> tFloat
>     SingleType TDouble      -> tDouble
>     SingleType TChar        -> tChar
>     SingleType TString      -> tString
>     SingleType TBool        -> tBool
>     SingleType TVoid        -> tUnit
>     TypeTuple  tds          -> TTuple (map annotTypeDef tds)
>     ListType   tDef         -> TList  (annotTypeDef tDef)
>     TParser    tDef         -> TyParser (annotTypeDef tDef)
>     FunType    tDef1 tDef2  -> annotTypeDef tDef1:->annotTypeDef tDef2
>     Constraint (TCNum      id) tDef -> ClassConstraint (id:>:tNum)      
>                                                        (annotTypeDef tDef)
>     Constraint (TCIntegral id) tDef -> ClassConstraint (id:>:tIntegral) 
>                                                        (annotTypeDef tDef)
>     Constraint (TCFloating id) tDef -> ClassConstraint (id:>:tFloating) 
>                                                        (annotTypeDef tDef)
>     x -> error "Annotate.annotTypeDef"

The annotation phase is completed by running the functions of this section.
