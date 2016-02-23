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
%if False

> module Typecheck where
> import Data.List
> import ParseTree
> import Annotate
> import TypecheckTree
> import TypecheckMonad
> import Tools
> import Control.Monad


%endif

\section{The Actual Type Checker}

The following module contains the core of this thesis, the type check algorithm. 
It finally uses all functions and data types that have been previously defined.
We follow the structure levels of ADP in a top down way, as announced in 
\ref{adpstructure}.

\subsection{The Type Checker}

The |typecheck| function runs the type check of an ADP program in a monadic
computation. 
We start with the empty environment |env0| and the empty list of
trace, warnings and errors |state0|.
The computation returns either an error or a correct result.
All information that the monad returns as a result,
is also returned by this function in a four-tuple.
This tuple contains either an error or the correct result in the first position,
and lists of strings for trace, errors and warnings in the second to fourth
position. As a correct result, all derived types of all algebra functions and grammar 
nonterminals are returned in a list of environments.
For each combination of algebra and grammar one environment of function types is derived.

> typecheck :: ADPProgram -> (Either Error [Env], [String], [String], [String])
> typecheck adpprog = case exp' of
>   Right result ->  (exp', t, w, e)
>   Left  err    ->  (exp', t, w, e++[(show [err])])
>  where
>       (exp', (t, w, e)) = run (tcPhases adpprog) (env0, state0)

As seen before, our monadic computation consists of two main steps,
the first of which is the conversion of the parse tree to a type check tree, 
as described in module @Annotate.lhs@ \ref{annotate}.
The second step is the type check on that ``new'' type check tree.
Throughout the computation, the monad is used to handle error messages.

> tcPhases :: ADPProgram -> TCMonad [Env]
> tcPhases pt = do
>    tct <- compileStep annotate  pt  "Type Check Tree" show Trace
>    compileStep        tcProgram tct "Type Check"      show Trace



\subsubsection*{Type Check     }

To type check a program, we have to check all its components via structural 
recursion.

Both external definitions and type synonyms just had to be added 
to the environment. This was done in the annotation phase. The information of
the algebra type was also utilised in the annotation phase.

We still have to check the algebras and the grammar.
In the first step, we check each algebra and return an environment 
of typed algebra functions for each algebra.
Then we check the grammar with each of these algebra environments 
and return for each algebra an environment of typed grammar rules.

The type checked algebra environments come in pairs with the algebra names.
The pairs have to be unzipped to get the environments.

Now we have a list of lists of typed algebra functions (one list for each 
algebra) from the first step and a list of lists (one list for each algebra 
again) of typed grammar nonterminals.

Finally, the environments of typed algebra functions are reordered,
 so that each algebra environment is returned in sequence with their 
associated grammar environment. \\


> tcProgram :: TCADPProgram -> TCMonad [Env]
> tcProgram (TCADPProgram env1 algs gramm) = do
>       algsAndEnvs   <- updateEnv env1 (mapM tcAlgebra algs)
>       grammEnvs <- mapM (prepGEnv gramm env1) algsAndEnvs
>       let (algNames, algEnvs) = unzip algsAndEnvs
>       return $ concat (zipWith (\a b -> [a,b]) algEnvs grammEnvs)



\subsubsection*{Import Declarations, Type Synonyms, and External Functions   }

Imported files were included before parsing.

The definitions of type synonyms have been checked in the annotation phase.
Defined type synonyms and type definitions of external functions are collected 
in the type check tree already (in |env1|). 
They are the basic known types. 
And if the standard library (see \ref{adpstdlib}) was imported,
this environment also includes the types of all built-in functions.
The basic environment |env1| can just be taken from the parse tree and be added 
to the environment.


\subsubsection{Type Check of an Algebra     }

An algebra consists basically of a list of algebra functions.
To check an algebra, we have to check each of the algebra function definitions.
We also state that we entered this algebra. This context information 
is appended to the trace in the first line of the |do|-block, 
and is kept for error messages in the last line of this function.

> tcAlgebra :: TCAlgebra -> TCMonad (Name, Env)
> tcAlgebra (TCAlgebra ln a fs) = do
>                             appendTrace $ "\nchecking algebra "++show a
>                             fs' <- tcAFunDefs fs
>                             return (a, fs')
>  `inAlg` (a,ln)



\paragraph*{Type Check of the Algebra Functions }


The algebra functions must not contain applications of other algebra 
functions. 
This means, the type of a function is not needed by the other algebra functions
and therefore does not need to be added to the algebra\footnote{compare to 
grammar function check}. 
So we just check each of the functions.

> tcAFunDefs :: [TCAFunDef] -> TCMonad Env
> tcAFunDefs ds = mapM tcAFunDef ds

To type check an algebra function, we have to type check the parameters first.
After checking if they all have a specified type (i.e. not type |Any|),
we prepare the variable bindings that have been created by the parameters.
Then we check if the function has a proper result type 
(errors \ref{LocSim.Algebra.AlgebraFunctionWrongResult.adp} and 
\ref{LocSim.Grammar.GrammarFunctionWrongResult.adp}).
The type of the whole function is constructed out of the types of the arguments 
and of the result type.
This function type and the variable bindings are then put into the current environment.

With this knowledge about the types of all these variables, we can
 compute the type of the right-hand side of the function.
The expected result type is finally compared to the derived result type.

In the last line of this function we add some context information again,
which will be used in case of an error.

> tcAFunDef :: TCAFunDef -> TCMonad TypedVar1
> tcAFunDef (TCAFunDef lnt lnf f ps vbs tres e) =
>     do appendTrace $ "   checking function "++show f
>        tps  <- tcParams ps
>        hasType tres
>        let tf   = tFun tps tres
>        let vbs' = zip vbs (repeat lnt)
>        te   <- tcRhs "Algebra" lnf ((f:>:tf,lnf):vbs') e
>        (tres,lnt) |^| (te,lnf) `errorIn` ("result type")
>        return (f:>:tf,lnf)
>   `inAFunDef` (f,lnf)

This function maps the type check function for a parameter to a list of 
parameters.

> tcParams :: [TCParam] -> TCMonad [TCParam]
> tcParams = mapM tcParam

Type checking a parameter is just testing if it has a proper declared type,
it must not have the unspecified type |Any|.

> tcParam :: TCParam -> TCMonad TCParam
> tcParam (x,t) = do 
>                 hasType t `inParam` x
>                 return (x,t)

Type checking a right-hand side of a definition requires the population of 
the environment with all variables that were bound on the left-hand side.
In context of a grammar we also apply a special grammar check for combinator 
expressions, |precheckGrammarExp|, which will be explained below.

> tcRhs :: String -> LineNumber -> Env -> TCExp -> TCMonad Type
> tcRhs context lnf tps e = updateEnv tps (do
>         if (context=="Grammar")
>          then precheckGrammarExp lnf e
>          else done
>         tcExp context lnf e)

\paragraph{Expression }

Next, we implement the type check of an expression, as described in 
section \ref{theorie:expression}.
Some cases are straightforward. For others we call separate functions.
The line number and context of the expression are passed on,
just in case subexpressions have to be checked.

> tcExp :: String -> LineNumber -> TCExp -> TCMonad Type
> tcExp context ln e = case e of
>    TCVar v          -> tcVar v
>    TCChar c         -> return tChar
>    TCString s       -> return tString
>    TChNum d         -> tcNum d
>    TCTuple es       -> if (context=="Grammar")
>           then typeError "Tuples must not be used in the Grammar."
>           else tcTuple ln context es
>    TCIf cnd thn els -> if (context=="Grammar")
>           then typeError "If-Then-Else must not be used in the Grammar."
>           else tcIf ln context cnd thn els
>    TCApp f es       -> tcApp context ln f es
>    x -> return $ error $ "Typecheck.tcExp"++show x

The following functions implement special cases for the constructors 
of the expression type. 
Compound expressions are checked via structural recursion.

> tcVar :: Name -> TCMonad Type
> tcVar v = do
>         (tv,lnDef) <- lookupVar v
>         return (TVar (v:>:tv))

> tcNum :: Double -> TCMonad Type
> tcNum s = return tDouble

> tcTuple :: LineNumber -> String -> [TCExp] -> TCMonad Type
> tcTuple ln context es = do
>         ts <- mapM (tcExp context ln) es `inTuple` (show (TCTuple es))
>         return (TTuple ts)

The condition of an if-then-else expression has to have type |Bool| 
(error \ref{LocSim.Algebra.IfExpressionNoBoolCondition.adp}).
If- and else-branch are required to have the same type 
(\ref{LocSim.Algebra.IfExpressionAlternatives.adp}).

> tcIf :: LineNumber -> String -> TCExp -> TCExp -> TCExp -> TCMonad Type
> tcIf ln context cnd thn els = do
>         tcnd <- tcExp context ln cnd
>         tthn <- tcExp context ln thn
>         tels <- tcExp context ln els
>         tBool =:= tcnd `errorIn` "condition of if"
>         tthn =:= tels `errorIn` "comparison of then- and else-branch"
>         return tthn

\paragraph{Expression: Function Application }

The function |tcApp| implements the type check for the most complex 
case of an expression, a function application.
The check can be used for both algebra and grammar function definitions, 
as introduced in \ref{theorie:tcapp}.
First, we have to look up the type of the applied function.
Then we check the argument expressions recursively,
substitute all type variables, and 
test if the argument types fit into the declared function type 
(errors \ref{LocSim.Algebra.FuctionApplicationWrongArgument.adp} and
\ref{LocSim.Grammar.FuctionApplicationWrongArgument.adp}), 
if the right number of them was provided at all
(errors 
\ref{LocSim.Algebra.FuctionApplicationNotEnoughArguments.adp}, 
\ref{LocSim.Grammar.FuctionApplicationNotEnoughArguments.adp},
\ref{LocSim.Algebra.FuctionAppliedToTooManyArguments.adp}, and
\ref{LocSim.Grammar.FuctionAppliedToTooManyArguments.adp}).

> tcApp :: String -> LineNumber -> Name -> [TCExp] -> TCMonad Type
> tcApp context lnUse f es = do
>   (tf,lnDef)   <- lookupVar f
>   let (constraint,tf') = getConstraint tf
>   let tfList           = typeToList tf'
>   let targs  = take (length es) tfList
>   let tres   = drop (length es) tfList
>   case tres of
>     []  -> typeError $ " Too many arguments"++message tfList targs
>     [x] -> do
>            tes <- mapM (tcExp context lnUse) es `inArg` ((head es),f)
>            let pars = zip targs tes
>            env <- tcVarBinds (lnDef,lnUse) context pars
>            let pars'   = mapFst (substituteVars env) pars
>            let tfList' = map (substituteVars env) tfList
>            case constraint of
>             [a:>:c]-> do
>                       b <- checkConstraint (a:>:c,lnDef) env
>                       let tfList''= map (substituteVars [b]) tfList'
>                       let tfListL = mapLn lnDef tfList''
>                       let tesL    = mapLn lnUse tes
>                       updateEnv (b:env) (typeOfApp 1 tfListL tesL)
>             []     -> do
>                       let tfListL = mapLn lnDef tfList'
>                       let tesL    = mapLn lnUse tes
>                       updateEnv env (typeOfApp 1 tfListL tesL)
>     (x:xs)-> typeError $ " Not enough arguments"++message tfList es
>    `inApp` (context, ((head es) ,f))
>  where message ts es = ":\n"++" Function "++show f++" should have "
>                   ++numerus ((length ts)-1) "argument"++", but given "
>                   ++show (length es)++"."
>        mapLn ln l = zip l (repeat ln)


\paragraph{Binding of Type Variables}

By the application of a function, one or more expressions are bound to each 
type variable in a function type.
These expressions all have types, 
and these types can be seen as being bound to the type variable. 
We have to check if these different bindings do not contradict.
This is done by intersecting the types.
The intersection of all types that are bound to a variable 
is the real type of the variable 
and substitutes the variable in the function type.
If the intersection is empty, this is a type error because some of the 
expressions have mismatching types.

This function checks the bindings of all type variables in a function 
application 
and also adds the number of the parameter that created the binding.

> tcVarBinds ::  (LineNumber,LineNumber) -> String -> [(Type, Type)] 
>                                           -> TCMonad [TypedVar1]
> tcVarBinds (lnDef,lnUse) context pairs = do
>                    -- variable bindings for all parameters
>                    allBindings  <- getExpBinds (lnDef,lnUse) pairs 
>                    -- list of lists of bindings for each parameter
>                    let blists = zipWith (zip.repeat) [1..] allBindings
>                    -- concat all bindings, because we know which parameter created it
>                    let blist = concat blists
>                    -- sort bindings by type variables
>                    let blists = groupBy (comparing varname) blist
>                    -- compare all bindings for each variable
>                    env <- mapM tcVar blists
>                    nubEnv env
>      where varname (argno,(a:>:t,ln)) = a
>            comparing :: (Eq a) => (b -> a) -> b -> b -> Bool
>            comparing p x y = (p x) == (p y)

This function checks the bindings of just one type variable 
in a function application.
The type variable must not be bound to mismatching types,
 i.e. the intersection of these types must not be empty 
(error 
\ref{LocSim.Algebra.FuctionApplicationTypeVarBoundToMismatchingTypes.adp} 
and 
\ref{LocSim.Grammar.FuctionApplicationTypeVarBoundToMismatchingTypes.adp}).

>            tcVar :: [(Int, TypedVar1)] -> TCMonad TypedVar1
>            tcVar [(argno1, tv)] = return tv
>            tcVar ((argno1, (a:>:t1,ln1)):(argno2, (b:>:t2,ln2)):bs) = do
>                 (t2',ln') <-(t1,ln1) |^| (t2,ln2) `errorIn` (
>                                             "arguments "++show argno1
>                                            ++" and "++show argno2
>                                            ++", type of variable "++show a
>                                            ++" bound to mismatching types")
>                 tcVar ((argno2, (b:>:t2',ln')):bs)

As we know from section \ref{typevars}, 
a type variable in a function type gets assigned one or more 
types if the function is applied to its arguments. 
This results in a list of bindings, where 
this function can be used to generate the list of bindings.
It gets the pair of line numbers for the function type definition, 
the function application, the types,
and at last the number of the argument that created the binding.
This argument number refers to the expression as an argument of the
``surrounding'' function application.\\
One argument can create more than one binding, if a tuple type is used.

> getExpBind :: (LineNumber,LineNumber) -> ((Type, Type),Int) -> TCMonad Env
> getExpBind lns@(lnDef, lnUse) (types,argno) = case types of
>       (TVar (v:>:Any),  t2    ) -> return [((v:>:t2),(lnUse))]
>       (t1,              Any   ) -> return []
>       (Any,             t2    ) -> typeError $ "Could not resolve Type."
>       (TVar (v:>:t1),   t2    ) -> getExpBind lns ((t1, t2),argno)
>       (TVar (v:>:Any), TVar (v':>:Any)) -> if (v==v') 
>                then return []
>                else return [((v:>:(TVar (v':>:Any))),(lnUse))]
>       (TSyn (s:>:t1),   t2     ) -> getExpBind lns ((t1, t2),argno)
>       (TList t1,       TList t2) -> getExpBind lns ((t1, t2),argno)
>       (TTuple as,     TTuple bs) -> do
>                listoflists <- mapM (getExpBind lns) 
>                                    (zip (zip as bs) (repeat argno))
>                let list     = concat listoflists
>                -- remove duplicates
>                nubEnv (sortBy (comparing fst) list)
>       (TyParser t1, TyParser t2) -> getExpBind lns ((t1, t2),argno)
>       (t11:->t12,   t21:->t22  ) -> do 
>                 t1 <- getExpBind lns ((t11, t21),argno)
>                 t2 <- getExpBind lns ((t12, t22),argno)
>                 return $ t1++t2
>       (ClassConstraint (t1:>:cl) t11, t21) -> typeError $ 
>                 "Class constraint found inside type declaration."
>       (t1,        TVar (v:>:t2)) -> getExpBind lns ((t1, t2),argno)
>       (t1,                 t2  ) -> do 
>             (t1,lnDef) |^| (t2,lnUse) `errorIn` (show argno++". argument")
>             return []
>       otherpair                -> return $ 
>                  error $ "Typecheck.getExpBind "++show otherpair

The function |getExpBinds| extends the above one to work on lists.
It is also the place where the ``argument number'' is added,
which refers to the fact that these types are in fact argument types of 
a function application.

> getExpBinds :: (LineNumber,LineNumber) -> [(Type, Type)] 
>                                           -> TCMonad [[TypedVar1]]
> getExpBinds lns ts = mapM (getExpBind lns) (zip ts [1..])


\paragraph{Type Class Membership}

To check if a function type has a type context for a certain type variable,
the function |getConstraint| may be used. 
We do not know if there is one, so we need a case to represent 
``no constraint''.
We could use a |Maybe| type here, but the representation as lists was chosen.
This was done because a list can easily be extended if we want to allow an 
arbitrary number of constraints in the future.
The constraint, if there is one, and the function type are returned as a pair.
If we have no constraint, an empty list is returned with the function type.

> getConstraint :: Type -> ([TypedVar], Type)
> getConstraint (ClassConstraint c t) = ([c],t)
> getConstraint  t                    = ([], t)

A type variable |a| may be restricted to a certain type class |c| by a type 
context.
The variable |a| is bound to a concrete type by its arguments,
these are expressions and have a type.
We have to check if the concrete type of |a| is a member of the type class |c| 
(errors \ref{LocSim.Algebra.FuctionApplicationTypeclassMembership.adp} and 
\ref{LocSim.Grammar.FuctionApplicationTypeclassMembership.adp}). 

> checkConstraint :: TypedVar1 -> Env -> TCMonad TypedVar1
> checkConstraint (v:>:c,cln) env = do
>   (t,ln) <- lookUp v env
>   (t',ln') <- (c,cln) |^| (t,ln) `errorIn` "check of type class membership"
>   return (v:>:t',ln')
>  where 

>   lookUp :: Name -> Env -> TCMonad (Type, LineNumber)
>   lookUp x env = case [(t,ln)|(y:>:t,ln)<-env,y==x] of
>      t:t':ts             -> typeError $ "annotate.varsInType: "++x
>                                    ++" found multiple times in environment"
>      [(TVar (v:>:t),ln)] -> if (v==x) then return (t,ln) 
>                                       else return ((TVar (v:>:t),ln))
>      [(TSyn (v:>:t),ln)] -> if (v==x) then return (t,ln) 
>                                       else return ((TSyn (v:>:t),ln))
>      [(t,ln)]            -> return (t,ln)
>      []                  -> typeError $ "annotate.varsInType: "++x
>                                       ++"not found in environment"


\subsubsection*{Helper Function for Algebra and Grammar}

This function allows to derive the type of an application, given the type of 
the function and the types of its arguments.
The types of the arguments must match their declared types (error 
\ref{LocSim.Algebra.FuctionApplicationWrongArgument.adp}, 
\ref{LocSim.Grammar.FuctionApplicationWrongArgument.adp}).

> typeOfApp  :: Int -> [(Type,LineNumber)] -> [(Type,LineNumber)] 
>                                          -> TCMonad Type
> typeOfApp argno [tf]        []     = return (fst tf)
> typeOfApp argno (targ:tres) (t:ts) = do 
>                 targ |^| t `errorIn` ("argument "++show argno)
>                 typeOfApp (argno+1) tres ts
> typeOfApp argno _           _ = typeError $ 
>               "Function applied to too many arguments ("++show argno++")."


\subsubsection{Type Check of the Grammar     }

To prepare the type check of the grammar with an algebra, 
we first have to build a starting environment.
This environment consists on the one hand of the basic starting environment 
|env1| of type synonyms and type declarations of external functions.
On the other hand it contains the list of types of the algebra functions 
we want to check with the grammar.
These functions |f| have to be renamed according to the names |g| used 
in the grammar, before they are put into the current environment along with 
|env1| and we type check the grammar.

> prepGEnv :: TCGrammar -> Env -> (String, Env) -> TCMonad Env
> prepGEnv gramm@(TCGrammar ln n alg fs cs) env1 (aname,algFuns) =  do
>         appendTrace $ "\nChecking the grammar with algebra "++show aname
>         if (length algFuns == length alg)
>           then do
>             let algFuns' = zipWith rename algFuns alg
>             updateEnv (env1 ++ algFuns') (tcGrammar gramm)
>           else do
>             typeError $ show (length algFuns)++" functions in algebra,\n"
>                       ++show (length alg)++" functions in grammar tuple."
>   `errorIn` ("usage of the grammar with algebra "++(show aname))
>  where
>   rename (f:>:t,l) g = (g:>:t,l)

To type check the grammar, we have to derive the types of all nonterminal 
symbols.

All user defined combinators must have the same type as |~~~|.
We look up this type in the environment, augment the combinators with their 
new type and add them to the environment.

Productions are quite often recursive.
Nonterminals may appear on the right-hand side in productions.
We need to know the types of these nonterminals on the right-hand side, 
if we want to type check a production, but that is just where they are defined.

To overcome this problem, we use fixed point iteration (as explained in 
section \ref{fixit}).
First, the types of all nonterminals are initialised with the preliminary type
|Any|.
We carry along a list of nonterminals which have no proper type yet.
This list is initialised with all nonterminals, for we just assigned type |Any| 
to each of them.

With these values we start a fixed point iteration to derive the types of 
the list of nonterminals.
We also give contextual information about entering the grammar again.

> tcGrammar :: TCGrammar -> TCMonad Env
> tcGrammar (TCGrammar ln g alg fs combs) = do
>         (t,l) <-lookupVar "~~~"
>         let combs' = map (\(v, ln) -> (v:>:t,ln)) combs
>         let ts         = initGFunDefs fs
>         let untypedNTs = [f| (f:>:t,ln)<-ts]
>         appendTrace $ "  Checking "
>                     ++numerus (length untypedNTs) "nonterminal"++" \n   "
>                     ++ ((concatWith ", \n   ").(map show)) untypedNTs++"\n"
>         updateEnv (ts++combs') (fixIt ts fs 0)
>   `inGr` (g,ln)


\paragraph{Typecheck of Grammar Function Definitions -- Fixed Point Iteration  }

This function initialises the nonterminals with type |Any| for the fixed 
point iteration.

> initGFunDefs :: [TCGFunDef] -> Env
> initGFunDefs ds = map initGDef ds
>  where 
>     initGDef :: TCGFunDef -> TypedVar1
>     initGDef (TCGFunDef ln f e) = (f:>:Any,ln)

The function |fixIt| implements a fixed point iteration over the types 
of the grammar nonterminals. 

In every iteration loop the new types of nonterminals are computed
under the assumption that the (preliminary) old types are true.
Then we compare the computed types to the preliminary ones.
If both types are the same, we return that type.
Otherwise we call the function again, using the newly computed type 
as preliminary type and with an updated list of untyped nonterminal symbols.

The computation of the types never extends the set of possible types for 
a nonterminal. The set is reduced or keeps its cardinality.
Therefore, we will eventually reach the smallest fixed point. 
This is the most particular type we may derive for a nonterminal, 
with the given amount of information about it.
Nonterminals with an unresolved type are reported (error 
\ref{LocSim.Grammar.NonterminalsHaveUnresolvedType.adp}).

> fixIt :: Env -> [TCGFunDef] -> Int -> TCMonad Env
> fixIt tds fs i = do
>       tds' <- tcGrammarFunDefs fs
>       let preliminaryNTs   = [f| (f :>:t ,ln )<-tds, 
>                                  (f':>:t',ln')<-tds', ((f==f')&&(t/=t'))]
>       case preliminaryNTs of
>         (d:ds) -> do
>           updateEnv tds' (fixIt tds' fs (i+1))
>         []     -> do
>           appendTrace $ "Derived types of nonterminals in "
>                         ++show i++" iterations."
>           let unresolved = [ f | (f:>:Any,ln) <- tds']
>           case unresolved of
>             (a:as) -> typeError $ "Some nonterminals"
>                                 ++" have an unresolved type: "
>                                 ++((concatWith ", ").(map show)) unresolved
>                                 ++"."
>             []     -> return tds

Now we describe the type check of grammar functions.
A grammar function may define a new nonterminal, that is used in a subsequent 
grammar function. Therefore we add each newly typed function to 
the environment immediately. 
Apart from this, we just type check each grammar function in the list.

> tcGrammarFunDefs :: [TCGFunDef] -> TCMonad Env
> tcGrammarFunDefs []      = return []
> tcGrammarFunDefs (d:ds)  =
>   do td  <- tcGrammarFunDef d
>      tds <- updateEnv [td] (tcGrammarFunDefs ds)
>      return  (td:tds)

\paragraph{Type Check of a Grammar Function Definition  }

To type check a grammar function definition, we use an approach that is 
similar to the check of an algebra function.
A difference is the lack of parameters in a grammar function.

We just look up the function type in the environment 
and add this information to the environment of the monad in which we
 compute the type of the right-hand side.

The expected result type is compared to the derived result type.
In the last line of this function we have some context information again,
which is used in case of an error.

> tcGrammarFunDef :: TCGFunDef -> TCMonad TypedVar1
> tcGrammarFunDef (TCGFunDef lnf f e) =
>     do (tf,lnt) <- lookupVar f
>        te       <- tcRhs "Grammar" lnf [] e
>        (t',ln)  <- (tf,lnt) |^| (te,lnf) `errorIn` ("result type ")
>        return (f:>:t',lnf)
>  `inGFunDef` (f,lnf)


\subsubsection{ADP-Specific Grammar Expression Check}

In a grammar production, the expressions on the right-hand side 
include many applications of combinators.
Although combinator applications can be checked like any other function
application, an additional check is introduced here.

The intuitional view of an ADP programmer may differ a bit from the 
way of implementation here. The right-hand side of a grammar function
|g = a <<< b ~~~ c ~~~ d|
will simply be seen by the programmer as an application of function |a|
to three arguments |b|, |c|, |d|.

The uses-combinator has a higher precedence than the next-combinator,
which means, that an expression may be structured with nested parentheses 
like this:
|((a <<< b) ~~~ c) ~~~ d)|.

We have shown a problematic error in the introduction:
What would happen if an algebra function was defined to have four arguments,
but it was only given three?
If we reported this error like in any other function application, 
we would get a useless message 
similar to the errors {\scshape GHC} gave us in 
the Haskell implementation of ADP in section \ref{ghcerror}.
The precedence of the combinators leads to these different views. 

Consider another example. 

Imagine we used an expression of a wrong type \emph{``in the last argument of 
an algebra function''}.
This is our abstract wiew. 

In contrast to this, a type check program that treats combinators like other 
functions, recognizes \emph{``a problem in the outermost combinator 
application''}. 
The type checker has no information 
about the algebra function which is nested somewhere in the left expression 
of the outermost combinator application. 
Therefore it can not generate a useful error message.

To overcome these problems, we will introduce a function takes the
``intuitional view'' of a grammar production into account.
Our grammar check function has special cases for the ADP combinators.
We start the check with an empty list which will be used to store future 
algebra function parameters.

> precheckGrammarExp :: LineNumber -> TCExp -> TCMonad ()
> precheckGrammarExp ln e = precheckGrammarExp' ln [] e

\paragraph{\emph{Next} -- Collect Arguments for Algebra Function }

If we encounter the |~~~| combinator in the outermost expression of the 
above example,
we have to collect the expression |d| on its right side as an argument for
some algebra function that we expect to be nested in the left expression.
In this way, we collect a list of arguments |[b,c,d]| for the algebra function |a|.
If we do not find any uses-combinator in the left expression, 
we can report an error 
(\ref{LocSim.Grammar.NextCombinatorWithoutUsingCombinator.adp}). 
The function |out| is applied to each of the
collected expressions, in order to extract the input from the parsers,
which would otherwise be done implicitly by the parser combinators.


> precheckGrammarExp' :: LineNumber -> [TCExp] -> TCExp -> TCMonad ()
> precheckGrammarExp' ln args (TCApp "~~~" [l,r]) = case (l,r) of
>    (e1,   e2@(TCApp f' es')) -> do
>                       precheckGrammarExp  ln e2
>                       appendTrace $ "Collected argument " ++ show e2
>                       precheckGrammarExp' ln ((TCApp "out" [e2]):args) e1
>    (e1@(TCApp f es),   e2  ) -> do
>                       appendTrace $ "Collected argument " ++ show e2
>                       precheckGrammarExp' ln ((TCApp "out" [e2]):args) e1
>    (e1,      e2) -> typeError $ "Usage of next (~~~) without uses (<<<)."

\paragraph{\emph{Uses} -- Check of Algebra Function }

If we do, we can test if the arguments fit to the algebra function
(error \ref{LocSim.Grammar.AlgebraFunctionWrongArity.adp}).
Of course we must also have an algebra function at the left of 
|<<<| 
(error \ref{LocSim.Grammar.UsingCombinatorWithoutAlgebraFunction.adp}).

> precheckGrammarExp' ln args (TCApp "<<<" [l,r]) =  case (l,r) of
>    (TCVar algfun,e2) -> do
>          (tf,lnDef)   <- lookupVar algfun
>          let tfList    = typeToList tf
>          if (length (e2:args) /= (length tfList)-1)
>            then typeError $ m algfun tfList e2 
>            else do
>                 appendTrace $ "Algebra function "++show algfun 
>                             ++" used with" ++ show (e2:args) 
>                             ++" (correct arity).\n"
>                 (tcApp "Grammar" ln algfun ((TCApp "out" [e2]):args)) 
>                                                `errorIn` "algebra function"
>                 done
>    (noalgfun, e2) -> typeError $ "no algebra function "
>                                ++"as first parameter of uses (<<<)."

>  where 
>    m f ts e2 = "Algebra function used with"
>              ++" wrong number of arguments:\n "++show  f
>              ++" ("++numerus ((length  ts)-1) "argument"++") "
>              ++"used with: "
>              ++concatWith ", " (map prettyPrint ((TCApp "out" [e2]):args))
>              ++" ("++numerus (length (e2:args)) "argument"++")."

\paragraph{\emph{Alternative} -- Check Both Alternatives }

An alternative-combinator @(|||)@ in the outermost expression requires that  
each of the alternatives are checked.

> precheckGrammarExp' ln args (TCApp "|||" es) = do
>                               mapM (precheckGrammarExp ln) es
>                               done

\paragraph{\emph{Select} -- Check Left Expression and Objective }

A select combinator |(...)| does not change the type, 
so we can just go on checking its left argument.
Before that, we check if an objective function is at the right 
side of the combinator.

> precheckGrammarExp' ln args (TCApp "..." [l,(TCVar r)]) = do
>                 r' <- lookupVar r
>                 (tObj,ln) |^| r' `errorIn` "type of objective function"
>                 precheckGrammarExp' ln args l
>  where tObj = ((TList Any) :-> (TList Any))

All other function applications are just checked in the ordinary way.

> precheckGrammarExp' ln [] (TCApp f es) = do
>     (tf,lnDef)   <- lookupVar f
>     let tfList    = typeToList tf
>     if (length es /= (length tfList)-1)
>            then typeError $ m tfList
>            else do 
>                 appendTrace $ show f ++ " used with" ++ show es 
>                                      ++" (correct arity)."
>                 tcApp "Grammar" ln f es
>                 done
>  where 
>   m ts = "Function used with wrong number of arguments:\n " 
>            ++show  f     ++" ("++numerus ((length ts)-1) "argument"
>            ++") used with "++concatWith ", "(map prettyPrint es)
>            ++" ("++numerus (length es) "argument"++")."


Other expressions are not checked, because they will be found in the usual
expression type check.

> precheckGrammarExp' ln args e = done

With the functions we gave in this chapter,
we presented a complete type check algorithm.
