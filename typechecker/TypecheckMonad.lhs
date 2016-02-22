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

> module TypecheckMonad where
> import ParseTree
> import TypecheckTree
> import List
> import Tools
> import Char
> import Monad

%endif


\subsection{The Type Check Monad: a Combined Monad}

The type check monad is a combined monad. It is a hybrid between three 
standard monads: a state monad, a reader monad and an 
exception monad \cite{newbern:monads}. 

The \emph{state monad} part stores collected information in the state. The state 
consists of three lists of strings. One list for trace information, one list for 
warnings, and one list of strings for error messages.

The \emph{reader monad} part is used for the organisation of known variables and 
scopes. Every time we want to run the monad, we have to provide an 
environment of known variables. Monad functions can access and modify the 
environment. So we can look up a variable to see if it is in the current scope.
The environment is only needed during computation and not returned.

The \emph{exception monad} part is responsible for the error handling. 
Type errors interrupt the computation.
While working their way up to the main monad, 
they are augmented with information about the context in which 
the error was found.


\subsubsection{The Monad    }

Our monad carries a state. This state is a collection of data
that is gained during computation.
We want to collect trace information, warnings, and errors.
Each of these is stored in a list of strings. 
The state is defined as a triple of these three lists.

> type State = ([String],[String],[String])

\subsubsection*{The Monadic Type    }
The three parts of the type check monad are also reflected in its type.
Because of its state monad property, it gets a state and returns a state.\\
Besides the state,
it gets an environment and returns some result.
That is because of its reader monad property.\\
In fact the result has an |Either|\/-type. It can be either an error or 
some computed return value.
The reason for this is the exception monad property.
By convention, the left alternative holds the error 
and the right one holds the right answer.

> data TCMonad a = TCMonad ((Env,State) -> (Either Error a,State))

The definition of the |TCMonad| shows how these properties are 
implemented.

> instance Monad TCMonad where

As we have seen, a monad is defined by defining its data type, 
return function and bind operator.

Here, the return function returns its argument value |x|, 
paired with the current state of the monad.
The |Right|\/-constructor is applied to the value |x|, 
because it is a computed result value (right of the |Either|\/-alternatives) 
and not an error (left of the |Either|\/-alternatives).

>    return x           =  TCMonad (\(e,s) -> (Right x,s))

The internal computation of the monad is defined by the bind operator,
which has type \\
|(>>=) :: Monad m => m a -> (a -> m b) -> m b|.
So it takes a monad |m| containing some type |a|,
and a function that transforms values of just type |a| 
to monads |a| containing values of some other type |b|.
It applies this function to the monad and the result is a value of type |b|,
which is wrapped in a monad |a| again.

Our bind operator gets a monad |tc| and a function |xtc|.
We want to return a |TCMonad|, which must also have the defined type.
We define the internal function of the monad anonymously 
with a lambda abstraction, which is common practice. 
It takes an environment and a state, and returns either an error or a
computed value of some type |a|. 

>    TCMonad tc>>=xtc =
>       TCMonad $ \ (env,s) -> case tc (env,s) of
>           (Left msg ,s') -> (Left msg, s')
>           (Right x  ,s') -> let TCMonad tc2 = xtc x
>                             in tc2 (env,s')

The |Either|-return type gives rise to a separation into two cases.
We run the monad |tc| (which was the first argument of the bind operator)
with the environment and state that were input to our anonymous function.
As a result we get either an error paired with a state 
or some return value paired with a state.
In case of an error, we do not apply the function |xtc|, 
but just propagate the error.
In case of a proper result, we can apply the function |xtc| to the result.
This case is the usual way sequential execution is implemented in a monad.

In both cases the state is just passed on. 
It holds our information during the computation.

The environment is always passed on to the next computation step.
It is always ``available'' during computation.
As we have seen before, it is not returned, because its not needed later 
(unless the argument |x| of the return function is an environment, 
since the result of computation is an environment).


The function |run| executes a computation in the monad 
and has a non-monadic result.

> run :: TCMonad a -> (Env, State) -> (Either Error a, State)
> run (TCMonad tc) = tc



\subsubsection{Collect Information in the State    }

The state is used to collect the trace, warnings, and errors in lists of 
strings. When we start the computation in the monad, we need an initial state.
We start with empty lists for trace, warnings, and errors:                    \\

> state0 :: State
> state0 = ([],[],[])

%if False                                                         \\

> readState                  :: TCMonad State
> readState                  =  TCMonad (\(e,s) -> (Right s,s))

%endif


The function |updateState| updates the state of the monad.
It is used to append some information to one of the lists.

> updateState                :: (State -> State) -> TCMonad ()
> updateState f              =  TCMonad (\(e,s) -> (Right (), f s)) 


Information may be added to each of the lists in the state during computation.
We just want to add information, we never remove it. The 
|append|-functions below allow the addition of strings to each of the lists.

At first we can append a string to the trace in the state triple. 
This monadic function returns nothing (Type |()|).

> appendTrace :: String -> TCMonad()
> appendTrace t = updateState app
>    where
>      app (trace, w, e) = (trace ++ [t], w, e)

The functions |appendWarning| and |appendError| are defined accordingly.


%if False

We can also append a string to the warnings in the state triple.

> appendWarning :: String -> TCMonad()
> appendWarning w = updateState app
>    where
>      app (t, warnings, e) = (t, warnings ++ [w], e)

The third case is to append a string to the errors in the state triple.

> appendError :: String -> TCMonad()
> appendError e = updateState app
>    where
>      app (t, w, errors) = (t, w, errors ++ [e])

%endif


\subsubsection{Alter and Use the Type Environment    }

The monad also provides the current environment of variables with their 
associated types. We define some functions to use and modify this environment.

As for the state, we have to provide an initial environment.
No types of variables are known before the type checker has read the 
ADP program.

> env0 :: Env
> env0 = []


%if False

We need a function which puts some information into the current environment. 
It extends the current environment by appending an additional list of typed 
variables (i.e. an environment again) to it.

> extendEnv :: Env -> TCMonad a -> TCMonad a
> extendEnv env' (TCMonad tc) = TCMonad $ \ (env,s) -> tc ((env'++env),s)

To replace a single variable in the current environment,
we define the following function.

> replaceVar :: TypedVar1 -> TCMonad a -> TCMonad a
> replaceVar ((v:>:t),ln) (TCMonad tc) = TCMonad $ \ (env,s) -> tc ((rep env),s)
>    where rep env = [(f:>:t,ln') | (f:>:t,ln')<-env, f/=v]++[(v:>:t,ln)]

%endif

Our environment should not remain empty.
To insert or replace a list of variables in the current environment,
we define the following function.

> updateEnv :: Env -> TCMonad a -> TCMonad a
> updateEnv assumps (TCMonad tc) = TCMonad $ \ (env,s) -> tc ((rep env),s)
>    where rep env = [(f:>:t,ln) | (f:>:t,ln)<-env, f `notElem` ids]++assumps
>          ids     = [f | (f:>:t,ln)<-assumps]

The function |nubEnv|\footnote{nub means ``essence'', and our function is defined
according to |nub| from Haskell's @List@ module} removes duplicates from a list of 
variable bindings and checks if the right-hand sides fit 
for different bindings to the same variable.

> nubEnv :: Env -> TCMonad Env
> nubEnv env =  case replaced of
>          [] -> return $ newEnv
>          _  -> typeError $ concatMap (\a -> "Variable defined already: \n"
>                                            ++(unlines . map show) a) ids
>   where env'  = nubBy (comparing fst) (nub env)
>         replaced = (nub env) \\ env'
>         ids = [ g | g <- groupBy (comparing varname) (sort env), 
>                                                     (length g) > 1 ]
>         newEnv = [ head g | g <- groupBy (comparing varname) (sort env) ]
>         comparing :: (Eq a) => (b -> a) -> b -> b -> Bool
>         comparing p x y = (p x) == (p y)
>         varname (a:>:t,ln) = a

%if False

Sometimes we may want to remove a variable from the current environment.

> removeVar :: Name -> TCMonad a -> TCMonad a
> removeVar id (TCMonad tc) = TCMonad $ \ (env,s) -> tc ((rem env),s)
>    where rem env = [(f:>:t,ln) | (f:>:t,ln)<-env, f/=id]

%endif

If we merge two lists of variable bindings,
there should still be only one binding per variable in the resulting list.
Similar bindings for a variable can be reduced to one binding,
and different bindings for the same variable will be reported as an error.
We could also use |++| and |nubEnv| to achieve this, but this function allows
better error diagnostics.

> addEnvs :: Env -> Env -> TCMonad Env
> addEnvs e1 e2 =  case replaced1 of
>          [] -> return (e1' ++ e2)
>          _  -> typeError $ "Variable defined one time "++show replaced1
>                          ++" and then "++show replaced2++" in environment."
>   where ids1 = [v | (v:>:t,ln) <- e1]
>         ids2 = [v | (v:>:t,ln) <- e2]
>         e1'  = [(f:>:t,ln) | (f:>:t,ln) <- e1, f `notElem` ids2]
>         e2'  = [(f:>:t,ln) | (f:>:t,ln) <- e2, f `notElem` ids1]
>         replaced1 = e1 \\ e1'
>         replaced2 = e2 \\ e1'

%if False

We provide the following function to show the current environment:

 > showEnv :: TCMonad b
 > showEnv = TCMonad $ \ (env, s) -> (Left (Error ((unlines . map show) env)), s)

%endif

The environment is used mainly to look up the types of variables in it.
This function looks up a variable and returns its type along with
the number of the line where the last piece of information about this type is 
located in the input program.
An exception (|typeError|) is raised if the variable is not found on look-up.
The function |typeError| is explained below.

> lookupVar :: Name -> TCMonad (Type,LineNumber)
> lookupVar x = TCMonad $ \ (env, s) ->
>   case [(t,ln)|(y:>:t,ln)<-env,y==x] of
>     t:t':ts -> (Left (Error (x++" defined multiple times")), s)
>     [t]     -> (Right t, s)
>     []      -> (Left (Error (x++" not in scope.")), s)


If we just want to peek if the variable has a known type in the current 
environment, we can use this function. 
It returns |True| if the type is defined, |False| otherwise,
and does \emph{not raise any exceptions}.

> knownVar :: Name -> TCMonad Bool
> knownVar x = TCMonad $ \ (env, s) ->
>   case [t | (y:>:t, ln) <- env, y==x] of
>     t:_ -> (Right True, s)
>     []  -> (Right False, s)


\subsubsection{Type Comparison    }

First, we define an operator for type comparison.

> infix  4 =:=

This operator compares two types and raises an exception if they are different.

> (=:=)      :: Type -> Type -> TCMonad ()
> t1 =:= t2 = do tEq <- t1 `tEq` t2
>                if (tEq)
>                  then done
>                  else typeError (mismatch t1 t2)
>   where

The comparison is done by the function |tEq|.
We can not just use the |==|-operator of Haskell, 
because different variable names or synonym names 
should not affect the comparison of types.

>     tEq :: Type -> Type -> TCMonad Bool
>     tEq (TSyn (f:>:a))              b = tEq a b
>     tEq a              (TSyn (f:>:b)) = tEq a b
>     tEq (TVar (f:>:a))              b = tEq a b
>     tEq a              (TVar (f:>:b)) = tEq a b
>     tEq (TList a)           (TList b) = tEq a b
>     tEq (TyParser a)     (TyParser b) = tEq a b
>     tEq (TTuple (a:as)) (TTuple (b:bs)) = do 
>                             tEq1 <- tEq a b
>                             tEq2 <- tEq (TTuple as) (TTuple bs)
>                             return $ tEq1 && tEq2
>     tEq (ta1 :-> ta2) (tb1 :-> tb2) = do 
>                             tEq1 <- tEq ta1 tb1
>                             tEq2 <- tEq ta2 tb2
>                             return $ tEq1 && tEq2
>     tEq a b  
>        | (a==(TCon "Int"    ) && b==(TCon "Double")) = do 
>                        return True
>        | (a==(TCon "Double") && b==(TCon "Int"    )) = do 
>                        return True
>        | (a==(TCon "Integer"    ) && b==(TCon "Double")) = do 
>                        return True
>        | (a==(TCon "Double") && b==(TCon "Integer"    )) = do 
>                        return True
>        | (a==(TCon "Int"    ) && b==(TCon "Integer")) = do 
>                        appendWarning "Int expected, Integer found."
>                        return True
>        | (a==(TCon "Integer") && b==(TCon "Int"    )) = do 
>                        appendWarning "Integer expected, Int found."
>                        return True
>        | otherwise = return $ (a==b)

\subsubsection*{Type Intersection    }\label{intersectionop}

Our simplified version of unification is based on the intersection of types.
We define the type-intersection-operator here, as introduced 
in section \ref{intersectionoperator}. 
Exceptions and warnings are generated on empty intersections.

> (|^|) :: (Type,LineNumber) -> (Type,LineNumber) -> TCMonad (Type,LineNumber)
>
> (tDef,lnDef) |^| (tUse,lnUse) = case (tDef,tUse) of 
>     ((TSyn (f:>:a)),              b) -> (a,lnDef) |^| (b,lnUse)
>     (a ,             (TSyn (f:>:b))) -> (a,lnDef) |^| (b,lnUse)
>     ((TVar (f:>:a)),              b) -> (a,lnDef) |^| (b,lnUse)
>     (a             , (TVar (f:>:b))) -> (a,lnDef) |^| (b,lnUse)
>     ((TList a)     ,      (TList b)) -> do 
>             (t,ln)           <- (a,lnDef) |^| (b,lnUse)
>             return ((TList t),ln)
>     ((TyParser a)  ,   (TyParser b)) -> do 
>             (t,ln)           <- (a,lnDef) |^| (b,lnUse)
>             return ((TyParser t),ln)
>     ((TTuple (a:as)), (TTuple (b:bs))) -> do 
>             (t,ln)            <- (a,lnDef) |^| (b,lnUse)
>             ((TTuple ts),ln') <- ((TTuple as),lnDef) |^| ((TTuple bs),lnUse)
>             return ((TTuple (t:ts)),ln)
>     ((ta1 :-> ta2), (tb1 :-> tb2)) -> do 
>             (t1,ln ) <- (ta1,lnDef) |^| (tb1,lnUse)
>             (t2,ln') <- (ta2,lnDef) |^| (tb2,lnUse)
>             return ((t1 :-> t2),ln)
>     (a, Any  ) -> return (a,lnDef)
>     (Any, b  ) -> return (b,lnUse)
>     (a, b)     ->  
>        if      (a==b)          then return (a,lnDef)
>        else if (a `inClass` b) then return (a,lnDef)
>        else if (b `inClass` a) then return (b,lnUse) 
>        else if (a==(TCon "Int"    ) && b==(TCon "Double")) then return ((TCon "Double"), lnUse)
>        else if (a==(TCon "Double" ) && b==(TCon "Int"   )) then return ((TCon "Double"), lnUse)
>        else if (a==(TCon "Integer") && b==(TCon "Double")) then return ((TCon "Double"), lnUse)
>        else if (a==(TCon "Double" ) && b==(TCon "Integer")) then return ((TCon "Double"), lnUse)
>        else if (a==(TCon "Int"    ) && b==(TCon "Integer")) then do 
>                                  appendWarning "Int expected, Integer found."
>                                  return ((TCon "Int"), lnUse)
>        else if (a==(TCon "Integer") && b==(TCon "Int"    )) then do 
>                                  appendWarning "Integer expected, Int found."
>                                  return ((TCon "Int"), lnUse)
>        else typeError $ emptyIntersection (tDef,lnDef) (tUse,lnUse)
>   where

As explained in \ref{theorie:polyadp},
ADP does not support the full polymorphism that Haskell supports. 
Type classes in our case do not provide special instance functions for their 
members. They can just be used to restrict some polymorphic functions to 
expressions of a certain type.
The test if a type |t| is a member of type class |c| is done by the function
|inClass|.

>     inClass :: Type -> Type -> Bool
>     inClass (TCon t) (TCon c) = (t `elem` members)
>        where 
>            members = head' [memberTs| (tClass,memberTs)<-instances, 
>                                       tClass==c]
>            instances = [("Num",      ["Int", "Integer", "Float", "Double"]),
>                         ("Integral", ["Int", "Integer"]),
>                         ("Floating", ["Float", "Double"])]
>            head' [] = []
>            head' xs = head xs
>     inClass _ _ = False



\subsubsection{Error Handling    }

%if False

> data Error = Error String | FunContext String Error 
>                           | CombContext String Error
> instance Show Error
>    where show (Error s)      =  s
>          show (FunContext s e) =  s++show e
>          show (CombContext s (CombContext s' e)) = show (CombContext s' e)
>          show (CombContext s e) = s++show e

%endif

A |typeError| can be compared to raising an exception.
This function gets an error message string and returns a new |TCMonad|\/ 
instance.
As we see, an error is generated from the error message which was given to 
this function.
This error is passed on in the monad as the |Left| alternative of the 
|Either| type.
The internal computation of the monad treats the |Left| alternative
like an exception. 
It is passed (``thrown'') to the next surrounding monadic 
computation and finally returned by the outermost one.

> typeError :: String -> TCMonad b
> typeError msg = TCMonad $ \ (env,s) -> (Left (Error (msg)),s)


On an empty intersection, this function is used to generate the error message. 
If the expected type is a function type, the mismatch is probably from 
an algebra function definition or a function application with not 
enough arguments.

> emptyIntersection :: (Type,LineNumber) -> (Type,LineNumber) -> String
> emptyIntersection (tDef,lnDef) (tUse,lnUse) = case tDef of
>                    (t:->t') -> m ++"\n Too many arguments."
>                    t        -> case tUse of
>                                     (t:->t') -> m ++"\n Too few arguments."
>                                     _        -> m
>  where m = " Type was declared "++prettyPrint tDef++" in "++show lnDef
>          ++",\n but used "++prettyPrint tUse++" in "++show lnUse++"."


If two types are not equal, we generate an error message as well.

> mismatch :: Type -> Type -> String
> mismatch tdef tuse = " Type "++prettyPrint tdef++" expected. \n"
>                    ++" But "++prettyPrint tuse++" used."


\subsubsection*{Error Context Information    }

The sequential computation in a monad makes it possible to add context
information to the error messages. 
We define an operator for the addition of context information.

> infixr 1 `errorIn`

To add this context information to error messages only, 
we take advantage of the two cases of the |Either| return type of the
 type check monad again.

This function gets a type check monad |tc| and a string as input.
A new instance of |TCMonad| is wrapped around the monad |tc|.
If the monad |tc| returns an error (|Left| return type of the |Either| 
alternatives), the new monad augments this error with context information.
If the monad returns a normal return type (|Right| return type in the either 
alternative), no context information is added.
This way we can specify a context for error messages of exceptions 
``passing by''.
We treat the context of a standard combinator application differently,
because combinator applications are often deeply nested, which leads to
verbose context messages.

> errorIn :: TCMonad a -> String -> TCMonad a
> TCMonad tc `errorIn` context = if (context `elem` ["<<<","~~~","|||","..."])
>  then
>    TCMonad $ \ (env,s) -> case tc (env,s) of
>      (Left msg,s') -> (Left (CombContext ("In "++context++":\n") msg),s')
>      (Right x ,s') -> (Right x, s')
>  else
>    TCMonad $ \ (env,s) -> case tc (env,s) of
>      (Left msg,s') -> (Left (FunContext ("In "++context++":\n") msg),s')
>      (Right x ,s') -> (Right x, s')

We also have several variations of the above function that already include a 
specified context. This example denotes an error in an algebra context:

> inAlg :: TCMonad a -> (Name,LineNumber) -> TCMonad a
> tc `inAlg` (a,ln) = tc `errorIn` ("algebra "++show a++" "++show ln)

We defined similar functions for other error contexts:
|inAFunDef|, |inParam|, |inTuple|, |inGr| to name a few.

%if False
Several functions to specify contexts for error messages of exceptions 
passing by. \\

\subsubsection{Algebra Context    }

> tc `inAFunDef` (f,ln) = tc `errorIn` ("definition of algebra function "
>                                      ++show f++" "++show ln)
> tc `inParam`   f = tc `errorIn` ("parameter "++show f)
> tc `inTuple`   f = tc `errorIn` ("tuple "++show f)
> tc `inList`    f = tc `errorIn` ("list "++show f)

\subsubsection{Grammar Context    }

> tc `inGr` (g,ln) = tc `errorIn` ("grammar "++show g++" "++show ln)
> tc `inGFunDef`(f,ln) = tc `errorIn` ("definition of grammar nonterminal "
>                                      ++show f++" "++show ln)

> tc `inAppG` (e,"<<<") = tc `errorIn` (show e++" "++"<<<")
> tc `inAppG` (e,"~~~") = tc `errorIn` ("~~~")
> tc `inAppG` (e,"|||") = tc `errorIn` ("|||")
> tc `inAppG` (e,"...") = tc `errorIn` ("...")
> tc `inAppG` (e,"out") = tc 
> tc `inAppG` (e, f   ) = tc `errorIn` ("an application of "++show f)

\subsubsection{Context in Both    }

> tc `inApp` (context,(e,f)) = case context of
>      "Algebra" -> tc `errorIn` ("an application of "++show f)
>      "Grammar" -> tc `inAppG` (e,f)
>      x         -> typeError $ "Typecheck.tcExp"++show x
>                             ++" is not a valid context"

> tc `inArg` (e,"<<<") = tc
> tc `inArg` (e,"~~~") = tc
> tc `inArg` (e,"|||") = tc
> tc `inArg` (e,"...") = tc
> tc `inArg` (e, f   ) = tc

%endif


\subsubsection{Substitutions of Variables in Types    }

This function corresponds to the |apply| function for substitutions
in Mark P. Jones' \cite{jones:thih} approach.
It replaces all occurrences of variables from a list of type variables 
in a type.

> substituteVars :: Env -> Type -> Type
> substituteVars subst ty = case ty of
>     TCon id        -> TCon id
>     Any            -> Any
>     TSyn (s:>:Any) -> TSyn (s:>:Any)
>     TSyn (s:>:t)   -> TSyn (s:>:(substituteVars subst t))
>     TVar (v:>:Any) -> TVar (v:>:(lookUp v subst))
>     TVar (v:>:t)   -> TVar (v:>:(substituteVars subst t))
>     TList t        -> TList (substituteVars subst t)
>     TTuple ts      -> TTuple ((map (substituteVars subst)) ts)
>     TyParser t     -> TyParser (substituteVars subst t)
>     t1 :-> t2      -> (substituteVars subst t1) :-> (substituteVars subst t2)
>     ClassConstraint tv t -> ClassConstraint tv (substituteVars subst t)
>     x              -> error $ "TypecheckMonad.substituteVars"
>  where 

In a similar way to the lookup function provided by the type check monad, 
this function looks up the type of a variable, here in a list of typed 
variables that should be substituted in a type.
If no substitution for that variable was found in the list, 
we return the type |Any|, because we have no information to further specify 
the type.

>   lookUp :: Name -> Env -> Type
>   lookUp x subst = case [ t | (y:>:t,ln) <- subst, y==x] of
>      t:t':ts -> error $ "TypecheckMonad.substituteVars: "++x
>                           ++" found multiple times in subst"
>      [t]     -> t
>      []      -> Any

As opposed to the function |substituteVars|, which substitutes a variable 
if that is possible,
we introduce a new function which expands \emph{all unexpanded variables} 
This expansion is mandatory. If a variable can not be expanded,
we throw an exception.

> expandVars :: Env -> Type -> TCMonad Type
> expandVars = expand "Var"

The same function is defined for type synonyms, because an ADP program must
provide a definition for each type synonym.

> expandSyns :: Env -> Type -> TCMonad Type
> expandSyns = expand "Syn"

The expansion is done here.

> expand :: String -> Env -> Type -> TCMonad Type
> expand what env typ = case typ of
>     TCon id        -> return $ TCon id
>     Any            -> return Any
>     TSyn (s:>:Any) -> case what of
>          "Var"->   return $ TSyn (s:>:Any)
>          "Syn"-> do 
>                    t  <- lookUp s env
>                    t' <- expand what env t
>                    return $ TSyn (s:>:t')
>     TSyn (s:>:t)   -> do 
>                    t' <- expand what env t
>                    return $ TSyn (s:>:t')
>     TVar (v:>:Any) -> case what of
>          "Var"-> do
>                    t  <- lookUp v env
>                    t' <- expand what env t
>                    return $ TVar (v:>:t')
>          "Syn"->   return $ TVar (v:>:Any)
>     TVar (v:>:t)   -> do
>                    t' <- expand what env t
>                    return $ TVar (v:>:t')
>     TList t        -> do
>                    t' <- expand what env t
>                    return $ TList t'
>     TTuple ts      -> do
>                    ts' <- (mapM (expand what env)) ts
>                    return $ TTuple ts'
>     TyParser t     -> do
>                    t' <- expand what env t
>                    return $ TyParser t'
>     t1 :-> t2      -> do
>                    t1' <- expand what env t1
>                    t2' <- expand what env t2
>                    return $ t1' :-> t2'
>     ClassConstraint tv' t -> do
>                    t' <- expand what env t
>                    return $ ClassConstraint tv' t'
>     x              -> return $ error $ "TypecheckMonad.expand"
>  where 

This function also looks up the type of a variable in the provided 
environment of typed variables, which we want to substitute.
If the variable is not found, we raise an exception, 
because the designated identifier is not defined.

>   lookUp :: Name -> Env -> TCMonad Type
>   lookUp x env = case [ t | (y:>:t,ln) <- env, y==x] of
>      t:t':ts -> typeError $ "TypecheckMonad.expand: "++x
>                           ++" found multiple times in environment"
>      [TVar (v:>:t)]     -> if (v==x) then return t else return (TVar (v:>:t))
>      [TSyn (v:>:t)]     -> if (v==x) then return t else return (TSyn (v:>:t))
>      [t]     -> return t
>      []      -> if isLower (head x) 
>                           then typeError $ "Could not expand type variable "
>                                          ++show x++"."
>                           else typeError $ "Could not expand type synonym " 
>                                          ++show x++"."

This helper function makes sure that a type is specified. 
It should be a known type, and not the unknown type |Any|.

> hasType :: Type -> TCMonad ()
> hasType Any = typeError " Type not specified, type Any found"
> hasType t   = done

%if False
This helper function is used to make the error messages more readable.
It decides if a word appears in singular or plural and adds an s 
as a plural ending if necessary.

> numerus :: Int -> String -> String
> numerus 1 s = "1 " ++s
> numerus n s = show n ++" "++s++"s"

> numerus' :: Int -> String -> String
> numerus' 1 s = s
> numerus' _ s = s++"s"

%endif

%if False
\subsubsection{Compile Step Function    }

verbosity level:

> data VLevel = Target | Trace | TraceMore | Debug
>     deriving (Eq, Ord)

Append a title to the trace if we have a higher verbosity level.

> showTitle :: String -> VLevel -> TCMonad ()
> showTitle s vl
>    | vl == Target  = noOut
>    | otherwise     = appendTrace ("\n" ++ s ++ "\n" 
>                                ++ replicate ((length s) + 2) '-' ++ "\n")

> noOut :: TCMonad ()
> noOut = done

Compile some input and add a trace, if we have a higher verbosity level \\
Input: a (monadic) compile function
       some input to compile
       a title string
       a pretty printing function
       a verbosity level \\
Output: the result of the compile function application to the input \\

> compileStep :: (Show a) => (input -> TCMonad a) -> 
>                            input -> String -> 
>                            (a -> String) -> 
>                            VLevel -> TCMonad a
> compileStep compilefct x title pp vl = do
>    e <- compilefct x
>    do
>        if vl >= Trace then showTitle title vl                      else noOut
>        if vl >= Trace then appendTrace (pp e)                      else noOut
>        if vl >= Debug then showTitle "===> internal structure:" vl else noOut
>        if vl >= Debug then appendTrace ((show) e)                  else noOut
>    return e
>     where lit = ""

%endif
