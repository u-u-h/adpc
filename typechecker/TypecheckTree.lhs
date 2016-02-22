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
%if False

> module TypecheckTree  where
> import ParseTree
> import Tools

%endif

\subsection{Module Type Check Tree    }

Now we will introduce a new datatype, the type check tree.
We use this tree to store all information about functions and their types
in the same place. 

In an ADP program the algebra type is defined in a parameterised form.
The algebras make this abstract algebra type concrete by providing the 
parameters.
This way the type is specialised for that algebra.

Our approach is to fill in these parameters in the first step in order to get
a concrete algebra type. 
The concrete algebra type is split into the types of the algebra functions.
The type of an algebra function type is then further split into the types 
of single parameters and expressions.

These parameters and expressions are stored in pairs with their types in the 
new representation of the algebra.
The algebra type is not needed any more, because the type information is 
contained within each algebra.

\subsubsection{ADP Program    }

We use a simplified version of the parse tree.
It consists of an environment of typed variables,
a list of algebras, and a grammar.

In contrast to the parse tree, this tree does not store the algebra type,
because the types are stored with their functions inside the algebra.

> data TCADPProgram = TCADPProgram Env [TCAlgebra] TCGrammar

%if False

> instance Show TCADPProgram
>    where show (TCADPProgram e a g) = "TCADPProgram\n\n"
>                                    ++(unlines.(map show)) e ++ "\n\n" 
>                                    ++show a ++ "\n\n" 
>                                    ++ show g

%endif




\subsubsection{Algebra    }



The new data type that stores the algebra stores the type information as well.
The constructor of the data type |TCAlgebra| has a line number as its first argument.
This number refers to the line where the keyword |#algebra| was found.
The next arguments of the constructor are the name of the algebra, 
and a list of algebra function definitions.
The algebra function definitions also include type information.

> data TCAlgebra = TCAlgebra LineNumber Name [TCAFunDef]

%if False

> instance Show TCAlgebra
>    where show (TCAlgebra ln a defs) = "TCAlgebra "
>                                  ++show a ++ "\n\n"
>                                  ++(unlines.(map show)) defs

%endif

As seen above, we have to define a data type to store an algebra function.
The type constructor has two line numbers as the first two arguments. 
The first is the line number of the type definition of that algebra function, 
the second is the line number in which the function definition was found. 
The next arguments are the name of the algebra function, 
then a list of the parameters for this function, and a list of typed variables,
which are the variable bindings created by these parameters.
The single type is the return type of that function.
The right-hand side of the function, which is represented as an expression,
is the last argument.

> data TCAFunDef = TCAFunDef LineNumber LineNumber 
>                            Name [TCParam] [TypedVar] Type TCExp
>         deriving Show

The type check algorithm will check each function definition in 
each algebra.

An algebra function has a list of parameters.
A parameter is just a pair of an expression and its associated type.

> type TCParam = (TCExp, Type)




\subsubsection{Grammar     }



The grammar is represented similar to an algebra, 
except that it contains no information about the types, because it is not available.
The grammar has a line number of the line where the keyword |#grammar| 
was found, the name of the grammar,
and a list of grammar productions.
The last argument to the constructor are the names of user-defined combinators.

> data TCGrammar = TCGrammar LineNumber Name [Name] [TCGFunDef] 
>                                                   [(Name,LineNumber)]

%if False

> instance Show TCGrammar
>    where show (TCGrammar ln g alg defs combs) = "TCGrammar "
>                                  ++show g ++ "\n\n" 
>                                  ++(unlines.(map show)) alg++ "\n\n" 
>                                  ++(unlines.(map show)) defs++ "\n\n" 
>                                  ++(unlines.(map show)) combs

%endif

In contrast to an algebra function, a grammar production must not have parameters.
A production consists of the line number where the production was found,
the name of the defined nonterminal,
and the right-hand side, which is again represented as an expression.

> data TCGFunDef = TCGFunDef LineNumber Name  TCExp
>         deriving Show

\subsubsection{Expressions    }

Expressions are at the heart of ADP programming 
``in the small'' (compare \cite{jones:harep}).
As we know, they denote the right-hand sides of algebra functions
or grammar productions.
An expression can be an atomic expression, like 
a variable, a numeric value, a character, or a string.
It can also be a composed expression,
 constructed from other subexpressions.
These can be a tuple of expressions, 
an application of a function to a list of subexpressions, 
or an if-then-else-expression, 
which has a condition and an if- and else-branch as subexpressions.

> data TCExp
>  = TCVar Name
>  | TChNum Double
>  | TCChar Char
>  | TCString String
>  | TCTuple [TCExp]
>  | TCApp Name [TCExp]
>  | TCIf TCExp TCExp TCExp
>  deriving (Show)

%if False

> instance PrettyPrint TCExp where 
>   prettyPrint (TCVar v) = v
>   prettyPrint (TChNum d) = show d
>   prettyPrint (TCChar c) = show c
>   prettyPrint (TCString s) = show s
>   prettyPrint (TCTuple es) = "("++ concatWith ", " (map prettyPrint es) ++")"
>   prettyPrint (TCApp "out" [e]) = prettyPrint e
>   prettyPrint (TCApp f es) = show f++" "++ concatWith " " (map prettyPrint es)
>   prettyPrint (TCIf e1 e2 e3) = "if ("++prettyPrint e1
>                               ++") then "++prettyPrint e2
>                               ++" else "++prettyPrint e3

%endif


\subsubsection{Types    }


Functions, variables, and expressions all have a static type.
Before we define a representation for the different types in an 
ADP program, let us define a relation 
that composes a function type out of two types. 
We define a binary operator, 
inspired by the function type operator |->| of Haskell.

> infixr 5 :->

As announced in section \ref{adpvshaskell},
we define all type constructors explicitly, instead of having an abstract 
notation (see section \ref{adpvshaskell}).

The different types may be basic or composed types. 
A standard primitive data type, or the unspecified type |Any| is a \emph{basic type}.

The other ones are \emph{composed types}, which again contain types.
Some of them consist of a type with a variable name or a 
synonym name or a class constraint ``wrapped around'' it.
A type synonym contains a typed variable, which consists of the synonym 
name |s| and the defined type |t| for 
the synonym: |(TSyn s:>:t)|,
where @:>:@ is equivalent to the has-type operator @::@ of Haskell and will 
be introduced below. \\
The list type represents a list in which all elements have one particular type,
whereas a type tuple consists of a list of different types, 
 because it can have a different type in each parameter.
In the grammar we combine parsers of some particular type.
At last there is the function type, composed of two types by the |:->| operator.

> data Type 
>   = TCon Name         -- single type
>   | Any               -- unrestricted type
>   | TVar TypedVar     -- type variable with known type
>   | TSyn  TypedVar    -- type synonym with name and type
>   | TList Type        -- list of types
>   | TTuple [Type]     -- tuples of types
>   | TyParser Type     -- type parser
>   | Type :-> Type     -- function type
>   | ClassConstraint TypedVar Type -- type class constraint
>   deriving (Eq, Show)

%if False

> instance PrettyPrint Type where
>     prettyPrint (TCon n) = n
>     prettyPrint (Any   ) = "Any"
>     prettyPrint (TVar  (v:>:Any)) =  v
>     prettyPrint (TVar  (v:>:t)) = prettyPrint t
>     prettyPrint (TSyn  (s:>:t)) = prettyPrint t
>     prettyPrint (TList (TCon "Char")) = "String"
>     prettyPrint (TList    t ) = "["++prettyPrint t++"]"
>     prettyPrint (TTuple   ts) = "("
>                               ++ concatWith ", " (map prettyPrint ts) 
>                               ++")"
>     prettyPrint (TyParser t ) = "Parser "++ prettyPrint t
>     prettyPrint (t :-> t') = "("++prettyPrint t++" -> "++prettyPrint t'++")"
>     prettyPrint (ClassConstraint (v:>:t) t') = show t++" "
>                                              ++show v++" => "
>                                              ++show t'

%endif

%if False

---- constant/single types ----------------------------------------------------

%endif

The following list of definitions represents the standard primitive data types.

> tInt, tBool, tChar :: Type
> tUnit    = TCon "()"
> tChar    = TCon "Char"
> tInt     = TCon "Int"
> tInteger = TCon "Integer"
> tFloat   = TCon "Float"
> tDouble  = TCon "Double"
> tBool    = TCon "Bool"

A |String| is in fact an internal type synonym for a list of characters.

> tString    :: Type
> tString     = TList tChar

Type classes are represented the same way.
ADP supports three type classes.

If a variable is of type class |Num|, it may have any numeric value.
If it is of type class |Integral|, it may have type |Int| or |Integer|.
And if it is of type class |Double|, it may have any type that represents 
floating point numbers (i.e. |Float|, |Double|).

> tNum      = TCon "Num"
> tIntegral = TCon "Integral"
> tFloating = TCon "Floating"


%if False

In fact, Parser b is a type synonym in the Haskell implementation,
and has the following meaning:

Parser b = ((TSyn "Subword") :-> list b)

%endif





\subsubsection*{Helper Functions for Function Types    }





The type check tree module provides some helper functions for the conversion
of types.

The function |typeToList| flattens a function type, which is composed with 
the function type operator |:->| to a list of types. 
Argument types and result type appear in sequence 
in the resulting list.

> typeToList :: Type -> [Type]
> typeToList t = case t of
>      t1 :-> t2                ->   t1 : typeToList t2
>      x                        ->   [x]


The function |listToType| is the inverted function.
It builds a tree-like type from a flat list of types with the function type 
operator.

> listToType :: [Type] -> Type
> listToType [] = error $ "listToType of empty list"
> listToType ts = foldr (:->) tres targs
>      where targs = init ts
>            tres  = last ts


We can compute the type of a function if we have
the types of its arguments and the type of the result.

> tFun :: [TCParam] -> Type -> Type
> tFun targs tres = foldr (:->) tres (map snd targs)



\subsubsection{Typed Variables and Environments    }


Some other data types and operators are convenient for the type check program.

We define a binary operator, which represents the
``has type'' relation. This relation is represented by the operator |::| in
Haskell. We use the operator |:>:|.

> infix 4 :>:

Our algorithm assigns types to variables. Therefore we have
an extra data type for these pairs of variables and types.
|TypedVar| represents a variable and its type, linked by the ``has type'' 
operator.

> data TypedVar = Name :>: Type
>  deriving Eq

%if False

> instance Show TypedVar where 
>    show (x:>:t) = show x ++ " :: " ++show t
> instance PrettyPrint TypedVar where 
>    prettyPrint (x:>:t) = show x ++ " :: " ++show t

%endif

Typed variables are ordered by ordering their variable names.

> instance Ord TypedVar where
>    c <= c'          =  varname c <= varname c'
>      where varname (v:>:t) = v 


A variant of the typed variable comes with a line number as well.
The line number refers to the line in the ADP program where the type 
was assigned to the variable. 
We keep track of line numbers to include them into error messages. 

> type TypedVar1 = (TypedVar,LineNumber)

Typed variables usually do not appear alone.
As explained in \ref{typecheckprob}, 
we call a list of typed variables a type environment, 
or in the program code just |Env|.

> type Env = [TypedVar1]
