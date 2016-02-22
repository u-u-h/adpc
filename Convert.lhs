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



Converts the new Syntax-Data-Structure from the new Parser (ParseTree,
NewParser,Parse2) to the one used by the old Parser (Parse.ly).


> module Convert ( 

>    adpprogram2adpprogram 

> ) where

> import ParseTree
> import Syntax
> import Expr
> import MathExp
> import Parse
> import Tools

TODO
- Zusaetzlich in Production:
  - :<<< -> Expression statt identifier
  - ListCompr
  - TTUnit
  - Benutzerdef. next-Kombin.: p :~~! (String, p)
  - :^^^
  - :/\\/
  - allg. ~~ : beim Parsen noch nicht notwendig

- Bei Terminalen, Filtern, und Algebrafunktionen (in Produktionen)
  allgemeine Expressions zulassen. Momentan hier nur:

  type AlgAppl   = (String, [String])
  type Term      = (String, [String])
  type Filter    = (String, [String])

- In Production nur noch ein next-Kombinator-Konstruktor

> adpprogram2adpprogram :: ParseTree.ADPProgram -> Parse.ADPProgram
> adpprogram2adpprogram (ADPProgram _ _ at algs grammar)
>   = (([],[],[],[]), -- userdef2userfunctions ufs, 
>      ([],[],[],[]),
>      grammar2grammar grammar,
>      map algebra2algdefs algs,
>      [],
>      [algebratype2algebratypedecl at])


> grammar2grammar (Grammar ln _ axiom _ _ prods) = (axiom, map grammarfundef2prodsource prods)

> grammarfundef2prodsource (GrammarFunDef ln n args TTFree p ps)             
>    = n :==== ("", Syntax.Freetabulated, ST (True, True), production2unit p, map grammarfundef2prodsource ps)
> grammarfundef2prodsource (GrammarFunDef ln n args TTNontabulated p ps)             
>    = n :==== ("", Syntax.Nontabulated, ST (True, True), production2unit p, map grammarfundef2prodsource ps)
> grammarfundef2prodsource (GrammarFunDef ln n args (TTTabulated tr) p ps)             
>    = n :==== ("", Syntax.Tabulated, ST tr, production2unit p, map grammarfundef2prodsource ps)
> grammarfundef2prodsource (CombinatorDef ln n cd)
>    = CombDef n (combinatordef2combParams cd)

> combinatordef2combParams = conv
>  where
>    conv (CombinatorTT  (a,b) (c,d)) = CombYSize (ST (MathExp.Number a, MathExp.Number b)) (ST (MathExp.Number c, MathExp.Number d))
>    conv (CombinatorSTT a     (c,d)) = CombYSize (ST (MathExp.Number a, MathExp.Infinite)) (ST (MathExp.Number c, MathExp.Number d))
>    conv (CombinatorTTS (a,b) c)     = CombYSize (ST (MathExp.Number a, MathExp.Number b)) (ST (MathExp.Number c, MathExp.Infinite))
>    conv (CombinatorSTS a     c)     = CombYSize (ST (MathExp.Number a, MathExp.Infinite)) (ST (MathExp.Number c, MathExp.Infinite))
>    conv (CombinatorHHH a     c)     = CombLA a c


-- > userdef2userfunctions fs = (concatMap convTerm fs, 
-- >                             concatMap convFilter fs,
-- >                             concatMap convLA fs,
-- >                             concatMap convLC fs)
-- >  where
-- >    convTerm (DefTerminal ln a b c ys) = [(a,b,c,numbertuple2sysize ys)]
-- >    convTerm _                         = []
-- >    convFilter (DefFilter ln n ys)     = [(n,numbertuple2ysize ys)]
-- >    convFilter _                       = []
-- >    convLA (DefLA ln n ys)             = [(n,numbertuple2sysize ys)]
-- >    convLA _                           = []
-- >    convLC (DefLC ln n ys)             = [(n,numbertuple2ysize ys)]
-- >    convLC _                           = []

-- > numbertuple2sysize (a,b) = (number2mathexp a, number2mathexp b)
-- > numbertuple2ysize (a,b)  = ST (number2mathexp a, number2mathexp b)
-- > number2mathexp (ParseTree.Number a) = MathExp.Number a
-- > number2mathexp (ParseTree.Infinite) = MathExp.Infinite
 
> algebratype2algebratypedecl :: AlgebraType -> AlgebraTypeDecl
> algebratype2algebratypedecl (AlgebraType ln name params (TypeTuple defs)) = (name, params, map conv defs)
>   where
>     conv :: TypeDef -> [SigArgument]
>     conv (SingleType s) = [SigId $ ParseTree.prettyPrint s]
>     conv (TypeTuple ts) = [SigTupel $ concatMap conv ts]
>     conv (ListType t)   = [SigList $ head $ conv t]
>     conv (FunType a b)  = conv a ++ conv b

> algebra2algdefs :: Algebra -> AlgDefs
> algebra2algdefs (Algebra ln (AlgebraFunType ln1 algname  typename typeparam) 
>                             (AlgebraFunDef  ln2 algname' fctorder fundefs))
>    = (algname, ([((algname', typename, concatMap typedef2sigargument typeparam), fctorder)], map conv fundefs))
>    where
>      conv (FunDef ln name params exp) = (name, [],  map parameter2sigargument params, expression2exp exp)
>         -- zweites Tupelelement: Typangabe; wird spaeter gefuellt; anfangs []

> expression2simpleAppl (Ident i)      = (i, [])
> expression2simpleAppl (FunAp i exps) = (i, concatMap conv exps)
>   where
>     conv (Ident i)    = [i]
>     conv (Char c)     = ["'" ++ [c] ++ "'"]
>     conv (String s)   = ["\"" ++ s ++ "\""]
>     conv (Num s)      = [show s]
>     conv (Tuple exps) = concatMap conv exps
>     conv x            = pattErr "expression2simpleAppl" x

> production2unit p = conv p
>   where
>     conv (ParseTree.CombinatorAp ln p "..." (ParseTree.ProdExpr _ (ParseTree.Ident i)))  = conv p :... i
>     conv (ParseTree.CombinatorAp ln p "|||" q)                                           = conv p :||| conv q
>     conv (ParseTree.CombinatorAp ln (ParseTree.ProdExpr _ i) "<<<" p)                    = expression2simpleAppl i :<<< conv p
>     conv (ParseTree.CombinatorAp ln p "~~~" q)                                           = conv p :~~~ conv q
>     conv (ParseTree.CombinatorAp ln p "with" (ParseTree.ProdExpr _ exp))                 = conv p `Syntax.With` expression2simpleAppl exp
>     conv (ParseTree.CombinatorAp ln p "><" q)                                            = TTUnit (conv p) (conv q)
>     conv (ParseTree.CombinatorAp ln p comb q)                                            = conv p :~~! (comb, conv q)

>     conv (ParseTree.ProdExpr ln exp) = Terminal $ expression2simpleAppl exp
>     conv (ListComprehension ln param exp lcexp) = ListCompr (expression2exp exp, map lcexpression2lcexp lcexp) 
>                                                             (parameter2indexvars param) (ST (MathExp.Number 0, MathExp.Infinite))

>     conv x = pattErr "production2unit" x



> parameter2indexvars (TupleParam [TupleParam [a1,b1],
>                                  TupleParam [a2,b2]]) = TT (p2s a1, p2s b1) (p2s a2, p2s b2)
> parameter2indexvars (TupleParam [a,b])                = ST (p2s a, p2s b)
> parameter2indexvars x = error $ "not supported for list comprehensions" ++ show x
> p2s (Parameter a) = a
> p2s Wildcard      = "_"
> p2s x             = error $ "not supported for list comprehensions" ++ show x

> parameter2sigargument = conv 
>   where
>     conv (Wildcard)      = SigId "_"
>     conv (Parameter i)   = SigId i
>     conv (TupleParam ps) = SigTupel $ map conv ps

> typedef2sigargument = conv
>   where
>     conv (SingleType s)  = [SigId $ ParseTree.prettyPrint s]
>     conv (TypeTuple ts)  = [SigTupel $ concatMap conv ts]
>     conv (ListType t)    = [SigList $ head $ conv t]
>     conv (FunType t1 t2) = conv t1 ++ conv t2

> lcexpression2lcexp x = conv x
>   where
>     conv (LCFilter ln e)      = LCExp (expression2exp e)
>     conv (LCLet ln p e)       = LCExp (ExpLet (parameter2exp p) (expression2exp e))
>     conv (LCGenerator ln p e) = LCExp (ExpIn (expression2exp p) (expression2exp e))

> parameter2exp = conv
>   where
>    conv (Parameter p)   = p2e (Parameter p)
>    conv (TupleParam ps) = ExpTupel (map p2e ps)
>    p2e (Parameter p)    = ExpVar p
>    p2e (Wildcard)       = ExpVar "_"

> expression2exp = conv
>   where
>     conv (Ident i)         = ExpVar i
>     conv (Char c)          = ExpChar c
>     conv (String s)        = ExpString s
>     conv (Num n)           = ExpNum n -- ExpVar (init $ init $ show n) 
>     conv (Tuple exps)      = ExpTupel $  map conv exps
>     conv (If a b c)        = ExpIf (conv a) (conv b) (conv c)
>     conv (BinOp e1 App e2) = ExpAppend (conv e1) (conv e2)
>     conv (BinOp e1 Cons e2)= ExpCons (conv e1) (conv e2)
>     conv (BinOp e1 b e2)   = ExpIOp (conv e1) (binop2string b) (conv e2)
>     conv (FunAp i [])      = ExpVar i
>     conv (FunAp i [arg])   | elem i ["id","minimum","maximum","sum"] = ExpChoice i (conv arg)
>     conv (FunAp i exps)    = ExpPOp i (map conv exps)
>     conv (e1 :..: e2)      = ExpEnum (conv e1) (conv e2)

> binop2string x = head [ s | (o,s) <- ops, o == x]
>   where 
>     ops = [(ParseTree.LT,"<"),(ParseTree.GT,">"),(LE,"<="),(GE,">="),(ParseTree.EQ,"=="),
>            (NE,"/="),(Mul,"*"),(Div,"/"),(Add,"+"),(Sub,"-"),
>            (Exp,"^"),(App,"++"),(Or,"||"),(And,"&&")] 
