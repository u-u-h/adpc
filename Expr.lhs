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



> module Expr(

>   Exp(..),
>   SigArgument(..),
>   AlgAppl, InputString, AlgebraTypeDecl, 
>   AlgTypeDef,
>   AutoAlgType, AlgDef, AlgDefs,
>   SignatureArea, Signature,
>   SigOperator,
>   collectExp,
>   collectExps,
>   expMakeAnd,
>   expMakeAndList,
>   expMakeOr,
>   expToMathExp,
>   expTupelToSS,
>   getAlgDef,
>   getAlgDefExpr,
>   getAlgDefList,
>   getAlgDef_for_constr,
>   insertVarBinds,
>   containsExpVar,
>   mapExp,
>   ppAlgAppl,
>   ppAlgDef,
>   ppAlgDefs,
>   ppAlgebraTypeDecl,
>   ppAutoAlgType,
>   ppSigArgument,
>   ppSignature,
>   ppSignatureArea,
>   rev_Expr,
>   squ

> ) where

> import MathExp
> import TLData
> import Tools
> import Constants
> import Data.Char
> import PrettyPrint

> rev_Expr =  "$Revision$"

Signatur:
------------

> data SigArgument = SigId String           |
>                    SigTupel [SigArgument] |   
>                    SigList  SigArgument         deriving (Eq, Show)

> type SigOperator = (String, [SigArgument])
> type Signature   = (String, [SigOperator])

> ppSignature (n, ops) = "data " ++ n ++ " = \n" ++ mapsep " |\n" ppSigOperator ops ++ "\n"
> ppSigOperator (n, args) = "   "  ++ n ++ " " ++ mapsep " " ppSigArgument args 
> ppSigArgument (SigId n)    = n
> ppSigArgument (SigTupel t) = "(" ++ mapsep ", " ppSigArgument t ++ ")"
> ppSigArgument (SigList l)  = "[" ++ ppSigArgument l ++ "]"

> type TypeDef = (String, SigArgument)
> ppTypeDef (n,a) = "type " ++ n ++ " = " ++ ppSigArgument a ++ "\n"

> type SignatureArea = (Signature, [TypeDef])
> ppSignatureArea (s, tdefs) = ppSignature s ++ concatMap ppTypeDef tdefs

Algebra Typ-Deklarationen:
---------------------------

> type AlgebraTypeDecl = (String, [String], [AlgebraTypeDeclFct])
> ppAlgebraTypeDecl (name, args, fcts) = "type " ++ name ++ " " ++ sepList " " args ++ " = (\n    " ++ 
>                                        mapsep ",\n    " ppAlgebraTypeDeclFct fcts ++ ")\n"

> type AlgebraTypeDeclFct = [SigArgument]
> ppAlgebraTypeDeclFct args = mapsep " -> " ppSigArgument args


Algebrafunktionen:
----------------------

> -- Algebrafunktionsanwendung mit optionalen Argumenten:
> type AlgAppl   = (String, [String])
> ppAlgAppl (f, args) = if args==[] then f else f ++ " " ++ sepList " " args

> -- der automatisch aus der Grammatik abgeleitete Typ einer Algebrafunktion
> -- wird benoetigt, falls vom Benutzer keiner angegeben wurde
> type AutoAlgType = (AlgAppl, [SigArgument])
> ppAutoAlgType (f, dts) = ppAlgAppl f ++ "::" ++ mapsep " -> " ppSigArgument (dts ++ [SigId "_Result"])

Definitionen:
--------------

> -- eine einzelne Definition
> type AlgDef = (String, AlgebraTypeDeclFct, [SigArgument], Exp)
> ppAlgDef (n, typ, args, rhs) = typedef ++ n ++ " " ++ (sepList " " (map ppSigArgument args)) ++ " = " ++ ppExp Normal rhs 
>     where typedef | typ == [] = ""
>                   | otherwise = n ++ " :: " ++ ppAlgebraTypeDeclFct typ ++ "\n"
> -- eine komplette Algebra
> type AlgDefs = (String, ([AlgTypeDef],[AlgDef]))

ppAlgDefs :: (String, ([((String, String, [SigArgument]), [String])], [(String, [SigArgument], [SigArgument], Exp)])) -> String

> ppAlgDefs :: AlgDefs -> String
> ppAlgDefs (name, (algtd,afs))   = "name: " ++ name ++ "\n"     ++ 
>                                   concatMap ppAlgTypeDef algtd ++
>                                   mapsep "\n" ppAlgDef afs

> -- hole fuer jede Algebra in algfs _genau_ eine Algebrafunktion mit Namen f:
> getAlgDefList :: [AlgDefs] -> String -> [AlgDef]
> getAlgDefList algfs f =  [ (algName, atype, args, rhs) | (algName, (_,algfs')) <- algfs, 
>                                                          (n, atype, args, rhs) <- [head' [(n, atype, args, rhs) | 
>                                                                                   (n, atype, args, rhs) <- algfs', f == n]
>                                                                                    ("algebra function " ++ f ++ " in algebra " ++ 
>                                                                                     algName ++ " not found.\nCurrent Algebra: " ++
>                                                                                     concatMap ppAlgDefs algfs)]] 


> -- eine moeglicherweise vorhandene Algebra-Typangabe
> type AlgTypeDef = ((String, String, [SigArgument]), [String])
> ppAlgTypeDef ((name, constr, args), order) = 
>    name ++ " :: " ++ constr ++ " " ++ mapsep " " ppSigArgument args ++ "\n" ++
>    name ++ " =\n   " ++ "(" ++ sepList ", " order ++ ")\n   where\n"

> -- Hilfsfunktionen:
> getAlgDef :: AlgDefs -> [AlgDef]
> getAlgDef (_, (_, alg)) = alg

> getAlgDefExpr :: AlgDefs -> String -> Exp
> getAlgDefExpr (an, (_, adfs)) fname = head' [ exp | (fn, _, _, exp) <- adfs,    fn == fname ]
>                                       $ "getAlgDefExpr: unknown algebra function " ++ fname ++ 
>                                         "\nin algebra "++ an ++ "."

> getAlgDef_for_constr :: AlgDefs -> String -> ([SigArgument], Exp)
> getAlgDef_for_constr (algName,(_,algfs)) f = head' [(args, rhs) | (n,_,args,rhs) <- ntIDalgf:algfs, lower n == lower f] $ 
>                                                    "no algebra function found for constructor " ++ f ++ " in algebra " ++ algName ++ "."
>     where 
>       ntIDalgf = (pNTID prefixes, [], [(SigId "a1")], ExpVar "a1")
>       lower    = map toLower

Rhs:

> type OpName      = String
> type InputString = String

> data Exp = 

>               ExpChar Char                        |
>               ExpString String                    |
>               ExpNum Double                       |

>               ExpVoid                             | -- ()
>               ExpVar        String                |
>               ExpPOp OpName [Exp]                 | -- prefix operator
>                                                     -- OpName( , , ..)
>               ExpIOp Exp OpName Exp               |
>               ExpIf  Exp Exp Exp                  |
>               ExpChoice String Exp                |
>               ExpTupel [Exp]                      |
>               ExpConstr String [Exp]              |

>               ExpInput  InputString MathExp       |
>               ExpInputS InputString SSubScripts   |
>               ExpME MathExp                       |

>               ExpTLVar VAccess                    |
>               ExpNil                              |

>               ExpCons    Exp Exp                  |
>               ExpAppend  Exp Exp                  |

>               ExpDot     Exp Exp                  |
>               ExpIn      Exp Exp                  |
>               ExpEnum    Exp Exp                  |
>               ExpLet     Exp Exp
>                                                  deriving (Eq, Show)


> instance Pretty Exp where
>     pretty = ppExp Normal
>     prettyLang = ppExp


Tools:
-------

> expToMathExp (ExpME m) = m
> expToMathExp (ExpNum n) = Number $ round n
> expToMathExp (ExpVar v) | isDigit (head v) = Number (strtoInt v)
>                         | otherwise        = Var v
> expToMathExp (ExpIOp e1 "+" e2) = (expToMathExp e1) :+ (expToMathExp e2)
> expToMathExp (ExpIOp e1 "-" e2) = (expToMathExp e1) :- (expToMathExp e2)
> expToMathExp (ExpIOp e1 "*" e2) = (expToMathExp e1) :* (expToMathExp e2)
> expToMathExp (ExpIOp e1 "/" e2) = (expToMathExp e1) :/ (expToMathExp e2)
> expToMathExp (ExpPOp "min" [ExpTupel [e1,e2]]) = Min (expToMathExp e1) (expToMathExp e2)
> expToMathExp (ExpPOp "max" [ExpTupel [e1,e2]]) = Max (expToMathExp e1) (expToMathExp e2)

> expToMathExp x = pattErr "expToMathExp" x


> expTupelToSS [ExpTupel [e1, e2]] = ST (expToMathExp e1, expToMathExp e2)
> expTupelToSS [ExpTupel [e1, e2], 
>               ExpTupel [e3, e4]] = TT (expToMathExp e1, expToMathExp e2) (expToMathExp e3, expToMathExp e4)
> expTupelToSS x = pattErr "expTupelToSS" x

> expMakeAndList [] = []
> expMakeAndList x  = [expMakeAnd x]

> expMakeAnd [x]    = x
> expMakeAnd (x:xs) = ExpIOp x "&&" (expMakeAnd xs)

> expMakeOr [x]    = x
> expMakeOr (x:xs) = ExpIOp x "||" (expMakeOr xs)

> insertVarBinds :: [(String, Exp)] -> Exp -> Exp
> insertVarBinds bs e = insertVarBindsOptPrefix bs "" e

> insertVarBindsOptPrefix :: [(String, Exp)] -> String -> Exp -> Exp
> insertVarBindsOptPrefix bs optPrefix (ExpVar v) = let nv = [ nv | (v', nv) <- bs, v == v'] in
>                                          case nv of
>                                           []        -> if (isDigit (head' v "insertVarBindsOptPrefix: head []")) 
>                                                        then ExpVar v
>                                                        else ExpVar (optPrefix ++ v)
>                                           otherwise -> head nv
> insertVarBindsOptPrefix bs optPrefix x          = mapExp (insertVarBindsOptPrefix bs optPrefix) x

> containsExpVar v exp = fst $ contains v False exp
>   where
>     contains v cll e@(ExpVar v2) | v == v2 = (True, e)
>     contains v cll e             = collectExp (contains v) cll e


Pretty printer:
-----------------

> noPar (ExpChar _)                 = True
> noPar (ExpString _)               = True
> noPar (ExpNum _)                  = True
> noPar (ExpVar _)                  = True
> noPar (ExpPOp _ _)                = True
> noPar (ExpChoice _ _ )            = True
> noPar (ExpTupel  _ )              = True
> noPar (ExpConstr _ _ )            = True
> noPar (ExpInput _ _)              = True
> noPar (ExpInputS _ _)             = True
> noPar (ExpTLVar _)                = True
> noPar (ExpNil)                    = True
> noPar (ExpME (Number _))          = True
> noPar _                           = False

> noParM (Number _) = True
> noParM (Var _)    = True
> noParM _          = False

> cqu = chr 39
> squ = chr 34

> ppExp Normal  (ExpChar c)    = [cqu,c,cqu]
> ppExp C       (ExpChar c)    = [cqu,c,cqu]
> ppExp F       (ExpChar c)    = [cqu,c,cqu]
> ppExp Pascal  (ExpChar c)    = [cqu,c,cqu]
> ppExp Latex   (ExpChar c)    = "\\," ++ [cqu] ++ "\\!" ++ replChar c ++ [cqu]
>   where
>   replChar c | elem c "$%" = "\\" ++ [c]
>              | otherwise   = [c]

> ppExp Normal  (ExpString s)  = squ:s ++ [squ]
> ppExp C       (ExpString s)  = squ:s ++ [squ]
> ppExp F       (ExpString s)  = squ:s ++ [squ]
> ppExp Pascal  (ExpString s)  = cqu:s ++ [cqu]
> ppExp Latex   (ExpString s)  = squ:s ++ [squ]

> ppExp tl      (ExpNum n)     = let (a,b) = span (/= '.') (show n)
>                                in if b == ".0" then show' a else show' (show n)
>    where
>      show' s = if n < 0 then "(" ++ s ++ ")" else s

> ppExp tl      (ExpVoid)      = "()"
> ppExp tl      (ExpVar a)     = a

Dies sollte eigentlich nicht mehr im Zielcode auftauchen.So ist der Trace allerdings etwas schoener:

> ppExp tl      (ExpPOp n [ExpTupel a]) = n ++ ppExp tl (ExpTupel a)

> ppExp Java (ExpPOp n a) = changeName n ++
>   "("++ sepList ", " (map (ppExp Java) a) ++ ")"
>   where
>     changeName n
>      | isPrefixes ["abs", "min", "max"] n = "Math." ++ n
>      | otherwise = n

> ppExp tl      (ExpPOp n a)            = n ++ "("++ sepList ", " (map (ppExp tl) a) ++ ")"

> ppExp tl      (ExpIOp a op b)  = ppExp' a  ++ " " ++ op' ++ " " ++ ppExp' b  where
>     ppExp' a = case noPar a of
>                    True  -> ppExp tl a
>                    False -> case tl of
>                               Latex     -> ppExp tl a
>                               otherwise -> "(" ++ ppExp tl a ++ ")"
>     op' = ppOp tl op

>     ppOp Normal "&&" = "&&"
>     ppOp C      "&&" = "&&"
>     ppOp F      "&&" = ".AND."
>     ppOp Pascal "&&" = "and"
>     ppOp Latex  "&&" = "\\wedge"

>     ppOp Normal "||" = "||"
>     ppOp C      "||" = "||"
>     ppOp F      "||" = ".OR."
>     ppOp Pascal "||" = "or"
>     ppOp Latex  "||" = "\\vee"

>     ppOp Normal "==" = "=="
>     ppOp C      "==" = "=="
>     ppOp F      "==" = "=="
>     ppOp Pascal "==" = "="
>     ppOp Latex  "==" = "\\equiv"

>     ppOp Normal "/=" = "/="
>     ppOp C      "/=" = "!="
>     ppOp F      "/=" = "/="
>     ppOp Pascal "/=" = "<>"
>     ppOp Latex  "/=" = "\\neq"

>     ppOp Normal "<=" = "<="
>     ppOp C      "<=" = "<="
>     ppOp F      "<=" = "<="
>     ppOp Pascal "<=" = "<="
>     ppOp Latex  "<=" = "\\leq"

>     ppOp Normal ">=" = ">="
>     ppOp C      ">=" = ">="
>     ppOp F      ">=" = ">="
>     ppOp Pascal ">=" = ">="
>     ppOp Latex  ">=" = "\\geq"

java goes like C in this case

>     ppOp Java op = ppOp C op

>     ppOp _ op        = op

> ppExp Java (ExpIf e a b)         = "(" ++ ppExp Java e ++ ") ? " ++ ppExp Java a ++ " : " ++ ppExp Java b

> ppExp C      (ExpIf e a b)         = "(" ++ ppExp C e ++ ") ? " ++ ppExp C a ++ " : " ++ ppExp C b
> ppExp F      (ExpIf e a b)         = "MERGE(" ++ ppExp F a ++ " , " ++ ppExp F b ++ " , " ++ ppExp F e ++ ")"
> ppExp Latex  (ExpIf e a b)         = "\\text{if }" ++ ppExp Latex e ++ "\\text{ then }" ++ ppExp Latex a ++ 
>                                      " \\nonumber \\\\\n &   && " ++ "\\phantom{\\text{if } " ++ ppExp Latex e ++ "}" ++ 
>                                                                      "\\text{ else }" ++ ppExp Latex b
> ppExp tl     (ExpIf e a b)         = "if " ++ ppExp tl e ++ " then " ++ ppExp tl a ++ " else " ++ ppExp tl b

> ppExp tl     (ExpChoice a x )      = "[" ++ a ++ " " ++ ppExp tl x ++ "]"
> ppExp tl     (ExpTupel exps)       = "(" ++ sepList ", " (map (ppExp tl) exps) ++ ")"
> ppExp tl     (ExpConstr name exps) = name ++ " " ++ sepList " " (map (ppExp' tl) exps)
>   where
>     ppExp' tl exp | par exp   = "(" ++ ppExp tl exp ++ ")"
>                   | otherwise =        ppExp tl exp
>     par (ExpConstr _ _) = True
>     par (ExpPOp _ _)    = True
>     par (ExpIOp _ _ _)  = True
>     par (ExpIf _ _ _)   = True
>     par _               = False 

> -- hier konsistent x und z !!!
> ppExp Normal (ExpInput inp m)  = if noParM m then inp ++ "!" ++ prettyLang Normal m else inp ++ "!" ++ "(" ++ prettyLang Normal m ++ ")"
> ppExp C      (ExpInput inp m)  = inp ++ "["  ++ prettyLang C      m ++ "]"
> ppExp F      (ExpInput inp m)  = inp ++ "("  ++ prettyLang F      m ++ ")"
> ppExp Pascal (ExpInput inp m)  = inp ++ "["  ++ prettyLang Pascal m ++ "]"
> ppExp Latex  (ExpInput inp m)  = inp ++ "_{" ++ prettyLang Latex  m ++ "}"

use Java Strings instead of pointer arithmetic

> ppExp Java (ExpInput inp m) = inp ++ ".charAt(" ++ prettyLang Java m ++ ")"

> ppExp Normal (ExpInputS inp (i,j)) = inp ++ "seq!(" ++ prettyLang Normal i ++ ", " ++ prettyLang Normal j ++ ")"
> ppExp C      (ExpInputS inp (i,j)) = inp ++ "seq("  ++ prettyLang C      i ++ ", " ++ prettyLang C      j ++ ")"
> ppExp F      (ExpInputS inp (i,j)) = inp ++ "seq("  ++ prettyLang F      i ++ ", " ++ prettyLang F      j ++ ")"
> ppExp Pascal (ExpInputS inp (i,j)) = inp ++ "seq("  ++ prettyLang Pascal i ++ ", " ++ prettyLang Pascal j ++ ")" 
> ppExp Latex  (ExpInputS inp (i,j)) = inp ++ "seq!(" ++ prettyLang Latex  i ++ ", " ++ prettyLang Latex  j ++ ")"

> ppExp tl     (ExpME j)     = prettyLang tl j

> ppExp lang (ExpTLVar v)  = prettyLang lang v

> ppExp Normal ExpNil  = "nil"
> ppExp C      ExpNil  = "NULL"
> ppExp F      ExpNil  = "NULL"
> ppExp Pascal ExpNil  = "nil"
> ppExp Latex  ExpNil  = "nil"  -- or bottom-element
> ppExp Java   ExpNil  = "null"

> ppExp tl (ExpCons   e1 e2) = ppExp tl e1 ++ ":"  ++ ppExp tl e2
> ppExp tl (ExpAppend e1 e2) = ppExp tl e1 ++ "++" ++ ppExp tl e2
> ppExp tl (ExpDot    e1 e2) = ppExp tl e1 ++ "." ++ ppExp tl e2
> ppExp tl (ExpIn     e1 e2) = ppExp tl e1 ++ " <- " ++ ppExp tl e2
> ppExp tl (ExpEnum   e1 e2) = "[" ++ ppExp tl e1 ++ " .. " ++ ppExp tl e2 ++ "]"
> ppExp tl (ExpLet    e1 e2) = "let " ++ ppExp tl e1 ++ " = " ++ ppExp tl e2


rest of Java goes like C ...

 ppExp Java foo = "/* FIXME Exp: " ++ (show foo) ++ " */"

> ppExp Java foo = ppExp C foo


----------------------------------------------------------------------------------------------------
Collector for [Exp]

> collectExps wrk cll []         = (cll,   [])
> collectExps wrk cll (exp:exps) = (cll'', (exp':exps'))
>     where
>       (cll',  exp')  = wrk             cll  exp
>       (cll'', exps') = collectExps wrk cll' exps

----------------------------------------------------------------------------------------------------
Collector for Exp
---------------------

> collectExp wrk cll (ExpChar c)         = (cll, ExpChar c)
> collectExp wrk cll (ExpString s)       = (cll, ExpString s)
> collectExp wrk cll (ExpNum n)          = (cll, ExpNum n)
> collectExp wrk cll (ExpVoid)           = (cll, ExpVoid)

> collectExp wrk cll (ExpVar s)      = (cll, ExpVar s)
> collectExp wrk cll (ExpPOp s exps) = (cll', ExpPOp s exps')   where (cll', exps') = collectExps wrk cll exps

> collectExp wrk cll (ExpIOp exp1 s exp2) = (cll'', ExpIOp exp1' s exp2')
>      where
>        (cll',  exp1') = wrk cll  exp1
>        (cll'', exp2') = wrk cll' exp2

> collectExp wrk cll (ExpIf  exp1 exp2 exp3) = (cll''', ExpIf  exp1' exp2' exp3')
>      where
>        (cll',   exp1') = wrk cll   exp1
>        (cll'',  exp2') = wrk cll'  exp2
>        (cll''', exp3') = wrk cll'' exp3

> collectExp wrk cll (ExpChoice s exp)  = (cll', ExpChoice s exp')  where (cll', exp')  = wrk cll exp

> collectExp wrk cll (ExpTupel exps)    = (cll', ExpTupel exps')    where (cll', exps') = collectExps wrk cll exps

> collectExp wrk cll (ExpConstr s exps) = (cll', ExpConstr s exps') where (cll', exps') = collectExps wrk cll exps

> collectExp wrk cll (ExpInput s m)     = (cll, ExpInput s m)
> collectExp wrk cll (ExpInputS s ss)   = (cll, ExpInputS s ss)
> collectExp wrk cll (ExpME m)          = (cll, ExpME m)

> collectExp wrk cll (ExpTLVar va)      = (cll, ExpTLVar va)
> collectExp wrk cll (ExpNil)           = (cll, ExpNil)

> collectExp wrk cll (ExpCons exp1 exp2) = (cll'', ExpCons exp1' exp2')
>      where
>        (cll',  exp1') = wrk cll  exp1
>        (cll'', exp2') = wrk cll' exp2
> collectExp wrk cll (ExpAppend exp1 exp2) = (cll'', ExpAppend exp1' exp2')
>      where
>        (cll',  exp1') = wrk cll  exp1
>        (cll'', exp2') = wrk cll' exp2
> collectExp wrk cll (ExpDot exp1 exp2) = (cll'', ExpDot exp1' exp2')
>      where
>        (cll',  exp1') = wrk cll  exp1
>        (cll'', exp2') = wrk cll' exp2
> collectExp wrk cll (ExpIn exp1 exp2) = (cll'', ExpIn exp1' exp2')
>      where
>        (cll',  exp1') = wrk cll  exp1
>        (cll'', exp2') = wrk cll' exp2
> collectExp wrk cll (ExpEnum exp1 exp2) = (cll'', ExpEnum exp1' exp2')
>      where
>        (cll',  exp1') = wrk cll  exp1
>        (cll'', exp2') = wrk cll' exp2
> collectExp wrk cll (ExpLet exp1 exp2) = (cll'', ExpLet exp1' exp2')
>      where
>        (cll',  exp1') = wrk cll  exp1
>        (cll'', exp2') = wrk cll' exp2


> mapExp wrk x = snd $ collectExp wrk' 0 x where
>    wrk' cll x = (cll, wrk x)

