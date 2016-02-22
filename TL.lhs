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



> module TL(

>   MemType(..),
>   TL(..),
>   KWorkersTL(..),

>   WorkersTL,

>   collectDataType,
>   collectTL,
>   collectTLs,
>   collectVAccess,
>   commentBox,
>   commentLn,
>   declToVA,
>   initStack,
>   longComment,
>   longCommentToBox,
>   makeWorkersTL,
>   meToTL, tlvar, tlnumber,
>   pushStack,
>   rev_TL,
>   tlLongComment,
>   tlLongComments,
>   tlNonterm,
>   wrkDataType,
>   wrkTL,
>   wrkTLs,
>   wrkTypeDecl,
>   wrkVAccess,
>   wrkVarDecls,

> ) where

> import Tools
> import MathExp
> import Expr
> import Syntax
> import Dss
> import TLData
> import Constants
> import PrettyPrint

> rev_TL =  "$Revision$"

%----------------------------------------------------------------------------------------------------
\section{Zielsprache}
%----------------------------------------------------------------------------------------------------

%----------------------------------------------------------------------------------------------------
\subsection{Abstrakte Syntax}
%----------------------------------------------------------------------------------------------------

> data MemType = MTDynamic    -- dynamic memory, handled via malloc/free
>              | MTStatic     -- global memory 
>              | MTTemp       -- temp memory
>                                                             deriving (Eq, Show)


> data TL = 
>   TLNil                                             |
>   TLLayout String                                   | -- ^ goes verbatim into
>                                                       --   the code
>   TLMacro String                                    | -- to descibe the @FOO@-Style macros in a cleaner way
>   TLComment [String]                                |
>   TLVar VarAccess                                   |  
>   TLDecls [VarDecl]                                 |
>   TLTypeDecls [TypeDecl]                            |
>   TLAssign   VarAccess TL                           |
>   TLAssignIF VarAccess Exp TL TL                    |
>   TLAlloc    MemType VarAccess Exp DataType         |  
>   TLAllocArr VarAccess [(Exp, Exp)]                 |  -- Fortran array allocation
>   TLExp Exp                                         |
>   TLFD  [String] DataType String [VarDecl] [VarDecl] [TL] TL | -- function
>                                                                -- definition
>   TLFA  Function [TL]                               |
>   TLFPA VarAccess [TL]                              |
>   TLIf  Exp [TL] [TL]                               |
>   TLIfs Exp [TL] [TL]                               |  -- for large number of alternatives -> better prettyprinting
>   TLIfDef String [TL]                               |
>   TLFor  VarName MathExp MathExp [TL]               | 
>   TLForI VarName MathExp MathExp MathExp [TL]        |  -- for-loop with increment !- 1 
>   TLWhileNN VarAccess [TL]                          |
>   TLWhile   Exp       [TL]                          |
>   TLBreak                                           |
>   TLDefines [(DataType, String, [String], [TL])]              |  -- return dt (optional), Name, Argumente, Definition
>   TLPrint String [TL]                               |  -- Format-String, Argumente
>   TLBlock [TL]
>                                                             deriving (Eq, Show)


> instance Pretty TL where
>     pretty = ppTLC 0
>     prettyLang C = ppTLC 0
>     prettyLang F = ppTLF 0
>     prettyLang Pascal = ppTLPascal 0
>     prettyLang Java = ppTLJava 0



Prettyprinter für Pascal:

> ppTLPascal ind TLNil = "nil"
> ppTLPascal ind (TLLayout l)   = l
> ppTLPascal ind (TLComment cs) = concatMap (\c -> spc ind ++ "// " ++ c ++ "\n") cs
> ppTLPascal ind (TLVar n)   = ppVarAccessPascal n
> ppTLPascal ind (TLDecls ds)  = "var\n" ++ concatMap ppVD ds where
>                                    ppVD v = spc (ind +tlStdInd) ++ ppVarDeclPascal v ++ ";\n"
> ppTLPascal ind (TLTypeDecls ds)  = "type\n" ++ concatMap ppTD ds where
>                                    ppTD v = spc (ind +tlStdInd) ++ ppTypeDeclPascal v ++ ";\n"

> ppTLPascal ind (TLAssign n (TLFA f args)) = spc ind ++ ppVarAccessPascal n ++ " := " ++ ppTLPascal_fa f args ++ ";\n"
> ppTLPascal ind (TLAssign n a)             = spc ind ++ ppVarAccessPascal n ++ " := " ++ ppTLPascal ind a ++ ";\n"
> -- ppTLPascal ind (TLAssign n (TLFPA f args)) = spc ind ++ ppVarAccessPascal n ++ " = " ++ 
> --                                                 ppTLPascal_fa ("(&" ++ ppVarAccessPascal f ++ ")") args ++ ";\n"
> ppTLPascal ind (TLAssignIF n e v1 v2)     = ppTLPascal ind (TLIf e [TLAssign n v1] [TLAssign n v2])
> ppTLPascal ind (TLAlloc _ v e d)   = spc ind ++ "new(" ++ ppVarAccessPascal v ++ ");\n"
> ppTLPascal ind (TLExp m)   = prettyLang Pascal m
> ppTLPascal ind (TLFD cmt rt f params vdecls body rv) = 
>                                        ppTLPascal ind (TLComment cmt) ++ 
>                                        (if rt==TLVoid then "procedure " else "function ") 
>                                        ++ f ++  
>                                        (if (length params)>0 then 
>                                           "(" ++ sepList "; " 
>                                                  (map ppVarDeclPascal params) 
>                                            ++ ")" else []) ++
>                                        (if rt == TLVoid then "" else (":" ++ ppDataTypePascal rt)) ++  
>                                        ";\n" ++
>                                        (if (length vdecls)>0 then "var\n" else "") ++
>                                        (concatMap ppVD vdecls) ++ "\n" ++
>                                        "begin\n" ++
>                                        concatMap (ppTLPascal (ind+tlStdInd)) body ++
>                                        rv' ++  
>                                        "end;\n" where
>                                           ppVD v = spc (ind +tlStdInd) ++ ppVarDeclPascal v ++ ";\n"
>                                           rv' = if rt == TLVoid then [] else 
>                                                    spc (ind +tlStdInd) ++ f ++ " := " ++ ppTLPascal ind rv ++ ";\n"
>                                           
                                           
> ppTLPascal ind (TLFA f args)  = spc ind ++ ppTLPascal_fa f args ++ ";\n"
> ppTLPascal ind (TLIf b t e) = spc ind ++ "if " ++ prettyLang Pascal b ++ " then begin\n" 
>                                 ++ concatMap (ppTLPascal (ind+tlStdInd)) t ++ 
>                                    spc ind ++ "end" 
>                                    ++ if length e == 0 then ";\n" else "\n" ++ spc ind ++ "else begin\n" ++ 
>                                       concatMap (ppTLPascal (ind+tlStdInd)) e ++ spc ind ++  "end;\n"
> ppTLPascal ind (TLIfs b t e) = ppTLPascal ind (TLIf b t e)

> ppTLPascal ind (TLIfDef _ tl) = concatMap (ppTLPascal ind) tl
> ppTLPascal ind (TLFor f b e d) = spc ind ++ "for " ++ f ++ ":=" ++ ppMathExpTL Pascal b ++ direc ++ ppMathExpTL Pascal e ++ 
>                                      " do begin\n" ++ 
>                                           concatMap (ppTLPascal (ind+tlStdInd)) d ++ spc ind ++ "end;\n" 
>                                  where
>                                    direc = if e == (Number 0) then " downto " else " to "

> ppTLPascal ind (TLWhileNN v body) = spc ind ++ "while " ++ ppVarAccessPascal v ++ " <> nil do begin\n" ++ 
>                                            concatMap (ppTLPascal (ind+tlStdInd)) body ++ 
>                                   spc ind ++ "end;\n"
> ppTLPascal ind (TLWhile e body) = spc ind ++ "while " ++ prettyLang Pascal e ++ " do begin\n" ++ 
>                                            concatMap (ppTLPascal (ind+tlStdInd)) body ++ 
>                                   spc ind ++ "end;\n"
> ppTLPascal ind (TLDefines defs) = ppTLDefines ind ppTLPascal defs
> ppTLPascal ind (TLFPA f args) = spc ind ++ ppTLPascal_fa ("(&" ++ ppVarAccessPascal f ++ ")") args ++ ";\n"

> ppTLPascal ind (TLPrint f [TLFA ff args]) = spc ind ++ "write(" ++ ppTLPascal_fa ff args ++ ");\n"
> ppTLPascal ind (TLPrint f args) | args == [] = spc ind ++ "write('" ++ f ++ "');\n"
>                                    | otherwise  = spc ind ++ "write(" ++ mapsep ", " (ppTLPascal ind) args ++ ");\n"
> ppTLPascal ind (TLBlock tls)  = concatMap (ppTLPascal ind) tls

> ppTLPascal _ (TLMacro text) = ppTLMacro text

> ppTLPascal_fa f args  = f ++ "(" ++ sepList ", " (map (ppTLPascal 0) args) ++ ")"


Prettyprinter für C:

> ppTLC ind TLNil = "NULL"
> ppTLC ind (TLLayout l)   = l
> ppTLC ind (TLComment cs) = concatMap ppC cs where
>                                ppC c = spc ind ++ "/* " ++ c' ++ " */\n" where
>                                  c' = case c of
>                                        ('-':cs)  -> drop d1 $ take (l-d2) c
>                                        otherwise -> c
>                                  d1 = ind `div` 2
>                                  d2 = ind - d1
>                                  l  = length c
>                                  
> ppTLC ind (TLVar n)  = ppVarAccessC n
> ppTLC ind (TLDecls ds)  =  concatMap ppVD ds where
>                                    ppVD v = spc ind ++ "static " ++ ppVarDeclC v ++ ";\n"
> ppTLC ind (TLTypeDecls ds)  = concatMap ppTypeDeclC ds 

> ppTLC ind (TLAssign n (TLFA f args))      = spc ind ++ ppVarAccessC n ++ " = " ++ ppTLC_fa f                               args ++ ";\n"
> ppTLC ind (TLAssign n (TLFPA f args))     = spc ind ++ ppVarAccessC n ++ " = " ++ ppTLC_fa ("(*" ++ ppVarAccessC f ++ ")") args ++ ";\n"
> ppTLC ind (TLAssign n a)                  = spc ind ++ ppVarAccessC n ++ " = " ++ ppTLC ind a ++ ";\n"
> ppTLC ind (TLAssignIF n e v1 v2)          = spc ind ++ ppVarAccessC n ++ " = " ++ prettyLang C e ++ " ? " ++
>                                                        ppTLC ind v1 ++ " : " ++ ppTLC ind v2 ++ ";\n"
> ppTLC ind (TLAlloc mt v e d)  = spc ind ++ ppVarAccessC v ++ "=(" ++ ppDataTypeC (PointerOf d) ++ ") " ++
>                                 fctname ++ "(" ++ arg ++ mult ++ "sizeof(" ++ ppDataTypeC d ++ "));\n"
>   where 
>     fctname = case mt of
>                    MTDynamic -> "malloc"
>                    otherwise -> "myalloc"
>     arg = case mt of
>                   MTDynamic -> ""
>                   MTStatic  -> "adp_statmem, "
>                   MTTemp    -> "adp_dynmem, "
>     mult  = if e == (ExpNum 1) then "" else "(" ++ prettyLang C e ++ ") * "

> ppTLC ind (TLExp m)   = prettyLang C m
> ppTLC ind (TLFD cmt rt f params vdecls body rv) = 
>                                            ppTLC ind (TLComment cmt) ++ 
>                                            "static " ++ ppDataTypeC rt
>                                            ++ f ++  
>                                            "(" ++ sepList ", " 
>                                                  (map ppVarDeclC params) 
>                                            ++ ")"  ++
>                                           if body == [] then ";\n" else 
>                                        "\n" ++
>                                        "{\n" ++
>                                        (concatMap ppVD vdecls) ++ (if vdecls == [] then "" else "\n") ++
>                                        concatMap (ppTLC (ind+tlStdInd)) body ++
>                                        rv' ++ 
>                                        "}\n" where
>                                           ppVD v = spc (ind +tlStdInd) ++ ppVarDeclC v ++ ";\n"
>                                           rv' = if rt == TLVoid then [] else 
>                                                    spc (ind +tlStdInd) ++ "return " ++ ppTLC' rv ++ ";\n"
>                                           ppTLC' (TLFA f args) = ppTLC_fa f args 
>                                           ppTLC' x             = ppTLC  ind x
>                                           

> ppTLC ind (TLFA f args)  = spc ind ++ ppTLC_fa f                               args ++ ";\n"
> ppTLC ind (TLFPA f args) = spc ind ++ ppTLC_fa ("(*" ++ ppVarAccessC f ++ ")") args ++ ";\n"

> ppTLC ind (TLIf b t e) = spc ind ++ "if (" ++ prettyLang C b ++ ") {\n" 
>                                  ++ concatMap (ppTLC (ind+tlStdInd)) t ++ 
>                                    spc ind ++ "}" ++ if length e == 0 then "\n" else "\n" ++ spc ind ++ "else {\n" ++ 
>                                    concatMap (ppTLC (ind+tlStdInd)) e ++ spc ind ++  "}\n"


> ppTLC ind (TLIfs b t e) = case (checkSwitch Nothing b e) of
>   Nothing -> spc ind ++ "if (" ++ prettyLang C b 
>                 ++ ") {\n" 
>                 ++ concatMap (ppTLC (ind+tlStdInd)) t ++ 
>                 spc ind ++ "}" ++ 
>                 if length e == 0 then ";\n" 
>                                  else " else \n" ++ 
>                                  concatMap (ppTLC ind) e
>   Just x ->  spc ind ++ "switch (" ++ x ++ ") {\n"
>                 ++ ppCaseC (ind + tlStdInd) b t e
>                 ++ spc ind ++ "}\n"

>  where
>    checkSwitch a 
>      (ExpIOp lhs ('=':'=':[]) (ExpTLVar (Direct ('S':'I':'G':'I':'D':_))))
>      rest
>      | rest == [] = a
>      | a == Nothing = checkSwitch (Just name) exp' rest'
>      | otherwise = if c == name then checkSwitch a exp' rest'
>                                 else Nothing
>      where
>        ((TLIfs exp' _ rest'):_) = rest
>        (Just c) = a
>        name = prettyLang C lhs
>    checkSwitch _ _ _ = Nothing

>    ppCaseC ind (ExpIOp _ ('=':'=':[]) (ExpTLVar (Direct (y)))) cond rest 
>      = spc ind ++ "case " ++ y ++ ":\n"
>             ++ concatMap (ppTLC (ind + tlStdInd)) cond
>             ++ spc (ind + tlStdInd) ++ "break;\n"
>             ++ if rest == [] then ""
>                              else ppCaseC ind exp' cond' rest'
>      where
>        ((TLIfs exp' cond' rest'):_) = rest


> ppTLC ind (TLIfDef d t) = spc ind ++ "#ifdef " ++ d ++ "\n" ++
>                                   concatMap (ppTLC (ind+tlStdInd)) t ++
>                           spc ind ++ "#endif\n"

> ppTLC ind (TLFor f b e d) = spc ind ++ "for (" ++ f ++ "=" ++ ppMathExpTL C b ++ "; " ++ f ++ direc ++ ppMathExpTL C e ++ "; " ++ f ++ direc' ++ ")" ++ 
>                                      " {\n" ++ 
>                                           concatMap (ppTLC (ind+tlStdInd)) d ++ spc ind ++ "}\n" 
>                                  where
>                                    direc  = if e == (Number 0) then ">=" else "<="
>                                    direc' = if e == (Number 0) then "--" else "++"
> ppTLC ind (TLForI f b e inc d) 
>                           = spc ind ++ "for (" ++ f ++ "=" ++ ppMathExpTL C b ++ "; " ++ f ++ direc ++ ppMathExpTL C e ++ "; " ++ f ++ direc' ++ ")" ++ 
>                                      " {\n" ++ 
>                                           concatMap (ppTLC (ind+tlStdInd)) d ++ spc ind ++ "}\n" 
>                                  where
>                                    direc  = if e == (Number 0) then ">=" else "<="
>                                    direc' = " += " ++ ppMathExpTL C inc 

> ppTLC ind (TLWhileNN v body) = spc ind ++ "while (" ++ ppVarAccessC v ++ " != NULL) {\n" ++ 
>                                            concatMap (ppTLC (ind+tlStdInd)) body ++ 
>                                   spc ind ++ "}\n"

> ppTLC ind (TLWhile e body)   = spc ind ++ "while (" ++ prettyLang C e ++ ") {\n" ++ 
>                                            concatMap (ppTLC (ind+tlStdInd)) body ++ 
>                                   spc ind ++ "}\n"
> ppTLC ind (TLBreak)          = spc ind ++ "break;\n"

> ppTLC ind x@(TLDefines defs)   = case (ppTLCEnum ind x) of
>                                   Nothing -> ppTLDefines ind ppTLC defs -- ++ ppTLDefinesCDebug ind defs
>                                   Just x  -> x
>  where

>   ppTLCEnum :: Int -> TL -> Maybe String
>   ppTLCEnum ind (TLDefines defs)
>     | enumCheck defs == True
>        = Just ("enum sigid {\n" ++ sepList ",\n" (map (ppEnumElem ind') defs)
>             ++ "\n};\n")
>     | otherwise = Nothing
>    where
>     enumCheck xs = and $ map eCheck xs
>     eCheck (_, 'S':'I':'G':'I':'D':'_':xs, [], def) = True
>     eCheck (_, _, _, _) = False
>     ppEnumElem :: Int -> (DataType, String, [String], [TL]) -> String
>     ppEnumElem ind (_, name, [], _) = spc ind ++ name
>     ind' = ind + tlStdInd


>   ppTLDefinesCDebug i defs = concatMap (ppDefDebug i) defs
>     where
>       ppDefDebug :: Int -> (DataType, String, [String], [TL]) -> String
>       ppDefDebug i (datatype, name, args, (stat:[])) =
>         spc i ++ "void " ++ name ++ "_setD(" ++ (idxs args) ++ ", struct " ++ (dt datatype) ++ " x)" ++
>         "\n" ++ spc i ++ "{\n" ++
>         spc j ++ (side stat) ++ " = x;\n" ++
>         "fprintf(stderr, \"" ++ name ++ ": %d %d\", I, J);" ++ 
>         spc j ++ (dt datatype) ++ "_print(x);" ++
>         spc i ++ "}\n\n\n"
>         where j = tlStdInd + i
>       dt (StructOf foo _) = foo
>       dt datatype = ppDataTypeC datatype ++ " "
>       idxs args = sepList ", " (map ("int " ++) args)
>       side stat = ppTLC 0 stat


> ppTLC ind (TLPrint f [TLFA ff args]) = spc ind ++ "printf(\"" ++ f ++ "\", " ++ ppTLC_fa ff args ++ ");\n"
> ppTLC ind (TLPrint f args) | args == [] = spc ind ++ "printf(\"" ++ f ++ "\");\n"
>                               | otherwise  = spc ind ++ "printf(\"" ++ f ++ "\", " ++ mapsep ", " (ppTLC ind) args ++ ");\n"
> ppTLC ind (TLBlock tls)  = concatMap (ppTLC ind) tls

> ppTLC _ (TLMacro text) = ppTLMacro text

> ppTLC_fa f args  = f ++ "(" ++ sepList ", " (map (ppTLC' 0) args) ++ ")"
>   where
>     ppTLC' 0 (TLFA f args) = ppTLC_fa f args
>     ppTLC' 0 x             = ppTLC 0 x



Prettyprinter für Fortran:

> ppTLF ind TLNil = "NULL"
> ppTLF ind (TLLayout l)   = l
> ppTLF ind (TLComment cs) = concatMap ppF cs where
>                                ppF c = spc ind ++ "! " ++ c' ++ " !\n" where
>                                  c' = case c of
>                                        ('-':cs)  -> drop d1 $ take (l-d2) c
>                                        otherwise -> c
>                                  d1 = ind `div` 2
>                                  d2 = ind - d1
>                                  l  = length c
>                                  
> ppTLF ind (TLVar n)  = ppVarAccessF n
> ppTLF ind (TLDecls ds)  =  concatMap ppVD ds where
>                                    ppVD v = spc ind ++ ppVarDeclF v ++ "\n"
> ppTLF ind (TLTypeDecls ds)  = concatMap ppTypeDeclF ds 

> ppTLF ind (TLAssign n (TLFA f args))      = spc ind ++ ppVarAccessF n ++ " = " ++ ppTLF_fa f                               args ++ "\n"
> ppTLF ind (TLAssign n (TLFPA f args))     = spc ind ++ ppVarAccessF n ++ " = " ++ ppTLF_fa ("(*" ++ ppVarAccessF f ++ ")") args ++ "\n"
> ppTLF ind (TLAssign n a)                  = spc ind ++ ppVarAccessF n ++ " = " ++ ppTLF ind a ++ "\n"
> ppTLF ind (TLAssignIF n e v1 v2)          = spc ind ++ ppVarAccessF n ++ " = " ++ 
>                                                           "MERGE(" ++ ppTLF ind v1 ++ " , " ++ ppTLF ind v2 ++ " , " ++ prettyLang F e ++ ")"
>                                                           ++ "\n"

> ppTLF ind (TLAlloc _ v e d)  = spc ind ++ "ALLOCATE(" ++ ppVarAccessF v ++ "(0 : " ++ prettyLang F (ExpME $ size :- (Number 1)) ++ "))\n" 
>       where size = case e of
>                         ExpME s  -> s
>                         otherwise -> error $ "ppTLF: unknown table dimension: " ++ show e
> ppTLF ind (TLAllocArr v dims) = spc ind ++ "ALLOCATE(" ++ ppVarAccessF v ++ "(" ++ mapsep ", " ppDim dims ++ "))\n"
>       where ppDim (b,e) = prettyLang F b ++ " : " ++ prettyLang F e

> ppTLF ind (TLExp m)   = prettyLang F m
> ppTLF ind (TLFD cmt rt f params vdecls body rv) = 
>                                        ppTLF ind (TLComment cmt) ++ 
>                                        (if procedure then "SUBROUTINE " else "FUNCTION ") 
>                                        ++ f
>                                        ++ "(" ++  sepList ", " (concatMap fst params) ++ ")"  ++
>                                        (if procedure then "" else " RESULT(" ++ resultVar ++ ")" ) 
>                                        ++ "\n" ++
>                                        spc (ind +tlStdInd) ++ "IMPLICIT NONE\n" ++
>                                        (concatMap ppVD params) ++ "\n" ++
>                                        (if procedure then "" else ppVD ([resultVar], rt) ++ "\n") ++
>                                        (concatMap ppVD vdecls) ++ (if vdecls == [] then "" else "\n") ++
>                                        concatMap (ppTLF (ind+tlStdInd)) body ++
>                                        rv' ++ 
>                                        "END SUBROUTINE\n" 
>                                        where
>                                           procedure = rt==TLVoid
>                                           resultVar = "RES_" ++ f
>                                           ppVD v = spc (ind +tlStdInd) ++ ppVarDeclF v ++ "\n"
>                                           rv' = if rt == TLVoid then [] else 
>                                                    spc (ind +tlStdInd) ++ resultVar ++ " = " ++ ppTLF' rv ++ "\n"
>                                           ppTLF' (TLFA f args) = ppTLF_fa f args 
>                                           ppTLF' x             = ppTLF  ind x
>                                           

> ppTLF ind (TLFA f args)  = spc ind ++ "CALL " ++ ppTLF_fa f                    args ++ "\n"
> ppTLF ind (TLFPA f args) = spc ind ++ ppTLF_fa ("(*" ++ ppVarAccessF f ++ ")") args ++ "\n"

> ppTLF ind (TLIf b t e) = spc ind ++ "IF (" ++ prettyLang F b ++ ") THEN\n" 
>                                  ++ concatMap (ppTLF (ind+tlStdInd)) t ++ 
>                                    spc ind ++ if length e == 0 then "END IF\n" else "\n" ++ spc ind ++ "ELSE \n" ++ 
>                                    concatMap (ppTLF (ind+tlStdInd)) e ++ spc ind ++  "END IF\n"
> ppTLF ind (TLIfs b t e) = spc ind ++ "IF (" ++ prettyLang F b ++ ") THEN\n" 
>                                   ++ concatMap (ppTLF (ind+tlStdInd)) t ++ 
>                                    spc ind ++ if length e == 0 then "END IF\n" else "ELSE\n" ++ 
>                                    concatMap (ppTLF ind) e 
> ppTLF ind (TLIfDef d t) = spc ind ++ "#ifdef " ++ d ++ "\n" ++
>                                   concatMap (ppTLF (ind+tlStdInd)) t ++
>                           spc ind ++ "#endif\n"

> ppTLF ind (TLFor f b e d) = spc ind ++ "DO " ++ f ++ "=" ++ ppMathExpTL F b ++ ", " ++ ppMathExpTL F e ++ ", " ++ direc ++ "\n" ++
>                                       concatMap (ppTLF (ind+tlStdInd)) d ++ spc ind ++ "END DO\n" 
>                                  where
>                                    direc = if e == (Number 0) then "-1" else "1"

> ppTLF ind (TLWhileNN v body) = spc ind ++ "while (" ++ ppVarAccessF v ++ " != NULL) {\n" ++ 
>                                            concatMap (ppTLF (ind+tlStdInd)) body ++ 
>                                   spc ind ++ "}\n"

> ppTLF ind (TLWhile e body)   = spc ind ++ "while (" ++ prettyLang F e ++ ") {\n" ++ 
>                                            concatMap (ppTLF (ind+tlStdInd)) body ++ 
>                                   spc ind ++ "}\n"

> ppTLF ind (TLDefines defs)   = ppTLDefines ind ppTLF defs

> ppTLF ind (TLPrint f [TLFA ff args]) = spc ind ++ "PRINT*, (" ++ ppTLF_fa ff args ++ ")\n"
> ppTLF ind (TLPrint f args) | args == [] && f == "\\n" = spc ind ++ "PRINT*\n"
>                               | args == []               = spc ind ++ "PRINT*, '" ++ f ++ "'\n"
>                               | otherwise                = spc ind ++ "PRINT*, (" ++ mapsep ", " (ppTLF ind) args ++ ")\n"
> ppTLF ind (TLBlock tls)  = concatMap (ppTLF ind) tls

> ppTLF _ (TLMacro text) = ppTLMacro text

> ppTLF_fa f args  = f ++ "(" ++ sepList ", " (map (ppTLF' 0) args) ++ ")"
>   where
>     ppTLF' 0 (TLFA f args) = ppTLF_fa f args
>     ppTLF' 0 x             = ppTLF 0 x





Java Pretty-Printer:
XXX Ein Modul je Language waere schoen  ...

> ppTLJava :: Int -> TL -> String

> ppTLJava ind (TLLayout text) = spc ind ++ text

> ppTLJava ind (TLComment comms) = spc ind ++ "/* " 
>   ++ unlines  (map (spc (ind + 3) ++) comms) ++ " */\n"

> ppTLJava ind (TLTypeDecls tds) = concatMap (ppTypeDeclJava ind) tds

> ppTLJava ind (TLFD _ _ name  _ [] [] _) = spc ind ++
>   "/* Filtered Forward declaration: " ++ name ++ " */\n"

XXX per pattern matching waechter => einzelne pattern

> ppTLJava ind (TLFD comments dt name params head body ret)
>  | filterFn name == True = "/* Filtered: " ++ name ++ " */\n"

>  | usedAsFnPointer name == True =
>      spc ind ++ "class " ++ klasse ++ " implements Backtrace {\n" ++
>      spc i ++ "public str1 back(int i, int j, int diff) {\n" ++
>      concatMap (++";\n") (concatMap (ppVarDeclJava j) head) ++
>      "\n" ++
>      concatMap (ppTLJava j) body ++
>      spc j ++ ppJret ret ++
>      spc i ++ "}\n" ++
>      spc ind ++ "}\n" ++
>      "\n" ++ spc ind ++ klasse ++ " " ++ name ++ " = new " ++ klasse ++ 
>      "();" ++ "\n\n"

>  | otherwise =  (if comments /= [] then ppTLJava ind (TLComment comments) 
>                                   else "") ++
>      spc ind ++ public name ++ ppDataTypeJava dt ++ " " ++ name ++ "(" ++
>      sepList ", " (concatMap (ppVarDeclJava 0) params) ++ ")\n" ++
>      spc ind ++ "{\n" ++
>      (concatMap (++";\n") (concatMap (ppVarDeclJava (ind + tlStdInd)) head))
>      ++ "\n" ++

>      (concatMap (ppTLJava (ind + tlStdInd)) body) ++

>      spc i ++ ppJret ret ++

>      spc ind ++ "}"


>  where
>    filterFn ('s':'o':'r':'t':'_':_) = True 
>    filterFn ('n':'u':'b':'_':_) = True

    filterFn ('f':'r':'e':'e':'a':'l':'l':[]) = True

>    filterFn "free_str_Signature" = True
>    filterFn _ = False

>    usedAsFnPointer ('b':'a':'c':'k':'_':_) = True
>    usedAsFnPointer _ = False

>    ppJret (TLVar (Direct ('_':[]), TLVoid)) = ""
>    ppJret foo = "return " ++ ppJFA foo ++ ";\n"
>      where
>        ppJFA (TLFA name params) = name ++ "(" ++ 
>          mapsep ", " ppJFA params ++ ")"
>        ppJFA foo = ppTLJava 0 foo

>    i = ind + tlStdInd
>    j = i + tlStdInd
>    klasse = "bt_" ++ name

>    public x = if isPrefixes ["mainloop", "freeall"] x then "public "
>                                                       else ""


> ppTLJava ind (TLVar vaccess) = ppVarAccessJava vaccess

enthaelt Backtrace Sonderbehandlungen

> ppTLJava ind (TLDecls decls) = concatMap ppDecl decls
>   where
>     ppDecl (names, foo)
>        = concatMap (++";\n") (map (ppElem foo) names)
>       where
>         ppElem foo@(PointerOf (PointerOf (PointerOf (StructOf dt [])))) name
>           | name == "pp_init"
>               = spc ind ++ dt ++ "[] " ++ name ++ ";\n" ++
>                 spc ind ++ "SetStr1[] " ++ name ++ "_set"
>           | otherwise = "/* FIXME TLDecl: " ++ (show foo) ++ "*/\n"
>         ppElem foo@(PointerOf (PointerOf (StructOf dt []))) name 
>           | isSpecial name = spc ind ++ dt ++ " " ++ name ++ ";\n" ++
>                              spc ind ++ "SetStr1 " ++ name ++ "_set"
>           | otherwise = head $ ppVarDeclJava ind ([name], foo) 
>           where
>             isSpecial = isPrefixes ["pp_next", "removeAddr"]
>         ppElem foo name = head $ ppVarDeclJava ind ([name], foo)



> ppTLJava ind (TLDefines defs) = concatMap (ppDef ind) defs
>   where
>     ppDef :: Int -> (DataType, String, [String], [TL]) -> String
>     ppDef i (_, name, [], (def:[])) = spc i ++
>       "final static int " ++ name ++ " = " ++ ppTLJava 0 def ++ ";\n"

Java function instead of C-Macro, which should be get inlined
 or else the java (JIT or whatever) Compiler is really stupid ...

>     ppDef i foo@(datatype, name, args, (stat:[])) =
>       spc i ++ (dt datatype) ++ name ++ "(" ++ (idxs args) ++ ")\n{\n" ++
>       spc j ++ "return " ++ (side stat) ++ ";\n" ++
>       spc i ++ "}\n" ++

>       spc i ++ "void " ++ name ++ "_set(" ++ (idxs args) ++ ", " ++ (dt datatype) ++ " x)" ++
>       "\n{\n" ++
>       spc j ++ (side stat ) ++ " =  x;\n" ++
>       spc i ++ "}\n\n\n" -- ++ ppDefDebug i foo
>       where j = tlStdInd + i

>     ppDef i foo@(_, name, args, def) = "/* FIXME TLDefines: " ++ (show foo) ++ " */\n"

>     dt datatype = ppDataTypeJava datatype ++ " "
>     idxs args = sepList ", " (map ("int " ++) args)
>     side stat = ppTLJava 0 stat

>     ppDefDebug :: Int -> (DataType, String, [String], [TL]) -> String
>     ppDefDebug i (datatype, name, args, (stat:[])) =
>       spc i ++ "void " ++ name ++ "_setD(" ++ (idxs args) ++ ", " ++ (dt datatype) ++ " x)" ++
>       "\n" ++ spc i ++ "{\n" ++
>       spc j ++ (side stat) ++ " = x;\n" ++
>       spc j ++ "System.err.println(\"" ++ name ++ "\" + \": \" + I + \" \" + J + \"\" + x);" ++
>       spc i ++ "}\n\n\n"
>       where j = tlStdInd + i




Backtrace Sonderbehandlung

> ppTLJava ind 
>  (TLAlloc _ (Direct name ,_) mexp (PointerOf (PointerOf (StructOf dt [])))) =
>    unlines [ spc ind ++ n ++ " = new " ++ d ++ "[" ++ pretty mexp ++ "];" |
>              (n, d) <- [ (name ++ "_set", "SetStr1"), (name, dt) ] ]

> ppTLJava ind (TLAlloc _ vAccess (ExpNum 1.0) dt) =
>   spc ind ++ ppVarAccessJava vAccess ++ " = new " ++ 
>   ppDataTypeJava dt ++ "();\n"

> ppTLJava ind (TLAlloc _ (Direct name, PointerOf (StructOf dt _ )) mexp _) =
>   spc ind ++ name ++ " = new " ++ dt ++ "[" ++ pretty mexp ++ "];\n"

case for tupel

> ppTLJava ind (TLAlloc _ (Direct name, StructOf dt _ ) mexp _) =
>   spc ind ++ name ++ " = new " ++ dt ++ "[" ++ pretty mexp ++ "];\n"

> ppTLJava ind foo@(TLAlloc _ vAccess size dt)
>   | isElemDt dt == True = spc ind ++ ppVarAccessJava vAccess ++

>       " = new " ++ ppDataTypeJava dt ++ "[" ++ prettyLang Java size ++ "];\n"
>   | otherwise = "/* FIXME TAlloc 1: " ++ (show foo) ++ " */\n"
>  where
>    isElemDt TLInt  = True
>    isElemDt TLReal = True
>    isElemDt _      = False

Java TLAssign Sonderbehandlungen:

C-String pointer arithmethik

Strings in Java

> ppTLJava ind (TLAssign (Direct _, TLVoid) 
>    (TLExp (ExpIOp (ExpTLVar (Direct _)) ('+':[]) 
>      (ExpPOp "strlen" [ExpTLVar (Direct _)])))) =
>   spc ind ++ "/* Filtered char pointer arithmetic: TLAssign */\n"

We use StringBuilder instead of StringBuffer, because the latter
is synchronized which we don't need and only costs performance
(the API is the same)

> ppTLJava ind (TLAssign (ArrayElem ((Number 0):[]) (Direct name),TLVoid)
>                                    (TLExp (ExpNum 0.0)))  =
>   spc ind ++ name ++ " = new StringBuilder(n*3);\n"

Backtrace Sonderbehandlung:

Backtrace-Get

> ppTLJava ind (TLAssign vAccess
>               (TLVar ((Address x),_))) =
>   spc ind ++ name ++ " = " ++ full ++ ";\n" ++
>   spc ind ++ combined "_set"  vAccess ++ " = " ++ s ++ 
>   ".proxy_" ++ t ++ ";\n" ++ " /* " ++ (show x) ++ " */\n"
>   where
>     (name, full, left, right) = (ppVarAccessJava vAccess,
>                            prettyLang Java x,
>                            sepList "." (take (length l-1) l),
>                            last l )
>       where
>         l = ppVAccessListJava x
>     combined n (ArrayElem a b,_) = prettyLang Java b ++ n ++ "[" ++ 
>                                       prettyLang Java (head a) ++ "]"
>     combined n _ = name ++ n 
>     divide (a :. b) = (prettyLang Java a, prettyLang Java b)
>     (s, t) = divide x


Backtrace-Set

> ppTLJava ind (TLAssign vAccess@((Pointer (Direct name)),_) 
>                        rhs@(TLVar (Direct new,TLVoid))) =
>   spc ind ++ name ++ "_set.set(" ++ new ++ ");\n"

> ppTLJava ind (TLAssign (Pointer (ArrayElem [Var a] 
>       (Direct src)),TLVoid)
>       (TLVar (ArrayElem [Var b] (Direct dest),TLVoid))) =
>   spc ind ++ src ++ "_set[" ++ a ++ "].set(" ++ dest ++ "[" ++ b ++ "]);\n"

shallow copy like used in copy_str_Signature

> ppTLJava ind (TLAssign (Pointer foo, _) (TLVar (Pointer bar, _))) =
>   spc ind ++ ppVAccessJavaLHS foo ++ " = " ++ 
>   prettyLang Java bar ++ ".copy();\n"

if LHS side is used as a C-Macro ...

> ppTLJava ind (TLAssign ((VANonterm name (ST(i, j))),_) bar) =
>   spc ind ++ name ++ "_set(" ++ prettyLang Java i ++ ", " ++ 
>   prettyLang Java j ++ ", " ++ ppTLJava 0 bar ++ ");\n"

Standardbehandlung und Rest, wo Pattern-Matching nicht ausreicht

> ppTLJava ind (TLAssign vAccess rhs)

>   | checkMinMax rhs = spc ind ++ ppMinMax vAccess rhs ++ ";\n"

>   | otherwise =
>      spc ind ++ ppVarAccessJavaLHS vAccess ++ " = " ++ 
>      (takeWhile (/= ';') $ ppTLJava 0 rhs) ++ ";\n"

>   where
>     checkMinMax (TLExp (ExpNum (-1.0e7))) = True
>     checkMinMax (TLExp (ExpNum (1.0e7)))  = True
>     checkMinMax _ = False
>     ppMinMax foo@(_, dt) (TLExp (ExpNum x))  =
>       ppVarAccessJavaLHS foo ++ " = " ++ ppRhs dt x
>       where

XXX Big Problem: possible integer overflows

is 1^10 000 000 even 'save'?

         ppRhs TLInt (1.0e7) = "Integer.MAX_VALUE"
         ppRhs TLInt (-1.0e7) = "Integer.MIN_VALUE"

>         ppRhs TLInt (1.0e7) = "10000000"
>         ppRhs TLInt (-1.0e7) = "(-10000000)"

>         ppRhs TLReal (1.0e7) = "Double.POSITIVE_INFINITY"
>         ppRhs TLReal (-1.0e7) = "Double.NEGATIVE_INFINITY"

>     ppMinMax foo bar = " /* FIXME ppMinMax: " ++ (show foo) ++ " " ++ (show bar) ++ " */ "


> ppTLJava ind (TLAssignIF vAccess exp cond alt) =
>   spc ind ++ ppVarAccessJava vAccess ++ " = " ++ prettyLang Java exp ++
>   " ? " ++ ppTLJava 0 cond ++ " : " ++ ppTLJava 0 alt ++ ";\n"

> ppTLJava ind TLNil = "null"

> ppTLJava ind (TLExp exp) = prettyLang Java exp

Strings in Java

> ppTLJava ind (TLFA ('s':'p':'r':'i':'n':'t':'f':[]) (name:params)) =
>   spc ind ++ ppTLJava 0 name ++ ".append(" ++ 
>   (takeWhile (/= ';') $ ppRhs params) ++ ");\n"
>   where
>     ppRhs (a:[]) = prettyLang Java a
>     ppRhs (_:b:[]) = ppTLJava 0 b

because of the garbage collection we need to null dangling references

> ppTLJava ind (TLFA "free_str_Signature" (_:name:_)) =
>   spc ind ++ ppTLJava 0 name ++ " = null; /* was free_str_Signature */\n"

> ppTLJava ind (TLFA "free" (name:[])) = 
>   spc ind ++ ppTLJava 0 name ++ " = null; /* was free() */\n"

> ppTLJava ind (TLFA name params) 
>  | isPrefixes ["memory_clear", "adplib_free"] name = "/* filtered: " ++
>      name ++ " */\n"
>  | otherwise =
>   spc ind ++ backName name ++ "(" ++ 
>   sepList ", " (map (ppTLJava 0) params) ++ ");\n"
>   where
>     backName n@('b':'a':'c':'k':'_':_) = n ++ ".back"
>     backName n = n

> ppTLJava ind (TLBlock statements) = concatMap (ppTLJava ind) statements


> ppTLJava ind (TLIf exp cond alt) =
>   spc ind ++ "if (" ++ prettyLang Java exp ++ boolCase exp ++ ") {\n" ++
>   concatMap (ppTLJava i) cond ++
>   spc ind ++ "}" ++
>   (if alt == [] then "\n" else " else {\n" ++ concatMap (ppTLJava i) alt ++
>                               spc ind ++ "}\n")
>   where
>     i = tlStdInd + ind

XXX well, dirty Hack - how to fix this? Change Codegen.lhs!

>     boolCase (ExpIOp _ _ _) = ""
>     boolCase foo@(ExpTLVar (Pointer (Direct "l") :. Direct "next")) = 
>       " != null"
>     boolCase foo@(ExpTLVar _) = " != 0 /*" ++ (show foo) ++ "*/"
>     boolCase foo@(ExpVar name)  = (if name == "rmAllowed" then " != 0 " 
>       else " != null " ) ++ "/*" ++ (show foo) ++ "*/"
>     boolCase foo@(ExpPOp _ _) = "" ++ " /*" ++ (show foo) ++ "*/"
>     boolCase foo = " != null /*" ++ (show foo) ++ "*/"




> ppTLJava ind (TLIfs b t e) = case (checkSwitch Nothing b e) of
>   Nothing -> spc ind ++ "if (" ++ prettyLang Java b 
>                 ++ ") {\n" 
>                 ++ concatMap (ppTLJava (ind+tlStdInd)) t ++ 
>                 spc ind ++ "}" ++ 
>                 if length e == 0 then ";\n" 
>                                  else " else \n" ++ 
>                                  concatMap (ppTLJava ind) e
>   Just x ->  spc ind ++ "switch (" ++ x ++ ") {\n"
>                 ++ ppCaseJava (ind + tlStdInd) b t e
>                 ++ spc ind ++ "}\n"

>  where
>    checkSwitch a 
>      (ExpIOp lhs ('=':'=':[]) (ExpTLVar (Direct ('S':'I':'G':'I':'D':_))))
>      rest
>      | rest == [] = a
>      | a == Nothing = checkSwitch (Just name) exp' rest'
>      | otherwise = if c == name then checkSwitch a exp' rest'
>                                 else Nothing
>      where
>        ((TLIfs exp' _ rest'):_) = rest
>        (Just c) = a
>        name = prettyLang Java lhs
>    checkSwitch _ _ _ = Nothing

>    ppCaseJava ind (ExpIOp _ ('=':'=':[]) (ExpTLVar (Direct (y)))) cond rest 
>      = spc ind ++ "case " ++ y ++ ":\n"
>             ++ concatMap (ppTLJava (ind + tlStdInd)) cond
>             ++ spc (ind + tlStdInd) ++ "break;\n"
>             ++ if rest == [] then ""
>                              else ppCaseJava ind exp' cond' rest'
>      where
>        ((TLIfs exp' cond' rest'):_) = rest


> ppTLJava ind (TLWhileNN va stats) = spc ind ++ "while (" ++ 
>   ppVarAccessJava va ++
>   "!= null) {\n" ++
>   concatMap (ppTLJava (ind + tlStdInd)) stats ++
>   spc ind ++ "}\n"

> ppTLJava ind (TLWhile exp stats) =
>   spc ind ++ "while (" ++ prettyLang Java exp ++ ") {\n" ++
>   concatMap (ppTLJava (ind + tlStdInd)) stats ++
>   spc ind ++ "}\n"


> ppTLJava ind (TLFor counter init cond stats) =
>   spc ind ++ "for (" ++ counter ++ " = " ++ prettyLang Java init ++ "; " ++ 
>   counter ++ (if cond == (Number 0) then ">=" else " <= ") ++

               ^^^  lol!

>   prettyLang Java cond ++ "; " ++
>   counter ++ (if cond == (Number 0) then "--" else "++") ++ ") {\n" ++
>   concatMap (ppInside (ind + tlStdInd)) stats ++
>   spc ind ++ "}\n"

special treatment for Tupel TLAssignIf stmts inside loops -> need to copy them
e.g.
for (k = i+1; k <= j-8; k++)
  v4 = v1.tup1 < v4.tup1 ? v1 : v4

>   where
>    ppInside ind foo@(TLAssignIF (Direct lhs, StructOf ('t':'u':'p':'e':'l':_) []) exp (TLVar (Direct r1, StructOf _ _)) (TLVar (Direct r2, StructOf _ _)))
>     | lhs == r2 =
>        spc ind ++ "if (" ++ prettyLang Java exp ++ ")\n" ++ 
>        spc (ind+tlStdInd) ++ lhs ++ " = " ++ r1 ++ ".copy();\n"
>     | otherwise = ppTLJava ind foo
>    ppInside ind foo = ppTLJava ind foo


> ppTLJava ind (TLForI counter init cond inkr stats) =
>   spc ind ++ "for (" ++ counter ++ " = " ++ pretty init ++ "; " ++
>   counter ++ " <= " ++ prettyLang Java cond ++ "; " ++
>   counter ++ " += " ++ prettyLang Java  inkr ++ ") {\n" ++
>   concatMap (ppTLJava (ind + tlStdInd)) stats ++
>   spc ind ++ "}\n"


> ppTLJava ind (TLFPA va stats) = spc ind ++ ppVarAccessJava va ++ ".back(" ++
>   sepList ", " (map (ppTLJava 0) stats) ++ ");\n"

> ppTLJava _ (TLMacro text) = ppTLMacro text


> ppTLJava ind tl = "/* FIXME Target Code\n" ++ (show tl) ++ "\n*/\n"










Alle Zielsprachen: 

> ppTLMacro text = text ++ "/* think about filtering this instead of using sed ;) */"

> ppTLDefines ind pp defs = concatMap ppDef defs where
>                                    ppDef (_, n,args,tls) = spc ind ++ "#define " ++ n ++ optArgs ++ " " ++ def ++ "\n"
>                                      where optArgs | args == [] = "" 
>                                                    | otherwise  = "(" ++ sepList ", " args ++ ")"
>                                            def     | length tls == 1  = pp ind $ head tls
>                                                    | otherwise        = "\n" ++ mapsep "\n\\" (pp $ ind + tlStdInd) tls


Wrapper-Funktion:

> ppTL :: Output -> TL -> String
> ppTL C       = ppTLC 0
> ppTL F       = ppTLF 0
> ppTL Pascal  = ppTLPascal 0
> ppTL Java    = ppTLJava 0

-------------------------------------------------------------------------
prettyprinter fuer MathExp - modifiziert fuer lookahead-Tabellenzugriffe:

> ppMathExpTL :: Output -> MathExp -> String
> ppMathExpTL tl m             = ppMathExpTL' tl (calcME m)

> ppMathExpTL' :: Output -> MathExp -> String
> ppMathExpTL' tl Infinite     = "Infinite"
> ppMathExpTL' tl (Number a)   = show a
> ppMathExpTL' tl (Var a)      = a

> ppMathExpTL' tl (a :+ b)     = ppMathExpTL' tl a ++ "+" ++ ppMathExpTL' tl b
> ppMathExpTL' tl (a :* b)     = a' ++ "*" ++ b' 
>                           where
>                             a' = if atomarME a then ppMathExpTL' tl a else "(" ++ ppMathExpTL' tl a ++ ")"
>                             b' = if atomarME b then ppMathExpTL' tl b else "(" ++ ppMathExpTL' tl b ++ ")"
> ppMathExpTL' tl (a :/ b)     = a' ++ "/" ++ b' 
>                           where
>                             a' = if atomarME a then ppMathExpTL' tl a else "(" ++ ppMathExpTL' tl a ++ ")"
>                             b' = if atomarME b then ppMathExpTL' tl b else "(" ++ ppMathExpTL' tl b ++ ")"


> ppMathExpTL' tl (a :- b)     = ppMathExpTL' tl a ++ "-" ++ b' 
>                           where
>                             b' = if atomarME b then ppMathExpTL' tl b else "(" ++ ppMathExpTL' tl b ++ ")"

> ppMathExpTL' tl (Max a b)         = "max(" ++ ppMathExpTL' tl a ++ ", " ++ ppMathExpTL' tl b ++ ")"
> ppMathExpTL' tl (Min a b)         = "min(" ++ ppMathExpTL' tl a ++ ", " ++ ppMathExpTL' tl b ++ ")"

> ppMathExpTL' C (LA f LAL (i, j))      = prettyLang C      (ArrayElem [i, j, Number 0] (Direct f))
> ppMathExpTL' C (LA f LAU (i, j))      = prettyLang C      (ArrayElem [i, j, Number 1] (Direct f))
> ppMathExpTL' F (LA f LAL (i, j))      = prettyLang F      (ArrayElem [i, j, Number 0] (Direct f))
> ppMathExpTL' F (LA f LAU (i, j))      = prettyLang F      (ArrayElem [i, j, Number 1] (Direct f))
> ppMathExpTL' Pascal (LA f LAL (i, j)) = prettyLang Pascal (ArrayElem [i, j, Number 0] (Direct f))
> ppMathExpTL' Pascal (LA f LAU (i, j)) = prettyLang Pascal (ArrayElem [i, j, Number 1] (Direct f))

> ppMathExpTL' C      (Offset s)        = prettyLang C      (ArrayElem [s] (Direct (poffset prefixes)))
> ppMathExpTL' F      (Offset s)        = prettyLang F      (ArrayElem [s] (Direct (poffset prefixes)))
> ppMathExpTL' Pascal (Offset s)        = prettyLang Pascal (ArrayElem [s] (Direct (poffset prefixes)))

> ppMathExpTL' tl foo@(a :# b) = prettyLang tl foo

> ppMathExpTL' _ x =  "/* FIXME ppMathExpTL: " ++ (show x) ++ " */"

-------------------------------------------------------------------------

Tools:
------

> longComment :: String -> [String]
> longComment s = [s ++ replicate (max 0 ((commentWidth - length s))) ' ', (replicate commentWidth '-')]

> tlLongComment :: String -> TL
> tlLongComment s = TLComment $ longComment s

> tlLongComments :: [String] -> TL
> tlLongComments s = TLComment $ map (\s -> s ++ replicate (max 0 ((commentWidth - length s))) ' ') s ++ [(replicate commentWidth '-')]

> commentLn :: String -> TL
> commentLn s = TLComment [p1 ++ " " ++ s ++ " " ++ p2]
>   where
>     l   = (commentWidth - 2 - length s)
>     p1  = replicate (max 0 (l `div` 2)) '-'
>     p2  = replicate (max 0 ((l `div` 2) + (l `mod` 2))) '-'

> commentBox :: [String] -> TL
> commentBox s | s == []   = TLComment [] 
>              | otherwise = TLComment $ p ++ map fill s ++ p 
>    where 
>      p = ["+" ++ replicate (w - 1) '-']
>      fill s = s ++ (replicate (max 0 (w - length s)) ' ')
>      w = maximum (map length s) + 10

> longCommentToBox :: [String] -> TL
> longCommentToBox [] = TLComment []
> longCommentToBox cs = commentBox $ [""] ++ init cs ++ [""]

> meToTL :: MathExp -> TL
> meToTL m   = TLExp $ ExpME m
> tlvar va   = TLVar $ toVA va
> tlnumber n = TLExp $ ExpNum n

declToVA :: (Show [([String], b)]) => [([String], b)] -> (VAccess, b)

> declToVA [([n], dt)] = (Direct n, dt)
> declToVA x           = pattErr "declToVA" x

> tlNonterm :: String -> SubScripts -> TL
> tlNonterm n s = TLVar (VANonterm n s, TLVoid)

Stack
------

> stackVar :: String
> stackVar = "stack"

> topStack :: String
> topStack = "top"

> pushStack :: TL -> TL
> pushStack v = TLAssign (toVA (Pointer (PreInc (Direct topStack)))) v

> popStack :: TL
> popStack = tlvar (Pointer (PostDec (Direct topStack)))

> initStack :: TL
> initStack = TLAssign (toVA (Direct topStack)) (tlvar (Direct stackVar))



> --------------------------------------------------------------------------------------------------------------------------
> --------------------------------------------------------------------------------------------------------------------------
> -- TL-Traversierung
> -----------------------------------------------

> type WorkersTL cll = (
>   cll -> TL         -> (cll, TL),
>   cll -> [TL]       -> (cll, [TL]),
>   cll -> [VarDecl]  -> (cll, [VarDecl]),
>   cll -> VarAccess  -> (cll, VarAccess),
>   cll -> VAccess    -> (cll, VAccess),
>   cll -> DataType   -> (cll, DataType),
>   cll -> TypeDecl   -> (cll, TypeDecl)
>   )

> wrkTL        (a,_,_,_,_,_,_) = a
> wrkTLs       (_,a,_,_,_,_,_) = a
> wrkVarDecls  (_,_,a,_,_,_,_) = a
> wrkVarAccess (_,_,_,a,_,_,_) = a
> wrkVAccess   (_,_,_,_,a,_,_) = a
> wrkDataType  (_,_,_,_,_,a,_) = a
> wrkTypeDecl  (_,_,_,_,_,_,a) = a

> -- helper for VAccess embedded in Expr
> wrkExp wrk cll (ExpTLVar va) = (cll', ExpTLVar va') 
>    where 
>     (cll', va') = wrkVAccess wrk cll va
> wrkExp wrk cll x = collectExp (wrkExp wrk) cll x

> ------------------------------------------------------------------------------------------
> -- collector for TL
> -------------------------------------
> collectTL wrk cll TLNil = (cll, TLNil)
> collectTL wrk cll (TLLayout a) = (cll, TLLayout a)
> collectTL wrk cll (TLMacro a) = (cll, TLMacro a)
> collectTL wrk cll (TLComment a) = (cll, TLComment a)
> collectTL wrk cll (TLVar a)    = (cll', TLVar a')    where (cll', a')  = wrkVarAccess wrk cll a
> collectTL wrk cll (TLDecls ds) = (cll', TLDecls ds') where (cll', ds') = wrkVarDecls wrk cll ds
> collectTL wrk cll (TLTypeDecls td) = (cll', TLTypeDecls td') 
>                                where (cll', td') = collectTypeDecls wrk cll td
> collectTL wrk cll (TLAssign va tl) = (cll'', TLAssign va' tl')
>                                where
>                                  (cll',  va') = wrkVarAccess wrk cll  va
>                                  (cll'', tl') = wrkTL wrk   cll' tl
> collectTL wrk cll (TLAssignIF va e tl1 tl2) = (cll'''', TLAssignIF va' e' tl1' tl2')
>                                where
>                                  (cll',     va') = wrkVarAccess wrk cll    va
>                                  (cll'',     e') = wrkExp       wrk cll'   e
>                                  (cll''',  tl1') = wrkTL        wrk cll''  tl1
>                                  (cll'''', tl2') = wrkTL        wrk cll''' tl2
> collectTL wrk cll (TLAlloc mm va e dt) = (cll''', TLAlloc mm va' e' dt')
>                                where
>                                  (cll',   va') = wrkVarAccess wrk cll   va
>                                  (cll'',  e')  = wrkExp       wrk cll'  e
>                                  (cll''', dt') = wrkDataType  wrk cll'' dt
> collectTL wrk cll (TLAllocArr va dims) = (cll'', TLAllocArr va' dims')
>                                where
>                                  (cll',   va')  = wrkVarAccess wrk cll  va
>                                  (cll'', dims') = wrkDims      wrk cll' dims
>                                  wrkDims wrk cll []             = (cll, [])
>                                  wrkDims wrk cll ((e1,e2):dims) = (cll''', (e1', e2'): dims')
>                                    where
>                                      (cll',  e1')    = wrkExp  wrk cll   e1
>                                      (cll'', e2')    = wrkExp  wrk cll'  e2
>                                      (cll''', dims') = wrkDims wrk cll'' dims
> collectTL wrk cll (TLExp e) = (cll', TLExp e') where (cll', e')  = wrkExp wrk cll e
> collectTL wrk cll (TLFD cmt dt na ds1 ds2 tls tl) = (cll5, TLFD cmt dt' na ds1' ds2' tls' tl')
>                                where
>                                  (cll1, dt')  = wrkDataType wrk  cll  dt
>                                  (cll2, ds1') = wrkVarDecls wrk  cll1 ds1
>                                  (cll3, ds2') = wrkVarDecls wrk  cll2 ds2
>                                  (cll4, tls') = wrkTLs wrk       cll3 tls
>                                  (cll5, tl')  = wrkTL wrk        cll4 tl

> collectTL wrk cll (TLFA f tls) = (cll', TLFA f tls')
>                             where (cll', tls') = wrkTLs wrk cll tls
> collectTL wrk cll (TLFPA va tls) = (cll'', TLFPA va' tls')
>                             where 
>                               (cll',  va')  = wrkVarAccess wrk  cll  va
>                               (cll'', tls') = wrkTLs wrk cll' tls
> collectTL wrk cll (TLIf e tls1 tls2) = (cll''', TLIf e' tls1' tls2')
>                             where
>                               (cll',   e')    = wrkExp     wrk cll   e
>                               (cll'',  tls1') = wrkTLs wrk cll'  tls1
>                               (cll''', tls2') = wrkTLs wrk cll'' tls2
> collectTL wrk cll (TLIfs e tls1 tls2) = (cll''', TLIfs e' tls1' tls2')
>                             where
>                               (cll',   e')    = wrkExp     wrk cll   e
>                               (cll'',  tls1') = wrkTLs wrk cll'  tls1
>                               (cll''', tls2') = wrkTLs wrk cll'' tls2
> collectTL wrk cll (TLFor v a b tls) = (cll', TLFor v a b tls') where (cll', tls') = wrkTLs wrk cll tls
> collectTL wrk cll (TLForI v a b i tls) = (cll', TLForI v a b i tls') where (cll', tls') = wrkTLs wrk cll tls
> collectTL wrk cll (TLWhileNN va tls) = (cll'', TLWhileNN va' tls')
>                             where
>                               (cll',  va')  = wrkVarAccess wrk  cll  va
>                               (cll'', tls') = wrkTLs wrk cll' tls
> collectTL wrk cll (TLWhile e tls) = (cll'', TLWhile e' tls')
>                             where
>                               (cll',   e')  = wrkExp     wrk cll   e
>                               (cll'', tls') = wrkTLs wrk cll' tls
> collectTL wrk cll (TLBreak) = (cll, TLBreak)
> collectTL wrk cll (TLDefines defs) = (cll', TLDefines defs')
>                             where
>                               (cll', defs')        = collectDefines cll defs
>                               collectDefines cll []  = (cll, [])
>                               collectDefines cll ((dt, nm, args, tls):ds) = (cll'', (dt, nm, args, tls'):ds')
>                                 where
>                                   (cll',  tls') = wrkTLs wrk cll tls
>                                   (cll'', ds' ) = collectDefines cll' ds
> collectTL wrk cll (TLPrint f tls) = (cll', TLPrint f tls')
>                             where (cll', tls') = wrkTLs wrk cll tls
> collectTL wrk cll (TLBlock tls)   = (cll', TLBlock tls')
>                             where (cll', tls') = wrkTLs wrk cll tls

> collectTL wrk cll x  =  pattErr "collectTL" x


> ------------------------------------------------------------------------------------------
> -- collector for [TL]
> -------------------------------------
> collectTLs wrk cll []       = (cll, [])
> collectTLs wrk cll (tl:tls) = (cll'', (tl':tls'))
>     where
>       (cll',  tl')  = wrkTL  wrk cll  tl
>       (cll'', tls') = wrkTLs wrk cll' tls

> ------------------------------------------------------------------------------------------
> -- collector for [VarDecl]
> -------------------------------------
> collectVarDecls wrk cll [] = (cll, [])
> collectVarDecls wrk cll ((na,dt):ds) = (cll'', ((na, dt'): ds'))
>     where
>       (cll',  dt') = wrkDataType wrk cll  dt
>       (cll'', ds') = wrkVarDecls wrk cll' ds


> ------------------------------------------------------------------------------------------
> -- collector for VarAccess
> -------------------------------------
> collectVarAccess wrk cll (va, dt) = (cll'', (va', dt'))
>   where
>     (cll',  va') = wrkVAccess wrk cll  va
>     (cll'', dt') = wrkDataType wrk  cll' dt
  

> ------------------------------------------------------------------------------------------
> -- collector for VAccess
> -------------------------------------
> collectVAccess wrk cll (Direct s)  = (cll, Direct s)
> collectVAccess wrk cll (Pointer p) = (cll', Pointer p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (v1 :. v2)  = (cll'', v1' :. v2')
>                                where
>                                  (cll',  v1') = wrkVAccess wrk cll  v1
>                                  (cll'', v2') = wrkVAccess wrk cll' v2
> collectVAccess wrk cll (ArrayElem e va) =  (cll', ArrayElem e va') where (cll', va') = wrkVAccess wrk cll va
> collectVAccess wrk cll (Cast dt va)     =  (cll'', Cast dt' va')
>                                where
>                                  (cll',  dt') = wrkDataType wrk  cll  dt
>                                  (cll'', va') = wrkVAccess wrk cll' va
> collectVAccess wrk cll (Address p) = (cll', Address p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PreInc p)  = (cll', PreInc  p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PreDec p)  = (cll', PreDec  p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PostInc p) = (cll', PostInc p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PostDec p) = (cll', PostDec p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (VANonterm nt s) = (cll,VANonterm nt s)
> -- collectVAccess wrk cll x = pattErr "collectVAccess" x

> ------------------------------------------------------------------------------------------
> -- collector for DataType
> -------------------------------------
> collectDataType wrk cll (PointerOf dt)    = (cll', PointerOf dt')    where (cll', dt') = wrkDataType wrk cll dt
> collectDataType wrk cll (Array e dt)      = (cll', Array e dt')      where (cll', dt') = wrkDataType wrk cll dt
> -- adts momentan nur atomare Typen, sonst hier auch Verarbeitung:
> collectDataType wrk cll (FPointer adts dt) = (cll', FPointer adts dt') where (cll', dt') = wrkDataType wrk cll dt  
> collectDataType wrk cll (StructOf na strct) = (cll', StructOf na strct') where
>                                              (cll', strct') = collectStruct wrk cll strct
>                                              collectStruct wrk cll []     = (cll, [])
>                                              collectStruct wrk cll ((n,dt):ss) = (cll'', (n, dt'):ss')
>                                                 where
>                                                   (cll', dt')  = wrkDataType   wrk cll dt
>                                                   (cll'', ss') = collectStruct wrk cll' ss

> collectDataType wrk cll x               = (cll, x)


> ------------------------------------------------------------------------------------------
> -- collector for [TypeDecl]
> -------------------------------------
> collectTypeDecls wrk cll []       = (cll, [])
> collectTypeDecls wrk cll (td:tds) = (cll'', (td':tds'))
>     where
>       (cll',  td')  = wrkTypeDecl wrk  cll  td
>       (cll'', tds') = collectTypeDecls wrk cll' tds


> ------------------------------------------------------------------------------------------
> -- collector for TypeDecl
> -------------------------------------
> collectTypeDecl wrk cll (TypeDef name dt) = (cll', TypeDef name dt')
>       where (cll', dt') = wrkDataType wrk cll dt
> collectTypeDecl wrk cll (StructDecl name vds) = (cll', StructDecl name vds')
>       where (cll', vds') = wrkVarDecls wrk cll vds


> ------------------------------------------------------------------------------------------
> -- helper for WorkersTL construction
> ----------------------------------------
> data KWorkersTL a = NOP | DO a
> makeWorkersTL worker (wrkTL, wrkTLs, wrkVarDecls, wrkVarAccess, wrkVAccess, wrkDataType, wrkTypeDecl) =
>       (m collectTL        wrkTL, 
>        m collectTLs       wrkTLs, 
>        m collectVarDecls  wrkVarDecls, 
>        m collectVarAccess wrkVarAccess, 
>        m collectVAccess   wrkVAccess, 
>        m collectDataType  wrkDataType, 
>        m collectTypeDecl  wrkTypeDecl)
>        where
>          m collector NOP    = collector worker
>          m collector (DO f) = f
