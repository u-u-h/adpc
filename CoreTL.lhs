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



> module CoreTL

> where
> import CoreTools
> import CorePrettyPrint
> import CoreSyntax

> rev_CoreTL =  "$Revision$"

> data TL 
>   = TLLayout String
>   | TLComment [String]
>   | TLDecls [VarDecl]
>   | TLTypeDecls [TypeDecl]
>   | TLAssign   VarAccess TLExp
>   | TLAlloc    VarAccess TLExp DataType
>   | TLFD  [String] DataType String [VarDecl] [VarDecl] [TL] TLExp
>   | TLPA  VarAccess [TLExp]
>   | TLIf  TLExp [TL] [TL]
>   | TLIfs TLExp [TL] [TL]                       -- for large number of alternatives -> better prettyprinting
>   | TLFor VarAccess TLExp TLExp [TL]
>   | TLWhile   TLExp       [TL]
>   | TLPrint String [TLExp]                      -- Format-String, Argumente
>   | TLBlock [TL]
>   | TLDefines [(String, [String], [TL])]        -- Name, Argumente, Definition
>   | TLIfDef String [TL]                               
>                                          deriving (Eq, Show)

> data TLExp 
>   = TLNil
>   | TLNum Double
>   | TLVar VarAccess
>   | TLIfExp TLExp TLExp TLExp
>   | TLFA VarAccess [TLExp]
>                                          deriving (Eq, Show)

> type VarAccess = (VAccess, DataType)

> data VAccess  
>   = Direct String
>   | Pointer VAccess
>   | VAccess :. VAccess
>   | ArrayElem [TLExp] VAccess
>   | Cast DataType VAccess
>   | Address VAccess
>   | PreInc VAccess
>   | PreDec VAccess
>   | PostInc VAccess
>   | PostDec VAccess                
>                                          deriving (Eq, Show)

> type VarDecl = ([String], DataType)
> data TypeDecl = TypeDef    String DataType
>               | StructDecl String [VarDecl]
>                                          deriving (Eq, Show)


> data DataType 
>   = TLBool
>   | TLChar
>   | TLString
>   | TLInt
>   | TLLong
>   | TLReal
>   | TLVoid
>   | TLUserType String
>   | PointerOf DataType
>   | StructOf String [(String,DataType)]   -- z.B.: struct s1 {struct {int e1; int e2;} unit; int enums;} table[10][10];
>   | Array [TLExp] DataType
>   | FPointer [DataType] DataType
>                                          deriving (Show, Eq)

> infixOps = [("<", "<"), ("<=", "<="), (">", ">"), (">=", ">="), ("==", "=="), ("/=", "!="), 
>             ("+", "+"), ("-", "-"), ("*", "*"), ("/", "/"),
>             ("&&","&&"), ("||","||")]


> instance Pretty TL where
>     pretty = ppTL 0

> ppTLs = concatMap (ppTL 0)

> ppTL ind (TLLayout l)   = l
> ppTL ind (TLComment cs) = concatMap ppC cs where
>                                ppC c = spc ind ++ "/* " ++ c' ++ " */\n" where
>                                  c' = case c of
>                                        ('-':cs)  -> drop d1 $ take (l-d2) c
>                                        otherwise -> c
>                                  d1 = ind `div` 2
>                                  d2 = ind - d1
>                                  l  = length c
>                                  
> ppTL ind (TLDecls ds)  =  concatMap ppVD ds where
>                                    ppVD v = spc ind ++ ppVarDecl v ++ ";\n"
> ppTL ind (TLTypeDecls ds)  = concatMap ppTypeDecl ds 
> ppTL ind (TLAssign n a)    = spc ind ++ ppVarAccess n ++ " = " ++ ppTLExp a ++ ";\n"
> ppTL ind (TLAlloc v e d)  = spc ind ++ ppVarAccess v ++ "=(" ++ ppDataType (PointerOf d) ++ ") " ++
>                              "calloc(" ++ ppTLExp e ++ ", sizeof(" ++ ppDataType d ++ "));\n"
> ppTL ind (TLFD cmt rt f params vdecls body rv) = 
>                                            ppTL ind (TLComment cmt) ++ 
>                                            "static " ++ ppDataType rt
>                                            ++ f ++  
>                                            "(" ++ sepList ", " 
>                                                  (map ppVarDecl params) 
>                                            ++ ")"  ++
>                                           if body == [] then ";\n" else 
>                                        "\n" ++
>                                        "{\n" ++
>                                        (concatMap ppVD vdecls) ++ (if vdecls == [] then "" else "\n") ++
>                                        concatMap (ppTL (ind+tlStdInd)) body ++
>                                        rv' ++ 
>                                        "}\n" where
>                                           ppVD v = spc (ind +tlStdInd) ++ ppVarDecl v ++ ";\n"
>                                           rv' = if rt == TLVoid then [] else 
>                                                    spc (ind +tlStdInd) ++ "return(" ++ ppTLExp rv ++ ");\n"

> ppTL ind (TLPA f args)  = spc ind ++ ppVarAccess f ++ "(" ++ mapsep ", " ppTLExp args ++ ");\n"

> ppTL ind (TLIf b t e) = spc ind ++ "if (" ++ ppTLExp b ++ ") {\n" 
>                                  ++ concatMap (ppTL (ind+tlStdInd)) t ++ 
>                                    spc ind ++ "}" ++ if length e == 0 then "\n" else "\n" ++ spc ind ++ "else {\n" ++ 
>                                    concatMap (ppTL (ind+tlStdInd)) e ++ spc ind ++  "}\n"
> ppTL ind (TLIfs b t e) = spc ind ++ "if (" ++ ppTLExp b ++ ") {\n" 
>                                   ++ concatMap (ppTL (ind+tlStdInd)) t ++ 
>                                    spc ind ++ "}" ++ if length e == 0 then ";\n" else " else \n" ++ 
>                                    concatMap (ppTL ind) e 
> ppTL ind (TLFor f b e d) = spc ind ++ "for (" ++ ppVarAccess f ++ "=" ++ ppTLExp b ++ "; " ++ ppVarAccess f ++ direc ++ ppTLExp e ++ "; " ++ 
>                                                  ppVarAccess f ++ direc' ++ ")" ++ 
>                                      " {\n" ++ 
>                                           concatMap (ppTL (ind+tlStdInd)) d ++ spc ind ++ "}\n" 
>                                  where
>                                    direc  = if e == (TLNum 0) then ">=" else "<="
>                                    direc' = if e == (TLNum 0) then "--" else "++"

> ppTL ind (TLWhile e body)   = spc ind ++ "while (" ++ ppTLExp e ++ ") {\n" ++ 
>                                            concatMap (ppTL (ind+tlStdInd)) body ++ 
>                                   spc ind ++ "}\n"

> ppTL ind (TLPrint f args) | args == [] = spc ind ++ "printf(\"" ++ f ++ "\");\n"
>                           | otherwise  = spc ind ++ "printf(\"" ++ f ++ "\", " ++ mapsep ", " ppTLExp args ++ ");\n"
> ppTL ind (TLBlock tls)  = concatMap (ppTL ind) tls
> ppTL ind (TLDefines defs)   = ppTLDefines ind ppTL defs
> ppTL ind (TLIfDef d t) = spc ind ++ "#ifdef " ++ d ++ "\n" ++
>                                   concatMap (ppTL (ind+tlStdInd)) t ++
>                           spc ind ++ "#endif\n"
> ppTL _ x = pattErr "ppTL" x

Alle Zielsprachen: 

> ppTLDefines ind pp defs = concatMap ppDef defs where
>                                    ppDef (n,args,tls) = spc ind ++ "#define " ++ n ++ optArgs ++ " " ++ def ++ "\n"
>                                      where optArgs | args == [] = "" 
>                                                    | otherwise  = "(" ++ sepList ", " args ++ ")"
>                                            def     | length tls == 1  = pp ind $ head tls
>                                                    | otherwise        = "\n" ++ mapsep "\n\\" (pp $ ind + tlStdInd) tls


> instance Pretty TLExp where
>     pretty = ppTLExp


> ppTLExp TLNil = "NULL"
> ppTLExp (TLNum n) = case (reverse $ show n) of
>                      ('0':'.':nn) -> reverse nn
>                      otherwise    -> show n
> ppTLExp (TLVar n)  = ppVarAccess n
> ppTLExp (TLIfExp b t e) = "(" ++ ppTLExp b ++ ") ? " ++ ppTLExp t ++ " : " ++ ppTLExp e
> ppTLExp (TLFA (Direct f, _) [a1, a2])  | elem f (map fst infixOps) = optPar a1 ++ f ++ optPar a2
>    where optPar e@(TLVar _) = ppTLExp e
>          optPar e@(TLNum _) = ppTLExp e
>          optPar e@(TLNil)   = ppTLExp e
>          optPar e           = "(" ++ ppTLExp e ++ ")"
> ppTLExp (TLFA f args)  = ppVarAccess f ++ "(" ++ mapsep ", " ppTLExp args ++ ")"
> ppTLExp x = pattErr "ppTLExp" x


> ppVarAccess (v, _) = pretty v

> instance Pretty VAccess where
>     pretty = ppVAccess

> ppVAccess (Direct v) = v
> ppVAccess ((Pointer v1) :. v2) = ppVAccess v1 ++ "->" ++ ppVAccess v2
> ppVAccess (Pointer v) = "(*" ++ ppVAccess v ++ ")"
> ppVAccess (v1 :. v2) = ppVAccess v1 ++ "." ++ ppVAccess v2
> ppVAccess (ArrayElem i v) = ppVAccess v ++ concatMap (\e -> "[" ++ pretty e ++ "]") i
> ppVAccess (Cast dt va)    = "((" ++ ppDataType dt ++ ")" ++ po ++ ppVAccess va ++ pc ++ ")"
>              where (po,pc) = case va of
>                               (Direct _) -> ("","")
>                               otherwise  -> ("(",")")
> ppVAccess (Address v) = "&(" ++ ppVAccess v ++ ")"
> ppVAccess (PreInc v)   = "(++" ++ ppVAccess v ++ ")"
> ppVAccess (PreDec v)   = "(--" ++ ppVAccess v ++ ")"
> ppVAccess (PostInc v)  = "(" ++ ppVAccess v ++ "++)"
> ppVAccess (PostDec v)  = "(" ++ ppVAccess v ++ "--)"
> ppVAccess x = pattErr "ppVAccess" x


> ppVarDecl (n, dtt)   | cansplit && parts > 1 = mapsep (";\n"++spc tlStdInd) (ppVarDecl.(\n -> (n, dtt))) (split parts n)
>                      | otherwise             = ppdt ++ ppn
>   where
>     cansplit = length n > 2
>     ppdt  = ppDataType dt
>     ind   = tlStdInd + length ppdt
>     (ppn, dt)   = case dtt of
>                     (Array dims dt)   -> (mapsep ", " (\n -> n ++ "[" ++ mapsep "][" pretty dims ++ "]") n, dt)
>                     ps@(PointerOf dt) -> (mapsep ", " (\n -> (replicate (getPDepth ps) '*') ++ n) n, getPType dt)
>                     (FPointer dts dt) -> (mapsep ", " (\n -> "(*" ++ n ++ ")(" ++ mapsep ", " ppDataType dts ++ ")") n, dt)
>                     dt                -> (sepList ", " n, dt)
>     parts = ((length ppn + ind) `div` commentWidth)  + 1

>     split :: Int -> [a] -> [[a]]
>     split 1 b = [b]
>     split 0 b = error "cannot divide list in 0 parts"
>     split a b = let {n = length b `div` a} in
>                     (take n b):split (a-1) (drop n b)


> ppTypeDecl (TypeDef n dt) = "typedef " ++ ppDataType dt ++ " " ++ n ++ ";"
> ppTypeDecl (StructDecl n decls) = "struct " ++ n ++ " {\n" ++ concatMap ppD decls ++ "};\n"
>    where ppD d = spc 3 ++ ppVarDecl d ++ ";\n"

> ppDataType TLBool              = "boolean "
> ppDataType TLChar              = "char "
> ppDataType TLString            = "char *"
> ppDataType TLInt               = "int "
> ppDataType TLLong              = "long "
> ppDataType TLReal              = "double "
> ppDataType TLVoid              = "void "
> ppDataType (TLUserType dt)     = dt ++ " "
> ppDataType (PointerOf TLVoid)  = "void *"
> ppDataType (PointerOf d)       = ppDataType d ++ "*"
> ppDataType (StructOf name dts) = "struct " ++ name ++ if dts == [] then " " else 
>                                                        "{" ++ concatMap (\(n, dt) -> ppDataType dt ++ n ++ ";") dts ++ "}"
> ppDataType x = pattErr "ppDataType" x


Tools:
------

> longComment s = [s ++ replicate (max 0 ((commentWidth - length s))) ' ', (replicate commentWidth '-')]

> tlLongComment :: String -> TL
> tlLongComment s = TLComment $ longComment s

> tlLongComments :: [String] -> TL
> tlLongComments s = TLComment $ map (\s -> s ++ replicate (max 0 ((commentWidth - length s))) ' ') s ++ [(replicate commentWidth '-')]

> commentLn s = TLComment [p1 ++ " " ++ s ++ " " ++ p2]
>   where
>     l   = (commentWidth - 2 - length s)
>     p1  = replicate (max 0 (l `div` 2)) '-'
>     p2  = replicate (max 0 ((l `div` 2) + (l `mod` 2))) '-'

> commentBox s | s == []   = TLComment [] 
>              | otherwise = TLComment $ p ++ map fill s ++ p 
>    where 
>      p = ["+" ++ replicate (w - 1) '-']
>      fill s = s ++ (replicate (max 0 (w - length s)) ' ')
>      w = maximum (map length s) + 10

> longCommentToBox [] = TLComment []
> longCommentToBox cs = commentBox $ [""] ++ init cs ++ [""]

> declToVA [([n], dt)] = (Direct n, dt)
> declToVA x           = pattErr "declToVA" x


Tools for DataType:
--------------------

> pDepth 0 t = t
> pDepth n t = pDepth (n-1) (PointerOf t)

> getPDepth (PointerOf dt) = 1 + getPDepth dt
> getPDepth _              = 0

> getPType (PointerOf dt) = getPType dt
> getPType dt             = dt

> -- Erzeuge eine Listenstruktur fuer einen Datentyp
> makeListStruct :: DataType -> DataType
> makeListStruct dt   = PointerOf $ StructOf name [("next", PointerOf (StructOf name [])), ("last", PointerOf (StructOf name [])), ("item", dt)]
>   where 
>     name = "list_" ++ take 10 (clean $ ppDataType dt )
>     clean [] = []
>     clean (x:xs) | elem x " *{};.->" = clean xs
>                  | otherwise         = x:clean xs


Standard-Einrueckung:
----------------------

> type Indent = Int
> tlStdInd     = 3   :: Indent

> commentWidth = 80 :: Int




> {-

Prettyprinter für Core-Expressions in C:

> ppCoreExpC ind (CoreSyntax.Var v)    = v
> ppCoreExpC ind (CoreSyntax.Constr c) = c
> ppCoreExpC ind (CoreSyntax.Num n) | n < 0     = "(" ++ show n ++ ")"
>                                   | otherwise = show n
> ppCoreExpC ind e@(CoreSyntax.Lc _ _)  = error $ "core expression not allowed in C: " ++ show e
> ppCoreExpC ind e@(_ CoreSyntax.:<- _) = error $ "core expression not allowed in C: " ++ show e
> ppCoreExpC ind e@(_ CoreSyntax.:.. _) = error $ "core expression not allowed in C: " ++ show e
> ppCoreExpC ind (CoreSyntax.Ap (CoreSyntax.Ap (CoreSyntax.Var f) e1) e2) 
>                        | elem f binOps = "(" ++ ppCoreExpC ind e1 ++ ") " ++ f ++ " (" ++ ppCoreExpC ind e2 ++ ")"
>                        | elem f errOps = error $ "function not allowed in C: " ++ show f
>    where  binOps = ["+","-","*","/","==","<=",">=","<",">"]
>           errOps = ["++"]
> ppCoreExpC ind (CoreSyntax.Ap (CoreSyntax.Ap (CoreSyntax.Var "!") e1) e2) = ppCoreExpC ind e1 ++ "[" ++ ppCoreExpC ind e2 ++ "]"

> ppCoreExpC ind x           = let (r, args) = CoreSyntax.findRedex x []
>                           in case r of
>                               (CoreSyntax.Constr "(,)") -> "(" ++ mapsep ", " (ppCoreExpC ind) args ++ ")"
>                               otherwise      -> "(" ++ ppCoreExpC ind r ++ "(" ++ mapsep ", " ppCoreArg args ++ "))"

>     where
>       ppCoreArg a@(CoreSyntax.Ap _ _) = "(" ++ ppCoreExpC ind a ++ ")"
>       ppCoreArg a          =        ppCoreExpC ind a


> -}


> --------------------------------------------------------------------------------------------------------------------------
> --------------------------------------------------------------------------------------------------------------------------
> -- TL-Traversierung
> -----------------------------------------------

> type WorkersTL cll = (
>   cll -> TL         -> (cll, TL),
>   cll -> [TL]       -> (cll, [TL]),
>   cll -> TLExp      -> (cll, TLExp),
>   cll -> [VarDecl]  -> (cll, [VarDecl]),
>   cll -> VarAccess  -> (cll, VarAccess),
>   cll -> VAccess    -> (cll, VAccess),
>   cll -> DataType   -> (cll, DataType),
>   cll -> TypeDecl   -> (cll, TypeDecl)
>   )

> wrkTL        (a,_,_,_,_,_,_,_) = a
> wrkTLs       (_,a,_,_,_,_,_,_) = a
> wrkTLExp     (_,_,a,_,_,_,_,_) = a
> wrkVarDecls  (_,_,_,a,_,_,_,_) = a
> wrkVarAccess (_,_,_,_,a,_,_,_) = a
> wrkVAccess   (_,_,_,_,_,a,_,_) = a
> wrkDataType  (_,_,_,_,_,_,a,_) = a
> wrkTypeDecl  (_,_,_,_,_,_,_,a) = a

> ------------------------------------------------------------------------------------------
> -- collector for TL
> -------------------------------------
> collectTL wrk cll (TLLayout a) = (cll, TLLayout a)
> collectTL wrk cll (TLComment a) = (cll, TLComment a)
> collectTL wrk cll (TLDecls ds) = (cll', TLDecls ds') where (cll', ds') = wrkVarDecls wrk cll ds
> collectTL wrk cll (TLTypeDecls td) = (cll', TLTypeDecls td') 
>                                where (cll', td') = collectTypeDecls wrk cll td
> collectTL wrk cll (TLAssign va tl) = (cll'', TLAssign va' tl')
>                                where
>                                  (cll',  va') = wrkVarAccess wrk   cll  va
>                                  (cll'', tl') = wrkTLExp     wrk   cll' tl
> collectTL wrk cll (TLAlloc va e dt) = (cll''', TLAlloc va' e' dt')
>                                where
>                                  (cll',   va') = wrkVarAccess wrk cll   va
>                                  (cll'',  e')  = wrkTLExp     wrk cll'  e
>                                  (cll''', dt') = wrkDataType  wrk cll'' dt
> collectTL wrk cll (TLFD cmt dt na ds1 ds2 tls tl) = (cll5, TLFD cmt dt' na ds1' ds2' tls' tl')
>                                where
>                                  (cll1, dt')  = wrkDataType wrk  cll  dt
>                                  (cll2, ds1') = wrkVarDecls wrk  cll1 ds1
>                                  (cll3, ds2') = wrkVarDecls wrk  cll2 ds2
>                                  (cll4, tls') = wrkTLs      wrk  cll3 tls
>                                  (cll5, tl')  = wrkTLExp    wrk  cll4 tl

> collectTL wrk cll (TLPA f tls) = (cll'', TLPA f' tls')
>                                where 
>                                  (cll',  f')   = wrkVarAccess  wrk cll  f
>                                  (cll'', tls') = collectTLExps wrk cll' tls
> collectTL wrk cll (TLIf e tls1 tls2) = (cll''', TLIf e' tls1' tls2')
>                             where
>                               (cll',   e')    = wrkTLExp   wrk cll   e
>                               (cll'',  tls1') = wrkTLs wrk cll'  tls1
>                               (cll''', tls2') = wrkTLs wrk cll'' tls2
> collectTL wrk cll (TLIfs e tls1 tls2) = (cll''', TLIfs e' tls1' tls2')
>                             where
>                               (cll',   e')    = wrkTLExp   wrk cll   e
>                               (cll'',  tls1') = wrkTLs wrk cll'  tls1
>                               (cll''', tls2') = wrkTLs wrk cll'' tls2
> collectTL wrk cll (TLFor v a b tls) = (cll4, TLFor v' a' b' tls') 
>                             where 
>                               (cll1, v')   = wrkVarAccess  wrk cll  v
>                               (cll2, a')   = wrkTLExp      wrk cll1 a
>                               (cll3, b')   = wrkTLExp      wrk cll2 b
>                               (cll4, tls') = wrkTLs        wrk cll3 tls'

> collectTL wrk cll (TLWhile e tls) = (cll'', TLWhile e' tls')
>                             where
>                               (cll',   e')  = wrkTLExp  wrk cll   e
>                               (cll'', tls') = wrkTLs    wrk cll' tls
> collectTL wrk cll (TLPrint f tls) = (cll', TLPrint f tls')
>                             where (cll', tls') = collectTLExps wrk cll tls
> collectTL wrk cll (TLBlock tls)   = (cll', TLBlock tls')
>                             where (cll', tls') = wrkTLs wrk cll tls
> collectTL wrk cll (TLDefines defs) = (cll', TLDefines defs')
>                             where
>                               (cll', defs')        = collectDefines cll defs
>                               collectDefines cll []  = (cll, [])
>                               collectDefines cll ((nm, args, tls):ds) = (cll'', (nm, args, tls'):ds')
>                                 where
>                                   (cll',  tls') = wrkTLs wrk cll tls
>                                   (cll'', ds' ) = collectDefines cll' ds


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
> -- collector for TLExp
> -------------------------------------
> collectTLExp wrk cll TLNil           = (cll, TLNil)
> collectTLExp wrk cll (TLNum n)       = (cll, TLNum n)
> collectTLExp wrk cll (TLVar a)       = (cll', TLVar a')    where (cll', a')  = wrkVarAccess wrk cll a
> collectTLExp wrk cll (TLIfExp c t e) = (cll''', TLIfExp c' t' e')
>     where
>       (cll',   c') = wrkTLExp wrk cll   c 
>       (cll'',  t') = wrkTLExp wrk cll'  t 
>       (cll''', e') = wrkTLExp wrk cll'' e 
> collectTLExp wrk cll (TLFA va exps)  = (cll'', TLFA va' exps')
>     where
>       (cll',  va')   = wrkVarAccess wrk cll  va
>       (cll'', exps') = collectTLExps    wrk cll' exps

> ------------------------------------------------------------------------------------------
> -- collector for [TLExp]
> -------------------------------------
> collectTLExps wrk cll []       = (cll, [])
> collectTLExps wrk cll (tl:tls) = (cll'', (tl':tls'))
>     where
>       (cll',  tl')  = wrkTLExp  wrk cll  tl
>       (cll'', tls') = collectTLExps wrk cll' tls


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
> collectVAccess wrk cll (ArrayElem e va) =  (cll'', ArrayElem e' va') 
>                               where 
>                                  (cll',  e')  = collectTLExps  wrk cll  e
>                                  (cll'', va') = wrkVAccess wrk cll' va
> collectVAccess wrk cll (Cast dt va)     =  (cll'', Cast dt' va')
>                                where
>                                  (cll',  dt') = wrkDataType wrk  cll  dt
>                                  (cll'', va') = wrkVAccess wrk cll' va
> collectVAccess wrk cll (Address p) = (cll', Address p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PreInc p)  = (cll', PreInc  p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PreDec p)  = (cll', PreDec  p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PostInc p) = (cll', PostInc p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll (PostDec p) = (cll', PostDec p') where (cll', p') = wrkVAccess wrk cll p
> collectVAccess wrk cll x = pattErr "collectVAccess" x

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
> makeWorkersTL worker (wrkTL, wrkTLs, wrkTLExp, wrkVarDecls, wrkVarAccess, wrkVAccess, wrkDataType, wrkTypeDecl) =
>       (m collectTL        wrkTL, 
>        m collectTLs       wrkTLs, 
>        m collectTLExp     wrkTLExp, 
>        m collectVarDecls  wrkVarDecls, 
>        m collectVarAccess wrkVarAccess, 
>        m collectVAccess   wrkVAccess, 
>        m collectDataType  wrkDataType, 
>        m collectTypeDecl  wrkTypeDecl)
>        where
>          m collector NOP    = collector worker
>          m collector (DO f) = f
