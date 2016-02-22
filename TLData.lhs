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



> module TLData(

>   VAccess(..),
>   VarDecl,
>   TypeDecl(..),
>   VarAccess,
>   DataType(..),
>   CompileOptions(..),
>   BTMode(..),
>   TAS(..),
>   CG(..),
>   GCMode(..),
>   OFMode(..),

>   chooseBT,
>   commentWidth,
>   freeNeeded,
>   fta,
>   getOptAR,
>   getOptARR,
>   getOptBEA,
>   getOptBT,
>   getOptCG,
>   getOptGC,
>   getOptIF,
>   getOptITA,
>   getOptLCF,
>   getOptNC,
>   getOptO,
>   getOptO2,
>   getOptOF,
>   getOptTAS,
>   getOptW,
>   getVANonterm,
>   haskellTypeToDatatype,
>   inCGList,
>   isBTSingle,  btSubOpts, isBTSubOpt,
>   isVANonterm,
>   knownHaskellTypes,
>   knownHaskellTypes',
>   mergeDecls,
>   pDepth,
>   ppDataTypeC,
>   ppDataTypePascal,
>   ppDataTypeJava,
>   ppNameTex,
>   ppTypeDeclC,
>   ppTypeDeclF,
>   ppTypeDeclPascal,
>   ppTypeDeclJava,
>   ppVarAccessC,
>   ppVarAccessF,
>   ppVarAccessPascal,
>   ppVarAccessJava,
>   ppVarDeclC,
>   ppVarDeclF,
>   ppVarDeclPascal,
>   ppVarDeclJava,
>   rev_TLData,
>   structAccess,
>   tlStdInd,
>   toVA,
>   ppVAccessListJava,
>   ppVarAccessJavaLHS,
>   ppVAccessJavaLHS,

> ) where

> import Constants
> import Tools
> import MathExp
> import PrettyPrint

> import List

> rev_TLData =  "$Revision$"

Konfiguration aus Parser:
---------------------------

> data CG = CGb | CGl | CGv | CGp deriving (Eq, Show)

> ppCG CGb  = "simple list"
> ppCG CGl = "list"
> ppCG CGv = "value"
> ppCG CGp  = "pairing"

> inCGList opts = elem (getOptCG opts) [CGl, CGp] || (getOptBT opts /= BTNone && not (isBTSingle opts)) 

Compile - Options:
------------------
- Resulttype for codegen
- Backtracing mode
- Generate code to show all table results
- Generate code to show _all_ table results
- Leak choice functions into inner expressions
- suppress comments in target code
- table allocation scheme
- Inline table access macros
- Target code optimization -O
- Target code optimization -O2
- Inline functions
- Beautify code
- Garbage Collection mode

> data CompileOptions = 
>    CompileOptions (CG, BTMode, Bool, Bool, Bool, Bool, [TAS], Bool, Bool, Bool, Bool, Bool, GCMode, OFMode, Bool)
> getOptCG  (CompileOptions (a,_,_,_,_,_,_,_,_,_,_,_,_,_,_)) = a
> getOptBT  (CompileOptions (_,a,_,_,_,_,_,_,_,_,_,_,_,_,_)) = a
> getOptAR  (CompileOptions (_,_,a,_,_,_,_,_,_,_,_,_,_,_,_)) = a
> getOptARR (CompileOptions (_,_,_,a,_,_,_,_,_,_,_,_,_,_,_)) = a
> getOptLCF (CompileOptions (_,_,_,_,a,_,_,_,_,_,_,_,_,_,_)) = a
> getOptNC  (CompileOptions (_,_,_,_,_,a,_,_,_,_,_,_,_,_,_)) = a
> getOptTAS (CompileOptions (_,_,_,_,_,_,a,_,_,_,_,_,_,_,_)) = a
> getOptITA (CompileOptions (_,_,_,_,_,_,_,a,_,_,_,_,_,_,_)) = a
> getOptIF  (CompileOptions (_,_,_,_,_,_,_,_,a,_,_,_,_,_,_)) = a
> getOptO   (CompileOptions (_,_,_,_,_,_,_,_,_,a,_,_,_,_,_)) = a
> getOptO2  (CompileOptions (_,_,_,_,_,_,_,_,_,_,a,_,_,_,_)) = a
> getOptBEA (CompileOptions (_,_,_,_,_,_,_,_,_,_,_,a,_,_,_)) = a
> getOptGC  (CompileOptions (_,_,_,_,_,_,_,_,_,_,_,_,a,_,_)) = a
> getOptOF  (CompileOptions (_,_,_,_,_,_,_,_,_,_,_,_,_,a,_)) = a
> getOptW   (CompileOptions (_,_,_,_,_,_,_,_,_,_,_,_,_,_,a)) = a


Table allocation schemes:

> data TAS = TASPointers | TASBlock | TASBlockInterleave | TASTriangularBlock | TASOffset   deriving Eq

Backtrace modes:

> data BTMode = BTNone | BTCompleteList | BTSingle | BTList | BTSubOpt | BTSubOptCut | BTPF deriving (Show, Eq)
> -- Single und List werden haeufig gemeinsam behandelt:
> isBTSingle opts = elem (getOptBT opts) [BTSingle, BTList]

> btSubOpts = [BTSubOpt, BTSubOptCut, BTPF]
> isBTSubOpt opts = elem (getOptBT opts) btSubOpts

> chooseBT opts alts | not (elem thisOpt relevantOpts) = head' [ res | ([], res) <- alts] 
>                                                              $ "chooseBT: no default option"
>                    | otherwise                       = head' [ res | (opts, res) <- alts, elem thisOpt opts] 
>                                                              $ "chooseBT: no result given for option " ++ show thisOpt
>   where
>     thisOpt      = getOptBT opts
>     relevantOpts = concatMap fst alts

Garbage collection modes:

> data GCMode = GCNone | GCcc | GCbdw | GCown  deriving (Show, Eq)
> freeNeeded opts = getOptGC opts == GCNone

Output format:

> data OFMode = OFHaskell | OFHaskellLines | OFSpace | OFLines   deriving (Show, Eq)

Standard-Einrueckung:
----------------------

> type Indent = Int
> tlStdInd     = 3   :: Indent

> commentWidth = 80 :: Int

Datentypen:
-----------------

> data DataType = 
>        TLBool                  |
>        TLChar                  |
>        TLRegion                |
>        TLString                |
>        TLInt                   |
>        TLLong                  |
>        TLReal                  |
>        TLResult                |
>        TLVoid                  |
>        TLUserType String       |
>        PointerOf DataType      |
>        StructOf String [(String,DataType)] |  -- z.B.: struct s1 {struct {int e1; int e2;} unit; int enums;} table[10][10];
>        Array [MathExp] DataType            |
>        FPointer [DataType] DataType
>                                deriving (Show)

> instance Eq DataType where

the only derivation from the automatically derived instance, because
of IL2 where the same tuples may have different names
(e.g. "typel1" vs. "")

>  StructOf _ foo == StructOf _ bar = foo == bar

>  TLBool == TLBool = True
>  TLChar == TLChar = True
>  TLRegion == TLRegion = True
>  TLString == TLString = True
>  TLInt == TLInt = True
>  TLLong == TLLong = True
>  TLReal == TLReal = True
>  TLResult == TLResult = True
>  TLVoid == TLVoid = True
>  TLUserType foo == TLUserType bar = foo == bar
>  PointerOf foo == PointerOf bar = foo == bar
>  Array f1 f2 == Array b1 b2 = (f1, f2) == (b1, b2)
>  FPointer f1 f2 == FPointer b1 b2 = (f1, f2) == (b1, b2)
>  _ == _ = False



> instance Pretty DataType where
>     prettyLang Normal = ppDataTypeC
>     prettyLang C      = ppDataTypeC
>     prettyLang F      = ppDataTypeF
>     prettyLang Pascal = ppDataTypePascal
>     prettyLang Java   = ppDataTypeJava
>--     prettyLang Latex  = ppDataTypeLatex -- gibts nicht
>     pretty            = ppDataTypeC

Targetlanguage Pascal:

> ppDataTypePascalTR (ST a)   = ppDataTypePascal a
> ppDataTypePascalTR (TT a b) = "(" ++ ppDataTypePascal a ++ ", " ++ ppDataTypePascal b ++ ")"

> ppDataTypePascal TLBool        = "boolean"
> ppDataTypePascal TLChar        = "char"
> ppDataTypePascal TLRegion      = "region"
> ppDataTypePascal TLString      = "string"
> ppDataTypePascal TLInt         = "integer"
> ppDataTypePascal TLLong        = "int64"
> ppDataTypePascal TLReal        = "real"
> ppDataTypePascal TLResult      = "result"
> ppDataTypePascal TLVoid        = "pointer"
> ppDataTypePascal (TLUserType dt)    = dt
> ppDataTypePascal (PointerOf TLVoid) = "pointer"
> ppDataTypePascal (PointerOf d)   = "^" ++ ppDataTypePascal d
> ppDataTypePascal (StructOf n _)  = n
> ppDataTypePascal (Array dims d)  = "array[" ++ sepList ", " (map (\n -> "0.." ++ pretty (calcME (n :- Number 1))) dims) ++ "] of " ++ ppDataTypePascal d
> ppDataTypePascal (FPointer dts dt) = "^fpointer(" ++ mapsep ", " ppDataTypePascal dts ++ "):" ++ ppDataTypePascal dt

Targetlanguage C:

> ppDataTypeCTR (ST a)   = ppDataTypeC a
> ppDataTypeCTR (TT a b) = "(" ++ ppDataTypeC a ++ ", " ++ ppDataTypeC b ++ ")"

> ppDataTypeC TLBool              = "boolean "
> ppDataTypeC TLChar              = "char "
> ppDataTypeC TLRegion            = "region "
> ppDataTypeC TLString            = "char *"
> ppDataTypeC TLInt               = "int "
> ppDataTypeC TLLong              = "long "
> ppDataTypeC TLReal              = "double "
> ppDataTypeC TLResult            = "result "
> ppDataTypeC TLVoid              = "void "
> ppDataTypeC (TLUserType dt)     = dt ++ " "
> ppDataTypeC (PointerOf TLVoid)  = "void *"
> ppDataTypeC (PointerOf d)       = ppDataTypeC d ++ "*"
> ppDataTypeC (StructOf name dts) = "struct " ++ name ++ if dts == [] then " " else 
>                                                        "{" ++ concatMap (\(n, dt) -> ppDataTypeC dt ++ n ++ ";") dts ++ "}"

Targetlanguage Fortran:

> ppDataTypeFTR (ST a)   = ppDataTypeF a
> ppDataTypeFTR (TT a b) = "(" ++ ppDataTypeF a ++ ", " ++ ppDataTypeF b ++ ")"

> ppDataTypeF TLBool              = "logical "
> ppDataTypeF TLChar              = "character "
> ppDataTypeF TLRegion            = "region "
> ppDataTypeF TLString            = "string "
> ppDataTypeF TLInt               = "integer "
> ppDataTypeF TLLong              = "integer "
> ppDataTypeF TLReal              = "real "
> ppDataTypeF TLResult            = "result "
> ppDataTypeF TLVoid              = "void "
> ppDataTypeF (TLUserType dt)     = dt ++ " "
> ppDataTypeF (PointerOf TLVoid)  = "void *"
> ppDataTypeF (PointerOf d)       = ppDataTypeF d ++ ", POINTER "
> ppDataTypeF (StructOf name dts) = "TYPE(" ++ name ++ ")" ++ if dts == [] then " " else 
>                                                        "{" ++ concatMap (\(n, dt) -> ppDataTypeF dt ++ n ++ ";") dts ++ "}"

Java Pretty-Printer:

> ppDataTypeJava (PointerOf (StructOf name@('t':'u':'p':'e':'l':_) [])) = name ++ "[]"
> ppDataTypeJava (PointerOf (StructOf name [])) = name
> ppDataTypeJava (PointerOf TLVoid) = "Object"
> ppDataTypeJava (PointerOf TLChar) = "StringBuilder"
> ppDataTypeJava (PointerOf (PointerOf (StructOf name []))) = name ++ "[]"
> ppDataTypeJava (PointerOf TLInt) = "int[]"
> ppDataTypeJava (StructOf name _) = name
> ppDataTypeJava (FPointer _ (PointerOf (StructOf ('s':'t':'r':'1':[]) []))) =
>   "Backtrace"
> ppDataTypeJava TLVoid = "void"
> ppDataTypeJava TLChar = "char"
> ppDataTypeJava TLInt = "int"
> ppDataTypeJava TLReal = "double"
> ppDataTypeJava foo = "/* FIXME DataType: " ++ (show foo) ++ "*/"


Tools:
-------

> pDepth 0 t = t
> pDepth n t = pDepth (n-1) (PointerOf t)

> getPDepth (PointerOf dt) = 1 + getPDepth dt
> getPDepth _              = 0

> getPType (PointerOf dt) = getPType dt
> getPType dt             = dt

Variablendeklarationen:
--------------------------

> type VarDecl = ([String], DataType)

> -- Targetlanguage Pascal:
> ppVarDeclPascal (n, dt) = sepList ", " n ++ ": " ++ ppDataTypePascal dt

> -- Targetlanguage C:
> ppVarDeclC (n, dtt)   | cansplit && parts > 1 = mapsep (";\n"++spc tlStdInd) (ppVarDeclC.(\n -> (n, dtt))) (split parts n)
>                       | otherwise             = case (utypeCheck ppdt ppn) of
>                                                  Nothing -> ppdt ++ ppn
>                                                  Just x  -> x

>   where
>     cansplit = length n > 2
>     ppdt  = ppDataTypeC dt
>     ind   = tlStdInd + length ppdt
>     (ppn, dt)   = case dtt of
>                     (Array dims dt)   -> (mapsep ", " (\n -> n ++ "[" ++ mapsep "][" pretty dims ++ "]") n, dt)
>                     ps@(PointerOf dt) -> (mapsep ", " (\n -> (replicate (getPDepth ps) '*') ++ n) n, getPType dt)
>                     (FPointer dts dt) -> (mapsep ", " (\n -> "(*" ++ n ++ ")(" ++ mapsep ", " ppDataTypeC dts ++ ")") n, dt)
>                     dt                -> (sepList ", " n, dt)
>     parts = ((length ppn + ind) `div` commentWidth)  + 1

>     split :: Int -> [a] -> [[a]]
>     split 1 b = [b]
>     split 0 b = error "cannot divide list in 0 parts"
>     split a b = let {n = length b `div` a} in
>                     (take n b):split (a-1) (drop n b)

Hacky, hacky, hacky - special case to make the utype a enum

>     utypeCheck _ ('u':'t':'y':'p':'e':[]) = Just "enum sigid utype"
>     utypeCheck _ _ = Nothing

> -- Targetlanguage Fortran:
> ppVarDeclF (n, dtt)   | cansplit && parts > 1 = mapsep ("\n"++spc tlStdInd) (ppVarDeclF.(\n -> (n, dtt))) (split parts n)
>                       | otherwise             = ppdt ++ add ++ " :: " ++ ppn
>   where
>     cansplit = length n > 2
>     ppdt  = ppDataTypeF dt
>     ind   = tlStdInd + length ppdt

>     -- ziemlich fieser Hack fuer Arrays.
>     -- am sinnvollsten ist, Array-Deklarationen in TL wirklich als Arrays anzugeben (und nicht als Pointer->pointer),
>     -- und dann pointer nur in C zu erzeugen.
>     (ppn, dt, add)   = case dtt of
>                     (Array dims dt)   -> (mapsep ", " (\n -> n ++ "[" ++ mapsep "][" pretty dims ++ "]") n, dt, "")
>                     (PointerOf (PointerOf dt))  -> (mapsep ", " (\n -> n ++ "(:,:)") n, dt, ", ALLOCATABLE ")
>                     (FPointer dts dt) -> (mapsep ", " (\n -> "(*" ++ n ++ ")(" ++ mapsep ", " ppDataTypeF dts ++ ")") n, dt, "")
>                     dt                -> (sepList ", " n, dt, "" )
>     parts = ((length ppn + ind) `div` commentWidth)  + 1

>     split :: Int -> [a] -> [[a]]
>     split 1 b = [b]
>     split 0 b = error "cannot divide list in 0 parts"
>     split a b = let {n = length b `div` a} in
>                     (take n b):split (a-1) (drop n b)


> -- fuehre Deklarationen zusammen, so dass Variablen des gleichen Typs in der gleichen
> -- Deklaration gefuehrt werden:
> mergeDecls :: [VarDecl] -> [VarDecl]
> mergeDecls = md [] where
>   md :: [VarDecl] -> [VarDecl] -> [VarDecl]
>   md res [] = res
>   md res ((vars, dt):ds) | elem dt (map snd res) = md (updRes res (vars, dt)) ds
>                          | otherwise             = md ((vars,dt):res)         ds
>   updRes :: [VarDecl] -> VarDecl -> [VarDecl]
>   updRes [] _ = []
>   updRes ((resv, resdt):res) (v,dt) | resdt == dt = (resv ++ v, resdt):res
>                                     | otherwise   = (resv,      resdt):updRes res (v,dt)


Java Pretty-Printer:

> ppVarDeclJava _ ([], _) = []

Backtrace special treatment in proxy

> ppVarDeclJava ind (name:names, dt) =
>   (spc ind ++ dn ++ " " ++ name ++ init name ++ initDt dt ++ proxy dt name):
>   (ppVarDeclJava ind (names, dt))
>   where
>     dn = ppDataTypeJava dt
>     init "score" = ppInit dt
>     init "nstr"  = " = null"
>     init _ = ""
>     initDt (StructOf foo@('t':'u':'p':'e':'l':_) _) = " = new " ++ foo ++ "()"
>     initDt _ = ""
>     ppInit TLInt = " = 0"
>     ppInit TLReal = " = 0.0"
>     ppInit _  = "/* FIXME ppVarDeclJava Init: " ++ (show dt) ++ " */"
>     proxy (PointerOf (StructOf "str1" [])) foo@('a':_) =
>       ";\n" ++
>       "class Proxy_" ++ foo ++ " implements SetStr1 {\n" ++
>       "public void set(str1 foo)\n" ++
>       "{\n" ++
>       "  " ++ foo ++ " = foo;\n" ++
>       "}\n}\n" ++
>       "SetStr1 proxy_" ++ foo ++ " = new Proxy_" ++ foo ++ "()"
>     proxy _ _ = ""


Datentypdeklarationen:
--------------------------

> data TypeDecl = TypeDef    String DataType
>               | StructDecl String [VarDecl]
>                                                 deriving (Eq, Show)

> ppTypeDeclPascal (TypeDef    n dt)    = n ++ " = " ++ ppDataTypePascal dt ++ ";"
> ppTypeDeclPascal (StructDecl n decls) = n ++ " = record\n" ++ concatMap ppD decls ++ "end;\n"
>    where ppD d = spc 3 ++ ppVarDeclPascal d ++ ";\n"

> ppTypeDeclC (TypeDef n dt) = "typedef " ++ ppDataTypeC dt ++ " " ++ n ++ ";"
> ppTypeDeclC (StructDecl n decls) = "struct " ++ n ++ " {\n" ++ concatMap ppD decls ++ "};\n"
>    where ppD d = spc 3 ++ ppVarDeclC d ++ ";\n"

> ppTypeDeclF (TypeDef n dt) = "typedef " ++ ppDataTypeF dt ++ " " ++ n ++ ";"
> ppTypeDeclF (StructDecl n decls) = "TYPE " ++ n ++ "\n" ++ concatMap ppD decls ++ "END TYPE "++ n ++"\n"
>    where ppD d = spc 3 ++ ppVarDeclF d ++ "\n"

Java Pretty-Printer:

> ppTypeDeclJava ind (StructDecl name vdecls) =
>   spc ind ++ "class " ++ name ++ implements name ++ " {\n" ++
>   concatMap (ppTestVDecl (ind + tlStdInd)) vdecls ++
>   clone name  ++ node ++
>   spc ind ++ "}\n\n"
>     where

emulate plain structs via constructors ...

>       ppTestVDecl i (n:[], (StructOf dtname [])) =
>         spc i ++ dtname ++ " " ++ n ++ ";\n\n" ++
>         spc i ++ "public " ++ name ++ "() {\n" ++
>         spc (i + tlStdInd) ++ n ++ " = new " ++ dtname ++ "();\n" ++
>         spc i ++ "}\n\n"
>       ppTestVDecl i vdecl = concatMap (++";\n") 
>         (map (((spc i) ++ "public ")++) (ppVarDeclJava 0 vdecl))

needed for TLAssign backtrace special case (i.e. .clone() is used there)

>       implements ('s':'t':'r':'_':_) = " implements Cloneable, Traversable"
>       implements ('t':'u':'p':'e':'l':_) = " implements " ++ ppTupel (head vdecls) ++ "Tupel, Cloneable"
>        where
>         ppTupel (_, TLReal) = "Double"
>         ppTupel (_, TLInt) = "Int"
>         ppTupel _ = error "ppTupel: unknown first tupel component"
>       implements _ = ""
>       clone ('t':'u':'p':'e':'l':_) = "\n\n" ++ spc ind ++
>         name ++ " copy()\n{\n" ++
>         spc j ++ name ++ " r = null;\n" ++
>         spc j ++ "try {\n" ++
>         spc k ++ "r = (" ++ name ++ ") clone();\n" ++
>         spc j ++ "} catch (Exception e) {\n" ++
>         spc k ++ "System.err.println(\"Internal Error: Clone\");\n" ++
>         spc k ++ "e.printStackTrace();\n" ++
>         spc j ++ "}\n" ++
>         spc j ++ "return r;\n" ++
>         spc ind ++ "}\n" ++
>         spc j ++ "public " ++ prettyLang Java (snd (head vdecls)) ++ " getMainValue() { return tup1; }\n"
>       clone ('s':'t':'r':'_':_) = "\n\n" ++ spc ind ++ "public " ++ 
>         name ++ " copy()\n{\n" ++
>         spc j ++ name ++ " r = null;\n" ++
>         spc j ++ "try {\n" ++
>         spc k ++ "r = (" ++ name ++ ") clone();\n" ++
>         spc j ++ "} catch (Exception e) {\n" ++
>         spc k ++ "System.err.println(\"Internal Error: Clone\");\n" ++
>         spc k ++ "e.printStackTrace();\n" ++
>         spc j ++ "}\n" ++
>         spc j ++ "r.reinit();\n" ++
>         spc j ++ "return r;\n" ++
>         spc ind ++ "}\n" ++
>         ppReInits
>       clone _ = ""
>       j = tlStdInd + ind
>       k = tlStdInd + j
>       ppReInits = "void reinit()\n{\n" ++ (concat $ concatMap ppProxies vdecls) ++
>         "}\n"
>       ppProxies (name:names, dt) = ( ppProxy dt name):(ppProxies (names, dt))
>       ppProxies _ = []
>       ppProxy (PointerOf (StructOf "str1" [])) foo@('a':_) =
>         "proxy_" ++ foo ++ " = new Proxy_" ++ foo ++ "();\n";
>       ppProxy _ _ = []

Node generation code

>       node
>        | name == "str1" =
>         "\nvoid traverse(Node n) { item.traverse(n); }\n"
>        | name == "str_Signature" =
>         "\npublic void traverse(Node n) { ((Traversable)entry).traverse(n); }\n"
>        | isPrefixOf "str_" name =
>          "\npublic void traverse(Node n) { Node x;\nn.setLabel(\"" ++ drop 4 name ++ "\");\n" ++
>          (unlines  (concatMap pppNode vdecls)) ++ "}\n"
>        | otherwise = ""
>       pppNode (xs, dt) = map ((flip ppNode) dt) xs
>       ppNode foo@('a':as) (PointerOf (StructOf _ _)) = 
>         "x = new Node(); n.addChild(x); " ++ foo ++ ".traverse(x);\n"
>       ppNode foo@('a':_) bar = 
>         "x = new Node(seq.unconverted.charAt(" ++ foo ++ "-1)+\"\"); n.addChild(x); /* " ++ 
>         (show bar) ++ " */\n"
>       ppNode _ _ = ""

> ppTypeDeclJava ind d = "/* FIXME TypeDecl: " ++ (show d) ++ " */"


Variablenzugriffe in der Zielsprache:
------------------------------------------

> type VarAccess = (VAccess, DataType)

> data VAccess = 
>        Direct String                 |
>        Pointer VAccess               |
>        VAccess :. VAccess            |
>        ArrayElem [MathExp] VAccess   |
>        Cast DataType VAccess         |
>        Address VAccess               |
>        PreInc VAccess                |
>        PreDec VAccess                |
>        PostInc VAccess               |
>        PostDec VAccess               |
>        VANonterm String SubScripts    
>                                          deriving (Eq, Show)

> instance Pretty VAccess where
>     pretty = ppVAccessC
>     prettyLang Normal = ppVAccessC
>     prettyLang C      = ppVAccessC
>--     prettyLang F      = ppVaccessF
>     prettyLang Pascal = ppVAccessPascal
>     prettyLang Latex  = ppVAccessLatex
>     prettyLang Java   = ppVAccessJava 


> -- Targetlanguage Pascal
> ppVarAccessPascal (v, _) = ppVAccessPascal v
> ppVAccessPascal (Direct v) = v
> ppVAccessPascal (Pointer v) = ppVAccessPascal v ++ "^"
> ppVAccessPascal (v1 :. v2) = ppVAccessPascal v1 ++ "." ++ ppVAccessPascal v2
> ppVAccessPascal (ArrayElem i v) = ppVAccessPascal v ++ "[" ++ sepList ", " (map pretty i) ++ "]"
> ppVAccessPascal (Cast dt va)    = "((" ++ ppDataTypePascal dt ++ ")" ++ po ++ ppVAccessPascal va ++ pc ++ ")"
>              where (po,pc) = case va of
>                               (Direct _) -> ("","")
>                               otherwise  -> ("(",")")
> ppVAccessPascal (Address v) = "&(" ++ ppVAccessPascal v ++ ")"
> ppVAccessPascal (PreInc v)   = "(++" ++ ppVAccessPascal v ++ ")"
> ppVAccessPascal (PreDec v)   = "(--" ++ ppVAccessPascal v ++ ")"
> ppVAccessPascal (PostInc v)  = "(" ++ ppVAccessPascal v ++ "++)"
> ppVAccessPascal (PostDec v)  = "(" ++ ppVAccessPascal v ++ "--)"
> ppVAccessPascal (VANonterm nt s@(ST (i,j)))           = optMacroAccess Pascal nt s (nt ++ "[" ++ mapsep ", " (prettyLang Pascal) (fta [i,j]) ++ "]")
> ppVAccessPascal (VANonterm nt s@(TT (i1,j1) (i2,j2))) = optMacroAccess Pascal nt s (nt ++ "[" ++ mapsep ", " (prettyLang Pascal) (fta [i1,j1,i2,j2]) ++ "]")

> -- Targetlanguage C
> ppVarAccessC (v, _) = ppVAccessC v
> ppVAccessC (Direct v) = v
> ppVAccessC ((Pointer v1) :. v2) = ppVAccessC v1 ++ "->" ++ ppVAccessC v2
> ppVAccessC (Pointer v) = "(*" ++ ppVAccessC v ++ ")"
> ppVAccessC (v1 :. v2) = ppVAccessC v1 ++ "." ++ ppVAccessC v2
> ppVAccessC (ArrayElem i v) = ppVAccessC v ++ concatMap (\e -> "[" ++ pretty e ++ "]") i
> ppVAccessC (Cast dt va)    = "((" ++ ppDataTypeC dt ++ ")" ++ po ++ ppVAccessC va ++ pc ++ ")"
>              where (po,pc) = case va of
>                               (Direct _) -> ("","")
>                               otherwise  -> ("(",")")
> ppVAccessC (Address v) = "&(" ++ ppVAccessC v ++ ")"
> ppVAccessC (PreInc v)   = "(++" ++ ppVAccessC v ++ ")"
> ppVAccessC (PreDec v)   = "(--" ++ ppVAccessC v ++ ")"
> ppVAccessC (PostInc v)  = "(" ++ ppVAccessC v ++ "++)"
> ppVAccessC (PostDec v)  = "(" ++ ppVAccessC v ++ "--)"
> ppVAccessC (VANonterm nt s@(ST (i,j)))           = optMacroAccess C nt s (nt ++ "[" ++ mapsep "][" (prettyLang C) (fta [i,j])         ++ "]")
> ppVAccessC (VANonterm nt s@(TT (i1,j1) (i2,j2))) = optMacroAccess C nt s (nt ++ "[" ++ mapsep "][" (prettyLang C) (fta [i1,j1,i2,j2]) ++ "]")


> -- Targetlanguage Fortran
> ppVarAccessF (v, _) = ppVAccessF v
> ppVAccessF (Direct v) = v
> ppVAccessF ((Pointer v1) :. v2) = ppVAccessF v1 ++ "->" ++ ppVAccessF v2
> ppVAccessF (Pointer v) = "(*" ++ ppVAccessF v ++ ")"
> ppVAccessF (v1 :. v2) = ppVAccessF v1 ++ "." ++ ppVAccessF v2
> ppVAccessF (ArrayElem i v) = ppVAccessF v ++ "(" ++ sepList ", " (map pretty i) ++ ")"
> ppVAccessF (Cast dt va)    = "((" ++ ppDataTypeF dt ++ ")" ++ po ++ ppVAccessF va ++ pc ++ ")"
>              where (po,pc) = case va of
>                               (Direct _) -> ("","")
>                               otherwise  -> ("(",")")
> ppVAccessF (Address v) = "&(" ++ ppVAccessF v ++ ")"
> ppVAccessF (PreInc v)   = "(++" ++ ppVAccessF v ++ ")"
> ppVAccessF (PreDec v)   = "(--" ++ ppVAccessF v ++ ")"
> ppVAccessF (PostInc v)  = "(" ++ ppVAccessF v ++ "++)"
> ppVAccessF (PostDec v)  = "(" ++ ppVAccessF v ++ "--)"
> ppVAccessF (VANonterm nt s@(ST (i,j)))           = optMacroAccess F nt s (nt ++ "(" ++ mapsep ", " (prettyLang F) (fta [i,j]) ++ ")")
> ppVAccessF (VANonterm nt s@(TT (i1,j1) (i2,j2))) = optMacroAccess F nt s (nt ++ "(" ++ mapsep ", " (prettyLang F) (fta [i1,j1,i2,j2]) ++ ")")


> -- Targetlanguage Latex:
> ppVAccessLatex  (VANonterm nt (ST (i,j)))           = ppNameTex nt ++ "_{" ++ mapsep ", " (prettyLang Latex) [i,j] ++ "}"
> ppVAccessLatex  (VANonterm nt (TT (i1,j1) (i2,j2))) = ppNameTex nt ++ "_{" ++ mapsep ", " (prettyLang Latex) [i1,j1,i2,j2] ++ "}"
> -- die restlichen erstmal wie C...
> ppVAccessLatex x = ppVAccessC x

> ppNameTex ""     = ""
> ppNameTex (x:xs) = (if x=='_' then "\\_" else [x]) ++ ppNameTex xs

> -- allgemein:
> optMacroAccess pp nt s tl | isTbl nt   = tl
>                           | isMacro nt = macro nt s
>                           | otherwise  = macro nt s -- error $ "unknown table access prefix " ++ nt
>   where
>     isTbl   nt = isPrefix (parr prefixes) nt || isPrefix (pdiffarr prefixes) nt
>     isMacro nt = isPrefix (ptbl prefixes) nt || isPrefix (pdifftbl prefixes) nt
>     -- Makro-Zugriffe sehen genauso aus wie in Fortran:
>     macro nt s@(ST (i,j))           = nt ++ "(" ++ mapsep ", " (prettyLang pp) [i,j] ++ ")"
>     macro nt s@(TT (i1,j1) (i2,j2)) = nt ++ "(" ++ mapsep ", " (prettyLang pp) [i1,j1,i2,j2] ++ ")"

> -- | filter table access; hier wird die Liste der Indizes gefiltert. Indizes mit obsoleteTableDim werden entfernt
> fta = filter isUsable
>     where
>       isUsable (Number n) = n /= obsoleteTableDim
>       isUsable x          = True


Java Pretty-Printer:

> ppVarAccessJava (va, _) = ppVAccessJava va

> ppVarAccessJavaLHS (va, _) = ppVAccessJavaLHS va

> ppVAccessJava (Direct name) = name

XXX remove the /* */ markers - only there for testing

> ppVAccessJava (Cast dt va) = "((" ++ ppDataTypeJava dt ++ "/* */)" ++ 
>   ppVAccessJava va ++ ")"

> ppVAccessJava (Pointer base :. Direct name) = ppVAccessJava base ++ "." ++ 
>   name

nur fuer pp_next auf der LHS

> ppVAccessJava (Pointer (Direct name)) = name


needed for Backtrace set

> ppVAccessJava (Pointer base) = ppVAccessJava base

> ppVAccessJava (base :. (Direct b)) = ppVAccessJava base ++ "." ++ b

> ppVAccessJava (ArrayElem mExps va) = ppVAccessJava va ++ "[" ++
>   sepList ", " (map pretty mExps) ++ "]"

> ppVAccessJava (VANonterm name subscripts@(ST (i,j))) = name ++
>   a ++ mapsep b (prettyLang Java) (fta [i,j]) ++ c
>   where
>     (a, b, c) = if isPrefix "tbl" name then ("(", ", ", ")")
>                                        else ("[", "][", "]")

> ppVAccessJava foo = "/* FIXME VAccess: " ++ (show foo) ++ " */"

XXX in java there is no casting at the left hand side allowed, if the
hole object gets casted ...

> ppVAccessJavaLHS (Cast _ va) = ppVAccessJava va
> ppVAccessJavaLHS foo = ppVAccessJava foo

> ppVAccessListJava (Direct name) = [name]
> ppVAccessListJava (Cast _ va) = ppVAccessListJava va
> ppVAccessListJava (Pointer base :. Direct name) = ppVAccessListJava base ++
>   [name]


Mapping Haskell-Datatype -> TL-Datatype:
-----------------------------------------

> knownHaskellTypes  = [
>   ("Char",    "%c",   TLChar), 
>   ("String",  "%s",   PointerOf TLChar), 
>   ("Int",     "%d",   TLInt), 
>   ("Integer", "%d",   TLInt), 
>   ("Float",   "%.2f", TLReal), 
>   ("Double",  "%f",   TLReal), 
>   ("Bool",    "%d",   TLBool),
>   ("_Result", "%???", TLResult)
>   ]

> knownHaskellTypes' = map fst3 knownHaskellTypes
> haskellTypeToDatatype typ = case dt of
>                               []        -> PointerOf (StructOf (pstr prefixes ++ typ) [])
>                               otherwise -> head dt 
>                     where dt = [dt | (n,_,dt) <- knownHaskellTypes, n == typ]


Tools:
------

> -- fuer Faelle, in denen der Datentyp irrelevant ist:
> toVA va = (va, TLVoid)

> structAccess :: VAccess -> [String] -> VAccess
> structAccess va []     = va
> structAccess va (s:ss) = structAccess (va :. (Direct s)) ss

> isVANonterm (VANonterm _ _) = True
> isVANonterm (e :. _)        = isVANonterm e
> isVANonterm _               = False

> getVANonterm (VANonterm n s) = (n,s)
> getVANonterm (e :. _)        = getVANonterm e
> getVANonterm x               = pattErr "getVANonterm" x

> getVANontermVA (VANonterm n s) = (VANonterm n s)
> getVANontermVA (e :. _)        = getVANontermVA e
> getVANontermVA x               = pattErr "getVANontermVA" x
