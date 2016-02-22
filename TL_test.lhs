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


> module TL_test
> where

> import TL_tng



t1 = Tl_Assign (TL_ASet (Tl_AVar Tl_Int_32 (Tl_Ident "foo"))) (Tl_Id (Tl_TableGet (Tl_OVar Tl_Table (Tl_Ident "Table 1")) (Tl_Dim (Tl_Id (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "x")))) (Tl_Id (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "y")))) ) ))


> t1 = Tl_Assign (Tl_ASet (Tl_AVar Tl_Int_32 (Tl_Ident "foo"))) 
>        (Tl_TableGet (Tl_Ident "table1") 
>         (Tl_Dim (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "x")))
>        (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "y")))))


> t2 = Tl_FnDef (Tl_Ident "movetables")
>   (Tl_Return Tl_Nothing)
>   (Tl_Parameter [(Tl_Atom Tl_Int_32 (Tl_Ident "p"))])
>   (Tl_FnHead [(Tl_Atom Tl_Int_32 (Tl_Ident "i")),
>               (Tl_Atom Tl_Int_32 (Tl_Ident "j"))])
>   [(Tl_ForUp (Tl_AVar Tl_Int_32 (Tl_Ident "j"))
>             (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "p")))
>             ((Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "n"))) :+ 
>              (Tl_CGet (Tl_ConstInt 1)))

>        [(Tl_ForUp (Tl_AVar Tl_Int_32 (Tl_Ident "i"))
>             (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "p")))
>             ((Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "j"))) :+ 
>              (Tl_CGet (Tl_ConstInt 1)))


>             [(Tl_Assign (Tl_TableSet (Tl_Ident "tbl_closed")
>               (Tl_Dim (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "i")) :-
>                        Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "p")))
>               (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "j")) :- 
>                        Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "p"))) ))
>               (Tl_TableGet (Tl_Ident "tbl_closed")
>                 (Tl_Dim (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "i")))
>                         (Tl_AGet (Tl_AVar Tl_Int_32 (Tl_Ident "j"))))
>    ))]
>   )]
>  )]

> map' :: (a->a) -> [a] -> [a]
> map' f (a:[]) = (a):[]
> map' f (a:as) = (f a):(map' f as)

> removeSem :: String -> String
> removeSem = filter (\n -> n /= ';')


> ppFn (Tl_FnDef (Tl_Ident name) ret param head stats) = 
>   (ppRet ret) ++ " " ++ name ++ "(" ++ (ppParam param) ++ ")\n{\n" 
>     ++ (ppFnHead head)
>     ++ (ppStats stats)
>     ++ "}\n\n"

> ppRet (Tl_Return decl) = ppDecl decl

> ppDecl Tl_Nothing = "void "
> ppDecl (Tl_Atom  dt (Tl_Ident name)) = ppDT dt ++ name

> ppDT Tl_Int_32 = "int "

> ppParam (Tl_Parameter decls) = foldr1 (++) (map' (++", ") (map (ppDecl) decls))

> ppFnHead  (Tl_FnHead decls) = unlines (map (++";") (map (ppDecl) decls))

> ppStats stats = unlines $ map (ppStat) stats

> ppStat (Tl_ForUp avar e1 e2 stats) =
>   "for (" ++ (ppStat (Tl_Assign (Tl_ASet avar) e1)) ++ " " ++
>     (ppCondExpr ((Tl_AGet avar) :< e2)) ++ "; " ++
>     removeSem (ppStat (Tl_Assign (Tl_ASet avar) ((Tl_AGet avar) 
>                                :+ (Tl_CGet (Tl_ConstInt 1))))) ++
>     ") {\n" ++
>     ppStats stats
>     ++ "\n}\n"

> ppStat (Tl_Assign (Tl_ASet (Tl_AVar _ (Tl_Ident lhs)))
>                   ((Tl_AGet (Tl_AVar _ (Tl_Ident x))) :+
>                     (Tl_CGet (Tl_ConstInt 1))))
>   | lhs == x = lhs ++ "++;"
>   | otherwise = lhs ++ " = " ++ x ++ " + 1;"

> ppStat (Tl_Assign (Tl_ASet (Tl_AVar _ (Tl_Ident lhs))) rhs) =
>   lhs ++ " = " ++ ppExpr rhs ++ ";"

> ppStat (Tl_Assign (Tl_TableSet (Tl_Ident lhs) (Tl_Dim j i)) rhs) =
>   lhs ++ ".set( " ++ ppExpr j ++ ", " ++ ppExpr i ++ ", " ++ ppExpr rhs 
>     ++ ");"

> ppExpr (Tl_TableGet (Tl_Ident table) (Tl_Dim x y)) =
>   table ++ ".get(" ++ (ppExpr x) ++ ", " ++ (ppExpr y) ++ ")"

> ppExpr (Tl_AGet (Tl_AVar _ (Tl_Ident vname))) = vname

> ppExpr (Tl_CGet c) = ppConstExpr c

> ppExpr (e1 :+ e2) = "(" ++ (ppExpr e1) ++ ") + (" ++ (ppExpr e2) ++ ")"

> ppExpr (e1 :- e2) = "(" ++ (ppExpr e1) ++ ") - (" ++ (ppExpr e2) ++ ")"

> ppCondExpr (e1 :< e2) = (ppExpr e1) ++ " < " ++ (ppExpr e2)

> ppConstExpr (Tl_ConstInt i) = show i

