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




> module TL_tng
> where


TODO
x Tuple
  Listen
  Backtracking
x Optimizsation fn like max/min

> data Tl_Program = Tl_Program Tl_Ident [Tl_Decl] [Tl_FnDef]
>         deriving Show;

> data Tl_Ident = Tl_Ident String
>         deriving Show;

> data Tl_Decl = Tl_Atom Tl_DT  Tl_Ident |
>                Tl_Obj  Tl_ADT Tl_Ident |
>                Tl_Nothing -- ^ i.e. void
>         deriving Show;

> data Tl_DT = Tl_Int_32  |
>              Tl_Uint_32 |
>              Tl_Int_64  |
>              Tl_Uint_64 |
>              Tl_Double  |
>              Tl_Tuple [Tl_DT]
>         deriving Show;

> data Tl_ADT = Tl_Table Tl_DT|
>               Tl_List Tl_DT |
>               Tl_BTree
>         deriving Show;

> data Tl_FnDef = Tl_FnDef Tl_Ident Tl_Return Tl_Parameter Tl_FnHead
>                   [Tl_Statement]
>         deriving Show;

> data Tl_Return = Tl_Return Tl_Decl
>         deriving Show;

> data Tl_Parameter = Tl_Parameter [Tl_Decl]
>         deriving Show;

> data Tl_FnHead = Tl_FnHead [Tl_Decl]
>         deriving Show;

> data Tl_Statement = Tl_If      Tl_Expr [Tl_Statement] |
>                     Tl_IfElse  Tl_Expr [Tl_Statement] [Tl_Statement] |
>                     Tl_Switch  Tl_Expr  [Tl_Case] Tl_Default |

>                     Tl_ForUp   Tl_AVar Tl_Expr Tl_Expr [Tl_Statement] |
>                     Tl_ForDown Tl_AVar Tl_Expr Tl_Expr [Tl_Statement] |
>                     Tl_For     Tl_AVar Tl_Expr Tl_Expr Tl_Expr [Tl_Statement] |
>                     Tl_While   Tl_Expr [Tl_Statement] |

>                     Tl_Assign  Tl_LHS Tl_Expr |
>                     Tl_Choose  Tl_LHS Tl_ChooseType [Tl_Expr] |

>                     Tl_NewTable Tl_DT Tl_Ident Tl_TableType Tl_Dim |
>                     Tl_DelTable Tl_DT Tl_Ident |

>                     Tl_NewList Tl_DT Tl_Ident |
>                     Tl_Next Tl_DT Tl_Ident    |
>                     Tl_DelList Tl_DT Tl_Ident
>         deriving Show;

> data Tl_ChooseType = Max |
>                      Min |
>                      Sum
>         deriving Show;

> data Tl_Case = Tl_Case Tl_ConstExpr [Tl_Statement]
>         deriving Show;

> data Tl_Default = Tl_None |
>                   Tl_Default [Tl_Statement]
>         deriving Show;

> data Tl_AVar = Tl_AVar Tl_DT Tl_Ident
>         deriving Show;

> data Tl_OVar = Tl_OVar Tl_ADT Tl_Ident
>         deriving Show;

> data Tl_Expr = Tl_Expr :+ Tl_Expr |
>                Tl_Expr :- Tl_Expr |
>                Tl_Expr :* Tl_Expr |
>                Tl_Expr :/ Tl_Expr |
>                Tl_Negate Tl_Expr    |
>                Tl_TableGet Tl_Ident Tl_Dim |
>                Tl_ListGet Tl_OVar |
>                Tl_HasNext Tl_OVar |
>                Tl_AGet Tl_AVar |
>                Tl_CGet Tl_ConstExpr |
>                TI_TGet Tl_AVar Int
>         deriving Show;

> data Tl_CondExpr = Tl_Expr :<= Tl_Expr |
>                    Tl_Expr :< Tl_Expr  |
>                    Tl_Expr :> Tl_Expr  |
>                    Tl_Expr :>= Tl_Expr |
>                    Tl_Expr :== Tl_Expr
>         deriving Show;

> data Tl_ConstExpr = Tl_ConstInt    Int |
>                     Tl_ConstDouble Double
>         deriving Show;

> data Tl_TableType = Tl_Quadr |
>                     Tl_Lin   |
>                     Tl_Const
>         deriving Show;

> data Tl_Dim = Tl_Dim Tl_Expr Tl_Expr
>         deriving Show;

> data Tl_LHS = Tl_ASet Tl_AVar |
>               Tl_TSet Tl_AVar Int | -- ^ Zuweisung an die Komponente eines Tupel
>               Tl_TableSet Tl_Ident Tl_Dim
>         deriving Show;


