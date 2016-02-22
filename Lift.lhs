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



> module Lift where

> import Tools
> import ParseTree

> adpLiftWheres :: Char -> ADPProgram -> ADPProgram
> adpLiftWheres sep (ADPProgram ts td atype algs (Grammar ln name ax tdf afs ps)) = 
>                   (ADPProgram ts td atype algs (Grammar ln name ax tdf afs (concatMap (lift ps []) ps)))
>   where

>     lift :: [GrammarFunDef] -> [String] -> GrammarFunDef -> [GrammarFunDef]
>     lift prods cnt (GrammarFunDef ln n args tt u wps) = [GrammarFunDef ln n' args tt u' []] ++ wps' 
>       where
>         n'   = pnameCnt (cnt ++[n])
>         wps' = concatMap (lift prods (cnt ++ [n])) wps
>         u'   = liftU (cnt ++ [n]) u

>         liftU :: [String] -> Production -> Production
>         liftU cnt tp = ren tp
>            where
>              ren (CombinatorAp ln p comb q)  = CombinatorAp ln (ren p) (getinternalName prods cnt comb) (ren q)
>              ren (ProdExpr ln (FunAp i []))  = ProdExpr ln (FunAp (getinternalName prods cnt i) [])
>              ren (ProdExpr ln (Ident i))     = ProdExpr ln (Ident (getinternalName prods cnt i))
>              ren x                        = x
>     lift prods cnt (CombinatorDef ln n def)  = [CombinatorDef ln n' def]
>       where
>         n'   = pnameCnt (cnt ++[n])

>     getinternalName :: [GrammarFunDef] -> [Name] -> Name -> String
>     getinternalName prods cnt x = case ([intn | (intn, (GrammarFunDef ln n _ _ _ _)) <- filterProd cnt prods, n==x] ++
>                                         [intn | (intn, (CombinatorDef ln n _))       <- filterProd cnt prods, n==x]) of 
>                                     [] -> x
>                                     xs -> head xs

>       where
>         filterProd :: [String] -> [GrammarFunDef] -> [(String, GrammarFunDef)]  
>         filterProd cnt ps = map sel (flattenProd [] (concatMap (filterProd' cnt) ps)) 
>           where 
>             sel (n,_,p) = (n,p)
          
>         filterProd' :: [String] -> GrammarFunDef -> [GrammarFunDef]  
>         filterProd' cnt (GrammarFunDef ln n args tt tp wys) = sortProd [GrammarFunDef ln n args tt tp wys']
>              where  wys' = case cnt of 
>                             []      -> []
>                             (tc:rc) -> if tc==n then concatMap (filterProd' rc) wys else []
          
>         filterProd' cnt (CombinatorDef ln n def)          = sortProd [CombinatorDef ln n def]
          
>         sortProd :: [GrammarFunDef] -> [GrammarFunDef]  
>         sortProd [] = []
>         sortProd (d@(GrammarFunDef _ _ _ _ _  []):ps)  = sortProd ps ++ [d]
>         sortProd (d@(GrammarFunDef _ _ _ _ _ wys):ps)  = [d] ++ sortProd ps
>         sortProd (d@(CombinatorDef _ _ _):ps)            = sortProd ps ++ [d]
          
>         flattenProd :: [String] -> [GrammarFunDef] -> [(String, [String], GrammarFunDef)]
>         flattenProd  _ [] = []
>         flattenProd cnt ((GrammarFunDef ln n args tt tp wys):ps) = flattenProd (cnt ++ [n]) wys ++ 
>                                                                  [(pnameCnt (cnt ++ [n]), cnt ++ [n], GrammarFunDef ln n args tt tp [])] ++ 
>                                                                  flattenProd cnt ps
>         flattenProd cnt (d@(CombinatorDef _ n params):ps) =     [(pnameCnt (cnt ++ [n]), cnt ++ [n], d)] ++
>                                                                 flattenProd cnt ps
      
>     pnameCnt cnt = sepList [sep] cnt
