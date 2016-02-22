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



> module Structs(

>   elemStruct,
>   allElemsStruct,
>   elemsStruct,
>   elemsStructNames,
>   getListItemDataType,
>   getUserTypeName,
>   isEnumPointer,
>   isListDT,
>   isListStruct,
>   isListDecl,
>   getStructDeclName,
>   isStruct,
>   isTupelStruct,
>   isTupel,
>   listItem,
>   listLast,
>   listNext,
>   listPointer,
>   listStep,
>   listStop,
>   listWrap,
>   makeList,
>   makeListStruct,
>   makeTupelStruct,
>   pointerType,
>   rev_Structs,
>   structElem,
>   toplevelElemsStruct,
>   tupelElemNumber,
>   getRelevantElem,
>   updStructName,
>   updStructNameDT,
>   varItem

> ) where

> import Constants
> import Tools
> import TL
> import TLData
> import Expr
> import PrettyPrint

> rev_Structs =  "$Revision$"


Strukturen und Listen
----------------------

> -- liefere ein Element einer Struktur mitsamt korrektem Datentyp
> structElem :: VarAccess -> String -> VarAccess
> structElem (v, StructOf _  elems) el = (v :. (Direct n), dt) 
>   where (n,dt) = head' [ (n,d) | (n,d) <- elems, n == el] $ "structElem: element " ++ el ++ " not found in structure " ++ pretty v
> structElem x el = pattErr ("structElem: " ++ el ++ " - ") x

> -- liefere das Feld "item" einer Liste
> listItem :: VarAccess -> VarAccess
> listItem (v, PointerOf ((StructOf _ [_, _, ("item", dt)]))) = ((Pointer v) :. (Direct "item"), dt)
> listItem x                                                  = (Direct $ "listItem: error: " ++ show x, TLInt)
> listItem x                                                  = pattErr "listItem" x

> -- liefere das Feld "next" einer Liste
> listNext :: VarAccess -> VarAccess
> listNext (v, dt)  = ((Pointer v) :. (Direct "next"), dt)
> -- listNext (v, PointerOf (StructOf _ [("next", dt), _, _])) = ((Pointer v) :. (Direct "next"), dt)
> -- listNext x                                                = pattErr "listNext" x

> -- liefere das Feld "last" einer Liste
> listLast :: VarAccess -> VarAccess
> listLast (v, PointerOf (StructOf _ [_, ("last", dt), _])) = ((Pointer v) :. (Direct "last"), dt)
> listLast x                                      = (Direct $ "listLast: error: " ++ show x, TLInt)
> listLast x                                      = pattErr "listLast" x 

> -- liefere den Listen-Pointer, zu dem ein item gehoert:
> listPointer :: VAccess -> VAccess
> listPointer (Pointer v)         = v
> listPointer (v :. (Direct _))   = listPointer v
> listPointer x                   = pattErr "listPointer" x

> -- erzeuge Code, der eine Liste korrekt beendet
> listStop :: VarAccess -> [TL]
> listStop v = [TLAssign (listNext v) TLNil, TLAssign (listLast v) (TLVar v)]

> -- erzeuge Code, um in einer Liste ein Element weiterzuspringen
> listStep :: VarAccess -> TL
> listStep v = TLAssign v (TLVar (listNext v))

> -- erzeuge Code, um ein Element in eine Liste zu packen
> listWrap :: MemType -> VarAccess -> VarAccess -> [TL]
> listWrap mt l@(_,PointerOf ldt) v = [TLAlloc mt l (ExpNum 1) ldt,
>                                      TLAssign (listItem l) (TLVar v)] ++ listStop l 
> listWrap mt x _                   = pattErr "listWrap" x

> -- varItem liefert den Inhalt einer Variablen, entweder direkt oder als Listen-Item:
> varItem  :: VarAccess -> VarAccess
> varItem l@(v, PointerOf (StructOf _ [_, _, ("item", dt)])) = listItem l
> varItem x                                                  = x

> -- liefere den Typ eines Pointers
> pointerType :: DataType -> DataType
> pointerType (PointerOf dt) = dt
> pointerType x              = pattErr "pointerType" x 

> -- liefere einen Pointer auf eine Variable:
> pointerTo :: VarAccess -> VarAccess
> pointerTo (v, dt) = (Pointer v, PointerOf dt)
> -- pointerTo x       = pattErr "pointerTo" x

> -- Erzeuge eine Listenstruktur fuer einen Datentyp
> makeListStruct :: DataType -> DataType
> makeListStruct dt   = PointerOf $ StructOf name [("next", PointerOf (StructOf name [])), ("last", PointerOf (StructOf name [])), ("item", dt)]
>   where 
>     name = "list_" ++ take 10 (clean $ ppDataTypeC dt )
>     clean [] = []
>     clean (x:xs) | elem x " *{};.->" = clean xs
>                  | otherwise         = x:clean xs

> makeList (v, dt) = (v, makeListStruct dt)

> -- liefere den item-Datentyp einer Listenstruktur
> getListItemDataType :: DataType -> DataType
> getListItemDataType (PointerOf (StructOf _ [_, _, ("item", dt)])) = dt
> getListItemDataType x                                             = pattErr "getListItemDataType" x

> isListStruct :: VarAccess -> Bool
> isListStruct (_,(PointerOf ((StructOf _ [("next", _), ("last", _), ("item", _)])))) = True
> isListStruct _                                                                      = False

> isListDT :: DataType -> Bool
> isListDT (PointerOf ((StructOf _ [("next", _), ("last", _), ("item", _)]))) = True
> isListDT _                                                                  = False

> isListDecl :: TypeDecl -> Bool
> isListDecl (StructDecl _ [(["next"],PointerOf (StructOf _ [])),
>                           (["last"],PointerOf (StructOf _ [])),
>                           (["item"],StructOf _ [])])             = True
> isListDecl _                                                     = False

> getStructDeclName :: TypeDecl -> String
> getStructDeclName (StructDecl name _) = name
> getStructDeclName x = pattErr "getStructDeclName" x

> -- Erzeuge eine Tupel-Struktur aus einer Liste von Datentypen
> makeTupelStruct :: [DataType] -> DataType
> makeTupelStruct dts = StructOf "" (map (\(n, dt) -> ("tup" ++ show n, dt)) (zip [1..] dts))

> isTupelStruct :: VarAccess -> Bool
> isTupelStruct (_, (StructOf _ (("tup1",_):("tup2",_):ds))) = True
> isTupelStruct _                                            = False

> isTupel (StructOf _ (("tup1",_):("tup2",_):ds)) = True
> isTupel _ = False

> isFirstTupelElem :: VAccess -> Bool
> isFirstTupelElem ( _ :. (Direct "tup1")) = True
> isFirstTupelElem _                       = False

> tupelElemNumber :: VAccess -> Int
> tupelElemNumber ( _ :. (Direct name)) | take 3 name == "tup" = (read (drop 3 name)) :: Int
>                                       | otherwise            = 0
> tupelElemNumber _                                            = 0

> getRelevantElem :: VarAccess -> VarAccess
> getRelevantElem v | isTupelStruct v = head (allElemsStruct v)
>                   | otherwise       = v


> isStruct :: VarAccess -> Bool
> isStruct (_, (StructOf _ (("tup1",_):("tup2",_):ds)))                            = False
> isStruct (_, (PointerOf ((StructOf _ [("next", _), ("last", _), ("item", _)])))) = False
> isStruct (_, (StructOf _ _))                                                     = True
> isStruct _                                                                       = False

> dimStruct :: VarAccess -> Int
> dimStruct (_,(StructOf _ ts)) = length ts
> dimStruct x                   = pattErr "dimStruct" x

> elemStruct :: VarAccess -> Int -> VarAccess
> elemStruct t@(v,(StructOf _ ts)) n 
>      | n > dimStruct t = error $ "elemStruct: elem out of range: " ++ show t ++ "\nelem: " ++ show n
>      | otherwise       = (v :. (Direct (fst $ ts!!(n-1))), snd $ ts!!(n-1))
> elemStruct x _         = pattErr "elemStruct" x

> elemsStruct :: VarAccess -> [VarAccess]  
> elemsStruct t@(v,(StructOf _ ts)) = map (elemStruct t) [1..dimStruct t]
> elemsStruct x                     = pattErr "elemsStruct" x

> allElemsStruct :: VarAccess -> [VarAccess]
> allElemsStruct (v,(StructOf _ ts)) = concatMap (elemStruct v) ts
>   where
>     elemStruct v (name,(StructOf _ ts)) = concatMap (elemStruct (v :. (Direct name))) ts 
>     elemStruct v (name, dt)             = [(v :. (Direct name), dt)]
> allElemsStruct x                     = pattErr "allElemsStruct" x

> -- kopie von allElemsStruct, nur dass nicht rekursiv in die Strukturen hineingelaufen wird
> toplevelElemsStruct :: VarAccess -> [VarAccess]
> toplevelElemsStruct (v,(StructOf _ ts)) = concatMap (elemStruct v) ts
>   where
>     elemStruct v (name, dt)             = [(v :. (Direct name), dt)]
> toplevelElemsStruct x                     = pattErr "toplevelElemsStruct" x

> updStructName (newn :. (Direct "item"), _) (s :. (Direct "item")) = newn :. (Direct "item")
> updStructName newn                         (s :. ss)              = (updStructName newn s) :. ss
> updStructName (newn, _)                    s                      = newn
> -- updStructName x _                                                 = pattErr "updStructName" x

> updStructNameDT (v,dt) name = (updStructName (v,dt) name, dt)

> elemsStructNames t@(v,(StructOf _ ts)) = map fst ts
> elemsStructNames x                     = pattErr "elemsStructNames" x

> isEnumPointer t@(v,(PointerOf (StructOf _ []))) = True
> isEnumPointer _                                 = False

> getUserTypeName t@(v,(PointerOf (StructOf n []))) = n
> getUserTypeName x = pattErr "getUserTypeName" x


