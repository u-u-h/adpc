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



%include tex/lhs2TeX.fmt

> module StringLib(

>   TermCode(..),

>   UserFunctions,

>   directDefYSize,
>   filterDef,
>   getArg,
>   laBounds,
>   rev_StringLib,
>   termDef

> ) where

> import Char
> import Constants
> import Syntax
> import MathExp
> import Expr
> import Tools
> import TLData

> rev_StringLib =  "$Revision$"

> type UserFunctions = ([(String,String,Bool,SYSize)], [(String,YSize)], [(String,SYSize)], [(String,YSize)])
> --                    Terminals                    , Filter          , Lookahead        , DirectDefs

Terminale Parser:
------------------

> data TermCode = TCEmpty | TCChar InputString MathExp | TCIFChar Char InputString MathExp | 
>                 TCIFiupac Char InputString MathExp | TCIFNotChar Char InputString MathExp | 
>                 TCRegion InputString SSubScripts | TCIFString String InputString SSubScripts | 
>                 TCLoc MathExp | TCUser String [Exp] | TCIFUser String [Exp]  deriving (Show)


 wird nicht mehr benoetigt:

 termListLexer = ["base", "basex", "basey", 
                  "uregion", "uregionx", "uregiony", 
                  "region", "regionx", "regiony",
                  "empty", 
                 "achar", "acharx", "achary", 
                  "astring", "astringx", "astringy", 
                  "astringp", "astringpx", "astringpy", 
                  "char", "charx", "chary", 
                  "string", "stringx", "stringy", 
                  "loc", "acharb","lbase"]

Für Two-Track

-- > termDef ufs (TST t)     = ST (termDef' ufs t)
-- > termDef ufs (TTT t1 t2) = TT (termDef' ufs t1)
-- >                                  (termDef' ufs t2)

-- > termDef_ys ufs t = case termDef ufs t of
-- >                      ST (y, _, _, _)                 -> ST y
-- >                      TT (y1, _, _, _) (y2, _, _, _)  -> TT y1 y2

-- > termDef_split ufs t = case termDef ufs t of
-- >                         ST (a, b, c, d)                       -> (ST a,     ST b,     ST c,     ST d)
-- >                         TT (a1, b1, c1, d1) (a2, b2, c2, d2)  -> (TT a1 a2, TT b1 b2, TT c1 c2, TT d1 d2)

> termDef_achar (name, args) inp = checkArgs name args 0 (ST (Number 1, Number 1), 
>                                                         SigId "Char", 
>                                                         (\(i,j) -> (ExpPOp ("tp_" ++ name) [ExpME i, ExpME j])),
>                                                         (\(i,j) -> TCChar inp j))

> termDef_char  (name, args) inp = checkArgs name args 1 (ST (Number 1, Number 1), 
>                                                         SigId "Char",   
>                                                         (\(i,j) -> (ExpPOp ("tp_" ++ name) [ExpChar (head (tail (getArg cChr 1 args))), ExpME i, ExpME j])),
>                                                         (\(i,j) -> TCIFChar (head (tail (getArg cChr 1 args))) inp j ))

> termDef_iupac  (name, args) inp = checkArgs name args 1 
>                                      (ST (Number 1, Number 1), 
>                                      SigId "Char",   
>                                      (\(i,j) -> (ExpPOp ("tp_" ++ name) 
>                                                  [ExpChar (head (tail (getArg cChr 1 args))), ExpME i, ExpME j])),
>                                      (\(i,j) -> TCIFiupac (head (tail (getArg cChr 1 args))) inp j ))

> termDef_region (name, args) inp ysize  = checkArgs name args 0 (ST ysize, 
>                                                           SigTupel [SigId "Int", SigId "Int"], 
>                                                           (\(i,j) -> (ExpPOp ("tp_" ++ name) [ExpME i, ExpME j])),
>                                                           (\(i,j) -> TCRegion inp (i, j)))

> termDef_string (name, args) inp = checkArgs name   args 1 (ST (strLength, strLength), 
>                                                           SigId "String",
>                                                           (\(i,j) -> (ExpPOp ("tp_" ++ name)  [ExpString str, ExpME i, ExpME j])),
>                                                           (\(i,j) -> TCIFString str inp (i, j) ))
>                               where 
>                                 str =  reverse (tail (reverse (tail (getArg cStr 1 args)))) 
>                                 strLength = Number (length str)


für die Verwendung der Beispiele:

> termDef _ ("base"     , args)  = termDef_achar ("base"     , args) _inpz
> termDef _ ("basex"    , args)  = termDef_achar ("basex"    , args) _inpx
> termDef _ ("basey"    , args)  = termDef_achar ("basey"    , args) _inpy

> termDef _ ("uregion"  , args)  = termDef_region ("uregion" , args) _inpz (Number 0, Infinite)
> termDef _ ("uregionx" , args)  = termDef_region ("uregionx", args) _inpx (Number 0, Infinite)
> termDef _ ("uregiony" , args)  = termDef_region ("uregiony", args) _inpy (Number 0, Infinite)

> termDef _ ("region"  , args)   = termDef_region ("region" , args)  _inpz (Number 1, Infinite)
> termDef _ ("regionx" , args)   = termDef_region ("regionx", args)  _inpx (Number 1, Infinite)
> termDef _ ("regiony" , args)   = termDef_region ("regiony", args)  _inpy (Number 1, Infinite)

Stringlib:

> termDef _ ("empty"    , args)  = checkArgs "empty"    args 0 (ST (Number 0, Number 0), 
>                                                           SigId "Bool", 
>                                                           (\(i,j) -> (ExpPOp "tp_empty" [ExpME i, ExpME j])),
>                                                           (\(i,j) -> TCLoc i))

> termDef _ ("acharb"   , args)  = checkArgs "acharb"   args 0 (ST (Number 1, Number 1), 
>                                                           SigId "Char", 
>                                                           (\(i,j) -> (ExpPOp "tp_acharb" [ExpME i, ExpME j])),
>                                                           (\(i,j) -> TCIFNotChar '$' _inpz j))

> termDef _ ("achar"     , args) = termDef_achar ("achar"     , args) _inpz
> termDef _ ("acharx"    , args) = termDef_achar ("acharx"    , args) _inpx
> termDef _ ("achary"    , args) = termDef_achar ("achary"    , args) _inpy

> termDef _ ("astring"  , args)  = termDef_region ("astring" , args)  _inpz (Number 0, Infinite)
> termDef _ ("astringx" , args)  = termDef_region ("astringx", args)  _inpx (Number 0, Infinite)
> termDef _ ("astringy" , args)  = termDef_region ("astringy", args)  _inpy (Number 0, Infinite)

> termDef _ ("astringp" , args)  = termDef_region ("astringp" , args) _inpz (Number 1, Infinite)
> termDef _ ("astringpx", args)  = termDef_region ("astringpx", args) _inpx (Number 1, Infinite)
> termDef _ ("astringpy", args)  = termDef_region ("astringpy", args) _inpy (Number 1, Infinite)

> termDef _ ("char"      , args)  = termDef_char   ("char"      , args) _inpz
> termDef _ ("charx"     , args)  = termDef_char   ("charx"     , args) _inpx
> termDef _ ("chary"     , args)  = termDef_char   ("chary"     , args) _inpy
> termDef _ ("iupac_base", args)  = termDef_iupac  ("iupac_base", args) _inpz

> termDef _ ("string"   , args)  = termDef_string ("string"   , args) _inpz
> termDef _ ("stringx"  , args)  = termDef_string ("stringx"  , args) _inpx
> termDef _ ("stringy"  , args)  = termDef_string ("stringy"  , args) _inpy

> termDef _ ("loc"      , args)  = checkArgs "loc"      args 0 (ST (Number 0, Number 0), 
>                                                           SigId "Int", 
>                                                           (\(i,j) -> (ExpPOp "tp_loc" [ExpME i, ExpME j])),
>                                                           (\(i,j) -> TCLoc i))

> termDef _ ("lbase"    , args)  = checkArgs "lbase"    args 0 (ST (Number 1, Number 1), 
>                                                           SigId "Int", 
>                                                           (\(i,j) -> (ExpPOp "tp_lbase" [ExpME i, ExpME j])),
>                                                           (\(i,j) -> TCLoc j))

> termDef ufs (t        , args)  = (ST ys, 
>                                   SigId dt,   -- the user has to define a legal haskell data type
>                                   (\(i,j) -> (ExpPOp ("tp_" ++ t) ((map ExpVar args) ++ [ExpME i, ExpME j]))),
>                                   tcode) 

>                                   where
>                                     (dt, addif, ys) = head' [(dt, addif, ys) | (n,dt,addif,ys) <- fst4 ufs, n == t] 
>                                                             ("unknown terminal " ++ t)
>                                     tcode = case addif of
>                                               False -> (\(i,j) -> TCUser   t ((map ExpVar args) ++ [ExpME i, ExpME j]))
>                                               True  -> (\(i,j) -> TCIFUser t ((map ExpVar args) ++ [ExpME i, ExpME j]))                              


Filter:
--------

> filterDef :: UserFunctions -> (String, [String]) -> (YSize, Bool, SubScripts -> Exp) 
> filterDef _ ("pairing", args)   = checkArgs "pairing" args 1 (ST (Number 2 , Infinite), True, fcodeParing (getArg cStr 1 args))
> filterDef _ ("pairingTT", args) = checkArgs "pairingTT" args 1 (TT (Number 2 , Infinite) (Number 2 , Infinite), True, fcodeParingTT (getArg cStr 1 args))

> filterDef _ ("pairingTTtop", args) = checkArgs "pairingTTtop" args 1 (TT (Number 2 , Infinite) (Number 0 , Infinite), True, 
>                                      fcodeParingTTtop (getArg cStr 1 args))
> filterDef _ ("pairingTTbot", args) = checkArgs "pairingTTbot" args 1 (TT (Number 0 , Infinite) (Number 2 , Infinite), True, 
>                                      fcodeParingTTbot (getArg cStr 1 args))
> filterDef _ ("pairingTTcross", args) = checkArgs "pairingTTcross" args 1 (TT (Number 1 , Infinite) (Number 1 , Infinite), True, 
>                                        fcodeParingTTcross (getArg cStr 1 args))

> filterDef _ ("minsize", args) = checkArgs "minsize" args 1 (ysize, False, fcodeBounds ysize) 
>                                where 
>                                   ysize = ST (strtoAInt (getArg cInt 1 args) , Infinite)
> filterDef _ ("maxsize", args) = checkArgs "maxsize" args 1 (ysize, False, fcodeBounds ysize)
>                                where 
>                                   ysize = ST (Number 0, strtoAInt (getArg cInt 1 args))
> filterDef _ ("size",    args) = checkArgs "size"    args 2 (ysize, False, fcodeBounds ysize)
>                                where 
>                                   ysize = ST (strtoAInt (getArg cInt 1 args) , strtoAInt (getArg cInt 2 args))
> filterDef _ ("minsizeTT", args) = checkArgs "minsizeTT" args 2 (ysize, False, fcodeBounds ysize) 
>                                where 
>                                   ys1 = (strtoAInt (getArg cInt 1 args) , Infinite)
>                                   ys2 = (strtoAInt (getArg cInt 2 args) , Infinite)
>                                   ysize = TT ys1 ys2
> filterDef _ ("maxsizeTT", args) = checkArgs "maxsizeTT" args 2 (ysize, False, fcodeBounds ysize) 
>                                where 
>                                   ys1 = (Number 0, strtoAInt (getArg cInt 1 args))
>                                   ys2 = (Number 0, strtoAInt (getArg cInt 2 args))
>                                   ysize = TT ys1 ys2
> filterDef _ ("maxsizeTTtop", args) = checkArgs "maxsizeTTtop" args 1 (ysize, False, fcodeBounds ysize) 
>                                where 
>                                   ys1 = (Number 0, strtoAInt (getArg cInt 1 args))
>                                   ys2 = (Number 0, Infinite)
>                                   ysize = TT ys1 ys2
> filterDef _ ("maxsizeTTbot", args) = checkArgs "maxsizeTTbot" args 1 (ysize, False, fcodeBounds ysize) 
>                                where 
>                                   ys1 = (Number 0, Infinite)
>                                   ys2 = (Number 0, strtoAInt (getArg cInt 1 args))
>                                   ysize = TT ys1 ys2

> filterDef _ ("sizeTT", args)  = checkArgs "maxsizeTT" args 4 (ysize, False, fcodeBounds ysize) 
>                                where 
>                                   ys1 = (strtoAInt (getArg cInt 1 args), strtoAInt (getArg cInt 2 args))
>                                   ys2 = (strtoAInt (getArg cInt 3 args), strtoAInt (getArg cInt 4 args))
>                                   ysize = TT ys1 ys2

> filterDef _ ("basepairing", _)        = (ST (Number 2, Infinite), True, fcodeUser ("basepairing",[]))
> filterDef _ ("stackpairing", _)       = (ST (Number 4, Infinite), True, fcodeUser ("stackpairing",[]))
> filterDef _ ("contains_region",[arg]) = (ST (Number 1, Infinite), True, fcodeUser ("tbl_contains_" ++ init(tail arg), []))

> filterDef ufs (f,       args) = (ST (Number 0, Infinite), True, fcodeUser (f, args))

 filterDef ufs (f,       args) = (head' [ys | (n,ys) <- snd4 ufs, n == f] 
                                     ("StringLib.lhs: unknown filter " ++ f), True,
                                 fcodeUser (f, args))

> fcodeEqual  (ST (i,j))               = ExpIOp   (ExpInput _inpz (calcME (i :+ (Number 1)))) "==" (ExpInput _inpz j)
> fcodeParing   f (ST (i,j))           = ExpPOp f [ExpInput _inpz (calcME (i :+ (Number 1))),       ExpInput _inpz j]

> fcodeParingTT f (TT (i1,j1) (i2,j2))    = ExpIOp (ExpPOp f [ExpInput _inpx (calcME (i1 :+ (Number 1))),      ExpInput _inpx j1]) "&&"
>                                                  (ExpPOp f [ExpInput _inpy (calcME (i2 :+ (Number 1))),      ExpInput _inpy j2])
> fcodeParingTTtop f (TT (i1,j1) (i2,j2)) =        (ExpPOp f [ExpInput _inpx (calcME (i1 :+ (Number 1))),      ExpInput _inpx j1]) 
> fcodeParingTTbot f (TT (i1,j1) (i2,j2)) =        (ExpPOp f [ExpInput _inpy (calcME (i2 :+ (Number 1))),      ExpInput _inpy j2])
> fcodeParingTTcross f (TT (i1,j1) (i2,j2)) =      (ExpPOp f [ExpInput _inpx j1, ExpInput _inpy j2])

> fcodeUser   (f,args) (ST (i,j))  = ExpPOp f (map ExpVar args ++ [ExpME i, ExpME j])

> fcodeBounds (ST (ysl, Infinite))   (ST (bl, bu))  = ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME ysl)
> fcodeBounds (ST (ysl, ysu))        (ST (bl, bu)) 
>    | ysl == ysu = ExpIOp  (ExpME (calcME (bu :- bl))) "==" (ExpME (ysl)) 
>    | otherwise  = ExpIOp  (ExpIOp  (ExpME (calcME (bu :- bl))) ">=" (ExpME ysl)) "&&"
>                           (ExpIOp  (ExpME  (calcME (bu :- bl))) "<=" (ExpME ysu))

> fcodeBounds (TT s1 s2) (TT y1 y2) = ExpIOp (fcodeBounds (ST s1) (ST y1)) "&&"
>                                           (fcodeBounds (ST s2) (ST y2))


LookAhead Funktionen:
---------------------

> laBounds :: UserFunctions -> String -> YSize
> laBounds ufs f = ST (head' [ys | (n,ys) <- thd4 ufs, n == f]
>                             ("unknown lookahead function " ++ f))

Direct definitions:
---------------------

> directDefYSize :: UserFunctions -> String -> YSize
> directDefYSize ufs f = (head' [ys | (n,ys) <- fth4 ufs, n == f]
>                           ("no yield size specified for direct parser definition " ++ f ++ "."))

Hilfsfunktionen:
-----------------

wandelt einen String in einen Wert vom Typ MathExp um:

> strtoAInt :: String -> MathExp
> strtoAInt s | all isDigit s = Number (strtoInt s)
>             | intNumber s   = Number (d2i s)
>             | otherwise     = Var s
>  where
>    intNumber :: String -> Bool
>    intNumber s = let (a,b) = span (/= '.') s
>                  in (all isDigit a && (b == ".0"  || b == ""))

>    d2i :: String -> Int
>    d2i s = let (a,b) = span (/= '.') s
>            in (read a) :: Int


liefert Element n der Argumentliste args:

> getArg tcheck n args = if tcheck e then e else error "wrong argument type for filter"
>                             where e = head (drop (n-1) args)

> cChr e = True
> cStr e = True
> cInt e = intNumber e || all isIdentChar e   -- fuer Variablenangaben in Filtern
>   where
>     isIdentChar c = isAlphaNum c || elem c "_"

>     intNumber :: String -> Bool
>     intNumber s = let (a,b) = span (/= '.') s
>                   in if (b /= ".0"  && b /= "") then error $ "parser: integer value required: " ++ show s
>                                                 else True


überprüft, ob die Argumentliste die benötigte Zahl der Argumente enthält.

 checkArgs :: String -> [a] -> Int -> (YSize, (MathExp -> MathExp -> MathExp)) -> (YSize, (MathExp -> MathExp -> MathExp))

> checkArgs f args n e 
>   | an > n    = error ("too many arguments for " ++ f)
>   | an < n    = error ("missing argument for " ++ f)
>   | otherwise = e 
>   where an = length args

