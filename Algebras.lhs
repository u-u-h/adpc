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



Backtrace - TODO
------------------

TWO-VAR-HACK korrigieren, indem Result-Typen einen Prefix haben
muessen. Falls keiner vorhanden, wird standardmaessig wie gehabt nur
der letzte Typ genommen

refactor
--------
V globdiff in pp nur bei terminierungsfaellen notwendig
V entryref als globale function
- nur jeweils eine Zuweisung entry = entryref, dann verwendung von
  entry -> zielcode wesentlich uebersichtlicher
  -> hier allerdings variablen fuer jeden signatur-Operator notwendig
- enum/pp-handling als kopie sehr unschoen

Main-Topics
-----------

- Zusammenhang zwischen Speicherbedarf, der wirklich fuer die
  Kandidaten verwendet wird und dem Peak-Speicherbedarf
  herstellen. Die Ausgabe von purify laesst vermuten, dass es hier
  eine grosse Diskrepanz gibt. Z.B. (globsimBack2compiled4rar):

  memtime:      Max VSize = 12424 KB
  purify:   5725657 Bytes =  5591 KB

  Ein moeglicher test waere die Verwendung einer eigenen
  allok-Funktion, die bei den new_enum_*-Aufrufen mitrechnet, wieviel
  Speicher hier allokiert wird.

  => Resultat:    14159772  mcalloc
                    ~"~     purify
                  29768000  memtime
  => Test mit groesseren Eingaben ergibt jeweils Faktor von ca. 2.1

  => Wenn dieses auch bei anderen Programmen so waere, waere das
     sicherlich in Ordnung. Es koennte allerdings sein, dass es sich
     hier aufgrund der grossen Anzahl von Kandidaten so verhaelt. Im
     Normalfall wird man wesentlich weniger Kandidaten haben, so dass
     hier vielleicht der Faktor wesentlich groesser wird. Das gilt es
     als naechstes zu untersuchen

- Speicher- und Effizienzvergleich mit der urspruenglichen
  Implementierung (fuer eine Datenstruktur). Hier ist ..compiled4org
  entsprechend vorbereitet, allerdings gibt es hier noch ein
  Speicherloch
  => Speicherloch bei org nun beseitigt: Effizienz wie folgt:
     rar 138.45 user, 49.32 system, 190.62 elapsed -- Max VSize = 1449824KB, Max RSS = 1449392KB
     org: 92.79 user, 49.24 system, 144.22 elapsed -- Max VSize = 1923064KB, Max RSS = 1922632KB

  ==> weitere Vorgehensweise: Compiler erweitern, so dass Freigabecode
      entsprechend org erzeugt wird. Dann Tests mit praxisrelevanteren
      Beispielen (nicht so exorbitant viele Kandidaten). Z.B. original
      Globsim von der Website: 8 optimale Kandidaten vs. 4 mio
      Kandidaten hier. Oder noch besser: local sim. with affine gap
      scores: 1 vs 260000 candidates.

V forward-Deklarationen fuer backtrace-Funktionen
V Spezieller Modus zum Aufbauen nur eines einzelnen Kandidaten
- Suboptimale Kandidaten. Dies sollte relativ einfach zu machen
  sein. Es wird beim aufbauen der Ergebnisliste dann halt nicht auf
  Gleichheit, sondern auf Differenz ueberprueft. Vielleicht kann man
  auch mit Durchschnittswerten agieren.

- Vergleich mit der Online-Enum Variante (angereichert mit
  spezialisierten free-Funktionen). Wenn diese schneller waere, waere
  das allerdings sehr merkwuerdig

------------------------------------------

- strings wie struct, isstrucured, etc. als konstanten
V Direct "alg_enum" nicht als konstante!
- <<resType = ntType opts il2dt algs axiom>> nicht ganz sauber. Was
  passiert hier, wenn wir Produktionen mit unterschiedlichen
  Ergebnistypen vorliegen haben?
V Fall des einzeln vorkommenden Nichtterminals abdecken.
- was passiert bei Konstruktoren mit mehreren Algebra-Verweisen
  (z.b. Bill = Mult Bill Char Bill)? Hier muesste es eigentlich eine
  new-Funktion fuer jede Kombination aus NT und Struktur geben. Dass
  kann es wohl nicht sein. Moeglicherweise schafft eine vorherige
  Grammatik-Analyse Abhilfe
V snd4 opts und Varianten recht unsauber. opts besser als abstrakten
  Datentyp implementieren
- Spezialfaelle wie Twotrack und Tupel-Strukturen 
- Was passiert, wenn wir schon in den "forward-Tabellen" Listen
  vorliegen haben?
- Speicherfreigaben

- Test: globsimBack2compiled4rar/rbr: neue Implementierung mit
  zusaetzlichen Datenstrukturen. Vermutung: Dadurch, dass der Speicher
  (bei rar - release after recursion) erst nach der Rekursion
  freigegeben wird, sollte der Speicherverbrauch enorm ansteigen, da
  erst nach Abschluss der Rekursionen der Speicher wieder freigegeben
  werden kann. rbr ist eine alternative Implementierung, bei der
  soweit wie moeglich der Speicher schon vor den rekursiven Aufrufen
  freigegeben wird. Dies ist aber vom Laufzeitverhalten
  merkwuerdigerweise fast identisch (siehe globsimBack2compiled4.tex).

- Testen: Zahl der Aufrufe von back_Alignment zaehlen.
- Zum Testen des Speicherbedarfs vielleicht eigene alloc-Funktion
  verwenden. Interessant waere auf jeden Fall die Groesse des fuer die
  eigentlichen Kandidaten verwendeten Speichers (new_enum_*).
- Mechanismus in pp_str_Alignment einbauen, der nur die Anzahl der
  Kandidaten zaehlt und die Ausgabe unterdrueckt.

- Fehler momentan beim Erzeugen der new_struct-Aufrufe. Hier wird
  direkt der str3 Pointer uebergeben. wenn diese Liste mehr als
  einelementig wird, wird in jedem Iterationsschritt jeweils die um
  eins reduzierte Liste uebergeben. Das ist auf jeden Fall Murks. Im
  aktuellen Beispiel tritt dieses Problem nicht auf, da die Liste
  (hier: v6) sowieso nur maximal einelementig ist. Loesungsansatz in
  globsimBack2compiled4rarFixed.c

- in Codegen: die drei Funktionen insertVarAccess, updateIndices und
  compileConstr werden jeweils nur einmal und im gleichen Ausdruck
  verwendet. Jede enthaellt allerdings Code zum Umbau entsprechend des
  backtracings. Hier vielleicht aufraeumen angebracht.

> module Algebras(

>   SpecialChoice(..),
>   SpecialChoiceUse,
>   btWorkDataType,
>   compileSig,
>   compileSignature,
>   enrichSpecialChoice,
>   extractEnumAlg,
>   extractppAlgs,
>   generateEnumAlg,
>   generateSignature,
>   includeTypeDef,
>   includeTypeDefs,
>   isTerminal,
>   makeSpecialChoice,
>   mergeSignature,
>   ppSpecialChoiceUse,
>   ppSpecialChoiceUses,
>   ppSpecialChoiceUses',
>   rev_Algebras,
>   soAbsFct,
>   soDiffDatatype,
>   sortAlgebraDefs,
>   specialChoiceName

> ) where

> import Char
> import List
> import Constants
> import MathExp
> import TLData
> import TL
> import Tools
> import Expr
> import Dss
> import IL2
> import Structs
> import Track

> rev_Algebras =  "$Revision$"

Config: -- TODO: als Option

> -- VAR-HACK
> vhack = 1 :: Int

> soDiffDatatype = TLInt

> soAbsFct | soDiffDatatype == TLInt  = "abs"
>          | soDiffDatatype == TLReal = "fabs"
>          | otherwise                = error $ "soAbsFct: not defined for datatype " ++ ppDataTypeC soDiffDatatype

Zusammenführen von Signatur und TypeDefs
------------------------------------------

> mergeSignature :: [SignatureArea] -> [Signature]
> mergeSignature []                     = []
> mergeSignature [((name, ops), tdefs)] = [(name, map mergeOp ops)]   -- es kann nur maximal eine geben
>   where
>     mergeOp (name, args) = (name, map mergeArg args)
>     mergeArg (SigId arg) | elem arg (name:knownHaskellTypes') = SigId arg
>                          | length td > 0                     = head td
>                          | otherwise                         = error $ "unknown type " ++ arg ++ " in signature"
>             where td = [ def | (n, def) <- tdefs, n == arg]
>     mergeArg (SigTupel args) = SigTupel (map mergeArg args)
>     mergeArg (SigList arg)   = SigList (mergeArg arg)

> -- sortieren der Algebren nach der benutzerdefinierten Reihenfolge:
> sortAlgebraDefs :: [String] -> [AlgDefs] -> [AlgDefs]
> sortAlgebraDefs [] algfs = algfs
> sortAlgebraDefs algs algfs = reverse $ sad [] algs 
>  where
>    sad st []      = st
>    sad st ("":as) = sad st as -- needed to deal with missing pp-Algebra
>    sad st (a:as) | length f == 0 = error $ "cannot find algebra " ++ a
>                  | length f > 1  = error $ "more than one algebra named " ++ a
>                  | otherwise     = sad ((head f):st) as
>                  where
>                    f = [ algd | algd@(name, _) <- algfs, name == a]


> -- extrahiere die pp-Algebren aus der Algebraliste
> extractppAlgs :: [String] -> [AlgDefs] -> ([AlgDefs], [AlgDefs])
> extractppAlgs []          algfs = ([], algfs)
> extractppAlgs ppAlgNames  algfs -- | length ppAlgs > 1  = error $ "more than one pretty printing algebra " ++ ppAlgName ++ " specified."
>                                 -- | length ppAlgs == 0 = error $ "pretty printing algebra " ++ ppAlgName ++ " not found."
>                                 | length ppAlgs /= length ppAlgNames = error $ "can not find pretty printing algebras " ++ sepList " " ppAlgNames
>                                 | otherwise                          = (ppAlgs, otherAlgs)
>   where
>     ppAlgs      = filter (\(name, _) ->      elem name ppAlgNames')  algfs
>     otherAlgs   = filter (\(name, _) -> not (elem name ppAlgNames')) algfs
>     ppAlgNames' = map (\n -> palg prefixes ++ n) ppAlgNames


> -- falls backtrace-Option aktiviert, entferne enumeration-Algebra und liefere ihren Namen:
> extractEnumAlg :: BTMode -> [AlgDefs] -> (String, [AlgDefs])
> extractEnumAlg BTNone algfs = ("", algfs)
> extractEnumAlg _      algfs | length enumAlgs > 1 = error "more than one enumeration algebra specified"
>                             | otherwise           = (enumAlgName, otherAlgs)
>   where
>     enumAlgs = filter isEnum algfs 
>     isEnum (name, (algtd, afs)) = any isConstr afs
>     isConstr (n, typ, args, ExpConstr _ _) = True
>     isConstr _                             = False
>     enumAlgName = fst $ head' enumAlgs $ "for backtrace option, you need to specify an enumeration algebra\n" ++ 
>                                          "together with your scoring algebra. For example:  -al score enum"
>     otherAlgs = filter (\(name, _) -> name /= enumAlgName) algfs

> -- automatische Signatur-Generierung
> generateSignature :: [AlgDefs] -> [AlgebraTypeDecl] -> String -> [String] -> Signature
> generateSignature algfs algtd csAlgName usedAlgFcts
>     | algtd == [] = error "generateSignature: no algebra type declaration given"
>     | algfs == [] = error "generateSignature: no algebras given"
>     | otherwise   = (sigName, sigOperators)
>   where
>     sigName = "Signature"

>     typeDecl = head algtd
>     (tdName, tdTypeVars, tdDefs) = typeDecl

>     csAlg      = head' [alg | (n, alg) <- algfs, n == csAlgName] $ "algebra not found: " ++ csAlgName
>     algTypeDef = head' (fst csAlg) $ "no type definition given for algebra " ++ csAlgName
>     algFNames  = snd algTypeDef
>     algTypeVars = thd3 $ fst algTypeDef

>     binding | length algFNames /= length tdDefs =
>                  error  $ "algebra type " ++ tdName++ " defines " ++
>                           show (length tdDefs) ++ " functions\n" ++
>                           "but is used with " ++ show (length algFNames) ++ " functions " ++
>                           "in algebra " ++ csAlgName
>             | length (usedAlgFcts \\ algFNames) > 0 =
>                  error $ "Algebra function(s) " ++ sepList ", "  (usedAlgFcts \\ algFNames) ++ " are used in the grammar,\n" ++
>                          "but are not defined in algebra " ++ csAlgName ++ "."
>             | otherwise = let 
>                             -- Bindungen der Namen an die Typen; hier sind alle Algebrafunktionen einschliesslich
>                             -- der Auswahlfunktionen enthalten:
>                             binds = zip algFNames tdDefs
>                             -- Filterung der wirklich verwendeten Algebrafunktionen (ohne Auswahlfunktionen)
>                           in filter ((flip elem usedAlgFcts).fst) binds

>     -- TODO: TWO-VAR-HACK
>     -- varBinding = zip tdTypeVars ((init algTypeVars) ++ [SigId sigName])
>     varBinding = zip tdTypeVars ((initn vhack algTypeVars) ++ replicate vhack (SigId sigName))

>     sigOperators = map genOperator binding
>       where
>         genOperator (name, types) = (name', types')
>           where
>             name' = toUpper (head name):tail name
>             -- TODO: TWO-VAR-HACK
>             -- types' = map updateVars (init types)
>             types' = map updateVars (initn vhack types)
>             updateVars (SigId n) = let n' = [ bind | (nn, bind) <- varBinding, nn == n]
>                                    in case n' of
>                                        []        -> SigId n
>                                        otherwise -> head n'
>             updateVars (SigTupel ns) = SigTupel (map updateVars ns)
>             updateVars (SigList n)   = SigList (updateVars n)


> generateEnumAlg :: [AlgDefs] -> String -> [String] -> [String] -> AlgDefs
> generateEnumAlg algfs csAlgName usedChoiceFunctions usedAlgFcts = (enumAlgName, ([enumAlgTypeDef], enumAlgDef))
>   where
>     enumAlgName = "enum"
>     csAlg       = head' [alg | (n, alg) <- algfs, n == csAlgName] $ "algebra not found: " ++ csAlgName
>     algTypeDef  = head' (fst csAlg) $ "no type definition given for algebra " ++ csAlgName
>     ((_, algTypeName, algTypeVars), algFNames) = algTypeDef
>     -- TODO: TWO-VAR-HACK
>     -- enumAlgTypeDef = ((enumAlgName, algTypeName, (init algTypeVars) ++ [SigId "Signature"]), algFNames)
>     enumAlgTypeDef = ((enumAlgName, algTypeName, (initn vhack algTypeVars) ++ replicate vhack (SigId "Signature")), algFNames)
>     enumAlgDef = map genDef usedAlgFcts ++ 
>                  map genChc usedChoiceFunctions ++
>                  map genUnused (algFNames \\ (usedAlgFcts ++ usedChoiceFunctions))
>      where
>        genDef name    = (name, [], [], ExpConstr (toUpper (head name) : tail name) [])
>        genChc name    = (name, [], [SigId "x"],ExpChoice "id" (ExpVar "x"))
>        genUnused name = (name, [], [], ExpConstr "Unused" [])

> -- Erweitern des Algebra-Namens um ein Prefix. Ansonsten potentielle
> -- Namenskonflikte in der Zielsprache:
> expandAlgNames :: [AlgDefs] -> [AlgDefs]
> expandAlgNames algs = map (\((name, alg),n) -> (palg prefixes ++ name ++ optNumber name n, alg)) (zip algs [1..])
>    where
>      -- falls mehrmals gleiche Algebra, z.B. count *** count
>      optNumber name n = case length [nm | (nm,_) <- algs, nm == name] of
>                           1         -> ""
>                           otherwise -> show n

> includeTypeDefs :: [SignatureArea] -> [AlgebraTypeDecl] -> [AlgDefs] -> [AlgDefs]
> includeTypeDefs signatureArea algebraTypeDecl algdefs = expandAlgNames $ map (includeTypeDef signatureArea algebraTypeDecl) algdefs
> includeTypeDef  signatureArea algebraTypeDecl inpAlg@(thisAlgName, (thisAlgTypeDef, algDefs))
>   -- wenn kein Algebra-Typ angegeben ist, ist hier weiter nichts zu tun:
>   | thisAlgTypeDef == [] = inpAlg
>   -- die angegebenen Algebra-Namen muessen uebereinstimmen
>   | thisAlgName /= thisAlgNameTD = error $ "different names in algebra definition: \n" ++
>                                            "algebra[" ++ thisAlgName ++ "]{ ...    vs.\n" ++
>                                            thisAlgNameTD ++ " :: ....\n"
>   -- der angegebene Algebra-Typ muss definiert sein:
>   | thisAlgTypeName /= globalAlgTypeName = error $ "Unknown algebra type " ++ thisAlgTypeName ++ ".\n" ++
>                                                    "Make sure to mark its definition by algebratype{ ... }, e.g.:\n\n" ++
>                                                    "algebratype{ \n" ++
>                                                    "\n" ++
>                                                    "> type Globsim_Algebra alphabet answer = (\n" ++
>                                                    ">   alphabet -> answer,                       -- nil\n" ++
>                                                    ">   alphabet -> answer -> answer,             -- d\n" ++
>                                                    ">   answer -> alphabet -> answer,             -- i\n" ++
>                                                    ">   alphabet -> answer -> alphabet -> answer, -- r\n" ++
>                                                    ">   [answer] -> [answer]                      -- h\n" ++
>                                                    ">   )\n" ++
>                                                    "\n" ++
>                                                    "}\n"

>   -- die angegebenen Typen muessen die gleiche Anzahl an Argumenten haben:
>   | length thisAlgTypeArgs /= length globalAlgTypeArgs = error $ "algebra type " ++ thisAlgTypeName ++ " is used with " ++
>                                                                  show (length thisAlgTypeArgs) ++ " arguments\n" ++
>                                                                  "in algebra " ++ thisAlgName ++ 
>                                                                  " but defined with " ++ show (length globalAlgTypeArgs) ++ " arguments " 

>   -- die Anzahl der definieren Typen muss gleich sein:
>   | length thisAlgOrder /= length globalAlgFctTypes    = error $ "algebra type " ++ thisAlgTypeName ++ " defines " ++
>                                                                  show (length globalAlgFctTypes) ++ " functions\n" ++
>                                                                  "but is used with " ++ show (length thisAlgOrder) ++ " functions " ++
>                                                                  "in algebra " ++ thisAlgName
>   -- ansonsten kann es erstmal losgehen...
>   | otherwise = (thisAlgName, (thisAlgTypeDef, result))
>   where
>     -- Auseinandernehmen der Argumente
>     (sigName, userTypeDefs) = case signatureArea of
>                           []              -> ("",[])
>                           [((sign,_),td)] -> (sign,td)
>     (globalAlgTypeName, globalAlgTypeArgs, globalAlgFctTypes) = case algebraTypeDecl of
>                           []        -> ("", [], [])
>                           [(a,b,c)] -> (a,b,c)
>     ((thisAlgNameTD, thisAlgTypeName, thisAlgTypeArgs), thisAlgOrder) = head' thisAlgTypeDef  "includeTypeDef: should not happen"
>                                                                              -- muss an dieser Stelle einelementig sein
>     -------------------------------------------------------------

>     -- Einsetzen der lokalen Typdefinitionen und der Typ-Parameter in den globalen Algebra-Funktions-Typ:
>     updatedGlobalAlgFctTypes = map (map update) globalAlgFctTypes

>     update (SigId arg@(a:as)) | isUpper a && elem arg (sigName:knownHaskellTypes') = SigId arg
>                               | isUpper a && length td > 0                         = update (head td)
>                               | isUpper a = error $ "unknown type " ++ arg ++ " in algebra type declaration"
>                               | isLower a && argBind /= []                         = update (head argBind)
>                               | otherwise = error $ "unknown symbol " ++ arg ++ " in algebra type declaration"
>             where 
>                td = [ def | (n, def) <- userTypeDefs, n == arg]
>                argBind = [ typ | (symbol, typ) <- zip globalAlgTypeArgs thisAlgTypeArgs, symbol == arg]

>     update (SigTupel args) = SigTupel (map update args)
>     update (SigList arg)   = SigList (update arg)

>     -- Zusammenführen der Algebra mit den Typen:
>     bindedTypes = zip thisAlgOrder updatedGlobalAlgFctTypes 
>     result = map (coordArgType.updateType) algDefs 
>     updateType (fctName, _, args, rhs) 
>         | typ == []          = error $ "undefined type for algebra function " ++ fctName ++ " in\n" ++
>                                        "algebra " ++ thisAlgName ++ "."
>         | neededArgs == 0    = (fctName, htyp, args, rhs)

>         | neededArgs < 0     = error $ "algebra function " ++ fctName ++ " in algebra " ++ thisAlgName ++ "\n" ++
>                                        "is defined with " ++ show (length args) ++ " arguments " ++
>                                        "but only with " ++ show (length htyp - 1) ++ " types."
>         | not (canCurry rhs) = error $ "algebra function " ++ fctName ++ " in algebra " ++ thisAlgName ++ "\n" ++
>                                        "is defined with " ++ show (length args) ++ " arguments " ++
>                                        "but with " ++ show (length htyp - 1) ++ " types.\n" ++
>                                        "I tried to curryfy, but failed." 
>         | otherwise  = curryfy 

>             where
>               typ  = [ t | (n,t) <- bindedTypes, n == fctName ]
>               htyp = head' typ "updateType: should not happen"
>               neededArgs = (length htyp - 1) - length args 

>               canCurry (ExpConstr _ _) = True
>               canCurry (ExpPOp _ _)    = True
>               canCurry (ExpVar (v:vs)) = isLower v  -- ansonsten hier auch Zahlen erfolgreich
>               canCurry x               = False

>               curryfy = (fctName, htyp, cargs, crhs rhs)

>               addArgs = map (\n -> "cf" ++ show n) [1..neededArgs]
>               cargs                   =               args ++ (map SigId  addArgs)
>               crhs (ExpConstr n args) = ExpConstr n $ args ++ (map ExpVar addArgs)
>               crhs (ExpPOp    n args) = ExpPOp    n $ args ++ (map ExpVar addArgs)
>               crhs (ExpVar n)         = ExpPOp    n (map ExpVar    addArgs)

>     -- Anpassen von Argumenten an Typen (z.b) 
>     --      f :: (Int, Int) -> Int, f a = sizeof a
>     --  =>                          f(a1,a2) = sizeof (a1,a2)
>     -- Durch updateType ist schon sichergestellt, dass |atype| == |args|
>     coordArgType (fctName, atype, args, rhs) = (fctName, atype, newArgs, newRhs)
>       where
>         -- erzeuge neue Argument-Liste aus Typen
>         argName pref n = pref ++ show n 
>         newArgs = map (typeToArg "a") (zip [1..] (init atype))
>         typeToArg pref (n, SigId _)       = (SigId $ argName pref n)
>         typeToArg pref (n, SigTupel args) = (SigTupel (map (typeToArg (pref ++ show n)) (zip [1..] args)))
>         typeToArg pref (n, SigList l)     = typeToArg pref (n, l)

>         -- binde die aus der Typangabe erzeugte Argumentliste an die Argumentliste aus der Funktionsdefinition:
>         bind = concatMap bindArgs (zip args newArgs)
>         bindArgs (SigId old, SigId new)       = [(SigId old, sigToExp (SigId new))]
>         bindArgs (SigTupel old, SigTupel new) | length old /= length new = error typeerror
>                                               | otherwise = concatMap bindArgs (zip old new)
>         bindArgs (SigId old, SigTupel new)    = [(SigId old, sigToExp (SigTupel new))]
>         bindArgs (SigTupel old, SigId new)    = error typeerror
>         typeerror = "type error in algebra function " ++ fctName ++ " in algebra " ++ thisAlgName ++ "\n" 

>         sigToExp (SigId n)       = ExpVar n
>         sigToExp (SigTupel args) = ExpTupel (map sigToExp args)

>         -- aktualisiere die rechte Seite mit den neuen Argumentnamen:
>         newRhs = flattTupels $ updateRhs bind rhs
>           where
>             updateRhs bs (ExpVar v) = let nv = [ nv | (SigId v', nv) <- bs, v == v'] in
>                                           case nv of
>                                             []        -> ExpVar v
>                                             otherwise -> head nv
>             updateRhs bs x          = mapExp (updateRhs bs) x
  
>             -- entferne Tupel aus Funktionsapplikationen in der RHS
>             flattTupels (ExpPOp n args) = ExpPOp n (concatMap flattTupels' args)
>             flattTupels x               = mapExp flattTupels x

>             flattTupels' (ExpTupel args) = concatMap flattTupels' args
>             flattTupels' x               = [mapExp flattTupels x]

----------------------------------------------------------------------------------------------------
Anreichern der speziellen Auswahlfunktionen (sort, nub, etc.)
---------------------------------------------------------------

> data SpecialChoice = SpecialSortUp | SpecialSortDown | SpecialNub deriving (Show, Eq)
> type SpecialChoiceUse = (SpecialChoice, String, DataType)
> ppSpecialChoiceUse (ct, name, dt) = name ++ " :: " ++ ppDataTypeC dt ++ " (" ++ show ct ++ ")"
> ppSpecialChoiceUses [] = "-"
> ppSpecialChoiceUses cs = mapsep "\n" ppSpecialChoiceUse cs

> ppSpecialChoiceUses' (spec, alg) = ppAlgDefs alg ++ "\n\nspecial functions to be generated:\n" ++ ppSpecialChoiceUses spec

> enrichSpecialChoice :: [AlgDefs] -> [([SpecialChoiceUse], AlgDefs)]
> enrichSpecialChoice algdefs = map enrich algdefs
>   where
>     enrich :: AlgDefs -> ([SpecialChoiceUse], AlgDefs)
>     enrich (algName, (d, defs)) = (concat replaced, (algName, (d, defs')))
>        where (replaced, defs') = unzip $ map (enrichDef algName) defs

>     enrichDef :: String -> AlgDef -> ([SpecialChoiceUse], AlgDef)
>     enrichDef algName (name, dt, args, exp) = (nub replaced,(name, dt, args, exp'))
>       where

>         typeBind = zip (concatMap flattenTupels args) (concatMap flattenTupels dt)

>         flattenTupels (SigId a)     = [SigId a]
>         flattenTupels (SigTupel as) = concatMap flattenTupels as
>         flattenTupels (SigList a)   = [SigList a]

>         (replaced, exp')  = replDef [] exp

>         -- replDef only processes the first expression level
>         replDef cll (ExpTupel exps) = (cll', ExpTupel exps')
>             where
>               (cll', exps') = repl' cll (zip exps (flattenTupels (last dt)))
>               repl' cll []          = (cll, [])
>               repl' cll ((e,dt):es) = (cll'', e':es')
>                 where
>                   (cll', e') | toTLData' dt == PointerOf TLChar = let (cll', e') = repl cll e
>                                                                   in (cll', ExpPOp "mkStrEntry" [e'])
>                              | otherwise                        = repl cll e
>                   (cll'', es') = repl' cll' es

>         replDef cll exp = repl cll exp

>         repl cll (ExpPOp f exps) | f == "sortUp"   = ((SpecialSortUp,   compFct, toTLData dt):cll', ExpPOp "sort" (exps' ++ [ExpVar compFct]))
>                                  | f == "sortDown" = ((SpecialSortDown, compFct, toTLData dt):cll', ExpPOp "sort" (exps' ++ [ExpVar compFct]))
>                                  | f == "sort"     = ((SpecialSortUp,   compFct, toTLData dt):cll', ExpPOp "sort" (exps' ++ [ExpVar compFct]))
>                                  | f == "nub"      = ((SpecialNub     , compFct, toTLData dt):cll', ExpPOp "nub"  (exps' ++ [ExpVar compFct]))
>                                  -- falls pp-Algebra:
>                                  | toTLData dt == (PointerOf TLChar) = (cll', ExpPOp f exps') -- (cll', ExpPOp "strmk"  [ExpPOp f exps'])
>                                  | otherwise                         = (cll', ExpPOp f exps')
>             where 
>               (cll', exps') = collectExps repl cll exps
>               compFct       = specialChoiceName f (toTLData dt)

>         repl cll (ExpCons   e1 e2) = (cll'', ExpPOp "strcatH" [e1', e2'])
>                                      where
>                                        (cll',  e1') = repl cll  e1 
>                                        (cll'', e2') = repl cll' e2
>         repl cll (ExpAppend e1 e2) = (cll'', ExpPOp "strcatH" [e1', e2'])
>                                      where
>                                        (cll',  e1') = repl cll  e1 
>                                        (cll'', e2') = repl cll' e2
>         repl cll (ExpChar c)       = (cll, ExpString [c])
>         repl cll v@(ExpVar vn)     | typ  == (PointerOf TLChar) = (cll, v) -- (cll, ExpPOp "strmk" [v])
>                                    | otherwise                  = (cll, v)
>                                    where
>                                      bind = [dt | (SigId n, SigId dt) <- typeBind, n == vn] ++ ["void"] -- fuer Konstanten o.ae.
>                                      typ = haskellTypeToDatatype $ head' bind "enrichDef: unknown type"

>         repl cll x = collectExp repl cll x

>         -- vMk s = ExpVerb $ "\"" ++ s ++ "\"" -- ExpPOp "strmk" [ExpVerb $ "\"" ++ s ++ "\""]

>         toTLData dt   = toTLData' (last dt)
>         toTLData' dt  = case dt of
>                           (SigId t)           -> haskellTypeToDatatype t
>                           (SigTupel args)     -> makeTupelStruct (map toTLData' args)
>                           (SigList (SigId t)) -> haskellTypeToDatatype t
>                           x -> pattErr "enrichSpecialChoice" x

> specialChoiceName f dt = f ++ "_" ++ filter isAlphaNum (show dt)

----------------------------------------------------------------------------------------------------

> makeSpecialChoice :: [String] -> [AlgDefs] -> [SpecialChoiceUse]
> makeSpecialChoice chcFcts algdefs = concatMap build types
>   where
>      fcts = concatMap (getAlgDefList algdefs) chcFcts
>      types = nub $ map (toTLData.snd4) fcts
>      build dt = [(SpecialSortUp, specialChoiceName "sort" dt, dt), (SpecialNub, specialChoiceName "nub" dt, dt)]
>  
>      toTLData dt   = toTLData' (last dt)
>      toTLData' dt  = case dt of
>                           (SigId t)                           -> haskellTypeToDatatype t
>                           (SigTupel args)                     -> makeTupelStruct (map toTLData' args)
>                           (SigList (SigId t))                 -> haskellTypeToDatatype t
>                           (SigList (SigTupel args))           -> makeTupelStruct (map toTLData' args)
>                           --(SigList (SigTupel ((SigId t):ts))) -> haskellTypeToDatatype t
>                           x -> pattErr "makeSpecialChoice" x

----------------------------------------------------------------------------------------------------
Vergabe der Datenstrukturen, je nach BT-Modus
--------------------------------------------------

> sigStructDT sigName = PointerOf (StructOf (pstr prefixes ++ sigName) [])

> sigDataType opts sigName | elem (getOptBT opts) [BTList, BTSubOpt, BTSubOptCut, BTPF] 
>                                                    = makeListStruct dt
>                          | otherwise               = dt
>     where dt = sigStructDT sigName

> btWorkDataType opts sigName il2dt algs axiom | getOptBT opts == BTCompleteList = ntType opts il2dt algs axiom
>                                              | otherwise                       = sigDataType opts sigName 

----------------------------------------------------------------------------------------------------

> flattenTupels (SigId a)     = [SigId a]
> flattenTupels (SigTupel as) = concatMap flattenTupels as
> flattenTupels (SigList a)   = error "list type in signature not yet supported (DB:1)" --[SigList a]

> isTerminal sigName args = all (\(SigId typ) -> typ /= sigName) (concatMap flattenTupels args)

> entryRefF cName field = ((Pointer (Cast (PointerOf (StructOf (pstr prefixes ++ cName) []))
>                          ((Pointer (Direct "c")):.(Direct "entry")))) :.
>                          (Direct field))
> entryRef cName n        = entryRefF cName ("a" ++ show n)
> entryRefVA cName n      = toVA $ entryRef cName n
> entryRefFVA cName field = toVA $ entryRefF cName field
> entryRefN cName field n = toVA $ entryRefF cName (field ++ show n)

> compileSigs :: TMode -> String -> String -> CompileOptions -> [IL2Type] -> [AlgDefs] -> [AlgDefs] -> Signature -> [TL]
> compileSigs trackMode enumAlgName axiom opts il2dt algs ppAlg (orgSigName, ops) = 
>          [tlLongComment "signature"]                ++ compileDefines ++ sigStruct ++ sigConstr ++ 
>          [tlLongComment "signature operators"]      ++ concatMap (compileSig trackMode enumAlgName axiom opts il2dt algs orgSigName) ops ++ 
>          ppSig                                      ++
>          structureKiller                            ++
>          structureBuilder                           ++
>          structureUpdater
>    where
>      structureKiller | getOptBT opts == BTSingle = []
>                      | getOptBT opts /= BTNone   = compileCopySig enumAlgName axiom opts il2dt algs (orgSigName, ops) ++
>                                                    compileFreeSig enumAlgName axiom opts il2dt algs (orgSigName, ops)
>                      | otherwise                 = []

>      structureBuilder 
>          | getOptBT opts /= BTNone = compileBuildSig trackMode enumAlgName axiom opts il2dt algs (orgSigName, ops)
>          | otherwise               = []

>      structureUpdater 
>          | elem (getOptBT opts) [BTSubOpt, BTSubOptCut]
>                                      = compileUpdateSig trackMode enumAlgName axiom opts il2dt algs (orgSigName, ops)
>          | otherwise                 = []

>      sigName = pstr prefixes ++ orgSigName
>      compileDefines = [TLDefines $ map cD (zip [1..] ops)]
>        where cD (n, (cName, _)) = (TLVoid, psigid prefixes ++ cName,[],[tlvar (Direct (show n))])
>      (optFCalledD, optFCalledA) | isBTSubOpt opts = ([(["fcalled"], TLChar)],
>                                                      [TLAssign (toVA ((Pointer (Direct "t")):.(Direct "fcalled"))) (tlnumber 0)])
>                                 | otherwise       = ([],[])
>      sigStruct = [TLTypeDecls [StructDecl sigName $ [(["utype"], TLInt), (["entry"], PointerOf TLVoid)] ++ optFCalledD]]
>      sigConstr = [TLFD [] (sigDataType opts orgSigName) 
>                        (pnew prefixes ++ orgSigName)
>                        [(["u"], TLInt), (["entry"], PointerOf TLVoid)]
>                        ([(["t"], tmpVarStruct)] ++ optListVar)
>                        ([TLAlloc MTTemp (toVA tmpVar) (ExpNum 1) (StructOf sigName []),
>                          TLAssign (toVA ((Pointer tmpVar):.(Direct "utype"))) (tlvar (Direct "u")),
>                          TLAssign (toVA ((Pointer tmpVar):.(Direct "entry"))) (tlvar (Direct "entry"))] ++
>                          optFCalledA ++ 
>                          optListWrap)
>                        (tlvar resultVar)] 
>                        where 
>                          tmpVar        = Direct "t"
>                          listVar       = Direct "l"
>                          tmpVarStruct  = sigStructDT orgSigName
>                          listVarStruct = makeListStruct tmpVarStruct 
>                          (optListVar, optListWrap, resultVar) 
>                                        | elem (getOptBT opts) [BTList, BTSubOpt, BTSubOptCut, BTPF]
>                                                                  = ([(["l"], listVarStruct)], 
>                                                                      listWrap MTTemp (listVar, listVarStruct) (tmpVar, tmpVarStruct), listVar)
>                                        | otherwise               = ([], [], tmpVar)


>      -- fuer SubOptCut wird kein prettyprinter benoetigt
>      ppSig | getOptBT opts == BTSubOptCut = []
>            | ppAlg == []                  = compileppSig enumAlgName axiom opts il2dt algs ppAlg (orgSigName, ops)
>            | otherwise                    = concatMap (\pp -> compileppSig enumAlgName axiom opts il2dt algs [pp] (orgSigName, ops)) ppAlg

> compileppSig enumAlgName axiom opts il2dt algs ppAlgL (sigName, ops) = 
>   [TLFD (longComment "signature pretty printer") 
>         -- Ergebnistyp
>         (chooseBT opts [(btSubOpts,soDiffDatatype), ([],TLVoid)])
>         -- Name
>         (ppp prefixes ++ pstr prefixes ++ sigName)
>         -- Argumente
>         (ifBT [(["l"], btWorkDataType opts sigName il2dt algs axiom)] [(["c"], sigDataType opts sigName)])
>         -- lokale Variablen
>         (chooseBT opts [
>           ([BTNone],  []),   
>           (btSubOpts,[(["c"], sigStructDT sigName)] ++ scoreVars),
>           ([],        [(["c"], sigStructDT sigName)])
>           ])
>         -- body
>         (ifBT backtraceCodeBody (ppSig ops))
>         -- Rueckgabe
>         (chooseBT opts [(btSubOpts, tlvar (Direct "score")), ([], TLNil)])]
>   where
>     ifBT bt other | getOptBT opts /= BTNone = bt
>                   | otherwise               = other

>     ifppAlg pp other | ppAlgL == [] = other
>                      | otherwise    = pp
>     ppAlg = head ppAlgL

>     backtraceCodeBody = chooseBT opts 
>        [([BTSingle],       [TLIf (ExpIOp (ExpTLVar (Direct "l")) "/=" ExpNil)
>                               ([TLAssign (toVA (Direct "c")) (tlvar (Direct "l"))] ++ ppSigCode)
>                               []]),
>         ([BTCompleteList], [TLIf (ExpIOp (ExpTLVar (Direct "l")) "/=" ExpNil)
>                             ([TLAssign (toVA (Direct "c")) (tlvar ((((Pointer (Direct "l")):.(Direct "item"))) :. (Direct enumAlgName)))] ++ 
>                               ppSigCode) []]),
>         ([BTList] ++ btSubOpts,[TLIf (ExpIOp (ExpTLVar (Direct "l")) "/=" ExpNil)
>                                 ([TLAssign (toVA (Direct "c")) (tlvar ((((Pointer (Direct "l")):.(Direct "item")))))] ++ 
>                                  ppSigCode) []]),
>         ([], error $ "unknown backtrace mode " ++ show (getOptBT opts))]

>     ppSigCode = ppSig ops
>     opCount   = maximum (map (length.(concatMap flattenTupels).snd) ops)
>     scoreVars | isBTSubOpt opts  = [(["score"] ++ map (\n -> "score" ++ show n) [1..opCount], soDiffDatatype)]
>               | otherwise        = []

>     ppSig []                 = []
>     ppSig ((cName,args):ops) = ifBT [TLIfs cond thenExpBT elseExp]
>                                        [TLIfs cond thenExp   elseExp]
>       where
>         scoreAssign | isBTSubOpt opts  = [TLAssign (toVA (Direct "score")) (TLExp scoreExp)]
>                     | otherwise        = []
>             where
>               scoreExp = case rhs of
>                            -- falls hier eine Tupelstruktur vorliegt, berechnen wir den Score nur fuer das erste Tupelelement
>                            (ExpTupel (r:rs)) -> insertVarBinds binding r
>                            otherwise         -> insertVarBinds binding rhs 
>               -- TODO: toLower setzt voraus, dass alle Algebrafunktionen klein geschrieben werden. Dies ist nicht zwangslaeufig so.
>               (algDef_args, rhs) = getAlgDef_for_constr (head algs) cName
>               binding = map bind $ zip3 [1..] (concatMap flattenTupels algDef_args) (concatMap flattenTupels args) 
>               bind (n, SigId arg, SigId typ) | sigName == typ = (arg, ExpTLVar $ Direct $ "score" ++ show n)
>                                              | otherwise      = (arg, ExpTLVar $ entryRef cName n)
>               bind (_, _,  SigList _ )       = error "list type in signature not yet supported (DB:2)"

>         cond = ExpIOp (ExpTLVar ((Pointer (Direct "c")):.(Direct "utype"))) "==" (ExpTLVar (Direct defname))
>         condsBT = concatMap ccond (zip [1..] (concatMap flattenTupels args))
>           where
>             ccond (n, SigId typ) | sigName == typ = [ExpIOp (ExpTLVar (entryRef cName n)) "/=" ExpNil]
>                                  | otherwise      = []
>             ccond (n,SigList _ ) = error "list type in signature not yet supported (DB:3)"

>         thenExpBT | length condsBT == 0   = thenExp
>                   | isBTSingle opts       = thenExp
>                   | otherwise             = (if length condsBT >= 2 then [TLAssign (toVA $ Direct "rmAllowed") (tlnumber 0)]
>                                                                     else []) ++ [TLIf (expMakeAnd condsBT) thenExp []]

>         defname = psigid prefixes ++ cName
>         elseExp = ppSig ops
>         thenExp | ppAlgL == [] = printOpen ++ snd (printArgs 1 args) ++ scoreAssign
>                 | otherwise    = printPP cName args ppAlg            ++ scoreAssign
>           where
>             printOpen   = printNeeded (ifppAlg [] (vPrint (cName ++ " ") []))

>             printArgs n []     = (n,[])
>             printArgs n [a]    = pa n a
>             printArgs n (a:as) = let (n', paa)   = pa n a 
>                                      (n'', paas) = printArgs n' as 
>                                  in (n'',
>                                     paa ++
>                                     (vPrint " " []) ++
>                                     paas)

>             pa n (SigTupel args) = let (n', paa) = printArgs n args
>                                    in (n',
>                                       (vPrint "(" []) ++
>                                       paa                             ++
>                                       (vPrint ")" []))
>             pa n (SigId typ)     | typ == sigName = (n+1,genStackOperation (entryRef cName n) (entryRefN cName "pp_init_a" n) ++
>                                                          printNeeded (vPrint "(" []) ++
>                                                          [optScoreAssign ("score" ++ show n)
>                                                              (TLFA (ppp prefixes ++ pstr prefixes ++ sigName) [tlvar (entryRef cName n)])] ++
>                                                          printNeeded (vPrint ")" []))
>                                  | otherwise      = (n+1,(vPrint (formatTag typ) [tlvar (entryRef cName n)]))

>             pa n (SigList _ ) = error "list type in signature not yet supported (DB:4)"

>             optScoreAssign var ppCall           | isBTSubOpt opts = TLAssign (toVA (Direct var)) ppCall
>                                                 | otherwise       = ppCall

>             genStackOperation tlv pp_init_var   | getOptBT opts == BTSingle = []
>                                                 | otherwise                 = 
>                                                     ifBT [TLIf (ExpIOp (ExpTLVar (fst $ listNext $ toVA $ tlv)) "/=" ExpNil)
>                                                            -- then
>                                                            [TLIf (ExpVar "rmAllowed")
>                                                                -- then
>                                                                [TLAssign (toVA (Direct "removeAddr")) (tlvar $ Address $ tlv)]
>                                                                -- else
>                                                                [TLAssign (toVA (Direct "removeAddr")) (TLExp ExpNil)],
>                                                             TLAssign (toVA (Direct "pp_next")) (tlvar $ Address $ tlv),
>                                                             TLAssign (toVA (Direct "pp_initC")) (tlnumber (-1))] 
>                                                            -- else 
>                                                            [TLIf (ExpIOp (ExpVar "removeAddr") "==" ExpNil)
>                                                               -- then
>                                                               [TLAssign (toVA (Direct "pp_initC")) (TLExp (ExpIOp (ExpVar "pp_initC") "+" (ExpNum 1))),
>                                                                TLAssign (toVA (ArrayElem [Var "pp_initC"] (Direct "pp_init")))
>                                                                    (tlvar $ Address $ tlv),
>                                                                TLAssign (toVA (ArrayElem [Var "pp_initC"] (Direct "pp_initA")))
>                                                                    (TLVar $ pp_init_var)
>                                                               ]
>                                                               -- else 
>                                                               []]
>                                                          ]
>                                                          -- not BT 
>                                                          []
>                                                  --old:    ifBT [pushStack (tlvar (Address tlv))] []


>             --------------------------------------------------------------------------------
>             -- Prettyprinter mit pretty printing algebra
>             ---------------------------------------------
>             printPP cName cArgs ppAlg = printExp ppRhs
>               where
>               (ppArgs, ppRhs) = getAlgDef_for_constr ppAlg cName 
>               binding = let
>                           fcArgs  = map (\(SigId t) -> t) (concatMap flattenTupels cArgs)
>                           fppArgs = map (\(SigId t) -> t) (concatMap flattenTupels ppArgs)
>                         in if length fcArgs /= length fppArgs then error ("type error in pretty printing function " ++ cName)
>                               else zip3 fcArgs fppArgs (map (\n -> (entryRef cName n, entryRefN cName "pp_init_a" n, "score" ++ show n)) [1..])

>               printExp (ExpVar v) | found == [] = (vPrint (formatTag "String") [TLExp $ ExpVar v])
>                                   | otherwise   = let (typ, tlv, pp_init_var, scoreVar) = head found
>                                                       code | typ == sigName = genStackOperation tlv pp_init_var ++
>                                                                                [optScoreAssign scoreVar 
>                                                                                    (TLFA (ppp prefixes ++ pstr prefixes ++ sigName) [tlvar tlv])]
>                                                            | otherwise      = (vPrint (formatTag typ) [tlvar tlv])
>                                                    in code
>                 where found = [ (typ, tlv, pp_init_var, scoreVar) | (typ, n, (tlv, pp_init_var, scoreVar)) <- binding, n == v ]

>               printExp (ExpPOp fname args) = (vPrint (formatTag "String") [TLFA fname tlArgs])
>                 where 
>                  tlArgs = map (TLExp.(insertBind binding)) args
>                  insertBind bs (ExpVar v) | found == [] = ExpVar v
>                                           | otherwise   = let tlv = head found
>                                                           in ExpTLVar tlv
>                    where found = [ tlv | (typ, n, (tlv, _, _)) <- bs, n == v ]
>                  insertBind bs x = mapExp (insertBind bs) x
>               printExp (ExpCons   e1 e2) = printExp e1 ++ printExp e2
>               printExp (ExpAppend e1 e2) = printExp e1 ++ printExp e2 
>               printExp (ExpChar c)       = (vPrint [c] [])
>               printExp (ExpString s)     = (vPrint s [])
>               printExp x  = error $ "in pp function " ++ cName ++ ":\n" ++
>                                     "the following feature is currently not supported for pretty printing functions:\n" ++ show x

>         printNeeded p | cName == pNTID prefixes = []
>                       | otherwise               = p

>         vPrint s args | isBTSubOpt opts = [TLFA "sprintf" ([tlvar (Direct "pp_outp"), TLExp $ ExpString s] ++ args),
>                                            TLAssign (toVA (Direct "pp_outp")) 
>                                                      (TLExp  (ExpIOp (ExpTLVar (Direct "pp_outp")) "+" 
>                                                                      (ExpPOp "strlen" [ExpTLVar (Direct "pp_outp")])))]
>                       | otherwise      = [TLPrint s args]

>         formatTag typ   = head [ fmt | (n,fmt,_) <- knownHaskellTypes, typ == n]

> compileSig :: TMode -> String -> String -> CompileOptions -> [IL2Type] -> [AlgDefs] -> String -> SigOperator -> [TL]
> compileSig trackMode enumAlgName axiom opts il2dt algs orgSigName (cName, args) 
>                        | getOptBT opts /= BTNone = [tlLongComment $ "operator " ++ cName] ++
>                                                    [declarationBT] ++ constrFctBT
>                        | otherwise               = [declaration, constrFct]
>   where
>     sigName = pstr prefixes ++ orgSigName

>     -- eliminate Tupels from arguments:
>     fargs = concatMap flattenTupels args

>     -- struct declarations:
>     declaration = TLTypeDecls [StructDecl (pstr prefixes ++ cName) structElems]
>       where 
>          structElems = map sE (zip [1..] fargs)
>          sE (n, SigId typ)  | orgSigName == typ = (["a" ++ show n], sigDataType opts orgSigName)
>                             | otherwise         = (["a" ++ show n], haskellTypeToDatatype typ)
>          sE (n, SigList _)                       = error "list type in signature not yet supported (DB:5)"

>     -- struct declarations for backtrace:
>     declarationBT = TLTypeDecls [StructDecl (pstr prefixes ++ cName) structElems]
>       where 
>          structElems = concatMap sE (zip [1..] fargs) ++ optionalDiffDecl
>          optionalDiffDecl -- nur bei Terminierungsfaellen:
>                           | isTerminal orgSigName fargs && elem (getOptBT opts) [BTSubOpt, BTSubOptCut]
>                                                       = [(["diff"], soDiffDatatype)]
>                           | otherwise                 = []

>          sE (n, SigId typ)  | orgSigName == typ = let
>                                                     sn = show n 
>                                                     (diffType, diffDecl) 
>                                                        | elem (getOptBT opts) [BTSubOpt, BTSubOptCut]
>                                                                                    = ([soDiffDatatype],[(["diff" ++ sn], soDiffDatatype)])
>                                                        | otherwise                 = ([],[]) 
>                                                     a_decl 
>                                                        | getOptBT opts == BTSubOptCut = []
>                                                        | otherwise                    = [(["a" ++ show n], resType)]
>                                                     pp_init_decl
>                                                        | getOptBT opts == BTSubOpt = [(["pp_init_a" ++ sn], resType)]
>                                                        | otherwise                 = []
>                                                     (arity, indexVars) = case trackMode of
>                                                         MST -> (replicate 2 TLInt ++ diffType, ["i" ++ sn, "j" ++ sn])
>                                                         MTT -> (replicate 4 TLInt ++ diffType, ["i1" ++ sn, "j1" ++ sn, 
>                                                                                                 "i2" ++ sn, "j2" ++ sn])
>                                                   in
>                                                   [(["isStructure" ++ show n], TLInt),
>                                                    (["structure" ++ show n], resType)] ++
>                                                   a_decl ++
>                                                   pp_init_decl ++
>                                                   [(["f" ++ show n], FPointer arity resType),
>                                                    (indexVars, TLInt)] ++
>                                                    diffDecl
>                             | otherwise         = if getOptBT opts == BTSubOptCut then []
>                                                                                   else [(["a" ++ show n], haskellTypeToDatatype typ)] 
>          sE (n, SigList _)                      = error "list type in signature not yet supported (DB:6)"
>          resType = btWorkDataType opts orgSigName il2dt algs axiom 

>     -- constructor functions:
>     constrFct = TLFD [] resType name params localVars body resValue 
>       where
>         resType = sigDataType opts orgSigName
>         name    = pnew prefixes ++ cName
>         params    = (map argDecl (zip [1..] fargs))
>           where
>             argDecl (n, SigId typ) | orgSigName == typ = ([("a" ++ show n)], sigDataType opts orgSigName)
>                                    | otherwise         = ([("a" ++ show n)], haskellTypeToDatatype typ)
>             argDecl (n, SigList _)                     = error "list type in signature not yet supported (DB:7)"

>         localVars = [(["t"], PointerOf (StructOf (pstr prefixes ++ cName) []))]
>         resValue  = TLFA (pnew prefixes ++ orgSigName) [tlvar (Direct (psigid prefixes ++ cName)), tlvar tmpVar]
>         tmpVar    = Direct "t"
>         body = [TLAlloc MTTemp (toVA tmpVar) (ExpNum 1) (StructOf (pstr prefixes ++ cName) [])] ++
>                map assArgs (zip [1..] fargs)
>                where 
>                  assArgs (n,_) = TLAssign (toVA ((Pointer tmpVar):.(Direct ("a"++show n)))) (tlvar (Direct ("a"++show n)))

>     -- constructor functions for backtrace
>     -----------------------------------------------------
>     constrFctBT  = map fct combinations
>       where
>         combinations = mkCombs combs
>            where
>              combs = map f (zip [1..] fargs)
>              f (n, s@(SigId typ)) | cName == pNTID prefixes = [(n, "f", s)]
>                                   | orgSigName == typ = [(n, "f", s), (n, "s", s)]
>                                   | otherwise         = [(n, "", s)]
>              f (n, SigList _)                         = error "list type in signature not yet supported (DB:8)"
>              mkCombs []     = [[]]
>              mkCombs (x:xs) = [ a:as | a <- x, as <- mkCombs xs]


>         fct fargs = TLFD [] resType name params localVars body resValue
>           where
>             resType  = sigDataType opts orgSigName
>             listType = btWorkDataType opts orgSigName il2dt algs axiom 
	      
>             name     | cName == pNTID prefixes = pnew prefixes ++ cName
>                      | otherwise               = pnew prefixes ++ cName ++ "_" ++ concatMap snd3 fargs
>             params   = concatMap argDecl fargs
>               where
>                 argDecl (n, "f", SigId typ) | orgSigName == typ = 
>                                                              let
>                                                              sn = show n
>                                                              diffType
>                                                                 | elem (getOptBT opts) [BTSubOpt, BTSubOptCut] = [soDiffDatatype]
>                                                                 | otherwise                                    = []
>                                                              (arity, indexVars) = case trackMode of
>                                                                MST -> (replicate 2 TLInt ++ diffType, [(["i" ++ sn], TLInt),  (["j" ++ sn], TLInt)])
>                                                                MTT -> (replicate 4 TLInt ++ diffType, [(["i1" ++ sn], TLInt), (["j1" ++ sn], TLInt),
>                                                                                                        (["i2" ++ sn], TLInt), (["j2" ++ sn], TLInt)])
>                                                              in [(["f" ++ sn], FPointer arity listType)] ++ indexVars
>                 argDecl (n, "s", SigId typ) | orgSigName == typ = [([("structure" ++ show n)], listType)]
>                 argDecl (n, _, SigId typ)   | getOptBT opts == BTSubOptCut 
>                                                                 = []
>                                             | otherwise         = [([("a" ++ show n)], haskellTypeToDatatype typ)]

>                 argDecl (n, _, SigList _)                       = error "list type in signature not yet supported (DB:9)"
	      
>             localVars = [(["t"], PointerOf (StructOf (pstr prefixes ++ cName) []))]
>             resValue  = TLFA (pnew prefixes ++ orgSigName) [tlvar (Direct (psigid prefixes ++ cName)), tlvar tmpVar]
>             tmpVar    = Direct "t"
>             body = [TLAlloc MTTemp (toVA tmpVar) (ExpNum 1) (StructOf (pstr prefixes ++ cName) [])] ++
>                    concatMap assArgs fargs
>               where 
>                 assArgs (n, "f", SigId typ) | orgSigName == typ = 
>                                                             let
>                                                             sn = show n
>                                                             indexAssigns = case trackMode of
>                                                               MST -> assign ("i"++show n) ++  assign ("j"++show n)
>                                                               MTT -> assign ("i1"++show n) ++ assign ("j1"++show n) ++ 
>                                                                      assign ("i2"++show n) ++ assign ("j2"++show n)
>                                                             in 
>                                                             [TLAssign (toVA ((Pointer tmpVar):.(Direct ("isStructure" ++ show n)))) (tlnumber 0)] ++
>                                                             assign ("f"++sn) ++ indexAssigns
>                 assArgs (n,"s",SigId typ) | orgSigName == typ = [TLAssign (toVA ((Pointer tmpVar):.(Direct ("isStructure" ++ show n)))) (tlnumber 1)] ++
>                                                                 assign ("structure" ++ show n)

>                 assArgs (n,_ ,SigId typ) | getOptBT opts == BTSubOptCut = []
>                                          | otherwise                    = assign ("a"++show n)

>                 assArgs (n,_, SigList _)                  = error "list type in signature not yet supported (DB:10)"
>                 assign v = [TLAssign (toVA ((Pointer tmpVar):.(Direct v))) (tlvar (Direct v))]


> compileSignature :: TMode -> String -> String -> CompileOptions -> [IL2Type] -> [AlgDefs] -> [AlgDefs] -> [Signature] -> [TL]
> compileSignature trackMode enumAlgName axiom opts il2dt algs ppAlg []    = []
> compileSignature trackMode enumAlgName axiom opts il2dt algs ppAlg [sig] = compileSigs trackMode enumAlgName axiom opts il2dt algs ppAlg updatedSig
>   where
>     -- Hier wird die Signatur um den _NTID fuer einzeln auftretende Nichtterminale erweitert:
>     updatedSig = case sig of
>                    (signame, ops) -> (signame, [(pNTID prefixes, [SigId signame])] ++ ops)

----------------------------------------------------------------------------------------------------
For Backtracing:
-------------------

> compileBuildSig trackMode enumAlgName axiom opts il2dt algs (sigName, ops) = 
>   [TLFD (longComment "structure builder") resType
>         (pbuild prefixes ++ pstr prefixes ++ sigName)
>         [(["l"],resType)]
>         [(["c"],sigStructDT sigName),(["cl"], resType)]
>         code
>         (tlvar (Direct "l"))]
>   where
>     resType = btWorkDataType opts sigName il2dt algs axiom  
>     code | getOptBT opts == BTSingle = 
>            [TLAssign (toVA (Direct "c")) (tlvar (Direct "l")),
>             TLIf (ExpIOp (ExpTLVar (Direct "c")) "/=" ExpNil) (ppSig ops) []]
>          | isBTSubOpt opts =
>            [TLIf (ExpIOp (ExpTLVar (Direct "l")) "/=" ExpNil)
>              ([TLAssign (toVA (Direct "cl")) (tlvar (Direct "l"))] ++
>               [TLAssign (toVA (Direct "c")) (tlvar sigItem)] ++
>               [TLIf (ExpIOp (ExpTLVar ((Pointer (Direct "c")):.(Direct "fcalled"))) "==" (ExpNum 0))
>                  -- then
>                  ([TLAssign (toVA ((Pointer (Direct "c")):.(Direct "fcalled"))) (tlnumber 1)] ++ 
>                   ppSig ops)
>                  -- else
>                  []])
>               --else
>               []]
>          | otherwise = 
>            [TLAssign (toVA (Direct "cl")) (tlvar (Direct "l"))] ++
>            [TLWhileNN (toVA (Direct "cl")) (
>               [TLAssign (toVA (Direct "c")) (tlvar sigItem)] ++ 
>               ppSig ops ++
>               [listStep (toVA (Direct "cl"))])]
>            where
>              sigItem | elem (getOptBT opts) [BTCompleteList]
>                                                        = ((((Pointer (Direct "cl")):.(Direct "item"))) :. (Direct enumAlgName))
>                      | otherwise                       = (Pointer (Direct "cl")):.(Direct "item")

>     ppSig [] = []
>     ppSig ((cName,args):ops) = [TLIfs cond thenExp elseExp]
>       where
>         cond = ExpIOp (ExpTLVar ((Pointer (Direct "c")):.(Direct "utype"))) "==" (ExpTLVar (Direct defname))
>         defname = psigid prefixes ++ cName
>         elseExp = ppSig ops
>         thenExp = concatMap pa (zip [1..] (fla args))
>           where
>             fla [] = []
>             fla ((SigTupel args):as) = fla args ++ fla as
>             fla ((SigId args):as)    = args : fla as
>             
>             pa (n, typ)   | typ == sigName = let
>                                              diffArg = chooseBT opts [([BTSubOpt, BTSubOptCut], [entryRefT cName "diff" n]), ([],[])]
>                                              indexVars = case trackMode of 
>                                                MST -> [entryRefT cName "i"  n, entryRefT cName "j" n] ++ diffArg
>                                                MTT -> [entryRefT cName "i1" n, entryRefT cName "j1" n, 
>                                                        entryRefT cName "i2" n, entryRefT cName "j2" n] ++ diffArg
>                                              optionalAssign = chooseBT opts 
>                                                                   [([BTSubOpt],[TLAssign (entryRefN cName "pp_init_a" n) (entryRefT cName "a" n)]),
>                                                                    ([],[])]
>                                              assign var exp | getOptBT opts == BTSubOptCut = [exp]
>                                                             | otherwise                    = [TLAssign var exp]
>                                              in
>                                              [TLIf (ExpTLVar (fst $ entryRefN cName "isStructure" n)) 
>                                                    -- then
>                                                    ((assign (entryRefN cName "a" n) 
>                                                             (TLFA (pbuild prefixes ++ pstr prefixes ++ sigName) [entryRefT cName "structure" n])) ++
>                                                      optionalAssign)
>                                                    -- else
>                                                    ((assign (entryRefN cName "a" n) 
>                                                             (TLFPA (entryRefN cName "f" n) indexVars)) ++ 
>                                                      optionalAssign)]
>                           | otherwise = [] 

>             entryRefT cName field n = TLVar $ entryRefN cName field n


----------------------------------------------------------------------------------------------------
Strukturen freigeben

> compileFreeSig enumAlgName axiom opts il2dt algs (sigName, ops) -- | True                  = []  -- solange noch nicht fertig, erstmal alles raus
>                                                                 -- | not $ freeNeeded opts = []
>                                                                 = 
>   [TLFD (longComment "free structures") TLVoid
>         fctname
>         [(["rmAll"], TLChar), (["l"],resType)]
>         [(["itr","nitr"], resType), (["c"],sigStructDT sigName)]
>         code
>         (tlvar (Direct "_"))]
>   where
>     fctname = pfree prefixes ++ pstr prefixes ++ sigName
>     resType = btWorkDataType opts sigName il2dt algs axiom
>     code = [TLAssign (toVA (Direct "itr")) (TLVar (toVA (Direct "l"))),
>             TLWhileNN (toVA (Direct "itr")) 
>                [TLAssign (toVA (Direct "c")) (TLVar (toVA ((Pointer (Direct "itr")) :. (Direct "item")))),
>                 TLIf (ExpIOp (ExpVar "c") "/=" ExpNil)
>                   -- then
>                   (cgFree ops)
>                   -- else
>                   [],
>                 TLAssign (toVA (Direct "nitr")) (TLVar (listNext (toVA (Direct "itr")))),
>                 TLFA "free" [tlvar (Direct "itr")],
>                 TLIf (ExpVar "rmAll")
>                   -- then
>                   [TLAssign (toVA (Direct "itr")) (TLVar (toVA (Direct "nitr")))]
>                   -- else
>                   [TLAssign (toVA (Direct "itr")) TLNil]]]

>     cgFree [] = []
>     cgFree ((cName,args):ops) = [TLIfs cond thenExp elseExp]
>       where
>         cond = ExpIOp (ExpTLVar ((Pointer (Direct "c")):.(Direct "utype"))) "==" (ExpTLVar (Direct defname))
>         defname = psigid prefixes ++ cName
>         elseExp = cgFree ops

>         fla [] = []
>         fla ((SigTupel args):as) = fla args ++ fla as
>         fla ((SigId args):as)    = args : fla as

>         recCount = length $ concatMap count (zip [1..] (fla args))
>           where
>             count (_, typ) | typ == sigName = [1]
>                            | otherwise      = [] 
>         thenExp = concatMap pa (zip [1..] (fla args)) ++ 
>                    [TLFA "free" [tlvar ((Pointer (Direct "c")):.(Direct "entry"))]] ++
>                    [TLFA "free" [tlvar (Direct "c")]]
>           where
>             pa (n, typ)   | typ == sigName && recCount <= 1 = 
>                               [TLIf (ExpVar "rmAll")
>                                  -- then
>                                  [TLFA fctname [tlvar (Direct "rmAll"), TLVar $ entryRefN cName "pp_init_a" n]]
>                                  -- else
>                                  [TLFA fctname [tlvar (Direct "rmAll"), TLVar $ entryRefN cName "a" n]]]
>                           | typ == sigName && recCount > 1=
>                                  [TLFA fctname [tlvar (Direct "1"), TLVar $ entryRefN cName "pp_init_a" n]]
>                           | otherwise = [] 

-- >                 TLIf (ExpTLVar $ fst (entryRefN cName "isStructure" n))
-- >                                                    (cgFree (entryRefN cName "structure" n))
-- >                                                    []
-- >                                              ] 
-- >                           | otherwise = [] 

-- >             cgFree :: VarAccess -> [TL]
-- >             cgFree v | isBTSingle opts =
-- >                        [TLAssign (toVA (Direct "l")) (TLVar v),
-- >                         TLFA fctname [tlvar (((Direct "l")))]]
-- >                      | otherwise =
-- >                        [TLAssign (toVA (Direct "l")) (TLVar v),
-- >                         TLAssign (toVA (Direct "l2")) (tlvar (Direct "l")),
-- >                         TLWhileNN (toVA (Direct "l"))
-- >                           [TLFA fctname [tlvar (((Pointer (Direct "l")):.(Direct "item")))],
-- >                            (listStep (toVA (Direct "l"))),
-- >                            TLFA "free" [tlvar (Direct "l2")],
-- >                            TLAssign (toVA (Direct "l2")) (tlvar (Direct "l"))]]


----------------------------------------------------------------------------------------------------
copy structure

> compileCopySig enumAlgName axiom opts il2dt algs (sigName, ops) -- | True                  = []  -- solange noch nicht fertig, erstmal alles raus
>                                                                 -- | not $ freeNeeded opts = []
>                                                                 = 
>   [TLFD (longComment "copy structures") resType
>         fctname
>         [(["l"],resType)]
>         [(["itr","nstr","last","first","setnext"], resType), 
>          (["entr"], PointerOf TLVoid), 
>          (["id"], TLInt)]
>         code
>         (tlvar (Direct "first"))]
>   where
>     fctname = pcopy prefixes ++ pstr prefixes ++ sigName
>     resType = btWorkDataType opts sigName il2dt algs axiom
>     resType' = case resType of
>                  (PointerOf dt) -> dt
>                  otherwise      -> error $ "compileCopySig.restype': undefined pattern."
>     sigStructDT' = case sigStructDT sigName of
>                  (PointerOf dt) -> dt
>                  otherwise      -> error $ "compileCopySig.sigStructDT': undefined pattern."
>     code = [TLAssign (toVA (Direct "copy_depth")) (TLExp (ExpIOp (ExpVar ("copy_depth")) "+" (ExpNum 1))),
>             TLAssign (toVA (Direct "itr")) (TLVar (toVA (Direct "l"))),
>             TLAssign (toVA (Direct "first")) TLNil,
>             TLAssign (toVA (Direct "setnext")) TLNil,
>             TLWhileNN (toVA (Direct "itr")) 
>              ([TLAlloc MTDynamic (toVA (Direct "nstr")) (ExpNum 1) resType',
>                TLAssign (listNext (toVA (Direct "nstr"))) TLNil,
>                TLAssign (toVA ((Pointer (Direct "nstr")) :. (Direct "last"))) (TLVar (toVA (Direct "nstr"))),
>                TLIf (ExpVar "setnext") [TLAssign (listNext (toVA (Direct "setnext"))) (TLVar (toVA (Direct "nstr")))] [],
>                TLAssign (toVA (Direct "setnext")) (TLVar (toVA (Direct "nstr"))),
>                TLIf (ExpIOp (ExpVar "first") "==" ExpNil) [TLAssign (toVA (Direct "first")) (TLVar (toVA (Direct "nstr")))] [],
>                TLAlloc MTDynamic (toVA ((Pointer (Direct "nstr")) :. (Direct "item"))) (ExpNum 1) (sigStructDT'),
>                TLAssign (toVA (Pointer ((Pointer (Direct "nstr")) :. (Direct "item"))))
>                   (TLVar (toVA (Pointer ((Pointer (Direct "itr")) :. (Direct "item"))))),
>                TLAssign (toVA (Direct "id"))
>                   (TLVar (toVA ((Pointer ((Pointer (Direct "itr")) :. (Direct "item")) :. (Direct "utype"))))),
>                TLAssign (toVA (Direct "entr"))
>                   (TLVar (toVA ((Pointer ((Pointer (Direct "itr")) :. (Direct "item")) :. (Direct "entry")))))] ++
>                 ------------------------------------------------------------
>                 cgCopy ops++
>                 [TLAssign (toVA (Direct "itr")) (TLVar (listNext (toVA (Direct "itr"))))]
>                 ),
>                 ------------------------------------------------------------
>            TLIf (ExpVar "first")
>              -- then
>              [TLAssign (toVA (Direct "last")) (TLVar (toVA (Direct "nstr"))),
>               TLAssign (toVA (Direct "itr")) (TLVar (toVA (Direct "first"))),
>               TLWhileNN (toVA (Direct "itr")) 
>                 [TLAssign (toVA ((Pointer (Direct "itr")) :. (Direct "last"))) (TLVar (toVA (Direct "last"))),
>                  TLAssign (toVA (Direct "itr")) (TLVar (toVA ((Pointer (Direct "itr")) :. (Direct "next"))))]]
>              -- else 
>              [],
>            TLAssign (toVA (Direct "copy_depth")) (TLExp (ExpIOp (ExpVar ("copy_depth")) "-" (ExpNum 1)))]


>     cgCopy [] = []
>     cgCopy ((cName,args):ops) = [TLIfs cond thenExp elseExp]
>       where
>         cond = ExpIOp (ExpTLVar (Direct "id")) "==" (ExpTLVar (Direct defname))
>         defname = psigid prefixes ++ cName
>         elseExp = cgCopy ops

>         fla [] = []
>         fla ((SigTupel args):as) = fla args ++ fla as
>         fla ((SigId args):as)    = args : fla as

>         recCount = length $ concatMap count (zip [1..] (fla args))
>           where
>             count (_, typ) | typ == sigName = [1]
>                            | otherwise      = [] 
>         thenExp = [TLAlloc MTDynamic (toVA ((Pointer ((Pointer (Direct "nstr")) :. (Direct "item")) :. (Direct "entry"))))
>                           (ExpNum 1) (StructOf (pstr prefixes ++ cName) [])] ++ 
>                    [TLAssign 
>                     (toVA (
>                       Pointer (Cast (PointerOf (StructOf (pstr prefixes ++ cName) []))
>                               ((Pointer (Pointer (Direct "nstr"):.(Direct "item"))) :.(Direct "entry")))))
>                     (TLVar (toVA ((Pointer 
>                        (Cast (PointerOf (StructOf (pstr prefixes ++ cName) [])) (Direct "entr"))))))] ++
>                    concatMap pa (zip [1..] (fla args))
>           where
>             pa (n, typ)   | typ == sigName =
>                      [TLIf 
>                          -- condition
>                          (ExpTLVar 
>                             ((Pointer (Cast (PointerOf (StructOf (pstr prefixes ++ cName) []))
>                               ((Pointer (Pointer (Direct "nstr"):.(Direct "item"))) :.(Direct "entry")))) :. 
>                                (Direct ("isStructure" ++ show n))))
>                          -- then
>                          [TLAssign (toVA (
>                               ((Pointer (Cast (PointerOf (StructOf (pstr prefixes ++ cName) []))
>                               ((Pointer (Pointer (Direct "nstr"):.(Direct "item"))) :.(Direct "entry")))) :. 
>                                (Direct ("structure" ++ show n)))))
>                               (TLFA fctname [TLVar (toVA (
>                               ((Pointer (Cast (PointerOf (StructOf (pstr prefixes ++ cName) [])) (Direct "entr"))) :. 
>                                (Direct ("structure" ++ show n)))))])] 
>                          -- else
>                          [] 

>                          ]


-- >                               [TLIf (ExpVar "rmAll")
-- >                                  -- then
-- >                                  [TLFA fctname [tlvar (Direct "rmAll"), TLVar $ entryRefN cName "pp_init_a" n]]
-- >                                  -- else
-- >                                  [TLFA fctname [tlvar (Direct "rmAll"), TLVar $ entryRefN cName "a" n]]]
-- >                           | typ == sigName && recCount > 1=
-- >                                  [TLFA fctname [tlvar (Direct "1"), TLVar $ entryRefN cName "pp_init_a" n]]

>                           | otherwise = [] 

----------------------------------------------------------------------------------------------------
SubOpt-Diff update

> compileUpdateSig trackMode enumAlgName axiom opts il2dt algs (sigName, ops) = 
>   [TLFD (longComment "update subopt difference values") TLVoid
>         thisFctName 
>         [(["c"],sigStructDT sigName), (["diff"], soDiffDatatype)]
>         []
>         code
>         (tlvar (Direct "_"))]
>   where
>     thisFctName = pupdate prefixes ++ pstr prefixes ++ sigName
>     resType = btWorkDataType opts sigName il2dt algs axiom  
>     code = [TLIf (ExpIOp (ExpTLVar (Direct "c")) "/=" ExpNil) (ppSig ops) []]

>     ppSig [] = []
>     ppSig ((cName,args):ops) = [TLIfs cond thenExp elseExp]
>       where
>         cond = ExpIOp (ExpTLVar ((Pointer (Direct "c")):.(Direct "utype"))) "==" (ExpTLVar (Direct defname))
>         defname = psigid prefixes ++ cName
>         elseExp = ppSig ops
>         thenExp = concatMap pa (zip [1..] (fla args)) ++ diffAssign
>           where
>             diffAssign | isTerminal sigName args && getOptBT opts == BTSubOpt = 
>                            [TLAssign (entryRefFVA cName "diff") (TLVar $ toVA $ Direct "diff")]
>                        | otherwise = []
>             fla [] = []
>             fla ((SigTupel args):as) = fla args ++ fla as
>             fla ((SigId args):as)    = args : fla as
>             
>             pa (n, typ)   | typ == sigName = [TLIf (ExpTLVar (fst $ entryRefN cName "isStructure" n)) 
>                                                    -- then
>                                                    [TLFA thisFctName [TLVar $ item $ entryRefN cName "structure" n, tlvar $ Direct "diff"]]
>                                                    -- else
>                                                    [TLAssign (entryRefFVA cName $ "diff" ++ show n) (TLVar $ toVA $ Direct "diff")]]
>                           | otherwise = [] 
>              where item (v, dt) = ((Pointer v) :. (Direct "item"), dt)

