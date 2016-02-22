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



Start with:

hugs -P/vol/pi/scratch/haskell/lib/hugs/libraries Core.lhs 

TODO
-----
- Parser
- Ermittlung rekursiver funktionen:
- sonderbehandlung fuer tabulate
- aufbau einer liste aller tabellierten funktionen
- diese wird als grundlage fuer die verarbeitung genommen
- rekursive, nichttabellierte definitionen werden nicht reduziert.
- erreichbarkeitsanalyse vom axiom aus. 
- Lc nur noch als Lc [Core] 

- reihenfolge
  1. parsen
  2. sonderbehandlung fuer tabulate:
    - definitionen, die als erste Applikation tabulate enthalten, aufsammeln,
    - diese werden nicht reduziert, genauso wenig wie die
  3. ermittlung rekursiver definitionen
    - dieses muesste nach jedem reduce-schritt wiederholt werden, um mit verschachtelten rekursionen umgehen zu koennen
    - rekursive definitionen werden dann nicht weiter reduziert
  4. Erreichbarkeitsanalyse von Axiom aus, alle Funktionen, die nicht vom axiom aus erreichbar sind, werden herausgeschmissen.
    - jetzt sollten nur noch produktionen uebrig bleiben; algebrafunktionen o.ae. wurden vorher geinlined.
  5. weitere LC-Reduktionen
  6. Yield size analyse mit fixpunkt-iteration

Details:
- fix liefert liste von iterationen; spezieller tracemode kann dann zus. diese liste ausgeben
- textuelle erlaeuterungen beim trace: welche variablen koennen ersetzt werden, warum wurden die schleifengrenzen veraendert, etc.

------------------------------------------------------------
V relativ fruehe optimierung: relOps normalisieren, so dass nur noch
  <= und >= enthalten sind. Dies macht die Verarbeitung etwas
  uebersichtlicher
- relOps auf ["<=", ">="] beschraenken, momemtan sicherhaltshalber noch drin
V Benchmark: GHC-Original vs. GHC-optimiert, z.B. fuer wuchty-bpmax
V () beruecksichtigen
V schoenerer Prettyprint fuer (,)
V indentation beschraenken
- Latex-rekurrenzen ausgehend von Core
- Yield-Size-Analyse optimieren
- refac: reduce : argumente liften
- innere mapCore-Aufrufe in den LCs moeglicherweise durch direkte
  aufrufe der worker-funktionen ersetzen, ansonsten koennte es
  Probleme geben falls schon in der ersten Iteration ein relevantes
  Konstrukt vorliegt
- bei depana auch schleifenvariablen beruecksichtigen
- data.FiniteMap benutzen
- yield-size-laengenabfragen wegoptimieren, indem die entsprechenden
  tabellenelemente entsprechend initialisiert werden; dies erfordert
  zwar geringfuegig mehr speicher, allerdings entfallen dadurch die
  abfragen bei jeden Tabellenzugriff.
------------------------------------------------------------
Fehler
-------
- bei Wuchty2.lhs: Wenn 
    sr  <<< base -~~ weak  ~~- base)
  benutzt wird, funktioniert die automatische Index-Analyse nicht mehr. Bei
    sr  <<< base ~~~ weak  ~~~ base)
  geht es.

Moeglichkeiten zur Atomar/Listen-Optimierung
---------------------------------------------
1) wie gehabt, auf TL-Ebene; 
   Nachteil: viele Fallunterscheidungen 

2) auf Core-Ebenen, Z.B:

- umschreiben von listenbeschreibungen ohne generatoren:
[ e ]                 ->  [ e ]
[ e | c ]             ->  [ if c then e else NULL_VAL ]          -- NULL_VAL abh. von Choice
[ e | k <- [i..j]]    ->  [ for i j CHC_F NULL_VAL (\k -> e) ]

[ e | v <- nt (i,j)]  ->  [ e[v/nt(i,j)] ]                       -- falls nt  atomar

[ e | v <- exp ]      ->  [ e[e/exp] ]                           -- false exp atomar
[ e | v <- exp ]      ->  [ e | v <- exp ]                       -- false exp nicht atomar

for i j f res e | i > j     = res
                | otherwise = for (i+1) j f (f (e i) res) e

- umschreiben von auswahlfunktionen:
maximum ( [e1] ++ [e2] ++ [e3] )
==> max (e1, max (e2, e3))   -- falls e1-3 atomar 

==> Nachteil: in dieser Variante neue Core-Elemente (if, for)
    (if wird sehr wahrscheinlich sowieso dazukommen)
------------------------------------------------------------
Anderer Ansatz:
(maximum [ v1 | v1 <- (xIns ((i, (j-1)))) ])
==>
let x = NULL_ELEM in
  [ let x = max x v1 in x | v1 <- xIns (..)]

--------
maximum [ v1 | k <- [i .. j], v1 <- (xIns (i, k)) ]
==>
let x = NULL_ELEM in
  [ let x = max x v1 in x | k <- [i .. j], v1 <- (xIns (i,k)) ]
------------------------------------------------------------

Backtrace-Idee
---------------
-- bpmax
d x s   = s + copen + extend
-- enum
d x s   = D x s

alle verwendungen von nts und verschachtelten LCs:
==> nt --> loc ~~~ nt ~~~ loc

alle argumente von algebrafunktionen mit Typ=resultType:
-- bpmax
d x s = s + copen + extend
-- enum
d x i s j = D x (makeFCall s i j)

alle Applications von algebrafunktionen ersetzen:
d -> d_bt
 where
 d_bt x i s j = (d_bpmax x s, d_enum x i s j)
------------------------------------------------------------
Idee zum Listen-Handling
------------------------
- Listenimplementierung als ADT
- Repraesentation:
struct str_item
   TAG int;    // item entweder konkretes Element oder wiederum Liste
   void *item;
==> haette den Vorteil, das weniger Listen kopiert werden muessten, 
    z.B bei allein auftretenden Nichtterminalen

    Hier muss bisher die Ergebnisliste des Nichtterminals kopiert
    werden, da ein nachfolgendes Append ansonsten diese Liste
    erweitern wuerde.  Mit dem neuen Ansatz, wuerde mittels
    mkList(nt(i,h)) ein einzelnes Listenelement mit einem Verweis auf
    die nt-Liste erzeugt werden.  Der aufwendige Part hier ist die
    Traversierung der ganzen Struktur. Wenn als ADT implementiert,
    sollte dies allerdings zu realisieren sein.
==> moeglicherweise loest dieses auch das Speicher-Problem bei den
    RNAShapes 
------------------------------------------------------------

Wuchty.lhs:
- Problem bei aeusseren "with basepairing"
- doppelte Abfragen herausfiltern: 
   z.B. (i_i<j_j),(i_i<(j_j-1)) ==> (i_i<(j_j-1))
- yieldsize-calc. funkt. nicht: z.B. wird ((j_j-i_i)>4) nicht als untere ys. 5 erkannt
  -> initialisierung besser als (Inf, 0)


> module Main where
> import System
> import List
> import CoreTools
> import CoreSyntax
> import CoreParser
> --import Typecheck.PPrint
> import CoreTL
> import CoreCodegen

> test = compile 1 "coretest/AffineGlob2.lhs"

The Main Compiler Phases:
----------------------------

> compile :: Integer -> [Char] -> IO ()
> compile vl inputFile = do
>    --inp <- readFile inputFile
>    --inp2 <- return $  unlines (takeWhile ((/=) "#lib") (lines inp))
>    --inp  <- return $  unlit inputFile inp
>    --inp2 <- return $  unlit inputFile inp2
>    --(_,types) <- return $  typeCheck inp True
>    --(defs,_)  <- return $  typeCheck inp2 False 
>    defs <- parseWithImport inputFile
>    --print "types:"
>    --putStrLn $ unlines $ map pretty types
>    --print "defs:"
>    --print defs

>    optOut vl $"Input:\n" 
>    optOut vl $ ppProg defs
>    defs <- return $ lambdaLift defs
>    optOut vl $"Lambda lifted:\n" 
>    optOut vl $ ppProg defs
>    optDebOut vl $ defs
>    --defs <- compStep vl "Filter reachable definitions"                             filterReachable                   ppProg       defs
>    tab  <- compStep vl "Find tabulated definitions"                               findTabulated                     ppStrings    defs

>    defs <- compStep vl "Filter reachable definitions"                             filterReachable                   ppProg       defs

>    -- defsT <- compStep vl "Add dummy indices to tabulated productions"               (applySubscripts MRDeriveArgs tab []) ppProg       defs
>    -- defsT <- compStep vl "Reduce applications"                                      (mps (fix (reduce MRDeriveArgs [1] tab defsT))) ppProg       defsT
>    -- tabArgs <- compStep vl "Collect arguments for tabulated functions"              (collectArgs tab)                (ppAssoc ppCore)   defsT

>    let tabArgs = []

>    defs <- compStep vl "Add indices to tabulated productions"                     (applySubscripts MRReduce tab tabArgs)     ppProg       defs
>    defs <- compStep vl "Reduce applications"                                      (mps (fix (reduce MRReduce [1] tab defs))) ppProg       defs
>    defs <- compStep vl "Filter reachable definitions"                             filterReachable                   ppProg       defs
>    defs <- compStep vl "Eliminate and-Operators"                                  (mps removeAnds)                  ppProg       defs
>    defs <- compStep vl "Normalize RelOps"                                         (mps normalizeRelOps)             ppProg       defs
>    defs <- compStep vl "Flat list comprehensions"                                 (mps flatLC)                      ppProg       defs
>    defs <- compStep vl "Reduce list comprehensions"                               (mps (fix reduceLC))              ppProg       defs
>    defs <- compStep vl "Reduce applications"                                      (mps (fix (reduce MRReduce [1] tab defs))) ppProg       defs
>    defs <- compStep vl "Filter unnecessary assignments from list comprehensions"  (mps filterLC)                    ppProg       defs
>    defs <- compStep vl "Simplify list comprehensions"                             (mps (fix simplifyLC))            ppProg       defs
>    defs <- compStep vl "Remove unnecessary filters from list comprehensions"      (mps removeEqLC)                  ppProg       defs
>    defs <- compStep vl "Filter unnecessary assignments from list comprehensions"  (mps filterLC)                    ppProg       defs
>    defs <- compStep vl "Filter reachable definitions"                             filterReachable                   ppProg       defs
>    defs <- compStep vl "Simplify arithmetic expressions"                          (mps simplifyExps)                ppProg       defs
>    defs <- compStep vl "Optimize loops"                                           (mps optLoops)                    ppProg       defs
>    defs <- compStep vl "Solve loop dependencies"                                  (mps depLoops)                    ppProg       defs
>    defs <- compStep vl "Remove redundant filters"                                 (mps rmRedundantFilters)          ppProg       defs

>    ysizes <- return $ initYSize tab
>    defs   <- compStep vl "Attach yield size filters"                              (mps (attachYSize tab ysizes))    ppProg       defs
>    _      <- compStep vl "Derive yield sizes"                                     deriveYSize                       (ppAssoc (mapsep ", " ppCore)) defs

>    rts  <- compStep vl "Derive result types"                                      deriveRes                         (ppAssoc ppResType) defs
>    defs <- compStep vl "High level optimizations"                                 (mps (fix (filterLC . (hlOptimizations rts))))
>                                                                                                                     ppProg       defs
>    defs <- compStep vl "Rename variables"                                         (mps renameVars)                  ppProg       defs
>    deps <- compStep vl "Derive dependencies"                                      (deriveDeps tab)                  ppDepTab     defs
>    sdeps <- compStep vl "Sort dependencies"                                       (sortDeps tab)                    ppStrings    deps
>    --ysizes <- compStep vl "Calculate yield sizes"                                  calcYSizes                      (ppAssoc ppYSize)   defs
>    if vl == target then putStrLn (ppProg defs) else return ()
>    putStrLn $ title "Haskell output"
>    putStrLn $ ppProgHaskell tab tabArgs defs
>    tl <- compStep vl "Generate C-code"                                              (cgProg tab)                      ppTLs        defs
>    return()


> fixM cur wrk = fixM' 5 cur wrk

> fixM' n currentResult wrk = do
>                            (newResult,defs) <- wrk currentResult
>                            if newResult  == currentResult || n == 0 then return (newResult, defs)
>                                                                     else fixM' (n-1) newResult wrk 



Apply function on all definitions of a program
------------------------------------------------

> mps f = map (\(n,a,exp) -> (n, a, f exp)) 

Find tabulated functions
--------------------------
TODO: recht unschoener Hack!

> findTabulated :: Prog -> [String]
> findTabulated ps = map fst3 $ filter isTab ps
>   where
>     isTab (_,_,exp) = let (redex, args) = findRedex exp []
>                       in case redex of
>                            (Var fct) -> if isSuffixOf "tabulated" fct then True else False
>                            otherwise -> False
>     isTab _                                                                        = False

Apply tabulated functions to subscripts (i,j)
---------------------------------------------
TODO: hier temp. wieder durch (i,j) ersetzt

> applySubscripts mode tab tabArgs ps = map apply ps
>   where
>     apply (n, args, exp) | elem n tab && mode == MRDeriveArgs 
>                                       = (n, args,  exp) -- Ap exp (Ap (Ap (Var "(,)") (Var "i")) (Var "j")))  -- (Ap exp (Var "argsVars"))) 

>     apply (n, args, exp) | elem n tab = (n, args ++ [ArgTuple [ArgVar "i_i", ArgVar "j_j"]], 
>                                                   --   (Ap exp tArgs))
>                                                   Ap exp (Ap (Ap (Var "(,)") (Var "i_i")) (Var "j_j")))
>       where tArgs = case getVal tabArgs n of
>                        Just n  -> n
>                        Nothing -> error $ "cannot derive argument type of function " ++ n
                

>     apply x                           = x


Attach yield size filters to all applications of tabulated functions 
--------------------------------------------------------------------

> makeBnd (Just l) Nothing      = appl ">=" [appl "-" [Var "j[]", Var "i[]"], Num l]
> makeBnd Nothing (Just u)      = appl "<=" [appl "-" [Var "j[]", Var "i[]"], Num u]
> makeBnd l@(Just _) u@(Just _) = appl "&&" [makeBnd l Nothing, makeBnd Nothing u]

> initYSize = flip zip (repeat (makeBnd (Just 0) Nothing))

> attachYSize :: [String] -> [(String, Core)] -> Core -> Core
> attachYSize tab ysizes = snd . attach 1
>   where
>     attach :: Int -> Core -> (Int, Core)
>     attach cll (Lc (c:cs)) = (cll'', Lc (c':cs'))
>       where
>         (cll', c')   = attach  cll  c
>         (cll'', cs') = attach' cll' cs
>         attach' cll [] = (cll, [])
>         attach' cll (exp@(ArgVar _ :<- Ap (Var nt) ss):exps)  | elem nt tab = let (cll', exps') = attach' cll exps
>                                                                               in  (cll', [bindArgs (justVal ysizes nt) ss, exp] ++ exps')
>         attach' cll (exp:exps)                                              = let (cll', exps') = attach' cll exps
>                                                                               in  (cll', exp: exps')
>     attach cll exp@(Ap (Var nt) ss)                           | elem nt tab = let v = makeVarName nt (show cll)
>                                                                               in (cll, Lc ((Var v) :
>                                                                                          [bindArgs (justVal ysizes nt) ss, (ArgVar v) :<- exp]))
>     attach cll x = collectCore attach cll x

>     bindArgs exp (Ap (Ap (Constr "(,)") i) j) = varBind [("i[]", i), ("j[]", j)] exp
>     bindArgs exp ss                           = pattErr "attachYSize.bindArgs" (exp, ss)

Derive yield size filters for all applications of tabulated functions 
-----------------------------------------------------------------------

> deriveYSize :: Prog -> [(String, [Core])]
> deriveYSize = map (\(n, _, c) -> (n, deriveYS c))

> deriveYS = map simpExp . nub . fst . derive []
>  where
>    derive cll exp@(Lc (c:cs)) = (makeOr' (cll ++ makeAnd' (c' ++ concatMap derive' cs)), exp)
>      where
>        (c', _) = derive [] c
>        derive'     (Ap (Ap (Var "&&") e1) e2) = derive' e1 ++ derive' e2
>        derive' exp@(Ap (Ap (Var op)   e1) e2) | elem op ysRelOps && (containsVar exp "i[]" || containsVar exp "j[]") = [exp]
>        derive' x                              = []
>    derive cll x = collectCore derive cll x 


> ysRelOps = ["<",">","<=",">=","=="]

> makeAnd' [] = []
> makeAnd' xs = [makeAnd $ and' [] xs]
>   where
>     and' res []   = res
>     and' res (exp:exps) = and' (exp:delBy eq exp res) (delBy eq exp exps)
>     eq e1@(Ap (Ap (Var ">=") _) _) e2@(Ap (Ap (Var ">=") _) _) 
>       = case (eval e1, eval e2) of
>           (Just n1, Just n2) -> n1 <= n2
>           otherwise          -> False
>     eq e1@(Ap (Ap (Var "<=") _) _) e2@(Ap (Ap (Var "<=") _) _) 
>       = case (eval e1, eval e2) of
>           (Just n1, Just n2) -> n1 >= n2
>           otherwise          -> False
>     eq _ _ = False
>     eval = evalExp ["i[]", "j[]"]

> makeOr' [] = []
> makeOr' xs = or' [] xs -- [makeOr $ or' [] xs]
>   where
>     or' res []   = res
>     or' res (exp:exps) = or' (exp:delBy eq exp res) (delBy eq exp exps)
>     eq e1@(Ap (Ap (Var ">=") _) _) e2@(Ap (Ap (Var ">=") _) _) 
>       = case (eval e1, eval e2) of
>           (Just n1, Just n2) -> n1 <= n2
>           otherwise          -> False
>     eq e1@(Ap (Ap (Var "<=") _) _) e2@(Ap (Ap (Var "<=") _) _) 
>       = case (eval e1, eval e2) of
>           (Just n1, Just n2) -> n1 >= n2
>           otherwise          -> False
>     eq _ _ = False
>     eval = evalExp ["i[]", "j[]"]

> delBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
> delBy eq x []         = []
> delBy eq x (y:ys)     = if x `eq` y then delBy eq x ys else y : delBy eq x ys


Find recursive functions
--------------------------

> isRecursive :: String -> Core -> Bool
> isRecursive = flip containsVar

> findRecursive :: Prog -> [String]
> findRecursive ps = map fst3 $ filter isRec ps
>   where
>     isRec (name, _, exp) = isRecursive name exp

Reachability analysis
----------------------

> reachAna = reachAna' []
>  where
>    reachAna' :: [String] -> Prog -> String -> [String]
>    reachAna' st ps f | elem f st = []
>    reachAna' st ps f             = case getDef ps f of
>                                      Nothing        -> []
>                                      Just (_,_,exp) -> let vars = collectVars exp
>                                                        in nub $ f:concatMap (reachAna' (f:st) ps) vars

> filterReachable :: Prog -> Prog
> filterReachable defs = filter isReach defs
>    where
>      isReach (n, _, _) = elem n reach
>      reach = reachAna defs "main"

Reduce function applications:
---------------------------------

> data ModeReduce = MRReduce | MRDeriveArgs   deriving Eq

> reduce mode n tab defs exp  = apply n defs exp redex args
>   where
>   (redex, args) = findRedex exp []

>   apply n defs root redex args | isSpine redex = apply' n defs root redex args 
>     where 
>     isSpine (Var _)    = True
>     isSpine (Constr _) = True
>     isSpine _          = False
>     isConstr (Constr _) = True
>     isConstr _          = False

>     apply' n defs root v' args | isConstr v' = rootArgsRed
>                                | elem v tab  = rootArgsRed             -- do not reduce tabulated definitions:
>                                | otherwise = case getDef defs v of
>                                     Nothing -> rootArgsRed
>                                     Just _  -> case canBindAll of
>                                                        False -> rootArgsRed
>                                                        True  -> replaced 
>       where
>       (Var v) = v'
>       Just (_, defArgs, exp) = getDef defs v
>       reducedArgs = map (reduce mode n tab defs) args
>       bind = concatMap bindTuples $ zip defArgs reducedArgs
>         where 
>         bindTuples (arg, Var "argsVars") | mode == MRDeriveArgs = map makeArgVar $ flatTup arg
>            where
>              flatTup     (ArgVar v) = [v]
>              flatTup     (ArgTuple vs) = concatMap flatTup vs
>              makeArgVar  v = Just (v, Var $ makeVarName ("arg_" ++ v) "")

>         bindTuples (ArgVar v, exp)                  = [Just (v, exp)]
>         bindTuples (ArgTuple vs, e@(Ap (Ap _ _) _)) = bindTup (reverse vs,e)
>         bindTuples x = [Nothing]
>         bindTuples x = pattErr "reduce.bindTuples" x

>         bindTup ([v], Ap (Constr "(,)") arg) = bindTuples (v, arg)
>         bindTup (v:vs,Ap e1 arg)             = bindTuples (v, arg) ++ bindTup (vs,e1)
>         bindTup x  = [Nothing]
>         bindTup x  = pattErr "reduce.bindTup" x

>       canBindAll = all just bind
>         where just (Just _) = True
>               just Nothing  = False

>       applied = varBind (map just bind) exp
>         where just (Just v) = v
>               just x        = pattErr "reduce.just" x

>       -- three cases:
>       --   |args| == |defArgs| -> root can be replaced with reduced expression
>       --   |args| >  |defArgs| -> spine includes more arguments than redex; find root of redex and replace; this happens when n == 0
>       --   |args| <  |defArgs| -> spine includes not enough arguments for reduction; return root instead and reduce arguments first.

>       replaced = replace root (length args - length defArgs) applied
>       replace x          n _   | n < 0 = x
>       replace _          0 exp         = exp
>       replace (Ap c1 c2) n exp         = Ap (replace c1 (n-1) exp) c2
>       replace x          _ exp         = pattErr "apply.replace" (x, exp)

>       rootArgsRed = applc v' reducedArgs

>   apply n defs root exp@(Lc (c:cs)) _  = let (Lc (c':cs')) = enhName n exp
>                                              level       = (head n+1):tail n
>                                          in Lc ((reduce mode level tab defs c') : (map (\(c,d) -> reduce mode (c:level) tab defs d) (zip [1..] cs')))
>   apply n defs root (c1 :<- c2)     _  = c1 :<- (reduce mode n tab defs c2)
>   apply n defs root (c1 :.. c2)     _  = (reduce mode n tab defs c1) :.. (reduce mode n tab defs c2)
>   apply n defs root (Num e)         _  = Num e
>   apply n defs root (Constr c)      _  = Constr c
>   apply n defs root x               _  = mapCore (reduce mode n tab defs) x
>
>   apply _ _    _    e               _  = pattErr "apply" e

> -- add suffix to local generator names:
> enhName n (Lc (c:cs)) = Lc ((varBind bind c):(map (varBind bind) cs))
>   where
>     generators = getGenerators cs
>     newNames = map enh generators
>        where enh name | elem spcId name = Var name
>                       | otherwise       = Var $ makeVarName name (concatMap show (reverse n))   
>     bind = zip generators newNames
> enhName n x = mapCore (enhName n) x


Collect Arguments for tabulated functions
------------------------------------------

> collectArgs tab defs = rename $ filterTab $ nub $ concatMap (\(_,_,exp) -> fst $ collect [] exp) defs
>   where
>    -- TODO : hier temporaer wieder durch (i,j) ersetzt
>     collect cll e@(Ap (Var n) arg) | elem n tab = ((n, (Ap (Ap (Var "(,)") (Var "i")) (Var "j"))):cll, e)   -- ((n, arg):cll, e)
>     collect cll e = collectCore collect cll e 

>     filterTab res = map f tab
>       where f name = case getVal res name of
>                        Just n  -> (name, n)
>                        Nothing -> error $ "cannot derive argument type of function " ++ name
>     rename = map (\(n, e) -> (n, renameVars e)) 

>     renameVars = snd . rename (1,[]) 
>        where
>          rename cll@(n,nm) (Var v) = let v' = genName n in ((n+1,   (v, v'): nm), Var v')
>          rename cll@(n,nm) (Num _) = let v' = genName n in ((n+1,            nm), Var v')
>          rename cll x       = collectCore rename cll x
>          genName 1 = makeVarName "i" ""
>          genName 2 = makeVarName "j" ""
>          genName 3 = makeVarName "i2" ""
>          genName 4 = makeVarName "j2" ""
>          genName n = makeVarName "arg" $ show n


Eliminate AND-Operators:
-------------------------

> removeAnds (Lc (c:cs)) = Lc ((mapCore removeAnds c):(concatMap flatAnd (mapCores removeAnds cs)))
>   where
>     flatAnd (Ap (Ap (Var "&&") e1) e2) = flatAnd e1 ++ flatAnd e2
>     flatAnd exp                        = [exp]

> removeAnds x = mapCore removeAnds x

Normalize RelOps:
-------------------

> normalizeRelOps (Lc (c:cs)) = Lc ((mapCore normalizeRelOps c) : (map norm (mapCores normalizeRelOps cs)))
>   where
>     norm (Ap (Ap (Var "<") e1) e2) = Ap (Ap (Var "<=") e1) (appl "-" [e2, Num 1])
>     norm (Ap (Ap (Var ">") e1) e2) = Ap (Ap (Var ">=") e1) (appl "+" [e2, Num 1])
>     norm exp                       = exp

> normalizeRelOps x = mapCore normalizeRelOps x


Flat inner list comprehensions
--------------------------------

> flatLC (Lc (c:cs)) = Lc ((mapCore flatLC c):cs')
>   where
>     cs' = concatMap flat cs
>     flat (c :<- (Lc (c':cs))) = concatMap flat cs ++ [c :<- (Lc [c'])]
>     flat (c :<- x)            = [c :<- mapCore flatLC x]
>     flat x                    = [mapCore flatLC x]
> flatLC x         = mapCore flatLC x

Elminate local list comprehensions 
-------------------------------------
Elminate local list comprehensions by inlining the corresponding lc-heads

> reduceLC (Lc (c:cs)) = varBindLC localDefs (Lc ((mapCore reduceLC c):cs'))
>   where
>     cs' = mapCores reduceLC cs
>     localDefs = concatMap get cs'
>       where get (ArgVar v :<- Lc [exp]) = [(v, exp)]
>             get _                     = []
> reduceLC x         = mapCore reduceLC x

Remove unnecessary assignments from lcs:
-----------------------------------------

> filterLC (Lc (c:cs)) = Lc ((mapCore filterLC c) : (filter isUsed (mapCores filterLC cs)))
>  where
>   -- collect used variables
>   used = concatMap collectVars (c:cs)
>   -- assignments not used are removed from lc:
>   isUsed (ArgVar v :<- _) = elem v used
>   isUsed _             = True

> filterLC x = mapCore filterLC x


Simplify lcs:
----------------

> simplifyLC (Lc (c:cs)) = result
>   where
>     -- search for equations:
>     equations = nub $ map (\(v,_,e) -> (v,simpExp e)) (concatMap (isoVar ["=="]) (filter isEq cs))
>       where isEq (Ap (Ap (Var "==") _) _) = True
>             isEq _                        = False
>     -- search for local definitions:
>     localDefs = getGenerators cs
>     -- derive equations for local definitions
>     localEquations = filter isLoc equations
>       where isLoc (v, _) = elem v localDefs
>     -- remove mutual equations
>     localEquations' = removeDups [] localEquations
>       where
>         removeDups _  []         = []
>         removeDups st ((v,e):es) = let vars    = collectVars e
>                                        locVars = intersect vars localDefs
>                                    in case locVars of
>                                             [lv]  | elem lv st -> removeDups st es
>                                             otherwise          -> (v,e):removeDups (v:st) es
>     -- replace variables with their corresponding expressions
>     result = varBindLC (reverse localEquations') (Lc ((mapCore simplifyLC c) : (mapCores simplifyLC cs)))

> simplifyLC x         = mapCore simplifyLC x

Remove unnecessary equational filters:
---------------------------------------

> removeEqLC (Lc (c:cs)) = Lc ((mapCore removeEqLC c) : (mapCores removeEqLC cs'))
>   where
>     -- search for local definitions:
>     -- localDefs = getGenerators cs

>     cs' = (filter needed cs)
>       where needed e@(Ap (Ap (Var "==") e1) e2) = simpExp e1 /= simpExp e2 -- anyCommon localDefs (collectVars e)
>             needed _                            = True
> removeEqLC x = mapCore removeEqLC x

Optimize loops in list comprehensions:
---------------------------------------

> optLoops (Lc (c:cs)) = Lc ((optLoops c) : (concatMap opt (map optLoops cs)))
>   where
>     enum = getLoops cs
>     constraints = concatMap getConstraint cs
>     getConstraint e = isoVar relOps e
>     getConstr v = [ c | c@(vv,_,_) <- constraints, v == vv]

>     opt ((ArgVar k) :<- (b :.. e)) = [((ArgVar k) :<- (simpExp b' :.. simpExp e'))]
>       where
>        cs = getConstr k
>        (b', e') = opt' (b,e) cs
>        opt' (b,e) [] = (b,e)
>        opt' (b,e) ((_, "<",  exp):cs) = opt' (b, simpExp $ appl "-" [exp, Num 1]) cs
>        opt' (b,e) ((_, "<=", exp):cs) = opt' (b, exp) cs
>        opt' (b,e) ((_, ">",  exp):cs) = opt' (simpExp $ appl "+" [exp, Num 1], e) cs
>        opt' (b,e) ((_, ">=", exp):cs) = opt' (exp, e) cs
>        opt' _     x                   = pattErr "opt'" x
>     opt e = let cs = map fst3 (getConstraint e)
>             in case cs of
>                  []        -> [e]
>                  cs  | anyCommon (map fst3 enum) cs -> []
>                      | otherwise                    -> [e]
> optLoops x = mapCore optLoops x

Solve loop dependencies:
------------------------

> depLoops (Lc (c:cs)) = Lc ((depLoops c) : (reverse (opt [] (reverse (map depLoops cs)))))
>   where
>     opt _ [] = []
>     opt st (((ArgVar k) :<- (b :.. e)): defs) = ((ArgVar k) :<- (b' :.. e')): opt ((k,b',e'):st) defs
>        where
>          b' = let vars = collectVars b
>                   binds = concatMap (getLoopBegin st) vars
>               in simpExp $ varBind binds b
>          e' = let vars = collectVars e
>                   binds = concatMap (getLoopEnd st) vars
>               in simpExp $ varBind binds e
>     opt st (x:xs) = x: opt st xs

>     getLoopBegin loops k = [ (k,b) | (kk, b, _) <- loops, kk == k]
>     getLoopEnd   loops k = [ (k,e) | (kk, _, e) <- loops, kk == k]

> depLoops x = mapCore depLoops x


Remove redundant filters
--------------------------

> rmRedundantFilters (Lc (c:cs)) = Lc ((mapCore rmRedundantFilters c) : (concatMap opt (mapCores rmRedundantFilters cs)))
>   where
>     enum = getLoops cs
>     constraints = concatMap getConstraint cs
>     getConstraint e = isoVar relOps e
>     getConstr v = [ c | c@(vv,_,_) <- constraints, v == vv]

>     opt e@(Ap (Ap (Var op) _) _) | elem op relOps = if obsolete e then [] else [e]
>     opt e                                         = [e]

>     obsolete e = obsolete' e cs -- constraints
>       where
>       obsolete' e []     = False
>       obsolete' e (c:cs) = obs e c || obsolete' e cs

>       obs (Ap (Ap (Var "<") (Var v1)) e1) (Ap (Ap (Var "<") (Var v2)) e2) = v1 == v2 && var e1 == var e2 && diff e1 > diff e2
>       obs (Ap (Ap (Var "<=") (Var v1)) e1) (Ap (Ap (Var "<=") (Var v2)) e2) = v1 == v2 && var e1 == var e2 && diff e1 > diff e2
>       obs (Ap (Ap (Var "<") (Var v1)) e1) 
>           (Ap (Ap (Var ">") (Ap (Ap (Var "-") (Var v2)) (Var v2'))) (Num n)) = v1 == v2' && var e1 == v2 && ((-1) * diff e1) <= n
>       obs _ _              = False

>       extr (Var v) = (v, 0)
>       extr (Ap (Ap (Var "+") (Var v)) (Num n)) = (v, n)
>       extr (Ap (Ap (Var "-") (Var v)) (Num n)) = (v, -1 * n)
>       extr _                                   = ("?", 0)

>       var  = fst.extr 
>       diff = snd.extr

> rmRedundantFilters x = mapCore rmRedundantFilters x

High level lc optimizations
---------------------------

> data CHContext = CCNo | CCOp String

> hlOptimizations rts = hlOpt CCNo
>   where

>     hlOpt cnt (Lc [c])                                       = hlOpt cnt c
>     hlOpt cnt (Lc (c:cs))   | conditionsOnly cs              = let conds = makeAnd $ map (hlOpt cnt) cs
>                                                                in If (hlOpt cnt conds) (hlOpt cnt c) (Var "NULL_ELEM")
>     hlOpt cnt (Lc ((Var v):[(ArgVar v') :<- exp])) | v == v' = exp
>     hlOpt cnt (Lc (c:cs))                                    = Lc ((hlOpt cnt (varBindLC bind c)) : (map (hlOpt cnt) (map (varBindLC bind) cs')))
>       where
>         bind = concatMap getAtomarGen cs
>         getAtomarGen ((ArgVar v) :<- exp@(Ap (Var nt) ss)) | isAtomar    rts nt  = [(v, exp)]
>         getAtomarGen ((ArgVar v) :<- exp)                  | isAtomarExp rts exp = [(v, exp)]
>         getAtomarGen _                                                           = []
>         cs' = cs \\ (map (\(v, exp) -> (ArgVar v) :<- exp)) bind

>     hlOpt cnt (Ap (Ap (Var "foldl1") (Var chc)) exp@(Lc [c]))    =                                  (hlOpt (CCOp chc) exp)
>     hlOpt cnt (Ap (Ap (Var "foldl1") (Var chc)) exp@(Lc (c:cs))) = Ap (Ap (Var "foldl1") (Var chc)) (hlOpt (CCOp chc) exp)
>     hlOpt cnt (Ap (Ap (Var "foldl1") (Var chc)) exp)             =                                  (hlOpt (CCOp chc) exp)

>     --hlOpt cnt (Ap (Var chc) exp@(Lc (c:cs))) |  elem chc chcFcts  =  Ap (Var chc) (hlOpt chc exp)
>     --hlOpt cnt (Ap (Var chc) exp)             |  elem chc chcFcts  =                hlOpt chc exp

>     hlOpt cnt@(CCOp chc) (Ap (Ap (Var "++") e1) e2)    =  Ap (Ap (Var chc) (addCnt chc (hlOpt cnt e1))) (addCnt chc (hlOpt cnt e2))
>     hlOpt cnt x = mapCore (hlOpt cnt) x

>     addCnt cnt exp@(Lc [c])                         = exp
>     addCnt cnt exp@(Lc (c:cs)) | any isGenerator cs = Ap (Ap (Var "foldl1") (Var cnt)) exp
>     addCnt cnt exp                                  = exp

> conditionsOnly = all isCondition

> isCondition (_ :<- _) = False
> isCondition _         = True

> isGenerator (_ :<- _) = True
> isGenerator _         = False

alignment  = (max ((if ((i[]==j[])) 0)) 
                  ((max [ (((xDel (((i[]+1), j[])))+copen)+extend) ] 
                  ((max [ ((y[32]+copen)+extend) | y[32] <- (xIns ((i[], (j[]-1)))) ] 
                        [ ((alignment (((i[]+1), (j[]-1))))+(w ((z!(i[]+1))) ((z!j[])))) ])))))

alignment  = (max ((if ((i[]==j[])) 0)) 
            ((max ((((xDel (((i[]+1), j[])))+copen)+extend)) 
            ((max [ ((y[32]+copen)+extend) | y[32] <- (xIns ((i[], (j[]-1)))) ] 
                (((alignment (((i[]+1), (j[]-1))))+(w ((z!(i[]+1))) ((z!j[]))))))))))

------------------------------------------------------------------------------

Dependency Analysis
-------------------

> deriveDeps tab defs = [(n', n, fst $ depana n False (thd3 $ just $ getDef defs n')) | n <- tab, n' <- tab]
>   where
>     depana n cll (Ap (Var v) ss) = (cll || (v == n && isZero ss) || cll', Ap (Var v) ss')
>        where (cll', ss') = depana n cll ss
>     depana n cll exp = collectCore (depana n) cll exp

>     isZero (Ap (Ap (Constr "(,)") (Var "i")) (Var "j")) = True
>     isZero _                                            = False
>     just (Just n) = n
>     just _        = error "deriveDeps: should not happen"

> ppDepTab deps = concatMap pp deps
>   where pp (n,n',True) = n ++ " -> " ++ n' ++ "\n"
>         pp _           = ""

> sortDeps :: [String] -> [(String, String, Bool)] -> [String]
> sortDeps _ []    = []
> sortDeps ps deps = ret where 

>              canGo = filter (noDep deps) ps

>              newdeps = [ (n, n', b) | (n, n', b) <- deps, notElem n canGo, notElem n' canGo ]
>              newps   = [ p | p <- ps, notElem p canGo ]
>              ret = if newdeps == deps 
>                      then error "cannot solve dependencies in source program" 
>                      else canGo ++ sortDeps newps newdeps

>              noDep [] _                = True
>              noDep ((n, _, b): recs) x = ((n == x && not b) || n /= x) && noDep recs x


Calculate Yield size:
---------------------

> type YSize = ([Int], [Int])
> ppYSize ([],    []) = "(0, Inf)"
> ppYSize ([low], []) = "(" ++ show low ++ ", Inf)"
> ppYSize ([],  [up]) = "(0, " ++ show up ++ ")"
> ppYSize ([low],[up])= "(" ++ show low ++ ", " ++ show up ++ ")"

> calcYSizes =  map (\(n,_, exp) -> (n, calcYSize exp))

> calcYSize (Lc [c]) = calcYSize c
> calcYSize (Lc (c:cs)) = (nub $ low, nub $ up)
>   where
>     low = concatMap (matchLow.simp) (isoVar ["<=","<","=="] yield)
>        where
>          matchLow ("i_i", "==", Var "j_j")                             = [0]
>          matchLow ("i_i", "==", Ap (Ap (Var "-") (Var "j_j")) (Num n)) = [n]
>          matchLow ("i_i", "<=", Var "j_j")                             = [0]
>          matchLow ("i_i", "<=", Ap (Ap (Var "-") (Var "j_j")) (Num n)) = [n]
>          matchLow ("j_j", ">=", Ap (Ap (Var "+") (Var "i_i")) (Num n)) = [n]
>          matchLow ("i_i", "<", Var "j_j")                              = [1]
>          matchLow ("i_i", "<", Ap (Ap (Var "-") (Var "j_j")) (Num n))  = [n+1]
>          matchLow ("j_j", ">", Ap (Ap (Var "+") (Var "i_i")) (Num n))  = [n+1]
>          matchLow x = [] --pattErr "calcYSize.matchLow" x
>     up = concatMap (matchUp.simp) (isoVar ["<=","<"] yield)
>        where
>          matchLow ("i_i", "==", Var "j_j")                             = [0]
>          matchLow ("i_i", "==", Ap (Ap (Var "-") (Var "j_j")) (Num n)) = [n]
>          matchUp ("i_i", "<=", Var "j_j")                              = []
>          matchUp ("i_i", ">=", Ap (Ap (Var "-") (Var "j_j")) (Num n))  = [n]
>          matchUp ("j_j", "<=", Ap (Ap (Var "+") (Var "i_i")) (Num n))  = [n]
>          matchUp ("i_i", ">", Ap (Ap (Var "-") (Var "j_j")) (Num n))   = [n-1]
>          matchUp ("j_j", "<", Ap (Ap (Var "+") (Var "i_i")) (Num n))   = [n-1]
>          matchUp x = [] --pattErr "calcYSize.matchUp" x

>     simp (v, op, e) = (v, op, simpExp e)

>     -- if a loop exists, the first one should contain the (i,j) boundary constraints
>     -- otherwise, take one of the (normally, there is only one) of the relops:
>     yield = head' ((map (\(_,b,e) -> appl "<=" [b,e]) (getLoops cs)) ++ getRelOps cs ++ [applv "<=" ["i_i", "j_j"]])
>                   "cannot derive yieldsize; should not happen!"

>     getRelOps cs = concatMap get cs
>       where get e@((Ap (Ap (Var op) e1) e2)) | elem op ("==":relOps) && containsVar e "i_i" && containsVar e "j_j" = [e]
>             get _                                                                                           = []


> calcYSize (Ap (Ap (Var "++") e1) e2) = (min' l1 l2, max' u1 u2)
>  where
>    (l1, u1) = calcYSize e1
>    (l2, u2) = calcYSize e2

>    min' [] _    = []
>    min' _ []    = []
>    min' [a] [b] = [min a b]

>    max' [] _    = []
>    max' _ []    = []
>    max' [a] [b] = [max a b]

> calcYSize (Ap _ exp) = calcYSize exp  -- for application of objective function

> calcYSize x = ([],[]) -- mapCore calcYSize x

Derive Result Type:
---------------------

> data ResType 
>      = RTUnknown
>      | RTAtom String
>      | RTListOf ResType
>      | RTTupleOf [ResType]
>                             deriving (Eq, Show)

> ppResType (RTUnknown)     = "?"
> ppResType (RTAtom s)      = s
> ppResType (RTListOf r)    = "[" ++ ppResType r ++ "]"
> ppResType (RTTupleOf rts) = "(" ++ mapsep ", " ppResType rts ++ ")"

> isAtomar rts v = let tp = case getVal rts v of
>                              Just t -> t
>                              otherwise -> error $ "no type given for " ++ v
>                  in case tp of 
>                         RTListOf  _ -> False
>                         otherwise   -> True

> isAtomarExp rts exp = let tp = deriveResExp rts exp
>                       in case tp of
>                            RTListOf _ -> False
>                            otherwise  -> True

> deriveRes defs = fix init deriveRes' defs
>   where
>     init = zip (map fst3 defs) (repeat (RTListOf (RTAtom "Int")))

>     fix currentRes wrk val = e
>       where
>         newRes = wrk currentRes val
>         e | newRes == currentRes = newRes
>           | otherwise            = fix newRes wrk val

> deriveRes' rts defs = map (\(n, _, exp) -> (n, deriveResExp rts exp)) defs

> deriveResExp rts (Var v) = case getVal rts v of
>                               Nothing -> RTUnknown
>                               Just rt -> rt

> deriveResExp rts (Num _) = RTAtom "Int"
> deriveResExp rts (Ap (Ap (Var "++") e1) e2) = maxRT (deriveResExp rts e1) (deriveResExp rts e2)
> --deriveResExp rts (Ap (Var f) e) | elem f atomChoiceFunctions = makeAtomRT (deriveResExp rts e)
> --                                | otherwise                  = maxRT (deriveResExp rts (Var f)) (deriveResExp rts e)
> deriveResExp rts (Ap (Ap (Var "foldl1") (Var chc)) e) = makeAtomRT (deriveResExp rts e)
> deriveResExp rts (Lc (c:cs)) = foldr1 maxRT genRT
>   where
>     genRT = map (deriveResExp rts) (c:cs)

> deriveResExp rts (_ :<- e)  = deriveResExp rts e
> deriveResExp rts (_ :.. _)  = RTListOf (RTAtom "Int")
> deriveResExp rts (Constr _) = RTAtom "Constr"
> deriveResExp rts (Ap (Ap (Var op) _) _) | elem op ["==","/=","<=","<",">",">="] = RTAtom "Bool"
> deriveResExp rts (Ap e1 e2) = deriveResExp rts e1
> deriveResExp rts (If c t e) = deriveResExp rts t
> deriveResExp rts x          = pattErr "deriveResExp" x

> maxRT x y | x == y      = x
> maxRT x@(RTListOf a) _  = x
> maxRT _ y@(RTListOf a)  = y
> maxRT x RTUnknown       = x
> maxRT RTUnknown y       = y
> maxRT x (RTAtom "Bool") = x
> maxRT (RTAtom "Bool") y = y
> maxRT x y = x
> maxRT x y = error $ "Type error: " ++ ppResType x ++ " ++ " ++ ppResType y

> makeAtomRT (RTListOf rt) = rt
> makeAtomRT rt            = rt

 atomChoiceFunctions = ["maximum", "minimum", "sum", "foldl1"]

List comprehension helpers:
----------------------------

> getGenerators cs = concatMap get cs
>        where get (ArgVar c :<- _) = [c]
>              get _                = []

> getLoops cs = concatMap get cs
>       where get ((ArgVar k) :<- (b :.. e)) = [(k,b,e)]
>             get _                          = []


Bind variables in expression:
--------------------------------------------

> varBind   vars e = varBind' True  vars e
> varBindLC vars e = varBind' False vars e

> varBind' :: Bool -> [(String, Core)] -> Core  -> Core
> varBind' b vars (Var v) = let vv = getVal vars v
>                           in case vv of
>                                Nothing -> Var v
>                                Just e  -> e
> varBind' True  vars ((ArgVar c1) :<- c2)  = (convVar (varBind' True vars (Var c1))) :<- (varBind' True vars c2) 
>     where convVar (Var v) = ArgVar v
>           convVar x       = pattErr "varBind'"  x
> varBind' False vars (c1 :<- c2)  = c1 :<- (varBind' False vars c2) 
> varBind' b vars e = mapCore (varBind' b vars) e

Collect used variables from a core expression:
----------------------------------------------
Caution! Does _not_ yield local generator variables!

> collectVars e = fst (collect [] e)
>   where
>     collect cll (Var v) = (v:cll, Var v)
>     collect cll (c1 :<- c2) = (cll', c1 :<- c2')
>         where (cll', c2') = collect cll c2
>     collect cll x       = collectCore collect cll x

> containsVar e v = elem v $ collectVars e

Isolate Variable, depends on given operator:
---------------------------------------------

> isoVar ops e = nub $ concatMap (\op -> isoVar1 op [] (simpExp e)) ops

> isoVar1 o st e@(Ap (Ap (Var op) e1) e2) | o == op                         = isoVar2 0 o st e
>                                         | elem o relOps && revOp op == o  = isoVar2 0 o st (Ap (Ap (Var o) e2) e1)
>                                         | otherwise                       = []
> isoVar1 _ _  _                        = []

> isoVar2 n _ st exp | n > 30      = []
>                    | elem exp st = []

> isoVar2 n o st exp@(Ap (Ap (Var op) (Var v)) e) =       (v, op, e) : isoVar2 (n+1) o (exp:st) (simpExp (Ap (Ap (Var (revOp op)) e) (Var v)))
> isoVar2 n o st exp@(Ap (Ap (Var op) (Ap (Ap (Var "+") e1) e2)) e3) = isoVar2 (n+1) o (exp:st) (simpExp (Ap (Ap (Var op) e1) (appl "-" [e3, e2]))) ++
>                                                                      isoVar2 (n+1) o (exp:st) (simpExp (Ap (Ap (Var op) e2) (appl "-" [e3, e1])))
> isoVar2 n o st exp@(Ap (Ap (Var op) (Ap (Ap (Var "-") e1) e2)) e3) = isoVar2 (n+1) o (exp:st) (simpExp (Ap (Ap (Var op) e1) (appl "+" [e3, e2]))) ++
>                                                                      isoVar2 (n+1) o (exp:st) (simpExp (Ap (Ap (Var (revOp op)) e2) (appl "-" [e1, e3])))
> isoVar2 n o st exp@(Ap (Ap (Var op) (Num v)) e) =                    isoVar2 (n+1) o (exp:st) (simpExp (Ap (Ap (Var (revOp op)) e) (Num v)))
> isoVar2 n o st exp@(Ap (Ap (Var op) e1) e2) =                        []
> isoVar2 n _ _  e = pattErr "isoVar2" e

> revOp "<"  = ">"
> revOp ">"  = "<"
> revOp "<=" = ">="
> revOp ">=" = "<="
> revOp "==" = "=="
> revOp x    = x -- pattErr "revOp" x

> relOps = ["<",">","<=",">="]
> --relOps = ["<=",">="]

> testIso e = putStrLn $ unlines $ map show $ isoVar [">="] e
> testIso2 e = putStrLn $ unlines $ map show $ isoVar relOps e

Simplify arithmetic expressions:
---------------------------------

> -- for complete core expressions:
> simplifyExps (Lc (c:cs)) = simp (Lc (c:cs))
>    where
>      simp exp@(Ap (Ap (Var f) e1) e2) | elem f ["+", "-"]    = simpExp exp
>      simp exp@(Ap (Ap (Var f) e1) e2) | elem f ("==":relOps) 
>            = case simpExp e1 of
>                 (Ap (Ap (Var "-") e) (Num n)) -> simpExp $ appl f [e, appl "+" [e2, Num n]]
>                 (Ap (Ap (Var "+") e) (Num n)) -> simpExp $ appl f [e, appl "-" [e2, Num n]]
>                 otherwise                     -> mapCore simp exp
>      simp x                                             = mapCore simp x
> simplifyExps x   = mapCore simplifyExps x

> -- for arithmetic expressions:

> simpExp = simpExp2 . simpExp1

> simpExp1 (Ap (Ap (Var "-") (Num n1)) (Num n2)) = Num $ n1 - n2
> simpExp1 (Ap (Ap (Var "+") (Num n1)) (Num n2)) = Num $ n1 + n2
> simpExp1 (Ap (Ap (Var "-") (Var v1)) (Var v2)) | v1 == v2 = Num 0
> simpExp1 (Ap (Ap (Var "-") (Ap (Ap (Var "+") e1) (Num n))) (Num n2)) = simpExp1 $ Ap (Ap (Var "+") (simpExp1 e1)) (Num $ n - n2)
> simpExp1 (Ap (Ap (Var "+") (Ap (Ap (Var "-") e1) (Num n))) (Num n2)) = simpExp1 $ Ap (Ap (Var "+") (simpExp1 e1)) (Num $ (-1 * n) + n2)
> simpExp1 (Ap (Ap (Var "+") (Ap (Ap (Var "+") e1) (Num n))) (Num n2)) = simpExp1 $ Ap (Ap (Var "+") (simpExp1 e1)) (Num $ n + n2)
> simpExp1 (Ap (Ap (Var "-") (Ap (Ap (Var "-") e1) (Num n))) (Num n2)) = simpExp1 $ Ap (Ap (Var "-") (simpExp1 e1)) (Num $ n + n2)
> simpExp1 (Ap (Ap (Var "+") e) (Num 0)) = simpExp1 e
> simpExp1 (Ap (Ap (Var "+") e) (Num n)) | n < 0 = (Ap (Ap (Var "-") (simpExp1 e)) (Num $ -1 * n))
> simpExp1 (Ap (Ap (Var "-") e) (Num 0)) = simpExp1 e
> simpExp1 (Ap (Ap (Var "-") e) (Num n)) | n < 0 = (Ap (Ap (Var "+") (simpExp1 e)) (Num $ -1 * n))
> simpExp1 (Ap (Ap (Var "+") (Num n)) e) = simpExp1 (Ap (Ap (Var "+") (simpExp1 e)) (Num n))
> simpExp1 (Ap (Ap (Var op) e1) e2)      = (Ap (Ap (Var op) (simpExp e1)) (simpExp e2))
> simpExp1 e = e

> -- simpExp2 optimizes only adds and subs:
> simpExp2 exp@(Ap (Ap (Var op) _) _) 
>    | not (elem op ["+","-"]) = exp
>    | otherwise = let val         = calcExp exp
>                      (Just val') = val
>                      sexp        = sExp exp
>                      res | isNumber sexp = Num val'
>                          | val' == 0  = sexp
>                          | val' < 0   = Ap (Ap (Var "-") sexp) (Num (-1 * val'))
>                          | otherwise = Ap (Ap (Var "+") sexp) (Num val')
>                  in case val of
>                      Nothing -> exp
>                      Just _  -> res
>    where isNumber (Num _) = True
>          isNumber _       = False

> simpExp2 exp = exp


> sExp (Var v) = Var v
> sExp (Num n) = Num n
> --sExp (Ap (Ap (Var "-") (Num n)) e) = Ap (Var "-") (sExp e)
> sExp (Ap (Ap (Var "+") (Num n)) e) = sExp e
> sExp (Ap (Ap (Var "-") e) (Num n)) = sExp e
> sExp (Ap (Ap (Var "+") e) (Num n)) = sExp e
> sExp (Ap (Ap (Var op) e1) e2)      = Ap (Ap (Var op) (sExp e1)) (sExp e2)
> sExp x = pattErr "sExp" x

> calcExp (Num n)                   = Just n
> calcExp (Var _)                   = Just 0
> calcExp (Ap (Ap (Var "-") e1) e2) = maybeOp (calcExp e1) (-) (calcExp e2)
> calcExp (Ap (Ap (Var "+") e1) e2) = maybeOp (calcExp e1) (+) (calcExp e2)
> calcExp x                         = Nothing -- pattErr "calcExp" x

> maybeOp Nothing _ _          = Nothing
> maybeOp _ _ Nothing          = Nothing
> maybeOp (Just a) op (Just b) = Just $ op a b 


> evalExp vars exp@(Ap (Ap op e1) e2)
>   = case exp' of
>       (Ap (Ap op (Num n1)) (Num n2)) -> Just $ n1 - n2
>       otherwise                      -> Nothing 
>   where
>     bind = zip vars (repeat (Num 0))
>     exp' = simpExp $ varBind bind exp

> evalExp _ _ = Nothing

Compiler IO
---------------

> compStep vl text func pp exp = do
>   res <- return $ func exp
>   if vl >= trace then do
>                       putStrLn $ "\n" ++ title ( text ++ ":" )
>                       putStrLn $ pp res
>                  else return ()
>   if vl == debug then do
>                       putStrLn $ "\n" ++ title "Debug output:"
>                       print res
>                  else return ()
>   return res

> optOut vl text | vl >= trace = putStrLn text
>                | otherwise   = return ()

> optDebOut vl text | vl >= debug = putStrLn $ show text
>                   | otherwise   = return ()


> title t = replicate 78 '-' ++ "\n" ++ t ++ "\n" ++ replicate (length t + 2) '-'


> main				:: IO ()
> main				=  do
>                                  args <- getArgs 
>                                  cmain args
>                                  return ()

> binName = "core"
> info =
>     "Usage: " ++ binName ++ " [options] <file>\n" ++
>     "Options:\n" ++
>     "  -h                     Display this information\n" ++
>     "  -vl <verbosity level>  Specify output verbosity level\n" ++
>     "                            verbosity level: \n" ++
>     "                               t -> target, r -> trace, d -> debug" 

> cmain [] = putStrLn noArgs where
>    noArgs = 
>      binName ++ ": missing file arguments\n"++
>      "Try `" ++ binName ++ " -h' for more information."

> cmain s = res where
>             res | elem "-h" s = putStrLn info
>                 | otherwise   = compile vl file
>             vl   = altList s "-vl" [("t", target), ("r",trace), ("d",debug)] 
>                            target "verbosity level"
>             file = case filterParams s of
>                           [f] -> f
>                           ff  -> error $ "no valid filename given: " ++ sepList " " ff

> paramList :: [(String, Int)]
> paramList = [("-vl", 1)]

> filterParams []     = []
> filterParams (s:ss) = case getVal paramList s of
>                             Just n -> filterParams (drop n ss)
>                             Nothing -> s:filterParams ss

> getParams [] _     = []
> getParams (s:ss) p | s == p    = case getVal paramList s of
>                                   Just n -> take n ss
>                                   Nothing -> []
>                    | otherwise = getParams ss p

> altList :: (Eq a) => [String] -> String -> [(String,a)] -> a -> String -> a
> altList s param alts std name = case getParams s param of
>                                  [] -> std
>                                  x  | used == [] -> error $ "cannot handle " ++ name ++ " " ++ sepList " " x
>                                     | otherwise  -> head used 
>                                     where used = [ res | (p, res) <- alts, [p] == x]
