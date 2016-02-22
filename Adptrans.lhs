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
%include tex/Dss.fmt

%if code 

> module Adptrans(

>   addidProd,
>   addidProds,
>   alterListCompr,
>   collectNTs,
>   findCRFilter,
>   findChoiceProds,
>   findUsedAlgebraFunctions,
>   getDuplicates,
>   getUsedProds,
>   mergeProds,
>   reachabilityAnalysis,
>   recurProds,
>   liftWheres,
>   replCombs,
>   replTerms,
>   rev_Adptrans,
>   unliftWheres,
>   mergeAppls

> ) where

> import Constants
> import List
> import MathExp
> import Tools
> import StringLib
> import Yieldsize
> import Parse
> import Syntax
> import Expr

> rev_Adptrans =  "$Revision$"

%endif

\include{Adptrans.descr}

%if code

%----------------------------------------------------------------------------------------------------
\subsection{Ersetzen der Terminale durch Nichtterminale}
%----------------------------------------------------------------------------------------------------

Aufsammeln der linken Seiten:

> collectNTs :: [ProdSource] -> [String]
> collectNTs ps = concatMap collect ps where
>   collect (n :====  (_,_,_,_,ps)) = n:concatMap collect ps
>   collect (DirectDefSource n _ _) = [n]
>   collect (CombDef _ _)           = []

Ersetzen der Terminale durch Nichtterminale:

> replTerms :: [String] -> [ProdSource] -> [ProdSource]
> replTerms nts ps = map repl ps where
>    repl (n :====  (c,tab,tr,u,ps')) = (n :====  (c,tab,tr,replU u, map repl ps'))
>    repl (CombDef a b)                          = CombDef a b
>    repl (DirectDefSource n ss (lc_e, lc_defs)) = (DirectDefSource n ss (lc_e, map replLC lc_defs))

>    replU (Terminal (n,args)) | not (elem n nts) = Terminal (n,args)
>                              | args /= []       = error $ "nonterminal " ++ n ++ " is used with arguments " ++ sepList ", " args ++ ".\n" ++
>                                                           "This is currently not supported."
>                              | otherwise        = Nonterminal n

>    replU x              = mapUnit replU x

>    replLC e@(LCExp _)        = e
>    replLC (LCUnit e unit ss) = LCUnit e (replU unit) ss


%----------------------------------------------------------------------------------------------------
\subsection{Ersetzen benutzerdefinierter Kombinatoren}
%----------------------------------------------------------------------------------------------------

> replCombs :: Char -> [ProdSource] -> [ProdSource]
> replCombs sep ps = repl ps [] ps
>   where
>     repl :: [ProdSource] -> [String] -> [ProdSource] -> [ProdSource]
>     repl _ _ [] = []
>     repl prods cnt ((n :==== (cmt, tab, tr, u, wps)): ps) = 
>          ((n :==== (cmt, tab, tr, u', wps')): ps') where
>                                                     wps' = repl prods (cnt ++ [n]) wps
>                                                     ps'  = repl prods cnt ps
>                                                     u'   = replU (prods, (cnt ++ [n])) u
      
>     repl prods cnt ((CombDef _ _): ps)            =   repl prods cnt ps
>     repl prods cnt (d@(DirectDefSource _ _ _):ps) = d:repl prods cnt ps
      
>     getCombDef prods cnt x = head' [ p | (n, (CombDef _ p)) <- filterProd sep cnt prods, n==x]
>                                    ("undefined combinator " ++ x)
      
>     replU :: ([ProdSource], [String]) -> Unit -> Unit
>     replU (prods, cnt) (a :~~! (vnext, b))  = case getCombDef prods cnt vnext of
>                                                   (CombYSize l r)   -> (a', l) :~~   (b', r)
>                                                   (CombLA    l r)   -> (a', l) :^^^  (b', r)
>                                                   (CombDB    l r d) -> (a', l) :/\\/ (b', r, d)
>                                                 where 
>                                                   a' = (replU (prods, cnt) a) 
>                                                   b' = (replU (prods, cnt) b)
>     replU params x                          = mapUnit (replU params) x

%----------------------------------------------------------------------------------------------------
\subsection{Sichtbarkeiten}
%----------------------------------------------------------------------------------------------------

> filterProd :: Char -> [String] -> [ProdSource] -> [(String, ProdSource)]  
> filterProd sep cnt ps = map sel (flattenProd sep [] (filterProd' sep cnt ps)) 
>   where 
>     sel (n,_,p) = (n,p)
      
> filterProdCntName :: Char -> [String] -> [ProdSource] -> [([String], ProdSource)]  
> filterProdCntName sep cnt ps = map sel (flattenProd sep [] (filterProd' sep cnt ps))
>   where 
>     sel (_,n,p) = (n,p)
  
> filterProd' :: Char -> [String] -> [ProdSource] -> [ProdSource]  
> filterProd' _ _ []  = []
> filterProd' sep cnt ((n :==== (c, tab, tr, u, wys)):ps) = sortProd ((n :==== (c, tab, tr, u, wys')): filterProd' sep cnt ps) 
>      where  wys' = case cnt of 
>                     []      -> []
>                     (tc:rc) -> if tc==n then filterProd' sep rc wys else []
  
> filterProd' sep cnt ((CombDef vnext params):ps)    = sortProd ((CombDef vnext params): filterProd' sep cnt ps)
> filterProd' sep cnt (d@(DirectDefSource _ _ _):ps) = sortProd (d: filterProd' sep cnt ps)
  
> sortProd :: [ProdSource] -> [ProdSource]  
> sortProd [] = []
> sortProd ((n :==== (c, tab, tr, u, [])):ps)    = sortProd ps ++ [(n :==== (c, tab, tr, u, []))]
> sortProd ((n :==== (c, tab, tr, u, wys)):ps)   = (n :==== (c, tab, tr, u, wys)) : sortProd ps
> sortProd ((CombDef vnext params):ps)           = sortProd ps ++ [(CombDef vnext params)]
> sortProd (d@(DirectDefSource _ _ _):ps)        = sortProd ps ++ [d]
  
> flattenProd :: Char -> [String] -> [ProdSource] -> [(String, [String], ProdSource)]
> flattenProd _ _ [] = []
> flattenProd sep cnt ((n :==== (c, tab, tr, u, wys)):ps) = flattenProd sep (cnt ++ [n]) wys ++ 
>                                                           [(pnameCnt sep (cnt ++ [n]), cnt ++ [n], (n :==== (c, tab, tr, u, [])))] ++ 
>                                                           flattenProd sep cnt ps
> flattenProd sep cnt ((CombDef vnext params):ps)         = [(vnext, [vnext], (CombDef vnext params))] ++
>                                                           flattenProd sep cnt ps
> flattenProd sep cnt (d@(DirectDefSource n _ _):ps)      = [(n, [n], d)] ++
>                                                           flattenProd sep cnt ps

> pnameCnt sep cnt = sepList [sep] cnt

%----------------------------------------------------------------------------------------------------
\subsection{Where-Konstruktionen auflösen}
%----------------------------------------------------------------------------------------------------

> liftWheres :: Char -> [ProdSource] -> [Prod]
> liftWheres sep ps = lift ps [] ps
>   where

>     lift :: [ProdSource] -> [String] -> [ProdSource] -> [Prod]
>     lift _ _ [] = []
>     lift prods cnt ((n :==== (cmt, tab, tr, u, wps)): ps) = 
>          ((n' :=== (cmt, tab, tr, u')): (wps' ++ ps')) where
>                                                     n'   = pnameCnt sep (cnt ++[n])
>                                                     wps' = lift prods (cnt ++ [n]) wps
>                                                     ps'  = lift prods cnt ps
>                                                     u'   = liftU (prods, (cnt ++ [n])) u
>     lift prods cnt ((DirectDefSource a b c):ps) = (DirectDef a b c):lift prods cnt ps
      
>     liftU :: ([ProdSource],[String]) -> Unit -> Unit
>     liftU (prods, cnt) (Nonterminal a) = Nonterminal (getinternalName prods cnt a)
>     liftU params x                     = mapUnit (liftU params) x

>     getinternalName :: [ProdSource] -> [Nt] -> Nt -> String
>     getinternalName prods cnt x = head' ([intn | (intn, (n :==== _))             <- filterProd sep cnt prods, n==x] ++
>                                          [intn | (intn, (DirectDefSource n _ _)) <- filterProd sep cnt prods, n==x])
>                                         ("getinternalName: unknown nonterminal " ++ x ++ showCnt cnt)
      
>     getContextName :: [ProdSource] -> [Nt] -> Nt -> [Nt]
>     getContextName prods cnt x = head' ([intn | (intn, (n :==== _))             <- filterProdCntName sep cnt prods, n==x] ++
>                                         [intn | (intn, (DirectDefSource n _ _)) <- filterProdCntName sep cnt prods, n==x]) 
>                                         ("getContextName: unknown nonterminal " ++ x ++ showCnt cnt)

%----------------------------------------------------------------------------------------------------
\subsection{Where-Konstruktionen wieder herstellen}
%----------------------------------------------------------------------------------------------------

> unliftWheres :: Char -> [ProdSource] -> [ProdSource]
> unliftWheres sep ps = filter isFlattened (iterate maxDepth (map flatNames ps))
>   where
>     names = map get ps
>       where
>         get (n :==== _)             = n
>         get (CombDef n _)           = n
>         get (DirectDefSource n _ _) = n
>         -- get x                       = pattErr "unliftWheres" x  
>     maxDepth = maximum (map getDepth names)

>     isFlattened (n :==== _) = getDepth n == 0
>     isFlattened _           = True

>     flatNames :: ProdSource -> ProdSource
>     flatNames c@(CombDef _ _)                  = c
>     flatNames d@(DirectDefSource _ _ _)        = d
>     flatNames (n :==== (cmt, tab, tr, u, wps)) = (n :==== (cmt, tab, tr, flat u, map flatNames wps))
>        where
>        flat (Nonterminal a) = Nonterminal (orgName a)
>        flat x               = mapUnit flat x
>     -- flatNames x                     = pattErr "unliftWheres.flatNames" x

>     iterate 0 ps     = ps
>     iterate stage ps = let ps' = unlift stage ps ps
>                        in iterate (stage-1) ps'

>     unlift stage ps []           = ps
>     unlift stage ps ((p@(n :==== _)):ps') | getDepth n == stage = unlift stage (updateProd ps p) ps'
>                                           | otherwise           = unlift stage ps ps'
>     -- CombDefs nur auf level 0:
>     unlift stage ps (_:ps')                                     = unlift stage ps ps'

>     updateProd [] p = error "unliftWheres.updateProd"
>     updateProd (p@(n :==== (cmt, tab, tr, u, wps)):ps) p'@(n' :==== pdef) 
>         | isParent n n' = (n :==== (cmt, tab, tr, u, wps ++ [(orgName n' :==== pdef)])):ps
>         | otherwise     = p: updateProd ps p'

>     getDepth n = length $ filter (== sep) n
>     splitName n | not $ elem sep n = [n]
>                 | otherwise        = let (nn, _:ns) = span (/= sep) n
>                                  in nn:splitName ns
>     isParent p n = init (splitName n) == splitName p
>     orgName n = last (splitName n)


%----------------------------------------------------------------------------------------------------
\subsection{Duplikate finden}
%----------------------------------------------------------------------------------------------------

> getDuplicates :: String -> [Prod] -> [(String, String)]
> getDuplicates axiom ps = filter (\(n,_) -> n /= axiom) (getDup ps)  -- the axiom can't be eliminated
>  where
>    getDup []                                       = []
>    getDup ((n :=== (_, Tabulated, _, Nonterminal nt)): ps) =           getDup ps
>    getDup ((n :=== (_, _,         _, Nonterminal nt)): ps) = (n, nt) : getDup ps
>    getDup ((n :=== (_, _, _      , _             )): ps)   =           getDup ps
>    getDup (d@(DirectDef _ _ _):ps)                 =           getDup ps

%----------------------------------------------------------------------------------------------------
\subsection{Nichttabellierte, rekursive Produktionen finden}
%----------------------------------------------------------------------------------------------------

> recurProds :: String -> [(String, String)] -> [Prod] -> [String]
> recurProds axiom dups ps = case isTabulated ps axiom of
>                              -- wenn das Axiom tabelliert ist, gibt es kein Problem
>                              Tabulated -> filterDups $ recs
>                              -- wenn es nicht tabelliert ist, muss es auch als rekursive Funktion implementiert werden:
>                              otherwise -> filterDups $ nub $ (axiom:recs)
>   where
>     filterDups rs = rs \\ (map fst dups)
>     recs   = nub $ concatMap (\(n :=== (_, _, _, unit)) -> recurUnit (ps, n, []) unit) (filter isProd ps)
>     -- Annahme: Direkte Definitionen duerfen nicht rekursiv sein:
>     isProd (n :=== (_,tab,_,_)) = tab /= Tabulated
>     isProd _                    = False

> isTabulated ps nt   = head' [tab | (n :=== (_,tab,_,_)) <-  ps, n==nt] 
>                         ("recurProds: should not occur!!!!, nt " ++ nt)

> recurUnit (ps, curNt, st) (Nonterminal a) | a == curNt      = [curNt]
>                                           | elem a st       = []
>                                           | isDirectDef a   = []
>                                           | isTabulated ps a == Tabulated = []
>                                           | otherwise    = recurUnit (ps, curNt, (a:st)) a' where 
>                                                  a' = head' [u | (n :=== (_,_,_,u)) <-  ps, n==a] 
>                                                             ("recurUnit: should not occur!!!!, nt " ++ a)
>                                                  isDirectDef a = case [n | (DirectDef n _ _) <-  ps, n==a] of
>                                                                    []        -> False
>                                                                    otherwise -> True 


> recurUnit params          x               = wrkUnit (recurUnit params) (++) [] x


%----------------------------------------------------------------------------------------------------
\subsection{Produktionen zusammenführen}
%----------------------------------------------------------------------------------------------------

> mergeProds :: UserFunctions -> [(String, String)] -> [String] -> [Prod] -> [Prod]
> mergeProds ufs dups recur prods = concatMap mergeProd prods
>  where
>    tabbed = collectTabulated prods
>    mergeProd ::  Prod -> [Prod]
>    mergeProd  (n :=== (c, tab, tr, unit)) 
>                | elem n (map fst dups) = []
>                | otherwise             = [(n :=== (c, tab, tr, mergeUnit unit))]

>    -- direkte Definitionen duerfen nicht eigenstaendig auftauchen:
>    mergeProd (DirectDef _ _ _) = []
>    -- mergeProd x                 = pattErr "mergeProds" x

>    mergeUnit (Nonterminal a) | elem a (map fst dups) = case [ d | (a',d) <- dups, a' == a] of
>                                                          []  -> error $ "internal error: no duplicate found for " ++ a
>                                                          [x] -> mergeUnit (Nonterminal x)
>                                                          xs  -> error $ "ambiguous definitions for " ++ a ++ ": " ++ 
>                                                                   concatMap (\x -> a ++ " = " ++ x ++ "\n") xs
>                              | elem a recur  = Nonterminal a
>                              | elem a tabbed = Nonterminal a
>                              | otherwise     = let
>                                                   a' = head' 
>                                                        ([u | (n :=== (_, _, _, u)) <-  prods, n==a] ++
>                                                         [ListCompr u iv (directDefYSize ufs n) | (DirectDef n iv u)  <-  prods, n==a])
>                                                       ("mergeUnit: should not occur!!!!, nt " ++ a)
>                                                in mergeUnit a' 
>    mergeUnit x               = mapUnit mergeUnit x

%----------------------------------------------------------------------------------------------------
\subsection{nichtverwendete Produktionen herauswerfen}
%----------------------------------------------------------------------------------------------------
TODO: Hier moechte man eigentlich, dass nur diejenigen Produktionen
uebernommen werden, die vom Axiom aus erreichbar sind

> getUsedProds :: String -> [Prod] -> [Prod]
> getUsedProds axiom ps = filter isUsed ps
>   where
>     isUsed (n :=== _) = elem n usedProds || n == axiom
>     isUsed x          = pattErr "getUsedProds.isUsed" x
>     usedProds = concatMap getProds ps
>       where
>         getProds (_ :=== (_, tab, tr, unit)) = getP unit
>         getProds x                           = pattErr "getUsedProds.getProds" x

>         getP (Nonterminal nt) = [nt]
>         getP x                = wrkUnit getP (++) [] x


> reachabilityAnalysis :: String -> [Prod] -> [Nt]
> reachabilityAnalysis axiom prods = analyze prods [axiom] [axiom]
>   where
>     analyze _ [] reachables    = reachables
>     analyze [] todos reachables = error $ "Can't find productions for some nonterminals (" ++ (show todos) ++ ")."
>     analyze prods (t:ts) reachables = let (prod',prods') = findProd t prods
>                                           rs = filter (\ s -> not $ elem s reachables) $ nub $ collectNTs (getUnit prod')
>                                       in analyze prods' (rs++ts) (rs ++ reachables)

>     -- liefert die Unit einer Produktion
>     getUnit :: Prod -> Unit
>     getUnit (nt :=== (comment,tab,tr,unit)) = unit

>     -- findet Produktion fuer das entsprechende Nichtterminal 'nt'
>     -- und liefert diese incl. den uebrigen Produktionen.
>     findProd :: Nt -> [Prod] -> (Prod,[Prod])
>     findProd nt [] = error $ "Can't find Production for nonterminal " ++ nt
>     findProd nt (prod@(name :=== _):ps) | nt == name  = (prod,ps)
>                                         | otherwise   = let (p,ps') = findProd nt ps in (p,prod:ps')
>     findProd nt _  = error "reachabilityAnalysis.findProd: Can't match pattern."

>     collectNTs :: Unit -> [Nt]
>     collectNTs u = fst $ coll [] u

>       where
>         coll nts (Nonterminal nt) = (nt: nts, Nonterminal nt)
>         coll nts x                = collectUnit coll nts x

%----------------------------------------------------------------------------------------------------
\subsection{Umbauen der Listenbeschreibungen}
%----------------------------------------------------------------------------------------------------

> alterListCompr :: [Prod] -> [Prod]
> alterListCompr ps = map alter ps 
>   where
>   nonterms = map getNt ps
>     where
>       getNt (n :=== _) = n
>       getNt (DirectDef n _ _) = n
>   alter (n :===  (c,tab,tr,u)) = (n :===  (c,tab,tr,alterU u))
>   alter (DirectDef n a b) = DirectDef n a b

>   alterU (ListCompr (lc_e, lc_defs) ijs ys) = ListCompr (updateIndex lc_e, map (alterLC.updateIndexLC) lc_defs) ijs' ys
>   -- alter (DirectDef n ijs (lc_e, lc_defs)) = (DirectDef n ijs' $ (updateIndex lc_e, map (alterLC.updateIndexLC) lc_defs))
>     where
>       (idxBinding,ijs') = case ijs of
>           (ST (i,j))           -> ([(i, ExpVar "lc_i"),   (j, ExpVar "lc_j")], ST ("lc_i", "lc_j"))
>           (TT (i1,j1) (i2,j2)) -> ([(i1, ExpVar "lc_i1"), (j1, ExpVar "lc_j1"),
>                                     (i2, ExpVar "lc_i2"), (j2, ExpVar "lc_j2")],
>                                    TT ("lc_i1", "lc_j1") ("lc_i2", "lc_j2"))
>       updateIndex e = insertVarBinds (locVarBinding ++ idxBinding)  e
>       updateIndexLC (LCExp e) = LCExp $ updateIndex e
>       -- an dieser Stelle kann nur LCExp auftreten:
>       alterLC (LCExp (ExpIn e1 (ExpPOp name args))) = LCUnit e1 symbol (expTupelToSS args)
>          where
>            symbol | elem name nonterms = Nonterminal name
>                   | otherwise          = (Terminal (name, []))
>       alterLC x                                     = x

>       -- alle lokalen Variablen werden mit einem lc_-Praefix versehen:
>       locVarBinding = map (\v -> (v, ExpVar $ "lc_" ++ v)) localVars
>       localVars = concatMap get lc_defs
>         where
>           get (LCExp (ExpIn  (ExpVar v) _))             = [v]
>           get (LCExp (ExpLet (ExpVar v) _))             = [v]
>           get (LCExp (ExpLet (ExpTupel v) _))           = let fla (ExpVar v)    = [v]
>                                                               fla (ExpTupel vs) = concatMap fla vs
>                                                               fla x             = pattErr "alterListCompr.fla" x
>                                                           in concatMap fla v
>           get _                                         = []

>   alterU x = mapUnit alterU x

%----------------------------------------------------------------------------------------------------
\subsection{region_contains filter finden}
%----------------------------------------------------------------------------------------------------

> findCRFilter :: [Prod] -> [String]
> findCRFilter ps = nub $ concatMap getUnit ps
>   where
>     getUnit (n :===  (_,_,_,unit)) = find unit
>     getUnit (DirectDef _ _ _)      = []
>     find  (u `With` ("contains_region", [arg])) = (init $ tail arg): find u
>     find   x                                    = wrkUnit find (++) [] x

%----------------------------------------------------------------------------------------------------
\subsection{Auswahlfunktionen identifizieren}
%----------------------------------------------------------------------------------------------------

> findChoiceProds :: [Prod] -> [String]
> findChoiceProds ps = nub $ concatMap getUnit ps
>   where
>     getUnit (n :===  (_,_,_,unit)) = findChoiceUnit unit
>     getUnit (DirectDef _ _ _)      = []
>     findChoiceUnit  (u :... h) = h: findChoiceUnit u
>     findChoiceUnit   x         = wrkUnit findChoiceUnit (++) [] x

%----------------------------------------------------------------------------------------------------
\subsection{Verwendete Algebrafunktionen identifizieren}
%----------------------------------------------------------------------------------------------------

> findUsedAlgebraFunctions :: [Prod] -> [String]
> findUsedAlgebraFunctions ps = nub $ concatMap getUnit ps
>   where
>     getUnit (n :===  (_,_,_,unit))  = find unit
>     getUnit (DirectDef _ _ _)       = []
>     find  ((f,_) :<<< u) = f: find u
>     find  (ListCompr (ExpPOp name _, _) _ _) = [name]
>     find   x             = wrkUnit find (++) [] x

%----------------------------------------------------------------------------------------------------
\subsection{Einzelne Nichtterminale nt mit _id <<< nt versehen -> wesentlich einfacher in Codegen}
%----------------------------------------------------------------------------------------------------
=> wird momentan nicht verwendet


> addidProds :: [Prod] -> [Prod]
> addidProds  ps = map addidProd ps

> addidProd ::  Prod -> Prod
> addidProd  (n :=== (c, tab, tr, unit)) = n :=== (c, tab, tr, addidUnit unit)

> addidUnit (Nonterminal a :||| Nonterminal b) = (idFct :<<< Nonterminal a) :||| (idFct :<<< Nonterminal b)
> addidUnit (Nonterminal a :||| b)             = (idFct :<<< Nonterminal a) :||| addidUnit b
> addidUnit (a :||| Nonterminal b)             = addidUnit a :||| (idFct :<<< Nonterminal b)
> addidUnit x                                  = mapUnit addidUnit x

%----------------------------------------------------------------------------------------------------
\subsection{Reduzieren von Algebrafunktionsanwendungen}
%----------------------------------------------------------------------------------------------------

> mergeAppls :: [AlgDefs] -> [Prod] -> ([AlgDefs], [Prod])
> mergeAppls [(algName, (algdt, algfs))] ps = let 
>                          ps'   = map (\(n :=== (cmt, tab, tr, u)) -> (n :=== (cmt, tab, tr, merge u))) ps
>                          newfs = nub $ concatMap (\(n :=== (_, _, _, u))   -> fst (collectfs [] u)) ps'
>                          alg' = [(algName, (algdt, map createFs newfs ++ algfs))]
>                      in (alg', ps')
>   where
>     collectfs cll ((f, _) :<<< u) | elem '(' f = collectfs (f:cll) u
>                                   | otherwise  = collectfs    cll  u
>     collectfs cll x               = collectUnit collectfs cll x

>     createFs n | not (elem '(' n) = getAlg n                  -- (n, [], [], ExpVar "x")
>                | otherwise = 
>                      let (fo, fi, num) = splitName n
>                          fi'           = createFs fi
>                      in  create fo fi' num

>       where
>         getAlg n = head' [ alg | alg@(nn,_,_,_) <- algfs, n==nn] $ error "mergeAppls: algebra function not found: " ++ n

>         splitName n = (fo, reverse fi, read (reverse nums) :: Int)
>            where
>              (fo, _:x) = span (/= '|') n
>              (_:nums, _:fi) = span (/= '(') $ reverse x

>         create fo fi n = let
>                            (name1, dt1, args1o, rhs1o) = getAlg fo
>                            (name2, dt2, args2o, rhs2o) = fi
>                            (args1, rhs1) = renameVars "1" args1o rhs1o
>                            (args2, rhs2) = renameVars "2" args2o rhs2o

>                            binding                 = map (\(SigId v, rhs) -> (v, rhs)) $ bindArgs (args1!!(n-1)) rhs2
>                            rhs1'                   = flatStrcat $ insertVarBinds binding rhs1
>                            args1'                  = take (n-1) args1 ++ args2      ++ drop n args1
>                            dt1'                    = take (n-1) dt1   ++ init dt2   ++ drop n dt1
>                            name1'                  = fo ++ "|" ++ name2 ++ "(" ++ show n ++ ")"
>                          in (name1', dt1', args1', rhs1')

>         renameVars id args rhs = (args', rhs')
>            where
>              args' = map ren args
>                where ren (SigTupel args) = (SigTupel (map ren args))
>                      ren (SigId arg)     = (SigId $ arg ++ id)
>              rhs' = insertVarBinds bind rhs
>                where bind = zip names (map (\n -> ExpVar $ n ++ id) names)
>                      names = concatMap flat args
>                      flat (SigTupel args) = concatMap flat args
>                      flat (SigId arg)     = [arg]

>         bindArgs (SigTupel args) (ExpTupel rhss) = zip args rhss
>         bindArgs (SigId arg)      rhs            = [(SigId arg, rhs)]

>         flatStrcat (ExpPOp "strcat2" [exp1, exp2]) = mkcat $ flat exp1 ++ flat exp2
>           where flat (ExpPOp "strcat2" [exp1, exp2]) = flat exp1 ++ flat exp2
>                 flat exp                             = [exp]
>                 mkcat [exp1, exp2] = ExpPOp "strcat2" [exp1, exp2]
>                 mkcat (exp:exps)   = ExpPOp "strcat2" [exp, mkcat exps]

>         flatStrcat x = mapExp flatStrcat x


>     merge ((f1,_) :<<< unit) = addchc ((newName, []) :<<< unit'')
>       where 

>         newName = sepList "|" (f1:map (\(f,n) -> f ++ "(" ++ (show n) ++ ")") appls)

>         unit' = mapUnit merge unit
>         elems = getElems unit'
>         appls = concatMap getAppls (zip [1..] elems)
>         chs   = concatMap getChc   elems
>         elems' = map rmAppls elems
>         elems'' = concatMap flat elems'
>            where flat (a :~~~ b) = flat a ++ flat b
>                  flat x          = [x]         

>         unit'' = foldl1 (:~~~) elems''

>         getElems (a :~~~ b) = getElems a ++ getElems b
>         getElems x          = [x]

>         getAppls (n,((f,_) :<<< _)) = [(f,n)]
>         getAppls (n,(u :... h))     = getAppls (n,u)
>         getAppls _                  = []

>         getChc  (u :... h) = [h]
>         getChc  _          = []

>         addchc u = case nub chs of
>                     []  -> u
>                     hs  -> u :... (sepList "_" hs)

>         rmAppls (f :<<< u) = u
>         rmAppls (u :... h) = (rmAppls u)
>         rmAppls x          = x

>     merge x = mapUnit merge x


Tools:
------

> showCnt n = " in context " ++ sepList "." n


%endif

