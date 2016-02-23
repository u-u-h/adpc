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



\%include tex/lhs2TeX.fmt

> module Phases(

>   compile_all,
>   compile_allSM,
>   rev_Phases,

> ) where

> import System.Random
> import System.Time
> import System.Process
> import System.Exit
> import Data.List
> import Constants   -- globale Konstanten
> import Tools       -- häufig benötigte Funktionen
> import MathExp     -- mathematische Ausdrücke
> import Expr        -- Ausdrücke allgemein
> import StringLib   -- Bibliothek mit den terminalen Parsen
> import Syntax      -- abstrakte Syntax
> import Parse       -- Lexer und Parser
> import ParseTree (prettyPrint)
> import Parse2
> import ParseMonad
> import Lift
> import Convert
> import Yieldsize   -- Yield size analysis und Transformation
> import Adptrans    -- ADP-Transformationen
> import Track       -- Track-Mode analysis
> import Dss         -- Subscript-Ermittlung
> import Depana      -- Abhängigkeitsanalyse
> import TL          -- Zielsprache
> import TLData      -- Datentypen der Zielsprache
> import Codegen1    -- Basis-Codeerzeugung
> import TLFrame     -- Ausgabe von Rahmenwerk für bequemere Tests
> import Range       -- Indexbereichsanalyse
> import PrettyPrint -- enthaelt die Klasse Pretty um alle PrettyPrinter zu vereinheitlichen

> import System.IO(hPutStrLn, stderr)
> import Control.Monad

> -- import "FMP/FMPADP"

nur für optmierende Codeerzeugung:

> import IL2         -- Einsetzen der Algebrafunktionen und Parsercode
> import ListCompr
> import Codegen     -- optimierende Codeerzeugung

> import WidthAna    -- Automatische Tabellierung

> import LatexRecs   -- Latex - Rekurrenzen 

> import Algebras
> import SM
> import Beautify

> import Layout(layout,stripComments,processImports) -- from hsutils - transform 'normal'.hs to {;} format
> import ConfigOutput

> -- fuer Typechecker

> import TC(getTCresult)


> rev_Phases =  "$Revision: 851 $"

%----------------------------------------------------------------------------------------------------
\section{Zusammenführung der Übersetzungsphasen}
%----------------------------------------------------------------------------------------------------



Configuration:
---------------

eod = fst ...

> eod :: (a, b) -> a
> eod (english, german) = 
> --language: 
>   english

-------------------------------------------

> noOut :: SM ()
> noOut = return ()

> targetOnly :: Integer -> SM () -> SM ()
> targetOnly vl action 
>          | vl == target = action
>          | otherwise    = noOut

> putLine s vl
>   | vl == target     = noOut
>   | vl == latextrace = putSM ("> " ++ s)
>   | vl == trace      = putSM s
>   | vl == tracemore  = putSM s
>   | vl == debug      = putSM s
>   | vl == specialtrace = noOut

> ppYsaF (n, ys)         = n ++ " -> " ++ ppYSize ys
> ppYsaL (n, ys)         = n ++ " -> " ++ ppSYSize ys
> ppYsaFTrace            = unlines . map ppYsaF

> ppYsaT (n, dt, addif, ys)  = n ++ " -> " ++ ppSYSize ys ++ " " ++ dt ++ " " ++ if addif then " addif" else ""
> ppDups (n,u)           = n ++ " = " ++ u
> ppDep  (n,u)           = n ++ "(i,j) -> " ++ sepList ", " (map (pretty) u)

> getrnd :: Int -> IO Int
> getrnd n = getStdRandom (randomR (10000,n))


> readLayout searchpath f = do
>    -- if f contains an '>' we have haskell source code, otherwise f is a filename:
>    (input,f)  <- if elem '>' f then return (f, "tempadp")
>                                else do
>                                     inp <- readFile f
>                                     return (inp, f)
>    input <- return $ stripComments input

     -- module system ------------------

>    input <- processImports prefix searchpath f 1 input

     -- layout -------------------------

>    let input_layout = layout input
>    return input_layout


> -- | 31 parameters are way to much ...
> compile_all cmdl ver soa arr off cm cc ap vl stopL cg bt lcf tas ita ifct 
>             window toptO toptO2 gc tl nc bea tabopt algs alpp cs (pc,pn) f fo parseTree tlFilter typecheckFlag searchpath = do 

>    input_layout <- if (ParseMonad.failed parseTree) then readLayout searchpath f 
>                                                     else return $ ""

>    bea        <- getBeaPattern bea
>    time       <- getClockTime
>    time       <- toCalendarTime time

>    (_, (res,errln)) <- return $ runSM ([],-1) $ 
>                compile_allSM cmdl (ver,time) soa arr off cm vl stopL cg bt lcf tas ita ifct 
>                              window toptO toptO2 gc tl nc bea tabopt algs alpp cs False (pc,pn) f input_layout parseTree tlFilter typecheckFlag
>    result <- return$ ((unlines.reverse) res)
>    if elem '>' f 
>      then return ()
>      else if errln == -1 
>              then (if fo=="stdout"
>                      then putStrLn result
>                      else (if fo == [] then return () else writeFile fo result))
>              else hPutStrLn stderr result
>    if ap/="" then system $ "patch " ++ fo ++ " " ++ ap  else return ExitSuccess
>    return (result, errln)

> compile_allSM cmdl cInfo soa arr off cm vl stopL cg bt lcf tas ita ifct window toptO toptO2  
>               gc tl nc bea (tc,iuc,tp,sp,tadd,taddc,taddn) algOrder ppAlgNames csAlgName cgi (pc,pn) f inp parseTree tlFilter typecheckFlag = do

>   ------------- Parsen und Syntaxfehler abfangen ---------------------------------------------------
>   if pc then (do
>     parsedRes1    <- return $ parse inp
>     if (Parse.failed parsedRes1) then (do
>         (err, line) <- return $ Parse.getFailed parsedRes1
>         putSMerr (err, line)) else noOut
>     parsedRes2    <- return $ parse2 inp f
>     if (ParseMonad.failed parsedRes2) then (do
>         (err, line) <- return $ ParseMonad.getFailed parsedRes2
>         putSMerr (err, line)) else noOut
>     if not (Parse.failed parsedRes1 || ParseMonad.failed parsedRes2) then (do
>         (Parse.Ok      parsed1)  <- return parsedRes1
>         (ParseMonad.Ok parsed2)  <- return parsedRes2
>         parsed2c                 <- return $ adpprogram2adpprogram parsed2
>         if parsed1 == parsed2c then putSM "parse trees equal\n"
>                                else putSM $ "parse trees NOT equal\n" ++ "parse1:\n--------------\n" ++ ppADPProgram parsed1
>                                                                   ++ "\n\nparse2:\n--------------\n" ++ ppADPProgram parsed2c 
>         ) else noOut) 
>     else do
>       (parsedRes1, parsedRes2) <- return $ if pn == 1 then ([parse  inp], [])
>                                                       else (if ParseMonad.failed parseTree then ([], [parse2 inp f]) else ([], [parseTree]))
>       if (pn == 1 && Parse.failed      (head parsedRes1)) ||  
>          (pn == 2 && ParseMonad.failed (head parsedRes2)) then (do
>            (err, line) <- return $ if pn == 1 then Parse.getFailed (head parsedRes1) else ParseMonad.getFailed (head parsedRes2)
>            putSMerr (err, line))
>          else (do
>          ----- Spezielle Backtrace-Analysen sollen beim Tracen unterdrückt werden, ------------------------
>          ----- falls im normalen Modus --------------------------------------------------------------------
>          vlBT       <- return (if bt /= BTNone then vl else target)
       
>          ------------- Titel ausgeben ---------------------------------------------------------------------
>          title <- return $ let
>           title 
>                  | cm == imperativeCM = (("Compilation of " ++ f ++ " into " ++ pptln tl),
>                                          ("Übersetzung von " ++ f ++ " nach " ++ pptln tl))
>                  | cm == optimizeCM   = (("Combinator optimization of " ++ f),
>                                          ("Kombinatoroptimierung von " ++ f))
>                  | cm == latexCM      = (("Generation of Latex - DP recurrences for " ++ f), 
>                                          ("Erzeugung von Latex - DP Rekurrenzen für " ++ f))
>                  | cm == tabulateCM && tc == TCGood  =    (("Derive good table configurations for " ++ f), 
>                                                           ("Ermittlung guter Tabellenkonfigurationen für " ++ f))
>                  | cm == tabulateCM && tc == TCOptimal  ||
>                    cm == tabulateCMextern            = (("Derive optimal table configurations for " ++ f), 
>                                                         ("Ermittlung optimaler Tabellenkonfigurationen für " ++ f))
>           in title 
>          showTitle      title                                                 vl 0 "begin"
        
>          ------------- Ausgabe der geparsten Eingabe ------------------------------------------------------
>          showTitle      ("Syntax analysis","Syntaxanalye")                    vl 1 "parsed"
       
>          ------------- Verarbeitung fuer Parser2 ----------------------------------------------------------
       
>          parsed <- if (pn == 2) then (do
>              (ParseMonad.Ok parsed) <- return $ head parsedRes2
>              _        <- compileStep asl                    parsed
>                          ("Parse result","Parser Ergebnis")  prettyPrint      vl 1 "parsed"
       
>              liftSep      <- return $ if (cm == optimizeCM) then '$' 
>                                                             else '_'
       
>              lifted   <- compileStep (asl.(adpLiftWheres liftSep))
>                                                             parsed
>                          ("Eliminate where constructs",
>                           "Auflösen verschachtelter Where-Konstruktionen")    
>                                                              prettyPrint      vl 2 "parsed:lifted"
>              convert  <- return $ adpprogram2adpprogram $ head lifted
       
>              return convert) 
>               else (do 
>                 (Parse.Ok parsed) <- return $ head parsedRes1
>                 return parsed)
       
>          --------------------------------------------------------------------------------------------------
       
>          userCode     <-  return $ snd6 parsed
        
>          showTitle      ("User defined functions", 
>                          "Benutzerdefinierte Funktionen")                     vl 2 "parsed:ufs"
        
>          ufs          <- return $ fst6 parsed
>          _            <- compileStep fst4                    ufs
>                          ("Terminal parsers",
>                           "Terminale parser")                ppYsaT           vl 3 "parsed:ufs:terminals"
        
>          _            <- compileStep snd4                    ufs
>                          ("Filter","Filter")                 ppYsaF           vl 3 "parsed:ufs:filter"
        
>          _            <- compileStep thd4                    ufs
>                          ("Lookahead functions",
>                           "Lookahead Funktionen")            ppYsaL           vl 3 "parsed:ufs:la"
        
>          _            <- compileStep fth4                    ufs
>                          ("Direct parser definitions",
>                           "Direkte Parsedefinitionen")       ppYsaF           vl 3 "parsed:ufs:lc"
        
>          prods        <- compileStep  (snd.thd6)             parsed         
>                          ("Grammar","Grammatik")             pretty           vl 2 "parsed:grammar"
        
>         ----------- Transformation der Grammatik: ProdSource -> Prod -------------------------------------
        
>          showTitle       ("Grammar transformations","Grammatik-Transformationen")   
>                                                                               vl 1 "adptrans"
>          axiom        <- return $ (fst.thd6) parsed
>          putLine         ("Axiom = " ++ axiom)                                vl
        
>          nts          <- compileStep collectNTs              prods
>                          ("Nonterminal symbols", "Nichtterminale")
>                                                              id               vl 2 "parsed:grammar:nts"
        
>          prods        <- compileStep  (replTerms nts)        prods          
>                          ("Replace terminal symbols","Ersetze Terminalsymbole") 
>                                                              pretty           vl 2 "parsed:grammar:termrepl"
        
>          liftSep      <- return $ if (cm == optimizeCM) then '$' 
>                                                         else '_'
        
>          prods        <- compileStep2 (replCombs liftSep)    prods
>                          ("Insert user defined combinator definitions" ,
>                           "Einsetzen benutzerdefinierter Kombinatordefinionen")
>                                                              pretty           vl 2 "adptrans:replcombs"
        
>          -- ProdSource -> Prod
>          prods        <- compileStep (liftWheres liftSep)    prods      
>                          ("Flat inner definitions",
>                           "Auflösen verschachtelter Definitionen")
>                                                              pretty           vl 2 "adptrans:flatten"
        
>          prods        <- compileStep alterListCompr          prods
>                          ("Alter list comprehensions in grammar",
>                           "Umbauen der Listenbeschreibungen in der Grammatik")
>                                                              (pretty)   vl 2 "parsed:grammar:altered"

>          crfilter     <- compileStep findCRFilter            prods
>                          ("Find contains_region filter in grammar",
>                           "Finde contains_region filter in der Grammatik")
>                                                              id          vl 2 "parsed:grammar:crfilter"
        
>         ----------- Verarbeitung der Algebren ------------------------------------------------------------
        
>          choiceFunctions <- compileStep findChoiceProds       prods
>                            ("Identify choice functions",
>                             "Identifiziere Auswahlfunktionen") id            vl 2 "parsed:choicefunctions"
        
>          algebraFunctions <- compileStep findUsedAlgebraFunctions   prods
>                            ("Identify algebra functions",
>                             "Identifiziere Algebrafunktionen") id            vl 2 "parsed:algebrafunctions"
        
>          algtd        <- compileStep  sth6                   parsed
>                          ("Algebra type declaration", 
>                           "Algebra Typdeklaration")          ppAlgebraTypeDecl  vl 2 "parsed:algebratypedecl"
        
>          algfs        <- compileStep  fth6                   parsed            
>                          ("Algebras","Algebras")             ppAlgDefs        vl 2 "parsed:algebras"
        
>          (signatureAr, signature, optionalAutoEnum) <- let 
>            sigResult 
>              -- falls keine automatische Signatur-Generierung gewuenscht, nehmen wir die geparste
>              | csAlgName == "" = do 
        
>               signatureAr  <- compileStep  fith6                  parsed
>                              ("Signature", "Signatur")           ppSignatureArea  vl 2 "parsed:signaturearea"
        
>               signature    <- compileStep mergeSignature          signatureAr
>                              ("Merge signature and type definitions",
>                               "Zusammenführen von Signatur und Typdefinitionen")
>                                                                  ppSignature      vl 2 "parsed:signature"
>               return (signatureAr, signature, [])
        
>              -- ansonsten: automatische Signatur-Generierung
>              | otherwise = do
        
>               signature    <- return $ generateSignature algfs algtd csAlgName algebraFunctions
>               _            <- compileStep id                      [signature]
>                          ("Automatically generate signature from algebra " ++ csAlgName,
>                           "Automatische Signatur-Erzeugung") ppSignature          vl 2 "parsed:autosig"
        
>               optionalAutoEnum <- return $ generateEnumAlg algfs csAlgName choiceFunctions algebraFunctions
>               _           <- compileStep  id                      [optionalAutoEnum]
>                          ("Automatically generate enumeration algebra",
>                           "Automatisch erzeugte Enum-Algebra")    ppAlgDefs        vl 2 "parsed:autoenum"
        
>               return ([(signature,[])],[signature],[optionalAutoEnum])
>            in sigResult
        
>          algfs        <- compileStep2 (sortAlgebraDefs $ algOrder ++ ppAlgNames) (optionalAutoEnum ++ algfs)
>                          ("Filter algebra definitions with user defined algebra order",
>                           "Sortieren der Algebra-Definitionen nach der benutzerdefinierten Reihenfolge")
>                                                              ppAlgDefs        vl 2 "parsed:algebras:filtered"
        
>          algfsBT      <- compileStep2 (includeTypeDefs signatureAr algtd) algfs 
>                          ("Merge algebra definitions with type definitions",
>                           "Zusammenführen von Algebra- und Typ-Definitionen")
>                                                              ppAlgDefs        vl 2 "parsed:algebras:merged"
        
>          -- falls eine pp-Algebra angegeben wurde, wird diese unter ppAlg gespeichert. algfsBT enthaelt diese
>          -- nicht mehr, da wir sie fuer die Erstellung der Rekurrenzen nicht mehr benoetigen:
>          (ppAlg, algfsBT)     <- return $ extractppAlgs ppAlgNames algfsBT
        
>          enriched     <- compileStep enrichSpecialChoice     algfsBT
>                          ("Add context dependant function pointers to objective functions",
>                           "Erweitere Auswahlfunktionen um kontextabhängige Funktionszeiger")
>                                                              ppSpecialChoiceUses'  vl 2 "parsed:algebras:enriched"
        
>          (specialChoice1, algfsBT) <- return $ (\(s,a)->(concat s, a)) (unzip enriched)
        
>          specialChoice2 <- compileStep (makeSpecialChoice choiceFunctions) algfsBT
>                            ("Special functions to be generated", "")
>                                                               ppSpecialChoiceUse vl 2 "parsed:specials"
>          specialChoice <- return $ nub $ specialChoice1 ++ specialChoice2
        
>          (enumAlgName, algfs) <- return $ extractEnumAlg bt algfsBT
>          _            <- compileStep asl                      enumAlgName
>                          ("Name of enumeration algebra", "Enumerierungs-Algebra")
>                                                              show             vlBT 2 "parsed:algebras:enumAlgName"
        
>          -- falls backtrace-Option, enthaelt algfs an dieser Stelle die um die enumalg reduzierte Algebrenliste
>          -- algfsBT enthaelt die Originalliste. Andernfalls enthalten beide die Originalliste.
        
>          _            <- compileStep id                      algfs
>                          ("Algebra definitions without enumeration algebra",
>                           "Algebra-Definitionen ohne Enumerierungs-Algebra")
>                                                              ppAlgDefs        vlBT 2 "parsed:algebras:withoutenum"
        
>          crtEnum      <- return (length algfs   > 1)
>          crtEnumBT    <- return (length algfsBT > 1)

>          -------- typecheck -----------------------------------------------------------------------------
>          -- TODO Compilation bei Typfehler stoppen mit: putSMerr (err, line)
>          showTitle     ("Typecheck" ,"Typ-Check")                                                 vl 1 "typecheck"
>          let (conciseOutput, verboseOutput) = getTCresult (head parsedRes2)
>          when (typecheckFlag && vl>=trace) 
>              (putSM verboseOutput)
>          when typecheckFlag
>             $ putSM conciseOutput

>          -------- automatische Tabellierung --------------------------------------------------------------
>          prods <- let
>            tabulated
>             ------------- Tabulate ------------------------------------------------------------------------
>             | (cm == imperativeCM || cm == optimizeCM) && tc /= TCNone = do
        
>                 let transformed = prods
        
>                 showTitle     ("Table configuration analysis" ,"Tabellierungsanalyse")              vl 1 "tabana"
        
>                 ----------- track mode analysis ------------------------------------------------------------------
>                 tms          <- compileStep (tmaProds crfilter)     transformed
>                                 ("Track mode analysis", 
>                                  "Track-Modus-Analyse")             ppTModesElem          vl 1 "tma"
                
>                 trackMode    <- compileStep (asl.maxTrackMode)      tms
>                                 ("Maximal track mode",
>                                  "Maximaler Track-Modus")           pretty                vl 2 "tma:max"
                
>                 trackMode    <- return $ head trackMode
                
>                 ----------- yield size analysis ------------------------------------------------------------------
>                 _            <- compileStep  (asl.length)             transformed         
>                                 ("Number of productions",
>                                  "Anzahl Produktionen")               show             vl 1 "count"
                
>                 _            <- compileStepM  (reverse.ysaProds ufs tms) transformed      
>                                 ("Trace yield size analysis" ,
>                                  "Ablauf der Kranzlängenanalyse")     ppYSizes      vl 1 "ysizes:trace"
                
>                 ysizes       <- compileStep  (head.ysaProds ufs tms)  transformed      
>                                 ("Yield sizes",
>                                  "Ergebnis der Kranzlängenanalyse")   ppYSizesElem   vl 1 "ysizes"
        
>                 -------- Derive tabulation type (none/listed/tabulated) -----------------------------------
>                 il           <- compileStep  (drecProds ufs tms ysizes) transformed      
>                                 ("Derive subscripts",
>                                  "Indexermittlung")                 ppILProd         vl 1 "dss"
        
>                 usages      <- compileStep (analyseDeps2 axiom)       il
>                                 ("Usage analysis",
>                                  "Nichtterminal-Verwendungsanalyse")           ppAnalyseDeps   vl 1 "ira"
        
>                 autoRange     <- compileStep (analyseRange crfilter tms usages)  il
>                                 ("Index range analysis",
>                                  "Indexbereichsanalyse")           ppAnalysedRange   vl 1 "ira"
>                 -------------------------------------------------------------------------------------------         
        
>                 nameIds    <- compileStep createNameIds             transformed
>                               ("Create name IDs",
>                                "Erzeugung der Namens-IDs")          show                   vl 2 "tabana:nameIds"
        
>                 mnlength   <- return $ maxNameLength nameIds
        
>                 eas        <- compileStep (eaProds iuc ufs tms ysizes nameIds) transformed
>                               ("Generate efficiency terms", 
>                                "Generierung der Effizienzterme")   (ppEff1 nameIds mnlength)       
>                                                                                            vl 2 "tabana:effterms"
        
>                 _          <- compileStep id                        eas
>                               ("User annotated table configuration",
>                                "Vom Benutzer vorgegebene Tabellenkonfiguration")
>                                                                (ppEff1b nameIds mnlength)  vl 2 "tabana:userconf"
        
>                 expTerms   <- compileStep (exportEffterms vl tc True tadd taddc taddn nameIds (rangeToPoly autoRange)) eas
>                               ("Results of automatic table design", 
>                                "Ergebnisse des automatischen Tabledesigns")
>                                                                 show vl 2 "tabana:export"
        
>                 result     <- compileStep (tabProds autoRange (annotateEff eas expTerms) nameIds)  transformed
>                               ("Annotate grammar with calculated tabulation information",
>                                "Annotiere Grammatik mit den berechneten Tabellierungen")
>                                                              pretty                        vl 2 "tabana:result"
        
>                 return result
        
>             | otherwise = return prods
>           in tabulated
        
>          --------------------------------------------------------------------------------------------------
        
>          (recur, transformed, algfs, algfsBT) <- let 
>           transformed
        
>             --------------- für Optimierung und Tabellierung keine Transformation notwendig  --------------
>             | cm == optimizeCM || 
>               cm == tabulateCM || cm == tabulateCMextern  = return ([],prods,algfs,algfsBT)
>             --------------- Grammatik-Transformationen für Compilierung nach C und Latex ------------------
>             | cm == imperativeCM || cm == latexCM = do
        
                reachable    <- compileStep  (reachabilityAnalysis axiom)     prods   
                                ("Identify reachable productions",
                                 "Identifiziere erreichbare Produktionen") id        vl 2 "adptrans:reachability"
        
                 ----------------------------------
                 for functional metapost -->    drawtree prods
                 ----------------------------------
        
>                dups         <- compileStep  (getDuplicates axiom)  prods   
>                                ("Duplicated productions",
>                                 "Doppelte Produktionen")           ppDups           vl 2 "adptrans:dups"
        
>                recur        <- compileStep  (recurProds axiom dups)  prods   
>                                ("Identify recursive productions",
>                                 "Identifiziere rekursive Produktionen") id          vl 2 "adptrans:recur"
       
>                merged       <- compileStep2 (mergeProds ufs dups recur) prods   
>                                ("Merge nontabulated productions",
>                                 "Zusammenführen nichttabellierter Produktionen")  
>                                                                    (pretty)  vl 2 "adptrans:merged"
        
>                merged       <- compileStep2 (getUsedProds axiom)   merged
>                                ("Eliminate unused productions",
>                                 "Löschen nicht verwendeter Produktionen")
>                                                                    (pretty)  vl 2 "adptrans:elim"
        
>                --(algfsBT, merged) <- return $ mergeAppls algfsBT merged
>                -- fieser Hack;
>                --algfs <- return algfsBT
        
        
>                _               <- compileStep id    merged
>                                 ("Merge algebra function applications",
>                                 "Zusammenführen von Algebrafunktionsanwendungen")
>                                                                    (pretty)  vl 2 "adptrans:elim"
        
>                _              <- compileStep id    algfsBT
>                                ("New algebra functions",
>                                 "Neue Algebrafunctionen")
>                                                                    (ppAlgDefs)  vl 2 "adptrans:elim"
        
        
                addedID      <- compileStep2 addidProds             merged
                                ("Add id application to single nonterminal symbols",
                                 "Erweitere einzeln auftretende Nichtterminale um id-Anwendung")  
                                                                    (pretty)  vl 2 "adptrans:addID"
        
        
>                return (recur, merged, algfs, algfsBT)
>           in transformed
        
>          ----------- track mode analysis ------------------------------------------------------------------
>          tms          <- compileStep (tmaProds crfilter)     transformed
>                          ("Track mode analysis", 
>                           "Track-Modus-Analyse")             ppTModesElem          vl 1 "tma"
        
>          trackMode    <- compileStep (asl.maxTrackMode)      tms
>                          ("Maximal track mode",
>                           "Maximaler Track-Modus")           pretty                vl 2 "tma:max"
        
>          trackMode    <- return $ head trackMode
        
>          ----------- yield size analysis ------------------------------------------------------------------
>          _            <- compileStep  (asl.length)            transformed         
>                          ("Number of productions",
>                           "Anzahl Produktionen")               show             vl 1 "count"
        
>          _            <- compileStepM  (reverse.ysaProds ufs tms) transformed      
>                          ("Trace yield size analysis" ,
>                           "Ablauf der Kranzlängenanalyse")     ppYSizes         vl 1 "ysizes:trace"
        
>          ysizes       <- compileStep  (head.ysaProds ufs tms)  transformed      
>                          ("Yield sizes",
>                           "Ergebnis der Kranzlängenanalyse")   ppYSizesElem     vl 1 "ysizes"
        
>          -------------------------------------------------------------------------------------------------- 
>          ---------- Aufteilung der Verarbeitungswege ------------------------------------------------------ 
>          -------------------------------------------------------------------------------------------------- 
>          _ <- let 
>           action 
>             ------------- Optimize ------------------------------------------------------------------------
>             | cm == optimizeCM = do
        
>                 trPhase1     <- compileStep  (trNext ufs ysizes)      transformed
>                                ("Transform to yield size enriched combinators",
>                                 "Transformation in Kranzlängenoptimierte Kombinatoren")
>                                                                      (pretty)  vl 1 "trphase1"
        
>                 -- Prod -> ProdSource
>                 trPhase2     <- compileStep trNextVar                 trPhase1
>                                ("Insert concrete combinators",
>                                 "Einsetzen konkreter Kombinatoren")  (pretty)  vl 1 "trphase2"
        
>                 unlifted     <- compileStep2 (unliftWheres '$')       trPhase2
>                                ("Unlift inner definitions",
>                                 "Wiederherstellen verschachtelter Definitionen")
>                                                                   (pretty)   vl 2 "adptrans:unlifted"
        
>                 trPhase2'    <- return $ unlines (map (pretty' False) unlifted)
        
>                 _ <- let 
>                  action
>                    ---------- normaler Verarbeitungsmodus -------------------------------------------------
>                    ---------- Originaldatei wieder zusammenbauen ------------------------------------------
>                    | not cgi = do
        
>                      (_,_,(header, _, trailer)) <- return $ extractArea (lines inp) "grammar{" "}"
>                      tcode <- return $ asl((unlines header) ++ 
>                                            trPhase2' ++
>                                            (unlines trailer))
        
>                      _            <- compileStep id                      tcode
>                                      ("Target code","Zielcode")          id               vl 1 "target"
        
>                      targetOnly vl $ putSM (head tcode)
        
>                    ---------- CGI Modus -------------------------------------------------------------------
>                    | cgi =  targetOnly vl $ putSM trPhase2' 
>                  in action
>                 return ()
        
>             ------------- Tabulate ------------------------------------------------------------------------
>             | cm == tabulateCMextern = do
        
>                 showTitle     ("Table configuration analysis" ,"Tabellierungsanalyse")              vl 1 "tabana"
        
>                 -------- Derive tabulation type (none/listed/tabulated) -----------------------------------
>                 il           <- compileStep  (drecProds ufs tms ysizes) transformed
>                                 ("Derive subscripts",
>                                  "Indexermittlung")                 ppILProd         vl 1 "dss"
        
>                 usages      <- compileStep (analyseDeps2 axiom)       il
>                                 ("Usage analysis",
>                                  "Nichtterminal-Verwendungsanalyse")           ppAnalyseDeps   vl 1 "ira"
        
>                 autoRange     <- compileStep (analyseRange crfilter tms usages)  il
>                                 ("Index range analysis",
>                                  "Indexbereichsanalyse")           ppAnalysedRange   vl 1 "ira"
>                 -------------------------------------------------------------------------------------------         
        
>                 nameIds    <- compileStep createNameIds             transformed
>                               ("Create name IDs",
>                                "Erzeugung der Namens-IDs")          show                   vl 2 "tabana:nameIds"
        
>                 mnlength   <- return $ maxNameLength nameIds
        
>                 eas        <- compileStep (eaProds iuc ufs tms ysizes nameIds) transformed
>                               ("Generate efficiency terms", 
>                                "Generierung der Effizienzterme")   (ppEff1 nameIds mnlength)       
>                                                                                            vl 2 "tabana:effterms"
        
>                 _          <- compileStep id                        eas
>                               ("User annotated table configuration",
>                                "Vom Benutzer vorgegebene Tabellenkonfiguration")
>                                                                (ppEff1b nameIds mnlength)  vl 2 "tabana:userconf"
        
>                 expTerms   <- compileStep (exportEffterms vl tc False tadd taddc taddn nameIds (rangeToPoly autoRange)) eas
>                               ("Results of automatic table design", 
>                                "Ergebnisse des automatischen Tabledesigns")
>                                                                 show                       vl 2 "tabana:export"
>       
        
>                 noOut
        
>             | cm == tabulateCM = do
        
>                 showTitle     ("Table configuration analysis" ,"Tabellierungsanalyse")              vl 1 "tabana"
        
>                 nameIds    <- compileStep createNameIds             transformed
>                               ("Create name IDs",
>                                "Erzeugung der Namens-IDs")          show                   vl 2 "tabana:nameIds"
        
>                 mnlength   <- return $ maxNameLength nameIds
        
>                 eas        <- compileStep (eaProds iuc ufs tms ysizes nameIds) transformed
>                               ("Generate efficiency terms", 
>                                "Generierung der Effizienzterme")   (ppEff1 nameIds mnlength)       
>                                                                                            vl 2 "tabana:effterms"
        
>                 _          <- compileStep id                        eas
>                               ("User annotated table configuration",
>                                "Vom Benutzer vorgegebene Tabellenkonfiguration")
>                                                                (ppEff1b nameIds mnlength)  vl 2 "tabana:userconf"
        
>                 tablist   <- compileStep (deriveTableNeeded tc)     eas
>                               ("Derive necessary nonterminals",
>                                "Ermittle notwendige Nichtterminale")
>                                                                    (ppEff1c nameIds)       vl 2 "tabana:tablesneeded"
        
>                 nontablist <- compileStep (deriveCycleFree tc tablist)  eas
>                               ("Derive unnecessary nonterminals",
>                                "Ermittle nicht zu tabellierende Nichtterminale")
>                                                                    (ppEff1c nameIds)       vl 2 "tabana:cyclefree"
        
>                 tablist    <- return (if sp then [] else tablist)
>                 nontablist <- return (if sp then [] else nontablist)
        
>                 if tp then (  
>                   targetOnly vl $ putSM $ "necessary:\n-----------\n" ++ mapsep "\n" (ppEff1c nameIds) tablist ++ 
>                                           "\nunnecessary:\n------------\n" ++ mapsep "\n" (ppEff1c nameIds) nontablist )
>                   else ( do 
        
>                   alleas     <- compileStep   (anaEffAll tablist nontablist)  eas
>                               ("Calculate efficiencies for all possible configurations", 
>                                "Berechnung der Laufzeiten für alle möglichen Konfigurationen")
>                                                                    (ppEffs2 nameIds mnlength)      
>                                                                                            vl 2 "tabana:alltabs"
        
>                   besteas    <- compileStep   (bestEff tc)           alleas
>                               ("Choose best configuration", "Auswahl der besten Konfiguration")
>                                                                    (ppEff2 nameIds mnlength)       
>                                                                                            vl 2 "tabana:best"
        
>                   tprods     <- compileStep   (tabProds [] besteas nameIds) transformed
>                               ("Tabulate Grammar", "Tabellierung in Grammatik eintragen")
>                                                                    (pretty' False)
>                                                                                            vl 2 "tabana:grammar"
        
>                   tgraph     <- compileStepM  id                     alleas
>                               ("Generate dependency graph", "")
>                                                                   (ppGraph4 nameIds)        vl 2 "tabana:graph"
        
>                   _          <- compileStepM  id                     alleas
>                               ("Detailed efficiency information", "")
>                                                                   (ppAllEffs nameIds mnlength) vl 2 "tabana:detailed"
        
>                   targetOnly vl $ putSM (ppEffs2 nameIds mnlength besteas)
>                   )
        
>             ------------- Latex or Imperative -------------------------------------------------------------
>             | cm == imperativeCM || cm == latexCM = do
        
>                 il           <- compileStep  (drecProds ufs tms ysizes) transformed      
>                                 ("Derive subscripts",
>                                  "Indexermittlung")                 ppILProd         vl 1 "dss"
        
>                 -----------------------
>                 _            <- compileStepM  id                     il
>                                ("Internal structure",
>                                 "Interne Struktur")                 ppILP            vl 2 "dss:internal"
>                 ----------------------
        
        
>                 ------------- Abhängigkeitsanalyse --------------------------------------------------------
        
>                 showTitle       ("Dependency analysis" ,"Abhängigkeitsanalyse")         vl 1 "depana"
        
>                 deptab      <- return $ tabulateDeps ufs tms ysizes transformed
>                 _           <- compileStep id                       [deptab]
>                                ("Dependencies" ,"Abhängigkeiten")   ppDepTab            vl 2 "depana:deps"
        
>                 _           <- compileStep id                       [deptab]
>                                 ("Output to latex-table", "Ausgabe als Tabelle")    
>                                                              (ppDepTabTex transformed)  vl 2 "depana:table"
        
>                 depSorted   <- compileStep  (sortDeps transformed)  deptab    
>                                 ("Sorted dependencies","Sortierte Abhängigkeiten")   
>                                                                     id                  vl 2 "depana:sorted"
        
>                 usages      <- compileStep (analyseDeps axiom)       il
>                                 ("Usage analysis",
>                                  "Nichtterminal-Verwendungsanalyse")           ppAnalyseDeps   vl 1 "ira"
        
>                 _ <- let 
>                  action 
        
>                     ---------- Latex - Rekurrenzen: -------------------------------------------------------
>                     | cm == latexCM = do
        
>                     _           <- compileStep id                       [il]
>                                    ("Output to latex-recurrence",
>                                     "Latex-Rekurrenzen")   (ppILProdstex ufs depSorted) vl 1 "iltex"
        
>                     latexrecs  <- return $ ppILProdstex ufs depSorted il
>                     targetOnly vl $ putSM latexrecs  
        
>                     ---------- imperative Codeerzeugung ---------------------------------------------------
>                     | cm == imperativeCM = do
        
>                        targetcode <- let 
>                         targetcode
        
>                          --------------- Codeerzeugung ILProd -> TL: --------------------------------------
>                          | cg == CGb = do -- old stuff - default is cg == CGv XXX delete this?
        
>                          frame        <- return $ getFrame userCode trackMode tl BTNone []
>                          targetcode   <- compileStep  (codegen1 ufs userCode frame algfs depSorted axiom)  il          
>                                       ("Target code in " ++ pptln tl ,
>                                        "Zielcode in " ++ pptln tl)               (prettyLang tl)   vl 1 "targetcode"
>                          return targetcode
        
>                          ------------ Codeerzeugung ILProd -> IL2 -> TL: ----------------------------------
>                          | otherwise = do -- default case
>            
>                          -- Standard-Compilierungs-Mode: algfs enhaelt die Algebra-Funktionen entsprechend der backtrace-Option
>                          -- falls backtrace aktiv, entfaellt an dieser Stelle die Enumeration-Algebra
        
>                          -- folgende Datenstrukturen werden zweimal mitgefuehrt:
>                          -- il2, il2types, opts, algfs, crtEnum
        
>                          opts        <- return $ CompileOptions (cg, BTNone, soa, arr,  lcf, nc, tas, ita, ifct, toptO, toptO2, fst3 bea, gc, off, window)

>                          -- lcs <- return $ il2lcProds algfs il
>                          -- pplcs <- return $ map ppLCprod lcs
>                          -- putSM $ unlines pplcs

>                          -- ListCompr conversion --------------------------------------------------

                          lcs         <- compileStep (sToLCProds algfs) il
                                          ("Generate LC expressions","")                               ppLCprod      vl 1 "il:lc"

                          rts         <- compileStep deriveResultTypes lcs
                                          ("Derive result types from LC expressions", "")              ppResType     vl 1 "il:rts"

                          lcs2        <- compileStep (eliminateLists rts) lcs
                                          ("Eliminate lists in LC expressions", "")                    ppLCprod      vl 1 "il:lcs2"

                          _           <- compileStep asl lcs2
                                          ("Output LaTeX recurrences","")                              ppLCprodLatex vl 1 "il:latex"

                          tcode       <- compileStep (lccodegen recur rts) lcs2
                                          ("Compile LC expressions to target code", "")             (prettyLang tl)  vl 1 "targetcode"

>                          --------------------------------------------------------------------------

>                          il2         <- compileStep (ph1IL2Prods ufs recur algfs opts crtEnum) il
>                                        ("Insert algebra and terminal parser definitions - phase 1",
>                                         "Einsetzen der Algebra- und Parser Definitionen - phase 1")  ppIL2Prod      vl 1 "il2:ph1"
        
>                          il2typesR    <- return $ deriveIL2Types opts algfs il2
>                          il2types     <- return $ snd il2typesR
>                          inStr        <- return $ fst il2typesR 
        
>                          _            <- compileStep id                                    il2types
>                                         ("Derive table result types",
>                                          "Ermittle Tabellen-Ergebnistypen")                ppIL2Type      vl 2 "il2:types"
        
>                          _            <- compileStepM id                                   [inStr]
>                                         ("Derive table result types - details",
>                                          "Ermittle Tabellen-Ergebnistypen - Details")      id             vl 2 "il2:types"
        
>                          il2          <- compileStep (ph2IL2Prods opts recur il2types crtEnum)  il2
>                                        ("Insert algebra and terminal parser definitions - phase 2",
>                                         "Einsetzen der Algebra- und Parser Definitionen - phase 2")  ppIL2Prod      vl 1 "il2:ph2"
        
>                          -- backtrace-Compilierungs-Mode: hier wird mit dem urspruenglichen Algebra-Satz gearbeitet:
>                          showTitle   ("Repeat analyses for backtracing" , "Analyse für Backtracing") 
>                                                                                                                     vlBT 1 "backtrace"
        
>                          optsBT      <- return $ CompileOptions (cg, bt, soa, arr, lcf, nc, tas, ita, ifct, toptO, toptO2, fst3 bea, gc, off, window)
>                          il2BT       <- compileStep (ph1IL2Prods ufs recur algfsBT optsBT crtEnumBT) il
>                                        ("Insert algebra and terminal parser definitions - phase 1",
>                                         "Einsetzen der Algebra- und Parser Definitionen - phase 1")  ppIL2Prod      vlBT 1 "backtrace:il2"
        
>                          il2typesR    <- return $ deriveIL2Types optsBT algfsBT il2BT
>                          il2typesBT   <- return $ snd il2typesR
>                          inStr        <- return $ fst il2typesR 
        
>                          _            <- compileStep id                                    il2typesBT
>                                         ("Derive table result types",
>                                          "Ermittle Tabellen-Ergebnistypen")                ppIL2Type      vlBT 2 "backtrace:il2:types"
        
>                          _            <- compileStepM id                                   [inStr]
>                                         ("Derive table result types - details",
>                                          "Ermittle Tabellen-Ergebnistypen - Details")      id             vlBT 2 "backtrace:il2:types"
        
>                          -- an dieser Stelle nehmen wir il2types (und nicht BT), weil die Nichtterminale, auf die innerhalb des
>                          -- backtrace-Codes zugegriffen wird, ja diejenigen aus der forward-Berechnung entsprechen:
>                          il2BT        <- compileStep (ph2IL2Prods optsBT recur il2types crtEnumBT)  il2BT
>                                        ("Insert algebra and terminal parser definitions - phase 2",
>                                         "Einsetzen der Algebra- und Parser Definitionen - phase 2")  ppIL2Prod   vlBT 1 "backtrace:il2"
        
--      ---------------------------------------------------------------------------------------------------------------------------------
        
        
>            --              _           <- compileStep id                                    [il2]
>            --                            ("Output to latex-recurrence", "Latex-Rekurrenzen")  
>            --                                                                               ppIL2Prodstex  vl 2 "il2:tex"
        
>                          frame        <- return $ getFrame userCode trackMode tl bt specialChoice
>                          ranges       <- return $ prodToTRanges prods
>                          cgStuff      <- return $ (tl, f, cmdl, cInfo, userCode, frame, trackMode, tms, ysizes, ranges, 
>                                                    recur, depSorted, axiom, optsBT, crtEnum, crtEnumBT, il2types, il2typesBT, 
>                                                    enumAlgName, algfs, algfsBT, specialChoice, ppAlg, signature, crfilter)
        
>                          targetcode   <- compileStep (codegen cgStuff) (il2, il2BT)
>                                         ("Target code in " ++ pptln tl,
>                                          "Zielcode in " ++ pptln tl)                        (prettyLang tl)  vl 1 "targetcode"
        
                          targetcode <- return [TLNil]
        
>                          return targetcode
>                         in targetcode
 
bea is as default (_, false, _) -> beautify does nothing

>                        targetOnly vl $ putSM $ (beautify bea) $  mapsep "\n" (prettyLang tl) (tlFilter targetcode)
        
                        targetOnly vl $ sequence_ (map (putSM . (prettyLang tl)) targetcode)
        
>                  in action
>                 return ()
>           in action
>          return ()
        
>          )  -- else Fall parse error

>  where

>   testStop label = if label == stopL then putSMError label else noOut

>   ----------------------------------------------------------------------------------------------------
>   -- Ausgabe-Funktionen
>   ----------------------------------------------------------------------------------------------------

>   specialtracelabels = ["begin","parsed:grammar","adptrans:merged","ysizes:trace","ysizes","dss","depana","depana:deps","depana:sorted","il2:ph2","targetcode"]

>   convertVL vl label | vl == specialtrace && elem label specialtracelabels = trace
>                      | vl == specialtrace                                  = target
>                      | otherwise                                           = vl

>   showTitle :: (String, b) -> Integer -> Int -> String -> SM ()
>   showTitle s vl_in chapterLevel label
>      | vl == target     = noOut
>      | vl == latextrace = do
>                           putSM 
>                             ("\n\\" 
>                              ++ section ++ "{" ++ eod s ++ "}\n" ++ labelStr ++ "\n")
>                           testStop label
>      | otherwise        = do
>                           putSM ("\n" ++ eod s ++ "\n" ++ replicate ((length (eod s)) + 2) '-' ++ "\n")
>                           testStop label
>      where 
>        vl = convertVL vl_in label
>        section = if chapterLevel <= 2 then concat (replicate chapterLevel "sub") ++ "section" else "paragraph"
>        labelStr = "\\label{trace:" ++ label ++ "}"

>   compileStep :: (Show a) => (x -> [a])
>                           -> x
>                           -> (String, b)
>                           -> (a -> String)
>                           -> Integer
>                           -> Int
>                           -> String
>                           -> SM [a]
>   compileStep compilefct x title pp vl_in chapterLevel label = do
>      let e  = compilefct x
>      let vl = convertVL vl_in label

>      if (length e == 0) then noOut 
>        else (do
>          if vl >= trace then showTitle title vl chapterLevel label                  else noOut
>          if vl >= trace then putSM 
>                         (unlines (map ((++) lit) ((((concatMap (lines . ((++) "\n") . pp) e)))))) 
>                           else noOut
>          if vl >= debug then showTitle ("===> internal structure:", 
>                                         "===> interne Struktur:") vl chapterLevel label  else noOut
>          if vl >= debug then sequence_ (map (putSM . (flip (++) "\n") . show) e) else noOut
>        )
>      return e    where
>                    lit = case vl of
>                            2         -> "> "
>                            otherwise -> ""

>   compileStepM compilefct x title pp vl chapterLevel label = compileStep compilefct x title pp vl' chapterLevel label 
>     where
>       vl' | vl >= tracemore = vl
>           | otherwise       = target

>   compileStep2 :: (Eq a, Show a) =>
>                   ([a] -> [a])
>                   -> [a]
>                   -> (String, b)
>                   -> (a -> String)
>                   -> Integer
>                   -> Int
>                   -> String
>                   -> SM [a]
>   compileStep2 compilefct x title pp vl_in chapterLevel label = do
>      let e  = compilefct x
>      let vl = convertVL vl_in label

>      if vl >= trace then showTitle title vl chapterLevel label  else noOut

>      if e == x then if vl >= trace then putSM (eod ("no changes\n","keine Änderungen")) else noOut
>         else (do 
>           if vl >= trace then putSM 
>                          (unlines (map ((++) lit) ((((concatMap (lines . ((++) "\n") . pp) e)))))) 
>                          else noOut
>           if vl >= debug then showTitle ("===> internal structure:", 
>                                          "===> interne Struktur:") vl chapterLevel label  else noOut
>           if vl >= debug then sequence_ (map (putSM . (flip (++) "\n") . show) e) else noOut
>         )
>      return e    where
>                    lit = case vl of
>                            2         ->   "> "
>                            otherwise ->   ""




> specialLine = "\n----------------------------------------------------------------------------------\n"

> asl a = [a]


