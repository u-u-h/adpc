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


=============================================================================================================================
TODO
=============================================================================================================================

- Ausgabebereich bei stdlib auch Teil der library, hier dann
  verschiedene Ausgabemodi:
  - simple
  - Haskell-like
  - komplette Tabelle
  - RNA-style
  - etc. 
  => einfacher zu pflegen als ueber den ADPC
  => in adplib.c (hier ja auch schon fasta-input drin)

V energy als float
V -c und -e richtig verarbeiten
V makefile template entsprechend lib modifizieren

V current:
  bloecke wie "output_optimal"
  moeglichst mit
  Zeilenumburechen. Momentan ist das
  XML und daraus folgend auch das
  C-File ziemlich unersichtlich.

- Namespace:
  Fuer alle Files geltende Variablen heissen PROJECT_
    PROJECT_NAME
    PROJECT_VERSION
    PROJECT_DATE
    PROJECT_AUTHOR
    PROJECT_COPYRIGHT
    PROJECT_LICENCE

V Konflikte aufloesen, falls algebren mit dem gleichen buchstaben anfangen 
- XML-Kommentare, wie?

- Makefile in Phase II aus XML bauen

- momentan muss interafce-name gleich dateiname sein
- neues projekt wird in verzeichnis erstellt
  - verzeichnisname=filename
  - rest: interface-name
- interactive-mode optional

   strings fuer interactive mode dann in interactive area
   <interface name="...>
   <interactive>
     <commandline="...">
     <intmessage="..."> etc
   <interactive>
   ==> anderes C-modul dazupacken

- colored-output optional, gesondertes element wie file


LHS
================================================================================
- refactorings


C-Lib
================================================================================

- segfault bei
  -f *.txt  ++
  -h   ===> MAXINPUT ueberlauf
- mal mit -Wall testen

- Steuerung komplett ueber opt-Stuktur:
  - window 
  - colored-output
  - outputmode (-O) 
  ===> hier fixe variablennamen notwendig

XML
================================================================================
- automatische %-Eintraege bei default set/info
- // wieder ersetzen
- dtd: switch default: bool-wert
- <man> als simple-html z.B. nur <H1> <B> <I> etc
  -> man direct erzeugen
- property "mandatory"


Generator
================================================================================
- (-X) in info-page
- colored_output ohne extra-code
V <module> tag mit automatischer integration des adpc
  - <module name="so" adpc="-al mfe -cs ...">
- library system:
  - <interface name="pknotsRG" lib="rna">
  - automatische main-erzeugung 
  - kopieren der entsprechenden files

Backends
================================================================================
- JavaGUI: 
  moeglicher Ansatz auch:
    Erzeugung von XML fuer XML-GUI-Systeme. z.B.
       Adpc.xml -> XMLTalk.xml -> Java-GUI
    Motivation: Adpc-XML ist einfacher als XMLTalk
    XMLTalk als Ausgangsbasis waere deutlich zu maechtig fuer
    Adpc.

- html-forms
- webservice WSDL
- mal XML-GUI Ansaetze sichten, XUL, XAML, etc.
  siehe XML_GUI.pdf


================================================================================
================================================================================


> module Main where

> import Data.Char
> import Data.List

> import System.IO(IO, writeFile, readFile, putStrLn)
> import System.Environment(getEnv, getArgs, getProgName)
> import Control.Exception
> import System.IO.Error
> import System.Time(toCalendarTime, getClockTime, calendarTimeToString)

> import Text.XML.HaXml.XmlContent(fReadXml)
> import Text.XML.HaXml.OneOfN(OneOf9(..))
> import Text.XML.HaXml.Wrappers (fix2Args)

> import InterfaceDTD

> import Sed(sedString, systemReport, makeSedSubst, VarReplace)

> import ConfigOutput(prefix)

> import System.Posix.User



=============================================================================================================================
Wrapper functions for InterfaceDTD
=============================================================================================================================

Options
================================================================================

> getHelp (Option _ opt _ _)= case opt of
>                                OneOf9 s -> s
>                                x         -> pattErr "getHelp" x
> getLonghelp (Option _ opt _ _)= case opt of
>                                TwoOf9 s -> s
>                                x         -> pattErr "getLonghelp" x
> getVersion (Option _ opt _ _)= case opt of
>                                ThreeOf9 s -> s
>                                x         -> pattErr "getVersion" x
> getSwitch (Option _ opt _ _)= case opt of
>                                FourOf9 s -> s
>                                x         -> pattErr "getSwitch" x
> getSet (Option _ opt _ _)= case opt of
>                                FiveOf9 s -> s
>                                x         -> pattErr "getSet" x
> getSwitchset (Option _ opt _ _)= case opt of
>                                SixOf9 s -> s
>                                x         -> pattErr "getSwitchSet" x
> getFile (Option _ opt _ _)= case opt of
>                                SevenOf9 s -> s
>                                x         -> pattErr "getFile" x
> getDirect (Option _ opt _ _)= case opt of
>                                EightOf9 s -> s
>                                x         -> pattErr "getDirect" x
> getComment (Option _ opt _ _)= case opt of
>                                NineOf9 s -> s
>                                x         -> pattErr "getComment" x

> isHelp (Option _ opt _ _)= case opt of
>                                OneOf9 s -> True
>                                x         -> False
> isLonghelp (Option _ opt _ _)= case opt of
>                                TwoOf9 s -> True
>                                x         -> False
> isVersion (Option _ opt _ _)= case opt of
>                                ThreeOf9 s -> True
>                                x         -> False
> isSwitch (Option _ opt _ _)= case opt of
>                                FourOf9 s -> True
>                                x         -> False
> isSet (Option _ opt _ _)= case opt of
>                                FiveOf9 s -> True
>                                x         -> False
> isSwitchset (Option _ opt _ _)= case opt of
>                                SixOf9 s -> True
>                                x         -> False
> isFile (Option _ opt _ _)= case opt of
>                                SevenOf9 s -> True
>                                x         -> False
> isDirect (Option _ opt _ _)= case opt of
>                                EightOf9 s -> True
>                                x         -> False
> isComment (Option _ opt _ _)= case opt of
>                                NineOf9 s -> True
>                                x         -> False


Interface
================================================================================

> getInterfaceOptions (Interface _ _ _ _ _ opts) = opts
> getInterfaceModules (Interface _ _ _ _ modules _) = modules

> getInterfaceName        (Interface attr _ _ _ _ _)   = interfaceName   attr
> getInterfaceAuthor      (Interface attr _ _ _ _ _)  = interfaceAuthor attr 
> getInterfaceVersion     (Interface attr _ _ _ _ _) = interfaceVersion attr 
> getInterfaceVersioninfo (Interface _ (Versioninfo ver) _ _ _ _) = lines ver     

> getInteractiveWelcome     (Interface _ _ (Messages (Interactive_welcome mes)     _ _) _ _ _) =
>                           replaceHash $ replaceNl mes
> getInteractiveCommandline (Interface _ _ (Messages _ (Interactive_commandline mes) _) _ _ _) = 
>                           replaceHash $ replaceNl $ mes 
> getInteractiveHelpHeader  (Interface _ _ (Messages _ _ (Interactive_help_header mes)) _ _ _) = 
>                           replaceHash $ replaceNl $ removeLastNls $ mes 
> getManualSynopsis         (Interface _ _ _ (Just (Manual (Just (Synopsis text)) _ _))    _ _) = 
>                           replaceHash $ removeLastNls $ text 
> getManualSynopsis         _  = ""

> getManualIntroduction     (Interface _ _ _ (Just (Manual _ (Just (Introduction text)) _))    _ _) = 
>                           replaceHash $ removeLastNls $ text
> getManualIntroduction     _  = ""

> getManualExamples         (Interface _ _ _ (Just (Manual _ _ (Just (Examples text))))    _ _) = 
>                           replaceHash $ removeLastNls $ text
> getManualExamples         _  = ""

Module
================================================================================

> getModuleName :: Module -> String
> getModuleName                 = moduleName             
> getModuleSource               = moduleSource           
> getModuleAdpc                 = moduleAdpc             
> getModuleCondition            = moduleCondition        
> getModuleOutput_optimal       = moduleOutput_optimal   
> getModuleOutput_subopt_start  = moduleOutput_subopt_start
> getModuleOutput_subopt        = moduleOutput_subopt
> getModuleOutput_subopt_end    = moduleOutput_subopt_end
> getModuleMain_name            = moduleMain_name        


Options
================================================================================

> getOptionSwitch (Option attr _ _ _)  = optionSwitch attr

> getOptionArgtext (Option attr _ _ _) = case optionArgtext attr of
>                                                  Just s  -> "<" ++ s ++ ">"
>                                                  Nothing -> ""
> getOptionText    (Option attr _ _ _) = replaceHash $ optionText attr

> getOptionCode    (Option _ _ code _) = case code of
>                                         Just (Code c)  -> lines $ replaceHash c
>                                         Nothing        -> []          
> getOptionMan     (Option _ _ _ man) = case man of
>                                         Just (Man c)   -> lines c ++ [""]
>                                         Nothing        -> ["no help entry available",""]          

> getOptionSettext  opt | isSet       opt = replaceHash $ setSettext       (getSet       opt)
>                       | isSwitch    opt = replaceHash $ switchSettext    (getSwitch    opt)
>                       | isSwitchset opt = replaceHash $ switchsetSettext (getSwitchset opt)
>                       | otherwise       = error "getOptionSettext" 
> getOptionInfotext opt | isSet       opt = replaceHash $ setInfotext (getSet opt)
>                       | isSwitch    opt = replaceHash $ switchInfotext (getSwitch opt)
>                       | isSwitchset opt = replaceHash $ switchsetInfotext (getSwitchset opt)
>                       | otherwise       = error "getOptionInfotext" 

> getOptionDisabletext opt = replaceHash $ switchsetDisabletext (getSwitchset opt) 

> getOptionVar opt 
>                  | isSet       opt = setVar (getSet opt)
>                  | isSwitch    opt = switchVar (getSwitch opt)
>                  | isSwitchset opt = switchsetVar (getSwitchset opt)
>                  | isFile      opt = fileVar (getFile opt) 
>                  | otherwise       = error "getOptionVar" 


> getOptionSwitchDefault opt 
>                            | isSwitch opt = case (switchDefault (getSwitch opt)) of
>                                                  Switch_default_on -> "1"
>                                                  _                 -> "0"
>                            | isSwitchset opt = case (switchsetDefault (getSwitchset opt)) of
>                                                  Switchset_default_on -> "1"
>                                                  _                    -> "0"

> getOptionDefaultValue :: Option -> String
> getOptionDefaultValue opt | isSet opt       = setDefault (getSet opt)
>                           | isSwitchset opt = switchsetDefaultval (getSwitchset opt)

Specials
================================================================================

> getSwitchsetVar opt = switchsetSwvar (getSwitchset opt) 

> getInputfileVar :: [Option] -> String
> getInputfileVar opts = case (concatMap get opts) of 
>                               [] -> "inputfile"
>                               xs -> head xs
>   where
>     get opt | isFile opt = [getOptionVar opt]
>             | otherwise  = []


> convertDatatype Switchset_datatype_int    = Set_datatype_int  
> convertDatatype Switchset_datatype_float  = Set_datatype_float
> convertDatatype Switchset_datatype_string = Set_datatype_string


=============================================================================================================================
Phase I: create xml and makefile
=============================================================================================================================

Generate a make target for every module
================================================================================

> genAdpcTargets :: LIB -> String -> [String] -> String -> String
> genAdpcTargets lib project algebraList ppAlg = concatMap gen (algebraList \\ [ppAlg])
>   where
>     gen algebra = let comment = "#targets for algebra " ++ algebra ++ "\n"
>                       targetName = project ++ "_" ++ algebra ++ ".c"
>                       objectName = project ++ "_" ++ algebra ++ ".o"
>                       dependsOn  = project ++ ".lhs " ++ 
>                                    project ++ ".xml"  -- xml vielleicht auch nicht
>                       windowFlag = case lib of
>                                       LIBSTD -> ""
>                                       LIBRNA -> " -W "
>                       standardFlags = " -O -lcf -ta bt -bt so -gc cc "
>                       taboptFlags   = " -iuc -cto -tadd 3 -taddc 30 "
>                       sed      = "sed -f " ++ project ++ "_" ++ algebra ++ ".sed "
>                       aout = project ++ "_" ++ algebra ++ ".adpc"
>                       adpccall = "$(ADPCOMPILE) -c " ++ "$< " ++ "-al " ++ algebra ++ 
>                                  " enum -cs " ++ algebra ++ 
>                                  " -alpp " ++ ppAlg ++ standardFlags ++ taboptFlags ++ windowFlag ++ "-o $@"
>                    in comment ++

>                       aout ++ ": " ++ dependsOn ++ "\n\t" ++
>                       adpccall ++ "\n\n" ++

>                       targetName ++ ": " ++ aout ++ " " ++
>                       project ++ "_" ++ algebra ++ ".sed" ++ "\n\t" ++
>                       sed  ++ "$< > $@" ++  "\n\n" ++

>                       objectName ++ ": " ++ targetName ++ "\n\t" ++ 
>                       "$(CC) $(CPPFLAGS) $(CFLAGS) -c " ++ "$<" ++ "\n\n"

> -- generate a simple list of modules (for makefile) 
> genAdpcModules :: String -> String -> [String] -> String -> String
> genAdpcModules suffix project algebraList ppAlg  = mapsep " " gen (algebraList \\ [ppAlg])
>   where
>     gen algebra = project ++ "_" ++ algebra ++ "." ++ suffix

Generate an xml - <MODULE>  entry for every algebra
================================================================================

> genXmlModules :: LIB -> String -> [String] -> String -> [(String, String)] 
>                  -> String
> genXmlModules lib project algebraList ppAlg algebraChoiceFcts = concatMap gen scoreAlgebras
>   where
>     scoreAlgebras = algebraList \\ [ppAlg]
>     addSwitches   = length scoreAlgebras > 1 
>     
>     gen algebra = 
>       let comment = "" -- "<-- module definition for algebra " ++ algebra ++ " --/> \n"
>           windowFlag = case lib of
>                               LIBSTD -> ""
>                               LIBRNA -> " -W "
>           standardFlags = " -O -lcf -ta bt -bt so -gc cc "
>           taboptFlags   = " -iuc -cto -tadd 3 -taddc 30 "
>           sed      = " | sed -f " ++ project ++ "_" ++ algebra ++ ".sed "
>           target   = " > " ++ project ++ "_" ++ algebra ++ ".c "
>           adpccall = "-al " ++ algebra ++ " enum -cs " ++ algebra ++ 
>                      " -alpp " ++ ppAlg ++ standardFlags ++ taboptFlags ++ windowFlag ++ sed ++ target
>           lprefix = case lib of
>                          LIBSTD -> "simple"
>                          LIBRNA -> "rna"
>       in comment ++ 
>          "<module\n" ++
>          xml_attr "name" [algebra] ++ 
>          xml_attr "source" [project ++ ".lhs"] ++
>          xml_attr "main_name"   ["main_" ++ map toLower project ++ "_" ++ algebra] ++
>          xml_attr "adpc"        [adpccall] ++
>          xml_attr "condition"   (if addSwitches then ["opt->switch_" ++ algebra] else ["1"]) ++ 
>          xml_attr "output_optimal"      (output_optimal      lprefix) ++
>          xml_attr "output_subopt_start" (output_subopt_start lprefix) ++
>          xml_attr "output_subopt"       (output_subopt       lprefix) ++
>          xml_attr "output_subopt_end"   (output_subopt_end   lprefix) ++
>          "></module>\n\n"
>          where 
>            output_optimal      lp = [
>                    (if lib == LIBRNA then 
>                      spc 6 ++ "if (opts->traceback_percent) traceback_diff = abs(result_score * opts->traceback_percent / 100);\n"
>                      else "") ++
>                    spc 6 ++ lp ++
>                    "_output_optimal(opts, seq, $" ++ algebra ++ "$, result_score, "      ++ rbegin ++ ", " ++ rend ++");"]
>            output_subopt_start lp = [spc 6 ++ lp ++
>                    "_output_subopt_start(opts, seq, $" ++ algebra ++ "$, result_score, " ++ rbegin ++ ", " ++ rend ++");"]
>            output_subopt       lp = [spc 12 ++ lp ++
>                    "_output_subopt(opts, seq, $" ++ algebra ++ "$, score, result_prettyprint);"]
>            output_subopt_end   lp = [spc 6 ++ lp ++
>                    "_output_subopt_end(opts, seq, $" ++ algebra ++ "$, result_score, "   ++ rbegin ++ ", " ++ rend ++");"]

>            (rbegin, rend) = case lookup algebra algebraChoiceFcts of
>               Just "sum"     -> ("0", "0")
>               Just "maximum" -> ("result_score - traceback_diff", "result_score")
>               Just "minimum" -> ("result_score", "result_score + traceback_diff")
>               foo              -> error $ "unknown choice function (" ++ (show foo) ++ ") for algebra " ++ algebra ++ "."

-- >            bold_line lchar = "pcolor(opts->colored_output,COLOR_BOLD);\n   " ++ 
-- >                              xml_printf (replicate 65 lchar ++ "\n") ++ "\n   " ++
-- >                              "pcolor(opts->colored_output,COLOR_DEFAULT);"

-- >            suboptChoice = 
-- >                  case lookup algebra algebraChoiceFcts of
-- >                    Nothing -> error $ "unknown choice function for algebra " ++ algebra ++ "."
-- >                    Just "sum"  
-- >                      -> xml_printf "\n"
-- >                    Just chc -> if chc /= "maximum" && chc /= "minimum" then 
-- >                                   error $ "unknown choice function " ++ chc
-- >                                else  
-- >                                let resOrder = if chc == "maximum" then ["result_score - traceback_diff", "result_score"]
-- >                                                                   else ["result_score", "result_score + traceback_diff"]
-- >                                in 
-- >                                    xml_printf_args 
-- >                                      "Suboptimal range: [%d - %d]\n" resOrder ++
-- >                                      "\n   " ++
-- >                                      xml_printf "\n" ++ "\n   " ++  
-- >                                      xml_printf " Score | Candidate\n"  ++ "\n   " ++
-- >                                      bold_line '-'
-- >
-- >            output_suboptimal =
-- >                  case lookup algebra algebraChoiceFcts of
-- >                    Nothing -> error $ "unknown choice function for algebra " ++ algebra ++ "."
-- >                    Just "maximum" 
-- >                      -> xml_attr "output_suboptimal" 
-- >                                  [spc 9 ++ xml_printf_args "%6d | %s\n" ["score", "result_prettyprint"]] 
-- >                    Just "minimum" 
-- >                      -> xml_attr "output_suboptimal" 
-- >                                  [spc 9 ++ xml_printf_args "%6d | %s\n" ["score", "result_prettyprint"]] 
-- >                    Just "sum"
-- >                      -> xml_attr "output_suboptimal" 
-- >                                  [spc 9 ++ xml_printf ""]
-- >            output_finished = xml_attr "output_finished" [""{- ,bold_line '=' -} ]

Generate an xml - <switch>  entry for every algebra
================================================================================

> genXmlModes project algebraList ppAlg | length algebras == 1 = ""
>                                       | otherwise            = concatMap gen (zip algebras (genAlgSwitches algebras))
>   where
>     algebras         = algebraList \\ [ppAlg]
>     gen (algebra,sw) =  let comment = "" --"<-- mode definition for algebra " ++ algebra ++ " --/> \n"
>                         in comment ++ "<option switch=\"" ++ [sw] ++ "\" " ++ genText algebra ++ ">\n" ++
>                                       "   <switch var=\"switch_" ++ algebra ++ "\"\n" ++
>                                       "           settext=\"Algebra " ++ algebra ++ "\"\n" ++
>                                       "           infotext=\"Algebra: " ++ algebra ++ "\" default=\"on\"/>\n" ++
>                                       "<man>\n" ++ 
>                                       "This switch activates or deactivates calculation for algebra " ++ algebra ++ ".\n" ++
>                                       "In default setting, the calculations for all algebras are activated.\n" ++
>                                       "</man>\n" ++ 
>                                       "</option>\n\n"
>     genText alg = "text=\"Choose algebra " ++ alg ++ "\""

>     genAlgSwitches :: [String] -> [Char]
>     genAlgSwitches algs = generate [(head (head algs))] (tail algs)
>       where
>         generate :: [Char] -> [String] -> [Char]
>         generate fixed (alg:algs) = let sw = head' alg
>                                     in if elem sw fixed then generate fixed ((tail alg):algs)
>                                                         else generate (fixed ++ [sw]) algs
>         generate fixed [] = fixed
      
>         head' [] = '!'
>         head' xs = head xs

Generate function declarations for progname.h file
================================================================================

> genModuleEntries project algebraList ppAlg algebraChoiceFcts = concatMap gen (algebraList \\ [ppAlg])
>   where
>     gen algebra = "int main_" ++ map toLower project ++ "_" ++ algebra ++ "(toptions *_opts, tsequence *_seq);\n"

=============================================================================================================================
Phase I: Generate Makefile, .XML, progname.h
=============================================================================================================================

> genAlgebraList :: String -> [String]
> genAlgebraList s = map init (lines s)

> searchAlgebraPP :: [String] -> Maybe String
> searchAlgebraPP algs = case intersect algs standardPPNames of 
>                               [] -> Nothing
>                               xs -> Just $ head xs

> standardPPNames = ["pp","pretty","prettyprint"]

> extractChoiceFunctions :: String -> [String]
> extractChoiceFunctions str = map test (lines str)
>   where
>     test str = if      (isSubString "id"      str) then "id"
>                else if (isSubString "maximum" str) then "maximum"
>                else if (isSubString "minimum" str) then "minimum"
>                else if (isSubString "sum"     str) then "sum"
>                else error "adpc: error in extractChoiceFunctions"

> genPhaseI :: String -> LIB -> String -> IO()
> genPhaseI prefixdir lib name = do
>        -- Derive Algebras --------------------------------------------------------
>        putStrLn $ "Deriving algebras...."
>        systemReport $ "cat " ++ name ++ " | sed '/algebra\\[.*\\]/s/> #algebra\\[\\([a-zA-Z]*\\)\\]/\\1/w algebras.txt' > /dev/null"
>        algebraFile <- readFile "algebras.txt"
>        algebraList <- return $ genAlgebraList algebraFile
>        putStrLn $ "Found algebras " ++ sepList ", " algebraList
>        putStrLn $ "Searching for prettyprinting algebra... (testing " ++ sepList ", " standardPPNames ++ ")"
>        ppAlg <- case (searchAlgebraPP algebraList) of
>                               Nothing -> error $ "I can not find a prettyprinting algebra. Please add one.\n"
>                               Just pp -> do
>                                          putStrLn $ "Found prettyprinting algebra: \"" ++ pp ++ "\"."
>                                          return pp
>        -- Derive choice functions-------------------------------------------------
>        putStrLn $ "Deriving choice functions...."
>        systemReport $ "cat " ++ name ++ "| grep '\\[minimum\\|\\[maximum\\|\\[sum\\|\\[id' > choicefcts.txt"
>        chcFile <- readFile "choicefcts.txt"
>        chcFunctions <- return $ extractChoiceFunctions chcFile
>        putStrLn $ "Found choice functions " ++ sepList ", " chcFunctions

>        algebraChoiceFcts <- return $ zip (concatMap (replicate (length chcFunctions `div` length algebraList)) algebraList) chcFunctions

>        putStrLn "Guessing...:"
>        mapM (\(alg,chc) -> putStrLn $ alg ++ " -> " ++ chc) algebraChoiceFcts

>        project <- return $ getFilePrefix name
>        putStrLn $ "Creating project: \"" ++ project ++ "\"."

>        ------- Makefile ------------------------------------------------------------
>        (rnalibobject, libflag) <- return $ if lib == LIBRNA then
>                                                  ("rnalib.o", "-l rna")
>                                          else    ("",         "-l std")

>        autointerface prefixdir "Makefile.ai" "Makefile"
>                         [("PROJECT_NAME",       project),
>                          ("MAKE_PREFIX",        prefixdir),
>                          ("MAKE_RNALIB_OBJECT", rnalibobject),
>                          ("MAKE_LIBFLAG",       libflag),
>                          ("MAKE_ADPCTARGETS",   genAdpcTargets lib project algebraList ppAlg),
>                          ("MAKE_ADPCMODULES_C", genAdpcModules "c" project algebraList ppAlg),
>                          ("MAKE_ADPCMODULES_O", genAdpcModules "o" project algebraList ppAlg)]

>        ------- XML -----------------------------------------------------------------
>        user        <- deriveUserName project
>        xmltemplate <- return $ if lib == LIBSTD then "stdlib.xml.ai" 
>                                                 else "rnalib.xml.ai"
>        autointerface prefixdir xmltemplate (project ++ ".xml")
>                        [("PROJECT_NAME",    project),
>                         ("PROJECT_AUTHOR",  user),
>                         ("XML_APDCMODULES", genXmlModules lib project algebraList ppAlg algebraChoiceFcts),
>                         ("XML_APDCMODES",   genXmlModes   project algebraList ppAlg)]

>        ------- Progname.h ----------------------------------------------------------
>        autointerface prefixdir "progname.h.ai"  (project ++ ".h")
>                        [("PROJECT_NAME",      project),
>                         ("C_MODULE_ENTRIES",  genModuleEntries project algebraList ppAlg algebraChoiceFcts)]

>        ------- Phase I completed ---------------------------------------------------
>        putStrLn $ color color_GREEN "Phase I successful! Type make to build."
>


=============================================================================================================================
Phase II
=============================================================================================================================
Generator functions for C backend
=============================================================================================================================

Generate one sed file for every algebra
================================================================================

genModuleSed erzeugt ein sed file fuer jede algebra.  Hier werden die
@@-strings vom adpcompile durch konkrete Werte fuer die jeweilige
Algebra ersetzt.

> genModuleSed :: Interface -> LIB -> [(String, VarReplace)]
> genModuleSed interface lib = map gen (getInterfaceModules interface)
>   where
>     gen mod = let sed = [("C_ADDITIONAL_HEADER", addheader lib),
>                          ("OUTPUT_OPTIMAL",       replXMLChars $ getModuleOutput_optimal mod),
>                          ("OUTPUT_SUBOPT_START",  replXMLChars $ getModuleOutput_subopt_start mod),
>                          ("OUTPUT_SUBOPT",        replXMLChars $ getModuleOutput_subopt mod),
>                          ("OUTPUT_SUBOPT_END",    replXMLChars $ getModuleOutput_subopt_end mod),
>                          ("RNALIB_FREE",          if lib == LIBRNA then "rnalib_free();\n" else ""),
>                          ("MODULENAME",           map toLower (getInterfaceName interface) ++ "_" ++ getModuleName  mod),
>                          ("MODULEMAININIT",       maininit lib),
>                          ("MODULEMAINFINISH",     "  freeall();")]
>                in (getInterfaceName interface ++ "_" ++ getModuleName  mod ++ ".sed", sed)
>     addheader LIBSTD = ""
>     addheader LIBRNA = "#include \"rnalib.h\"\n" ++
>                        "int maxloop;\n"

>     maininit LIBSTD = "traceback_diff = opts->traceback_diff;"
>     maininit LIBRNA = "traceback_diff = opts->traceback_diff * 100;\n" ++
>                       "   maxloop = opts->maxloop;\n" ++
>                       "   rnalib_init(opts,seq);"


> generateSedFile (file, replacements) = do
>                                        putStrLn $ color color_BLUE file
>                                        writeFile file filecontent
>   where
>     filecontent = concatMap (\(var, subst) -> makeSedSubst var subst ++ "\n") replacements


typedef "toptions"
================================================================================

> genTOptions :: Interface -> String
> genTOptions (Interface attr _ _ _ _ opts) =  unlines (arrange "\t/" (concatMap genOpt opts))
>   where
>     genOpt opt 
>                | isSwitch opt    = [spc 2 ++ "char\t" ++ getOptionVar opt ++ "; " ++ comment opt]
>                | isSet opt       = [spc 2 ++ ppDT (setDatatype (getSet opt)) ++ getOptionVar opt ++ "; " ++ comment opt]
>                | isSwitchset opt = [spc 2 ++ "char\t" ++ getSwitchsetVar opt ++ "; " ++ comment opt] ++
>                                    [spc 2 ++ ppDT (convertDatatype (switchsetDatatype (getSwitchset opt))) ++ getOptionVar opt ++ "; " ++ comment opt]
>                | isFile opt      = [spc 2 ++ ppDT Set_datatype_string ++ getOptionVar opt ++ "; " ++ comment opt]
>                | otherwise       = []

>     ppDT Set_datatype_int    = "int\t"
>     ppDT Set_datatype_float  = "float\t"
>     ppDT Set_datatype_string = "char\t*"



C function "reset_mode"
================================================================================

> genResetMode :: Interface -> String
> genResetMode interface@(Interface _ _ _ modules _ opts) = 
>     header ++ unlines (arrange "=/" (concatMap genOpt (zip ([1] ++ repeat 0) moduleopts))) ++ trailer 
>   where
>     header = "static void reset_mode(toptions *opt) {\n"
>     trailer = "}\n"

>     moduleopts = filter isSwitch opts

>     genOpt (def,opt) | isSwitch opt = 
>                           [spc 2 ++ accOpt (getOptionVar opt) ++ " = " ++ show def ++ "; " ++ comment opt]
>     genOpt _        = []


C function "init_defaults"
================================================================================

> genInitDefaults :: Interface -> String
> genInitDefaults interface@(Interface _ _ _ _ _ opts) = header ++ unlines (arrange "=/" (concatMap genOpt opts)) ++ trailer
>   where
>     header = "static void init_defaults(toptions *opt) {\n"
>     trailer = unlines $ [
>                  "  opt->terminate            = 0;",
>                  "  opt->interactive          = 0;",
>                  "  /* PS output */",
>                  "  opt->number_of_graphics   = 0;",
>                  "  opt->graphics_includeinfo = 0;",
>                  "  opt->graphics_highlight   = 0;",
>                  "  /* output mode */",
>                  "  opt->output_mode          = 0;",
>                  "  opt->format_string        =NULL;",
>                  "}\n"]

>     genOpt opt 
>        | isSwitch opt    = [spc 2 ++ accOpt (getOptionVar opt) ++ " = " ++ getOptionSwitchDefault opt ++ "; " ++ 
>                             comment opt]
>        | isSet opt       = (if setDatatype (getSet opt) == Set_datatype_string then 
>                            [spc 2 ++ "if (" ++ accOpt (getOptionVar opt) ++ ") free(" ++ accOpt (getOptionVar opt) ++ ");"] 
>                             else []) ++
>                            [spc 2 ++ accOpt (getOptionVar opt) ++ " = " ++ getOptionDefaultValue opt ++ "; " ++ comment opt]
>        | isSwitchset opt = [spc 2 ++ accOpt (getSwitchsetVar opt) ++ " = " ++ getOptionSwitchDefault opt++ "; " ++ comment opt] ++
>                            (if switchsetDatatype (getSwitchset opt) == Switchset_datatype_string then 
>                            [spc 2 ++ "if (" ++ accOpt (getOptionVar opt) ++ ") free(" ++ accOpt (getOptionVar opt) ++ ");"] else []) ++
>                            [spc 2 ++ accOpt (getOptionVar opt) ++ " = " ++ 
>                              getOptionDefaultValue opt ++ "; " ++ comment opt]
>        | isFile opt      = [spc 2 ++ "if (" ++ accOpt (getOptionVar opt) ++ ") free(" ++ accOpt (getOptionVar opt) ++ ");"] ++
>                            [spc 2 ++ accOpt (getOptionVar opt) ++ " = NULL; " ++ comment opt ]
>        | otherwise       = []

Argument String for getopt
================================================================================

> genGetoptString :: Interface -> String
> genGetoptString (Interface _ _ _ _ _ opts) = filter (/= '_') $ concatMap get opts
>   where
>     get opt | isLonghelp opt || isSwitchset opt || isSet opt || isFile opt = getOptionSwitch opt ++ ":"
>             | otherwise                                                    = getOptionSwitch opt


Help overview screen
================================================================================

> genHelpOverview :: Interface -> Int -> [Option] -> String
> genHelpOverview interface ind opts = header ++ 
>                            unlines (arrange "\t" (concatMap genOpt opts ++ trailer)) ++ 
>                            spc ind ++ "opt->terminate = 1;\n"
>  where
>    header = indent ind $ [
>     "if (interactive) {",
>     c_printf_block 2 (getInteractiveHelpHeader interface),
>     "}",
>     "else {",
>     c_printf_block 2 (getManualSynopsis interface),
>     "printf(\"Options:\\n\");",
>     "}"]

>    trailer = map (\s -> spc ind ++ s) [
>     "if (opt->interactive) {",
>     "printf(\"Additional interactive mode commands:\\n\");",
>     "printf(\"  :s          \tShow current configuration\\n\");",
>     "printf(\"  :d          \tReset configuration\\n\");",
>     "printf(\"  :e <string> \tExecute system command\\n\");",
>     "printf(\"  :q          \tQuit\\n\");",
>     "}"]

>    genOpt opt = [spc ind ++ "printf(\"" ++ str opt ++ "\\n\");"] 
>      where 
>        str opt | isComment opt = getOptionText opt
>                | otherwise     = "  -" ++ getOptionSwitch opt ++ " " ++ getOptionArgtext opt ++ "\t" ++ getOptionText opt


C function "process_args"
================================================================================

> genProcessArgs :: Interface -> String
> genProcessArgs interface@(Interface attr _ _ _ _ opts) = concatMap genOpt opts 
>   where
>     genOpt opt | isComment opt = ""
>                | otherwise = "      " ++ comment opt ++ "\n" ++
>                              "      case '" ++ (getOptionSwitch opt) ++ "':\n" ++ gen 8 opt ++ 
>                                      indent 8 (getOptionCode opt) ++ spc 8 ++ "break;\n"
>       where
>         gen ind opt 
>           | isHelp opt     = genHelpOverview interface ind opts 
>           | isLonghelp opt = indent ind $ [
>              "if      (optarg[0]=='-') { manoptmode = '-'; manopt = optarg[1]; }",
>              "else if (optarg[0]==':') { manoptmode = ':'; manopt = optarg[1]; }",
>              "else                     { manoptmode = '-'; manopt = optarg[0]; }",
>              "if (!interactive) printf(\"\\n\");",
>              "#include \"" ++ getInterfaceName interface ++ "-man.c\"",
>              "if (!interactive) printf(\"\\n\");",
>              "opt->terminate = 1;"]
>           | isVersion opt = spc ind ++ "printf(\"%s (%s)\\n\",PACKAGE_STRING,RELEASE_DATE);\n" ++
>                             concatMap (\s -> spc ind ++ "printf(\"" ++ s ++ "\\n\");\n") (getInterfaceVersioninfo interface) ++
>                             spc ind ++ "opt->terminate = 1;\n"
>           | isFile opt =  spc ind ++ "if (" ++ accOpt (getOptionVar opt) ++ ") free(" ++ accOpt (getOptionVar opt) ++ ");\n" ++
>                            spc ind ++ accOpt (getOptionVar opt) ++ " = mkstr(optarg);\n" 


>         gen ind opt 
>           | isSet opt = let 
>                set        = getSet opt
>                datatype   = setDatatype set
>                var        = setVar set
>                setstring  = getOptionSettext opt 
>                defaultval = getOptionDefaultValue opt
>                minval     = setMinval set
>                maxval     = setMaxval set
>             in genSet ind var datatype setstring defaultval minval maxval
>           | isSwitchset opt = let
>                set        = getSwitchset opt
>                datatype   = convertDatatype $ switchsetDatatype set
>                var        = switchsetVar set
>                swvar      = getSwitchsetVar opt
>                setstring  = getOptionSettext opt
>                disablestr = getOptionDisabletext opt
>                defaultval = getOptionDefaultValue opt
>                minval     = switchsetMinval set
>                maxval     = switchsetMaxval set
>                checkdisable = indent ind $ [
>                      "if ((interactive) && (optarg[0] == '-')) {",
>                      "  printf(\"" ++ disablestr ++ "\\n\");",
>                      "  " ++ accOpt swvar ++ " = 0;",
>                      "}"]
>             in checkdisable ++ spc ind ++ "if (optarg[0] != '-') {\n" ++ 
>                spc ind ++ "  " ++ accOpt swvar ++ " = 1;\n"  ++
>                genSet (ind+2) var datatype setstring defaultval minval maxval ++ 
>                spc ind ++ "}\n"

>           where
>             genSet :: Int -> String -> Set_datatype -> String -> String 
>                       -> Maybe String -> Maybe String -> String
>             genSet ind var datatype setstring _ minv maxv = freeStr ++ scanVal ++ checkmin ++ checkmax ++ output 
>               where
>                 freeStr = if datatype == Set_datatype_string then spc ind ++ "if (" ++ accOpt var ++ ") free(" ++ accOpt var ++ ");\n"
>                                            else ""
>                 scanVal = case datatype of
>                            Set_datatype_string   -> spc ind ++ accOpt var ++ " = mkstr(optarg);\n"
>                            otherwise -> spc ind ++ "sscanf(optarg,\"" ++ pr ++ "\",&(" ++ accOpt var ++ "));\n"
>                              where
>                                pr = case datatype of
>                                       Set_datatype_int   -> "%d"
>                                       Set_datatype_float -> "%f"
        	      
>                 checkmin = case minv of
>                                  Just minv -> spc ind ++ accOpt var ++ " = max(" ++ minv ++ ", " ++ accOpt var ++ ");\n"
>                                  Nothing   -> ""
>                 checkmax = case maxv of
>                                  Just maxv -> spc ind ++ accOpt var ++ " = min(" ++ maxv ++ ", " ++ accOpt var ++ ");\n"
>                                  Nothing   -> ""
>                 output =              spc ind ++ "if (interactive) printf(\"" ++ setstring ++ "\\n\", " ++ accOpt var ++ ");\n"


>         gen ind opt 
>           | isSwitch opt = let 
>                set        = getSwitch opt
>                var        = switchVar set
>                setstring  = getOptionSettext opt
>                defaultval = getOptionSwitchDefault opt
>             in  indent ind $ 
>                  ["if (interactive) {",
>                   "  printf(\"" ++ setstring ++ "\");",
>                   "  " ++ accOpt var ++ " = 1 - " ++ accOpt var ++ ";",
>                   "  if (" ++ accOpt var ++ ") printf (\" = ON. Type -" ++ (getOptionSwitch opt) ++ " again to switch off.\\n\");",
>                   "  else printf (\" = OFF. Type -" ++ (getOptionSwitch opt) ++ " again to switch on.\\n\");",
>                   "}",
>                   "else " ++ accOpt var ++ " = " ++ ppDef defaultval ++ ";"]
>              where
>                ppDef "0"  = "1"
>                ppDef _    = "0"

>         gen _ _  = []


C function "print_settings"
================================================================================

> genPrintSettings :: Interface -> String
> genPrintSettings interface@(Interface _ _ _ _ _ opts) = header ++
>      unlines (arrange "%" (concatMap genOpt opts)) ++ trailer
>   where
>     header = unlines $ [
>                "static void print_settings(toptions *opt) {",
>                "  printf(\"Current settings\\n-----------------\\n\");"]
>     trailer = "}\n"

>     genOpt opt | isSet opt       = ["  printf(\"" ++ getOptionInfotext opt ++ mksw opt ++
>                                     "\\n\", " ++ accOpt (getOptionVar opt) ++ ");"]
>                | isSwitch opt    = ["  printf(\"" ++ getOptionInfotext opt ++ 
>                                     "        %s" ++ mksw opt ++ "\\n\", " ++ 
>                                         accOpt (getOptionVar opt) ++ " ? " ++ "\"ON\" : \"OFF\" );"]
>                | isSwitchset opt = ["  if (" ++ accOpt (getSwitchsetVar opt) ++ ")",
>                                     "  printf(\"" ++ getOptionInfotext opt ++ mksw opt ++
>                                     "\\n\", " ++ accOpt (getOptionVar opt) ++ ");",
>                                     "  else",
>                                     "  printf(\"" ++ repl (getOptionInfotext opt) ++ mksw opt ++"\\n\",\"\");"]
>                | otherwise       = []

>     repl s = let (a,b) = span (/= '%') s
>              in a ++ "%s-"
>     mksw opt = "  (-" ++ getOptionSwitch opt ++ ")"

module functions calls in the main project file
================================================================================

> genModuleStarts interface = concatMap gen (getInterfaceModules interface)
>   where
>     gen :: Module -> String
>     gen mod = let name = getModuleName mod
>                   cond = getModuleCondition mod 
>               in "        if (" ++ cond ++ ") {\n" ++
>                  "          main_" ++ map toLower (getInterfaceName interface) ++ "_" ++ map toLower name ++ "(opt, seq);\n" ++
>                  "        }\n"

=============================================================================================================================
Generator for pod manual file
=============================================================================================================================

> genPod :: Interface -> String
> genPod interface@(Interface _ _ _ _ _ opts) =  unlines (arrange "\t" (concatMap genOpt opts)) ++ "\n"
>  where
>    genOpt opt | isComment opt = ["=head2 " ++ getOptionText opt,""]
>               | otherwise     = ["B<-" ++ getOptionSwitch opt ++ "> " ++ optC ++ argtext ++ "\t" ++ getOptionText opt,""] ++ getOptionMan opt
>      where
>        argtext = getOptionArgtext opt
>        optC    = if argtext == "" then "" else "C"


=============================================================================================================================
Glueing it all together....
=============================================================================================================================

> main :: IO ()
> main = do
>        args <- getArgs 
>        cmain args
>        return ()

main worker
================================================================================

> data LIB = LIBSTD | LIBRNA
>                     deriving (Show, Eq)

> isFileType file end = map toUpper ((\s -> drop (length s - 3) s) file) == map toUpper end
> getFilePrefix s = take (length s - 4) s

> cmain (a:as) = do
>      progName <- getProgName
>      let (prefixdir, args) = if progName == "adpc-bin" then (a, as) else (prefix, (a:as))
>      (infile, lib) <- case args of
>                             [file]             -> do
>                                                   putStrLn "No library given. Trying to guess..."
>                                                   systemReport $ "grep energy " ++ file ++ " > energy.txt"
>                                                   efile <- readFile "energy.txt"
>                                                   res <- if efile == [] then (do
>                                                        putStrLn "No energy found in input file. Using standard lib..."
>                                                        return (file, LIBSTD))
>                                                      else (do
>                                                        putStrLn "Energy found in input file. Using RNA lib..."
>                                                        return (file, LIBRNA))
>                                                   return res
>
>                             ["-l","std", file] -> return (file, LIBSTD)
>                             ["-l","rna", file] -> return (file, LIBRNA)
>                             [] -> error "not enough parameters: adpc-bin PREFIX [-L {std|rna}] FILE"

>       --------------------------------------------------------------
>       --- Phase I --------------------------------------------------
>       --------------------------------------------------------------
>      if isFileType infile "lhs" then do
>           putStrLn ("reading "++infile)
>           genPhaseI prefixdir lib infile

>        else if isFileType infile "xml" then do 
>           -------- read xml input ------------------
>           putStrLn ("reading "++infile)
>           interface   <- fReadXml infile
>           outfile     <- return $ getInterfaceName interface
       
>           -------- generate files ------------------
>           --------------------------------------------------------------
>           --- Phase II -------------------------------------------------
>           --------------------------------------------------------------

>           optionsfile <- return $ if lib == LIBSTD then "std_options.h.ai"
>                                                    else "rna_options.h.ai"
>           autointerface prefixdir optionsfile "options.h" 
>                      [("C_TOPTIONS", genTOptions interface )]

>           autointerface prefixdir "prognamei.c.ai" (outfile ++ ".c")
>                      [("PROJECT_NAME",                   getInterfaceName        interface), 
>                       ("PROJECT_VERSION",                getInterfaceVersion     interface),
>                       ("PROJECT_VERSIONINFO",            unlines $ getInterfaceVersioninfo interface),
>                       ("C_RESET_MODE",                   genResetMode            interface),
>                       ("C_INIT_DEFAULTS",                genInitDefaults         interface),
>                       ("C_GETOPTSTRING",                 genGetoptString         interface),
>                       ("C_PROCESS_ARGS",                 genProcessArgs          interface),
>                       ("C_PRINT_SETTINGS",               genPrintSettings        interface),
>                       ("PROJECT_NAME",                   getInterfaceName        interface),
>                       ("C_FILENAMEVAR",                  "inputfile"),
>                       ("PROJECT_INTERACTIVEMESSAGE",     getInteractiveWelcome interface),
>                       ("PROJECT_INTERACTIVECOMMANDLINE", getInteractiveCommandline interface),
>                       ("C_READSEQNEXT",                  if lib==LIBSTD then "line" else "fasta"),
>                       ("C_MODULESTART",                  genModuleStarts interface)]

>           autointerface prefixdir "progname.pod.ai" (outfile ++ ".pod")
>                      [("PROJECT_NAME",                   getInterfaceName interface),
>                       ("PROJECT_AUTHOR",                 getInterfaceAuthor interface),
>                       ("PROJECT_VERSION",                getInterfaceVersion interface),
>                       ("PROJECT_VERSIONINFO",  unlines $ getInterfaceVersioninfo interface),
>                       ("PROJECT_SYNOPSIS",               getManualSynopsis interface),
>                       ("PROJECT_INTRODUCTION",           getManualIntroduction interface),
>                       ("PROJECT_EXAMPLES",               getManualExamples interface),
>                       ("POD_OPTIONS",                    genPod interface)]

>           date       <- getClockTime
>           dat        <- toCalendarTime date
>           user       <- deriveUserName (getInterfaceName interface)

>           autointerface prefixdir "config.h.ai" "config.h"
>                      [("PROJECT_NAME",                   getInterfaceName interface),
>                       ("PROJECT_VERSION",                removeSpace $ getInterfaceVersion interface),
>                       ("PROJECT_COPYRIGHT",              unlines $ getInterfaceVersioninfo interface),
>                       ("PROJECT_AUTHOR",                 getInterfaceAuthor interface),
>                       ("PROJECT_DATE",                   "\"" ++ calendarTimeToString dat ++ "\"")]

>           putStrLn "generate global sed file:"
>           generateSedFile (getInterfaceName interface ++ ".sed",
>                      [("PROJECT_NAME",                   getInterfaceName interface),
>                       ("PROJECT_VERSION",                removeSpace $ getInterfaceVersion interface),
>                       ("PROJECT_AUTHOR",                 getInterfaceAuthor interface),
>                       ("PROJECT_COPYRIGHT",              unlines $ getInterfaceVersioninfo interface),
>                       ("PROJECT_DATE",                   "\"" ++ calendarTimeToString dat ++ "\""),
>                       ("MAKE_ADPCMODULES_C",             concatMap (\mod -> getInterfaceName interface ++ "_" ++ getModuleName mod ++ ".c ") 
>                                                             (getInterfaceModules interface)),
>                       ("MAKE_RNALIB_SOURCES",            if lib==LIBSTD then "" else "rnalib.h rnalib.c"),
>                       ("MAKE_RNALIB_EXTRA_SOURCES",      if lib==LIBSTD then "" else "intloop11.c intloop21.c intloop22.c")])

>           putStrLn "generate sed files for every algebra:"
>           sedfiles <- return $ genModuleSed interface lib
>           sequence_ $ map generateSedFile sedfiles

>           -------- Finish --------------------------
>           putStrLn ("Done.")
      
>          else 
>             putStrLn ("I have no idea what to do with file " ++ infile)

adpc info screen
================================================================================

> cmain _ = putStrLn $ "adpc 1." ++ getRev revision ++ "\n" ++
>                      "Usage: adpc <interface>.xml\n" ++
>                      "Build binary:       make\n" ++
>                      "Build distribution: make <interface>-distrib\n" ++
>                      "Clean up:           make clean"

> autointerface prefixdir infile outfile vars = do
>   putStrLn $ color color_BLUE $ infile ++ " -> " ++ outfile ++ "." 
>   input    <- readFile $ forWindows $ prefixdir ++ "/share/adpc/lib/" ++ infile
>   output   <- return $ sedString infile vars input
>   writeFile outfile output

=============================================================================================================================
Tools
=============================================================================================================================

XML Tools
================================================================================

> xml_attr :: String -> [String] -> String
> xml_attr attr [string] =
>    "  " ++ attr ++ "=\"" ++ string ++ "\"\n"   
> xml_attr attr strings =
>    "  " ++ attr ++ "=\"\n" ++ concatMap (\x -> "   " ++ x ++ "\n") strings ++ "\"\n"   

> xml_printf :: String -> String
> xml_printf s = "printf($" ++ replXML  s ++ "$);"

> xml_printf_args :: String -> [String] -> String
> xml_printf_args s args = "printf($" ++ replXML s ++ "$, " ++  sepList ", " args ++ ");"

> c_comment :: String -> String
> c_comment s = "/* ----- " ++ s ++ " " ++ replicate (70 - length s) '-' ++ " */"

> replXML []     = [] 
> replXML (x:xs) = case lookup x mapping of
>                                Just string -> string ++ replXML xs
>                                Nothing     -> [x]    ++ replXML xs
>   where
>     mapping = [('\n', "\\\\n"),
>                ('%',"#"),
>                ('\"', "$")]

> replXMLChars :: String -> String
> replXMLChars [] = []
> replXMLChars ('$':xs) = "\"" ++ replXMLChars xs
> replXMLChars ('#':xs) = "%" ++ replXMLChars xs
> replXMLChars (x:xs)   = x:replXMLChars xs

C Tools
================================================================================

> c_printf_block :: Int -> String -> String
> c_printf_block ind string = concatMap (\s -> spc ind ++ "printf(\"" ++ s ++ "\\n\");") (lines string)

color tools
================================================================================

> color_DEFAULT     = "\x1b[0m"
> color_BOLD        = "\x1b[1m"
> color_BLACK       = "\x1b[0;30m"
> color_BLUE        = "\x1b[0;34m"
> color_GREEN       = "\x1b[0;32m"
> color_CYAN        = "\x1b[0;36m"
> color_RED         = "\x1b[0;31m"
> color_PURPLE      = "\x1b[0;35m"
> color_BROWN       = "\x1b[0;33m"
> color_GRAY        = "\x1b[0;37m"
> color_DARKGRAY    = "\x1b[1;30m"
> color_LIGHTBLUE   = "\x1b[1;34m"
> color_LIGHTGREEN  = "\x1b[1;32m"
> color_LIGHTCYAN   = "\x1b[1;36m"
> color_LIGHTRED    = "\x1b[1;31m"
> color_LIGHTPURPLE = "\x1b[1;35m"
> color_YELLOW      = "\x1b[1;33m"
> color_WHITE       = "\x1b[1;37m"

> color col str  = col ++ str ++  "\x1b[0m"

div. tools
================================================================================

> removeSpace = filter (/= ' ')

> iswindows = False

> forWindows x | iswindows = repl $ "c:/unix/cygwin/" ++ x
>              | otherwise = x
>  where
>    repl ('/':xs) = '\\':repl xs
>    repl (x:xs)   = x:repl xs
>    repl [] = []

> revision =  "$Revision: 1.7 $ "

> accOpt :: String -> String
> accOpt s = "opt->" ++ s;

> myGetEnv :: String -> IO String
> myGetEnv v = catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
>                        (getEnv v)
>                        (\_ -> return "")

> deriveUserName :: String -> IO String
> deriveUserName project = do
>--                          user <- myGetEnv "USER"
>                          name <- getEffectiveUserName
>                          return name


> comment opt = comm (getOptionSwitch opt) (getOptionText opt)
>   where
>     comm c descr = let (a,b) = span (/= '\t') descr
>                    in "// " ++ a ++ " (-" ++ c ++ ")"


> indent :: Int -> [String] -> String
> indent n lines = concatMap (\c -> spc n ++ c ++ "\n") lines

> indentCmt :: Int -> [String] -> String
> indentCmt n lines = concatMap (\c -> "//" ++  spc n ++ c ++ "\n") lines

> cmtLine = "// ========================================================================="

> arrange :: [Char] -> [String] -> [String]
> arrange []     lines = lines
> arrange (s:ss) lines = arrange ss (arr s lines)
>   where

>     arr :: Char -> [String] -> [String]
>     arr sep lines = map arr lines
>       where
>         maxpos = maximum $ map getpos lines
>         getpos l = getpos' 0 l
>         getpos' n []     = 0
>         getpos' n (c:cs) | c == sep  = n
>                          | otherwise = getpos' (n+1) cs
>         arr l = arr' 0 l
>         arr' _ []        = []
>         arr' n xs@(c:cs) | c == sep  = replicate (maxpos - n) ' ' ++ xs
>                          | otherwise = c:arr' (n+1) cs

> isSubString :: (Eq a) => [a] -> [a] -> Bool
> isSubString s t = any (== True) [ s == take (length s) (drop d t)  | d <- [0..length t - length s]]

> replaceNl []        = []
> replaceNl ('\n':xs) = "\\n" ++ replaceNl xs
> replaceNl (x:xs)    = x:replaceNl xs

> replaceHash []        = []
> replaceHash ('#':xs) = "%" ++ replaceHash xs
> replaceHash (x:xs)    = x:replaceHash xs

> removeLastNls = reverse . snd . span (== '\n') . reverse

> spc n = replicate n ' ' 

> pattErr n p = error $ n ++ ": undefined pattern " ++ show p

> mapsep sep f xs = sepList sep (map f xs)
> sepList _ []       = []
> sepList _ [x]      = x
> sepList t (x:xs)   = x ++ t ++ sepList t xs

> getRev r = fst $ span isDigit $ tail $ snd $ span (/= '.') r
