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


> module TC where
> -- module Main where

> import System
> import NewParser
> import Lexer
> import ParseMonad
> import ParseTree
> import TypecheckMonad
> import TypecheckTree
> import Typecheck
> import Tools
> import Annotate
> import Layout(stripComments, processImports)
> import ConfigOutput(prefix)


> main :: IO ()
> main = do
>        args <- getArgs
>        let verbose     =     ("-v" `elem` args) -- default: off
>        let colourError = not ("-c" `elem` args) -- default: on
>        test verbose colourError [a | a <-args, (head a)/='-']
>        done


> mainParse verbose filename = do
>                    input <- readFile filename
>                    input <- return $ stripComments input
>                    -- module system ------------------
>                    input <- processImports prefix "" filename 1 input
>                    if verbose then putStrLn $ "preprocessed:\n" 
>                                             ++ input else done
>                    -- layout -------------------------
>                    writeFile (filename ++ ".stripped") input
>                    system $ "layout " ++ filename ++ ".stripped > " 
>                                       ++ filename ++ ".layout"
>                    input <- readFile (filename ++ ".layout")
>                    if verbose then putStrLn $ "layouted:\n" 
>                                             ++ input 
>                               else done
>                    -- parsing ------------------------
>                    parsed <- return $ parse input (filename, 1)
>                    return parsed


> test :: Bool -> Bool -> [String] -> IO ()
> test verbose colourError args = case args of
>      [filename] -> do
>                    parsed <- mainParse False filename
>                    if failed parsed 
>                     then do
>                       (err, line)  <- return $ getFailed parsed
>                       putStrLn err
>                     else do
>                       parsedResult <- return $ getOk parsed
>                       if verbose 
>                          then do
>                               putStrLn "Syntax correct."
>
>                               putStrLn "Prettyprint of parse tree:\n" 
>                               putStrLn $ show parsedResult
>                               --putStrLn $ prettyPrint parsedResult
>                          else done
>
>                       putStrLn "Type Check:\n"
>                       let tcResult = typecheck parsedResult
>
>                       if verbose 
>                          then do
>                               putStrLn "Trace\n"
>                               putStrLn $ unlines $ snd4 tcResult
>                               putStrLn "Warnings\n"
>                               putStrLn $ show    $ thd4 tcResult
>                               putStrLn "\nErrors\n"
>                               putStrLn $ unlines $ fth4 tcResult
>                               putStrLn "Result\n"
>                          else done
>
>                       putStrLn $ showResult colourError $ fst4 tcResult
>      _          -> error $ "\nUsage: tc [options] <filename>\n"
>                          ++"Options:\n"
>                          ++" -v toggles verbose output\n"
>                          ++" -c toggles coloured output"
>
> showResult :: Bool -> Either Error [Env] -> String
> showResult colourError = either (showErr colourError) (unlines . map (unlines . map showTV1))
>
>  where
>          showTV1 :: TypedVar1 -> String
>          showTV1 (v:>:t,ln) = v++" :: "++(prettyPrint t)
>                                ++" (derived in "++(showLN ln)++")"
>          showLN :: LineNumber -> String
>          showLN (m,l) = show m++", line "++show l
>
>          showErr :: Bool -> Error -> String
>          showErr colourError = if colourError then ((colour red).show) else show
>
>          colour c s  = "\x1b[00;" ++ show c  ++ "m" ++ s ++ "\x1b[00;0m"
>          red   = 31


> getTCresult (ParseMonad.Ok parsedTC) = (conciseOutput, verboseOutput)
>   where
>     verboseOutput = "Typecheck Trace\n"++ (unlines $ snd4 tcResult)++
>       "Typecheck Warnings\n"++ (show    $ thd4 tcResult)++
>       "\nTypecheck Errors\n"++ (unlines $ fth4 tcResult)

>     conciseOutput = showResult False $ fst4 tcResult

>     tcResult = typecheck parsedTC

