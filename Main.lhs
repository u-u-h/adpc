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



> module Main (

>   main

> ) where

> import System
> import Compile
> import CGI
> import LatexRecs_cgi
> import Optimize_cgi
> import Compile_cgi
> import Alggen_cgi

This is just a wrapper file, used to build
a compiler version runnable from the commandline

> main = do
>        qs <- getQueryString
>        case qs of
>             Nothing -> do
>                        args <- getArgs
>                        (_, errline) <- cmain args
>                        if errline /= -1 then exitWith (ExitFailure 1) else return ()
>                        return ()
>             Just s -> do
>                       cgiVars <- getCgiVars
>                       env     <- return $ cgiVars ++ urlDecode s
>                       res     <- case lookup "cgitype" env of
>                                   Just "LatexRecs" -> main_latexrecs_cgi env
>                                   Just "Optimize"  -> main_optimize_cgi env
>                                   Just "Compile"   -> main_compile_cgi env
>                                   Just "Alggen"    -> main_alggen_cgi env
>                                   x                -> error $ "unknown cgitype " ++ show x
>                       putStr res


> myGetEnv :: String -> IO String
> myGetEnv v = catch (getEnv v) (const (return ""))
>                       
> getQueryString :: IO (Maybe String)
> getQueryString = do
>    method <- myGetEnv "REQUEST_METHOD"
>    case method of
>       "POST" -> do len <- myGetEnv "CONTENT_LENGTH"
>                    inp <- getContents
>                    return $ Just (take (read len) inp)
>       "GET"  -> do s <- myGetEnv "QUERY_STRING"
>                    return $ Just s
>       otherwise -> return Nothing


