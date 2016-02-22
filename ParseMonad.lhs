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




> module ParseMonad where

> import ParseTree

> -- data ParseResult a = Ok { getResult :: a } | Failed (String, Int)
> data ParseResult a = Ok a
>                   | Failed (String, Int)
>   deriving Show;

> type P a = String -> LineNumber -> ParseResult a


> thenP :: P a -> (a -> P b) -> P b
> m `thenP` k = \s li ->
>     case m s li of 
>         Ok a -> k a s li
>         Failed e -> Failed e

> returnP :: a -> P a
> returnP a = \s li -> Ok a

> failP :: (String,Int) -> P a
> failP err = \s li -> Failed err

> catchP :: P a -> ((String,Int) -> P a) -> P a
> catchP m k = \s li ->
>    case m s li of
>       Ok a -> Ok a
>       Failed e -> k e s li


> failed (Failed _) = True
> failed (Ok _    ) = False

> getFailed (Failed s) = s
> getOk     (Ok s)     = s


