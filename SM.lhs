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



SM == State Monad

> module SM(

>   SM,
>   putSM,
>   putSMError,
>   putSMerr,
>   rev_SM,
>   runSM,

> ) where

> rev_SM =  "$Revision$"

> type MType = ([String],Int)

> data SM a = SM (MType -> (a,MType))  -- The monadic type

> instance Monad SM where
>   -- defines state propagation
>   SM c1 >>= fc2         =  SM (\s0 -> let (r,s1) = c1 s0 
>                                           SM c2 = fc2 r in
>                                          c2 s1)
>   return k              =  SM (\s -> (k,s))

>  -- extracts the state from the monad
> readSM                  :: SM MType
> readSM                  =  SM (\s -> (s,s))

>  -- updates the state of the monad
> updateSM                :: (MType -> MType) -> SM ()  -- alters the state
> updateSM f              =  SM (\s -> ((), f s)) 

> -- run a computation in the SM monad
> runSM                   :: MType -> SM a -> (a,MType)
> runSM s0 (SM c)         =  c s0


> putSM :: String -> SM ()
> putSM s = updateSM (cons' (s,-1)) where
>             cons' (s, n) (s', n') = (s:s', n)

> putSMerr :: (String, Int) -> SM ()
> putSMerr s = updateSM (cons' s) where
>             cons' (s, n) (s', n') = (s:s', n)
 
> putSMError label = SM (\(s,_) -> error $ (unlines.reverse) (s ++ ["\n", "stopped at label "++ label]))

> test = do
>         putSM "Hello"
>         putSM "World"
>         putSMError "dfkj2"
>         putSM "testing"
