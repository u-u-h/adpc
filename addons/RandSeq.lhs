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


> module RandSeq
> where

> import Random
> import Array



> dice :: IO ()
> dice = do
>   x <- getStdRandom (randomR (True,False))
>   print x

> foo 0 = return ()
> foo n = dice >> foo (n-1)

------------------------------------------------------------

> data Alphabet = A | C | G | U | N
>   deriving (Show, Enum, Bounded)

 instance (Enum a, Bounded a) => Random a where

> instance Random Alphabet where
>  randomR (a,b) g = case (randomR (fromEnum a, fromEnum b) g) of
>                      (x, g) -> (toEnum x, g)
>  random g        = randomR (minBound, maxBound) g


> dices n s = do
>   let x = take n $  randomRs (A, N) $ mkStdGen s
>   putStr (concatMap (show) x)

------------------------------------------------------------

> list2array l = array (0, length l -1) [ el | el <- (zip [0..] l) ] 

> rand_array a n s = [ a!i | i <- xs ]
>  where
>   xs = take n $ randomRs (bounds a) $ mkStdGen s

> print_list y = do
>   putStr $ concat y
