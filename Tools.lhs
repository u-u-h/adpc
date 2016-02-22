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



> module Tools where

> import Char
> import List

> rev_Tools =  "$Revision: 740 $"


Single-/Two-Track
------------------

> data TrackMode a = ST a | TT a a   deriving (Eq, Show)

------------------------

> -- | Standard head with customized error handling
> head' :: [a] -> String -> a
> head' [] e = error e
> head' l _  = head l

> pattErr :: (Show a) => String -> a -> b
> pattErr n p = error $ n ++ ": undefined pattern " ++ show p

> fst3 (a,_,_) = a
> snd3 (_,a,_) = a
> thd3 (_,_,a) = a

> fst4 (a,_,_,_) = a
> snd4 (_,a,_,_) = a
> thd4 (_,_,a,_) = a
> fth4 (_,_,_,a) = a

> fst5  (a,_,_,_,_) = a
> snd5  (_,a,_,_,_) = a
> thd5  (_,_,a,_,_) = a
> fth5  (_,_,_,a,_) = a
> fith5 (_,_,_,_,a) = a

> fst6  (a,_,_,_,_,_) = a
> snd6  (_,a,_,_,_,_) = a
> thd6  (_,_,a,_,_,_) = a
> fth6  (_,_,_,a,_,_) = a
> fith6 (_,_,_,_,a,_) = a
> sth6  (_,_,_,_,_,a) = a

> spc :: Int -> String
> spc n = replicate n ' ' 

> strtoInt :: String -> Int
> strtoInt s = strtoInt' 0 (reverse s) where
>   strtoInt' _ []     = 0
>   strtoInt' e (c:cs) = (digitToInt c)*(10^e) + strtoInt' (e+1) cs

> sepList :: [a] -> [[a]] -> [a]
> sepList _ []       = []
> sepList _ [x]      = x
> sepList t (x:xs)   = x ++ t ++ sepList t xs

> mapsep :: [a] -> (b -> [a]) -> [b] -> [a]
> mapsep sep f xs = sepList sep (map f xs)

> mapsToEq :: (Eq b) => (a -> b) -> [a] -> Bool
> mapsToEq _ [] = True
> mapsToEq f xs = length (nub (map f xs)) == 1

> sepList' :: String -> [String] -> String
> sepList' t x = if length x == 0 then "" else sepList t x ++ t

> sepListi :: (Show a) => String -> [a] -> String
> sepListi _ []       = ""
> sepListi _ [x]      = (show x)
> sepListi t (x:xs)   = (show x) ++ t ++ sepListi t xs

> replaceChar :: (Eq a) =>  [a] -> a -> [a] -> [a]
> replaceChar s c s2 = concatMap repl s
>   where
>     repl x | x == c    = s2
>            | otherwise = [x]

> isPrefix :: (Eq a) => [a] -> [a] -> Bool
> isPrefix p s = take (length p) s == p

> isPrefixes :: (Eq a) => [[a]] -> [a] -> Bool
> isPrefixes p s = or (map (flip isPrefix s) p)

> dropPrefix :: [a] -> [b] -> [b]
> dropPrefix p s = drop (length p) s

> -- | Drop k-suffix
> initn :: Int -> [a] -> [a]
> initn k s = take (length s - k) s

FIXME remove getRev and clean up revisions stuff in Compile.lhs

> getRev :: String -> Int
> getRev r = 42

 getRev r = let -- nstr = init $ drop 2 $ snd $ span (/= ':') r
               nstr = filter isDigit r
            in if all isDigit' nstr then read nstr
            else error $ "wrong rev format: " ++ r ++ ", parsed: " ++ nstr
    where isDigit' c = isDigit c || c == ' '

Fixpoint operator:

> fixit :: (Eq result) => ([result] -> a -> result) -> [result] -> Int -> [a] -> String -> [result]
> fixit wrk currentResults counter ps errmsg = e 
>    where
>    newResults = map (wrk currentResults) ps
>    e | newResults  == currentResults = newResults 
>      | counter     < 0               = error errmsg
>      | otherwise                     = fixit wrk newResults (counter - 1) ps errmsg

Association:

> type Assoc a b  = [(a,b)]

> ppAssocElem :: (Show a, Show b) => (b -> String) -> (a, b) -> String
> ppAssocElem pp (n, c) = show n ++ " -> " ++ pp c ++ "\n"
> ppAssoc :: (Show a, Show b) => (b -> String) -> Assoc a b -> String
> ppAssoc pp = concatMap (ppAssocElem pp)

> lookupA :: (Eq a, Show a) => Assoc a b -> a -> [b]
> lookupA ass x         = [ b | (a,b) <- ass, a==x ] 

> lookupA1 :: (Eq a, Show a) => Assoc a b -> a -> String -> b
> lookupA1 ass x err     = head' [ b | (a,b) <- ass, a==x ] $ "unknown " ++ err ++ " for name " ++ show x ++ "."

> deleteA :: (Eq a, Show a) => Assoc a b -> a -> Assoc a b
> deleteA [] x          = error $ "element " ++ show x ++ " not found."
> deleteA ((a,b):ass) x | x == a = ass
>                       | otherwise = (a,b): deleteA ass x

> insertA :: (Eq a, Show a) => Assoc a b -> a -> b -> Assoc a b
> insertA ass a b       = (a,b):ass

> replaceA :: (Eq a, Show a) => Assoc a b -> a -> b -> Assoc a b
> replaceA ass a b      | length found == 0 = (a,b) : ass
>                       | otherwise         = insertA (deleteA ass a) a b 
>   where
>     found = [ b | (a',b') <- ass, a' == a ]

> appendA :: (Eq a, Show a) => Assoc a [b] -> a -> [b] -> Assoc a [b]
> appendA ass a b      | length found == 0 = (a,b) : ass
>                      | otherwise         = insertA (deleteA ass a) a (b ++ bs)
>   where
>     found = [ b' | (a',b') <- ass, a' == a ]
>     bs    = head found

Typecheck Tools
---------------

> -- join list with given glue element

> concatWith :: [a]-> [[a]]-> [a]
> concatWith glue xs = (concat . intersperse glue) xs

> comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
> comparing p x y = compare (p x) (p y)

> f # x = fmap f x
> concatMapM f xs = concat # mapM f xs

> done :: Monad m => m ()
> done = return ()

> mapFst f = map (apFst f)
> apFst f (x,y) = (f x,y)