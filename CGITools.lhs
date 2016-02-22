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



> module CGITools where
> import Char

> replace :: String -> [String] -> String
> replace [] _ = []
> replace ('#':'#':'#':n:ts) ps = ps!!(digitToInt' n) ++ replace ts ps
> replace (t:ts)             ps = t:replace ts ps

> replace' :: String -> [(Int,String)] -> String
> replace' [] _ = []
> replace' ('#':'#':'#':n:ts) rs = (lookup (digitToInt' n) rs) ++ replace' ts rs
>     where lookup n [] = ""
>           lookup n ((i,str):rs) = if (n == i) then str else lookup n rs
> replace' (t:ts) rs = t : replace' ts rs

> digitToInt' :: Char -> Int
> digitToInt' c = fst' $ filter (\(ch,_) -> ch == upperCh) digitMap
>     where fst' [] = error "CGITools.digitToInt' : can't convert digit."
>           fst' ((ch,n):as) = n
>           upperCh = toUpper c

> digitMap :: [(Char,Int)]
> digitMap = [('0',0),('1',1),('2',2),('3',3),('4',4),('5',5),('6',6),('7',7),('8',8),('9',9),
>             ('A',10),('B',11),('C',12),('D',13),('E',14),('F',15),('G',16),('H',17),('I',18),('J',19),
>             ('K',20),('L',21),('M',22),('N',23),('O',24),('P',25),('Q',26),('R',27),('S',28),('T',29),
>             ('U',30),('V',31),('W',32),('X',33),('Y',34),('Z',35)]

> convertnl [] = []
> convertnl ('\r':'\n':xs) = '\n':convertnl xs
> convertnl (x:xs)         = x:convertnl xs

> showOption sel name = "<OPTION" ++ (if sel then " SELECTED" else "") ++ ">" ++ name ++ "</OPTION>"

> showOptions n sel []           = []
> showOptions n sel (name:names) = showOption (n==sel) name ++ showOptions (n+1) sel names

> ppHTML = ppHTML' 0
> ppHTML' i ('<':xs) = "&lt;" ++ ppHTML' i xs
> ppHTML' 0 ('>':xs) = "<PRE>\n" ++ ppHTML' 1 ("&gt;" ++ xs)
> ppHTML' 0 (x:xs)   = "<B>" ++ ppHTML' 3 (x:xs)
> ppHTML' 1 ('\n':xs) = "\n" ++ ppHTML' 2 xs
> ppHTML' 1 ('>':xs) = "&gt;" ++ ppHTML' 1 xs
> ppHTML' 1 (x:xs)    = x : ppHTML' 1 xs
> ppHTML' 2 ('>':xs)  = "&gt;" ++ ppHTML' 1 xs
> ppHTML' 2 (' ':xs)  = "\n" ++ ppHTML' 2 xs
> ppHTML' 2 ('\n':xs) = "\n" ++ ppHTML' 2 xs
> ppHTML' 2 (x:xs)    = "</PRE>\n<B>" ++ ppHTML' 3 (x:xs) 
> ppHTML' 3 ('\n':xs) = "<BR>\n" ++ ppHTML' 4 xs
> ppHTML' 3 ('>':xs) = "&gt;" ++ ppHTML' 3 xs
> ppHTML' 3 (x:xs)    = x: ppHTML' 3 xs
> ppHTML' 4 ('>':xs)  = "</B>\n<PRE>\n" ++ ppHTML' 1 ("&gt;" ++ xs) 
> ppHTML' 4 ('\n':xs) = "<BR>\n" ++ ppHTML' 4 xs
> ppHTML' 4 (x:xs)    = x: ppHTML' 3 xs
> ppHTML' n _         = if n == 1 || n == 2 then "</PRE>\n" else "</B>\n"


> convertURL []        = []
> convertURL (' ':xs)  = "+" ++ convertURL xs
> convertURL ('>':xs)  = "%3E" ++ convertURL xs
> convertURL ('<':xs)  = "%3C" ++ convertURL xs
> convertURL ('=':xs)  = "%3D" ++ convertURL xs
> convertURL ('!':xs)  = "%21" ++ convertURL xs
> convertURL (',':xs)  = "%2C" ++ convertURL xs
> convertURL ('+':xs)  = "%2B" ++ convertURL xs
> convertURL ('~':xs)  = "%7E" ++ convertURL xs
> convertURL ('|':xs)  = "%7C" ++ convertURL xs
> convertURL ('(':xs)  = "%28" ++ convertURL xs
> convertURL (')':xs)  = "%29" ++ convertURL xs
> convertURL ('\n':xs) = "%0D%0A" ++ convertURL xs
> convertURL ('[':xs)  = "%5B" ++ convertURL xs
> convertURL (']':xs)  = "%5D" ++ convertURL xs
> convertURL ('"':xs)  = "%22" ++ convertURL xs
> convertURL ('\\':xs) = "%5C" ++ convertURL xs
> convertURL ('{':xs)  = "%7B" ++ convertURL xs
> convertURL ('}':xs)  = "%7D" ++ convertURL xs
> convertURL (':':xs)  = "%3A" ++ convertURL xs
> convertURL ('@':xs)  = "%40" ++ convertURL xs
> convertURL (';':xs)  = "%3B" ++ convertURL xs
> convertURL ('/':xs)  = "%2F" ++ convertURL xs
> convertURL ('?':xs)  = "%3F" ++ convertURL xs
> convertURL ('\'':xs) = "%27" ++ convertURL xs
> convertURL ('`':xs)  = "%60" ++ convertURL xs
> convertURL ('&':xs)  = "%26" ++ convertURL xs
> convertURL ('^':xs)  = "%5E" ++ convertURL xs
> convertURL ('%':xs)  = "%25" ++ convertURL xs
> convertURL ('$':xs)  = "%24" ++ convertURL xs
> convertURL ('#':xs)  = "%23" ++ convertURL xs
> convertURL (x:xs)    = x : convertURL xs




Bekommt den ADP-Code als String und liefert
ein Tripel zurueck: ADP-Code als HTML, die benoetigten
Funktionsdefinitionen, und eine Liste von kleinen Code-Fragmenten,
die bei Bedarf in einem gesonderten Fenster angezeigt werden sollen.

> preprocessCode :: String -> (String, String, [String])
> preprocessCode inp = (htmlResult, scriptCode, htmlBlks')
>     where blks = splitCode inp
>           htmlBlks = let mkHTML (header,litScrpt,dim) = ("<B>" ++ header ++ "</B>\n" ++ (link header) ++
>                                              "\n\n<PRE>\n" ++ litScrpt ++ "\n</PRE>\n", dim)
>                              where link header = if (lineEmpty header)
>                                                     || (toLowerCase header) `contains` "bind input"
>                                                     || (toLowerCase header) `contains` "haskell header" then "" else "###1"
>                      in map mkHTML blks
>           htmlResult = concatMap (\ (n,(blk,(w,h))) -> replace' blk [(1,jsLink n w h)]) $ zip [0..] htmlBlks 
>           jsLink n w h = "<a href=\"javascript:OpenCode" ++
>                          "('" ++ (show n) ++ "','" ++ (show w) ++ "','" ++ (show h) ++ "')\">" ++
>                          "<img src=\"/adp/images/openwin.jpg\" border=\"0\"" ++
>                          "alt=\"Open source code in new window\"></a>"
>           scriptCode = "<!--\nfunction OpenCode" ++ "(x,w,h) {\n" ++
>                        "CodeWindow = window.open(\"/cgi-bin/adp_###9?showCode=\"+x" ++
>                        ", \"adpCode_\"+x" ++
>                        ", \"width=\"+w+\",height=\"+h+\",resizable=yes,scrollbars=yes\");"++
>                        "CodeWindow.focus();}\n//-->\n"
>           htmlBlks' = map (\ (blk,_) -> replace' blk []) htmlBlks 


> splitCode :: String -> [(String,String,(Int,Int))]
> splitCode inp = splitCode' $ lines inp
> splitCode' []  = []
> splitCode' inp = (prepare $ unlines header, prepare $ unlines litScrpt, calcDim litScrpt):blks
>     where (header,rest) = splitWhile (not . startsWith '>') inp
>           (litScrpt,rest') = splitWhile (\ls -> startsWith '>' ls || lineEmpty ls) rest
>           blks = splitCode' rest'
>           prepare = replaceWith '>' "&gt" . replaceWith '<' "&lt"

>           calcDim :: [String] -> (Int, Int)
>           calcDim inp  = (min ((+) 100 $ (*) 7 $ maximum $ map length inp) 1000, min ((+) 110 $ (*) 18 $ length inp) 700)


> splitWhile :: (a -> Bool) -> [a] -> ([a],[a])
> splitWhile f [] = ([],[])
> splitWhile f (x:xs) = if f x then (x:splt,rest) else ([],(x:xs))
>     where (splt,rest) = splitWhile f xs


> startsWith :: Char -> String  -> Bool
> startsWith c []      = False
> startsWith c (x:xs)  = x==c


> contains :: String -> String -> Bool
> contains [] _ = False
> contains inp@(x:xs) s = if inp `startsWith` s then True else contains xs s
>     where startsWith _ [] = True
>           startsWith [] _  = False
>           startsWith (x:xs) (y:ys) = if x == y then startsWith xs ys else False


> replaceWith :: Char -> String -> String -> String
> replaceWith c str []     = []
> replaceWith c str (x:xs) | x == c    = str ++ replaceWith c str xs
>                          | otherwise = x : replaceWith c str xs


> toLowerCase :: String -> String
> toLowerCase = map toLower


> isWhiteSpace :: Char -> Bool
> isWhiteSpace ch = elem ch [' ', '\n', '\r', '\t']


> lineEmpty :: String -> Bool
> lineEmpty = lineEmpty' . filter (not . isWhiteSpace)
>    where lineEmpty' "" = True
>          lineEmpty' _  = False


> mapSep sep inp = foldr1 (\a b -> a++sep++b) inp


> testPP f =  do
>           inp <- readFile f
>           putStrLn (ppHTML inp)

> testSize input alg limit ret = case (length input) > limit of
>             True -> "To save webserver and bandwidth resources the \n" ++
>                     "input length for algebra \"" ++ alg ++ "\" is limited to "++ show limit ++".\n" ++
>                     "Please choose a shorter input or download the\n" ++
>                     "sources to try it on your local machine."
>             False -> shorten ret

> shorten inp = shorten' 0 inp where
>   limit =  10000                 -- enter limit here !!
>   shorten' _ []     = []
>   shorten' n (x:xs) | n > limit = ".....\n\n" ++ 
>                                   "To save webserver and bandwidth resources the \n" ++
>                                   "program output is limited to "++ show limit ++" characters.\n" ++
>                                   "To see the complete output, please download the sources\n"++
>                                   "to try it on your local machine."
>                     | otherwise = x:shorten' (n+1) xs

