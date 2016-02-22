
Algebra type:

> #algebratype{

> type Zuker_Algebra alph answer = (
>   answer -> answer,					  -- str
>   alph -> (Int, Int) -> alph -> answer,			  -- hl
>   alph -> answer -> answer -> alph -> answer,		  -- bi
>   alph -> answer -> alph -> answer,			  -- sr
>   alph -> (Int, Int) -> answer -> alph -> answer,		  -- bulgeL
>   alph -> answer -> (Int, Int) -> alph -> answer,		  -- bulgeR
>   alph -> (Int, Int) -> answer -> (Int, Int) -> alph -> answer,   -- il
>   alph -> answer -> answer,				  -- openL
>   answer -> alph -> answer,				  -- openR
>   answer -> answer -> answer,				  -- branch
>   [answer] -> [answer]            		          -- h
>   ) 

> }

> #extern dots :: a -> String

energy

> #algebra[prettyprint]{

> prettyprint :: Zuker_Algebra Char  String
> prettyprint = (str,hl,bi,sr,bulgeL,bulgeR,il,openL,openR,branch,h) where
>   str s                    = s
>   hl _ (h1,h2) _           = "("++ dots (h2-h1) ++ ")"
>   bi _ s1 s2 _             = "("++ s1 ++ s2 ++ ")"
>   sr _ s _                 = "("++ s ++ ")"
>   bulgeL _   (l1,l2) s _   = "("++ dots (l2-l1) ++ s ++ ")"
>   bulgeR _ s (r1,r2)   _   = "("++ s ++ dots (r2-r1) ++ ")"
>   il _ (l1,l2) s (r1,r2) _ = "("++ dots (l2-l1) ++ s ++ dots (r2-r1) ++ ")"
>   openL _ s                = "." ++ s
>   openR s _                = s ++ "."
>   branch s1 s2             = s1 ++ s2
>   h xs                     = [id xs]

> }

Counting algebra:

> #algebra[count]{

> count :: Zuker_Algebra Char  Int
> count = (str,hl,bi,sr,bulgeL,bulgeR,il,openL,openR,branch,h) where
>    str    x       = x
>    hl     _ _ _   = 1
>    bi     _ x y _ = x*y
>    sr     _ x _   = x
>    bulgeL _ _ x _ = x
>    bulgeR _ x _ _ = x
>    il   _ _ x _ _ = x
>    openL  _ x     = x
>    openR  x _     = x
>    branch x y     = x*y
>    h xs           = [sum xs]

> }

Base Pair Algebra:

> #algebra[pairmax]{

> pairmax :: Zuker_Algebra Char  Int
> pairmax = (str,hl,bi,sr,bulgeL,bulgeR,il,openL,openR,branch,h) where
>    str    x       = x
>    hl     _ _ _   = 1
>    bi     _ x y _ = x + y + 1
>    sr     _ x _   = x + 1
>    bulgeL _ _ x _ = x + 1
>    bulgeR _ x _ _ = x + 1
>    il   _ _ x _ _ = x + 1
>    openL  _ x     = x
>    openR  x _     = x
>    branch x y     = x + y
>    h xs           = [maximum xs]

> }

Stacking Base Pair Algebra:

> #algebra[stackmax]{

> stackmax :: Zuker_Algebra Char  Int
> stackmax = (str,hl,bi,sr,bulgeL,bulgeR,il,openL,openR,branch,h) where
>    str    x       = x
>    hl     _ _ _   = 0
>    bi     _ x y _ = x + y
>    sr     _ x _   = x + 1
>    bulgeL _ _ x _ = x 
>    bulgeR _ x _ _ = x 
>    il   _ _ x _ _ = x 
>    openL  _ x     = x
>    openR  x _     = x
>    branch x y     = x + y
>    h xs           = [maximum xs]

> }


The yield grammar:

> #extern minloopsize  :: Filter

> #grammar{

> zuker81 alg inp = axiom struct where
>   (str,hl,bi,sr,bulgeL,bulgeR,il,openL,openR,branch,h) = alg
>   
>   struct = str <<< w
> 
>   v = (hairpin ||| twoedged ||| bifurcation) `with` basepairing ... h
>   
>   hairpin     = hl <<< base -~~ (region `with` minloopsize) ~~- base
>   bifurcation = bi <<< base -~~ w +~+ w ~~- base ... h
>   twoedged    = stack ||| bulgeleft ||| bulgeright ||| interior ... h
>   
>   stack      = sr     <<< base -~~            v            ~~- base
>   bulgeleft  = bulgeL <<< base -~~ region +~+ v            ~~- base
>   bulgeright = bulgeR <<< base -~~            v +~+ region ~~- base
>   interior   = il     <<< base -~~ region +~+ v +~+ region ~~- base
>  
>   w = tabulated (openleft ||| openright ||| v ||| connected ... h)
>   
>   openleft  = openL  <<< base -~~ w
>   openright = openR  <<< w    ~~- base
>   connected = branch <<< w    +~+ w    ... h

> }
