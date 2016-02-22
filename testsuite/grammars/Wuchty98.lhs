
Algebra type:

> #algebratype{

> type Algebra alph answer = (

>    answer -> answer,                      -- str
>    (Int, Int) -> answer,                      -- ss
>    alph -> (Int, Int) -> alph -> answer,      -- hl
>    alph -> answer -> alph -> answer,      -- sr
>    (Int, Int) -> answer -> answer,            -- bl
>    answer -> (Int, Int) -> answer,            -- br
>    (Int, Int) -> answer -> (Int, Int) -> answer,  -- il
>    alph -> answer -> alph -> answer,      -- ml
>    (Int, Int) -> answer -> answer,            -- blk
>    alph -> answer,                          -- nil
>    answer -> answer -> answer,            -- cons
>    answer -> answer,                      -- ul
>    [answer] -> [answer])                  -- h


energy

> }

> #extern dots :: a -> String

> #algebra[pretty]{

> pretty :: Algebra Char String
> pretty = (str, ss, hl, sr, bl, br, il, ml, blk, nil, cons, ul, h) where
>    str s            = s
>    ss r             = dots r
>    hl _ r _         = "("++ dots r ++ ")"
>    sr _   s   _     = "("++      s ++ ")"
>    bl  r s          = dots r ++ s
>    br s r           = s ++ dots r
>    il r1 s r2       = dots r1 ++ s ++ dots r2
>    ml _   s   _     = "("++      s     ++ ")"
>    blk r s          = dots r ++ s
>    nil _            = " "
>    cons a b         = a ++ b
>    ul s             = s
>    h xs             = [id xs]

    h xs     = minimum xs


> }


Shape algebra:

Count algebra:

> #algebra[count]{

> count :: Algebra Char Int
> count = (str, ss, hl, sr, bl, br, il, ml, blk, nil, cons, ul, h) where
>    str a = a
>    ss a = 1
>    hl a b c = 1
>    sr a b c = b
>    bl a b = b
>    br a b = a
>    il a b c = b
>    ml a b c = b
>    blk a b = b
>    nil a = 1
>    cons a b = a * b
>    ul a = a
>    h xs = [sum xs]

> }

Basepair maximization algebra:

> #algebra[bpmax]{

> bpmax :: Algebra Char Int
> bpmax = (str, ss, hl, sr, bl, br, il, ml, blk, nil, cons, ul, h) where
>    str a    = a
>    ss _     = 0
>    hl _ _ _ = 1
>    sr _ s _ = s + 1
>    bl _ s   = s
>    br s _   = s
>    il _ s _ = s
>    ml _ s _ = s + 1
>    blk _ s  = s
>    nil _    = 0
>    cons a b = a + b
>    ul s     = s
>    h xs     = [maximum xs]

> }


Stacking basepair maximization algebra:

> #algebra[spmax]{

> spmax :: Algebra Char Int
> spmax = (str, ss, hl, sr, bl, br, il, ml, blk, nil, cons, ul, h) where
>    str a    = a
>    ss _     = 0
>    hl _ _ _ = 0
>    sr _ s _ = s + 1
>    bl _ s   = s - 1
>    br s _   = s - 1
>    il _ s _ = s - 1
>    ml _ s _ = s + 1
>    blk _ s  = s 
>    nil _    = 0
>    cons a b = a + b
>    ul s     = s
>    h xs     = [maximum xs]

> }


The yield grammar:

> #grammar{

> wuchty98 alg inp = axiom struct where
>  (str,ss,hl,sr,bl,br,il,ml,blk,nil,cons,ul,h) = alg

>  struct       =  str  <<< comps                            ||| 
>                  str  <<< (ul  <<< singlestrand)           |||
>                  str  <<< (nil <<< empty)                  ... h

>  block        = tabulated(
>                           strong                           ||| 
>                  blk  <<< region ~~~ strong                ... h)

>  comps        = tabulated(
>                  cons <<< block  ~~~ comps                 ||| 
>                  ul   <<< block                            |||
>                  cons <<< block  ~~~ (ul <<< singlestrand) ... h)

>  singlestrand =  ss   <<< region

>  strong       = tabulated(
>                  (sr  <<< base -~~ strong  ~~- base |||
>                   sr  <<< base -~~  weak   ~~- base)
>                       `with` basepairing                   ... h)
>  weak         = tabulated(
>                   (hl <<< base -~~          region3             ~~- base  |||
>                    sr <<< base -~~ (bl   <<< region ~~~ strong) ~~- base  |||
>                    sr <<< base -~~ (br   <<< strong ~~~ region) ~~- base  ||| 
>                    ml <<< base -~~ (cons <<< block  ~~~ comps ) ~~- base  |||
>                    sr <<< base -~~ (il   <<< region ~~~ strong  ~~~ region) ~~- base) 
>                       `with` basepairing                    ... h) 

>  region3 = region `with` (minsize 3)

> }
