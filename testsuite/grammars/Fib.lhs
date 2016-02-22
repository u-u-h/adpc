
Algebra type:

> #algebratype{

> type Fib_Algebra alphabet answer = (
>   alphabet -> answer,                       -- f1
>   alphabet -> alphabet -> answer,           -- f2
>   answer -> answer,                         -- fn
>   answer -> alphabet -> answer,             -- fn1
>   answer -> alphabet -> alphabet -> answer, -- fn2
>   [answer] -> [answer]                      -- h
>   )

> }

> #algebra[prettyprint]{

> prettyprint :: Fib_Algebra Char String
> prettyprint = (f1, f2, fn, fn1, fn2, h) where
>   f1 a      = "F(1)"
>   f2 a b    = "F(2)"
>   fn x      = x
>   fn1 x a   = x 
>   fn2 x a b = x
>   h x       = [id x]

   h l       = [foldr1 (\x -> (x ++).('+' :)) l]

> }



Counting algebra: 

> #algebra[count]{

> count :: Fib_Algebra Char Integer
> count = (f1, f2, fn, fn1, fn2, h) where
>   f1 a      = 1
>   f2 a b    = 1
>   fn x      = x
>   fn1 x a   = x
>   fn2 x a b = x
>   h l       = [sum l]

> }

The yield grammar:

> #grammar{

> fibonacci alg f = axiom fib where

>   (f1, f2, fn, fn1, fn2, h) = alg

>   fib = tabulated(
>          f1 <<< achar           |||
>          f2 <<< achar ~~- achar |||
>          fn <<< fibn                `with` minsize 3)

>   fibn = fn1 <<< fib ~~- achar           |||
>          fn2 <<< fib ~~- achar ~~- achar ... h  

> }
