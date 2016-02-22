TODO:
------
- Aussagekraeftigere Fehlermeldungen
- val c d     = 1 gibt einen Haskell-Fehler:
    Program error: listToType of empty list
  => tritt immer dann auf, wenn in Algebrafunktion mehr Argumente
     verwendet werden, als deklariert wurden
- Zeilennummern auch in Grammatik moeglich? ==> besser als "in an
  argument of.." - Liste. Auch hilfreich: Angabe der Algebrafunktion,
  deren Anwendung nicht passt.
- Bei Fehlern in der Grammatik auch hinweis auf zuwenig/zuviele
  argumente geben
- Produktion
   formula = tabulated (
             number |||
             add  <<< formula  ~~-  formula   ~~~  formula  |||
             mult <<< formula  ~~-  times  ~~~  formula  ... h)
  wird nicht als Fehler erkannt.
- Ebenfalls nicht erkannt wird:
   formula = tabulated (
             number |||
             add  <<< formula  ~~-  plus   ~~~  times |||
             mult <<< formula  ~~-  times  ~~~  formula  ... h)
- Wuchty98 faelschliche Fehlermeldung in Prod. weak


#import stdlib.adp

Algebra type:

> #algebratype{

> type Bill_Algebra alphabet answer = (
>   alphabet -> answer,                       -- val
>   answer -> alphabet -> answer,             -- ext
>   answer -> alphabet -> answer -> answer,   -- add
>   answer -> alphabet -> answer -> answer,   -- mult
>   [answer] -> [answer]                      -- h
>   ) 

> }


Counting algebra: 

> #algebra[count]{

> count :: Bill_Algebra Char Int
> count = (val, ext, add, mult, h) where
>   val c       = 1 
>   ext n c     = 1
>   add  x t y  = x * y
>   mult x t y  = x * y
>   h l         = [sum l]

> }


> #algebra[prettyprint]{
> 
> prettyprint :: Bill_Algebra Char String 
> prettyprint = (val, ext, add, mult, h) where
>   val   c     = c 
>   ext   n c   = n ++ c 
>   add   x c y = "(" ++ x ++ c ++ y ++ ")" 
>   mult  x c y = "(" ++ x ++ c ++ y ++ ")" 
>   h x         = [id x]
> 
> }


The buyer's algebra:

> #algebra[buyer]{

> buyer :: Bill_Algebra Char Int
> buyer = (val, ext, add, mult, h) where
>   val c         = decode c
>   ext n c       = 10*n + decode c
>   add  x t y    = x + y
>   mult x t y    = x * y
>   h l           = [minimum l]

> }

The seller's algebra:

> #algebra[seller]{

> seller :: Bill_Algebra Char Int
> seller = (val, ext, add, mult, h) where
>   val c       = decode c
>   ext n c     = 10*n + decode c 
>   add  x c y  = x + y
>   mult x c y  = x * y
>   h l         = [maximum l]




> }

The yield grammar:


> #grammar{

> bill alg f = axiom formula where
>   (val, ext, add, mult, h) = alg

>   formula = tabulated (
>             number |||
>             add  <<< formula  ~~-  plus   ~~~  formula |||
>             mult <<< formula  ~~-  times  ~~~  formula  ... h)

>   number = tabulated(
>             val <<< digit ||| ext <<< number ~~- digit ... h)
>   digit  = char '0' ||| char '1' ||| char '2' ||| char '3' |||
>            char '4' ||| char '5' ||| char '6' ||| char '7' |||
>            char '8' ||| char '9'

>   plus  = char '+'
>   times = char '*'

> }
