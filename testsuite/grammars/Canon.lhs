> #import stdlib.adp



The signature:

Algebra type:


> #algebratype{

> type Canon_Algebra alphabet answer = (
>   Int -> answer,                                   -- nil
>   alphabet -> answer ->             answer, -- d
>               answer -> alphabet -> answer, -- i
>   alphabet -> answer -> alphabet -> answer, -- r
>   alphabet -> answer ->             answer, -- dx
>               answer -> alphabet -> answer, -- ix
>   [answer] -> [answer]                      -- h
>   )

> }

Enumeration algebra:


Pretty printing algebra:


Counting algebra:

> #algebra[count]{

> count :: Canon_Algebra Char Int
> count = (nil, d, i, r, dx, ix, h) where
>    nil _    = 1
>    d x s   = s
>    i s y   = s
>    r a s b = s
>    dx x s  = s
>    ix s y  = s

    h []    = []

>    h l     = [sum l]

> }

Affine gap score algebra:

> #algebra[affine]{

> affine :: Canon_Algebra Char Int
> affine = (nil, d, i, r, dx, ix, h) where
>    nil _    = 0
>    d x s   = s + (-15) + (-1)
>    i s y   = s + (-15) + (-1)
>    r a s b = if a==b then (4 + s) else (-3 + s)
>    dx x s  = s + (-1)
>    ix s y  = s + (-1)

h []    = []

>    h l     = [maximum l]


> }


The yield grammar:

> #grammar{

> canon_alignments alg inpX inpY = axiom alignment where
>   (nil, d, i, r, dx, ix, h) = alg
> 
>   alignment = tabulated (
>                match                          |||
>                d <<< base -~~ xDel           |||
>                i <<<           xIns ~~- base ... h )
> 
>   xDel      = tabulated (
>                match                           |||
>                dx <<< base -~~ xDel           |||
>                i  <<<           xIns ~~- base ... h )
> 
>   xIns      = tabulated (
>                match                 |||
>                ix <<< xIns ~~- base ... h )
> 
>   match     = tabulated (
>                nil <<< empty                         |||
>                r   <<< base -~~ alignment ~~- base ... h)

> }

