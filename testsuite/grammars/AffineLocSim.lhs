
Algebra type:

> #algebratype{
  
> type AffineLocsim_Algebra alphabet answer = (
>   (Int, Int) -> answer,                     -- nil
>   alphabet -> answer -> answer,             -- d
>   answer -> alphabet -> answer,             -- i
>   alphabet -> answer -> alphabet -> answer, -- r
>   alphabet -> answer -> answer,             -- dx
>   answer -> alphabet -> answer,             -- ix
>   answer -> alphabet -> answer, -- skipr
>   alphabet -> answer -> answer, -- skipl 
>   [answer] -> [answer]                      -- h
>   )

> }


> #algebra[count]{
  
> count :: AffineLocsim_Algebra Char Int
> count = (nil, d, i, r, dx, ix, skip_right, skip_left, h) where
>    nil a   = 1
>    d x s   = s
>    i s y   = s
>    r a s b = s
>    dx x s  = s
>    ix s y  = s
>    skip_right a b = a
>    skip_left  a b = b
>    h l     = [sum l]

> }

Affine gap score algebra:

> #algebra[affine]{

> affine :: AffineLocsim_Algebra Char Int
> affine = (nil, d, i, r, dx, ix, skip_right, skip_left, h) where
>    nil a   = 0
>    d x s   = s + (-15) + (-1)
>    i s y   = s + (-15) + (-1)
>    r a s b = if a==b then (s+4) else (s-3)
>    dx x s  = s + (-1)
>    ix s y  = s + (-1)
>    skip_right a b = a
>    skip_left  a b = b
>    h l     = [maximum l]

> }

The yield grammar:

> #grammar{

> affinelocsim alg f = axiom skipR where
>   (nil, d, i, r, dx, ix, skip_right, skip_left,  h) = alg


>   skipR     = skip_right <<<           skipR ~~- achar |||
>               skipL                                    ... h

>   skipL     = skip_left  <<< achar -~~ skipL           |||
>               alignment                                 ... h

>   alignment  = tabulated(
>                nil <<< astring                        |||
>                d   <<< achar  -~~ xDel                |||
>                i   <<<            xIns      ~~- achar |||
>                r   <<< achar  -~~ alignment ~~- achar ... h)

>   xDel      = tabulated (
>                alignment              |||
>                dx <<< achar  -~~ xDel ... h )

>   xIns      = tabulated (
>                alignment             |||
>                ix <<< xIns ~~- achar ... h )

> }
