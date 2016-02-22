

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
>   h l         = sum l

> }


> #algebra[prettyprint]{
> 
> prettyprint :: Bill_Algebra Char String 
> prettyprint = (val, ext, add, mult, h) where
>   val   c     = c 
>   ext   n c   = n ++ c 
>   add   x c y = "(" ++ x ++ c ++ y ++ ")" 
>   mult  x c y = "(" ++ x ++ c ++ y ++ ")" 
>   h x         = id x
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
>   h l           = minimum l

> }

The seller's algebra:

> #algebra[seller]{

> seller :: Bill_Algebra Char Int
> seller = (val, ext, add, mult, h) where
>   val c       = decode c
>   ext n c     = 10*n + decode c 
>   add  x c y  = x + y
>   mult x c y  = x * y
>   h l         = maximum l




> }

The yield grammar:


> #grammar{

> bill alg f = axiom formula where
>   (val, ext, add, mult, h) = alg

>   tail1  = listed(
>      sadd <<< lbase   +~~ tail1 |||
>      cadd <<< motif1 ~~~ tail2 ... h )
>   motif1 = s1
>   s1 = hairpin
>
>   tail2  = listed(
>      sadd <<< lbase   +~~ tail2 |||
>      cadd <<< motif2 ~~~ tail5 ... h )
>   motif2 = s2
>   s2 = is <<< loc .~~ slstem3 ~~. loc
>   slstem3 = tabulated (
>       (sr <<< lbase +~~ slstem3 ~~+ lbase) `with` (pairing base) |||
>       (br <<< lbase +~~ lbase ++~ s3 ~~~ region ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (bl <<< lbase +~~ lbase ++~ region ~~~ s3 ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (il <<< lbase +~~ lbase ++~  (region `with` (maxsize 32)) ~~~ s3 ~~~
>         (region `with` (maxsize 30)) ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>   s3 = is <<< loc .~~ slstem4 ~~. loc
>   slstem4 = tabulated (
>       (sr <<< lbase +~~ slstem4 ~~+ lbase) `with` (pairing base) |||
>       (br <<< lbase +~~ lbase ++~ s4 ~~~ region ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (bl <<< lbase +~~ lbase ++~ region ~~~ s4 ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (il <<< lbase +~~ lbase ++~  (region `with` (maxsize 32)) ~~~ s4 ~~~
>         (region `with` (maxsize 30)) ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>   s4 = hairpin
>
>   tail5  = listed(
>      sadd <<< lbase   +~~ tail5 |||
>      cadd <<< motif5 ~~~ tail6 ... h )
>   motif5 = s5
>   s5 = hairpin
>
>   tail6  = listed(
>      sadd <<< lbase   +~~ tail6 |||
>      cadd <<< motif6 ~~~ tail11 ... h )
>   motif6 = s6
>   s6 = is <<< loc .~~ mlstem7 ~~. loc
>   mlstem7  = tabulated (
>       (sr <<< lbase +~~ mlstem7  ~~+ lbase) `with` (pairing base) |||
>       (ml <<< lbase +~~ lbase ++~ ml_tail7 ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>   
>   ml_tail7 =  tabulated (
>                            sadd <<< lbase      +~~ ml_tail7 |||
>                            cadd <<< ml_motif7 ~~~ ml_tail9 ... h )
>   ml_motif7 = mlpen <<< hairpin
>   ml_tail9 = tabulated(
>                            sadd  <<< lbase  +~~ ml_tail9       |||
>                                      ml_motif10            |||
>                            addss <<<(ml_motif10)  ~~~ region ... h)
>   ml_motif10 = mlpen <<< hairpin
>   
>
>   tail11  = listed(
>      sadd <<< lbase   +~~ tail11 |||
>      cadd <<< motif11 ~~~ tail12 ... h )
>   motif11 = s11
>   s11 = hairpin
>
>   tail12  = listed(
>      sadd <<< lbase   +~~ tail12 |||
>      cadd <<< motif12 ~~~ tail15 ... h )
>   motif12 = s12
>   s12 = is <<< loc .~~ slstem13 ~~. loc
>   slstem13 = tabulated (
>       (sr <<< lbase +~~ slstem13 ~~+ lbase) `with` (pairing base) |||
>       (br <<< lbase +~~ lbase ++~ s13 ~~~ region ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (bl <<< lbase +~~ lbase ++~ region ~~~ s13 ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (il <<< lbase +~~ lbase ++~  (region `with` (maxsize 32)) ~~~ s13 ~~~
>         (region `with` (maxsize 30)) ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>   s13 = is <<< loc .~~ slstem14 ~~. loc
>   slstem14 = tabulated (
>       (sr <<< lbase +~~ slstem14 ~~+ lbase) `with` (pairing base) |||
>       (br <<< lbase +~~ lbase ++~ s14 ~~~ region ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (bl <<< lbase +~~ lbase ++~ region ~~~ s14 ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (il <<< lbase +~~ lbase ++~  (region `with` (maxsize 32)) ~~~ s14 ~~~
>         (region `with` (maxsize 30)) ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>   s14 = hairpin
>
>   tail15  = listed(
>    sadd  <<< lbase  +~~ tail15  |||
>    addss <<< motif15 ~~~ region |||
>    ul    <<< motif15            ... h)
>   motif15 = s15
>   s15 = is <<< loc .~~ mlstem16 ~~. loc
>   mlstem16  = tabulated (
>       (sr <<< lbase +~~ mlstem16  ~~+ lbase) `with` (pairing base) |||
>       (ml <<< lbase +~~ lbase ++~ ml_tail16 ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>   
>   ml_tail16 =  tabulated (
>        sadd <<< lbase      +~~ ml_tail16 |||
>        cadd <<< ml_motif16 ~~~ ml_tail28 ... h )
>   ml_motif16 = mlpen <<< is <<< loc .~~ mlstem18 ~~. loc
>   mlstem18  = tabulated (
>        (sr <<< lbase +~~ mlstem18  ~~+ lbase) `with` (pairing base) |||
>        (ml <<< lbase +~~ lbase ++~ ml_tail18 ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>    
>   ml_tail18 =  tabulated (
>               sadd <<< lbase      +~~ ml_tail18 |||
>               cadd <<< ml_motif18 ~~~ ml_tail26 ... h )
>   ml_motif18 = mlpen <<< is <<< loc .~~ mlstem20 ~~. loc
>   mlstem20  = tabulated (
>               (sr <<< lbase +~~ mlstem20  ~~+ lbase) `with` (pairing base) |||
>               (ml <<< lbase +~~ lbase ++~ ml_tail20 ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>           
>   ml_tail20 =  tabulated (
>                            sadd <<< lbase      +~~ ml_tail20 |||
>                            cadd <<< ml_motif20 ~~~ ml_tail22 ... h )
>   ml_motif20 = mlpen <<< hairpin
>   ml_tail22 = tabulated(
>                            sadd <<< lbase      +~~ ml_tail22 |||
>                            cadd <<< ml_motif22 ~~~ ml_tail24 ... h )
>   ml_motif22 = mlpen <<< hairpin
>   ml_tail24 = tabulated(
>                            sadd  <<< lbase  +~~ ml_tail24       |||
>                                      ml_motif25            |||
>                            addss <<<(ml_motif25)  ~~~ region ... h)
>   ml_motif25 = mlpen <<< hairpin
>           
>   ml_tail26 = tabulated(
>               sadd  <<< lbase  +~~ ml_tail26       |||
>                         ml_motif27            |||
>               addss <<<(ml_motif27)  ~~~ region ... h)
>   ml_motif27 = mlpen <<< hairpin
>    
>   ml_tail28 = tabulated(
>        sadd <<< lbase      +~~ ml_tail28 |||
>        cadd <<< ml_motif28 ~~~ ml_tail30 ... h )
>   ml_motif28 = mlpen <<< hairpin
>   ml_tail30 = tabulated(
>        sadd <<< lbase      +~~ ml_tail30 |||
>        cadd <<< ml_motif30 ~~~ ml_tail32 ... h )
>   ml_motif30 = mlpen <<< hairpin
>   ml_tail32 = tabulated(
>        sadd  <<< lbase  +~~ ml_tail32       |||
>                  ml_motif33            |||
>        addss <<<(ml_motif33)  ~~~ region ... h)
>   ml_motif33 = mlpen <<< is <<< loc .~~ slstem34 ~~. loc
>   slstem34 = tabulated (
>        (sr <<< lbase +~~ slstem34 ~~+ lbase) `with` (pairing base) |||
>        (br <<< lbase +~~ lbase ++~ s34 ~~~ region ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>        (bl <<< lbase +~~ lbase ++~ region ~~~ s34 ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>        (il <<< lbase +~~ lbase ++~  (region `with` (maxsize 32)) ~~~ s34 ~~~
>          (region `with` (maxsize 30)) ~~+ lbase ~~+ lbase) `with` (pairing stack) ... h)
>   s34 = hairpin
>   
>
>   hairpin = is <<< loc.~~ hairpin' ~~. loc
>   hairpin' = tabulated(
>       (hl <<< lbase +~~ lbase ++~ (region `with` (minsize 3)) ~~+ lbase ~~+ lbase) `with` (pairing stack) |||
>       (sr <<< lbase +~~ hairpin' ~~+ lbase) `with` (pairing base) ... h)
>   dummy = hl <<< base +~~ region ~~+ base ... h



> }
