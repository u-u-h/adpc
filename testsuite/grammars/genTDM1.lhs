> #algebratype{

> type FS_Algebra base cmpl = (   
>            base -> cmpl -> cmpl ,  -- sadd
>            cmpl -> cmpl -> cmpl ,  -- cadd

>            base -> cmpl -> base -> cmpl, --sr
>            base -> base -> (Int,Int) -> base -> base         -> cmpl,   --hl
>            base -> base -> (Int,Int) -> cmpl -> base -> base -> cmpl,   --bl
>            base -> base -> cmpl -> (Int,Int) -> base -> base -> cmpl,   --br
>            base -> base -> (Int,Int) -> cmpl -> (Int,Int) -> base -> base -> cmpl, --il

>            base-> base->         cmpl        -> base-> base -> cmpl, --ml

>            base -> cmpl -> base -> cmpl, --dlr
>            cmpl -> cmpl -> cmpl,     --append
>            cmpl ->  cmpl,              -- ul
>            cmpl -> (Int,Int) -> cmpl,   -- addss
>            (Int,Int) -> cmpl -> cmpl,   -- ssadd

>            (Int,Int) -> cmpl ,              -- ss
>            cmpl   -> cmpl,           -- mlpen
>            Int -> cmpl,  --nil

>            [cmpl] -> [cmpl],  --h
>            [cmpl] -> [cmpl],  --h_l
>            [cmpl] -> [cmpl]  --h_s
>            )  

> }


> #algebra[energy]{

> energy :: FS_Algebra Int Int 
> energy  =  (sadd,cadd,sr,hl,bl,br, il, ml, dlr, append, ul, addss, ssadd, ss, mlpen, nil, h, h_l, h_s)

>     where
>       sadd  lb e = e
>       cadd  e1 e = e1 + e

>       sr     lb         e      rb      = e + sr_energy (lb,rb)
>       hl llb lb    _            rb rrb =     hl_energy (lb,rb)     + sr_energy (llb,rrb) 
>       bl llb lb   (i,j) e       rb rrb = e + bl_energy (lb,i,j,rb) + sr_energy (llb,rrb)
>       br llb lb e (i,j)         rb rrb = e + br_energy (lb,i,j,rb) + sr_energy (llb,rrb)
>       il llb lb   (i,j) e (k,l) rb rrb = e + sr_energy (llb,rrb) + il_energy (i,j,k,l)
>       ml llb lb         e       rb rrb = 380 +  e + sr_energy (llb,rrb) + termaupenalty (lb,rb) + dli_energy (lb,rb) + dri_energy (lb,rb)

>       dlr   dl e dr   = e + dl_energy (dl+1,dr) + dr_energy (dl+1,dr)+ termaupenalty ((dl+1),dr)

>       append  e1 e     = e1 + e
>       addss    e (i,j) =      e + ss_energy (i,j)
>       ul       e       = e

>       ssadd (i,j)  e   = 40 + e + ss_energy (i,j)
>       ss (i,j)         = 0
>       nil     _        = 0
>       mlpen e                = e + 40       

>       h   es = [minimum es]
>       h_l es = [minimum es]
>       h_s es = [minimum es] 

> }

> #algebra[pp]{

> pp :: FS_Algebra Int String
> pp  =  (sadd,cadd,sr,hl,bl,br, il, ml, dlr, append, ul, addss, ssadd, ss, mlpen, nil, h, h_l, h_s)
 
>     where
>       sadd  lb e = "." ++ e              
>       cadd  x  e = x ++ e

>       sr lb e rb = "(" ++ e ++ ")"       
>       hl llb lb    r    rb rrb = "((" ++ dots r ++"))"       
>       bl llb bl    x e  br rrb = "((" ++ dots x ++ e ++"))"
>       br llb bl e  x    br rrb = "((" ++ e ++ dots x ++"))"
>       il llb lb lr x rr rb rrb = "((" ++ dots lr  ++ x ++ dots rr ++ "))" 
>       ml llb bl    x    br rrb = "((" ++ x ++ "))" 

>       dlr   dl x dr =  x
>       append c1 c = c1 ++ c
>       ul  c1 = c1
>       addss  c1 r = c1 ++ dots r
>       ssadd  r x = dots r ++ x              

>       nil _ = ""
>       ss    r          = dots r         
>       mlpen x          = x        

>       h   es = [id es]
>       h_l es = [id es]
>       h_s es = [id es] 

> }

> #algebra[pf]{

> pf ::  FS_Algebra Int Double
> pf  =  (sadd,cadd,sr,hl,bl,br, il, ml, dlr, append, ul, addss, ssadd, ss, mlpen, nil, h, h_l, h_s)
 
>  where

>    sadd   lb q = scale(1) * q
>    cadd q1 q = q1 * q

>    hl llb lb (i,j)         rb rrb = scale(j-i+4)                   * mk_pf (sr_energy  (llb,rrb) + hl_energy  (lb,rb))
>    sr     lb       q       rb     = scale(2)                   * q * mk_pf (sr_energy  (lb,rb))
>    bl llb lb (i,j) q       rb rrb = scale(j-i +4)              * q * mk_pf (sr_energy  (llb,rrb) + bl_energy  (lb, i,j, rb))
>    br llb lb       q (i,j) rb rrb = scale(j-i +4)              * q * mk_pf (sr_energy  (llb,rrb) + br_energy  (lb, i,j, rb))
>    il llb lb (i,j) q (k,l) rb rrb = scale(j-i+ l-k +4)         * q * mk_pf (sr_energy  (llb,rrb) + il_energy  (i,j,k,l))
>    ml llb lb       q       rb rrb = scale(4) * q * mk_pf (380 + sr_energy (llb,rrb) + termaupenalty (lb,rb)+ dli_energy  (lb,rb) + dri_energy (lb,rb))

>    dlr dl   q dr   =  q * mk_pf (dl_energy  (dl+1,dr) + dr_energy (dl+1,dr) + termaupenalty ((dl+1),dr))
>    addss  q  (i,j)    = scale(j-i) * q * mk_pf (ss_energy (i,j))
>    ssadd     (i,j) q  = scale(j-i) * q * mk_pf (40  + ss_energy (i,j))
>    append q1       q2 = q1* q2
>    ul     q           = q 

>    mlpen  q           = q * mk_pf 40
>    ss     (i,j)       = scale(j-i) * mk_pf (ss_energy (i,j))
>    nil  _     = 1.0 

>    h xs   = [sum xs]
>    h_l xs = [sum xs]
>    h_s xs = [sum xs]

> }


> #grammar{

> gentdm alg f = axiom tail1 where
>   (sadd,cadd,sr,hl,bl,br, il, ml, dlr, append, ul, addss, ssadd, ss, mlpen, nil, h, h_l, h_s) = alg

>   (~~!)  = (*~~) 0 (1,30) 
>   (~~!!) = (~~*) (3,32) 0
>
>   tail1  = 
>        sadd  <<< base   +~~ tail1  |||
>        addss <<< motif1 ~~~ region |||
>        ul    <<< motif1            ... h_l
>   motif1 = s1
>   s1 = dlr <<< loc .~~ slstem1 ~~. loc
>   slstem1  = tabulated (
>               (bl <<< lbase +~~ lbase ++~ region30 ~~!! motif2              ~~+ lbase ~~+ lbase |||
>                br <<< lbase +~~ lbase ++~               motif2 ~~! region30 ~~+ lbase ~~+ lbase |||
>                il <<< lbase +~~ lbase ++~ region30 ~~!! motif2 ~~! region30 ~~+ lbase ~~+ lbase |||
>                sr <<< lbase +~~  motif2 ~~+ lbase)                        `with` stackpair       ...h)
>   motif2 =dlr <<< loc .~~ slstem2 ~~. loc
>   slstem2  = tabulated (
>               (bl <<< lbase +~~ lbase ++~ region30 ~~!! motif3              ~~+ lbase ~~+ lbase |||
>                br <<< lbase +~~ lbase ++~               motif3 ~~! region30 ~~+ lbase ~~+ lbase |||
>                il <<< lbase +~~ lbase ++~ region30 ~~!! motif3 ~~! region30 ~~+ lbase ~~+ lbase |||
>                sr <<< lbase +~~  motif3 ~~+ lbase)                        `with` stackpair       ...h)
>   motif3 =dlr <<< loc .~~ slstem3 ~~. loc
>   slstem3  = tabulated (
>               (bl <<< lbase +~~ lbase ++~ region30 ~~!! motif4              ~~+ lbase ~~+ lbase |||
>                br <<< lbase +~~ lbase ++~               motif4 ~~! region30 ~~+ lbase ~~+ lbase |||
>                il <<< lbase +~~ lbase ++~ region30 ~~!! motif4 ~~! region30 ~~+ lbase ~~+ lbase |||
>                sr <<< lbase +~~  motif4 ~~+ lbase)                        `with` stackpair       ...h)
>   motif4 =hairpin
>   region30 = region `with` (maxsize 30)
>
>   hairpin = dlr <<< loc.~~ hairpin' ~~.loc
>        where hairpin' = tabulated(
>               (hl <<< lbase +~~ lbase ++~ (region `with` (minsize 3)) ~~+ lbase ~~+ lbase |||
>                sr <<< lbase +~~ hairpin' ~~+ lbase)                `with` stackpair       ...h)

> }
