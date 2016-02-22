
Algebra type for algebras pf_sample and mfe:
--------------------------------------------------

> #algebratype{

> type Canonical_Algebra alph1 alph2 closed  =
>  (alph1 -> (closed, Int, Int) -> (closed, Int, Int),  --sadd
>   (closed, Int, Int) -> (closed, Int, Int) -> (closed, Int, Int), --cadd
>   (closed, Int, Int) -> alph1 -> (closed, Int, Int) -> (closed, Int, Int), --ambd
>   Int -> (closed, Int, Int),               --nil
>   alph1 -> (closed, Int, Int) -> (closed, Int, Int),  --edl
>   (closed, Int, Int) -> alph1 -> (closed, Int, Int),  --edr
>   alph1 -> (closed, Int, Int) -> alph1 -> (closed, Int, Int),  --edlr
>   (closed, Int, Int) -> (closed, Int, Int),                    --drem   
>   (closed, Int, Int) -> (closed, Int, Int),                    --is
>   alph1 -> (closed, Int, Int) -> alph1 -> (closed, Int, Int),  --sr
>   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> (closed, Int, Int),  --hl
>   alph1 -> alph1 -> (closed, Int, Int) -> alph1 -> alph1 -> (closed, Int, Int), --sp
>   alph2 -> (closed, Int, Int) -> (closed, Int, Int) ,                           --bl         
>   (closed, Int, Int) -> alph2 -> (closed, Int, Int),                            --br
>   alph2 -> (closed, Int, Int) -> alph2 -> (closed, Int, Int),                   --il
>   alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Int, Int),                   --ml
>   alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, Int),          --mldr
>   alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, Int),          --mladr
>   alph1 -> alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, Int), --mldlr
>   alph1 -> alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, Int), --mladlr
>   alph1 -> alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, Int), --mldladr
>   alph1 -> alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, Int), --mladldr
>   alph1 -> alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Int, Int),          --mldl
>   alph1 -> alph1 -> alph1 -> (closed, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Int, Int),          --mladl
>   (closed, Int, Int, Int, Int) -> alph2 -> (closed, Int, Int, Int, Int),  --addss
>   alph2 -> (closed, Int, Int) -> (closed, Int, Int, Int, Int),  --ssadd
>   (closed, Int, Int, Int, Int) -> (closed, Int, Int, Int, Int) -> (closed, Int, Int, Int, Int), --cons
>   (closed, Int, Int) -> (closed, Int, Int, Int, Int),                   --ul
>   (closed, Int, Int, Int, Int) -> (closed, Int, Int, Int, Int) -> (closed, Int, Int, Int, Int),  --combine
>   (closed, Int, Int, Int, Int) -> alph1 -> (closed, Int, Int, Int, Int) -> (closed, Int, Int, Int, Int), --acomb
>   [closed] -> [closed], --h
>   [closed] -> [closed], --h_i
>   [closed] -> [closed], --h_l
>   [closed] -> [closed]  --h_s
>  )

> }


Algebra type for algebra prob:
--------------------------------------------------


 type Canonical_Algebra alph1 alph2 closed  =
  (alph1 -> (closed, String, Int, Int) -> (closed, String, Int, Int),  --sadd
   (closed, String, Int, Int) -> (closed, String, Int, Int) -> (closed, String, Int, Int), --cadd
   (closed, String, Int, Int) -> alph1 -> (closed, String, Int, Int) -> (closed, String, Int, Int), --ambd
   Int -> (closed, String, Int, Int),               --nil
   alph1 -> (closed, String, Int, Int) -> (closed, String, Int, Int),  --edl
   (closed, String, Int, Int) -> alph1 -> (closed, String, Int, Int),  --edr
   alph1 -> (closed, String, Int, Int) -> alph1 -> (closed, String, Int, Int),  --edlr
   (closed, String, Int, Int) -> (closed, String, Int, Int),                    --drem   
   (closed, String, Int, Int) -> (closed, String, Int, Int),                    --is
   alph1 -> (closed, String, Int, Int) -> alph1 -> (closed, String, Int, Int),  --sr
   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> (closed, String, Int, Int),  --hl
   alph1 -> alph1 -> (closed, String, Int, Int) -> alph1 -> alph1 -> (closed, String, Int, Int), --sp
   alph2 -> (closed, String, Int, Int) -> (closed, String, Int, Int) ,                           --bl         
   (closed, String, Int, Int) -> alph2 -> (closed, String, Int, Int),                            --br
   alph2 -> (closed, String, Int, Int) -> alph2 -> (closed, String, Int, Int),                   --il
   alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, String, Int, Int),                   --ml
   alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, String, Int, Int),          --mldr
   alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, String, Int, Int),          --mladr
   alph1 -> alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, String, Int, Int), --mldlr
   alph1 -> alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, String, Int, Int), --mladlr
   alph1 -> alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, String, Int, Int), --mldladr
   alph1 -> alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, String, Int, Int), --mladldr
   alph1 -> alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, String, Int, Int),          --mldl
   alph1 -> alph1 -> alph1 -> (closed, String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, String, Int, Int),          --mladl
   (closed, String, Int, Int, Int, Int) -> alph2 -> (closed, String, Int, Int, Int, Int),  --addss
   alph2 -> (closed, String, Int, Int) -> (closed, String, Int, Int, Int, Int),  --ssadd
   (closed, String, Int, Int, Int, Int) -> (closed, String, Int, Int, Int, Int) -> (closed, String, Int, Int, Int, Int), --cons
   (closed, String, Int, Int) -> (closed, String, Int, Int, Int, Int),                   --ul
   (closed, String, Int, Int, Int, Int) -> (closed, String, Int, Int, Int, Int) -> (closed, String, Int, Int, Int, Int),  --combine
   (closed, String, Int, Int, Int, Int) -> alph1 -> (closed, String, Int, Int, Int, Int) -> (closed, String, Int, Int, Int, Int), --acomb
   [closed] -> [closed], --h
   [closed] -> [closed], --h_i
   [closed] -> [closed], --h_l
   [closed] -> [closed]  --h_s
   )



Algebra type for algebra prob2:
--------------------------------------------------

dalgebratype{

 type Canonical_Algebra alph1 alph2 closed  =
  (alph1 -> (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int),  --sadd
   (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int), --cadd
   (closed, Float,  String, Int, Int) -> alph1 -> (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int), --ambd
   Int -> (closed, Float,  String, Int, Int),               --nil
   alph1 -> (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int),  --edl
   (closed, Float,  String, Int, Int) -> alph1 -> (closed, Float,  String, Int, Int),  --edr
   alph1 -> (closed, Float,  String, Int, Int) -> alph1 -> (closed, Float,  String, Int, Int),  --edlr
   (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int),                    --drem   
   (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int),                    --is
   alph1 -> (closed, Float,  String, Int, Int) -> alph1 -> (closed, Float,  String, Int, Int),  --sr
   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int),  --hl
   alph1 -> alph1 -> (closed, Float,  String, Int, Int) -> alph1 -> alph1 -> (closed, Float,  String, Int, Int), --sp
   alph2 -> (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int) ,                           --bl         
   (closed, Float,  String, Int, Int) -> alph2 -> (closed, Float,  String, Int, Int),                            --br
   alph2 -> (closed, Float,  String, Int, Int) -> alph2 -> (closed, Float,  String, Int, Int),                   --il
   alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Float,  String, Int, Int),                   --ml
   alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int),          --mldr
   alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int),          --mladr
   alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int), --mldlr
   alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int), --mladlr
   alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int), --mldladr
   alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int), --mladldr
   alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Float,  String, Int, Int),          --mldl
   alph1 -> alph1 -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Float,  String, Int, Int),          --mladl
   (closed, Float,  String, Int, Int, Int, Int) -> alph2 -> (closed, Float,  String, Int, Int, Int, Int),  --addss
   alph2 -> (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int, Int, Int),  --ssadd
   (closed, Float,  String, Int, Int, Int, Int) -> (closed, Float,  String, Int, Int, Int, Int) -> (closed, Float,  String, Int, Int, Int, Int), --cons
   (closed, Float,  String, Int, Int) -> (closed, Float,  String, Int, Int, Int, Int),                   --ul
   (closed, Float,  String, Int, Int, Int, Int) -> (closed, Float,  String, Int, Int, Int, Int) -> (closed, Float,  String, Int, Int, Int, Int),  --combine
   (closed, Float,  String, Int, Int, Int, Int) -> alph1 -> (closed, Float,  String, Int, Int, Int, Int) -> (closed, Float,  String, Int, Int, Int, Int), --acomb
   [closed] -> [closed], --h
   [closed] -> [closed], --h_i
   [closed] -> [closed], --h_l
   [closed] -> [closed]  --h_s
   )

}

Algebra type for algebra probdb:
--------------------------------------------------

dalgebratype{

 type Canonical_Algebra alph1 alph2 closed  =
  (alph1 -> (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int),  --sadd
   (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int), --cadd
   (closed, Int, String, String, Int, Int) -> alph1 -> (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int), --ambd
   Int -> (closed, Int, String, String, Int, Int),               --nil
   alph1 -> (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int),  --edl
   (closed, Int, String, String, Int, Int) -> alph1 -> (closed, Int, String, String, Int, Int),  --edr
   alph1 -> (closed, Int, String, String, Int, Int) -> alph1 -> (closed, Int, String, String, Int, Int),  --edlr
   (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int),                    --drem   
   (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int),                    --is
   alph1 -> (closed, Int, String, String, Int, Int) -> alph1 -> (closed, Int, String, String, Int, Int),  --sr
   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int),  --hl
   alph1 -> alph1 -> (closed, Int, String, String, Int, Int) -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int), --sp
   alph2 -> (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int) ,                           --bl         
   (closed, Int, String, String, Int, Int) -> alph2 -> (closed, Int, String, String, Int, Int),                            --br
   alph2 -> (closed, Int, String, String, Int, Int) -> alph2 -> (closed, Int, String, String, Int, Int),                   --il
   alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int),                   --ml
   alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int),          --mldr
   alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int),          --mladr
   alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int), --mldlr
   alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int), --mladlr
   alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int), --mldladr
   alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int), --mladldr
   alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int),          --mldl
   alph1 -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> alph1 -> (closed, Int, String, String, Int, Int),          --mladl
   (closed, Int, String, String, Int, Int, Int, Int) -> alph2 -> (closed, Int, String, String, Int, Int, Int, Int),  --addss
   alph2 -> (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int, Int, Int),  --ssadd
   (closed, Int, String, String, Int, Int, Int, Int) -> (closed, Int, String, String, Int, Int, Int, Int) -> (closed, Int, String, String, Int, Int, Int, Int), --cons
   (closed, Int, String, String, Int, Int) -> (closed, Int, String, String, Int, Int, Int, Int),                   --ul
   (closed, Int, String, String, Int, Int, Int, Int) -> (closed, Int, String, String, Int, Int, Int, Int) -> (closed, Int, String, String, Int, Int, Int, Int),  --combine
   (closed, Int, String, String, Int, Int, Int, Int) -> alph1 -> (closed, Int, String, String, Int, Int, Int, Int) -> (closed, Int, String, String, Int, Int, Int, Int), --acomb
   [closed] -> [closed], --h
   [closed] -> [closed], --h_i
   [closed] -> [closed], --h_l
   [closed] -> [closed]  --h_s
   )

}


> #algebra[prettyprint]{

> prettyprint :: Canonical_Algebra String (Int,Int) String 
> prettyprint = 
>         (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
>          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) where
>   sadd _  (s, _, _)  = "."++s
>   cadd (s1, _, _) (s2, _, _) = s1++s2
>   ambd (s1, _, _) b (s2, _, _) = s1++("."++s2)
>   nil  _     = ""
>   edl  _  (s, _, _)   = "."++s
>   edr  (s, _, _)  _   = s++"."
>   edlr _  (s, _, _) _ = "."++s++"."
>   drem (x, _, _)      = x
>   is (x, _, _)        = x
>   sr   _  (s, _, _) _ = "("++s++")"
>   hl   _  _ (h1,h2) _ _   = "("++"("++dots (h1,h2)++"))"
>   sp   _  _ (s, _, _) _ _ = "("++"("++s++"))"
>   bl   (l1,l2) (s, _, _)  = dots (l1,l2)++s
>   br   (s, _, _) (r1,r2)  = s++dots (r1,r2)
>   il  (l1,l2) (s, _, _) (r1,r2) = dots (l1,l2)++s++dots (r1,r2)
>   ml    _ _ (s,_,_,_,_) _ _     = "("++"("++s++"))"
>   mldr  _ _ (s,_,_,_,_) _ _ _   = "("++"("++s++"."++"))"
>   mladr  _ _ (s,_,_,_,_) _ _ _   = "("++"("++s++"."++"))"
>   mldlr _ _ _ (s,_,_,_,_) _ _ _ = "("++"("++"."++s++"."++"))"
>   mladlr _ _ _ (s,_,_,_,_) _ _ _ = "("++"("++"."++s++"."++"))"
>   mldladr _ _ _ (s,_,_,_,_) _ _ _ = "("++"("++"."++s++"."++"))"
>   mladldr _ _ _ (s,_,_,_,_) _ _ _ = "("++"("++"."++s++"."++"))"
>   mldl  _ _ _ (s,_,_,_,_) _ _   = "("++"("++"."++s++"))"
>   mladl  _ _ _ (s,_,_,_,_) _ _   = "("++"("++"."++s++"))"
>   addss (s,_,_,_,_) (r1,r2) = s++dots (r1,r2)
>   ssadd (l1,l2) (s,_,_) = dots (l1,l2)++s
>   cons  (s1,_,_,_,_)  (s2,_,_,_,_)    = s1++s2
>   ul  (s,_,_)     = s
>   combine (s1,_,_,_,_) (s2,_,_,_,_) = s1 ++ s2
>   acomb (s1,_,_,_,_) b (s2,_,_,_,_) = s1 ++ '.' ++ s2
>   h   x = [id x]
>   h_i x = [id x]
>   h_l x = [id x]
>   h_s x = [id x]

> }

 prettyprint1 = prettyprint '\\' '/'
 prettyprint2 = prettyprint '/' '\\'

DotBracket algebra:

 dotBracket = prettyprint '.' '.'

Shape probabilities:

algebra[prob]{

 prob ::  Canonical_Algebra Int (Int,Int) Float
 prob = (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,
                    hl,sp,bl,br,il,
                    ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,
                    mldl,mladl,addss,ssadd,cons,
                    ul,combine,acomb,h,h_i,h_l,h_s)                where

    sadd b (q,s,lb,rb) = (scale(1) * q,s,lb,rb)
    cadd (q1,s1,lb1,rb1) (q2,s2,lb2,rb2)   = (q1 * q2, s1++ s2, lb1,rb1)
    ambd (q1,s1,lb1,rb1) b (q2,s2,lb2,rb2) = (scale(1) * q1 * q2 * mk_pf(min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))), s1 ++ s2, lb1,rb1)
    nil _ = (1.0,"",n,n)
    edl  dl (q,s,lb,rb)    = (scale(1) * q * mk_pf (dl_energy (lb,rb)),singlestrand++edangle_op++s++edangle_cl,lb,rb)
    edr     (q,s,lb,rb) dr = (scale(1) * q * mk_pf (dr_energy (lb,rb)),edangle_op++s++edangle_cl++singlestrand,lb,rb)
    edlr dl (q,s,lb,rb) dr = (scale(2) * q * mk_pf (dl_energy (lb,rb) + dr_energy (lb,rb)),singlestrand++edangle_op++s++edangle_cl++singlestrand,lb,rb) 
    drem (e,s,lb,rb) = (e,edangle_op++s++edangle_cl,lb,rb)
    is    (q,s,lb,rb)    = (q * mk_pf (termaupenalty lb rb),s,lb,rb)
    sr lb (q,s,_,_) rb = (scale(2) * q * mk_pf (sr_energy (lb,rb)),s,lb,rb)
    hl llb lb (i,j) rb rrb = (scale(j-i+4) * mk_pf (hl_energy (lb,rb) + sr_energy (llb,rrb)),loop_op++loop_ss++loop_cl,llb,rrb)
    sp llb lb (q,s,_,_) rb rrb = (scale(4) * q * mk_pf (sr_energy (llb,rrb)), s,llb,rrb)
    bl (l,r) (q,s,lend,rend) = (scale(r-l) * q * mk_pf (bl_energy (l,l,r,rend+1)),loop_op++loop_ss++s++loop_cl,l,rend)
    br (q,s,lend,rend) (l,r) = (scale(r-l) * q * mk_pf (br_energy (lend-1,l,r,r+1)),loop_op++s++loop_ss++loop_cl,lend,r)
    il (l1,l2) (q,s,l,r) (r1,r2) = (scale((l2-l1) + (r2-r1)) * q * mk_pf (il_energy(l1,l2,r1,r2)), loop_op++loop_ss++s++loop_ss++loop_cl,l1, r2)
    ml llb lb (q,s,_,_,_,_) rb rrb          = (scale(4) * q * mk_pf (380 + sr_energy (llb,rrb) + termaupenalty lb rb),loop_op++s++loop_cl,llb,rrb)
    mldr llb lb (q,s,_,_,_,_) dr rb rrb     = (scale(5) * q * mk_pf (380 + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),loop_op++  s ++ singlestrand ++loop_cl, llb,rrb)
    mladr llb lb (q,s,_,_,k,l) dr rb rrb = (scale(5) * q * mk_pf(380 + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),loop_op++  s ++ singlestrand ++loop_cl,llb,rrb) 
    mldlr llb lb dl (q,s,_,_,_,_) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb)  + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mladlr llb lb dl (q,s,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) +  (min (dri_energy (lb,rb)) (dr_energy (k,l))) + sr_energy (llb,rrb) + termaupenalty lb rb),loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mldladr llb lb dl (q,s,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb) + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mladldr llb lb dl (q,s,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) + dri_energy (lb,rb) + sr_energy (llb,rrb) + termaupenalty lb rb),loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mldl llb lb dl (q,s,_,_,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + dli_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb), loop_op++  singlestrand ++ s ++loop_cl, llb,rrb)
    mladl llb lb dl (q,s,i,j,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + const_e + min (dli_energy (lb,rb)) (dl_energy (i,j)) + sr_energy (llb,rrb) + termaupenalty lb rb), loop_op++  singlestrand ++ s ++loop_cl, llb,rrb)
    addss (q,s,lb1,rb1,lb2,rb2) (i,j) = (scale(j-i) * q * mk_pf(ss_energy (i,j)),s ++ singlestrand, lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems
    ssadd (i,j) (q,s,lb,rb) = (scale(j-i) * q * mk_pf(40 + ss_energy (i,j)),singlestrand ++ s, lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    cons (q1,s1,lb1a,lb1b,_, _) (q2,s2,_,_,rb2a,rb2b) = (q1 * q2, s1++s2, lb1a,lb1b,rb2a, rb2b) -- uebergabe der indizes des ersten und letzten stems

    ul (q,s,lb,rb) = (q * mk_pf(40),s,lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    combine (q1,s1,lb1,rb1,_,_) (q2,s2,_,_,lb2,rb2) = (q1 * q2,s1++s2,lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems 
    acomb (q1,s1,lba,rba,lb1,rb1) b (q2,s2,lb2,rb2,lbb,rbb) = (scale(1) * q1 * q2 * mk_pf (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))),s1 ++ singlestrand ++ s2, lba,rba,lbb,rbb)

  h   es = [hshapes es]
  h_i es = [hshapes_i es]
  h_l es = [hshapes_l es]
  h_s es = [hshapes_s es] 

}

algebra[probdb]{

 probdb ::  Canonical_Algebra Int (Int,Int) Float
 probdb = (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,
                    hl,sp,bl,br,il,
                    ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,
                    mldl,mladl,addss,ssadd,cons,
                    ul,combine,acomb,h,h_i,h_l,h_s)                where

    sadd b (q,e,s,d,lb,rb) = (scale(1) * q,e,s,"."++d,lb,rb)
    cadd (q1,e1,s1,d1,lb1,rb1) (q2,e2,s2,d2,lb2,rb2)   = (q1 * q2, e1 + e2, s1++s2, d1++d2,lb1,rb1)
    ambd (q1,e1,s1,d1,lb1,rb1) b (q2,e2,s2,d2,lb2,rb2) = (scale(1) * q1 * q2 * mk_pf(min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))), e1 + e2 + (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))), s1 ++ s2, d1++"."++d2, lb1,rb1)
    nil _ = (1.0,0,"","",n,n)
    edl  dl (q,e,s,d,lb,rb)    = (scale(1) * q * mk_pf (dl_energy (lb,rb)),e + dl_energy (lb,rb),singlestrand++edangle_op++s++edangle_cl,"."++d,lb,rb)
    edr     (q,e,s,d,lb,rb) dr = (scale(1) * q * mk_pf (dr_energy (lb,rb)),e + dr_energy (lb,rb),edangle_op++s++edangle_cl++singlestrand,d++".",lb,rb)
    edlr dl (q,e,s,d,lb,rb) dr = (scale(2) * q * mk_pf (dl_energy (lb,rb) + dr_energy (lb,rb)),e + dl_energy (lb,rb) + dr_energy (lb,rb),singlestrand++edangle_op++s++edangle_cl++singlestrand,"."++d++".",lb,rb) 
    drem (q,e,s,d,lb,rb) = (q,e,edangle_op++s++edangle_cl,d,lb,rb)
    is    (q,e,s,d,lb,rb)    = (q * mk_pf (termaupenalty lb rb),e + termaupenalty lb rb,s,d,lb,rb)
    sr lb (q,e,s,d,_,_) rb = (scale(2) * q * mk_pf (sr_energy (lb,rb)),e + sr_energy (lb,rb),s,"("++d++")",lb,rb)
    hl llb lb (i,j) rb rrb = (scale(j-i+4) * mk_pf (hl_energy (lb,rb) + sr_energy (llb,rrb)),hl_energy (lb,rb) + sr_energy (llb,rrb),loop_op++loop_ss++loop_cl,"("++"("++dots (i,j)++"))",llb,rrb)
    sp llb lb (q,e,s,d,_,_) rb rrb = (scale(4) * q * mk_pf (sr_energy (llb,rrb)),e + sr_energy (llb,rrb),s,"("++"("++d++"))",llb,rrb)
    bl (l,r) (q,e,s,d,lend,rend) = (scale(r-l) * q * mk_pf (bl_energy (l,l,r,rend+1)),e + bl_energy l    (l,r) (rend+1),loop_op++loop_ss++s++loop_cl,dots (l,r)++d,l,rend)
    br (q,e,s,d,lend,rend) (l,r) = (scale(r-l) * q * mk_pf (br_energy (lend-1,l,r,r+1)),e + br_energy (lend-1) (l,r) (r+1),loop_op++s++loop_ss++loop_cl,d++dots (l,r),lend,r)
    il (l1,l2) (q,e,s,d,l,r) (r1,r2) = (scale((l2-l1) + (r2-r1)) * q * mk_pf (il_energy(l1,l2,r1,r2)), e + il_energy (l1,l2) (r1,r2),loop_op++loop_ss++s++loop_ss++loop_cl,dots (l1,l2)++d++dots (r1,r2),l1, r2)
    ml llb lb (q,e,s,d,_,_,_,_) rb rrb          = (scale(4) * q * mk_pf (380 + sr_energy (llb,rrb) + termaupenalty lb rb),380 + e + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++s++loop_cl,"("++"("++d++"))",llb,rrb)
    mldr llb lb (q,e,s,d,_,_,_,_) dr rb rrb     = (scale(5) * q * mk_pf (380 + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),380 + e + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++  s ++ singlestrand ++loop_cl, "("++"("++d++"."++"))",llb,rrb)
    mladr llb lb (q,e,s,d,_,_,k,l) dr rb rrb = (scale(5) * q * mk_pf(380 + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),380 + e + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++  s ++ singlestrand ++loop_cl,"("++"("++d++"."++"))",llb,rrb) 
    mldlr llb lb dl (q,e,s,d,_,_,_,_) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb)  + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),380 + e + dli_energy (lb,rb)  + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,"("++"("++"."++d++"."++"))",llb,rrb)
    mladlr llb lb dl (q,e,s,d,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) +  (min (dri_energy (lb,rb)) (dr_energy (k,l))) + sr_energy (llb,rrb) + termaupenalty lb rb),380 + e + (min (dli_energy (lb,rb)) (dl_energy (i,j))) +  (min (dri_energy (lb,rb)) (dr_energy (k,l))) + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,"("++"("++"."++d++"."++"))",llb,rrb)
    mldladr llb lb dl (q,e,s,d,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb) + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),380 + e + dli_energy (lb,rb) + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,"("++"("++"."++d++"."++"))",llb,rrb)
    mladldr llb lb dl (q,e,s,d,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) + dri_energy (lb,rb) + sr_energy (llb,rrb) + termaupenalty lb rb),380 + e + (min (dli_energy (lb,rb)) (dl_energy (i,j))) + dri_energy (lb,rb) + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,"("++"("++"."++d++"."++"))",llb,rrb)
    mldl llb lb dl (q,e,s,d,_,_,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + dli_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb), 380 + e + dli_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb, loop_op++  singlestrand ++ s ++loop_cl, "("++"("++"."++d++"))",llb,rrb)
    mladl llb lb dl (q,e,s,d,i,j,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + const_e + min (dli_energy (lb,rb)) (dl_energy (i,j)) + sr_energy (llb,rrb) + termaupenalty lb rb), 380 + e + min (dli_energy (lb,rb)) (dl_energy (i,j)) + sr_energy (llb,rrb) + termaupenalty lb rb,loop_op++  singlestrand ++ s ++loop_cl, "("++"("++"."++d++"))",llb,rrb)
    addss (q,e,s,d,lb1,rb1,lb2,rb2) (i,j) = (scale(j-i) * q * mk_pf(ss_energy (i,j)),e + ss_energy (i,j),s ++ singlestrand, d++dots (i,j), lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems
    ssadd (i,j) (q,e,s,d,lb,rb) = (scale(j-i) * q * mk_pf(40 + ss_energy (i,j)),40 + e + ss_energy (i,j),singlestrand ++ s, dots (i,j)++d,lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    cons (q1,e1,s1,d1,lb1a,lb1b,_, _) (q2,e2,s2,d2,_,_,rb2a,rb2b) = (q1 * q2, e1 + e2, s1++s2, d1++d2,lb1a,lb1b,rb2a, rb2b) -- uebergabe der indizes des ersten und letzten stems

    ul (q,e,s,d,lb,rb) = (q * mk_pf(40),40 + e,s,d,lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    combine (q1,e1,s1,d1,lb1,rb1,_,_) (q2,e2,s2,d2,_,_,lb2,rb2) = (q1 * q2,e1 + e2,s1++s2,d1 ++ d2,lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems 
    acomb (q1,e1,s1,d1,lba,rba,lb1,rb1) b (q2,e2,s2,d2,lb2,rb2,lbb,rbb) = (scale(1) * q1 * q2 * mk_pf (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))),e1 + e2 + (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))), s1 ++ singlestrand ++ s2, d1 ++ '.' ++ d2,lba,rba,lbb,rbb)

  h   es = [hshapes es]
  h_i es = [hshapes_i es]
  h_l es = [hshapes_l es]
  h_s es = [hshapes_s es] 

}



algebra[prob2]{

 prob2 ::  Canonical_Algebra Int (Int,Int) Float
 prob2 = (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,
                    hl,sp,bl,br,il,
                    ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,
                    mldl,mladl,addss,ssadd,cons,
                    ul,combine,acomb,h,h_i,h_l,h_s)                where

    sadd b (q,x,s,lb,rb) = (scale(1) * q,x,s,lb,rb)
    cadd (q1,x1,s1,lb1,rb1) (q2,x2,s2,lb2,rb2)   = (q1 * q2, x1*x2, s1++ s2, lb1,rb1)
    ambd (q1,x1,s1,lb1,rb1) b (q2,x2,s2,lb2,rb2) = (scale(1) * q1 * q2 * mk_pf(min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))), x1*x2, s1 ++ s2, lb1,rb1)
    nil _ = (1.0,1.0,"",n,n)
    edl  dl (q,x,s,lb,rb)    = (scale(1) * q * mk_pf (dl_energy (lb,rb)),x,singlestrand++edangle_op++s++edangle_cl,lb,rb)
    edr     (q,x,s,lb,rb) dr = (scale(1) * q * mk_pf (dr_energy (lb,rb)),x,edangle_op++s++edangle_cl++singlestrand,lb,rb)
    edlr dl (q,x,s,lb,rb) dr = (scale(2) * q * mk_pf (dl_energy (lb,rb) + dr_energy (lb,rb)),x,singlestrand++edangle_op++s++edangle_cl++singlestrand,lb,rb) 
    drem (e,x,s,lb,rb) = (e,x,edangle_op++s++edangle_cl,lb,rb)
    is    (q,x,s,lb,rb)    = (q * mk_pf (termaupenalty lb rb),x,s,lb,rb)
    sr lb (q,x,s,_,_) rb = (scale(2) * q * mk_pf (sr_energy (lb,rb)),x,s,lb,rb)
    hl llb lb (i,j) rb rrb = (scale(j-i+4) * mk_pf (hl_energy (lb,rb) + sr_energy (llb,rrb)),1.0,loop_op++loop_ss++loop_cl,llb,rrb)
    sp llb lb (q,x,s,_,_) rb rrb = (scale(4) * q * mk_pf (sr_energy (llb,rrb)),x, s,llb,rrb)
    bl (l,r) (q,x,s,lend,rend) = (scale(r-l) * q * mk_pf (bl_energy (l,l,r,rend+1)),x,loop_op++loop_ss++s++loop_cl,l,rend)
    br (q,x,s,lend,rend) (l,r) = (scale(r-l) * q * mk_pf (br_energy (lend-1,l,r,r+1)),x,loop_op++s++loop_ss++loop_cl,lend,r)
    il (l1,l2) (q,x,s,l,r) (r1,r2) = (scale((l2-l1) + (r2-r1)) * q * mk_pf (il_energy(l1,l2,r1,r2)),x, loop_op++loop_ss++s++loop_ss++loop_cl,l1, r2)
    ml llb lb (q,x,s,_,_,_,_) rb rrb          = (scale(4) * q * mk_pf (380 + sr_energy (llb,rrb) + termaupenalty lb rb),x,loop_op++s++loop_cl,llb,rrb)
    mldr llb lb (q,x,s,_,_,_,_) dr rb rrb     = (scale(5) * q * mk_pf (380 + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),x,loop_op++  s ++ singlestrand ++loop_cl, llb,rrb)
    mladr llb lb (q,x,s,_,_,k,l) dr rb rrb = (scale(5) * q * mk_pf(380 + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),x,loop_op++  s ++ singlestrand ++loop_cl,llb,rrb) 
    mldlr llb lb dl (q,x,s,_,_,_,_) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb)  + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),x,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mladlr llb lb dl (q,x,s,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) +  (min (dri_energy (lb,rb)) (dr_energy (k,l))) + sr_energy (llb,rrb) + termaupenalty lb rb),x,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mldladr llb lb dl (q,x,s,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb) + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),x,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mladldr llb lb dl (q,x,s,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) + dri_energy (lb,rb) + sr_energy (llb,rrb) + termaupenalty lb rb),x,loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl,llb,rrb)
    mldl llb lb dl (q,x,s,_,_,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + dli_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb), x,loop_op++  singlestrand ++ s ++loop_cl, llb,rrb)
    mladl llb lb dl (q,x,s,i,j,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + const_e + min (dli_energy (lb,rb)) (dl_energy (i,j)) + sr_energy (llb,rrb) + termaupenalty lb rb),x, loop_op++  singlestrand ++ s ++loop_cl, llb,rrb)
    addss (q,x,s,lb1,rb1,lb2,rb2) (i,j) = (scale(j-i) * q * mk_pf(ss_energy (i,j)),x,s ++ singlestrand, lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems
    ssadd (i,j) (q,x,s,lb,rb) = (scale(j-i) * q * mk_pf(40 + ss_energy (i,j)),x,singlestrand ++ s, lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    cons (q1,x1,s1,lb1a,lb1b,_, _) (q2,x2,s2,_,_,rb2a,rb2b) = (q1 * q2, x1*x2, s1++s2, lb1a,lb1b,rb2a, rb2b) -- uebergabe der indizes des ersten und letzten stems

    ul (q,x,s,lb,rb) = (q * mk_pf(40),x,s,lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    combine (q1,x1,s1,lb1,rb1,_,_) (q2,x2,s2,_,_,lb2,rb2) = (q1 * q2,x1*x2, s1++s2,lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems 
    acomb (q1,x1,s1,lba,rba,lb1,rb1) b (q2,x2,s2,lb2,rb2,lbb,rbb) = (scale(1) * q1 * q2 * mk_pf (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))),x1*x2, s1 ++ singlestrand ++ s2, lba,rba,lbb,rbb)

  h   es = [hshapes es]
  h_i es = [hshapes_i es]
  h_l es = [hshapes_l es]
  h_s es = [hshapes_s es] 

}

 shape edangle_op edangle_cl loop_ss loop_op loop_cl singlestrand basearray takes = 

algebra[shape]{

 shape :: Canonical_Algebra Int (Int,Int) String 

 shape = (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) where
   sadd _ (s,_,_)           = s --shape_sadd(s)  -- if shape_cond_sadd s then "_" else app singlestrand s  -- singlestrand == "" && s == ""
   cadd (s1,_,_) (s2,_,_)   = s1 ++ s2 -- shape_cadd(s1,s2) -- if shape_cond_sadd s2  then s1 else app s1 s2  -- (singlestrand == "" && s2 =="_")
   ambd (s1,_,_) b (s2,_,_) = s1 ++ s2 -- app (app s1 singlestrand) s2
   nil  _       = ""
   edl  _  (s,_,_)    = singlestrand++edangle_op++s++edangle_cl
   edr  (s,_,_) _     = edangle_op++s++edangle_cl++singlestrand
   edlr _ (s,_,_) _   = singlestrand++edangle_op++s++edangle_cl++singlestrand
   drem    (s,_,_)    = edangle_op++s++edangle_cl
   is (x,_,_)         = x
   sr _ (s,_,_) _     = s
   hl _ _ _ _ _ = loop_op++loop_ss++loop_cl
   sp _ _ (s,_,_) _ _ = s
   bl _ (s,_,_)       = loop_op++loop_ss++s++loop_cl
   br (s,_,_) _       = loop_op++s++loop_ss++loop_cl
   il _ (s,_,_) _     = loop_op++loop_ss++s++loop_ss++loop_cl
   ml _ _ (s,_,_,_,_) _ _ = loop_op++s++loop_cl
   mldr _ _ (s,_,_,_,_) _ _ _  = loop_op++  s ++ singlestrand ++loop_cl
   mladr _ _ (s,_,_,_,_) _ _ _ = loop_op++  s ++ singlestrand ++loop_cl
   mldlr _ _ _ (s,_,_,_,_) _ _ _  = loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl
   mladlr _ _ _ (s,_,_,_,_) _ _ _ = loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl
   mldladr _ _ _ (s,_,_,_,_) _ _ _ = loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl
   mladldr _ _ _ (s,_,_,_,_) _ _ _ = loop_op++ singlestrand ++ s ++ singlestrand ++loop_cl
   mldl _ _ _ (s,_,_,_,_) _ _  = loop_op++  singlestrand ++ s ++loop_cl
   mladl _ _ _ (s,_,_,_,_) _ _ = loop_op++  singlestrand ++ s ++loop_cl
   addss (s,_,_,_,_) _    = s ++ singlestrand
   ssadd _ (s,_,_)    = singlestrand ++ s
   cons (s1,_,_,_,_) (s2,_,_,_,_)   = s1 ++ s2
   ul (s,_,_)         = s
   combine (s1,_,_,_,_) (s2,_,_,_,_)= s1 ++ s2
   acomb (s1,_,_,_,_) b (s2,_,_,_,_)= s1 ++ singlestrand ++ s2
   h   x = [id x]
   h_i x = [id x]
   h_l x = [id x]
   h_s x = [id x]

}


 shape :: Canonical_Algebra Int (Int,Int) String 

 shape = (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) where
   sadd _ (s,_,_)           = shape_sadd(s)  -- if shape_cond_sadd s then "_" else app singlestrand s  -- singlestrand == "" && s == ""
   cadd (s1,_,_) (s2,_,_)   = shape_cadd(s1,s2) -- if shape_cond_sadd s2  then s1 else app s1 s2  -- (singlestrand == "" && s2 =="_")
   ambd (s1,_,_) b (s2,_,_) = app (app s1 singlestrand) s2
   nil  _       = ""
   edl  _  (s,_,_)    = singlestrand++edangle_op++s++edangle_cl
   edr  (s,_,_) _     = edangle_op++s++edangle_cl++singlestrand
   edlr _ (s,_,_) _   = singlestrand++edangle_op++s++edangle_cl++singlestrand
   drem    (s,_,_)    = edangle_op++s++edangle_cl
   is (x,_,_)         = x
   sr _ (s,_,_) _     = s
   hl _ _ _ _ _ = loop_op++loop_ss++loop_cl
   sp _ _ (s,_,_) _ _ = s
   bl _ (s,_,_)       = loop_op++loop_ss++s++loop_cl
   br (s,_,_) _       = loop_op++s++loop_ss++loop_cl
   il _ (s,_,_) _     = loop_op++loop_ss++s++loop_ss++loop_cl
   ml _ _ (s,_,_,_,_) _ _ = loop_op++s++loop_cl
   mldr _ _ (s,_,_,_,_) _ _ _  = loop_op++ (app s singlestrand) ++loop_cl
   mladr _ _ (s,_,_,_,_) _ _ _ = loop_op++ (app s singlestrand) ++loop_cl
   mldlr _ _ _ (s,_,_,_,_) _ _ _  = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
   mladlr _ _ _ (s,_,_,_,_) _ _ _ = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
   mldladr _ _ _ (s,_,_,_,_) _ _ _ = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
   mladldr _ _ _ (s,_,_,_,_) _ _ _ = loop_op++ (app singlestrand (app s singlestrand)) ++loop_cl
   mldl _ _ _ (s,_,_,_,_) _ _  = loop_op++ (app singlestrand s) ++loop_cl
   mladl _ _ _ (s,_,_,_,_) _ _ = loop_op++ (app singlestrand s) ++loop_cl
   addss (s,_,_,_,_) _    = app s singlestrand
   ssadd _ (s,_,_)    = app singlestrand s
   cons (s1,_,_,_,_) (s2,_,_,_,_)   = app s1 s2
   ul (s,_,_)         = s
   combine (s1,_,_,_,_) (s2,_,_,_,_)= app s1 s2
   acomb (s1,_,_,_,_) b (s2,_,_,_,_)= app (app s1 singlestrand) s2
   h   x = [id x]
   h_i x = [id x]
   h_l x = [id x]
   h_s x = [id x]

 shape1 = shape "" "" "_" "[" "]" "_"  -- all loops are represented different
 shape2 = shape "" "" "" "[" "]" "_"   -- bulges and internal loops have same representation
 shape3 = shape "" "" "" "[" "]" ""    -- bulges and internal loops have same representation, no external loop
 shape4 = shape "[" "]" "" "" "" "_"     -- edangles (complete substructures) and external loop contribute
 shape5 = shape "[" "]" "" "" "" ""      -- only edangles contribute


> #algebra[mfe]{

> mfe :: Canonical_Algebra Int (Int,Int) Float
> mfe  = (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,
>                    hl,sp,bl,br,il,
>                    ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,
>                    mldl,mladl,addss,ssadd,cons,
>                    ul,combine,acomb,h,h_i,h_l,h_s)                where
>    sadd lb (e,_,rb) = (e,lb,rb)
>    cadd (e1,lb1,rb1) (e2,lb2,rb2)     = (e1 + e2,lb1,rb1)  -- uebergabe der indizes des ersten stems
>    ambd (e1,lb1,rb1) db (e2,lb2,rb2) = (e1 + e2 + (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))),lb1,rb1) -- uebergabe der indizes des ersten stems
>    nil _ = (0,n,n)
>    edl  dl (e,lb,rb)    = (e + dl_energy (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
>    edr     (e,lb,rb) dr = (e + dr_energy (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
>    edlr dl (e,lb,rb) dr = (e + dl_energy (lb,rb) + dr_energy (lb,rb),lb,rb) -- uebergabe der indizes des ersten stems
>    drem (e,lb,rb) = (e,lb,rb)
>    is   (e,lb,rb)    = (e + termaupenalty lb rb,lb,rb)
>    sr   lb (e,_,_) rb = (e + sr_energy (lb,rb),lb,rb)
>    hl llb lb loop rb rrb = (hl_energy (lb,rb) + sr_energy (llb,rrb),llb,rrb)
>    sp llb lb (e,_,_) rb rrb = (e + sr_energy (llb,rrb), llb,rrb)
>    bl (l,r) (e,lend,rend) = (e + bl_energy l    (l,r) (rend+1),l,rend)
>    br (e,lend,rend) (l,r) = (e + br_energy (lend-1) (l,r) (r+1),lend,r)
>    il (l1,l2) (e,l,r) (r1,r2) = (e + il_energy (l1,l2) (r1,r2), l1, r2)
>    ml llb lb (e,_,_,_,_) rb rrb  = (380 + e + sr_energy (llb,rrb) + termaupenalty lb rb,llb,rrb)
>    mldr llb lb (e,_,_,_,_) dr rb rrb     = (380 + e + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb,llb,rrb)
>    mladr llb lb (e,_,_,k,l) dr rb rrb     = (380 + e + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb,llb,rrb) 
>    mldlr llb lb dl (e,_,_,_,_) dr rb rrb = (380 + e + dli_energy (lb,rb)  + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb,llb,rrb)
>    mladlr llb lb dl (e,i,j,k,l) dr rb rrb = (380 + e + (min (dli_energy (lb,rb)) (dl_energy (i,j))) +  (min (dri_energy (lb,rb)) (dr_energy (k,l))) + sr_energy (llb,rrb) + termaupenalty lb rb,llb,rrb)
>    mldladr llb lb dl (e,i,j,k,l) dr rb rrb = (380 + e + dli_energy (lb,rb) + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb,llb,rrb)
>    mladldr llb lb dl (e,i,j,k,l) dr rb rrb = (380 + e + (min (dli_energy (lb,rb)) (dl_energy (i,j))) + dri_energy (lb,rb) + sr_energy (llb,rrb) + termaupenalty lb rb,llb,rrb)
>    mldl llb lb dl (e,_,_,_,_) rb rrb     = (380 + e + dli_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb, llb,rrb)
>    mladl llb lb dl (e,i,j,_,_) rb rrb     = (380 + e + min (dli_energy (lb,rb)) (dl_energy (i,j)) + sr_energy (llb,rrb) + termaupenalty lb rb, llb,rrb)
>    addss (e,lb1,rb1,lb2,rb2) (i,j) = (e + ss_energy (i,j),lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems
>    ssadd (i,j) (e,lb,rb) = (40 + e + ss_energy (i,j),lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
>    cons (e1,lb1a,lb1b,_, _) (e2,_,_,rb2a,rb2b) = (e1 + e2,lb1a,lb1b,rb2a, rb2b) -- uebergabe der indizes des ersten und letzten stems
>    ul (e,lb,rb) = (40 + e,lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
>    combine (e1,lb1,rb1,_,_) (e2,_,_,lb2,rb2) = (e1 + e2,lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems 
>    acomb (e1,lba,rba,lb1,rb1) b (e2,lb2,rb2,lbb,rbb) = (e1 + e2 + (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))),lba,rba,lbb,rbb)

>    h   xs = [minimum xs]
>    h_i xs = [minimum xs]

    h_l xs = [minimum_unstable xs]

>    h_l xs = [minimum xs]
>    --h_l xs = if (minE_xs < 0.0) then [(minE_xs,i,j)] else []
>    --  where (minE_xs,i,j) = minimum xs
>    h_s xs = [minimum xs]

> }


    (_,n) = bounds basearray



 mfe_range :: Array Int Ebase -> Float ->      -- closed      answer   
              Canonical_Algebra Int (Int,Int) (Float,Int,Int) (Float,(Int,Int),(Int,Int))
 mfe_range basearray takes = (sadd,cadd,ambd ,nil,edl ,edr ,edlr ,drem,is ,sr ,
                          hl ,sp ,bl ,br ,il ,ml ,mldr ,mladr ,mldlr ,mladlr ,mldladr ,mladldr ,
                          mldl ,mladl ,addss,ssadd,cons,ul,combine,acomb ,h,h_i,h_l,h_s) where
    (sadd1,cadd1,ambd1,nil1,edl1,edr1,edlr1,drem1,is1,sr1,hl1,sp1,bl1,br1,il1,
     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,
     cons1,ul1,combine1,acomb1,h1,h_i1,h_l1,h_s1) = mfe basearray takes
    sadd = sadd1
    cadd = cadd1
    ambd = ambd1
    nil  = nil1
    edl  = edl1
    edr  = edr1
    edlr = edlr1
    drem = drem1
    is   = is1
    sr   = sr1
    hl   = hl1
    sp   = sp1
    bl   = bl1
    br   = br1
    il   = il1
    ml   = ml1
    mldr = mldr1
    mladr = mladr1
    mldlr = mldlr1
    mladlr = mladlr1
    mldladr = mldladr1
    mladldr = mladldr1
    mldl = mldl1
    mladl = mladl1
    addss = addss1
    ssadd = ssadd1
    cons = cons1
    ul = ul1
    combine =combine1
    acomb = acomb1

    h [] = []
    h xs = filter_erange (lowest+takes) xs
       where (lowest,_,_) = minimum xs 
             filter_erange _ [] = []
             filter_erange limit ((x1,x2,x3):xs) 
              | x1 <= limit = (x1,x2,x3): (filter_erange limit xs)
              | otherwise   = filter_erange limit xs
    h_l [] = []
    h_l xs = filter_erange (lowest+takes) xs
       where (lowest,_,_) = minimum xs 
             filter_erange _ [] = []
             filter_erange limit ((x1,x2,x3):xs) 
              | x1 < 0.0 && x1 <= limit = (x1,x2,x3): (filter_erange limit xs)
              | otherwise   = filter_erange limit xs 
    h_s = h
    h_i = h
    (_,n) = bounds basearray


Partition function algebra:

 mean_nrg:: Float
 mean_nrg= -0.1843  -- mean energy for random sequences: 184.3*length cal
 mean_scale :: Float
 mean_scale = exp (-mean_nrg/(r_gas * temperature))

 r_gas = 0.00198717 -- [kcal/mol] <-- 1.98717 [cal/mol]
 temperature  = 310.15  -- [K]
 mk_pf x = exp ((-x/100) / (r_gas * temperature)) -- (-x/100) because energies are given multiplied by 100

 p_func :: Array Int Ebase -> Float ->      -- closed      answer   
           Canonical_Algebra Int (Int,Int) (Float,Int,Int) (Float,(Int,Int),(Int,Int))
 p_func basearray takes = (sadd,cadd,ambd basearray,nil,edl basearray,edr basearray,edlr basearray,drem,is basearray,sr basearray,
                    hl basearray,sp basearray,bl basearray,br basearray,il basearray,
                    ml basearray,mldr basearray,mladr basearray,mldlr basearray,mladlr basearray,mldladr basearray,mladldr basearray,
                    mldl basearray,mladl basearray,addss,ssadd,cons,
                    ul,combine,acomb basearray,h,h_i,h_l,h_s)                where
    scale:: Array Int Float
    scale  = array (0,n) ((0, 1.0) : [(i,  scale!(i-1)/ mean_scale) |i<-[1..n]])

    sadd b (q,lb,rb) = (scale!1 * q,lb,rb)
    cadd (q1,lb1,rb1) (q2,lb2,rb2)   = (q1 * q2, lb1,rb1)  -- uebergabe der indizes des ersten stems
    ambd inp (q1,lb1,rb1) b (q2,lb2,rb2) = (scale!1 * q1 * q2 * mk_pf(min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))), lb1,rb1)  -- uebergabe der indizes des ersten stems
    nil _ = (1.0,n,n)
    edl  inp dl (q,lb,rb)    = (scale!1 * q * mk_pf (dl_energy inp (lb,rb)),lb,rb)  -- uebergabe der indizes des ersten stems
    edr  inp    (q,lb,rb) dr = (scale!1 * q * mk_pf (dr_energy inp (lb,rb)),lb,rb)  -- uebergabe der indizes des ersten stems
    edlr inp dl (q,lb,rb) dr = (scale!2 * q * mk_pf (dl_energy inp (lb,rb) + dr_energy inp (lb,rb)),lb,rb)  -- uebergabe der indizes des ersten stems
    drem = id
    is inp    (q,lb,rb)    = (q * mk_pf (termaupenalty (inp!lb) (inp!rb)),lb,rb)
    sr inp lb (q,_,_) rb = (scale!2 * q * mk_pf (sr_energy inp (lb,rb)),lb,rb)
    hl inp llb lb (i,j) rb rrb = (scale!(j-i+4) * mk_pf (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb)),llb,rrb)
    sp inp llb lb (q,_,_) rb rrb = (scale!4 * q * mk_pf (sr_energy inp (llb,rrb)), llb,rrb)
    bl inp (l,r) (q,lend,rend) = (scale!(r-l) * q * mk_pf (bl_energy inp l    (l,r) (rend+1)),l,rend)
    br inp (q,lend,rend) (l,r) = (scale!(r-l) * q * mk_pf (br_energy inp (lend-1) (l,r) (r+1)),lend,r)
    il inp (l1,l2) (q,l,r) (r1,r2) = (scale!((l2-l1) + (r2-r1)) * q * mk_pf (il_energy inp (l1,l2) (r1,r2)), l1, r2)
    ml inp llb lb (q,_,_) rb rrb          = (scale!4 * q * mk_pf (380 + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb)
    mldr inp llb lb (q,_,_) dr rb rrb     = (scale!5 * q * mk_pf (380 + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb)
    mladr inp llb lb (q,_,(k,l)) dr rb rrb = (scale!5 * q * mk_pf(380 + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb) 
                                         where dangle_e = min (dri_energy inp (lb,rb)) (dr_energy inp (k,l))
    mldlr inp llb lb dl (q,_,_) dr rb rrb = (scale!6 * q * mk_pf (380 + dli_energy inp (lb,rb)  + dri_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb)
    mladlr inp llb lb dl (q,(i,j),(k,l)) dr rb rrb = (scale!6 * q * mk_pf (380 + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb)
                                  where dangle_e = (min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))) +  (min (dri_energy inp (lb,rb)) (dr_energy inp (k,l)))
    mldladr inp llb lb dl (q,(i,j),(k,l)) dr rb rrb = (scale!6 * q * mk_pf (380 + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb)
                                  where dangle_e = dli_energy inp (lb,rb) + min (dri_energy inp (lb,rb)) (dr_energy inp (k,l))
    mladldr inp llb lb dl (q,(i,j),(k,l)) dr rb rrb = (scale!6 * q * mk_pf (380 + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)),llb,rrb)
                                  where dangle_e = (min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))) + dri_energy inp (lb,rb)
    mldl inp llb lb dl (q,_,_) rb rrb     = (scale!5 * q * mk_pf (380 + dli_energy inp (lb,rb)  + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)), llb,rrb)
    mladl inp llb lb dl (q,(i,j),_) rb rrb     = (scale!5 * q * mk_pf (380 + e + dangle_e + sr_energy inp (llb,rrb) + termaupenalty (inp!lb) (inp!rb)), llb,rrb)
                                     where dangle_e = min (dli_energy inp (lb,rb)) (dl_energy inp (i,j))
    addss (q,(lb1,rb1),(lb2,rb2)) (i,j) = (scale!(j-i) * q * mk_pf(ss_energy (i,j)),(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems
    ssadd (i,j) (q,lb,rb) = (scale!(j-i) * q * mk_pf(40 + ss_energy (i,j)),(lb,rb),(lb,rb)) -- uebergabe der indizes des ersten und letzten stems
    cons (q1,lb1,rb1) (q2,lb2,rb2) = (q1 * q2,lb1,rb2) -- uebergabe der indizes des ersten und letzten stems
    ul (q,lb,rb) = (q * mk_pf(40),(lb,rb),(lb,rb)) -- uebergabe der indizes des ersten und letzten stems
    combine (q1,(lb1,rb1),_) (q2,_,(lb2,rb2)) = (q1 * q2,(lb1,rb1),(lb2,rb2)) -- uebergabe der indizes des ersten und letzten stems 
    acomb inp (q1,(lba,rba),(lb1,rb1)) b (q2,(lb2,rb2),(lbb,rbb)) = (scale!1 * q1 * q2 * mk_pf (min (dr_energy inp (lb1,rb1)) (dl_energy inp (lb2,rb2))),(lba,rba),(lbb,rbb))

    h []   = []
    h xs   = [foldl1 (sum_triples) xs]
         where sum_triples (x1,_,_) (x2,l,r) = (x1+x2,l,r)

    h_l = h

   h_l [] = []
   h_l xs = [(y,a,b) | (y,a,b) <- [foldl1 (sum_triples) xs], y >= scale!(min n (b-a+2))]
        where sum_triples (x1,_,_) (x2,l,r) = (x1+x2,l,r)

    h_s    = h
    h_i    = h

    (_,n) = bounds basearray


    scale:: Array Int Float
    scale  = array (0,n) ((0, 1.0) : [(i,  scale!(i-1)/ mean_scale) |i<-[1..n]])


algebra[pf_sample]{

 pf_sample ::  Canonical_Algebra Int (Int,Int) Float
 pf_sample = (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,
                    hl,sp,bl,br,il,
                    ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,
                    mldl,mladl,addss,ssadd,cons,
                    ul,combine,acomb,h,h_i,h_l,h_s)                where

    sadd b (q,lb,rb) = (scale(1) * q,lb,rb)
    cadd (q1,lb1,rb1) (q2,lb2,rb2)   = (q1 * q2, lb1,rb1)  -- uebergabe der indizes des ersten stems
    ambd (q1,lb1,rb1) b (q2,lb2,rb2) = (scale(1) * q1 * q2 * mk_pf(min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))), lb1,rb1)  -- uebergabe der indizes des ersten stems
    nil _ = (1.0,n,n)
    edl  dl (q,lb,rb)    = (scale(1) * q * mk_pf (dl_energy (lb,rb)),lb,rb)  -- uebergabe der indizes des ersten stems
    edr     (q,lb,rb) dr = (scale(1) * q * mk_pf (dr_energy (lb,rb)),lb,rb)  -- uebergabe der indizes des ersten stems
    edlr dl (q,lb,rb) dr = (scale(2) * q * mk_pf (dl_energy (lb,rb) + dr_energy (lb,rb)),lb,rb)  -- uebergabe der indizes des ersten stems
    drem (e,lb,rb) = (e,lb,rb)
    is    (q,lb,rb)    = (q * mk_pf (termaupenalty lb rb),lb,rb)
    sr lb (q,_,_) rb = (scale(2) * q * mk_pf (sr_energy (lb,rb)),lb,rb)
    hl llb lb (i,j) rb rrb = (scale(j-i+4) * mk_pf (hl_energy (lb,rb) + sr_energy (llb,rrb)),llb,rrb)
    sp llb lb (q,_,_) rb rrb = (scale(4) * q * mk_pf (sr_energy (llb,rrb)), llb,rrb)
    bl (l,r) (q,lend,rend) = (scale(r-l) * q * mk_pf (bl_energy (l,l,r,rend+1)),l,rend)
    br (q,lend,rend) (l,r) = (scale(r-l) * q * mk_pf (br_energy (lend-1,l,r,r+1)),lend,r)
    il (l1,l2) (q,l,r) (r1,r2) = (scale((l2-l1) + (r2-r1)) * q * mk_pf (il_energy(l1,l2,r1,r2)), l1, r2)
    ml llb lb (q,_,_,_,_) rb rrb          = (scale(4) * q * mk_pf (380 + sr_energy (llb,rrb) + termaupenalty lb rb),llb,rrb)
    mldr llb lb (q,_,_,_,_) dr rb rrb     = (scale(5) * q * mk_pf (380 + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),llb,rrb)
    mladr llb lb (q,_,_,k,l) dr rb rrb = (scale(5) * q * mk_pf(380 + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),llb,rrb) 
    mldlr llb lb dl (q,_,_,_,_) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb)  + dri_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb),llb,rrb)
    mladlr llb lb dl (q,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) +  (min (dri_energy (lb,rb)) (dr_energy (k,l))) + sr_energy (llb,rrb) + termaupenalty lb rb),llb,rrb)
    mldladr llb lb dl (q,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + dli_energy (lb,rb) + min (dri_energy (lb,rb)) (dr_energy (k,l)) + sr_energy (llb,rrb) + termaupenalty lb rb),llb,rrb)
    mladldr llb lb dl (q,i,j,k,l) dr rb rrb = (scale(6) * q * mk_pf (380 + (min (dli_energy (lb,rb)) (dl_energy (i,j))) + dri_energy (lb,rb) + sr_energy (llb,rrb) + termaupenalty lb rb),llb,rrb)
    mldl llb lb dl (q,_,_,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + dli_energy (lb,rb)  + sr_energy (llb,rrb) + termaupenalty lb rb), llb,rrb)
    mladl llb lb dl (q,i,j,_,_) rb rrb     = (scale(5) * q * mk_pf (380 + const_e + min (dli_energy (lb,rb)) (dl_energy (i,j)) + sr_energy (llb,rrb) + termaupenalty lb rb), llb,rrb)
    addss (q,lb1,rb1,lb2,rb2) (i,j) = (scale(j-i) * q * mk_pf(ss_energy (i,j)),lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems
    ssadd (i,j) (q,lb,rb) = (scale(j-i) * q * mk_pf(40 + ss_energy (i,j)),lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    cons (q1,lb1a,lb1b,_, _) (q2,_,_,rb2a,rb2b) = (q1 * q2,lb1a,lb1b,rb2a, rb2b) -- uebergabe der indizes des ersten und letzten stems

    ul (q,lb,rb) = (q * mk_pf(40),lb,rb,lb,rb) -- uebergabe der indizes des ersten und letzten stems
    combine (q1,lb1,rb1,_,_) (q2,_,_,lb2,rb2) = (q1 * q2,lb1,rb1,lb2,rb2) -- uebergabe der indizes des ersten und letzten stems 
    acomb (q1,lba,rba,lb1,rb1) b (q2,lb2,rb2,lbb,rbb) = (scale(1) * q1 * q2 * mk_pf (min (dr_energy (lb1,rb1)) (dl_energy (lb2,rb2))),lba,rba,lbb,rbb)


    h   xs = [sum xs]
    h_i xs = [sum xs]

    h_l xs = [sum xs]
    h_s xs = [sum xs]

}

    h   xs = [maximum xs]
    h_i xs = [maximum xs]

    h_l xs = [maximum xs]
    h_s xs = [maximum xs]

}

 --------------------------------------------------
    h []   = []
    h xs   = [foldl1 (sum_triples) xs]
         where sum_triples (x1,_,_) (x2,l,r) = (x1+x2,l,r)

    h_l = h
    h_s    = h
    h_i    = h
 --------------------------------------------------


    h []   = []
    h xs   = [first_bigger' (getRandomInt (fst_3 (foldl1 sum_triples xs))) (summed_up xs) xs]
         where sum_triples (x1,_,_) (x2,l,r) = (x1+x2,l,r)
               fst_3 (a,b,c) = a
               summed_up (p:[]) = [p]
               summed_up (p:ps) = p:(summed_up' p ps)
              summed_up' y (p:ps) = summed_up ((sum_triples y p):ps)
               first_bigger [] _ _      = []
              first_bigger (a:as) ps ys = first_bigger' a ps ys : first_bigger as ps ys
              first_bigger' a ((p,_,_):ps) (y:ys) = if p >= a then y else first_bigger' a ps ys

    h_l    = h
    h_s    = h
    h_i    = h

}

    (_,n) = bounds basearray


Algebra cross product:

 infix ***
 (alg1 *** alg2) basearray takes =
         (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) where
    (sadd1,cadd1,ambd1,nil1,edl1,edr1,edlr1,drem1,is1,sr1,hl1,sp1,bl1,br1,il1,
     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,cons1,ul1,combine1,acomb1,h1,h_i1,h_l1,h_s1) = alg1 basearray takes
    (sadd2,cadd2,ambd2,nil2,edl2,edr2,edlr2,drem2,is2,sr2,hl2,sp2,bl2,br2,il2,
     ml2,mldr2,mladr2,mldlr2,mladlr2,mldladr2,mladldr2,mldl2,mladl2,addss2,ssadd2,cons2,ul2,combine2,acomb2,h2,h_i2,h_l2,h_s2) = alg2 basearray takes

    sadd b (a1,a2) = (sadd1 b a1, sadd2 b a2)
    cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
    ambd (c1,c2) b (a1,a2) = (ambd1 c1 b a1, ambd2 c2 b a2)
    nil a = (nil1 a, nil2 a)
    edl b (c1,c2) = (edl1 b c1, edl2 b c2)
    edr (c1,c2) b = (edr1 c1 b, edr2 c2 b)
    edlr b (c1,c2) b' = (edlr1 b c1 b', edlr2 b c2 b')
    drem (c1,c2) = (drem1 c1, drem2 c2)
    is (c1,c2)   = (is1 c1, is2 c2)
    sr b (c1,c2) b' = (sr1 b c1 b', sr2 b c2 b')
    hl b1 b2 u b2' b1' = (hl1 b1 b2 u b2' b1', hl2 b1 b2 u b2' b1')
    sp b1 b2 (c1,c2) b2' b1' = (sp1 b1 b2 c1 b2' b1',sp2 b1 b2 c2 b2' b1') 
    bl u (c1,c2) = (bl1 u c1, bl2 u c2)
    br (c1,c2) u = (br1 c1 u, br2 c2 u)
    il r1 (c1,c2) r2 = (il1 r1 c1 r2 ,il2 r1 c2 r2) 
    ml b1 b2 (m1,m2) b2' b1' = (ml1 b1 b2 m1 b2' b1', ml2 b1 b2 m2 b2' b1')
    mldr b1 b2 (m1,m2) d b2' b1' = (mldr1 b1 b2 m1 d b2' b1',
                                    mldr2 b1 b2 m2 d b2' b1')
    mladr b1 b2 (m1,m2) d b2' b1' = (mladr1 b1 b2 m1 d b2' b1',
                                     mladr2 b1 b2 m2 d b2' b1')
    mldlr b1 b2 d (m1,m2) d_ b2' b1' = (mldlr1 b1 b2 d m1 d_ b2' b1',
                                        mldlr2 b1 b2 d m2 d_ b2' b1')
    mladlr b1 b2 d (m1,m2) d_ b2' b1' = (mladlr1 b1 b2 d m1 d_ b2' b1',
                                         mladlr2 b1 b2 d m2 d_ b2' b1')
    mldladr b1 b2 d (m1,m2) d_ b2' b1' = (mldladr1 b1 b2 d m1 d_ b2' b1',
                                          mldladr2 b1 b2 d m2 d_ b2' b1')
    mladldr b1 b2 d (m1,m2) d_ b2' b1' = (mladldr1 b1 b2 d m1 d_ b2' b1',
                                          mladldr2 b1 b2 d m2 d_ b2' b1')
    mldl b1 b2 d (m1,m2) b2' b1' = (mldl1 b1 b2 d m1 b2' b1', mldl2 b1 b2 d m2 b2' b1')
    mladl b1 b2 d (m1,m2) b2' b1' = (mladl1 b1 b2 d m1 b2' b1', mladl2 b1 b2 d m2 b2' b1')
    addss (c1,c2) u = (addss1 c1 u, addss2 c2 u)
    ssadd u (c1,c2) = (ssadd1 u c1, ssadd2 u c2)
    cons (c1,c2) (c_1,c_2) = (cons1 c1 c_1, cons2 c2 c_2)
    ul (c1,c2) = (ul1 c1, ul2 c2)
    combine (c1,c2) (c_1,c_2) = (combine1 c1 c_1, combine2 c2 c_2)
    acomb (c1,c2) b (c_1,c_2) = (acomb1 c1 b c_1, acomb2 c2 b c_2)

    h xs   = [(x1,x2)| x1 <- nub $   h1 [ y1 | (y1,y2) <- xs],
                       x2 <-         h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
    h_i xs = [(x1,x2)| x1 <- nub $ h_i1 [ y1 | (y1,y2) <- xs],
                       x2 <-       h_i2 [ y2 | (y1,y2) <- xs, y1 == x1]]
    h_l xs = [(x1,x2)| x1 <- nub $ h_l1 [ y1 | (y1,y2) <- xs],
                       x2 <-       h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
    h_s xs = [(x1,x2)| x1 <- nub $ h_s1 [ y1 | (y1,y2) <- xs],
                       x2 <-       h_s2 [ y2 | (y1,y2) <- xs, y1 == x1]]
 
 infix *-*
 (alg1 *-* alg2) basearray takes =
         (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) where
    (sadd1,cadd1,ambd1,nil1,edl1,edr1,edlr1,drem1,is1,sr1,hl1,sp1,bl1,br1,il1,
     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,cons1,ul1,combine1,acomb1,h1,h_i1,h_l1,h_s1) = alg1 basearray takes
    (sadd2,cadd2,ambd2,nil2,edl2,edr2,edlr2,drem2,is2,sr2,hl2,sp2,bl2,br2,il2,
     ml2,mldr2,mladr2,mldlr2,mladlr2,mldladr2,mladldr2,mldl2,mladl2,addss2,ssadd2,cons2,ul2,combine2,acomb2,h2,h_i2,h_l2,h_s2) = alg2 basearray takes

    sadd b (a1,a2) = (sadd1 b a1, sadd2 b a2)
    cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
    ambd (c1,c2) b (a1,a2) = (ambd1 c1 b a1, ambd2 c2 b a2)
    nil a = (nil1 a, nil2 a)
    edl b (c1,c2) = (edl1 b c1, edl2 b c2)
    edr (c1,c2) b = (edr1 c1 b, edr2 c2 b)
    edlr b (c1,c2) b' = (edlr1 b c1 b', edlr2 b c2 b')
    drem (c1,c2) = (drem1 c1, drem2 c2)
    is (c1,c2)   = (is1 c1, is2 c2)
    sr b (c1,c2) b' = (sr1 b c1 b', sr2 b c2 b')
    hl b1 b2 u b2' b1' = (hl1 b1 b2 u b2' b1', hl2 b1 b2 u b2' b1')
    sp b1 b2 (c1,c2) b2' b1' = (sp1 b1 b2 c1 b2' b1',sp2 b1 b2 c2 b2' b1') 
    bl u (c1,c2) = (bl1 u c1, bl2 u c2)
    br (c1,c2) u = (br1 c1 u, br2 c2 u)
    il r1 (c1,c2) r2 = (il1 r1 c1 r2 ,il2 r1 c2 r2) 
    ml b1 b2 (m1,m2) b2' b1' = (ml1 b1 b2 m1 b2' b1', ml2 b1 b2 m2 b2' b1')
    mldr b1 b2 (m1,m2) d b2' b1' = (mldr1 b1 b2 m1 d b2' b1',
                                    mldr2 b1 b2 m2 d b2' b1')
    mladr b1 b2 (m1,m2) d b2' b1' = (mladr1 b1 b2 m1 d b2' b1',
                                     mladr2 b1 b2 m2 d b2' b1')
    mldlr b1 b2 d (m1,m2) d_ b2' b1' = (mldlr1 b1 b2 d m1 d_ b2' b1',
                                        mldlr2 b1 b2 d m2 d_ b2' b1')
    mladlr b1 b2 d (m1,m2) d_ b2' b1' = (mladlr1 b1 b2 d m1 d_ b2' b1',
                                         mladlr2 b1 b2 d m2 d_ b2' b1')
    mldladr b1 b2 d (m1,m2) d_ b2' b1' = (mldladr1 b1 b2 d m1 d_ b2' b1',
                                          mldladr2 b1 b2 d m2 d_ b2' b1')
    mladldr b1 b2 d (m1,m2) d_ b2' b1' = (mladldr1 b1 b2 d m1 d_ b2' b1',
                                          mladldr2 b1 b2 d m2 d_ b2' b1')
    mldl b1 b2 d (m1,m2) b2' b1' = (mldl1 b1 b2 d m1 b2' b1', mldl2 b1 b2 d m2 b2' b1')
    mladl b1 b2 d (m1,m2) b2' b1' = (mladl1 b1 b2 d m1 b2' b1', mladl2 b1 b2 d m2 b2' b1')
    addss (c1,c2) u = (addss1 c1 u, addss2 c2 u)
    ssadd u (c1,c2) = (ssadd1 u c1, ssadd2 u c2)
    cons (c1,c2) (c_1,c_2) = (cons1 c1 c_1, cons2 c2 c_2)
    ul (c1,c2) = (ul1 c1, ul2 c2)
    combine (c1,c2) (c_1,c_2) = (combine1 c1 c_1, combine2 c2 c_2)
    acomb (c1,c2) b (c_1,c_2) = (acomb1 c1 b c_1, acomb2 c2 b c_2)

    h xs   = [(x1,x2)| x1 <- h1 [ y1 | (y1,y2) <- xs],
                       x2 <- h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
    h_i xs = [(x1,x2)| x1 <- h_i1 [ y1 | (y1,y2) <- xs],
                       x2 <- h_i2 [ y2 | (y1,y2) <- xs, y1 == x1]]
    h_l xs = [(x1,x2)| x1 <- h_l1 [ y1 | (y1,y2) <- xs],
                       x2 <- h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
    h_s xs = [(x1,x2)| x1 <- h_s1 [ y1 | (y1,y2) <- xs],
                       x2 <- h_s2 [ y2 | (y1,y2) <- xs, y1 == x1]]

Algebra cross product (sorting on 2nd argument):

 infix *+*
 (alg1 *+* alg2) basearray takes =
         (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
          ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) where
    (sadd1,cadd1,ambd1,nil1,edl1,edr1,edlr1,drem1,is1,sr1,hl1,sp1,bl1,br1,il1,
     ml1,mldr1,mladr1,mldlr1,mladlr1,mldladr1,mladldr1,mldl1,mladl1,addss1,ssadd1,cons1,ul1,combine1,acomb1,h1,h_i1,h_l1,h_s1) = alg1 basearray takes
    (sadd2,cadd2,ambd2,nil2,edl2,edr2,edlr2,drem2,is2,sr2,hl2,sp2,bl2,br2,il2,
     ml2,mldr2,mladr2,mldlr2,mladlr2,mldladr2,mladldr2,mldl2,mladl2,addss2,ssadd2,cons2,ul2,combine2,acomb2,h2,h_i2,h_l2,h_s2) = alg2 basearray takes

    sadd b (a1,a2) = (sadd1 b a1, sadd2 b a2)
    cadd (c1,c2) (a1,a2) = (cadd1 c1 a1, cadd2 c2 a2)
    ambd (c1,c2) b (a1,a2) = (ambd1 c1 b a1, ambd2 c2 b a2)
    nil a = (nil1 a, nil2 a)
    edl b (c1,c2) = (edl1 b c1, edl2 b c2)
    edr (c1,c2) b = (edr1 c1 b, edr2 c2 b)
    edlr b (c1,c2) b' = (edlr1 b c1 b', edlr2 b c2 b')
    drem (c1,c2) = (drem1 c1, drem2 c2)
    is (c1,c2)   = (is1 c1, is2 c2)
    sr b (c1,c2) b' = (sr1 b c1 b', sr2 b c2 b')
    hl b1 b2 u b2' b1' = (hl1 b1 b2 u b2' b1', hl2 b1 b2 u b2' b1')
    sp b1 b2 (c1,c2) b2' b1' = (sp1 b1 b2 c1 b2' b1',sp2 b1 b2 c2 b2' b1') 
    bl u (c1,c2) = (bl1 u c1, bl2 u c2)
    br (c1,c2) u = (br1 c1 u, br2 c2 u)
    il r1 (c1,c2) r2 = (il1 r1 c1 r2 ,il2 r1 c2 r2) 
    ml b1 b2 (m1,m2) b2' b1' = (ml1 b1 b2 m1 b2' b1', ml2 b1 b2 m2 b2' b1')
    mldr b1 b2 (m1,m2) d b2' b1' = (mldr1 b1 b2 m1 d b2' b1',
                                    mldr2 b1 b2 m2 d b2' b1')
    mladr b1 b2 (m1,m2) d b2' b1' = (mladr1 b1 b2 m1 d b2' b1',
                                     mladr2 b1 b2 m2 d b2' b1')
    mldlr b1 b2 d (m1,m2) d_ b2' b1' = (mldlr1 b1 b2 d m1 d_ b2' b1',
                                        mldlr2 b1 b2 d m2 d_ b2' b1')
    mladlr b1 b2 d (m1,m2) d_ b2' b1' = (mladlr1 b1 b2 d m1 d_ b2' b1',
                                         mladlr2 b1 b2 d m2 d_ b2' b1')
    mldladr b1 b2 d (m1,m2) d_ b2' b1' = (mldladr1 b1 b2 d m1 d_ b2' b1',
                                          mldladr2 b1 b2 d m2 d_ b2' b1')
    mladldr b1 b2 d (m1,m2) d_ b2' b1' = (mladldr1 b1 b2 d m1 d_ b2' b1',
                                          mladldr2 b1 b2 d m2 d_ b2' b1')
    mldl b1 b2 d (m1,m2) b2' b1' = (mldl1 b1 b2 d m1 b2' b1', mldl2 b1 b2 d m2 b2' b1')
    mladl b1 b2 d (m1,m2) b2' b1' = (mladl1 b1 b2 d m1 b2' b1', mladl2 b1 b2 d m2 b2' b1')
    addss (c1,c2) u = (addss1 c1 u, addss2 c2 u)
    ssadd u (c1,c2) = (ssadd1 u c1, ssadd2 u c2)
    cons (c1,c2) (c_1,c_2) = (cons1 c1 c_1, cons2 c2 c_2)
    ul (c1,c2) = (ul1 c1, ul2 c2)
    combine (c1,c2) (c_1,c_2) = (combine1 c1 c_1, combine2 c2 c_2)
    acomb (c1,c2) b (c_1,c_2) = (acomb1 c1 b c_1, acomb2 c2 b c_2)

    h xs   = sortIt [(x1,x2)| x1 <- nub $   h1 [ y1 | (y1,y2) <- xs],
                              x2 <-         h2 [ y2 | (y1,y2) <- xs, y1 == x1]]
      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))
    h_i xs = sortIt [(x1,x2)| x1 <- nub $ h_i1 [ y1 | (y1,y2) <- xs],
                              x2 <-       h_i2 [ y2 | (y1,y2) <- xs, y1 == x1]]
      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))
    h_l xs = sortIt [(x1,x2)| x1 <- nub $ h_l1 [ y1 | (y1,y2) <- xs],
                              x2 <-       h_l2 [ y2 | (y1,y2) <- xs, y1 == x1]]
      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))
    h_s xs = sortIt [(x1,x2)| x1 <- nub $ h_s1 [ y1 | (y1,y2) <- xs],
                              x2 <-       h_s2 [ y2 | (y1,y2) <- xs, y1 == x1]]
      where sortIt = sortBy (\b -> \a -> compare (snd a) (snd b))


The yield grammar for non-ambigous dangling bases:

 canonicals_nonamb takes alg inp_tr = axiom struct where
  
     (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
      ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) = alg baseArray takes

> #grammar{
> canonicals_nonamb takes alg inp_tr = axiom struct where
  
>     (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il, ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) = alg 

>     struct        = helixtype1   ||| 
>                     helixtype2   ... h_s     -- use h_s because of choice function recognition bug

helixtype1: no singlestrand on lefthand side
helixtype2: singlestrand on lefthand side

>     helixtype1    = tabulated (
>                     ambd <<< edanglel  ~~- lbase ~~~ noleft_dangle              ||| 
>                     ambd <<< nodangle  ~~- lbase ~~~ noleft_dangle              ||| 
>                     cadd <<< edanglel  ~~~ (noleft_dangle ||| (nil <<< empty)) |||
>                     cadd <<< nodangle  ~~~ (noleft_dangle ||| (nil <<< empty)) |||
>                     cadd <<< edangler  ~~~ (left_dangle ||| helixtype2)        |||   
>                     cadd <<< edanglelr ~~~ (left_dangle ||| helixtype2)        |||   
>                     nil  <<< empty                                             ... h)

>     helixtype2    = tabulated (
>                     sadd <<< lbase    -~~ helixtype2     |||
>                     sadd <<< lbase    -~~ left_dangle    ... h)

>     left_dangle   = tabulated (
>                     ambd <<< edanglel  ~~- lbase ~~~ noleft_dangle              ||| 
>                     cadd <<< edanglel  ~~~ (noleft_dangle ||| (nil <<< empty)) |||
>                     cadd <<< edanglelr ~~~ (left_dangle ||| helixtype2)        |||
>                     nil  <<< empty                                             ... h)

>     noleft_dangle = tabulated (
>                     cadd <<< edangler ~~~ (left_dangle  ||| helixtype2)       |||
>                     cadd <<< nodangle ~~~ (noleft_dangle ||| (nil <<< empty)) |||
>                    ambd <<< nodangle  ~~- lbase ~~~ noleft_dangle             ... h)
             
     edanglel      = edl  <<< lbase -~~ initstem           ... h_l
     edangler      = edr  <<<           initstem ~~- lbase ... h_l
     edanglelr     = edlr <<< lbase -~~ initstem ~~- lbase ... h_l
     nodangle      = drem <<<           initstem           ... h_l

>     edanglel      = edl  <<< lbase -~~ (initstem ... h_l)
>     edangler      = edr  <<<           (initstem ... h_l) ~~- lbase 
>     edanglelr     = edlr <<< lbase -~~ (initstem ... h_l) ~~- lbase 
>     nodangle      = drem <<<           (initstem ... h_l)

>     initstem      = is <<< closed

>     closed        = tabulated (
>                     stack ||| hairpin ||| multiloop ||| leftB ||| rightB ||| iloop ... h) 

>                     
>     multiloop     = (mldl   <<< lbase -~~ lbase ~~-   lbase ~~~ ml_comps1          ~~- lbase ~~- lbase |||
>                      mladl  <<< lbase -~~ lbase ~~-   lbase ~~~ ml_comps2          ~~- lbase ~~- lbase |||  -- ambiguous dangle
>                      mldr   <<< lbase -~~ lbase ~~~             ml_comps3 ~~- lbase ~~- lbase ~~- lbase |||
>                      mladr  <<< lbase -~~ lbase ~~~             ml_comps2 ~~- lbase ~~- lbase ~~- lbase |||  -- ambiguous dangle
>                      mldlr  <<< lbase -~~ lbase ~~-   lbase ~~~ ml_comps4 ~~- lbase ~~- lbase ~~- lbase |||
>                      mladlr <<< lbase -~~ lbase ~~-   lbase ~~~ ml_comps2 ~~- lbase ~~- lbase ~~- lbase |||  -- ambiguous dangle both
>                      mldladr<<< lbase -~~ lbase ~~-   lbase ~~~ ml_comps1 ~~- lbase ~~- lbase ~~- lbase |||  -- ambiguous dangle right
>                      mladldr<<< lbase -~~ lbase ~~-   lbase ~~~ ml_comps3 ~~- lbase ~~- lbase ~~- lbase |||  -- ambiguous dangle left
>                      ml     <<< lbase -~~ lbase ~~~             ml_comps2 ~~- lbase ~~- lbase ) `with` stackpairing     ... h

>     ml_comps1     = tabulated (
>                     combine <<< block_dl  ~~~ no_dl_no_ss_end          |||
>                     combine <<< block_dlr ~~~ dl_or_ss_left_no_ss_end  |||
>                    acomb   <<< block_dl  ~~- lbase ~~~ no_dl_no_ss_end ... h_i)

>     ml_comps2     = tabulated (
>                     combine <<< (ul <<< nodangle) ~~~ no_dl_no_ss_end          |||
>                     combine <<< (ul <<< edangler) ~~~ dl_or_ss_left_no_ss_end  |||
>                    acomb   <<< (ul <<< nodangle) ~~- lbase ~~~ no_dl_no_ss_end ... h_i)

>     ml_comps3     = combine <<< (ul <<< edangler) ~~~ dl_or_ss_left_ss_end  |||
>                     combine <<< (ul <<< nodangle) ~~~ no_dl_ss_end          |||
>                    acomb   <<< (ul <<< nodangle) ~~- lbase ~~~ no_dl_ss_end ... h_i

>     ml_comps4     = combine <<< block_dl  ~~~  no_dl_ss_end        |||
>                     combine <<< block_dlr ~~~ dl_or_ss_left_ss_end |||
>                    acomb   <<< block_dl ~~- lbase ~~~ no_dl_ss_end ... h_i

>     block_dl      = tabulated( 
>                     ssadd <<< region ~~~ edanglel |||
>                    ul    <<< edanglel            ... h_i)

>     block_dlr     = tabulated( 
>                     ssadd <<< region ~~~ edanglelr |||
>                    ul    <<< edanglelr            ... h_i)

>     no_dl_no_ss_end =        ml_comps2 |||
>                      ul <<< nodangle  -- ... h_i

>     dl_or_ss_left_no_ss_end = ml_comps1 |||
>                             block_dl  -- ... h_i

>     no_dl_ss_end = tabulated (
>                               ml_comps3                    |||
>                      ul    <<< edangler                     |||
>                      addss <<< (ul <<< edangler) ~~~ region ... h_i)

>     dl_or_ss_left_ss_end = tabulated (
>                                      ml_comps4            |||
>                                       block_dlr            |||
>                             addss <<< block_dlr ~~~ region ... h_i)

>     stack         = (sr  <<< lbase -~~ closed ~~- lbase) `with` basepairing

>     hairpin       = (hl  <<< lbase -~~ lbase ~~~ (region `with` (minsize 3))
>                          ~~- lbase ~~- lbase)
>                     `with` stackpairing

>     leftB         = (sp  <<< lbase -~~ lbase ~~~ (bl <<< (region `with` (maxsize maxloop))  ~~~ initstem)
>                          ~~- lbase ~~- lbase)
>                     `with` stackpairing -- ... h

>     rightB        = (sp  <<< lbase -~~ lbase ~~~ (br <<< initstem ~~~ (region `with` (maxsize maxloop)))
>                          ~~- lbase ~~- lbase)
>                     `with` stackpairing -- ... h

>     iloop         = (sp  <<< lbase -~~ lbase ~~~ (il <<< (region `with` (maxsize maxloop)) ~~~ closed ~~~ (region `with` (maxsize maxloop)))
>                          ~~- lbase ~~- lbase)
>                     `with` stackpairing -- ... h

> }

Bind input:

     inp       = translate inp_tr
     axiom     = axiom' n
     z         = mk (inp)
     (_,n)     = bounds z
     baseArray     = (fst.str2inp) inp
     base (i,j)= [ j | (i+1) == j ]
     region (i,j) =  [(i,j) | i < j]
     tabulated = table n
     listed :: Parser a -> Parser a
     listed p = q $ array (0,n) [(j, p (j,n)) | j <- [0..n]] where
       q t (i,j) = if j==n then t~i else []
     
     infixl 7 ~~~
     (~~~) = (~~*) (2,2) 3
     infixl 7 ~~~
     (~~~) = (~~*) (3,3) 14
     minloopsize :: Int -> Filter
     minloopsize n = match inp where
       match inp (i,j) = i+n<=j
     maxsize n = match inp where
       match inp (i,j) = j-i<=n
     stackpairing :: Filter
     stackpairing  = match inp where
       match inp (i,j) = i+3<j && basepair (z!(i+1), z!(j))
                               && basepair (z!(i+2), z!(j-1))
     basepairing :: Filter
     basepairing  = match inp where
       match inp (i,j) = i+1<j && basepair (z!(i+1), z!(j))
     basepair ('A','U') = True
     basepair ('U','A') = True
     basepair ('C','G') = True
     basepair ('G','C') = True
     basepair ('G','U') = True
     basepair ('U','G') = True
     basepair ( x , y ) = False


 canonicals takes alg inp_tr = axiom struct where
  
     (sadd,cadd,ambd,nil,edl,edr,edlr,drem,is,sr,hl,sp,bl,br,il,
      ml,mldr,mladr,mldlr,mladlr,mldladr,mladldr,mldl,mladl,addss,ssadd,cons,ul,combine,acomb,h,h_i,h_l,h_s) = alg baseArray takes

     struct        = listed ( 
                     sadd <<< base    -~~ struct |||
                     cadd <<< edangle ~~~ struct |||
                     nil  <<< empty              ... h)

     edangle       = edl  <<< base -~~ initstem          |||
                     edr  <<<          initstem ~~- base |||
                     edlr <<< base -~~ initstem ~~- base |||
                     drem <<<          initstem          ... h_l

     initstem      = is <<< closed 

     closed        = tabulated (
                     stack ||| hairpin ||| leftB ||| rightB ||| iloop ||| multiloop ... h)

     multiloop     = (mldl  <<< base -~~ base ~~- base ~~~ ml_components
                            ~~- base ~~- base |||
                      mldr  <<< base -~~ base ~~!           ml_components ~~- base
                            ~~- base ~~- base |||
                      mldlr <<< base -~~ base ~~- base ~~~ ml_components ~~- base
                            ~~- base ~~- base |||
                      ml    <<< base -~~ base ~~!           ml_components
                            ~~- base ~~- base )
                      `with` stackpairing ... h

     ml_components = combine <<< block ~~~ comps ... h_i

     comps         = tabulated (
                     cons  <<< block ~~~ comps  |||
                               block            |||
                     addss <<< block ~~~ region ... h_i)

     block         = tabulated (
                     ul    <<< edangle            |||
                     ssadd <<< region ~~~ edangle ... h_i)

     stack         = (sr  <<< base -~~ closed ~~- base) `with` basepairing

     hairpin       = (hl  <<< base -~~ base ~~! (region `with` minloopsize 3)
                          ~~- base ~~- base)
                     `with` stackpairing 

     leftB         = (sp  <<< base -~~ base ~~! (bl <<< region  ~~~ initstem)
                          ~~- base ~~- base)
                     `with` stackpairing -- ... h

     rightB        = (sp  <<< base -~~ base ~~! (br <<< initstem ~~~ region)
                          ~~- base ~~- base)
                     `with` stackpairing -- ... h

     iloop         = (sp  <<< base -~~ base ~~! (il <<< (region `with` (maxsize 30)) ~~~ closed ~~~ (region `with` (maxsize 30)))
                          ~~- base ~~- base)
                     `with` stackpairing -- ... h

ind input:

     inp       = translate inp_tr
     axiom     = axiom' n
     z         = mk (inp)
     (_,n)     = bounds z
     baseArray     = (fst.str2inp) inp
     base (i,j)= [ j | (i+1) == j ]
     region (i,j) =  [(i,j) | i < j]
     tabulated = table n
     listed :: Parser a -> Parser a
     listed p = q $ array (0,n) [(j, p (j,n)) | j <- [0..n]] where
       q t (i,j) = if j==n then t!i else []
     
     infixl 7 ~~!
     (~~!) = (~~*) (2,2) 3
     infixl 7 ~~~
     (~~~) = (~~*) (3,3) 14
     minloopsize :: Int -> Filter
     minloopsize n = match inp where
       match inp (i,j) = i+n<=j
     maxsize n = match inp where
       match inp (i,j) = j-i<=n
     stackpairing :: Filter
     stackpairing  = match inp where
       match inp (i,j) = i+3<j && basepair (z!(i+1), z!(j))
                               && basepair (z!(i+2), z!(j-1))
     basepairing :: Filter
     basepairing  = match inp where
       match inp (i,j) = i+1<j && basepair (z!(i+1), z!(j))
     basepair ('A','U') = True
     basepair ('U','A') = True
     basepair ('C','G') = True
     basepair ('G','C') = True
     basepair ('G','U') = True
     basepair ('U','G') = True
     basepair ( x , y ) = False


 translate :: [Char] -> [Char]
 translate [] = []
 translate (x:xs) 
       | x == 't'  = 'U' : translate xs 
       | x == 'T'  = 'U' : translate xs
       | x == 'u'  = 'U' : translate xs
       | x == 'U'  = 'U' : translate xs
       | x == 'a'  = 'A' : translate xs 
       | x == 'A'  = 'A' : translate xs 
       | x == 'c'  = 'C' : translate xs 
       | x == 'C'  = 'C' : translate xs 
       | x == 'g'  = 'G' : translate xs 
       | x == 'G'  = 'G' : translate xs
       | otherwise =  error ("Wrong Character in sequence: "++x: xs)


pp verieinigt beim zusammenfuegen der strings aufeinanderfolgende  "_"s zu einem "_"

 app :: String -> String -> String
 app [] ys = ys
 app "_" "_" = "_"
 app (x:[]) (y:[]) = x:y:[]
 app (x:[]) (y:ys) = app (app (x:[]) (y:[])) ys
 app (x:xs) ys = x : app xs ys 
