
userdefs{
filter stackpairing = (4,Infinite)
filter basepairing = (2,Infinite)
directdef pknot   = (11, Infinite)
}



> #algebratype{

> type FS_Algebra base cmpl = (   
>            base -> cmpl -> cmpl ,  -- sadd
>	     cmpl -> cmpl -> cmpl ,  -- cadd

>	     base -> cmpl -> base -> cmpl, --is
>	     base -> cmpl -> base -> cmpl, --sr
>	     base -> base -> (Int,Int) -> base -> base         -> cmpl,   --hl
>	     base -> base -> (Int,Int) -> cmpl -> base -> base -> cmpl,   --bl
>	     base -> base -> cmpl -> (Int,Int) -> base -> base -> cmpl,   --br

>	     base -> base -> (Int,Int) -> cmpl -> (Int,Int) -> base -> base -> cmpl, --il

>	     base-> base->         cmpl       -> base-> base -> cmpl, --ml
>	     base-> base-> base -> cmpl	    -> base-> base -> cmpl, --mldl
>	     base-> base->         cmpl -> base-> base-> base -> cmpl, --mldr
>	     base-> base-> base -> cmpl -> base-> base-> base -> cmpl, --mldlr

>	     base -> cmpl -> base -> cmpl, --dl
>	     base -> cmpl -> base -> cmpl, --dr
>	     base -> cmpl -> base -> cmpl, --dlr
>	     base -> cmpl -> base -> cmpl, --edl
>	     base -> cmpl -> base -> cmpl, --edr
>	     base -> cmpl -> base -> cmpl, --edlr
>	     cmpl -> cmpl,		   -- drem
>	     cmpl -> cmpl -> cmpl,     -- cons (in fact it is append)
>	     cmpl ->  cmpl,	       -- ul
>	     cmpl ->  cmpl,	       -- pul
>	     cmpl -> (Int,Int) -> cmpl,   -- addss
>	     (Int,Int) -> cmpl -> cmpl,   -- ssadd

>	     Int -> cmpl,  --nil

>            cmpl -> cmpl -> cmpl, --combine
>            base -> (cmpl, Int)  -> base -> (cmpl,Int),  --sum
>            base -> (Int,Int)    -> base -> (cmpl,Int),  --sumend

>	     base -> cmpl -> base -> cmpl -> base -> cmpl, -- flush
>	     base -> cmpl -> base -> cmpl -> base -> cmpl, -- nflush
>	     cmpl				  -> cmpl, -- co
>	     (Int,Int) -> cmpl			  -> cmpl, -- cor
>	             (cmpl, Int, Int)		  -> cmpl, -- pk
>	     base -> (cmpl, Int, Int)		  -> cmpl, -- pkmldl
>	             (cmpl, Int, Int) -> base	  -> cmpl, -- pkmldr
>	     base -> (cmpl, Int, Int) -> base	  -> cmpl, -- pkmldlr
>                    (cmpl, Int, Int)		  -> cmpl, -- pkml
>
>	     Int -> (Int,Int) -> cmpl -> (Int,Int) -> cmpl -> 
>		      (Int,Int) -> cmpl -> (Int,Int) -> (cmpl, Int, Int), -- pk'
>	     
>	     base -> (cmpl, Int, Int)         -> cmpl, -- kndl
>	             (cmpl, Int, Int) -> base -> cmpl, -- kndr 
>	     base -> (cmpl, Int, Int) -> base -> cmpl, -- kndlr

>	     Int -> cmpl -> base -> cmpl, -- frd
>	     Int -> base -> cmpl -> cmpl, -- bkd
>	     base -> (cmpl, Int, Int) -> base	 -> (cmpl,Int,Int,Int), -- scale
>	     (cmpl,Int,Int,Int) -> cmpl,  --unscale

>	     Int -> Int -> Int  	       	         -> cmpl, --emptymid
>	     Int -> Int -> Int		 -> cmpl, --midbase
>	     Int -> Int -> Int -> Int		 -> cmpl, --middlro
>	     Int -> base -> cmpl	 -> cmpl,   --middl
>	     Int -> cmpl -> base -> cmpl,   --middr
>	     Int -> Int -> base -> cmpl -> base -> cmpl,   --middlr
>	     cmpl -> cmpl,     --midregion
>	     (Int,Int) -> cmpl,   --pss	      

>            base -> (cmpl,Int,Int,Int) -> (cmpl,Int,Int,Int),  --skipleft
>            (cmpl,Int,Int,Int) -> base -> (cmpl,Int,Int,Int),  --skipright

>	     [cmpl] -> [cmpl]  --h_p


>            )  

> }


> #algebra[energy]{

> energy :: FS_Algebra Int Int 
> energy  =  (sadd,cadd,is,sr,hl,bl,br, il,
>	       ml, mldl, mldr, mldlr, dl, dr, dlr, edl, edr, edlr,
>	       drem, cons, ul, pul, addss, ssadd, nil, combine, sum, sumend, 
>              flush, nflush, co, cor, pk, pkmldl, pkmldr, pkmldlr, pkml, pk',
>	       kndl, kndr,  kndlr, frd, bkd, scale, unscale, emptymid,
>	       midbase, middlro, middl, middr, middlr, midregion, pss, skipleft, skipright, h)

>     where
>       sadd  lb e = e
>       cadd  e1 e = e1 + e

>       is  lloc     e rloc    = e  + termaupenalty (lloc+1,rloc)
>       sr  lb       e rb     = e + sr_energy (lb,rb)
>       hl  llb lb _   rb rrb =     hl_energy (lb,rb) + sr_energy (llb,rrb) 
>       bl  llb bl (i,j) e br rrb = e + bl_energy (bl,i,j,br) + sr_energy (llb,rrb)
>       br  llb bl e (i,j) br rrb = e + br_energy (bl,i,j,br) + sr_energy  (llb,rrb)

>       il llb lb (i,j) e (k,l) rb rrb    = e + sr_energy (llb,rrb) + il_energy (i,j,k,l)

>       ml    llb bl    e    br rrb = 380 +  e + sr_energy  (llb,rrb) + termaupenalty (bl,br)
>       mldl  llb bl dl e    br rrb = 380 +  e + dli_energy  (bl,br) + sr_energy  (llb,rrb) + termaupenalty (bl,br)
>       mldr  llb bl    e dr br rrb = 380 +  e + dri_energy  (bl,br) + sr_energy  (llb,rrb) + termaupenalty (bl,br)
>       mldlr llb bl dl e dr br rrb = 380 +  e + dli_energy  (bl,br) + dri_energy  (bl,br) + sr_energy  (llb,rrb) + termaupenalty (bl,br)
> 
>       dl    dl e rloc = e + dl_energy  (dl+1,rloc)
>       dr  lloc e dr   = e + dr_energy  (lloc+1,dr-1)
>       dlr   dl e dr   = e + dl_energy  (dl+1,dr-1) + dr_energy  (dl+1,dr-1)
>       edl   dl e rloc = e + dl_energy  (dl+1,rloc) + termaupenalty (dl+1,rloc)
>       edr lloc e dr   = e + dr_energy  (lloc+1,dr-1) + termaupenalty (lloc+1,dr-1)
>       edlr  dl e dr   = e + dl_energy  (dl+1,dr-1) + dr_energy  (dl+1,dr-1) + termaupenalty (dl+1,dr-1)
>       drem     e      = e 

>       cons  e1 e   = e1 + e
>       addss    e (i,j) =      e + ss_energy (i,j)
>       ul       e   = 40 + e
>       pul      e   =      e
>       ssadd (i,j)  e   = 40 + e + ss_energy (i,j)
>       nil _ = 0

>       combine a b = a+b

>       sum      lb (c,k) rb   = (c + sr_energy (lb, rb), k+1)
>       sumend   lb _     rb   = (0,1)

>       flush lloc e1 mloc e2 rloc = 80 + e1 + e2 - 100 + stack_dg_ac(lloc+1,rloc,mloc+1,mloc)
>       nflush lloc e1 mb e2 rloc  = 80 + e1 + e2       + stack_dg_ac(lloc+1,rloc,mb+1,mb-1)

>       co e = e
>       cor r e1 = ss_energy r + e1
>       pk         (e,k,l)     = e
>       pkmldl  lb (e,k,l)     = e + 600 + wkn *  dl_energy  (lb+1,l)
>       pkmldr     (e,k,l) rb  = e + 600 + wkn *  dr_energy  (k+1,rb-1) 
>       pkmldlr lb (e,k,l) rb  = e + 600 + wkn * (dl_energy  (lb+1,l) + dr_energy  (k+1,rb-1))
>       pkml       (e,k,l)     = e + 600
 
>       pk' e a fro (i',j') mid (k,l) bac b  =  (pkinit + e + 3*npp +  fro +  mid +  bac  + dangles  a (i',j') (k,l) b, i',l)
 								
>       kndr      (e,k,l) rb  = e +   npp + wkn * dr_energy  (k+1,rb-1)
>       kndl   lb (e,k,l)     = e +   npp + wkn * dl_energy  (lb+1,l)
>       kndlr  lb (e,k,l) rb  = e + 2*npp + wkn * (dl_energy  (lb+1,l)+ dr_energy  (k+1,rb-1))

>       frd j e base = e+ wkn * dl_energy  (base+1,j) + npp     -- base dangling of pseudoknot stem
>       bkd i   base e = e+ wkn * dr_energy  (i+1,base-1) + npp    -- base dangling of pseudoknot stem
>
>       scale    i (e,k,l) j =  ((e * 1000) / (j-i), j-i, i,j )
>       unscale     (e,i,_,_)    = (e * i) / 1000

>       emptymid k l i =  wkn * stack_dg_ac(l,k+1,i,i+1)     
>       midbase  k l j =  wkn * stack_dg_ac(l,k+1,j-1,j+1) + npp 
>       middlro  k l i j =  2*npp + wkn *  (dri_energy  (l,i+2)+ dli_energy  (i-1,k+1))
 
>       middl   k   lb e    = e+ npp + wkn * dli_energy  (lb-1,k+1) 
>       middr     l    e rb = e+ npp + wkn * dri_energy  (l,rb+1)
>       middlr  k l lb e rb = e+ 2*npp + wkn *  (dri_energy  (l,rb+1)+ dli_energy  (lb-1,k+1))

>       midregion e = e
>       pss  (i,j)  = sspenalty (j-i)

>       skipleft _ c   = c
>       skipright c _ = c


>       h   es = [minimum es]

> }

> #algebra[pp]{

> pp :: FS_Algebra Int String
> pp  =  (sadd,cadd,is,sr,hl,bl,br, il,
>	       ml, mldl, mldr, mldlr, dl, dr, dlr, edl, edr, edlr,
>	       drem, cons, ul, pul, addss, ssadd, nil, combine, sum, sumend, 
>              flush, nflush, co, cor, pk, pkmldl, pkmldr, pkmldlr, pkml, pk',
>	       kndl, kndr,  kndlr, frd, bkd, scale, unscale, emptymid,
>	       midbase, middlro, middl, middr, middlr, midregion, pss, skipleft, skipright, h)
 
>     where
>       sadd  lb e = "." ++ e		
>       cadd  x  e = x ++ e

>       is _ x _ = x
>       sr  lb e rb = "(" ++ e ++ ")"	
>       hl  llb lb r   rb rrb = "((" ++ dots r ++"))"	
>       bl  llb bl x e br rrb = "((" ++ dots x ++ e ++"))"
>       br  llb bl e x br rrb = "((" ++ e ++ dots x ++"))"

>       il   llb lb lr x rr rb rrb = "((" ++ dots lr  ++ x ++ dots rr ++ "))" 

>       ml    llb bl    x    br rrb = "((" ++ x ++ "))" 
>       mldl  llb bl dl x    br rrb = "((."++ x ++ "))"
>       mldr  llb bl    x dr br rrb = "((" ++ x ++ ".))" 
>       mldlr llb bl dl x dr br rrb = "((."++ x ++ ".))"
>       dl    dl x _  = "."++ x	  
>       dr    _  x dr =       x ++"."	  
>       dlr   dl x dr = "."++ x ++"."	  
>       edl   dl x _  = "."++ x	  
>       edr   _  x dr =       x ++"."	  
>       edlr  dl x dr = "."++ x ++"."	       
>       drem     x    = x	  

>       cons  c1 c = c1 ++ c
>       ul  c1 = c1
>       pul  c1 = c1
>       addss  c1 r = c1 ++ dots r
>       ssadd  r x = dots r ++ x		
>       nil _ = ""

>       combine  a b     = a ++ b

>       sum       _ k _     = ""
>       sumend    _ _ _     = ""


>       flush   _ c1 _ c2 _ = c1 ++ c2
>       nflush  _ c1 _ c2 _ = c1 ++"."++ c2

>       co  c1 = c1
>       cor  r c1 = dots r ++ c1

>       pk	   (c,_,_)   = c
>       pkmldl   _ (c,_,_)   = "." ++c
>       pkmldr     (c,_,_) _ = c ++ "."
>       pkmldlr  _ (c,_,_) _ = "." ++ c ++ "."
>       pkml	   (c,_,_)   = c

>       pk' _ (a1,a2) u (b1,b2) v (a1',a2') w (b1',b2') = open1 (a1,a2) ++ "." ++ u ++ open2 (b1,b2) ++ v ++ close1 (a1',a2') ++ w ++ ".." ++ close2 (b1',b2')


>       kndl    _ (c,_,_)   = "." ++ c
>       kndr      (c,_,_) _ = c ++ "."
>       kndlr   _ (c,_,_) _ = "." ++ c ++ "."

>       frd _     c _ = c ++"."
>       bkd _  _  c   = "." ++ c 

>       scale    i (c,k,l) j = c -- (c,j-i)
>       unscale (c,l,_,_) = c

>       emptymid _ _ _ =  ""
>       midbase  _ _ _ =  "." 
>       middlro  _ _ _ _ = ".."
>       middl    _    _ c	= "."++ c
>       middr    _    	c _	=  c ++ "."
>       middlr   _ _     _ c _     = "." ++ c ++ "."
>       midregion  c		= c
>       pss  r			= dots r

>       skipleft _ (c,_,_,_)   = c
>       skipright (c,_,_,_) _ = c

>       h   es = [id es]

> }



> #grammar{

> pknotsrg alg f = axiom struct where

>   (sadd,cadd,is,sr,hl,bl,br, il,
>	       ml, mldl, mldr, mldlr, dl, dr, dlr, edl, edr, edlr,
>	       drem, cons, ul, pul, addss, ssadd, nil, combine, sum, sumend, 
>              flush, nflush, co, cor, pk, pkmldl, pkmldr, pkmldlr, pkml, pk',
>	       kndl, kndr,  kndlr, frd, bkd, scale, unscale, emptymid,
>	       midbase, middlro, middl, middr, middlr, midregion, pss, skipleft, skipright, h) = alg

>   struct        = listed (
>	            sadd <<< lbase    +~~  struct |||
>                   cadd <<< edangle ~~~  struct |||
>                   nil <<< empty ... h)
>              --where   

>   edangle      =  tabulated(
>                    edl  <<< lbase +~~ closed ~~. loc  |||
>	             edr  <<<  loc .~~ closed ~~+ lbase |||
>		     edlr <<< lbase +~~ closed ~~+ lbase |||
>	             drem <<<          initstem          |||
>	      	     kndr <<<          knot   ~~+ lbase |||
>		     kndl <<< lbase +~~ knot 		 |||
>		     kndlr <<< lbase +~~ knot   ~~+ lbase |||
>		     pk   <<<	       knot	 	 ... h)
>                --where
>   initstem =  is <<< loc .~~ closed ~~. loc 
>   closed      = tabulated (
>		    stack ||| (hairpin ||| leftB   ||| rightB ||| iloop  ||| multiloop) `with` stackpairing ... h)
>                 --where
>   stack     = (sr  <<< lbase +~~ closed ~~+ lbase)  `with` basepairing 
>   hairpin   = (hl  <<< lbase +~~ lbase     ++~ (region `with` (minsize 3))     ~~+ lbase ~~+ lbase) 
>   leftB     = (bl  <<< lbase +~~ lbase     ++~ region ~~~ initstem             ~~+ lbase ~~+ lbase) ... h
>   rightB    = (br  <<< lbase +~~ lbase     ++~	      initstem ~~~ region  ~~+ lbase ~~+ lbase) ... h


>   iloop     = (il  <<< lbase +~~ lbase     ++~ (region `with` (maxsize 32)) ~~~ closed ~~~ 
>                                                  (region `with` (maxsize 30)) ~~+ lbase ~~+ lbase) ... h 

>   multiloop = (mldl  <<< lbase +~~ lbase ++~ lbase ~~~ ml_components          ~~+ lbase ~~+ lbase |||
>                    mldr  <<< lbase +~~ lbase ++~ 	  ml_components ~~+ lbase ~~+ lbase ~~+ lbase |||
>                    mldlr <<< lbase +~~ lbase ++~ lbase ~~~ ml_components ~~+ lbase ~~+ lbase ~~+ lbase |||
>                    ml    <<< lbase +~~ lbase ++~ 	  ml_components          ~~+ lbase ~~+ lbase)  ... h
>                    --where
>   ml_components = combine <<< block ~~~ comps    
                   
>   comps     = tabulated (
>		    cons  <<< block ~~~ comps  |||
>                             block              |||
>                   addss <<< block ~~~ region ... h)
>
>   block     = tabulated (
>		    ul      <<<		   dangle          |||
>                   ssadd   <<< region ~~~ dangle	   |||
>		    pkmldl  <<< lbase   +~~ knot	   ||| 
>		    pkmldr  <<<		   knot ~~+ lbase |||
>		    pkmldlr <<< lbase   +~~ knot ~~+ lbase |||
>		    pkml    <<<		   knot          ... h)    -- pk inside multiloop
>                   --where

>   dangle  = dl   <<< lbase +~~  initstem ~~. loc  |||
>		    dr   <<<  loc .~~  initstem ~~+ lbase |||
>		    dlr  <<< lbase +~~  initstem ~~+ lbase |||
>	            drem <<<           initstem          ... h

>   knot      = tabulated ( pknot  `with` (size (11,max_pknot_length))                  ... h)
>   pknot    = \(i,j) -> [pk' energy a u b v a' w b'  | l <- [i+2 .. j-1], k <- [l+1 .. j-2],  
			
	    The helices a and b should not overlap each other and not exceed [l...k]. Furthermore each helix must have 
	    a minimum length of two bases. Due to stereochemical reasons one base in the front part and two
	    bases in the back part are left explicitely unpaired. These bases should brigde the stacks. 

>				        let (_,h)         = stacklen (i,k), 
>					h >= 2, 
>					let (_, betalen)  = stacklen (l,j),
>                                       let h' = if (betalen + h) > (k-l) then k-l-h else betalen,
>                                       h' >= 2,

>					let (betanrg,  _) = stacklen (l,j),
>				        let (alphanrg, _) = stacklen (i,k), 
>					let (correctionterm, _) = stacklen (max(0,l+h'-1),min(j-h'+1,n)), 	
>                                       let middle_k = j-h',
>                                       let middle_l = i+h,
>                                       let energy  = alphanrg + betanrg - correctionterm + pbp * (h+h'),

>					a <- region   (i     , i+h  ),
>					u <- front    (i+h+1,    l  ),
>					b <- region   (l    , l+h'  ),
>					v <- middle   (l+h', k-h  ),
>					a'<- region   (k-h  , k     ),
>					w <- back     (k    , j-h'-2),
>					b'<- region   (j-h' , j     )		 
>    					]		 
>				      	     	     

>   front      = co          <<< front'                  |||        
>	         frd j     <<< front' ~~+ lbase         ... h    -- one base dangling of b,b'


>   front'     = pul	     <<< emptystrand             |||	      
>	         comps                 ... h
>
>   middle     = emptymid lc_middle_k lc_middle_l <<< empty                     |||
>	  	 midbase  lc_middle_k lc_middle_l <<< lbase                     |||
>		 middlro  lc_middle_k lc_middle_l <<< lbase ~~~ lbase           ||| 
>	         middl    lc_middle_k             <<< lbase +~~ mid             |||
>	         middr                lc_middle_l <<<           mid ~~+ lbase   |||
>	         middlr   lc_middle_k lc_middle_l <<< lbase +~~ mid ~~+ lbase   |||
>	         midregion                        <<<           mid            ... h
>   mid        = pul         <<< singlestrand            |||        
>		 comps               ... h
>	    
>   back       = co          <<< back'			 |||   --  one base dangling of a,a'
>		 bkd i     <<< lbase +~~ back'          ... h
>   back'      = pul <<< emptystrand                     |||  
>		 comps                         ... h 

>   singlestrand  = pss <<< region 
>   emptystrand   = pss <<< uregion 

>   stacklen = tabulated(
>              (sum    <<<   lbase +~~ stacklen                  ~~+ lbase)  `with` basepairing  |||
>              (sumend <<<   lbase +~~ (region `with` (minsize 3)) ~~+ lbase)  `with` basepairing  ... h)

> }


