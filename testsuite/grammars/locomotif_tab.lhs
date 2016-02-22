> #algebratype{

>  type FS_Algebra base comp = (
>  		base -> comp -> comp ,  -- sadd
>  		comp -> comp -> comp ,  -- cadd
>  		comp -> comp -> comp ,  -- mlcons
>  		base -> comp   -> base -> comp, --dlr
>  		base -> comp   -> base -> comp, --sr
>  		base -> (Int,Int) -> base -> comp, --hl
>  		base -> (Int,Int) -> comp -> base -> comp, --bl
>  		base -> comp -> (Int,Int) -> base -> comp, --br
>  		base -> (Int,Int) -> comp -> (Int,Int) -> base -> comp, --il
>  		base -> comp -> base -> comp, --ml
>  		comp -> comp -> comp,     -- append
>  		comp -> comp,	       -- ul
>  		comp -> (Int,Int) -> comp,   -- addss
>  		comp -> (Int,Int) -> comp,   -- addssml
>  		(Int,Int) -> comp -> comp,   -- ssadd
>  		(Int,Int) -> comp ,	       -- ss
>  		base -> comp,  --nil
>  		Int -> (Int,Int) -> comp -> (Int,Int) -> comp -> (Int,Int) -> comp -> (Int,Int) -> comp,  --pk
>  		comp -> comp,           -- pul
>  		(Int,Int) -> comp,       -- pss

>  		base -> (comp,Int) -> base -> (comp,Int),  --sum
>  		base -> (Int,Int) -> base -> (comp,Int),  --sumend
>  		[comp] -> [comp]  --h
>  		) 
>  }


> #algebra[mfe]{

>   mfe :: FS_Algebra Int Int 
>   mfe = (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, addssml, ssadd, ss, nil, pk, pul, pss, sum, sumend, h) where
>  	sadd  lb e = e 
>  	cadd  e1 e = e1 + e
>  	mlcons e1 e = e1 + e + 40
>  	dlr dl    e    dr  = e + dl_energy (dl+1,dr) + dr_energy (dl+1,dr) + termaupenalty (dl+1,dr)
>  	sr  lb        e    rb  = e + sr_energy  (lb,rb)
>  	hl  lb        _    rb  =     hl_energy  (lb,rb) 
>  	bl  lb  (i,j) e    rb  = e + bl_energy  (lb,i,j, rb)
>  	br  lb        e (i,j)  rb  = e + br_energy (lb,i,j, rb)
>  	il  lb (i,j) e (k,l) rb= e + il_energy (i,j,k,l) 
>  	ml  lb       e     rb  = 380 + e + termaupenalty (lb,rb) + dli_energy (lb,rb) + dri_energy (lb,rb)
>  	append  e1 e   = e1 + e
>  	addss    e (i,j) = e + ss_energy (i,j)
>  	addssml    e (i,j) = 40 + e + ss_energy (i,j)
>  	ul       e   = 40 + e
>  	ssadd (i,j)  e   = e + ss_energy (i,j)
>  	ss    (i,j)	     = ss_energy (i,j)
>  	nil _ = 0
>  	pk e a fro (i',j') mid (k,l) bac b = pkinit + e + 3*10 + fro + mid + bac
>  	pul e = e
>  	pss (i,j) = sspenalty (j-i)
>  	sum lb (c,k) rb = (c + sr_energy (lb,rb),k+1)
>  	sumend lb _ rb = (0,1)
>  	h   es = [minimum es]
>  }


> #algebra[pp]{

>      pp :: FS_Algebra Int String 

>      pp =  (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, addssml, ssadd, ss, nil, pk, pul, pss, sum, sumend, h) where
>  	sadd  lb  e = "." ++ e
>  	cadd   x  e =  x  ++ e
>  	mlcons x  e =  x  ++ e
>  	dlr  _    x    _   =              x
>  	sr  lb    x    rb  = "("  ++      x ++ ")"	
>  	hl  lb    (i,j)    rb  = "(" ++ dots (i,j) ++")"	
>  	bl  bl (i,j)   e  br  = "(" ++ dots (i,j) ++ e ++")"
>  	br  bl  e (i,j)    br  = "(" ++ e ++ dots (i,j) ++")"
>  	il  lb (i,j) x (k,l) rb  = "(" ++ dots (i,j) ++ x ++ dots (k,l) ++ ")"
>  	ml  bl    x    br  = "(" ++ x ++ ")" 
>  	append  c1 c = c1 ++ c
>  	ul  c1 = c1
>  	addss  c1 (i,j)   = c1 ++ dots (i,j) 
>  	addssml  c1 (i,j)   = c1 ++ dots (i,j) 
>  	ssadd     (i,j) x =       dots (i,j) ++ x
>  	ss        (i,j)	  =       dots (i,j) 	
>  	nil _ = ""
>  	pk _ (a1,a2) u (b1,b2) v (a1',a2') w (b1',b2') = open1 (a1,a2) ++ "." ++ u ++ open2 (b1,b2) ++v ++ close1 (a1',a2') ++ w ++ ".." ++ close2 (b1',b2')
>  	pul c1 = c1
>  	pss (i,j) = dots (i,j)
>  	sum _ k _ = ""
>  	sumend _ _ _ = ""
>  	h es = [id es]
>  }


> #grammar {

>  tdm alg f = axiom rnastruct where
>     (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, addssml, ssadd, ss, nil, pk, pul, pss, sum, sumend, h) = alg

>     rnastruct = listed ( sadd <<< lbase -~~ rnastruct |||
>                                      addss <<< structstart ~~~ uregion	... h)
>     structstart = motif0


>     motif0 = (dlr <<< loc ~~~ stem0 ~~~ loc)	... h
>     stem0 = tabulated ( (sr <<< lbase -~~ stem0 ~~- lbase |||
>                                          sr <<< lbase -~~ motif1 ~~- lbase) `with` basepairing	... h)

>     motif1 = multiloop1	... h
>     multiloop1 = (ml <<< lbase -~~ ml_tail0 ~~- lbase) `with` basepairing	... h

>     ml_tail0 = tabulated ( ssadd <<< uregion ~~~ ml_nexttail1	... h)
>     ml_nexttail1 = tabulated (( mlcons <<< (dlr <<< loc ~~~ ml_motif_bp2 ~~~ loc) ~~~ ml_tail1)	... h )
>     ml_motif_bp2 = (sr <<< lbase -~~ ml_motif2 ~~- lbase) `with` basepairing	... h

>     ml_motif2 = stem2	... h
>     stem2 = tabulated ( ((sr <<< lbase -~~ ((sr <<< lbase -~~ ((sr <<< lbase -~~ ((sr <<< lbase -~~ maxstem2 ~~- lbase) `with` basepairing)  ~~- lbase) `with` basepairing)  ~~- lbase) `with` basepairing)  ~~- lbase) `with` basepairing) 	... h)
>     maxstem2 = (sr <<< lbase -~~ ( motif3 |||
>                                  (sr <<< lbase -~~ ( motif3 |||
>                                     (sr <<< lbase -~~ ( motif3 |||
>                                        (sr <<< lbase -~~ ( motif3 |||
>                                           (sr <<< lbase -~~ ( motif3 |||
>                                              (sr <<< lbase -~~ motif3 ~~- lbase) `with` basepairing)
>                                               ~~- lbase) `with` basepairing)
>                                            ~~- lbase) `with` basepairing)
>                                         ~~- lbase) `with` basepairing)
>                                      ~~- lbase) `with` basepairing)
>                                   ~~- lbase) `with` basepairing	 ... h

>     motif3 = iloop3	... h
>     iloop3 = tabulated ( (il <<< lbase -~~ region `with` maxsize 30 ~~~ motif_il4 ~~~ region `with` maxsize 30 ~~- lbase) `with` basepairing	... h)
>     motif_il4 = (sr <<< lbase -~~ motif4 ~~- lbase) `with` basepairing	... h

>     motif4 = hairpin4	... h
>     hairpin4 = (hl <<< lbase -~~ (region `with` minsize 3) ~~- lbase) `with` basepairing	... h

>     ml_tail1 = tabulated ( ssadd <<< uregion ~~~ ml_lasttail2	... h)
>     ml_lasttail2 = tabulated (( addssml <<< (dlr <<< loc ~~~ ml_motif_bp5 ~~~ loc) ~~~ uregion)	 ... h )
>     ml_motif_bp5 = (sr <<< lbase -~~ ml_motif5 ~~- lbase) `with` basepairing	... h

>     ml_motif5 = stem5 `with` size (20,60)	... h
>     stem5 = tabulated ( (sr <<< lbase -~~ stem5 ~~- lbase |||
>                                          sr <<< lbase -~~ motif6 ~~- lbase) `with` basepairing `with` maxsize 60	... h)

>     motif6 = iloop6	... h
>     iloop6 = tabulated ( (il <<< lbase -~~ (region `with` size (3,8)) ~~~ motif_il7 ~~~ (region `with` size (10,20)) ~~- lbase) `with` basepairing	... h)
>     motif_il7 = (sr <<< lbase -~~ motif7 ~~- lbase) `with` basepairing	... h

>     motif7 = hairpin7	... h
>     hairpin7 = (hl <<< lbase -~~ (region `with` size (3,58)) ~~- lbase) `with` basepairing	... h


>  }