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
>  		[comp] -> [comp]  --h
>  		) 
>  }


> #algebra[mfe]{

>   mfe :: FS_Algebra Int Int 
>   mfe = (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, addssml, ssadd, ss, nil, h) where
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

>  	h   es = [minimum es]
>  }


> #algebra[pp]{

>      pp :: FS_Algebra Int String 

>      pp =  (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, addssml, ssadd, ss, nil, h) where
>  	sadd  lb  e = "." ++ e
>  	cadd   x  e =  x  ++ e
>  	mlcons x  e =  x  ++ e
>  	dlr  _    x    _   =              x
>  	sr  lb    x    rb  = "("  ++      x ++ ")"	
>  	hl  lb    (i,j)         rb  = "(" ++ dots (i,j) ++")"	
>  	bl  bl    (i,j) e       br  = "(" ++ dots (i,j) ++ e ++")"
>  	br  bl  e (i,j)         br  = "(" ++ e ++ dots (i,j) ++")"
>  	il     lb (i,j) x (k,l) rb  = "(" ++ dots (i,j) ++ x ++ dots (k,l) ++ ")"
>  	ml  bl    x    br  = "(" ++ x ++ ")" 
>  	append   c1 c       = c1 ++ c
>  	ul       c1         = c1
>  	addss    c1 (i,j)   = c1 ++ dots (i,j) 
>  	addssml  c1 (i,j)   = c1 ++ dots (i,j) 
>  	ssadd       (i,j) x =       dots (i,j) ++ x
>  	ss          (i,j)   =       dots (i,j)
>  	nil _            = ""

>  	h es = [id es]
>  }


> #grammar {

>  tdm alg f = axiom rnastruct where
>     (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, addssml, ssadd, ss, nil, h) = alg

>     rnastruct = listed ( sadd <<< lbase -~~ rnastruct |||
>                          addss <<< structstart ~~~ uregion	... h)
>     structstart = tabulated (( cadd <<< motif0 ~~~ tail0) `with` maxsize 150	... h )


>     motif0 = (dlr <<< loc ~~~ stem0 ~~~ loc) `with` maxsize 50	... h
>     stem0 = tabulated ( (sr <<< lbase -~~ stem0 ~~- lbase |||
>                          sr <<< lbase -~~ motif5 ~~- lbase) `with` basepairing `with` maxsize 50	... h)

>     motif5 = hairpin5	... h
>     hairpin5 = (hl <<< lbase -~~ ((region `with` size (5,11))) ~~- lbase) `with` basepairing	... h

>     tail0 = tabulated (( cadd <<< motif6 ~~~ tail1) `with` maxsize 150	... h )

>     motif6 = ss <<< (region `with` (maxsize 150))	... h

>     tail1 = tabulated (( cadd <<< motif7 ~~~ tail2) `with` maxsize 150	... h )

>     motif7 = (dlr <<< loc ~~~ stem7 ~~~ loc) `with` maxsize 40	... h
>     stem7 = tabulated ( (sr <<< lbase -~~ stem7 ~~- lbase |||
>                          sr <<< lbase -~~ motif8 ~~- lbase) `with` basepairing `with` maxsize 40	... h)

>     motif8 = hairpin8	... h
>     hairpin8 = (hl <<< lbase -~~ (region `with` size (3,10)) ~~- lbase) `with` basepairing	... h

>     tail2 = tabulated (( cadd <<< motif9 ~~~ tail3) `with` maxsize 150	... h )

>     motif9 = ss <<< (region `with` (maxsize 150))	... h

>     tail3 = stem7 ... h

>  }