algebratype{

type FS_Algebra base comp = (
             base -> comp -> comp ,  -- sadd
	     comp -> comp -> comp ,  -- cadd
	     comp -> comp -> comp ,  -- mlcons
             base -> comp   -> base -> comp, --dlr
	     base -> comp   -> base -> comp, --sr
	     base -> (Int,Int) -> base -> comp, --hl
	     base -> (Int,Int) -> comp ->           base -> comp, --bl
	     base ->           comp -> (Int,Int) -> base -> comp, --br
	     base -> (Int,Int) -> comp -> (Int,Int) -> base -> comp, --il
	     base ->           comp           -> base -> comp, --ml
	     comp -> comp -> comp,     -- append
	     comp ->  comp,	       -- ul
	     comp -> (Int,Int) -> comp,   -- addss
	     (Int,Int) -> comp -> comp,   -- ssadd
	     (Int,Int) -> comp ,	       -- ss
	     base -> comp,  --nil
	     [comp] -> [comp]  --h
	   ) 
}
	      	     
algebra[mfe]{

 mfe :: FS_Algebra Int Int ;
 mfe = (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, ssadd, ss, nil, h) where
	sadd  lb e = e; 
	cadd  e1 e = e1 + e;
	mlcons e1 e = e1 + e + 40;
	dlr dl    e    dr  = e + dl_energy (dl+1,dr) + dr_energy (dl+1,dr) + termaupenalty (dl+1,dr);
	sr  lb        e    rb  = e + sr_energy  (lb,rb);
	hl  lb        _    rb  =     hl_energy  (lb,rb) ;
	bl  lb  (i,j) e    rb  = e + bl_energy  (lb,i,j, rb);
	br  lb        e (i,j)  rb  = e + br_energy (lb,i,j, rb);
	il  lb (i,j) e (k,l) rb= e + il_energy (i,j,k,l) ;
	ml  lb       e     rb  = 380 + e + termaupenalty (lb,rb) + dli_energy (lb,rb) + dri_energy (lb,rb);
	append  e1 e   = e1 + e;
	addss    e (i,j) = 40 + e + ss_energy (i,j);
	ul       e   = 40 + e;
	ssadd (i,j)  e   = e + ss_energy (i,j);
	ss    (i,j)	     = ss_energy (i,j);
	nil _ = 0;
	h   es = [minimum es];
}

algebra[pp]{

    pp :: FS_Algebra Int String ;
    pp =  (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, ssadd, ss, nil, h) where
	sadd  lb  e = "." ++ e;
	cadd   x  e =  x  ++ e;
 	mlcons x  e =  x  ++ e;
	dlr  _    x    _   =              x;
	sr  lb    x    rb  = "("  ++      x ++ ")";	
	hl  lb    (i,j)    rb  = "(" ++ dots (i,j) ++")";	
	bl  bl (i,j)   e  br  = "(" ++ dots (i,j) ++ e ++")";
	br  bl  e (i,j)    br  = "(" ++ e ++ dots (i,j) ++")";
	il  lb (i,j) x (k,l) rb  = "(" ++ dots (i,j) ++ x ++ dots (k,l) ++ ")";
	ml  bl    x    br  = "(" ++ x ++ ")" ;
	append  c1 c = c1 ++ c;
	ul  c1 = c1;
	addss  c1 (i,j)   = c1 ++ dots (i,j) ;
	ssadd     (i,j) x =       dots (i,j) ++ x;
	ss        (i,j)	  =       dots (i,j) ;	
	nil _ = "";
 	h es = [id es];
}

#Grammar ex4 rnastruct (Int,Int) (sadd, cadd, mlcons, dlr, sr, hl, bl, br, il,ml,append, ul, addss, ssadd, ss, nil, h);

grammar[rnastruct]{

rnastruct = listed ( sadd <<< lbase -~~ rnastruct |||
                                  addss <<< motif0 ~~~ region	... h);



motif0 = stem0	... h;
stem0 = tabulated ( ((sr <<< iupac_base 'g' -~~ ((sr <<< iupac_base 'c' -~~ maxstem0 ~~- iupac_base 'g') `with` basepairing) ~~- iupac_base 'c') `with` basepairing)	... h);
   maxstem0 = tabulated ( (sr <<< lbase -~~ maxstem0 ~~- lbase) `with` basepairing |||
                                              motif1	... h);

motif1 = bulge1	... h;
bulge1 = tabulated ( (bl <<< lbase -~~ (region `with` contains_region "c" ) ~~~ motif_b2 ~~- lbase) `with` basepairing	... h);
motif_b2 = (sr <<< lbase -~~ motif2 ~~- lbase) `with` basepairing	... h;

motif2 = stem2	... h;
stem2 = tabulated ( (sr <<< lbase -~~ stem2 ~~- lbase |||
                                     sr <<< lbase -~~ motif3 ~~- lbase) `with` basepairing	... h);

motif3 = hairpin3	... h;
hairpin3 = (hl <<< lbase -~~ ((region `with` size (6,6)) `with` contains_region "cga") ~~- lbase) `with` basepairing	... h;

}