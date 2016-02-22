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

rnastruct = listed ( cadd <<< motif0 ~~~ tail0	... h ); 


motif0 = ss <<< region	... h;


tail0 = listed ( cadd <<< motif1 ~~~ tail3	... h ); 



motif1 = iloop1	... h;
iloop1 = tabulated ( (il <<< lbase -~~ region ~~~ motif_il2 ~~~ region ~~- lbase) `with` basepairing	... h);
motif_il2 = (sr <<< lbase -~~ motif2 ~~- lbase) `with` basepairing	... h;

motif2 = stem2	... h;
stem2 = tabulated ( (sr <<< lbase -~~ stem2 ~~- lbase |||
                                     sr <<< lbase -~~ motif3 ~~- lbase) `with` basepairing	... h);

motif3 = bulge3	... h;
bulge3 = tabulated ( (br <<< lbase -~~ motif_b4 ~~~ region ~~- lbase) `with` basepairing	... h);
motif_b4 = (sr <<< lbase -~~ motif4 ~~- lbase) `with` basepairing	... h;

motif4 = multiloop4	... h;
multiloop4 = (ml <<< lbase -~~ ml_tail1 ~~- lbase) `with` basepairing	... h;

ml_tail1 = tabulated ( ssadd <<< region ~~~ ml_nexttail2	... h);
ml_nexttail2 = tabulated ( mlcons <<< ml_motif_bp5 ~~~ ml_tail2	... h );
ml_motif_bp5 = (sr <<< lbase -~~ ml_motif5 ~~- lbase) `with` basepairing	... h;

ml_motif5 = closed5	... h;
closed5 = tabulated ( stack5 ||| iloop5 ||| bulgeR5 ||| bulgeL5 |||
                                       (sr <<< lbase -~~ motif6 ~~- lbase) `with` basepairing	... h );
stack5 = (sr <<< lbase -~~ closed5 ~~- lbase) `with` basepairing	... h;
iloop5 = (il <<< lbase -~~ region ~~~ closed5 ~~~ region ~~- lbase) `with` basepairing	... h;
bulgeR5 = (br <<< lbase -~~ closed5 ~~~ region ~~-lbase) `with` basepairing	... h;
bulgeL5 = (bl <<< lbase -~~ region ~~~ closed5 ~~-lbase) `with` basepairing	... h;

motif6 = hairpin6	... h;
hairpin6 = (hl <<< lbase -~~ (region `with` minsize 3) ~~- lbase) `with` basepairing	... h;

ml_tail2 = tabulated ( ssadd <<< region ~~~ ml_lasttail3	... h);
ml_lasttail3 = tabulated ( addss <<< ml_motif_bp7 ~~~ region	 ... h );
ml_motif_bp7 = (sr <<< lbase -~~ ml_motif7 ~~- lbase) `with` basepairing	... h;

ml_motif7 = stem7	... h;
stem7 = tabulated ( (sr <<< lbase -~~ stem7 ~~- lbase |||
                                     sr <<< lbase -~~ motif8 ~~- lbase) `with` basepairing	... h);

motif8 = hairpin8	... h;
hairpin8 = (hl <<< lbase -~~ (region `with` minsize 3) ~~- lbase) `with` basepairing	... h;


tail3 = ss <<< region	... h;





}

