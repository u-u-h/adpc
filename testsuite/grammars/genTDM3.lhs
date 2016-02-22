
userdefs{
filter stackpair = (4,Infinite)
filter basepair = (2,Infinite)
}


target{
#include "ADPio.h"
#include "ADPenergy2.h"

int initDiff;
char *pp_goutp;

#define mean_scale (1.34855)
#define mk_pf(X) (exp ((X)/ (-61.6321)))
#define scale(size) scale_ar[size]

double *scale_ar;

void init_scale_ar()
{
  int i;
  scale_ar=(double *) calloc(n+2, sizeof(double));
  scale_ar[0] = 1.0;
  for (i = 1; i<= n; i++) scale_ar[i] = scale_ar[i-1] / mean_scale;
}

#}

soutput{
   l = back_struct(0, n);
   pp_str_Signature(l);
   printf("  (%.2f)\n", ((double) tbl_struct(0, n)) / 100);
#}

main{
int main(int argc, char **argv)                
{   
 float idiff; 
 int maxloop = 30;                                          
   init_ADPio(argv[1]);
   init_ADPenergy();
   sscanf(argv[2],"%f",&idiff); 
   initDiff = idiff;

   /* allocate output buffer string */                      
   pp_goutp = (char *) calloc(30*n, sizeof(char));          

   init_scale_ar();
   mainloop();                                 
   exit(0);                                    
} 
#}

smainadd{
   init_ADPio(input);
   printf("%s\n", input);       
   init_ADPenergy();
#}

algebratype{

> type FS_Algebra base cmpl = (   
>            base -> cmpl -> cmpl ,  -- sadd
>	     cmpl -> cmpl -> cmpl ,  -- cadd

>	     base -> cmpl -> base -> cmpl, --sr
>	     base -> base -> (Int,Int) -> base -> base         -> cmpl,   --hl
>	     base -> base -> (Int,Int) -> cmpl -> base -> base -> cmpl,   --bl
>	     base -> base -> cmpl -> (Int,Int) -> base -> base -> cmpl,   --br
>	     base -> base -> (Int,Int) -> cmpl -> (Int,Int) -> base -> base -> cmpl, --il

>	     base-> base->         cmpl        -> base-> base -> cmpl, --ml

>	     base -> cmpl -> base -> cmpl, --dlr
>	     cmpl -> cmpl -> cmpl,     --append
>	     cmpl ->  cmpl,	       -- ul
>	     cmpl -> (Int,Int) -> cmpl,   -- addss
>	     (Int,Int) -> cmpl -> cmpl,   -- ssadd

>	     (Int,Int) -> cmpl ,	       -- ss
>	     cmpl   -> cmpl,           -- mlpen
>	     Int -> cmpl,  --nil

>	     [cmpl] -> [cmpl],  --h
>	     [cmpl] -> [cmpl],  --h_l
>	     [cmpl] -> [cmpl]  --h_s
>            )  

}


algebra[energy]{

> energy :: FS_Algebra Int Int 
> energy  =  (sadd,cadd,sr,hl,bl,br, il, ml, dlr, append, ul, addss, ssadd, ss, mlpen, nil, h, h_l, h_s)

>     where
>       sadd  lb e = e
>	cadd  e1 e = e1 + e

>	sr     lb         e      rb      = e + sr_energy (lb,rb)
>	hl llb lb    _            rb rrb =     hl_energy (lb,rb)     + sr_energy (llb,rrb) 
>	bl llb lb   (i,j) e       rb rrb = e + bl_energy (lb,i,j,rb) + sr_energy (llb,rrb)
>	br llb lb e (i,j)         rb rrb = e + br_energy (lb,i,j,rb) + sr_energy (llb,rrb)
>	il llb lb   (i,j) e (k,l) rb rrb = e + sr_energy (llb,rrb) + il_energy (i,j,k,l)
>	ml llb lb         e       rb rrb = 380 +  e + sr_energy (llb,rrb) + termaupenalty (lb,rb) + dli_energy (lb,rb) + dri_energy (lb,rb)

>	dlr   dl e dr   = e + dl_energy (dl+1,dr) + dr_energy (dl+1,dr)+ termaupenalty ((dl+1),dr)

>	append  e1 e     = e1 + e
>	addss    e (i,j) =      e + ss_energy (i,j)
>	ul       e       = e

>	ssadd (i,j)  e   = 40 + e + ss_energy (i,j)
>	ss (i,j)         = 0
>	nil     _        = 0
>	mlpen e	         = e + 40	

>	h   es = [minimum es]
>	h_l es = [minimum es]
>	h_s es = [minimum es] 

}

algebra[pp]{

> pp :: FS_Algebra Int String
> pp  =  (sadd,cadd,sr,hl,bl,br, il, ml, dlr, append, ul, addss, ssadd, ss, mlpen, nil, h, h_l, h_s)
 
>     where
>	sadd  lb e = "." ++ e		
>	cadd  x  e = x ++ e

>	sr lb e rb = "(" ++ e ++ ")"	
>	hl llb lb    r    rb rrb = "((" ++ dots r ++"))"	
>	bl llb bl    x e  br rrb = "((" ++ dots x ++ e ++"))"
>	br llb bl e  x    br rrb = "((" ++ e ++ dots x ++"))"
>	il llb lb lr x rr rb rrb = "((" ++ dots lr  ++ x ++ dots rr ++ "))" 
>	ml llb bl    x    br rrb = "((" ++ x ++ "))" 

>	dlr   dl x dr =  x
>	append c1 c = c1 ++ c
>	ul  c1 = c1
>	addss  c1 r = c1 ++ dots r
>	ssadd  r x = dots r ++ x		

>	nil _ = ""
>	ss    r	   = dots r  	
>	mlpen x	   = x	 

>	h   es = [id es]
>	h_l es = [id es]
>	h_s es = [id es] 

}

algebra[pf]{

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

}


> module Main where


> import Array
> import System(getArgs)
> import Foldingspace
> import RNACombinators
> import Matchalgebras
> import RNAshape

> main ::  IO()
> main  = do
>	  [arg1,arg2] <- getArgs
>	  let input	 = arg1
>	      result	 = case  arg2 of
>			         "1"  ->  show (match input (energyalg *** prettyprintalg))
>				 "2"  ->  show (match input (basepairalg *** prettyprintalg *** countalg))
>				 "3"  ->  show (match input (basepairalg *** prettyprintalg))
>				 "4"  ->  show (match input (basepairalg *** countalg))
>				 "5"  ->  show (match input (countalg    *** basepairalg))
>				 "6"  ->  show (match input (countalg))
>				 otherwise -> error "select 1..6\n" in

>	      putStr (result ++"\n")


> match :: [Char] -> (RNAInput -> FS_Algebra Int a b) -> [b] 
> match sequence  algebra = axiom tail1 where

>   tabulated		= table  n
>   listed		= table1 n
>   n			= length sequence
>   axiom		= axiom' n
>   inp			= mk (rna sequence)
>   basepair (i,j)      = basepair'  (inp,(i,j))
>   stackpair (i,j)     = stackpair' (inp,(i,j))
>   minsize m (i,j) = minloopsize'  m (inp,(i,j))

>  
>   (sadd, cadd, sr, hl, bl, br, il, ml, dlr, append, ul, addss, ssadd, ss, mlpen, nil, h, h_l, h_s) = algebra inp

grammar[tail1]{

>   tail1 = sadd <<< base -~~ tail1 ||| 
>           addss <<< motif1 ~~! region ||| 
>           ul <<< motif1 ... h_l
>        where
>        infixl 7 ~~!
>        (~~!) = (*~*) 11 1

>        --endwhere

>   motif1 = s1

>   s1 = dlr <<< loc ~~! slstem1 ~~!! loc
>        where
>        infixl 7 ~~!
>        (~~!) = (~~*) (0,0) 11
>        infixl 7 ~~!!
>        (~~!!) = (*~~) 11 (0,0)

>        --endwhere

>   slstem1 = ((bl <<< lbase -~~ lbase ~~! region30 ~~!! motif2 ~~- lbase ~~- lbase ||| 
>             br <<< lbase -~~ lbase ~~!!! motif2 ~~!!!! region30 ~~- lbase ~~- lbase ||| 
>             il <<< lbase -~~ lbase ~~! region30 ~~!! motif2 ~~!!!!! region30 ~~- lbase ~~- lbase ||| 
>             sr <<< lbase -~~ motif2 ~~- lbase) `with` stackpair) ... h
>        where
>        (~~!) = (~~) (2,2) (1,30)
>        (~~!!) = (~~*) (3,32) 9
>        (~~!!!) = (~~*) (2,2) 9
>        (~~!!!!) = (*~~) 11 (1,30)
>        (~~!!!!!) = (*~~) 12 (1,30)

>        --endwhere

>   motif2 = dlr <<< loc ~~! slstem2 ~~!! loc
>        where
>        infixl 7 ~~!
>        (~~!) = (~~*) (0,0) 9
>        infixl 7 ~~!!
>        (~~!!) = (*~~) 9 (0,0)

>        --endwhere

>   slstem2 = ((bl <<< lbase -~~ lbase ~~! region30 ~~!! motif3 ~~- lbase ~~- lbase ||| 
>             br <<< lbase -~~ lbase ~~!!! motif3 ~~!!!! region30 ~~- lbase ~~- lbase ||| 
>             il <<< lbase -~~ lbase ~~! region30 ~~!! motif3 ~~!!!!! region30 ~~- lbase ~~- lbase ||| 
>             sr <<< lbase -~~ motif3 ~~- lbase) `with` stackpair) ... h
>        where
>        infixl 7 ~~!
>        (~~!) = (~~) (2,2) (1,30)
>        infixl 7 ~~!!
>        (~~!!) = (~~*) (3,32) 7
>        infixl 7 ~~!!!
>        (~~!!!) = (~~*) (2,2) 7
>        infixl 7 ~~!!!!
>        (~~!!!!) = (*~~) 9 (1,30)
>        infixl 7 ~~!!!!!
>        (~~!!!!!) = (*~~) 10 (1,30)

>        --endwhere

>   motif3 = hairpin

>   region30 = ((region) `with` (maxsize 30))

>   hairpin = dlr <<< loc ~~! hairpin_Pr ~~!! loc
>        where
>        infixl 7 ~~!
>        (~~!) = (~~*) (0,0) 7
>        infixl 7 ~~!!
>        (~~!!) = (*~~) 7 (0,0)
>        hairpin_Pr = tabulated(
>                     ((hl <<< lbase -~~ lbase ~~! ((region) `with` (minsize 3)) ~~- lbase ~~- lbase ||| 
>                     sr <<< lbase -~~ hairpin_Pr ~~- lbase) `with` stackpair) ... h)
>             where
>             infixl 7 ~~!
>             (~~!) = (~~*) (2,2) 3

>             --endwhere

>        --endwhere

}

