
Algebra type:

> #algebratype{

> type Canonical_Algebra alph1 alph2 closed inloop multiloopcomp answer =
>  (alph1 -> answer -> answer,  --sadd
>   closed -> answer -> answer, --cadd
>   alph1 -> answer,               --nil
>   alph1 -> closed -> alph1 -> closed,  --sr
>   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> closed,  --hl
>   alph1 -> alph1 -> closed -> alph1 -> alph1 -> closed, --sp
>   alph2 -> closed -> closed ,                           --bl         
>   closed -> alph2 -> closed,                            --br
>   alph1 -> alph1 -> inloop -> alph1 -> alph1 -> closed,  --iln
>   alph1 -> closed -> alph1 -> inloop,                    --ils
>   alph1 -> closed -> alph1 -> alph2 -> inloop,           --ilr
>   inloop -> alph1 -> inloop,                             --ill
>   alph2 -> alph1 -> closed -> inloop,                    --ill2
>   alph1 -> inloop -> alph1 -> inloop,                    --ilx
>   alph1 -> alph1 -> multiloopcomp -> alph1 -> alph1 -> closed, --ml
>   closed -> alph2 -> closed,  --addss
>   alph2 -> closed -> closed,  --ssadd
>   closed -> closed -> closed, --cons
>   closed -> closed,                   --ul
>   closed -> closed -> multiloopcomp,  --combine
>   [closed] -> [closed], --h
>   [inloop] -> [inloop], --h_i
>   [closed] -> [closed], --h_l
>   [answer] -> [answer]  --h_s
>   )

> }

Pretty printing algebra:


> #algebra[prettyprint]{

> prettyprint :: Canonical_Algebra Char (Int,Int) String String String String
> prettyprint = 
>         (sadd,cadd,nil,sr,hl,sp,bl,br,iln,ils,ilr,ill,ill2,ilx,
>          ml,addss,ssadd,cons,ul,combine,h,h_i,h_l,h_s) where
>   sadd _  s  = "."++s
>   cadd s1 s2 = s1++s2
>   nil  _     = ""
>   sr   _  s _ = "("++s++")"
>   hl   _  _ (h1,h2) _ _   = "(("++dots(h1, h2)++"))"
>   sp   _  _ s _ _ = "(("++s++"))"
>   bl   (l1,l2) s  = dots(l1, l2)++s
>   br   s (r1,r2)  = s++dots(r1, r2)
>   iln  _  _ s  _  _    = "(("++s++"))"
>   ils  _  s _          = "."++s++"."
>   ilr  _  s _  (r1,r2) = "."++s++"."++dots(r1, r2)
>   ill  s _             = s++"."
>   ill2  (l1,l2) _  s   = dots(l1, l2)++"."++s
>   ilx  _  s _          = "."++s++"."
>   ml    _ _ s _ _     = "(("++s++"))"
>   addss s (r1,r2) = s++dots(r1, r2)
>   ssadd (l1,l2) s = dots(l1, l2)++s
>   cons  s1  s2    = s1++s2
>   ul  s     = s
>   combine s1 s2 = s1 ++ s2
>   h   xs = [id xs]
>   h_i xs = [id xs]
>   h_l xs = [id xs]
>   h_s xs = [id xs]

> }

Counting algebra:

> #algebra[count]{

> count :: Canonical_Algebra Char (Int,Int) Integer Integer Integer Integer
> count = (sadd,cadd,nil,sr,hl,sp,bl,br,iln,ils,ilr,ill,ill2,ilx,
>             ml,addss,ssadd,cons,ul,combine,h,h_i,h_l,h_s) where 
>    sadd _ b = b
>    cadd a b = a*b
>    nil _ = 1
>    sr _ b _ = b
>    hl _ _ _ _ _ = 1
>    sp _ _ c _ _ = c
>    bl _ d = d
>    br c _ = c
>    iln _ _ c _ _ = c
>    ils _ b _ = b
>    ilr _ b _ _ = b
>    ill a _ = a
>    ill2 _ _ c = c
>    ilx _ b _ = b
>    ml _ _ c _ _ = c
>    addss a _ = a
>    ssadd _ b = b
>    cons a b = a * b
>    ul a  = a
>    combine a b = a*b
>    h   xs = [sum xs]
>    h_i xs = [sum xs]
>    h_l xs = [sum xs]
>    h_s xs = [sum xs]

> }

Minimal free energy algebra:

> #extern termaupenalty :: (Int, Int) -> Int
> #extern sr_energy :: (Int, Int) -> Int
> #extern hl_energy :: (Int, Int) -> Int
> #extern bl_energy :: (Int, Int, Int, Int) -> Int
> #extern br_energy :: (Int, Int, Int, Int) -> Int
> #extern il_energy :: (Int, Int, Int, Int) -> Int
> #extern il11_energy :: Int -> Int -> Int
> #extern il21_energy :: Int -> Int -> Int
> #extern il12_energy :: Int -> Int -> Int
> #extern il22_energy :: Int -> Int -> Int
> #extern dli_energy  :: (Int, Int) -> Int
> #extern dri_energy  :: (Int, Int) -> Int
> #extern dl_energy  :: (Int, Int) -> Int
> #extern dr_energy  :: (Int, Int) -> Int
> #extern ss_energy :: (Int, Int) -> Int

	> #algebra{
	
	> mfe :: Canonical_Algebra Char (Int,Int) (Float,Int,Int) (Float,Int) Float Float
	> mfe  = (sadd,cadd,nil,sr array,
	>              hl array,sp array,bl array,br array,
	>              iln array,ils array,ilr array,ill array,ill2 array,ilx,
	>              ml array,addss,ssadd,cons,
	>              ul,combine,h,h_i,h_l,h_s)                where
	>    sadd lb e = e
	>    cadd (e1,_,_) e = e1 + e
	>    nil _ = 0.0
	>    sr inp lb (e,_,_) rb = (e + sr_energy inp (lb,rb),lb,rb)
	>    hl inp llb lb loop rb rrb = (hl_energy inp (lb,rb) + sr_energy inp (llb,rrb),llb,rrb)
	>    sp inp llb lb (e,_,_) rb rrb = (e + sr_energy inp (llb,rrb), llb,rrb)
	>    bl inp (l,r) (e,lend,rend) = (e + bl_energy inp l (l,r) (rend+1) ,l,rend)
	>    br inp (e,lend,rend) (l,r) = (e + br_energy inp (lend-1) (l,r) (r+1) ,lend,r)
	>    iln inp llb lb (e,_) rb rrb = (e + (top_stack inp lb rb) + (sr_energy inp (llb,rrb)),
	>                                        llb,rrb)
	>    ils inp lb (e,_,_) rb = (e + (asym 0) + (bot_stack inp lb rb) + (il_ent 2), 2)
	>    ilr inp lb (e,_,_) rb (i,j) = (e + (asym (j-i)) + (bot_stack inp lb rb) 
	>                                   + (il_ent (j-i+2)), j-i+2)
	>    ill inp (e,l) rb = (e,l)
	>    ill2 inp (i,j) lb (e,lend,rend) = (e + (asym (j-i)) + (bot_stack inp lb (rend+1)) 
	>                                    + (il_ent (j-i+2)), j-i+2)
	>    ilx lb (e,k) rb = (e + (il_ent (k+2)) - (il_ent k), k+2)
	>    ml inp llb lb e rb rrb = (4.7 + e + sr_energy inp (llb,rrb),llb,rrb)
	>    addss (e,lb,_) (i,j) = (e + ss_energy (i,j),lb,j)
	>    ssadd (i,j) (e,_,rb) = (0.1 + e + ss_energy (i,j),i,rb)
	>    cons (e1,lb,_) (e2,_,rb) = (e1 + e2,lb,rb)
	>    ul (e,lb,rb) = (0.1 + e,lb,rb)
	>    combine (e1,_,_) (e2,_,_) = e1 + e2
	>    h = take k . sortBy compare . nub
	>    h_l  = take k . sortBy compare . nub
	>    h_s  = take k . sortBy compare . nub
	>    h_i  = take k . sortBy compare . nub
	
	> }

Algebra product operation:

The yield grammar:

> #extern minloopsize  :: Int -> Filter

> #grammar{

> canonicals alg f = axiom struct where
>  
>     (sadd,cadd,nil,sr,hl,sp,bl,br,iln,
>      ils,ilr,ill,ill2,ilx,ml,addss,ssadd,
>      cons,ul,combine,h,h_i,h_l,h_s) = alg

>     struct        = sadd <<< base   ~~~ struct |||
>                     cadd <<< closed ~~~ struct |||
>                     nil  <<< empty               ... h_s

>     closed        = tabulated (
>                     stack ||| hairpin ||| leftB ||| rightB ||| 
>                     iloop ||| multiloop ... h)

>     multiloop     = (ml <<< base ~~~ base ~~~ ml_components ~~~ base ~~~ base )
>                                                        `with` stackpairing ... h

>     ml_components = combine <<< block ~~~ comps

>     comps         = tabulated (
>                     cons  <<< block ~~~ comps  |||
>                               block            |||
>                     addss <<< block ~~~ region ... h_l)

>     block         = tabulated (
>                     ul    <<< closed            |||
>                     ssadd <<< region ~~~ closed ... h)

>     stack         = (sr <<< base ~~~ closed  ~~~ base) `with` basepairing

>     hairpin       = (hl <<< base ~~~ base ~~~ (region `with` minloopsize 3)
>                          ~~~ base ~~~ base)
>                     `with` stackpairing

>     leftB         = (sp <<< base ~~~ base ~~~ (bl <<< region  ~~~ closed)
>                          ~~~ base ~~~ base)
>                     `with` stackpairing ... h

>     rightB        = (sp <<< base ~~~ base ~~~ (br <<< closed ~~~ region)
>                         ~~~ base ~~~ base)
>                     `with` stackpairing ... h 

>     iloop         = (iln <<< base ~~~ base ~~~ inloop
>                          ~~~ base ~~~ base)
>                     `with` stackpairing ... h

>     inloop        = tabulated (
>                     ilx <<< base ~~~ inloop ~~~ base ||| loopend ... h_i)

>     loopend       = tabulated (
>                     ill <<< (ill2 <<< region ~~~ base ~~~ closed) ~~~ base |||
>                     ilr <<<            base ~~~ closed ~~~ base ~~~ region |||
>                     ils <<<            base ~~~ closed ~~~ base        ... h_i)

> }
