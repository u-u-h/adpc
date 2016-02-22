algebratype{

type Bill_Algebra alphabet answer = (
  alphabet -> answer, 
  answer -> alphabet -> answer,
  answer -> alphabet -> answer -> answer,
  answer -> alphabet -> answer -> answer,
  [answer] -> [answer]                   
  ) 

}


algebra[prettyprint]{

prettyprint :: Bill_Algebra Char String ;
prettyprint = (val, ext, add, mult, h) where
  val   c     = c ;
  ext   n c   = n ++ c ;
  add   x c y = "(" ++ x ++ c ++ y ++ ")" ;
  mult  x c y = "(" ++ x ++ c ++ y ++ ")" ;
  h           = id ;

}


algebra[count]{

count :: Bill_Algebra Char Int ;
count = (val, ext, add, mult, h) where
  val c       = 1 ;
  ext n c     = 1 ;
  add  x t y  = x * y ;
  mult x t y  = x * y ;
  h l         = [sum l] ;

}

algebra[buyer]{

buyer :: Bill_Algebra Char Int ;
buyer = (val, ext, add, mult, h) where
  val c         = decode c ;
  ext n c       = 10*n + decode c ;
  add  x t y    = x + y ;
  mult x t y    = x * y ;
  h l           = [minimum l] ;

}

algebra[seller]{

seller :: Bill_Algebra Char Int  ;
seller = (val, ext, add, mult, h) where
  val c       = decode c  ;
  ext n c     = 10*n + decode c  ;
  add  x c y  = x + y  ;
  mult x c y  = x * y  ;
  h l         = [maximum l] ;

}

algebra[time]{

time :: Bill_Algebra Char Int  ;
time = (val, ext, add, mult, h) where
  val c       = 0  ;
  ext n c     = 0  ;
  add  x c y  = max x y + 2  ;
  mult x c y  = max x y + 5  ;
  h l         = [minimum l]  ;

}


#Grammar elmamun formula (Int,Int) (val, ext, add, mult, h);

grammar[formular]{

formula = tabulated (
          number |||
          add  <<< formula  ~~-  plus   ~~~  formula  |||
          mult <<< formula  ~~-  times  ~~~  formula  ... h) ;

number = val <<< digit ||| ext <<< number ~~- digit ... h ;
digit  = (char '0') ||| (char '1') ||| (char '2') ||| (char '3') |||
         (char '4') ||| (char '5') ||| (char '6') ||| (char '7') |||
         (char '8') ||| (char '9') ;

plus  = (char '+') ; 
times = (char '*') ;

}
