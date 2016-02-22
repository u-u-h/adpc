------------------------------------------------------------------------------
-- The ADP Compiler 
-- Copyright (C) 2001-2008 Peter Steffen, Christian Lang, Marco Ruether, 
--                         Georg Sauthoff, Stefanie Schirmer
--
-- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
-- Updates: http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html
------------------------------------------------------------------------------

This file is part of ADPC (The ADP Compiler).

ADPC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

ADPC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ADPC.  If not, see <http://www.gnu.org/licenses/>.



> module Tests where

> import "HUnit/HUnit.lhs"
> import "Compile.lhs"
> import Phases
> import Dss
> import Expr
> import MathExp
> import Track
> import Tools
> import TLData
> import Syntax

> rev_Tests =  "$Revision$ "


Tests für dss
---------------

> drtest0 = "f" :<<< (Terminal ("region", []))
> drtest1 = "f" :<<< (Terminal ("region", []) :~~~ Terminal ("base", []))
> drtest2 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("base", []))
> drtest3 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("base", []))

> drtest4 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("base", []))
> drtest5 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("base", []) :~~~ Terminal ("base", []))

> drtest6  = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("region", [])  :~~~ Terminal ("base", []) :~~~ Terminal ("base", []))
> drtest6b = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("region3", []) :~~~ Terminal ("base", []) :~~~ Terminal ("base", []))

> drtest7 = "f" :<<< (Terminal ("region", []) :~~~ Terminal ("region", []))
> drtest7b = "f" :<<< (Terminal ("region", []) :~~~ Terminal ("region3", []))
> drtest7c = "f" :<<< (Terminal ("region3", []) :~~~ Terminal ("region3", []))
> drtest7d = "f" :<<< (Terminal ("region3b", []) :~~~ Terminal ("region", []))
> drtest7e = "f" :<<< (Terminal ("region3", []) :~~~ Terminal ("region", []))

> drtest8 = "f" :<<< (Terminal ("region", []) :~~~ Terminal ("base", []) :~~~ Terminal ("region", []))
> drtest9 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("base", []) :~~~ 
>                     Terminal ("region", []) :~~~ Terminal ("base", [])  :~~~ Terminal ("base", [])) 
> drtest10 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("base", []) :~~~  Terminal ("base", []) :~~~
>                      Terminal ("region", []) :~~~ Terminal ("base", [])  :~~~ Terminal ("base", [])) 
> drtest11 = "f" :<<< (Terminal ("base", []) :~~~ Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("region", []) :~~~  Terminal ("base", []) :~~~
>                      Terminal ("region", []) :~~~ Terminal ("base", [])  :~~~ Terminal ("base", [])) 

---------------
Alternativen:

> drtest_alt1 = drtest10 :||| drtest11 :||| drtest9 :||| drtest8

> drtest_alt2 = "f" :<<< ((Terminal ("base", [])) :~~~ ((Terminal ("base", []))   :|||
>                                                 (Terminal ("region", [])))  :~~~ (Terminal ("base", [])))

> drtest_alt3 = "f" :<<< ((Terminal ("base", [])) :~~~ ((Terminal ("base", []))   :|||
>                                            (Terminal ("region", [])) :|||
>                                            ("f2" :<<< (Terminal ("region", []) :~~~ Terminal ("region", []))))  :~~~ (Terminal ("base", [])))

> drtest_alt4 = "f" :<<< ((Terminal ("region", [])) :~~~ ((Terminal ("base", []))   :|||
>                                              (Terminal ("region", [])))  :~~~ (Terminal ("region", [])))

> drtest_alt5 = "f" :<<< ((Terminal ("base", [])) :~~~ ((Terminal ("base", []))   :|||
>                                            (Terminal ("region", [])) :|||
>                                            (Terminal ("uregion", [])) :|||
>                                            ("f2" :<<< (Terminal ("region", []) :~~~ Terminal ("region", []))))  :~~~ (Terminal ("base", [])))

> drtest_alt6 = drtest0 :||| drtest1 :||| drtest2 :|||  drtest5 :||| drtest3 


-------------
with:

> drtest_with1 = "f" :<<< ((Terminal ("region", [])) `With` ("equal", []))
> drtest_with2 = ("f" :<<< (Terminal ("base", []) :~~~ Terminal ("region", []) :~~~ Terminal ("base", []))) `With` ("equal",[])
> drtest_with2b = ("f" :<<< (Terminal ("base", []) :~~~ (Terminal ("region", []) `With` ("equal",[])) :~~~ Terminal ("base", [])))
> drtest_with2c = ("f" :<<< (Terminal ("base", []) :~~~ (Terminal ("region", []) `With` ("pairing",["basepairing"])) :~~~ Terminal ("base", [])))

-------------
minsize:

> drtest_with3 = "f" :<<< ((Terminal ("region", [])) `With` ("minsize",["3"]))
> drtest_with4 = "f" :<<< (((Terminal ("region", [])) `With`("minsize",["3"])) :~~~ Terminal ("region", []))
> drtest_with5 = ("f" :<<< (Terminal ("region", []) :~~~ Terminal ("region", []))) `With` ("minsize",["3"])


--------------
maxsize:

> drtest_with6 = "f" :<<< ((Terminal ("region", [])) `With` ("maxsize",["3"]))
> drtest_with7 = "f" :<<< (((Terminal ("region", [])) `With` ("maxsize",["3"])) :~~~ Terminal ("region", []))
> drtest_with8 = ("f" :<<< (Terminal ("region", []) :~~~ Terminal ("region", []))) `With` ("maxsize",["3"])

-------------

size:

> drtest_with9 = "f" :<<< ((Terminal ("region", [])) `With` ("size",["3","6"]))
> drtest_with10 = "f" :<<< (((Terminal ("region", [])) `With` ("size",["3","6"])) :~~~ Terminal ("region", []))
> drtest_with11 = ("f" :<<< (Terminal ("region", []) :~~~ Terminal ("region", []))) `With` ("size",["3","6"])


----------------
Auswahl:

> drtest_choice1 = (("f" :<<< (Terminal ("region", []) :~~~ Terminal ("region", []))) `With` ("minsize",["3"])) :... "h"



> dsstests = TestLabel "derive subscripts" (TestList 
>    [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11])

> test0 = TestCase (assertEqual "drtest0" 
>                               ((("f",[ST (StructOf TLRegion)]),(ST (Var "i",Var "j"),ST (Number 1,Infinite)),[]) 
>                                  :/<<</ (ILTerminal ("region",[]) (ST (Var "i",Var "j"))))
>                               (dss_test drtest0))

> test1 = TestCase (assertEqual "drtest1"
>                  ((("f",[ST (StructOf TLRegion),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 2,Infinite)),[]) :/<<</ (ILTerminal ("region",[]) (ST (Var "i",(Var "j") :- Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest1))

> test2 = TestCase (assertEqual "drtest2"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 3,Infinite)),[]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("region",[]) (ST ((Var "i") :+ Number 1,(Var "j") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest2))

> test3 = TestCase (assertEqual "drtest3"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 3,Number 3)),[]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 1,(Var "i") :+ Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest3))

> test4 = TestCase (assertEqual "drtest4"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 5,Number 5)),[]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 1,(Var "i") :+ Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 2,(Var "i") :+ Number 3))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 3,(Var "i") :+ Number 4))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest4))

> test5 = TestCase (assertEqual "drtest5"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 5,Infinite)),[]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 1,(Var "i") :+ Number 2))) :/~~~/ (ILTerminal ("region",[]) (ST ((Var "i") :+ Number 2,(Var "j") :- Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 2,(Var "j") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest5))

> test6 = TestCase (assertEqual "drtest6"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 5,Infinite)),[("k",(Var "i") :+ Number 2,(Var "j") :- Number 3)]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("region",[]) (ST ((Var "i") :+ Number 1,Var "k"))) :/~~~/ (ILTerminal ("region",[]) (ST (Var "k",(Var "j") :- Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 2,(Var "j") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest6))

> test7 = TestCase (assertEqual "drtest7"
>                  ((("f",[ST (StructOf TLRegion),ST (StructOf TLRegion)]),(ST (Var "i",Var "j"),ST (Number 2,Infinite)),[("k",(Var "i") :+ Number 1,(Var "j") :- Number 1)]) :/<<</ (ILTerminal ("region",[]) (ST (Var "i",Var "k")) :/~~~/ (ILTerminal ("region",[]) (ST (Var "k",Var "j")))))
>                  (dss_test drtest7 ))

> test8 = TestCase (assertEqual "drtest8"
>                  ((("f",[ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLRegion)]),(ST (Var "i",Var "j"),ST (Number 3,Infinite)),[("k",(Var "i") :+ Number 2,(Var "j") :- Number 1)]) :/<<</ (ILTerminal ("region",[]) (ST (Var "i",(Var "k") :- Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "k") :- Number 1,Var "k"))) :/~~~/ (ILTerminal ("region",[]) (ST (Var "k",Var "j")))))
>                  (dss_test drtest8))

> test9 = TestCase (assertEqual "drtest9"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 7,Infinite)),[("k",(Var "i") :+ Number 4,(Var "j") :- Number 3)]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 1,(Var "i") :+ Number 2))) :/~~~/ (ILTerminal ("region",[]) (ST ((Var "i") :+ Number 2,(Var "k") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "k") :- Number 1,Var "k"))) :/~~~/ (ILTerminal ("region",[]) (ST (Var "k",(Var "j") :- Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 2,(Var "j") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest9))

> test10 = TestCase (assertEqual "drtest10"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 8,Infinite)),[("k",(Var "i") :+ Number 5,(Var "j") :- Number 3)]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 1,(Var "i") :+ Number 2))) :/~~~/ (ILTerminal ("region",[]) (ST ((Var "i") :+ Number 2,(Var "k") :- Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "k") :- Number 2,(Var "k") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "k") :- Number 1,Var "k"))) :/~~~/ (ILTerminal ("region",[]) (ST (Var "k",(Var "j") :- Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 2,(Var "j") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest10))

> test11 = TestCase (assertEqual "drtest11"
>                  ((("f",[ST (StructOf TLChar),ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLRegion),ST (StructOf TLChar),ST (StructOf TLChar)]),(ST (Var "i",Var "j"),ST (Number 8,Infinite)),[("k2",(Var "i") :+ Number 5,(Var "j") :- Number 3),("k",(Var "i") :+ Number 3,(Var "k2") :- Number 2)]) :/<<</ (ILTerminal ("base",[]) (ST (Var "i",(Var "i") :+ Number 1)) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "i") :+ Number 1,(Var "i") :+ Number 2))) :/~~~/ (ILTerminal ("region",[]) (ST ((Var "i") :+ Number 2,Var "k"))) :/~~~/ (ILTerminal ("region",[]) (ST (Var "k",(Var "k2") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "k2") :- Number 1,Var "k2"))) :/~~~/ (ILTerminal ("region",[]) (ST (Var "k2",(Var "j") :- Number 2))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 2,(Var "j") :- Number 1))) :/~~~/ (ILTerminal ("base",[]) (ST ((Var "j") :- Number 1,Var "j")))))
>                  (dss_test drtest11))

