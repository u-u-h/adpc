/*
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
*/

/*! \file taboptimal_in.h

    \brief The main program of the built-in-variant of the software for the
    \brief brute-force-calculation of the optimal table configuration.

*/

#ifndef _TAB_OPTIMAL_IN_H_
#define _TAB_OPTIMAL_IN_H_


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>


/* We define the following macros before we include the other files,
   because these macros are used in some of these files.
*/

/*! \brief The name of the result-file */
#define TABRESULT_FILENAME "tdopt.txt"

/*! \brief Define the number of seconds between two runtime-estimations.

    The brute-force-optimization considers it approximatively only.
*/
#define ESTIMATE_TIME 30

/*! \brief Define after how many iterations the elapsed time shall be checked
    \brief to test if it is time to print a new runtime-estimation.
    
    This is the initial value of the variable "NumTime". It is modified by the
    brute-force-optimization, depending on the speed of execution, to avoid
    too much or too less tests.
*/
#define INIT_NUMTIME 10000


#include <tabulate.h>
#include <asm_typedefs.h>
#include <asm_runtime.h>
#include <poly_runtime.h>
#include <functions.h>
#include <input.h>
#include <output.h>


/*! \brief The startfunction for the brute-force-calculation. It is called by
    \brief "tabulate" and corresponds to the main-function in the stand-alone-
    \brief variant.
    \param inputdata The Nonterminals in the List-representation - see
                     "tabulate.c"
    \param log_level Verbosity level (see "asm_typedefs.h")
*/

struct dep_graph *  table_design_optimal (Nts inputdata, int log_level);


#endif
