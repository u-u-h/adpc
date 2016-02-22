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

/*! \file taboptimal.h

    \brief The main program of the stand-alone-variant of the software for the
    \brief brute-force-calculation of the optimal table configuration.

*/


#ifndef _TABOPTIMAL_H_
#define _TABOPTIMAL_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>


/* We define the following macros before we include the other files,
   because these macros are used in some of these files.
*/

/*! \brief The default name of the result-file */
#define TABRESULT_FILENAME "tdopt.txt"

/*! \brief The default name of the program's-execuation-state-file */
#define STATE_FILENAME "tdoptstate.dat"

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
#include <taboptimal_fkt.h>


/* The following variables are declared global because they are involved in
   signal-handling and have to be accessible for the signal-handler-function.
   Note that Microsoft Windows operating systems do not support signal-
   handling.
*/

/*! \brief The signal variable. It is set, if an important signal has arrived
    \brief which indicates that the program is to be stopped, continued or
    \brief terminated.
*/

#ifndef WINDOWS
volatile int ext_signal;
#endif


/*! \brief This variable defines after how many interations the brute-force-
    \brief optimization checks for signals (and the elapsed time, to print a
    \brief new runtime-estimation)

    "NumTime" is set to zero if a signal has arrived, so the signal will be
    handled in the next iteration of the brute-force-optimization - without
    any reasonible delay.
*/

#ifndef WINDOWS
volatile unsigned int NumTime;
#endif


#endif
