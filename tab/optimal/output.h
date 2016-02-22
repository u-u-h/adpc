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

/*! \file output.h

    \brief Functions to write output-information to streams. They are used for
    \brief both screen-output and textfile-output.

*/


#ifndef _OUTPUT_H_
#define _OUTPUT_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <tabulate.h>
#include <asm_typedefs.h>
#include <asm_runtime.h>
#include <input.h>
#include <functions.h>
#include <poly_runtime.h>
#include <index_list.h>


/*! \brief Print a list of nonterminals.
    \param file Target-stream
    \param nonterms Nonterminals (necessary to print the space 
                    of the nonterminals)
    \param nts The list of the nonterminals that are to be printed
*/

void print_nt_list (FILE* file, Asm_Nts nonterms, Nt_List nts);


/*! \brief Print a table configuration.
    \param file Target-stream
    \param nonterms Nonterminals
    \param n Total number of nonterminals
*/

void print_table_configuration (FILE* file, Asm_Nts nonterms, int n);


/*! \brief Print the internal represantation of the nonterminals and the
    \brief parser dependencies (for debugging).
    \param file Target-stream
    \param nonterms Nonterminals
    \param n Total number of nonterminals
*/

void print_asm_dep_graph(FILE* file, Asm_Nts nonterms, int n);


/*! \brief Print a Binary- or Graycode. This is not used in the standard-
    \brief implementation, but useful if you want to test the code-
    \brief generation.
    \param file Target-stream
    \param code Bit-word that represents the code
    \param n Number of bits in the code
*/

void print_Code (FILE* file, char* code, int n);


/*! \brief Print a short hint if the user annotaion increases the runtime
    \brief asymptotically.
    \param file Target-stream
    \param runtime Runtime-record
*/

void comment_user_annotation (FILE* file, Runtime_Info runtime);


/*! \brief Print a summary of the results of the program's run to a stream.
    \param file Target-stream
    \param nonterms Nonterminals
    \param number Nonterminal-number-record
    \param runtime Runtime-record
    \param list List-record (contains the nonterminals of the diverse
                categories)
    \param Tables The list of all asymptotically optimal table configurations
    \param graph Graph-represantation of the nonterminals (necessary to
                 compute the polynomial runtime)
    \param opt_nonterms Optimization-nonterminals - see the description in 
                        "taboptimal.c" and "set_opt_nonterms"
    \param best The optimal asymptotical runtime
    \param options A pointer to the options-record that represents the
                   commandline-options. NULL is used if no options are
                   available (if called by "taboptimal_in.c")
    \param OptCard The cardinality of the optimal table configurations
    \param OptCount The number of the asymptotically optimal table
                    configurations (number of elements in "Tables")
*/

void write_resfile (FILE* file, Asm_Nts nonterms, Number_Info number,
                    Runtime_Info runtime, Nonterms_Info list,
                    TableDesign_List Tables, struct graph* graph,
                    int* opt_nonterms, int best, Opt_Rec* options,
                    int OptCard, unsigned int OptCount);


/*! \brief Print the asymptotically optimal table configurations to a stream.
    \param file Target-stream
    \param nonterms Bit-word that represents the code
    \param number Nonterminal-number-record
    \param Tables The list of all asymptotically optimal table configurations
    \param graph The nonterminals in the graph-representation
    \param opt_nonterms Optimization-nonterminals - see the description in 
                        "taboptimal.c" and "set_opt_nonterms"
    \param best The optimal asymptotical runtime
*/

void print_asmopt_tableconf (FILE* file, Asm_Nts nonterms, Number_Info number,
                             TableDesign_List Tables, struct graph* graph,
                             int* opt_nonterms, int best);


/*! \brief Print the polynomial optimal table configurations to a stream.
    \param file Target-stream
    \param nonterms Bit-word that represents the code
    \param number Nonterminal-number-record
    \param Tables The list of all asymptotically optimal table configurations
    \param graph Graph-represantation of the nonterminals (necessary to
                 compute the polynomial runtime)
    \param opt_nonterms Optimization-nonterminals - see the description in 
                        "taboptimal.c" and "set_opt_nonterms"
    \param best_runtime The best polynomial runtime (with respect to the
                        asymptotically optimal table configurations in
                        "Tables")
*/
                    
void print_polyopt_tableconf (FILE* file, Asm_Nts nonterms, Number_Info number,
                              TableDesign_List Tables, struct graph* graph,
                              int* opt_nonterms, Poly best_runtime);


/*! \brief Print the runtime and the usage of each nonterminal for a 
    \brief given table configuration.
    \param file Target-stream
    \param nonterms Nonterminals. The tab-fields contain the table
                    configuation.
    \param n Number of nonterminals
    \param graph The graph-representaion of the nonterminals
    \param run_state Running-State-Array. This is a technical parameter. See
                     the description in "asm_typedefs.h" and its usage in
                     "asm_runtime". It has to be initialized as in
                     "taboptimal.c". This initialization is restored when this
                     function finishes.
    \param tab_runtime Tabulation-Array. This is a technical parameter. It has
                       to be initialized with zeros. See "taboptimal.c". This
                       initialization is restored when the function finishes.
*/

void print_details (FILE* file, Asm_Nts nonterms, int n, struct graph* graph,
                    Run_Type run_state, char* tab_runtime);
          

/*! \brief Print the length of a time-interval to a stream. This function is
    \brief used to display the used runtime so far, the estimated remaining
    \brief runtime and the actual used runtime of the brute-force-
    \brief optimization. 
    \param file Target-stream
    \param secTime Length of the time-interval in seconds
*/

void print_time (FILE* file, double secTime);


#endif
