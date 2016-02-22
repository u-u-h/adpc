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

/*! \file asm_runtime.h

    \brief Functions to deal with asymptotical runtimes and asymptotical
    \brief usages.

*/


#ifndef _ASM_RUNTIME_H_
#define _ASM_RUNTIME_H_


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include <tabulate.h>
#include <asm_typedefs.h>
#include <output.h>


/*! \brief Convert a polynomial runtime to its corresponding asymptotical
    \brief runtime.
    \param pol The polynomial runtime
    \return The corresponding asymptotical runtime
*/

int poly2asm (ADPCPoly pol);


/*! \brief Convert a asymptotical runtime to a string (pretty print).
    
    The memory for the string is allocated by this function.
    
    \param r The asymptotical runtime
    \return The corresponding (pretty printed) string
*/

char* asm2string (int r);

  
/*! \brief Multiply two asymptotical runtimes.
    \param r1 The first asymptotical runtime
    \param r2 The second asymptotical runtime
    \return The result r = r1 * r2
*/

inline int asm_times (int r1, int r2);


/*! \brief Add two asymptotical runtimes.
    \param r1 The first asymptotical runtime
    \param r2 The second asymptotical runtime
    \return The result r = r1 + r2
*/

inline int asm_plus (int r1, int r2);


/*! \brief Determine the best possible asymptotical runtime - with repect to
    \brief the user annotation.
    
    This is a heuristic function. In every case of practical relevance we
    encountered so far it gives the right result. This even holds for many
    classes of "special" grammars, which are of almost no practical use. But
    there might be some very special cases where it fails and returns a too
    large value. But it never returns a too small runtime. This is a
    deterministic function. Its result depends on the grammar only.
    
    The parameters run_state and tab_runtime are used to avoid the allocation
    of memory each time this function runs. You can use these two global
    arrays instead. They are changed temporaryly only, so they have the
    initial values again when this function finishes.
    
    \param nonterms Nonterminals. The tab-fields contain the user annotation;
                    the usage-fields contain the correct asymptotical usages.
    \param n Number of Nonterminals
    \param run_state Running-State-Array. This is a technical parameter. See
                     the description in "asm_typedefs.h" and its usage in
                     "asm_runtime". It has to be initialized as in
                     "taboptimal.c". The initialization is restored when the
                     function finishes.
    \param tab_runtime Tabulation-Array. This is a technical parameter. It has
                       to be initialized with zeros. See "taboptimal.c". This
                       initialization is restored when the function finishes.
    \return The best possible runtime
*/

int asm_best_runtime (Asm_Nts nonterms, int n, Run_Type run_state,
                      char* tab_runtime);


/*! \brief Calculate the runtime for a given table configuration.
    
    If the runtime of the given table configuration exceeds the best possible/
    optimal runtime then the return-value will be larger than the optimal
    runtime, but not necessarily the correct runtime of this table
    configuration. The calculation is aborted if it is determined that the
    runtime of this table configuration is worse than the optimal runtime,
    because in the very most cases you want just this fact and not the
    exact (worse) runtime. You can avoid this behavior by setting the
    parameter "best" to EXPONENTIAL.
    
    The parameters run_state and tab_runtime are used to avoid the allocation
    of memory each time this function runs. You can use these two global
    arrays instead. They are changed temporary only, so they have the initial
    values again when this function finishes.
    
    \param nonterms Nonterminals. The tab-fields contain the table
                    configuration where the values 0 or 1 are valid only.
    \param start The nonterminal whose runtime shall be calculated (usually
                 the axiom).
    \param n Number of Nonterminals
    \param best The best possible asymptotical runtime. The calculation is
                aborted if the runtime exceeds the best possible runtime.
    \param run_state Running-State-Array. This is a technical parameter. See
                     the description in "asm_typedefs.h" and its usage in
                     "asm_runtime". It has to be initialized as in
                     "taboptimal.c". The initialization is restored when the
                     function finishes.
    \param tab_runtime Tabulation-Array. It has to be initialized with zeros.
                       The entries of all tabulated nonterminals that are used
                       by the nonterminal "q" are to be set to 1. See 
                       "taboptimal.c".
    \return The runtime of the given table configuration
*/

int calc_asm_runtime (Asm_Nts nonterms, int start, int n, int best,
                      Run_Type run_state, char* tab_runtime);


/*! \brief Calculate the runtime for a nonterminal with respect to a given
    \brief table configuration, but do not consider the expenditure on
    \brief tabulating nonterminals.
    
    If the runtime of the given table configuration exceeds the best possible/
    optimal runtime then the return-value will be larger than the optimal
    runtime, but not necessarily the correct runtime of this table
    configuration. The calculation is aborted if it is determined that the
    runtime of this table configuration is worse than the optimal runtime,
    because in the very most cases you want just this fact and not the
    exact (worse) runtime. You can avoid this behavior by setting the
    parameter "best" to EXPONENTIAL.
    
    The parameters run_state and tab_runtime are used to avoid the allocation
    of memory each time this function runs. You can use these two global
    arrays instead. They are changed temporary only, so they have the initial
    values again when this function finishes.
    
    \param nonterms Nonterminals. The tab-fields contain the table
                    configuration where the values 0 or 1 are valid only.
    \param start The nonterminal whose runtime shall be calculated (usually
                 the axiom).
    \param n Number of Nonterminals
    \param best The best possible asymptotical runtime. The calculation is
                aborted if the runtime exceeds the best possible runtime.
    \param run_state Running-State-Array. This is a technical parameter. See
                     the description in "asm_typedefs.h" and its usage in
                     "asm_runtime". It has to be initialized as in
                     "taboptimal.c". The initialization is restored when the
                     function finishes.
    \param tab_runtime Tabulation-Array. This is a technical parameter. It has
                       to be initialized with zeros. See "taboptimal.c". The
                       initialization is restored when the function finishes.
    \return The runtime of the given table configuration
*/

int calc_asm_runtime_notab (Asm_Nts nonterms, int start, int n, int best,
                            Run_Type run_state, char* tab_runtime);
                      

/*! \brief Get the asymptotical usages of each nonterminal.
    
    The asymptotical usage of a nonterminal is the asymptotical number of
    calls to that nonterminal according to the given table configuration.

    \param nonterms Nonterminals. The tab-fields contain the table
                    configuration. The usage-fields are to be set.
    \param n Number of nonterminals
    \param start The nonterminal to start the consideration (usually the
                 axiom).
*/

void asm_get_usages (Asm_Nts nonterms, int n, int start);


/*! \brief Apply the preprocessing reductions to reduce the number of
    \brief optimization nonterminals.
     
    Which preprocessing reductions are applied depends on the options: It is
    influenced by the options "-l", "-e" and "-g".
    
    The parameters run_state and tab_runtime are used to avoid the allocation
    of memory each time this function runs. You can use this two global arrays
    instead. They are changed temporaryly only, so they have the initial
    values again when this function finishes.
    
    \param nonterms Nonterminals. The usage-fields have to be already set.
    \param n Number of nonterminals
    \param run_state Running-State-Array. This is a technical parameter. See
                     the description in "asm_typedefs.h" and its usage in
                     "asm_runtime". It has to be initialized as in
                     "taboptimal.c". The initialization is restored when the
                     function finishes.
    \param tab_state Tabulation-Array. This is a technical parameter. It has
                     to be initialized with zeros. See "taboptimal.c". This
                     initialization is restored when the function finishes.
    \param opt_nontermstate The optimization-state of the nonterminals. If
                            and only if the array-entry corresponding to a
                            nonterminal q equals zero than q is an
                            optimization-nonterminal.
    \param n_red The number of reduced nonterminals (nonterminals whose
                 tabulation state has been defined by the preprocessing
                 reductions) (RESULT)
    \param list The list of the reduced nonterminals (RESULT)
    \param options Option-record. The options "-l", "-e" and "-g" are
                   evaluated to decide which reductions can be applied. Use
                   NULL if no options are available (built-in-variant).
*/

void asm_apply_reductions (Asm_Nts nonterms, int n, Run_Type run_state,
                           char* tab_state, int* opt_nontermstate, int* n_red,
                           Nt_List* list, Opt_Rec* options);


#endif
