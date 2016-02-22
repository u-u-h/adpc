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

/*! \file functions.h

    \brief Auxiliary functions that are used by both "taboptimal.c" and
    \brief "taboptimal_in.c".

*/


#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <tabulate.h>
#include <asm_typedefs.h>
#include <asm_runtime.h>
#include <input.h>
#include <output.h>
#include <poly_runtime.h>


/*! \brief Update a table configuration by adapting the tabulation state of
    \brief the optimization-nonterminals.
    \param nonterms Nonterminals. The tab-fields are to be modified according
                    to the settings in "opt_nonterms" and "td". 
    \param n_opt Number of optimization-nonterminals
    \param td Table configuration of the optimization-nonterminals - see
              datastructure "TableDesin_List"
    \param opt_nonterms The optimization-nonterminals. Each array-entry
                        contains the ID of one optimization-nonterminal. It is
                        an "array-embedded list" of the optimization-
                        nonterminals. See also the description in 
                        "taboptimal.c".
*/

void update_table_configuration (Asm_Nts nonterms, int n_opt, char* td,
                                 int* opt_nonterms);


/*! \brief Convert a table configuration that is given as "Asm_Nts" into the
    \brief "struct graph*"-format.
    
    Precondition: Both structures - "nonterms" and "graph" - have to represent
                  the same parser-dependencies! This function sets the table
                  configuration _only_! It does _not_ convert the whole
                  representation!
    
    \param nonterms Nonterminals in "Asm_Nts"-representation
    \param n Number of nonterminals
    \param graph Nonterminals in "graph"-representation. The tabulate-fields
                 are to be modified according to the tab-fields in "nonterms".
*/
                 
void convert_table_configuration (Asm_Nts nonterms, int n,
                                  struct graph* graph);


/*! \brief Get the best polynomial runtime of all asymptotically optimal
    \brief table configurations.
    \param nonterms Nonterminals in "Asm_Nts"-representation
    \param number Nonterminal-number-record
    \param Tables The list of all asymptotically optimal table configurations
    \param graph Nonterminals in "graph"-representation
    \param opt_nonterms Optimization-nonterminals - see the description in 
                        "taboptimal.c" and "set_opt_nonterms"
    \param best The best polynomial runtime of the table configurations in
                "Tables" (RESULT)
    \param best_count The number of table configurations in "Tables" that
                      achieve the best polynomial runtime (RESULT)
*/

void get_best_polynomial_runtime (Asm_Nts nonterms, Number_Info number,
                                  TableDesign_List Tables,
                                  struct graph* graph, int* opt_nonterms,
                                  Poly* best, unsigned int* best_count);


/*! \brief Create a list of all unused nonterminals.
    \param nonterms Nonterminals
    \param n Number of Nonterminals
    \param opt_nontermstate The optimization-state of the nonterminals. If a
                            nonterminal q is not in use then 
                            "opt_nontermstate[q]" is set to 1. See the
                            description in "taboptimal.c" and
                            "set_opt_nonterms".
    \param n_unused Number of unused nonterminals (RESULT)
    \param list List of all unused nonterminals (RESULT)
*/

void list_unused_nonterms (Asm_Nts nonterms, int n, int* opt_nontermstate,
                           int* n_unused, Nt_List* list);


/*! \brief Create a list of all nonterminals that require less than O(n^2)
    \brief space in case of tabulation.
    \param nonterms Nonterminals
    \param n Number of Nonterminals
    \param n_lin Number of nonterminals that require less than O(n^2) space
                 (RESULT)
    \param list List of all nonterminals that require less than O(n^2) space
                (RESULT)
*/

void list_lin_nonterms (Asm_Nts nonterms, int n, int* n_lin, Nt_List* list);
                       

/*! \brief Set up the user annotation.
    \param nonterms Nonterminals
    \param n Number of nonterminals
    \param opt_nontermstate The optimization-state of the nonterminals. If a
                            nonterminal is annotated by the user (or if it is
                            not in use) then its corresponding array-entry is
                            set to 1. See the description in "taboptimal.c"
                            and "set_opt_nonterms".
    \param n_user The number of nonterminals that are annotated by the user
                  (RESULT)
    \param list List of all nonterminals that are annotated by the user
                (RESULT)
*/

void set_user_configuration (Asm_Nts nonterms, int n, int* opt_nontermstate,
                             int* n_user, Nt_List* list);


/*! \brief Create the "array-embedded list" of all nonterminals that are used
    \brief for the brute-force-optimization.
    \param opt_nontermstate The optimization-state of the nonterminals. If
                            and only if "opt_nontermstate[q]" equals zero then
                            q is an optimization-nonterminal.
    \param n Number of nonterminals
    \param opt_nonterms The (so far empty) "array-embedded list" of the
                        optimization-nonterminals. It is to be filled by this
                        function.
    \param opt_n Number of optimization-nonterminals (RESULT)
    \param list List of all optimization-nonterminals (RESULT)
*/
   
void set_opt_nonterms (int* opt_nontermstate, int n, int* opt_nonterms,
                       int* opt_n, Nt_List* list);



/* The following macros are used in the mainloop of the brute-force-
   optimization, where its runtime is the critical point. We use macros
   because they do not produce any overhead that could slow down the
   execution and because the source code remains clearly arranged. We tried
   inline-functions instead - with the result that they are slower. */


/*! \brief Binary-Code-Counter. This macro adds 1 to a Bit-word. It is used
    \brief to enumerate all 2^m possible table configurations of the m
    \brief optimization-nonterminals.
    \param BinaryCount The bit-word that represents the Binary-code
    \param n Number of bits
    \param chPos position of the last change (this is where a 0 changes to a
                 1). It equals to the position of the change when enumerating
                 in Graycode. (RESULT)
*/

#define CODE_COUNT(BinaryCount, n, chPos)                                    \
                                                                             \
  { chPos = 0;                                                               \
    int code_count_cont = 1;                                                 \
    while (code_count_cont && (chPos < n))                                   \
      if (BinaryCount[chPos] == 0)                                           \
        { BinaryCount[chPos] = 1;                                            \
          code_count_cont = 0;                                               \
        }                                                                    \
      else                                                                   \
        BinaryCount[chPos++] = 0;                                            \
  }


/*! \brief Create a new list of asymptotically optimal table configurations
    \brief and delete the old list. This macro is used if a better table
    \brief configuration is found (in the brute-force-optimization).
    \param Tables The list of asymptotically optimal table configurations
    \param CurTable Pointer to the last added entry in "Tables"
    \param GrayCount A Bit-word that represents the new (better) table
                     configuration of the optimization-nonterminals. They
                     are enumerated in the order of the Graycode which equals
                     this table configuration.
    \param GrayCount_len The number of bits in "GrayCount", which equals the
                         number of the optimization-nonterminals
    \param OptCount The number of asymptotically optimal table configurations,
                    which is to be set to 1.
    \param OptCard The cardinality of the asymptotically optimal table
                   configurations, which is to be set to "Card".
    \param Card The cardinality of the new (better) table configuration.
*/

#define NEW_BEST_TABLECONF(Tables, CurTable, GrayCount, GrayCount_len,       \
                           OptCount, OptCard, Card)                          \
                                                                             \
  { while (Tables != NULL)                                                   \
      { free(Tables->td);                                                    \
        CurTable = Tables;                                                   \
        Tables = Tables->next;                                               \
        free(CurTable);                                                      \
      }                                                                      \
    Tables = malloc(sizeof(struct tabledesign_list));                        \
    Tables->td = malloc(GrayCount_len);                                      \
    memcpy(Tables->td, GrayCount, GrayCount_len);                            \
    Tables->next = NULL;                                                     \
    CurTable = Tables;                                                       \
    OptCount = 1;                                                            \
    OptCard = Card;                                                          \
  }


/*! \brief Append a new table configuration to the list of asymptotically
    \brief optimal table configurations.
    \param CurTable Pointer to the last added entry in the list of table
                    configurations
    \param GrayCount A Bit-word that represents the new table configuration of
                     the optimization-nonterminals. They are enumerated in the
                     order of the Graycode which equals this table
                     configuration.
    \param GrayCount_len The number of bits in "GrayCount", which equals the
                         number of the optimization-nonterminals
    \param OptCount The number of asymptotically optimal table configurations.
                    It is to be increased by 1.
*/
  
#define APPEND_TABLECONF(CurTable, GrayCount, GrayCount_len, OptCount)       \
                                                                             \
  { CurTable->next = malloc(sizeof(struct tabledesign_list));                \
    CurTable = CurTable->next;                                               \
    CurTable->td = malloc(GrayCount_len);                                    \
    memcpy(CurTable->td, GrayCount, GrayCount_len);                          \
    CurTable->next = NULL;                                                   \
    OptCount++;                                                              \
  }


/*! \brief Print a new table configuration to a stream.
    \param file Target-stream
    \param UpdateCount The number of solution updates so far. This number is
                       to be displayed and increased by 1.
    \param CountIterations The number of executed iterations from the start of
                           the brute-force-optimization to the last time-check
                           (see "taboptimal.c").
    \param Counter1 The number of executed iterations since the last time-
                    check (see "taboptimal.c").
    \param OptCard Cardinality of the new table configuration
    \param nonterms Nonterminals
    \param n Number of nonterminals
    \param new_poly The polynomial runtime of the new table configuration.
                    NULL is used to suppress it.
*/

#define PRINT_UPDATE_INFO(file, UpdateCount, CountIterations, Counter1,      \
                          OptCard, nonterms, n, new_poly)                    \
                                                                             \
  { fprintf(file, "\nSolution update no. %u :\n", UpdateCount++);            \
    fprintf(file, "Found new intermediate solution ");                       \
    fprintf(file, "in iteration %.0lf : %d Tables\n  ",                      \
                    (double) (CountIterations+Counter1), OptCard);           \
    print_table_configuration(file,nonterms,n);                              \
    if (new_poly)                                                            \
      { fprintf(file, "\nPolynomial runtime : ");                            \
        poly_print(file, new_poly);                                          \
      }                                                                      \
    fprintf(file, "\n");                                                     \
  }


#endif
