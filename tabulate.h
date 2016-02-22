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

/*! \file tabulate.h
    
    \brief Implementation of the interface-functions Haskell <-> C.
    
*/


#ifndef _TABULATE_H_
#define _TABULATE_H_

#include <stdlib.h>
#include <stdio.h>


/*! \brief A polynomial.
   
    x_0 * n^0 + x_1 * n^1 + ... + x_l * n^l
    is represented as: [l, x_0, x_1, .. ,x_l]

   FIXME: IIRC adpc uses actually [l+1, x_0, .., x_l] see convert_poly

   in the tab-part we use uint64_ now ...
*/
typedef int *ADPCPoly;


/*!\brief The list of all nonterminals a nonterminal depends on. */

typedef struct deps {
  struct deps *next;
  int nt;           /*!< Nonterminal-ID */
  ADPCPoly usages;      /*!< Degree of dependence */
} *Deps;


/*!\brief The list of all nonterminals. */

typedef struct nts {
  struct nts *next;
  int nt;               /*!< Nonterminal-ID */
  int tab;              /*!< state of tabulation, given by the user:
                               0 -> tabulate nt
                               1 -> do not tabulate nt
                               2 -> determine best state automatically */
  ADPCPoly tabulationType;  /*!< Required space in case of tabulation 
                            (1, n or n^2) */
  ADPCPoly runtime;         /*!< Runtime according to the assumption that all
                             nonterminals in "deps" are tabulated. */
  Deps deps;            /*!< list of all dependencies to other nonterminals */
} *Nts;


// mode:  good: 1, optimal: 2, approx: 3, printarray: 4
enum action { MODE_GOOD = 1,
              MODE_OPTIMAL,
              MODE_APPROX,
              MODE_PRINT };
// returnmode: print results and exit: 1, return result array: 2
enum ret_action { RET_EXIT = 1,
                  RET_RESULT };
// verbosity:  none: 1, more: 2, most: 3
enum verbosity { VERB_NONE = 1,
               VERB_MORE,
               VERB_MOST };



/*! \brief A simple prettyprinter for polynomials.
    \param file Target-Stream
    \param p Polynomial to print
*/

void printPoly (FILE* file, ADPCPoly p);


/*! \brief A simple prettyprinter for dependence-lists.
    \param file Target-Stream
    \param deps Dependence-list to print
*/

void printDeps (FILE* file, Deps deps);


/*! \brief A simple prettyprinter for nonterminal-dependencies.
    \param file Target-Stream
    \param nts Nonterminals to print
*/

void printNts(FILE* file, Nts nts);


/*! \brief Deserialize the array given by the ADP-compiler (Haskell)
    \brief => Built the graph.
    \param arr The serialized array
    \param size The size of the array (RESULT)
*/

Nts deserializeGraph(int *arr, int *size);


/*! \brief Print the array (serialized graph) to a file.
    \param file Target-Stream
    \param arr The serialized array
    \param size The size of the array
*/

void serialize_array(FILE* file, int *arr, int size);


/*! \brief Read the array (serialized graph) from a file.
    \param file Source-Stream
    \param arr The serialized array
    \param size The size of the array
*/

void read_array(FILE* file, int** arr, int* size);


/*! \brief Free the memory used to store nts.
    \param nts Nonterminals to delete
*/

void nts_delete(Nts nts);


#endif
