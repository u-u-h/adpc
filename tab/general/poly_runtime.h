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

#ifndef _POLY_RUNTIME_H_
#define _POLY_RUNTIME_H_

/*! \file poly_runtime.h
    \brief Functions to compute the exact runtime of an ADP program.
 */

#include "../../tabulate.h"
#include "poly_type.h"
#include "set.h"
#include "dll.h"
#include "graph.h"
#include "bag.h"

// TODO: nur poly mit fester gr"o"se verwenden -> in place times m"oglich

/*! \brief Default size of a polynom used in grasp */
#define POLY_SIZE 20

/*! \brief An edge of a dependency graph */
struct dep_edge {
  struct edge edge; /*!< extends struct edge */
  Poly usage; /*!< how much the edge.to NT gets called */
};

/*! \brief A vertex of a dependency graph */
struct nt_vertex {
  struct vertex vertex; /*!< extends struct vertex */
  int tabulated; /*!< tabulation state, -1 never tabulate, 0 not tabulated,
                      1 tabulated */
  Poly runtime; /*!< runtime if every NT is tabulated */
  Poly space; /*!< space needed for tabulation of this NT, most n^2, linear
                  also possible, or even a constant factor */
  int annotate; /*!< user annotation, 0 tabulate it, 1 dont, 2 automatic */
  double greediness; /*!< the computed greediness of this vertex */

  int in_tab_count; /*!< count of tabulated parents */
  int out_tab_count; /*!< count of tabulated children */
};

/*! \brief A runstate representation as list element

    Part of an index list.
 */
struct run_state {
  struct list_head head; /*!< extends struct list_head */
  int nt;
  Poly prefactor;
};

/*! \brief The dependency graph.

    Main ADT.
 */
struct dep_graph {
  struct graph graph; /*!< extends struct graph */
  double max_greediness; /*!< the maximal greediness of the graph */
  vertex_function greedy_fn; /*!< the greedy function to compute the greediness
                                  if every vertex */
};

/*! \brief Convertes adpc input to the graph ADT.
    \param g an initialized dep_graph */
void nts_to_graph(Nts nts, struct graph *g);

/*! \brief Pretty print the dependency graph */
void print_dep_graph(struct graph *g);

/*! \brief Pretty print the tabulated NTs */
void print_tab_graph(struct graph *g);

/*! \brief Computes the runtime of a given ADP program and prints some
    \brief debug information 
    \param g ADP program as dependency graph
    \return computed runtime */
Poly compute_runtime(struct graph *g);

/*! \brief Computes the runtime of a given ADP program
    \param g ADP program as dependency graph
    \return computed runtime */
Poly compute_runtime_nodebug(struct graph *g);

/*! \brief Computes the runtime of a given ADP program faster
    \param g ADP program as dependency graph
    \param best_asm_rt known best possible asymptotic runtime
    \return computed runtime or poly_illegal() if runtime asymptotic
           worse than best_asm_rt */
Poly compute_runtime_lim(struct graph *g, int best_asm_rt);

/*! \brief Computes the sum of the degree of the usages */
int compute_degree_sum(struct edges *e);

/*! \brief Frees all memory allocated by the graph ADT
 */
void dep_graph_delete(struct dep_graph *g);

/*! \brief Internal runtime function.

  Used by christian.
  */
Poly runtime(int q, struct graph *g,
    struct bag *rec_bag,
    struct run_state **run_states);

int runtime_improve_constant_factors(struct dep_graph *g, int max, int factor,
    int input_length);

#endif
