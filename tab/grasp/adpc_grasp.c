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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <poly_runtime.h>
#include <graph.h>


#include <sys/time.h>
#include <time_util.h>

#include "asm_factory.h"
#include "tab_grasp.h"

#include "adpc_grasp.h"
#include "../../tabulate.h"
#include "../general/log.h"

struct dep_graph * table_design_grasp(Nts nts, int vl)
{
  double alpha = -1;
  int it = -1;
  int fn_index = 3;
  struct dep_graph *g;
  int best_asm_rt;

  struct timeval start_tv, stop_tv, result_tv;

  if (log_level(VERB_MORE))
    gettimeofday(&start_tv, NULL);

  vertex_function fn_array[4] = { greedy_degree_sum,
                                  greedy_degree,
                                  greedy_in_out_mul,
                                  greedy_tab_mul };

  g = (struct dep_graph*) graph_init(50, 50, 10,
      sizeof(struct dep_graph),
      sizeof(struct nt_vertex),
      sizeof(struct dep_edge),
      NULL);

  g->greedy_fn = fn_array[fn_index];

  nts_to_graph(nts, &(g->graph) );

  best_asm_rt = get_best_asm_rt(nts);

  log_printf(VERB_MORE, "Best asymptotic runtime: %d\n", best_asm_rt);

  graph_apply_reductions(g, nts, best_asm_rt);

  nts_delete(nts);

  tab_grasp(g, alpha, it, best_asm_rt);

  if (log_level(VERB_MORE)) {
    gettimeofday(&stop_tv, NULL);
    tval_minus(&result_tv, &start_tv, &stop_tv);
    tval_print(&result_tv);
    printf("\n");
  }

  return g;
}
