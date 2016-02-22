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

#include <asm_runtime.h>
#include <input.h>

#include "asm_factory.h"
#include "../../tabulate.h"
#include "../general/log.h"

int get_best_asm_rt(Nts nts)
{
  Asm_Nts asm_nts;
  int count;
  Run_Type run_state;
  char* tab_runtime;
  int i;
  int r;

  convert_input(nts, &asm_nts, &count);

  for (i = 0; i < count; i++)
      asm_nts[i].tab = 2;  // tabulate no nonterminal

  run_state = malloc(count * sizeof(struct run_type));
  for (i = 0; i < count; i++) {
    run_state[i].runstate = -1;
    run_state[i].list = NULL;
  }

  tab_runtime = calloc(count, sizeof(char));

  r = asm_best_runtime(asm_nts, count, run_state, tab_runtime);

  free(run_state);
  free(tab_runtime);
  asm_nts_delete(asm_nts, count);
  
  // convert to poly compatible asm representation
  return r-1;
}
// best_asm_rt in grasp representation;
void graph_apply_reductions(struct dep_graph *g, Nts nts, int best_asm_rt)
{
  Asm_Nts asm_nts;
  int count;
  Run_Type run_state;
  char* tab_runtime;
  int i;
  int *opt_nt_states;
  int reduc_count;
  Nt_List reduc_list, itr, next;

  struct nt_vertex *v;

  best_asm_rt++;

  convert_input(nts, &asm_nts, &count);

  for (i=0; i<count; i++)
      asm_nts[i].tab = 2;  // tabulate no nonterminal

  run_state = malloc(count * sizeof(struct run_type));
  for (i = 0; i<count; i++) {
    run_state[i].runstate = -1;
    run_state[i].list = NULL;
  }

  tab_runtime = calloc(count, sizeof(char));
  opt_nt_states = calloc(count, sizeof(int));
  reduc_list = NULL;
  reduc_count = 0;

  asm_get_usages(asm_nts, count, 0);

  asm_apply_reductions(asm_nts, count, run_state, tab_runtime,
      opt_nt_states, &reduc_count, &reduc_list, NULL);

  log_printf(VERB_MOST, "reduced %d NTs.\n", reduc_count);
  for (i=0; i<count; i++) {
    v = (struct nt_vertex*) graph_vertex(&(g->graph), i+1);
    if (asm_nts[i].tab == 1) {
      log_printf(VERB_MOST, "Nt %d gets never tabulated\n", i+1);
      v->tabulated = -1;
    }
    else
      if (asm_nts[i].tab == 0) {
        log_printf(VERB_MOST, "Nt %d have to be tabulated\n", i+1);
        v->tabulated = 1;
      }
  }
  
  free(run_state);
  free(tab_runtime);
  free(opt_nt_states);

  itr = reduc_list;
  while (itr) {
    next = itr->next;
    free(itr);
    itr = next;
  }

  asm_nts_delete(asm_nts, count);
}

