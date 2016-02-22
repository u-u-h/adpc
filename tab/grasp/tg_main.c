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

#include <adpc_deserialize.h>
#include <poly_runtime.h>
#include <graph.h>
#include <random.h>


#include <sys/time.h>
#include <time_util.h>

#include "asm_factory.h"
#include "tab_grasp.h"

void help(char **argv)
{
  fprintf(stderr,
      "%s [options ...] [filename, -, nothing]\n"
      "\nOptions:\n"
      "\t-a x\talpha value (default -1, i.e. random alpha every itr)\n"
      "\t-d\tdisable search and application of reductions\n"
      "\t-i x\t# of iterations, or -1 if automatic itr adaptation (default)\n"
      "\t-g x\tgreedy function index (default 3)\n"
      "\t-r x\trandom seed (default 1)\n"
      "\t-t x\tuser supplied asm rt as upper bound\n"
      "\t-z\tcompute random seed\n"
      "\t-h\tthis message\n",
      argv[0]);
}

void dump_command_line(int argc, char **argv)
{
  int i;
  printf("# ");
  for (i=0; i<argc; i++)
    printf("%s ", argv[i]);
  printf("\n");
}

int main(int argc, char **argv)
{
  char c;
  char *filename = NULL;
  double alpha = -1;
  int it = -1;
  int fn_index = 3;
  Nts nts;
  struct dep_graph *g;
  int r_old = 0, r_new = 0;
  unsigned int seed = 1;
  int best_asm_rt;
  int user_rt = 0;
  int appl_reduc = 1;

  struct timeval start_tv, stop_tv, result_tv;

  gettimeofday(&start_tv, NULL);

  vertex_function fn_array[6] = { greedy_degree_sum,
                                  greedy_degree,
                                  greedy_in_out_mul,
                                  greedy_tab_mul,
                                  greedy_tab_plus,
                                  greedy_tab_max};

  while ((c = getopt(argc, argv, "a:i:g:r:t:zhd")) != -1)
    switch (c) {
      case 'a' : alpha = strtod(optarg, NULL); break;
      case 'i' : it = atoi(optarg); break;
      case 'g' : fn_index = atoi(optarg); break;
      case 'r' : r_old = 1;
                 seed = strtoul(optarg, NULL, 10);
                 break;
      case 't' : user_rt = atoi(optarg); break;
      case 'z' : r_new = 1;
                 break;
      case 'd' : appl_reduc = 0;
                 break;
      case 'h' : 
      case '?' :
      default  : help(argv);
                 return 0;
                 break;
    }
  if (optind < argc)
    if (strcmp(argv[optind], "-"))
      filename = argv[optind];

  dump_command_line(argc, argv);
  
  if (r_old)
    srand(seed);
  if (r_new)
    seed = random_init();
  printf("Using seed: %u \n", seed);
  
  nts = adpc_deserialize(filename);
  g = (struct dep_graph*) graph_init(50, 50, 10,
      sizeof(struct dep_graph),
      sizeof(struct nt_vertex),
      sizeof(struct dep_edge),
      NULL);

  g->greedy_fn = fn_array[fn_index];

  nts_to_graph(nts, &(g->graph) );

  if (user_rt)
    best_asm_rt = user_rt;
  else
    best_asm_rt = get_best_asm_rt(nts);
  printf("Best asymptotic runtime: %d\n", best_asm_rt);

  if (appl_reduc)
    graph_apply_reductions(g, nts, best_asm_rt);

  nts_delete(nts);

  tab_grasp(g, alpha, it, best_asm_rt);

  gettimeofday(&stop_tv, NULL);
  tval_minus(&result_tv, &start_tv, &stop_tv);
  tval_print(&result_tv);
  printf("\n");

  dep_graph_delete(g);

  return 0;
}
