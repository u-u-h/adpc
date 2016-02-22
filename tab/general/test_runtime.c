#include <stdio.h>
#include <stdlib.h>

#include "adpc_deserialize.h"
#include "poly_runtime.h"

void mark_tabulated(struct graph *g, int argc, char **argv)
{
  int i, j;
  struct nt_vertex *v;
  if (argc < 3)
    return;
  for (i=2; i<argc; i++) {
    j = atoi(argv[i]);
    v = (struct nt_vertex*) graph_vertex(g, j);
    v->tabulated = 1;
  }
}

int main(int argc, char **argv)
{
  Nts nts;
  struct dep_graph *g;
  Poly result;
  
  nts = adpc_deserialize(argv[1]);
  //TODO: get graph size from adpc_deserialize
  g = (struct dep_graph*) graph_init(50, 50, 10,
      sizeof(struct dep_graph),
      sizeof(struct nt_vertex),
      sizeof(struct dep_edge),
      NULL);
  nts_to_graph(nts, &(g->graph) );

  mark_tabulated(&(g->graph), argc, argv);

  print_dep_graph(&(g->graph));
  result = compute_runtime(&(g->graph));
  printf("\nComputed runtime: ");
  poly_print(stdout, result);
  printf("\n");
  return 0;
}
