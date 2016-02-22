#include <stdio.h>
#include <stdlib.h>

#include "graph.h"

int main() {
  struct graph *g, *h;

  g = graph_init(100, 100, 100,
      sizeof(struct graph), sizeof(struct vertex), sizeof(struct edge), NULL);
  graph_insert_edge(g, 1, 3);
  graph_insert_edge(g, 2, 3);
  graph_insert_edge(g, 4, 3);
  graph_insert_edge(g, 4, 1);
  graph_insert_edge(g, 4, 2);
  graph_insert_edge(g, 1, 4);
  graph_print_edges(g);
  graph_print_vertices(g);
  printf("remove 4\n");
  graph_remove_vertex(g, 4);
  graph_print_edges(g);
  graph_print_vertices(g);

  printf("graph copy\n");
  h = graph_copy(g);
  graph_delete(g);
  graph_print_edges(h);
  graph_print_vertices(h);
  graph_delete(h);
  return 0;
}
