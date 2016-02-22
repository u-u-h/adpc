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
#include <assert.h>

#include "graph.h"
#include "dll.h"

/* perhaps use some more efficient structure for in/out edges?
   e.g. perfect hashing, b-trees, ... */

struct graph* graph_init(
        int initial_vertices,
        int vertex_step_size,
        int edge_step_size,
        size_t graph_size,
        size_t vertex_size,
        size_t edge_size,
        vertex_function update)
{
  struct graph *g;
  g = calloc(1, graph_size);
  g->graph_size = graph_size;
  g->array = calloc(initial_vertices, sizeof(struct vertex*));
  g->bound = initial_vertices - 1;
  g->vertex_step_size = vertex_step_size;
  g->edge_step_size = edge_step_size;
  g->update = update;
  g->vertex_size = vertex_size;
  g->edge_size = edge_size;
  return g;
}

/*! \brief Enlarges the array of vertices.
  \param l index, which you want to access and is greater than g->bound */
void graph_enlarge(struct graph* g, int l)
{
  int t;
  struct vertex **r;
  t = g->bound;
  while (l > g->bound)
    g->bound += g->vertex_step_size;
  r = realloc(g->array, (g->bound + 1) * sizeof(struct vertex*));
  if (!r) {
    perror("realloc");
    abort();
  }
  g->array = r;
  memset(g->array + t + 1, 0,
      (g->bound - t) * sizeof(struct vertex*));
}

struct vertex *graph_vertex(struct graph *g, int k)
{
  if (k>g->bound)
    return NULL;
  else
    return *(g->array + k);
}

struct edge *graph_edge(struct graph *g, int k, int l)
{
  struct vertex *v;
  struct edge *e;
  struct edge_iterator itr;

  v = graph_vertex(g, k);

  graph_edge_iterator(&(v->out), &itr);
  while ((e = graph_edge_itr_next(&itr)))
    if (e->to == l)
      return e;
 
  assert(NULL);
  return NULL;
}

struct vertex *graph_add_vertex(struct graph *g, int k)
{
  struct vertex *v;
  if (k > g->bound)
    graph_enlarge(g, k);
  *(g->array + k) = calloc(1, g->vertex_size);
  v = *(g->array + k);
  if (k >= g->last_index)
    g->last_index = k + 1;
  (g->vertex_count)++;
  (v->in).array = calloc(g->edge_step_size, g->edge_size);
  (v->in).bound = g->edge_step_size - 1;
  (v->out).array = calloc(g->edge_step_size, g->edge_size);
  (v->out).bound = g->edge_step_size - 1;
  return v;
}

int edge_exists(struct graph* g, int i, int j)
{
  int z;
  struct vertex *v;
  struct edge *item;
  if ((i > g->last_index - 2) || (j > g->last_index - 2))
    return 0;
  v = *(g->array + i);
  if (!v)
    return 0;
  for (z=0; z < (v->out).last_index; z++) {
    item = *((v->out).array + z);
    if ((item) && (item->to == j))
      return 1;
  }
  return 0;
}

/*! \brief Enlarges the edge array. */
void item_enlarge(struct edges *e, int step_size)
{
  int t;

  t = e->bound;
  (e->bound) += step_size;
  e->array = realloc(e->array, (e->bound + 1) * sizeof(struct edge*));
  if (!(e->array)) {
    perror("realloc");
    abort();
  }
  memset(e->array + t + 1, 0, (e->bound - t) * sizeof(struct edge*));
}

/*! \brief Adds an edge to an edge array. */
void vertex_add_link(struct edges *e, int step_size, struct edge *item)
{
  if (e->last_index > e->bound)
    item_enlarge(e, step_size);
  *(e->array + e->last_index) = item;
  (e->last_index)++;
  (e->count)++;
}

struct edge *graph_insert_edge(struct graph *g, int i, int j)
{
  int max;
  struct vertex *v;
  struct edge *item;

  if (edge_exists(g, i, j))
    return NULL;
  max = i > j ? i : j;
  if (max > g->bound)
    graph_enlarge(g, max);
 
  if (!*(g->array + i))
    graph_add_vertex(g, i);
  if (!*(g->array + j))
    graph_add_vertex(g, j);

  item = malloc(g->edge_size);
  memset(item, 0, g->edge_size);
  item->from = i;
  item->to = j;

  v = *(g->array + i);
  vertex_add_link(&(v->out), g->edge_step_size, item);
  v = *(g->array + j);
  vertex_add_link(&(v->in), g->edge_step_size, item);
  (g->edge_count)++;
  if (g->update) {
    (g->update)(g, i, NULL);
    (g->update)(g, j, NULL);
  }
  return item;
}

/*! \brief Removes the edge from the supplied array.
  \return address of edge structure
  */
struct edge *edge_clean(struct edges *e, int i, int j) {
  struct edge *item = NULL;
  int z;

  for (z=0; z < e->last_index; z++) {
    item = *(e->array + z);
    if ((item) && (item->from == i) && (item->to == j)) {
      *(e->array + z) = NULL;
      if (z == e->last_index - 1)
        (e->last_index)--;
      (e->count)--;
      break;
    }
  }
  return item;
}

struct edge *graph_remove_edge(struct graph* g, int i, int j)
{
  struct vertex *v, *w;
  struct edge *item;
  v = *(g->array+i);
  w = *(g->array+j);

  if ((!v) || (!w)) {
    fprintf(stderr, "Edge doesn't exists!\n");
    abort();
  }
  
  item = edge_clean(&(v->out), i, j);
  edge_clean(&(w->in), i, j);
  (g->edge_count)--;
  return item;
}

/*! \brief Walker callback funtiocn for ::graph_complete_edges */
void vertex_complete_edges(struct graph *g, int i, void *extra)
{
  struct vertex *v, *w;
  struct edge *item;
  int z;
  v = *(g->array + i);
  for (z=0; z < (v->out).last_index; z++) {
    item = *((v->out).array + z);
    w = *(g->array + item->to);
    *((w->in).array + (w->in).last_index) = item;
    ((w->in).last_index)++;
  }
}

/*! \brief Adds edges to the parent nodes.
  \param g Graph without edges to parent nodes.

  Parent edges correspond to existing child edges. */
void graph_complete_edges(struct graph *g)
{
  graph_walk_vertices(g, vertex_complete_edges, NULL);
}

struct graph* graph_copy(struct graph* g)
{
  struct graph *h;
  struct vertex *v, *w;
  int j, z;
  struct edge *item;
  
  h = calloc(1, g->graph_size);
  *h = *g;
  h->array = calloc(h->bound + 1, sizeof(struct vertex*));

  for (z=0; z < h->bound + 1; z++) {
    v = *(g->array + z);
    if (!v)
      continue;
    w = *(h->array + z) = malloc(h->vertex_size);
    memcpy(w, v, h->vertex_size);
    
    (w->out).array = calloc((w->out).bound + 1, sizeof(struct edge*));
    for (j=0; j < (w->out).last_index; j++) {
      item = *((v->out).array + j);
      if (item) {
        *((w->out).array + j) = malloc(h->edge_size);
        memcpy(*((w->out).array + j), item, h->edge_size);
      }
    }

    (w->in).last_index = 0;
    (w->in).array = calloc((w->in).bound + 1, sizeof(struct edge*));
  }
  graph_complete_edges(h);

  return h;
}

struct vertex* graph_remove_vertex(struct graph* g, int k)
{
  struct vertex *v;
  struct edge *item;
  int i, j;

  v = *(g->array + k);
  j = (v->out).last_index;
  for (i=0; i < j; i++) {
    item = *((v->out).array + i);
    if (item)
      graph_remove_edge(g, item->from, item->to);
  }
  j = (v->in).last_index;
  for (i=0; i < j; i++) {
    item = *((v->in).array + i);
    if (item)
      graph_remove_edge(g, item->from, item->to);
  }
  
  *(g->array + k) = NULL;
  (g->vertex_count)--;
  if (g->last_index == k+1)
    (g->last_index)--;
  return v;
}

struct vertex* graph_remove_vertex_e(struct graph* g, int k,
    struct list_head **e_head)
{
  struct vertex *v;
  struct edge *item;
  struct list_head *head;
  struct edge_head *el;
  int i, j;
  list_init((*e_head = malloc(sizeof(struct list_head))));
  head = *e_head;

  v = *(g->array + k);
  j = (v->out).last_index;
  for (i=0; i < j; i++) {
    item = *((v->out).array + i);
    if (item) {
      el = malloc(sizeof(struct edge_head));
      el->edge = graph_remove_edge(g, item->from, item->to);
      list_add_tail(&(el->head), head);
    }
  }
  j = (v->in).last_index;
  for (i=0; i < j; i++) {
    item = *((v->in).array + i);
    if (item) {
      el = malloc(sizeof(struct edge_head));
      el->edge = graph_remove_edge(g, item->from, item->to);
      list_add_tail(&(el->head), head);
    }
  }
  
  *(g->array + k) = NULL;
  (g->vertex_count)--;
  if (g->last_index == k+1)
    (g->last_index)--;
  return v;
}

void graph_walk_vertices(struct graph* g, vertex_function fn, void* extra)
{
  int z;
  struct vertex *v;
  for (z=0; z < g->last_index; z++) {
    v = *(g->array + z);
    if (v)
      fn(g, z, extra);
  }
}

/*! Helper structure for ::edge_walker */
struct eset {
  edge_function fn;
  void *extra;
};

/*! \brief Callback function which helps ::graph_walk_edges */
void edge_walker(struct graph *g, int i, void *extra)
{
  int z;
  struct edge *item;
  struct vertex *v = *(g->array + i);
  struct eset *e = extra;
  edge_function fn = e->fn;
  void *edge_extra = e->extra;
  for (z=0; z < (v->out).last_index; z++) {
    item = *((v->out).array + z);
    if (item)
      fn(g, item->from, item->to, edge_extra);
  }
}

void graph_walk_edges(struct graph* g, edge_function fn, void *extra)
{
  struct eset e;
  e.fn = fn;
  e.extra = extra;
  graph_walk_vertices(g, edge_walker, &e);
}

/*! \brief Callback helper function for ::graph_print_edges */
void edge_printer(struct graph *g, int i, int j, void *extra)
{
  printf("%d %d\n", i, j);
}

void graph_print_edges(struct graph *g)
{
  if (!g)
    printf("NULL graph\n");
  graph_walk_edges(g, edge_printer, NULL);
}

/*! \brief Callback helper function for ::graph_print_vertices */
void vertex_printer(struct graph *g, int i, void *extra)
{
  printf("%d ", i);
}

void graph_print_vertices(struct graph *g)
{
  if (!g) {
    printf("NULL graph\n");
    return;
  }
  graph_walk_vertices(g, vertex_printer, NULL);
  printf("\n");
}

/*! \brief Callback function which helps ::graph_delete. */
void vertex_cleaner(struct graph *g, int i, void *extra)
{
  struct vertex *v;
  struct edge *item;
  int z;

  v = *(g->array + i);
  for (z=0; z<(v->out).last_index; z++) {
    item = *((v->out).array + z);
    if (item)
      free(item);
  }
  free((v->in).array);
  free((v->out).array);
  free(v);
}

void graph_delete(struct graph *g)
{
  graph_walk_vertices(g, vertex_cleaner, NULL);
  free(g->array);
  free(g);
}

void graph_read(struct graph *g, char *filename)
{
  FILE *file;
  char c;
  char buffer[2][10];
  int pos=0;
  int el=0;
  int a=0, b;
  file = fopen(filename, "r");
  while (fread(&c, 1, 1, file) != 0) {
    if (((el == 0) || (el == 2)) && (c == ' '))
      ;
    else
    if ((c >= '0') && (c <= '9')) {
      if ((el == 0) || (el == 2))
        el++;
      if (el == 1)
        buffer[0][pos++] = c;
      else if (el == 3)
        buffer[1][pos++] = c;
    }
    else
      if ((c == ' ') && (el == 1)) {
        buffer[0][pos] = 0;
        a = atoi(buffer[0]);
        el++;
        pos = 0;
      }
      else
        if (c == '\n') {
          buffer[1][pos] = 0;
          b = atoi(buffer[1]);
          graph_insert_edge(g, a, b);
          el = 0;
          pos = 0;
        }
  }
  fclose(file);
}

struct edge_iterator *graph_edge_iterator(struct edges *e,
    struct edge_iterator *i)
{
  if (!i)
    i = malloc(sizeof(struct edge_iterator));
  i->current_index = -1;
  i->edges = e;
  return i;
}

struct edge *graph_edge_itr_next(struct edge_iterator *i)
{
  (i->current_index)++;
  if (i->current_index > i->edges->last_index-1)
    return NULL;
  while (!*(i->edges->array + i->current_index)) {
    (i->current_index)++;
    if (i->current_index > i->edges->last_index-1)
      return NULL;
  }
  return *(i->edges->array + i->current_index);
}
    
struct vertex_iterator *graph_vertex_iterator(struct graph *g,
    struct vertex_iterator *i)
{
  if (!i)
    i = malloc(sizeof(struct vertex_iterator));
  i->graph = g;
  i->current_index = -1;
  return i;
}

struct vertex *graph_vertex_itr_next(struct vertex_iterator *i)
{
  (i->current_index)++;
  if (i->current_index > i->graph->last_index-1)
    return NULL;
  while (!*(i->graph->array + i->current_index)) {
    (i->current_index)++;
    if (i->current_index > i->graph->last_index-1)
      return NULL;
  }
  return *(i->graph->array + i->current_index);
}

