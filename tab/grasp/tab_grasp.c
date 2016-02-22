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

#include <assert.h>

#include <poly_runtime.h>
#include <graph.h>
#include <poly_type.h>
#include <array_int.h>
#include <random.h>

#include "../general/dep_graph.h"

#include "tab_grasp.h"
#include "../general/log.h"
#include "../../tabulate.h"

void greedy_degree_sum(struct graph *g, int q, void *extra)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, q);
  int i, j;
  if (v->tabulated != 0) 
    v->greediness = -1;
  else {
    i = compute_degree_sum(&((v->vertex).in));
    j = compute_degree_sum(&((v->vertex)).out);
    v->greediness = i * j;
  }
}

void greedy_degree(struct graph *g, int q, void *extra)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, q);
  int i, j;
  if (v->tabulated != 0)
    v->greediness = -1;
  else {
    i = compute_degree_sum(&((v->vertex).in));
    j = compute_degree_sum(&((v->vertex)).out);
    v->greediness = i + j;
  }
}

void greedy_in_out_mul(struct graph *g, int q, void *extra)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, q);
  int i, j;
  if (v->tabulated != 0)
    v->greediness = -1;
  else {
    i = (v->vertex).in.count;
    j = (v->vertex).out.count;
    v->greediness = i * j;
  }
}

void greedy_tab_mul(struct graph *g, int q, void *extra)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, q);
  int i, j;
  if (v->tabulated != 0)
    v->greediness = -1;
  else {
    i = (v->vertex).in.count - v->in_tab_count;
    j = (v->vertex).out.count - v->out_tab_count;
    v->greediness = i * j;
  }
}

void greedy_tab_plus(struct graph *g, int q, void *extra)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, q);
  int i, j;
  if (v->tabulated != 0)
    v->greediness = -1;
  else {
    i = (v->vertex).in.count - v->in_tab_count;
    j = (v->vertex).out.count - v->out_tab_count;
    v->greediness = i + j;
  }
}

void greedy_tab_max(struct graph *g, int q, void *extra)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, q);
  int i, j;
  if (v->tabulated != 0)
    v->greediness = -1;
  else {
    i = (v->vertex).in.count - v->in_tab_count;
    j = (v->vertex).out.count - v->out_tab_count;
    v->greediness = i > j ? i : j;
  }
}

void vertex_create_tab_count(struct dep_graph *g, int k)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(&(g->graph), k), *w;
  struct edge *e;
  struct edge_iterator itr;

  v->in_tab_count = 0;
  v->out_tab_count = 0;

  graph_edge_iterator(&((v->vertex).in), &itr);
  while ((e = graph_edge_itr_next(&itr))) {
    w = (struct nt_vertex*) graph_vertex(&(g->graph), e->from);
    if (w->tabulated == 1)
      (v->in_tab_count)++;
  }
  graph_edge_iterator(&((v->vertex).out), &itr);
  while ((e = graph_edge_itr_next(&itr))) {
    w = (struct nt_vertex*) graph_vertex(&(g->graph), e->to);
    if (w->tabulated == 1)
      (v->out_tab_count)++;
  } 
}

void compute_greediness(struct dep_graph *g)
{
  struct vertex_iterator itr;
  struct nt_vertex *v;
  
  g->max_greediness = 0;

  graph_vertex_iterator(&(g->graph), &itr);
  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&itr)))
    vertex_create_tab_count(g, itr.current_index);

  graph_vertex_iterator(&(g->graph), &itr);
  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&itr)))
    (g->greedy_fn)(&(g->graph), itr.current_index, NULL);
}

void compute_max_greediness(struct dep_graph *g)
{
  struct vertex_iterator itr;
  struct nt_vertex *v;
  
  g->max_greediness = 0;
  graph_vertex_iterator(&(g->graph), &itr);
  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&itr)))
    if (v->greediness > g->max_greediness)
      g->max_greediness = v->greediness;
}

void make_rcl(struct dep_graph *g, struct array_int *rcl, double alpha)
{
  double bound;
  struct vertex_iterator itr;
  struct nt_vertex *v;

  compute_max_greediness(g);
  bound = alpha * g->max_greediness;

  graph_vertex_iterator(&(g->graph), &itr);
  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&itr))) {
    if (v->greediness > bound)
      array_int_add(rcl, itr.current_index);
  }
}

void vertex_tabulate(struct dep_graph *g, int k, int l)
{
  struct nt_vertex *v, *w;
  struct edge_iterator itr;
  struct edge *e;
  v = (struct nt_vertex*) graph_vertex(&(g->graph), k);
  
  assert(v);

  if (v->tabulated == l)
    return;
  
  v->tabulated = l;
  (g->greedy_fn)(&(g->graph), k, NULL);

  graph_edge_iterator(&((v->vertex).in), &itr);
  while ((e = graph_edge_itr_next(&itr))) {
    (g->greedy_fn)(&(g->graph), e->from, NULL);
    w = (struct nt_vertex*) graph_vertex(&(g->graph), e->from);
    if (l)
      (w->out_tab_count)++;
    else
      (w->out_tab_count)--;
  }
  graph_edge_iterator(&((v->vertex).out), &itr);
  while ((e = graph_edge_itr_next(&itr))) {
    (g->greedy_fn)(&(g->graph), e->to, NULL);
    w = (struct nt_vertex*) graph_vertex(&(g->graph), e->to);
    if (l)
      (w->in_tab_count)++;
    else
      (w->in_tab_count)--;
  } 
}

void construct(struct dep_graph *g, struct set *tab_list, double a)
{
  struct array_int rcl;
  double alpha;
  int i, j, k;

  if (a == -1) {
    i = random_index(101);
    alpha = 0.01 * ((double) i);
  } else
    alpha = a;

  compute_greediness(g);

  array_int_init(&rcl, (g->graph).vertex_count);
  for (i=0; i < (g->graph).vertex_count; i++) {
    make_rcl(g, &rcl, alpha);
    if (rcl.count == 0)
      break;
    j = random_index(rcl.count);
    k = array_int_get(&rcl, j);
    
    vertex_tabulate(g, k, 1);

    if (!set_contains(tab_list, k))
      set_add(tab_list, k);
    array_int_reset(&rcl);
  }
  array_int_delete(&rcl);
}

int int_compare(int a, int b)
{
  return a < b ? -1 : ( a > b ? 1 : 0 );
}

int is_new_rt_better(struct dep_graph *g, struct set *tab_list,
    int old_elements, Poly new_rt, Poly old_rt)
{
  int a, b, c, r;

  if (poly_illegal(new_rt))
    return 0;
  if (poly_illegal(old_rt))
    return 1;

  a = poly_asm_compare(new_rt, old_rt);

  /*
  printf("new_rt: "); poly_print(stdout, new_rt);
  printf("\nold_rt: "); poly_print(stdout, old_rt);
  */

  if (a == 0) {
    c = int_compare(tab_list->elements, old_elements);
    if (c == 0) {
      b = poly_compare(new_rt, old_rt);
      if (b < 0)
        r = 1;
      else
        r = 0;
    } else {
      if (c < 0)
        r = 1;
      else 
        r = 0;
    }
  } else 
    if (a < 0)
      r = 1;
    else
      r = 0;
/*
  printf("\n #new_tab: %d, #old_tab: %d, a: %d, b: %d, c: %d, decision: %d\n", tab_list->elements, old_elements, a, b, c, r);
*/
  return r;
}

Poly local_search(struct dep_graph *g, struct set *tab_list, int best_asm_rt)
{
  Poly rt, new_rt;
  struct nt_vertex *v;
  int i;
  int old_elements;

  rt = compute_runtime_lim(&(g->graph), best_asm_rt);
  //rt = compute_runtime(&(g->graph));
  old_elements = tab_list->elements;

  for (i=0; i < tab_list->size; i++) {
    if (!set_contains(tab_list, i))
      continue;
    v = (struct nt_vertex*) graph_vertex(&(g->graph),
        i);

    v->tabulated = 0;
    set_remove(tab_list, i);
    new_rt = compute_runtime_lim(&(g->graph), best_asm_rt);
    //new_rt = compute_runtime(&(g->graph));
/*
    printf("--- Local search: %i\t Runtime: ", i);
    poly_print(stdout, new_rt); printf(" ");
    print_tab_graph(&(g->graph));
*/
    if (is_new_rt_better(g, tab_list, old_elements, new_rt, rt)) {
        poly_copy_free(&rt, &new_rt);
        old_elements = tab_list->elements;
    }
    else {
      v->tabulated = 1;
      set_add(tab_list, i);
      free(new_rt);
    }
  }
  return rt;
  
}

void tab_grasp(struct dep_graph *g, double alpha, int it_opt, int best_asm_rt)
{
  Poly rt, old_rt = NULL;
  struct set tab_list, best_tab;
  int i;
  int old_elements;
  int it;
  int it_step = 0;
  int it_upper_bound = 10000;

  if (it_opt == -1) {
    it_step = (g->graph).vertex_count;
    it = it_step;
  }
  else
    it = it_opt;

  log_printf(VERB_MOST, "Grammar has %d NTs\n", (g->graph).vertex_count);

  old_rt = poly_exp();
  set_new(&tab_list, (g->graph).vertex_count + 1);
  set_new(&best_tab, (g->graph).vertex_count + 1);
  old_elements = (g->graph).vertex_count;

  for (i=0; i<it; i++) {
    construct(g, &tab_list, alpha);
    rt = local_search(g, &tab_list, best_asm_rt);
/*
    printf("~~~ Iteration: %i\t Runtime: ", i);
    poly_print(stdout, rt); printf(" ");
    print_tab_graph(&(g->graph));
*/
    if (is_new_rt_better(g, &tab_list, old_elements, rt, old_rt)) {
        old_elements = tab_list.elements;
        poly_copy_free(&old_rt, &rt);

        set_copy(&best_tab, &tab_list);

        //
        if (log_level(VERB_MORE)) {
          printf("++++++++++++++++++++++++++++++++++++++++\n");
          printf("Iteration: %i\t Runtime: ", i);
          poly_print(stdout, old_rt); printf("\n");
          print_tab_graph(&(g->graph));
          printf("++++++++++++++++++++++++++++++++++++++++\n");
        }
        //

        if ((it_opt == -1) && (it < it_upper_bound))
          it += it_step;
    }
    else
      free(rt);

    mark_graph(g, &tab_list, 0);
    assert(tab_list.elements == 0);

  }

  log_printf(VERB_MORE, "needed %d iterations\n", it);

  mark_graph(g, &best_tab, 1);

  set_delete(&tab_list);
  set_delete(&best_tab);
  if (old_rt)
    free(old_rt);
}
