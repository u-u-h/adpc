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

#include <assert.h>

#include "../../tabulate.h"
#include "poly_runtime.h"
#include "bag.h"
#include "index_list.h"
#include "graph.h"

#include "comb.h"
#include "dep_graph.h"
#include "log.h"

void free_runstate(struct list_head *head)
{
  struct run_state *state = (struct run_state*) head;
  free(state->prefactor);
  free(state);
}

void update_prefactor(struct run_state **run_states, Poly usage) {
  struct list_head *head, *state_itr;
  struct run_state *current_state;

  head = &((run_states[0])->head);
  state_itr = head->next;
  while (state_itr != head) {
    current_state = (struct run_state*) state_itr;

#ifdef DEBUG_POLY
    fprintf(stderr, "$ Vor ");
    poly_print(stderr, current_state->prefactor);
    fprintf(stderr, " mal ");
    poly_print(stderr, usage);
    fprintf(stderr, " gleich ");
#endif

    current_state->prefactor = poly_times_free(current_state->prefactor, usage,
        POLY_SIZE);

#ifdef DEBUG_POLY
    poly_print(stderr, current_state->prefactor);
    fprintf(stderr, "\n");
#endif
    
    state_itr = state_itr->next;
  }
}

void reset_prefactor(struct run_state **run_states, Poly usage) {
  struct list_head *head, *state_itr;
  struct run_state *current_state;

  head = &((run_states[0])->head);
  state_itr = head->next;
  while (state_itr != head) {
    current_state = (struct run_state*) state_itr;

#ifdef DEBUG_POLY
    fprintf(stderr, "# Vor ");
    poly_print(stderr, current_state->prefactor);
    fprintf(stderr, " geteilt durch ");
    poly_print(stderr, usage);
    fprintf(stderr, " gleich ");
#endif

    poly_div(current_state->prefactor, usage,
        POLY_SIZE);

#ifdef DEBUG_POLY
    poly_print(stderr, current_state->prefactor);
    fprintf(stderr, "\n");
#endif

    state_itr = state_itr->next;
  }
}

/*! \brief Solves reccurrences for NT q.
  \return -1, 0, 1 if rec_bag contains q and other elements, doesn't contain q,
        contains q */
int solve_recs(struct bag *rec_bag, int q) {
  if (bag_contains(rec_bag, q)) {
    if (rec_bag->count > 1)
      return -1;
    else {
      bag_remove(rec_bag, q);
      return 1;
    }
  } else
    return 0;
}

Poly runtime(int q, struct graph *g,
    struct bag *rec_bag,
    struct run_state **run_states)
{
  struct nt_vertex *v, *w;
  int p;
  Poly res, result;
  struct edge_iterator itr;
  struct dep_edge *edge;
  struct run_state *r_state;
  struct bag local_rec_bag;

  result = poly_int(0, POLY_SIZE);
  v = (struct nt_vertex*) graph_vertex(g, q);
  if (run_states[q]) { // not NULL
    if (poly_compare_int((run_states[q])->prefactor, 1) == 0) {
      bag_add(rec_bag, q);
      return result;
    }
    else {
      free(result);
      return poly_exp(); //exponential
    }
  } else {
    r_state = calloc(1, sizeof(struct run_state));
    r_state->prefactor = poly_int(1, POLY_SIZE);
    index_list_add((struct list_head**) run_states, q, &(r_state->head));
  }

  poly_add(result, v->runtime);
  graph_edge_iterator(&((v->vertex).out), &itr);
  while ((edge = (struct dep_edge*) graph_edge_itr_next(&itr))) {
    p = (edge->edge).to;
    w = (struct nt_vertex*) graph_vertex(g, p);

    if (w->tabulated != 1) {
      
      poly_sub(result, edge->usage);
      update_prefactor(run_states, edge->usage);

      bag_init(&local_rec_bag, g->vertex_count + 1);
      res = runtime(p, g, &local_rec_bag, run_states);
      bag_union_delete(rec_bag, &local_rec_bag, g->vertex_count + 1);

      if (poly_exponential(res)) {
        free_runstate(index_list_remove((struct list_head**) run_states, q));
        free(result);
        return res;
      }

      res = poly_times_free(res, edge->usage, POLY_SIZE);
      poly_add(result, res);
      free(res);

      reset_prefactor(run_states, edge->usage);

    }
  }

  switch (solve_recs(rec_bag, q)) {
    case -1 : 
      free_runstate(index_list_remove((struct list_head**) run_states, q));
      free(result);
      return poly_exp();
    case 1 :
      res = poly_n(1, POLY_SIZE);
      result = poly_times_free(result, res, POLY_SIZE);
      free(res);
      break;
  }

  // clear running state of q
  free_runstate(index_list_remove((struct list_head**) run_states, q));
  return result;
}

Poly compute_runtime(struct graph *g)
{
  struct bag rec_bag;
  struct run_state **run_states;
  Poly result, res;
  int i;
  struct nt_vertex *v;
  struct vertex_iterator itr;
  
  bag_init(&rec_bag, g->vertex_count + 1);
  run_states = (struct run_state**) index_list_init(g->vertex_count+1);
 
  result = runtime(1, g, &rec_bag, run_states);

  if (!poly_exponential(result)) {
    // axiom == NT 1
    v = (struct nt_vertex*) graph_vertex(g, 1);
    if (v->tabulated == 1) {
      result = poly_times_free(result, v->space, POLY_SIZE);
    }

#ifdef DEBUG_RT
    fprintf(stderr, "====================\n");
    fprintf(stderr, "Axiom rt: ");
    poly_print(stderr, result);
    fprintf(stderr, "\n");
#endif

    // compute additional runtime of tabulated NT
    graph_vertex_iterator(g, &itr);
    while ((v = (struct nt_vertex*) graph_vertex_itr_next(&itr)))
      if ((itr.current_index > 1) && (v->tabulated == 1)) {
        // TODO: is rec_bag and run_states always empty?
        if (rec_bag.count != 0) {
          bag_delete(&rec_bag);
          bag_init(&rec_bag, g->vertex_count + 1);
        }
        assert(rec_bag.count == 0);
        assert( ((*run_states)->head).next == &((*run_states)->head) );

        i = itr.current_index;
        res = runtime(i, g, &rec_bag, run_states);
        res = poly_times_free(res, v->space, POLY_SIZE);

#ifdef DEBUG_RT
        fprintf(stderr, "NT %d rt: ", i);
        poly_print(stderr, res);
        fprintf(stderr, "\n");
#endif

        poly_add(result, res);
        free(res);
      }
  }

  index_list_free((struct list_head**)run_states);
  bag_delete(&rec_bag);
  return result;
}


////

Poly compute_runtime_nodebug(struct graph *g)
{
  struct bag rec_bag;
  struct run_state **run_states;
  Poly result, res;
  int i;
  struct nt_vertex *v;
  struct vertex_iterator itr;
  
  bag_init(&rec_bag, g->vertex_count + 1);
  run_states = (struct run_state**) index_list_init(g->vertex_count+1);
 
  result = runtime(1, g, &rec_bag, run_states);

  if (!poly_exponential(result)) {
    // axiom == NT 1
    v = (struct nt_vertex*) graph_vertex(g, 1);
    if (v->tabulated == 1) {
      result = poly_times_free(result, v->space, POLY_SIZE);
    }

    // compute additional runtime of tabulated NT
    graph_vertex_iterator(g, &itr);
    while ((v = (struct nt_vertex*) graph_vertex_itr_next(&itr)))
      if ((itr.current_index > 1) && (v->tabulated == 1)) {
        // TODO: is rec_bag and run_states always empty?
        if (rec_bag.count != 0) {
          bag_delete(&rec_bag);
          bag_init(&rec_bag, g->vertex_count + 1);
        }
        assert(rec_bag.count == 0);
        assert( ((*run_states)->head).next == &((*run_states)->head) );

        i = itr.current_index;
        res = runtime(i, g, &rec_bag, run_states);
        res = poly_times_free(res, v->space, POLY_SIZE);

        poly_add(result, res);
        free(res);
      }
  }

  index_list_free((struct list_head**)run_states);
  bag_delete(&rec_bag);
  return result;
}


////


Poly runtime_lim(int q, struct graph *g,
    struct bag *rec_bag,
    struct run_state **run_states,
    int best_asm_rt) {
  struct nt_vertex *v, *w;
  int p;
  Poly res, result;
  struct edge_iterator itr;
  struct dep_edge *edge;
  struct run_state *r_state;
  struct bag local_rec_bag;

  result = poly_int(0, POLY_SIZE);
  v = (struct nt_vertex*) graph_vertex(g, q);
  if (run_states[q]) { // not NULL
    if (poly_compare_int((run_states[q])->prefactor, 1) == 0) {
      bag_add(rec_bag, q);
      return result;
    }
    else {
      free(result);
      return poly_exp(); //exponential
    }
  } else {
    r_state = calloc(1, sizeof(struct run_state));
    r_state->prefactor = poly_int(1, POLY_SIZE);
    index_list_add((struct list_head**) run_states, q, &(r_state->head));
  }

  poly_add(result, v->runtime);
  graph_edge_iterator(&((v->vertex).out), &itr);
  while ((edge = (struct dep_edge*) graph_edge_itr_next(&itr))) {
    p = (edge->edge).to;
    w = (struct nt_vertex*) graph_vertex(g, p);

    if (w->tabulated != 1) {

      poly_sub(result, edge->usage);
      
      update_prefactor(run_states, edge->usage);

      bag_init(&local_rec_bag, g->vertex_count + 1);
      res = runtime_lim(p, g, &local_rec_bag, run_states, best_asm_rt);
      bag_union_delete(rec_bag, &local_rec_bag, g->vertex_count + 1);

      if (poly_exponential(res) || poly_illegal(res)) {
        free_runstate(index_list_remove((struct list_head**) run_states, q));
        free(result);
        return res;
      }

      res = poly_times_free(res, edge->usage, POLY_SIZE);
      poly_add(result, res);
      free(res);

      if (poly_asm_compare_int(result, best_asm_rt) > 0) {
        free_runstate(index_list_remove((struct list_head**) run_states, q));
        free(result);
        return poly_ill();
      }
        

      reset_prefactor(run_states, edge->usage);

    }
  }

  switch (solve_recs(rec_bag, q)) {
    case -1 : 
      free(result);
      result =  poly_exp();
      break;
    case 1 :
      res = poly_n(1, POLY_SIZE);
      result = poly_times_free(result, res, POLY_SIZE);
      free(res);

      if (poly_asm_compare_int(result, best_asm_rt) > 0) {
        free(result);
        result = poly_ill();
      }
      break;
  }

  // clear running state of q
  free_runstate(index_list_remove((struct list_head**) run_states, q));
  return result;
}

Poly compute_runtime_lim(struct graph *g, int best_asm_rt)
{
  struct bag rec_bag;
  struct run_state **run_states;
  Poly result, res;
  int i;
  struct nt_vertex *v;
  struct vertex_iterator itr;
  
  bag_init(&rec_bag, g->vertex_count + 1);
  run_states = (struct run_state**) index_list_init(g->vertex_count+1);
 
  result = runtime_lim(1, g, &rec_bag, run_states, best_asm_rt);

  if (!(poly_exponential(result) || poly_illegal(result))) {
    // axiom == NT 1
    v = (struct nt_vertex*) graph_vertex(g, 1);
    if (v->tabulated == 1) {
      result = poly_times_free(result, v->space, POLY_SIZE);
    }

    if (poly_asm_compare_int(result, best_asm_rt) <= 0) {
      // compute additional runtime of tabulated NT
      graph_vertex_iterator(g, &itr);
      while ((v = (struct nt_vertex*) graph_vertex_itr_next(&itr)))
        if ((itr.current_index > 1) && (v->tabulated == 1)) {
          // TODO: is rec_bag and run_states always empty?
          if (rec_bag.count != 0) {
            bag_delete(&rec_bag);
            bag_init(&rec_bag, g->vertex_count + 1);
          }
          assert(rec_bag.count == 0);
          assert( ((*run_states)->head).next == &((*run_states)->head) );

          i = itr.current_index;
          res = runtime_lim(i, g, &rec_bag, run_states, best_asm_rt);

          if (poly_illegal(res)) {
            free(result);
            result = res;
            break;
          }

          res = poly_times_free(res, v->space, POLY_SIZE);
          poly_add(result, res);
          free(res);

          if (poly_asm_compare_int(result, best_asm_rt) > 0) {
            free(result);
            result = poly_ill();
            break;
          }
        }
    } else {
      free(result);
      result = poly_ill();
    }
  }

  index_list_free((struct list_head**)run_states);
  bag_delete(&rec_bag);
  return result;
}

////
/*! \brief Converts adpc poly representation (p[0] = l + 1)
  to poly.c representation: p[0] = l

  x_0 * n^0 + ... + x_l * n^l */
Poly convert_poly(ADPCPoly p)
{
  Poly r;
  int i;
  r = calloc(POLY_SIZE + 2, sizeof(Factor));
  r[0] = p[0] - 1;
  for (i=0; i < r[0]+1; i++)
    r[1+i] = p[1+i];
  return r;
}

void nts_to_graph(Nts nts, struct graph *g)
{
  Nts itr;
  Deps d_itr;
  struct nt_vertex *v;
  struct dep_edge *e;
  int q;
  
  itr = nts;
  do {
    q = itr->nt;
    v = (struct nt_vertex*) graph_vertex(g, itr->nt);
    if (!v)
      v = (struct nt_vertex*) graph_add_vertex(g, itr->nt);
    v->runtime = convert_poly(itr->runtime);
    v->space = convert_poly(itr->tabulationType);
    v->annotate = itr->tab;

    d_itr = itr->deps;
    while (d_itr) {
      e = (struct dep_edge*) graph_insert_edge(g, itr->nt, d_itr->nt);
      e->usage = convert_poly(d_itr->usages);
      d_itr = d_itr->next;
    }
    itr = itr->next;
  } while (itr);
}

void print_dep_walker(struct graph *g, int i, void *extra)
{
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, i);
  struct edge_iterator itr;
  struct dep_edge *edge;
  printf("-------------------------\n");
  printf("Nt: %d, user configuration: %d,  tab. runtime: ", i, v->annotate);
  poly_print(stdout, v->runtime);
  printf(", space: ");
  poly_print(stdout, v->space);
  if (v->tabulated == 1)
    printf("\ttabulated");
  printf("\n");
  printf("Dependencies:\n");
  graph_edge_iterator(&((v->vertex).out), &itr);
  while ((edge = (struct dep_edge*) graph_edge_itr_next(&itr))) {
    printf("\t-> Nt: %d, usage: ", (edge->edge).to);
    poly_print(stdout, edge->usage);
    printf("\n");
  }
}

void print_dep_graph(struct graph *g)
{
  graph_walk_vertices(g, print_dep_walker, NULL);
}

void print_tab_walker(struct graph *g, int i, void *extra)
{
  int *j = extra;
  struct nt_vertex *v = (struct nt_vertex*) graph_vertex(g, i);
  if (v->tabulated == 1) {
    printf(" %d", i);
    (*j)++;
  }
}

void print_tab_graph(struct graph *g)
{
  int i = 0;
  printf("{");
  graph_walk_vertices(g, print_tab_walker, &i);
  printf(" } #%d\n", i);
}

int compute_degree_sum(struct edges *e)
{
  int r = 0;
  struct edge_iterator itr;
  struct dep_edge *dep;

  graph_edge_iterator(e, &itr);
  while ((dep = (struct dep_edge*) graph_edge_itr_next(&itr)))
    r += poly_degree(dep->usage) + 1;
  return r;
}

void dep_graph_delete(struct dep_graph *g)
{
  struct vertex_iterator v_itr;
  struct edge_iterator e_itr;
  struct dep_edge *e;
  struct nt_vertex *v;

  graph_vertex_iterator(&(g->graph), &v_itr);

  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&v_itr))) {
    free(v->runtime);
    free(v->space);

    graph_edge_iterator(&((v->vertex).out), &e_itr);
    while ((e = (struct dep_edge*) graph_edge_itr_next(&e_itr))) {
      free(e->usage);
    }
  }
  graph_delete(&(g->graph));
}


typedef int (*constant_factor_fn)(Poly best_rt, Poly new_rt, void* extra);

int highest_poly(Poly best_rt, Poly new_rt, void *extra)
{
  Factor bound = * ((Factor*) extra);
  if (poly_asm_compare(best_rt, new_rt))
    return 1;

  int d = poly_degree(best_rt);

  if ((new_rt[d+1] <= bound) && (new_rt[d+1] < best_rt[d+1]))
    return 0;
  else
    return 1;
}

struct complete_data {
  double bound;
  int n;
};

int complete_poly(Poly best_rt, Poly new_rt, void *extra)
{
  struct complete_data *d = extra;
  double bound = d->bound;
  int n = d->n;
  double current = poly_insert(best_rt, n);
  double new = poly_insert(new_rt, n);
  
  if ((new <= bound) && (new < current))
    return 0;
  else
    return 1;
}

void comb_set(struct set *s, struct comb_handle *h, int j)
{
  int i;
  for (i = 0; i< h->val_length; i++)
    if (j)
      set_add(s, (h->val)[i]);
    else
      set_remove(s, (h->val)[i]);
}


// FIXME stupid mark_graph removing elements from set - change that!
int runtime_improve_constant_factors(struct dep_graph *g, int max, int factor,
    int input_length)
{
  Poly best_rt, new_rt;
  int best_asm_rt;
  int length;
  int *array;
  struct comb_handle h;
  struct set table, best_table;
  int result = 1;
  Factor bound;
  struct complete_data data;
  void *extra;
  constant_factor_fn is_constant_factor_better;

  best_rt = compute_runtime_nodebug(&(g->graph));

  best_asm_rt = poly_degree(best_rt);

  if (input_length == -1) {
    bound = best_rt[best_asm_rt+1]
      - (Factor) ( ((double) factor / 100.0) * (double) (best_rt[best_asm_rt+1]));
    extra = &bound;
    is_constant_factor_better = highest_poly;
  } else {
    double a = poly_insert(best_rt, input_length);
    log_printf(VERB_MOST, "Runtime: %g (n=%d)\n", a, input_length);
    data.bound = a -  ((double) factor / 100.0) * a;
    data.n = input_length;
    extra = &data;
    is_constant_factor_better = complete_poly;
  }

  if (log_level(VERB_MOST)) {
    if (input_length == -1)
      printf("Upper bound: %" PRIfactor "\n", bound);
    else
      printf("Upper bound: %g (n = %d\n", data.bound, data.n);
    printf("Original runtime: ");
    poly_print(stdout, best_rt); printf("\n");
    print_tab_graph(&(g->graph)); printf("\n");
  }
  
  length = g->graph.vertex_count;

  set_new(&table, length + 1);
  set_new(&best_table, length + 1);

  array = calloc(length+1, sizeof(int));
  dep_graph_tab_flat_array(g, array);

  comb_map_init(&h, max, array, length);

  while (!comb_map_next(&h)) {
    comb_set(&table, &h, 1);
    mark_graph(g, &table, 1);

    new_rt = compute_runtime_lim(&(g->graph), best_asm_rt);

    if (!is_constant_factor_better(best_rt, new_rt, extra)) {
      if (log_level(VERB_MORE)) {
        printf("Found better runtime with smaller constant factors!\n\t");
        poly_print(stdout, new_rt); printf("\n\t");
        if (input_length != -1)
          printf("New bound: %g (n = %d)\n\t",
              poly_insert(new_rt, data.n), data.n);
        print_tab_graph(&(g->graph)); printf("\n");
      }
    
      poly_copy_free(&best_rt, &new_rt);
      set_copy(&best_table, &table);
      comb_set(&best_table, &h, 1);
    } else
      free(new_rt);

    comb_set(&table, &h, 1);
    mark_graph(g, &table, 0);
  }

  if (best_table.elements) {
    mark_graph(g, &best_table, 1);
    result = 0;
  }

  free(array);
  free(best_rt);
  set_delete(&table);
  set_delete(&best_table);
  
  return result;
}
