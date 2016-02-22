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

#include "dep_graph.h"

void dep_graph_tab_array(struct dep_graph *g, int *array)
{
  struct vertex_iterator v_itr;
  struct nt_vertex *v;

  graph_vertex_iterator(&(g->graph), &v_itr);

  int j = 0;
  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&v_itr))) {
    if (v->tabulated > 0)
      array[j++] = v_itr.current_index;
  }
}

void dep_graph_tab_flat_array(struct dep_graph *g, int *array)
{
  struct vertex_iterator v_itr;
  struct nt_vertex *v;

  graph_vertex_iterator(&(g->graph), &v_itr);

  int j = 1;
  array[0] = 1;
  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&v_itr)))
    if (v->tabulated > 0)
      array[j++] = v->tabulated;
    else
      // ignore reductions ( -1) at this point
      array[j++] = 0;
}

void mark_graph(struct dep_graph *g, struct set *tab_list, int z)
{
  struct nt_vertex *v;
  int i, j;

  for (i=0; i < tab_list->size; i++) {
    if (!set_contains(tab_list, i))
        continue;
    j = i;
    v = (struct nt_vertex*) graph_vertex(&(g->graph), j);
    v->tabulated = z;
    set_remove(tab_list, i);
  }
}


void dep_graph_fix_annotation(Nts inputdata, struct dep_graph *g)
{
  struct vertex_iterator v_itr;
  struct nt_vertex *v;
  struct vertex_iterator w_itr;
  struct nt_vertex *w;
  struct dep_graph *f;

  f = (struct dep_graph*) graph_init(50, 50, 10,
      sizeof(struct dep_graph),
      sizeof(struct nt_vertex),
      sizeof(struct dep_edge),
      NULL);

  nts_to_graph(inputdata, &(f->graph) );

  graph_vertex_iterator(&(g->graph), &v_itr);
  graph_vertex_iterator(&(f->graph), &w_itr);

  while ((v = (struct nt_vertex*) graph_vertex_itr_next(&v_itr))) {
    w = (struct nt_vertex*) graph_vertex_itr_next(&w_itr);
    if ((v->tabulated == -1) && (w->annotate == 2))
      v->tabulated = 0;
  }

  graph_delete(&(f->graph));
}
