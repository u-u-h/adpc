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

#ifndef _TAB_GRASP_H_
#define _TAB_GRASP_H_

#include <poly_runtime.h>

void greedy_degree_sum(struct graph *g, int q, void *extra);
void greedy_degree(struct graph *g, int q, void *extra);
void greedy_in_out_mul(struct graph *g, int q, void *extra);
void greedy_tab_mul(struct graph *g, int q, void *extra);
void greedy_tab_plus(struct graph *g, int q, void *extra);
void greedy_tab_max(struct graph *g, int q, void *extra);

void tab_grasp(struct dep_graph *g, double alpha, int it, int best_asm_rt);

#endif
