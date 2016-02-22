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

#ifndef _DEP_GRAPH_H_
#define _DEP_GRAPH_H_

#include "poly_runtime.h"

#include "set.h"

#include <tabulate.h>

void dep_graph_tab_array(struct dep_graph *g, int *array);

void dep_graph_tab_flat_array(struct dep_graph *g, int *array);

void mark_graph(struct dep_graph *g, struct set *tab_list, int z);

void dep_graph_fix_annotation(Nts inputdata, struct dep_graph *g);

#endif
